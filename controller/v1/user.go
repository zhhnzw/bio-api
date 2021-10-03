package v1

import (
	"bio-api/controller"
	"bio-api/dao/mysql"
	"bio-api/dao/redis"
	"bio-api/utils"
	"github.com/gin-contrib/sessions"
	"github.com/gin-gonic/gin"
	"go.uber.org/zap"
	"golang.org/x/crypto/bcrypt"
	"net/http"
	"strings"
)

const (
	StatusOk       string = "操作成功!"
	GetFailed      string = "操作失败!"
	CreateFailed   string = "创建失败!"
	CreateRepeated string = "重复创建!"
	DeleteFailed   string = "删除失败!"
	UpdateFailed   string = "更新失败!"
)

type UserForm struct {
	Id       int    `json:"id"`
	UserName string `json:"userName" example:"guest"`
	NickName string `json:"nickName"`
	Password string `json:"password" example:"f81015fee0b7ad8d472717286c0c7a55"`
	Mobile   string `json:"mobile"`
	Email    string `json:"email"`
	Avatar   string `json:"avatar"`
	Roles    []struct {
		Id       int    `json:"id"`
		RoleName string `json:"name"`
		RoleDesc string `json:"desc"`
	} `json:"roles"`
	IsValid   bool `json:"isValid"`
	PageSize  int  `gorm:"-"`
	PageIndex int  `gorm:"-"`
}

// @Summary 登录
// @Tags 用户
// @Accept application/json
// @Produce application/json
// @Param object body UserForm false "查询参数"
// @Success 200 {object} utils.Resp
// @Router /v1/login [post]
func Login(c *gin.Context) {
	resp := utils.Resp{Data: make(map[string]string), Code: "1"}
	var form UserForm
	if err := c.ShouldBind(&form); err != nil {
		c.JSON(http.StatusBadRequest, resp)
		return
	}
	model := mysql.SysUser{
		UserName: form.UserName,
		Password: form.Password,
	}
	var model1 mysql.SysUser
	db := mysql.DB.Table("sys_user").Select(mysql.SysUserQueryFields).Where("user_name=?", model.UserName).First(&model1)
	if db.Error != nil && !strings.Contains(db.Error.Error(), "record not found") {
		resp.Message = "服务端故障, 查询用户信息失败!"
		zap.L().Error(resp.Message, zap.Error(db.Error))
		c.JSON(http.StatusInternalServerError, resp)
		return
	} else if db.RowsAffected == 0 {
		resp.Message = "没有这个账号!"
		c.JSON(http.StatusBadRequest, resp)
		return
	} else if model1.IsValid == false {
		resp.Message = "该账号已被禁用, 请与管理员联系!"
		c.JSON(http.StatusBadRequest, resp)
		return
	}
	err := bcrypt.CompareHashAndPassword([]byte(model1.Password), []byte(model.Password))
	if err != nil {
		resp.Message = "密码错误!"
	} else {
		resp.Message = StatusOk
		data := make(map[string]interface{})
		data["avatar"] = model1.Avatar
		data["email"] = model1.Email
		data["isSuper"] = model1.IsSuper
		data["mobile"] = model1.Mobile
		data["nickName"] = model1.NickName
		data["userName"] = model1.UserName
		resp.Data = data
		resp.Code = "0"
	}
	session := sessions.Default(c)
	session.Set("userName", model.UserName)
	err = session.Save()
	if err != nil {
		zap.L().Error("", zap.Error(err))
		c.JSON(http.StatusInternalServerError, utils.Resp{Message: "服务端故障, session操作失败", Code: "1"})
		return
	}
	if err := redis.RDB.SAdd(controller.RedisKeyLoginUsers, model.UserName).Err(); err != nil {
		c.JSON(http.StatusInternalServerError, utils.Resp{Message: "服务端故障, redis增加登录用户失败", Code: "1"})
		return
	}
	c.JSON(http.StatusOK, resp)
}

// @Summary 注销
// @Tags 用户
// @Accept application/json
// @Produce application/json
// @Security ApiKeyAuth
// @param Authorization header string false "Cookie"
// @Success 200 {object} utils.Resp
// @Router /v1/logout [post]
func Logout(c *gin.Context) {
	session := sessions.Default(c)
	userName := session.Get("userName")
	if err := redis.RDB.SRem(controller.RedisKeyLoginUsers, userName).Err(); err != nil {
		c.JSON(http.StatusInternalServerError, utils.Resp{Message: "服务端故障, redis剔除登录用户失败", Code: "1"})
		return
	}
	session.Clear()
	c.JSON(http.StatusOK, utils.Resp{Code: "0"})
}

type alterPwdStr struct {
	OldPwd string `json:"oldPwd"`
	NewPwd string `json:"newPwd"`
}

func AlterPwd(c *gin.Context) {
	var model alterPwdStr
	err := c.ShouldBind(&model)
	if err != nil {
		c.JSON(http.StatusBadRequest, utils.Resp{Data: nil, Message: "参数不正确", Code: "1"})
		return
	}
	session := sessions.Default(c)
	userName := session.Get("userName")
	resp := utils.Resp{Code: "1"}
	var model1 mysql.SysUser
	db := mysql.DB.Table("sys_user").Select(mysql.SysUserQueryFields).Where("user_name=?", userName).First(&model1)
	if db.Error != nil {
		resp.Message = "没有这个账号!"
		c.JSON(http.StatusBadRequest, resp)
		return
	}
	err = bcrypt.CompareHashAndPassword([]byte(model1.Password), []byte(model.OldPwd))
	if err != nil {
		resp.Message = "原密码错误!"
		c.JSON(http.StatusBadRequest, resp)
		return
	}
	encodePassword, err := bcrypt.GenerateFromPassword([]byte(model.NewPwd), bcrypt.DefaultCost)
	utils.CheckErr(err, "")
	userModel := mysql.SysUser{Password: string(encodePassword), UserName: userName.(string), IsValid: model1.IsValid}
	if _, err := userModel.UpdateUser(); err != nil {
		zap.L().Error("", zap.Error(err))
		resp.Message = UpdateFailed
		c.JSON(http.StatusBadRequest, resp)
		return
	} else {
		resp.Message = StatusOk
		resp.Code = "0"
		c.JSON(http.StatusOK, resp)
		return
	}
}

func CreateUser(c *gin.Context) {
	var model UserForm
	err := c.ShouldBind(&model)
	if err != nil {
		zap.L().Error("", zap.Error(err))
		c.JSON(http.StatusBadRequest, utils.Resp{Data: nil, Message: "参数不正确", Code: "1"})
		return
	}
	if len(model.UserName) < 1 {
		c.JSON(http.StatusBadRequest, utils.Resp{Message: "userName 必传", Code: "1"})
		return
	}
	userModel := mysql.SysUser{
		UserName: model.UserName,
		NickName: model.NickName,
		Password: model.Password,
		Mobile:   model.Mobile,
		Email:    model.Email,
		Avatar:   model.Avatar,
		IsValid:  true,
	}
	resp := utils.Resp{Data: make(map[string]string), Code: "1"}
	encodePassword, err := bcrypt.GenerateFromPassword([]byte(userModel.Password), bcrypt.DefaultCost)
	utils.CheckErr(err, "")
	userModel.Password = string(encodePassword)
	if succeed, e := userModel.CreateUser(); succeed {
		resp.Code = "0"
		c.JSON(http.StatusOK, resp)
	} else if strings.Contains(e.Error(), "Duplicate entry") {
		resp.Message = CreateRepeated
		c.JSON(http.StatusBadRequest, resp)
	} else {
		resp.Message = err.Error()
		c.JSON(http.StatusBadRequest, resp)
	}
}

// @Summary 获取用户信息
// @Description 获取用户信息
// @Tags 用户
// @Accept application/json
// @Produce application/json
// @Param object query mysql.SysUser false "查询参数"
// @Security ApiKeyAuth
// @Success 200 {object} utils.Resp
// @Router /v1/user [get]
func GetUsers(c *gin.Context) {
	var model mysql.SysUser
	err := c.ShouldBind(&model)
	if err != nil {
		c.JSON(http.StatusBadRequest, utils.Resp{Data: nil, Message: "参数不正确", Code: "1"})
		return
	}
	resp := utils.Resp{Data: make(map[string]string), Message: "", Code: "1"}
	if results, rows, err := model.GetUsers(); err != nil {
		zap.L().Error("", zap.Error(err))
		resp.Message = GetFailed
		resp.Data = results
	} else {
		resp.Message = StatusOk
		resp.Data = map[string]interface{}{"data": results, "count": rows}
		resp.Code = "0"
	}
	c.JSON(http.StatusOK, resp)
}
