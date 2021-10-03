package mysql

import (
	"bio-api/utils"
	"errors"
	"go.uber.org/zap"
	"strconv"
	"time"
)

type SysUser struct {
	Id          int            `json:"id"`
	UserName    string         `json:"userName" form:"userName"`
	NickName    string         `json:"nickName" form:"nickName"`
	Password    string         `json:"-"`
	Mobile      string         `json:"mobile" form:"mobile"`
	Email       string         `json:"email" form:"email"`
	IsValid     bool           `json:"isValid" form:"isValid"`
	IsSuper     bool           `json:"isSuper" form:"isSuper"`
	Avatar      string         `json:"avatar" form:"avatar"`
	CreatedTime time.Time      `json:"-" form:"-" gorm:"-"`
	UpdatedTime utils.JSONTime `json:"updateTime" form:"-" gorm:"-"`
	PageSize    int            `gorm:"-" json:"-" form:"pageSize"`
	PageIndex   int            `gorm:"-" json:"-" form:"pageIndex"`
	FilterValue string         `gorm:"-" json:"-" form:"filterValue"`
}

func (*SysUser) TableName() string {
	return "sys_user"
}

type User struct {
	Id          int            `json:"id"`
	UserName    string         `json:"userName" form:"userName"`
	NickName    string         `json:"nickName" form:"nickName"`
	Password    string         `json:"-"`
	Mobile      string         `json:"mobile" form:"mobile"`
	Email       string         `json:"email" form:"email"`
	IsValid     bool           `json:"isValid" form:"isValid"`
	Avatar      string         `json:"avatar" form:"avatar"`
	CreatedTime time.Time      `json:"-" form:"-" gorm:"-"`
	UpdatedTime utils.JSONTime `json:"updateTime" form:"-" gorm:"-"`
	PageSize    int            `gorm:"-" json:"-" form:"pageSize"`
	PageIndex   int            `gorm:"-" json:"-" form:"pageIndex"`
	FilterValue string         `gorm:"-" json:"-" form:"filterValue"`
}

func (*User) TableName() string {
	return "tb_user"
}

var SysUserQueryFields []string

func init() {
	SysUserQueryFields = []string{"id", "user_name", "nick_name", "password", "mobile", "email", "is_valid", "is_super", "avatar", "updated_time"}
}

func (model *SysUser) CreateUser() (bool, error) {
	DB.NewRecord(model)
	d := DB.Create(model)
	if DB.NewRecord(model) {
		zap.L().Warn("mysql 插入失败", zap.Any("model", model))
		return false, d.Error
	}
	return true, nil
}

func (model *SysUser) GetUsers() ([]SysUser, int, error) {
	results := make([]SysUser, 0, model.PageSize)
	db := DB.Table("sys_user").Select(SysUserQueryFields)
	if len(model.FilterValue) > 0 {
		if _, err := strconv.Atoi(model.FilterValue); err != nil {
			db = db.Where("user_name LIKE ?", "%"+model.FilterValue+"%")
		} else {
			db = db.Where("mobile LIKE ?", "%"+model.FilterValue+"%")
		}
	}
	if len(model.UserName) > 0 {
		db = db.Where("user_name=?", model.UserName)
	}
	var rows int
	db.Count(&rows)
	if db.Error != nil {
		return results, rows, db.Error
	}
	db = db.Order("updated_time DESC").
		Limit(model.PageSize).Offset((model.PageIndex - 1) * model.PageSize).Find(&results)
	return results, rows, db.Error
}

func (model *SysUser) GetUser() (*SysUser, error) {
	db := DB.Select(SysUserQueryFields).Where("id=?", model.Id).First(model)
	return model, db.Error
}

func (model *SysUser) UpdateUser() (int64, error) {
	if model.Id < 1 && len(model.UserName) < 1 {
		return 0, errors.New("id 和 user_name 必传其一")
	}
	db := DB
	if model.Id > 0 {
		db = db.Where("id=?", model.Id)
	} else if len(model.UserName) > 0 {
		db = db.Where("user_name=?", model.UserName)
	}
	db.Update("is_valid", model.IsValid)
	db = db.Updates(*model)
	return db.RowsAffected, db.Error
}

func (model *SysUser) DeleteUser() error {
	if model.Id < 1 {
		return errors.New("必须按id号删除记录")
	}
	var userModel SysUser
	db := DB.Select("id, user_name").Where("id=?", model.Id).Find(&userModel)
	db = DB.Where("id=?", model.Id).Delete(&userModel)
	if db.Error != nil {
		return db.Error
	} else {
		return nil
	}
}
