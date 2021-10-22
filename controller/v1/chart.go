package v1

import (
	"bio-api/settings"
	"bio-api/utils"
	"fmt"
	"github.com/gin-gonic/gin"
	"github.com/senseyeio/roger"
	"go.uber.org/zap"
	"io/ioutil"
	"net/http"
	"os"
	"strings"
)

// upload
// @Summary 上传文件
// @Description 上传数据源文件以供生成统计图表
// @Tags chart
// @Accept multipart/form-data
// @Param file formData file true "文件"
// @Produce json
// @Success 200 {object} utils.Resp
// @Router /v1/upload [post]
func Pie(c *gin.Context) {
	resp := utils.Resp{Data: make(map[string]string), Code: "1"}
	file, e := c.FormFile("file")
	if e != nil {
		resp.Message = "upload file error:" + e.Error()
		c.JSON(http.StatusOK, resp)
		return
	} else {
		// 保存文件
		if err := c.SaveUploadedFile(file, "./r/"+file.Filename); err != nil {
			c.JSON(http.StatusInternalServerError, resp)
			return
		}
		genFileName := strings.Replace(file.Filename, "txt", "html", 1)
		// 调用r语言函数生成图表
		err := callR(
			"pie",
			fmt.Sprintf("'%s'", file.Filename), // 带单引号对r的调用就是字符串
			fmt.Sprintf("'%s'", genFileName),
			"'pie'",
			"NULL",
			"2", // 没有单引号，对r的调用是int
			"NULL")
		if err != nil {
			resp.Message = "callR error:" + err.Error()
			c.JSON(http.StatusInternalServerError, resp)
			return
		}
		result, err := getHTMLFromResult("./r/" + genFileName)
		if err != nil {
			resp.Message = "getHTMLFromResult error:" + err.Error()
			c.JSON(http.StatusInternalServerError, resp)
			return
		}
		c.String(http.StatusOK, result)
		//c.HTML(http.StatusOK, "./r/pie.html", nil)
	}
}

// f: r语言函数
// params: 传入r语言函数的参数
func callR(f string, params ...string) error {
	rClient, err := roger.NewRClient(settings.Conf.BioChartConfig.Host, settings.Conf.BioChartConfig.Port)
	if err != nil {
		zap.L().Error("Failed to connect", zap.Error(err))
		return err
	}
	sess, err := rClient.GetSession()
	if err != nil {
		return err
	}
	//ret, err := sess.Eval("setwd('~/workspace/mygithub/bio-api/r')")
	_, err = sess.Eval(fmt.Sprintf("setwd('%s')", settings.Conf.RPath))
	if err != nil {
		return err
	}
	//TODO:有err，但是却正常加载了这个r文件
	if v, err := sess.Eval("source('base.r')"); err != nil {
		zap.L().Warn("source('base.r') waring", zap.Any("source_func_return", v), zap.Error(err))
	}
	call := fmt.Sprintf("%s(%s)", f, strings.Join(params, ","))
	zap.L().Info("call r", zap.String("command", call))
	if v, err := sess.Eval(call); err != nil {
		zap.L().Error("call r failed", zap.Any("call_return", v), zap.Error(err))
		return err
	}
	//sess.SendCommand("x <- c(1,2,3,4,5,6,7,8,9,10)")
	//sess.SendCommand("y <- c(1,2,3,4,5,6,7,8,9,10)")
	//sess.SendCommand("jpeg('test1.jpg')")
	//sess.SendCommand("qqplot(x, y)")
	//sess.SendCommand("dev.off()")
	sess.Close()
	return nil
}

func getHTMLFromResult(path string) (string, error) {
	file, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer file.Close()
	content, err := ioutil.ReadAll(file)
	return string(content), nil
}
