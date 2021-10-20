package v1

import (
	"bio-api/utils"
	"fmt"
	"github.com/gin-gonic/gin"
	"github.com/senseyeio/roger"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strings"
)

const PATH = "/Users/zhhnzw/workspace/mygithub/bio-api/r"

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
		// 调用r语言函数生成图表
		err := callR(file.Filename)
		if err != nil {
			resp.Message = "callR error:" + err.Error()
			c.JSON(http.StatusInternalServerError, resp)
			return
		}
		result, err := getHTMLFromResult()
		if err != nil {
			resp.Message = "getHTMLFromResult error:" + err.Error()
			c.JSON(http.StatusInternalServerError, resp)
			return
		}
		c.String(http.StatusOK, result)
		//c.HTML(http.StatusOK, "./r/pie.html", nil)
	}
}

func callR(fileName string) error {
	rClient, err := roger.NewRClient("127.0.0.1", 6312)
	if err != nil {
		log.Println("Failed to connect")
		return err
	}
	sess, err := rClient.GetSession()
	if err != nil {
		return err
	}
	//ret, err := sess.Eval("setwd('~/workspace/mygithub/bio-api/r')")
	_, err = sess.Eval(fmt.Sprintf("setwd('%s')", PATH))
	if err != nil {
		return err
	}
	//TODO:有err，但是却正常加载了这个r文件
	if v, err := sess.Eval("source('base.r')"); err != nil {
		log.Println(v, err)
	}
	// pie('./pie.txt','./pie.html','pie',NULL,2,NULL)
	genFileName := strings.Replace(fileName, "txt", "html", 1)
	call := fmt.Sprintf("pie('%s','%s','pie',NULL,2,NULL)", fileName, genFileName)
	if v, err := sess.Eval(call); err != nil {
		log.Println(v, err)
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

func getHTMLFromResult() (string, error) {
	file, err := os.Open(PATH + "/pie1.html")
	if err != nil {
		return "", err
	}
	defer file.Close()
	content, err := ioutil.ReadAll(file)
	return string(content), nil
}
