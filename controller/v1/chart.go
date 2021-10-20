package v1

import (
	"bio-api/utils"
	"github.com/gin-gonic/gin"
	"github.com/senseyeio/roger"
	"io/ioutil"
	"log"
	"net/http"
	"os"
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
	} else {
		log.Println(file.Filename)
		callR()
		result := getHTMLFromResult()
		//resp.Code = "0"
		log.Println(result)
		c.String(http.StatusOK, result)
		//c.HTML(http.StatusOK, "./r/pie.html", nil)
		//c.JSON(http.StatusOK, resp)
	}
}

func callR() {
	rClient, err := roger.NewRClient("127.0.0.1", 6312)
	if err != nil {
		log.Println("Failed to connect")
		return
	}
	sess, _ := rClient.GetSession()
	//ret, err := sess.Eval("setwd('~/workspace/data/bio')")
	ret, err := sess.Eval("setwd('~/workspace/mygithub/bio-api/r')")
	log.Println(ret, err)
	v, err := sess.Eval("getwd()")
	log.Println(v)
	if err != nil {
		log.Println(err)
	}
	if v, err := sess.Eval("source('base.r')"); err != nil {
		log.Println(v, err)
	} else {
		log.Println("???")
		log.Println(v)
	}

	sess.SendCommand("pie('./pie.txt','./pie.html','pie',NULL,2,NULL)")
	//sess.SendCommand("x <- c(1,2,3,4,5,6,7,8,9,10)")
	//sess.SendCommand("y <- c(1,2,3,4,5,6,7,8,9,10)")
	//sess.SendCommand("jpeg('test1.jpg')")
	//sess.SendCommand("qqplot(x, y)")
	//sess.SendCommand("dev.off()")
	sess.Close()
}

func getHTMLFromResult() string {
	file, err := os.Open(PATH + "/pie.html")
	if err != nil {
		panic(err)
	}
	defer file.Close()
	content, err := ioutil.ReadAll(file)
	return string(content)
}
