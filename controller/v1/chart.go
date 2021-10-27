package v1

import (
	"bio-api/settings"
	"bio-api/utils"
	"fmt"
	"github.com/gin-gonic/gin"
	"github.com/senseyeio/roger"
	"go.uber.org/zap"
	"net/http"
	"strings"
)

// upload
// @Summary 上传数据文件，取得r语言生成的文件路径
// @Description 上传数据源文件以供生成统计图表
// @Tags chart
// @Accept multipart/form-data
// @Param file formData file true "文件"
// @Produce json
// @Success 200 {object} utils.Resp
// @Router /v1/chart [post]
func Chart(c *gin.Context) {
	resp := utils.Resp{Data: make(map[string]string), Code: "1"}
	file, e := c.FormFile("file")
	chartType := c.Query("type")
	if e != nil {
		resp.Message = "upload file error:" + e.Error()
		c.JSON(http.StatusOK, resp)
		return
	} else {
		// 保存文件
		if err := c.SaveUploadedFile(file, "./r/static/"+file.Filename); err != nil {
			c.JSON(http.StatusInternalServerError, resp)
			return
		}
		if chartType == "pie" {
			genFileName := strings.Replace(file.Filename, "txt", "html", 1)
			// 调用r语言函数生成图表
			err := callR(
				chartType,
				fmt.Sprintf("'./static/%s'", file.Filename), // 带单引号对r的调用就是字符串
				fmt.Sprintf("'./static/%s'", genFileName),   // 生成的文件写入到 r/static/ 下
				"'pie'",
				"NULL",
				"2", // 没有单引号，对r的调用是int
				"NULL")
			if err != nil {
				resp.Message = "callR error:" + err.Error()
				c.JSON(http.StatusInternalServerError, resp)
				return
			}
			resp.Data = "/static/" + genFileName
		} else if chartType == "beeswarm_chart" {
			genFileName := strings.Replace(file.Filename, "txt", "svg", 1)
			err := callR(
				chartType,
				fmt.Sprintf("'./static/%s'", file.Filename), // 带单引号对r的调用就是字符串
				fmt.Sprintf("'./static/%s'", genFileName),   // 生成的文件写入到 r/static/ 下
				"'蜂群图'",
				"'X轴'",
				"'Y轴'",
				"c('#FF0000','#00FF95','#3C00FF')", // 没有单引号，对r的调用是int
			)
			if err != nil {
				resp.Message = "callR error:" + err.Error()
				c.JSON(http.StatusInternalServerError, resp)
				return
			}
			resp.Data = "/static/" + genFileName
		} else if chartType == "ternaryplot_plot" {
			pointSize := c.DefaultQuery("point_size", "0.4")
			file1, err := c.FormFile("file1")
			if err != nil {
				resp.Message = "upload file1 error:" + err.Error()
				c.JSON(http.StatusOK, resp)
				return
			}
			if err := c.SaveUploadedFile(file1, "./r/static/"+file1.Filename); err != nil {
				c.JSON(http.StatusInternalServerError, resp)
				return
			}
			genFileName := strings.Replace(file.Filename, "txt", "svg", 1)
			genFileName1 := strings.Replace(file1.Filename, "txt", "svg", 1)
			err = callR(
				chartType,
				fmt.Sprintf("c('./static/%s','./static/%s')", file.Filename, file1.Filename),                              // 带单引号对r的调用就是字符串
				fmt.Sprintf("c('./static/%s','./static/%s','./static/ternaryplot_plot3.txt')", genFileName, genFileName1), // 生成的文件写入到 r/static/ 下
				"'Ternary plot'",
				pointSize,
				"FALSE",
			)
			if err != nil {
				resp.Message = "callR error:" + err.Error()
				c.JSON(http.StatusInternalServerError, resp)
				return
			}
			//TODO: 文件名临时写死，版本迭代会上传到独立的文件系统
			resp.Data = [3]string{"/static/" + genFileName, "/static/" + genFileName1, "/static/ternaryplot_plot3.txt"}
		} else if chartType == "groupedviolin" {
			genFileName := strings.Replace(file.Filename, "txt", "jpg", 1)
			genFileName1 := strings.Replace(file.Filename, "txt", "csv", 1)
			err := callR(
				chartType,
				fmt.Sprintf("'./static/%s'", file.Filename),                              // 带单引号对r的调用就是字符串
				fmt.Sprintf("c('./static/%s','./static/%s')", genFileName, genFileName1), // 生成的文件写入到 r/static/ 下
				"FALSE",
				"groupedviolin",
			)
			if err != nil {
				resp.Message = "callR error:" + err.Error()
				c.JSON(http.StatusInternalServerError, resp)
				return
			}
			resp.Data = [2]string{"/static/" + genFileName, "/static/" + genFileName1}
		} else if chartType == "violin" {
			genFileName := strings.Replace(file.Filename, "txt", "png", 1)
			err := callR(
				chartType,
				fmt.Sprintf("'./static/%s'", file.Filename), // 带单引号对r的调用就是字符串
				fmt.Sprintf("'./static/%s'", genFileName),   // 生成的文件写入到 r/static/ 下
				"FALSE",
			)
			if err != nil {
				resp.Message = "callR error:" + err.Error()
				c.JSON(http.StatusInternalServerError, resp)
				return
			}
			resp.Data = "/static/" + genFileName
		} else if chartType == "box_line" {
			genFileName := strings.Replace(file.Filename, "txt", "svg", 1)
			err := callR(
				chartType,
				fmt.Sprintf("c('./static/%s')", file.Filename), // 带单引号对r的调用就是字符串
				fmt.Sprintf("'./static/%s'", genFileName),      // 生成的文件写入到 r/static/ 下
				"c('group')",
				"c()",
				"NULL",
				"NULL",
			)
			if err != nil {
				resp.Message = "callR error:" + err.Error()
				c.JSON(http.StatusInternalServerError, resp)
				return
			}
			resp.Data = [1]string{"/static/" + genFileName}
		} else if chartType == "volcano_plot" {
			genFileName := strings.Replace(file.Filename, "txt", "svg", 1)
			err := callR(
				chartType,
				fmt.Sprintf("'./static/%s'", file.Filename), // 带单引号对r的调用就是字符串
				fmt.Sprintf("'./static/%s'", genFileName),   // 生成的文件写入到 r/static/ 下
				"'vol'",
				"1.5",
				"4",
				"30",
				"6",
				"2",
				"7",
				"0.01",
				"'#2f5688'",
				"'#BBBBBB'",
				"'#CC0000'",
			)
			if err != nil {
				resp.Message = "callR error:" + err.Error()
				c.JSON(http.StatusInternalServerError, resp)
				return
			}
			resp.Data = [1]string{"/static/" + genFileName}
		} else if chartType == "maplot" {
			genFileName := strings.Replace(file.Filename, "txt", "jpg", 1)
			err := callR(
				chartType,
				fmt.Sprintf("'./static/%s'", file.Filename), // 带单引号对r的调用就是字符串
				fmt.Sprintf("'./static/%s'", genFileName),   // 生成的文件写入到 r/static/ 下
				"'MA_plot'",
				"4",
				"5",
				"7",
				"1",
				"'A'",
				"'M'",
				"'red'",
				"'gray'",
				"'green'",
			)
			if err != nil {
				resp.Message = "callR error:" + err.Error()
				c.JSON(http.StatusInternalServerError, resp)
				return
			}
			resp.Data = [1]string{"/static/" + genFileName}
			//upset_chart("./sample/BaseFunction/upset_chart/seniorvenn.txt", #输入文件路径
			//"./upsetchart.svg", #图片输出路径
			//8,8, #宽✖高
			//"Set Size", #集合(横矩形)名
			//"Intersection Size", #纵矩形名
			//order_by = c("freq") #c("freq"), c("degree"), c("freq", "degree")
			//)
		} else if chartType == "upset_chart" {
			genFileName := strings.Replace(file.Filename, "txt", "svg", 1)
			err := callR(
				chartType,
				fmt.Sprintf("'./static/%s'", file.Filename), // 带单引号对r的调用就是字符串
				fmt.Sprintf("'./static/%s'", genFileName),   // 生成的文件写入到 r/static/ 下
				"8",
				"8",
				"'Set Size'",
				"'Intersection Size'",
				"order_by=c('freq')",
			)
			if err != nil {
				resp.Message = "callR error:" + err.Error()
				c.JSON(http.StatusInternalServerError, resp)
				return
			}
			resp.Data = [1]string{"/static/" + genFileName}
		} else {
			resp.Message = "not support that type: " + chartType
			c.JSON(http.StatusOK, resp)
			return
		}
		resp.Code = "0"
		c.JSON(http.StatusOK, resp)
	}
}

// f: r语言函数
// params: 传入r语言函数的参数
func callR(f string, params ...string) error {
	rClient, err := roger.NewRClient(settings.Conf.BioChartConfig.Host, settings.Conf.BioChartConfig.Port)
	if err != nil {
		zap.L().Error(
			"Failed to connect",
			zap.Error(err),
			zap.String("address", fmt.Sprintf("%s:%d", settings.Conf.BioChartConfig.Host, settings.Conf.BioChartConfig.Port)))
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
