package v1

import (
	"bytes"
	"encoding/json"
	"fmt"
	"github.com/gin-gonic/gin"
	"go.uber.org/zap"
	"io/ioutil"
	"net"
	"net/http"
	"net/http/httputil"
	"strconv"
	"time"
)

type uploadResp struct {
	Domain  string
	Md5     string
	Mtime   float32
	Path    string
	RetCode int
	RetMsg  string
	Scene   string
	Scenes  string
	Size    int
	Src     string
	Url     string
}

// upload
// @Summary 上传文件
// @Description 上传数据源文件以供生成统计图表
// @Tags file
// @Accept multipart/form-data
// @Param file formData file true "文件"
// @Produce json
// @Success 200 {object} utils.Resp
// @Router /v1/upload [post]
func Upload(c *gin.Context) {
	// go-fastdfs 需要配置 peers 来组成集群：https://blog.csdn.net/tmt123421/article/details/90522244
	//session := sessions.Default(c)
	//userName := session.Get("userName").(string)
	userName := "u231"
	// 代理转发
	// 修改请求体
	target := "127.0.0.1:8080"
	director := func(req *http.Request) {
		req.URL.Scheme = "http"
		req.URL.Host = target
		req.URL.Path = "/group1/upload"
		req.Host = target
		req.URL.RawQuery = fmt.Sprintf("path=%s&output=json", userName)
	}
	// 修改响应体
	modifyResponse := func(response *http.Response) error {
		body, err := ioutil.ReadAll(response.Body)
		if err != nil {
			return err
		}
		fmt.Println(string(body))
		zap.L().Info("upload success", zap.String("fastdfs response", string(body)))
		resp := uploadResp{}
		if err := json.Unmarshal(body, &resp); err != nil {
			return err
		}
		data, err := json.Marshal(map[string]interface{}{"code": resp.RetCode, "msg": resp.RetMsg})
		if err != nil {
			return err
		}
		fmt.Println(string(data))
		response.Body = ioutil.NopCloser(bytes.NewBuffer(data))
		// https://blog.csdn.net/ListFish/article/details/117387840
		// 下面这2项也修改了，客户端才能正常收到新的body
		response.ContentLength = int64(len(data))
		response.Header.Set("Content-Length", strconv.FormatInt(response.ContentLength, 10))
		return nil
	}
	// 连接池
	transport := http.Transport{
		DialContext: (&net.Dialer{
			Timeout:   time.Minute,
			KeepAlive: time.Minute,
		}).DialContext,
		MaxIdleConns:    50,
		IdleConnTimeout: time.Minute,
	}
	proxy := &httputil.ReverseProxy{
		Director:       director,
		ModifyResponse: modifyResponse,
		Transport:      &transport,
	}
	proxy.ServeHTTP(c.Writer, c.Request)
}

// download
// @Summary 下载文件
// @Description 每个用户只能下载自己上传的文件
// @Tags file
// @Param userName path string true "userName"
// @Param fileName path string true "fileName"
// @Router /group1/{userName}/{fileName} [get]
func Download(c *gin.Context) {
	// 代理转发
	target := "127.0.0.1:8080"
	director := func(req *http.Request) {
		req.URL.Scheme = "http"
		req.URL.Host = target
		req.Host = target
	}
	proxy := &httputil.ReverseProxy{Director: director}
	proxy.ServeHTTP(c.Writer, c.Request)
}
