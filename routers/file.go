package routers

import (
	"bio-api/controller/v1"
	"github.com/gin-gonic/gin"
)

func SetFileRouter(router *gin.Engine) {
	router.GET("/group1/:userName/:fileName", v1.Download)
	router.POST("/v1/upload", v1.Upload)
}
