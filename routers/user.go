package routers

import (
	"bio-api/controller/v1"
	"github.com/gin-gonic/gin"
)

func SetUserRouter(router *gin.Engine) {
	router.POST("/v1/logout", v1.Logout)
	router.POST("/v1/alterPwd", v1.AlterPwd)
	//userRouter := router.Group("/v1/user")
	//userRouter.GET("", v1.GetUsers)
	//userRouter.POST("", v1.CreateUser)
}
