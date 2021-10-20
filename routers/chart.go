package routers

import (
	"bio-api/controller/v1"
	"github.com/gin-gonic/gin"
)

func SetChartRouter(router *gin.Engine) {
	chartRouter := router.Group("/v1/chart")
	chartRouter.POST("/pie", v1.Pie)
}
