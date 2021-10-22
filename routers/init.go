package routers

import (
	"bio-api/controller"
	"bio-api/controller/v1"
	_ "bio-api/docs"
	"bio-api/logger"
	"bio-api/settings"
	"bio-api/utils"
	"fmt"
	"github.com/gin-contrib/cors"
	"github.com/gin-contrib/sessions"
	"github.com/gin-contrib/sessions/redis"
	"github.com/gin-gonic/gin"
	gs "github.com/swaggo/gin-swagger"
	"github.com/swaggo/gin-swagger/swaggerFiles"
	"net/http"
)

func Setup() *gin.Engine {
	if settings.Conf.Mode == gin.ReleaseMode {
		gin.SetMode(gin.ReleaseMode)
	}
	router := gin.New()
	router.Use(logger.GinLogger(), logger.GinRecovery(true))
	config := cors.DefaultConfig()
	config.AllowOrigins = settings.Conf.AllowOrigins
	//config.AllowCredentials = true
	//router.Use(cors.New(config))
	address := fmt.Sprintf("%s:%d", settings.Conf.RedisConfig.Host, settings.Conf.RedisConfig.Port)
	store, err := redis.NewStore(16, "tcp", address, settings.Conf.RedisConfig.Password, []byte("secret"))
	utils.CheckErr(err, "")
	router.Use(sessions.Sessions("session", store))
	router.GET("/swagger/*any", gs.WrapHandler(swaggerFiles.Handler))
	router.POST("/v1/login", v1.Login)
	router.POST("/v1/register", v1.CreateUser)
	router.Use(controller.SetAuthMiddleware())
	SetUserRouter(router)
	//SetFileRouter(router)
	SetChartRouter(router)
	router.StaticFS("/static", http.Dir("./r/static"))
	router.NoMethod(func(c *gin.Context) {
		c.JSON(
			http.StatusMethodNotAllowed,
			utils.Resp{Message: "方法不允许"})
	})
	router.NoRoute(func(c *gin.Context) {
		c.JSON(http.StatusNotFound,
			utils.Resp{Message: "资料不存在"})
	})
	return router
}
