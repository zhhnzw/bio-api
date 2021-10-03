package controller

import (
	"bio-api/dao/redis"
	"bio-api/utils"
	"github.com/gin-contrib/sessions"
	"github.com/gin-gonic/gin"
	"net/http"
)

const RedisKeyLoginUsers string = "bio_login_users" // 记录已登录的用户 redis set 类型

func SetAuthMiddleware() gin.HandlerFunc {
	return func(c *gin.Context) {
		session := sessions.Default(c)
		if userName, ok := session.Get("userName").(string); !ok {
			c.JSON(http.StatusBadRequest, utils.Resp{Message: "请重新登录", Code: "1"})
			c.Abort()
			return
		} else if findOut, err := redis.RDB.SIsMember(RedisKeyLoginUsers, userName).Result(); err != nil {
			c.JSON(http.StatusInternalServerError, utils.Resp{Message: "服务端故障, redis查找已登录用户失败", Code: "1"})
			c.Abort()
			return
		} else if findOut == false {
			c.JSON(http.StatusUnauthorized, utils.Resp{Message: "请重新登录", Code: "1"})
			c.Abort()
			return
		}
	}
}
