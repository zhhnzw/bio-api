package redis

import (
	"bio-api/settings"
	"fmt"
	"github.com/go-redis/redis"
	"go.uber.org/zap"
)

// 声明一个全局的rdb变量
var RDB *redis.Client

// Init 初始化连接
func Init(cfg *settings.RedisConfig) (err error) {
	addr := fmt.Sprintf("%s:%d",
		cfg.Host,
		cfg.Port,
	)
	RDB = redis.NewClient(&redis.Options{
		Addr:     addr,
		Password: cfg.Password, // no password set
		DB:       cfg.DB,       // use default DB
		PoolSize: cfg.PoolSize,
	})
	zap.L().Info("redis connect:" + addr)
	_, err = RDB.Ping().Result()
	return
}

func Close() {
	_ = RDB.Close()
}
