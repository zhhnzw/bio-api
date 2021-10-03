package redis

import (
	"bio-api/utils"
	"time"
)

type Cache struct {
	RedisKeyName string
	RedisKeyType string
	Result       interface{} //  缓存结果
}

func (cache *Cache) StoreStringCache() error {
	return RDB.Set(cache.RedisKeyName, utils.ToJson(cache.Result), time.Minute).Err() // 缓存1分钟
}

func (cache *Cache) GetStringCache() (string, error) {
	return RDB.Get(cache.RedisKeyName).Result()
}
