```bash
$ docker build -t bio-r:v1.0 .

# 这个 -v 只是本地运行docker测试用的
$ docker run -p 6311:6311 -v /Users/zhhnzw/workspace/mygithub/bio-api/r:/workspace --name bio-r -dit bio-r:v1.0 bash
```

### 对于recharts这个包
mac下可参考这个指令：`install.packages("recharts",repos=c("http://yihui.name/xran", "http://cran.rstudio.com"));`

### 推送本地镜像到阿里云镜像仓库
```bash
$ docker login --username=*********** registry.cn-qingdao.aliyuncs.com
$ docker tag [ImageId] registry.cn-qingdao.aliyuncs.com/zw_private/bio-r:[镜像版本号]
$ docker push registry.cn-qingdao.aliyuncs.com/zw_private/bio-r:[镜像版本号]
```

### 拉取阿里云镜像
`docker pull registry.cn-qingdao.aliyuncs.com/zw_private/bio-r:[镜像版本号]`