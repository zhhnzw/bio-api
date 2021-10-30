```bash
$ docker build -t bio-r:v1.0 .

# 这个 -v 只是本地运行docker测试用的
$ docker run -p 6311:6311 -v /Users/zhhnzw/workspace/mygithub/bio-api/r:/workspace --name bio-r -dit bio-r:v1.0 bash
```

### 对于recharts这个包
mac下可参考这个指令：`install.packages("recharts",repos=c("http://yihui.name/xran", "http://cran.rstudio.com"));`