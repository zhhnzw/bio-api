# bio-api
生物信息云画图平台-api服务

本地运行本项目：`go run main.go`

访问swagger接口文档：http://localhost:8000/swagger/index.html#/

基于docker运行本项目：

① 首次运行需创建网络：`docker network create outside`

② 执行：`docker-compose up -d`

移除容器及镜像：`docker-compose down --rmi='all'`

![生物信息云平台部署图](生物信息云平台部署图.png)
![生物信息应用架构](生物信息应用架构.png)
![服务组成](服务组成.png)

需要启动的服务(配置文件`config.yaml`)：
* golang后端应用
* Rserve(在R客户端中执行:`library(Rserve);Rserve(args="--no-save", port=6311)`)
* Mysql
* Redis
* <del>go-fastdfs文件存储服务</del>

<del>go-fastdfs文件保存路径：</del>
<del>~/workspace/data/fastdfs/files</del>