# bio-api
生物信息云画图平台-api服务

访问swagger接口文档：http://localhost:8000/swagger/index.html#/

基于docker运行本项目：

① 首次运行需创建网络：`docker network create outside`

② 执行：`docker-compose up -d`

移除容器及镜像：`docker-compose down --rmi='all'`

![生物信息云平台部署图](生物信息云平台部署图.png)
![生物信息应用架构](生物信息应用架构.png)
![服务组成](服务组成.png)

当前需要启动的服务：
go-fastdfs文件存储服务
Rserve
java后端应用(下周重写成golang版本)
golang后端应用
vue前端(下周重写成react版本)

启动Rserve：
在R客户端中：
library(Rserve)
Rserve(args="--no-save", port=6312)

文件保存路径：
~/workspace/data/fastdfs/files

R生成的图片保存路径：
~/workspace/data/bio

