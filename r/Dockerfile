FROM r-base:4.1.1
#FROM debian:stable-slim

# 替换debain apt源
#ADD sources.list /etc/apt/
#RUN echo \
#    deb http://mirrors.aliyun.com/debian/ stretch main non-free contrib\
#    deb-src http://mirrors.aliyun.com/debian/ stretch main non-free contrib\
#    deb http://mirrors.aliyun.com/debian-security stretch/updates main\
#    deb-src http://mirrors.aliyun.com/debian-security stretch/updates main\
#    deb http://mirrors.aliyun.com/debian/ stretch-updates main non-free contrib\
#    deb-src http://mirrors.aliyun.com/debian/ stretch-updates main non-free contrib\
#    deb http://mirrors.aliyun.com/debian/ stretch-backports main non-free contrib\
#    deb-src http://mirrors.aliyun.com/debian/ stretch-backports main non-free contrib\
#    > /etc/apt/sources.list

#RUN echo \
#    deb http://mirrors.aliyun.com/debian/ jessie main non-free contrib\
#    deb http://mirrors.aliyun.com/debian/ jessie-proposed-updates main non-free contrib\
#    deb-src http://mirrors.aliyun.com/debian/ jessie main non-free contrib\
#    > /etc/apt/sources.list

# 安装系统依赖包
#RUN apt-get update && apt-get install -y libcurl4-gnutls-dev libcurl4-gnutls-dev

# 安装依赖包
RUN R -e 'local({r <- getOption("repos");r["CRAN"] <- "https://mirrors.tuna.tsinghua.edu.cn/CRAN/";options(repos=r)}); \
install.packages(c("plyr", "RCurl", "RJSONIO", "RCurl", "RJSONIO", "usethis", "plotrix")); \
install.packages("recharts",repos=c("http://yihui.name/xran", "http://cran.rstudio.com"));'

WORKDIR /

# 从本地安装 rCharts 依赖包
COPY ramnathv-rCharts-479a4f9.tar.gz /
RUN R -e 'setwd("/");install.packages("ramnathv-rCharts-479a4f9.tar.gz",repos=NULL);install.packages("Rserve",,"http://rforge.net/", type="source")'

COPY run_for_docker.sh /
COPY base.r /

# 声明需要开放的端口
EXPOSE 6311

# TODO: r程序不能放单独的容器里跑，因为r程序读取本地文件，也会往本地写文件，不方便操作远程文件系统，跟golang程序放到一个容器吧
# 执行
ENTRYPOINT ["sh", "/run_for_docker.sh"]