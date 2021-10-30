FROM r-base:4.1.1

# 安装系统依赖包
RUN apt-get update && apt-get -y install libfontconfig1.dev libcurl4-openssl-dev libssl-dev libxml2-dev

# 安装项目依赖包
RUN R -e 'local({r <- getOption("repos");r["CRAN"] <- "https://mirrors.tuna.tsinghua.edu.cn/CRAN/";options(repos=r)}); \
install.packages(c("RCurl", "usethis", "systemfonts", "svglite", "devtools")); \
install.packages(c("plyr", "RJSONIO", "RJSONIO", "plotrix", "reshape2", "beeswarm", "beanplot", "vcd", "circlize", "ggprism", "vioplot", "ggrepel", "UpSetR", "vegan")); \
library("devtools");install_github("taiyun/recharts")'

WORKDIR /workspace

# 从本地安装 rCharts 依赖包
COPY ramnathv-rCharts-479a4f9.tar.gz /workspace
RUN R -e 'install.packages("ramnathv-rCharts-479a4f9.tar.gz",repos=NULL);install.packages("Rserve",,"http://rforge.net/", type="source")'

# 只拷贝这一个文件，其他文件从volume挂载
COPY run_for_docker.sh /workspace

# 声明需要开放的端口
EXPOSE 6311

# 执行
ENTRYPOINT ["sh", "./run_for_docker.sh"]