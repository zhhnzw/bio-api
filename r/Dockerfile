FROM r-base:4.1.1

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

# TODO: r程序docker要跟golang程序docker绑定到相同的volume
# 执行
ENTRYPOINT ["sh", "/run_for_docker.sh"]