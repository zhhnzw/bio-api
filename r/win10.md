local({r <- getOption("repos");r["CRAN"] <- "https://mirrors.tuna.tsinghua.edu.cn/CRAN/";options(repos=r)}); 

install.packages("devtools");

library("devtools");

install.packages(c("plyr", "RCurl", "RJSONIO", "RCurl", "RJSONIO", "usethis", "plotrix"));

install_github(c("ramnathv/rCharts", "yummyZhou/recharts"));

install.packages("Rserve",,"http://rforge.net/",type="source");