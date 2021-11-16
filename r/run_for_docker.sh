R -e "library(Rserve);Rserve(args='--no-save', port=6311)"
# R CMD Rserve --RS-enable-remote # Rserve允许远程连接只能这样启动，但是这就在后台启动了，所以后面还要加个避免容器退出的操作
while :; do
  sleep 300
done