#原函数注释
#' 使用R基本绘图函数绘制y轴不连续的柱形图
#'
#' 绘制y轴不连续的柱形图，具有误差线添加功能。断点位置通过btm和top参数设置，如果不设置，函数可自动计算合适的断点位置。
#'  gap.barplot function
#'  df 长格式的data.frame，即数据框中每一列为一组绘图数据。
#'  y.cols 用做柱形图y值的数据列（序号或名称），一列为一组。
#'  sd.cols 与y值列顺序对应的误差值的数据列（序号或名称）。
#'  btm 低位断点。如果btm和top均不设置，程序将自动计算和设置断点位置。
#'  top 高位断点。
#'  min.range 自动计算断点的阈值：最大值与最小值的最小比值
#'  max.fold 自动计算断点时最大值与下方数据最大值的最大倍数比
#'  ratio 断裂后上部与下部y轴长度的比例。
#'  gap.width y轴断裂位置的相对物理宽度（非坐标轴实际刻度）
#'  brk.type 断点类型，可设为normal或zigzag
#'  brk.bg 断点处的背景颜色
#'  brk.srt 断点标记线旋转角度
#'  brk.size 断点标记线的大小（长度）
#'  brk.col 断点标记线的颜色
#'  brk.lwd 断点标记线的线宽
#'  cex.error 误差线相对长度，默认为1
#'  ... 其他传递给R基本绘图函数barplot的参数
#'  返回barplot的原始返回值，即柱形图的x坐标

#断轴柱状图--y轴不连续柱状图
#file_path1     输入文件路径1——数据文件
#file_path2     输入文件路径2——分组文件
#out_path       图片输出路径
#broken_color   柱形图颜色
#y_cols         用做柱形图y值的数据列（序号或名称），一列为一组
#sd.cols        与y值列顺序对应的误差值的数据列（序号或名称）
#btm            下断点值
#top            上断电值
#main_title     主标题
#x_lable        x轴标题
#y_lable        y轴标题
#min.range      自动计算断点的阈值：最大值与最小值的最小比值
#max.fold       自动计算断点时最大值与下方数据最大值的最大倍数比
#ratio          断裂后上部与下部y轴长度的比例
#gap.width      y轴断裂位置的相对物理宽度（非坐标轴实际刻度）
#brk.type       断点类型，可设为normal或zigzag
#brk.bg         断点处的背景颜色
#brk.srt        断点标记线旋转角度
#brk.size       断点标记线的大小（长度）
#brk.col        断点标记线的颜色
#brk.lwd        断点标记线的线宽
#cex.error      误差线相对长度，默认为1
#...            其他传递给R基本绘图函数barplot的参数

broken_chart <- function(file_path,
                         out_path,
                         broken_color = NULL,
                         btm = NULL,
                         top = NULL,
                         main_title = NULL,
                         x_lable = NULL,
                         y_lable = NULL,
                         ratio = 1,
                         y_cols = NULL,
                         sd.cols = NULL,
                         min.range = 10, max.fold = 5,
                         gap.width = 1, brk.type = "normal",
                         brk.bg = "white", brk.srt = 135, brk.size = 1,
                         brk.col = "black", brk.lwd = 1,
                         cex.error = 1, ...) {
  library(svglite)

  file_data1 <- read.table(file_path[1], header = TRUE)
  file_data2 <- read.table(file_path[2], header = FALSE)

  rownum = nrow(file_data1)
  colnum = ncol(file_data1)

  #数据处理——开始
  my_data = c()

  for(i in 1:rownum) {
    for(j in 1:((colnum-1)/3)){
      my_data = c(my_data, (file_data1[i,(j-1)*3+2]+file_data1[i,(j-1)*3+3]+file_data1[i,(j-1)*3+4])/3)#求取平均值并存储至my_data
    }
  }

  df <- array(my_data, dim = c(rownum, (colnum - 1) / 3))#创建二维数组

  #更改数据位置
  for(i in 1:rownum){
    for(j in 1:((colnum - 1) / 3)){
      df[i,j] = my_data[(i-1) * 3 + j]
    }
  }
  #数据处理——结束

  if(is.null(broken_color)){
    broken_color = rainbow(ncol(df))
  }

  if(is.null(y_cols)){
    y.cols = 1:ncol(df)
  }

  svglite(out_path) #画图开始

  if (missing(df))
    stop("No data provided.")
  if (is.numeric(y.cols))
    ycol <- y.cols else ycol <- colnames(df) == y.cols
  if (!is.null(sd.cols))
    if (is.numeric(sd.cols))
      scol <- sd.cols else scol <- colnames(df) == sd.cols
  ## Arrange data
  opts <- options()
  options(warn = -1)
  y <- t(df[, ycol])
  colnames(y) <- NULL
  if (missing(sd.cols))
    sdx <- 0 else sdx <- t(df[, scol])
  sdu <- y + sdx
  sdd <- y - sdx
  ylim <- c(0, max(sdu) * 1.05)
  ## 如果没有设置btm或top，自动计算
  if (is.null(btm) | is.null(top)) {
    #自动计算断点位置
    datax <- sort(as.vector(sdu))
    flags <- FALSE
    abtm <- atop <- NULL
    if (max(datax)/min(datax) < min.range)
      autox <- list(flag = flags, btm = abtm, top = atop)
    m <- max(datax)
    abtm <- datax[2]
    i <- 3
    while (m/datax[i] > max.fold) {
      abtm <- datax[i]
      flags <- TRUE
      i <- i + 1
    }
    if (flags) {
      abtm <- abtm + 0.05 * abtm
      ax <- 2
      atop <- datax[i] * (ax - 1)/ax
      while (atop < abtm) {
        ax <- ax + 1
        atop <- datax[i] * (ax - 1)/ax
        if (ax > 100) {
          flags <- FALSE
          break
        }
      }
    }
    autox <- list(flag = flags, btm = abtm, top = atop) #
    if (autox$flag) {
      btm <- autox$btm
      top <- autox$top
    } else {
      xx <- barplot(y, beside = TRUE, ylim = ylim, ...)
      if (!missing(sd.cols)){
        #绘制误差线
        horiz = FALSE
        if (!horiz) {
          arrows(xx, y, y1 = y - (sdu - y), length = 0.1 * cex.error, angle = 90, ...)
          arrows(xx, y, y1 = y + (sdu - y), length = 0.1 * cex.error, angle = 90, ...)
        } else {
          arrows(y, xx, x1 = y - (sdu - y), length = 0.1 * cex.error, angle = 90, ...)
          arrows(y, xx, x1 = y + (sdu - y), length = 0.1 * cex.error, angle = 90, ...)
        }
      }
      box()
      return(invisible(xx))
    }
  }
  ## Set up virtual y limits
  halflen <- btm - ylim[1]
  xlen <- halflen * 0.1 * gap.width
  v_tps1 <- btm + xlen  # virtual top positions
  v_tps2 <- v_tps1 + halflen * ratio
  v_ylim <- c(ylim[1], v_tps2)
  r_tps1 <- top  # real top positions
  r_tps2 <- ylim[2]
  ## Rescale data
  lmx <- summary(lm(c(v_tps1, v_tps2) ~ c(r_tps1, r_tps2)))
  lmx <- lmx$coefficients
  sel1 <- y > top
  sel2 <- y >= btm & y <= top
  y[sel1] <- y[sel1] * lmx[2] + lmx[1]
  y[sel2] <- btm + xlen/2
  sel1 <- sdd > top
  sel2 <- sdd >= btm & sdd <= top
  sdd[sel1] <- sdd[sel1] * lmx[2] + lmx[1]
  sdd[sel2] <- btm + xlen/2
  sel1 <- sdu > top
  sel2 <- sdu >= btm & sdu <= top
  sdu[sel1] <- sdu[sel1] * lmx[2] + lmx[1]
  sdu[sel2] <- btm + xlen/2
  ## bar plot
  xx <- barplot(y, beside = TRUE, ylim = v_ylim, axes = FALSE, names.arg = NULL,
                col = broken_color, main = main_title, xlab = x_lable, ylab = y_lable,
                ...)
  ## error bars
  if (!missing(sd.cols)){
    #绘制误差线
    horiz = FALSE
    if (!horiz) {
      arrows(xx, y, y1 = y - (sdu - y), length = 0.1 * cex.error, angle = 90, ...)
      arrows(xx, y, y1 = y + (sdu - y), length = 0.1 * cex.error, angle = 90, ...)
    } else {
      arrows(y, xx, x1 = y - (sdu - y), length = 0.1 * cex.error, angle = 90, ...)
      arrows(y, xx, x1 = y + (sdu - y), length = 0.1 * cex.error, angle = 90, ...)
    }
  }
  ## Real ticks and labels
  brks1 <- pretty(seq(0, btm, length = 10), n = 4)
  brks1 <- brks1[brks1 >= 0 & brks1 < btm]
  brks2 <- pretty(seq(top, r_tps2, length = 10), n = 4)
  brks2 <- brks2[brks2 > top & brks2 <= r_tps2]
  labx <- c(brks1, brks2)
  ## Virtual ticks
  brks <- c(brks1, brks2 * lmx[2] + lmx[1])
  axis(2, at = brks, labels = labx)
  box()
  ## break marks
  pos <- par("usr")
  xyratio <- (pos[2] - pos[1])/(pos[4] - pos[3])
  xlen <- (pos[2] - pos[1])/50 * brk.size
  px1 <- pos[1] - xlen
  px2 <- pos[1] + xlen
  px3 <- pos[2] - xlen
  px4 <- pos[2] + xlen
  py1 <- btm
  py2 <- v_tps1
  rect(px1, py1, px4, py2, col = brk.bg, xpd = TRUE, border = brk.bg)
  x1 <- c(px1, px1, px3, px3)
  x2 <- c(px2, px2, px4, px4)
  y1 <- c(py1, py2, py1, py2)
  y2 <- c(py1, py2, py1, py2)
  xx1 <- x1 - xlen * cos(brk.srt * pi/90)
  yy1 <- y1 + xlen * sin(brk.srt * pi/90)/xyratio
  xx2 <- x2 + xlen * cos(brk.srt * pi/90)
  yy2 <- y2 - xlen * sin(brk.srt * pi/90)/xyratio
  px <- list(x1 = xx1, x2 = xx2, y1 = yy1, y2 = yy2)
  if (brk.type == "zigzag") {
    x1 <- c(x1, px1, px3)
    x2 <- c(x2, px2, px4)
    if (brk.srt > 90) {
      y1 <- c(y1, py2, py2)
      y2 <- c(y2, py1, py1)
    } else {
      y1 <- c(y1, py1, py1)
      y2 <- c(y2, py2, py2)
    }
  }
  if (brk.type == "zigzag") {
    px$x1 <- c(pos[1], px2, px1, pos[2], px4, px3)
    px$x2 <- c(px2, px1, pos[1], px4, px3, pos[2])
    mm <- (v_tps1 - btm)/3
    px$y1 <- rep(c(v_tps1, v_tps1 - mm, v_tps1 - 2 * mm), 2)
    px$y2 <- rep(c(v_tps1 - mm, v_tps1 - 2 * mm, btm), 2)
  }
  par(xpd = TRUE)
  segments(px$x1, px$y1, px$x2, px$y2, lty = 1, col = brk.col, lwd = brk.lwd)
  options(opts)
  par(xpd = FALSE)
  invisible(xx)


  graphics.off() #画图结束
}


# #使用示例
# broken_chart("C:/Users/w1792/Desktop/test/broken_columns/broken_columns_genus.txt",
#              "C:/Users/w1792/Desktop/test/broken_columns/broken_columns_group.txt",
#              "C:/Users/w1792/Desktop/broken.svg",
#              btm = 1,
#              top = 3,
#              main_title = "Broken Columns",
#              x_lable = "Group",
#              y_lable = "Data"
# )
#



# 3. beeswarm_chart       蜂群图
beeswarm_chart = function(file_path, out_path, main_name = "", xlab_name = "", ylab_name = "", bee_color){
  library(reshape2)
  library(beeswarm)

  file_data <- read.table(file_path, header = TRUE) #读取文件
  file_data = melt(data = file_data, id = names(file_data)[1]) #修改数据框

  x_points = file_data[,"variable"]
  y_points = file_data[,"value"]

  if(is.null(bee_color)){
    bee_color = c("#ff6666", "#00ff99", "#3366ff")
  }

  svg(out_path)#图片输出路径

  beeswarm(y_points ~ x_points, #设定x轴y轴数据，并用"~"连接
           file_data, #数据框
           spacing = 0.75,
           vertical = TRUE,
           method = 'center',
           pch = 16:18,
           col = bee_color,
           main = main_name,
           xlab = xlab_name,
           ylab = ylab_name
  )

  bxplot(y_points ~ x_points,
         file_data,
         add = TRUE)

  legend("topright",
         legend = unique(x_points), #unique识别唯一值
         pch = 16:18,
         col = bee_color)

  dev.off()#画图结束

}
# beeswarm_chart('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210503003846_fengqun.txt','E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210503003846_fengqun.svg','蜂群图','X轴','Y轴',c("#FF0000","#00FF95","#3C00FF"))

#file_path          输入文件路径
#out_path           输出图片路径
#p_width            图片宽度——inches
#p_height           图片高度——inches
#set_lable          集合（横矩形）名
#y_lable            纵矩形名
#point_color        点颜色
#y_color            纵矩形颜色
#set_color          集合(横矩形)颜色
#ydata_show         y轴数据是否显示——"yes" or "no"
#order_by           矩阵中交集排序方式，"freq"从大到小排序，其他选项degree, 先freq后degree
#point_size         点大小
#line_size          线宽度
#upset_order        按使用sets参数输入的顺序保留集合。函数中默认值为FALSE，按大小对集合进行排序，此处为TRUE
#intersection_num   要绘制的交点数。如果设置为NA，将绘制所有交点

upset_chart <- function(file_path,
                        out_path,
                        p_width = 8,
                        p_height = 8,
                        set_lable = "Set Size",
                        y_lable = "Intersection Size",
                        point_color = c("black"),
                        y_color = c("black"),
                        set_color = NULL,
                        ydata_show = "yes",
                        order_by = c("freq"),
                        point_size = 3,
                        line_size = 1,
                        upset_order = TRUE,
                        intersection_num = NA
){
  library(UpSetR)
  library(svglite)

  file_data <- read.table(file_path, header = TRUE, sep = "\t")

  col_name <- colnames(file_data)

  colnum = ncol(file_data)
  rownum = nrow(file_data)

  datalist <- list()

  for(i in 1:colnum){
    t = c()
    for(j in 1:rownum){
      if(is.na(file_data[j,i])){
        break
      }
      t = c(t,file_data[j,i])
    }

    datalist_t = list(t)
    datalist = c(datalist, datalist_t)
  }

  names(datalist) <- col_name #修改列表名

  if(is.null(set_color)){
    set_color = rainbow(colnum) #设置集合默认颜色
  }

  p <- upset(fromList(datalist),
             nsets = length(datalist),
             sets = col_name,
             order.by = order_by, #矩阵中交集排序方式，"freq"从大到小排序，其他选项degree, 先freq后degree
             point.size = point_size, #点大小
             line.size = line_size, #线宽度
             text.scale = c(1.5, 1.2, 1.2, 1, 1.5, 1), # ytitle, ylabel, xtitle, xlabel, sets, number
             matrix.color = point_color, #点颜色
             main.bar.color = y_color, #y轴颜色
             mainbar.y.label = y_lable, #y轴标题
             sets.bar.color = set_color, #x轴集合颜色
             nintersects = intersection_num, #要绘制的交点数。如果设置为NA，将绘制所有交点
             keep.order = upset_order, #按使用sets参数输入的顺序保留集合。默认值为FALSE，按大小对集合进行排序
             sets.x.label = set_lable, #x轴标题
             show.numbers = ydata_show, #y轴数据是否显示
             group.by = "degree" #数据分组依据("degree" or "sets")
  )

  #保存图片
  svglite(out_path, width = p_width, height = p_height)
  print(p) #该图片只能通过print方式写至创建的空白画布中
  graphics.off() #upset画图开启的好像不是一条线，所以需要使用graphics.off()函数，全部关闭
}


# upset_chart("seniorvenn.txt", #输入文件路径
#             "C:/Users/w1792/Desktop/upsetchart.svg", #图片输出路径
#             8,8, #宽✖高
#             "Set Size", #集合(横矩形)名
#             "Intersection Size", #纵矩形名
#             order_by = c("freq") #c("freq"), c("degree"), c("freq", "degree")
# )


# 5. podfigure            豆荚图
# podfigure(c('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210503160531_doujia1.txt',
#             'E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210503160531_doujia2.txt'),
#           'E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210503160531_doujia1.svg',
#           c("#FF0000"),c("#15FF00"),'Splitviolin','X','Y')
podfigure = function(file_path,out_path, bean_color1,  bean_color2,
                       main_name = "", xlab_name = "", ylab_name = ""){
  library(beanplot)
  library(reshape2)


  file_data1 <- read.table(file_path[1], header = TRUE)
  file_data2 <- read.table(file_path[2], header = TRUE)

  colnum = ncol(file_data1)

  file_data1 = melt(file_data1, id = names(file_data1)[1])#修改数据框
  file_data2 = melt(file_data2, id = names(file_data2)[1])#修改数据框

  #判断颜色是否为空
  if(is.null(bean_color1)){
    bean_color1 = "#F99794"
  }

  if(is.null(bean_color2)){
    bean_color2 = "#8AB7FC"
  }

  svg(out_path)#图片输出路径

  beanplot(file_data1$value ~ file_data1$variable,
           data =file_data1,
           side = "first",#first,second,both,no
           boxwex = 0.8,#每列宽度
           at = 1:5 - 0.4,#每列位置
           ll = 0,#黑条长度
           col = bean_color1,#颜色
           border = NA,
           cutmin =0,
           xlim = c(0,colnum-1),#x轴边界
           cex.axis = 0.8,#横轴文字大小
           main = main_name,
           xlab = xlab_name,
           ylab = ylab_name)

  beanplot(file_data2$value ~ file_data2$variable,
           data =file_data2,
           side = "second",
           boxwex = 0.8,
           at = 1:5 - 0.4,
           ll = 0,
           col = bean_color2,
           border = NA,
           cutmin =0,
           cex.axis = 0.8,#横轴文字大小
           add = TRUE)

  legend("topright", #图例位置
         legend = c("Group1", "Group2"), #unique识别唯一值
         pch = 16,
         col = c(bean_color1, bean_color2))

  dev.off()#画图结束

}

# 7. ternaryplot          三元图

#三元图
#file_path1   输入文件1——数据文件
#file_path2   输入文件2——分组文件
#out_path1    图片输出路径1——png格式
#out_path2    图片输出路径2——png格式
#out_path3    文件输出路径——txt格式
#main_name    图片主标题
#points_size  点大小--默认为空，即点大小都一样，输入建议数字大小为0.3~0.6
#whether_legend 是否画图例——TRUE 画图例，FALSE 不画

ternaryplot_plot <- function(file_path, out_path, main_name = "",points_size = NULL, whether_legend = TRUE){
  library(grid)
  library(vcd)
  library(svglite)

  file_data1 <- read.table(file_path[1], header = TRUE)
  file_data2 <- read.table(file_path[2], header = TRUE)

  colname1 <- colnames(file_data1)
  colname2 <- colnames(file_data2)
  colname <-c(colname1[1], colname2[1], colname2[2], colname2[3], colname1[11])

  rownum = nrow(file_data1)#行数

  arr <- array(1:(rownum*5), c(rownum,5))#创建rownum行5列数组
  my_data <- as.data.frame(arr)#根据数据创建数据框
  names(my_data) <- colname#修改数据框列名

  for(i in 1:rownum){
    my_data[i,1] = file_data1[i, 1]
    for(j in 1:3){
      my_data[i,1+j] = (file_data1[i,(j-1)*3+2] + file_data1[i,(j-1)*3+3] + file_data1[i,(j-1)*3+4])/3
    }
    my_data[i,5] = file_data1[i,11]
  }
  #导出数据，txt格式
  write.table(my_data, #需要导出的数据
              out_path[3], #文件路径
              sep = "\t", #分隔符，默认为空格
              row.names = FALSE, #是否输出行名，默认为TRUE
              col.names = TRUE, #是否输出列名，默认为TRUE
              quote = FALSE)#字符串是否使用引号表示，默认为TRUE
  #输出图片1——富集三元图
  #点的大小
  #my_data$size <- (apply(my_data[2:4], 1, mean)) ^ points_size
  #点的颜色——富集三元图
  my_data[which(my_data[,5] == colname2[1]), "color"] <- "red"
  my_data[which(my_data[,5] == colname2[2]), "color"] <- "blue"
  my_data[which(my_data[,5] == colname2[3]), "color"] <- "green3"
  my_data[which(my_data[,5] == "none"), "color"] <- "gray"
  if(is.null(points_size)){
    #输出图片1——富集三元图
    svglite(out_path[1])
    ternaryplot(my_data[2:4],
                scale = NULL,
                col = my_data$color, #点的颜色
                prop_size = FALSE,
                #cex = my_data$size, #点的大小
                main = main_name)
    if(whether_legend){
      grid_legend(x = "topright",
                  pch = 16, #点样式
                  col = c('red', 'blue', 'green3'),
                  label = colname2,
                  title = "Enrich",
                  size = 1, #点大小
                  frame = FALSE)
    }
    graphics.off()
    #输出图片2——分布三元图
    svglite(out_path[2])
    ternaryplot(my_data[2:4],
                scale = NULL,
                col = rainbow(rownum), #点的颜色
                prop_size = FALSE,
                #cex = my_data$size, #点的大小
                main = main_name)
    #画图例
    if(whether_legend){
      grid_legend(x = "topright",
                  #y = 0.7,
                  #xpd = TRUE,
                  pch = 16, #点样式
                  col = rainbow(rownum),
                  label = my_data[,1],
                  title = "Group",
                  size = 1, #点大小
                  frame = FALSE)
    }
    graphics.off()
  }
  else{
    my_data$size <- (apply(my_data[2:4], 1, mean)) ^ points_size#自定义点大小
    #输出图片1——富集三元图
    svglite(out_path[1])
    ternaryplot(my_data[2:4],
                scale = NULL,
                col = my_data$color, #点的颜色
                prop_size = FALSE,
                cex = my_data$size, #点的大小
                main = main_name)
    if(whether_legend){
      grid_legend(x = "topright",
                  pch = 16, #点样式
                  col = c('red', 'blue', 'green3'),
                  label = colname2,
                  title = "Enrich",
                  size = 1, #点大小
                  frame = FALSE)
    }
    graphics.off()
    #输出图片2——分布三元图
    svglite(out_path[2])
    ternaryplot(my_data[2:4],
                scale = NULL,
                col = rainbow(rownum), #点的颜色
                prop_size = FALSE,
                cex = my_data$size, #点的大小
                main = main_name)
    #画图例
    if(whether_legend){
      grid_legend(x = "topright",
                  #y = 0.7,
                  #xpd = TRUE,
                  pch = 16, #点样式
                  col = rainbow(rownum),
                  label = my_data[,1],
                  title = "Group",
                  size = 1, #点大小
                  frame = FALSE)
    }
    graphics.off()
  }
}
# ternaryplot_plot(c('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210515164222_ternary_input.txt','E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210515164222_ternary_group.txt'),c('E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210515164222_ternary(1).svg','E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210515164222_ternary(2).svg','E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210515164222_ternary.txt'),'Ternary plot',0.4,FALSE)


# 9. circos     circos物种关系图
# circos(c('E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630192914_taxonomy.txt','E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630192914_group.txt','E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630192914_otu_table.txt'),'E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210630192914_taxonomy.pdf')
circos =function(data_filepath,out_picpath,width_n = 20, height_n = 8){
  library(circlize)
  library(dplyr)
  library(tibble)
  library(reshape2)
  library(circlize)
  library(ComplexHeatmap) #可用此包添加图例
  library(grid) #可用此包调整画板
  pdf(out_picpath, width =width_n, height = height_n)
  circle_size = unit(1, 'snpc')

  #读取 taxonomy.txt 的内容，获取“OTU/分类”排序，OTU 名称
  taxonomy <- read.delim(data_filepath[1], sep = '\t', stringsAsFactors = FALSE)
  tax_phylum <- unique(taxonomy[,2])
  taxonomy[,2] <- factor(taxonomy[,2], levels = tax_phylum)
  all_otu <- taxonomy[,1]
  taxonomy[,1] <- factor(taxonomy[,1], levels = all_otu)

  #读取 group.txt 的内容，获取“样本/分组”排序，样本名称
  group <- read.delim(data_filepath[2], sep = '\t', stringsAsFactors = FALSE)
  all_group <- unique(group[,2])
  group[,2] <- factor(group[,2], levels = all_group)
  all_sample <- group[,1]

  #读取 otu_table.txt，排序 OTU 和样本
  otu_table <- read.delim(data_filepath[3], sep = '\t')
  colnames(otu_table)[1] <- 'OTU_ID'
  otu_table <- merge(taxonomy, otu_table, by = 'OTU_ID')
  otu_table <- otu_table[order(taxonomy[,2], otu_table[,1]), ]
  rownames(otu_table) <- otu_table[,1]
  otu_table <- otu_table[all_sample]

  ##生成作图数据
  #circlize 外圈属性数据
  all_ID <- c(all_otu, all_sample)
  accum_otu <- rowSums(otu_table)
  accum_sample <- colSums(otu_table)
  all_ID_xlim <- cbind(rep(0, length(all_ID)),data.frame(c(accum_otu, accum_sample)))

  #circlize 内圈连线数据
  otu_table$otu_ID <- all_otu
  plot_data <- melt(otu_table, id = 'otu_ID') #此处使用了reshape2包中的melt()命令
  colnames(plot_data)[2] <- 'sample_ID'
  plot_data$otu_ID <- factor(plot_data$otu_ID, levels = all_otu)
  plot_data$sample_ID <- factor(plot_data$sample_ID, levels = all_sample)
  plot_data <- plot_data[order(plot_data$otu_ID, plot_data$sample_ID), ]
  plot_data <- plot_data[c(2, 1, 3, 3)]

  #然后作图
  #整体布局
  gap_size <- c(rep(3, length(all_otu) - 1), 6, rep(3, length(all_sample) - 1), 6)
  circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 270, gap.degree = gap_size)
  circos.initialize(factors = factor(all_ID, levels = all_ID), xlim = all_ID_xlim)


  #颜色设置
  color_otu <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5',
                 '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', '#377EB8', '#4DAF4A', '#984EA3', '#FF7F00',
                 '#FFFF33', '#A65628', '#F781BF', '#66C2A5',"#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                 '#FFB9B9','#FFE7E7','#E7FFE7','#B9FFB9','#00FF00')
  color_sample <- c('#6181BD', '#F34800', '#64A10E', '#FF00FF', '#c7475b', '#049a0b','#A25C00','#D02E00','#DFFFFFFF','#9FFFFFFF')
  color_phylum <- c('#BEAED4', '#FDC086', '#FFFF99', '#386CB0', '#F0027F','#FFDFFFFF','FF80FFFF','#FFFF00FF','#4DFF00FF','#00E5FFFF','#FFE0B2FF')
  color_group <- c('#4253ff', '#ff4308','#999999',"#56B4E9", "#009E73")

  names(color_otu) <- all_otu
  names(color_sample) <- all_sample

  #整体画板布局
  gap_size <- c(rep(3, length(all_otu) - 1), 6, rep(3, length(all_sample) - 1), 6)
  circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 270, gap.degree = gap_size)
  circos.initialize(factors = factor(all_ID, levels = all_ID), xlim = all_ID_xlim)
  #绘制 OTU 分类、样本分组区块（第一圈）
  circos.trackPlotRegion(
    ylim = c(0, 1), track.height = 0.03, bg.border = NA,
    panel.fun = function(x, y) {
      sector.index = get.cell.meta.data('sector.index')
      xlim = get.cell.meta.data('xlim')
      ylim = get.cell.meta.data('ylim')
    } )

  for (i in 1:length(tax_phylum)) {
    tax_OTU <- {subset(taxonomy, phylum == tax_phylum[i])}[,1]
    highlight.sector(tax_OTU, track.index = 1, col = color_phylum[i], text = tax_phylum[i], cex = 0.5, text.col = 'black', niceFacing = FALSE)
  }

  for (i in 1:length(all_group)) {
    group_sample <- {subset(group, group_ID == all_group[i])}[,1]
    highlight.sector(group_sample, track.index = 1, col = color_group[i], text = all_group[i], cex = 0.7, text.col = 'black', niceFacing = FALSE)
  }
  #添加 OTU 百分比注释（第二圈）
  circos.trackPlotRegion(
    ylim = c(0, 1), track.height = 0.05, bg.border = NA,
    panel.fun = function(x, y) {
      sector.index = get.cell.meta.data('sector.index')
      xlim = get.cell.meta.data('xlim')
      ylim = get.cell.meta.data('ylim')
    } )

  circos.track(
    track.index = 2, bg.border = NA,
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data('xlim')
      ylim = get.cell.meta.data('ylim')
      sector.name = get.cell.meta.data('sector.index')
      xplot = get.cell.meta.data('xplot')

      by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.25, 1)
      for (p in c(0, seq(by, 1, by = by))) circos.text(p*(xlim[2] - xlim[1]) + xlim[1], mean(ylim) + 0.4, paste0(p*100, '%'), cex = 0.4, adj = c(0.5, 0), niceFacing = FALSE)

      circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3)
    } )
  #绘制 OTU、样本主区块（第三圈）
  circos.trackPlotRegion(
    ylim = c(0, 1), track.height = 0.03, bg.col = c(color_otu, color_sample), bg.border = NA, track.margin = c(0, 0.01),
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data('xlim')
      sector.name = get.cell.meta.data('sector.index')
      circos.axis(h = 'top', labels.cex = 0.4, major.tick.percentage = 0.4, labels.niceFacing = FALSE)
      circos.text(mean(xlim), 0.2, sector.name, cex = 0.4, niceFacing = FALSE, adj = c(0.5, 0))
    } )

  #绘制 OTU、样本副区块（第四圈）
  circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.03, track.margin = c(0, 0.01))
  #绘制 OTU-样本关联连线（最内圈）
  for (i in seq_len(nrow(plot_data))) {
    circos.link(
      plot_data[i,2], c(accum_otu[plot_data[i,2]], accum_otu[plot_data[i,2]] - plot_data[i,4]),
      plot_data[i,1], c(accum_sample[plot_data[i,1]], accum_sample[plot_data[i,1]] - plot_data[i,3]),
      col = paste0(color_otu[plot_data[i,2]], '70'), border = NA )

    circos.rect(accum_otu[plot_data[i,2]], 0, accum_otu[plot_data[i,2]] - plot_data[i,4], 1, sector.index = plot_data[i,2], col = color_sample[plot_data[i,1]], border = NA)
    circos.rect(accum_sample[plot_data[i,1]], 0, accum_sample[plot_data[i,1]] - plot_data[i,3], 1, sector.index = plot_data[i,1], col = color_otu[plot_data[i,2]], border = NA)

    accum_otu[plot_data[i,2]] = accum_otu[plot_data[i,2]] - plot_data[i,4]
    accum_sample[plot_data[i,1]] = accum_sample[plot_data[i,1]] - plot_data[i,3]
  }

  #添加图例
  otu_legend <- Legend(
    at = all_otu, labels = taxonomy$detail, labels_gp = gpar(fontsize = 8),
    grid_height = unit(0.5, 'cm'), grid_width = unit(0.5, 'cm'), type = 'points', pch = NA, background = color_otu)

  pushViewport(viewport(x = 0.85, y = 0.5))
  grid.draw(otu_legend)
  upViewport()

  #推荐作图完成后，清除目前的 circlize 样式，以免影响继续作图
  circos.clear()
  dev.off()
}
# circos(c('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210515132700_taxonomy.txt',
#          'E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210515132700_group.txt',
#          'E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210515132700_otu_table.txt'),
#        'E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210515132700_taxonomy.pdf')

# 11. groupedviolin 分组小提琴图
#数据.csv只有一个输入，请注意最后一列必须为分组的属性值
groupedviolin =function(data_filepath,out_picpath,first_column_is_num=F,title_n=NULL,color="lightblue",border_c="royalblue",colMed_c='white',rectCol_n='lightblue',lineCol_n="violetred"){
  library(ggplot2)
  library(ggprism)
  data1<- read.table(data_filepath,header=T)
  data2<- read.table(data_filepath,header=F)
  len_dim=dim(data1)[2]
  len_row=dim(data1)[1]
  if(first_column_is_num){
    group=NULL
    for(i in 2:len_dim-1){group<-c(group,rep(data2[1,i],len_row))}
    value=NULL
    for(j in 2:len_dim-1){value<-c(value,data1[,j])}
    type<-data1[,len_dim]
    df<-data.frame(group,value,type)
    jpeg(file = out_picpath[1])
    print(ggplot(data = df,aes(x=group,y=value,fill=type))+
            geom_violin(position = position_dodge(0.6))+
            geom_boxplot(width=0.3,position = position_dodge(0.9)))
    dev.off()
    #write.csv(ggplot_build(p)$data[[2]], file=out_numpath)
  }else{
    group=NULL
    for(i in 3:len_dim-1){group<-c(group,rep(data2[1,i],len_row))}
    value=NULL
    for(j in 3:len_dim-1){value<-c(value,data1[,j])}
    type<-data1[,len_dim]
    df<-data.frame(group,value,type)
    jpeg(file = out_picpath[1])
    print(p<-ggplot(data = df,aes(x=group,y=value,fill=type))+
            geom_violin()+
            geom_boxplot(width=0.3,position = position_dodge(0.9)))
    #print(ggplot_build(p)$data[[2]])
    num<-c('upper:',ggplot_build(p)$data[[2]]$upper,'ymin:',ggplot_build(p)$data[[2]]$ymin,'ymax:',ggplot_build(p)$data[[2]]$ymax,
           'lower:',ggplot_build(p)$data[[2]]$lower,'middle:',ggplot_build(p)$data[[2]]$middle)
    write.csv(num, file=out_picpath[2],row.names = F)
    dev.off()
  }
}
# groupedviolin('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210515143018_mut_violin.txt',c('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210515143018_mut.jpg','E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/mut.csv'),FALSE,'groupedviolin')


# 12. violin            小提琴图
#使用实例violin('violin.txt','violin.png','violin_num.csv')
violin =function(data_filepath,out_picpath,first_column_is_num=F,title_n=NULL,color="lightblue",border_c="royalblue",colMed_c='white',rectCol_n='lightblue',lineCol_n="grey"){
  library(vioplot)
  data1<-read.table(data_filepath,header = T)#输入是表格
  if(first_column_is_num){
    jpeg(file = out_picpath[1])
    num<-vioplot(data1,main = title_n,col=color,colMed=colMed_c,rectCol=rectCol_n,lineCol=lineCol_n,border=border_c)
    dev.off()
    write.csv(num, file=out_picpath[2])
  }
  else{
    data1<- data1[, -1]
    jpeg(file = out_picpath[1])
    num<-vioplot(data1,main = title_n,col=color,colMed=colMed_c,rectCol=rectCol_n,border=border_c,lineCol=lineCol_n)
    dev.off()
    write.csv(num, file=out_picpath[2])
  }
}

# 12b. violin_dy            小提琴图
#小提琴图
violin_dy=function(data_filepath,out_picpath){
  library(ggplot2)
  library(vioplot)
  library(reshape2)
  file_data=read.table(data_filepath,header=TRUE,sep="\t",na.strings=c("NA"),check.names=F)
  file_data=melt(file_data,id=names(file_data)[1])

  plot=ggplot(file_data, aes(x=variable, y=value))
  plot+geom_violin(aes(fill=variable,linetype=NA),trim = FALSE)+
    geom_boxplot(width=0.2,outlier.colour = NA,aes(fill=variable),show.legend = FALSE)+
    # geom_jitter(shape=16,aes(fill=variable),alpha=0.5,size=0.3,position = position_jitter(0.2),show.legend = FALSE)+
    stat_summary(fun=median, geom="point",color="white",size=2)+
    xlab("")+ylab("")+
    labs(fill="",color="")+
    theme_bw()

  ggsave(out_picpath,width = 10,height = 10)
}
# violin_dy("E:/code_app/R_code/Rdata/new_violin.txt","E:/code_app/R_code/Rdata/violin_out.svg")


#13. freqhistogram   频率直方图
freqhistogram =function(data_filepath,out_picpath,title_n,X_name="value",Y_name="Y",type_n=2,theme_n=NULL,bar_width=500/50){
  library(rCharts)
  library(devtools)
  library(magrittr)
  library(plotrix)
  library(recharts)
  library(htmlwidgets)
  data1<-read.table(data_filepath, header = T)#输入是表格
  value=c(data1[,1])
  if(type_n==1){#频率
    out <- echartr(data1,value,type='histogram' ,width=600) %>%
      setTitle(title_n) %>%
      setYAxis(name=Y_name)%>%
      setXAxis(name=X_name)%>%
      setTooltip(formatter='none') %>% setSeries(1, barWidth=bar_width)%>%
      setTheme(theme = theme_n, calculable=TRUE)
    saveWidget(widget=out, file=out_picpath) #html在浏览器可直接查看图
    # return(out)
  }
  else if(type_n==2){#密度
    out<-echartr(data1,value, type='hist', subtype='density', width=600) %>%
      setTitle(title_n) %>% setYAxis(name=Y_name) %>% setXAxis(name=X_name) %>%
      setTooltip(formatter='none') %>% setSeries(1, barWidth=bar_width)%>%
      setTheme(theme = theme_n, calculable=TRUE)
    saveWidget(widget=out, file=out_picpath) #html在浏览器可直接查看图
    # return(out)
  }
}

#14.绘制饼图
pie =function(data_filepath,out_picpath,title_n,theme_n=NULL,type_n=2,sub_title=NULL,radius_3=3,labelcex_3 =0.8,height_3=0.1){
  library(rCharts)
  library(devtools)
  library(magrittr)
  library(plotrix)
  library(recharts)
  library(htmlwidgets)
  #install_github("madlogos/recharts")
  data1<-read.table(data_filepath, header = T)
  group=c(data1[,1])
  value=c(data1[,2])
  df<-data.frame(group,value)
  if(type_n==2){#普通2D饼图
    out<-hPlot(x="group",y="value",data=df,type='pie',title=title_n,subtitle = sub_title)
    out$save(out_picpath)#html只能在IDE查看代码
  }
  else if(type_n==3){
    #3D饼图
    pdf(file = paste(unlist(strsplit(out_picpath,split = '[.]'))[1],'.pdf',sep=""))
    png(file = paste(unlist(strsplit(out_picpath,split = '[.]'))[1],'.png',sep=""))
    pievalue <- paste(value,"%",sep="")
    pielabels <- paste(group,pievalue,sep="\n")
    pie3D(value)
    #pie3D(value,radius=radius_3,height=height_3,labels=pielabels,border=NA,labelcex =labelcex_3,explode=0.1,main=title_n)

    dev.off()
  }
  else if(type_n==1){#高级2D饼图
    out<-echartr(df, group, value, type='pie') %>%
      setTitle(title_n)%>%
      setTheme(theme = theme_n, calculable=TRUE)
    saveWidget(widget=out, file=out_picpath) #html在浏览器可直接查看图
  }

}
# pie('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210506220332_pie.txt','E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210506220332_pie.html','pie','shine',1,NULL)
# pie('E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210701170457_pie.txt','E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210701170457_pie.html','pie','shine',1,'p1')


# 15. scatter3d         3D散点图
#x-axis:选择作为x轴的列数
#y-axis:选择作为Y轴的列数
#p_col:点的颜色
#p_size:点的大小
#title_name:标题名字
#h_val:做y=h_val的水平辅助线
#h_col:水平辅助线的颜色
#h_lty:水平辅助线的样式（其中1为实线，2为虚线，3为点线，4为点线线段）
#v_val:做x=v_val的竖直辅助线
#v_col:竖直辅助线的颜色
#v_lty:同h_lty
#d_val:斜辅助线y=kx+b中b的值
#d_slo:斜辅助线y=kx+b中k的值
#d_lty:斜辅助线的样式
#d_col:斜辅助线的颜色

#读入为txt文件，三列分别为x,y,z，有表头
#size_n=.1为点的大小
#shape_n为点的形状:p为点图、s为球面图、l为线图、h为透视线图；
#color_scatter为点的颜色'royalblue1', 'darkcyan', 'oldlace','cornsilk','coral',
#'chartreuse','crimson','blue','black','magenta','yellow')
#输出格式为html，输出大小width_n × height_n
#实例scatter3d('scatter3d.txt','scatter.html',X_name = 'x',Y_name = 'y',Z_name = 'z')

# scatter3d =function(data_filepath,out_picpath,size_n=.2,shape_n='s',color_scatter="red",X_name=NULL,Y_name=NULL,Z_name=NULL,width_n=600,height_n=600){
#   library(rgl)
#   library(magick)
#   data1<-read.table(data_filepath, header = T)#输入是表格
#   x=c(data1[,1])
#   y=c(data1[,2])
#   z=c(data1[,3])
#   plot3d( x, y, z, type = shape_n, col=color_scatter,radius =size_n ,
#           xlab=X_name, ylab=Y_name, zlab=Z_name)
#   out<-writeWebGL(filename=out_picpath,width=width_n,height=height_n)#保存为html
#   dev.off()
#   return(out)
# }


# scatter_plot("andy","/Users/nieandi/Desktop/scatter/scatter_exa.txt","/Users/nieandi/Desktop/scatter/scatter_exa.jpg",
#              2,3,"red",1,
#              "scatter plot",5000,2,"black",5000,2,"black",5000,2,3,"black")

scatter_plot<-function(file_path,out_path,
                       x_axis=2,y_axis=3,p_col="red",p_size=1,title_name="scatter plot",
                       h_val=5000,h_lty=2,h_col="black",v_val=5000,v_lty=2,v_col="black",
                       d_val=5000,d_slo=2,d_lty=3,d_col="black"){
  library(ggplot2)
  data_F<-read.table(file_path,header = TRUE,sep="\t",na.strings=c("NA"))
  data<-read.table(file_path,header = TRUE,sep="\t",na.strings=c("NA"))
  #head(data)
  row<-dim(data)[2]
  print(row)

  p<-ggplot(data,aes(x=data[,x_axis],y=data[,y_axis]))+
    geom_point(color=p_col,size=p_size)+
    labs(title=title_name,x=data_F[1,x_axis],y=data_F[1,y_axis])+
    geom_hline(yintercept = h_val,lty=h_lty,color=h_col)+
    geom_vline(xintercept = v_val,lty=v_lty,color=v_col)+
    geom_abline(intercept=d_val,slope=d_slo,lty=d_lty,color=d_col)
  theme_bw()
  p
  ggsave(out_path)
}




# 16 线性回归图
regressionline =function(data_filepath,out_picpath,title_n=NULL,size_n=2,color_scatter="black",color_line="red",X_name=NULL,Y_name=NULL,shape_n=16,type_n=2){
  library(ggplot2)
  library(hrbrthemes)
  data1<-read.table(data_filepath, header = T)#输入是表格
  group=c(data1[,1])
  value=c(data1[,2])

  data1 <- data.frame(
    group, value)

  # Basic scatter plot.
  p1 <- ggplot(data1, aes(x=group, y=value)) +
    geom_point( color=color_scatter) +
    theme_ipsum()
  if(type_n==1){
    # with linear trend
    p2 <- ggplot(data1, aes(x=group, y=value)) +
      geom_point(color=color_scatter,size =size_n,shape=shape_n) +
      geom_smooth(method=lm , color=color_line,se=FALSE) +
      theme_ipsum()
    p2<-p2 +
      ggtitle(title_n) +
      theme(plot.title = element_text(hjust = 0.5,size = 8)) #设置标题居中
    #横纵坐标
    p2 <- p2 + labs(x = X_name,
                    y = Y_name)
    ggsave(out_picpath, plot = p2)
    return (p2)
  }
  # linear trend + confidence interval
  if(type_n==2){
    # with linear trend
    p2 <- ggplot(data1, aes(x=group, y=value)) +
      geom_point() +
      geom_smooth(method=lm , color=color_line, se=FALSE) +
      theme_ipsum()
    p3 <- ggplot(data1, aes(x=group, y=value)) +
      geom_point(color=color_scatter,size =size_n,shape=shape_n) +
      geom_smooth(method=lm , color=color_line, fill="#69b3a2", se=TRUE) +
      theme_ipsum()
    p3<-p3 +
      ggtitle(title_n) +
      theme(plot.title = element_text(hjust = 0.5,size = 8)) #设置标题居中
    #横纵坐标
    p3 <- p3 + labs(x = X_name,
                    y = Y_name)
    ggsave(out_picpath, plot = p3)
    return (p3)
  }
}

# 17. weightednetwork 权重网络图
#weight_min权重下限
#weight_max权重上限
weightednetwork<-function(file_path,save_path,weight_min,weight_max){
  library(igraph)
  library(dplyr)
  edges<-read.table(file_path,header=TRUE,sep="\t",na.strings=c("NA"))
  head(edges)
  edges<-edges %>% filter(weight>=weight_min & weight<=weight_max)
  graph<-graph.data.frame(edges,directed = FALSE, vertices = NULL)
  e<-unique(edges$fromNode)
  e<-as.data.frame(e)
  color=1:dim(e)[1]
  #V(graph)$type<-dim(e)[1]
  V(graph)$color = color
  V(graph)$shape="circle"
  V(graph)$size <- degree(graph)*0.5
  #V(graph)$color=rainbow()
  V(graph)$label.cex=0.4
  V(graph)$label.dist=0.4
  E(graph)$arrow.size=0.1 #设置箭头大小
  #生成图
  E(graph)$width<-E(graph)$weight/2
  graph$layout<-layout.circle
  svg(save_path)
  plot(graph)
  dev.off()
}

# 18. directednetwork 有向网络图
directednetwork<-function(file_name,save_path){
  library(igraph)
  data<-read.table(file_name,header=FALSE,sep="\t",na.strings=c("NA"))
  head(data)
  graph<-graph.data.frame(data,directed = TRUE, vertices = NULL)
  set.seed(50) #生成随机数，这样图的布局就会可重复，而不是每次生成的时候都变
  #具体修改过程
  vcolors<-1:10
  ecolors <- c("gray50", "tomato", "orange", "red", "yellow")
  shapes<-c("rectangle","circle")
  V(graph)$size <- 8
  V(graph)$label.dist<-2
  V(graph)$label.cex=1
  V(graph)$width<-0.3
  V(graph)$color<-vcolors
  E(graph)$color <- ecolors[E(graph)$type] #根据类型设置颜色,按照类型分组
  E(graph)$arrow.size<-0.5 #设置箭头大小
  #生成图
  graph$layout<-layout.circle
  svg(save_path)
  plot(graph)
  dev.off()
}
# directednetwork('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210515154618_direct.txt','E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210515154618_direct.png')
# directed_plot("andy","/Users/nieandi/Desktop/directed/direct.txt","/Users/nieandi/Desktop/directed/dir_test.png")


# 19. seniorbubble    高级气泡图
seniorbubble<-function(file_name,save_path,row,type,
                      t1_part,t1_all,t1_pw,t1_pv,
                      t2_part,t2_all,t2_pw,t2_pv,t2_qv,pq_type){
  library(ggplot2)
  a<-read.table(file_name,header = T,sep="\t",na.strings=c("NA"))
  if(row<=0){row=1}
  len<-length(a[,1])
  if(row>len){row=len}
  data<-a[1:row,]#仅使用1-row行的数据
  if(type==1){
    data$richFactor<-data[,t1_part]/data[,t1_all]#richFactor为差异表达的基因中位于该 pathway 条目的基因数目
    #与所有有注释基因中位于该 pathway 条目的基因总数的比值
    ggplot(data,aes(richFactor,data[,t1_pw]))+
      geom_point(aes(size=data[,t1_part],color=data[,t1_pv]))+
      scale_color_gradient(low="green",high = "red")+
      labs(title="Statistics of Pathway Enrichment",
           x="RichFactor",y="Pathway",color="Pvalue",size="Gene_number")+
      theme_bw()
    ggsave(save_path)
  }
  if(type==2){
    data$richFactor<-data[,t2_part]/data[,t2_all]#part/all算richfactor
    p<-ggplot(data,aes(richFactor,data[,t2_pw]))
    if(pq_type==1){
      p1<-p+geom_point(aes(size=data[,t2_part],color=data[,t2_pv]))+
        scale_color_gradient(low="green",high = "red")+
        labs(title="Statistics of Pathway Enrichment",
             x="RichFactor",y="Pathway",color="Pvalue",size="Gene_number")+
        theme_bw()
      p1
      ggsave(save_path)
    }
    if(pq_type==2){
      p2<-p+geom_point(aes(size=data[,t2_part],color=data[,t2_qv]))+
        scale_color_gradient(low="green",high = "red")+
        labs(title="Statistics of Pathway Enrichment",
             x="RichFactor",y="Pathway",color="Qvalue",size="Gene_number")+
        theme_bw()
      p2
      ggsave(save_path)
    }else{
      p1<-p+geom_point(aes(size=data[,t2_part],color=data[,t2_pv]))+
        scale_color_gradient(low="green",high = "red")+
        labs(title="Statistics of Pathway Enrichment",
             x="RichFactor",y="Pathway",color="Pvalue",size="Gene_number")+
        theme_bw()
      p1
      ggsave(save_path)
    }
  }

}

# Bubble_plot("andy","/Users/nieandi/Desktop/Bubble/bubble_exa.txt",
#             "/Users/nieandi/Desktop/Bubble/bubble_exa.svg",row=20,type=2,
#             t2_part = 2,t2_all=3,t2_pw = 1,t2_pv = 4,t2_qv=5,pq_type = 1)


# 21. vn_plot                维恩图
#最多可画5个集合的维恩图
#if (!require(devtools)) install.packages("devtools")
#devtools::install_github("yanlinlin82/ggvenn")
vn_plot<-function(filename,save_path){
  #library(ggvenn)
  library(VennDiagram)
  vn_data<-read.table(file = filename,header = TRUE,sep="\t",na.strings=c("NA"))
  head(vn_data)
  column<-dim(vn_data)[2]
  if(column==1){
    venn.plot<-venn.diagram(
      list(`Set1`=vn_data[,1]),
      filename = NULL,
      col = "black",
      lwd=0.5,
      fontface = "bold", #标签字体
      fill = "red", #填充色
      alpha = 0.7, #透明度
      cex = 4, #标签字体大小
      cat.cex = 3, #类名字体大小
      cat.fontface = "bold", #类名字体
      margin = 0.04 #边际距离
    )
    svg(file=save_path)
    grid.draw(venn.plot)
    dev.off()
  }
  if(column==2){
    venn.plot<-venn.diagram(
      x= list(
        `Set 1` =vn_data[,1],
        `Set 2` = vn_data[,2]
      ),
      filename =NULL,
      lwd = 0.5,
      fill = c("cornflowerblue", "darkorchid1"),
      alpha = 0.6,
      label.col = "white",
      cex = 1.5,
      fontfamily = "serif",
      fontface = "bold",
      cat.col = c("cornflowerblue", "darkorchid1"),
      cat.cex = 2,
      cat.fontfamily = "serif",
      cat.fontface = "bold",
      margin = 0.05,
      cat.dist = c(0.03, 0.03),
      cat.pos = c(-20, 20)
    )
    svg(file=save_path)
    grid.draw(venn.plot)
    dev.off()
  }
  if(column==3){
    venn.plot<-venn.diagram(
      list(`Set 1` =vn_data[,1],
           `Set 2` = vn_data[,2],
           `Set 3` = vn_data[,3]),
      filename = NULL,
      col = "black",
      lwd=0.5,
      fill = c("red", "blue", "green"),
      alpha = 0.5,
      label.col = c("darkred", "white", "darkblue", "white",
                    "white", "white", "darkgreen"),
      cex = 2.0,
      fontfamily = "serif",
      fontface = "bold",
      cat.col = c("darkred", "darkblue", "darkgreen"),
      cat.cex = 2.5,
      cat.fontfamily = "serif",
      cat.dist = c(0.06, 0.06, 0.03),
      cat.pos = 0
    )

    svg(file=save_path)
    grid.draw(venn.plot)
    dev.off()
  }
  if(column==4){
    venn.plot<-venn.diagram(
      list(`Set 1` =vn_data[,1],
           `Set 2` = vn_data[,2],
           `Set 3` = vn_data[,3],
           `Set 4` = vn_data[,4]),
      filename = NULL,
      col = "black",
      lwd = 0.5, # 边框线的宽度
      fill = c("cornflowerblue", "green", "yellow", "darkorchid1"),
      alpha = 0.50,
      label.col = c("orange", "white", "darkorchid4", "white", "white", "white",
                    "white", "white", "darkblue", "white",
                    "white", "white", "white", "darkgreen", "white"),
      cex = 2.0,
      fontfamily = "serif",
      fontface = "bold",
      cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4"),
      cat.cex = 1.8,
      cat.fontface = "bold",
      cat.fontfamily = "serif"
    )
    svg(file=save_path)
    grid.draw(venn.plot)
    dev.off()
  }
  if(column==5){
    venn.plot<-venn.diagram(
      list(`Set 1` = vn_data[,1],
           `Set 2` = vn_data[,2],
           `Set 3` = vn_data[,3],
           `Set 4` = vn_data[,4],
           `Set 5` = vn_data[,5]),
      filename = NULL,
      lwd = 0.5,
      col = "black",  #"transparent",
      fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
      alpha = 0.60,
      cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
      cat.cex = 0.8,
      cat.fontface = "bold",
      margin = 0.07,
      cex = 0.8
    )
    svg(file=save_path)
    grid.draw(venn.plot)
    dev.off()
  }
}
# vn_plot("E:/R/code/nad_folder/example/vn_text_1set.txt","E:/R/code/nad_folder/example/1set.svg")


# 22. krona        Krona层级饼图
# sunburst_plot("andy","/Users/nieandi/Desktop/sun/sun.txt","/Users/nieandi/Desktop/sun/ex.html")
# sunburst_plot<-function(file_path,save_path){
#   library(sunburstR)
#   library(R2HTML)
#   data<-read.table(file_path,header=TRUE,sep="\t",na.strings=c("NA"))
#
#   head(data)
#   m<-dim(data)[2]#列数
#   n<-dim(data)[1]#行数
#   for(i in 1:n){
#     for(j in 1:m){
#       if(data[i,j]==""){
#         data[i,j]=NA
#       }
#     }
#   }
#   data$val<-data[,2]
#   if(m>2){
#     for(i in 3:m){
#       data$val=paste(data$val,data[,i],sep="-")
#     }
#   }
#   newval<-gsub("-NA","" ,data$val)#将数据中的空值剔除
#   newval<-as.data.frame(newval)
#   num<-data[,1]
#   num<-as.data.frame(num)
#   d<-cbind(newval,num)
#   sunburst(d)
#   sunhtml<-sunburst(d)
#   HTML(save_path,sunhtml)
# }

# 23. maplot                MA图
# MA图
#a_column:第一个标本数据所在的列数
#b_column:第二个标本数据所在的列数
#q_column:pvalue/qvalue值所在的列数
#p_size:散点大小
#x_name:x轴名称
#y_name:y轴名称
#down_col:down点的颜色
#no_col：no点的颜色
#up_col：up点的颜色
# maplot("andy","/Users/nieandi/Desktop/MA/MA_exa.txt","/Users/nieandi/Desktop/MA/MA_exa.jpg",
#        "MA_plot",4,5,7,1,
#         "A","M","red","gray","green")
maplot=function(file_name,out_path,title_name="MA_plot",a_column=4,b_column=5,q_column=7,
                 p_size=1,x_name="A",y_name="M",
                 down_col="red",no_col="gray",up_col="green"){
  library(ggplot2)
  data<-read.table(file_name,header = TRUE,sep="\t",na.strings=c("NA"))
  head(data)
  A<-(log2(data[,a_column])+log2(data[,b_column]))/2
  M<-log2(data[,a_column])-log2(data[,b_column])
  data$group<- ifelse(M>=2&data[,q_column]<=0.05,"Up",
                      ifelse(M<=-2&data[,q_column]<=0.05,"Down","Not sig"))
  p <- ggplot(data,aes(x=A,y=M)) +
    geom_point(aes(color=group),size=p_size) +
    scale_color_manual(values = c(down_col,no_col,up_col)) +
    theme_bw() +
    labs(title=title_name,x=x_name, y=y_name) +
    geom_hline(yintercept=0, linetype=1, colour="black") +
    geom_hline(yintercept=c(-1,1), linetype=2, colour="black") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  p
  ggsave(out_path)
}


#24.火山图
#例：volcano_plot("andy","/Users/nieandi/Desktop/volcano/vol.csv","/Users/nieandi/Desktop/volcano/1.jpg","vol",1.5,4,30,6,2,7,0.01,"#2f5688","#BBBBBB","#CC0000")
#x_column:log2(FC)所在的列数，x轴所代表的值
#y_column:P.value或者Q.value所在的列数，y轴所代表的值
#x_val:x轴阙值，差异倍数的阙值，通常为1.5或2（可以自行输入其他数字）
#y_val:y轴阙值，P.value或者Q.value的阙值，通常为0.01和0.05（二选一就可）
#x_max:x轴的上界
#y_max:y轴的上界
volcano_plot<-function(file_name,out_path,title_name,p_size,x_max,y_max,x_column,x_val,y_column,y_val,
                       down_col,not_col,up_col){
  library(ggplot2)
  library(ggrepel)
  df<-read.table(file_name,header = T,
                 sep="\t",na.strings=c("NA"))
  #head(df)
  #dim(df)
  df$group<- ifelse(df[,x_column]>=x_val&df[,y_column]<=y_val,"Up",
                    ifelse(df[,x_column]<=(-1)*x_val&df[,y_column]<=y_val,"Down","Not sig"))
  table(df$group)#算分组后的个数
  #df$pvalue_log10<-(-log10(PValue))
  #df1<-df[df$pvalue_log10>=10,]#把值挑出来
  #dim(df1)
  ggplot(df,aes(x=df[,x_column],y=-log10(df[,y_column])))+
    geom_point(aes(color=group),size=p_size)+
    xlim((-1)*x_max,x_max)+
    ylim(0,y_max)+
    scale_color_manual(values=c(down_col,not_col,up_col))+#c("#2f5688","#BBBBBB","#CC0000")
    ##geom_label_repel(data=df1,aes(x=logFC,y=-log10(P.Value),label=gene_name))+
    labs(title=title_name,x ="log2(FC)", y ="-log10(Pvalue)")+
    geom_hline(yintercept = 1.30,linetype="dashed")+
    geom_vline(xintercept = c((-1)*x_val,x_val),linetype="dashed")+#xy虚线
    theme_bw()
  ggsave(out_path)
}
# volcano_plot("andy","/Users/nieandi/Desktop/volcano/vol_exa.txt","/Users/nieandi/Desktop/volcano/vol_exa.jpg","vol",1.5,4,30,6,2,7,0.01,"#2f5688","#BBBBBB","#CC0000")



# 25. heatmap               热图
# heatmap('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210503155332_heatmap.rpkm.txt',
#         'E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210503155332_heatmap.svg',
#         c(1,2,3,4,5),5,'row',TRUE,TRUE,c('#007D32','#000000','#AA0000'),10,38,13,TRUE,'#C4C4C4',TRUE,TRUE)
heatmap=function(data_filepath,out_picpath,U_ncol,U_nrow,U_scale,U_cluster_row,U_cluster_cols,U_color,U_fontsize,U_cellwidth,U_cellheight,U_display_number,U_border_color,U_show_rownames,U_show_colnames,U_number_color="grey30" ){
  library(pheatmap)
  library(ggplot2)
  line_data=read.table(data_filepath,header=TRUE,row.names = 1,sep="\t",na.strings=c("NA"),check.names=F)

  sample_names=c()
  gene_names=row.names(line_data)[1:U_nrow]
  for(i in 1:length(U_ncol)){
    sample_names=c(sample_names,names(line_data)[U_ncol[i]])
  }

  merge_data=line_data[1:U_nrow,U_ncol[1]]
  for(i in 2:length(U_ncol)){
    col_data=line_data[1:U_nrow,U_ncol[i]]
    merge_data=data.frame(merge_data,col_data)
  }
  names(merge_data)=sample_names
  row.names(merge_data)=gene_names

  if(is.null(U_color)){
    plot=pheatmap(merge_data,scale = U_scale,cluster_row = U_cluster_row,cluster_cols = U_cluster_cols,
                  fontsize = U_fontsize,display_numbers = U_display_number,
                  border_color = U_border_color,number_color = U_number_color,
                  show_rownames = U_show_rownames,show_colnames = U_show_colnames,
                  cellwidth = U_cellwidth,cellheight = U_cellheight)
    ggsave(out_picpath,plot)
  }
  else{
    plot=pheatmap(merge_data,scale = U_scale,cluster_row = U_cluster_row,cluster_cols = U_cluster_cols,
                  fontsize = U_fontsize,
                  color = colorRampPalette(rev(U_color))(102),display_numbers = U_display_number,
                  border_color = U_border_color,number_color = U_number_color,
                  show_rownames = U_show_rownames,show_colnames = U_show_colnames,
                  cellwidth = U_cellwidth,cellheight = U_cellheight)
    ggsave(out_picpath,plot)
  }
}
# heatmap('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210503154657_heatmap.rpkm.txt','E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210503154657_heatmap.svg',c(1,2),5,'row',TRUE,TRUE,c('#007D32','#000000','#AA0000'),10,30,20,TRUE,'#C4C4C4',TRUE,TRUE)
# heatmap('E:/eclipse_workspace/biologicalCloudTools/public/UploadFolder/001@qq.com/20210518124701_heatmap.rpkm.txt','E:/eclipse_workspace/biologicalCloudTools/public/UserFolder/001@qq.com/20210518124701_heatmap.svg',c(1,2,3,4,5,6),'8','row',TRUE,TRUE,c("#007D32","#000000","#AA0000"),10,'8','3',TRUE,'#C4C4C4',TRUE,TRUE)

# 27. box                 盒形图
#箱形图
#data_filepath 数据文件路径
#out_picpath 输出的图片路径
#U_groupname 组名，数量同data_filepath的数量一致
#U_box_color 箱的颜色,按上传源数据文件的顺序，用户未自定义则传入空
#U_Y_min Y轴最小值

#U_Y_MAX Y轴最大值
box_line=function(data_filepath,out_picpath,U_groupname,U_box_color,U_Y_min,U_Y_max){
  library(ggplot2)
  library(svglite)
  #生成对应数量的group变量
  group1=c()
  group2=c()
  values=c()
  for(i in 1:length(data_filepath)){
    line_data=read.table(data_filepath[i],header=TRUE,sep="\t",na.strings=c("NA"),check.names = FALSE)
    colnum=ncol(line_data)
    rownum=nrow(line_data)
    for(j in 1:colnum){
      for(k in 1:rownum){
        group1=c(group1,U_groupname[i])
        group2=c(group2,names(line_data)[j])
        values = c(values,line_data[k,j])
      }
    }
  }
  df=data.frame(group1,group2,values)
  plot=ggplot(df, aes(x=group2, y=values))
  if(is.null(U_box_color)){
    if(is.null(U_Y_min) || is.null(U_Y_max)){
      plot+geom_boxplot(aes(fill=group1))+
        labs(x = "", y = "",fill="")
    }
    else{
      plot+geom_boxplot(aes(fill=group1))+
        labs(x = "", y = "",fill="")+
        ylim(U_Y_min,U_Y_max)
    }
  }else{
    if(is.null(U_Y_min) || is.null(U_Y_max)){
      plot+geom_boxplot(aes(fill=group1))+
        scale_fill_manual(values = U_box_color)+
        labs(x = "", y = "",fill="")
    }
    else{
      plot+geom_boxplot(aes(fill=group1))+
        scale_fill_manual(values = U_box_color)+
        labs(x = "", y = "",fill="")+
        ylim(U_Y_min,U_Y_max)
    }
  }
  ggsave(out_picpath,dpi=300)
}
# data_path=c("E:/code_app/R_code/R_Pic/box_data1.txt","E:/code_app/R_code/R_Pic/box_data2.txt","E:/code_app/R_code/R_Pic/box_data3.txt")
# save_path="E:/code_app/R_code/R_Pic/box_out.svg"
# groupname=c("group1","group2","group3")
# box_line(data_path,save_path,groupname,c(),NULL,NULL)
