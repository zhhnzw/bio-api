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


# 15. scatter3d         3D散点图

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
maplot=function(file_name,out_path,title_name,a_column,b_column,q_column,
                 p_size,x_name,y_name,
                 down_col,no_col,up_col){
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
# maplot("andy","/Users/nieandi/Desktop/MA/MA_exa.txt","/Users/nieandi/Desktop/MA/MA_exa.jpg","MA_plot",4,5,7,1,
#         "A","M","red","gray","green")



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
