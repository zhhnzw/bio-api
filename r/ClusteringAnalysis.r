#聚类分析图



#NMDS分析
#file_path1丰度表，txt
#file_path2 分组表，txt
#out_path 输出，自定义，支持pdf,png,jpg,svg,html等
#n_height,n_width，输出格式长和宽
# NMDS('nmds_out.txt','nmds_group.txt','nmds.pdf')
NMDS <- function(file_path,out_path,n_height=18,n_width=18){
  library(vegan)
  library(ggplot2)
  otu<-read.table(file_path[1],header=T,sep="\t")
  rownames(otu)<-otu[,1]
  otu<-otu[,-1]
  otu<-data.frame(t(otu))
  map<-read.table(file_path[2],header=T,sep="\t")
  sites<-as.vector(map[,1])
  amt<-as.vector(map[,2])
  sol <- metaMDS(otu)
  MyMeta = data.frame(sites,amt,row.names = "sites")
  NMDS = data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2])
  
  #使用ggplot2画图，逐步添加
  P<-ggplot(data = NMDS, aes(MDS1,MDS2))+theme_bw()+
    theme(panel.grid=element_blank())+
    geom_point(aes(data = MyMeta,shape= MyMeta$amt, color = MyMeta$amt))+
    #stat_ellipse(aes( data = MyMeta, fill=MyMeta$amt), level = 0.95, alpha = 0.5, show.legend = FALSE) +   #添加置信椭圆，注意不是聚类
    geom_text(label=MyMeta$amt,size=3,hjust=0.5, vjust=0)+
    theme(legend.key=element_rect(colour="white",fill="white",size=3,linetype="dashed"))+
    theme(legend.title=element_blank())+
    geom_polygon(data=MyMeta$sites,aes(fill=MyMeta$amt,alpha=0.8),show.legend=FALSE)+
    annotate("text",x=min(NMDS$MDS1),y=min(NMDS$MDS2),hjust=0,vjust=0,label=paste("Stress:",sol$stress))
  ggsave(out_path,height=n_height,width=n_width,units = "cm")
  
}
#NMDS('nmds_out.txt','nmds_group.txt','nmds.pdf')
# NMDS(c('E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630215855_nmds_out.txt','E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630215855_nmds_group.txt'),'E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210630215855_nmds.svg',18,18)



#PCOA图
#file_path1丰度表，txt
#file_path2 分组表，txt
#out_path 输出，自定义，支持pdf,png,jpg,svg,html等
#n_height,n_width，输出格式长和宽
#main_text为标题,x_lab为x轴标签，y_lab为y轴标签
#x_col为X轴颜色，y_col为Y轴颜色，grid_col为网格颜色
#species_color为标签颜色，species_size为标签大小
#实例PCOA('pcoa_out.txt','pcoa_group.txt','pcoa.pdf')
PCOA<-function(file_path,out_path,n_height=12,n_width=18,
               main_text="PCoA",
               x_lab='PCoA axis1:',
               y_lab='PCoA axis2:',
               x_col='grey',
               y_col='grey',
               grid_col='grey',
               species_color = "lightskyblue",
               species_size=3){
  #导入作图包
  library(vegan)
  #library(ape)
  library(ggplot2)
  library(svglite)
  #svglite(out_path)
  #pdf(out_path)
  #颜色选择
  color=c( "#3C5488B2","#00A087B2", 
           "#F39B7FB2","#91D1C2B2", 
           "#8491B4B2", "#DC0000B2", 
           "#7E6148B2","yellow", 
           "darkolivegreen1", "lightskyblue", 
           "darkgreen", "deeppink", "khaki2", 
           "firebrick", "brown1", "darkorange1", 
           "cyan1", "royalblue4", "darksalmon", 
           "darkgoldenrod1", "darkseagreen", "darkorchid")
  #读取otu数据文件
  otu <- read.delim(file_path[1], row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
  # otu <- read.delim('pcoa_out.txt',header = T)
  #otu <- read.table('pcoa_out.txt', skip=1,row.names = 1,  stringsAsFactors = FALSE)
  otu <- data.frame(t(otu))
  
  #根据物种组成计算样方距离，结果以 dist 数据类型存储
  bray_dis <- vegdist(otu, method = 'bray')	
  
  #样方排序坐标
  #pcoa <- pcoa(bray_dis,correction="none")
  pcoa <- cmdscale(bray_dis, k = (nrow(otu) - 1), eig = TRUE)
  site <- data.frame(pcoa$point)[1:2]
  site$name <- rownames(site)
  #读取分组文件
  map<-read.table(file_path[2],header=T,sep="\t",row.names=1)
  #site$group <- c(rep('A', 12), rep('B', 12), rep('C', 12))
  #将分组文件和数据文件以行名合并
  merged=merge(site,map,by="row.names",all.x=TRUE)
  #使用 wascores() 被动添加物种得分（坐标），丰度加权平均方法
  species <- wascores(pcoa$points[,1:4], otu)
  #计算 丰度物种
  abundance <- apply(otu, 2, sum)
  abundance <- names(abundance[order(abundance, decreasing = TRUE)])
  
  species <- data.frame(species[abundance,1:2])
  species$name <- rownames(species)
  
  pcoa_exp <- pcoa$eig/sum(pcoa$eig)
  pcoa1 <- paste(x_lab, round(100*pcoa_exp[1], 2), '%')
  pcoa2 <- paste(y_lab, round(100*pcoa_exp[2], 2), '%')
  #ggplot2 作图
  
  p<-ggplot(data = merged, aes(X1, X2)) +
    geom_point(aes(color = group)) +
    stat_ellipse(aes(fill = group), geom = 'polygon', level = 0.95, alpha = 0.1, show.legend = F) +	#添加置信椭圆
    scale_color_manual(values =color[1:length(unique(map$group))]) +
    scale_fill_manual(values =color[1:length(unique(map$group))]) +
    theme(panel.grid.major = element_line(color = grid_col, size = 0.2), panel.background = element_rect(color = 'black', fill = 'transparent'), 
          plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0, color = x_col, size = 0.5) +
    geom_hline(yintercept = 0, color = y_col, size = 0.5) +
    geom_text(data = species, aes(label = name), color=species_color, size = species_size) +	#丰度物种标签
    labs(x = pcoa1, y = pcoa2, title = main_text) 
  ggsave(out_path,height=n_height,width=n_width,units = "cm")
  
}
#PCOA('pcoa_out.txt','pcoa_group.txt','pcoa.pdf')






#  tsne分析
#与基迪奥不同：取消了分组文件，分组数据在数据文件中单独一列，位于最后一列
#没有显示样本名称选项
#没有行列绘图选项，以一行的数据为一个降维数据点进行绘图，即只能进行“行绘图”
#file_path            数据文件路径
#out_path             输出图片路径
#out_file_path        输出降维后数据路径--txt格式
#u_point_size         点大小，默认：2
#u_pca                是否应执行初始PCA步骤，默认：TRUE
#u_partial_pca        是否应使用截断PCA计算主成分（需要irlba包）。对于大型输入矩阵，这会更快（默认值：FALSE）
#u_normalize          归一化，在使用normalize_input进行距离计算之前，是否应该对数据进行内部标准化(默认值：TRUE）
#u_alpha              点透明度，默认：0.5
#u_line_color         分割线颜色(若设为白色，则表现为不显示)，默认：'black'
#u_title              主标题
#u_x_label            x轴标签
#u_y_label            y轴标签
#u_show_legend        是否显示图例，默认：TRUE


tsne_chart <- function(file_path, out_path,
                       u_point_size = 2,
                       u_pca = TRUE,
                       u_partial_pca = FALSE,
                       u_normalize = TRUE,
                       u_alpha = 0.5,
                       u_line_color = 'black',
                       u_title = "tSNE",
                       u_x_label = "x-tSNE",
                       u_y_label = "y-tSNE",
                       u_show_legend = TRUE){
  library(Rtsne)
  library(ggplot2)
  
  file_data <- read.table(file_path, header = TRUE, row.names = 1, sep = "\t") #读取数据文件
  unique_data <- unique(file_data)
  numcol <- ncol(unique_data) #列数
  group_data <- unique_data[,numcol] #分组数据列
  mdata <- as.matrix(unique_data[,1:numcol-1]) #将数据列转换为矩阵
  
  if(u_pca & u_partial_pca){ #两个不能同时为TRUE
    u_partial_pca = FALSE
  }
  
  set.seed(1)
  #使用Rtsne函数降维
  tsne_out <- Rtsne(X = mdata, #矩阵；数据矩阵（每行是一个观察值，每列是一个变量）
                    dims = 2, #结果维度，默认为2
                    initial_dims = 50, #初始PCA步骤中应保留的维度数（默认值：50）
                    perplexity = 20, #复杂度参数（困惑参数、最佳邻居数）,不大于(3 * perplexity < nrow(X) - 1)
                    theta = 0.5, #速度/精度权衡（精度较低时增加），精确TSNE设置为0.0（默认值：0.5）
                    check_duplicates = TRUE, #检查是否存在重复项。最好确保不存在重复项，并将此选项设置为FALSE，特别是对于大型数据集（默认值：TRUE）
                    pca = u_pca, #是否应执行初始PCA步骤（默认值：TRUE）
                    partial_pca = u_partial_pca, #是否应使用截断PCA计算主成分（需要irlba包）。对于大型输入矩阵，这会更快（默认值：FALSE）
                    max_iter = 5000, #迭代次数（默认值：1000）--2000
                    verbose = FALSE, #getOption("verbose", FALSE), #是否应打印进度更新（默认值：全局“verbose”选项，如果未设置，则为FALSE）
                    is_distance = FALSE, #表面X是否是距离矩阵（实验，默认值：FALSE）
                    # Y_init = NULL, #矩阵；对象的初始位置。如果为NULL，则使用随机初始化（默认值：NULL）。请注意，当使用此项时，将跳过具有夸大的困惑值和较大动量项的初始阶段。
                    # pca_center = TRUE, #在应用主成分分析之前，数据是否应该居中(默认值：TRUE）
                    # pca_scale = FALSE, #在应用主成分分析之前，是否应缩放数据(默认值：FALSE）
                    normalize = u_normalize, #在使用normalize_input进行距离计算之前，是否应该对数据进行内部标准化(默认值：TRUE）
                    #stop_lying_iter = ifelse(is.null(Y_init), 250L, 0L), #迭代之后，困惑不再被夸大（默认值：250，除非使用Y_init，然后是0）
                    #mom_switch_iter = ifelse(is.null(Y_init), 250L, 0L), #迭代，之后使用最终动量（默认值：250，除非使用Y_init，然后为0）
                    # momentum = 0.5, #优化第一部分使用的动量（默认值：0.5）
                    # final_momentum = 0.8, #优化的最后部分使用的动量（默认值：0.8）
                    # eta = 200, #学习率（默认值：200.0）
                    # exaggeration_factor = 12, #用于在优化的第一部分乘以P矩阵的放大系数（默认值：12.0）
                    #num_threads = 1 #使用OpenMP的线程数，默认值1.0对应于使用所有可用的内核
  )
  
  tsne_result <- tsne_out$Y #获取降维后的数据--矩阵
  colnames(tsne_result)=c('tsne_1','tsne_2') #设置矩阵列名
  
  #将矩阵转换为数据框
  tsne_data = data.frame(sample=rownames(unique_data), #行名
                         tsne_result, #矩阵
                         group = group_data) #新增分组列--file_data分组列(最后一列)
  
  #导出数据，txt格式
  write.table(tsne_data, #需要导出的数据
              out_path[2], #文件路径
              sep = "\t", #分隔符，默认为空格
              row.names = FALSE, #是否输出行名，默认为TRUE
              col.names = TRUE, #是否输出列名，默认为TRUE
              quote = FALSE)#字符串是否使用引号表示，默认为TRUE
  
  #画图
  p <- ggplot(data=tsne_data,aes(x=tsne_1,y=tsne_2))+
    geom_point(aes(colour = group), #颜色
               shape = 16, #形状
               size = u_point_size, #点大小
               alpha = u_alpha, #透明度
               show.legend = u_show_legend) + #是否显示图例
    labs(x = u_x_label, y = u_y_label, title = u_title)+
    theme(text=element_text(size=10),
          plot.title = element_text(hjust = 0.5), #设置标题居中
          legend.title=element_blank(),
          panel.background = element_rect(fill = "white", colour = "black",size = 0.2),
          legend.key = element_rect(fill = "white", colour = "white"),
          legend.background = (element_rect(colour= "white",fill = "white")))+
    geom_vline(xintercept = 0, colour = u_line_color)+ #横线
    geom_hline(yintercept = 0, colour = u_line_color) #竖线
  
  ggsave(out_path[1], p)
  
  
}

# tsne_chart("tsne_out.txt",
#            "tsne.svg",
#            "tsne.txt",
#            2,
#            u_pca = TRUE,
#            u_partial_pca = FALSE,
#            u_normalize = TRUE,
#            u_alpha = 1,
#            u_line_color = 'grey',
#            u_title = "tSNE",
#            u_x_label = "x-tSNE",
#            u_y_label = "y-tSNE",
#            u_show_legend = TRUE)





#PCA
#data_filepath1 数据文件1路径,支持txt,csv,xlsx
#data_filepath2 数据文件2路径,支持txt,csv,xlsx
#data_filepath3 数据文件2路径,支持txt,csv,xlsx
#out_picpath 输出的图片路径，svg格式
#scale 是否归一化 (TRUE,FALSE)
plot_PCA=function(data_filepath,out_picpath,scale,type=0){
  library(usethis)
  library(openxlsx)
  library(gmodels)
  library(ggbiplot)
  library(devtools)
  library(ggrepel)
  library(ggplot2)
  
  line_data1=NULL
  if(type == 0){
    file_type1=unlist(strsplit(data_filepath[1],"[.]"))
  }else{
    file_type1=unlist(strsplit(data_filepath[1],"[.]"))
  }
  file_type1=file_type1[length(file_type1)]
  if(file_type1=="txt")
    line_data1=read.table(data_filepath[1],header=TRUE,row.names = 1,sep="\t",na.strings=c("NA"),check.names=F)
  if(file_type1=="csv")
    line_data1=read.csv(data_filepath[1],header=TRUE,row.names = 1,sep=",",na.strings=c("NA"),check.names=F)
  if(file_type1=="xlsx")
    line_data1=read.xlsx(data_filepath[1],colNames = TRUE,rowNames = TRUE,na.strings=c("NA"))
  
  line_data2=NULL
  if(type == 1){
    file_type2=unlist(strsplit(data_filepath[2],"[.]"))[-1]
    file_type2=file_type2[length(file_type2)]
    if(file_type2=="txt")
      line_data2=read.table(data_filepath[2],row.names = 1,sep="\t",na.strings=c("NA"),check.names=F)
    if(file_type2=="csv")
      line_data2=read.csv(data_filepath[2],row.names = 1,sep=",",na.strings=c("NA"),check.names=F)
    if(file_type2=="xlsx")
      line_data2=read.xlsx(data_filepath[2],colNames =FALSE,rowNames = TRUE,na.strings=c("NA"))
  }
  
  line_data3=NULL
  if(type == 2){
    file_type3=unlist(strsplit(data_filepath[2],"[.]"))[-1]
    file_type3=file_type3[length(file_type3)]
    if(file_type3=="txt")
      line_data3=read.table(data_filepath[2],sep="\t",na.strings=c("NA"),check.names=F)
    if(file_type3=="csv")
      line_data3=read.csv(data_filepath[2],sep=",",na.strings=c("NA"),check.names=F)
    if(file_type3=="xlsx")
      line_data3=read.xlsx(data_filepath[2],colNames =FALSE,na.strings=c("NA"))
    line_data3=line_data3[,1]
  }
  
  line_data1=-t(line_data1)
  
  if(!is.null(line_data3)){
    if(length(line_data3)<2)
      return()
    merge_data=row.names(line_data1)
    # merge_data=data.frame(merge_data,line_data1[,2])
    # names(merge_data)=c("s","v")
    gene_group=colnames(line_data1)
    for(i in 1:length(line_data3)){
      for(j in 1:length(gene_group)){
        if(line_data3[i]==gene_group[j]){
          merge_data=data.frame(merge_data,line_data1[,j])
          break
        }
      }
    }
    merge_data=merge_data[,-1]
    names(merge_data)=line_data3
    line_data1=merge_data
  }
  
  data.pca<-fast.prcomp(line_data1,scale.=scale)
  
  if(is.null(line_data2)){
    plot=ggbiplot(data.pca,var.axes = FALSE,groups = row.names(line_data1))
    plot+geom_vline(xintercept = 0,linetype="dotted")+
      geom_hline(yintercept = 0,linetype="dotted")+
      # geom_point(aes(colour=rownames(line_data1))) +
      geom_text_repel(aes(label = rownames(line_data1))) +
      theme_bw()+
      theme(panel.grid=element_blank(),legend.position = "none")
    ggsave(out_picpath)
  }
  else{
    plot=ggbiplot(data.pca,var.axes = FALSE,groups = line_data2[,1])
    plot+geom_vline(xintercept = 0,linetype="dotted")+
      geom_hline(yintercept = 0,linetype="dotted")+
      geom_text_repel(aes(label = rownames(line_data1))) +
      scale_color_discrete(name="")+
      theme_bw()+
      theme(panel.grid=element_blank())
    ggsave(out_picpath)
  }
}
# plot_PCA('E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210613160432_PCA.txt','E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210613160432_PCA.svg',FALSE,0)
# plot_PCA("C:\\Users\\whg\\Desktop\\R\\191116-R包vegan的群落PCA及tb-PCA分析\\env_table.txt",
#          NULL,
#          NULL,
#          "E:/code_app/R_code/Rdata/PAC.svg",
#          F)

# plot_PCA("E:/code_app/R_code/Rdata/exam.fpkm3.eg.xlsx",
#          "E:/code_app/R_code/Rdata/exam.group3.eg.xlsx",
#          "E:/code_app/R_code/Rdata/exam.row.eg.xlsx",
#          "E:/code_app/R_code/Rdata/PAC.svg",
#          F)


hca_plot<-function(file_path,save_path){
  hca<-read.table(file_path,header = T,row.names = 1,
                  sep="\t",na.strings=c("NA"))
  hca<-t(hca)
  dist.r<- dist(hca)  
  hca.r <- hclust(dist.r,method = "complete") 
  svg(save_path)
  plot(hca.r)
  dev.off()
}
# hca_plot("/Users/nieandi/Desktop/gene/hca/hca.txt","/Users/nieandi/Desktop/gene/hca/hca.svg")


#remotes::install_github('coolbutuseless/ggrgl', ref='main')
#install.packages("scatterplot3d")
#install.packages("gmodels")
#install.packages("randomcoloR")
#group_num 分组数目(未分组即未输入分组文件时group_num为0)
pca_3d <- function(file_path,save_path,group_num){
  
  library(ggplot2)
  library(ggrgl)
  library(scatterplot3d)
  library(gmodels)
  library(svglite)
  library(randomcoloR)
  data<-read.table(file_path[1],header = T,row.names = 1, sep="\t",na.strings=c("NA"))
  data<-t(data)
  if(is.na(file_path[2])){
    data.pca<-fast.prcomp(data)
    num <- nrow(data)
    pca.result <- data.pca$x
    pca.result <- data.frame(pca.result)
    pca.result$group <- row.names(data)
    group_color <- distinctColorPalette(num) #差异明显的不同颜色
  }else{
    group <- read.table(file_path[2],header = F,row.names = 1,sep="\t",na.strings=c("NA"))
    palette <- distinctColorPalette(group_num) #差异明显的不同颜色
    data1 <- merge(data, group, by = "row.names", all = TRUE)
    len <- ncol(data1)-1
    data.pca<-fast.prcomp(data1[,2:len])
    pca.result <- data.pca$x
    pca.result <- data.frame(pca.result)
    pca.result$group <- data1[,ncol(data1)]
    group_color <- palette[as.numeric(as.factor(pca.result$group))]
    data_col <- as.data.frame(group_color)
  }
  sdev <- data.pca$sdev
  pro <- sdev^2/sum(sdev^2)
  svg(save_path)
  pic<-scatterplot3d(pca.result[,1:3] , pch=16, color = group_color,type = 'h',
                     main="PCA",
                     xlab = paste("PC1:(",100*round(pro[1], 4),"%)"),
                     ylab = paste("PC2:(",100*round(pro[2], 4),"%)"),
                     zlab = paste("PC3:(",100*round(pro[3], 4),"%)"))
  text(pic$xyz.convert(pca.result[, 1:3]),
       labels = rownames(data) , cex= 0.8, col = "black")
  if(!is.na(file_path[2])){
    legend('top', legend = levels(as.factor(pca.result$group)),
           col = palette, pch = 16,inset = -0.1, xpd = TRUE, horiz = TRUE)
  }
  while (!is.null(dev.list())) dev.off()
}
# file_path="asd"
# if(class(file_path) == 'Array'){
#   print("ADS")
# }else{
#   print(class(file_path))
#   print(file_path[1])
#   print(is.na(file_path[2]))
# }
# pca_3d(c('E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630225840_exam.txt','E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630225840_group.txt'),'E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210630225840_exam.svg',2)
# pca_3d(c('E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630225840_exam.txt',
#          NULL),
#        'E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210630225840_exam.svg',2)
# pca_3d('/Users/nieandi/Desktop/gene/PCA/exam.txt','/Users/nieandi/Desktop/gene/PCA/group.txt','/Users/nieandi/Desktop/gene/PCA/pca3d.svg',2)
# pca_3d('/Users/nieandi/Desktop/gene/PCA/exam.txt',NULL,'/Users/nieandi/Desktop/gene/PCA/pca3d_1.svg',0)




#CCA
#file_path1           otu
#file_path2           env
#file_path3           group
#out_file_path1       样本坐标表
#out_file_path2       物种坐标表
#out_file_path3       环境因子坐标
#out_file_path4       DCA结果表
#out_picture_path     图片

CCA_chart <- function(file_path, out_file_path){
  library(ggplot2)
  library(ggrepel)
  library(permute)
  library(lattice)
  library(vegan)

  otu <- read.table(file_path[1], header = TRUE, row.names = 1, sep = "\t") #OUT丰度表
  env <- read.table(file_path[2], header = TRUE, row.names = 1, sep = "\t") #环境因子表
  group <- read.table(file_path[3], header = FALSE, sep = "\t") #分组信息表

  otu <- data.frame(t(otu)) #转置

  otu_cca <- cca(otu~., env)

  #CCA 结果解读
  otu_cca.scaling <- summary(otu_cca, scaling = 2)

  svg(out_file_path[5])
  plot(otu_cca, scaling = 2, main = NULL, display = c('wa', 'sp', 'cn'))
  dev.off()

  #CCA 结果提取
  #使用物种加权和计算的样方得分
  otu_cca_site.scaling <- data.frame(otu_cca.scaling$site[ ,1:6]) #样本坐标表
  otu_cca_site.scaling <- cbind(Sample = row.names(otu_cca_site.scaling), otu_cca_site.scaling)
  #物种
  otu_cca_sp.scaling <- data.frame(otu_cca.scaling$species[ ,1:6]) #物种坐标表
  otu_cca_sp.scaling <- cbind(Species = row.names(otu_cca_sp.scaling), otu_cca_sp.scaling)
  #环境
  otu_cca_env.scaling <- data.frame(otu_cca.scaling$biplot[ ,1:6]) #环境因子坐标
  otu_cca_env.scaling <- cbind(Constrain = row.names(otu_cca_env.scaling), otu_cca_env.scaling)

  write.table(otu_cca_site.scaling, out_file_path[1], row.names = FALSE, quote = FALSE, sep = "\t")
  write.table(otu_cca_sp.scaling, out_file_path[2], row.names = FALSE, quote = FALSE, sep = "\t")
  write.table(otu_cca_env.scaling, out_file_path[3], row.names = FALSE, quote = FALSE, sep = "\t")

  #eig分析
  cca_eig <- otu_cca$CCA$eig
  eiglength <- length(cca_eig)
  eig_proportion <- 1:eiglength #eig比例
  eig_total_proportion <- 1:eiglength #eig累计比例

  sum <- 0
  for(i in 1:eiglength){
    sum = sum + cca_eig[i]
  }

  sumproportion <- 0
  for(i in 1:eiglength){
    pro = cca_eig[i]/sum
    eig_proportion[i] = pro
    sumproportion = sumproportion + pro
    eig_total_proportion[i] = sumproportion
  }

  otu_cca_eig <- data.frame(cca_eig, eig_proportion, eig_total_proportion)
  colnames(otu_cca_eig) <- c("Eigenvalue", "Proportion Explained", "Cumulative Proportion")
  otu_cca_eig <- cbind(CCA = row.names(otu_cca_eig), otu_cca_eig)

  write.table(otu_cca_eig, out_file_path[4], row.names = FALSE, quote = FALSE, sep = "\t")
}
# CCA_chart(c('E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630224535_ccatop20.txt',
#             'E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630224535_ccaenvir.txt',
#             'E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210630224535_ccagroup.txt'),
#           c('E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210630224535_sample.txt',
#             'E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210630224535_species.txt',
#             'E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210630224535_constrain.txt',
#             'E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210630224535_eig.txt',
#             'E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210630224535_CCA.png'))






