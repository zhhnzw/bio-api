#1.富集圈图
#data_path1:txt格式，A data frame with columns for 'category', 'ID', 'term', adjusted p-value ('adj_pval') and 'genes'
#data_path2:txt格式，A data frame with columns for 'ID', 'logFC'
#两个文件的标题行必须包括以上款项且命名完全相同，注意两个文件ID要对应。详细请参考实例文件
#out_path：支持输出pdf，svg,png,jpg等基本格式
#height_n,length_n:输出文件长和宽
#num_sub：内圈柱状的数量
#label_size:外圈标签的大小
#rad1_len:内圈半径
#rad2_len:外圈半径
#legend_show:是否显示图例
#zsc_col：用于定义 c(high, midpoint,low) 形式的内圈的颜色的字符向量
#lfc_col：指定上调和下调基因颜色的字符向量，外圈颜色
#用例：enrich_dicos("richcircos.txt","gene.txt","enrich_circos.svg")
enrich_dicos<-function(data_path,out_path,num_sub=15,label_size=5
                       ,rad1_len=3,rad2_len=4,legend_show=T,zsc_col=c("red", "white", "blue"),
                       lfc_col=c("cornflowerblue", "firebrick1"),height_n=8,
                       length_n=8){
  #安装
  # install.packages("GOplot")
  # library(devtools)
  # install_github('wencke/wencke.github.io')
  #开始
  library(ggplot2)
  #svglite(out_path)
  library(GOplot)
  data1<-read.table(data_path[1],sep='\t',header=T,stringsAsFactors = F)
  data2<-read.table(data_path[2],sep='\t',header=T,stringsAsFactors = F)
  circ<-circle_dat(data1,data2)
  #GOBubble(circ,display = 'single',labels=3,table.legend = F)
  GOCircle(circ, nsub = num_sub, label.size = label_size, rad1 = rad1_len, rad2 = rad2_len, table.legend = legend_show,
           zsc.col=zsc_col,lfc.col=lfc_col)
  ggsave(out_path,height = height_n,width = length_n)
  graphics.off() #画图结束
}
# enrich_dicos("richcircos.txt","gene.txt","enrich_circos.svg")

#2.GO富集差异气泡图
#data_path1:txt格式，A data frame with columns for 'category', 'ID', 'term', adjusted p-value ('adj_pval') and 'genes'
#data_path2:txt格式，A data frame with columns for 'ID', 'logFC'
#两个文件的标题行必须包括以上款项且命名完全相同，注意两个文件ID要对应，详细请参考实例文件
#out_path：支持输出pdf，svg,png,jpg等基本格式
#height_n,length_n:输出文件长和宽
#main_title：主题
#label_n:展示标签的阈值，默认为5
#color_n：气泡颜色，必须一组三个。
#dis_paly:展示形式，多板或单板，'multiple'和'single'
#legend_show:是否显示图例
#用例：goenrich_buttlen("richcircos.txt","gene.txt","enrich_buttlen.svg")
goenrich_buttlen<-function(data_path,out_path,lable_n=5,height_n=8,length_n=8,main_title=NULL,
                           dis_paly='single',color_n=c("chartreuse4", "brown2", "cornflowerblue")){
  #安装
  #install.packages("GOplot")
  #library(devtools)
  #install_github('wencke/wencke.github.io')
  #开始
  library(ggplot2)
  #svglite(out_path)
  library(GOplot)
  data1<-read.table(data_path[1],sep='\t',header=T,stringsAsFactors = F)
  data2<-read.table(data_path[2],sep='\t',header=T,stringsAsFactors = F)
  circ<-circle_dat(data1,data2)
  
  GOBubble(circ,display = dis_paly,labels=lable_n,title=main_title,colour = color_n,table.legend = F)
  ggsave(out_path,height = height_n,width = length_n)
  graphics.off() #画图结束
}
# goenrich_buttlen(c('E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210701222418_richcircos.txt','E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210701222418_gene.txt'),'E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210701222418_richcircos.svg')

# goenrich_buttlen("richcircos.txt","gene.txt","enrich_buttlen.svg")

#3.KEGG富集差异气泡图
#data_path.包含标题的txt或csv文件，标题必须为ID 	Description 	enrich_factor 	pvalue 	gene_number
#标题不同将可能导致运行失败。
#out_path：支持输出pdf，svg,png,jpg等基本格式
#high_color:最高值颜色
#low_color:最低值颜色
#height_n,length_n:输出文件长和宽
#main_title:主标题，X标题,Y标题
#实例keggenrich_buttlen('keggbuttlen.txt','keggbuttlen.svg')
keggenrich_buttlen<-function(data_path,out_path,high_color='red',low_color='green',
                             main_title="KEGG_enrich",x_label='Enrich_factor',y_label='Description',
                             height_n=8,length_n=8
){
  
  read.table(data_path,sep = '\t',header = T,check.names = F) ->all
  class(all)
  library(ggplot2)
  p <- ggplot(data = all,mapping = aes(x = enrich_factor,y = Description))+
    geom_point(aes(color= -log10(pvalue),size = gene_number)) +
    scale_colour_gradient(high = high_color,low = low_color) +
    theme_bw()+
    labs(title = main_title,
         x = x_label,
         y = y_label)
  ggsave(out_path,height = height_n,width = length_n)
  graphics.off() #画图结束
}
# keggenrich_buttlen('E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210701223528_keggbuttlen.txt','E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210701223528_keggbuttlen.svg')
# keggenrich_buttlen('keggbuttlen.txt','keggbuttlen.svg')


#4.DO富集差异气泡图
#输入文件第一列必须为DOID
#out_path：支持输出pdf，svg,png,jpg等基本格式
##height_n,length_n:输出文件长和宽
#实例doenrich_buttlen('dodata.txt','do_buttle.svg')
doenrich_buttlen<-function(data_path1,out_path,height_n=8,length_n=8){
  #安装： 
  # devtools::install_github("GuangchuangYu/DOSE")
  # install.packages('ggnewscale')
  # if (!requireNamespace("BiocManager", quietly = TRUE))
  #   install.packages("BiocManager")
  # BiocManager::install("clusterProfiler")

  library('DOSE')
  library(clusterProfiler)
  library(ggplot2)
  data1<-read.table(data_path1)
  enrich.do <- enrichDO(gene = data1[,1],
                        ont = 'DO',
                        pvalueCutoff = 0.05,
                        pAdjustMethod = 'BH',
                        minGSSize = 5,
                        maxGSSize = 500,
                        qvalueCutoff = 0.05,
                        readable = F)
  dotplot(enrich.do)
  ggsave(out_path,height = height_n,width = length_n)
  graphics.off() #画图结束
  
}
# doenrich_buttlen('E:/idea_java_code/biologicalCloudToolsData/UploadFolder/001@qq.com/20210701224027_dodata.txt','E:/idea_java_code/biologicalCloudToolsData/UserFolder/001@qq.com/20210701224027_dodata.svg')
# doenrich_buttlen('dodata.txt','do_buttle.svg')


#5.DO富集分析
#功能：生成柱状图，气泡图和网络图
#输入文件第一列必须为DOID
#out_path：1，2，3分别代表柱状图，气泡图和网络图。支持输出pdf，svg,png,jpg等基本格式
##height_n,length_n:输出文件长和宽
#示例do_enrich('dodata.txt','do_barplot.svg','do_dotplot.svg','do_cnetplot.svg')
do_enrich<-function(data_path,out_path,height_n=8,length_n=8){
  #安装
  # devtools::install_github("GuangchuangYu/DOSE")
  # install.packages('ggnewscale')
  library('DOSE')
  library(clusterProfiler)
  library(ggplot2)
  data1<-read.table(data_path,header = T)
  enrich.do <- enrichDO(gene = data1$DOID,
                        ont = 'DO',
                        pvalueCutoff = 0.05,
                        pAdjustMethod = 'BH',
                        minGSSize = 5,
                        maxGSSize = 500,
                        qvalueCutoff = 0.05,
                        readable = F)
  
  barplot(enrich.do)
  ggsave(out_path[1],height = height_n,width = length_n)
  
  dotplot(enrich.do)
  ggsave(out_path[2],height = height_n,width = length_n)
  
  cnetplot(enrich.do)
  ggsave(out_path[3],height = height_n,width = length_n)
  graphics.off() #画图结束
  
}
# do_enrich('dodata.txt','do_barplot.svg','do_dotplot.svg','do_cnetplot.svg')


#6.KEGG富集分析
#功能：生成柱状图，气泡图,网络图,圈图
#输入文件第一列必须为KEGGID
#out_path：1，2，3分别代表柱状图，气泡图，网络图，圈图。支持输出pdf，svg,png,jpg等基本格式
##height_n,length_n:输出文件长和宽

kegg_enrich<-function(data_path,out_path,height_n=8,length_n=8){
  library(clusterProfiler)
  data1<-read.table(data_path,header = T)
  enrich.do <- enrichKEGG(gene = data1$DOID,
                          organism = 'hsa',
                          pvalueCutoff = 0.05)
  
  barplot(enrich.do)#柱形图
  ggsave(out_path[1],height = height_n,width = length_n)
  
  dotplot(enrich.do)#气泡图
  ggsave(out_path[2],height = height_n,width = length_n)
  
  cnetplot(enrich.do)#网络图
  ggsave(out_path[3],height = height_n,width = length_n)
  
  cnetplot(enrich.do,circular=T,colorEdge=T)#圈图
  ggsave(out_path[4],height = height_n,width = length_n)
  graphics.off() #画图结束
  
  
}
# kegg_enrich('dodata.txt','kegg_barplot.svg','kegg_dotplot.svg','kegg_cnetplot.svg','kegg_circleplot.svg',
# )


#type 1为cog，type 2为kog
COG<-function(file_path,save_path,type){
  m <- read.table(file_path,header = T,sep = "\t")
  library(randomcoloR)
  svg(save_path,width = 15, height = 7 )
  layout(matrix(c(1,2),nrow = 1),widths = c(20,13))# layout布局，1行2列#
  par(mar=c(3,4,4,1)+0.1)
  class <- c("J","A","K","L","B","D","Y","V","T","M","N","Z","W","U","O","C","G","E","F","H","I","P","Q","R","S")
  t <- factor(as.character(m$Code),levels = class)
  m<-m[order(t),]
  barplot(m$Gene.Number,space = F,col = distinctColorPalette(25),ylab = "Number of Genes",names.arg = class)
  if(type==1){
    title(main = "COG function classfication")}
  else{
    title(main = "KOG function classfication")
  }
  par(mar= c(2,0,2,1)+0.1)
  plot(0,0,type = "n",xlim = c(0,1),ylim = c(0,26),bty="n",axes = F,xlab = "",ylab = "")
  for(i in 1:length(class)){
    text(0,26-i+0.5,paste(m$Code[i],m$Functional.Categories[i]),pos=4,cex=1,pty=T)
  }
  
  while (!is.null(dev.list())) dev.off()
}
# COG("/Users/nieandi/Desktop/cog.class.annot.txt","/Users/nieandi/Desktop/COG.svg",1)
# COG("/Users/nieandi/Desktop/cog.class.annot.txt","/Users/nieandi/Desktop/KOG.svg",2)

