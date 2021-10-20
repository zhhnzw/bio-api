#14.绘制饼图
pie =function(data_filepath,out_picpath,title_n,theme_n=NULL,type_n=2,sub_title=NULL,radius_3=3,labelcex_3 =0.8,height_3=0.1){
  library(rCharts)
  library(devtools)
  library(magrittr)
  library(plotrix)
  library(recharts)
  library(htmlwidgets)
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
# pie('./pie.txt','./pie1.html','pie',NULL,2,NULL)