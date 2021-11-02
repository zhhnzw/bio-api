source('./base.r')
source('./FunctionalAnalysis.r')
source('./ClusteringAnalysis.r')
# pie('./pie.txt','./pie1.html','pie',NULL,2,NULL)

# beeswarm_chart('./sample/BaseFunction/beeswarm_chart/beeswarm_exam.txt','./beeswarm.svg','蜂群图','X轴','Y轴',c("#FF0000","#00FF95","#3C00FF"))

# podfigure(c('./sample/BaseFunction/podfigure/splitviolin1.txt',
#             './sample/BaseFunction/podfigure/splitviolin2.txt'),
#           './podfigure.svg',
#           c("#FF0000"),c("#15FF00"),'Splitviolin','X','Y')

# ternaryplot_plot(
# c('./sample/BaseFunction/ternaryplot_plot/ternary_input.txt',
#     './sample/BaseFunction/ternaryplot_plot/ternary_group.txt'),
# c('./ternaryplot_plot1.svg',
#     'ternaryplot_plot2.svg',
#     'ternaryplot_plot3.txt'),
# 'Ternary plot',0.4,FALSE)

# circos(c('./sample/BaseFunction/circos/taxonomy.txt',
#          './sample/BaseFunction/circos/group.txt',
#          './sample/BaseFunction/circos/otu_table.txt'),
#        './taxonomy.pdf')

# groupedviolin(
# './sample/BaseFunction/groupedviolin/mut_violin.txt',
# c('./mut.jpg','./mut.csv'),
# FALSE,'groupedviolin')

violin('./sample/BaseFunction/violin/violin.txt','./violin.png',FALSE)

# data_path=c("./sample/BaseFunction/box_line/box.txt")
# save_path="./box_out.svg"
# groupname=c("group1")
# box_line(data_path,save_path,groupname,c(),NULL,NULL)

# volcano_plot("./sample/BaseFunction/volcano_plot/vol_exa.txt","./vol_exa.svg","vol",1.5,4,30,6,2,7,0.01,"#2f5688","#BBBBBB","#CC0000")

# maplot("./sample/BaseFunction/maplot/MA_exa.txt","./MA_exa.jpg",
#        "MA_plot",4,5,7,1,
#         "A","M","red","gray","green")

# upset_chart("./sample/BaseFunction/upset_chart/seniorvenn.txt", #输入文件路径
#             "./upsetchart.svg", #图片输出路径
#             8,8, #宽✖高
#             "Set Size", #集合(横矩形)名
#             "Intersection Size", #纵矩形名
#             order_by = c("freq") #c("freq"), c("degree"), c("freq", "degree")
# )


# FunctionalAnalysis.r
# COG("./sample/FunctionalAnalysis/COG/cog.class.annot.txt","./COG.svg",1)

# ClusteringAnalysis.r
# NMDS(c('./sample/ClusteringAnalysis/NMDS/nmds_out.txt','./sample/ClusteringAnalysis/NMDS/nmds_group.txt'),'./nmds.svg',18,18)
# PCOA(c('./sample/ClusteringAnalysis/PCOA/pcoa_out.txt','./sample/ClusteringAnalysis/PCOA/pcoa_group.txt'),'pcoa.svg')
# tsne_chart("./sample/ClusteringAnalysis/tsne_chart/tsne.txt",
#            "./tsne.svg",
#            "./tsne.txt")