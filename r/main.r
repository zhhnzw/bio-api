source('./base.r')
# pie('./pie.txt','./pie1.html','pie',NULL,2,NULL)

# beeswarm_chart('./sample/BaseFunction/beeswarm_chart/beeswarm_exam.txt','./beeswarm.svg','蜂群图','X轴','Y轴',c("#FF0000","#00FF95","#3C00FF"))

# podfigure(c('./sample/BaseFunction/podfigure/splitviolin1.txt',
#             './sample/BaseFunction/podfigure/splitviolin2.txt'),
#           './podfigure.svg',
#           c("#FF0000"),c("#15FF00"),'Splitviolin','X','Y')

ternaryplot_plot(
c('./sample/BaseFunction/ternaryplot_plot/ternary_input.txt',
    './sample/BaseFunction/ternaryplot_plot/ternary_group.txt'),
c('./ternaryplot_plot1.svg',
    'ternaryplot_plot2.svg',
    'ternaryplot_plot3.txt'),
'Ternary plot',0.4,FALSE)
