source_code('R','plot_theme.R')
distplots<-lapply(A$v,function(v){obs<-D$raw$training[[v]];obs<-obs[is.finite(obs)];imp<-unlist(lapply(D$X$training,function(x)x[,v]));z<-rbind(data.frame(value=obs,series='Observed'),data.frame(value=imp,series='Completed'));p<-ggplot(z,aes(value,colour=series))+geom_density(linewidth=.8)+scale_colour_manual(values=c(Completed='#B2604C',Observed='#326C9A'))+labs(x=lab[v],y='Density')+theme(legend.position='bottom');if(v %in% c('ldh','amylase'))p<-p+scale_x_log10();p})
save_fig('Figure_S2',arrangeGrob(grobs=lapply(distplots,grob),ncol=3),210,200)
