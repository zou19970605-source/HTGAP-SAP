source_code('machine_learning','read_predictions.R')
ccs<-c('training','temporal2025','rct')
plots<-lapply(seq_along(ccs),function(i){
  cc<-ccs[i];z<-ml[[cc]][,c('truth',use)]
  dc<-dca(reformulate(use,'truth'),data=z,thresholds=seq(.01,.5,.01))
  dd<-as.data.frame(dc$dca);dd<-dd[dd$variable%in%use,]
  ggplot(dd,aes(threshold,net_benefit,colour=variable))+geom_hline(yintercept=0,colour='grey65')+geom_line(linewidth=.8)+scale_colour_manual(values=pal,labels=labels[use],breaks=use)+coord_cartesian(ylim=c(-.025,.16))+labs(x='Threshold probability',y='Net benefit',title=paste(LETTERS[i],cn[cc]))+guides(colour=guide_legend(ncol=1))
})
save_ml('Figure_S12',arrangeGrob(grobs=lapply(plots,ggplotGrob),ncol=3),230,115)
