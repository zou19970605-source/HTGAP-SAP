source_code('machine_learning','read_predictions.R')
plots<-lapply(seq_along(ml),function(i){
  cc<-names(ml)[i];d<-ml[[cc]]
  rows<-lapply(use,function(m){p<-d[[m]];y<-d$truth;b<-unique(quantile(p,seq(0,1,.1)));if(length(b)<2)b<-c(min(p)-1e-8,max(p)+1e-8);bin<-cut(p,b,include.lowest=TRUE);z<-aggregate(data.frame(predicted=p,observed=y),list(bin=bin),mean);data.frame(model=m,predicted=z$predicted,observed=z$observed)})
  z<-do.call(rbind,rows)
  ggplot(z,aes(predicted,observed,colour=model))+geom_abline(slope=1,intercept=0,colour='grey65',linetype=2)+geom_line(linewidth=.8)+geom_point(size=1.8)+scale_colour_manual(values=pal,labels=labels[use],breaks=use)+scale_x_continuous(breaks=c(0,.5,1))+scale_y_continuous(breaks=c(0,.5,1))+coord_equal(xlim=c(0,1),ylim=c(0,1),expand=FALSE)+labs(x='Predicted probability',y='Observed probability',title=paste(LETTERS[i],cn[cc]))+guides(colour=guide_legend(ncol=2))
})
save_ml('Figure_S11',arrangeGrob(grobs=lapply(plots,ggplotGrob),ncol=2),210,220)
