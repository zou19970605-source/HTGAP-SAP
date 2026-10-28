source_code('R','nomogram_plots.R')
outcome_names<-c(sap='SAP (%)',nof='New-onset OF (%)',ipn='Infected necrosis (%)')
riskplots<-list()
for(o in names(outcome_names))for(c in names(cn)){
  z<-A$risk[A$risk$cohort==c&A$risk$outcome==o,];z$group<-factor(z$group,c('Low','Intermediate','High'))
  ymax<-max(A$risk$upper[A$risk$outcome==o],na.rm=TRUE)*100
  z$label_y<-100*z$upper+.035*ymax
  riskplots[[length(riskplots)+1]]<-ggplot(z,aes(group,100*rate,fill=group))+geom_col(width=.58)+geom_errorbar(aes(ymin=100*lower,ymax=100*upper),width=.12)+geom_text(aes(y=label_y,label=paste0(events,'/',available)),vjust=0,size=3.1,family='Arial')+scale_fill_manual(values=c('#79B3B5','#D6AA57','#B36566'),guide='none')+coord_cartesian(ylim=c(0,1.15*ymax))+labs(x=if(o=='ipn')'Predicted risk group'else NULL,y=outcome_names[o],title=if(o=='sap')cn[c]else NULL)+theme(axis.text.x=element_text(size=10),plot.title=element_text(size=12,face='bold'))
}
savefig('Figure_S13',function(){grid.newpage();grid.draw(arrangeGrob(grobs=lapply(riskplots,ggplotGrob),ncol=3))},220,220)

dev.off()
