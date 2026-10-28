options(stringsAsFactors=FALSE)
library(grid);library(gridExtra);library(ggplot2);library(svglite)
source_code('R','admission_outcomes.R')
cairo_pdf(tempfile(fileext='.pdf'),width=15,height=6,family='Arial')
ofcols<-c('No OF'='#326C9A','OF'='#C06455')
cohortlabels<-c(training='Development',temporal2025='Temporal')
ef33<-read.csv('analysis/admission_OF_effects.csv')
z33<-subset(ef33,outcome %in% c('sap','pmof','ipn','ihm') & n_OF>0)
rates33<-rbind(data.frame(cohort=z33$cohort,outcome=z33$outcome,group='No OF',rate=z33$risk_no_OF,lo=z33$no_OF_lower,hi=z33$no_OF_upper),
               data.frame(cohort=z33$cohort,outcome=z33$outcome,group='OF',rate=z33$risk_OF,lo=z33$OF_lower,hi=z33$OF_upper))
rates33$outcome<-factor(rates33$outcome,rev(c('sap','pmof','ipn','ihm')),rev(c('SAP','Persistent MOF','IPN','Death')))
rates33$cohort<-factor(rates33$cohort,names(cohortlabels),cohortlabels)
rates33$sign<-ifelse(rates33$group=='No OF',-1,1)
rates33$x<-100*rates33$rate*rates33$sign
rates33$xmin<-ifelse(rates33$sign<0,-100*rates33$hi,100*rates33$lo)
rates33$xmax<-ifelse(rates33$sign<0,-100*rates33$lo,100*rates33$hi)
rates33$percentage<-sprintf('%.1f',100*rates33$rate)
base33<-theme_classic(base_family='Arial',base_size=13)+theme(axis.text=element_text(size=12,colour='#263746'),axis.title=element_text(size=13),strip.background=element_blank(),strip.text=element_text(size=13,face='bold'),legend.position='none',plot.title=element_text(size=14,face='bold',hjust=.5),plot.margin=margin(3,4,1,2),panel.spacing=unit(4,'mm'))
mirror33<-function(labels=TRUE,intervals=FALSE){
 p<-ggplot(rates33,aes(x,outcome,fill=group))+geom_col(width=.68)+geom_vline(xintercept=0,colour='#687883',linewidth=.35)+facet_wrap(~cohort,nrow=1)+scale_fill_manual(values=ofcols)+scale_x_continuous(limits=c(-85,85),breaks=c(-80,-40,0,40,80),labels=abs)+labs(x='Incidence (%)',y=NULL)+base33
 if(intervals)p<-p+geom_errorbar(aes(xmin=xmin,xmax=xmax),orientation='y',width=.12,linewidth=.4,colour='#3B4247')
 if(labels)p<-p+geom_text(aes(x=x+sign*2,label=percentage,hjust=ifelse(sign<0,1,0)),size=3.7,family='Arial')
 p
}
raw33<-readRDS('data_private/supplemental_objects.rds')$admission
raw33<-raw33[!is.na(raw33$admission_of)&raw33$cohort %in% names(cohortlabels),]
raw33$group<-factor(raw33$admission_of,0:1,names(ofcols))
raw33$cohort<-factor(raw33$cohort,names(cohortlabels),c('Dev.','2025'))
los33<-function(variable,title){
 d<-raw33[is.finite(raw33[[variable]])&raw33[[variable]]>=0,];d$value<-d[[variable]]
 ggplot(d,aes(cohort,value,fill=group,group=interaction(cohort,group)))+
  geom_violin(scale='width',trim=TRUE,position=position_dodge(width=.78),width=.72,alpha=.78,colour=NA)+
  geom_boxplot(width=.12,position=position_dodge(width=.78),outlier.shape=NA,fill='white',linewidth=.35)+
  scale_fill_manual(values=ofcols)+scale_y_continuous(trans='log1p',breaks=c(0,1,3,7,14,30,60,120))+
  labs(x=NULL,y='Days',title=title)+base33+theme(panel.spacing=unit(0,'mm'),axis.text.x=element_text(size=12),axis.title.y=element_text(size=12),axis.text.y=element_text(size=11))
}
export33<-function(stem,g,w,h){for(ext in getOption('htgap.formats')){
 file<-paste0('figures/',stem,'.',ext)
 if(ext=='pdf')cairo_pdf(file,w/25.4,h/25.4,family='Arial')
 if(ext=='svg')svglite(file,w/25.4,h/25.4)
 if(ext=='png')png(file,width=w,height=h,units='mm',res=300,type='cairo')
 if(ext=='tiff')tiff(file,width=w,height=h,units='mm',res=600,type='cairo',compression='lzw')
 grid.newpage();grid.draw(g);dev.off()
}}
legend33<-grobTree(rectGrob(x=.37,y=.5,width=.018,height=.4,gp=gpar(fill=ofcols[1],col=NA)),textGrob('No admission OF',x=.39,y=.5,just='left',gp=gpar(fontfamily='Arial',fontsize=13)),rectGrob(x=.61,y=.5,width=.018,height=.4,gp=gpar(fill=ofcols[2],col=NA)),textGrob('Admission OF',x=.63,y=.5,just='left',gp=gpar(fontfamily='Arial',fontsize=13)))
full33<-arrangeGrob(legend33,ggplotGrob(mirror33(FALSE,TRUE)),arrangeGrob(ggplotGrob(los33('icu_los','ICU stay')),ggplotGrob(los33('hosp_los','Hospital stay')),ncol=2),ncol=1,heights=c(.06,.40,.54))
export33('Figure_S6',full33,300,215)
write.csv(rates33,'analysis/outcome_display_v33_rates.csv',row.names=FALSE)
dev.off()
