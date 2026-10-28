options(stringsAsFactors=FALSE,warn=1)
library(rms);library(ggplot2);library(grid);library(gridExtra);library(pROC)
library(svglite)
A<-readRDS('data_private/analysis_objects.rds');D<-A$D
cols<-c(training='#326C9A',temporal2025='#1B8A84',rct='#B2604C')
groups<-c(Low='#79B3B5',Intermediate='#D6AA57',High='#B36566')
cn<-A$cohort_names
lab<-A$label;lab['respiratory_rate']<-'Respirations';lab['pao2']<-'PaO2';lab['pf_ratio']<-'PaO2/FiO2'
theme_set(theme_classic(base_size=12,base_family='Arial')+theme(axis.title=element_text(size=12),axis.text=element_text(size=10.5,colour='#20252A'),plot.title=element_text(size=13,face='bold',hjust=0),plot.subtitle=element_text(size=10),legend.title=element_blank(),legend.text=element_text(size=10.5),legend.position='bottom',strip.background=element_blank(),strip.text=element_text(size=12,face='bold'),plot.margin=margin(8,9,8,7)))
save_fig<-function(name,g,width=200,height=160){
 path<-paste0('figures/',name)
 for(ext in getOption('htgap.formats')){
  if(ext=='pdf')cairo_pdf(paste0(path,'.pdf'),width=width/25.4,height=height/25.4,family='Arial')
  if(ext=='svg')svglite(paste0(path,'.svg'),width=width/25.4,height=height/25.4,system_fonts=list(sans='Arial'))
  if(ext=='png')png(paste0(path,'.png'),width=width,height=height,units='mm',res=300,type='cairo')
  if(ext=='tiff')tiff(paste0(path,'.tiff'),width=width,height=height,units='mm',res=600,compression='lzw',type='cairo')
  if(is.function(g))g() else {grid.draw(g)}
  dev.off()
 }
}
grob<-function(p)ggplotGrob(p)
rocdata<-function(r) data.frame(fpr=1-r$specificities,tpr=r$sensitivities)
