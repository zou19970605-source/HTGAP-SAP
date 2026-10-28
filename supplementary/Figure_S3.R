source_code('R','selection_plots.R')
allcv<-list()
for(i in seq_along(F$cv)) {
  col<-(i-1)%%2;row<-(i-1)%/%2
  p<-capture(cv_panel,i=i,letter=LETTERS[i],w=92,h=72,title='',y_limits=c(.50,.90),annotate_selection=FALSE)
  allcv<-c(allcv,move(p,5+col*98,5+row*78))
}
export(allcv,'Figure_S3',195,239,TRUE,300)
dev.off()
