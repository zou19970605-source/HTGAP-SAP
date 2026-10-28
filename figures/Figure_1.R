library(grid);library(svglite)
flow <- read.csv(input_file('cohort_flow_counts.csv'))
flow <- flow[match(c('training','temporal2025','rct'),flow$cohort),]
stopifnot(!anyNA(flow$cohort),all(flow$HTGAP_start-flow$onset_over_72h-flow$incomplete_admission_OF-flow$admission_OF==flow$included))
boxes <- list(development_start=c(15, 16, 175, 87),temporal_start=c(279, 16, 573, 85),regional_start=c(701, 13, 862, 82),development_exclusions=c(169, 87, 383, 250),temporal_exclusions=c(506, 86, 714, 244),regional_exclusions=c(857, 80, 1067, 242),development_final=c(15, 257, 205, 306),temporal_final=c(343, 252, 533, 305),regional_final=c(704, 246, 869, 300),total=c(15, 345, 873, 393))
arrows <- list(c(91,87,91,254),c(91,156,163,156),c(91,306,91,343),c(425,85,425,249),c(425,151,503,151),c(425,305,425,343),c(781,82,779,244),c(780,146,855,146),c(781,300,781,343))
number <- function(x) format(x,big.mark=',',scientific=FALSE,trim=TRUE)
draw_flow <- function() {
  W<-1074;H<-405;grid.newpage()
  gp<-function(bold=FALSE)gpar(fontfamily='Times New Roman',fontsize=12,fontface=if(bold)'bold'else'plain',lineheight=1.10)
  for(a in arrows)grid.lines(c(a[1],a[3])/W,1-c(a[2],a[4])/H,arrow=arrow(length=unit(1.5,'mm'),type='closed'),gp=gpar(lwd=.75,fill='black'))
  for(id in names(boxes)){b<-boxes[[id]];grid.roundrect(mean(b[c(1,3)])/W,1-mean(b[c(2,4)])/H,diff(b[c(1,3)])/W,diff(b[c(2,4)])/H,r=unit(if(grepl('exclusions',id))7 else 3,'mm'),gp=gpar(fill='white',col='black',lwd=1))}
  text<-function(id,s){b<-boxes[[id]];grid.text(s,mean(b[c(1,3)])/W,1-mean(b[c(2,4)])/H,gp=gp())}
  start<-c(' HTG-AP\nadmissions in Jiangxi\n(2020–2024)',' HTG-AP admissions\nJiangxi cohort\n(2025)',' HTG-AP patients\nMulticentre RCT\n(2019–2023)')
  final<-c('Development cohort','Temporal validation','Regional validation')
  ids<-c('development','temporal','regional')
  for(i in seq_len(3)){
    z<-flow[i,];id<-ids[i]
    text(paste0(id,'_start'),paste0(number(z$HTGAP_start),start[i]))
    lines<-c(paste(number(z$HTGAP_start-z$included),'excluded'))
    if(z$onset_over_72h>0)lines<-c(lines,paste0('Onset >72 h (n = ',number(z$onset_over_72h),')'))
    lines<-c(lines,'Incomplete admission OF',paste0('assessment (n = ',number(z$incomplete_admission_OF),')'),paste0('OF at admission (n = ',number(z$admission_OF),')'))
    b<-boxes[[paste0(id,'_exclusions')]]
    for(j in seq_along(lines))grid.text(lines[j],(b[1]+12)/W,1-(b[2]+24+(j-1)*25)/H,just='left',gp=gp(j==1))
    text(paste0(id,'_final'),paste0(final[i],'\nNo OF (n = ',number(z$included),')'))
  }
  text('total',paste0('A total of ',number(sum(flow$included)),' admissions were included across the three cohorts'))
}
w<-300;h<-w*405/1074
for(ext in getOption('htgap.formats')){
  f<-paste0('figures/Figure_1.',ext)
  if(ext=='pdf')cairo_pdf(f,w/25.4,h/25.4,family='Times New Roman')
  if(ext=='svg')svglite(f,w/25.4,h/25.4)
  if(ext=='png')png(f,width=w,height=h,units='mm',res=300,type='cairo')
  if(ext=='tiff')tiff(f,width=w,height=h,units='mm',res=600,type='cairo',compression='lzw')
  draw_flow();dev.off()
}
