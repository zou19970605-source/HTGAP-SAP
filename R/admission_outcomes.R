raw <- read.csv(input_file('admission_comparison.csv'))
raw <- subset(raw,cohort %in% c('training','temporal2025') & !is.na(admission_of))
stopifnot(all(raw$admission_of %in% 0:1))
wilson <- function(k,n){if(!n)return(c(NA,NA));z<-qnorm(.975);d<-1+z*z/n;m<-(k/n+z*z/(2*n))/d;h<-z*sqrt(k/n*(1-k/n)/n+z*z/(4*n*n))/d;c(m-h,m+h)}
rows <- list()
for(cc in c('training','temporal2025'))for(o in c('sap','pmof','ipn','ihm')){
  z<-raw[raw$cohort==cc,];counts<-lapply(0:1,function(g){x<-z[[o]][z$admission_of==g];x<-x[is.finite(x)];c(n=length(x),k=sum(x==1))})
  a<-counts[[1]];b<-counts[[2]];ci0<-wilson(a['k'],a['n']);ci1<-wilson(b['k'],b['n'])
  rows[[length(rows)+1]]<-data.frame(cohort=cc,outcome=o,n_no_OF=a['n'],n_OF=b['n'],risk_no_OF=a['k']/a['n'],risk_OF=b['k']/b['n'],no_OF_lower=ci0[1],no_OF_upper=ci0[2],OF_lower=ci1[1],OF_upper=ci1[2])
}
write.csv(do.call(rbind,rows),'analysis/admission_OF_effects.csv',row.names=FALSE)
saveRDS(list(admission=raw),'data_private/supplemental_objects.rds')
