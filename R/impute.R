library(mice)
raw <- X <- y <- list()
for (cc in cohorts) {
  d <- read.csv(input_file(paste0('clean_',cc,'.csv')),check.names=FALSE,na.strings=c('','NA'))
  stopifnot(all(c('sap',predictors) %in% names(d)),!anyNA(d$sap),all(d$sap %in% 0:1))
  raw[[cc]] <- d
  x <- d[,predictors,drop=FALSE]
  for (v in binary_predictors) x[[v]] <- factor(x[[v]],levels=0:1)
  method <- vapply(x,function(z) if(!anyNA(z)) '' else if(is.factor(z)) 'logreg' else 'pmm','')
  pm <- matrix(1L,length(predictors),length(predictors),dimnames=list(predictors,predictors));diag(pm) <- 0L
  im <- mice(x,m=5,maxit=10,method=method,predictorMatrix=pm,seed=20261027,printFlag=FALSE,remove.collinear=FALSE)
  X[[cc]] <- lapply(seq_len(5),function(i) {
    z <- as.data.frame(lapply(complete(im,i),function(a) if(is.factor(a)) as.numeric(as.character(a)) else as.numeric(a)))
    stopifnot(!anyNA(z))
    as.matrix(z)
  })
  y[[cc]] <- as.integer(d$sap)
  saveRDS(im,file.path('data_private',paste0('mice_',cc,'.rds')))
}
spec <- data.frame(standard_name=predictors,abbreviation=unname(variable_labels),type=ifelse(predictors %in% binary_predictors,'binary','numeric'))
saveRDS(list(X=X,y=y,vars=predictors,spec=spec,raw=raw,m=5),'data_private/imputations.rds')
