library(mice);library(glmnet);library(Boruta);library(ranger)
D <- readRDS('data_private/imputations.rds')
X <- D$X$training;y <- D$y$training;M <- length(X);vars <- candidate26
fold_path <- input_file('selection_folds.csv')
if (file.exists(fold_path)) {
  folds <- read.csv(fold_path)
  stopifnot(nrow(folds)==length(y),identical(as.integer(folds$sap),as.integer(y)))
  fold <- folds$fold
} else {
  set.seed(20261027);fold <- integer(length(y))
  for (g in 0:1) {ix <- which(y==g);fold[ix] <- sample(rep(1:5,length.out=length(ix)))}
}
stopifnot(setequal(unique(fold),1:5))
backward <- boruta <- cv <- vector('list',M)
for (i in seq_len(M)) {
  d <- data.frame(sap=y,X[[i]][,vars,drop=FALSE])
  f <- glm(reformulate(vars,'sap'),data=d,family=binomial(),control=glm.control(maxit=100))
  backward[[i]] <- step(f,direction='backward',k=2,trace=0)
  cv[[i]] <- cv.glmnet(X[[i]][,vars,drop=FALSE],y,family='binomial',alpha=1,foldid=fold,nfolds=5,type.measure='auc',standardize=TRUE,parallel=FALSE,nlambda=100)
  set.seed(20261027+i)
  boruta[[i]] <- Boruta(x=as.data.frame(X[[i]][,vars,drop=FALSE]),y=factor(y,levels=0:1),pValue=.01,mcAdj=TRUE,maxRuns=80,doTrace=0,holdHistory=TRUE,getImp=getImpRfZ,ntree=300,num.threads=2)
}
frequency <- function(sets) setNames(vapply(vars,function(v) sum(vapply(sets,function(s) v %in% s,logical(1))),integer(1)),vars)
a <- frequency(lapply(backward,function(f) setdiff(names(coef(f)),'(Intercept)')))
l <- frequency(lapply(cv,function(f) {b <- as.matrix(coef(f,s='lambda.min'));setdiff(rownames(b)[b[,1]!=0],'(Intercept)')}))
cf <- frequency(lapply(boruta,function(b) names(b$finalDecision)[b$finalDecision=='Confirmed']))
tf <- frequency(lapply(boruta,function(b) names(b$finalDecision)[b$finalDecision=='Tentative']))
sets <- list(Logistic=vars[a>=3],LASSO=vars[l>=3],Boruta=vars[cf+tf>=3])
freq <- data.frame(variable=vars,confirmed=cf,tentative=tf,rejected=M-cf-tf)
saveRDS(list(vars=vars,sets=sets,backward=backward,cv=cv,boruta=boruta,frequency=freq,fold=fold),'data_private/selection.rds')
