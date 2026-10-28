library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3extralearners)
library(pROC)

D <- readRDS('data_private/imputations.rds')
vars <- final_predictors
datasets <- lapply(names(D$X),function(cc) {
  data.frame(sap=factor(D$y[[cc]],levels=c(0,1)),D$X[[cc]][[1]][,vars,drop=FALSE])
})
names(datasets) <- names(D$X)
stopifnot(all(vapply(datasets,function(d) !anyNA(d),logical(1))))
task <- TaskClassif$new('HTGAP',backend=datasets$training,target='sap',positive='1')
task$col_roles$stratum <- 'sap'
set.seed(20261027)
folds <- rsmp('cv',folds=5)
folds$instantiate(task)

definitions <- list(
  logistic=list(id='classif.log_reg',parameters=list()),
  glmnet=list(id='classif.cv_glmnet',parameters=list(alpha=.5,nfolds=5L)),
  lda=list(id='classif.lda',parameters=list()),
  AdaBoostM1=list(id='classif.AdaBoostM1',parameters=list()),
  bayes_net=list(id='classif.bayes_net',parameters=list()),
  catboost=list(id='classif.catboost',parameters=list(iterations=500L,depth=4L,learning_rate=.03,verbose=FALSE)),
  extratrees=list(id='classif.ranger',parameters=list(num.trees=500L,splitrule='extratrees',num.random.splits=1L)),
  gbm=list(id='classif.gbm',parameters=list(n.trees=500L,interaction.depth=2L,shrinkage=.02,n.minobsinnode=10L)),
  kknn=list(id='classif.kknn',parameters=list(k=15L)),
  lightgbm=list(id='classif.lightgbm',parameters=list(num_iterations=500L,num_leaves=15L,learning_rate=.03,verbosity=-1L)),
  naive_bayes=list(id='classif.naive_bayes',parameters=list()),
  nnet=list(id='classif.nnet',parameters=list(size=5L,decay=.01,maxit=500L,trace=FALSE)),
  ranger=list(id='classif.ranger',parameters=list(num.trees=500L,min.node.size=5L)),
  rpart=list(id='classif.rpart',parameters=list(cp=.01,minsplit=20L)),
  svm=list(id='classif.svm',parameters=list(kernel='radial',cost=1)),
  xgboost=list(id='classif.xgboost',parameters=list(nrounds=500L,max_depth=3L,eta=.03,nthread=2L))
)
requested <- Sys.getenv('HTGAP_ALGORITHMS','')
if(nzchar(requested)) {
  selected <- strsplit(requested,',',fixed=TRUE)[[1]]
  stopifnot(all(selected %in% names(definitions)))
  definitions <- definitions[selected]
}

out <- 'machine_learning_results'
dir.create(out,showWarnings=FALSE)
pred <- lapply(datasets,function(d) data.frame(row_ids=seq_len(nrow(d)),truth=as.integer(as.character(d$sap))))
oof <- pred$training
failures <- list()
for(j in seq_along(definitions)) {
  name <- names(definitions)[j]
  spec <- definitions[[j]]
  tryCatch({
    learner <- lrn(spec$id,predict_type='prob')
    learner$param_set$values <- modifyList(learner$param_set$values,spec$parameters)
    model <- as_learner(po('scale') %>>% learner)
    set.seed(20261027+j)
    cv <- resample(task,model,folds,store_models=FALSE)
    p <- cv$prediction()
    probability <- p$prob[,'1']
    stopifnot(!anyNA(probability),!anyDuplicated(p$row_ids))
    oof[[name]] <- probability[match(seq_len(nrow(oof)),p$row_ids)]
    model$train(task)
    for(cc in names(datasets)) {
      p <- model$predict_newdata(datasets[[cc]][,vars,drop=FALSE])$prob[,'1']
      stopifnot(length(p)==nrow(pred[[cc]]),all(is.finite(p)),all(p>=0 & p<=1))
      pred[[cc]][[name]] <- p
    }
    saveRDS(model,file.path(out,paste0(name,'.rds')))
    message('Completed ',name)
  },error=function(e) {
    failures[[name]] <<- data.frame(algorithm=name,message=conditionMessage(e))
    message(name,': ',conditionMessage(e))
  })
}
if(length(failures))write.csv(do.call(rbind,failures),file.path(out,'unavailable_algorithms.csv'),row.names=FALSE)
complete <- Reduce(intersect,c(list(setdiff(names(oof),c('row_ids','truth'))),lapply(pred,function(x)setdiff(names(x),c('row_ids','truth')))))
if(!length(complete))stop('No classifier completed training and validation.')
oof <- oof[,c('row_ids','truth',complete),drop=FALSE]
pred <- lapply(pred,function(x)x[,c('row_ids','truth',complete),drop=FALSE])
metric <- function(y,p) {
  pos<-p>=.5;tp<-sum(pos & y==1);fp<-sum(pos & y==0);tn<-sum(!pos & y==0);fn<-sum(!pos & y==1)
  ratio<-function(a,b)if(b>0)a/b else NA_real_
  r<-roc(y,p,levels=c(0,1),direction='<',quiet=TRUE)
  ci<-as.numeric(ci.auc(r,method='delong'))
  data.frame(auc=as.numeric(auc(r)),auc_lower=ci[1],auc_upper=ci[3],sensitivity=ratio(tp,tp+fn),specificity=ratio(tn,tn+fp),precision=ratio(tp,tp+fp),npv=ratio(tn,tn+fn),acc=mean(pos==y),fbeta=ratio(2*tp,2*tp+fp+fn),bbrier=mean((p-y)^2))
}
sets <- c(list(oof=oof),pred)
for(cc in names(sets)) {
  z<-sets[[cc]]
  write.csv(z,file.path(out,paste0(cc,'_prediction_data.csv')),row.names=FALSE)
  tab<-do.call(rbind,lapply(complete,function(m)cbind(Model=m,metric(z$truth,z[[m]]))))
  write.csv(tab,file.path(out,paste0(cc,'_all_measures.csv')),row.names=FALSE)
}
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
