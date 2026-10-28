library(ggplot2);library(grid);library(gridExtra);library(dcurves)
folders<-c(temporal=input_file('ml','temporal'),regional=input_file('ml','regional'))
ml<-list(
  oof=read.csv(file.path(folders['temporal'],'benchmark_prediction_data.csv')),
  training=read.csv(file.path(folders['temporal'],'multi_train_prediction_data.csv')),
  temporal2025=read.csv(file.path(folders['temporal'],'multi_external.validation_prediction_data.csv')),
  rct=read.csv(file.path(folders['regional'],'multi_external.validation_prediction_data.csv')))
benchmark<-read.csv(file.path(folders['temporal'],'benchmark_all_measures.csv'))
use<-unique(c('logistic',head(benchmark$Model[order(-benchmark$auc)],4)))
labels<-c(logistic='Logistic',gbm='GBM',ranger='Random forest',catboost='CatBoost',extratrees='Extra trees',glmnet='Elastic net',svm='SVM',lda='LDA',kknn='KNN',AdaBoostM1='AdaBoost',bayes_net='Bayesian network',lightgbm='LightGBM',naive_bayes='Naive Bayes',nnet='Neural network',rpart='Decision tree',xgboost='XGBoost')
pal<-setNames(c('#326C9A','#1B8A84','#B2604C','#9D8B58','#8976A6')[seq_along(use)],use)
cn<-c(oof='Training cross-validation',training='Development',temporal2025='Temporal validation',rct='Regional validation')
for(z in ml){stopifnot(all(c('truth',use)%in%names(z)),all(z$truth%in%0:1));for(m in use)stopifnot(all(is.finite(z[[m]])),all(z[[m]]>=0 & z[[m]]<=1))}
theme_set(theme_classic(base_family='Arial',base_size=13)+theme(legend.position='bottom',legend.title=element_blank(),legend.text=element_text(size=10),plot.title=element_text(size=13,face='bold'),plot.margin=margin(8,10,8,8)))
save_ml<-function(name,g,w,h){for(ext in getOption('htgap.formats')){
  f<-paste0('figures/',name,'.',ext)
  if(ext=='pdf')cairo_pdf(f,w/25.4,h/25.4,family='Arial')
  if(ext=='svg')svglite::svglite(f,w/25.4,h/25.4)
  if(ext=='png')png(f,width=w,height=h,units='mm',res=300,type='cairo')
  if(ext=='tiff')tiff(f,width=w,height=h,units='mm',res=600,type='cairo',compression='lzw')
  grid.newpage();grid.draw(g);dev.off()
}}
