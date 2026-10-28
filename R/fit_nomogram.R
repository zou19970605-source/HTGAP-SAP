options(stringsAsFactors=FALSE,warn=1)
library(pROC);library(mice);library(rms);library(ggplot2);library(jsonlite)
if(!requireNamespace('dcurves',quietly=TRUE))stop('dcurves required')
D<-readRDS('data_private/imputations.rds')
cohorts<-names(D$X); M<-5L
v<-c('temperature','neutrophil_pct','ldh','amylase','base_excess','pao2','pf_ratio')
label<-setNames(D$spec$abbreviation,D$vars)
cohort_names<-c(training='Development',temporal2025='Temporal validation',rct='Regional validation')
clinical<-setNames(lapply(cohorts,function(c)read.csv(input_file(paste0('clinical_',c,'.csv')))),cohorts)
for(c in cohorts)stopifnot(identical(as.numeric(clinical[[c]]$sap),as.numeric(D$y[[c]])))
write_out<-function(x,file)write.csv(x,paste0('analysis/',file,'.csv'),row.names=FALSE,fileEncoding='UTF-8')
fits<-lapply(1:M,function(i)glm(reformulate(v,'sap'),data=data.frame(sap=D$y$training,D$X$training[[i]][,v]),family=binomial()))
Q<-sapply(fits,coef);b<-rowMeans(Q);ubar<-Reduce('+',lapply(fits,vcov))/M;B<-cov(t(Q));Tmat<-ubar+(1+1/M)*B
pooled<-mice::pool(mice::as.mira(fits));coeftab<-summary(pooled,conf.int=TRUE)
coeftab$OR<-exp(coeftab$estimate);coeftab$OR_low<-exp(coeftab$`2.5 %`);coeftab$OR_high<-exp(coeftab$`97.5 %`)
coeftab$increment<-c(1,1,10,100,100,5,10,100)
coeftab$scaled_OR<-exp(coeftab$estimate*coeftab$increment);coeftab$scaled_low<-exp(coeftab$`2.5 %`*coeftab$increment);coeftab$scaled_high<-exp(coeftab$`97.5 %`*coeftab$increment)
write_out(coeftab,'pooled_coefficients')
metric<-function(y,p){
 y<-as.numeric(y);p<-as.numeric(p)
 r<-pROC::roc(y,p,levels=c(0,1),direction='<',quiet=TRUE);ci<-as.numeric(ci.auc(r,method='delong'))
 lp<-qlogis(pmin(pmax(p,1e-8),1-1e-8));f<-glm(y~lp,family=binomial())
 # Solve the intercept likelihood score directly: default IRLS can diverge for extreme offsets.
 a<-uniroot(function(alpha)mean(plogis(alpha+lp))-mean(y),c(-60,60),tol=1e-12)$root
 calibrated<-plogis(a+lp);ase<-1/sqrt(sum(calibrated*(1-calibrated)))
 stopifnot(abs(mean(calibrated)-mean(y))<1e-9)
 s<-coef(f)[2];sse<-sqrt(vcov(f)[2,2])
 if(as.numeric(auc(r))>=1-1e-12 || !isTRUE(f$converged)){s<-NA_real_;sse<-NA_real_}
 data.frame(n=length(y),events=sum(y),event_rate=mean(y),auc=as.numeric(auc(r)),auc_low=ci[1],auc_high=ci[3],brier=mean((y-p)^2),brier_null=mean(y)*(1-mean(y)),mean_predicted=mean(p),O_E=sum(y)/sum(p),calibration_intercept=a,intercept_low=a-1.96*ase,intercept_high=a+1.96*ase,calibration_slope=s,slope_low=s-1.96*sse,slope_high=s+1.96*sse,log_loss=-mean(y*log(pmax(p,1e-8))+(1-y)*log(pmax(1-p,1e-8))))
}
wilson<-function(k,n){if(n==0)return(c(NA,NA));z<-qnorm(.975);d<-1+z^2/n; a<-(k/n+z^2/(2*n))/d; h<-z*sqrt(k/n*(1-k/n)/n+z^2/(4*n^2))/d;c(max(0,a-h),min(1,a+h))}
preds<-list();metrics<-list();rocs<-list();cal<-list();calbins<-list();dca<-list();risk<-list();thresholds<-list();mimet<-list()
for(c in cohorts){
 xi<-D$X[[c]];y<-D$y[[c]]
 pp<-sapply(xi,function(x)plogis(cbind(1,x[,v])%*%b));p<-rowMeans(pp)
 preds[[c]]<-data.frame(row=seq_along(y),sap=y,prediction=p,risk_group=cut(p,c(-Inf,.1,.3,Inf),right=FALSE,labels=c('Low','Intermediate','High')))
 metrics[[c]]<-cbind(cohort=c,metric(y,p));rocs[[c]]<-pROC::roc(y,p,levels=c(0,1),direction='<',quiet=TRUE)
 lp<-qlogis(pmin(pmax(p,1e-8),1-1e-8));flex<-glm(y~splines::ns(lp,df=3),family=binomial())
 grid<-seq(max(.001,min(p)),min(.99,max(p)),length.out=201); pr<-predict(flex,newdata=data.frame(lp=qlogis(grid)),type='link',se.fit=TRUE)
 cal[[c]]<-data.frame(cohort=c,predicted=grid,observed=plogis(pr$fit),lower=plogis(pr$fit-1.96*pr$se.fit),upper=plogis(pr$fit+1.96*pr$se.fit))
 bin<-pmin(10,ceiling(rank(p,ties.method='first')/length(p)*10))
 calbins[[c]]<-do.call(rbind,lapply(split(seq_along(p),bin),function(ix){ci<-wilson(sum(y[ix]),length(ix));data.frame(cohort=c,n=length(ix),predicted=mean(p[ix]),observed=mean(y[ix]),lower=ci[1],upper=ci[2])}))
 dc<-dcurves::dca(sap~Nomogram,data=data.frame(sap=y,Nomogram=p),thresholds=seq(.01,.5,.01))
 dca[[c]]<-cbind(cohort=c,as.data.frame(dc$dca))
 for(g in levels(preds[[c]]$risk_group)){
   ix<-which(preds[[c]]$risk_group==g);cl<-clinical[[c]][ix,,drop=FALSE]
   for(o in c('sap','nof','pmof','ipn','ihm','icu_los','hosp_los')){
     vals<-cl[[o]]; ok<-!is.na(vals);nn<-sum(ok)
     if(o %in% c('icu_los','hosp_los')){qu<-quantile(vals,c(.25,.5,.75),na.rm=TRUE);re<-data.frame(cohort=c,group=g,outcome=o,n=length(ix),available=nn,events=NA,rate=NA,lower=NA,upper=NA,median=qu[2],q1=qu[1],q3=qu[3])}
     else {kk<-sum(vals==1,na.rm=TRUE);ci<-wilson(kk,nn);re<-data.frame(cohort=c,group=g,outcome=o,n=length(ix),available=nn,events=kk,rate=kk/nn,lower=ci[1],upper=ci[2],median=NA,q1=NA,q3=NA)}
     risk[[length(risk)+1]]<-re
   }
 }
 for(i in 1:M)mimet[[length(mimet)+1]]<-cbind(cohort=c,imputation=i,metric(y,plogis(cbind(1,xi[[i]][,v])%*%coef(fits[[i]]))))
 for(t in c(.05,.1,.2,.3,.5)){
   pos<-p>=t;tp<-sum(pos&y==1);fp<-sum(pos&y==0);tn<-sum(!pos&y==0);fn<-sum(!pos&y==1)
   thresholds[[length(thresholds)+1]]<-data.frame(cohort=c,threshold=t,TP=tp,FP=fp,TN=tn,FN=fn,sensitivity=tp/(tp+fn),specificity=tn/(tn+fp),PPV=ifelse(tp+fp>0,tp/(tp+fp),NA),NPV=tn/(tn+fn))
 }
}
metrics<-do.call(rbind,metrics);write_out(metrics,'model_performance');write_out(do.call(rbind,cal),'calibration_curve');write_out(do.call(rbind,calbins),'calibration_groups');write_out(do.call(rbind,dca),'decision_curves');write_out(do.call(rbind,risk),'risk_group_outcomes');write_out(do.call(rbind,thresholds),'threshold_performance');write_out(do.call(rbind,mimet),'imputation_performance')
for(c in cohorts)write.csv(preds[[c]],paste0('data_private/predictions_',c,'.csv'),row.names=FALSE)

# Baseline characteristics use observed, non-imputed measurements.
baseline<-list();missing<-list();group_base<-list();outcomes<-list()
for(vv in c('sex','age','bmi','onset_days',D$vars[!D$vars %in% c('age','bmi')])){
 for(c in cohorts){
  x<-if(vv %in% names(clinical[[c]]))clinical[[c]][[vv]] else D$raw[[c]][[vv]]
  binary<-vv %in% c('sex','hypertension','diabetes','hyperlipidemia');n<-sum(!is.na(x));miss<-sum(is.na(x));q<-if(binary)rep(NA,3)else as.numeric(quantile(x,c(.25,.5,.75),na.rm=TRUE))
  baseline[[length(baseline)+1]]<-data.frame(cohort=c,variable=vv,available=n,missing=miss,missing_pct=100*miss/length(x),binary=binary,count=if(binary)sum(x==1,na.rm=TRUE)else NA,percent=if(binary)mean(x==1,na.rm=TRUE)*100 else NA,median=q[2],q1=q[1],q3=q[3])
  if(vv %in% D$vars)missing[[length(missing)+1]]<-data.frame(cohort=c,variable=vv,missing=miss,n=length(x),missing_pct=100*miss/length(x))
 }
 x<-if(vv %in% names(clinical$training))clinical$training[[vv]] else D$raw$training[[vv]]; y<-D$y$training
 pv<-if(binary){tryCatch(fisher.test(table(x,y))$p.value,error=function(e)NA_real_)}else suppressWarnings(wilcox.test(x~y)$p.value)
 for(g in 0:1){xx<-x[y==g];nn<-sum(!is.na(xx));q<-if(binary)rep(NA,3)else as.numeric(quantile(xx,c(.25,.5,.75),na.rm=TRUE));group_base[[length(group_base)+1]]<-data.frame(variable=vv,sap=g,available=nn,missing=sum(is.na(xx)),binary=binary,count=if(binary)sum(xx==1,na.rm=TRUE)else NA,percent=if(binary)mean(xx==1,na.rm=TRUE)*100 else NA,median=q[2],q1=q[1],q3=q[3],p_value=pv)}
}
for(c in cohorts)for(o in c('sap','nof','pmof','ipn','ihm','icu_los','hosp_los')){
 x<-clinical[[c]][[o]]; nn<-sum(!is.na(x));binary<-!o %in% c('icu_los','hosp_los');q<-if(binary)rep(NA,3)else as.numeric(quantile(x,c(.25,.5,.75),na.rm=TRUE))
 outcomes[[length(outcomes)+1]]<-data.frame(cohort=c,outcome=o,n=length(x),available=nn,missing=sum(is.na(x)),events=if(binary)sum(x==1,na.rm=TRUE)else NA,percent=if(binary)100*mean(x==1,na.rm=TRUE)else NA,median=q[2],q1=q[1],q3=q[3])
}
write_out(do.call(rbind,baseline),'baseline_characteristics');write_out(do.call(rbind,group_base),'development_SAP_comparison');write_out(do.call(rbind,missing),'missingness');write_out(do.call(rbind,outcomes),'clinical_outcomes')

# Paired comparison with recorded baseline clinical scores.
scoremetrics<-list();score_roc<-list()
for(c in c('training','temporal2025')){for(s in intersect(c('sirs','apache','bisap'),names(clinical[[c]]))){
 x<-clinical[[c]][[s]];ok<-is.finite(x);yy<-D$y[[c]][ok];pp<-preds[[c]]$prediction[ok];rr<-roc(yy,x[ok],direction='<',levels=c(0,1),quiet=TRUE);rn<-roc(yy,pp,direction='<',levels=c(0,1),quiet=TRUE);ci<-as.numeric(ci.auc(rr));cc<-as.numeric(ci.auc(rn));test<-roc.test(rn,rr,method='delong',paired=TRUE)
 scoremetrics[[length(scoremetrics)+1]]<-data.frame(cohort=c,score=s,n=sum(ok),events=sum(yy),score_auc=as.numeric(auc(rr)),score_low=ci[1],score_high=ci[3],nomogram_auc=as.numeric(auc(rn)),nomogram_low=cc[1],nomogram_high=cc[3],auc_difference=as.numeric(auc(rn)-auc(rr)),p_value=test$p.value)
 score_roc[[paste(c,s)]]<-list(cohort=c,score=s,score_roc=rr,nomogram_roc=rn)
}}
scoremetrics<-do.call(rbind,scoremetrics);scoremetrics$p_FDR<-p.adjust(scoremetrics$p_value,'BH');write_out(scoremetrics,'clinical_score_comparison')

# Complete case, single oxygen indicator and demographic subgroup analyses.
sensitivity<-list();ccvars<-complete.cases(D$raw$training[,v]);ccfit<-glm(reformulate(v,'sap'),data=D$raw$training[ccvars,],family=binomial())
for(c in cohorts){
 raw<-D$raw[[c]];ok<-complete.cases(raw[,v]);pcc<-predict(ccfit,newdata=raw[ok,],type='response');sensitivity[[length(sensitivity)+1]]<-cbind(analysis='Complete-case refit',cohort=c,metric(D$y[[c]][ok],pcc))
 groups<-list('Age <=75'=raw$age<=75,'BMI <=30'=!is.na(raw$bmi)&raw$bmi<=30,'Male'=clinical[[c]]$sex==1,'Female'=clinical[[c]]$sex==0)
 for(nm in names(groups)){ix<-which(groups[[nm]]);if(sum(D$y[[c]][ix])>=5)sensitivity[[length(sensitivity)+1]]<-cbind(analysis=nm,cohort=c,metric(D$y[[c]][ix],preds[[c]]$prediction[ix]))}
}
for(omit in c('pao2','pf_ratio')){
 sv<-setdiff(v,omit);ff<-lapply(1:M,function(i)glm(reformulate(sv,'sap'),data=data.frame(sap=D$y$training,D$X$training[[i]][,sv]),family=binomial()));bb<-rowMeans(sapply(ff,coef))
 for(c in cohorts){pp<-rowMeans(sapply(D$X[[c]],function(x)plogis(cbind(1,x[,sv])%*%bb)));sensitivity[[length(sensitivity)+1]]<-cbind(analysis=if(omit=='pao2')'Without PaO2' else 'Without PaO2/FiO2',cohort=c,metric(D$y[[c]],pp))}
}
write_out(do.call(rbind,sensitivity),'sensitivity_performance')
vifs<-do.call(rbind,lapply(1:M,function(i)data.frame(imputation=i,variable=v,glm_vif=diag(solve(cov2cor(vcov(fits[[i]])[v,v]))),design_vif=diag(solve(cor(D$X$training[[i]][,v]))))))
write_out(vifs,'collinearity')

train1<-data.frame(SAP=D$y$training,D$X$training[[1]][,v]);dd<-datadist(train1);options(datadist='dd')
nomfit<-lrm(reformulate(v,'SAP'),data=train1,x=TRUE,y=TRUE);stopifnot(max(abs(coef(nomfit)-coef(fits[[1]])))<1e-4)
nomfit$coefficients<-setNames(as.numeric(b),names(nomfit$coefficients));nomfit$var<-Tmat
saveRDS(list(D=D,v=v,label=label,b=b,T=Tmat,fits=fits,pooled=pooled,coeftab=coeftab,clinical=clinical,preds=preds,metrics=metrics,rocs=rocs,cal=cal,calbins=calbins,dca=dca,risk=do.call(rbind,risk),scoremetrics=scoremetrics,score_roc=score_roc,sensitivity=do.call(rbind,sensitivity),nomfit=nomfit,dd=dd,cohort_names=cohort_names,baseline=do.call(rbind,baseline),missingness=do.call(rbind,missing)), 'data_private/analysis_objects.rds')
writeLines(capture.output(sessionInfo()),'analysis/sessionInfo.txt')
