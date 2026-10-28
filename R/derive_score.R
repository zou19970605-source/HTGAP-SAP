options(stringsAsFactors=FALSE,warn=1)
library(pROC);library(rms);library(mice);library(jsonlite)
A<-readRDS('data_private/analysis_objects.rds');D<-A$D;v<-A$v;M<-5L
cohorts<-names(D$X);rounding<-setNames(c(.1,1,10,10,.5,5,10),v)
cut_rows<-list();cuts<-setNames(numeric(7),v);direction<-sign(A$b[v])
for(vv in v){
 z<-sapply(1:M,function(i){r<-roc(D$y$training,D$X$training[[i]][,vv],direction=if(direction[vv]>0)'<' else '>',quiet=TRUE,levels=c(0,1));th<-coords(r,'best',best.method='youden',ret='threshold')$threshold;median(th[is.finite(th)])})
 cuts[vv]<-round(median(z)/rounding[vv])*rounding[vv]
 cut_rows[[vv]]<-data.frame(variable=vv,cutpoint=cuts[vv],direction=if(direction[vv]>0)'>=' else '<=',imputation_cutpoint_median=median(z),rounding_increment=rounding[vv])
}
binary_data<-function(x){a<-sapply(v,function(vv)if(direction[vv]>0)as.integer(x[,vv]>=cuts[vv]) else as.integer(x[,vv]<=cuts[vv]));as.data.frame(a)}
binary_fits<-lapply(1:M,function(i)glm(reformulate(v,'sap'),data=data.frame(sap=D$y$training,binary_data(D$X$training[[i]])),family=binomial()))
bp<-mice::pool(mice::as.mira(binary_fits));bs<-summary(bp,conf.int=TRUE);beta<-rowMeans(sapply(binary_fits,coef))[-1]
# Round coefficient-proportional weights using 0.5 log-odds units per point.
weights<-round(beta/.5);signflip<-weights<0;weights_abs<-abs(weights)
scorefun<-function(x){z<-as.matrix(binary_data(x));if(any(signflip))z[,signflip]<-1-z[,signflip,drop=FALSE];as.numeric(z%*%weights_abs)}
score_tr<-sapply(D$X$training,scorefun)
scorefits<-lapply(1:M,function(i)glm(D$y$training~score_tr[,i],family=binomial()));score_b<-rowMeans(sapply(scorefits,coef))
score_def<-do.call(rbind,cut_rows);score_def$binary_coefficient<-beta[score_def$variable];score_def$points<-weights_abs[score_def$variable];score_def$direction[signflip]<-ifelse(score_def$direction[signflip]=='>=','<','>')
write.csv(score_def,'analysis/simplified_score_definition.csv',row.names=FALSE)
write.csv(bs,'analysis/simplified_score_binary_regression.csv',row.names=FALSE)
performance<-list();pred<-list();risk<-list();point_risk<-list();roc_score<-list()
for(c in cohorts){
 ss<-sapply(D$X[[c]],scorefun);pp<-plogis(score_b[1]+score_b[2]*ss);p<-rowMeans(pp);y<-D$y[[c]]
 r<-roc(y,p,direction='<',levels=c(0,1),quiet=TRUE);ci<-as.numeric(ci.auc(r));rc<-roc.test(A$rocs[[c]],r,paired=TRUE,method='delong');lp<-qlogis(p);cal<-glm(y~lp,family=binomial());ca<-glm(y~1+offset(lp),family=binomial())
 performance[[c]]<-data.frame(cohort=c,n=length(y),events=sum(y),auc=as.numeric(auc(r)),auc_low=ci[1],auc_high=ci[3],brier=mean((p-y)^2),calibration_intercept=coef(ca)[1],calibration_slope=coef(cal)[2],difference_nomogram_minus_score=as.numeric(auc(A$rocs[[c]])-auc(r)),delong_p=rc$p.value)
 pred[[c]]<-data.frame(row=seq_along(y),sap=y,score_mean=rowMeans(ss),score_first=ss[,1],prediction=p)
 roc_score[[c]]<-r
 for(s in sort(unique(ss[,1]))){ix<-ss[,1]==s;point_risk[[length(point_risk)+1]]<-data.frame(cohort=c,score=s,n=sum(ix),events=sum(y[ix]),rate=mean(y[ix]),predicted=plogis(score_b[1]+score_b[2]*s))}
}
write.csv(do.call(rbind,performance),'analysis/simplified_score_performance.csv',row.names=FALSE)
write.csv(do.call(rbind,point_risk),'analysis/simplified_score_observed_risk.csv',row.names=FALSE)
write.csv(data.frame(score=0:sum(weights_abs),risk=plogis(score_b[1]+score_b[2]*(0:sum(weights_abs)))),'analysis/simplified_score_risk_mapping.csv',row.names=FALSE)

# Restricted cubic splines describe development-cohort associations adjusted for the other six predictors.
curves<-list();tests<-list();knots_list<-list()
for(vv in v){
 knots<-as.numeric(quantile(D$X$training[[1]][,vv],c(.1,.5,.9)));knots_list[[vv]]<-knots
 grid<-seq(quantile(D$X$training[[1]][,vv],.02),quantile(D$X$training[[1]][,vv],.98),length.out=120)
 Q<-U<-matrix(NA,length(grid),M);pvals<-numeric(M)
 for(i in 1:M){
  x<-as.data.frame(D$X$training[[i]][,v]);z<-data.frame(sap=D$y$training,x[,setdiff(v,vv),drop=FALSE]);basis<-rcspline.eval(x[[vv]],knots=knots,inclx=TRUE);z$s1<-basis[,1];z$s2<-basis[,2]
  f<-glm(reformulate(c(setdiff(v,vv),'s1','s2'),'sap'),data=z,family=binomial());f0<-glm(reformulate(c(setdiff(v,vv),'s1'),'sap'),data=z,family=binomial());pvals[i]<-anova(f0,f,test='LRT')$`Pr(>Chi)`[2]
  ref<-median(x[[vv]]);br<-rcspline.eval(ref,knots=knots,inclx=TRUE);bg<-rcspline.eval(grid,knots=knots,inclx=TRUE);delta<-sweep(bg,2,as.numeric(br));bb<-coef(f)[c('s1','s2')];V<-vcov(f)[c('s1','s2'),c('s1','s2')]
  Q[,i]<-delta%*%bb;U[,i]<-rowSums((delta%*%V)*delta)
 }
 q<-rowMeans(Q);vari<-rowMeans(U)+(1+1/M)*apply(Q,1,var);curves[[vv]]<-data.frame(variable=vv,value=grid,OR=exp(q),lower=exp(q-1.96*sqrt(vari)),upper=exp(q+1.96*sqrt(vari)))
 tests[[vv]]<-data.frame(variable=vv,imputation=1:M,nonlinearity_p=pvals)
}
write.csv(do.call(rbind,curves),'analysis/spline_curves.csv',row.names=FALSE);write.csv(do.call(rbind,tests),'analysis/spline_nonlinearity_by_imputation.csv',row.names=FALSE)
saveRDS(list(definition=score_def,performance=do.call(rbind,performance),predictions=pred,point_risk=do.call(rbind,point_risk),score_coefficients=score_b,score_rocs=roc_score,max_score=sum(weights_abs),curves=do.call(rbind,curves),knots=knots_list),'data_private/score_splines.rds')
