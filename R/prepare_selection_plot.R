options(stringsAsFactors=FALSE,warn=1)
library(mice);library(glmnet);library(jsonlite)

selection<-readRDS('data_private/selection.rds')
D<-readRDS('data_private/imputations.rds')
X<-D$X$training;y<-D$y$training;raw<-D$raw$training;spec<-D$spec;M<-5L
labels<-setNames(spec$abbreviation,D$vars);labels['pao2']<-'PaO₂';labels['pf_ratio']<-'PaO₂/FiO₂'
vars<-selection$vars
A<-selection$sets$Logistic;L<-selection$sets$LASSO;B<-selection$sets$Boruta
T<-Reduce(intersect,list(A,L,B));target<-T
binary<-c('hypertension','diabetes','hyperlipidemia')
units<-vapply(vars,function(v)if(v %in% binary)1 else mean(vapply(X,function(x)sd(x[,v]),numeric(1))),numeric(1))
fitone<-function(vs)lapply(X,function(x){d<-data.frame(sap=y,x[,vs,drop=FALSE]);glm(reformulate(vs,'sap'),data=d,family=binomial(),control=glm.control(maxit=100))})
poolfit<-function(fs)as.data.frame(summary(mice::pool(fs),conf.int=TRUE))
uni<-list();uni_fits<-list()
for(v in vars){fs<-fitone(v);z<-poolfit(fs);z<-z[z$term==v,];z$variable<-v;uni[[v]]<-z;uni_fits[[v]]<-fs}
adj_fits<-fitone(A);adj<-poolfit(adj_fits);adj<-adj[adj$term!='(Intercept)',];adj$variable<-as.character(adj$term)
forest<-do.call(rbind,lapply(seq_along(vars),function(i){
 v<-vars[i];u<-uni[[v]];a<-adj[adj$variable==v,]
 data.frame(variable=v,label=labels[v],row=i,scale=units[v],contrast=if(v %in% binary)'present vs absent' else 'per SD',
  crude_logOR=u$estimate*units[v],crude_OR=exp(u$estimate*units[v]),crude_lower=exp(u$`2.5 %`*units[v]),crude_upper=exp(u$`97.5 %`*units[v]),crude_P=u$p.value,
  adjusted_logOR=if(nrow(a))a$estimate*units[v] else NA_real_,adjusted_OR=if(nrow(a))exp(a$estimate*units[v]) else NA_real_,
  adjusted_lower=if(nrow(a))exp(a$`2.5 %`*units[v]) else NA_real_,adjusted_upper=if(nrow(a))exp(a$`97.5 %`*units[v]) else NA_real_,
  adjusted_P=if(nrow(a))a$p.value else NA_real_,AIC_selected=v %in% A,intersection=v %in% T)
}))
write.csv(forest,'source_data/forest_plot.csv',row.names=FALSE,fileEncoding='UTF-8')
histories<-list();bf<-selection$frequency
bf$decision<-ifelse(bf$confirmed>=3,'Confirmed',ifelse(bf$confirmed+bf$tentative>=3,'Tentative','Rejected'))
for(i in 1:M){b<-selection$boruta[[i]];h<-b$ImpHistory
 for(v in colnames(h)){zz<-which(is.finite(h[,v]));histories[[length(histories)+1L]]<-data.frame(imputation=i,iteration=zz,variable=v,importance=h[zz,v])}}
bh<-do.call(rbind,histories)
bi<-do.call(rbind,lapply(unique(bh$variable),function(v){z<-bh$importance[bh$variable==v];data.frame(variable=v,label=if(v %in% vars)labels[v] else v,median=median(z),
 decision=if(v %in% vars)bf$decision[match(v,bf$variable)] else 'Shadow',n_recorded=length(z))}))
bi<-bi[order(-bi$median),];row.names(bi)<-NULL
write.csv(bh,'source_data/boruta_importance_history.csv',row.names=FALSE);write.csv(bi,'source_data/boruta_plot_decisions.csv',row.names=FALSE,fileEncoding='UTF-8')
write.csv(bf,'source_data/boruta_selection_frequency.csv',row.names=FALSE)
cv<-selection$cv
cvtab<-do.call(rbind,lapply(1:M,function(i){z<-cv[[i]];data.frame(imputation=i,lambda=z$lambda,log_lambda=log(z$lambda),auc=z$cvm,se=z$cvsd,nzero=z$nzero,
 lambda_min=z$lambda.min,lambda_1se=z$lambda.1se)}))
write.csv(cvtab,'source_data/lasso_cross_validation.csv',row.names=FALSE)
membership<-data.frame(variable=vars,label=labels[vars],Logistic=vars %in% A,LASSO=vars %in% L,Boruta=vars %in% B,intersection=vars %in% T)
write.csv(membership,'source_data/selection_membership.csv',row.names=FALSE,fileEncoding='UTF-8')
combinations<-aggregate(rep(1,nrow(membership)),membership[,c('Logistic','LASSO','Boruta')],sum);names(combinations)[4]<-'n';write.csv(combinations,'source_data/intersection_counts.csv',row.names=FALSE)
corr<-matrix(NA_real_,length(target),length(target),dimnames=list(target,target));ns<-corr
for(a in target)for(b in target){ok<-complete.cases(raw[,c(a,b)]);ns[a,b]<-sum(ok);corr[a,b]<-cor(raw[[a]][ok],raw[[b]][ok],method='spearman')}
corlong<-expand.grid(variable1=target,variable2=target);corlong$rho<-as.vector(corr);corlong$n<-as.vector(ns)
write.csv(corlong,'source_data/spearman_correlations.csv',row.names=FALSE)
saveRDS(list(forest=forest,uni_fits=uni_fits,adj_fits=adj_fits,boruta_history=bh,boruta_info=bi,boruta_frequency=bf,cv=cv,cvtab=cvtab,
 labels=labels,vars=vars,target=target,sets=list(Logistic=A,LASSO=L,Boruta=B),membership=membership,correlations=corr,pair_n=ns,observed=raw[,target],n=length(y),events=sum(y)),
 'source_data/figure_data.rds')
