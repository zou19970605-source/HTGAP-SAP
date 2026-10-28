options(stringsAsFactors=FALSE,warn=1)
library(ggplot2);library(grid);library(gridExtra);library(pROC);library(svglite)
options(device=function(...)cairo_pdf(filename=tempfile(fileext='.pdf'),width=300/25.4,height=420/25.4,family='Arial',...))

A<-readRDS('data_private/analysis_objects.rds')
S<-readRDS('data_private/score_splines.rds')
D<-A$D;cohorts<-c('training','temporal2025','rct');comparison_cohorts<-c('training','temporal2025');M<-5L
cn<-c(training='Development',temporal2025='Temporal validation',rct='Multicentre validation')
cols<-c(training='#326C9A',temporal2025='#1B8A84',rct='#B2604C')
models<-c('Simplified score'='#B2604C','Nomogram'='#326C9A','APACHE II'='#665B8E','SIRS'='#348C78','BISAP'='#AF853D')
grades<-c('0–4','5–7','8–12');grade_cols<-setNames(c('#79B3B5','#D6AA57','#B36566'),grades)
vlab<-c(temperature='Temp',neutrophil_pct='NEU',ldh='LDH',amylase='AMY',base_excess='BE',pao2='PaO2',pf_ratio='PaO2/FiO2')
def<-S$definition
wr<-function(x,name)write.csv(x,paste0('analysis/score_',name,'.csv'),row.names=FALSE,fileEncoding='UTF-8')
wilson<-function(k,n){if(n==0)return(c(NA,NA));z<-qnorm(.975);den<-1+z*z/n;mid<-(k/n+z*z/(2*n))/den;half<-z*sqrt(k/n*(1-k/n)/n+z*z/(4*n*n))/den;c(mid-half,mid+half)}
scorefun<-function(x)Reduce('+',lapply(seq_len(nrow(def)),function(j){dd<-def[j,];as.numeric(if(dd$direction=='>=')x[,dd$variable]>=dd$cutpoint else x[,dd$variable]<=dd$cutpoint)*dd$points}))
roc_data<-function(r) data.frame(fpr=1-r$specificities,tpr=r$sensitivities)
auc_row<-function(y,p){r<-roc(y,p,quiet=TRUE,direction='<',levels=c(0,1));ci<-as.numeric(ci.auc(r,method='delong'));list(roc=r,auc=as.numeric(auc(r)),lower=ci[1],upper=ci[3])}
rdiff<-function(r1,r2){a<-as.numeric(auc(r1)-auc(r2));se<-sqrt(as.numeric(var(r1,method='delong')+var(r2,method='delong')-2*cov(r1,r2,method='delong')));c(diff=a,low=a-1.96*se,high=a+1.96*se)}
get_test<-function(values,group,binary){
 ok<-is.finite(values)&!is.na(group);values<-values[ok];group<-droplevels(group[ok])
 if(binary){
  tab<-table(group,factor(values,levels=0:1));chi<-suppressWarnings(chisq.test(tab,correct=FALSE))
  if(any(chi$expected<5)){tt<-fisher.test(tab,workspace=2e7);return(c(p=tt$p.value,method='Fisher exact'))}
  return(c(p=chi$p.value,method='Pearson chi-square'))
 }
 tt<-kruskal.test(values~group);c(p=tt$p.value,method='Kruskal–Wallis')
}

perf<-roc_all<-comparisons<-roc_comparison<-comparison_common<-distributions<-point_risks<-outcomes<-tests<-los<-components<-list()
score_sets<-list()
for(cc in cohorts){
 y<-D$y[[cc]];cl<-A$clinical[[cc]];ss<-sapply(D$X[[cc]],scorefun)
 probs<-rowMeans(plogis(S$score_coefficients[1]+S$score_coefficients[2]*ss))
 stopifnot(max(abs(probs-S$predictions[[cc]]$prediction))<1e-12,all(ss[,1]==S$predictions[[cc]]$score_first))
 sg<-cut(ss[,1],c(-Inf,4,7,Inf),labels=grades);score_sets[[cc]]<-list(score=ss[,1],group=sg,p=probs)
 r<-auc_row(y,probs)
 pp<-S$performance[S$performance$cohort==cc,];stopifnot(abs(r$auc-pp$auc)<1e-12)
 perf[[cc]]<-data.frame(cohort=cc,n=length(y),events=sum(y),auc=r$auc,lower=r$lower,upper=r$upper,brier=mean((probs-y)^2),prediction_basis='Mean predicted probability')
 roc_all[[cc]]<-cbind(cohort=cc,roc_data(r$roc))
 for(st in 0:12){
  ix<-ss[,1]==st;n<-sum(ix);k<-sum(y[ix]);ci<-wilson(k,n)
  point_risks[[length(point_risks)+1]]<-data.frame(cohort=cc,score=st,n=n,events=k,observed=if(n)k/n else NA,lower=ci[1],upper=ci[2],predicted_risk=plogis(S$score_coefficients[1]+S$score_coefficients[2]*st))
  for(yy in 0:1)distributions[[length(distributions)+1]]<-data.frame(cohort=cc,score=st,sap=yy,n=sum(ix&y==yy),percent=100*sum(ix&y==yy)/length(y))
 }
 if(cc %in% comparison_cohorts){
 available_scores<-intersect(c('apache','sirs','bisap'),names(cl))
 score_labels<-c(apache='APACHE II',sirs='SIRS',bisap='BISAP')
 comparators<-c(list(Nomogram=A$preds[[cc]]$prediction),setNames(lapply(available_scores,function(x)cl[[x]]),score_labels[available_scores]))
 for(nm in names(comparators)){
  comp<-comparators[[nm]];ix<-is.finite(comp);r1<-auc_row(y[ix],probs[ix]);r2<-auc_row(y[ix],comp[ix]);dd<-rdiff(r1$roc,r2$roc)
  tt<-roc.test(r1$roc,r2$roc,paired=TRUE,method='delong')
  comparisons[[length(comparisons)+1]]<-data.frame(cohort=cc,comparator=nm,n=sum(ix),events=sum(y[ix]),score_auc=r1$auc,score_lower=r1$lower,score_upper=r1$upper,comparator_auc=r2$auc,comparator_lower=r2$lower,comparator_upper=r2$upper,delta_auc=dd[1],delta_lower=dd[2],delta_upper=dd[3],p_value=tt$p.value)
 }
 # All models within each ROC panel are evaluated on one common available subset.
 common<-complete.cases(cl[,available_scores,drop=FALSE]);all_models<-c(list('Simplified score'=probs),comparators)
 for(nm in names(all_models)){
  rr<-auc_row(y[common],all_models[[nm]][common]);roc_comparison[[length(roc_comparison)+1]]<-cbind(cohort=cc,model=nm,n=sum(common),events=sum(y[common]),roc_data(rr$roc))
  comparison_common[[length(comparison_common)+1]]<-data.frame(cohort=cc,model=nm,n=sum(common),events=sum(y[common]),auc=rr$auc,lower=rr$lower,upper=rr$upper)
 }
 }
 for(oo in c('sap','nof','pmof','ipn','ihm','icu_los','hosp_los')){
  vv<-cl[[oo]];binary<-!oo%in%c('icu_los','hosp_los');tt<-get_test(vv,sg,binary)
  tests[[length(tests)+1]]<-data.frame(cohort=cc,outcome=oo,n=sum(is.finite(vv)),p_value=as.numeric(tt['p']),test=unname(tt['method']))
  for(g in grades){
   ix<-sg==g;z<-vv[ix];valid<-z[is.finite(z)];n<-length(valid)
   if(binary){k<-sum(valid==1);ci<-wilson(k,n);outcomes[[length(outcomes)+1]]<-data.frame(cohort=cc,group=g,outcome=oo,group_n=sum(ix),n=n,missing=sum(ix)-n,events=k,rate=k/n,lower=ci[1],upper=ci[2])}
   else {q<-quantile(valid,c(.25,.5,.75),names=FALSE);los[[length(los)+1]]<-data.frame(cohort=cc,group=g,outcome=oo,group_n=sum(ix),n=n,missing=sum(ix)-n,median=q[2],q1=q[1],q3=q[3])}
  }
 }
 for(vv in names(vlab))for(g in grades){z<-D$raw[[cc]][sg==g,vv];z<-z[is.finite(z)];q<-quantile(z,c(.25,.5,.75),names=FALSE);components[[length(components)+1]]<-data.frame(cohort=cc,group=g,variable=vv,n=length(z),median=q[2],q1=q[1],q3=q[3])}
}
perf<-do.call(rbind,perf);roc_all<-do.call(rbind,roc_all);cmp<-do.call(rbind,comparisons)
cmp$p_FDR<-NA_real_;ii<-cmp$comparator!='Nomogram';cmp$p_FDR[ii]<-p.adjust(cmp$p_value[ii],'BH')
cmp$fdr_family<-ifelse(ii,'Six paired clinical-score comparisons in the two local cohorts','Nomogram comparison reported without new multiplicity adjustment')
ccm<-do.call(rbind,comparison_common);rcomp<-do.call(rbind,roc_comparison)
dist<-do.call(rbind,distributions);risk<-do.call(rbind,point_risks);outs<-do.call(rbind,outcomes);lot<-do.call(rbind,los);tst<-do.call(rbind,tests)
tst$p_FDR<-NA_real_;sec<-tst$outcome!='sap';tst$p_FDR[sec]<-p.adjust(tst$p_value[sec],'BH');tst$fdr_family<-ifelse(sec,'18 secondary outcome comparisons','Primary SAP comparison')
wr(perf,'performance');wr(cmp,'paired_comparisons');wr(cmp[cmp$comparator!='Nomogram',],'clinical_score_comparisons');wr(ccm,'common_subset_auc');wr(rcomp,'common_subset_roc');wr(roc_all,'score_roc');wr(dist,'score_distribution');wr(risk,'point_risk');wr(outs,'grade_binary_outcomes');wr(lot,'grade_length_of_stay');wr(tst,'grade_tests');wr(do.call(rbind,components),'grade_component_summary');wr(def,'definition')

theme_set(theme_classic(base_family='Arial',base_size=14)+theme(axis.text=element_text(size=12,colour='#252B30'),axis.title=element_text(size=13),plot.title=element_text(size=15,face='bold'),plot.subtitle=element_text(size=11),legend.title=element_blank(),legend.text=element_text(size=11),legend.position='bottom',strip.background=element_blank(),strip.text=element_text(size=13,face='bold'),plot.margin=margin(7,9,7,7)))
as_cohort<-function(z){z$cohort<-factor(z$cohort,cohorts,unname(cn));z}
tagged<-function(g,tag,title=NULL){arrangeGrob(g,top=textGrob(paste0(tag,if(!is.null(title))paste0('  ',title)else''),x=unit(1,'mm'),just='left',gp=gpar(fontfamily='Arial',fontface='bold',fontsize=17)))}
card<-function(){
 labels<-unname(vlab[def$variable]);threshold<-paste(ifelse(def$direction=='>=','≥','≤'),vapply(def$cutpoint,function(x)format(x,trim=TRUE),''))
 txt<-data.frame(Predictor=labels,Threshold=threshold,Points=def$points)
 tb<-tableGrob(txt,rows=NULL,theme=ttheme_minimal(base_size=14,base_family='Arial',padding=unit(c(2.6,2.5),'mm'),core=list(fg_params=list(hjust=0,x=.05)),colhead=list(fg_params=list(fontface='bold',hjust=0,x=.05))))
 arrangeGrob(tb,textGrob('Total: 0–12 points',gp=gpar(fontfamily='Arial',fontsize=15,fontface='bold')),ncol=1,heights=c(.87,.13))
}
pr<-as_cohort(roc_all);plabs<-setNames(sprintf('%s: %.3f (%.3f–%.3f)',unname(cn[perf$cohort]),perf$auc,perf$lower,perf$upper),unname(cn))
rocplot<-ggplot(pr,aes(fpr,tpr,colour=cohort))+geom_abline(slope=1,intercept=0,colour='grey75',linetype=2)+geom_line(linewidth=.9)+scale_colour_manual(values=setNames(unname(cols),unname(cn)),labels=plabs)+scale_x_continuous(breaks=c(0,.5,1))+scale_y_continuous(breaks=c(0,.5,1))+coord_equal(xlim=c(0,1),ylim=c(0,1),expand=FALSE)+labs(x='1 − specificity',y='Sensitivity')+guides(colour=guide_legend(ncol=1))+theme(legend.text=element_text(size=10),legend.key.height=unit(3,'mm'),legend.spacing.y=unit(0,'mm'),legend.margin=margin(0,0,0,0))
rp<-as_cohort(risk[risk$n>0,]);map<-read.csv('analysis/simplified_score_risk_mapping.csv')
riskplot<-ggplot(rp,aes(score,observed,colour=cohort))+geom_line(data=map,aes(score,risk),inherit.aes=FALSE,colour='#303A43',linewidth=1)+geom_errorbar(aes(ymin=lower,ymax=upper),position=position_dodge(.45),width=.12,alpha=.45,linewidth=.45)+geom_point(position=position_dodge(.45),size=1.8)+scale_colour_manual(values=setNames(unname(cols),unname(cn)))+scale_x_continuous(breaks=seq(0,12,3))+scale_y_continuous(labels=function(x)paste0(100*x,'%'),breaks=c(0,.5,1))+coord_cartesian(ylim=c(0,1))+labs(x='Total score',y='SAP risk (%)')+guides(colour=guide_legend(ncol=1))+theme(legend.text=element_text(size=10),legend.key.height=unit(3,'mm'),legend.spacing.y=unit(0,'mm'),legend.margin=margin(0,0,0,0))

compp<-lapply(comparison_cohorts,function(cc){
 z<-rcomp[rcomp$cohort==cc,];ss<-ccm[ccm$cohort==cc,];z$model<-factor(z$model,names(models));labels<-setNames(sprintf('%s  %.3f',ss$model,ss$auc),ss$model)
 short<-ifelse(ss$model=='Simplified score','Score',ss$model)
 ggplot(z,aes(fpr,tpr,colour=model))+geom_abline(slope=1,intercept=0,colour='grey80',linetype=2)+geom_line(linewidth=.85)+scale_colour_manual(values=models,labels=labels,drop=TRUE,guide='none')+scale_x_continuous(breaks=c(0,.5,1))+scale_y_continuous(breaks=c(0,.5,1))+coord_equal(xlim=c(0,1),ylim=c(0,1),expand=FALSE)+labs(x='1 − specificity',y='Sensitivity',title=cn[cc],subtitle=sprintf('n = %s; SAP = %s',ss$n[1],ss$events[1]))+annotate('text',x=.97,y=rev(seq(.065,by=.085,length.out=nrow(ss))),label=sprintf('%s  %.3f',short,ss$auc),colour=unname(models[ss$model]),hjust=1,size=3.55,family='Arial')
})

dz<-as_cohort(dist);dz$outcome<-factor(dz$sap,c(0,1),c('No SAP','SAP'))
dp<-ggplot(dz,aes(score,percent,fill=outcome))+geom_col(width=.82)+facet_wrap(~cohort,nrow=1)+scale_fill_manual(values=c('No SAP'='#BDD0D9','SAP'='#B2604C'))+scale_x_continuous(breaks=seq(0,12,2))+scale_y_continuous(breaks=c(0,10,20,30))+labs(x='Total score',y='Admissions (%)')+theme(legend.position='bottom',legend.margin=margin(0,0,0,0),legend.key.spacing.x=unit(3,'mm'),plot.margin=margin(6,10,2,10))

oz<-as_cohort(outs);oz$group<-factor(oz$group,rev(grades));oz$outcome<-factor(oz$outcome,c('sap','nof','pmof','ipn','ihm'),c('SAP','New organ failure','Persistent MOF','Infected necrosis','In-hospital death'))
oz$cell_label<-sprintf('%.1f%%\n%d/%d',100*oz$rate,oz$events,oz$n)
op<-ggplot(oz,aes(outcome,group,fill=rate))+geom_tile(colour='white',linewidth=1.2)+geom_text(aes(label=cell_label,colour=rate>.40),size=3.6,lineheight=.98,family='Arial')+scale_colour_manual(values=c('FALSE'='#26343C','TRUE'='white'),guide='none')+scale_fill_gradient(low='#EDF2F5',high='#326C9A',limits=c(0,.75),guide='none')+facet_grid(cohort~.,switch='y')+labs(x=NULL,y='Score')+scale_x_discrete(position='top')+theme(axis.line=element_blank(),axis.ticks=element_blank(),axis.text.x=element_text(size=12),axis.text.y=element_text(size=12),strip.placement='outside',strip.text.y.left=element_text(angle=0,size=12),panel.spacing.y=unit(3,'mm'),plot.margin=margin(3,9,3,0))

ld<-do.call(rbind,lapply(cohorts,function(cc)do.call(rbind,lapply(c('icu_los','hosp_los'),function(oo)data.frame(cohort=cc,group=score_sets[[cc]]$group,outcome=oo,value=A$clinical[[cc]][[oo]])))))
ld<-ld[is.finite(ld$value),];ld<-as_cohort(ld);ld$group<-factor(ld$group,grades);ld$outcome<-factor(ld$outcome,c('icu_los','hosp_los'),c('ICU\nstay','Hospital\nstay'))
lp<-ggplot(ld,aes(group,value,fill=group))+geom_boxplot(width=.56,outlier.size=.6,outlier.alpha=.18,linewidth=.45)+facet_grid(outcome~cohort)+scale_fill_manual(values=grade_cols,guide='none')+scale_y_continuous(trans='log1p',breaks=c(0,7,30,120))+labs(x='Score',y='Length of stay (days)')+theme(strip.text.y=element_text(size=12,angle=0),panel.spacing.y=unit(3,'mm'),plot.margin=margin(5,16,3,10))

savefig<-function(g,name,w,h){
 for(ex in getOption('htgap.formats')){
  f<-paste0(name,'.',ex)
  if(ex=='pdf')cairo_pdf(f,w/25.4,h/25.4,family='Arial')
  if(ex=='svg')svglite(f,w/25.4,h/25.4,system_fonts=list(sans='Arial'))
  if(ex=='png')png(f,width=w,height=h,units='mm',res=300,type='cairo')
  if(ex=='tiff')tiff(f,width=w,height=h,units='mm',res=600,compression='lzw',type='cairo')
  grid.newpage();grid.draw(g);dev.off()
 }
}
row1<-arrangeGrob(tagged(card(),'A','Point score'),tagged(ggplotGrob(rocplot),'B','Discrimination'),tagged(ggplotGrob(riskplot),'C','Score and SAP risk'),ncol=3,widths=c(.33,.34,.33))
row2<-tagged(arrangeGrob(grobs=lapply(compp,ggplotGrob),ncol=2),'D','Comparison with clinical scores')
row3<-tagged(ggplotGrob(dp),'E','Score distribution')
row4<-tagged(ggplotGrob(op),'F','Outcomes across score groups')
row5<-tagged(ggplotGrob(lp),'G','Length of stay')
figure<-arrangeGrob(row1,row2,row3,row4,row5,ncol=1,heights=c(82,90,60,108,80))
savefig(figure,'figures/Figure_4',300,420)

if(dev.cur()>1)dev.off()
