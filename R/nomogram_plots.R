options(stringsAsFactors=FALSE,warn=1)
library(ggplot2);library(grid);library(gridExtra);library(pROC);library(jsonlite)
A<-readRDS('data_private/analysis_objects.rds')
cn<-A$cohort_names;cols<-c(training='#326C9A',temporal2025='#1B8A84',rct='#B2604C')
lab<-c(temperature='Temp',neutrophil_pct='NEU',ldh='LDH',amylase='AMY',base_excess='BE',pao2='PaO2',pf_ratio='PaO2/FiO2')
v<-names(lab);b<-A$b[v];intercept<-unname(A$b[1])
ticks<-list(temperature=seq(36,39.5,.5),neutrophil_pct=seq(20,100,20),ldh=seq(0,2500,500),amylase=seq(0,3000,500),base_excess=seq(15,-25,-5),pao2=seq(60,300,60),pf_ratio=seq(1000,300,-100))
low<-sapply(v,function(x)min(b[x]*range(ticks[[x]])))
span<-sapply(v,function(x)diff(range(b[x]*ticks[[x]])))
unit_point<-max(span)/100; origin<-intercept+sum(low)
check_error<-sapply(A$D$X,function(xx){x<-xx[[1]][,v];pt<-sweep(sweep(x,2,b,'*'),2,low,'-')/unit_point;max(abs(origin+rowSums(pt)*unit_point-(intercept+x%*%b)))})
stopifnot(max(check_error)<1e-12)

wilson<-function(k,n){z<-qnorm(.975);p<-k/n;den<-1+z*z/n;mid<-(p+z*z/(2*n))/den;h<-z*sqrt(p*(1-p)/n+z*z/(4*n*n))/den;c(max(0,mid-h),min(1,mid+h))}
impact<-list();dca<-list()
for(c in names(cn)){
  y<-A$D$y[[c]];p<-A$preds[[c]]$prediction;n<-length(y)
  for(t in seq(0,1,.01)){
    nh<-sum(p>=t);nt<-sum(p>=t & y==1);ch<-wilson(nh,n);ct<-wilson(nt,n)
    impact[[length(impact)+1]]<-data.frame(cohort=c,threshold=t,n=n,high_risk=nh,high_risk_SAP=nt,high_per_1000=1000*nh/n,SAP_per_1000=1000*nt/n,high_lower=1000*ch[1],high_upper=1000*ch[2],SAP_lower=1000*ct[1],SAP_upper=1000*ct[2])
  }
  dc<-dcurves::dca(SAP~Nomogram,data=data.frame(SAP=y,Nomogram=p),thresholds=seq(.01,.99,.01))
  zz<-as.data.frame(dc$dca);zz$cohort<-c;dca[[c]]<-zz
  own<-sapply(seq(.01,.99,.01),function(t)(sum(y==1 & p>=t)-sum(y==0 & p>=t)*t/(1-t))/n)
  stopifnot(max(abs(own-zz$net_benefit[zz$variable=='Nomogram']))<1e-12)
}
impact<-do.call(rbind,impact);write.csv(impact,'analysis/clinical_impact_curves.csv',row.names=FALSE)
write.csv(do.call(rbind,dca),'analysis/decision_curves_full_range.csv',row.names=FALSE)

theme_set(theme_classic(base_size=10.5,base_family='Arial')+theme(axis.title=element_text(size=10.5),axis.text=element_text(size=9.5,colour='#20252A'),plot.title=element_text(size=11.5,face='bold',hjust=.5),legend.title=element_blank(),legend.text=element_text(size=9),legend.position='bottom',legend.key.width=unit(3.5,'mm'),legend.key.height=unit(2.8,'mm'),legend.spacing.x=unit(1,'mm'),plot.margin=margin(3,5,3,3)))
rd<-function(r)data.frame(x=1-r$specificities,y=r$sensitivities)
rocplot<-function(c){m<-A$metrics[A$metrics$cohort==c,];ggplot(rd(A$rocs[[c]]),aes(x,y))+geom_abline(slope=1,intercept=0,colour='grey75',linetype=2)+geom_line(colour=cols[c],linewidth=.85)+scale_x_continuous(breaks=c(0,.5,1))+scale_y_continuous(breaks=c(0,.5,1))+coord_cartesian(xlim=c(-.025,1.035),ylim=c(0,1),expand=FALSE)+annotate('text',x=.53,y=.23,label=sprintf('AUC %.3f\n95%% CI %.3f–%.3f',m$auc,m$auc_low,m$auc_high),size=2.9,family='Arial')+labs(x='1 - specificity',y='Sensitivity',title='ROC')}
calplot<-function(c){z<-A$cal[[c]];ggplot(z,aes(predicted,observed))+geom_abline(slope=1,intercept=0,colour='grey60',linetype=2)+geom_ribbon(aes(ymin=lower,ymax=upper),fill=cols[c],alpha=.14)+geom_line(colour=cols[c],linewidth=.85)+geom_point(data=A$calbins[[c]],colour=cols[c],size=1.5)+geom_rug(data=A$preds[[c]],aes(x=prediction),inherit.aes=FALSE,sides='b',alpha=.12)+scale_x_continuous(breaks=c(0,.5,1))+scale_y_continuous(breaks=c(0,.5,1))+coord_cartesian(xlim=c(-.025,1.035),ylim=c(0,1),expand=FALSE)+labs(x='Predicted probability',y='Observed probability',title='Calibration')}
dcaplot<-function(c){z<-dca[[c]];z$curve<-factor(z$variable,c('Nomogram','all','none'),c('Model','All','None'));ggplot(z,aes(threshold,net_benefit,colour=curve,linetype=curve))+geom_line(linewidth=.8)+scale_colour_manual(values=c(unname(cols[c]),'#808080','#B9B9B9'))+scale_linetype_manual(values=c(1,2,3))+scale_x_continuous(breaks=c(0,.5,1))+scale_y_continuous(breaks=c(0,.05,.10,.15))+coord_cartesian(xlim=c(-.025,1.035),ylim=c(-.025,.16),expand=FALSE)+labs(x='Threshold probability',y='Net benefit',title='Decision curve')+guides(colour=guide_legend(nrow=1),linetype=guide_legend(nrow=1))}
cicplot<-function(c){z<-impact[impact$cohort==c,];zz<-rbind(data.frame(threshold=z$threshold,curve='High risk',value=z$high_per_1000,lo=z$high_lower,hi=z$high_upper),data.frame(threshold=z$threshold,curve='High risk with SAP',value=z$SAP_per_1000,lo=z$SAP_lower,hi=z$SAP_upper));zz$curve<-factor(zz$curve,c('High risk','High risk with SAP'));ggplot(zz,aes(threshold,value,colour=curve,fill=curve,linetype=curve))+geom_ribbon(aes(ymin=lo,ymax=hi),alpha=.10,colour=NA,show.legend=FALSE)+geom_line(linewidth=.8)+scale_colour_manual(values=c('#687682',unname(cols[c])))+scale_fill_manual(values=c('#687682',unname(cols[c])))+scale_linetype_manual(values=c(1,2))+scale_x_continuous(breaks=c(0,.5,1))+scale_y_continuous(breaks=c(0,500,1000))+coord_cartesian(xlim=c(-.025,1.035),ylim=c(0,1000),expand=FALSE)+labs(x='Threshold probability',y='Number per 1,000',title='Clinical impact')+guides(colour=guide_legend(ncol=1),linetype=guide_legend(ncol=1))}
cairo_pdf(tempfile(fileext='.pdf'),width=240/25.4,height=300/25.4,family='Arial')
rows<-lapply(names(cn),function(c){g<-lapply(list(rocplot(c),calplot(c),dcaplot(c),cicplot(c)),function(p)ggplotGrob(p+theme(plot.margin=margin(3,9,3,3))));common<-do.call(unit.pmax,lapply(g,`[[`,'heights'));g<-lapply(g,function(x){x$heights<-common;x});arrangeGrob(grobs=g,ncol=4)})

gt<-function(txt,x,y,size=9,bold=FALSE,just='centre',col='#20252A'){grid.text(txt,x,y,just=just,gp=gpar(fontfamily='Arial',fontsize=size,fontface=if(bold)'bold'else'plain',col=col))}
ln<-function(x0,y0,x1,y1,col='#343D44',lwd=.65){grid.lines(c(x0,x1),c(y0,y1),gp=gpar(col=col,lwd=lwd))}
nomogram_panel<-function(){
  x0<-.21;x1<-.97;yy<-seq(.91,.11,length.out=11)
  # Each predictor's point scale uses the same model coefficient multiplier.
  for(k in seq(0,100,10))ln(x0+(x1-x0)*k/100,yy[1],x0+(x1-x0)*k/100,yy[8],col='#EAEFF2',lwd=.4)
  axisrow<-function(vals,positions,y,labname,stagger=FALSE){ln(min(positions),y,max(positions),y);for(j in seq_along(vals)){ln(positions[j],y,positions[j],y-.010);gt(vals[j],positions[j],y-.027-if(stagger&&j%%2==0).021 else 0,7.9)};gt(labname,.018,y,9.2,just='left')}
  axisrow(seq(0,100,20),seq(x0,x1,length.out=6),yy[1],'Points')
  display_ticks<-ticks;display_ticks$temperature<-c(36,38,39.5);display_ticks$amylase<-c(0,1000,2000,3000);display_ticks$base_excess<-c(15,-5,-25);display_ticks$pf_ratio<-c(1000,800,600,400,300)
  for(i in seq_along(v)){x<-display_ticks[[i]];pp<-(b[i]*x-low[i])/unit_point;axisrow(format(x,trim=TRUE),x0+(x1-x0)*pp/100,yy[i+1],lab[v[i]],stagger=v[i]%in%c('temperature','base_excess'))}
  total_lim<-ceiling(sum(span)/unit_point/50)*50;tot<-seq(0,total_lim,50)
  axisrow(tot,x0+(x1-x0)*tot/total_lim,yy[9],'Total points')
  lp<-seq(-6,4,2);axisrow(lp,x0+(x1-x0)*(lp-origin)/unit_point/total_lim,yy[10],'Linear predictor')
  rt<-c(.01,.10,.50,.90,.99);rpos<-(qlogis(rt)-origin)/unit_point
  axisrow(format(rt,nsmall=2),x0+(x1-x0)*rpos/total_lim,yy[11],'SAP risk')
}
palette<-colorRampPalette(c('#2166AC','#D1E5F0','#F7F7F7','#FDDBC7','#B2182B'))(401)
color_value<-function(z)palette[pmax(1,pmin(401,round((z+10)/20*400)+1))]
heatmap_panel<-function(){
  for(i in seq_along(v)){
    j<-(i-1)%%2;k<-(i-1)%/%2;xleft<-.035+j*.455;xr<-xleft+.385;yc<-.86-k*.175
    vv<-seq(min(ticks[[i]]),max(ticks[[i]]),length.out=300)
    grid.raster(matrix(color_value(b[i]*vv),nrow=1),x=(xleft+xr)/2,y=yc,width=xr-xleft,height=.060,interpolate=FALSE)
    grid.rect((xleft+xr)/2,yc,xr-xleft,.060,gp=gpar(fill=NA,col='#555555',lwd=.55));gt(lab[v[i]],(xleft+xr)/2,yc+.054,9.3,TRUE)
    tv<-pretty(range(vv),n=3);tv<-tv[tv>=min(vv)&tv<=max(vv)];tx<-xleft+(xr-xleft)*(tv-min(vv))/diff(range(vv))
    for(jj in seq_along(tv)){ln(tx[jj],yc-.030,tx[jj],yc-.040,lwd=.5);gt(tv[jj],tx[jj],yc-.064,7.8)}
  }
  legcols<-rev(color_value(seq(-10,10,length.out=301)));grid.raster(matrix(legcols,ncol=1),x=.955,y=.59,width=.025,height=.60,interpolate=FALSE)
  for(z in c(-10,-5,0,5,10))gt(z,.985,.29+.60*(z+10)/20,7.5,just='left')
  gt('Contribution',.95,.93,8.0)
  x0<-.04;x1<-.88;y<-.125;lo<-qlogis(.01)-intercept;hi<-qlogis(.99)-intercept;score<-seq(lo,hi,length.out=401)
  colors<-colorRampPalette(c('#2166AC','#F7F7F7','#B2182B'))(401)
  grid.raster(matrix(colors,nrow=1),x=(x0+x1)/2,y=y,width=x1-x0,height=.043,interpolate=FALSE)
  grid.rect((x0+x1)/2,y,x1-x0,.043,gp=gpar(fill=NA,col='#555555',lwd=.55))
  ss<-seq(11,19,2);pos<-x0+(x1-x0)*(ss-lo)/(hi-lo);for(j in seq_along(ss))gt(ss[j],pos[j],y+.045,8.0)
  gt('Score',(x0+x1)/2,y+.083,9.3,TRUE)
  rr<-c(.01,.05,.25,.5,.75,.95,.99);pos<-x0+(x1-x0)*(qlogis(rr)-intercept-lo)/(hi-lo);for(j in seq_along(rr))gt(format(rr[j],nsmall=2),pos[j],y-.043,7.8)
  gt('SAP risk',(x0+x1)/2,y-.083,9.3,TRUE)
}
draw<-function(){
  grid.newpage()
  gt('A',.014,.985,15,TRUE,just='left');gt('Nomogram',.245,.985,12,TRUE)
  gt('B',.515,.985,15,TRUE,just='left');gt('Color nomogram',.745,.985,12,TRUE)
  pushViewport(viewport(x=.25,y=.825,width=.49,height=.30));nomogram_panel();popViewport()
  pushViewport(viewport(x=.755,y=.825,width=.47,height=.30));heatmap_panel();popViewport()
  centers<-c(.552,.340,.128)
  for(i in 1:3){
    gt(c('C','D','E')[i],.014,centers[i]+.101,15,TRUE,just='left')
    gt(cn[i],.055,centers[i]+.101,12,TRUE,just='left',col=cols[i])
    pushViewport(viewport(x=.5,y=centers[i]-.006,width=.985,height=.192));grid.draw(rows[[i]]);popViewport()
  }
}
savefig<-function(name,fun,w,h){for(ext in getOption('htgap.formats')){
  f<-paste0('figures/',name,'.',ext)
  if(ext=='pdf')cairo_pdf(f,w/25.4,h/25.4,family='Arial')
  if(ext=='svg')svglite(f,w/25.4,h/25.4,system_fonts=list(sans='Arial'))
  if(ext=='png')png(f,width=w,height=h,units='mm',res=300,type='cairo')
  if(ext=='tiff')tiff(f,width=w,height=h,units='mm',res=600,type='cairo',compression='lzw')
  fun();dev.off()
}}
