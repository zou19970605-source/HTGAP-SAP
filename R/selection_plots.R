options(stringsAsFactors=FALSE,warn=1)
library(grid);library(jsonlite)
F<-readRDS('source_data/figure_data.rds')
C<-new.env();C$cmd<-list()
FONT_SCALE<-1.20
cairo_pdf(tempfile(fileext='.pdf'),width=2,height=2,family='Arial')
add<-function(type,...)C$cmd[[length(C$cmd)+1L]]<-c(list(type=type),list(...))
txt<-function(x,y,label,size=6.5,bold=FALSE,anchor='middle',color='#202020',angle=0){
 size<-size*FONT_SCALE
 label<-as.character(label)
 if(grepl('[₀₁₂]',label)&&angle==0){
  ch<-strsplit(label,'',fixed=TRUE)[[1]];sub<-ch %in% c('₀','₁','₂');group<-cumsum(c(TRUE,diff(sub)!=0));parts<-split(ch,group);issub<-vapply(split(sub,group),function(a)a[1],logical(1))
  ss<-vapply(parts,paste,collapse='',character(1));ss<-chartr('₀₁₂','012',ss);sz<-ifelse(issub,size*.72,size)
  widths<-vapply(seq_along(ss),function(i)convertWidth(grobWidth(textGrob(ss[i],gp=gpar(fontfamily='Arial',fontsize=sz[i],fontface=if(bold)'bold' else 'plain'))),'mm',valueOnly=TRUE),numeric(1))
  x0<-x-if(anchor=='middle')sum(widths)/2 else if(anchor=='right')sum(widths) else 0
  for(i in seq_along(ss)){add('text',x=x0,y=y+if(issub[i])size*25.4/72*.20 else 0,label=ss[i],size=sz[i],bold=bold,anchor='left',color=color,angle=0);x0<-x0+widths[i]}
 }else add('text',x=x,y=y,label=label,size=size,bold=bold,anchor=anchor,color=color,angle=angle)
}
line<-function(x,y,col='#333333',width=.4,dash=FALSE)add('line',x=x,y=y,col=col,width=width,dash=dash)
poly<-function(x,y,fill='#CCCCCC',col=NA,width=.25)add('polygon',x=x,y=y,fill=fill,col=col,width=width)
rect<-function(x,y,w,h,fill=NA,col=NA,width=.4)poly(c(x,x+w,x+w,x),c(y,y,y+h,y+h),fill,col,width)
circles<-function(x,y,r,fill,col=NA,width=.3)add('circle',x=x,y=y,r=r,fill=fill,col=col,width=width)
clip_start<-function(x,y,w,h)add('clip_start',x=x,y=y,w=w,h=h)
clip_end<-function()add('clip_end')
esc<-function(x){x<-gsub('&','&amp;',x,fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);x<-gsub('>','&gt;',x,fixed=TRUE);x<-gsub('"','&quot;',x,fixed=TRUE);x}
svg_color<-function(col,attribute){if(is.na(col))return(paste0(attribute,'="none"'));q<-col2rgb(col,alpha=TRUE);paste0(attribute,'="',rgb(q[1],q[2],q[3],maxColorValue=255),'" ',attribute,'-opacity="',sprintf('%.4f',q[4]/255),'"')}
export_svg<-function(cmd,path,W,H){
 out<-c('<?xml version="1.0" encoding="UTF-8"?>',sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%smm" height="%smm" viewBox="0 0 %s %s">',W,H,W,H),'<rect width="100%" height="100%" fill="white"/>')
 clipid<-0
 for(z in cmd){
  if(z$type=='clip_start'){clipid<-clipid+1;s<-sprintf('<defs><clipPath id="clip%s"><rect x="%s" y="%s" width="%s" height="%s"/></clipPath></defs><g clip-path="url(#clip%s)">',clipid,z$x,z$y,z$w,z$h,clipid)}
  else if(z$type=='clip_end')s<-'</g>'
  else if(z$type=='text'){
   anc<-switch(z$anchor,left='start',middle='middle',right='end')
   rot<-if(z$angle!=0)sprintf(' transform="rotate(%s %s %s)"',-z$angle,z$x,z$y) else ''
   s<-sprintf('<text x="%s" y="%s" font-family="Arial,Helvetica,sans-serif" font-size="%s" font-weight="%s" text-anchor="%s" dominant-baseline="central" %s%s>%s</text>',
    z$x,z$y,z$size*25.4/72,if(z$bold)'bold' else 'normal',anc,svg_color(z$color,'fill'),rot,esc(z$label))
  }else if(z$type=='line'){
   s<-sprintf('<polyline points="%s" fill="none" %s stroke-width="%s"%s stroke-linejoin="round"/>',paste(paste(z$x,z$y,sep=','),collapse=' '),svg_color(z$col,'stroke'),z$width*25.4/72,if(z$dash)' stroke-dasharray="0.8 0.65"' else '')
  }else if(z$type=='polygon')s<-sprintf('<polygon points="%s" %s %s stroke-width="%s"/>',paste(paste(z$x,z$y,sep=','),collapse=' '),svg_color(z$fill,'fill'),svg_color(z$col,'stroke'),z$width*25.4/72)
  else if(z$type=='circle'){
   s<-sprintf('<circle cx="%s" cy="%s" r="%s" %s %s stroke-width="%s"/>',z$x,z$y,z$r,svg_color(z$fill,'fill'),svg_color(z$col,'stroke'),z$width*25.4/72)
  }
  out<-c(out,s)
 }
 writeLines(c(out,'</svg>'),path,useBytes=TRUE)
}
render<-function(cmd,W,H){
 grid.newpage();pushViewport(viewport(width=unit(W,'mm'),height=unit(H,'mm')))
 grid.rect(gp=gpar(fill='white',col=NA))
 xoffset<-0;yoffset<-0;Hpage<-H
 for(z in cmd){
  if(z$type=='clip_start'){
   pushViewport(viewport(x=unit(z$x+z$w/2,'mm'),y=unit(Hpage-z$y-z$h/2,'mm'),width=unit(z$w,'mm'),height=unit(z$h,'mm'),clip='on'))
   xoffset<-z$x;yoffset<-z$y;H<-z$h;next
  }
  if(z$type=='clip_end'){popViewport();xoffset<-0;yoffset<-0;H<-Hpage;next}
  z$x<-z$x-xoffset;z$y<-z$y-yoffset
  if(z$type=='text')grid.text(z$label,x=unit(z$x,'mm'),y=unit(H-z$y,'mm'),just=c(if(z$anchor=='middle')'center' else z$anchor,'center'),rot=z$angle,gp=gpar(fontfamily='Arial',fontsize=z$size,fontface=if(z$bold)'bold' else 'plain',col=z$color))
  else if(z$type=='line')grid.lines(x=unit(z$x,'mm'),y=unit(H-z$y,'mm'),gp=gpar(col=z$col,lwd=z$width/.75,lty=if(z$dash)2 else 1,linejoin='round'))
  else if(z$type=='polygon')grid.polygon(x=unit(z$x,'mm'),y=unit(H-z$y,'mm'),gp=gpar(fill=z$fill,col=z$col,lwd=z$width/.75))
  else if(z$type=='circle')grid.circle(x=unit(z$x,'mm'),y=unit(H-z$y,'mm'),r=unit(z$r,'mm'),gp=gpar(fill=z$fill,col=z$col,lwd=z$width/.75))
 }
 popViewport()
}
export<-function(cmd,name,W,H,tiff=TRUE,png_dpi=300){
 base<-paste0('figures/',name)
 cairo_pdf(paste0(base,'.pdf'),width=W/25.4,height=H/25.4,family='Arial',onefile=FALSE);render(cmd,W,H);dev.off()
 export_svg(cmd,paste0(base,'.svg'),W,H)
 png(paste0(base,'.png'),width=W,height=H,units='mm',res=png_dpi,type='cairo',bg='white');render(cmd,W,H);dev.off()
 if(tiff && 'tiff' %in% getOption('htgap.formats')){grDevices::tiff(paste0(base,'.tiff'),width=W,height=H,units='mm',res=600,compression='lzw',type='cairo',bg='white');render(cmd,W,H);dev.off()}
 cat('EXPORTED',name,'\n')
}
move<-function(cmd,dx=0,dy=0,s=1)lapply(cmd,function(z){if(!is.null(z$x))z$x<-z$x*s+dx;if(!is.null(z$y))z$y<-z$y*s+dy;if(!is.null(z$r))z$r<-z$r*s;if(!is.null(z$size))z$size<-z$size*s;if(!is.null(z$width))z$width<-z$width*s;if(!is.null(z$w))z$w<-z$w*s;if(!is.null(z$h))z$h<-z$h*s;z})
capture<-function(fun,...){C$cmd<-list();fun(...);C$cmd}
tag<-function(letter,title){if(nchar(letter))txt(0,3,letter,size=10.5,bold=TRUE,anchor='left');txt(if(nchar(letter))6 else 0,3,title,size=8,bold=TRUE,anchor='left')}
pstr<-function(x)if(is.na(x))'—' else if(x<.001)'<0.001' else sprintf('%.3f',x)
orstr<-function(a,l,u)if(is.na(a))'—' else sprintf('%.2f (%.2f–%.2f)',a,l,u)
forest_panel<-function(letter='A'){
 tag(letter,'Logistic regression');d<-F$forest
 line(c(0,100),c(8,8),width=.6);line(c(0,100),c(19,19),width=.5)
 txt(0,14,'Variable',6.2,TRUE,'left');txt(29,11,'Univariable',6.1,TRUE);txt(58,11,'Multivariable (AIC)',6.1,TRUE)
 txt(25,16,'OR (95% CI)',5.8,TRUE);txt(39,16,'P',5.8,TRUE);txt(54,16,'OR (95% CI)',5.8,TRUE);txt(69,16,'P',5.8,TRUE)
 limits<-range(c(d$crude_lower,d$crude_upper,d$adjusted_lower,d$adjusted_upper),na.rm=TRUE)
 lim<-2^c(floor(log2(limits[1])),ceiling(log2(limits[2])))
 xx<-function(z)77+(log(z)-log(lim[1]))/diff(log(lim))*23
 line(rep(xx(1),2),c(20,115.5),'#9C9C9C',.35,TRUE)
 for(i in seq_len(nrow(d))){z<-d[i,];yy<-22+(i-1)*3.6
  txt(0,yy,z$label,6.15,z$AIC_selected,'left')
  txt(25,yy,orstr(z$crude_OR,z$crude_lower,z$crude_upper),5.6);txt(39,yy,pstr(z$crude_P),5.6)
  txt(54,yy,orstr(z$adjusted_OR,z$adjusted_lower,z$adjusted_upper),5.6);txt(69,yy,pstr(z$adjusted_P),5.6)
  for(k in c('crude','adjusted')){
   a<-z[[paste0(k,'_OR')]];if(is.na(a))next
   lo<-xx(z[[paste0(k,'_lower')]]);hi<-xx(z[[paste0(k,'_upper')]]);mid<-xx(a);y0<-yy+if(k=='crude')-.5 else .5
   line(c(lo,hi),c(y0,y0),'#454545',.38);line(c(lo,lo),y0+c(-.38,.38),'#454545',.38);line(c(hi,hi),y0+c(-.38,.38),'#454545',.38)
   if(k=='crude')circles(mid,y0,.32,'#B23B79') else rect(mid-.31,y0-.31,.62,.62,'#0072B2')
  }
 }
 line(c(0,100),c(115.5,115.5),width=.5);ticks<-2^seq(log2(lim[1]),log2(lim[2]));if(length(ticks)>6)ticks<-sort(unique(c(1,ticks[seq(1,length(ticks),2)])))
 line(c(77,100),c(118,118),width=.45)
 for(z in ticks){line(c(xx(z),xx(z)),c(118,119),width=.4);txt(xx(z),121,format(z,trim=TRUE),5.6)}
 txt(88.5,125,'OR (95% CI)',6.2)
 circles(2,124,.4,'#B23B79');txt(4,124,'Univariable',6,FALSE,'left')
 rect(28,123.6,.8,.8,'#0072B2');txt(30,124,'Multivariable',6,FALSE,'left')
}
boruta_panel<-function(letter='B'){
 tag(letter,'Boruta importance');d<-F$boruta_info;h<-F$boruta_history
 colors<-c(Confirmed='#3FA7A0',Tentative='#E4AE48',Rejected='#D87878',Shadow='#ABADB0')
 minx<-floor(min(h$importance));maxx<-ceiling(max(h$importance));left<-16;right<-74
 xx<-function(x)left+(x-minx)/(maxx-minx)*(right-left)
 ticks<-pretty(c(minx,maxx),n=6);ticks<-ticks[ticks>=minx&ticks<=maxx]
 for(z in ticks)line(rep(xx(z),2),c(12,114.5),'#E6E8EB',.3)
 for(i in seq_len(nrow(d))){z<-d[i,];yy<-14+(i-1)*3.45;val<-h$importance[h$variable==z$variable]
  den<-density(val,n=160,from=min(val),to=max(val));half<-den$y/max(den$y)*1.25
  poly(c(xx(den$x),rev(xx(den$x))),c(yy-half,rev(yy+half)),adjustcolor(colors[z$decision],.8),colors[z$decision],.22)
  circles(xx(z$median),yy,.3,'white',colors[z$decision],.45)
  txt(14.2,yy,z$label,6.1,z$variable %in% F$target,'right')
 }
 line(c(left,right),c(115,115),width=.45)
 for(z in ticks){line(rep(xx(z),2),c(115,116),width=.4);txt(xx(z),118,as.character(z),6)}
 txt((left+right)/2,122,'Importance (Z score)',6.5)
 lx<-c(0,22,41,59)
 for(i in seq_along(colors)){circles(lx[i]+.8,128,.65,colors[i]);txt(lx[i]+2.2,128,names(colors)[i],5.8,FALSE,'left')}
}
cv_panel<-function(i=1,letter='C',w=96,h=76,title='LASSO cross-validation',y_limits=NULL,annotate_selection=TRUE){
 tag(letter,title);z<-F$cv[[i]];left<-11;right<-w-2;top<-16;bottom<-h-13
 x<-log(z$lambda);xr<-range(x);yr<-range(z$cvm-z$cvsd,z$cvm+z$cvsd);yr[1]<-floor(yr[1]*20)/20;yr[2]<-ceiling(yr[2]*20)/20
 if(!is.null(y_limits))yr<-y_limits
 xx<-function(v)left+(v-xr[1])/diff(xr)*(right-left);yy<-function(v)bottom-(v-yr[1])/diff(yr)*(bottom-top)
 line(c(left,left,right),c(top,bottom,bottom),width=.5)
 for(v in pretty(xr,n=5)){if(v<xr[1]||v>xr[2])next;line(rep(xx(v),2),c(bottom,bottom+1),width=.4);txt(xx(v),bottom+3,format(v,trim=TRUE),6)}
 for(v in pretty(yr,n=5)){if(v<yr[1]||v>yr[2])next;line(c(left-1,left),rep(yy(v),2),width=.4);txt(left-1.7,yy(v),sprintf('%.2f',v),6,FALSE,'right')}
 for(j in seq_along(x)){
  ylo<-yy(z$cvm[j]-z$cvsd[j]);yhi<-yy(z$cvm[j]+z$cvsd[j]);xp<-xx(x[j])
  line(rep(xp,2),c(ylo,yhi),'#B9BDC2',.32);line(xp+c(-.5,.5),rep(ylo,2),'#B9BDC2',.32);line(xp+c(-.5,.5),rep(yhi,2),'#B9BDC2',.32)
 }
 line(xx(x),yy(z$cvm),'#C95160',.55);circles(xx(x),yy(z$cvm),.29,'#C95160')
 sel<-c(z$lambda.min,z$lambda.1se)
 for(k in 1:2){xp<-xx(log(sel[k]));line(rep(xp,2),c(top,bottom),if(k==1)'#0072B2' else '#6A6A6A',.6,TRUE);txt(xp,top-2,if(k==1)'λmin' else 'λ1se',6.3,TRUE,color=if(k==1)'#0072B2' else '#5A5A5A')}
 inds<-unique(round(seq(1,length(x),length.out=21)))
 for(j in inds)txt(xx(x[j]),11.5,z$nzero[j],5.6)
 txt((left+right)/2,h-4,'Log(λ)',7);txt(2.5,(top+bottom)/2,'AUC',7,angle=90)
 if(annotate_selection){
  selected<-F$sets$LASSO
  rows<-split(selected,ceiling(seq_along(selected)/4))
  txt(15,38,sprintf('Selected predictors (n = %d)',length(selected)),6.4,TRUE,'left',color='#24658B')
  for(k in seq_along(rows))txt(15,43+(k-1)*4.5,paste(F$labels[rows[[k]]],collapse=', '),6,FALSE,'left')
 }
}
overlap_panel<-function(letter='D'){
 tag(letter,'Predictor overlap')
 circles(28,35,21,adjustcolor('#6BAED6',.24),'#6BAED6',.7)
 circles(48,35,21,adjustcolor('#70B7A3',.23),'#70B7A3',.7)
 circles(26,35,13,adjustcolor('#CB7C97',.23),'#B76583',.65)
 txt(20,10,sprintf('LASSO (n = %d)',length(F$sets$LASSO)),7,TRUE,color='#24658B');txt(57,10,sprintf('Boruta (n = %d)',length(F$sets$Boruta)),7,TRUE,color='#317E6C')
 txt(26,29,'Logistic',6.6,TRUE,color='#80415A');txt(26,32,sprintf('n = %d',length(F$sets$Logistic)),5.8,color='#80415A')
 txt(18,40,length(setdiff(F$sets$Logistic,F$sets$Boruta)),9);txt(33,40,length(Reduce(intersect,F$sets)),12,TRUE);txt(38,19,length(setdiff(intersect(F$sets$LASSO,F$sets$Boruta),F$sets$Logistic)),9);txt(63,37,length(setdiff(F$sets$Boruta,union(F$sets$LASSO,F$sets$Logistic))),9)
 selected<-F$labels[Reduce(intersect,F$sets)]
 txt(38,65,paste(head(selected,4),collapse=', '),7,TRUE)
 txt(38,70,paste(tail(selected,max(0,length(selected)-4)),collapse=', '),7,TRUE)
}
