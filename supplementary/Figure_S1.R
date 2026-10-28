source_code('R','plot_theme.R')
mi<-A$missingness;mi$cohort<-factor(cn[mi$cohort],cn);mi$variable<-factor(unname(lab[mi$variable]),rev(unname(lab[D$vars])))
mg<-ggplot(mi,aes(variable,cohort,fill=missing_pct))+geom_tile(colour='white',linewidth=.4)+geom_text(aes(label=ifelse(missing_pct==0,'',sprintf('%.1f',missing_pct))),size=3.1)+scale_fill_gradient(low='#F6F8FA',high='#376D9A',name='Missing (%)')+coord_flip()+labs(x=NULL,y=NULL)+theme(axis.text.y=element_text(size=10),axis.text.x=element_text(size=10),legend.title=element_text(size=10),legend.position='bottom')
save_fig('Figure_S1',grob(mg),150,235)
