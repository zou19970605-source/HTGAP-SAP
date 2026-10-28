source_code('R','plot_theme.R')
sen<-A$sensitivity;sen$cohort<-factor(cn[sen$cohort],cn);sen$analysis<-factor(sen$analysis,rev(unique(sen$analysis)))
sg<-ggplot(sen,aes(auc,analysis,colour=cohort))+geom_point(position=position_dodge(.55),size=2)+geom_errorbar(aes(xmin=auc_low,xmax=auc_high),position=position_dodge(.55),width=.15,orientation='y')+scale_colour_manual(values=unname(cols))+labs(x='AUC (95% CI)',y=NULL)+coord_cartesian(xlim=c(.55,.95))
save_fig('Figure_S5',grob(sg),190,135)
