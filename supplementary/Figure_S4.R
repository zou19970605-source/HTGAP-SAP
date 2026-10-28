library(ggplot2)
library(grid)
tests <- read.csv('analysis/spline_pooled_tests.csv',check.names=FALSE)
needed <- c('variable', 'p_overall', 'p_nonlinear')
if (!all(needed %in% names(tests))) stop('CSV must contain variable, p_overall, and p_nonlinear.')
A <- readRDS('data_private/analysis_objects.rds')
S <- readRDS('data_private/score_splines.rds')
v <- A$v
if (!setequal(tests$variable, v) || anyDuplicated(tests$variable)) stop('Expected exactly the seven model predictors.')
tests <- tests[match(v, tests$variable), ]
if (anyNA(tests[needed]) || any(as.matrix(tests[c('p_overall','p_nonlinear')]) < 0) ||
    any(as.matrix(tests[c('p_overall','p_nonlinear')]) > 1)) stop('Invalid P values.')
format_p <- function(x) ifelse(x < .001, '< 0.001', paste0('= ', formatC(x, digits=3, format='f')))
display <- A$label[v]
display[c('pao2','pf_ratio')] <- c('PaO2','PaO2/FiO2')
# Labels are passed through the facet strip. This reserves dedicated space above
# each curve and does not cover the plotted confidence intervals.
headings <- setNames(paste0(LETTERS[seq_along(v)], '  ', display,
                    '\nOverall P ', format_p(tests$p_overall),
                    '\nNonlinearity P ', format_p(tests$p_nonlinear)), v)
rcs <- S$curves
rcs$variable <- factor(rcs$variable, levels=v)
p <- ggplot(rcs, aes(value, OR)) +
  geom_hline(yintercept=1, linetype=2, colour='grey60') +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15, fill='#326C9A') +
  geom_line(colour='#326C9A', linewidth=.7) +
  scale_y_log10() +
  facet_wrap(~variable, scales='free_x', ncol=3,
             labeller=as_labeller(headings)) +
  labs(x='Predictor value', y='Adjusted odds ratio') +
  theme_classic(base_size=10.3, base_family='Arial') +
  theme(axis.title=element_text(size=10.3),
        axis.text=element_text(size=9, colour='#20252A'),
        strip.background=element_blank(),
        strip.text=element_text(size=9.7, face='bold', hjust=0,
                                margin=margin(0,0,5,0), lineheight=1.05),
        plot.margin=margin(7,8,7,6),
        panel.spacing.x=unit(5,'mm'), panel.spacing.y=unit(6,'mm'))
w <- 180; h <- 188

for (ext in getOption('htgap.formats')) {
  f <- paste0('figures/Figure_S4.',ext)
  if(ext=='pdf') cairo_pdf(f,w/25.4,h/25.4,family='Arial')
  if(ext=='svg') svglite::svglite(f,w/25.4,h/25.4)
  if(ext=='png') png(f,width=w,height=h,units='mm',res=300,type='cairo')
  if(ext=='tiff') tiff(f,width=w,height=h,units='mm',res=600,type='cairo',compression='lzw')
  print(p);dev.off()
}
