library(mice); library(Hmisc); library(jsonlite)
A <- readRDS('data_private/analysis_objects.rds')
S <- readRDS('data_private/score_splines.rds')
D <- A$D; v <- A$v; M <- length(D$X$training)
stopifnot(M>=2L)
out <- 'analysis'
results <- list(); details <- list(); checks <- list(); coefficients <- list()
for(vv in v) {
  other <- setdiff(v,vv); knots <- S$knots[[vv]]
  full <- linear <- absent <- vector('list',M)
  old <- subset(S$curves,variable==vv)
  Q <- U <- matrix(NA_real_,nrow(old),M)
  for(i in seq_len(M)) {
    x <- as.data.frame(D$X$training[[i]][,v])
    z <- data.frame(sap=D$y$training,x[,other,drop=FALSE])
    basis <- rcspline.eval(x[[vv]],knots=knots,inclx=TRUE)
    z$s1 <- basis[,1]; z$s2 <- basis[,2]
    # Common terms precede all tested terms: D1 requires consistent term order.
    full[[i]] <- glm(reformulate(c(other,'s1','s2'),'sap'),data=z,family=binomial())
    linear[[i]] <- glm(reformulate(c(other,'s1'),'sap'),data=z,family=binomial())
    absent[[i]] <- glm(reformulate(other,'sap'),data=z,family=binomial())
    f <- full[[i]]
    stopifnot(f$converged, f$rank==9L, !anyNA(coef(f)), nobs(f)==length(D$y$training),
              identical(names(coef(linear[[i]])),head(names(coef(f)),-1L)),
              identical(names(coef(absent[[i]])),head(names(coef(f)),-2L)))
    ref <- median(x[[vv]])
    delta <- sweep(rcspline.eval(old$value,knots=knots,inclx=TRUE),2,
                   as.numeric(rcspline.eval(ref,knots=knots,inclx=TRUE)))
    bb <- coef(f)[c('s1','s2')]; V <- vcov(f)[c('s1','s2'),c('s1','s2')]
    Q[,i] <- delta %*% bb; U[,i] <- rowSums((delta %*% V)*delta)
    details[[length(details)+1L]] <- data.frame(variable=vv,imputation=i,
      p_overall_lrt=anova(absent[[i]],f,test='LRT')$`Pr(>Chi)`[2],
      p_nonlinear_lrt=anova(linear[[i]],f,test='LRT')$`Pr(>Chi)`[2])
    coefficients[[paste(vv,i,sep='_')]] <- list(coef=coef(f),vcov=vcov(f))
  }
  q <- rowMeans(Q); vari <- rowMeans(U)+(1+1/M)*apply(Q,1,var)
  discrepancy <- max(abs(q-log(old$OR)),abs(q-1.96*sqrt(vari)-log(old$lower)),
                     abs(q+1.96*sqrt(vari)-log(old$upper)))
  stopifnot(discrepancy < 1e-8)
  overall <- mice::D1(mice::as.mira(full),mice::as.mira(absent),dfcom=df.residual(full[[1]]))
  # The single nonlinear term uses scalar Rubin pooling and Barnard-Rubin df.
  # This avoids the installed mitml finite-df D1 boundary at k*(m-1)==4.
  nn <- mice::pool.scalar(sapply(full,function(f)coef(f)['s2']),
                         sapply(full,function(f)vcov(f)['s2','s2']),n=length(D$y$training),k=length(coef(full[[1]])))
  scalar_t <- nn$qbar/sqrt(nn$t)
  scalar_p <- 2*pt(abs(scalar_t),df=nn$df,lower.tail=FALSE)
  standard <- summary(mice::pool(mice::as.mira(full)))
  stopifnot(abs(scalar_p-standard$p.value[standard$term=='s2'])<1e-10)
  oo <- as.data.frame(overall$result)
  results[[vv]] <- data.frame(variable=vv,label=unname(A$label[vv]),n=length(D$y$training),events=sum(D$y$training),
    p_overall=oo[['P(>F)']],p_nonlinear=scalar_p,
    F_overall=oo$F.value,df1_overall=oo$df1,df2_overall=oo$df2,
    F_nonlinear=scalar_t^2,df1_nonlinear=1,df2_nonlinear=nn$df,
    riv_overall=oo$RIV,riv_nonlinear=nn$r,
    knot_10=knots[1],knot_50=knots[2],knot_90=knots[3])
  checks[[vv]] <- list(curve_max_log_discrepancy=discrepancy,rank=9L,
                      n_imputations=M,converged=TRUE,consistent_coefficient_order=TRUE)
}
res <- do.call(rbind,results); rownames(res)<-NULL
write.csv(res,file.path(out,'spline_pooled_tests.csv'),row.names=FALSE)
dd <- do.call(rbind,details)
write.csv(dd,file.path(out,'spline_tests_by_imputation.csv'),row.names=FALSE)
