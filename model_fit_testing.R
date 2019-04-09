resids.fig <- function(mod, df) {
  residdf <- dplyr::mutate(df, resids = residuals(mod, type = 'normalized'),
                           fits = fitted(mod))
  fig2 <-ggplot(residdf, aes(x = fits, y = resids)) + geom_point() +
    labs(x = 'Fitted values', y = '')
  
  fig3 <- ggplot(residdf) + stat_qq(aes(sample = resids)) +
    labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles')
  
  # qqline plot = FALSE, according to James should work
  
  fig4 <- ggplot(residdf, aes(x = resids)) + geom_histogram(aes(y=..density..), colour = 'grey50') +
    labs(x = 'Residuals', y = 'Frequency') + scale_y_continuous(expand = c(0, 0)) +
    stat_function(fun = dnorm, color = "red", args = list(mean = mean(residdf$resids),
                                                          sd = sd(residdf$resids)))
  grid.draw(rbind(ggplotGrob(fig2), ggplotGrob(fig3), ggplotGrob(fig4), size = 'first'))
  
  return(summary(mod))
}

resids.fig(lm.mod2, cpt.cl)


cpt.cl$resid<-residuals(lm.mod2)
cpt.cl$resid<-as.numeric(cpt.cl$resid)

cpt.cl$fitted<-predict(lm.mod2)
cpt.cl$fitted<-as.numeric(cpt.cl$fitted)

resage.plot<-ggplot(cpt.cl, aes(x=age, y=resid, color=treatment))
resage.plot+geom_point(
)+scale_color_manual(values=c("black", "firebrick1")
)+geom_hline(aes(yintercept=0),
             size=1.2, color="royalblue2"
)+facet_wrap(~temp)


resfit.plot<-ggplot(cpt.cl, aes(x=fitted, y=resid, color=treatment))
resfit.plot+geom_point(
)+scale_color_manual(values=c("black", "firebrick1")
)+geom_hline(aes(yintercept=0),
             size=1.2, color="royalblue2"
)+facet_wrap(~temp)

