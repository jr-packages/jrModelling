## ----echo=FALSE----------------------------------------------------------
results='show';echo=TRUE

## ----setup, include=FALSE, cache=FALSE----------
library(knitr)
opts_knit$set(out.format = "latex")
knit_theme$set(knit_theme$get("greyscale0"))

options(replace.assign=FALSE,width=50)

opts_chunk$set(fig.path='knitr_figure/graphics-', 
               cache.path='knitr_cache/graphics-', 
               fig.align='center', 
               dev='pdf', fig.width=5, fig.height=5, 
               fig.show='hold', cache=FALSE, par=TRUE)
knit_hooks$set(crop=hook_pdfcrop)

knit_hooks$set(par=function(before, options, envir){
  if (before && options$fig.show!='none') {
    par(mar=c(3,3,2,1),cex.lab=.95,cex.axis=.9,
        mgp=c(2,.7,0),tcl=-.01, las=1)
  }}, crop=hook_pdfcrop)

## ----echo=echo, results=results-----------------
x = c(4,4,7,8,12,15,16,17,14,11,7,5)
y = c(73, 57, 81, 94, 110, 124, 134, 139, 124, 103, 81, 80)
m = lm(y~x)
summary(m)
##The p-value for the gradient is 9.9e-09
##This suggests temperatue is useful

## ----echo=echo, results=results-----------------
##The p-value for the correlation is also 9.9e-09
cor.test(x, y)

## ----F1, fig.keep='none', echo=(1:2)*echo-------
plot(x, y, xlab="Temp", ylab="Sales")
abline(m, col=2, lty=2)
text(5, 130, "r=0.983")

## ----ref.label='F1', dev='pdf', out.width='\\textwidth', echo=FALSE----
plot(x, y, xlab="Temp", ylab="Sales")
abline(m, col=2, lty=2)
text(5, 130, "r=0.983")

## ----F1, echo=3*echo, results=results, fig.keep='none'----
plot(x, y, xlab="Temp", ylab="Sales")
abline(m, col=2, lty=2)
text(5, 130, "r=0.983")

## ----F2, fig.keep='none', echo=echo-------------
##Model diagnosics look good
plot(fitted.values(m), rstandard(m))

## ----fig.keep='none',echo=echo, results=results----
qqnorm(rstandard(m))
##Model diagnosics look good

## ----echo=FALSE---------------------------------
x = c(4, 4, 7, 8, 12, 15, 16, 17, 14, 11, 7, 5)
y = c(73, 57, 81, 94, 110, 124, 134, 139, 124, 103, 81, 80)

## ----echo=echo----------------------------------
m = lm(y~x)

## ----results=results,echo=echo------------------
cor(x, y)

## ----echo=echo,fig.keep='none'------------------
plot(x, y)
abline(m)

## ----echo=echo,fig.keep='none'------------------
plot(fitted.values(m), rstandard(m), ylim=c(-2.5, 2.5))
abline(h=c(-2, 0, 2), lty=3, col=4)

## ----echo=echo,fig.keep='none'------------------
qqnorm(rstandard(m))
qqline(rstandard(m), col=4)

## -----------------------------------------------
library("jrModelling")
data(graves)

## -----------------------------------------------
data(drphil)

## ----echo=echo, results=results-----------------
(m = lm(IQ ~ AgeBegin + AgeEnd + TotalYears, data=drphil))
#The problem is TotalYears = AgeEnd - AgeBegin
#Solution: remove TotalYears

## ----echo=echo, results=results-----------------
x1 = c(109, 114, 108, 123, 115, 108, 114)
x2 = c(113, 114, 113, 108, 119, 112, 110)
x3 = c(103, 94, 114, 107, 107, 113, 107)
dd = data.frame(values = c(x1, x2, x3), type = rep(c("M", "S", "H"), each=7))
m = aov(values ~ type, dd)
summary(m)
##The p value is around 0.056.
##This suggests a difference may exist.

## ----F3, fig.keep='none', echo=echo, results=results----
plot(fitted.values(m), rstandard(m))
## Residual plot looks OK

## ----ref.label='F3', dev='pdf', out.width='\\textwidth', echo=FALSE----
plot(fitted.values(m), rstandard(m))
## Residual plot looks OK

## ---- echo=echo, results=results----------------
TukeyHSD(m)

## -----------------------------------------------
data(hep)
##Remove the athletes names and final scores.
hep_s = hep[,2:8]

## ----fig.keep="none", echo=echo, results=results----
plot(hclust(dist(hep_s)), labels=hep[,1])

## ----echo=echo, results=results-----------------
##Round to 2dp
signif(cor(hep_s), 2)

## ----echo=echo, results=results-----------------
##Remove:
##1st column: athletes name
##Last column: It's a combination of the other columns
dd = hep[ ,2:8]

##Run principle components
prcomp(dd)

## ----echo=echo, results=results-----------------
##Yes!. run800m dominates the loading since
##the scales differ

## ----echo=echo, results=results, fig.keep="none"----
prcomp(dd, scale=TRUE)
biplot(prcomp(dd, scale=TRUE))

## ----eval=FALSE---------------------------------
#  install.packages("survival")

## ----message=FALSE------------------------------
library(survival)

## -----------------------------------------------
data(lung)

## -----------------------------------------------
dim(lung)

## ----results="hide"-----------------------------
Surv(lung$time, lung$status)

## ----results="hide"-----------------------------
survfit(Surv(lung$time, lung$status)~1)

## ----fig.keep="none"----------------------------
plot(survfit(Surv(lung$time, lung$status)~1))

## ----fig.keep="none"----------------------------
plot(survfit(Surv(lung$time, lung$status)~lung$sex))

## -----------------------------------------------
data(heart)

## ----results="hide"-----------------------------
Surv(heart$start, heart$stop, heart$event) 

## ----eval=FALSE---------------------------------
#  vignette("solutions2", package = "jrModelling")

