## ----include = FALSE-----------------------
library(tufte)
knitr::opts_chunk$set(results = "hide", echo = FALSE)

## ------------------------------------------
x = c(4,4,7,8,12,15,16,17,14,11,7,5)
y = c(73, 57, 81, 94, 110, 124, 134, 139, 124, 103, 81, 80)
m = lm(y~x)
summary(m)
##The p-value for the gradient is 9.9e-09
##This suggests temperature is useful

## ------------------------------------------
##The p-value for the correlation is also 9.9e-09
cor.test(x, y)

## ----F1, fig.keep='none'-------------------
plot(x, y, xlab="Temp", ylab="Sales")
abline(m, col=2, lty=2)

## ----fig.margin = TRUE, fig.cap="Scatterplot with the earnings data. Also shows the line of best fit.", out.width='\\textwidth', echo=FALSE----
plot(x, y, xlab="Temp", ylab="Sales")
abline(m, col=2, lty=2)
text(5, 130, "r=0.983")

## ---- fig.keep='none', eval = FALSE--------
#  text(5, 130, "r=0.983")

## ----F2, fig.keep='none'-------------------
##Model diagnosics look good
plot(fitted.values(m), rstandard(m))

## ----fig.keep='none'-----------------------
qqnorm(rstandard(m))
##Model diagnosics look good

## ----echo=FALSE----------------------------
x = c(4, 4, 7, 8, 12, 15, 16, 17, 14, 11, 7, 5)
y = c(73, 57, 81, 94, 110, 124, 134, 139, 124, 103, 81, 80)

## ------------------------------------------
m = lm(y~x)

## ------------------------------------------
cor(x, y)

## ----fig.keep='none'-----------------------
plot(x, y)
abline(m)

## ----fig.keep='none'-----------------------
plot(fitted.values(m), rstandard(m), ylim=c(-2.5, 2.5))
abline(h=c(-2, 0, 2), lty=3, col=4)

## ----fig.keep='none'-----------------------
qqnorm(rstandard(m))
qqline(rstandard(m), col=4)

## ---- echo = TRUE, message = FALSE---------
library("jrModelling")
data(graves)

## ----fig.keep='none'-----------------------
fit = lm(OI ~ age + Sex, data = graves)

## ----fig.keep='none'-----------------------
plot(graves$OI, rstandard(fit))
abline(h=c(-2, 0, 2), col=2, lty=3)
plot(graves$age, rstandard(fit))
abline(h=c(-2, 0, 2), col=2, lty=3)
plot(graves$Sex, rstandard(fit))
abline(h=c(-2, 0, 2), col=2, lty=3)
plot(fitted.values(fit), rstandard(fit))
abline(h=c(-2, 0, 2), col=2, lty=3)
qqnorm(rstandard(fit))
qqline(rstandard(fit), col = 2, lty = 2)

# The q-q plot shows the residuals lying close to the fitted straight 
# line which suggests that the normality assumption is satisfied.
# The residuals in the first plot appear to show a pattern. 
# Consider transforming the response variable or the explanatory variables
# or adding a square term / interaction term to your model.

## ------------------------------------------
data(drphil)

## ------------------------------------------
(m = lm(IQ ~ AgeBegin + AgeEnd + TotalYears, data=drphil))
#The problem is TotalYears = AgeEnd - AgeBegin
#Solution: remove TotalYears

## ------------------------------------------
x1 = c(109, 114, 108, 123, 115, 108, 114)
x2 = c(113, 114, 113, 108, 119, 112, 110)
x3 = c(103, 94, 114, 107, 107, 113, 107)
dd = data.frame(values = c(x1, x2, x3), type = rep(c("M", "S", "H"), each=7))
m = aov(values ~ type, dd)
summary(m)
##The p value is around 0.056.
##This suggests a difference may exist.

## ----F3, fig.keep='none'-------------------
plot(fitted.values(m), rstandard(m))
## Residual plot looks OK

## ----fig.margin = TRUE, out.width='\\textwidth', echo=FALSE, fig.cap = "Model diagnosics for the music data."----
plot(fitted.values(m), rstandard(m))

## ------------------------------------------
TukeyHSD(m)

## ---- echo = TRUE--------------------------
data(hep)
##Remove the athletes names and final scores.
hep_s = hep[,2:8]

## ----fig.keep="none"-----------------------
plot(hclust(dist(hep_s)), labels=hep[,1])

## ------------------------------------------
##Round to 2dp
signif(cor(hep_s), 2)

## ------------------------------------------
##Remove:
##1st column: athletes name
##Last column: It's a combination of the other columns
dd = hep[ ,2:8]

##Run principle components
prcomp(dd)

## ------------------------------------------
##Yes!. run800m dominates the loading since
##the scales differ

## ----  fig.keep="none"---------------------
prcomp(dd, scale=TRUE)
biplot(prcomp(dd, scale=TRUE))

