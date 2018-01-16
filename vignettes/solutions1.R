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

## ----echo=FALSE---------------------------------
x = c(78.64,79.01, 79.57, 79.52, 80.71, 79.95, 78.50,
  79.10, 81.98, 80.09, 80.29, 80.22)
y = c(81.92, 81.12, 82.47, 82.86, 82.89, 82.45,
     82.51, 81.11, 83.07, 82.77, 82.38, 83.14)
dd = data.frame(x, y)

## -----------------------------------------------
library("jrModelling")

## ----echo=FALSE---------------------------------
x = c(78.64,79.01, 79.57, 79.52, 80.71, 79.95, 78.50,
  79.10, 81.98, 80.09, 80.29, 80.22)
y = c(81.92, 81.12, 82.47, 82.86, 82.89, 82.45,
     82.51, 81.11, 83.07, 82.77, 82.38, 83.14)
dd = data.frame(x, y)
## var(y)
## var.test(x, y)

## ----echo=echo, results=results-----------------
##Data for question 1 
## Easier using Excel and export as CSV
x = c(78.64,79.01, 79.57, 79.52, 80.71, 79.95, 78.50,
  79.10, 81.98, 80.09, 80.29, 80.22)
y = c(81.92, 81.12, 82.47, 82.86, 82.89, 82.45,
     82.51, 81.11, 83.07, 82.77, 82.38, 83.14)
dd = data.frame(x, y)

## ----echo=FALSE---------------------------------
d1 = data.frame(value = x)
d2 = data.frame(value = y)

## ----echo=echo, results=results, eval=FALSE-----
#  ## Suppose you have two separate data files. Here is some code that will help ## you combine them. First we read in the separate files:
#  d1 = read.csv("Method1.csv")
#  d2 = read.csv("Method2.csv")

## ----echo=echo, results=results-----------------
## In order to combine the data frames, 
## they must have the same column names:
head(d1, 2)
head(d2, 2)

## ----echo=echo, results=results-----------------
## We combine data frames using rbind (row bind)
d = rbind(d1, d2)

## ----echo=echo, results=results-----------------
## Finally we create a new column to indicate the Method
## rep is the replicate function. See ?rep
d$Method = rep(1:2, each=12)
head(d, 2)

## ----echo=echo, results=results-----------------
t.test(value ~ Method, data=d, var.equal=FALSE)

## ----echo=echo, results=results-----------------
var.test(value ~ Method, data=d)

## ----echo=echo, results=results-----------------
t.test(value ~ Method, data=d, var.equal=TRUE)

## ----echo=echo, results=results-----------------
x = c(348, 353, 359, 357, 350, 355, 359, 367, 345, 362, 343, 367)
m = chisq.test(x)
##Since p > 0.05 we can't accept the alternative hypothesis. 
##However, the question is worded as though we can "prove" the Null
##hypotheis, which we obviously can't do.

## ----echo=echo, results=results-----------------
##expected values
(expected = m[["expected"]])

## ----echo=echo, results=results-----------------
##Residuals
m[["residuals"]]

## ----warning=FALSE,echo=echo, results=results----
h = c(17, 8, 22)
nh = c(35, 53, 491)
dd = data.frame(h, nh)
m = chisq.test(dd)

## ----echo=echo, results=results-----------------
m[["expected"]]

## ----echo=echo, results=results-----------------
##Some of the expected values are less then 5
##So consider combining cells.

## ----eval=FALSE---------------------------------
#  vignette("solutions1", package = "jrModelling")

