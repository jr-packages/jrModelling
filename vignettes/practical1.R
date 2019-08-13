## ----include = FALSE-----------------------
library(tufte)
knitr::opts_chunk$set(results = "hide", echo = FALSE)

## ---- echo=FALSE, message = FALSE, warning = FALSE----
library("tibble")
x = c(78.64,79.01, 79.57, 79.52, 80.71, 79.95, 78.50,
  79.10, 81.98, 80.09, 80.29, 80.22)
y = c(81.92, 81.12, 82.47, 82.86, 82.89, 82.45,
     82.51, 81.11, 83.07, 82.77, 82.38, 83.14)
dd = tibble(x = x, y = y)

## ---- echo=FALSE---------------------------
x = c(78.64,79.01, 79.57, 79.52, 80.71, 79.95, 78.50,
  79.10, 81.98, 80.09, 80.29, 80.22)
y = c(81.92, 81.12, 82.47, 82.86, 82.89, 82.45,
     82.51, 81.11, 83.07, 82.77, 82.38, 83.14)
dd = tibble(x = x, y = y)
## var(y)
## var.test(x, y)

## ---- message = FALSE, warning = FALSE-----
library("tibble")
##Data for question 1
## Easier using Excel and export as CSV
x = c(78.64,79.01, 79.57, 79.52, 80.71, 79.95, 78.50,
  79.10, 81.98, 80.09, 80.29, 80.22)
y = c(81.92, 81.12, 82.47, 82.86, 82.89, 82.45,
     82.51, 81.11, 83.07, 82.77, 82.38, 83.14)
dd = tibble(x = x, y = y)

## ---- echo=FALSE---------------------------
d1 = tibble(value = x)
d2 = tibble(value = y)

## ----  eval=FALSE--------------------------
#  ## Suppose you have two separate data files. Here is some code that will help ## you combine
#  ## them. First we read in the separate files:
#  d1 = read.csv("Method1.csv")
#  d2 = read.csv("Method2.csv")

## ------------------------------------------
## In order to combine the data frames,
## they must have the same column names:
head(d1, 2)
head(d2, 2)

## ------------------------------------------
## We combine data frames using rbind (row bind)
d = rbind(d1, d2)

## ------------------------------------------
## Finally we create a new column to indicate the Method
## rep is the replicate function. See ?rep
d$Method = rep(1:2, each=12)
head(d, 2)

## ------------------------------------------
t.test(value ~ Method, data=d, var.equal=FALSE)

## ------------------------------------------
t.test(value ~ Method, data=d, var.equal=TRUE)

## ------------------------------------------
x = c(348, 353, 359, 357, 350, 355, 359, 367, 345, 362, 343, 367)
m = chisq.test(x)
##Since p > 0.05 we can't accept the alternative hypothesis.
##However, the question is worded as though we can "prove" the Null
##hypotheis, which we obviously can't do.

## ---- message = FALSE, warning = FALSE-----
library("broom")
m_aug = augment(m)
m_aug$.expected

## ------------------------------------------
m_aug$.residuals

## ---- warning=FALSE------------------------
h = c(17, 8, 22)
nh = c(35, 53, 491)
dd = tibble(h, nh)
m = chisq.test(dd)

## ------------------------------------------
m_aug = augment(m)
m_aug$.expected

## ------------------------------------------
##Some of the expected values are less then 5
##So consider combining cells.

## ---- eval=FALSE---------------------------
#  vignette("solutions1", package = "jrModelling")

