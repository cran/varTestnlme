## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup, results='hide'----------------------------------------------------
library(varTestnlme)

## -----------------------------------------------------------------------------
# Load the packages
library(nlme)
library(lme4)
library(saemix)

## ----results='hide'-----------------------------------------------------------

data("Orthodont")

# using nlme, ith correlated slope and intercept
m1 <- lme(distance ~ 1 + Sex + age + age*Sex, random = ~ 1 + age | Subject, data = Orthodont, method = "ML")
m0 <- lme(distance ~ 1 + Sex + age + age*Sex, random = ~ 1 | Subject, data = Orthodont, method = "ML")

## -----------------------------------------------------------------------------
vt1.nlme <- varTest(m1,m0)

## -----------------------------------------------------------------------------
m1 <- lme(distance ~ 1 + Sex + age + age*Sex, random = list(Subject = pdDiag(~1+age)), data = Orthodont, method = "ML")
m0 <- lm(distance ~ 1 + Sex + age + age*Sex, data = Orthodont)

varTest(m1,m0)

## -----------------------------------------------------------------------------
varTest(m1,m0,pval.comp = "both")

