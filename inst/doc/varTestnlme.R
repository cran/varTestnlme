## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(varTestnlme)

## -----------------------------------------------------------------------------
# Load the packages
library(nlme)
library(lme4)
library(saemix)

## ----results='hide'-----------------------------------------------------------
data("Orthodont")

# using nlme, with correlated slope and intercept
m1.nlme <- lme(distance ~ 1 + Sex + age + age*Sex, random = pdDiag(Subject ~ 1 + age), data = Orthodont, method = "ML")
m0.nlme <- lme(distance ~ 1 + Sex + age + age*Sex, random = ~ 1 | Subject, data = Orthodont, method = "ML")

# using lme4, with correlated slope and intercept
m1.lme4 <- lmer(distance ~ 1 + Sex + age + age*Sex + (1 + age | Subject), data = Orthodont, REML = FALSE)
m0.lme4 <- lmer(distance ~ 1 + Sex + age + age*Sex + (1 | Subject), data = Orthodont, REML = FALSE)


## -----------------------------------------------------------------------------
vt1.nlme <- varTest(m1.nlme,m0.nlme)
vt1.lme4 <- varTest(m1.lme4,m0.lme4)

## -----------------------------------------------------------------------------
# using nlme, with uncorrelated slope and intercept
m1diag.nlme <- lme(distance ~ 1 + Sex + age + age*Sex, random = pdDiag(Subject ~ 1 + age), data = Orthodont, method = "ML")

# using lme4, with uncorrelated slope and intercept
m1diag.lme4 <- lmer(distance ~ 1 + Sex + age + age*Sex + (1 + age || Subject), data = Orthodont, REML = FALSE)

vt1diag.nlme <- varTest(m1diag.nlme,m0.nlme)
vt1diag.lme4 <- varTest(m1diag.lme4,m0.lme4)


## -----------------------------------------------------------------------------
m0noRE <- lm(distance ~ 1 + Sex + age + age*Sex, data = Orthodont)

vt <- varTest(m1diag.nlme,m0noRE,pval.comp = "both")
summary(vt)
vt2 <- varTest(m1diag.lme4,m0noRE)
summary(vt2)

## -----------------------------------------------------------------------------
vt <- varTest(m1diag.nlme,m0noRE,fim="compute",pval.comp = "both")
summary(vt)
vt2 <- varTest(m1diag.lme4,m0noRE,fim="compute",pval.comp = "both")
summary(vt2)


## -----------------------------------------------------------------------------
m1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             family = binomial, data = cbpp)
m0 <- glm(cbind(incidence, size - incidence) ~ period,
             family = binomial, data = cbpp)
varTest(m1,m0)

## -----------------------------------------------------------------------------
# with nlme
fm1Theo.nlme <- nlme(conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
                     Theoph, 
                     fixed = lKe + lKa + lCl ~ 1,
                     start=c( -2.4, 0.45, -3.2),
                     random = pdSymm(lKa + lCl ~ 1))
fm2Theo.nlme <- nlme(conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
                     Theoph, 
                     fixed = lKe + lKa + lCl ~ 1,
                     start=c( -2.4, 0.45, -3.2),
                     random = pdDiag(lCl ~ 1))
varTest(fm1Theo.nlme,fm2Theo.nlme, pval.comp = "both")

# with lme4
Th.start  <- c(lKe = -2.4, lKa = 0.45, lCl = -3.2)
nm1  <- nlmer(conc ~ SSfol(Dose , Time ,lKe , lKa , lCl) ~ 
                0+lKe+lKa+lCl +(lKe+lKa+lCl|Subject), 
              nAGQ=0, 
              Theoph,
              start = Th.start)
nm0  <- nlmer(conc ~ SSfol(Dose , Time ,lKe , lKa , lCl) ~ 
                0+lKe+lKa+lCl +(lKa+lCl|Subject), 
              nAGQ=0, 
              Theoph,
              start = Th.start)
varTest(nm1,nm0)

## -----------------------------------------------------------------------------
fm1 <- nlme(conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
                     Theoph, 
                     fixed = lKe + lKa + lCl ~ 1,
                     start=c( -2.4, 0.45, -3.2),
                     random = pdDiag(lKe + lKa + lCl ~ 1))
fm0 <- nls(conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
                     Theoph, 
                     start=list(lKe=-2.4,lKa=0.45,lCl=-3.2))
varTest(fm1,fm0)

