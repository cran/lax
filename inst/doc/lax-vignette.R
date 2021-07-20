## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## -----------------------------------------------------------------------------
library(lax)
# Column 4 of ow contains 1 for Oxford and -1 for Worthing
large <- gev_refit(ow$temp, ow, mul = 4, sigl = 4, shl = 4, show = FALSE,
                   method = "BFGS")
# Adjust the loglikelihood and standard errors
adj_large <- alogLik(large, cluster = ow$year, cadjust = FALSE)
# MLEs, SEs and adjusted SEs
t(summary(adj_large))

## -----------------------------------------------------------------------------
confint(adj_large)
confint(adj_large, type = "none")

## ---- fig.align='center', fig.width=7, fig.height=7---------------------------
library(chandwich)
which_pars <- c("scale", "shape")
gev_none <- conf_region(adj_large, which_pars = which_pars, type = "none")
gev_vertical <- conf_region(adj_large, which_pars = which_pars)
plot(gev_none, gev_vertical, lwd = 2, xlim = c(3.1, 4.5), ylim = c(-0.35, -0.05),
     xlab = expression(sigma[0]), ylab = expression(xi[0]))

## -----------------------------------------------------------------------------
small <- gev_refit(ow$temp, ow, mul = 4, sigl = 4, show = FALSE, 
                   method = "BFGS")
adj_small <- alogLik(small, cluster = ow$year, cadjust = FALSE)
summary(adj_small)
anova(adj_large, adj_small)
anova(adj_large, adj_small, type = "none")

## ---- echo = FALSE------------------------------------------------------------
got_texmex <- requireNamespace("texmex", quietly = TRUE)

## ---- eval = got_texmex-------------------------------------------------------
library(texmex, quietly = TRUE)
# Note: phi = log(scale)
evm_fit <- evm(temp, ow, gev, mu = ~ loc, phi = ~ loc, xi = ~loc)
adj_evm_fit <- alogLik(evm_fit, cluster = ow$year)
summary(adj_evm_fit)

## ---- echo = FALSE, eval = got_texmex-----------------------------------------
detach("package:texmex")

## ---- echo = FALSE------------------------------------------------------------
got_evd <- requireNamespace("evd", quietly = TRUE)

## ---- eval = got_evd----------------------------------------------------------
library(evd, quietly = TRUE)
fgev_fit <- fgev(ow$temp, nsloc = ow[, "loc"])
adj_fgev_fit <- alogLik(fgev_fit, cluster = ow$year)
summary(adj_fgev_fit)

## ---- echo = FALSE, eval = got_evd--------------------------------------------
detach("package:evd")

## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
got_extRemes <- requireNamespace("extRemes", quietly = TRUE)

## ---- eval = got_extRemes, message = FALSE, warning = FALSE-------------------
library(extRemes, quietly = TRUE)
fevd_fit <- fevd(temp, ow, location.fun = ~ ow$loc, scale.fun = ~ ow$loc,
                 shape.fun = ~ ow$loc)
adj_fevd_fit <- alogLik(fevd_fit, cluster = ow$year)
summary(adj_fevd_fit)

## ---- echo = FALSE, eval = got_extRemes---------------------------------------
detach("package:extRemes")

## ---- echo = FALSE, message = FALSE-------------------------------------------
got_eva <- requireNamespace("eva", quietly = TRUE)

## ---- eval = got_extRemes, message = FALSE, warning = FALSE-------------------
library(eva, quietly = TRUE)
gevr_fit <- gevrFit(ow$temp, information = "observed",
                    locvars = ow, locform = ~ ow$loc, 
                    scalevars = ow, scaleform = ~ ow$loc,
                    shapevars = ow, shapeform = ~ ow$loc)
adj_gevr_fit <- alogLik(gevr_fit, cluster = ow$year)
summary(adj_gevr_fit)

## ---- echo = FALSE, eval = got_eva--------------------------------------------
detach("package:eva")

## ---- echo = FALSE------------------------------------------------------------
got_evir <- requireNamespace("evir", quietly = TRUE)

## ---- eval = got_evir, message = FALSE----------------------------------------
library(evir, quietly = TRUE)
gev_fit <- gev(ow$temp)
adj_gev_fit <- alogLik(gev_fit)
summary(adj_gev_fit)

## ---- echo = FALSE, eval = got_evir-------------------------------------------
detach("package:evir")

## ---- echo = FALSE------------------------------------------------------------
got_fExtremes <- requireNamespace("fExtremes", quietly = TRUE)

## ---- eval = got_fExtremes----------------------------------------------------
library(fExtremes, quietly = TRUE)
gevFit_fit <- gevFit(ow$temp)
adj_gevFit_fit <- alogLik(gevFit_fit)
summary(adj_gevFit_fit)

## ---- echo = FALSE, eval = got_fExtremes--------------------------------------
detach("package:fExtremes")

## ---- echo = FALSE, message = FALSE-------------------------------------------
got_mev <- requireNamespace("mev", quietly = TRUE)

## ---- eval = got_mev----------------------------------------------------------
library(mev, quietly = TRUE)
gfit <- fit.gev(ow$temp)
adj_gfit <- alogLik(gfit)
summary(adj_gfit)

## ---- echo = FALSE, eval = got_mev--------------------------------------------
detach("package:mev")

## ---- echo = FALSE, message = FALSE-------------------------------------------
got_POT <- requireNamespace("POT", quietly = TRUE)

## ---- eval = got_POT----------------------------------------------------------
library(POT, quietly = TRUE)
set.seed(24082019)
x <- POT::rgpd(200, 1, 2, 0.25)
fit <- fitgpd(x, 1, "mle")
adj_fit <- alogLik(fit)
summary(adj_fit)

