## ----echo=FALSE---------------------------------------------------------------
req_suggested_packages <- c("rtdists", "RWiener", "testthat")
pcheck <- lapply(req_suggested_packages, requireNamespace,
                 quietly = TRUE)
if (any(!unlist(pcheck))) {
   message("Required package(s) for this vignette are not available/installed and code will not be executed.")
   knitr::opts_chunk$set(eval = FALSE)
}

## ----setup, include=FALSE-------------------------------------------------------------------------
op <- options(width = 100)
knitr::opts_chunk$set(
  collapse = TRUE,
  error = TRUE, # ensures compilation even if testthat checks fail
  comment = "#>"
)

## ----validity-pkg, eval=TRUE----------------------------------------------------------------------
library("fddm")
require("rtdists")
require("RWiener")
source(system.file("extdata", "Gondan_et_al_density.R", package = "fddm", mustWork = TRUE))

## ----validity-run, eval=TRUE----------------------------------------------------------------------
# Define parameter space
RT <- c(0.001, 0.1, 1, 10)
A <- c(0.5, 1, 5)
V <- c(-5, 0, 5)
t0 <- 1e-4 # must be nonzero for RWiener
W <- c(0.2, 0.5, 0.8)
SV <- c(0, 0.5, 1.5)
SV_THRESH <- 1e-6
eps <- 1e-6 # this is the setting from rtdists

nRT <- length(RT)
nA <- length(A)
nV <- length(V)
nW <- length(W)
nSV <- length(SV)
N <- nRT * nA * nV * nW * nSV
twoN <- 2 * N

rt <- rep(RT, each = nSV * nW * nV * nA, times = 1)
a  <- rep(A,  each = nSV * nW * nV, times = nRT)
v  <- rep(V,  each = nSV * nW, times = nRT * nA)
w  <- rep(W,  each = nSV, times = nRT * nA * nV)
sv <- rep(SV, each = 1, times = nRT * nA * nV * nW)

# fddm methods
SWSE_s_17 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "small",
              n_terms_small = "SWSE", summation_small = "2017"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "small",
                  n_terms_small = "SWSE", summation_small = "2017")
)
SWSE_s_14 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "small",
              n_terms_small = "SWSE", summation_small = "2014"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "small",
                  n_terms_small = "SWSE", summation_small = "2014")
)
SWSE_t_17 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "eff_rt",
              n_terms_small = "SWSE", summation_small = "2017"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "eff_rt",
                  n_terms_small = "SWSE", summation_small = "2017")
)
SWSE_t_14 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "eff_rt",
              n_terms_small = "SWSE", summation_small = "2014"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "eff_rt",
                  n_terms_small = "SWSE", summation_small = "2014")
)
SWSE_b_17 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "terms_large",
              n_terms_small = "SWSE", summation_small = "2017"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE,
                  switch_mech = "terms_large", n_terms_small = "SWSE",
                  summation_small = "2017")
)
SWSE_b_14 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "terms_large",
              n_terms_small = "SWSE", summation_small = "2014"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE,
                  switch_mech = "terms_large", n_terms_small = "SWSE",
                  summation_small = "2014")
)
Gondan_s_17 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "small",
              n_terms_small = "Gondan", summation_small = "2017"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "small",
                  n_terms_small = "Gondan", summation_small = "2017")
)
Gondan_s_14 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "small",
              n_terms_small = "Gondan", summation_small = "2014"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "small",
                  n_terms_small = "Gondan", summation_small = "2014")
)
Gondan_b_17 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "terms",
              n_terms_small = "Gondan", summation_small = "2017"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "terms",
                  n_terms_small = "Gondan", summation_small = "2017")
)
Gondan_b_14 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "terms",
              n_terms_small = "Gondan", summation_small = "2014"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "terms",
                  n_terms_small = "Gondan", summation_small = "2014")
)
Navarro_s_17 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "small",
              n_terms_small = "Navarro", summation_small = "2017"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "small",
                  n_terms_small = "Navarro", summation_small = "2017")
)
Navarro_s_14 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "small",
              n_terms_small = "Navarro", summation_small = "2014"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "small",
                  n_terms_small = "Navarro", summation_small = "2014")
)
Navarro_b_17 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "terms",
              n_terms_small = "Navarro", summation_small = "2017"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "terms",
                  n_terms_small = "Navarro", summation_small = "2017")
)
Navarro_b_14 <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "terms",
              n_terms_small = "Navarro", summation_small = "2014"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "terms",
                  n_terms_small = "Navarro", summation_small = "2014")
)
Navarro_l <- data.frame(
  res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
              sv = sv, err_tol = eps, log = FALSE, switch_mech = "large",
              n_terms_small = "Navarro"),
  dif = numeric(N),
  log_res = dfddm(rt = rt, response = "lower", a = a, v = v, t0 = t0, w = w,
                  sv = sv, err_tol = eps, log = TRUE, switch_mech = "large",
                  n_terms_small = "Navarro")
)

# non-fddm methods
t <- rt - t0
M <- exp(v * a * w + v*v * t / 2 +
         (sv*sv * a*a * w*w - 2 * v * a * w - v*v * t) / (2 + 2 * sv*sv * t)
        ) / sqrt(1 + sv*sv * t)
if (require("RWiener")) {
  RWiener  <- data.frame(
    res = numeric(N),
    dif = numeric(N)
  )
  for (i in 1:N) { # RWiener can't handle model parameters as vectors
    RWiener[i, "res"] <- dwiener(rt[i], resp = "lower", alpha = a[i],
                                 delta = v[i], tau = t0, beta = w[i],
                                 give_log = FALSE)
  }
  RWiener[["res"]] <- M * RWiener[["res"]]
}
Gondan_R <- data.frame(
  res = M * fs(t = t, a = a, v = v, w = w, eps = eps),
  dif = numeric(N)
)
if (require("rtdists")) {
  rtdists <- data.frame(
    res = ddiffusion(rt, "lower", a = a, v = v, t0 = t0, z = w*a, sv = sv),
    dif = numeric(N)
  )
}

# Calculate differences (use fddm's SWSE_t_17 method as truth)
ans <- SWSE_t_17[["res"]]
SWSE_s_17[["dif"]] <- abs(SWSE_s_17[["res"]] - ans)
SWSE_s_14[["dif"]] <- abs(SWSE_s_14[["res"]] - ans)
SWSE_t_17[["dif"]] <- abs(SWSE_t_17[["res"]] - ans)
SWSE_t_14[["dif"]] <- abs(SWSE_t_14[["res"]] - ans)
SWSE_b_17[["dif"]] <- abs(SWSE_b_17[["res"]] - ans)
SWSE_b_14[["dif"]] <- abs(SWSE_b_14[["res"]] - ans)
Gondan_s_17[["dif"]] <- abs(Gondan_s_17[["res"]] - ans)
Gondan_s_14[["dif"]] <- abs(Gondan_s_14[["res"]] - ans)
Gondan_b_17[["dif"]] <- abs(Gondan_b_17[["res"]] - ans)
Gondan_b_14[["dif"]] <- abs(Gondan_b_14[["res"]] - ans)
Navarro_s_17[["dif"]] <- abs(Navarro_s_17[["res"]] - ans)
Navarro_s_14[["dif"]] <- abs(Navarro_s_14[["res"]] - ans)
Navarro_b_17[["dif"]] <- abs(Navarro_b_17[["res"]] - ans)
Navarro_b_14[["dif"]] <- abs(Navarro_b_14[["res"]] - ans)
Navarro_l[["dif"]] <- abs(Navarro_l[["res"]] - ans)
if (require("RWiener")) {
  RWiener[["dif"]] <- abs(RWiener[["res"]] - ans)
}
Gondan_R[["dif"]] <- abs(Gondan_R[["res"]] - ans)
if (require("rtdists")) {
  rtdists[["dif"]] <- abs(rtdists[["res"]] - ans)
}

## ----validity-test, eval=TRUE---------------------------------------------------------------------
library("testthat")

# Ensure all densities are non-negative
test_that("Non-negativity of densities", {
  expect_true(all(SWSE_s_17[["res"]] >= 0))
  expect_true(all(SWSE_s_14[["res"]] >= 0))
  expect_true(all(SWSE_t_17[["res"]] >= 0))
  expect_true(all(SWSE_t_14[["res"]] >= 0))
  expect_true(all(SWSE_b_17[["res"]] >= 0))
  expect_true(all(SWSE_b_14[["res"]] >= 0))
  expect_true(all(Gondan_s_17[["res"]] >= 0))
  expect_true(all(Gondan_s_14[["res"]] >= 0))
  expect_true(all(Gondan_b_17[["res"]] >= 0))
  expect_true(all(Gondan_b_14[["res"]] >= 0))
  expect_true(all(Navarro_s_17[["res"]] >= 0))
  expect_true(all(Navarro_s_14[["res"]] >= 0))
  expect_true(all(Navarro_b_17[["res"]] >= 0))
  expect_true(all(Navarro_b_14[["res"]] >= 0))
  expect_true(all(Navarro_l[["res"]] >= 0))
  if (require("RWiener")) {
    expect_true(all(RWiener[["res"]] >= 0))
  }
  expect_true(all(Gondan_R[["res"]] >= -eps)) # density between 0 and -eps := 0
  if (require("rtdists")) {
    expect_true(all(rtdists[["res"]] >= 0))
  }
})

# Test accuracy within 2*eps (allows for convergence from above and below)
test_that("Consistency among internal methods", {
  expect_true(all(SWSE_s_17[["dif"]] <= 2 * eps))
  expect_true(all(SWSE_s_14[["dif"]] <= 2 * eps))
  expect_true(all(SWSE_t_17[["dif"]] <= 2 * eps))
  expect_true(all(SWSE_t_14[["dif"]] <= 2 * eps))
  expect_true(all(SWSE_b_17[["dif"]] <= 2 * eps))
  expect_true(all(SWSE_b_14[["dif"]] <= 2 * eps))
  expect_true(all(Gondan_s_17[["dif"]] <= 2 * eps))
  expect_true(all(Gondan_s_14[["dif"]] <= 2 * eps))
  expect_true(all(Gondan_b_17[["dif"]] <= 2 * eps))
  expect_true(all(Gondan_b_14[["dif"]] <= 2 * eps))
  expect_true(all(Navarro_s_17[["dif"]] <= 2 * eps))
  expect_true(all(Navarro_s_14[["dif"]] <= 2 * eps))
  expect_true(all(Navarro_b_17[["dif"]] <= 2 * eps))
  expect_true(all(Navarro_b_14[["dif"]] <= 2 * eps))
  testthat::skip_on_os("solaris")
  testthat::skip_if(dfddm(rt = 0.001, response = "lower",
                          a = 5, v = -5, t0 = 1e-4, w = 0.8, sv = 1.5,
                          err_tol = 1e-6, log = FALSE, switch_mech = "large") >
                    1e-6)
  expect_true(all(Navarro_l[rt/a/a >= 0.009, "dif"] < 2 * eps)) # see KE 1
})

test_that("Accuracy relative to established packages", {
  if (require("RWiener")) {
    expect_true(all(RWiener[sv < SV_THRESH, "dif"] <= 2 * eps)) # see KE 2
  }
  if (require("rtdists")) {
    expect_true(all(rtdists[["dif"]] <= 2 * eps))
  }
  testthat::skip_on_os("solaris")
  testthat::skip_if(dfddm(rt = 0.001, response = "lower",
                          a = 5, v = -5, t0 = 1e-4, w = 0.8, sv = 1.5,
                          err_tol = 1e-6, log = FALSE, switch_mech = "large") >
                    1e-6)
  expect_true(all(Gondan_R[sv < SV_THRESH, "dif"] <= 2 * eps)) # see KE 2
})

# Test consistency in fddm log vs non-log (see KE 3)
test_that("Log-Consistency among internal methods", {
  expect_equal(SWSE_s_17[SWSE_s_17[["res"]] > eps*eps, "log_res"],
               log(SWSE_s_17[SWSE_s_17[["res"]] > eps*eps, "res"]))
  expect_equal(SWSE_s_14[SWSE_s_14[["res"]] > eps*eps, "log_res"],
               log(SWSE_s_14[SWSE_s_14[["res"]] > eps*eps, "res"]))
  expect_equal(SWSE_t_17[SWSE_t_17[["res"]] > eps*eps, "log_res"],
               log(SWSE_t_17[SWSE_t_17[["res"]] > eps*eps, "res"]))
  expect_equal(SWSE_t_14[SWSE_t_14[["res"]] > eps*eps, "log_res"],
               log(SWSE_t_14[SWSE_t_14[["res"]] > eps*eps, "res"]))
  expect_equal(SWSE_b_17[SWSE_b_17[["res"]] > eps*eps, "log_res"],
               log(SWSE_b_17[SWSE_b_17[["res"]] > eps*eps, "res"]))
  expect_equal(SWSE_b_14[SWSE_b_14[["res"]] > eps*eps, "log_res"],
               log(SWSE_b_14[SWSE_b_14[["res"]] > eps*eps, "res"]))
  expect_equal(Gondan_s_17[Gondan_s_17[["res"]] > eps*eps, "log_res"],
               log(Gondan_s_17[Gondan_s_17[["res"]] > eps*eps, "res"]))
  expect_equal(Gondan_s_14[Gondan_s_14[["res"]] > eps*eps, "log_res"],
               log(Gondan_s_14[Gondan_s_14[["res"]] > eps*eps, "res"]))
  expect_equal(Gondan_b_17[Gondan_b_17[["res"]] > eps*eps, "log_res"],
               log(Gondan_b_17[Gondan_b_17[["res"]] > eps*eps, "res"]))
  expect_equal(Gondan_b_14[Gondan_b_14[["res"]] > eps*eps, "log_res"],
               log(Gondan_b_14[Gondan_b_14[["res"]] > eps*eps, "res"]))
  expect_equal(Navarro_s_17[Navarro_s_17[["res"]] > eps*eps, "log_res"],
               log(Navarro_s_17[Navarro_s_17[["res"]] > eps*eps, "res"]))
  expect_equal(Navarro_s_14[Navarro_s_14[["res"]] > eps*eps, "log_res"],
               log(Navarro_s_14[Navarro_s_14[["res"]] > eps*eps, "res"]))
  expect_equal(Navarro_b_17[Navarro_b_17[["res"]] > eps*eps, "log_res"],
               log(Navarro_b_17[Navarro_b_17[["res"]] > eps*eps, "res"]))
  expect_equal(Navarro_b_14[Navarro_b_14[["res"]] > eps*eps, "log_res"],
               log(Navarro_b_14[Navarro_b_14[["res"]] > eps*eps, "res"]))
  expect_equal(Navarro_l[Navarro_l[["res"]] > eps*eps, "log_res"],
               log(Navarro_l[Navarro_l[["res"]] > eps*eps, "res"]))
})

## ----known-errors, eval=TRUE----------------------------------------------------------------------
rt <- 1.5
t <- rt - 1e-4
a <- 0.5
v <- 4.5
w <- 0.5
eps <- 1e-6
sv <- 0.9
sv0 <- exp(-v*a*w - v*v*t/2) / (a*a) # for constant drift rate
sv0_9 <- exp((-2*v*a*w - v*v*t + sv*sv*a*a*w*w)/(2 + 2*sv*sv*t)) /
         (a*a*sqrt(1+sv*sv*t)) # for variable drift rate
ks_0 <- ks(t/(a*a), w, eps/sv0) # = 2; the summation will only calculate 2 terms
ks_9 <- ks(t/(a*a), w, eps/sv0_9) # = 5; but the summation needs 5 terms

cat("the summation will only calculate", ks_0, "terms, but it needs", ks_9, "terms.")

## ----fitting-pkg, eval=TRUE-----------------------------------------------------------------------
library("fddm")
library("rtdists")

## ----loglik-fun, eval=TRUE------------------------------------------------------------------------
ll_fb_SWSE_17 <- function(pars, rt, resp, truth, err_tol) {
  v <- numeric(length(rt))
  v[truth == "upper"] <- pars[[1]]
  v[truth == "lower"] <- pars[[2]]
  dens <- dfddm(rt = rt, response = resp, a = pars[[3]], v = v,
                t0 = pars[[4]], w = pars[[5]], sv = pars[[6]], err_tol = 1e-6,
                log = TRUE, switch_mech = "terms_large", switch_thresh = 0.8,
                n_terms_small = "SWSE", summation_small = "2017")
  return( ifelse(any(!is.finite(dens)), 1e6, -sum(dens)) )
}

ll_fb_Gon_17 <- function(pars, rt, resp, truth, err_tol) {
  v <- numeric(length(rt))
  v[truth == "upper"] <- pars[[1]]
  v[truth == "lower"] <- pars[[2]]
  dens <- dfddm(rt = rt, response = resp, a = pars[[3]], v = v,
                t0 = pars[[4]], w = pars[[5]], sv = pars[[6]], err_tol = 1e-6,
                log = TRUE, switch_mech = "terms", n_terms_small = "Gondan",
                summation_small = "2017")
  return( ifelse(any(!is.finite(dens)), 1e6, -sum(dens)) )
}

ll_fb_Nav_17 <- function(pars, rt, resp, truth, err_tol) {
  v <- numeric(length(rt))
  v[truth == "upper"] <- pars[[1]]
  v[truth == "lower"] <- pars[[2]]
  dens <- dfddm(rt = rt, response = resp, a = pars[[3]], v = v,
                  t0 = pars[[4]], w = pars[[5]], sv = pars[[6]], err_tol = 1e-6,
                  log = TRUE, switch_mech = "terms", n_terms_small = "Navarro",
                  summation_small = "2017")
  return( ifelse(any(!is.finite(dens)), 1e6, -sum(dens)) )
}

ll_RTDists <- function(pars, rt, resp, truth) {
  rtu <- rt[truth == "upper"]
  rtl <- rt[truth == "lower"]
  respu <- resp[truth == "upper"]
  respl <- resp[truth == "lower"]

  densu <- ddiffusion(rtu, respu, a = pars[[3]], v = pars[[1]],
                      z = pars[[5]]*pars[[3]], t0 = pars[[4]], sv = pars[[6]])
  densl <- ddiffusion(rtl, respl, a = pars[[3]], v = pars[[2]],
                      z = pars[[5]]*pars[[3]], t0 = pars[[4]], sv = pars[[6]])

  densities <- c(densu, densl)
  if (any(densities <= 0)) return(1e6)
  return(-sum(log(densities)))
}

## ----fitting-fun, eval=TRUE-----------------------------------------------------------------------
rt_fit <- function(data, id_idx = NULL, rt_idx = NULL, response_idx = NULL,
                   truth_idx = NULL, response_upper = NULL, err_tol = 1e-6) {

  # Format data for fitting
  if (all(is.null(id_idx), is.null(rt_idx), is.null(response_idx),
      is.null(truth_idx), is.null(response_upper))) {
    df <- data # assume input data is already formatted
  } else {
    if(any(data[,rt_idx] < 0)) {
      stop("Input data contains negative response times; fit will not be run.")
    }
    if(any(is.na(data[,response_idx]))) {
      stop("Input data contains invalid responses (NA); fit will not be run.")
    }

    nr <- nrow(data)
    df <- data.frame(id = character(nr),
                     rt = double(nr),
                     response = character(nr),
                     truth = character(nr),
                     stringsAsFactors = FALSE)

    if (!is.null(id_idx)) { # relabel identification tags
      for (i in 1:length(id_idx)) {
        idi <- unique(data[,id_idx[i]])
        for (j in 1:length(idi)) {
          df[["id"]][data[,id_idx[i]] == idi[j]] <- paste(
            df[["id"]][data[,id_idx[i]] == idi[j]], idi[j], sep = " ")
        }
      }
      df[["id"]] <- trimws(df[["id"]], which = "left")
    }

    df[["rt"]] <- as.double(data[,rt_idx])

    df[["response"]] <- "lower"
    df[["response"]][data[,response_idx] == response_upper] <- "upper"

    df[["truth"]] <- "lower"
    df[["truth"]][data[,truth_idx] == response_upper] <- "upper"
  }

  # Preliminaries
  ids <- unique(df[["id"]])
  nids <- max(length(ids), 1) # if inds is null, there is only one individual

  init_vals <- data.frame(vu = c( 0,  10, -.5,  0,  0,  0,  0,  0,  0,   0,  0),
                          vl = c( 0, -10,  .5,  0,  0,  0,  0,  0,  0,   0,  0),
                          a  = c( 1,   1,   1, .5,  5,  1,  1,  1,  1,   1,  1),
                          t0 = c( 0,   0,   0,  0,  0,  0,  0,  0,  0,   0,  0),
                          w  = c(.5,  .5,  .5, .5, .5, .5, .5, .2, .8,  .5, .5),
                          sv = c( 1,   1,   1,  1,  1,  1,  1,  1,  1, .05,  5))
  ninit_vals <- nrow(init_vals)

  algo_names <- c("fb_SWSE_17", "fb_Gon_17", "fb_Nav_17", "rtdists")
  nalgos <- length(algo_names)
  ni <- nalgos*ninit_vals

  # Initilize the result dataframe
  cnames <- c("ID", "Algorithm", "Convergence", "Objective",
              "vu_init", "vl_init", "a_init", "t0_init", "w_init", "sv_init",
              "vu_fit", "vl_fit", "a_fit", "t0_fit", "w_fit", "sv_fit")
  res <- data.frame(matrix(ncol = length(cnames), nrow = nids*ninit_vals*nalgos))
  colnames(res) <- cnames

  # label the result dataframe
  res[["ID"]] <- rep(ids, each = ni) # label individuals
  res[["Algorithm"]] <- rep(algo_names, each = ninit_vals) # label algorithms
  res[["vu_init"]] <- init_vals[["vu"]] # label initial vu
  res[["vl_init"]] <- init_vals[["vl"]] # label initial vl
  res[["a_init"]]  <- init_vals[["a"]]  # label initial a
  res[["w_init"]]  <- init_vals[["w"]]  # label initial w
  res[["sv_init"]] <- init_vals[["sv"]] # label initial sv

  # Loop through each individual and starting values
  for (i in 1:nids) {
    # extract data for id i
    dfi <- df[df[["id"]] == ids[i], ]
    rti <- dfi[["rt"]]
    respi <- dfi[["response"]]
    truthi <- dfi[["truth"]]

    # starting value for t0 must be smaller than the smallest rt
    min_rti <- min(rti)
    t0_lo <- 0.01*min_rti
    t0_me <- 0.50*min_rti
    t0_hi <- 0.99*min_rti
    init_vals[["t0"]] <- c(rep(t0_me, 5), t0_lo, t0_hi, rep(t0_me, 4))

    # label the result dataframe
    res[["t0_init"]][((i-1)*ni+1):(i*ni)] <- init_vals[["t0"]] # label initial t0

    # loop through all of the starting values
    for (j in 1:ninit_vals) {
      temp <- nlminb(init_vals[j, ], ll_fb_SWSE_17,
                     rt = rti, resp = respi, truth = truthi, err_tol = err_tol,
                     # limits:   vu,   vl,   a,      t0, w,  sv
                     lower = c(-Inf, -Inf, .01,       0, 0,   0),
                     upper = c( Inf,  Inf, Inf, min_rti, 1, Inf))
      res[["Convergence"]][(i-1)*ni+0*ninit_vals+j] <- temp[["convergence"]]
      res[["Objective"]][(i-1)*ni+0*ninit_vals+j] <- temp[["objective"]]
      res[(i-1)*ni+0*ninit_vals+j, 11:16] <- temp[["par"]]

      temp <- nlminb(init_vals[j, ], ll_fb_Gon_17,
                     rt = rti, resp = respi, truth = truthi, err_tol = err_tol,
                     # limits:   vu,   vl,   a,      t0, w,  sv
                     lower = c(-Inf, -Inf, .01,       0, 0,   0),
                     upper = c( Inf,  Inf, Inf, min_rti, 1, Inf))
      res[["Convergence"]][(i-1)*ni+1*ninit_vals+j] <- temp[["convergence"]]
      res[["Objective"]][(i-1)*ni+1*ninit_vals+j] <- temp[["objective"]]
      res[(i-1)*ni+1*ninit_vals+j, 11:16] <- temp[["par"]]

      temp <- nlminb(init_vals[j, ], ll_fb_Nav_17,
                     rt = rti, resp = respi, truth = truthi, err_tol = err_tol,
                     # limits:   vu,   vl,   a,      t0, w,  sv
                     lower = c(-Inf, -Inf, .01,       0, 0,   0),
                     upper = c( Inf,  Inf, Inf, min_rti, 1, Inf))
      res[["Convergence"]][(i-1)*ni+2*ninit_vals+j] <- temp[["convergence"]]
      res[["Objective"]][(i-1)*ni+2*ninit_vals+j] <- temp[["objective"]]
      res[(i-1)*ni+2*ninit_vals+j, 11:16] <- temp[["par"]]

      temp <- nlminb(init_vals[j, ], ll_RTDists,
                     rt = rti, resp = respi, truth = truthi,
                     # limits:   vu,   vl,   a,      t0, w,  sv
                     lower = c(-Inf, -Inf, .01,       0, 0,   0),
                     upper = c( Inf,  Inf, Inf, min_rti, 1, Inf))
      res[["Convergence"]][(i-1)*ni+3*ninit_vals+j] <- temp[["convergence"]]
      res[["Objective"]][(i-1)*ni+3*ninit_vals+j] <- temp[["objective"]]
      res[(i-1)*ni+3*ninit_vals+j, 11:16] <- temp[["par"]]
    }
  }
  return(res)
}

## ----fitting-run, eval=FALSE----------------------------------------------------------------------
#  data(med_dec, package = "fddm")
#  med_dec <- med_dec[which(med_dec[["rt"]] >= 0), ]
#  fit <- rt_fit(med_dec, id_idx = c(2,1), rt_idx = 8, response_idx = 7,
#                truth_idx = 5, response_upper = "blast", err_tol = 1e-6)

## ----fitting-run-internal, eval=FALSE, include=FALSE----------------------------------------------
#  save(fit, compress = "xz", compression_level = 9,
#       file = "inst/extdata/valid_fit.Rds")

## ----fitting-prep, eval=TRUE----------------------------------------------------------------------
fit_prep <- function(fit) {
  nr <- nrow(fit)
  fit[["Obj_diff"]] <- rep(0, nr)
  fit[["vu_diff"]]  <- rep(0, nr)
  fit[["vl_diff"]]  <- rep(0, nr)
  fit[["a_diff"]]   <- rep(0, nr)
  fit[["t0_diff"]]  <- rep(0, nr)
  fit[["w_diff"]]   <- rep(0, nr)
  fit[["sv_diff"]]  <- rep(0, nr)

  ids <- unique(fit[["ID"]])
  nids <- length(ids)
  algos <- unique(fit[["Algorithm"]])
  nalgos <- length(algos)

  fit_idx <- c(4, 11:16)
  dif_idx <- 17:23
  ninit <- nrow(fit[fit[["ID"]] == ids[1] & fit[["Algorithm"]] == algos[1], ])
  for (i in 1:nids) {
    for (j in 1:ninit) {
      actual_idx <- seq((i-1)*ninit*nalgos+j, i*ninit*nalgos, by = ninit)
      min_obj_idx <- actual_idx[which.min(fit[actual_idx, 4])]
      best_fit <- fit[min_obj_idx, fit_idx]
      for (k in 0:(nalgos-1)) {
        fit[(i-1)*(ninit*nalgos) + k*ninit + j, dif_idx] <-
          fit[(i-1)*(ninit*nalgos) + k*ninit + j, fit_idx] - best_fit

      }
    }
  }
  return(fit)
}

## ----fit-load, eval=TRUE--------------------------------------------------------------------------
# load data, will be in the variable 'fit'
load(system.file("extdata", "dfddm_density", "valid_fit.Rds", package = "fddm", mustWork = TRUE))
fit <- fit_prep(fit)

cat("Results for ID = experienced 2")
fit[(0:3)*11+1, ]

## ----fit-estimates, eval=TRUE---------------------------------------------------------------------
# Define error tolerance
eps <- 1e-4

out <- fit[unique(which(abs(fit[, c(3, 17:23)]) > eps, arr.ind = TRUE)[, 1]), ]
out[, -c(1:2)] <- zapsmall(out[, -c(1:2)])
out

## ----session-info, collapse=TRUE------------------------------------------------------------------
sessionInfo()

## ----reset-options, include=FALSE---------------------------------------------
options(op)  # reset options

