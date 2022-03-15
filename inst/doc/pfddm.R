## ----echo=FALSE---------------------------------------------------------------
req_suggested_packages <- c("rtdists", "microbenchmark", 
                            "reshape2", "ggplot2", "ggforce")
pcheck <- lapply(req_suggested_packages, requireNamespace, 
                 quietly = TRUE)
if (any(!unlist(pcheck))) {
   message("Required package(s) for this vignette are not available/installed and code will not be executed.")
   knitr::opts_chunk$set(eval = FALSE)
}

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  error = TRUE,
  comment = "#>"
)

## ----bm-par-space-------------------------------------------------------------
# Define parameter space
RT <- seq(0.1, 2, by = 0.1)
A <- seq(0.5, 3.5, by = 0.5)
V <- c(-5, -2, 0, 2, 5)
t0 <- 0
W <- seq(0.3, 0.7, by = 0.1)
SV <- c(0, 1, 2, 3.5)
err_tol <- 1e-6 # this is the setting from rtdists

## ----bm-packages--------------------------------------------------------------
# packages for generating benchmark data
library("fddm")
source(system.file("extdata", "Blurton_et_al_distribution.R",
                   package = "fddm", mustWork = TRUE))
library("rtdists")
library("microbenchmark")

# packages for plotting
library("reshape2")
library("ggplot2")
library("ggforce")

## ----bm-vec-fun---------------------------------------------------------------
rt_benchmark_vec <- function(RT, resp, V, A, t0 = 1e-4, W = 0.5, SV = 0.0,
                             err_tol = 1e-6, times = 1000) {

  fnames <- c("Mills", "NCDF", "Blurton", "rtdists")
  nf <- length(fnames) # number of functions being benchmarked
  nV <- length(V)
  nA <- length(A)
  nW <- length(W)
  nSV <- length(SV)
  resp <- rep(resp, length(RT)) # for RWiener

  # Initialize the dataframe to contain the microbenchmark results
  mbm_res <- data.frame(matrix(ncol = 4+nf, nrow = nV*nA*nW*nSV))
  colnames(mbm_res) <- c('V', 'A', 'W', 'SV', fnames)
  row_idx <- 1

  # Loop through each combination of parameters and record microbenchmark results
  for (v in 1:nV) {
    for (a in 1:nA) {
      for (w in 1:nW) {
        for (sv in 1:nSV) {
          mbm <- microbenchmark(
          Mills = pfddm(rt = RT, response = resp, a = A[a], v = V[v], t0 = t0,
                        w = W[w], sv = SV[sv], log = FALSE, method = "Mills",
                        err_tol = err_tol),
          NCDF = pfddm(rt = RT, response = resp, a = A[a], v = V[v], t0 = t0,
                       w = W[w], sv = SV[sv], log = FALSE, method = "NCDF",
                       err_tol = err_tol),
          Blurton = G_0(t = RT-t0, a = A[a], nu = V[v], w = W[w],
                        eta2 = SV[sv]*SV[sv], sigma2 = 1, eps = err_tol), # only "lower" resp
          rtdists = pdiffusion(RT, resp, a = A[a], v = V[v], t0 = t0,
                               z = W[w]*A[a], sv = SV[sv], precision = 3),
          times = times)
        # add the v, a, and w values to the dataframe
        mbm_res[row_idx, 1] <- V[v]
        mbm_res[row_idx, 2] <- A[a]
        mbm_res[row_idx, 3] <- W[w]
        mbm_res[row_idx, 4] <- SV[sv]
        # add the median microbenchmark results to the dataframe
        for (i in 1:nf) {
          mbm_res[row_idx, 4+i] <- median(mbm[mbm[,1] == fnames[i],2])
        }
        # iterate start value
        row_idx = row_idx + 1
        }
      }
    }
  }
  return(mbm_res)
}

## ----bm-vec-run, eval=FALSE---------------------------------------------------
#  bm_vec <- rt_benchmark_vec(RT = RT, resp = "lower", V = V, A = A, t0 = t0,
#                             W = W, SV = SV, err_tol = err_tol, times = 1000)

## ----bm-vec-save, eval=FALSE, include=FALSE-----------------------------------
#  save(bm_vec, compress = "xz", compression_level = 9,
#       file = "../inst/extdata/pfddm_distribution/bm_vec_0-2.Rds")

## ----bm-vec, fig.height=5, fig.width=8----------------------------------------
# load data, will be in the variable 'bm_vec'
load(system.file("extdata", "pfddm_distribution", "bm_vec_0-2.Rds",
                 package = "fddm", mustWork = TRUE))

t_idx <- match("SV", colnames(bm_vec))
bm_vec[, -seq_len(t_idx)] <- bm_vec[, -seq_len(t_idx)]/1000 # convert to microseconds
mbm_vec <- melt(bm_vec, measure.vars = -seq_len(t_idx),
                variable.name = "FuncName", value.name = "time")

Names_vec <- c("Mills", "NCDF", "Blurton", "rtdists")
Color_vec <- c("#b34d4d", "#4d80b3", "#c5a687", "#ac8053")
Outline_vec <- c("#b34d4d", "#4d80b3", "#c5a687", "#ac8053")

mi <- min(bm_vec[, (t_idx+1):(ncol(bm_vec)-2)])
ma <- max(bm_vec[, (t_idx+1):(ncol(bm_vec)-2)])

ggplot(mbm_vec, aes(x = factor(FuncName, levels = Names_vec), y = time,
                    color = factor(FuncName, levels = Names_vec),
                    fill = factor(FuncName, levels = Names_vec))) +
  geom_violin(trim = TRUE, alpha = 0.5) +
  scale_color_manual(values = Outline_vec, guide = FALSE) +
  scale_fill_manual(values = Color_vec, guide = FALSE) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.5) +
  stat_summary(fun = mean, geom = "errorbar",
               aes(ymax = ..y.., ymin = ..y..),
               width = .35, linetype = "dashed") +
  scale_x_discrete(labels = c(
    bquote(F[s] ~ Mills), bquote(F[s] ~ NCDF), "Blurton (Mills)", "rtdists")) +
  facet_zoom(ylim = c(mi, ma)) +
  labs(x = "Implementation", y = "Time (microseconds)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text.x = element_text(size = 16, angle = 90,
                                   vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20,
                                    margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 20,
                                    margin = margin(0, 10, 0, 0)),
        legend.position = "none")

## ----bm-ind-fun---------------------------------------------------------------
rt_benchmark_ind <- function(RT, resp, V, A, t0 = 1e-4, W = 0.5, SV = 0.0,
                             err_tol = 1e-6, times = 100) {
  fnames <- c("Mills", "NCDF", "Blurton", "rtdists")
  nf <- length(fnames) # number of functions being benchmarked
  nRT <- length(RT)
  nV <- length(V)
  nA <- length(A)
  nW <- length(W)
  nSV <- length(SV)

  # Initialize the dataframe to contain the microbenchmark results
  mbm_res <- data.frame(matrix(ncol = 5+nf, nrow = nRT*nV*nA*nW*nSV))
  colnames(mbm_res) <- c('RT', 'V', 'A', 'W', 'SV', fnames)
  row_idx <- 1

  # Loop through each combination of parameters and record microbenchmark results
  for (rt in 1:nRT) {
    for (v in 1:nV) {
      for (a in 1:nA) {
        for (w in 1:nW) {
          for (sv in 1:nSV) {
            mbm <- microbenchmark(
            Mills = pfddm(rt = RT[rt], response = resp, a = A[a], v = V[v],
                          t0 = t0, w = W[w], sv = SV[sv], log = FALSE,
                          method = "Mills", err_tol = err_tol),
            NCDF = pfddm(rt = RT[rt], response = resp, a = A[a], v = V[v],
                         t0 = t0, w = W[w], sv = SV[sv], log = FALSE,
                         method = "NCDF", err_tol = err_tol),
            Blurton = G_0(t = RT[rt]-t0, a = A[a], nu = V[v], w = W[w],
                          eta2 = SV[sv]*SV[sv], sigma2 = 1, eps = err_tol), # only "lower" resp
            rtdists = pdiffusion(RT[rt], resp, a = A[a], v = V[v], t0 = t0,
                                 z = W[w]*A[a], sv = SV[sv], precision = 3),
            times = times)
          # add the v, a, and w values to the dataframe
          mbm_res[row_idx, 1] <- RT[rt]
          mbm_res[row_idx, 2] <- V[v]
          mbm_res[row_idx, 3] <- A[a]
          mbm_res[row_idx, 4] <- W[w]
          mbm_res[row_idx, 5] <- SV[sv]
          # add the median microbenchmark results to the dataframe
          for (i in 1:nf) {
            mbm_res[row_idx, 5+i] <- median(mbm[mbm[,1] == fnames[i],2])
          }
          # iterate start value
          row_idx = row_idx + 1
          }
        }
      }
    }
  }
  return(mbm_res)
}

## ----bm-ind-run, eval=FALSE---------------------------------------------------
#  bm_ind <- rt_benchmark_ind(RT = RT, resp = "lower", V = V, A = A, t0 = t0,
#                             W = W, SV = SV, err_tol = err_tol, times = 1000)

## ----bm-ind-save, eval=FALSE, include=FALSE-----------------------------------
#  save(bm_ind, compress = "xz", compression_level = 9,
#       file = "../inst/extdata/pfddm_distribution/bm_ind_0-2.Rds")

## ----bm-meq-prep, fig.height=5, fig.width=8-----------------------------------
# load data, will be in the variable 'bm_ind'
load(system.file("extdata", "pfddm_distribution", "bm_ind_0-2.Rds",
                 package = "fddm", mustWork = TRUE))

t_idx <- match("SV", colnames(bm_ind))
bm_ind[,-seq_len(t_idx)] <- bm_ind[, -seq_len(t_idx)]/1000 # convert to microseconds
mbm_ind <- melt(bm_ind, measure.vars = -seq_len(t_idx),
                variable.name = "FuncName", value.name = "time")

Names_meq <- c("Mills", "NCDF", "Blurton", "rtdists")
Color_meq <- c("#b34d4d", "#4d80b3", "#c5a687", "#ac8053")

my_labeller <- as_labeller(c(Mills = "F[s] ~ Mills",
                             NCDF = "F[s] ~ NCDF",
                             fl_Nav_09 = "f[l] ~ Nav",
                             Blurton = "Blurton (Mills)",
                             rtdists = "rtdists"),
                           default = label_parsed)

ggplot(mbm_ind, aes(x = RT, y = time,
                    color = factor(FuncName, levels = Names_meq),
                    fill = factor(FuncName, levels = Names_meq))) +
  stat_summary(fun.min = min, fun.max = max,
               geom = "ribbon", color = NA, alpha = 0.1) +
  stat_summary(fun.min = function(z) { quantile(z, 0.1) },
               fun.max = function(z) { quantile(z, 0.9) },
               geom = "ribbon", color = NA, alpha = 0.2) +
  stat_summary(fun = mean, geom = "line") +
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2),
                labels = as.character(c(0.1, 0.25, 0.5, 1, 2))) +
  scale_color_manual(values = Color_meq) +
  scale_fill_manual(values = Color_meq) +
  labs(subtitle = paste(
         "The darker shaded regions represent the 10% and 90% quantiles",
         "The lighter shaded regions represent the min and max times",
         sep = ";\n"),
       x = bquote(t ~ ", response time"),
       y = "Time (microseconds)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.subtitle = element_text(size = 16,
                                     margin = margin(0, 0, 15, 0)),
        axis.text.x = element_text(size = 16, angle = 90,
                                   vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20,
                                    margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 20,
                                    margin = margin(0, 10, 0, 0)),
        strip.text = element_text(size = 16),
        strip.background = element_rect(fill = "white"),
        legend.position = "none") +
  facet_wrap(~ factor(FuncName, levels = Names_meq), scales = "free_y",
             labeller = my_labeller)

## ----session-info, collapse=TRUE----------------------------------------------
sessionInfo()

