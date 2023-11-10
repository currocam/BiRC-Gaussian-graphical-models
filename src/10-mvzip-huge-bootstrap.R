#!/usr/bin/env Rscript --vanilla

# Load libraries
library(tidyverse)
library(conflicted)
library(furrr)
library(huge)
library(pulsar)
library(modelr)

run <- function(
  n, n_boots, n_lambda, n_reps, lambda_min_ratio,
  log_space, seed, threads, infile, outfile
) {
  # Load 200 points of simulated data
  set.seed(seed)
  data <- readr::read_rds(infile) |>
    purrr::pluck("data") |>
    as.data.frame() |>
    dplyr::slice_sample(n = n, replace = FALSE)
  # Prepare bootstraps
  boot <- modelr::bootstrap(data = data, n = n_boots)
  # Run pulsar on each bootstrap
  purrr::map2(boot$strap, 1:n_boots, .progress = TRUE, function(x, i) {
    set.seed(seed + i)
    x <- x |> as.data.frame() |> as.matrix()
    x <- compositions::clr(x + 1) |> as.matrix()
    ## Assert samples sum to zero
    stopifnot(all(abs(rowSums(x)) < 1e-10))
    max_lambda  <- pulsar::getMaxCov(x)
    grid_lambda <- pulsar::getLamPath(
      max = max_lambda, min = max_lambda * lambda_min_ratio,
      len = n_lambda, log = log_space
    )
    fit <- pulsar::pulsar(
      data = x,
      fun = huge::huge,
      fargs = list(method = "mb", lambda = grid_lambda, verbose = TRUE),
      criterion = "stars",
      thresh = 0.05, # beta scalar threshold in the paper
      rep.num = n_reps,
      ncores = threads,
      refit = TRUE
    )
    fit$stars$merge[[fit$stars$opt.index]]
  }) |>
    readr::write_rds(outfile, "bz2")
}
run(
  n = as.integer(snakemake@params$n),
  n_boots = as.integer(snakemake@wildcards$nboots),
  n_lambda = snakemake@params$n_lambda,
  n_reps = snakemake@params$n_reps,
  lambda_min_ratio = snakemake@params$lambda_min_ratio,
  log_space = snakemake@params$log_space,
  seed = snakemake@params$seed,
  threads = snakemake@threads,
  infile = snakemake@input[[1]],
  outfile = snakemake@output[[1]]
)