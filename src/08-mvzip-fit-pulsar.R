#!/usr/bin/env Rscript --vanilla

# Load libraries
library(tidyverse)
library(conflicted)
library(huge)
library(pulsar)
run <- function(
  n, n_lambda, n_reps, lambda_min_ratio,
  log_space, seed, threads, infile, outfile
) {
  set.seed(seed)
  data <- readr::read_rds(infile) |>
    purrr::pluck("data") |>
    as.data.frame() |>
    dplyr::slice_sample(n = n, replace = FALSE) |>
    as.matrix()
  print(dim(data))
  ## Centered log-ratio transformation
  data <- compositions::clr(data + 1)  |> as.matrix()
  ## Assert samples sum to zero
  stopifnot(all(abs(rowSums(data)) < 1e-10))
  ## Fit pulsar
  max_lambda  <- pulsar::getMaxCov(data)
  grid_lambda <- pulsar::getLamPath(
    max = max_lambda, min = max_lambda * lambda_min_ratio,
    len = n_lambda, log = log_space
  )
  pulsar::pulsar(
    data = data,
    fun = huge::huge,
    fargs = list(method = "mb", lambda = grid_lambda, verbose = TRUE),
    criterion = "stars",
    thresh = 0.05, # beta scalar threshold in the paper
    rep.num = n_reps,
    ncores = threads,
    refit = TRUE
  ) |>
    readr::write_rds(outfile, "bz2")
}
run(
  n = as.integer(snakemake@wildcards$n),
  n_lambda = snakemake@params$n_lambda,
  n_reps = snakemake@params$n_reps,
  lambda_min_ratio = snakemake@params$lambda_min_ratio,
  log_space = snakemake@params$log_space,
  seed = snakemake@params$seed,
  threads = snakemake@threads,
  infile = snakemake@input[[1]],
  outfile = snakemake@output[[1]]
)