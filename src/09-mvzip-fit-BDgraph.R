#!/usr/bin/env Rscript --vanilla

# Load libraries
library(tidyverse)
library(conflicted)
library(BDgraph)

run <- function(n, iter, seed, g_prior, df_prior, threads, infile, outfile) {
  set.seed(seed)
  data <- readr::read_rds(infile) |>
    purrr::pluck("data") |>
    as.data.frame() |>
    dplyr::slice_sample(n = n, replace = FALSE) |>
    as.matrix()
  data <- compositions::clr(data + 1)  |> as.matrix()
  ## Assert samples sum to zero
  stopifnot(all(abs(rowSums(data)) < 1e-10))

  data |>
    BDgraph::bdgraph(
      method = "ggm", # gaussian graphical model
      algorithm = "bdmcmc", #Birth-Death MCMC
      iter = iter, # number of iterations
      burnin = iter / 2, # number of burn-in iterations
      g.prior = g_prior, # prior distribution of edges
      df.prior = df_prior, # degree of freedom for G-Wishart distribution
      g.start = "full", # starting graph
      save = TRUE, #  save the adjacency matrices
      cores = threads, # number of cores
      threshold = 1e-8, # threshold for the convergence of the precision matrix
    ) |>
    readr::write_rds(outfile, "bz2")
}

run(
  n = as.integer(snakemake@wildcards$n),
  iter = snakemake@params$iter,
  seed = snakemake@params$seed,
  g_prior = snakemake@params$g_prior,
  df_prior = snakemake@params$df_prior,
  threads = snakemake@threads,
  infile = snakemake@input[[1]],
  outfile = snakemake@output[[1]]
)