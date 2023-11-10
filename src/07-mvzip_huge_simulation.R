#!/usr/bin/env Rscript --vanilla

# Load libraries
library(tidyverse)
library(conflicted)
library(huge)
library(VGAM)

run <- function(n, d, graph, seed, lambda, outfile) {

  set.seed(seed)
  x <- huge::huge.generator(
    n = n, # sample size
    d = d, # number of variables
    graph = graph, # graph structure
  )

  depth <- runif(n = n, min = 0.8, max = 1.20)
  counts <- matrix(x$data * depth, nrow = n, ncol = d) |>
    pnorm() |>
    as.data.frame() |>
    map(qzipois, lambda = lambda) |>
    bind_cols()


  latent_sigma <- x$sigma

  distances_null <- 1:500 |>
    map(\(i) {
      set.seed(i + 200)
      x <- mvtnorm::rmvnorm(n = 50, sigma = latent_sigma) |>
        cor()
      set.seed(1000 + i)
      y <- mvtnorm::rmvnorm(n = 50, sigma = latent_sigma) |>
        cor()
      norm(x - y, type = "F")
    }) |>
    as.numeric()

  distance <- norm(cor(counts) - cor(x$data), type = "f")

  stopifnot(distance < max(distances_null))

  list(
    data = as.matrix(counts),
    latent_normal = x$data,
    theta = x$theta
  ) |>
    readr::write_rds(outfile)
}

run(
  snakemake@params$n, snakemake@params$d,
  snakemake@wildcards$graph, snakemake@params$seed,
  snakemake@params$lambda_, snakemake@output[[1]]
)