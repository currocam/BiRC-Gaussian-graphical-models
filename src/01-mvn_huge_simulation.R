#!/usr/bin/env Rscript --vanilla

# Load libraries
library(tidyverse)
library(conflicted)
library(huge)

run <- function(n, d, graph, seed, outfile) {
  set.seed(seed)
  huge::huge.generator(
    n = n, # sample size
    d = d, # number of variables
    graph = graph, # graph structure
  ) |>
    readr::write_rds(outfile)
}

run(
  snakemake@params$n, snakemake@params$d,
  snakemake@wildcards$graph, snakemake@params$seed,
  snakemake@output[[1]]
)