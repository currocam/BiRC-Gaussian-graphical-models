#!/usr/bin/env Rscript --vanilla

# Load libraries
library(tidyverse)
library(conflicted)
library(BDgraph)
library(huge)
library(Rcpp)

Rcpp::sourceCpp("src/05-mvn-F1-score.cpp")

read_input <- function(x, threshold) {
  x <- x |> readr::read_rds()
  x <- class(x) |> switch(
    pulsar = x$stars$merge[[x$stars$opt.index]] |>
      as.matrix(),
    bdgraph = BDgraph::plinks(x),
    stop("Invalid data type")
  )
  x[x <= threshold] <- 0
  x[x > threshold] <- 1
  x
}

run <- function(true_infile, theta_mb, theta_bdmcmc, threshold, outfile) {
  true_theta <- true_infile |>
    readr::read_rds() |>
    purrr::pluck("theta") |>
    as.matrix()

  dplyr::bind_rows(
    tibble::tibble(
      n = stringr::str_extract(theta_mb, "n(\\d+)", group = 1) |>
        as.numeric(),
      method = "StARS_MB",
      F1_score = purrr::map(theta_mb, read_input, threshold = threshold) |>
        purrr::map(\(x) f1_score(true_theta, x)) |>
        as.numeric()
    ),
    tibble::tibble(
      n = stringr::str_extract(theta_bdmcmc, "n(\\d+)", group = 1) |>
        as.numeric(),
      method = "BDgraph_bdmcmc",
      F1_score = purrr::map(theta_bdmcmc, read_input, threshold = threshold) |>
        purrr::map(\(x) f1_score(true_theta, x)) |>
        as.numeric()
    ),
  ) |>
    readr::write_tsv(outfile)
}

run(
  true_infile = snakemake@input$theta_true[[1]],
  theta_mb = snakemake@input$theta_mb,
  theta_bdmcmc = snakemake@input$theta_bdmcmc,
  threshold = snakemake@params$threshold,
  outfile = snakemake@output[[1]]
)