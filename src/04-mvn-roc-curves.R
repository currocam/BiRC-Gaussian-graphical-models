#!/usr/bin/env Rscript --vanilla

# Load libraries
library(tidyverse)
library(conflicted)
library(BDgraph)
library(huge)
library(Rcpp)

roc_curve <- function(true_infile, pred_infile) {
  # Load data and source Rcpp functions
  Rcpp::sourceCpp("src/04-mvn-roc-curves.cpp")
  true_theta <- true_infile |>
    readr::read_rds() |>
    purrr::pluck("theta") |>
    as.matrix()
  pred_theta <- pred_infile |> readr::read_rds()
  pred_theta <- class(pred_theta) |> switch(
    pulsar = pred_theta$stars$merge[[pred_theta$stars$opt.index]] |>
      as.matrix(),
    bdgraph = BDgraph::plinks(pred_theta),
    stop("Invalid data type")
  )

  seq(-0.01, 1.01, 0.01) |>
    purrr::map(
      \(thres) classifying_metrics(true_theta, pred_theta, thres) # nolint
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      tpr = TP / (TP + FN), # True positive rate
      fpr = FP / (FP + TN)  # False positive rate
    ) |>
    dplyr::select(tpr, fpr)
}

run <- function(true_infile, theta_mb, theta_bdmcmc, outfile) {
  # Extract n_sample from file names
  names(theta_mb) <- stringr::str_extract(theta_mb, "n(\\d+)", group = 1) |>
    as.numeric()
  names(theta_bdmcmc) <- stringr::str_extract(
    theta_bdmcmc, "n(\\d+)", group = 1
  ) |>
    as.numeric()
  # Run roc_curve for each method and n_sample
  purrr::map(theta_mb, \(x) roc_curve(true_infile, x)) |>
    dplyr::bind_rows(.id = "n") |>
    dplyr::mutate(method = "StARS_MB") |>
    dplyr::bind_rows(
      purrr::map(theta_bdmcmc, \(x) roc_curve(true_infile, x)) |>
        dplyr::bind_rows(.id = "n") |>
        dplyr::mutate(method = "BDgraph_bdmcmc")
    ) |>
    readr::write_tsv(outfile)
}

run(
  true_infile = snakemake@input$theta_true[[1]],
  theta_mb = snakemake@input$theta_mb,
  theta_bdmcmc = snakemake@input$theta_bdmcmc,
  outfile = snakemake@output[[1]]
)