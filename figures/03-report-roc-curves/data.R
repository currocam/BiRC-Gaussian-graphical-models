library(tidyverse)
read_data <- function(dir){
  infiles <- list.files(dir, pattern = "*.tsv", full.names = TRUE)
  names(infiles) <- list.files(dir, pattern = "*.tsv") |> str_remove(".tsv")
  purrr::map(infiles, readr::read_tsv, show_col_types = FALSE) |>
    bind_rows(.id = "graph_structure")
}

roc_curves <- bind_rows(
  read_data( "data/results/mvn_roc_curves") |> mutate(gen_model = "mvn"),
  read_data( "data/results/mvzip_roc_curves") |> mutate(gen_model = "zip"),
)

roc_curves |> pull("gen_model") |> fct_count()

roc_curves |> pull("graph_structure") |> fct_count()

f1_scores <- bind_rows(
  read_data( "data/results/mvn_F1_scores/") |> mutate(gen_model = "mvn"),
  read_data( "data/results/mvzip_F1_scores/") |> mutate(gen_model = "zip"),
)

inner_join(roc_curves, f1_scores) |>
  write_rds(file = "figures/03-report-roc-curves/data.Rds", compress = "gz")