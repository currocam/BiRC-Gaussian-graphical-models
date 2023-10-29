library(tidyverse)
source("figures/theme.R")
dir <- "data/results/mvn_roc_curves"
infiles <- list.files(dir, pattern = "*.tsv", full.names = TRUE)
names(infiles) <- list.files(dir, pattern = "*.tsv") |> str_remove(".tsv")
data <- purrr::map(infiles, readr::read_tsv, show_col_types = FALSE) |>
  bind_rows(.id = "graph_structure")

dir <- "data/results/mvn_F1_scores/"
infiles <- list.files(dir, pattern = "*.tsv", full.names = TRUE)
names(infiles) <- list.files(dir, pattern = "*.tsv") |> str_remove(".tsv")
f1_scores <- purrr::map(infiles, readr::read_tsv, show_col_types = FALSE) |>
  bind_rows(.id = "graph_structure")

plot_roc_curve <- function(graph) {
  data |>
    left_join(f1_scores) |>
    filter(graph_structure == graph) |>
    mutate(
      F1_score = round(F1_score, digits = 2),
      label = str_glue("{method} with n = {n} (F1 = {F1_score})")
    ) |>
    ggplot(aes(x = fpr, y = tpr)) +
    geom_line(colour = "deeppink3", linewidth = 0.7, alpha = 0.9) +
    labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)", title = str_glue("ROC Curve ({graph} graph structure)")) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12)) +
    guides(colour = guide_legend(title = "Sample size")) +
    scale_colour_brewer(palette="Dark2") +
    facet_wrap(~label, nrow = 4) +
    xlim(c(0, 1))
}

data$graph_structure |> 
  unique() |>
  walk(\(graph) {
    plot <- plot_roc_curve(graph)
    filepath <- str_glue("figures/01-mvn_roc_curves/{graph}.pdf")
    ggsave(plot = plot, filename = filepath,width = fig.witdh, height = fig.height, units = "mm")
  })


