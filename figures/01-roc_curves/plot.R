library(tidyverse)
source("figures/theme.R")
source("figures/01-roc_curves/data.R")

data <- read_rds("figures/01-roc_curves/data.Rds")


plot_roc_curve <- function(graph, model) {
  data |>
    filter(graph_structure == graph) |>
    filter(gen_model == model) |>
    mutate(F1_score = round(F1_score, digits = 2)) |>
    mutate(
      label = str_glue("{method} with n = {n} (F1 = {F1_score})") |>
        fct_reorder2(method, n)
    ) |>
    ggplot(aes(x = fpr, y = tpr)) +
    geom_line(colour = "deeppink3", linewidth = 0.7, alpha = 0.9) +
    labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)", title = str_glue("ROC Curve ({graph}, {model})")) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12)) +
    guides(colour = guide_legend(title = "Sample size")) +
    scale_colour_brewer(palette="Dark2") +
    facet_wrap(~label, nrow = 4) +
    xlim(c(0, 1))
}

expand_grid(
  graph = unique(data$graph_structure),
  model = unique(data$gen_model)
) |>
  pmap(\(graph, model, ...) {
      plot <- plot_roc_curve(graph, model)
      filepath <- str_glue("figures/01-roc_curves/{graph}_{model}.pdf")
      ggsave(plot = plot, filename = filepath,width = fig.witdh, height = fig.height, units = "mm")
      }
  )

p1 <- data |>
  distinct(graph_structure, n,method,  gen_model, F1_score) |>
  mutate(n = as.factor(n)) |>
  ggplot(aes(x = n, y = F1_score, fill = method)) +
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~graph_structure + gen_model)+
  xlab("Number of samples") +
  ylab("F-score")+
  ggtitle("F-score for different graph structures and sample size")+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave(plot = p1, filename = "figures/01-roc_curves/F1-score.pdf", width = fig.witdh, height = fig.height, units = "mm")
  
