library(tidyverse)
library(igraph)
source("figures/theme.R")

modularities <- read_rds("figures/04-confidence_credibility/modularity.Rds")

plot1 <- modularities |>
  filter(metric != "true") |>
  filter(!metric %in% c("inferred_pulsar", "inferred_BDgraph") ) |>
  ggplot(aes(x = modularity, y = structure))+
  geom_boxplot() +
  geom_point(
    data = modularities |>
      filter(metric == "true") |>
      select(modularity, structure),
    aes(x = modularity, y = structure), colour = "red"
    ) +
  geom_point(
    data = modularities |>
      filter(metric %in% c("inferred_pulsar", "inferred_BDgraph")) |>
      mutate(metric = str_remove(metric, "inferred_") |> tolower()) |>
      select(metric, Generative_model, modularity, structure),
    aes(x = modularity, y = structure), colour = "blue"
  ) +
  facet_wrap(~metric+Generative_model)

ggsave("figures/04-confidence_credibility/modularity.pdf", plot1, width = fig.witdh, height = fig.height, units = "mm")

spearman_distances <- read_rds("figures/04-confidence_credibility/spearman_distances.Rds")

plot2 <- spearman_distances |>
  filter(!metric %in% c("inferred_pulsar", "inferred_BDgraph") ) |>
  ggplot(aes(x = spearman_distances, y = structure))+
  geom_boxplot() +
  geom_point(
    data = spearman_distances |>
      filter(metric %in% c("inferred_pulsar", "inferred_BDgraph") ) |>
      mutate(metric = str_remove(metric, "inferred_")) |>
      select(metric, spearman_distances, structure, Generative_model),
    color = "blue",
    aes(x = spearman_distances, y = structure)
  ) +
  facet_wrap(~metric+Generative_model) +
  xlab("Spearman correlation of the true and inferred distances")+
  ylab("")

ggsave("figures/04-confidence_credibility/spearman_distances.pdf", plot2, width = fig.witdh, height = fig.height, units = "mm")

ami <- read_rds("figures/04-confidence_credibility/ami.Rds")

plot3 <- ami |>
  filter(!metric %in% c("inferred_pulsar", "inferred_BDgraph") ) |>
  ggplot(aes(x = ami, y = structure))+
  geom_boxplot() +
  geom_point(
    data = ami |>
      filter(metric %in% c("inferred_pulsar", "inferred_BDgraph") ) |>
      mutate(metric = str_remove(metric, "inferred_")) |>
      select(metric, ami, structure, Generative_model),
    color = "blue",
    aes(x = ami, y = structure)
  ) +
  facet_wrap(~metric+Generative_model) +
  xlab("Adjusted mutual information of the true and inferred modules (random walk)")+
  ylab("")

ggsave("figures/04-confidence_credibility/ami.pdf", plot3, width = fig.witdh, height = fig.height, units = "mm")



