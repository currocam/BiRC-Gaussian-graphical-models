library(tidyverse)
library(igraph)
library(aricode)

# Read Rds files function to graph
read_sim_graph <- function(x){
  read_rds(x)  |>
    pluck("theta") |>
    as.matrix() |>
    graph_from_adjacency_matrix(mode = "undirected")
}

read_pulsar_graph <- function(x){
  read_rds(x)  |>
    map(\(y){
      y <- as.matrix(y)
      y <- ifelse(y< 0.5, 0, 1)
      graph_from_adjacency_matrix(y, mode = "undirected")
  })
}

read_bdgraph_graph <- function(x){
  y <- read_rds(x)  
  sample(size = 10000, x = y$sample_graphs, replace = TRUE, y$graph_weights) |>
    map(\(g){
      m <- matrix( 0, 100, 100 )
      m[lower.tri(m)] <- strsplit(g, "") |> unlist() |> as.numeric()
      m[upper.tri(m)] <- t(m)[upper.tri(m)]
      graph_from_adjacency_matrix(m, mode = "undirected")
    })
}

# Read simulated (ground truth)

infiles_sim <- "data/simulated/" |>
  list.files(pattern = "*.Rds", recursive = TRUE, full.names = TRUE)
names(infiles_sim) <- "data/simulated/" |>
  list.files(pattern = "*.Rds", recursive = TRUE) |>
  str_replace(pattern = "/", replacement = "_") |>
  str_remove(".Rds")

sim_graphs <- map(infiles_sim, read_sim_graph)

# Compute true modularities

true_mod <- sim_graphs |>
  map(cluster_walktrap) |>
  map(modularity) |>
  as.numeric()

# Compute pulsar modularities

infiles_pulsar <- "data/interm/" |>
  list.files(pattern = "nboots200.Rds", recursive = TRUE, full.names = TRUE)
names_infiles_pulsar <- "data/interm//" |>
  list.files(pattern = "nboots200.Rds", recursive = TRUE)
names(infiles_pulsar) <- paste(
  sep = "_",
  if_else(str_detect(names_infiles_pulsar, "StARS_mvzip"), "mvzip", "mvn"),
  str_extract(names_infiles_pulsar, "mb_(.+)_nboots", 1)
)

pulsar_mod <- map(infiles_pulsar,.progress = TRUE, \(file){
  read_pulsar_graph(file) |>
    map(cluster_walktrap) |>
    map(modularity) |>
    as.numeric()
})

# Compute bdgraph modularities


infiles_bdgraph <- c(
  list.files("data/interm/BDgraph/", pattern = "_n100.Rds", recursive = TRUE, full.names = TRUE),
  list.files("data/interm/BDgraph_mvzip/", pattern = "_n100.Rds", recursive = TRUE, full.names = TRUE)
)

names(infiles_bdgraph) <- paste(
  sep = "_",
  if_else(str_detect(infiles_bdgraph, "_mvzip"), "mvzip", "mvn"),
  str_extract(infiles_bdgraph, "bdmcmc_(.+)_n", 1)
)

bdgraph_mod <- map(infiles_bdgraph,.progress = TRUE, \(file){
  read_bdgraph_graph(file) |>
    map(cluster_walktrap) |>
    map(modularity) |>
    as.numeric()
})

# Create tidy modularities tibble

modularities <- tibble(
  type = names(sim_graphs),
  modularity = true_mod,
  metric = "true"
) |>
  bind_rows(
    bind_rows(bdgraph_mod) |>
      pivot_longer(cols = everything(), values_to = "modularity", names_to = "type") |>
      mutate(metric = "bdgraph")
  ) |>
  bind_rows(
    bind_rows(pulsar_mod) |>
      pivot_longer(cols = everything(), values_to = "modularity", names_to = "type") |>
      mutate(metric = "pulsar")
  ) |>
  separate(type, into = c("Generative_model", "structure"), sep = "_")

# Read the final predicitions of each

infiles_pulsar_best <- c(
  list.files("data/interm/StARS/", pattern = "_n100.Rds", recursive = TRUE, full.names = TRUE),
  list.files("data/interm/StARS_mvzip/", pattern = "_n100.Rds", recursive = TRUE, full.names = TRUE)
)

names(infiles_pulsar_best) <- paste(
  sep = "_",
  if_else(str_detect(infiles_pulsar_best, "_mvzip"), "mvzip", "mvn"),
  str_extract(infiles_pulsar_best, "mb_(.+)_n", 1)
)


inferred_graphs <- list(
  "inferred_BDgraph" = map(infiles_bdgraph, \(x){
    read_rds(x) |> 
      BDgraph::select(cut = 0.5) |>
      graph_from_adjacency_matrix(mode = "undirected")
  }),
  "inferred_pulsar" = map(infiles_pulsar_best, \(x){
    fit <- read_rds(x)
    y <- fit$stars$merge[[fit$stars$opt.index]] |> as.matrix()
    y <- ifelse(y< 0.5, 0, 1)
    graph_from_adjacency_matrix(y, mode = "undirected")
  })
)

# Save the modularities

inferred_modularities <- inferred_graphs |>
  map(~map(.x, cluster_walktrap) |> map(modularity) |> as.numeric())

modularities <- bind_rows(inferred_modularities) |>
  mutate(type = names(inferred_graphs$inferred_BDgraph)) |>
  pivot_longer(-type, names_to = "metric", values_to = "modularity") |>
  separate(type, into = c("Generative_model", "structure"), sep = "_") |>
  bind_rows(modularities)

write_rds(modularities, "figures/04-confidence_credibility/modularity.Rds")

# Compute spearman of distances

compute_spearman_distance <- function(x, y){
  xdist <- distances(x, mode = "undirected")
  ydist <- distances(y, mode = "undirected")
  cor(xdist[upper.tri(xdist)], ydist[upper.tri(ydist)], method = "spearman")
}

bdgraph_spearman <- map2(
  sim_graphs, infiles_bdgraph, .progress = TRUE,
  \(sim, file){
  read_bdgraph_graph(file) |>
    map(\(x) compute_spearman_distance(x, sim)) |>
    as.numeric()
})

pulsar_spearman <- map2(
  sim_graphs, infiles_pulsar,.progress = TRUE, 
  \(sim, file){
  read_pulsar_graph(file) |>
      map(\(x) compute_spearman_distance(x, sim)) |>
      as.numeric()
  })

spearman <- bind_rows(
  bind_rows(bdgraph_spearman) |> mutate(metric = "BDgraph"),
  bind_rows(pulsar_spearman) |> mutate(metric = "pulsar")
) |>
  pivot_longer(-metric, names_to = "type", values_to = "spearman_distances") |>
  separate(type, into = c("Generative_model", "structure"), sep = "_")

spearman <- inferred_graphs |>
  map(~map2(sim_graphs, .x, compute_spearman_distance)) |>
  bind_rows() |>
  mutate(metric = names(inferred_graphs)) |>
  pivot_longer(-metric, names_to = "type", values_to = "spearman_distances") |>
  separate(type, into = c("Generative_model", "structure"), sep = "_") |>
  bind_rows(spearman)

write_rds(spearman, "figures/04-confidence_credibility/spearman_distances.Rds")

# Compute adjusted mutual information of the clusters

compute_adjusted_mutual_information <- function(x, y){
  xclust <- x |> cluster_walktrap()
  yclust <- y |> cluster_walktrap()
  aricode::AMI(xclust$membership, yclust$membership)  
}

bdgraph_ami <- map2(
  sim_graphs, infiles_bdgraph, .progress = TRUE,
  \(sim, file){
    read_bdgraph_graph(file) |>
      map(\(x) compute_adjusted_mutual_information(x, sim)) |>
      as.numeric()
  })

pulsar_ami <- map2(
  sim_graphs, infiles_pulsar,.progress = TRUE, 
  \(sim, file){
    read_pulsar_graph(file) |>
      map(\(x) compute_adjusted_mutual_information(x, sim)) |>
      as.numeric()
  })

ami <- bind_rows(
  bind_rows(bdgraph_ami) |> mutate(metric = "BDgraph"),
  bind_rows(pulsar_ami) |> mutate(metric = "pulsar")
) |>
  pivot_longer(-metric, names_to = "type", values_to = "ami") |>
  separate(type, into = c("Generative_model", "structure"), sep = "_")

ami <- inferred_graphs |>
  map(~map2(sim_graphs, .x, compute_adjusted_mutual_information)) |>
  bind_rows() |>
  mutate(metric = names(inferred_graphs)) |>
  pivot_longer(-metric, names_to = "type", values_to = "ami") |>
  separate(type, into = c("Generative_model", "structure"), sep = "_") |>
  bind_rows(ami)

write_rds(ami, "figures/04-confidence_credibility/ami.Rds")



