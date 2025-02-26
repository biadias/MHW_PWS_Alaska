#------------------------------------------------------------------------------#
#AUTHORS: Dias, B.S.
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#------------------------------------------------------------------------------#
library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggpubr)
library(igraph)
library(scales)
library(dplyr)
library(purrr)
library(tibble)
library(jsonlite)
library(rjson)
library(here)



# Load JSON and convert to tibbles
datajson <- rjson::fromJSON(file = "DATA/foodweb.json")
#JSON file was created by the Ecopath Matlab package
#The function is listed at https://github.com/kakearney/foodwebgraph-pkg
pwslistnodes <- as.list(datajson$nodes[[1]]) 
pwslistedges <- as.list(datajson$links1[[1]])
pwslistedgesTG <- as.list(datajson$links2[[1]])

pwsnodes <- pwslistnodes %>% 
  map(~ data.frame(.)) %>% 
  map_dfr(~ mutate_all(., as.character)) %>% 
  as_tibble()

pwsedges <- pwslistedges %>% 
  map(~ data.frame(.)) %>% 
  map_dfr(~ mutate_all(., as.character)) %>% 
  as_tibble()

pwsedgesTG <- pwslistedgesTG %>% 
  map(~ data.frame(.)) %>% 
  map_dfr(~ mutate_all(., as.character)) %>% 
  as_tibble()

# Process nodes: add numeric id and select desired columns
pwsnodes <- pwsnodes %>% add_column(id_numb = 1:nrow(pwsnodes))
pwsnode <- pwsnodes %>% dplyr::select(id_numb, id, TL, TG, B)
colnames(pwsnode) <- c("id", "label", "TL", "group", "value")
pwsnode$id <- pwsnode$label

# Process edges: convert Weight to numeric and scale values
edges <- pwsedges %>% mutate_at('Weight', as.numeric)
colnames(edges) <- c("from", "to", "width")
#The scale2 uses a function that standardizes values by subtracting the mean, dividing by the standard deviation, and taking the absolute value.
scale2 <- function(x, na.rm = FALSE) abs((x - mean(x, na.rm = na.rm)) / sd(x, na.rm))
edges1 <- edges %>% mutate_if(is.numeric, scale2)


# Cluster betweenness
graph_ig <- graph_from_data_frame(d = edges1, vertices = pwsnode, directed = FALSE)
clusterbt <- cluster_edge_betweenness(graph_ig)
clusterbt_df <- data.frame(as.list(membership(clusterbt))) %>% t() %>% as.data.frame()
clusterbt_df$label <- rownames(clusterbt_df)


# Create group column: join clustering info with node table
nodes <- left_join(pwsnode, clusterbt_df, by = "label")
colnames(nodes)[7] <- "clusterid"
nodes <- nodes %>% drop_na() %>% 
  mutate(group= as.numeric(group))


# Create igraph object with directed edges for plotting
netgraph <- graph_from_data_frame(d = edges1, vertices = nodes, directed = TRUE)

colors_net <- colorRampPalette(c("#3A9AB2", "#6FB2C1", "#91BAB6", "#A5C2A3", "#BDC881", "#DCCB4E", "#E3B710", "#E79805", "#EC7A05", "#EF5703", "#F11B00"))
colnet <- colors_net(max(nodes$group)+1)

nodes_val <- as.numeric(nodes$value) 

#Reescaling so we can see all the nodes
scale_value <- function(x, orig_min = min(nodes_val), orig_max = max(nodes_val), 
                        new_min = 1, new_max = 100) {
  scaled_value <- new_min + ((x - orig_min) / (orig_max - orig_min)) * (new_max - new_min)
  return(scaled_value)
}
nodes_val2 <- scale_value(nodes_val, new_min = 1, new_max = 30)
#node_size <- setNames(as.numeric(nodes_val2),nodes$label)
#node_size2 <- node_size*10


## Tidygraph ####
###1 All ####

graph <- as_tbl_graph(netgraph)
lay <- create_layout(graph, "fr")
lay$x <- lay$x * 3 
lay$y <- as.numeric(lay$TL)

set.seed(123)
# plot using ggraph
set_graph_style(plot_margin = margin(30,30,30,30))
jitter <- position_jitter(width = 0.1, height = 0.1)


library(extrafont) 
font_import()
loadfonts()

netplot <- ggraph(lay, node.diastance=5) +
  geom_edge_link(aes(edge_width = width, color = after_stat(index)),
                 lineend = "round", alpha=0.30) +
  scale_edge_colour_gradient(high = "#ffd06f", low = "#aadce0") +
  geom_edge_loop(aes(edge_width = width, color = after_stat(index)), alpha=0.85,
                 lineend = "round") +
  scale_edge_width(range = c(0.5, 10)) +
  geom_node_point(aes(size = nodes_val2), color="white") +
  geom_node_point(aes(alpha= 0.8, color= factor(group), size = nodes_val2)) + 
  scale_size(range = c(1, max(nodes_val2))) +
  scale_fill_manual(values = colors_net(max(nodes$group) + 1)) +
  scale_color_manual(values = colors_net(max(nodes$group) + 1)) +
  geom_node_text(
    aes(label = label),
    size = 3,
    color = "gray15",
    repel = TRUE,
    #nudge_y= 0.1,
   # nudge_x = 0.1,
    check_overlap = TRUE,
    point.padding = unit(0.9, "lines"),
    segment.size=0.25,
   max.overlaps = Inf
  ) +
  labs(y = "Trophic Level") +
  scale_y_continuous(breaks = seq(floor(min(lay$y)), ceiling(max(lay$y)), by = 1), 
                     expand=expansion(c(.10, .10))) +
  scale_x_continuous(expand=expansion(c(.10, .10)))+
  theme_classic() +
  theme(legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank())

netplot

ggsave("FIGURES/NetworkPlot_PWS.png",netplot, width = 8, height = 7,  dpi = 600)
ggsave("FIGURES/NetworkPlot_PWS.pdf",netplot, device="pdf",  width=8, height=7)

