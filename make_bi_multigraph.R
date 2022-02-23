library(tidyverse)
library(tidygraph)
library(igraph)




make_bi_multigraph = function(df, type1 = T){
  
  bipartite = df %>% 
    rename(from=1, to = 2) %>%
    distinct(from, to) %>% 
    graph_from_data_frame() 
  
  V(bipartite)$type <- bipartite_mapping(bipartite)$type 
  
  bipartite_projection = bipartite.projection(bipartite)
  
  bipartite_type1 = bipartite_projection[[1]]
  
  bipartite_type2 = bipartite_projection[[2]]
  
 


  
  edges = df %>% 
    rename(from=1, to = 2) %>%
    distinct(from, to) 
  
  if(type1== TRUE){ 
    
    nodes = bipartite_type1 %>% 
      as_tbl_graph()  %>% 
      as_tibble() %>% mutate(id = 1:nrow(.))
    
    multigraph_edges = bipartite_type1 %>%
      as_tbl_graph() %>% 
      activate(edges) %>%
      as_tibble() %>%
      left_join(nodes, by = c('from' = 'id')) %>%
      left_join(nodes, by = c('to' = 'id')) %>% 
      select(-from, -to, -weight) %>%
      left_join(edges, by =c('name.x' = 'from')) %>%
      left_join(edges, by =c('name.y' = 'from')) %>%
      filter(to.x == to.y) %>%
      select(from = name.x, to = name.y,edge = to.x) 
}

else 
{
  nodes = bipartite_type2 %>% 
    as_tbl_graph()  %>% 
    as_tibble() %>% mutate(id = 1:nrow(.))
  
  multigraph_edges = bipartite_type2 %>%
    as_tbl_graph() %>% 
    activate(edges) %>%
    as_tibble() %>%
    left_join(nodes, by = c('from' = 'id')) %>%
    left_join(nodes, by = c('to' = 'id')) %>% 
    select(-from, -to, -weight) %>%
    left_join(edges, by =c('name.x' = 'to')) %>%
    left_join(edges, by =c('name.y' = 'to')) %>%
    filter(from.x == from.y) %>%
    select(from = name.x, to = name.y,edge = from.x)
  
}

  
  
multigraph_edges

}