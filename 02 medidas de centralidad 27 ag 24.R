
rm(list=ls())
pacman::p_load(tidyverse, xlsx, visNetwork, threejs, igraph)


d_edges <- 
  
  
  read.xlsx(file = "C://Users/james/OneDrive/Master ESS UPV EHU Trabajos/001 TFM/02 Entrevista/Lista de entidades.xlsx", 
            sheetName = "Relaciones") %>% as_tibble()

d_nodes <- 
  
  read.xlsx(file = "C://Users/james/OneDrive/Master ESS UPV EHU Trabajos/001 TFM/02 Entrevista/Lista de entidades.xlsx", 
            sheetName = "Metadata") %>% as_tibble()

nodes <- 
  
  d_nodes %>% 
  
  select(id=ENT,label=ENT, group=Tipo, socio=Socio.Olatukoop) %>% 
  
  mutate(group=case_when(group=="Es" ~ "ES", 
                         T ~group)) %>% 
  
  mutate(size=case_when(socio=="1" ~ 30,
                        T ~ 15)) %>% 
  mutate(size=case_when(id=="Olatukoop" ~ 50,
                        T ~size)) %>% 
  mutate(size=case_when(group=="Resto" ~ 10,
                        T ~size))

edges_t <-
  
  d_edges %>% 
  
  select(from=Entidad_1,
         to=Entidad_2)

g <- graph_from_data_frame(edges_t, directed = FALSE)

# Calcular el grado de centralidad
degree_centrality <- degree(g, mode = "all")

# Calcular la centralidad de cercanía
closeness_centrality <- closeness(g, mode = "all")

# Calcular la centralidad de intermediación (betweenness)
betweenness_centrality <- betweenness(g, directed = FALSE)

# Calcular la centralidad de eigenvector
eigenvector_centrality <- eigen_centrality(g)$vector

# Combinar los resultados en un data frame
centrality_measures <- data.frame(
  node = V(g)$name,
  degree = degree_centrality,
  closeness = closeness_centrality,
  betweenness = betweenness_centrality,
  eigenvector = eigenvector_centrality
) %>% as_tibble()

# juntar con la base de datos de nodos y exportación

nodes %>% left_join(centrality_measures, by=c("id"="node")) %>% 

  data.frame() %>% 
  
  write.xlsx(x=., file = "medidas_centralidad.xlsx" ,
             row.names = F, append = T ,
             showNA = F,
             sheetName = "medidas")



