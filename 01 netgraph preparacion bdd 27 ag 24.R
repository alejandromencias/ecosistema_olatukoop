
rm(list=ls())
pacman::p_load(tidyverse, xlsx, visNetwork, threejs)


d_edges <- 
  
  
  read.xlsx(file = "BDD TFM 27 agosto.xlsx", 
            sheetName = "Relaciones") %>% as_tibble()
  
d_nodes <- 

  read.xlsx(file = "BDD TFM 27 agosto.xlsx", 
            sheetName = "Metadata") %>% as_tibble()

# View(d_edges)
# View(d_nodes)


# -------------------------------------------------------------------------

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
  

nodes %>% count(socio, size, sort = T)

edges_t <-
  
  d_edges %>% 
  
  select(from=Entidad_1,
         to=Entidad_2)

nodes %>% distinct()

# Graficos ----------------------------------------------------------------

grafico1 <- 
  
  visNetwork(nodes, edges_t) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

grafico2 <- 
  
  visNetwork(nodes, edges_t) %>%
  
  visNodes(
    shape = "dot", # Forma de los nodos
    shadow = TRUE, # Sombra
    size = nodes$size,
    color = list(
      background = "#97C2FC", # Color de fondo
      border = "#2B7CE9", # Color del borde
      highlight = list(
        background = "#D2E5FF", # Color de fondo al resaltar
        border = "#2B7CE9" # Color del borde al resaltar
      )
    ),
    font = list(
      color = "#343434", # Color de la fuente
      size = 14, # Tamaño de la fuente
      face = "Arial" # Tipo de fuente
    )
  ) %>%
  
  visGroups(
    groupname = "Resto", 
    color = list(
      background = "#C1C1C1", # Color de fondo
      border = "#C1C1C1"      # Color del borde
    )
  ) %>%
  
  visEdges(
    color = list(
      color = "#848484", # Color del borde
      highlight = "#2B7CE9", # Color del borde al resaltar
      hover = "#2B7CE9" # Color del borde al pasar el ratón
    ),
    smooth = list(
      enabled = TRUE, # Habilitar bordes suavizados
      type = "dynamic" # Tipo de suavizado
    )
  ) %>%
  visLayout(randomSeed = 42) %>% # Semilla para el layout aleatorio
  visPhysics(
    # stabilization = TRUE, # Habilitar estabilización
    solver = "forceAtlas2Based", # Algoritmo de distribución
    forceAtlas2Based = list(gravitationalConstant = -50) # Configuración del algoritmo
  ) %>%
  visOptions(
    highlightNearest = list(
      enabled = TRUE,
      degree = 1, # Visualizar hasta la segunda conexión
      hover = TRUE # Resaltar también al pasar el ratón
      # selectedBy = "group",
      # algorithm = "all"
    ), # Resaltar el nodo más cercano
    nodesIdSelection = TRUE # Habilitar la selección de nodos por ID
  ) %>%
  visInteraction(
    hover = TRUE, # Habilitar el efecto hover
    tooltipDelay = 200 # Retardo del tooltip
  )  %>%
  
  # Ajustar el layout para incluir la leyenda en la parte inferior
  visLayout(
    randomSeed = 42,
    hierarchical = FALSE
  )

grafico2

 visSave(graph = grafico1, file = "output plots ecosystem/tfm_ale_ecosistemared.html")
# visSave(graph = grafico2, file = "G://My Drive/BDSKoop Praktica/E6 Resultados y conclusiones/3 graficos red BDSKoop/plot2_24_junio.html")



