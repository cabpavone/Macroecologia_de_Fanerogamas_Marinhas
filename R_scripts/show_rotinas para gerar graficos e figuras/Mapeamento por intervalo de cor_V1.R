# Carregando os pacotes

library(maptools)     
library(spdep)          
library(cartography)    
library(tmap)           
library(leaflet)        
library(dplyr)
library(rgdal)
library(dplyr)
library(RColorBrewer) 

# Caminho ondes os dados estao salvos
dir = dirname(rstudioapi::getActiveDocumentContext()$path)

##########################################################################
# Importando shapefile (mapa do Brasil)

# Caminho dos mapas
dirmaps = paste(dir, 'Mapa', sep = '/')

# Importando shape file (arquivo do mapa)
shp <- readOGR(dirmaps, "BR_UF_2019", stringsAsFactors=FALSE, encoding="UTF-8")
##########################################################################

##########################################################################
# Importando codigos do IBGE

# Caminho + nome do arquivo com codigos
diribge = paste(dir, 'Dados', 'municipios.csv', sep = '/')

# Importando e filtrando as colunas referentes aos municipios
ibge <- read.csv(diribge, header = T, sep = ";")
ibge <- ibge[c("UF", "Nome_UF")]

# Eliminando duplicatas 
ibge = unique(ibge)
##########################################################################

##########################################################################
# Lendo os dados e escolhendo a especie

dirdados <- paste(dir, 'Dados', 'Map.csv', sep = '/')
pg <- read.csv(dirdados, header = T, sep = ";")

pg$Count = 1
pg <- pg[pg$Species == 'Pontoporia blainvillei',]
# Contando as linhas por cidade
pg <- pg %>% group_by(State) %>% mutate(cumsum = cumsum(Count))

pg <- pg %>%
  group_by(State) %>%
  summarise(Casos = max(cumsum))

pg <- as.data.frame(pg)
pg
##########################################################################

##########################################################################
# Juntando as 3 bases (encalhes, codigos IBGE e o shapefile)

# Encalhes + IBGE
pg <- merge(pg, ibge, by.x = "State", by.y = "Nome_UF")
pg
# Encalhes + o shape
pg <- merge(shp, pg, by.x = "CD_UF", by.y = "UF")
pg
# Corrigindo encoding dos nomes das cidades
Encoding(pg$NM_UF) <- "UTF-8"
pg
##########################################################################

##########################################################################
# Criando categorias para colocar os nomes na legenda


pg$Categoria <- 0
pg$Categoria[pg$Casos > 0 & pg$Casos < 100] <- 1
pg$Categoria[pg$Casos >= 100 & pg$Casos < 1000] <- 2
pg$Categoria[pg$Casos >= 1000 & pg$Casos < 2000] <- 3
pg$Categoria[pg$Casos >= 2000] <- 4

# Transformando em fator
pg$Categoria <- factor(pg$Categoria,
                       levels = c(1:4),
                       labels = c('(0, 100)', '[100, 1000)', '[1000, 2000)', 'Mais de 2000')
  )
##########################################################################

##########################################################################
# Mapa

# Adicionando coordenadas geograficas
proj4string(pg) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Cores do mapa
pal <- colorFactor("YlGnBu", domain = pg$Categoria) 

# mouse em cima do mapa
state_popup <- paste0("<strong>Estado: </strong>", 
                      pg$NM_UF, 
                      "<br><strong>Encalhes: </strong>", 
                      pg$Casos)

mapHTML = leaflet(data = pg) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(pg$Categoria), 
              fillOpacity = 0.8, 
              color = "#BDBDC3",
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal,
            values = ~pg$Categoria,
            title = "Ocorrências de Encalhes",
            opacity = 1)
# Ver o mapa
mapHTML

###########################################################################


