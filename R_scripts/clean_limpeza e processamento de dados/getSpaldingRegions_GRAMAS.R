########################################
## Camila Barreto e Carla Pavone       ######
## Trabalho final: Macroecologia       ######
## Spalding Regions: AGRAMAS           ######
## 14/11/2020                          ######
#############################################

## Organizando planilha de gramas ##

df <- finalDataBaseSPruned

df<-data.frame(lat=df$decimalLatitude,
                      long=df$decimalLongitude)


df$lat <- as.numeric(as.character(df$lat)) # tranforma classe dos dados
df$long <- as.numeric(as.character(df$long))

str(df)

# Remover linhas duplicadas, simultaneamente, nas colunas lat e long: 
library(dplyr)
df <- df %>% distinct(lat, long, .keep_all = TRUE)

############################################################################################
# 
# Script to turn lat/long data into
# marine ecoregions using Spalding's Marine Ecoregions
# of the World (MEOW).
#
# To run, first acquire the appropriate GIS files from 
# http://maps.tnc.org/gis_data.html and unzip them into
# a directory (MEOW-TNC) - then go from there.
#
# getRegionalInfo returns Realms, Provinces, and Ecoregions
# for a single lat/long. I'm sure this can be improved, as this is
# my first attempt at doing anything GIS-y in R!
#
# - Jarrett Byrnes
#
# Last Updated 6/10/2014
############################################################################################



library(rgdal)
library(raster) 
# for shapefiles, first argument of the read/write/info functions is the
# directory location, and the second is the file name without suffix

# optionally report shapefile details
# ogrInfo("C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/MEOW-TNC", "meow_ecos")


regions <- readOGR("C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/MEOW-TNC", "meow_ecos")

#let's see the map
# library(maptools)
# library(maps)
# x11()
# map("world",xlim=c(-180,180));box()
# 
# plot(regions, axes=TRUE, border="gray")


getRegionalInfo  <- function(lat1, long1){
  #lat1 <- c(50.09444)
  #long1 <- c(-127.5589)
  
  
  #first, extract the co-ordinates (x,y - i.e., Longitude, Latitude)
  
  coords <- cbind(long1, lat1)
  
  FB.sp <- SpatialPointsDataFrame(coords,data.frame(value = c(4)))
  
  proj4string(FB.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  #  plot(regions)
  #  plot(FB.sp, add=T)
  
  
  dsdat <- over(regions, FB.sp, add=T, fn = mean) 
  
  ret <- data.frame(ECOREGION = regions$ECOREGION[which(dsdat$value==4)])
  
  if(nrow(ret)==0) ret <- data.frame(ECOREGION = NA)
  return(ret)
  
}

df$ecoregion <-rep(NA, 94893) #criando coluna ecoregion no dataframe df
# df$ecoregion com 94893 linhas porque o df2 tem essa quantidade de linhas



for (i in 1: 94893)
{
  df$ecoregion[i] <- getRegionalInfo(df$lat[i], df$long[i])
}

str(df) # coluna ecoregions eh uma lista. Eh preciso trasnforma-la:
df = data.frame(lat = df$lat, long = df$long) %>% mutate(ecoregion = list(df$ecoregion) %>% unlist())
write.table(df, "C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/ecoregions_gramas.sem.NA.csv", sep =  ";", dec = ",", row.names = FALSE)

###########################################


df2 <- df[79708:94893, ]
df2$ecoregion <-rep(NA, 15186)

for (i in 1: 15186)
{
  df2$ecoregion[i] <- getRegionalInfo(df2$lat[i], df2$long[i])
}

df2$ecoregion = data.frame(lat = df2$lat, long = df2$long) %>% mutate(ecoregion = list(df2$ecoregion) %>% unlist())

df2 <- rbind(df, df2)

# write.table(df2, "C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/ecoregions_gramas.csv", sep =  ";", dec = ",", row.names = FALSE)

#########################################################################################
### O script acima foi realizado em duas etapas (df e df2) devido ao volume de dados. ###
#########################################################################################
