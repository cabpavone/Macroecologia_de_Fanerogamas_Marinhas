########################################
## Camila Barreto e Carla Pavone       ######
## Trabalho final: Macroecologia       ######
## Spalding Regions: ALGAS             ######
## 12/11/2020                          ######
#############################################

## Organizando planilha de algas ##

algas_simplificado <- finalDataBaseBPruned

algas.all<-data.frame(country= algas_simplificado$country, 
                      lat=algas_simplificado$decimalLatitude,long=algas_simplificado$decimalLongitude,
                      family=algas_simplificado$family,sp=algas_simplificado$name)

algas.all$country <- as.character(algas.all$country) # trasnforma classe dos dados em character
algas.all$family <- as.character(algas.all$family) 
algas.all$sp <- as.character(algas.all$sp) 
algas.all$lat <- as.numeric(as.character(algas.all$lat)) 
algas.all$long <- as.numeric(as.character(algas.all$long))

str(algas.all)


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
ogrInfo("C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/MEOW-TNC", "meow_ecos")


regions <- readOGR("C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/MEOW-TNC", "meow_ecos")

#let's see the map
library(maptools)
library(maps)
x11()
map("world",xlim=c(-180,180));box()

plot(regions, axes=TRUE, border="gray")

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


#Not Run Demos
# getRegionalInfo(50.00806, -127.4342)
getRegionalInfo(-51.93285,  -58.24305) #testando funçao
getRegionalInfo(50.71772, -127.3736)



# contruindo dataframe com lat e long para todos os dados
df <- data.frame(lat = algas.all$lat, long = algas.all$long)

# Remover linhas duplicadas, simultaneamente, nas colunas lat e long: 
library(dplyr)

df2 <- df %>% distinct(lat, long, .keep_all = TRUE)
df2$ecoregion <-rep(NA, 119508) #criando coluna ecoregion no dataframe df2
# df2$ecoregion com 119508 linhas porque o df2 tem essa quantidade de linhas

#########
## df3 ##
#########

df3 <- data.frame(df2[c(1:10000), ])
for (i in 1: 10000)
{
      df3$ecoregion[i] <- getRegionalInfo(df3$lat[i], df3$long[i])
}

str(df3) # coluna ecoregions eh uma lista. Eh preciso trasnforma-la:
df3 = data.frame(lat = df3$lat, long = df3$long) %>% mutate(ecoregion = list(df3$ecoregion) %>% unlist())
#write.table(df3, "C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/ecoregions_01_10000.csv", sep =  ";", dec = ",", row.names = FALSE)

##############################

#########
## df4 ##
#########

df4 <- data.frame(df2[c(10001:11000), ])
for (i in 1 :1000)
{
  df4$ecoregion[i] <- getRegionalInfo(df4$lat[i], df4$long[i])
}

str(df4) # coluna ecoregions eh uma lista. Eh preciso trasnforma-la:
df4 = data.frame(lat = df4$lat, long = df4$long) %>% mutate(ecoregion = list(df4$ecoregion) %>% unlist())


#########
## df5 ##
#########

df5 <- data.frame(df2[c(11001:17000), ])
for (i in 1 :6000)
{
  df5$ecoregion[i] <- getRegionalInfo(df5$lat[i], df5$long[i])
}


df5 = data.frame(lat = df5$lat, long = df5$long) %>% mutate(ecoregion = list(df5$ecoregion) %>% unlist())
str(df5)


#########
## df6 ##
#########

df6 <- data.frame(df2[c(17001:18000), ])
for (i in 1 :1000)
{
  df6$ecoregion[i] <- getRegionalInfo(df6$lat[i], df6$long[i])
}

df6 = data.frame(lat = df6$lat, long = df6$long) %>% mutate(ecoregion = list(df6$ecoregion) %>% unlist())
str(df6) 

#########
## df7 ##
#########

df7 <- data.frame(df2[c(18001:19000), ])
for (i in 1 :1000)
{
  df7$ecoregion[i] <- getRegionalInfo(df7$lat[i], df7$long[i])
}

df7 = data.frame(lat = df7$lat, long = df7$long) %>% mutate(ecoregion = list(df7$ecoregion) %>% unlist())
str(df7) 


#########
## df8 ##
#########

df8 <- data.frame(df2[c(19001:20000), ])
for (i in 1 :3000)
{
  df8$ecoregion[i] <- getRegionalInfo(df8$lat[i], df8$long[i])
}

df8 = data.frame(lat = df8$lat, long = df8$long) %>% mutate(ecoregion = list(df8$ecoregion) %>% unlist())
str(df8)

#########
## df9 ##
#########

df9 <- data.frame(df2[c(20001:23000), ])
for (i in 1 :3000)
{
  df9$ecoregion[i] <- getRegionalInfo(df9$lat[i], df9$long[i])
}


df9 = data.frame(lat = df9$lat, long = df9$long) %>% mutate(ecoregion = list(df9$ecoregion) %>% unlist())
str(df9) 

df23000 <- rbind(df3, df4, df5, df6, df7, df8, df9)

str(df23000)

#write.table(df23000, "C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/ecoregions.23000.csv", sep =  ";", dec = ",", row.names = FALSE)


#########
## df10 ##
#########

df10 <- data.frame(df2[c(23001:119508), ])
for (i in 1 :96508)
{
  df10$ecoregion[i] <- getRegionalInfo(df10$lat[i], df10$long[i])
}


df10 = data.frame(lat = df10$lat, long = df10$long) %>% mutate(ecoregion = list(df10$ecoregion) %>% unlist())
str(df10) 

df.total <- rbind(df23000, df10)

df.total <- droplevels(df.total[!df.total$ %in% "NA", ]) # exclui linhas com NA


# write.table(df.total, "C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/ecoregions.total.csv", sep =  ";", dec = ",", row.names = FALSE)


toto <- df.total[df.total$ecoregion == "NA", ] # valores de NA em ecoregions
  
# testando pacote "mregions"
# uai <- mr_rev_geo_code(lat = 48.86900, lon = -3.562000)

# testando função getRegionalInfo:
# getRegionalInfo(lat1 = 48.86900, long1 = -3.562000)
# getRegionalInfo(lat1 = -41.17244, long1 = 144.678130)
# getRegionalInfo(lat1 = 51.74638, long1 = -5.29920000)
# getRegionalInfo(lat1 = 45.57500, long1 = -64.965453)


# ##################################################################
# ####### PARTE DO CODIGO geRegionalInfo ORIGINAL FOI TROCADA ######
# ##################################################################
# dsdat <- over(regions, FB.sp, add=T, fn = mean) 
# 
# ret <- data.frame(ECOREGION = regions$ECOREGION[which(dsdat$value==4)],
#                   PROVICE = regions$PROVINCE[which(dsdat$value==4)],
#                   REALM = regions$REALM[which(dsdat$value==4)])
# 
# if(nrow(ret)==0) ret <- data.frame(ECOREGION = NA,
#                                    PROVICE = NA,
#                                    REALM = NA)
# return(ret)
####################################################################################
