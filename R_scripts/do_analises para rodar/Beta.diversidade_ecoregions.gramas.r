######################################################
## Camila Barreto e Carla Pavone                ######
## Trabalho final: Macroecologia                ######
## Spalding Regions: Beta diversidade de GRAMAS ######
## 13/11/2020                                   ######
######################################################

### Unificando dataframes de OCORRENCIA e ECORREGIOES  ###
##########################################################

ecoregions <- read.table("C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/ecoregions.total.csv", sep =  ";", dec = ",", header = TRUE)

gramas_simplificado <- finalDataBaseSPruned

ocorrencia.gramas<-data.frame(lat=gramas_simplificado$decimalLatitude,
                             long=gramas_simplificado$decimalLongitude,
                             family=gramas_simplificado$family,sp=gramas_simplificado$name)

# Arrumando classes das colunas:
ocorrencia.gramas$family <- as.character(ocorrencia.gramas$family) 
ocorrencia.gramas$sp <- as.character(ocorrencia.gramas$sp) 
ocorrencia.gramas$lat <- as.numeric(as.character(ocorrencia.gramas$lat)) 
ocorrencia.gramas$long <- as.numeric(as.character(ocorrencia.gramas$long))

# Letras minusculas para nomes de especies:
ocorrencia.gramas$sp <- tolower (ocorrencia.gramas$sp)

ocorrencia.gramas <- data.frame(family = ocorrencia.gramas$family, sp = ocorrencia.gramas$sp, 
                               lat = ocorrencia.gramas$lat, long = ocorrencia.gramas$long)
str(ecoregions)
str(ocorrencia.gramas)

# Unificando planilhas de ocorrencia e nomes de ecoregioes
ocorrencia.gramas <- merge (ocorrencia.gramas, ecoregions[,], by= c("lat","long"), all.x = T)

write.table (ocorrencia.gramas, "C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/ocorrencia.gramas.csv", sep=";", dec = ",", row.names = F)


#################################
## Presença / Ausencia         ##
#################################

df <- read.table("C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/ocorrencia.gramas_sem.NA.csv", sep =  ";", dec = ",", header = TRUE)

# construindo data frame com dados de ecorregioes e especies
gramas_presabs <- data.frame(ecoregion = df$ecoregion, sp = df$sp)


# contruindo matriz de presenÃ§a/ausencia

# install.packages("fuzzySim", repos="http://R-Forge.R-project.org")

if (!require(fuzzySim)){install.packages("fuzzySim")}; library(fuzzySim)

# trasnformando dataframe comumem dataframe de presenca/ausencia:
gramas_presabs <- splist2presabs(gramas_presabs, sites.col = "ecoregion", sp.col = "sp", keep.n = FALSE)

#write.table (gramas_presabs, "C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/gramas_presabs.csv", sep=";", dec = ",", row.names = F)


# nomeando as linhas com nomes das ecorregioes:
rownames(gramas_presabs) <- gramas_presabs$ecoregion
gramas_presabs <- gramas_presabs[, -1] # exclui primeira coluna com nome da ecorregiao, pois esses nomes ja estao nomeando as linhas

#############################################
### Executando script de BETA-DIVERSIDADE ###
#############################################

library(betapart)
# OBTENDO OBJETOS BETAPART

gramas_presabs.core <- betapart.core(gramas_presabs)


# MEDIDAS MULTIPLAS DO SITE

gramas_presabs.multi <- beta.multi(gramas_presabs, index.family = "sorensen")


# BETA.PAIR

gramas_presabs.pair <- beta.pair(gramas_presabs, index.family = "sorensen")


head(gramas_presabs.samp)
head(gramas_presabs.samp$beta.sim)
as.matrix(gramas_presabs.samp$beta.sim)

# MDS

library(vegan)
x11()
toto <- metaMDS(comm = gramas_presabs.pair$beta.sim, k=2, trymax = 20, autotransform = FALSE, previous.best = TRUE)

plot(toto, xlim = c(-1,1), ylim = c(-0.5,0.5))
plot(scores(toto), type = "n", xlim = c(-1,1), ylim = c(-0.5,0.5))
text(scores(toto), rownames(gramas_presabs), cex = 0.3)
# 
lolo <- data.frame(round (scores(toto), 4))
# lolo[lolo$NMDS1>0.5,]
lolo[lolo$NMDS2>0.5,]
lolo[lolo$NMDS2< "-0.30",] 
# 
# 
# # PLOTANDO AS DISTRIBUIÇOES DE COMPONENTES 
# 
# 
# dist <- as.matrix(gramas_presabs.samp$beta.sim)
# head(dist)
# 
# 
# x11() # abrindo nova janela grafica
# 
# plot(density(gramas_presabs.multi$beta.SOR), xlim=c(0,0.8), ylim=c(0, 19), xlab='Beta diversity', main='', lwd=3)
# 
# lines(density(gramas_presabs.multi$beta.SNE), lty=1, lwd=2) # aninhamento
# 
# lines(density(gramas_presabs.multi$beta.SIM), lty=2, lwd=2) # substituiçao



# plotando clusters

## dist <- gramas_presabs.samp$sampled.values  # pode apagar essa linha eu acho

# substituiçao
library(ggplot2)

#install.packages('dendextend')
library(dendextend)


dend <- hclust(gramas_presabs.pair$beta.sim, method= "average")
dend1 <- as.dendrogram(dend)

# cex.axis determina o tamanho das letras. Nesse caso, o tamanho da fonte dos paises

# CORES PADRAO

# X11(width = 20, height = 20)

# dend1 %>% set("branches_k_color", k = 4) %>% 
#   plot(main = '',sub='', xlab='', cex = 0.6, cex.axis = 0.8)
# 
# 
# title(xlab=expression(beta[sim]), line = 0.3, cex.lab = 1.4)


# CORES PERSONALIZADAS:
### Dendograma na horizontal:###

X11(width = 7, height = 7)

par(mar = c(3,10,1,10))

dend1 %>% set("branches_k_color", 
              value = c("blue","red", "green", "purple", "orange"), k = 12) %>%
  set("labels_cex", 0.2)%>%
  plot(main = '', sub='', xlab='', cex = 0.6, cex.axis = 0.8, horiz = TRUE)

abline(v = 0.91, lty = 2, col = 'blue')

# cex.lab determina o tamanho do titulo do eixo. Nesse caso, Bsim


### Dendograma na vertical:###
# X11(width = 7, height = 14)
# 
# par(mar = c(10,3, 2,5))
# 
# 
# dend1 %>% set("branches_k_color", 
#               value = c("blue","red", "green", "purple"), k = 4) %>%
#   set("labels_cex", 0.6)%>%
#   plot(main = '', sub='', xlab='', cex = 0.6, cex.axis = 0.8)


# aninhamento

# X11(width = 20, height = 7)

# plot(hclust(pair$beta.sne, method="average"), hang= -1, main='', sub='', xlab='', cex = 0.6, cex.axis = 0.8)
# 
# title(xlab=expression(beta[sne]), line=0.3, cex.lab = 1.4)