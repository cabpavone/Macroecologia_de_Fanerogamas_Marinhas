#####################################################
## Camila Barreto e Carla Pavone               ######
## Trabalho final: Macroecologia               ######
## Spalding Regions: Beta diversidade de ALGAS ######
## 13/11/2020                                  ######
#####################################################

### Unificando dataframes de OCORRENCIA e ECORREGIOES  ###
##########################################################

ecoregions <- read.table("C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/ecoregions.total.csv", sep =  ";", dec = ",", header = TRUE)

algas_simplificado <- finalDataBaseBPruned

ocorrencia.algas<-data.frame(lat=algas_simplificado$decimalLatitude,
                    long=algas_simplificado$decimalLongitude,
        family=algas_simplificado$family,sp=algas_simplificado$name)

# Arrumando classes das colunas:
ocorrencia.algas$family <- as.character(ocorrencia.algas$family) 
ocorrencia.algas$sp <- as.character(ocorrencia.algas$sp) 
ocorrencia.algas$lat <- as.numeric(as.character(ocorrencia.algas$lat)) 
ocorrencia.algas$long <- as.numeric(as.character(ocorrencia.algas$long))

# Letras minusculas para nomes de especies:
ocorrencia.algas$sp <- tolower (ocorrencia.algas$sp)
ocorrencia.algas <- data.frame(family = ocorrencia.algas$family, sp = ocorrencia.algas$sp, 
                               lat = ocorrencia.algas$lat, long = ocorrencia.algas$long)
str(ecoregions)
str(ocorrencia.algas)

# Unificando planilhas de ocorrencia e nomes de ecoregioes
ocorrencia.algas <- merge (ocorrencia.algas, ecoregions[,], by= c("lat","long"), all.x = T)

# write.table (ocorrencia.algas, "C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/ocorrencia.algas.csv", sep=";", dec = ",", row.names = F)


#################################
## Presença / Ausencia         ##
#################################

df <- read.table("C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/Spalding/ocorrencia.algas_sem.NA.csv", sep =  ";", dec = ",", header = TRUE)

# construindo data frame com dados de ecorregioes e especies
algas_presabs <- data.frame(ecoregion = df$ecoregion, sp = df$sp)


# contruindo matriz de presenÃ§a/ausencia

# install.packages("fuzzySim", repos="http://R-Forge.R-project.org")

if (!require(fuzzySim)){install.packages("fuzzySim")}; library(fuzzySim)

# trasnformando dataframe comumem dataframe de presenca/ausencia:
algas_presabs <- splist2presabs(algas_presabs, sites.col = "ecoregion", sp.col = "sp", keep.n = FALSE)

#write.table (algas_presabs, "C:/Users/camil/OneDrive/Documentos/DOUTORADO/Disciplinas/Macroecologia e Biogeografia/Projeto/Dados/algas_presabs.csv", sep=";", dec = ",", row.names = F)


# nomeando as linhas com nomes das ecorregioes:
rownames(algas_presabs) <- algas_presabs$ecoregion
algas_presabs <- algas_presabs[, -1] # exclui primeira coluna com nome da ecorregiao, pois esses nomes ja estao nomeando as linhas

#############################################
### Executando script de BETA-DIVERSIDADE ###
#############################################

library(betapart)
# OBTENDO OBJETOS BETAPART

algas_presabs.core <- betapart.core(algas_presabs)


# MEDIDAS MULTIPLAS DO SITE

algas_presabs.multi <- beta.multi(algas_presabs, index.family = "sorensen")


# BETA.PAIR

algas_presabs.pair <- beta.pair(algas_presabs, index.family = "sorensen")


head(algas_presabs.samp)
head(algas_presabs.samp$beta.sim)
as.matrix(algas_presabs.samp$beta.sim)

# MDS

library(vegan)
x11()
toto <- metaMDS(comm = algas_presabs.pair$beta.sim, k=2, trymax = 20, autotransform = FALSE, previous.best = TRUE)

plot(toto, xlim = c(-1,1), ylim = c(-0.5,0.5))
plot(scores(toto), type = "n", xlim = c(-1,1), ylim = c(-0.5,0.5))
text(scores(toto), rownames(algas_presabs), cex = 0.3)
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
# dist <- as.matrix(algas_presabs.samp$beta.sim)
# head(dist)
# 
# 
# x11() # abrindo nova janela grafica
# 
# plot(density(algas_presabs.multi$beta.SOR), xlim=c(0,0.8), ylim=c(0, 19), xlab='Beta diversity', main='', lwd=3)
# 
# lines(density(algas_presabs.multi$beta.SNE), lty=1, lwd=2) # aninhamento
# 
# lines(density(algas_presabs.multi$beta.SIM), lty=2, lwd=2) # substituiçao



# plotando clusters

## dist <- algas_presabs.samp$sampled.values  # pode apagar essa linha eu acho

# substituiçao
library(ggplot2)

#install.packages('dendextend')
library(dendextend)


dend <- hclust(algas_presabs.pair$beta.sim, method= "average")
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


