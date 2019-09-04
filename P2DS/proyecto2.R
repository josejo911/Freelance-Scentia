
require(ggpubr) # Para mejorar la visualización gráfica
require(tidyverse) # Para explotar, manipular y visualizar datos que comparten info
require(corrplot) # Para visualizar la matriz de correlación
require(cluster) #Para calcular la silueta
library(fpc) # Para hacer el plotcluster
library(NbClust) # Para determinar el numero de clusters optimo
library(factoextra) # Para hacer graficos bonitos de clustering
library(rela) # Para poder utilizar paf()
library(psych) # Para poder utilizar KMO()
library(FactoMineR)
library(corrplot)
library(REdaS)
library(ggplot2) # Graficas bonitas
library(ggpubr) # Graficas bonitas x2
#library(ggmap)
library(arules) # Reglas de asociacion
library(factoextra) 
library(arulesViz)
library(dplyr)
library(caret) # Muestreo estratificado
library(class) # Para KNN
library(e1071) # Requisito para la matriz de confusión
library(openxlsx)

setwd("/Users/quiebres/Documents/Ivan Maldonado/UVG/Sexto Semestre/Data Science/Proyecto2DS")


# Leyendo el dataset de csv importacion
#Guatemala
VSG <- read.xlsx("Paginacion1519.xlsx", sheet = 1, startRow = 1, colNames = TRUE,
                 rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                 skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                 namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)
#Honduras
VSH <- read.xlsx("Paginacion1519.xlsx", sheet = 2, startRow = 1, colNames = TRUE,
                 rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                 skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                 namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)
#Salvador
VSS <- read.xlsx("Paginacion1519.xlsx", sheet = 3, startRow = 1, colNames = TRUE,
                 rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                 skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                 namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)
#Nicaragua
VSN <- read.xlsx("Paginacion1519.xlsx", sheet = 4, startRow = 1, colNames = TRUE,
                 rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                 skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                 namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

#----------------------- A priori
reglas<-apriori(VSG, parameter = list(support = 0.50,
                                                   confidence = 0.60,
                                                   target = "rules"))
inspect(head(reglas, n = 33))

top10subRules <- head(reglas, n = 10, by = "confidence")
plot(top10subRules, method="graph", engine="htmlwidget")

nuevoVSS <- VSS[,c(1:9)]

reglasS<-apriori(nuevoVSS, parameter = list(support = 0.10,
                                       confidence = 0.90,
                                       target = "rules"))
inspect(head(reglasS, n = 11))

nuevoVSH <- VSH[,c(1:9)]
reglaH <- apriori(nuevoVSH, parameter = list(support = 0.10,
                                        confidence = 0.10,
                                        target = "rules"))
inspect(head(reglaH, n = 10))

nuevoVSN <- VSN[,c(1:9)]
reglaN <- apriori(nuevoVSN, parameter = list(support = 0.10,
                                             confidence = 0.10,
                                             target = "rules"))
inspect(head(reglaN, n = 13))
