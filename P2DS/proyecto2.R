
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

setwd("C:/Users/smayr/Documents/Tercer año/Semestre 6/Data Science/Proyecto 2/Proyecto2DS/P2DS")


# Leyendo el dataset de csv importacion
datos <- read.csv("Paginacion.csv", TRUE, ",")