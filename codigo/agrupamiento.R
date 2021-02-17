#############################
### Agrupamiento de datos ###
### sesión R-Ladies Btá   ###
#############################
### D. Giraldo, Feb 2021

setwd("~/RLadies_bta_2021") 

# Cargar datos
A <- read.csv("datos/neuropsycho_data.csv")
head(A)

# Cargar librerías
library(dplyr)
library(reshape2)
library(ggplot2)
library(cluster)
library(factoextra)
library(NbClust)

# Necesitamos unicamente con los puntajes cognitivos de las personas con MCI:
Amci <- filter(A, dx == "MCI")
B <- Amci %>%
    select(MEMORY, LANGUAGE, EXECUTIVE, 
           VISUOSPATIAL, ORIENTATION, ATTENTION)

set.seed(123)
# Primer ejemplo: k-means con k=2
k <- 2
k2res <- kmeans(B, k)
str(k2res)

# Ahora con k=3
k <- 3
k3res <- kmeans(B, k)
str(k3res)

# Comparación
table(k2res$cluster, k3res$cluster)

# Visualizar los resultados: 
# Factoextra los grafica en as 2 direcciones de mayor varianza.
fviz_cluster(k2res, data = B, geom = "point", pointsize = 0.75)
fviz_cluster(k3res, data = B, geom = "point", pointsize = 0.75)

# K-means y el resto de métodos tiene inicialización aleatoria 
k <- 5
set.seed(1987)
res1 <- kmeans(B, k)
set.seed(2020)
res2 <- kmeans(B, k)
table(res1$cluster, res2$cluster)

pl <- fviz_cluster(res1, data = B, geom = "point", pointsize = 0.75)
ggsave("presentacion/imgs/fviz_k5resA.png", pl, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")
pl <- fviz_cluster(res2, data = B, geom = "point", pointsize = 0.75)
ggsave("presentacion/imgs/fviz_k5resB.png", pl, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")

# Método del codo con within cluster sum of squares (wcss)
k2res$withinss
k3res$withinss

set.seed(123)

dfwcss <- data.frame(k = 1:10, tot.wcss = numeric(10))
for (k in 1:10){
    kres <-  kmeans(B, k)
    dfwcss$tot.wcss[k]  <- kres$tot.withinss
    print(kres$withinss)
    pl <- fviz_cluster(kres, data = B, geom = "point", pointsize = 0.75)
    out_plotname <- sprintf("presentacion/imgs/fviz_k%dres.png", k)
    ggsave(out_plotname, pl, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")
}

pl <- ggplot(dfwcss, aes(k, tot.wcss)) + geom_point() + geom_line() + scale_x_continuous(breaks = 1:10)
ggsave("presentacion/imgs/wcss.png", pl, width = 16, height = 8, units = "cm", dpi = 300, bg = "transparent")

# en un solo comando 
pl <- fviz_nbclust(B, kmeans, method = "wss")
ggsave("presentacion/imgs/fviz_wss.png", pl, width = 16, height = 8, units = "cm", dpi = 300, bg = "transparent")

# Método de la silueta
pl <- fviz_nbclust(B, kmeans, method = "silhouette")
ggsave("presentacion/imgs/fviz_silueta.png", pl, width = 16, height = 8, units = "cm", dpi = 300, bg = "transparent")

# Método del gap statistic
pl <- fviz_nbclust(B, kmeans, method = "gap_stat")
ggsave("presentacion/imgs/fviz_gap.png", pl, width = 16, height = 8, units = "cm", dpi = 300, bg = "transparent")

# 30 índices
NK <- NbClust(data = B, method = "kmeans")
# El número "recomendado" de grupos es 3

# Agrupamiento con k = 3
res <- kmeans(B, 3)
table(res$cluster)

# Descripción de los subgrupos
Amci$k3 <- as.factor(res$cluster)
nd <- colnames(B)
m <- melt(Amci, measure.vars = nd)
pl <- ggplot(m, aes(k3, value, colour = variable)) + geom_boxplot() + theme(legend.position = "top")
ggsave("presentacion/imgs/boxplots3.png", pl, width = 12, height = 12, units = "cm", dpi = 300, bg = "transparent")






