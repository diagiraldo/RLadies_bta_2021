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
B <- A %>%
    filter(dx == "MCI") %>%
    select(MEMORY, LANGUAGE, EXECUTIVE, 
           VISUOSPATIAL, ORIENTATION, ATTENTION)

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
p2 <- fviz_cluster(k2res, data = B, geom = "point", pointsize = 0.75)
p3 <- fviz_cluster(k3res, data = B, geom = "point", pointsize = 0.75)

ggsave("presentacion/imgs/fviz_k2res.png", p2, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")
ggsave("presentacion/imgs/fviz_k3res.png", p3, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")

pl <- fviz_cluster(kmeans(B, 4), data = B, geom = "point", pointsize = 0.75)
ggsave("presentacion/imgs/fviz_k4res.png", pl, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")

pl <- fviz_cluster(kmeans(B, 5), data = B, geom = "point", pointsize = 0.75)
ggsave("presentacion/imgs/fviz_k5res.png", pl, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")

pl <- fviz_cluster(kmeans(B, 6), data = B, geom = "point", pointsize = 0.75)
ggsave("presentacion/imgs/fviz_k6res.png", pl, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")

pl <- fviz_cluster(kmeans(B, 7), data = B, geom = "point", pointsize = 0.75)
ggsave("presentacion/imgs/fviz_k7res.png", pl, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")

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

# ----------------------------------------
# Varianza intra grupo - within cluster sum of squares (wcss)
k2res$withinss


set.seed(1995)

k <- 2
res <- kmeans(B, k)
res$withinss
res$tot.withinss

# suma de varianzas intra clase < varianza total 
res$totss
res$tot.withinss
res$betweenss

# A mayor número de grupos, menor es la suma de varianzas intra clase 
set.seed(2020)
df <- data.frame(k = 1:10, tot.wcss = numeric(10))
for (k in 1:10){
    df$tot.wcss[k]  <- kmeans(B, k)$tot.withinss
}

ggplot(df, aes(k, tot.wcss)) + geom_point() + geom_line()

# en un solo comando 
fviz_nbclust(B, kmeans, method = "wss")

# elbow method ---> nothing clear 

# Metodo de la silueta 
fviz_nbclust(B, kmeans, method = "silhouette")

# Gap statistic
fviz_nbclust(B, kmeans, method = "gap_stat")

# Y otro tanto de índices
NK <- NbClust(data = B, method = "kmeans")

# El número "recomendado" de grupos es 3
k <- 3
res <- kmeans(B, k)
A$grupo3 <- as.factor(res$cluster)

table(res$cluster)
table(A$dx, res$cluster)

nd <- colnames(B)
m <- melt(A, measure.vars = nd)
head(m)
ggplot(m, aes(grupo3, value, colour = variable)) + geom_boxplot()



