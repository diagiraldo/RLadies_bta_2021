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

# Ejemplos de otras distancias
tmp_B <- B[sample(1:nrow(B), 100),]

d <- dist(tmp_B, method = "euclidean")
pl <- fviz_dist(d, show_labels = FALSE, order = FALSE)
pl
ggsave("presentacion/imgs/ex_euclid.png", pl, width = 13, height = 12, units = "cm", dpi = 300, bg = "transparent")

d <- dist(tmp_B, method = "manhattan")
pl <- fviz_dist(d, show_labels = FALSE, order = FALSE)
pl
ggsave("presentacion/imgs/ex_manhattan.png", pl, width = 13, height = 12, units = "cm", dpi = 300, bg = "transparent")

d <- dist(tmp_B, method = "maximum")
pl <- fviz_dist(d, show_labels = FALSE, order = FALSE)
pl
ggsave("presentacion/imgs/ex_maxi.png", pl, width = 13, height = 12, units = "cm", dpi = 300, bg = "transparent")

d <- dist(tmp_B, method = "binary")
pl <- fviz_dist(d, show_labels = FALSE, order = FALSE)
pl
ggsave("presentacion/imgs/ex_binary.png", pl, width = 13, height = 12, units = "cm", dpi = 300, bg = "transparent")

# Distancia de Mahalanobis
my_mah_dist <- function(M, invcov){
    n <- nrow(M)
    d_mah <-  matrix(data = NA, nrow = n, ncol = n)
    for (i in 1:n){
        for (j in 1:i){
            x <- as.matrix(M[i,] - M[j,])
            d_mah[i,j] <- x %*% invcov %*% t(x)
        }
    }
    d <- as.dist(sqrt(d_mah))
    return(d)
}

tmp_B <- B[sample(1:nrow(B), 100),]
d <- my_mah_dist(tmp_B, solve(cov(B)))

pl <- fviz_dist(d, show_labels = FALSE, order = FALSE)
pl
ggsave("presentacion/imgs/ex_mah.png", pl, width = 13, height = 12, units = "cm", dpi = 300, bg = "transparent")

# K-medoids / PAM
d <- my_mah_dist(B, solve(cov(B)))
k <- 3
pamres <- pam(d, k, diss = TRUE, pamonce = 5)

table(pamres$clustering, res$cluster)

# Revisar el número de subgrupos
pl <- fviz_nbclust(B, FUNcluster = pam, method = "wss", diss = d) 
pl
ggsave("presentacion/imgs/pammah_fviz_wss.png", pl, width = 16, height = 8, units = "cm", dpi = 300, bg = "transparent")

pl <- fviz_nbclust(B, FUNcluster = pam, method = "silhouette", diss = d) 
pl
ggsave("presentacion/imgs/pammah_fviz_sil.png", pl, width = 16, height = 8, units = "cm", dpi = 300, bg = "transparent")

pl <- fviz_nbclust(B, FUNcluster = pam, method = "gap", diss = d) 
pl
ggsave("presentacion/imgs/pammah_fviz_silgap.png", pl, width = 16, height = 8, units = "cm", dpi = 300, bg = "transparent")

NK <- NbClust(data = B, diss = d, distance = NULL, method = "ward.D2")

# Descripción de los subgrupos
Amci$pam3 <- as.factor(pamres$clustering)
nd <- colnames(B)
m <- melt(Amci, measure.vars = nd)
pl <- ggplot(m, aes(pam3, value, colour = variable)) + geom_boxplot() + theme(legend.position = "top")
ggsave("presentacion/imgs/boxplots_pam3.png", pl, width = 12, height = 12, units = "cm", dpi = 300, bg = "transparent")


# Comparación de resultados con kmeans y pam
pamres$data <- B
pl <- fviz_cluster(pamres, geom = "point", pointsize = 0.75)
pl
ggsave("presentacion/imgs/fviz_pam3.png", pl, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")

pl <- fviz_cluster(res, data = B, geom = "point", pointsize = 0.75)
pl
ggsave("presentacion/imgs/fviz_kmeans3.png", pl, width = 14, height = 12, units = "cm", dpi = 300, bg = "transparent")

