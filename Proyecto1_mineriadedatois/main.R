library(devtools)
library(spotifyr)
library(tidyverse)
library(knitr)
library(dplyr)

# En primer lugar tenemos que abrir un archivo que contiene nombres de canciones 
# con todos los datos que necesitaremos para crear los modelos
dfSongs<- readRDS("~/Desktop/GIT/Minería de Datos/Proyecto_1_Mineria/Proyecto1_mineriadedatois/beats.rds")


head(dfSongs)
dim(dfSongs)

# LIMPIEZA DE DATOS

# quitamos todas las columnas que no tendrán relevancia en el estudio posterior
dfSongs <- select(dfSongs, -album_id, -album_type, -album_release_date_precision, -disc_number, -is_local, -track_preview_url)
summary(dfSongs)  # Vemos que todoas las columnas están en el tipo de dato correcto para su estudiuo
# contamos el número de nulos que hay por columna (https://www.diegocalvo.es/eliminar-na-o-valores-nulos-en-r/)
sapply(dfSongs, function(x) sum(is.na(x)))  
# vemos que solo hay datos nulos en album_release_year. Esto es probable que no nos dé
# problemas a futuro ya que para el modelo se priorizarán otras caracteríasticas de las canciones.
# Para evitar cualquier problema en todo caso, eliminaremos esta columna junto a album_release_date ya que 
# estas tienen una estrecha relación y tampoco se ocupará la segunda en los modelos.
dfSongs <- select(dfSongs, -album_release_year, -album_release_date)

head(dfSongs)
dim(dfSongs)
sapply(dfSongs, function(x) sum(is.na(x))) 

# A continuación se creará un data frame que solo contanga las columnas numéricas 
dfSNum <- dfSongs[,c(3:13, 16:17, 21)]

# Escalamos la data numérica
dfSNum = scale(dfSNum) %>% 
  as_tibble()

# por último, le colocamos un id tanto a la tabla numérica como a la principal 
# para relacionarlas de una forma facil
dfSongs <- tibble::rowid_to_column(dfSongs, "index")
dfSNum <- tibble::rowid_to_column(dfSNum, "index")
head(dfSongs)
head(dfSNum)

summary(dfSNum)
# Ahora está todo listo para trabajar con los datos




# MODELO 1
library(dbscan)

# seleccionamos solo las variables a estudiar para realizar la clusterización 
dfModel1 <- dfSNum[, c("energy", "valence", "danceability", "acousticness", "instrumentalness", "speechiness")]

head(dfModel1)

kNNdistplot(dfModel1, k = 15) # elejimos k=15 ya que este es un número de clusters
# acorde al contexto en el cual estamos trabajando
abline(h = 0.45,  lty = 2)  # elejimos eps = 0.45 ya que a partir de este valor aproximadamente
                           # (en el eje y) se comienza a ver un comportamiento exponencial en el gráfico
model1 = dbscan(dfModel1, eps = 0.45, minPts = 15)

model1

# Vemos que en el cluster número 1 (el segundo en la tabla), se agrupa el 93.2%
# de los datos. Esto se debe a la alta densidad que hay en esa zona de nuestro 
# plano multidimensional. Esto es perjudicial para el modelo de clusterización por 
# lo que intentaremos hacer este primer modelo con k-means.


# Vamos a ver como evoluciona la suma de cuadrados intra-cluster 
# en la medida que aumentamos el numero de k
SSinterior <- numeric(50)

for(k in 1:50){
  modelo <- kmeans(dfModel1, centers = k)
  SSinterior[k] <- modelo$tot.withinss
}

SSinterior

# Método de codos patra ver el número de centroides que se van a ocupar
ggplot() + geom_point(aes(x = 1:50, y = SSinterior), color = 'blue') + 
  geom_line(aes(x = 1:50, y = SSinterior), color = 'blue') + 
  ggtitle("M?todo del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

# Se puede ver el "codo" entre 9 y 11 (eje x) por lo que escojeremos 
# 10 centroides par nuestr modelo 1 con k-means

modelo_kmeans <- kmeans(dfModel1, centers = 10)
modelo_kmeans
# K-means clustering with 10 clusters of sizes 51399, 48909, 42181, 83735, 45537, 22239, 39647, 28434, 77148, 8393
# acá si podemos ver muchas canciones por cada uno de los cluster por lo que 
# la alta densidad de puntos que hay en el dataFrame no afectan negativamente al modelo
# k-means como con el modelo DBscan

# agregamos la información de a que cluster corresponde cada punto y también 
# la columna index para posteriormente mergear deModel1 y dfSongs
dfModel1 <- dfSNum[, c("index", "energy", "valence", "danceability", "acousticness", "instrumentalness", "speechiness")]
# creo la variable cluster en la tabla model1
dfModel1$cluster1 <- modelo_kmeans$cluster %>% as.factor()

dfModel1$cluster1
head(dfModel1)


# juntamos dfModel1 y dfSongs para tener la información de a que cluster pertenece cada canción 
dfModel1 <- merge(dfModel1, dfSongs, by = 'index')

# buscamos la cancion elegida
specificSong = dfModel1 %>% filter_all(any_vars(. %in% c(idxSongID)))
# conseguimos el número de cluster de la canción
cluster_ = unique(specificSong$cluster1)

# creamos un Dataframe que solo contiene canciones del cluster correspondiente al 
# cluster de la canción seleccionada
dfModel1_selec <- dfModel1 %>% filter(cluster1 == cluster_)





###############################################################################
# MODELO 2: para este segundo modelo utilizaremos GMM

# importamos todas las librerías que utilizaremos en el proceso de clusterización
library(factoextra)
library(MASS)
library(ggplot2)
# install.packages("mclust")
library(mclust)

dfModel2 <- dfSNum[, c("energy", "valence", "danceability", "acousticness", "instrumentalness", "speechiness")]

# dado que el dataFrame es extenso y que vamos a usar varias columnas de este para tener un
# modelo de clusterización más preciso, el proceso de crear las distribuciones gaussianas 
# toma bastante tiempo
mc = Mclust(dfModel2)

# Este plot nos muestra cual es el mejor k parea el modelo (punto de quebre)
plot(mc)  # en BIC, el valor más alto en el eje de las ordenadas se consigue con VVV 
# y el número de componentes (clusters) es 9

summary(mc)  # acá nos aparecen los hiperparámetros con los que se consigue 
# una mejor clustarización: Mclust VVV (ellipsoidal, varying volume, shape, 
# and orientation) model with 9 components

mc$classification

# agregamos la información de a que cluster corresponde cada punto y también 
# la columna index para posteriormente mergear deModel2 y dfSongs
dfModel2 <- dfSNum[, c("index", "energy", "valence", "danceability", "acousticness", "instrumentalness", "speechiness")]
# creo la variable cluster en la tabla model1
dfModel2$cluster2 <- mc$classification %>% as.factor()

dfModel2$cluster2
head(dfModel2)


# juntamos dfModel2 y dfSongs para tener la información de a que cluster pertenece cada canción 
dfModel2 <- merge(dfModel2, dfSongs, by = 'index')

# buscamos la cancion elegida
specificSong = dfModel2 %>% filter_all(any_vars(. %in% c(idxSongID)))
# conseguimos el número de cluster de la canción
cluster_2 = unique(specificSong$cluster)

# creamos un Dataframe que solo contiene canciones del cluster correspondiente al 
# cluster de la canción seleccionada
dfModel2_selec <- dfModel2 %>% filter(cluster2 == cluster_2)






