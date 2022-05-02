library(devtools)
library(spotifyr)
library(tidyverse)
library(knitr)
library(dplyr)

# En primer lugar tenemos que abrir un archivo que contiene nombres de canciones 
# con todos los datos que necesitaremos para crear los modelos
# dfSongs<- readRDS("~/Desktop/GIT/Minería de Datos/Proyecto_1_Mineria/Proyecto1_mineriadedatois/beats.rds")
dfSongs<- readRDS(file.choose())

head(dfSongs)
dim(dfSongs)

# sacamos una mustra de el DataFrame original ya que en la reducción de dimensionalidad
# se necesita mucha capacidad de cómputo como para hacerla con las 447622 filas 
dfSongs <- dfSongs[1:20000,]

# LIMPIEZA DE DATOS

# quitamos todas las columnas que no tendrán relevancia en el estudio posterior
dfSongs <- dplyr::select(dfSongs, -album_id, -album_type, -album_release_date_precision, -disc_number, -is_local, -track_preview_url)
summary(dfSongs)  # Vemos que todoas las columnas están en el tipo de dato correcto para su estudiuo
# contamos el número de nulos que hay por columna (https://www.diegocalvo.es/eliminar-na-o-valores-nulos-en-r/)
sapply(dfSongs, function(x) sum(is.na(x)))  
# vemos que solo hay datos nulos en album_release_year. Esto es probable que no nos dé
# problemas a futuro ya que para el modelo se priorizarán otras caracteríasticas de las canciones.
# Para evitar cualquier problema en todo caso, eliminaremos esta columna junto a album_release_date ya que 
# estas tienen una estrecha relación y tampoco se ocupará la segunda en los modelos.
dfSongs <- dplyr::select(dfSongs, -album_release_year, -album_release_date)

# A continuación se creará un data frame que solo contanga las columnas numéricas 
dfSNum <- dfSongs[,c(3:13, 16:17, 21)]

# Por último, limpiamos la data a la que no le queremos aplicar una reducción de dimensionalidad
dfSNum <- dplyr::select(dfSNum, -key, -mode, -time_signature, -duration_ms, -track_number)


# Normalizamos la data numérica
dfSNum = scale(dfSNum)  
  as_tibble()


# REDUCCIÓN DE DIMENSIONALIDAD DEL DATAFRAMEN NUMÉRICO
# install.packages("Rtsne")
# library(Rtsne)

set.seed(1)
tsne <- Rtsne(dfSNum, k = 2, initial_dims = 9, check_duplicates= FALSE)
dfSNum <- tsne$Y
dfSNum <- data.frame(dfSNum)

# por último, le colocamos un id tanto a la tabla numérica como a la principal 
# para relacionarlas de una forma facil
dfSongs <- tibble::rowid_to_column(dfSongs, "index")
dfSNum <- tibble::rowid_to_column(dfSNum, "index")

summary(dfSNum)

# Por último, calculamos la estadística de hopkings para ver la capacidad de 
# clusterizción del DataFrame numérico
library(factoextra)
res <- get_clust_tendency(dfSNum, n = nrow(dfSNum)-1, graph = FALSE)
res$hopkins_stat

# Ahora está todo listo para trabajar con los datos


# MODELO 1
library(dbscan)

# seleccionamos solo las variables a estudiar para realizar la clusterización 
dfModel1 <- dfSNum[, c("X1", "X2")]

kNNdistplot(dfModel1, k = 15) # elejimos k=15 ya que este es un número de clusters
# acorde al contexto en el cual estamos trabajando
abline(h = 1.6,  lty = 2)  # elejimos eps = 1.6 ya que a partir de este valor aproximadamente
                           # (en el eje y) se comienza a ver un comportamiento exponencial en el gráfico
model1 = dbscan(dfModel1, eps = 1.6, minPts = 15)

model1

# Vemos que en el cluster número 1 (el segundo en la tabla), se agrupa el 93.2%
# de los datos. Esto se debe a la alta densidad que hay en esa zona de nuestro 
# plano multidimensional. Esto es perjudicial para el modelo de clusterización por 
# lo que intentaremos hacer este primer modelo con k-means.

# K-MEANS:

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

# Se puede ver el "codo" entre 8 y 10 (eje x) por lo que escojeremos 
# 9 centroides par nuestr modelo 1 con k-means

modelo_kmeans <- kmeans(dfModel1, centers = 9)
modelo_kmeans
# K-means clustering with 9 clusters of sizes 2273, 2029, 2106, 2354, 2654, 1939, 2184, 2189, 2272
# acá si podemos ver muchas canciones por cada uno de los cluster por lo que 
# la alta densidad de puntos que hay en el dataFrame no afectan negativamente al modelo
# k-means como con el modelo DBscan

# agregamos la información de a que cluster corresponde cada punto y también 
# la columna index para posteriormente mergear deModel1 y dfSongs
dfModel1 <- dfSNum[, c("index", "X1", "X2")]
# creo la variable cluster en la tabla model1
dfModel1$cluster1 <- modelo_kmeans$cluster %>% as.factor()

head(dfModel1)

# juntamos dfModel1 y dfSongs para tener la información de a que cluster pertenece cada canción 
dfModel1 <- merge(dfModel1, dfSongs, by = 'index')

# buscamos la cancion elegida
specificSong = dfModel1 %>% filter_all(any_vars(. %in% c(idxSongID)))
# conseguimos el número de cluster de la canción
cluster_ = unique(specificSong$cluster1)

# creamos un Dataframe que solo contiene canciones del cluster correspondiente al 
# cluster de la canción seleccionada
dfModel1 <- dfModel1 %>% filter(cluster1 == cluster_)



###############################################################################
# MODELO 2: para este segundo modelo utilizaremos GMM

# importamos todas las librerías que utilizaremos en el proceso de clusterización
library(factoextra)
library(MASS)
library(ggplot2)
# install.packages("mclust")
library(mclust)

dfModel2 <- dfSNum[, c("X1", "X2")]

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
dfModel2 <- dfSNum[, c("index", "X1", "X2")]
# creo la variable cluster en la tabla model1
dfModel2$cluster2 <- mc$classification %>% as.factor()

head(dfModel2)

# juntamos dfModel2 y dfSongs para tener la información de a que cluster pertenece cada canción 
dfModel2 <- merge(dfModel2, dfSongs, by = 'index')

# buscamos la cancion elegida
specificSong = dfModel2 %>% filter_all(any_vars(. %in% c(idxSongID)))
# conseguimos el número de cluster de la canción
cluster_2 = unique(specificSong$cluster)

# creamos un Dataframe que solo contiene canciones del cluster correspondiente al 
# cluster de la canción seleccionada
dfModel2 <- dfModel2 %>% filter(cluster2 == cluster_2)


################################################################################

dfFinal_1 <- dfModel1[, c("artist_name", "track_name", "duration_ms", "track_id")]
dfFinal_2 <- dfModel2[, c("artist_name", "track_name", "duration_ms", "track_id")]

# DaraFrame con las filas comunes entre dfModel1 y dfModel2
# dfComun = intersect(dfFinal_1, dfFinal_2)


# Para conseguir el tiempo que está en milisegundos en minutos, hay que dividir 
# el valor de tiempo entre 3600000
dfFinal_2$duration_ms <- (dfFinal_2$duration_ms)/60000
# cambiamos el nombre de la columna
dfFinal_2 <- dfFinal_2%>% rename(duration_min = duration_ms)


# agregamos una columna index para seleccionar las canciones de dfFinal_2 al azar
dfFinal_2 <- tibble::rowid_to_column(dfFinal_2, "index")


# buscamos la cancion elegida
specificSong = dfFinal_2 %>% filter_all(any_vars(. %in% c(idxSongID)))
# conseguimos la duración de la canción
time_ = unique(specificSong$duration_min)  

indx_canciones_elegidas = sample(0:dim(dfFinal_2)[1], dim(dfFinal_2)[1],replace=FALSE)  # entrega n números aleatorio desde el 0 hasta 
# el número total de filas que hay en dfFinal_2 siendo n el número total de filas que hay en dfFinal_2
# replace=FALSE para que no se piuedan repetir los números

# lo siguiente se hizo para sacar de la lista de números aleatorios al index que corresponde a la canción elegida
indx_canciones_elegidas <- data.frame(indx_canciones_elegidas)
indx_canciones_elegidas <- filter(indx_canciones_elegidas, indx_canciones_elegidas != specificSong$index)


dfOut <- data.frame(  # creamos el df n donde se encontrará la playlist final
  "artist_name" = specificSong$artist_name,
  "track_name" = specificSong$track_name,
  "duration_min" = ((specificSong$duration_min))
)

# Data frame to list
dfOut <- as.list(dfOut)


indx = 1
while(time_ <= 180) { # tener en cuenta que 3 horas son 180 minutos
  
  actual_indx = indx_canciones_elegidas[indx, ] # agarramos un número de la matriz de números aleatorios
  current_song = filter(dfFinal_2, index == actual_indx) # en la columna index, buscamos el número aleatorio para encontrar la canción específica que se agregará en esta iteración
  
  # Add the new row
  dfOut$artist_name <- rbind(dfOut$artist_name, current_song$artist_name)
  dfOut$track_name <- rbind(dfOut$track_name, current_song$track_name)
  dfOut$duration_min <- rbind(dfOut$duration_min, current_song$duration_min)

  indx = indx + 1 # acualizamos index
  time_ = (time_ + current_song$duration_min) # actualizamos el tiempo total (la suma de todos los tiempos)
}

# Convert to data.frame
dfOut <- data.frame(dfOut, stringsAsFactors = FALSE)



