library(devtools)
library(spotifyr)
library(tidyverse)
library(knitr)
library(dplyr)

idxSongID = "4pP4SPB221OGmvyb7ssaTa"


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
dfModel1 <- dfSNum[, c("energy", "valence", "danceability", "acousticness")]

head(dfModel1)


#kNNdistplot(dfModel1, k = 20)  
#abline(h = 0.2,  lty = 2)  # elejimos eps = 0.2 ya que a partir de este valor aproximadamente
                           # (en el eje y) se comienza a ver un comportamiento exponencial en el gráfico
model1 = dbscan(dfModel1, eps = 0.2, minPts = 20)

model1

# agregamos la información de a que cluster corresponde cada punto y también 
# la columna index para posteriormente mergear deModel1 y dfSongs
dfModel1 <- dfSNum[, c("index", "energy", "valence", "danceability", "acousticness")]
dfModel1$cluster = model1$cluster

head(dfModel1)

# juntamos dfModel1 y dfSongs para tener la información de a que cluster pertenece cada canción 
dfModel1 <- merge(dfModel1, dfSongs, by = 'index')

# buscamos la cancion elegida
specificSong = dfModel1 %>% filter_all(any_vars(. %in% c(idxSongID)))
# conseguimos el número de cluster de la canción
cluster_ = unique(specificSong$cluster)

# creamos un Dataframe que solo contiene canciones del cluster correspondiente al 
# cluster de la canción seleccionada
dfModel1_selec <- dfModel1 %>% filter(cluster == cluster_)

