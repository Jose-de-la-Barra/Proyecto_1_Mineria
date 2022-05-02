# Este script es para crear la playlist relacionada a la canción ingresada en menu.R
# Contiene solo el modelo que se elijió como el "mejor" con los hiperparámetros
# que se eligieron gracias al análisis que se encuentra en modelos.R

app <- function(idxSong) {
  library(devtools)
  library(spotifyr)
  library(tidyverse)
  library(knitr)
  library(dplyr)
  # install.packages("Rtsne")
  library(Rtsne)
  library(factoextra)
  library(MASS)
  library(ggplot2)
  # install.packages("mclust")
  library(mclust)
  
  # En primer lugar tenemos que abrir un archivo que contiene nombres de canciones 
  # con todos los datos que necesitaremos para crear los modelos
  # dfSongs<- readRDS("~/Desktop/GIT/Minería de Datos/Proyecto_1_Mineria/Proyecto1_mineriadedatois/beats.rds")
  dfSongs<- readRDS(file.choose())
  
  # sacamos una muestra del DataFrame original ya que, al hacer la reducción de dimensionalidad
  # con las 447622 filas del DataFrame original, se necesita una capacidad de cómputo 
  # que requiere un tiempo considerable (al igual que con otras funciones que están en los modelos)
  dfSongs <- dfSongs[0:50000,]
  
  # LIMPIEZA DE DATOS
  
  # quitamos todas las columnas que no tendrán relevancia en el estudio posterior
  dfSongs <- dplyr::select(dfSongs, -album_id, -album_type, -album_release_date_precision, -disc_number, -is_local, -track_preview_url)
  dfSongs <- dplyr::select(dfSongs, -album_release_year, -album_release_date)
  
  # A continuación se creará un data frame que solo contanga las columnas numéricas 
  dfSNum <- dfSongs[,c(3:13, 16:17, 21)]
  
  # Por último, limpiamos la data a la que no le queremos aplicar una reducción de dimensionalidad
  dfSNum <- dplyr::select(dfSNum, -key, -mode, -time_signature, -duration_ms, -track_number)
  
  
  # Normalizamos la data numérica
  dfSNum = scale(dfSNum)  
  
  
  # REDUCCIÓN DE DIMENSIONALIDAD DEL DATAFRAMEN NUMÉRICO
  set.seed(1)
  tsne <- Rtsne(dfSNum, k = 2, initial_dims = 9, check_duplicates= FALSE)
  dfSNum <- tsne$Y
  dfSNum <- data.frame(dfSNum)
  
  # por último, le colocamos un id tanto a la tabla numérica como a la principal 
  # para relacionarlas de una forma facil
  dfSongs <- tibble::rowid_to_column(dfSongs, "index")
  dfSNum <- tibble::rowid_to_column(dfSNum, "index")
  # Ahora está todo listo para trabajar con los datos
  
  ###############################################################################
  # Para este modelo utilizaremos GMM
  
  dfModel2 <- dfSNum[, c("X1", "X2")]
  
  # dado que el dataFrame es extenso y que vamos a usar varias columnas de este para tener un
  # modelo de clusterización más preciso, el proceso de crear las distribuciones gaussianas 
  # toma bastante tiempo
  mc = Mclust(dfModel2)
  
  
  # agregamos la información de a que cluster corresponde cada punto y también 
  # la columna index para posteriormente mergear deModel2 y dfSongs
  dfModel2 <- dfSNum[, c("index", "X1", "X2")]
  # creo la variable cluster en la tabla model1
  dfModel2$cluster2 <- mc$classification %>% as.factor()
  
  # juntamos dfModel2 y dfSongs para tener la información de a que cluster pertenece cada canción 
  dfModel2 <- merge(dfModel2, dfSongs, by = 'index')
  
  # buscamos la cancion elegida
  specificSong = dfModel2 %>% filter_all(any_vars(. %in% c(idxSong)))
  # conseguimos el número de cluster de la canción
  cluster_2 = unique(specificSong$cluster)
  
  # creamos un Dataframe que solo contiene canciones del cluster correspondiente al 
  # cluster de la canción seleccionada
  dfModel2 <- dfModel2 %>% filter(cluster2 == cluster_2)
  
  
  ################################################################################
  # Creación del archivo de salida
  dfFinal_2 <- dfModel2[, c("artist_name", "track_name", "duration_ms", "track_id")]
  
  # En esta ocación ocuparemos solamente el modelo que ocupa GMM (modelo 2) ya que este modelo, se 
  # comporta de una mejor manera cuando hay una gran densidad de datios que se pueden 
  # agrupar de múltiples formas distintas.
  
  # Para conseguir el tiempo que está en milisegundos en minutos, hay que dividir 
  # el valor de tiempo entre 3600000
  dfFinal_2$duration_ms <- (dfFinal_2$duration_ms)/60000  # pasamos de ms a segundos
  # cambiamos el nombre de la columna
  dfFinal_2 <- dfFinal_2%>% rename(duration_min = duration_ms)
  
  
  # agregamos una columna index para seleccionar las canciones de dfFinal_2 al azar
  dfFinal_2 <- tibble::rowid_to_column(dfFinal_2, "index")
  
  
  # buscamos la cancion elegida
  specificSong = dfFinal_2 %>% filter_all(any_vars(. %in% c(idxSong)))
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
  dfOut <- as.list(dfOut)
  
  indx = 1
  while(time_ <= 180 ) { # tener en cuenta que 3 horas son 180 minutos
    
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
  
  # escribimos el archivo de salida con las canciones 
  write.table(dfOut, file = "playlist.txt", sep=",", row.names=FALSE)
}
