# Aquí es donde se ingresan los datos de entrada

# idxSongID puede ser tanto la canción como el ID de la canción a la que 
# quieres hacerle una playlist

idxSong = "Ingrese acá el ID o el nombre de la canción seleccionada"

# Consideraciones:

# 1. Para llamar a la función principal por primera vez, hay que ir a funcion_principal.R y correr "app"
# (type=function) para guardarla. Si no se hace esto, la siguiente línea de código tirará un error.

# 2. Si bien en main.c hay dos modelos realizados, solo usamos el segundo en funcion_principal.R
# (todo esto está explicado en modelos.R)

# 3. Si se desea aumentar la muestra (número se canciones) sobre la cual se hace
# la muestra o si se quiere disminuir el tiempo que se demora el algoritmo en clusterizar
# y categorizar las canciones, hay que modificar la línea de código número 27 en el caso
# del archivo funcion_principal.R y la línea 3 en el caso del archivo modelos.R.

app(idxSong)
