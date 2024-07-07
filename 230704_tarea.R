
# TAREA ------------------------------------------------------------------------
### Las tareas deberán ser entregadas a través de git. 
### En el repositorio de github que hemos estado usando existe una rama llamada 
### tareas, en la cual hay una carpeta para cada clase. 
### Hagan fetch (o pull) de esa rama y empujen sus tareas a la carpeta 
### correspondiente. 
### La estructura del archivo debe ser la siguiente: 
### nombre_apellido1_apellido2.R
###
### Nota: Es deseable que las tareas solo contengan funciones
###
### Ejercicio 1 ################################################################
### El siguiente código genera un resultado muy similar al del último ejercicio
### revisado en la clase. 
### La diferencia es que la implementación es mediante un ciclo for. 
### Adicionalmente, se agrega una funcionalidad que agrega 
### al título el año correspondiente.
###
### CODIGO DE REFERENCIA
gapminder_list <- split(gapminder, gapminder$year)
plot_with_for <- function(tablas){
  plots <- list(vector(length = length(tablas) ))
  i <- 1
  for (plot in tablas) {
    table <- sum_something(plot, continent, pop)
    plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1] )  )
    i <-  i + 1
  }
  return(plots)
}

plots <- plot_with_for(gapminder_list)

### La tarea consiste en llegar al mismo resultado, pero utilizando únicamente 
### las herramientas de purrr. 
### Crea una función llamada plot_with_purrr que reciba una lista de tablas 
### y devuelva una lista de gráficos
###
### Pista: La función imap puede ser de gran ayuda
###
### Ejercicio 2 ################################################################
### Modifica la función plot_table para que el año del gráfico aparezca en el 
### subtítulo y no en el título. 
### La función modificada debería recibir un parámetro extra llamado subtitulo, 
### que permita agregar el año al subtítulo del gráfico.
###
### Una vez que hayas modificado tu función, utilízala dentro de 
### plot_with_purrr. 
### Cada gráfico debería tener el año correspondiente en el subtítulo.
###
### Ejercicio 3 ################################################################
### El siguiente for anidado genera pares de x e y. 
### El ejercicio consiste en escribir una función llamada nested_map que 
### utilice una sintaxis de purrr. 
### La función debe recibir dos vectores numéricos (de igual o distinto largo) 
### e imprimir pares de número.
###
### Es posible que la sintaxis llegue a ser un poco confusa. 
### Reflexiona sobre la pertinencia de purrr para tareas de este tipo.
###
### CODIGO DE REFERENCIA
nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8)

# Limpiar environment ----------------------------------------------------------
rm(list=ls())

# Cargar librerías -------------------------------------------------------------

# Definir directorios ----------------------------------------------------------