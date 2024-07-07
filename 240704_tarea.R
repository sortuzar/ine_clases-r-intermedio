
# Limpiar environment ----------------------------------------------------------
rm(list=ls())

# Cargar librerías -------------------------------------------------------------
### Crear vector de librerías ##################################################
librerias <- c("tidyverse",
               "ggplot2", # crear gráficos
               "feather", # cargar BBDD CASEN
               "gapminder" # contiene BBDD que usaremos
               )

### Definir función para cargar librerías ######################################
carga_librerias <- function(librerias) {
  for (i in librerias) {
    esta_disponible <- require(i, character.only = TRUE, quietly= TRUE)
    if(esta_disponible){
      library(i, character.only = TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

### Cargar librerías usando función ############################################
carga_librerias(librerias = librerias)

# Definir directorios ----------------------------------------------------------
### Definir ubicación del proyecto #############################################
folder_project <- rprojroot::find_rstudio_root_file()
folder_here <- folder_project

### Definir ubicación del script que contiene las rutas de las BBDD ############
source(paste0(folder_here,"/aux_dirs_input.R"))

# Cargar BBDD CASEN ------------------------------------------------------------
casen <- feather::read_feather(file_casen)

# Definir funciones sum_something() y plot_table() -----------------------------
### Diapositivas de las funciones ##############################################
###### Diapositivas sum_something() ######
###### https://clases-r-intermedio.github.io/2_programacion_funcional/#30
###### Diapositivas plot_table() ######
###### https://clases-r-intermedio.github.io/2_programacion_funcional/#32

### Definir función sum_something() ############################################
sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

### Probar función sum_something() #############################################
sum_something(
  ###### Seleccionar BBDD ######
  data=casen,
  ###### Seleccionar variable de agrupación ######
  group_var=region,
  ###### Seleccionar variable que vamos a sumar ######
  var=ytotcor)

### Definir función plot_table() ###############################################
plot_table <- function(table, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

### Probar función plot_table() ################################################
###### Crear objeto con sum_something() (es prerrequisito) ######
tabla <- sum_something(casen, region, ytotcor)

###### Ejecutar plot_table() sobre el output creado con sum_something() ######
plot_table(tabla, region, n,  "Total del ingreso por región" )

###### Eliminar objeto usado en ejemplo ######
rm(tabla)

# Replicar primera parte del ejercicio (antes de la tarea) ---------------------
### Diapositivas ###############################################################
### https://clases-r-intermedio.github.io/2_programacion_funcional/#71

# EJERCICIO 1 ------------------------------------------------------------------
### Diapositivas ###############################################################
### https://clases-r-intermedio.github.io/2_programacion_funcional/#74

