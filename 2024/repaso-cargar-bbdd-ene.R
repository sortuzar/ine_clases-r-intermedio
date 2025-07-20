#*******************************************************************************
#* EJERCICIO********************************************************************
#*******************************************************************************

# Limpiar environment ----------------------------------------------------------
rm(list = ls())

# Cargar librerías -------------------------------------------------------------
### Crear vector de librerías ##################################################
librerias <- c("tidyverse")

### Definir función para cargar librerías ######################################
carga_librerias <- function(librerias) {
  for (i in librerias) {
    esta_disponible <- require(i, character.only = TRUE, quietly = TRUE)
    if (esta_disponible) {
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
### Definir ubicación del directorio local #####################################
carpeta_proyecto <- rprojroot::find_rstudio_root_file()
carpeta_aqui <- paste(carpeta_proyecto, "2024", sep = "/")
carpeta_datos <- paste(carpeta_aqui, "data", sep = "/")

# Cargar BBDD ENE y unir los df en una lista -----------------------------------
### Encontrar los archivos en el directorio indicado ###########################
carpeta_datos_ene <-
  paste(
    carpeta_datos,
    "datos_ene",
    sep = "/"
  )

files <-
  list.files(
    path = carpeta_datos_ene,
    full.names = T,
    recursive = FALSE
  )

### Crear vector de trimestres #################################################
trimestres <-
  list.files(
    path = carpeta_datos_ene
  ) %>%
  str_extract(pattern = "-[[:digit:]]{2}-") %>%
  str_remove_all("-")

### Crear lista que contiene los df ############################################
varios_ene <- map(files, ~ read_csv2(.x, guess_max = 80000)) # varios_ene tiene las BBDD en una lista

### Poner nombre a cada elemento de la lista usando el vector trimestres #######
names(varios_ene) <- paste0("trimestre_", trimestres) # ponemos nombres a cada trimestre

### Iterar el objeto varios_ene para obtener los nombres de elementos ##########
nombres_lista <- imap(varios_ene, ~.y) # iterar varios_ene y obtener su nombre (para eso es imap())

### Revisar output del último proceso ##########################################
nombres_lista # (el output también es una lista)

### Podemos crear una columna con el trimestre en cada df usando imap() ########
imap(varios_ene, ~ .x %>%
  mutate(trimestre = .y) %>%
  select(1:3, trimestre))

### Limpiar environment ########################################################
rm(
  trimestres,
  varios_ene,
  files,
  nombres_lista
)

### Esto no era parte de la tarea, pero me resultó útil repasarlo ##############

# Cargar BBDD ENE usando pocas líneas de código --------------------------------
varios_ene <-
  ### Primero especificar rutas con list.files() ###############################
  list.files(carpeta_datos_ene, full.names = T) %>%
  ### Segundo pegar nombres extrayendo información del trimestre con regex #####
  set_names(paste0("trimestre_", str_extract(., "(?<=-)[[:digit:]]{2}(?=-)"))) %>%
  ### Iterar con imap() la carga de cada archivo csv ###########################
  imap(
    ###### Cargar CSV ######
    ~ read_csv2(.x, # argumento es .x (primer elemento que varía)
      guess_max = 80000
    ) %>%
      ###### Crear columna de trimestre ######
      mutate(
        trimestre = .y
        # identifica .y y crea una variable llamada
        # trimestre en todos los df con el valor .y (segundo elemento que varía)
      ) %>%
      ###### Reubicar columna de trimestre ######
      relocate(trimestre, .before = 1) # ubica la columna de trimestre al inicio del df
  )
