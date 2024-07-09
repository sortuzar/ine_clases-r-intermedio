
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

### Crear listado de data frames separados por año #############################
gapminder_list <- split(gapminder, gapminder$year)

### Usar purrr para generar un gráfico para cada año ###########################
plots_by_year <- gapminder_list %>% 
  map(sum_something, continent, pop) %>% 
  map(plot_table, continent, n, "Población mundial, según continente" )

### Limpiar environment ########################################################
rm(gapminder_list,
   plots_by_year)

# EJERCICIO 1 ------------------------------------------------------------------
### Diapositivas ###############################################################
### https://clases-r-intermedio.github.io/2_programacion_funcional/#74

### Crear listado de data frames separados por año #############################
gapminder_list <- split(gapminder, gapminder$year)

### Veamos bien qué hace el for loops que nos entregan #########################
plot_with_for <- 
  function(
    ### El argumento tablas es una LISTA de DATA FRAMES
    tablas)
    {
    ### Abre el environment de la función
    
    ### En plots almacena una lista compuesta de un vector de longitud igual a la longitud del argumento tablas
    plots <- list(vector(length = length(tablas) ))
    ### Por lo tanto, si gapminder_list tiene una longitud de 12, la lista en plots tendrá una longitud de 12 elementos/vectores
    
    ### Se define i como 1
    i <- 1
    
    ### Abre un loop que itera sobre cada data frame en el argumento tablas
    for (plot in tablas) {
      ### Crea el objeto table usando la función sum_something()
      table <- sum_something(
        ### data=
        plot, 
        ### group_var=
        continent, 
        ### var=
        pop
        ### Cierra sum_something()
        )
      
      ### Guarda en el lugar i dentro de plots el siguiente objeto plot_table
      plots[[i]] <- plot_table(
        
        ### table=
        table, 
        ### x_var=
        continent, 
        ### y_var=
        n, 
        ### input_title=
        paste("Población mundial, según continente. Año", plot$year[1] )
        ### Cierra plot_table()
        )
      
      ### Se redefine i como el i actual + 1
      i <-  i + 1
    }
    
    ### Devuelve el objeto plots
    return(plots)
}

plots <- plot_with_for(gapminder_list)

### OBJETIVO:
### Crea una función llamada plot_with_purrr que reciba una lista de tablas y devuelva una lista de gráficos

### Limpiar environment ########################################################
rm(plot_with_for,
   plots)

### Crear un plot individualmente ##############################################
gapminder_list[["1952"]] %>%
  ### Crear tabla
  sum_something(group_var=continent,
                var=pop) %>%
  ### Crear plot
  ggplot(aes(x=continent,
             y=n)) +
  geom_bar(stat="identity") + # parámetro fijo
  labs(title=paste("Población mundial, según continente. Año", 
                   as.character(unique(gapminder_list[["1952"]]$year)),
                   " (PLOT HECHO INDIVIDUALMENTE)"))

### Ahora hagamos la serie completa con map() ##################################
ej1_prueba <- purrr::map(
  .x=gapminder_list,
  ~sum_something(data=.x,
                 group_var=continent,
                 var=pop) %>% 
    ggplot(aes(x=continent,
               y=n)) +
    geom_bar(stat="identity") +
    labs(title=paste("Población mundial, según continente. Año",
                     as.character(unique(.x$year)),
                     " (PLOT HECHO CON MAP,\nSIN EMPAQUETAR EN UNA FUNCION)"))
  )

### Ver los resultados #########################################################
### Esto nos da una lista de gráficos con el año en el título
ej1_prueba[["1952"]]
ej1_prueba[["1957"]]

### Empaquetar código en una función ###########################################
plot_with_purrr <- function(
    ### El argumento tablas toma como input una lista de data frames
  tablas)
{
  ### Abrir environment de la función
  
  ### Aplicar map()
  plots <- purrr::map(
    .x=tablas,
    ~sum_something(data=.x,
                   group_var=continent,
                   var=pop) %>% 
      ggplot(aes(x=continent,
                 y=n)) +
      geom_bar(stat="identity") +
      labs(title=paste("Población mundial, según continente. Año",
                       as.character(unique(.x$year)),
                       " (PLOT HECHO CON MAP,\nEMPAQUETADO EN UNA FUNCION)")))
  
  ### Devolver plots
  return(plots)
}

### Probar función #############################################################
ej1_resultado <- plot_with_purrr(tablas=gapminder_list)

### Ver los resultados #########################################################
ej1_resultado[["1952"]]
ej1_resultado[["1957"]]