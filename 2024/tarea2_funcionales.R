#*******************************************************************************
#* TAREA 2**********************************************************************
#*******************************************************************************

# Limpiar environment ----------------------------------------------------------
rm(list = ls())

# Cargar librerías -------------------------------------------------------------
### Crear vector de librerías ##################################################
librerias <- c(
  "tidyverse",
  "ggplot2", # crear gráficos
  "feather", # cargar BBDD CASEN
  "gapminder" # contiene BBDD que usaremos
)

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
### Definir ubicación del proyecto #############################################
folder_project <- rprojroot::find_rstudio_root_file()
folder_here <- paste0(folder_project, "/2024")

### Definir ubicación del script que contiene las rutas de las BBDD ############
source(paste0(folder_here, "/auxiliar/aux_dirs_input.R"))

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
  data = casen,
  ###### Seleccionar variable de agrupación ######
  group_var = region,
  ###### Seleccionar variable que vamos a sumar ######
  var = ytotcor
)

### Definir función plot_table() ###############################################
plot_table <- function(table, x_var, y_var, input_title) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var))) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

### Probar función plot_table() ################################################
###### Crear objeto con sum_something() (es prerrequisito) ######
tabla <- sum_something(casen, region, ytotcor)

###### Ejecutar plot_table() sobre el output creado con sum_something() ######
plot_table(tabla, region, n, "Total del ingreso por región")

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
  map(plot_table, continent, n, "Población mundial, según continente")

### Limpiar environment ########################################################
rm(
  gapminder_list,
  plots_by_year
)

# EJERCICIO 1 ------------------------------------------------------------------
### Diapositivas ###############################################################
### https://clases-r-intermedio.github.io/2_programacion_funcional/#74

### Crear listado de data frames separados por año #############################
gapminder_list <- split(gapminder, gapminder$year)

### Veamos bien qué hace el for loops que nos entregan #########################
plot_with_for <-
  function( ### El argumento tablas es una LISTA de DATA FRAMES
           tablas) {
    ### Abre el environment de la función

    ### En plots almacena una lista compuesta de un vector de longitud igual a la longitud del argumento tablas
    plots <- list(vector(length = length(tablas)))
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
        paste("Población mundial, según continente. Año", plot$year[1])
        ### Cierra plot_table()
      )

      ### Se redefine i como el i actual + 1
      i <- i + 1
    }

    ### Devuelve el objeto plots
    return(plots)
  }

plots <- plot_with_for(gapminder_list)

### OBJETIVO:
### Crea una función llamada plot_with_purrr que reciba una lista de tablas y devuelva una lista de gráficos

### Limpiar environment ########################################################
rm(
  plot_with_for,
  plots
)

### Crear un plot individualmente ##############################################
gapminder_list[["1952"]] %>%
  ### Crear tabla
  sum_something(
    group_var = continent,
    var = pop
  ) %>%
  ### Crear plot
  plot_table(
    x_var = continent,
    y_var = n,
    input_title = paste(
      "Población mundial, según continente. Año",
      as.character(unique(gapminder_list[["1952"]]$year)),
      "\n(PLOT HECHO INDIVIDUALMENTE)"
    )
  )

### Ahora hagamos la serie completa con map() ##################################
ej1_prueba <- purrr::map(
  .x = gapminder_list,
  ~ sum_something(
    data = .x,
    group_var = continent,
    var = pop
  ) %>%
    plot_table(
      x_var = continent,
      y_var = n,
      input_title = paste(
        "Población mundial, según continente. Año",
        as.character(unique(.x$year)),
        "\n(PLOT HECHO CON MAP, SIN EMPAQUETAR EN UNA FUNCION)"
      )
    )
)

### Ver los resultados #########################################################
### Esto nos da una lista de gráficos con el año en el título
ej1_prueba[["1952"]]
ej1_prueba[["1957"]]

### Empaquetar código en una función ###########################################
plot_with_purrr <- function(
    ### El argumento tablas toma como input una lista de data frames
    tablas) {
  ### Abrir environment de la función

  ### Aplicar map()
  plots <- purrr::map(
    .x = tablas,
    ~ sum_something(
      data = .x,
      group_var = continent,
      var = pop
    ) %>%
      plot_table(
        x_var = continent,
        y_var = n,
        input_title = paste(
          "Población mundial, según continente. Año",
          as.character(unique(.x$year)),
          "\n(PLOT HECHO CON MAP,\nEMPAQUETADO EN UNA FUNCION)"
        )
      )
  )

  ### Devolver plots
  return(plots)
}

### Probar función #############################################################
ej1_resultado <- plot_with_purrr(tablas = gapminder_list)

### Ver los resultados #########################################################
ej1_resultado[["1952"]]
ej1_resultado[["1957"]]

# EJERCICIO 2 ------------------------------------------------------------------
### Diapositivas ###############################################################
### https://clases-r-intermedio.github.io/2_programacion_funcional/#76

### Modificar función plot_table() #############################################
plot_table_mod <- function(
    table,
    x_var,
    y_var,
    input_title,
    subtitulo) {
  ggplot(
    table,
    aes(
      x = !!enexpr(x_var),
      y = !!enexpr(y_var)
    )
  ) +
    geom_bar(stat = "identity") +
    labs(
      title = input_title,
      subtitle = subtitulo
    )
}

### Crear versión modificada de plot_with_purrr() ##############################
plot_with_purrr_mod <- function(
    ### El argumento tablas toma como input una lista de data frames
    tablas) {
  ### Abrir environment de la función

  ### Aplicar map()
  plots <- purrr::map(
    .x = tablas,
    ~ sum_something(
      data = .x,
      group_var = continent,
      var = pop
    ) %>%
      plot_table_mod(
        x_var = continent,
        y_var = n,
        input_title = "Población mundial, según continente",
        subtitulo = paste0(
          as.character(unique(.x$year)),
          " (AHORA EL AÑO VA EN EL SUBTITULO)"
        )
      )
  )
  ### Devolver plots
  return(plots)
}

### Aplicar función modificada #################################################
ej2_resultado <- plot_with_purrr_mod(tablas = gapminder_list)

### Ver los resultados #########################################################
ej2_resultado[["1952"]]
ej2_resultado[["1957"]]

# EJERCICIO 3 ------------------------------------------------------------------
### Diapositivas ###############################################################
### https://clases-r-intermedio.github.io/2_programacion_funcional/#77

### Revisar función asignada ###################################################
nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2) {
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8)

### Comentarios:
### La función utiliza dos loops for anidados
### Así, a cada valor de X le asigna cada uno de los valores de Y
### Como un for está anidado en el otro, no es necesario que X e Y tengan la...
### ...misma longitud, ya que A CADA elemento de X se asignan TODOS los...
### ...elementos de Y

### El ejercicio consiste en escribir una función llamada nested_map()...
### ...que utilice una sintaxis de purrr.

### Definir función nestep_map() ###############################################
nested_map <- function(
    ### Argumento 1
    vectorX,
    ### Argumento 2
    vectorY) {
  ### Abrir ambiente de la función

  ### Función de segundo orden
  valorX <- map(
    ### Argumento 1
    .x = vectorX,
    ### Función
    function(x) {
      ### Habilitar otro environment de función

      ### Habilitar comentarios
      cat(paste0("\nPara el valor ", as.character(unique(x)), "...\n"))

      ### Función de primer orden
      valorY <- map(
        ### Argumento 2
        .x = vectorY,
        ### Función
        function(y) {
          ### Habilitar otro environment de función más

          ### Habilitar comentarios
          cat(paste0("\n...asigna el valor ", as.character(unique(y)), "\n"))

          ### Crear combinación de X e Y
          combinacion_x_y <- paste(x, y)

          ### Imprime combinación de X e Y
          print(combinacion_x_y)

          ### Cerrar environment de función de primer orden
        }
      )

      ### Cerrar environment de función de segundo orden
    }
  )

  ### Cerrar environment de función nested_map()
}

### Probar función nested_map() ################################################
nested_map(1:3, 5:8)

### Probar con vectores de longitud muy diferente ##############################
nested_map(1:3, 1:30)

### Probar con vectores de distinta naturaleza #################################
nested_map(
  c("a", "b", "c", "d"),
  1:10
)

nested_map(
  rep("ola", 10),
  rep("ola", 10)
)
