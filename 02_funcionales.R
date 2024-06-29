# -------------------------------------------------------------------------
# Clase 2: evaluacion no estandar y funcionales ---------------------------
# -------------------------------------------------------------------------

# Limpiar environment -----------------------------------------------------
rm(list=ls())

# Carga de paquetes -------------------------------------------------------
librerias <- c("tidyverse", "gapminder", "rlang", "feather", "guaguas")

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

carga_librerias(librerias = librerias)

# Fijar directorios -------------------------------------------------------
### Ubicación del proyecto ################################################
folder_project <- rprojroot::find_rstudio_root_file()
folder_here <- paste0(folder_project,"/01_funciones")

### Bases de datos ########################################################
source(paste0(folder_project,"/aux_dirs_input.R"))

# 01. Cargar datos --------------------------------------------------------
casen <- read_feather(file_casen)

# 02. Ejemplo con ERROR --------------------------------------------------

calcular_cosas <- function(data, var) {
  data %>% 
    summarise(min = min(var),
              max = max(var),
              mean = mean(var),
              median = median(var)
    )
}

calcular_cosas(gapminder, pop)


# 03. Solucionando el error -----------------------------------------------

calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var)
  data %>% 
    summarise(min = min(!!enexpr(var)),
              max = max(!!enquo_var),
              mean = mean(!!enquo_var),
              median = median(!!enquo_var)
    )
}
calcular_cosas(gapminder, pop)


# 04. Encapsular expresiones ----------------------------------------------

expr(x + y)

# muy literal
devolver_expresion <- function(x) {
  expr(x)}

devolver_expresion(a + b)

# Ahora si funciona
devolver_expresion <- function(x) {
  enexpr(x)
}
devolver_expresion(a + b)


# 05. Evaluar expresiones encapsuladas ------------------------------------

# Operador bang bang = !!
calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var) 
  data %>% 
    summarise(min = min(!!enquo_var))
}
calcular_cosas(gapminder, pop)


# 06. Usar el nombre de un parámetro para nombrar una variable ------------

# Idea 1 (no funciona)
calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var) 
  data %>% 
    summarise(var = min(!!enquo_var))
}

calcular_cosas(gapminder, pop)

# Idea 2 (error)
calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var) 
  data %>% 
    summarise(!!enquo_var = min(!!enquo_var))
}
calcular_cosas(gapminder, pop)

# Se resuleve con un nuevo operador
calcular_cosas <- function(data, var) {
  data %>% 
    summarise({{var}} := min({{var}}))}
calcular_cosas(gapminder, pop)


# 07. Convertir strings en simbolos ---------------------------------------

calcular_cosas <- function(data, var) {
  enquo_var <- sym(var)
  data %>% 
    summarise(!!sym(var)  := min(!!sym(var)))}
calcular_cosas(gapminder, "pop")

#Otra opción con rlang
calcular_cosas <- function(data, var) {
  enquo_var <- rlang::parse_expr(var)
  data %>% 
    summarise(!!enquo_var  := min(!!enquo_var))
}
calcular_cosas(gapminder, "pop")

# 08. Uso del nuevo operador {{}} -----------------------------------------

calcular_cosas <- function(data, var) {
  data %>% 
    summarise({{var}} := min({{var}}))
}
calcular_cosas(gapminder, pop)


# EJERCICIO 1 -------------------------------------------------------------

'Trabajaremos con los datos de Casen. Escriba una función llamada sum_something que agrupe por una variable y sume otra. 
Por ejemplo, sum_something(casen, region, ytotcor) debería devolver lo siguiente:'

## # A tibble: 3 × 2
##   region          n
##    <dbl>      <dbl>
## 1      1 2526228202
## 2      2 2577850836
## 3      3 2157490923

# Solucion

casen %>% 
  group_by(region)


sum_something <- function(datos, grupo, variable){
  datos %>% 
    group_by(!!enexpr(grupo)) %>% 
    summarise(!!enexpr(variable) := sum(!!enexpr(variable)))
}

sum_something <- function(datos, grupo, variable){
  datos %>% 
    group_by({{grupo}}) %>% 
    summarise({{variable}} := sum({{variable}}))
}

sum_something(casen, region, ytotcor)

sum_something <- function(datos, grupo, variable){
  variable <- paste0(variable, "_suma")
  
  datos %>% 
    group_by(!!sym(grupo)) %>% 
    summarise(!!sym(variable) := sum(!!sym(variable)))
}

sum_something(casen, "region", "ytotcor")

# Parte II
'Crea una función plot_table que acompañe a sum_something. plot_table debe recibir la tabla creada por sum_something y
devolver un gráfico de barras. Es importante que la función reciba el nombre de la variable x y la variable y. Además, 
debe existir un parámetro para agregar un título al gráfico. Pruebe el llamado a la función 
plot_table(table, region, n, "Total del ingreso por región")'



tabla_region <- sum_something(casen, region, ytotcor)

tabla_region %>% 
  ggplot(aes(x = region, y = ytotcor)) +
  geom_col() +
  labs(title = "Total del ingreso por región")

plot_table <- function(table, x, y, titulo){
  
  table %>% 
    ggplot(aes(x = {{x}}, y = {{y}})) +
    geom_col() +
    ggtitle(titulo)
  
}

plot_table(tabla_region, region, ytotcor, "Total del ingreso por región")

# Pequeño anexo de mensajes con rlang
message("hola")
stop("hola")
warning("advertencia")

rlang::inform(c(i = "informacion", x = "Error", v = "Ticket"))
rlang::abort(c(i = "informacion", x = "Error", v = "Ticket"))
rlang::warn(c(i = "informacion", x = "Error", v = "Ticket"))

# Ejemplo de flujo
# if(nrow(base) > 1000){
#   rlang::abort(c(i = "informacion", x = "Error", v = "Ticket"))
# }

# 01. Introducción a funcionales ------------------------------------------


# “To understand computations in R, two slogans are helpful:
#   
#   • Everything that exists is an object.
# • Everything that happens is a function call."
# 
# — John Chambers


# 02. Usar función como parámetro -----------------------------------------

randomise <- function(f) f(runif(10))
randomise(mean)
randomise(sum)
randomise(median)


# 03. Loops con purrr -----------------------------------------------------

cuadrado <- function(x) x ** 2

cuadrado(1)
cuadrado(2)
cuadrado(3)

map(1:3, cuadrado)


# 04. Sabemos lo que entra y sale -----------------------------------------

triple_chr <- function(x) as.character(x * 3)

# Error
map_int(1:3, triple_chr)

map_chr(1:3, triple_chr)

# 05. Trabajo con listas --------------------------------------------------

lista_vectores <- list(1:3, 1:10, 2:9, 1)
map_dbl(lista_vectores, mean)



# ¿Qué pasa si queremos usar otros parámetros?
lista_vectores <- list(1:3, c(1:10, NA))
map_dbl(lista_vectores, ~mean(.x, na.rm = TRUE))





# A la izquierda va el parámetro que varía y a la derecha el fijo.
100:150 %>% mean
100:150 %>% min
100:150 %>% max



lista_funciones <- list(mean, min, max)
map(lista_funciones, ~.x(100:150, na.rm = TRUE))


casen$ytrabajocor
casen$yaimcorh

map(c("yaimcorh", "ytrabajocor"), 
    ~casen %>% 
      summarise(!!sym(.x) := mean(!!sym(.x), na.rm = TRUE)))

# data.frames como listas

typeof(mtcars)

map(mtcars, mean)



# 06. Uso de map2 ---------------------------------------------------------

media_sumar <- function(vector, valor) {
  mean(vector) + valor
}

lista_vectores <- list(1:3, 1:5, 2:5)
valores <- c(2, 3, 8)

map2_dbl(lista_vectores, valores, media_sumar)


# EJERCICIO 2 -------------------------------------------------------------

'Tenemos una lista muy larga de elementos con nombres inscritos en el registro civil'

lista_anios <- split(guaguas, guaguas$anio)
names(lista_anios)[1:5]

'Vamos a imaginar que cada elemento ocupa mucha memoria, por lo que deben ser procesados en secuencia'
'La idea es calcular la suma de nombres (variable n) para cada año, filtrando por una variable que puede ser hombre o mujer'
set.seed(2023)
filtro <-  map_chr(1:length(lista_anios), ~sample(x = c("F", "M"), size = 1))
filtro[1:5]


lista_anios[[1]] %>% 
  filter(sexo == "F") %>% 
  summarise(n = sum(n))
tabla_region %>% pull(ytotcor)

sumar_n <- function(x,y){
  x %>% 
    filter(sexo == y) %>% 
    summarise(n = sum(n)) %>% 
    pull(n)
}

map2(lista_anios, filtro, sumar_n)

sumar_n <- as_mapper(~ .x %>% 
                       filter(sexo == .y) %>% 
                       summarise(n = sum(n)) %>% 
                       pull(n))

map2_dbl(lista_anios, filtro, sumar_n)

map2_dbl(lista_anios, filtro, sumar_n)

# Solucion ----------------------------------------------------------------


'...'

# Funciones adicionales del paquete purrr ---------------------------------

# 01. Necesito iterar sobre 3 o más input vectores ------------------------

media_sumar_dividir <- function(vector, valor_suma, valor_division) {
  (mean(vector) + valor_suma) / valor_division
}
lista_vectores <- list(1:3, 1:5, 2:5)
valores_suma <- c(2, 3, 8)
valores_division <- c(2, 1, 9)

pmap_dbl(list(lista_vectores, valores_suma, valores_division), media_sumar_dividir)



# 02. A veces interesando los efectos colaterales ------------------------

animales <- c("perro", "gato", "elefante")
map(animales, print)

# el output de map es siempre una lista, todo lo demás es colateral. Walk nos entrega el efecto colateral
walk(animales, print)


# 03. Usos de walk --------------------------------------------------------

continentes <- split(gapminder, gapminder$continent)

library(feather)
files <- paste0("02_funcionales/data/", names(continentes), ".feather")
walk2(continentes, files, write_feather)


# 04. Iteración sobre los nombres de una lista ----------------------------

files <- list.files("02_funcionales/data/datos_ene/", full.names = T)
trimestres <- list.files("02_funcionales/data/datos_ene/") %>% 
  str_extract(pattern = "-[[:digit:]]{2}-") %>% 
  str_remove_all("-")

varios_ene <- map(files, ~read_csv2(.x, guess_max = 80000))
names(varios_ene) <- paste0("trimestre_", trimestres)  
nombres_lista <-  imap(varios_ene, ~.y)
nombres_lista 


imap(varios_ene, ~.x %>% 
       mutate(trimestre = .y) %>%
       select(1:3, trimestre))


list.files("02_funcionales/data/datos_ene/", full.names = T) %>% 
  set_names(paste0("trimestre_", str_extract(., "(?<=-)[[:digit:]]{2}(?=-)"))) %>% 
  imap(~read_csv2(.x, guess_max = 80000) %>% 
         mutate(trimestre = .y)) 


# 05. Ejemplo uso de imap -------------------------------------------------

# Error
ocupados <- imap(varios_ene,
                 .f = ~.x %>% 
                   mutate({{.y}} := if_else(activ == 1, 1, 0) ))
ocupados[[1]] %>% count(trimestre_01)

# Solucion


# EJERCICIO 3 -------------------------------------------------------------

'Tenemos un listado de dataframes separados por año'

gapminder_list <- split(gapminder, gapminder$year)

'Retomemos nuestras funciones sum_something y plot_table.'
'La idea es usar purrr con el listado que tenemos y generar un gráfico para cada año en el que se muestre la población de cada continente.'

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

plot_table <- function(table, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}