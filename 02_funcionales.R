# ------------------------------------------------------------------------------
# Clase 2: evaluacion no estandar y funcionales --------------------------------
# ------------------------------------------------------------------------------

# Limpiar environment ----------------------------------------------------------
rm(list=ls())

# Carga de paquetes ------------------------------------------------------------
### Crear vector de librerías ##################################################
librerias <- c("tidyverse", 
               "gapminder", # paquete con bases de datos
               "rlang", 
               "feather", # cargar base de CASEN
               "guaguas" # paquete de datos demográficos de Chile
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

# Fijar directorios ------------------------------------------------------------
### Ubicación del proyecto #####################################################
folder_project <- rprojroot::find_rstudio_root_file()
folder_here <- paste0(folder_project,"/02_funcionales")

### Bases de datos #############################################################
source(paste0(folder_project,"/aux_dirs_input.R"))

# Comentarios antes de partir la clase -----------------------------------------
###### Funcionales significa funciones con otra función como argumento

# 01. Cargar datos -------------------------------------------------------------
casen <- read_feather(file_casen)

# 02. Ejemplo con ERROR --------------------------------------------------------

calcular_cosas <- function(data, var) {
  data %>% 
    summarise(min = min(var),
              max = max(var),
              mean = mean(var),
              median = median(var)
    )
}

calcular_cosas(gapminder, pop) # object pop not found
# no encuentra pop dado cómo se define la función
# gapminder es una bbdd de la librería gapminder
# pop es una de las columnas de gapminder
View(gapminder)

# debemos ingresar la variable como una *expresión*, no como un vector

# redefinir un nuevo objeto dentro del environment para resolver esto

# 03. Solucionando el error ----------------------------------------------------
### Definir la función #########################################################
calcular_cosas <- function(data, var) {
  ### Crear objeto auxiliar que encapsula enexpr en objetos con prefijo "enquo_"
  enquo_var <- enexpr(var) # quo = quote, así exquo_var es una expresión enriquecida de var
  data %>% 
    summarise(
      ### !! se denomina "bang-bang" (como pistola)
      ### Exige que se lea la expresión *COMO EXPRESIÓN*
      ### Hace funcionales ciertas citaciones o llamados a variables
      
      ### Usando argumento enexpr explícitamente, no encapsulándolo en enquo_var
      min = min(!!enexpr(var)),
      ### Usando argumento encapsulado en enquo_var, sin usarlo explícitamente
      max = max(!!enquo_var),
      mean = mean(!!enquo_var),
      median = median(!!enquo_var)
    )
  }

### Aplicar la función #########################################################
calcular_cosas(gapminder, pop)


# 04. Encapsular expresiones ----------------------------------------------

expr(x + y)
###### Los argumentos de una función se pueden evaluar directamente o dar
###### de manera citada
###### Los argumentos pueden ser EVALUADOS o QUOTED (¿CITADOS?) y son cosas distintas

pop # no existe en el environment, por eso no lo carga

### Ejemplo de argumentos quoted
by_cyk <- mtcars %>% 
  summarise(mean=mean(mpg)) # mpg está quoted

expr(x + y) # expr encapsula la función x + y

# muy literal
devolver_expresion <- function(x) {
  expr(x)}

devolver_expresion(a + b) # devuelve X
###### Definimos un argumento x como argumento de la expresión
###### independiente de lo que pongamos en los paréntesis, la función siempre entrega x
###### por cómo está definida la función
###### La función expr se toma las cosas de manera muy literal y no es flexible 
###### a la hora de capturar expresiones

# Ahora sí funciona
devolver_expresion <- function(x) {
  enexpr(x)
}
devolver_expresion(a + b) # enexpr evalúa x en la función a + b, entonces devuelve a + b

devolver_expresion(pollito) # devuelve pollito

# 05. Evaluar expresiones encapsuladas ------------------------------------

# Operador bang bang = !!
calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var) 
  data %>% 
    summarise(min = min(!!enquo_var))
}
calcular_cosas(gapminder, pop) 

### Los ejemplos anteriores nos permiten citar objetos (columnas, etc.) en vez de evaluarlos
### y así podemos llamarlos desde las funciones sin tener problemas
### el !! hace UNQUOTING, porque ya hicimos quoting con enexpr

# 06. Usar el nombre de un parámetro para nombrar una variable ------------

# Idea 1 (no funciona)
calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var) 
  data %>% 
    summarise(var = min(!!enquo_var))
}

calcular_cosas(gapminder, pop) # la columna se denomina var, R fue demasiado literal

## esto se denomina evaluación no estándar (término técnico)

## podremos asignar nombres en función de ciertas condiciones? para ahorrarnos trabajo

## mezclaremos la evaluación no estándar con un nuevo operador para resolver esto

# Idea 2 (error)
calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var) 
  data %>% 
    summarise(!!enquo_var = min(!!enquo_var))
}
calcular_cosas(gapminder, pop) # esto no corre
# el signo igual no debería estar ahí
# tenemos un signo específico para estos contextos
# forma específica de asignar nuevas variables cuando estamos usando esta clase de códigos
# el operador es := (la morsita, o walrus)
# utilizando la morsita R entiende que lo que estamos haciendo es reasignar o asignar variables
# a través de una citación no estándar de una expresión congelada que estoy activando
# y que esa nueva variable o variable sobreescrita se genera a partir de una función que también ocupa
# la evaluación no estándar para aplicarse
# en la práctica se resuelve con poquito código

# Se resuelve con un nuevo operador
calcular_cosas <- function(data, var) {
  data %>% 
    summarise({{var}} := min({{var}}))} # evaluación no estándar + morsita
calcular_cosas(gapminder, pop)

# {{}} # se denomina doble embrace, son un operador que resuelve esto con menos código
# se supone que no importa si el input es un texto o un nombre/expresión
# no necesita bang-bang porque congela la expresión y la unquotea altiro, nos ahorra escribirlo

# 07. Convertir strings en simbolos ---------------------------------------

calcular_cosas <- function(data, var) {
  enquo_var <- sym(var) # sym es específico para caracteres
  data %>% 
    summarise(!!sym(var)  := min(!!sym(var)))}
calcular_cosas(gapminder, "pop") # si usamos sym el input debe ser un carácter (texto)

#Otra opción con rlang
calcular_cosas <- function(data, var) {
  enquo_var <- rlang::parse_expr(var)
  data %>% 
    summarise(!!enquo_var  := min(!!enquo_var))
}
calcular_cosas(gapminder, "pop") # el input debe ser una cadena de texto

# Una manera artesanal podría ser la siguiente
calcular_cosas <- function(data, var) {
  enquo_var <- sym(var) # sym es específico para caracteres
  data %>% 
    summarise(!!sym(paste0(var,"_xiwi"))  := min(!!sym(var)))}
calcular_cosas(gapminder, "pop") # si usamos sym el input debe ser un carácter (texto)

# 08. Uso del nuevo operador {{}} -----------------------------------------

calcular_cosas <- function(data, var) {
  data %>% 
    summarise({{var}} := min({{var}}))
}
calcular_cosas(gapminder, pop)



# EJERCICIO
sum_something <- function(data, 
                            var_agrupar,
                            var_sumar)
{
  data %>% 
    group_by({{var_agrupar}}) %>% 
    summarise({{var_sumar}} := sum({{var_sumar}},
                                na.rm=TRUE))
}

casen_table <- sum_something(casen %>% 
                select(region,
                       ytotcor),
              region,
              ytotcor)

# parte 2
plot_table <- function(mytable,
                       variable_segmentacion,
                       variable_segmentar)
{
  mytable %>% 
    ggplot(aes(x={{variable_segmentacion}},
               y={{variable_segmentar}})) +
    geom_col()
}

plot_table(mytable=casen_table,
           variable_segmentacion = region,
           variable_segmentar = ytotcor)

# conviene partir por hacer la función sin evaluación no estándar
# veamos que funcione para un caso de los elementos que queremos iterar
# acá está resuelto como lo replicamos en clases
casen_table %>% 
  ggplot(aes(x=region,
             y=ytotcor)) + 
  geom_col() +
  labs(title="Ola")

plot_table <- function(mytable,
                       variable_segmentacion,
                       variable_segmentar)
{
  mytable %>% 
    ggplot(aes(x={{variable_segmentacion}},
               y={{variable_segmentar}})) + 
    geom_col() +
    labs(title="Ola")
}

plot_table(mytable=casen_table,
           variable_segmentacion = region,
           variable_segmentar = ytotcor) # FUNCIONA



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

randomise <- function(f) f(runif(10)) # runif genera un subset de datos con valores aleatorios (distribución uniforme)
randomise(f=mean) # su input es un argumento que obtiene la media de valores aleatorizados
randomise(f=sum)
randomise(f=median)

# Típicamente, las funcionales se usan como alternativa a los loops

# Usaremos la librería purrr

# 03. Loops con purrr -----------------------------------------------------

cuadrado <- function(x) x ** 2

cuadrado(1)
cuadrado(2)
cuadrado(3)

map(1:3, cuadrado)
# ¿qué hace map()? 
# para el conjunto de valores 1:3, con longitud determinada,
# aplica la función cuadrado
# al dominio constituido por {1, 2, 3}, aplica la función cuadrado()
# entonces tendríamos el recorrido {cuadrado(1), cuadrado(2), cuadrado(3)}
# se asemeja mucho a la definición matemática de función
# nos interesa aplicar una misma función a múltiples elementos, para eso usaremos map
# pero necesitamos saber qué será ese resultado y cómo trabajaremos con ese resultado
# por definición el output siempre será una lista, independiente del tipo de objeto que yo ingresé
# en el ejemplo visto, obtengo 3 listas, cada una de esas listas contiene un elemento
# si guardo eso en un objeto obtengo una lista con 3 elementos, cada uno de los cuales contiene un elemento, que es un vector numérico
map_dbl(1:3,cuadrado)

map_int(1:3,cuadrado)

map_chr(1:3,cuadrado) # tiene problemas porque tendría que transformar el número

map_df(1:3,cuadrado) # tiene problemas

map_df(data.frame(ola=1:3),cuadrado)

# 04. Sabemos lo que entra y sale -----------------------------------------
### Clase 4
### Jueves 4 de julio de 2024

triple_chr <- function(x) as.character(x * 3)

# Error
map_int(1:3, triple_chr) # ERROOOORRRRR

map_chr(1:3, triple_chr) # funciona

### map() se define como funcional (funciones que admiten funciones como argumentos o parámetros)

# 05. Trabajo con listas --------------------------------------------------

lista_vectores <- list(1:3, 1:10, 2:9, 1) # todos los elementos tienen longitudes distintas
### no necesita haber correspondencia entre longitud o naturaleza/tipo de los objetos
### las listas son extremadamente flexibles en este sentido
map_dbl(lista_vectores, mean) # podemos escribir mean así sin más solo si no agregamos más especificaciones en argumentos o parámetros 



# ¿Qué pasa si queremos usar otros parámetros?
lista_vectores <- list(1:3, c(1:10, NA))
map_dbl(lista_vectores, ~mean(.x, na.rm = TRUE)) # ahora debemos agregar colita de chancho y paréntesis

### es lo mismo que escribir:
map_dbl(.x = lista_vectores, ~mean(.x, na.rm = TRUE)) # ahora debemos agregar colita de chancho y paréntesis
### por eso el primer argumento en mean() es .x (reemplaza cada uno de los elementos de la lista de vectores)
### en el contexto de map() .x significa recursividad; es lo mismo que . en across

### ver objeto de listas
typeof(mtcars)

map_dbl(mtcars, mean) ### entiende que mtcars es un data frame (una especie de lista) e itera la función sobre cada una de las columnas

# A la izquierda va el parámetro que varía y a la derecha el fijo.
100:150 %>% mean
100:150 %>% min
100:150 %>% max
### a la izquierda va el elemento iterado y a la derecha va el objeto fijo (función)
### (la función es la misma en cada iteración, es un elemento fijo, en cambio lo que iteramos es variable)
### podemos jugar con qué es lo fijo y lo variado para hacer varias operaciones

lista_funciones <- list(mean, min, max)
map(lista_funciones, ~.x(100:150, na.rm = TRUE))
### Entrega:
### - la media de 100:150 (output de la primera función)
### - el mínimo de 100:150 (output de la segunda función)
### - el máximo de 100:150 (output de la tercera función)
### aquí invierto qué corresponde a lo fijo y lo variable:
### lo que se itera es la función a aplicar y lo fijo es el vector 100:150
### si identificamos bien lo fijo y lo variable podemos pensar formas de obtener objetos aplicando lo fijo a lo que varía

# existe map()... y map2()
# map2() permite que aquella parte que es lo iterado tiene un lado B
# luego, puedo iterar dos cosas al mismo tiempo; es muy útil
# lo importante: si vamos a iterar dos cosas: entonces vamos a cerciorarnos de que esas cosas que iteramos
# ...tengan la misma longitud, porque se van a ir correspondiendo

# en map también podemos usar evaluación no estándar para asignar nuevos nombres, recodificar variables en un mismo mutate, etc 
casen$ytrabajocor
casen$yaimcorh

map(c("yaimcorh", "ytrabajocor"), 
    ~casen %>% 
      summarise(!!sym(.x) := mean(!!sym(.x), na.rm = TRUE)))

# data.frames como listas

typeof(mtcars)

map(mtcars, mean)



# 06. Uso de map2 ---------------------------------------------------------
### ahora sí map2()

### creo la función media_sumar()
media_sumar <- function(vector, valor) {
  mean(vector) + valor
}

### recibe un vector y un entero único
### suma el conjunto de valores del vector que estoy ingresando con ese valor en específico

lista_vectores <- list(1:3, 1:5, 2:5) # tengo una lista con tres elementos, pero dentro de cada elemento hay diferente número de elementos/objetos
valores <- c(2, 3, 8)

map2_dbl(.x=lista_vectores, # elementos a iterar 1
         .y=valores, # elementos a iterar 2
         .f=media_sumar # componente fijo (función)
         ) # map no se complica es que .x e .y sean el mismo tipo de objeto, pero sí le importa que su longitud sea la misma (3 y 3)
# para el primer elemento de la lista y el primer vector de valores, calcula la media_sumar (estima la media de .x y súmale .y)
mean(lista_vectores[[1]])+valores[1] # comprobamos que da el resultado correcto
# para el segundo elemento de la lista y el segundo vector de valores, calcula la media_sumar (estima la media de .x y súmale .y)
mean(lista_vectores[[2]])+valores[2] # comprobamos que da el resultado correcto
# para el tercer elemento de la lista y el tercer vector de valores, calcula la media_sumar (estima la media de .x y súmale .y)
mean(lista_vectores[[3]])+valores[3] # comprobamos que da el resultado correcto

# Ejercicio
### correr este código que nos entregan
lista_anios <- split(guaguas::guaguas,~anio)
names(lista_anios)[1:5]

# para cada año, obtener la suma de los nombres de hombres o mujeres (filtrar) para cada elemento de nuestra lista guaguas
 ### correr este código que nos entregan
filtro <- map_chr(1:length(lista_anios),
                  ~sample(x=c("F","M"),
                          size=1))
filtro[1:5]

### dentro de cada elemento de la lista lista_anios hay un data frame
### tengo que: entrar al data frame, usar el filtro para seleccionar al azar si tomo los hombres o mujeres
### y obtener el valor por año
### luego, el primer elemento a iterar son los años
### a cada elemento año debe corresponder un valor de filtro a iterar

### primero cuento los nombres para un data frame específico
ejemplo_data <- lista_anios[[1]]
ejemplo_contar_nombres <- length(sort(na.omit(unique(ejemplo_data$nombre))))
ejemplo_contar_nombres # 2014 nombres distintos

### usando el insumo anterior genero mi función de recuento de nombres
contar_nombres <- function(
    ### necesito dos argumentos
    ### data frame
    data,
    ### 
    var_nombre)
{
  ### environment de la función
  recuento_nombres <- {{data}} %>%
    select({{var_nombre}}) %>% 
    unlist()
  
  recuento_nombres <- recuento_nombres[[1]]
  
  recuento_nombres <- length(sort(na.omit(unique(recuento_nombres))))
  
  ### devolver objeto
  return(recuento_nombres)
    }

### ahora pruebo la función
contar_nombres(data=anios_lista[[1]],
               var_nombre=nombre) # no funciona

# solución correcta
### probar con un df específico
lista_anios[[1]] %>%
  filter(sexo==filtro[1]) %>% # el primer elemento de la lista de años se corresponde con el primer objeto del vector filtro 
  summarise(n=sum(n))

### creemos una función
sumar_n <- function(x,y)
{
  x %>% 
    filter(sexo==y) %>% 
    summarise(n=sum(n)) %>% 
    pull(n)
}

sumar_n(x=lista_anios[[1]],
        y=filtro[1])

### aplico map2
map2(.x=lista_anios,
     .y=filtro,
     .f=sumar_n)

### veamos el valor en posición 88 de vector de lista_anios
map2(.x=lista_anios[88], # debes usar un corchete, no 2
     .y=filtro[88],
     .f=sumar_n) # no se ocupa el embrace {{}} porque no estamos evlauando la variable sexo de manera no estándar, estamos iterando el valor que toma

### con as_mapper definimos la función como un map
sumar_n_map <- as_mapper(~ .x %>% 
                           filter(sexo==.y) %>% 
                           summarise(n=sum(n)) %>% 
                           pull(n))
map2_dbl(lista_anios,
         filtro,
         sumar_n_map)


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
# pmap() trabaja con más de dos elementos a iterar (podemos pensar en tríos, por ejemplo)
# tienen que tener la misma longitud, debe haber correspondencia entre elemento1-elemento2-elemento3-etc.
# no importa qué iteras pero presupone correspondencia para saber cómo asignar o hacer el match de las tríadas

# usemos la función media_sumar_dividir que tiene tres argumentos
media_sumar_dividir <- function(vector, valor_suma, valor_division) {
  (mean(vector) + valor_suma) / valor_division
}
# primero estima la media del argumento 1
# segundo le suma el valor del argumento 2
# tercero lo divide por el valor del argumento 3
# hay correspondencia en el sentido de que a cada argumento 1 le corresponde un argumento 2 y un argumento 3
lista_vectores <- list(1:3, 1:5, 2:5) # longitud = 3
valores_suma <- c(2, 3, 8) # longitud = 3
valores_division <- c(2, 1, 9) # longitud = 3

pmap_dbl(list(lista_vectores, valores_suma, valores_division), media_sumar_dividir) #_dbl entrega output en formato numérico
# como no necesitamos más argumentos podemos escribir sin colita de chancho ni paréntesis

data.frame(ola=pmap_dbl(list(lista_vectores,
                             valores_suma,
                             valores_division),
                        media_sumar_dividir)) %>%
  print()
# 02. A veces interesando los efectos colaterales ------------------------
### walk puede resultar más cómodo en algunos contextos
### funciona igual que map solo que no da side-effects (mensajes, etc.)

animales <- c("perro", "gato", "elefante")

### con map
map(animales, # vector 
    print # función
    )

### el output de map es siempre una lista, todo lo demás es colateral. Walk nos entrega el efecto colateral
walk(animales, print)
### el output de walk siempre es una lista también

walk(animales,print) # walk no tiene las versiones de walk_df, walk_dbl, etc., siempre devuelve una lista
walk(animales,print) %>% unlist() %>% pull()
# 03. Usos de walk --------------------------------------------------------

continentes <- split(gapminder, gapminder$continent)

library(feather)
folder_ejercicio_output <- folder_project
files <- paste0(folder_ejercicio_output,
                "/", 
                names(continentes), 
                ".feather")



walk2(continentes, # lista de continentes
      files, # rutas de archivos 
      write_feather # guardar el data frame
      )


# 04. Iteración sobre los nombres de una lista ----------------------------

files <- list.files(paste0(folder_data2,"/datos_ene/"), full.names = T)
trimestres <- list.files(paste0(folder_data2,"/datos_ene/")) %>% 
  str_extract(pattern = "-[[:digit:]]{2}-") %>% 
  str_remove_all("-")

varios_ene <- map(files, ~read_csv2(.x, guess_max = 80000)) # varios_ene tiene las BBDD en una lista

names(varios_ene) <- paste0("trimestre_", trimestres) # ponemos nombres a cada trimestre 

nombres_lista <-  imap(varios_ene, ~.y) # iterar varios_ene y obtener su nombre (para eso es imap())
# el output también es una lista
nombres_lista 

# creamos una columna con el nombre del trimestre en la base de cada trimestre
imap(varios_ene, ~.x %>% 
       mutate(trimestre = .y) %>%
       select(1:3, trimestre))
# revisar el ejemplo luego

list.files(paste0(folder_data2,"/datos_ene"), full.names = T) %>% 
  set_names(paste0("trimestre_", str_extract(., "(?<=-)[[:digit:]]{2}(?=-)"))) %>% 
  imap(~read_csv2(.x, guess_max = 80000) %>% 
         mutate(trimestre = .y)) 
# tarea para el martes

# ver en ppt ejemplo for loops de lo que se espera que hagamos y la idea es que lo usemos con el loop de tidyverse que elijamos
# adaptar a contexto purrr()







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