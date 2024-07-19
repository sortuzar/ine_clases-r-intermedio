
# Sesión 04. data.table ---------------------------------------------------

# Limpiar environment -----------------------------------------------------
rm(list=ls())

# Cargar paquetes ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               data.table,
               tictoc,
               janitor)

# Cargar directorios ------------------------------------------------------
folder_project <- rprojroot::find_rstudio_root_file()
folder_here <- paste0(folder_project,"/clases")
source(paste0(folder_project,"/auxiliar/aux_dirs_input.R"))

# Existe dtplyr, pero no lo recomendamos 100%, es mejor quedarse en el ambiente
# de un paquete (en este caso, data.table)

# Importar y exportar datos con data.table --------------------------------

# podemos comparar el tiempo de ejecución entre read.csv y fread()
tictoc::tic()
censo_viviendas <- readr::read_csv(file_censo2017viv)
tictoc::toc()
# 19,09 sec elapsed
tictoc::tic()
censo_viviendas <- data.table::fread(file_censo2017viv) #.csv, .txt y .feather
tictoc::toc()
# 7,97 sec elapsed
## estandarizamos los nombres de las variables
censo_viviendas <- janitor::clean_names(censo_viviendas)

help(clean_names)

#Cargar datos de personas
dt_censo = data.table::fread(file_censo2017per)
dt_censo <- janitor::clean_names(dt_censo)

#Cargar datos de hogares
#dt_censo = data.table::fread("data/csv-hogares-censo-2017/Microdato_Censo2017-Hogares.csv")

# Ahora veamos cuánto demora exportar...
tictoc::tic()
readr::write_csv(censo_viviendas,paste0(folder_here,"/ejemplo_viviendas.csv"))
tictoc::toc()
# 1,72 sec elapsed
tictoc::tic()
data.table::fwrite(censo_viviendas,paste0(folder_here,"/ejemplo_viviendas.csv"))
tictoc::toc()
# 0,39 sec elapsed

#Argumento append=TRUE de fwirte() permite grabar el archivo sin abrirlo, 
#pegando filas adicionales
#útil para almacenamientos persistentes/continuos

# al cargar con fread cargas un data frame en formato data.table
# si no, técnicamente tienes que convertirlo a un objeto data.table


# Utilizando data.table ---------------------------------------------------
#Sintaxis base: data.table[i,j, by]

# Convertir df en data.table ----------------------------------------------

dt_viviendas <- data.table(censo_viviendas)   
# o
dt_viviendas <- as.data.table(censo_viviendas)
# o 
setDT(censo_viviendas)
# con esto el objeto queda preparado para que se le apliquen las funciones del paquete

class(censo_viviendas)


# Trabajar por filas ------------------------------------------------------

# Estructura general
#DT[FILAS,COLUMNAS,VARIABLE DE AGRUPACIÓN]
#DT[i,j,by]

# filas: parámetro i

#Seleccionar por indexación
dt_viviendas[1:6,]
dt_viviendas[1:6] # si omitimos la coma data.table entiende que trabajamos por filas

dt_viviendas[c(1:6,99:120,1500:2000),] # podemos seleccionar grupos de filas no continuas

#Filtrar por valores, como con dplyr::filter()
dt_viviendas[region == 13]
dt_viviendas[region == 13 & area == 1]

# También podemos usar el operador %in%
dt_viviendas[region%in%11:13]
dt_viviendas[!region%in%11:13]

#Utilizando order() para ordenar filas por variables
dt_viviendas[order(region),]

# Utilizando order() para ordenar filas por varias variables
dt_viviendas[order(c(region,area,region,dc,nviv)),]

# Trabajar por columnas ---------------------------------------------------
# j: segundo parámetro

# va después de la primera coma, antes de la segunda, pero la segunda es opcional
# funciona como select de dplyr
dt_viviendas[, "region"] # solo una columna con todas las filas
dt_viviendas[, c("region", "area")] # solo dos columnas con todas las filas

# también en j va a la estimación de estadísticos de resumen usando funciones

# Funciones para resumir datos --------------------------------------------

# Estimación de estadísticos de resumen (univariados)
dt_viviendas[, sum(cant_hog,na.rm=TRUE)] # número de hogares en el país
dt_viviendas[, mean(cant_hog)]
# son las funciones nativas de R Base, necesitan las mismas especificaciones que usaríamos en otro entorno

# .() funciona como list()
dt_viviendas[,.(mean(cant_hog),sum(cant_hog))]
dt_viviendas[,.(promedio_hogares = mean(cant_hog),
                suma_hogares = sum(cant_hog))]
# el argumento es .(), funciona de manera análoga a list
# podemos renombrar las columnas creadas!

# veamos en qué consiste el output
ejemplo_output <- dt_viviendas[,.(promedio_hogares = mean(cant_hog),suma_hogares = sum(cant_hog))]
class(ejemplo_output) # entrega un data frame formato data.table

# Crear nuevas variables con := -------------------------------------------
dt_viviendas[,hacinamiento := p04/cant_per] # ahora volvemos al operador morsita :=
dt_viviendas[,c("p04","cant_per","hacinamiento")]
# funciona de forma análoga a mutate
# además la variable hacinamiento quedó en el data frame sin reasignarlo, con el operador morsita esto ya quedó en el data.table viviendas
# además: qué hacemos para evitar dividir por valores 0?
# usaremos una condición if_else de data.table que es fifelse

# fifelse() como ifelse()
dt_viviendas[,hacinamiento := fifelse(p04 != 98, # si y solo si p04 es distinto de 98
                                      p04/cant_per, # calcula p04/cant_per
                                      NA_real_, # de lo contrario imputa NA_real_
                                      na= NA)] # argumento na significa 
dt_viviendas[,c("p04","cant_per","hacinamiento")] # así reescribimos la variable hacinamiento sin tener que reasignarla

# fcase() como case_when()
# para definir condiciones múltiples
tictoc::tic()
dt_censo[, residencia_habitual := fcase(p10 == 1, "En esta vivienda", # no ocupamos colita de chancho
                                        p10 == 2, "En otra vivienda",
                                        p10 == 3, "En otra comuna",
                                        p10 == 4, "En otro país",
                                        default = "missing" # equivalente a TRUE, no cumplimiento de ninguna de las condiciones anteriores
)][] # el corchete vacío hace que se vea en la consola el output del objeto que estamos editando
tictoc::toc()
# 0,6 segundos

unique(dt_censo$residencia_habitual)

dt_censo[,unique(residencia_habitual)] # podemos aplicar unique dentro del data.table


dt_censo[, residencia_habitual :=NULL]
tictoc::tic()
dt_censo %>% 
  mutate(residencia_habitual = case_when(p10 == 1 ~"En esta vivienda",
                                         p10 == 2 ~"En otra vivienda",
                                         p10 == 3 ~"En otra comuna",
                                         p10 == 4 ~"En otro país",
                                         TRUE ~ NA
  ))
tictoc::toc()
# 2,61 segundos

# Estimaciones agrupadas --------------------------------------------------

# Estimaciones que ocupan by

#Promedios por comuna
dt_viviendas[,mean(cant_per),by = comuna]

dt_viviendas[,.(promedio_personas=mean(cant_per)),by = comuna] # especificando nombre output

#Totales
dt_viviendas[,.N,by = .(area, comuna)] # agrupar por más de una columna; .() permite enlistar las columnas

# cuando escribimos .N cuenta la cantidad de filas que hay por cada condición seleccionada (viviendas por cada área y cada comuna)

# también se puede mezclar creación de variables + estimación de valores agrupados según condiciones

#Subgrupos por especie con .SD (subset of data.table)
dt_iris <- data.table(iris)

# Función .SD, similar a across y permite generar subgrupos dentro de un data table

#Primera fila de cada especie
dt_iris[,.SD[1],by=Species] # obtener primera fila de cada especie

#Última fila de cada especie
dt_iris[,.SD[.N],by=Species] # obtener última fila de cada especie

#Fila con pétalo más corto por especie
dt_iris[,.SD[which.min(Petal.Length)],by=Species] # aplicar condiciones según valores de terceras variables

#Es similar a esto: 
iris %>%
  group_by(Species) %>%
  arrange(Petal.Length) %>%
  slice(1)

# extraemos la primera fila de cada especie, no es algo tan novedoso
dt_iris[,.SD[1:3],.SDcols=1:3] # para agrupar por podemos ocupar .SDcols

# extraemos las 3 primeras filas de las variables que tengan la palabra Sepal, es un poco mas novedoso
dt_iris[,.SD[1:3],.SDcols=patterns("Sepal")]

#Estimamos promedio para variables que contienen "Sepal" por especie
dt_iris[,lapply(.SD,mean),.SDcols=patterns("Sepal"), by = Species]

# Podemos sumar funciones con c()
dt_iris[,c(.N,lapply(.SD,mean)),.SDcols=patterns("Sepal"), by = Species]

# Comparando data.table con lapply() y map()
un<- Sys.time()
dt_iris[,lapply(.SD,mean),by = Species]
do<- Sys.time()
do -un

un<- Sys.time()
dt_iris[,purrr::map(.SD,mean),by = Species] # dentro de data.table podemos ocupar funciones de purrr
do<- Sys.time()
do -un

# Parámetro keyby= para ordenar el resultado

dt_viviendas[,.N, by = region]
dt_viviendas[,.N, keyby = region] # lo mismo que by pero ordenando el output de forma creciente

#Agrupando por más de una variable
dt_viviendas[,.N,by=p01:p03c]



# Primer ejercicio --------------------------------------------------------

#Calcular media de personas por regiones y comunas

ejercicio1 <- data.table(censo_viviendas) 

ejercicio1_output <- ejercicio1[,.(promedio_personas=mean(cant_per)),by=.(region,comuna)]




















#Solución: 
dt_viviendas[, mean(cant_per), by = .(region, comuna)]



# Sintaxis avanzada -------------------------------------------------------

#Usando i y j 
dt_viviendas[region==13, mean(cant_hog)]

#Usando i, j y by
dt_viviendas[region == 13, mean(cant_hog), by = area]

#Concatenando acciones con data.table, como con dplyr %>% 

dt_esp <- dt_viviendas[region %in% c(1:10),][,c("area","comuna","cant_per")][,masde_4 := fifelse(cant_per > 4,1,0)]
dt_esp[, .N, by = masde_4]

dt_esp <- dt_viviendas[region %in% c(1:10),
                       ][,c("area","comuna","cant_per")
                         ][,masde_4 := fifelse(cant_per > 4,1,0)]
dt_esp[, .N, by = masde_4]

### como lo hariamos con data.table
tictoc::tic()
dt_censo[,.N,by=comuna][
  ,porc := round(100*N/sum(N),2)
][]
tictoc::toc()

### como lo hariamos con dplyr
tictoc::tic()
dt_censo  %>% 
  group_by(comuna) %>% 
  summarise(N=n()) %>% 
  mutate(porc = round(100*N/sum(N),2))
tictoc::toc()


## Uniendo tablas ----------------------------------------------------------

viv_comuna = dt_viviendas[,.N, by = comuna]
pers_comuna = dt_censo[, .N, by = comuna]

#Argumento on = funciona como dplyr::left_join()
viv_comuna[pers_comuna, on = "comuna"]


# Ejercicio final ---------------------------------------------------------

#Obtener la comuna con mayor poblacion por región

















#Solución

dt_viviendas[,.(personas = sum(cant_per)), by = .(comuna, region)
             ][, .SD[which.max(personas)], by = region]

comunas = read.csv2("data/csv-personas-censo-2017/etiquetas_persona_comuna_15r.csv")

dt_censo[,.(personas = .N),by=.(comuna,region)
         ][,.SD[which.max(personas)],by = region
           ][comunas, on = "comuna==valor", nomatch=NULL]
