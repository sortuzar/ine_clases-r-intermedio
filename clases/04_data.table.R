rm(list=ls())


# Sesión 04. data.table ---------------------------------------------------


# Cargar paquetes ---------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               data.table,
               tictoc)


# Importar y exportar datos con data.table --------------------------------

# podemos comparar el tiempo de ejecución entre read.csv y fread()
tictoc::tic()
censo_viviendas <- readr::read_csv("data/csv-viviendas-censo-2017/Microdato_Censo2017-Viviendas.csv")
tictoc::toc()
# 66.472 sec elapsed
tictoc::tic()
censo_viviendas <- data.table::fread("data/csv-viviendas-censo-2017/Microdato_Censo2017-Viviendas.csv")
tictoc::toc()
# 2.968 sec elapsed
## estandarizamos los nombres de las variables
censo_viviendas <- janitor::clean_names(censo_viviendas)

#Cargar datos de personas
dt_censo = data.table::fread("data/csv-personas-censo-2017/Microdato_Censo2017-Personas.csv")
dt_censo <- janitor::clean_names(dt_censo)

#Cargar datos de hogares
#dt_censo = data.table::fread("data/csv-hogares-censo-2017/Microdato_Censo2017-Hogares.csv")

tictoc::tic()
readr::write_csv(censo_viviendas,"data/csv-viviendas-censo-2017/ejemplo_viviendas.csv")
tictoc::toc()
# 99.184 sec elapsed
tictoc::tic()
data.table::fwrite(censo_viviendas,"data/csv-viviendas-censo-2017/ejemplo_viviendas.csv")
tictoc::toc()
# 1.17 sec elapsed

#Argumento append=TRUE de fwirte() permite grabar el archivo sin abrirlo, 
#pegando filas adicionales



# Utilizando data.table ---------------------------------------------------
#Sintaxis base: data.table[i,j, by]

# Convertir df en data.table ----------------------------------------------

dt_viviendas <- data.table(censo_viviendas)   
# o
dt_viviendas <- as.data.table(censo_viviendas)
# o 
setDT(censo_viviendas)


# Trabajar por filas ------------------------------------------------------

#Seleccionar por indexación
dt_viviendas[1:6,]
dt_viviendas[1:6]

#Filtrar por valores, como con dplyr::filter()
dt_viviendas[region == 13]
dt_viviendas[region == 13 & area == 1]

#Utilizando order() para ordenar filas por variables
dt_viviendas[order(region),]


# Trabajar por columnas ---------------------------------------------------

dt_viviendas[, "region"]
dt_viviendas[, c("region", "area")]


# Funciones para resumir datos --------------------------------------------

dt_viviendas[, sum(cant_hog)]
dt_viviendas[, mean(cant_hog)]

# .() funciona como list()
dt_viviendas[,.(mean(cant_hog),sum(cant_hog))]
dt_viviendas[,.(promedio_hogares = mean(cant_hog),suma_hogares = sum(cant_hog))]


# Crear nuevas variables con := -------------------------------------------
dt_viviendas[,hacinamiento := p04/cant_per]
dt_viviendas[,c("p04","cant_per","hacinamiento")]

# fifelse() como ifelse()
dt_viviendas[,hacinamiento := fifelse(p04 != 98,p04/cant_per,NA_real_,na= NA)]
dt_viviendas[,c("p04","cant_per","hacinamiento")]

# fcase() como case_when()
tictoc::tic()
dt_censo[, residencia_habitual := fcase(p10 == 1, "En esta vivienda",
                                        p10 == 2, "En otra vivienda",
                                        p10 == 3, "En otra comuna",
                                        p10 == 4, "En otro país",
                                        default = "missing"
)][]
tictoc::toc()
# 0.36 sec elapsed
dt_censo[, residencia_habitual :=NULL]
tictoc::tic()
dt_censo %>% 
  mutate(residencia_habitual = case_when(p10 == 1 ~"En esta vivienda",
                                         p10 == 2 ~"En otra vivienda",
                                         p10 == 3 ~"En otra comuna",
                                         p10 == 4 ~"En otro país"
  ))
tictoc::toc()
# 2.33 sec elapsed



# Estimaciones agrupadas --------------------------------------------------

#Promedios por comuna
dt_viviendas[,mean(cant_per),by = comuna]

#Totales
dt_viviendas[,.N,by = .(area, comuna)]

#Subgrupos por especie con .SD (subset of data.table)
dt_iris <- data.table(iris)

#Primera fila de cada especie
dt_iris[,.SD[1],by=Species]

#Última fila de cada especie
dt_iris[,.SD[.N],by=Species]

#Fila con pétalo más corto por especie
dt_iris[,.SD[which.min(Petal.Length)],by=Species]

#Es similar a esto: 
iris %>%
  group_by(Species) %>%
  arrange(Petal.Length) %>%
  slice(1)

# extraemos la primera fila de cada especie, no es algo tan novedoso
dt_iris[,.SD[1:3],.SDcols=1:3]

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
dt_iris[,purrr::map(.SD,mean),by = Species]
do<- Sys.time()
do -un

# Parámetro keyby= para ordenar el resultado

dt_viviendas[,.N, by = region]
dt_viviendas[,.N, keyby = region]

#Agrupando por más de una variable
dt_viviendas[,.N,by=p01:p03c]



# Primer ejercicio --------------------------------------------------------

#Calcular media de personas por regiones y comunas






















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
