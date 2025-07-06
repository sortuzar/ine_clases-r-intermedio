
# Clase 01: funciones con dplyr -------------------------------------------

# Limpiar environment -----------------------------------------------------
rm(list=ls())

# Cargar paquetes ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               haven,
               tictoc, # permite medir el tiempo de una operación
               feather # 
               )

# Fijar directorios -------------------------------------------------------
### Ubicación del proyecto ################################################
folder_project <- rprojroot::find_rstudio_root_file()
folder_here <- paste0(folder_project)

### Bases de datos ########################################################
source(paste0(folder_project,"/aux_dirs_input.R"))

# Cargar datos ------------------------------------------------------------
casen = read_feather(file_casen)

demon <- read_csv(file_demon) %>% 
  janitor::clean_names()

categorias <- read_csv(file_categorias)

# rowwise() ---------------------------------------------------------------

### Martes 25 de junio de 2024
### Clase 1
mtcars %>% 
  group_by(gear) %>% 
  summarise(media = mean(hp))

# R por defecto trabaja de manera vectorizada;
# la función rowwise() nos permite trabajar creando una "variable" para la fila en vez de una constante
mtcars2 <- mtcars %>% 
  rowwise() %>% 
  mutate(suma = sum(cyl, disp, hp,drat)) %>% 
  ungroup()

mtcars2 %>% select(cyl, disp, hp,drat, suma)

# pero rowwise() puede ser extremadamente lento
#casen <- casen %>% 
#  rowwise() %>% 
#  mutate(suma = sum(c_across(starts_with("yta")))) # funciona extremadamente lento
# si escribo class(casen) dirá "rowwise_df"

# c_across puede ser lento, sumado a rowwise se puede hacer muy lento
mtcars2 <- mtcars %>% 
  rowwise() %>% 
  mutate(suma = sum(c_across(where(is.numeric)))) %>% # se escribe c_across en rowwise(), no así en otras funciones de dplyr
  ungroup() 
# c_across lo podemos usar con where(), starts_with(), ends_with(), c(), etc.

mtcars2 %>% select_if(is.numeric)

df <- tibble(
  x = list(1, 2:3, 4:6)
) # ¿cuál es la longitud de cada elemento dentro de la lista?

df %>% 
  mutate(largo = length(x)) # a nivel del data frame, pega información del vector x entero en cada fila

df %>% 
  mutate(largo = map_int(x, length))
# map_int me permite iterar
# entrega el valor que yo esperaría: el primer elemento tiene 1 objeto, el segundo tiene 2, y el tercero tiene 3

df %>% 
  mutate(largo = sapply(x, length))
# sapply hace lo mismo

df %>% 
  rowwise() %>% 
  mutate(largo = length(x)) # agregar un rowwise hace lo mismo que map_int o sapply
# también entrega el valor que yo esperaría

df <- tribble(
  ~ n, ~ min, ~ max,
  4,     0,     1,
  2,    10,   100,
  3,   100,  1000,
)

df2 <- df %>% 
  mutate(data = runif(n, min, max)) #No crea lo que busco
df2
# runif es una función para simular datos que tiene 3 argumentos:
# - n que quiero
# - valor mínimo
# - valor máximo
# sólo para distribuciones uniformes
# pero: sin rowwise esto me asigna un valor por fila, que no es lo que busco

df2 <- df %>% 
  rowwise() %>% 
  mutate(data = list(runif(n, min, max)))
df2$data[c(1,2,3)]
# ahora cada fila tiene la cantidad de valores que necesito
# (por eso se almacena en una lista)

# rowSums() versus rowwise()
tic()
casen <- casen %>% 
  rowwise() %>% 
  mutate(suma = sum(yautcorh, ytrabajocor, ymonecorh, yaimcorh, ytotcorh, yoprcor)) %>% 
  ungroup()
toc() # 3.09 segundos

tic()
casen <- casen %>% 
  mutate(sum = rowSums(select(., yautcorh, ytrabajocor, ymonecorh, yaimcorh, ytotcorh, yoprcor))) 
toc() # 0.03 segundos
# también existe rowMeans()
# rowwise() se puede combinar con muchas otras funciones

# Ejercicio
### Variables:
### - y2803c
### - ytro
### - y0101c
### - y0701c

### Objetivo: columna que contenga el mayor de estos valores a nivel de hogar
### La idea es ocupar rowwise
### df: casen
ejercicio_rowwise <- casen %>% 
  rowwise() %>% 
  mutate(ejercicio_rowwise=max(sort(na.omit(unique(c(y2803c,
                                                     ytro,
                                                     y0101c,
                                                     y0701c)))),
                               na.rm=TRUE)) %>% 
  ungroup()

ejercicio_rowwise %>% 
  select(folio,ejercicio_rowwise)

## Ejercicio con rowwise() (diapo 12) -------------------------------------

casen <- casen %>%
  rowwise() %>% # para cada persona (fila)
  mutate(max = max(y2803c, ytro, y0101c, y0701c, na.rm = T)) %>% # para cada persona obtenemos el valor más alto
  ungroup() %>% # por consistencia del código se recomienda desagrupar antes de hacer otra cosa
  group_by(folio) %>% # luego, para cada folio, vemos cuál es el valor máximo de las personas que integran el hogar 
  mutate(maximo = max(max)) %>% # creamos otra variable: a todas las personas del hogar entrega el valor del ingreso más alto del hogar
  ungroup() # desagrupar: desactiva rowwise() y ungroup()

casen %>% select(folio, max, maximo, y2803c, ytro, y0101c, y0701c)


# "Loops" en dplyr --------------------------------------------------------

mtcars2 <- mtcars
head(mtcars2)
# for-loops de R base
for (var in names(mtcars)) {
  mtcars2[[var]] <- (mtcars[[var]] - mean(mtcars[[var]])) / (max(mtcars[[var]]) - min(mtcars[[var]]))
}
head(mtcars2)
# forma del código es poco amigable

# forma dplyr de hacer lo mismo
mtcars2 <- mtcars %>% 
  mutate_all(~ (. - mean(.)) / (max(.) - min(.)) ) # variantes de mutate()
head(mtcars2)

mtcars2 <- mtcars %>% 
  mutate_at(vars(mpg, disp), ~ (. - mean(.)) / (max(.) - min(.)) ) # no olvidar colita de chancho
head(mtcars2)

names(mtcars2)
mtcars2 <- mtcars %>% 
  mutate_at(vars(mpg, disp),  list(norm = ~(. - min(.)) / (max(.) - min(.)),
                                   ola = ~(. = 1))) # ahora podemos agregar un nombre: norm, que por defecto viene con sufijo
names(mtcars2)
# para las variables especificadas se crea un var_norm (mpg_norm, disp_norm)
# con el list podemos hacer una variable nueva, pero además otra operación

mtcars2 <- mtcars %>% 
  mutate(across(c("mpg", "disp") , .fns =  list(norm = ~(. - min(.)) / (max(.) - min(.)) )))
mtcars2 %>% 
  select(mpg,mpg_norm,disp,disp_norm)

mtcars2 <- mtcars %>% 
  mutate(across(
    ### Variables
    c("mpg", "disp") ,
    ### Función
    .fns =  list(
      ### Variable 1: normalizar
      norm = ~(. - min(.)) / (max(.) - min(.)),
      ### Variable 2: variable ola
      ola = ~(. = "ola")
      ),
    ### Especificar nombres
    .names = "{.col}_{.fn}"))
names(mtcars2)

mtcars2 %>% 
  select(mpg,mpg_norm,disp,disp_norm) %>% 
  print(n=nrow(x=max(row_number(mtcars2),na.rm=TRUE)))

nrow(mtcars2)
mtcars2 <- mtcars %>% 
  mutate_if(is.numeric,  list(norm =  ~(. - min(.)) / (max(.) - min(.)) ))
names(mtcars2)

# ejercicio mutate across
ejercicio_across <- casen %>%
  ### Agrupar a nivel de hogar
  group_by(folio) %>% 
  ### Crear variables
  mutate(across(
    ### Variables input
    all_of(c("ytotcor",
             "yautcor",
             "ytrabajocor",
             "yotp")),
    ### Función
    .fns =  list(
      ### Variable 1: normalizar
      hog = ~(. = sum(.,na.rm=TRUE))
      ),
    ### Especificar nombres
    .names = "{.col}_{.fn}")) %>% 
  ### Desagrupar
  ungroup()
    
ejercicio_across %>% 
  arrange(folio) %>% # ordenar por folio para confirmar que se repitan las observaciones por hogar
  select(folio,
         ytotcor,
         ytotcor_hog,
         yautcor,
         yautcor_hog,
         ytrabajocor,
         ytrabajocor_hog,
         yotp,
         yotp_hog) %>% 
  print(n=30)

## Ejercicio mutate(across()) (diapo17) ----------------------------------------------

casen <- casen %>% 
  group_by(folio) %>% 
  mutate(across(c("ytotcor", "yautcor", "ytrabajocor", "yotp"),
                .fns = list(hog = ~sum(.))  ,
                .names = "{.col}_{.fn}")) %>% 
  ungroup()

casen %>% 
  select(folio, ytotcor, ytotcor_hog, yautcor, yautcor_hog) %>% 
  slice(1:4)


## Operaciones complejas ---------------------------------------------------

### Jueves 27 de junio de 2024
### Clase 2
### Seguimos con loops en dplyr
### Supongamos que quiero normalizar todas las columnas de mi dataset
### (obviamente columnas que cumplan ciertas características)
### Lo podemos hacer con mutate across como vimos antes
mtcars2 <- mtcars %>% 
  mutate_at(vars(mpg, disp),  list(norm = ~(. - min(.)) / (max(.) - min(.)) )) %>% 
  mutate_at(vars(mpg_norm, disp_norm),  ~(. + mean(.))) %>% 
  mutate_at(vars(mpg_norm, disp_norm),  ~(. / median(.)))

head(mtcars2 %>% select(mpg, mpg_norm, disp, disp_norm))

### Hacer una operación compleja sobre los datos
### Operación que consiste en varias operaciones más pequeñas
### - Normalizar
### - Sumar algo
### - Dividir por algo
### Una opción es hacer una codificación "carretera", que crea varias líneas de código...
### ...encadenadas por pipes
### (ver diapo 19)

do_silly_stuff <- function(x) {
  normalizar <-  (x - min(x)) / (max(x) - min(x))
  norm_media <-  normalizar + mean(normalizar)
  norm_mediana <- norm_media / median(norm_media)
  return(norm_mediana) # nos asegura devolver el objeto final
  # los intermedios quedan en el environment de la función
  # si no los especificamos no nos devolvería nada
}

mtcars2 <- mtcars %>% 
  mutate(across(c("mpg", "disp"),
                .fns = list(norm = ~do_silly_stuff(.))  ))
### Nos ahorramos varias líneas de código si comprimimos todas las operaciones...
### ...en una sola función
### La función recibe un único argumento, x, que corresponde a las variables que ingresemos

head(mtcars2 %>% select(mpg, mpg_norm, disp, disp_norm))

#Alternativamente

mtcars2 <- mtcars %>% 
  mutate(across(c("mpg", "disp"),
                .fns = list(norm = do_silly_stuff  )))


head(mtcars2 %>% select(mpg, mpg_norm, disp, disp_norm))

# Repaso "breve" de funciones ---------------------------------------------

### Componentes de una función:
### - Body (cuerpo)
### - Formals (argumentos)
### - Environment (distinto de "global environment")
sumar_xy <- function(x, y) {
  x + y
}

body(sumar_xy)

formals(sumar_xy)  #argumentos

environment(sumar_xy)

### Probar la función
sumar_xy(x=2,
         y=3)

## TAMBIEN FUNCIONA CON LA FLECHITA
sumar_xy(x<-2,
         y<-3)

wrapper <- function() {
  sumar_xy <- function(x, y) {
    x + y
  }
  return(environment(sumar_xy))  
}

wrapper() # luego, la función no está guardada en el ambiente global

# si quiero ver qué hay dentro de wrapper, puedo ver con ls()
ls(wrapper())

# ejercicio
# en ambiente global creo z = 3
z <- 3
crear_z <- function() {
  # dentro del ambiente de la función creo otro objeto llamado z
  # como están en distintos ambientes no se sobrescriben
  z <- 100
  return(z)
}

crear_z() # devuelve 100, no considera el z de afuera
 
print(z) #Son diferentes

# igual la recomendación es evitar usar el mismo nombre, aunque existan en ambientes distintos

sumar_xy <- function(x, y) { # función con dos argumentos pero tres valores
  x + y + z #z ya estaba definidio
} 

sumar_xy(x=1, 
         y=2) # lo que hace la función es sumar 1 + 2 + z, con z = 3 (valor que toma del ambiente global) 

z <- 1
sumar_xy <- function(x, y) { # también es una función con dos argumentos y tres valores
  z <- 100 # pero si defino z adentro es diferente
  x + y + z
}
sumar_xy(x=1, 
         y=2) # cambia el resultado de la operación

# ¿puedo ver el ambiente de la función?
environment(sumar_xy)

### Ambiente global
z <- 1
sumar_xy <- function(x, y) {
  ### Ambiente de la función 1
  z <- 100
  interna <- function(){
    ### Ambiente de la función 2
    c(x + y + z)
    
    # La función busca el valor de z en el ambiente de la función 2; no lo encuentra
    # Lo busca en el ambiente de la función 1; encuentra z = 100
    # Entonces ejecuta x + y + 100
  }
  interna()
}
sumar_xy(1, 2) # luego el resultado es 1 + 2 + 100

### También se puede hacer algo como
sumar_xy <- function(x, y, z=100) { # así no definimos z dentro del environment
  ### Ambiente de la función 1
  c(x + y + z)
  }
sumar_xy(x=1, 
         y=2) # luego el resultado es 1 + 2 + 100

# hasta ahora hemos visto funciones INTERNAS
# ¿cómo operan las funciones EXTERNAS?

# primero ejecuto la función externa
z <- 1
externa <- function(){
  c(x + y + z)
} 


sumar_xy <- function(x, y) {
  z <- 100
  externa()
}
sumar_xy(1, 2)

formals(externa) # definí los argumentos arriba! por eso no funciona

# ahora sí le agregamos argumentos
z <- 1
externa <- function(x, y){ #agregamos argumentos
  c(x + y + z)
}

sumar_xy <- function(x, y) {
  z <- 100
  externa(x, y) # externa contiene el valor z del ambiente, sería distinto si se creara dentro de sumar_xy
  # (la definí en el ambiente global, por eso R busca el valor de z en el ambiente global)
}
sumar_xy(1, 2)

# creamos una nueva función: sumar xyz
rm(z)
sumar_xyz <- function(x, y, z) {
  x + y
}
sumar_xyz(1, 2, z="qn pa") # esto funciona aunque no asignemos valor a z

# evaluación lazy: R no va a evaluar argumentos que no necesita, aunque estén definidos en la función

z <- 1

sumar_xyz <- function(x, y, z) {
  x + y + z
}
#sumar_xyz(1, 2) # ahora no corre

# pero:
sumar_xyz(x=1,
          y=2,
          z=z) # lo puedo reemplazar acá si no está definido dentro de la función

sumar_xyz <- function(x, y, z = 5) { # podemos asignar un valor por defecto para z
  x + y + z
}
sumar_xyz(1, 2) # así no tengo que definir z siempre sino solo cuando me interesa que z sea distinto al valor por defecto

sumar_xyz(1, 2, 0.4) # sólo cambia cuando le pido algo diferente


# Ejercicio
### Ocupamos los datos de demon slayer y categorías
### Estimamos el IMC de cada personaje y ver en cuál categoría calzan

### dataframe
ejercicio_demon <- demon %>% 
  select(designation,
         height_cm,
         weight_kg)

### Función get_imc()
get_imc <- function(masa # kg
                    ,
                    altura # metros
                    )
  ### Ambiente de la función
  {
  ### Calcular IMC
  imc <- masa/(altura^2) # también se puede escribir como **2
  
  ### Devolver IMC
  return(imc)
}

### Obtener columna de altura en metros
ejercicio_demon <- ejercicio_demon %>% 
  mutate(height_m=height_cm/100)

### Obtener IMC
ejercicio_demon <- ejercicio_demon %>% 
  mutate(imc=get_imc(masa=weight_kg,
                     altura=height_m))

### Visualizar resultados
ejercicio_demon %>% 
  print(n=nrow(.))

rm(get_imc)

### Ahora obtengamos el valor de la categoría de IMC según el número en la columna imc
### Definimos una función llamada get_label()
### Continúa abajo

# Ejercicio funciones 1 (diapo 32) -----------------------------------------------------

get_imc <- function(weight_kg,height_cm){
  imc <- weight_kg/(height_cm/100)**2
  return(imc)
}

get_imc <- function(weight, height) {
  imc <- weight /(height / 100) **2
  return(imc)
}

demon <- demon %>% 
  mutate(imc = get_imc(weight = weight_kg, height = height_cm))

demon %>% select(name, imc)

categorias

get_label <- function(imc) {
  ### Primero evalúa si se cumple esta condición
  if (imc <= 18.5) {
    ### Si se cumple, asigna la etiqueta "bajo peso"
    label <- "bajo peso"
    ### Si no se cumple, evalúa la siguiente condición
  } else if (imc <= 24.9) {
    ### Si se cumple, asigna la etiqueta "peso normal"
    label <- "peso normal"
    ### Su no se cumple, evalúa la siguiente condición
  } else if (imc <= 29.9) {
    ### Si se cumple, asigna la etiqueta sobrepeso
    label <- "sobrepeso"
    ### Si no se cumple, evalúa la siguiente condición
  } else if (imc > 29.9) {
    ### Si se cumple, asigna la etiqueta obesidad
    label <- "obesidad"
  }
  ### La función opera como una "cascada"
  ### La última condición se define de tal manera que abarca todos los casos "residuo"
  
  ### Después de todo lo anterior, entrega el valor de label que corresponda
  return(label)
}

get_label(imc=23)

demon <- demon %>% 
  mutate(imc = get_imc(weight = weight_kg, height = height_cm)) %>% 
  mutate(label = get_label(imc =  imc) ) # no funciona porque la función recibe de un valor a la vez, no un vector de valores

demon <- demon %>% 
  mutate(imc = get_imc(weight = weight_kg, height = height_cm)) %>% 
  rowwise() %>% # ocupando rowwise() entiende que estamos trabajando con el valor de cada fila por sí mismo
  mutate(label = get_label(imc =  imc) ) %>% 
  ungroup() 

demon %>% select(name, imc, label) 

### reestructuro la función para ahorrarme este problema
get_label <- function(imc) {
  ### defino una función para generar la etiqueta dentro de la función get_label
  generate_label <- function(imc) {
    if (imc <= 18.5) {
      label <- "bajo peso"
    } else if (imc <= 24.9) {
      label <- "peso normal"
    } else if (imc <= 29.9) {
      label <- "sobrepeso"
    } else if (imc > 29.9) {
      label <- "obesidad"
    }
    return(label)    
  }
  return(map(imc, generate_label)) # con map genero una etiqueta para cada valor del vector por separado
}

# PERO: el output de una función map() siempre es una lista
# necesito desanidar esos valores
# si no lo hago el output guardado en la columna será un vector con listas de 1 elemento
demon <- demon %>% 
  mutate(imc = get_imc(weight = weight_kg, height = height_cm)) %>% 
  mutate(label = get_label(imc =  imc) )

demon %>% select(name, imc, label) %>% 
  unnest() # desanidar
# igual esto se puede meter adentro de la función, u ocupar una variante de map(), etc.

# Ejercicio funciones 2 (diapo 37) ----------------------------------------

rnorm(10000)

get_sd <- function(x) {
  cuadrados <- (x - mean(x))**2 # distancias respecto a la media
  suma_cuadrados <- sum(cuadrados) # suma de cuadrados
  n <- length(x) # n
  sqrt(suma_cuadrados / (n - 1)) # salida
}

set.seed(123)
vector <- rnorm(n = 10000)
head(vector)
get_sd(vector)


# Funciones de orden superior ---------------------------------------------

### Función vectorizada
vectorized_function <- function(x, func) {
  new_x <-  do_silly_stuff(x)
  out <-  func(new_x)
  return(out)
}
### Podemos integrar en las funciones como argumento otras funciones

vectorized_function(
  ### Argumento x
  x=c(rnorm(10)), 
  ### Argumento func
  func=mean # evalúa la función mean con el argumento new_x que resulta de aplicar do_silly_stuff a lo que viene en X
  )
vectorized_function(c(rnorm(10)), median)

sum_integers <- function(n) {
  total <- 0
  for (i in 1:n) {
    total <- total + i
  }
  return(total)
}

sum_integers(7)


sum_log10 <- function(n) {
  total <- 0
  for (i in 1:n) {
    total <- total + log10(i)
  }
  return(total)
}

sum_log10(5)

sum_power <- function(n) {
  total <- 0
  for (i in 1:n) {
    total <- total + i**2
  }
  return(total)
}

sum_power(3)

#Función genérica
sum_something <- function(n, func) {
  total <- 0
  for (i in 1:n) {
    total <- total + func(i)
  }
  return(total)
}

power2 <- function(x) {x**2}

identity <- function(x) {x}

sum_something(10, log10)

sum_something(4, power2)


# Fábrica de funciones ----------------------------------------------------

factory_root <- function(power_input) {# definir el N de la potencia
  new_power <- function(x) {
    x**(1/power_input)    
  }
  return(new_power)
}
root2 <- factory_root(2) # lo que devuelve ES UNA FUNCION, no un VECTOR
root2(100)
# generamos nuevas funciones a partir de una función

root3 <- factory_root(3)
root3(100)
root4 <- factory_root(4)
root4(100)

# otra opción es hacer una función genérica
general_root <- function(x, power) {x**(1/power)}
# x: valor a transformar
# power: potencia
general_root(100, 2) # entrega 100^(1/2)
general_root(100, 3)
general_root(100, 4)

add_print <- function(func) {
  string_function <-  rlang::as_string(rlang::ensym(func))
  new_function <- function(x) {
    result <- func(x)
    print(paste0("El resultado de ", string_function,  ": ", result ))
    return(result)
  }
  return(new_function)
}

mean_print <- add_print(mean)

mean_print(c(1,2,3))


# Operadores infix --------------------------------------------------------
### operadores básicos
### los operadores son funciones en el fondo
### el operador "+" entrega una suma
### como es una función que puede recibir hartos argumentos, puedo darle la forma de una función

3 + 2
`+`(3, 2) # forma de función

3 * 2
`*`(3, 2) # forma de función

# en python se puede pegar el texto a un número usando +
"perro" + " gato"
3 * " perro" # acá no funciona

`+` <- function(x, y) {
  paste(x , y)
} # puedo modificar la función + para que en vez de sumar valores los pegue
"perro" + "gato"

3 + 2 # PERO AHORA ESTO NO FUNCIONA

rm(`+`) # cuando la borro se restaura lo normal

3 + 2 # SE CANCELA EL TERCER IMPACTO

### Crear en un ambiente particular una función usando el símbolo +
ambiente_esoterico <- rlang::env()
ambiente_esoterico$`+` <- function(x, y) {
  paste(x , y)
}

rlang::eval_tidy(expr("un" + "saludo"), env =  ambiente_esoterico) # con esto puedo llamar a la función del ambiente esotérico

"un" + "saludo" # aquí no funciona porque llama a la función del ambiente global

rlang::eval_tidy(expr(3 + 7), env =  ambiente_esoterico) # no entrega 10 sino un texto
