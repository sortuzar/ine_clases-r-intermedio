# -------------------------------------------------------------------------
# Clase 3: Manejo de strings y regex --------------------------------------
# -------------------------------------------------------------------------

# Limpiar environment -----------------------------------------------------
rm(list=ls())

# 00. Carga de paquetes ---------------------------------------------------
librerias <- c("tidyverse", "guaguas", "readr")

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
folder_here <- paste0(folder_project)

### Bases de datos ########################################################
source(paste0(folder_project,"/aux_dirs_input.R"))


# 01. Cargar datos --------------------------------------------------------

telefonos <- read_csv(file_telefonos)

# 02. Comportamiento de strings en R --------------------------------------

# R va a escapar ciertos caracteres especiales
(mi_string <- "hola, soy una string")
(mi_string <- 'hola, yo también soy una string')

### asignar entre paréntesis se hace la asignación y además imprime el objeto que guardamos

(mi_string <- "quiero incluir "comillas" dentro de una string") # no funciona, R trata comillas como un objeto que no encuentra en el environment

(mi_string <- 'quiero incluir "comillas" dentro de una string') # manera 1: alternar entre comillas simples y comillas dobles
### da lo mismo cuáles comillas van afuera y cuáles adentro, se recomienda estandarizar una manera por orden mental y evitar errores tontos

(mi_string <- "quiero incluir \"comillas\" dentro de una string") # manera 2: backslash \ permite distinguir entre símbolo como símbolo o símbolo como expresión regular (puntos, paréntesis, etc)
print(mi_string)

writeLines(mi_string) # writeLines() muestra cómo se debería ver el texto, lo imprime como expresión

# Existen otros caracteres que al escaparlos hacen otras cosas en el texto
(mi_string <- "quiero que mi string se \ndespliegue en distintas líneas") # imprimir el string incluye los saltos de línea como texto
writeLines(mi_string) # si quiero que ejecute la función del slash-n tengo que ocupar writeLines, parse, etc., algo que entregue la expresión

(mi_string <- "quiero \t agregar \t \"tabs\" \na mi \tstring") #\tabs entrega un espacio más grande dentro del texto
writeLines(mi_string)

ejemplo <- c("\"", "\\") # tengo que usar \ para escapar el \
print(ejemplo)
writeLines(ejemplo) # escapa la comilla en el primer vector y escapa el \ en el segundo vector

# regex permite generalizar la búsqueda de ciertos textos para identificarlos con cierta independencia de maneras particulares en que se ingresaron

# permiten describir patrones en un conjunto de texto

# 04. Equivalencia de stringr con R base -----------------------------------
### ocupamos librería stringr
### stringi es una función diseñada para ser comprehensiva
### stringr toma las funciones más relevantes (49 de 256) y las hace más amigables para el uso cotidiano de R

# str_length para ver el largo de los caracteres
frutas = c("mazana", "naranja", "uva", "platano", "pera", NA)
str_length(frutas)
nchar(frutas)

# str_c para pegar strings
str_c(c("x", "y", "z"), collapse = ", ")
paste(c("x", "y", "z"), collapse = ", ")


# 05. Trabajo con string r ------------------------------------------------

# str_extract para extraer una cadena de texto
str_extract("Paseo Bulnes 418", pattern = "\\d+") # dígitos - todos los dígitos (extensión de 1 o más), 
# refiere a la extensión de la \\d que definí antes

# str_replace para remover o reemplazar cadenas de texto
direcciones <- c("Avenida Libertador Bernardo O'Higgins     10  58" , 
                 "Call.e Morandé 801", 
                 "Calle Paseo Bulnes     2018")

### podemos editar aspectos de estos vectores usando funciones como str_replace
str_replace_all(direcciones, pattern = c("\\s\\d+"), replacement = "")
# reemplaza todos los espacios seguidos por un número por un vacío
# pero no elimina todos los espacios
# eso se debe a que no definimos la longitud del número de espacios que nosotros
# queremos eliminar (no le dijimos 1, 5, 0 o más, etc.)
# solamente definimos espacio

str_replace_all(direcciones, pattern = c("\\s+\\d+"), replacement = "")
# con esto subsanamos ese problema: eliminar espacios que tengan una longitud de 1 o más

# Otra forma de remover
str_extract(direcciones, pattern = "\\D+") # D mayúscula significa distinto de un dígito (distinto de \d)
# no elimino lo que no quiero sino que extraigo lo que sí quiero:
# filtro todo lo que no es un número o más de un número
# extraigo todo aquello que sea distinto de una cadena de uno o más números

# Eliminar palabras especificas
str_replace(direcciones, pattern = "Calle|Avenida", replacement = "")
# identifica las expresiones Calle o Avenida y reemplázalas por nada
# | significa esto o esto otro, incluye ambos casos
# problema: Call.e no lo considera como parte de la expresión que quiero eliminar
# además deja el espacio en blanco

# Como pasaje también
str_replace(direcciones , pattern = "^\\w+\\s", replacement = "")
# ^ se denomina ancla
# voy a buscar ese patrón AL PRINCIPIO DE LA CADENA DE TEXTO
# R se detiene en el primero que hace un match con la condición o patrón que yo especifico
# yo puedo forzar que esa búsqueda se haga desde el inicio de la cadena de texto


# 06. Expresiones regulares -----------------------------------------------

frutas <- c("mazana", "naranja", "uva", "platano", "pera")

# Consultar por cadenas completas: ¿las strings contienen una string completa?
str_match(frutas, pattern = "uva")
# str_match deja con NA aquello que no coincide con el patrón que definí, y si coincide me entrega ese patrón

# Consultar por patrón en cualquier lugar del texto
str_view(frutas, pattern = "an", html = TRUE)

# Rastrear strings con un caracter especial
nombres = c("maria", "mario", "camilo", "camila")

str_detect(nombres, pattern = "mari(o|a)") # busca textos que coincidan con mari_o o mari_a 
nombres[str_detect(nombres, pattern = "mari(o|a)")]
# 07. Anclas --------------------------------------------------------------

# Ancla de inicio si queremos nombres que parten con c
str_detect(nombres, pattern = "^c")

# Ancla de término si queremos nombres que terminen en o
str_detect(nombres, pattern = "o$")
# en vezde usar el gorrito con ancla al inicio del string, ocupo el $ que pone el ancla al final del string


nombres = c("maria", "mario.", "camilo", "camila.")
str_detect(nombres, pattern = "\\.") # detectar los puntos
# el punto es un símbolo que forma parte de regex entonces para usarlo como símbolo lo tengo que escapar

str_view(nombres, ".+m", html = FALSE) # no detecta a mario o maría
str_view(nombres, ".*m", html = FALSE) # si reemplazo el + por un * entiende que es 0 o más, entonces puede estar al inicio
# el + presupone que efectivamente haya ALGO, no considera que no haya NADA
# el * no presupone que haya algo

# EJERCICIO 1 -------------------------------------------------------------
# ejemplo

guaguas %>% 
  filter(str_detect(nombre, pattern = "eta")) %>%
  count(nombre)

# 1 Tabular los nombres solo de las mujeres que tienen un nombre terminado en "o" nacidas el mismo año que tú.

guaguas %>% 
  filter(str_detect(nombre, pattern = "o$") & str_detect(sexo, "^F$") & str_detect(anio, "2001")) %>% 
  count(nombre)

guaguas %>% 
  filter(str_detect(nombre, pattern = "o$") & sexo == "F" & anio == 2001) %>% 
  count(nombre)

# 2 Tabular los nombres solo de los hombres que tienen un nombre terminado en "a" nacidos el mismo año que tú.

guaguas %>% 
  filter(str_detect(nombre, pattern = "a$") & sexo == "M" & anio == 2001) %>% 
  count(nombre)

# 3 Nombres de personas que su nombre termine con "e", con o sin acento, nacidas el mismo año que tú.

guaguas %>% 
  filter(str_detect(nombre, pattern = "e$|é$") & anio == 2001) %>% 
  count(nombre)

guaguas %>% 
  filter(str_detect(nombre, pattern = "(e|é)$") & anio == 2001) %>% 
  count(nombre)

guaguas %>% 
  filter(str_detect(nombre, pattern = "[eé]$") & anio == 2001) %>% 
  count(nombre)

# 4.1 Primero, separa la base guaguas en una lista que contenga un data frame para cada año (anio).

guaguas %>% 
  split(.$anio)

# 4.2 Construye una función que cree dos nuevas variables en un data frame:
extrae_letras <- function(data, variable){
  data %>% 
    mutate(first_letters = str_sub({{variable}}, start = 1, end = 2),
           last_letters = str_sub({{variable}}, start = -2, end = -1))
}

guaguas %>% 
  split(.$anio) %>% 
  map(~extrae_letras(.x, nombre))

# "first_letters", que contenga las primeras 2 letras de cada nombre y 
# "last_letters", que contenga las últimas 2 letras de cada nombre.



str_view(c("hllla como estas?", "Holla", "hola"), pattern = "(?i)ho?l{1,3}a", html = TRUE)
str_view(".,$%_-#", pattern = "[:punct:]", html = TRUE)
str_view("hola", "[^o]", html = TRUE)


# EJERCICIO 2 -------------------------------------------------------------

'Hoooola, mi nombre es Ignacio, soy sociólogo de formación y trabajo en el INE hace ya algunos años... 
Mi teléfono es: +569 87622455 /
Mi correo electrónico del trabajo es <ifaglonij@ine.gob.cl> 
Uno de mis correos personales es: <ignacio.aglonij@gmail.com> 
Como le pasa a mucha gente, el primer correo electrónico que tuve no era muy serio...<gatoflojo@hotmail.cl>  
Mi dirección es Paseo Bulnes 418, piso 1. Mmmm, ¿qué más les puedo contar? 
Estoy haciendo un posgrado (o postgrado, no sé cómo se dice), un master en Tecnologías de la Información en la U de Chile y no doy fe de lo que digo aquí sea verdad... (quizas una que otra cosa sí)
Esto si que es cierto:
Nro tarjeta de crédito: 6753 9870 8768 8976
Fecha de vencimiento: 06/25
Número de seguridad: 896'

# 1 Detecta el saludo "Hola", independiente de lo coloquial que sea (de la cantidad de "o"). Prueba dos formas de regex.
# 2 Capta tanto la palabra "posgrado" como "postgrado". Prueba dos formas de regex.
# 3 Detecta la expresión "Mmmm".
# 4 Detecta el patrón del número de celular completo.
# 5 Valida el patrón de los correos gmail y hotmail.
# 6 Valida el patrón de un correo INE (@ine.gob.cl).
# 7 Detecta un patrón de número de tarjeta de crédito.
# 8 Detecta una fecha de vencimiento de una tarjeta de crédito.


# 01. Otras aplicaciones con regex ----------------------------------------

telefonos %>% 
  count(ciudad)

# Podemos capturar observaciones que se escriben de distinta forma
telefonos %>% 
  filter(str_detect(ciudad, "quilpu(e|é)"))

# ¿Cómo capturar las menciones de valparaiso?
telefonos %>% 
  filter(str_detect(ciudad, "(?i)valpara[i|í]so")) %>% 
  count(ciudad)

# y La Serena
telefonos %>% 
  filter(str_detect(ciudad, "Serena")) %>% 
  count(ciudad)

# EJERCICIO 3 -------------------------------------------------------------

#¿Cómo lo harían para homologar la ciudad en una variable limpia?
telefonos %>% 
  mutate(ciudad_limpia = case_when(str_detect(ciudad, "(?i)^quilpu[eé]") ~ "Quilpué",
                                   str_detect(ciudad, "(?i)^valpara[ií]so|valpo$") ~ "Valparaíso",
                                   str_detect(ciudad, "(?i)^Serena") ~ "La Serena",
                                   TRUE ~ ciudad))

# Como pueden observar, los teléfonos no están en un solo formato.
telefonos %>% 
  mutate(telefono_limpio = str_replace_all(numero_telefonico, "\\D", 
                                           replacement = ""),
         telefono_limpio = case_when(nchar(telefono_limpio) != 9 ~ str_replace(telefono_limpio, "^56",
                                                                               replacement = ""),
                                     TRUE ~ telefono_limpio))

telefonos %>% 
  select(matches("telef|ciud"))

# Podemos usar las herramientas aprendidas para homologar el formato, 
# eliminando los números que son estandar en una nueva variable fono_clean.
