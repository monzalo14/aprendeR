# Instalamos y cargamos los paquetes necesarios:
install.packages('tidyr')
library(tidyr)
library(dplyr)

# Recordatorio: función gather
# Primero, creamos una función para poder reciclar código:
my_sample <- function(){
  sample(x = 20:200, 
         size = 26, 
         replace = T)
}

# Creamos unos datos de distribución de grupos de edad en ciertas regiones
misdatos <- data.frame(region = LETTERS, 
                       age0_14 = my_sample(), 
                       age15_30 = my_sample(),
                       age31_45 = my_sample())

View(misdatos)
# Notemos cómo hay tres variables en estos datos: región, que tiene su propia columna, 
# grupo de edad, que está dispersa en tres columnas distintas, y frecuencia, que ni siquiera tiene una columna.

# Vamos a usar la función gather para colapsar la variable edad en una sola columna, y crear la variable frecuencia:
misdatos %>%
gather(key = edad, value = frecuencia, -region) %>% 
  arrange(region) %>% View()

# Vamos a ver un ejemplo de la tarea. Este se parece mucho a lo que hemos visto en nuestras bases de datos.
# Creamos unos vectores para samplear de demandados:
demandados <- c('Walmart',
                'Oxxo',
                'Electra',
                'Elmex Superior')
demandados2 <- c('Quien resulte responsable',
                 'Manpower',
                 'Compis de Outsourcing, S.A')
demandados3 <- c('IMSS', 'SAR', 'INFONAVIT', 'Otro compi')

# Fijamos la semilla. Esto nos garantiza que, al repetir el ejercicio, R va a tomar la misma muestra pseudoaleatoria:
set.seed(140693)

# Creamos unos datos con un id de expediente y tres demandados, para cada observación.
datos_calculadora <- data.frame(id_exp = paste0('2_', 
                                                sample(1:200, 20),
                                                '_2017'),
                                d_1 = sample(demandados, 20, replace = T),
                                d_2 = sample(demandados2, 20, replace = T),
                                d_3 = sample(demandados3, 20, replace = T)) 

View(datos_calculadora)
# De nuevo, notemos cómo la variable "tipo de demandado" (1, 2 o 3) está dispersa en tres columnas,
# mientras que la variable nombre de demandado no tiene su propia columna. 
# Vamos a usar gather de manera análoga que en el ejemplo anterior. 
# Así, colapsamos los tipos de demandados en una sola columna, y el nombre en la otra.
# Nota cómo la sintáxis es simple: key es la variable en la que vamos a poner los nombres de las columnas que vamos a colapsar
# (en este caso, tipo demandado), y value es la variable en la que vamos a poner los valores que están en las columnas (en este caso, nombre).

datos_calculadora %>%
gather(key = tipo_demandado, value = nombre, -id_exp) %>% 
arrange(id_exp) %>% View()

# Ahora sí, ya tenemos una lista de demandados y ids de expediente. Nuestra unidad observacional son ahora los demandados
# (i.e., lo que me interesa es saber cuánto aparecen los demandados, no los expedientes)
# Si quisiéramos hacer una cuenta de frecuencias, podemos simplemente usar group_by y summarise, como lo hemos hecho antes.

datos_calculadora %>%
gather(key = tipo_demandado, value = nombre, -id_exp) %>% 
group_by(nombre) %>%
summarise(freq = n()) %>% 
arrange(-freq) %>%
View()

# Spread
# Ahora, tenemos otro tipo de problema. 
# Imaginemos que tenemos tres distintas variables mezcladas en una sola columna:

misdatos_vertical <- data.frame(region = sort(rep(LETTERS[1:10], 3)),
                                type = rep(c('indice_desarrollo',
                                             'indice_marginacion',
                                             'indice_seguridad'), 10),
                                valor = rnorm(30))

# Necesitamos separar la columna de type en tres variables distintas, y asignarles el valor de la derecha que les corresponde.
# Para eso, podemos usar la función spread:

misdatos_vertical %>%
  spread(key = type, value = valor) %>% View()

# Notemos que ahora, key significa la columna que va a distribuirse para formar los nombres de las variables,
# y value los valores que esas variables van a tomar.

# Separate 
# Por último, vamos a ver un ejemplo en el que dos o más variables están juntas en una misma columna.
datos_juntos <- data.frame(estado = LETTERS,
                           m_0_14 = my_sample(),
                           m_15_30 = my_sample(),
                           m_31_45 = my_sample(),
                           f_0_14 = my_sample(),
                           f_15_30 = my_sample(),
                           f_31_45 = my_sample())

View(datos_juntos)
# Notemos que los nombres de columnas hablan de dos variables distintas: nombres y grupos de edad, mientras que de nuevo,
# la variable frecuencia no tiene su propia columna.
# Necesitamos separar nombre de grupo de edad. Sin embargo, no queda muy claro cómo sacar la variable frecuencia.
# Nos conviene más colapsar primero todos los nombre_grupoedad, para crear la variable frecuencia, y luego separarlas.

datos_juntos %>%
  gather(key = grupo, value = frecuencia, -estado) %>% 
  arrange(estado) %>% View()

# Ahora sólo nos queda separar los nombre_grupoedad. Para eso, podemos usar la función separate.
datos_juntos %>%
  gather(key = grupo, value = frecuencia, -estado) %>% 
  arrange(estado) %>% 
  separate(grupo, into = c('sexo', 'edad'), 
           extra = 'merge') %>%
  View()

# Pendiente: en la clase preguntaron sobre cómo hacer para separar pero con la segunda separación. 
# Revisando la documentación, parece que no hay manera de hacer esto con separate. Sin embargo, podríamos
# usar la función inversa: unite, para separar primero en tres y luego unir dos de las columnas.
# Así:

datos_juntos %>%
  gather(key = grupo, value = frecuencia, -estado) %>% 
  arrange(estado) %>% 
  separate(grupo, into = c('col1', 'col2', 'col3')) %>%
  unite(col12, col1, col2) %>%
  View()

## Limpiando texto

# gsub
# Esta nos ayuda a sustituir caracteres que no queremos. Un ejemplo sencillo:
ejemplo <- c('Pau_Cast',
             'Misael_Rod',
             'Raul_Blan',
             'Ricardo_Oliv',
             'Moni_Zam', 
             'Patricio_Ram',
             'Salome_Ag')

ejemplo %>%
gsub('_', ' ', .)

# Notemos que gsub funciona muy bien con expresiones regulares.
# En esta, le decimos que tome cualquier caracter que no sea una letra y lo sustituya por un espacio.

ejemplo %>%
gsub('[^[:alpha:]]+', ' ', .)

# Otro ejemplo de expresiones regulares: sustituir todos los caracteres que no sean números
# Recordemos que el signo de ^ es una negación.

ejemplo_salarios <- c('$500,000.00', '$60,000.00')
ejemplo_salarios %>%
  gsub('[^[:digit:]]', '', .)

# Otro ejemplo de gsub: podemos usar | para hacer una conjunción, y decirle entonces a gsub: 
# "Los caracteres que sean "$" o ",", sustitúyelos por un '' (es decir, quítalos). 
# el \\ antes del $ implica que tenemos que "escaparlo". Podemos ver más detalles de eso la próxima clase.

ejemplo_salarios %>%
  gsub('\\$|,', '', .)

# Último ejemplo: podemos meter gsub en una función, para arreglar cosas como los salarios de un solo jalón.
misdatos_2 <- data.frame(var1 = ejemplo_salarios,
                         var2 = 1:2)

mifuncion <- function(x){
  x %>%
  gsub('[^[:digit:]]', '', .) %>%
  as.numeric()/100
}

misdatos_2 %>%
  mutate(var1 = mifuncion(var1)) %>%
  sapply(class)
