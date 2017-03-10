install.packages('tidyr')
library(tidyr)
library(dplyr)

# Gather
my_sample <- function(){
  sample(x = 20:200, 
         size = 26, 
         replace = T)
}

misdatos <- data.frame(region = LETTERS, 
                       age0_14 = my_sample(), 
                       age15_30 = my_sample(),
                       age31_45 = my_sample())

View(misdatos)

misdatos %>%
gather(key = edad, value = frecuencia, -region) %>% 
  arrange(region) %>% View()


demandados <- c('Walmart',
                'Oxxo',
                'Electra',
                'Elmex Superior')
demandados2 <- c('Quien resulte responsable',
                 'Manpower',
                 'Compis de Outsourcing, S.A')
demandados3 <- c('IMSS', 'SAR', 'INFONAVIT', 'Otro compi')

set.seed(140693)
datos_calculadora <- data.frame(id_exp = paste0('2_', 
                                                sample(1:200, 20),
                                                '_2017'),
                                d_1 = sample(demandados, 20, replace = T),
                                d_2 = sample(demandados2, 20, replace = T),
                                d_3 = sample(demandados3, 20, replace = T)) 

View(datos_calculadora)

datos_calculadora %>%
gather(key = tipo_demandado, value = nombre, -id_exp) %>% 
arrange(id_exp) %>% View()

#install.packages('magrittr')

datos_calculadora %>%
gather(key = tipo_demandado, value = nombre, -id_exp) %>% 
group_by(nombre) %>%
summarise(freq = n()) %>% 
arrange(-freq) %>%
View()

# Spread
misdatos_vertical <- data.frame(region = sort(rep(LETTERS[1:10], 3)),
                                type = rep(c('indice_desarrollo',
                                             'indice_marginacion',
                                             'indice_seguridad'), 10),
                                valor = rnorm(30))

misdatos_vertical %>%
  spread(key = type, value = valor) %>% View()

# Separate 
datos_juntos <- data.frame(estado = LETTERS,
                           m_0_14 = my_sample(),
                           m_15_30 = my_sample(),
                           m_31_45 = my_sample(),
                           f_0_14 = my_sample(),
                           f_15_30 = my_sample(),
                           f_31_45 = my_sample())

View(datos_juntos)

datos_juntos %>%
  gather(key = grupo, value = frecuencia, -estado) %>% 
  arrange(estado) %>% View()
    
datos_juntos %>%
  gather(key = grupo, value = frecuencia, -estado) %>% 
  arrange(estado) %>% 
  separate(grupo, into = c('sexo', 'edad'), 
           extra = 'merge') %>%
  View()

## Limpiando texto

# gsub
ejemplo <- c('Pau_Cast',
             'Misael_Rod',
             'Raul_Blan',
             'Ricardo_Oliv',
             'Moni_Zam', 
             'Patricio_Ram',
             'Salome_Ag')

ejemplo %>%
gsub('_', ' ', .)
ejemplo %>%
gsub('[^[:alpha:]]+', ' ', .)
ejemplo_salarios <- c('$500,000.00', '$60,000.00')
ejemplo_salarios %>%
  gsub('[^[:digit:]]', '', .)

ejemplo_salarios %>%
  gsub('\\$|,', '', .)

misdatos <- data.frame(var1 = 1:10, var2 = 10:19)
names(misdatos) <- c('mi var', 'var')

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
