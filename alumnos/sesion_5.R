mifuncion <- function(x){
  x[is.na(x)] <- 0
  x
}

x <- c(1:20, NA)

mifuncion(x)

library(lubridate)

limpia_fechas <- function(date){
  fecha <- as.Date(as.numeric(date), origin="1899-12-30")
  fecha[grepl("\\/", date)] <- dmy(date[grepl("\\/", date)])
  fecha
}

fechas <- c(17248, '02/02/16')

iris %>%
  summarise_at(vars(starts_with('Petal')), funs(mean, sd)) 

iris %>%
  summarise(Petal.Length_mean = mean(Petal.length))

df %>%
  select(starts_with('nombre_d'), id_exp) %>%
  gather(key = tipo_dem, value = nombre_dem, -id_exp) %>% 
  View()

grepl('IMSS', x) | grepl('INFONAVIT', x) 

reconoce <- function(x){
  ifelse(grepl('IMSS', x) | grepl('INFONAVIT', x), 1, 0)
}

reconoce_caracteres_raros <- function(x){
  sum(grepl('[^0-9]', x))
}

misdatos <- data.frame(var1 = c(LETTERS, 1:10, paste0(letters[1:3], '0')), 
                       var2 = c(1:10, letters, 1:3))

misdatos %>%
  summarise_all(reconoce_caracteres_raros)
  