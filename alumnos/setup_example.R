instalar <- function(paquete) {
  if (!require(paquete,character.only = TRUE, 
               quietly = TRUE, 
               warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), 
                     dependencies = TRUE, 
                     repos = "http://cran.us.r-project.org")
    library(paquete, 
            character.only = TRUE, 
            quietly = TRUE, 
            warn.conflicts = FALSE)
  }
}

paquetes <- c('dplyr', 'lubridate', 'ggplot2', 'Hmisc', 'RColorBrewer', 'psych', 'knitr', 'vcd',
              'devtools', 'readxl', 'stringr')

lapply(paquetes, instalar)
rm(paquetes, instalar)