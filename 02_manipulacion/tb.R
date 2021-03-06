library(reshape2)
library(plyr)
library(stringr)
options(stringsAsFactors = FALSE)

## Codigo original: https://github.com/hadley/tidy-data/blob/master/data/tb.r#L27
## Ejemplo de referencia en: http://vita.had.co.nz/papers/tidy-data.pdf

# Load -----------------------------------------------------------------------
raw <- read.csv("tidyr_datasets/tb.csv", na.strings = "")
raw$new_sp <- NULL
raw <- subset(raw, year == 2000)
names(raw)[1] <- "country"

names(raw) <- str_replace(names(raw), "new_sp_", "")
raw$m04 <- NULL
raw$m514 <- NULL
raw$f04 <- NULL
raw$f514 <- NULL

# Melt -----------------------------------------------------------------------

clean <- tidyr::gather(raw, key = column, value = cases, -country, -year)

clean1 <- clean

# Break up variable in to sex and age ----------------------------------------

clean$sex <- str_sub(clean$column, 1, 1)

ages <- c("04" = "0-4", "514" = "5-14", "014" = "0-14", "1524" = "15-24", "2534" = "25-34", "3544" = "35-44", "4554" = "45-54", "5564" = "55-64", "65"= "65+", "u" = NA)

clean$age <- factor(ages[str_sub(clean$column, 2)], levels = ages)

clean <- clean[c("country", "year", "sex", "age", "cases")]

clean2 <- clean
