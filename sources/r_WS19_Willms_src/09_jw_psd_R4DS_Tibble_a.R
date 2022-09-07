# J. Willms, Nov 2019
# Modul: Programmierung für statistische Datenanalyse
#
# Quelltexte: R4DS: Exploratory Data Analysis
# siehe auch: https://r4ds.had.co.nz/exploratory-data-analysis.html


library(tidyverse)

as_tibble(iris)

iris %>% View()

# 10.3.2 Subsetting
# To use these in a pipe, you’ll need to use the special placeholder .:

diamonds %>% .$price

diamonds %>% select(price)
