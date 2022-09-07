# J. Willms, Nov 2019
# Skript: Programmierung für statistische Datenanalyse
#
# Quelltexte: Kapitel 7  
# siehe auch: https://r4ds.had.co.nz/transform.html


### Voraussetzungen  #######
library(nycflights13) 
library(tidyverse)
?nycflights13
 
str(flights)
flights
?flights


### filter() #######
?filter
filter(flights, month == 1, day == 1) 
filter(flights, month == 11 | month == 12)
filter(flights, month %in% c(11, 12))


### arrange() #######
?arrange
arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))


### select() #######
?select
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
select(flights, starts_with("dep"))
select(flights, ends_with("time"))
select(flights, contains("dep"))
select(flights, time_hour, air_time, everything())


### mutate() #######
(flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time))
mutate(flights_sml, gain = dep_delay - arr_delay, speed = distance / air_time * 60)
mutate(flights_sml, gain = dep_delay - arr_delay, hours = air_time / 60, 
       gain_per_hour = gain / hours)   
transmute(flights, year, gain = dep_delay - arr_delay,hours = air_time / 60,
        gain_per_hour = gain / hours)


### summarize() #######
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))
summarize(flights, delay = mean(dep_delay, na.rm = TRUE), sd = sd(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))


### Pipe-Operator() #######
d <- 1:10
sd(d)

potenz <- function(x,n) return(x^n)
zentriere <- function(x) return(x - mean(x))
mittel <- function(x) return( sum(x)/ (length(x)-1)) # Precondion: length(x) > 1

sqrt(mittel(potenz(zentriere(d),2)))

res <- zentriere(d)
res <- potenz(res,2)
res <- mittel(res)
res <- sqrt(res)
res

require(tidyverse)

d %>% 
  zentriere() %>%
  potenz(2)  %>%
  mittel() %>%
  sqrt()  


### group_by() #######
fluege <- select(flights, month, day, contains("delay"))
fluege <- filter(fluege, !is.na(arr_delay), !is.na(dep_delay))
fluege

fluege <- flights %>%
  select(month, day, contains("delay")) %>%
  filter(!is.na(arr_delay), !is.na(dep_delay))
fluege

fluege_month <- group_by(fluege, month)
fluege_month

summarize(fluege, delay = mean(arr_delay))
summarize(fluege_month, delay = mean(arr_delay))
summarize(ungroup(fluege_month), delay = mean(arr_delay))

fluege_monthDay <- group_by(fluege, month, day)
summarize(fluege_monthDay, delay = mean(dep_delay))

fluege_month 
fluege_month  %>%
  mutate(m_arr_delay = mean(arr_delay))

# Vergleiche:
summarize(fluege_month, delay = mean(arr_delay))

fluege_month  %>%
  filter(mean(arr_delay) > 10)   
 
fluege_month  %>%
  filter(mean(arr_delay) > 10) %>% 
  summarize(count = n())


