# J. Willms, Nov 2019
# Skript: Programmierung für statistische Datenanalyse
#
# Quelltexte: Kapitel 6 

#library(tidyverse)
library(ggplot2)
str(mpg)
?mpg 
 
## Umrechnung von Miles per Gallon (mpg) nach Liter pro 100 km
literPer100km <- function(mpg) {
  return(round(235.195 / mpg, digits = 1))
}

fuel.mpg <- seq(10,40,by=5)
fuel.mpg
literPer100km(fuel.mpg) 
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
qplot(displ, hwy, data = mpg)


###  6.1.1	Beispiel: Ästhetische Eigenschaften (aesthetic) festlegen ###########
### Alle Punkte blau
ggplot(data = mpg) +                          # outside of aes()
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) +                           # inside aes()
  geom_point(mapping = aes(x = displ, y = hwy, color = year))

ggplot(data = mpg) +                         
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

ggplot(data = mpg) +                          
  geom_point(mapping = aes(x = displ, y = hwy, color = 1:234))


### 6.1.2	Beispiel: Ästhetische Eigenschaften (aesthetic) für geom_point() #####
ggplot(data = mpg) +                         
  geom_point(mapping = aes(x = displ, y = hwy), 
             size = 3, stroke = 1.5, shape = 21, fill = "blue", color ="orange") 

