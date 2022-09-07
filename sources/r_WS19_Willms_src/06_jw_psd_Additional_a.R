# J. Willms, Nov 2019  

library(ggplot2)
str(mpg)
?mpg


 
# Umrechnung von Miles per Gallon nach Liter pro 100 km
literPer100km <- function(mpg) {
  return(round(235.195 / mpg, digits = 1))
  
}

##   mpg_de: Deutsche Bezeichnungen #####

mpg_de <- mpg
mpg_de$cty <- literPer100km(mpg_de$cty)
mpg_de$hwy <- literPer100km(mpg_de$hwy)

# Name ?bersetzen
names(mpg_de)



names(mpg_de)[1:11] <- c("Hersteller", "Modell", "Hubraum", "Baujahr", 
                         "Zylinder", "Schaltung", "Antrieb",
                         "Stadtverbrauch", "Autobahnverbrauch", "Antriebsart", 
                         "Fahrzeugtyp")

str(mpg_de)

#Alternativ: rename aus   plyr   https://www.rdocumentation.org/packages/plyr/versions/1.8.4/topics/rename
  

## lm wird für jede Farbe gemacht
ggplot(data = mpg_de) + aes(x = Hubraum, y = Autobahnverbrauch, color = Fahrzeugtyp)+ 
  geom_point() +geom_smooth(method=lm)

ggplot(data = mpg_de) + aes(x = Hubraum, y = Autobahnverbrauch, color = Hersteller)+ 
  geom_point() +geom_smooth(method=lm)

