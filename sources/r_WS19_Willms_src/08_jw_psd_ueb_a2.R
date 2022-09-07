# J. Willms, November 2019
# Programmierung für statistische Datenanalyse
# Quelltexte: Hinweise und Lösungen zu den Übungen zu Kapitel 8 
# siehe auch https://r4ds.had.co.nz/exploratory-data-analysis.html
# EDA = Exploratory Data Analysis

library(tidyverse)


########## Aufgabe 1 ######
## a) Diskrete Variable: geom_bar()
##    Kontinuierliche Variable: geom_histogram()

## b)
# Bei der Auswertung des Datensatzes diamonds wurden folgende Plots vorgeschlagen
# Was ist falsch oder zumindest fraglich und warum?
 
# Falsch? Zumindest fraglich!   carat ist kontinuierlich
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = carat))

# Falsch! cut ist diskret
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = cut))

## Besser,  da cut: kategorial / diskret / wenige Ausprägungen
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# da carat: kontinuierlich / stetig / "unendliche" viele Ausprägungen
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat))

# Vergleiche:
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat, fill=cut))

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat, fill="blue"))

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), fill="blue")

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat, fill=..count..))



########## Aufgabe 2 ######
###  count() und tally()  

#a)	Sind folgende Funktionsaufrufe identisch? Was bewirken Sie?
# Identisch sind
diamonds %>% tally()
diamonds %>% count()
diamonds %>% summarize(n=n())
tibble(n=nrow(diamonds))
# Zählen: Anzahl der Datensätze  

# b)	Sind folgende Funktionsaufrufe identisch? Was bewirken Sie?
# Zählen: Identisch sind
diamonds %>% group_by(carat) %>% summarize(n=n())
diamonds %>% group_by(carat) %>% tally()
diamonds %>% group_by(carat) %>% count()
# Gruppieren nach der eigentlich kontinuierlichen Variable carat
# Innerhalb der Gruppe zählen


# Zählen: Wie kann man mit count() oder tally() kürzer schreiben
diamonds %>% group_by(carat) %>% summarize(n=n())
# LSG: 
diamonds %>% count(carat)   # automatisches Gruppieren
 
# Summieren: Wie kann man mit count() oder tally() kürzer schreiben
diamonds %>% summarize(n=sum(carat)) 
# LSG: 
diamonds %>% tally(carat)  # kein Gruppieren

#  Summieren: Wie kann man mit count() oder tally() kürzer schreiben 
tibble(n=diamonds %>% select(carat) %>% sum())
# LSG: 
diamonds %>% tally(carat)  # kein Gruppieren



########## Aufgabe 3  ######
# Nur die ersten 8 Observationen aus diamonds
(dd <- diamonds[1:8,])
dd$carat    # Variable carat (Karatgewicht)

# Teilt die Daten in Intervalle der Größe 0.5 ein; gibt einen Faktor zurück
cut_width(dd$carat, 0.5)

# Teilt die Daten in Intervalle der Größe 0.5 ein und zählt;  
# gibt einen Tibble zurück
dd %>% count(cut_width(carat, 0.5))

count(dd, cut_width(carat, 0.5))
# ist Kurzform von
count(dd, cut_width(dd$carat, 0.5))
# und somit identisch zu 
dd %>% count(cut_width(carat, 0.5))



########## Aufgabe 4 ######
## a) Was bewirkt die Funktion ganzOderWeniger()?
ganzOderWeniger <- function(x, delta) {
  return(floor(x+delta) == ceiling(x))
}

## b)	Erklären Sie den folgenden Quelltext und werten Sie 
## die entsprechende Ausgabe aus. Was ist daran auffällig? 
diamonds %>% 
  filter(ganzOderWeniger(carat,0.05)) %>%  
  count(carat)

# äqivalent hierzu:
diamonds %>% 
  filter(ganzOderWeniger(carat,0.05)) %>% 
  group_by(carat) %>%
  summarize(n=n())

## c)	Vergleichen Sie folgende Variante mit Aufgabenteil b)? 
# Gibt es die Auffälligkeiten immer noch?
diamonds %>% 
  filter(ganzOderWeniger(2*carat,0.1)) %>%  
  count(carat) %>% 
  print(n=Inf)

# Lösungen
# a) ganzOderWeniger(x, delta): true <=> x+e ganzahlig für ein e mit 0<= e <= delta
delta <- 0.05
ganzOderWeniger(0.9, delta)
ganzOderWeniger(0.94, delta)
ganzOderWeniger(0.95, delta)
ganzOderWeniger(0.98, delta)
ganzOderWeniger(0.99, delta)
ganzOderWeniger(0.992, delta)
ganzOderWeniger(1, delta)
ganzOderWeniger(1.0001, delta)
ganzOderWeniger(seq(1,2, by=0.1), 0.1)

# b) Karat-Angaben direkt unter einem ganzahligen Wert (wie 0.99 oder 1.98)
#    sind ungewöhnlich selten

# c) wie b), nur dass jetzt statt Vielfache von 1 nun Vielfache von 1/2 betrachtet werden
# Ähnliche Auffälligkeiten gibt es auch bei Vielfachen von 1/2

# Zur Kontrolle
diamonds %>% filter(carat >=0.99, carat <=1) %>%  count(carat)
diamonds %>% filter(carat >=1.99, carat <=2) %>%  count(carat)
diamonds %>% filter(carat >=2.99, carat <=3) %>%  count(carat)
diamonds %>% filter(carat >=3.99, carat <=4) %>%  count(carat)
diamonds %>% filter(carat >=4.99, carat <=5) %>%  count(carat)




 


