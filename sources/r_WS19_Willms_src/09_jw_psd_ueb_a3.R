# J. Willms, Dezember 2019
# Programmierung für statistische Datenanalyse
# Quelltexte: Hinweise und Lösungen zu den Übungen 9
# Tidy data, Relational data


library(tidyverse)
library(nycflights13)


########## Aufgabe 1  ######
### Gegeben ist:
zeiten1 <- tibble(
  n = (4:9)*1e3L,
  mess1 = c(3.656, 4.672, 5.719, 6.720, 7.641, 8.750),
  mess2 = c(3.781, 4.735, 5.672, 6.782, 7.673, 8.798),
  mess3 = c(3.719, 4.719, 5.672, 6.688, 7.704, 9.048),
  mess4 = c(3.766, 4.688, 5.625, 6.610, 7.750, 8.735)
)
zeiten1

 

########## LSG
## a)	Wandeln Sie zeiten1 in zeiten2 um (siehe Auflistung rechts).
zeiten2 <- zeiten1 %>% gather(-n, key=Nr, value=zeit)
print(zeiten2, n=Inf)

# Bemerkung: besser wäre vielleicht, den KEY als Faktor zu speichern
# Wieso?
zeiten2 <- zeiten1 %>% gather(-n, key=Nr, value=zeit, factor_key = TRUE)

# b)	Handelt es sich bei zeiten1 bzw. bei zeiten2 um „Tidy Data“ im Sinne von Wickham?
# Wenn man die vier Messungen pro n-Wert als vier Beobachtungen auffasst, 
# dann ist zeiten1 "untidy" und zeiten2 "tidy"

# c)	Mit welcher Anweisung kann aus zeiten2 der ursprüngliche Datensatz zeiten1 erzeugt werden?
zeiten2 %>% spread(key=Nr, value=zeit)


# d)	Berechnen Sie in Abhängigkeit von n die Mittelwerte der Laufzeiten. 
# Gehen Sie zuerst davon aus, dass lediglich zeiten1 gegeben ist, 
# danach gehen Sie davon aus, dass nur zeiten2 gegeben ist.
zeiten1
apply(select(zeiten1,-n),1,mean)
# oder:
zeiten1 %>% select(-n) %>% apply(1,mean)
# Bemerkung: In beiden obigen Fällen ist das Ergebnis kein Tibble

zeiten2
# Leichter, da "tidy"; Ergebnis ist ein Tibble!
zeiten2 %>% group_by(n) %>%     
  summarize(Zeit =mean(zeit))



########## Aufgabe 2 mit NA   ######
### wie Aufgabe 1, nur mit NA
n <- (4:9)*1e3L
mess1 <- c(3.656, NA, 5.719, NA, 7.641, 8.750)
mess2 <- c(3.781, 4.735, NA, 6.782,NA , 8.798)
mess3 <- c(NA, 4.719, 5.672, 6.688, 7.704, 9.048)
mess4 <- c(NA, 4.688, 5.625, 6.610, 7.750, NA) 
mess5 <- c(3.766, 4.672,NA, 6.720, 7.673, 8.735  )
mess6 <- c(3.719, NA, NA, NA, NA, NA) 
zeiten1 <- tibble(n, mess1, mess2, mess3, mess4, mess5, mess6)
zeiten1

# Diesmal ist laut Aufgabebstellung der key-Wert ein Faktor
zeiten2 <- zeiten1 %>% 
  gather(-n, key=Nr, value=zeit, factor_key = TRUE, na.rm = TRUE)
print(zeiten2, n=Inf)

# b)	wie in Aufgabe 1 (nur wird hier aus meiner Sicht noch deutlicher, dass es sich pro
# Zeile um mehrere Beobachtungen handelt)

# c)	Mit welcher Anweisung kann aus zeiten2 der ursprüngliche Datensatz zeiten1 erzeugt werden?

zeiten1 %>% select(-n) %>% apply(1,mean,na.rm=TRUE)

zeiten2 %>% group_by(n) %>%     
  summarize(Zeit =mean(zeit))




########## Aufgabe 3  ######
# Welche Zielflughäfen sind in flights (als dest) aufgelistet, erscheinen aber nicht als FAA-Kennung in airports?
flights
airports
anti_join(flights, airports, by = c("dest" = "faa")) %>% select(dest) %>%  unique() 
# oder:
flights %>% anti_join(airports %>% rename(dest = faa))%>% select(dest) %>%  unique()


########## Aufgabe 4 ###### 
# Geben Sie diejenigen Datensätze aus airports aus, die als Zielflughafen in flights (als dest) vorkommen.
airports %>% rename(dest = faa) %>% semi_join(flights)
# oder
airports %>% semi_join(flights, by = c("faa" = "dest"))



########## Aufgabe 5 ######
# Geben Sie diejenigen Datensätze aus airports aus, als Abflugflughafen in flights (als origin) vorkommen.
airports %>% rename(origin = faa) %>% semi_join(flights)
# oder
airports %>% semi_join(flights, by = c("faa" = "origin"))



########## Aufgabe 6 ######
# Geben Sie Breitengrad und Längengrad der drei Abflughäfen im Datensatz flights aus. 
abflug <- airports %>% rename(origin = faa) %>% semi_join(flights) %>% select(origin, lat, lon)
abflug
# Zum Vergleich und zur Kontrolle die drei Abflughäfen (ohne Breitengrad und Längengrad) anders ermittelt
flights %>% select(origin) %>% unique()

# Unterscheiden sich die Koordinatenangaben deutlich und was sind die entsprechenden Mittelwerte? 
summary(abflug)
# Mean lat  :40.70   Mean lon   :-73.94 
meanLat <- mean(abflug$lat)
meanLon <- mean(abflug$lon)
meanLat  
meanLon  


########## Aufgabe 7 ######
### a)	Nennen Sie den Datensatz flights in den Datensatz flights_withCoord um und fügen 
# Sie aus dem Datensatz airports den Breitengrad (Latitude) und den Längengrad (Longitude) 
# des jeweiligen Zielflughafens in den Datensatz flights ein.  

airport_koordinaten <- airports %>%
  select(faa, lat, lon) %>% 
  rename(Breitengrad = lat) %>% 
  rename(Längengrad = lon)

airport_koordinaten

flights_withCoord<- flights %>%
  left_join(
    airport_koordinaten,
    by = c("dest" = "faa")
  )

flights_withCoord 
flights_withCoord %>% select(-(year:carrier))
flights  %>% select(-(year:carrier))


### b) Wie groß sind die Korrelationen zwischen
# - Breitengrad und Fluglänge
# - Längengrad und Fluglänge

# Korrelationen
cor(flights_withCoord$Breitengrad, flights_withCoord$distance)  # == NA
cor(flights_withCoord$Breitengrad, flights_withCoord$distance, use="complete.obs")
cor(flights_withCoord$Längengrad, flights_withCoord$distance, use="complete.obs")

# Korrelationen ändern sich nicht durch lineare Transformationen:
cor(flights_withCoord$Breitengrad, flights_withCoord$distance/1.6, use="complete.obs")
cor(flights_withCoord$Längengrad, flights_withCoord$distance/1.6, use="complete.obs")



###  c)	Betrachten Sie nun für jeden Flug die Differenz der Längen- und Breitengrade von Abflug- und Zielflughafen. Bestimmen Sie die Wurzel aus der Summe der Quadrate dieser beiden Differenzen 
#  und ermitteln Sie die Korrelation mit der Fluglänge. 
#  Was für einen Wert erwarten Sie?

# Antwort: einen Wert knapp unter +1

# Aus Speichergründen werden keine neuen Variablen für die Koordinaten
# der drei Zielflughäfen eingeführt. Stattdessen werden benannte Vektoren benutzt:
abflug   # siehe oben
laengen <- abflug[["lon"]] 
laengen

# Längengrade: 
names(laengen) <- abflug[["origin"]]
str(laengen)
laengen["JFK"]
#vergleiche:
meanLon

# Breitengrade: 
breiten <- abflug[["lat"]]
names(breiten) <- abflug[["origin"]]
breiten["JFK"]
meanLat

flights_withCoord
flights_withCoord %>%  
  select(origin, Breitengrad, Längengrad, distance)%>% 
  mutate(deltaLat = Breitengrad - meanLat)

# Genauer
flights_withCoord %>%  
  select(origin, Breitengrad, Längengrad, distance)%>% 
  mutate(deltaLat = Breitengrad - breiten[origin])

coord <-  flights_withCoord %>%  
  select(origin,Breitengrad, Längengrad, distance) %>% 
  mutate(deltaLat = Breitengrad - breiten[origin], deltaLon = Längengrad - laengen[origin])

coord


# Hilfsfunktion zur geometrischen Längenberechnung (auf einer Ebene)
laenge <- function(x,y) {
  return(sqrt(x*x+y*y))
}

# Check:
laenge(3,4) 


coord <- coord %>%  mutate(r = laenge(deltaLat, deltaLon))
coord

# Endergebnis:
cor(coord$r, coord$distance, use="complete.obs")

 

########## Aufgabe 8  ######
# Was geben die folgenden Anweisungen aus und wieso?
  

# Bemerkung: Das Ergebnis ist immer eine Liste L
# L(i): Vektor der Länge i mit N(0,1)-verteilten Zufallszahlen
map(1:10, rnorm)

# wie oben
map

# L(i): Vektor der Länge 4 mit N(i,1)-verteilten Zufallszahlen
map(1:10, ~rnorm(4, .))



########## Aufgabe 9 ######
# Berechnen Sie die Mittelwerte aller Spalten in mtcars.
map_dbl(mtcars, mean) 
# Möglich ist auch:
mtcars %>% summarize_all(mean)  # summarize_all() nicht besprochen 
# oder:
mtcars %>% gather(key="key", value="value", factor_key = TRUE) %>% 
  group_by(key) %>% 
  summarize(mean(value))



 
########## Aufgabe 10  ######
# Implementieren Sie mithilfe der Funktionen is.numeric() und map_lgl() eine Funktion
# numerisch(), die als Argument einen Tibble df erwartet und den Tibble zurückgibt, 
# der genau aus  allen numerischen Spalten von df besteht. 
# is.numeric() tests for objects that behave like numbers.
numerisch <- function(df) {
  df[map_lgl(df, is.numeric)]
}

map_lgl(diamonds, is.numeric)
numerisch(diamonds)

# Dann:
cor(numerisch(diamonds), use="complete.obs")
cor(numerisch(mpg), use="complete.obs")



########## Aufgabe 11  ######
# Erklären Sie folgenden Quelltext:
# ...

# Lösche alle Zeilen, die ein NA enthalten, und erzeuge
# eine Spalte "vol", die in grober Näherung dem Volumen entspricht
d <- diamonds %>% drop_na() %>% mutate(vol=x*y*z)

# Führe eine lineare Regression durch;  einfaches Modell: price = a * vol + b
lmd <- lm(price ~ vol, data = d)
sum_lmd <- summary(lmd)

# Bestimmtheitsmaß des Modell
sum_lmd$r.squared
# __________________________


models <- d %>% 
  split(.$cut)
models
# d wird aufgeteilt in Gruppen gemäß dem Faktor cut
#    models <- d %>%  split(.$cut) ist eine Liste mit fünf Elementen; 
#    jedes Listenelement ist ein Teil-Tibble des ursprünglichen Tibbles d.
#    Jede Zeile von d ist in genau einem Teil-Tibble enthalten.

# Danach wir für jeden Teil-Tibble eine lineare Regression durchgeführt: 
models <- d %>% 
  split(.$cut) %>% 
  map(~lm(price ~ vol, data = .))
# Das Ergebnis ist wiederum eine Liste mit fünf Elementen:
# alle fünf Listenelemente sind Objekte der Klasse "lm" (die von der Funktion lm() zurückgegeben wurden)
 

# Um das Bestimmtheitsmaß zu erhalten, muss zuerst mithilfe von map() die summary-Funktion auf jedes 
# Listenelement angewandt werden, was wieder eine Liste mit fünf Elementen ergibt:
lms <- models %>% 
  map(summary)

lms1 <- lms[["Fair"]]
# Jedes dieser fünf Listenelemente ist wiederum eine Liste, die auch das Element $r.squared enthält.
# Mit map_dbl() kann auf diese fünf "Versionen" von $r.squared zugegriffen werden:
models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)


 