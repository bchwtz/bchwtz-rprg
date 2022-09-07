# J. Willms, November 2019
# Programmierung für statistische Datenanalyse
# Quelltexte: Hinweise und Lösungen zu den Übungen zu Kapitel 7 

require(tidyverse)
require(nycflights13)
#?flights 


########## Aufgabe 1 ######
# Welche der drei Funktionsaufrufe sind identisch?
filter(flights, month == 1, day == 1)
filter(flights, month == 1 & day == 1)
filter(flights, month == 1 && day == 1)  

f1 <-filter(flights, month == 1, day == 1)
f2 <-filter(flights, month == 1 & day == 1)
f3 <-filter(flights, month == 1 && day == 1)
identical(f1,f2)
identical(f1,f3)    # Unterschied zwischen && und & beachten!
                    # && ist problematisch mit Vektoren 
 
########## Aufgabe 2 ######
# Gesucht: Alle Flüge, die mehr als 150 Minuten verspätet den Zielflughafen erreichten:
filter(flights, arr_delay > 150)

# Identisch ist
flights %>%  filter(arr_delay > 150)


# Gesucht: Alle Flüge, die mehr als 150 Minuten verspätet den Zielflughafen erreichten,
# obwohl sie pünktlich gestartet waren:
filter(flights, arr_delay > 150, dep_delay <= 0)


# Gesucht: Alle Flüge in den Monaten September, Oktober, November und Dezember, 
# die mehr als 150 Minuten Verspätung den Zielflughafen erreichten:
filter(flights, arr_delay > 150, month >= 9, month <= 12) # oder:
filter(flights, arr_delay > 150, month >= 9 & month <= 12) # oder:
filter(flights, arr_delay > 150, month %in% 9:12)


########## Aufgabe 3 ######
# Sind Verspätungen von mehr als 150 Minuten in den Monaten April bis September 
# deutlich seltener als in den restlichen sechs Monaten des Jahres?
# Nein, fast 50% häufiger 
vSommer <- filter(flights, arr_delay > 150, month >= 4 & month <= 9)
vSommer
vWinter <-filter(flights, arr_delay > 150, !(month >= 4 & month <= 9)) 
vWinter
# oder:
filter(flights, arr_delay > 150, month < 4 | month > 9)

# Was sind die genauen Prozentzahlen
# 4,051 Verspätungen im Sommer: 65.7%
# 2,118 Verspätungen im Winter: 34.3%
(nSommer <- nrow(vSommer))
(nWinter <- nrow(vWinter))
(nAll <- nSommer+nWinter)

round(nSommer/nAll*100,1)
round(nWinter/nAll*100,1)

# CHECK:
identical(nAll, nrow(filter(flights, arr_delay > 150)))

# Alternativ mit Pipes (Ergebnis ist ein Tibble)
flights %>% 
  filter(arr_delay > 150, month >= 4 & month <= 9)  %>% 
  count()
 


########## Aufgabe 4 ######

# Alle Flüge, die mit mehr als 60 Minuten Verspätung gestartet sind, 
# aber mehr als 60 Minuten im Flug aufgeholt haben:
filter(flights, dep_delay > 60, dep_delay - arr_delay > 60)


# Alle Flüge, die mehr als 60 Minuten verspätet gestartet sind, aber mehr als 60 Minuten 
# im Flug aufgeholt haben und nicht verspätet gelandet sind
# Es gibt drei Flüge
filter(flights, dep_delay > 60, dep_delay - arr_delay > 60, arr_delay <= 0)



########## Aufgabe 5 ######
# a) Bei wie vielen Flügen ist die Abflugzeit nicht aufgelistet?
# 8,255
filter(flights, is.na(dep_time))  
# bzw.
filter(flights, is.na(dep_time)) %>%  count()

# b) Bei wie vielen Flügen ist die Verspätung der Abflugzeit nicht aufgelistet?
# 8,255
filter(flights, is.na(dep_delay))

# c) Bei wie vielen Flügen ist die Verspätung der Abflugzeit aufgelistet, 
# aber nicht die Abflugzeit?
# 0
filter(flights, !is.na(arr_delay), is.na(dep_delay))
filter(flights, is.na(dep_delay), !is.na(arr_delay) )
filter(flights, is.na(dep_delay) & !is.na(arr_delay) )
# filter(flights, is.na(dep_delay), arr_delay > -Inf ) 


########## Aufgabe 6 ######

# Was ist der Unterschied zwischen:
arrange(flights, year, month, day)
arrange(flights, day, month, year)
# Unterschiedliche sortiert

f1 <- arrange(flights, year, month, day)
f2 <- arrange(flights, day, month, year)

f1[900,]
f2[900,]


########## Aufgabe 7 ######
# a)	Wie lang ist der längste Flugroute? Von wo nach wo?
flights  %>% summarize(max(distance))

flights %>% select(origin, dest, distance) %>% 
  unique() %>% 
  filter(distance == max(distance) ) 

# Alternativ: Lösung mithilfe vion b)

# b)	Listen Sie sortiert nach Länge alle Flugrouten auf, die länger als 4000 km sind. 
# Ausgegeben werden sollen pro Zeile: Start- und Zielflughafen und die Länge 
# in Meilen und km (Umrechnung: 1.000 Meilen entsprechen 1.609,344 km).  

f <- flights  %>% 
  select(origin, dest, distance) %>% 
  arrange(desc(distance)) %>% 
  unique() %>% 
  mutate(km = round(distance * 1.609344))  %>% 
  filter(km >= 4000)
f
# Alternative Schreibweise:
# (f <- arrange(flights, desc(distance)))
# (f <- select(f, origin, dest, distance))
# (f <-unique(f))
# (f <- mutate(f, km = round(distance * 1.609344, 2) ))
# (f <- filter(f,km >= 4000))

# Wie lang ist die längste Flugroute? Von wo nach wo?
# Von wo nach wo: "JFK" => "HNL"
# Wie lang: 4983 Meilen
k <- 1
f[k,]$origin
f[k,]$dest
f[k,]$distance
 

flights  %>% summarize(max(distance))
flights %>% select(origin, dest, distance) %>% 
  unique() %>% 
  filter(distance == max(distance) ) 
 

########## Aufgabe 8 ######
# Was wird ausgegeben?
select(flights, year, month, year, day, year)
# Spalten werden nicht dupliziert 

# Was wird ausgegeben?
spalten <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, spalten)

# Was wird ausgegeben?
select(flights, contains("DEP"))
# standardmäßig wird nicht zwischen Klein- und Großschreibung unterschieden
 
# Was wird ausgegeben?
flights %>% 
  select(flight, contains("DEP")) %>% 
  head(4)

########## Aufgabe 9 ######
# Die Zeitformate dep_time und sched_dep_time benutzen das Format HHMM oder HMM;
# 1607 entspricht also 16:07 Uhr 
# Erstellen Sie eine Tabelle mit allen Flüge mit den Spalten
#       year, month, day, flight, dep_time, sched_dep_time 
# Dabei sollen dep_time und sched_dep_time eine Darstellung haben, die der 
# Anzahl der Minuten nach Mitternacht entspricht. Schreiben Sie hierfür eine 
# Funktion zeitInMinuten(). Die mögliche Zeitangabe 2400 für Mitternacht soll 
# dabei auf 0 Minuten abgebildet werden 
# (also soll zeitInMinuten(2400) den Wert 0 zurückgeben).

zeitInMinuten <- function(x) {
  (x %/% 100 * 60 + x %% 100) %% 1440
}

flights %>% 
  transmute(year, month, day, flight,
            dep_time = zeitInMinuten(dep_time),
            sched_dep_time = zeitInMinuten(sched_dep_time))
  

########## Aufgabe 10 ######
# Erklären Sie folgende Ausgabe:

flights %>% 
  select(flight, distance) %>%  
  unique() %>%
  group_by(flight)  %>% 
  mutate(dMin = min(distance), dMax = max(distance)) %>%
  mutate(r = min_rank(distance)) %>% 
  filter(dMax - dMin > 2500) %>% 
  arrange(dMax - dMin, distance) %>% 
  print(n = Inf)


# Aufteilen der Abfrage

f <- flights %>% 
  select(flight, distance) %>%  
  unique() %>%
  group_by(flight)  %>% 
  mutate(dMin = min(distance), dMax = max(distance)) %>%
  mutate(r = min_rank(distance)) 

f %>% filter(flight <=10) %>% 
  arrange(flight,distance) %>% 
  print(n=Inf)

# Jede Flugnummer bildet eine Gruppe
# Für jeden Flug innerhalb einer Gruppe gibt r den Rang bzgl. der Länge des jeweiligen Fluges
# dMin und dMax sind die minimalen bzw. maximalen Fluglängen innerhalb einer Gruppe

# für r=1 gilt dMin == distance
nrow(f %>% filter(dMin == distance) %>% filter(r!= 1) ) == 0
nrow(f %>% filter(r== 1) %>% filter(dMin != distance)) == 0 
# dMax == distance gilt für den maximalen r-Wert innerhalb einer Gruppe

# Ausgegeben werden nur Flüge, für deren Gruppe gilt: dMax - dMin > 2500,
# d.h. die Differenz zwischen längsten und kürzesten Flug beträgt 2500,
#
# Die Ausgabe ist aufsteigend sortiert, zuerst nach dMax - dMin
# distance ist zweites Sortierkriterium
# Ausgegeben wird der komplete Datensatz
flights %>% 
  select(flight, distance) %>%  
  unique() %>%
  group_by(flight)  %>% 
  mutate(dMin = min(distance), dMax = max(distance)) %>%
  mutate(r = min_rank(distance)) %>% 
  filter(dMax - dMin > 2500) %>% 
  arrange(dMax - dMin, distance) %>% 
  print(n = Inf)


