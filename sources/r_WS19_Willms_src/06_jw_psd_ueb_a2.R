# J. Willms, November 2019
# Programmierung für statistische Datenanalyse
# Quelltexte: Hinweise und Lösungen zu den Übungen zu Kapitel 6

library(ggplot2)

########## Aufgabe 1 ######
anzahl <- c(5,10,14,3,7)
noten <- data.frame(Note=as.character(1:5), Anzahl=anzahl)

# FALSCH:  da geom_bar() defaultmäßig mit stat_count() verknüpft ist
ggplot(noten) +
  geom_bar(mapping = aes(x = Note))

# RICHTIG:
ggplot(noten) +
  geom_bar(mapping = aes(x = Note, y = Anzahl), fill="yellow", color = "red",
           stat = "identity") +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") +
  theme_bw()
 
# Alternativ:
ggplot(noten) +
  geom_col(mapping = aes(x = Note, y = Anzahl), fill="yellow", color = "red",
           stat = "identity") +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") +
  theme_bw()


########## Aufgabe 2 ######
n <- 32
x <- y <- c(1:n, 1:n, 1:(n/2), 1:(n/4))
ggplot(data = data.frame(x,y)) + 
  geom_point(mapping = aes(x = x, y = y))

ggplot(data = data.frame(x,y)) + 
  geom_point(mapping = aes(x = x, y = y), position = "jitter")

## Vergleiche auch:
ggplot(data = data.frame(x,y)) + 
  geom_point(mapping = aes(x = x, y = y), alpha=0.3)

ggplot(data = data.frame(x,y)) + 
  geom_point(mapping = aes(x = x, y = y), position = "jitter", alpha=0.3)



########## Aufgabe 3 ######
### Vorgegebene Daten
set.seed(1)
anzahl <-  100
MatNr <- sample(1E6:1E7-1, anzahl)
names(MatNr) <- sample(c("ET","MB", "WIng", "WInfo"), anzahl, replace=TRUE)
student <- data.frame(MatNr)
student$Stdgang <- names(MatNr)
student$Punkte <- as.integer(rnorm(anzahl, mean = 68, sd = 12)) 


head(student,30)
# Wie in den Übungen zu Kapitel 5 
# (kann sicherlich vereinfacht werden)

## Besser (BB)
notenFaktor <- cut(student$Punkte,  breaks=c(0, 49, 58, 69, 80, Inf), labels = 5:1)

## Nicht benötigt:
notenFaktor <- cut(student$Punkte,  breaks=c(0, 49, 58, 69, 80, Inf))
str(notenFaktor)
levels(notenFaktor)<-5:1 

# Kontrolle:
head(student$Punkte,10)
head(notenFaktor,10)

student$Note <- notenFaktor
head(student,10)
str(student)

# Ordnung "tauschen" 
# falsch: levels(student$Note)<-1:5   # aus 1 wird nun 5
# head(student,10)

# richtig:
student$Note <-  factor(student$Note,levels <- 1:5)
head(student,10)
str(student)


# Siehe auch http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
ggplot(data = student) + 
  geom_bar(mapping = aes(x = Note, fill = Stdgang)) +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") +
  scale_fill_brewer(palette="Dark2") +
  theme_bw()

ggplot(data = student) + 
  geom_bar(mapping = aes(x = Note, fill = Stdgang), position = "dodge") +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") +
  scale_fill_brewer(palette="Dark2") +
  theme_bw()

# # Auch interessant:
# ggplot(data = student) +
#   geom_bar(mapping = aes(x = Note, fill = Stdgang), position = position_dodge(width = 0.8)) +
#   labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") +
#   scale_fill_brewer(palette="Dark2") +
#   theme_bw()



########## Aufgabe 4 ######
# Erzeugen Sie mit den Daten der letzten Übungsaufgabe die folgenden vier Grafiken:

## 1. Grafik  ######
# 1. Versuch (fehlerhaft)
ggplot(data = student) + 
  geom_bar(mapping = aes(x = Note)) +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") + 
  facet_wrap(~ Stdgang, nrow = 2) + 
  theme_bw()

# 2. Versuch (immer noch nicht ganz richtig)
ggplot(data = student) + 
  geom_bar(mapping = aes(x = Note, color = Note)) +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") + 
  facet_wrap(~ Stdgang, nrow = 2) + 
  theme_bw()

# 3. Versuch (besser, aber ...)
ggplot(data = student) + 
  geom_bar(mapping = aes(x = Note, fill = Note)) +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") + 
  facet_wrap(~ Stdgang, nrow = 2) + 
  theme_bw()

# richtig: 
ggplot(data = student) + 
  geom_bar(mapping = aes(x = Note, fill = Note)) +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") + 
  facet_wrap(~ Stdgang, nrow = 2) +
  scale_fill_brewer(palette="Dark2") +
  theme_bw()


## 2. Grafik  ######
ggplot(data = student) + 
  geom_bar(mapping = aes(x = Note, fill = Stdgang), position = "dodge") +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") + 
  facet_wrap(~ Note, nrow = 2) +
  scale_fill_brewer(palette="Dark2") +
  theme_bw()


## 3. Grafik  ######
f <- factor(student$Punkte >= 50)
levels(f) <- c("durchgefallen", "bestanden")
student$Bestanden <- f

ggplot(data = student) + 
  geom_bar(mapping = aes(x = Note, fill= Stdgang), position = "dodge") +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") + 
  facet_wrap( Stdgang ~ Bestanden, nrow = 2) +
  scale_fill_brewer(palette="Dark2") +
  theme_bw()

## 4. Grafik  ######
ggplot(data = student) + 
  geom_bar(mapping = aes(x = Note, fill= Stdgang), position = "dodge") +
  labs(title = "Notenspiegel", subtitle = "Klausur DS 2019") + 
  facet_wrap(  Bestanden ~ Stdgang , nrow = 2) +
  scale_fill_brewer(palette="Dark2") +
  theme_bw()



 






 