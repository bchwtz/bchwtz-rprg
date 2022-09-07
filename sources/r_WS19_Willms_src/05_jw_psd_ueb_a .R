# J. Willms, November 2019
# Programmierung für statistische Datenanalyse
# Quelltexte: Hinweise und Lösungen zu den Übungen zu Kapitel 5

# ---------------------------------  TEIL 1 ----------------------
########## Aufgabe 1 ######
x <- rnorm(1e4)
h <- hist(x)

str(h)
class(h)    # Objekt der Klasse histogramm

is.list(h)
is.vector(h)

# 6 Listenelemente: breaks counts  density   mids   xname    equidist 
str(unlist(h))
class(unlist(h))
#?hist


########## Aufgabe 2   ######
### VORSICHT: is.vector(as.vector(x)) ist nicht immer wahr
### (leider nicht konsistent, somit verwirrend)
is.vector(as.vector(h))
str(as.vector(h))
 
# Anderes Beispiel 
x <- list(1:4, T)
str(x)
str(as.vector(x))
is.vector(as.vector(x))   # TRUE

class(x) <- "meschede"
str(x)
str(as.vector(x))    # Klassenattribut beibehalten 
is.vector(as.vector(x)) # FALSE; da Klasse "meschede" kein Vektor

?is.vector
?is.list


########## Aufgabe 3  ######
# Mit welcher Anweisung können Sie in der Liste l1 den Buchstaben C 
# durch den Buchstaben U ersetzen?
v <- 1:4
w <- LETTERS[1:8]
l1 <- list(v,T, w[-1])
str(l1)
l1

l1[[3]][2] <- "X"
str(l1)


########## Aufgabe 4   ######

l1 <- list ("1"=4, "3"=8, "5"=12, "7"=16 )
# a)	die Summe der Listenelemente (also 4+8+12+16) bestimmen?
#which R statement will give the sum of all elements in a?
# sum(l1)          # Fehler
# sum(list(l1))    # Fehler
sum(unlist(l1))

# b)	die Summe der Namen der Listenelemente (also 1+3+5+7) bestimmen?
sum(as.numeric(names(l1)))

########## Aufgabe 5  ######
l1  <- list(1:4, 1:100)
length(l1[2]) # falls definiert (also length(l1) >1, dann 1 

## Hinweis:
length(l1[[2]])


########## Aufgabe 6 ######
l1 <- list(10:1, "Data Science",  c("Data Science", "FH SWF"), c(T,T,F))
length(l1)
length(l1[2])   # Falsch, Ergebnis aber richtig!
length(l1[[2]])
length(l1[[3]])
sapply(l1,length)


########## Aufgabe 7  ######
l1 <- list(10:1, "Data Science",  c("Data Science", "FH SWF"), c(T,T,F))
str(l1)
names(l1) <- LETTERS[1:length(l1)]
str(l1)


#          Faktor       ====  
########## Aufgabe 8  ######
set.seed(42)
data <- sample(LETTERS[1:5], 100, replace=TRUE)
f1 <- factor(data, levels= c( "A", "B","C"), ordered = TRUE)
f1

# a) Ersetzen Sie in f1 den Wert B durch X
levels(f1)
str(levels(f1))
levels(f1)[2] <- "X"
f1

# b)  Ersetzen Sie in f1 die Werte A und C durch Y.
levels(f1)
levels(f1)[1] <- "Y"
levels(f1)[3] <- "Y"
f1

# c) c)	Löschen Sie in f1 alle NA-Werte
#f1Old <- f1 
f1 <- f1[!is.na(f1)]
f1
 
##########  Aufgabe 9 ######
# Machen Sie sich mit den Funktionen rev() und unique() vertraut
?rev
?unique

rev(1:10)
x <- c(1:10, 20:3, 4:12)
unique(x)

# a)
# Stets wahr
identical(rev(1:100),100:1)

# b)
# Nicht immer wahr
d <- sample(1:10, 50, replace = TRUE)
identical(sort(unique(d)), 1:10)

teste <- function(sampleLength = 30, nIter = 1000) {
  count <- 0
  for(i in 1:nIter) {
    data <- sample(1:10, sampleLength, replace = TRUE)
    d1 <- sort(unique(data))
    if (length(d1)  < 10) {
      count <- count + 1
    }
  }
  return(count/nIter)
}

# Für sLength=10 ergibt sich folgende Wahrscheinlichkeit
(p <- 1-factorial(9)/10^9)  # == 1 - factorial(10)/10^10  # 0.9996371
# Für sLength >> 20 ist folgende obere Grenze (upper bound) ein gute Abschätzung
# pUBound <- 9*(9/10)^(sLength-1)   # == 10*(9/10)^sLength
set.seed(1)
sLength <- 1e5
for (sLength in seq(10,100,by=10)){
  res <- teste(sLength, 10000) 
  pUBound <-   10*(9/10)^(sLength-1)
  print(sprintf("sampleLength: %d, res = %f, Obere Grenze: %f", sLength, res, pUBound))
  print("---------------")
}

# Für sLength <- 1e5
# [1] "sampleLength: 10, res = 0.999600, Obere Grenze: 3.874205"
# [1] "sampleLength: 20, res = 0.775800, Obere Grenze: 1.350852"
# [1] "sampleLength: 30, res = 0.375500, Obere Grenze: 0.471013"
# [1] "sampleLength: 40, res = 0.140200, Obere Grenze: 0.164232"
# [1] "sampleLength: 50, res = 0.053600, Obere Grenze: 0.057264"
# [1] "sampleLength: 60, res = 0.020700, Obere Grenze: 0.019967"
# [1] "sampleLength: 70, res = 0.005300, Obere Grenze: 0.006962"
# [1] "sampleLength: 80, res = 0.002600, Obere Grenze: 0.002427"
# [1] "sampleLength: 90, res = 0.001300, Obere Grenze: 0.000846"
# [1] "sampleLength: 100, res = 0.000200, Obere Grenze: 0.000295"


# c)
set.seed(42)
data <- sample(LETTERS[1:5], 100, replace=TRUE)
f1 <- factor(data, levels= c( "A", "B", "C"), ordered = TRUE)
f1
rev(f1) 
unique(f1)
 

##########  Aufgabe 10 ######
bezeichnung <- paste("M", 1:6, sep="")
nr <- 1:6
str(bezeichnung)
df <- data.frame(nr, bezeichnung)
df
names(df)

# Gilt das auch für Listen? Antwort: NEIN
l1 <- list(nr, bezeichnung)
l1
names(l1)

# ---------------------------------  TEIL 2 ----------------------
set.seed(1)
anzahl <-  100
Punkte <-  rnorm(anzahl, mean = 65, sd = 20)
Punkte <- as.integer(Punkte)
MatNr <- sample(1E6:1E7-1, anzahl)
names(MatNr) <- sample(c("ET","MB"), anzahl, replace=TRUE)
Bestanden <- F
student <- data.frame(MatNr, Punkte, Bestanden)
student$Stdgang <- names(MatNr)
rm(Punkte, MatNr,Bestanden,anzahl)

head(student)
str(student)


##########  Aufgabe 11 ######
# a) Aus wie viele Variablen und aus wie viele Observationen besteht student?
str(student)   # 100 obs. of  4 variables
length(student[])

# b)	Was wird ausgegeben durch:  
head(student, n=10)
head(student, n=-95)  # head(student, n=5), da length(student[1]) = 100
head(student, n=-93)
tail(student)

# c)	Was wird ausgegeben durch:  
student[1]
student[2]
str(student[1])    # data frame
#vergleiche: 
student[,1]
str(student[,1])

# d)	Geben Sie die erste Zeile von Student aus.
student[1,]
# student[2,]   # 2. Zeile
 
 
# e)	Um die Klausur zu bestehen, reichen 50 Punkte. Ändern Sie student entsprechend!
student$Bestanden <- student$Punkte >= 50
head(student)


##########  Aufgabe 12 ######
## sortieren
# a)	Was geben die Funktionen zurück:
order(student$Punkte)
order(student$MatNr)
order(student$Stdgang) 

# b)	Was wird zurückgegeben:
student[order(student$Punkte), ]
student[order(student$MatNr), ]
student[order(student$Stdgang), ]  

# c)
# Geordnet nach Punkten nur Fachbereich MB
s <- student[order(student$Punkte),]
s <- s[s$Stdgang=="MB",]
head(s, 10)


##########  Aufgabe 13 ######
# Notenschema  
# Note 4: 50–58    Note 3: 59-69    Note 2: 70-80    Note 1: >=81
head(student) 
 
# a)
# Größte Punktzahl ist nicht bekannt, daher wird Inf benutzt
# alternativ: maxP verwenden # maxP <- max(100, max(student$Punkte)) 
notenFaktor <- cut(student$Punkte,  breaks=c(0, 49, 58, 69, 80, Inf))
str(notenFaktor)
levels(notenFaktor)<-5:1
str(notenFaktor)

student$Note <- notenFaktor
student
head(student, n=10)

# b)	Geben Sie nach Punkten sortiert, alle Studierenden aus, 
# die mindestens 48 und höchsten 52 Punkte erhalten haben 

student$Punkte >= 48 & student$Punkte <= 52

s <- student[student$Punkte >= 48 & student$Punkte <= 52, ]
s <- s[order(s$Punkte),]
s

# c)	Geben Sie nach Punkten sortiert, alle Studierenden aus, 
# die mindestens 79 und höchsten 82 Punkte erhalten haben:
s <- student[student$Punkte >= 79 & student$Punkte <= 82, ]
s <- s[order(s$Punkte),]
s

# d)	Für den Notenspiegel erstellen Sie ein entsprechendes Balkendiagramm
# die Ordung umdrehen, damit die Noten in der Reihenfolge 1,2,3,4,5 ausgegeben werden
head(student)
str(notenFaktor)
notenFaktor1 <- factor(notenFaktor,levels <- 1:5 )
str(notenFaktor1)
student$Note <- notenFaktor1
head(student)

barplot(table(student$Note), main = "Notenspiegel", xlab="Noten", ylab="Anzahl")

# e)	Ist summary(student) hilfreich?
summary(student)
# Antwort: ja (mit Einschränkungen)

 