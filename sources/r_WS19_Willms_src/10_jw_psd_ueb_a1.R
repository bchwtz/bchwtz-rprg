# J. Willms, Dezember 2019
# Programmierung für statistische Datenanalyse
# Quelltexte: Hinweise und Lösungen zu den Übungen 10
# Objektorientierte Programmierung in R, Laufzeitverhalten, three dots ellipsis

library(tidyverse)


########## Aufgabe 1  ######
# Woran erkennt man, dass ein R-Objekt x kein Objekt einer S3 Klasse ist?

# Ist x ein Objekt einer S3 Klasse, dann ist in der Liste, die attributes(x) zurückgibt,
# das Listenelement "class" enthalten (die Umkehrung gilt allerdings nicht; es gibt leider in Base-R 
# keine einfache Methode, mit der herausgefunden werden kann, ob x ein S3-Objekt ist (siehe pryr::otype())

# Im Gegensatz dazu, gibt die Funktion class() die "implizte" Klasse zurück. Auch R-Objekte,
# die keine Objekte einer S3 Klasse sind, gehören zu einer impliziten Klasse.
 
# Beispiele:
x <- 1:4
attributes(x)
class(x)

x <- matrix(1:20,4)
attributes(x)
class(x)
attributes(x)

x<- tibble(x=1:4, y=2*x)
attributes(x)
class(x)

x<- data.frame(x=1:4, y=2*x)
attributes(x)
class(x)
isS4(x)     # testet, ob ein S4-Objekt;  die Funktion isS3(x) gibt es nicht

x <- getClass("MethodDefinition")  # S4-Objekt
attributes(x)
class(x)
isS4(x)

# x <- binseq(c(1,1,-1.0,1))
# attributes(x)
# class(x)


########## Aufgabe 2  ######
# Implementieren Sie eine S3-Klasse binseq, die binäre (nicht-leere) Folgen modelliert. 
# Eine binäre Folge ist eine endliche Folge, die nur aus den Elementen -1 und +1 besteht. 
# Die Klasse soll sich wie folgt verhalten: ...

### a) Implementieren Sie die Funktionen new_binseq(), validate_binseq() und binseq() ####
# Gehen Sie dabei genauso vor wie in Kapitel 13 in „Advanced R“ von 
# Hadley Wickham https://adv-r.hadley.nz/ 1 beschrieben-


## binseq: binäre (nicht-leere) Folgen modelliert. 
## Eine binäre Folge ist eine endliche Folge, die nur aus den 
## Elementen -1 und +1 besteht.

### Constructor  
new_binseq <- function(x) {
  n=length(x)
  structure(
    x, 
    n = n,
    class = "binseq"
  )
}

### Validator  
validate_binseq <- function(x) {
  values <- unclass(x)
  n <- attr(x, "n")
  
  if(n != length(values)) {
    stop("Falscher n-Wert", call. = FALSE)
  }
  
  if(n == 0) {
    stop("n==0", call. = FALSE)
  }
  
  if(!is.integer(x)) {
    stop("Kein Integer-Vektor", call. = FALSE)
  }
  
  if (!all(!is.na(values) & (values == 1   | values == -1))) {
    stop("Alle Elemente dürfen nur den Wert 1 oder -1 haben", call. = FALSE)
  }
  
  return(x)
}

## Helper  
# In anderen OO-Sprachen ist dies der Kontruktor
binseq <- function(x) {
  if(is.logical(x)){
    x <- 2L*x-1L
  } else {
    x <- as.integer(x)
  }
  validate_binseq(new_binseq(x))
}

### Erste Tests ###
### Ausgabe noch fehlerhaft
s <- binseq(c(1,1,1,-1,1,-1,-1))
str(s) 
print(s)
attributes(s)


binseq(c(1,1,-1.0,1))
binseq(c(1,3,-1.0,1))
binseq(double())
binseq(c("a", "b"))
binseq(-1)
binseq(c(1,1,1,0))
binseq(c(T,T,T,F))
identical(binseq(c(T,T,T,F)), binseq(c(1,1,1,-1)))


## b) Erzeugen Sie die Methode print.binseq() für die generische Funktion print() ####
print.binseq <- function(s, ...) {  
  print(paste("Binäre Folge der Länge",length(s) ))
  print(as.integer(s))
}

# Vergleiche:
methods(print)



### Erneuter Test (diesmal mit korrekter Ausgabe)
binseq(c(1,1,-1.0,1))
binseq(c(1,3,-1.0,1))
binseq(double())
binseq(c("a", "b"))
binseq(-1)
binseq(c(1,1,1,0))
binseq(c(T,T,T,F))
identical(binseq(c(T,T,T,F)), binseq(c(1,1,1,-1)))
 

########## Aufgabe 3 ###### 
# a)	Was sind generische Funktionen?
# Eine Funktion kann für Objekte aus verschiedenen S3-Klassen auf jeweils unterschiedliche Implementierungen
# verweisen (siehe UseMethod()): Eine solche Funktion nennt man generisch.
 

# Was ist der Unterschied zwischen einer generischen Funktion und einer Methode?
# Eine Methode ist einer generischen Funktion zugeordnet. In R ist eine Methode nur konzeptionell Teil
# der Klasse bzw. Objekts. Eine Methode ist eine normale Funktion, die folgender Namenskonvention genügt:
#         generischerFunktionsname.Klassenname
#               BEISPIELE: print.binSeq  print.factor  (unschön: print.data.frame)

# Beispiel: print() ist eine generische Funktion.
# Ist x ein Objekt der S3-Klasse binSeq und wird die generische Funktion print(x) aufgerufen, 
# so wird über diesen Aufruf die Methode print.binSeq(x) aufgerufen.

# Was gibt die Funktion methods() zurück? (Zum Beispiel: methods(mean) oder methods(print))
# Die Funktion methods() gibt alle bekannten Methoden einer generischen Funktion zurück.




########## Aufgabe 4 ######

# Aperiodische Korrelation
autokorrelation1 <- function(s) {
  n <- length(s)
  res <- vector(mode = "integer", length = n - 1)
  for (k in 1:(n-1)) {
    res[k] <- as.integer(t(s[1:(n - k)]) %*% s[(k + 1):n])
  }
  return(res)
}


# a)	Modifizieren Sie die Funktion autokorrelation1(), indem Sie die Schleife 
# durch map_int() (aus purrr) ersetzen. Nennen Sie die modifizierte Version autokorrelation2().

require(purrr)
autokorrelation2 <- function(s) { 
  n <- length(s)
  return(map_int(1:(n-1L), ~as.integer(t(s[1:(n - .)]) %*% s[(. + 1L):n])) )
}

randBinSeq <- function(n) {
  return(sample(c(-1L, 1L), n, replace = TRUE))
}

# Erster Test
s <- randBinSeq(10) 
autokorrelation1(s)
autokorrelation2(s)

# Zweiter Test
for(i in 1:10){
  n <- sample(1:100,1)
  s <- randBinSeq(10) 
  kor1 <- autokorrelation1(s)
  kor2 <- autokorrelation2(s)
  print(kor1)
  print(kor2)
  if(!identical(kor1, kor2))
    stop("Unterschiedliche Rückgabewerte")
  print("-------------------")
}

# b) Vergleichen Sie die Laufzeiten der beiden Funktionen mithilfe 
# von system.time(), beispielsweise durch:

s <- randBinSeq(2E4)
system.time(autokorrelation1(s))
system.time(autokorrelation1(s)) 

# c) Erstellen Sie einen weiteren Laufzeittest und interpretieren Sie die Ergebnisse:
require(compiler)
f <- cmpfun(autokorrelation1)
system.time(f(s)) 

# Durch den JIT-Compiler, der in akutellen R-Versionen benutzt wird, scheint es keine
# signifikanten Laufzeitunterschiede zu geben




########## Aufgabe 5 ######
# Erzeugen Sie eine neue generische Funktion autokor() und implementieren Sie für die Klasse 
# binseq (siehe oben) eine entsprechende Methode, die die aperiodische Autokorrelation zurückgibt. 

 
autokor <- function(x) {
  UseMethod("autokor")
} 

# Noch gibt es keine entsprechende Methode
methods(autocor)  # Fehler in methods(autocor) : Objekt 'autocor' nicht gefunden

 
# Aperiodische Korrelation als Methode für die Klasse binSeq
autokor.binseq <- function(s) {
  n <- length(s)
  res <- vector(mode = "integer", length = n - 1)
  for (k in 1:(n-1)) {
    res[k] <- as.integer(t(s[1:(n - k)]) %*% s[(k + 1):n])
  }
  return(res)
}

binseq(1)
methods(autokor)

# Erster Test
s <- randBinSeq(10) 
autokorrelation1(s)
autokor(s)         # Fehler, da s kein Objekt der Klasse binseq und autokor.default() nicht implementiert


# Erzeugt zufällige binäre Folgen der klasse binseq
rand_binseq <- function(n) {
  return(binseq(sample(c(-1L, 1L), n, replace = TRUE)))
}
s <- rand_binseq(10) 
autokorrelation1(s)
autokor(s) 

# Wann ist dieses Vorgehen sinnvoll?
# Wenn es mehr als eine Klasse gibt, die klassenspezifische autokor-Methoden benötigen
 


########## Aufgabe 6 ######

foo1 <- function(x,...) {
  print(x)
  args <- list(...)
  for(a in args){
    print(a)
  }
}

foo1(1)
foo1(1,2,3,4, mean=6)
foo1(1:4,"Neu",1:6, letters[1:8])

# foo1() kann mit beliebig vielen Argumenten aufgerufen werden
# Abgesehen vom ersten Argument, werden im Funktionsrumpf alle Argumente in einer Liste gespeichert.
# Alle Argumente werden ausgeben.


####
foo2 <- function(n, ...) {
  return(rnorm(n, ...))
}

foo2(4)
foo2(4,100)
foo2(4, mean=100)
foo2(4, mean=100, sd=500)
foo2(4, meen=100, sd=500)

# Alle übergebenen Argumente werden der Funktion rnorm übergeben


####
foo3 <- function(x, ...) {
  return(c(mean(x,...), median(x,...)))
}

x <- c(1,2,100,4)
foo3(x)
x <- c(1,2,NA, 100,4)
foo3(x)
foo3(x, na.rm=TRUE)
foo3(x, rm.na=TRUE) # Keine Fehlermeldung! 
# Alle übergebenen Argumente werden weitergegeben.



