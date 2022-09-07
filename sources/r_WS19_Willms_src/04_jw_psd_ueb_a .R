# J. Willms, November 2019
# Programmierung für statistische Datenanalyse
#
# Quelltexte: Hinweise und Lösungen zu den Übungen zu Kapitel 4


########## Aufgabe 1 ######
# Was wird zurückgegeben?
ifelse(1:8%%2 == 0, letters, LETTERS)

# Vergleiche:
if(1:8%%2 == 0) letters else LETTERS


########## Aufgabe 2 ######
# Was wird ausgegeben?
set.seed(1)
x <- sample(1:9, 10,TRUE)
x
order(x)
x[order(x)]
sort(x)

?order
?sample
#--b)	Gibt eine Permutation zurück, so dass x[order(x)] sortiert ist

#--c)	
# Ist für einen Vektor x der Aufruf  x[order(x)] identisch zu sort(x)?
# JA


########## Aufgabe 3 ######
(d <- sqrt(1:8 - 4))
(d <- d[!is.na(d)])
# NAs werden entfernt 

########## Aufgabe 4: Lauflänge ######
# Dreizeiler

llaenge <- function(x)
{
#  if (!is.vector(x) && !is.list(x)) 
#   stop("'x' muss ein atomarer Vektor sein")
  n <- length(x)
  v <- x[-1L] != x[-n]
  diff(c(0L, which(v) ,n))
}

# Erste Tests
x <- c(0,0,0,1,1,0,0,0,0,1,0,1,1) 
llaenge(x)
x <- c(1,1,1,1,0,0) 
llaenge(x)
x <- c(0,0,0,0,1,1) 
llaenge(x)
x <- c("a","a","a","a","a","a","b","b","b","b","a") 
llaenge(x)
x <- c("rot", "rot", "blau") 
llaenge(x)


########## Aufgabe 5: Entropie Funktion ######
# Hinweis >> Setze:  0 * log(0) := 0

# Precondition: p Wahrscheinlichkeitsvektor
entropie1 <- function(p)
{
  -sum(ifelse(p > 0, p * log2(p), 0))
}

entropie2 <- function(p)
{
  p <- p[p>0] # normalisieren 
  -sum(p*log2(p))
}

# Alternativ:
entropie2b <- function(p)
{
  p <- p[p!=0] # normalisieren 
  -sum(p*log2(p))
}

# b)
p <-  c(0.25, 0.25, 0.25, 0.25)
(entropie1(p))
(entropie2(p))
(entropie2b(p))
p <-  c(0.25, 0, 0.25, 0.25, 0.25)
(entropie1(p))
(entropie2(p))
(entropie2b(p))
p <-  c(0.2, 0.2, 0.2, 0.2, 0.2)
(entropie1(p))
(entropie2(p))
(entropie2b(p))
p <-  c(0.04, 0.02, 0.04, 0.9 )
(entropie1(p))
(entropie2(p))
(entropie2b(p))
 
p <-  c(0, 1,0, 0 )
(entropie1(p))
(entropie2(p))
(entropie2b(p))


testEntropie<- function(anzahl = 16L, maxLaenge = 12) {
  for(i in 1:anzahl){ 
    n <- sample(0:maxLaenge,1)
    x <- sample(c(0L,1L),n,TRUE) 
    summe <- sum(x)
  
    if(summe > 0) {
      p <-   x/sum(x)
      h <- entropie1(p)
      if(all.equal(h, log2(n))==TRUE) {
        print("Gleichverteilung:")
        print(p)
        print(h)
        print("------------------------")
      }  
      if(h < 0) {
        print(p)
        print(h)
        stop("Fehler: h negativ")
      }   
      if(h > log2(n)) { 
        print(p)
        print(h)
        stop("Fehler: h zu gross") 
      }   
    }
  }  
}


set.seed(1)
testEntropie()
testEntropie(10000,100)


########## Aufgabe 6: Komplexe Vektoren  ######
#--- (a)
2^1:16
2^(1:16)
(0+1i)^(1:16) 
exp(2*pi*1i/16)^(1:16)

#--- (b)
complex(argument = 2*pi/16)
exp(2*pi*1i/16)
u <- complex(argument = 2*pi/16)
v <- exp(2*pi*1i/16)
u-v

identical(u,v)
u==v
# Nein

#--- (c)
z <- exp(2*pi*1i/16)^(1:16)
z
z^16
identical(z^16, 1)
all.equal(z^16, 1)
all.equal(z^16, as.complex(1))
all.equal(z^16, rep(as.complex(1),16))

# Bemerkung: 
z
Re(z)   # Realteil
Im(z)   # Imaginärteil


########## Aufgabe 7: Vektorindizierung  ######
#--- (a)
4+T
4+F
T+T

#--- (b)
(x <- (1:30*13)%%7)

sum(x==2)
length(x[x==2])
length(which(x == 2))
# identisch, falls keine NA-Werte beteiligt:
(x <- c(1/(-5:5), NA,2))
x[x==2]
sum(x==2)
length(x[x==2])
length(which(x == 2))

# Vergleiche:
x[x==1]
x==1
which(x == 1)

#--- (c)
a <- 2^(1/2)
x <- a^(1:6)
x
sum(x==2)
length(x[x==2])
length(which(x == 2))
all(x == 2)
any(x == 2)

identical(2,x[2])
2==x[2]
all.equal(2,x[2])

######### Aufgabe 8   ######
(x <- 1:20 *-1)
(x <- -1:-20)
x[-(1:5)]
x[6:10]

######### Aufgabe 9: Vektorindizierung   ######
x <- runif(1000)
#  Mittelwert des dritten Viertels
mean(x[501:750])

# Mittelwert der zweiten Hälfte
mean(x[501:1000])

# Anzahl der Elemente im dritten Viertel von x, die größer als 0.5 sind?
sum(x[501:750] > 0.5)
x[x[501:750] > 0.5]

#--- (c)
y <- x[c(51:100, 301:400)]
y[y >= 0.9]

# Falsch:  
x[x[c(51:100, 301:400)] >= 0.9]
x[c(51:100, 301:400)] >= 0.9

########## Aufgabe 10: abs()  ######
x <- runif(10)-0.5
xKopie <- x
x
x[ x < 0] <- -x[ x < 0]
x
abs(xKopie) 
all(x == abs(xKopie))
x <- xKopie
# kürzer und klarer 
x <- abs(x)

########## Aufgabe 11: table()   ######
x <- (1:1000*131)%%17
str(x)
table(x)
?table
str(table(x))

x[x>=12] <- 20
table(x)
str(table(x))


########## Aufgabe 12: aendern()   ######

# Siehe auch Abschnitt 3.2.3
x <- 1:10
aendern <- function(i){ 
  x[i] <- 99L
}
aendern(1)
x

# LSG
x <- 1:10
aendern <- function(i){ 
  x[i] <- 99L
  return(x)
}
x <- aendern(1)
x

########## Aufgabe 13: auto["VW"]   ######
a <- LETTERS[c(T,F,F)]
b <- letters[c(F,T,F)] 
worte <- paste(a,b, sep = "")
auto <- 1:9
names(auto) <- worte

a
b
worte
auto

# b)
auto["Vw"]    # Wert 8
auto["VW"]   # Wert NA

# c)
paste(a,b)
paste(a,b, sep = "")
paste(a,b, sep = "::")
paste(a,b, sep="::", collapse = ", ")
paste(a,b, sep="", collapse = ", ")
paste(a,b, sep="", collapse = "")
 
# ---- mehr
x <- "eins"
length(x)
nchar(x)
paste("eins", "zwei")
paste0("eins", "zwei", sep = "")
# paste0(..., collapse) is equivalent to paste(..., sep = "", collapse), 
# slightly more efficiently.
 a <- strsplit("eins zwei", split = "")


########## Aufgabe 14: Text  ######
nchar("Data Sciences") 
strn <- paste(LETTERS[1:8], collapse = "")
strn
nchar(strn)
substring(strn,1,1)
substring(strn,1,2)
substring(strn,1,4)
substring(strn,2,4)

?nchar
?substring

########## Aufgabe 15: Schachbrett  ######
# Gesucht int-Vektor der Länge 64 mit den Werten 1,2,3,4,...
# und mit Namen: "a1" "b1" "c1" "d1" ... 

a <- letters[1:8]
(aa <- rep(a,8))
b <- 1:8
(bb <- rep(b,each=8))

(namen <- paste(aa,bb, sep=""))

w <- 1:64
names(w) <- namen 

w
str(w)

########## Aufgabe 16: Schachbrett 2  ######
A <-  matrix(1, nrow=8, ncol = 8)
rownames(A) <- letters[1:8] 
colnames(A) <- as.character(1:8) 
A
str(A)



########## Aufgabe 17: Matrix Namen  ######
A <- matrix(1:6, nrow=2) 
colnames(A) <- c("a", "b", "c")
rownames(A) <- c("X", "Y")

A
A["X", "b"] 
A["Y", "c"]
A["Yc"]
A["Y", ]
A[, "a"]

str(A)
str(A["Y", ])   # vector
str(A[, "a"])   # vector
str(A["Y", , drop=FALSE])

class(A["Y", ])
class(A["Y", , drop=FALSE])

########## Aufgabe 18: zusammenfuehren()   ######
# merge, interleave
zusammenfuehren <- function(x,y){
  A <- matrix(c(x,y), nrow=2, byrow=TRUE)
  #print(A)
  as.vector(A)
}

zusammenfuehren(letters,LETTERS)
a <- letters[1:8]
b <- LETTERS[1:8]
zusammenfuehren(a,b)


########## Aufgabe 19: Löschen / hinzufügen ######
(A <- matrix(1:20, nrow=4)) 
(A[,3])
(A[,-3])
(A <- A[,-3])
(A <- A[-2,])
(A <- rbind(A, 1:4))

(det(A)) 
(t(A))
 

########## Aufgabe 20: Transponierte, Multiplikation ######
(A <- matrix(1:20, nrow=4)) 
(B <- t(A))
A%*%B
B%*%A
A*A

########## Aufgabe 21: Verteilung Determinante ######
n <- 20L
nIter <- 1000L
(d <- rep(NA_real_, nIter)) 

set.seed(1)
for(i in 1:nIter){
  A <- matrix(runif(n*n, min=-2, max=2), nrow=n)
  d[i]=det(A)
}

str(d) 
summary(d)
hist(d, nclass = 100)
boxplot(d)

dabs <- abs(d) 
summary(dabs) 
hist(d, nclass = 100)
boxplot(dabs)

dabs[dabs < 1e6]
dabs[dabs < 1e7]
# a) keine
min(dabs)
dabs[dabs < 1e6]
dabs[dabs < 1e7]
# b) nein
length(unique(d))== length(d)


########### Aufgabe 22:  apply / paste ######

A <- matrix(1:32, nrow=4) 

aus <- function(x){
  print(paste(x, collapse = "->"))
  return(NULL)
}

# a)
apply(A, 1, aus)
apply(A, 2, aus) 

# b)
A
apply(A, 1,summary)
apply(A, 2,summary)

# c)
b <- c(letters, LETTERS)
A
apply(A, 1,function(x) {b[x]})
apply(A, 2,function(x) {b[x]})




 


