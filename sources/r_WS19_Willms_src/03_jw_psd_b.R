# J. Willms, August 2019
# Skript: Programmierung für statistische Datenanalyse
#
# Quelltexte: Kapitel 3

##########: Namen und Objekte:  Ein erster Vergleich mit C und C++   ####
n <- 5
str(n) 
object.size(n) 

n <- c(5,7)
str(n)
typeof(n)
is.vector(n)
length(n)
n <- 5
str(n)
typeof(n)
is.vector(n)
length(n)

x <- 5 
typeof(x)
str(x)
x[1] 
x[2] 

x<-1:1000 
typeof(x)
str(x)

x<-c("eins", "zwei", "drei")
typeof(x)
str(x)

a <- c(1, 5, 3, 2)
b <- a

a <- c(1, 5, 3, 2)
b <- a
b[2]<-0
a
b

rm(b)
b


##########: Funktionen ####
f1 <- function (x,y,z){
  tmp <-  2*y+3*z
  return(x+tmp)
}

f1(1,1,1)
f1(1,2,3)
neuerName <- f1
neuerName(1,2,3)

a <- 1
f1(a,a,a)
a
tmp     # Fehler: nicht mehr sichtbar

f2 <- function (x,y,z){ 
  return(x+2*y+3*z)
}

f3 <- function (x,y,z){ 
   x+2*y+3*z
}

f1(4,2,1)
f2(4,2,1)
f3(4,2,1) 

a <- 1:10
f1(a,a,a) 

a <- 1:10
b <- 1:2
f1(a,a,b) 
c <- 0
f1(a,b,c) 

f1(10,10,2)
f1(10,2,0)

f1
sqrt 
atan2


##########: Benannte und optionale Argumente ####
f4 <- function (x,y=1,z=0){ 
  x+2*y+3*z
}
a <- 1:10
b <- 1:2
f4(a)
f4(a,b)
f4(a,a,b)
f4()       # Fehler:

f4(z=b, x=a, y=a)
f4(a,z=a)   # Äquivalent zu: f4(a,1,a)
f4(a,1,a)


##########: Call-by-value und Copy-on-modify ####
f <- function(x) { 
  x[1] <- 42 
  x <- 2*x 
}

v <- 1:10
w <- f(v)
v
w

g <- function(x) { 
  return(length(x))
}

v <- 1:1e9 # E-Notation: 1e9 entspricht einer Milliarde
w <- g(v)
w


##########: Operator versus Funktionsaufruf ####
'*' (6,7)

x <- 1:12
sin(x^3-x^2/(2*pi))



##########: Verzweigung: if-else #### 
x <- 5

if (x%%2 == 0){
  z <- x
} else {
  z <-x+1
}
z

if (x%%2 == 0) z <- x else z <-x+1
z

z <- if (x%%2 == 0) x else x+1
z


##########: Verzweigung: ifelse ####
# keine gute Idee
x <- 1:10
if (x%%2 == 0){
  z <- x 
} else { 
  z <-x+1
}
z

x <- 1:10
z <- if (x%%2 == 0) x else x+1
z

x <- 1:10
z <- ifelse(x%%2 == 0, x, x+1)
z

x <- 2:13
z <- ifelse(x%%2 == 0, 5, 9)
z

?ifelse 


##########: Schleife: for  ####
x <- 11:15
for(i in x ) print(i^2) 


##########: Mehrfachverzweigung: switch  ####
switch(1, "eins", "zwei", "drei", "vier")
switch(3, "eins", "zwei", "drei", "vier")

