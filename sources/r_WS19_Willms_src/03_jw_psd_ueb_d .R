# J. Willms, August 2019
# Programmierung für statistische Datenanalyse
#
# Quelltexte: Übungen zu Kapitel 3


##########: Aufgabe 1 ######

'+' (7,'*' (2, 4))
1e3
10e3
log(0)
0*log(0)
1e-4*log10(1e-4) # NEU
1e-8*log10(1e-8)

##########: Aufgabe 2 ######
# Welchen Wert haben folgenden Vektoren und was ist ihr Typ?

i <- 1:10
gerade <-  function(x){
  x %% 2 == 0
}

g <- gerade(i)
x <- 2.1*i
c <- 4*x +1i*x
z <- as.character(i)     # Nicht im Skript

 
# LSG: -----
str(i)
typeof(i)

str(g)
typeof(g)

str(x)
typeof(x)

str(c)
typeof(c)

str(z)
typeof(z) 

?as.character
?as.double
?as.integer
 

##########: Aufgabe 3: size() ######
x <- 1
object.size(x)
for(i in 1:20) x <- c(x,x)
str(x)
object.size(x)  

y <- rep(1,2^20)
str(y)
identical(x, y)
object.size(y)
rm(x) 

2^20*8 +48   # 48 == 56 -8
2^20*8
object.size(1:2^40)


#Vergleiche:
xl <- 1L
object.size(xl)
for(i in 1:20) xl <- c(xl,xl)
str(xl)
object.size(xl)
rm(xl)

# Vergleiche:
y <- rep(1,2^40)  # Error: cannot allocate vector of size 8192.0 Gb
y <- 1:2^50       # OK
y[1] <- 5
rm(y)


##########: Aufgabe 4: normalverteilte Werte ######
x <- rnorm(1000, mean=1, sd = 2)
mean(x)
var(x)


##########: Aufgabe 5: Schleifen ######
# Vereinfache:
n <- 3
count <- 0
repeat {
  if(n >= 10 & count < 5) break
  print(n^2)
  n <- n+2
  count <- count+1
}

# LSG
n <- 3
count <- 0
while(n < 10 & count < 5){
  print(n^2)
  n <- n+2
  count <- count+1
}


##########: Aufgabe 6a: Skalieren  Wahrscheinlichkeit ######
 
skalierenW <- function(x) {
  x/sum(x)
}


x <- c(10,10,30)
skalierenW(x)
p <- c(0.2,0.2,0.6)

# Test
for(i in 1:10) {
  x <- sample(1:100, 5, TRUE)
  p <- skalierenW(x)
  all(p >= 0)
  print(sum(p) == 1)
  all(x/sum(x) == p) 
}

##########: Aufgabe 6b: Skalieren  Skalieren Affin ######
 
skalierenAffin <- function(x) {
  #xbereich <- range(x)   # Bereich von x
  #xbereich <- range(x, na.rm = TRUE)   # Bereich von x
  xbereich <- range(x, finite = TRUE)   # Bereich von x, finite = TRUE includes na.rm = TRUE.
  (x - xbereich[1]) / (xbereich[2]-xbereich[1])
  
}
?range

skalierenAffin(1:5)
skalierenAffin(6:10)
skalierenAffin(c(0,1,20))
skalierenAffin(c(0,1,20)+10)
skalierenAffin(c(-0.5, -1, 1, 0.5))

# Kritische Fälle:
skalierenAffin(c((1:5), NA))
skalierenAffin(1/(-2:2))
skalierenAffin(c(-0.5, -1, 1, 0.5) ) #Neu


##########: Aufgabe 7: Sensor ###### 
?runif

rstrecken<- function(n, min = 0, max = 1) {
  return(abs(diff(runif(n))))
}
 
# Plot ----
#n <- seq(100, 100000, by=100)
n <- seq(100L, 1e5L, by=100L)
str(n)
Durchschnitt <- NULL
for(nn in n){  
  Durchschnitt <- c(Durchschnitt, mean(rstrecken(nn))) 
}

str(Durchschnitt)
plot(n, Durchschnitt, type="l", col="blue")


### Beachte: ?runif
# runif(n, min = 0, max = 1)
# n	 number of observations. If length(n) > 1, the length is taken to be the number required.
 

# Histogramm ----
 
Durchschnitt <- NULL
(nn <- 1e5)
for(i in 1:100){  
  Durchschnitt <- c(Durchschnitt, mean(rstrecken(nn)))
}

hist(Durchschnitt)



##########: Aufgabe 8: Lauflängen ###### 
 
llaenge <- function(x)
{
  if (!is.vector(x) && !is.list(x)) 
    stop("'x' muss ein atomarer Vektor sein")
  
  n <- length(x)
  v <- x[-1L] != x[-n]
  diff(c(0L, which(v) ,n))
}

llaenge1 <- function(x)
{
  if(all(x!=1 & x!=0))
    stop("keine 0-1-Folge")
  return(llaenge(x))
}

x <- c(1,1,1,1,6,0,0)
all(x==1 | x==0)
all(x!=1 & x!=0)


x [x!= x[1]]

all(x==1 | x==0)


llaengeInverse <- function(r, first= 0L, second=1L)
{
  if (!is.vector(r) && !is.list(r)) 
    stop("'x' muss ein atomarer Vektor sein")
  pattern <- rep_len(c(first,second), length(r))   # siehe ?rep. rep_len(x, length.out)
  # if(length(r)==3) pattern[1] = 5  # FEHLER-Ausgabe testen
  rep(pattern,r)
}


testL1 <- function(x, second=1L){
  print(x)
  r <- llaenge(x)
  print(r) 
  print(llaengeInverse(r, x[1], second))
  print(llaenge1(x))
}



x <- c(0,0,0,1,1,0,0,0,0,1,0,1,1)
testL1(x) 

x <- c(1,1,1,1,0,0)
testL1(x,0) 

x <- c(0,0,0,0,1,1)
testL1(x,2)

x <- c("a","a","a","a","a","a","b","b","b","b","a")
testL1(x,"b")

x <- c("rot", "rot", "blau")
testL1(x,"blau")


# Testen: 

testLaufLaenge <- function(anzahl = 16L, maxLaenge = 12) {
  for(i in 1:anzahl){ 
    n <- sample(1:maxLaenge,1)
    x <- sample(c(0L,1L),n,TRUE)
    r <- llaenge(x)
    print(r)
    y <- llaengeInverse(r, 1L, 0L)
    if(y[1] != x[1])
      y <- 1-y
    if(any(x != y))  
      stop("Test nicht bestanden: ", x)
  }  
}

testLaufLaenge()
testLaufLaenge(100, 20)










