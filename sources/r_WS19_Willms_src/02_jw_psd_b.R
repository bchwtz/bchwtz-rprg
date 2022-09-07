# J. Willms, August 2019
# Skript: Programmierung für statistische Datenanalyse
#
# Quelltexte: Kapitel 2

# Ausgabe der Installationsverzeichnisse
?.libPaths()         # siehe Kapitel 1


##########: Numerische Ausdrücke und Zuweisungen ####
6*7

'*' (6,7)

2^10   # 2 hoch 10
10/3   # ganzzahlige Division?


##########: Variablen  ####  
res = 10/3       # '=' sollte vermieden werden

res <- 10/3
res
name <- 10
name
name <- "zehn"
name

Res = 1.4
Res
res


##########: Ein Vektor als einfacher Datensatz ####
p <- c(2,3,5,7,11,13,17,19,23,29)
p
p[1]
p[2]
p[10]
p[11]
p[12]
p[13]
p[13] <- 41

p
p^2
2*p+1
p%%4

mean(p)
sd(p)            

mean(p, na.rm=TRUE)    # rm steht f?r remove
sd(p, na.rm=TRUE)

x <- 1:16
sqrt(x)


##########:	Recycling ####
x <- rep(1,6) # entspricht c(1,1,1,1,1,1)
x
y <- c(1,2)
x+y
 
c(1,1,1,1,1,1) + c(1,2,1,2,1,2)

?rep
x <- rep(1,6)
y <- 1:4   # wie c(1L,2L, 3L, 4L)
str(y)
x+y

rep_len(1:3, 5)
rep_len(1:3, 7)

7.1:3.3 
2:5 + 10
?Syntax


##########: Mehr zu RStudio ####
x <- seq(0, 4*pi, by=0.1)
x

help(seq)
?seq

str(x)
#plot(x, sin(x))
plot(x, sin(x), type="l", col="blue") 


##########: Demos ####
demo()
Nile
str(Nile)

mean(Nile)
sd(Nile)
summary(Nile)
plot(Nile)
hist(Nile)
boxplot(Nile) 

##########: 
rnorm(10)
runif(10)

?runif

y <- rnorm(1000)
summary(y)

x <- rnorm(1000000)
summary(x)

hist(y)
hist(x, breaks = 100)


y <- runif(1000)
summary(y)

x <- runif(1000000)
summary(x)

hist(y)
hist(x, breaks = 100)

 
z <- seq(-6, 6, by=0.1)
plot(z, 100000*dnorm(z), type="l", col="blue")


##########: RNG ####
?set.seed
rnorm(3)
rnorm(3)
set.seed(1)
rnorm(3)
rnorm(3)
set.seed(1)
rnorm(3)
rnorm(3)

 