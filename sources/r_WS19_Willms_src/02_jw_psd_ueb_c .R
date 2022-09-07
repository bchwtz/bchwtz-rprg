# J. Willms, August 2019
# Programmierung für statistische Datenanalyse
#
# Quelltexte: Übungen zu Kapitel 2


##########: Aufgabe 3a  ####
1/0
0/0
1/0 - 1/0
# Was ist der Unterschied zwischen Inf und NaN ? (siehe ?NaN)

47%%10
47%/% 10  # Im Skript nicht erklärt, siehe ?'%/%' 
2^10
?'%/%'

-9%%5
-11%%5
3.1%%3
pi%%3 

factorial(5)
factorial(60)
abs(-3)

choose(4,2)
choose(6,1)
choose(6,2)
choose(6,3)
choose(6,4) 
choose(49,6) 


x <- 1:6
tan(atan(x))
log(exp(x))


##########: Aufgabe 3b  ####
x <- c(2,3,5,2)
sum(x)
prod(x)
cumsum(x)
cumprod(x)

min(x)
max(x)
range(x)
sort(x)


mean(x)
var(x)
sd(x)

x %% 5
x %% 5 - 3
x + c(1,2)
x + 1:8 

8:4
3.8 :7.2
7.2 : 3.8 
rep(x,3)
rep(x,each = 3) #Neu: im Unterchied zu 

#------------------------
x <- c(2,3,5,2)
x[6] <- 10
x
sum(x)
sum(x, na.rm = TRUE)
mean(x)
mean(p, na.rm = TRUE)

 
##########: Aufgabe 3c  ####
x <- c(5,3,5,7)
x
y <- c(2,3,c(1,2),7) 
y
z <- c(x,y,17)
z
z <- c(z,z)
z
length(z)


##########: Aufgabe 4, diff()  ####
(p <- c(2,3,5,7,11,13,17,19,23,29))
diff(p)
dp <- diff(p)
dp
cumsum(p)

cumsum(c(p[1],dp))

x <- runif(20)   # NEU
x == cumsum(c(x[1],diff(x)))
all(x == cumsum(c(x[1],diff(x))))


##########: Aufgabe 5a  ####
x <- 1:5
?sample
set.seed(1)
sample(5,3)
sample(5,3)
sample(5,5)
sample(5,5)
sample(5,3,TRUE)
sample(5,3,TRUE)
sample(5,5,TRUE)
sample(5,5,TRUE)

##########: Aufgabe 5b, zufällige binäre Folge  ####
sample(c(0,1), 100, TRUE)

##########: Aufgabe 5c, fünf Permutationen  ####
sample(0:9, 10)
sample(0:9, 10)
sample(0:9, 10)
sample(0:9, 10)
sample(0:9, 10)
 

##########: Aufgabe 6; Random Walk  #### 
set.seed(1)
n <- 10e4
Schritte <- 1:n
Position <- cumsum(sample(c(-1, 1), n, TRUE))
plot(Schritte, Position, type="l", col="blue")



 


