# J. Willms, September 2019
# Programmierung für statistische Datenanalyse
#
# Quelltexte: Kapitel 4

##########: typeof() und length() ####
a <- 42
typeof(a)

x <- 1:10
typeof(x)

z <- c(1,2,3)
typeof(z)

f3 <- function (x,y,z){ 
  x+2*y+3*z
}
typeof(f3)

typeof(sqrt) 


typeof(NULL)

typeof(NA)
z <- (1:4)[20]
z
typeof(z)
 
x <- 1:10
length(x)


##########: Attribute ####
x <- 1:10
attributes(x)

attributes(Nile)
str(Nile) 

class(1:10) 
class(c(1,2,3)) 
class(sqrt) 
class(NULL)
class(Nile)


##########: Vektoren ####
x1 <- c(T, F)           # logical (entspricht booleschen Wert)
x2 <- 4:5               # integer
x3 <- c(4, 5)           # double
x4 <- c(4+1i, 5+2i)     # complex
x5 <- c("vier", "5")    # character (entspricht einer Zeichenkette) 

x1 <- c(TRUE, FALSE) 
 
str(x2)
typeof(x2)
mode(x2)
storage.mode(x2)

str(x3)
typeof(x3)
mode(x3)
storage.mode(x3)

x <- c(3L, 5.2)
typeof(x)
typeof(x[1])

z <- c(3.1, 1L,"drei", 2+4i)
typeof(z)
str(z)


##########: Filter und Vektor-Indizierung ####
x <- 3L*(1:6)
n <- 4
u <- c(6,1,3)

n
u
x
x[n]  # das n-te Element
x[-n]  # alle, bis auf n-te Element
x[1:n]  # die ersten nElemente
x[-(1:n)]  # all bis auf die ersten n Elemente
x[u]  #  die Elemente x[u[1]], x[u[2]], . x[u[m]]
x[x > 7]  # all gr??er als 7
x[x >= 3 & x <= 7]  # die Elemente zwischen 3 und 7
x[x %in% u]  # alle Elemente, die u enthalten sind

res <- x > 7
str(res)

x[x>7]
subset(x, x>7)
which(x > 7)

x %in% u


##########: Elemente einfügen ####
x <- 3L*(1:8)
x
x <- c(x,5L) # 5 am Ende hinzufügen
x
x <- c(x[1:3],20L,x[4:length(x)]) # 20 als 4. Element einfügen
x

any(x%%4==0)
all(x%%4==0)



##########: Test auf Gleichheit ####
u <- 1:3
v <- c(1,2,3)
u==v
all(u==v)
identical(u,v)
?identical


#########: Elementnamen ####
x <- c(a=1, b=2, 3, delta=4)
x
x[1]
typeof(x[1])
is.vector(x)

a <- attributes(x)

names(x)
names(x) <- c("alpha", "beta","gamma", "delta") 
names(x)
str(x)

x["beta"]

x
str(x)
attributes(x)

letters
LETTERS
str(letters)

x <- 1:20
names(x) <- LETTERS[1:length(x)]
x
str(x)
x["H"]
 

######:  Matrizen erzeugen ####
matrix()

A <- matrix(1:6, nrow=2)
A
str(A)

B <- matrix(1:6, nrow=2, byrow = TRUE)
B
str(B)


C <- cbind(1:2,3:4,5:6)
C
D <- rbind(c(1,3,5), c(2,4,6))
D

rbind(c(1L,3L,5L), c(2L,4L,6L))

D[1,2]
D[2,3] <- 24
D

A <- matrix(1:6, nrow=2)
dim(A)
nrow(A)
ncol(A)
nrow
ncol

A
str(A)
typeof(A)
str(A)
attributes(A)
class(A) 
typeof(A)

E <- 1:6
attr(E,"dim") <- c(2,3) # oder: dim(E) <- c(2,3)
str(E)
attributes(E)
class(E) 
typeof(E)


######:  Indexierung  ####
F <- matrix(1:30, nrow=5)
F
c2 <- F[,2] # 2. Spalte
c2
z3 <- F[3,] # 3. Zeile
z3
class(F)
class(c2)
class(z3)

F[1:2,5:6]    # Matrix
F[1:2,6]      # Vektor
F[1:2,6, drop=FALSE] # Matrix
 
F[-1,]
F[1:3,1:2] <- 100:105
 
F[F< 13] 
F 
F[F[,3]< 13,]
F[,3]


F[,3]< 13

#######: Zeilen- und Spaltennamen  ####

A <- matrix(1:6, nrow=2)
A
attributes(A)
colnames(A)
colnames(A) <- c("a", "b", "c")
rownames(A) <- c("X", "Y")
A
str(A)
attributes(A)

a <- attributes(A)
a$dimnames
tt(a$dimnames)
#######: Einfügen und Löschen von Zeilen und Spalten  ###

A <- matrix(1:12, nrow=3)
A
A <-cbind(A,c(97,98,99))
A
A <-cbind(A[,1:2],c(44,55,66),A[,3:5]) 
A
A <-rbind(A[1,], -1:-6, A[2:3,])
A

A <-A[c(1,2,4),]
A
A <-A[c(1,3),c(2,4,6)]
A


#######: Matrixoperationen  ####
A <- matrix(1:9,3)
A[1,1]=0
A
colSums(A) 
rowSums(A) 
colMeans(A) 
rowMeans(A) 

t(A)
det(A) 

A+A
1.1*A

A*A
A %*% A


#######: Lineares Gleichungssystem  ####
z <- c(1,2,3)
b <- A %*% z
b
solve(A,b)
 
as.matrix(z)


B <- solve(A)
A %*% B
B %*% A

E <- B %*% A
round(E,12)


#######: Eigenwerte  ####
eig <- eigen(A)
eig 
eig$values
eig$vectors

e <-eig$values
E <- eig$vectors

v <-  as.matrix(E[,1]) # Eigenvektor zum Eigenwert e[1]
str(v)
b <- A %*% v
c <- e[1]*v
str(b)
str(b - c)
all.equal(b,c)
?all.equal
 

for(i in 1:3) {
  v <-  as.matrix(E[,i])
  b <- A %*% v
  c <- e[i]*v
  str(b)
  str(b - c)
  print(all.equal(b,c))
}

rep(1:3, times=3)
rep(1:3,each=3)
?rep

A %*% E
rep(e,each=3)*E

rep(1:3, 3)
rep(1:3, times=3)
rep(1:3,each=3)
rep(e,each=3)
?rep


#######: apply ####
?apply

A <- matrix(1:15, nrow=3)
A

rowSums(A)
apply(A,1,sum)

colSums(A)
apply(A,2,sum)

rowMeans(A)
apply(A,1,mean)

colMeans(A)
apply(A,2,mean)


quadraticMean <- function(x){
  return(mean(x*x))
}

quadraticMean(A)
apply(A,1,quadraticMean)
apply(A,2,quadraticMean)


bereich <- function(x){
  return(c(min(x), max(x)))
}

bereich(A)
apply(A,1,bereich)
apply(A,2,bereich)
 
range
range(A)
apply(A,1,range)
apply(A,2,range)

f <- function(x){
  return(x/c(2,4))
}


#######: Array ####
?array

a=1:30
str(a)
A=matrix(a, nrow=5)
str(A)
B=array(a,dim=c(5,3,2))
str(B)

B
B[,,1]
B[,2,1]
B[5,3,2]
length(B)
dim(B)

A <- array(1:12, dim=1)
str(A)
class(A)
is.vector(A)

B <- array(1:12, dim=c(2,6))
str(B)
class(B)
is.matrix(B)
C <- matrix(1:12, nrow = 2)
str(C)
identical(B, C)



#######: Ziffern: MNIST-Bilder #####

# PRECONDITION: die Datei "ziffern.rds" im Arbeitsverzeichnis
ziffern <- readRDS("ziffern.rds")
#saveRDS(ziffern, "ziffern1.rds")

str(ziffern)
object.size(ziffern)

plot(as.raster(ziffern[1,,], max = 255))
plot(as.raster(ziffern[2,,], max = 255))

plot(as.raster(255-ziffern[1,,], max = 255))
plot(as.raster(255-ziffern[2,,], max = 255))






