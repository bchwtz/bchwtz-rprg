# J. Willms, September 2019
# Programmierung für statistische Datenanalyse
#
# Quelltexte: Kapitel 5

########## Listen erzeugen  ####
?list

x <- list(nachname="Meier", matNr = 123456, noten=c(1.3,2.7))
x

x.noten
x$noten
x$matNr

str(x)

length(x)
typeof(x)
class(x)
is.list(x)
attributes(x)

str(x["noten"])
str(x[3])
str(x[["noten"]])
str(x[[3]])
str(x$noten)

v <- 1:4
v[[1]]
identical(v[1], v[[1]])
xv <- as.list(1,2,3,4)
str(xv)
str(xv[1])
str(xv[[1]])
identical(xv[1], v[[1]])

# Ohne Tags
x1 <- list("Meier", 123456, c(1.3,2.7))
x1
str(x1)

x[1:2]
x[[1:2]]


# Beachte den Unterschied
str(list(1:4))
str(list(1,2,3,4))
str(as.list(1:4))

length(list(1:4))
length(as.list(1:4))


# Listen können andere Listen enthalten
yx <- list(pvl=c(T,T,F), x)
str(yx)

yx2 <-list(yx,yx[2])
str(yx2)

yx3 <-list(list(list(T)))
str(yx3)

 
unlist(yx2) 
?unlist
 
# Elemente einfügen
x <- list(nachname="Meier", matNr = 123456, noten=c(1.3,2.7))
str(x)
x$vorname <- "Udo"
x[[2]] <- 222333     # besser ?:  x[["matNr"]] <- 222333
x$noten <- NULL
str(x)

z <- list(11,12,13)
z1 <- c(z, c(14,15))
str(z1)
z2 <- c(c(14,15), z)
str(z2)

 
#Hingegen:
z3 <- list(z, c(11,12,13))
str(z3)

str(z)
z[4:5] <- c(14,15)
str(z)

identical(z1,z)
 

#### List versus Vektor ####

x <-  c(1,2,3)
y <- x
y[3] <- 4

l1 <- list(1,2,3)
l2 <- l1
l2[[3]] <- 4

#### lapply() ####
l <- list(9:0, 1:100, 4)
str(l)
lapply(l, mean)

sapply(l,mean)
?sapply

sapply(l,range)
lapply(l,range)
str(sapply(l,range))

#### Factor ####

?factor
studiengang <- c("MB", "ET", "WIng", "WInfo")
data <-  sample(studiengang, 30, replace = TRUE)
data
str(data)

f1 <- factor(data)
f1
str(f1)
levels(f1)
attributes(f1)

f2 <- factor(data, c("MB", "ET", "WIng", "WInfo"))
str(f2)

f3 <- factor(data, c("MB", "ET", "WIng"))
str(f3)

str(as.vector(f1))
str(as.numeric(f1))
str(levels(f)[f])

table(f1) 
barplot(table(f1))
?barplot

t <- table(f1)
tt(t)
is.vector(t)
is.list(t)
is.array(t)

table(f1)
table(data)
summary(f1)

tt(summary(f1))
tt(table(f1))


###### Ordnung im Faktor #####
min(f1)
range(f1)

sort(f1)
 

f4 <- factor(data, levels= c( "ET", "WIng","MB", "WInfo"), ordered = TRUE)

min(f4)
range(f4)
sort(f4)
barplot(table(f4))


table(f)
table(data)


################### Data Frame: zeiten1 #################
df1 <- read.csv2("zeiten1.csv")
df1
str(df1)
summary(df1)

names(df1)
df1$n
df1$Messung.1
str(df1$n)
str(df1$Messung.1)
df1$n[2]


str(df1[1])
str(df1[[1]])
str(df1["n"])
str(df1[["n"]])
 

str(df1[2,1]) 
str(df1[1,2])  


str(df1[1,]) 
str(df1[,1]) 
str(df1[1])

str(df1[-1])
str(df1[2:5])
identical(df1[-1],df1[2:5])
apply(df1[-1],1, mean)
apply(df1[-1],2, mean)

lapply(df1[-1], mean)
sapply(df1[-1], mean)


###########  data.frame() #####################
n <- (4:9)*1e7L
mess1 <- c(3.656, 4.672, 5.719, 6.720, 7.641, 8.750)
mess2 <- c(3.781, 4.735, 5.672, 6.782, 7.673, 8.798)
mess3 <- c(3.719, 4.719, 5.672, 6.688, 7.704, 9.048)
mess4 <- c(3.766, 4.688, 5.625, 6.610, 7.750, 8.735)
df2 <- data.frame(n, mess1, mess2, mess3, mess4)
names(df2)
str(df2) 
identical(df1, df2)


###############   data.frame und Faktor ############
bezeichnung <- paste("M", 1:6, sep="")
str(bezeichnung)
df_a <- data.frame(n, bezeichnung, mess1)
str(df_a) 


df_b <- data.frame(n, bezeichnung, mess1, stringsAsFactors = FALSE)
str(df_b) 


### Vergleich mit einer Liste: keine Übernahme des Names
x <- list(bezeichnung, matNr = 123456, noten=c(1.3,2.7))  
str(x)

str(mittelw)
df4$bez <- bezeichnung
str(df4)
str(df4$bez)

df5 <- data.frame(n, bezeichnung, mittelw)
str(df5)


##### Filtering ####

df2$mess1 > 6
df2$mess1 > df2$mess3
df2[df2$mess1 > df2$mess3,]
df2[mess1 > mess3,]


subset(df1, df1$mess1 > df1$mess3)
# oder kürzer
subset(df1, mess1 > mess3)


##### Hinzufügen / Löschen ####
df3 <- df2 

df3$diff1 <- df2$mess2 -df2$mess1
str(df3)

Nummer <- 1:6
df3 <- cbind(df3[1], Nummer, df3[-1])
str(df3)

df3$diff1 <-NULL
str(df3)



