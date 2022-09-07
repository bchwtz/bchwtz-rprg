# J. Willms, August 2019
# Programmierung für statistische Datenanalyse
#
# Quelltexte: Fallstudie 3: Visualisierung mit ggplot3 (Kapitel 6)
 
library(ggplot2)

### Kapitel 2: Funktionsplot am Beispiel der Verteilung der chi-Quadrat Verteilung #####  

ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, colour = "red")

ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = sin, colour = "red")

?dchisq 
sizeline <- 1
ggplot(data.frame(x = c(0, 10)), aes(x)) + 
  stat_function(fun = dchisq, args = list(df=1), color = "cyan", size = sizeline) +
  stat_function(fun = dchisq, args = list(df=2), color = "red", size = sizeline) +
  stat_function(fun = dchisq, args = list(df=3), color = "blue", size = sizeline)+
  stat_function(fun = dchisq, args = list(df=4), color = "green3", size = sizeline)+
  labs(title = "Dichten der Chi-Quadrat-Verteilungen", subtitle ="Data Science, J. Willms") 

sizeline <- 1
ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dt, args = list(df=1), color = "cyan", size = sizeline) +
  stat_function(fun = dt, args = list(df=2), color = "red", size = sizeline) +
  stat_function(fun = dt, args = list(df=3), color = "blue", size = sizeline)+
  stat_function(fun = dt, args= list(df=4), color = "green3", size = sizeline)+
  labs(title = "Dichten der t-Verteilungen", subtitle ="Data Science, J. Willms") +
  theme_light()

##### Aufgabe a) Die chi-Quadrat-Verteilung ist ein Spezialfall der Gamma-Verteilung #####
##  Zeigen Sie, dass Sie Abbildung 3 auch mit dgamma() statt mit dchisq() erzeugen können.

## Abb3. wie oben: 
sizeline <- 1
ggplot(data.frame(x = c(0, 10)), aes(x)) + 
  stat_function(fun = dchisq, args = list(df=1), color = "cyan", size = sizeline) +
  stat_function(fun = dchisq, args = list(df=2), color = "red", size = sizeline) +
  stat_function(fun = dchisq, args = list(df=3), color = "blue", size = sizeline)+
  stat_function(fun = dchisq, args = list(df=4), color = "green3", size = sizeline)+
  labs(title = "Dichten der Chi-Quadrat-Verteilungen", subtitle ="Data Science, J. Willms")

?dgamma
## mit dgamma statt dchisq
sizeline <- 1
ggplot(data.frame(x = c(0, 10)), aes(x))   + 
    stat_function(fun = dgamma, args = list(shape=1/2, rate=1/2), color = "cyan", size = sizeline) +
    stat_function(fun = dgamma, args = list(shape=1, rate=1/2), color = "red", size = sizeline) +
    stat_function(fun = dgamma, args = list(shape=3/2, rate=1/2), color = "blue", size = sizeline)+
    stat_function(fun = dgamma, args = list(shape=2,   rate=1/2), color = "green3", size = sizeline)+
    labs(title = "Dichten der Chi-Quadrat-Verteilungen (mithilfe der Gammaverteilung)", subtitle ="Data Science, J. Willms") 
#________________________________________________________________________________   

########  Kapitel 3: Histogramme ########
set.seed(1)
x = rnorm(50)
h <- hist(x, breaks = 10) 
h
# entspricht freq = FALSE
# hist(x, freq = FALSE, breaks = 10)
# vergleiche: hist(x, freq = TRUE, breaks = 10)

library(ggplot2)
ggplot(data.frame(x = x), aes(x)) + 
  geom_histogram(bins=9)   # y: count

str(h)

ggplot(data.frame(x = x), aes(x)) + 
  geom_histogram(boundary=-2.5, binwidth = 0.5, fill="white", color="black") + 
  labs(title = "Histogramm of x") +
  theme_minimal()

df <- data.frame(x = x)
str(df)

######## 3.1 Histogramm: Anzahl der Gruppen ######

for(nBins in 7:11) {
  p <- ggplot(data.frame(x = x), aes(x)) + 
    geom_histogram(boundary=-2.5, bins = nBins, fill="gray", color="black") + 
    labs(title = "Histogramm of x") +
    theme_bw()
  print(p)
}

#=============== JW: benötigt die Funktion multiplot()
# Die Funktion multiplot() ist nicht Bestandteil der Fallstudie und des Moduls.
# Sie wurde lediglich benutzt, um für das Skript Grafiken mit mehreren Plots zu erstellen.
# Die Definition der Funktion multiplot() finden Sie am Ende diese Skriptes.

pHist <-  function(nBins){
  p <- ggplot(data.frame(x = x), aes(x)) + 
    geom_histogram(boundary=-2.5, bins = nBins, fill="gray", color="black") + 
    labs(title = "Histogramm of x") +
    theme_bw()
  return (p)
}
multiplot(pHist(7), pHist(8), pHist(9), pHist(10), pHist(11), pHist(11), cols=2)


######## 3.2: Unterschiedliche untere Grenzen: Schneehöhen #####
 
sHoehe <- c(25.0 , 39.8 , 39.9 , 40.1 , 46.7 , 49.1 , 49.6 , 51.1 , 51.6,
            53.5 , 54.7 , 55.5 , 55.9 , 58.0 , 60.3 , 63.6 , 65.4 , 66.1,
            69.3 , 70.9 , 71.4 , 71.5 , 71.8 , 72.9 , 74.4 , 76.2 , 77.8,
            78.1 , 78.4 , 79.0 , 79.3 , 79.6 , 80.7 , 82.4 , 82.4 , 83.0,
            83.6 , 83.6 , 84.8 , 85.5 , 87.4 , 88.7 , 89.6 , 89.8 , 89.9,
            90.9 , 97.0 , 98.3 , 101.4 , 102.4 , 103.9 , 104.5 , 105.2 , 110.0,
            110.5 , 110.5 , 113.7 , 114.5 , 115.6 , 120.5 , 120.7 , 124.7 , 126.4)
df <- data.frame(x=sHoehe) 
str(df)

transparenz <- 0.2
for(b in seq(16, 24, by=2)) {
  p <- ggplot(df, aes(x)) + 
    geom_histogram(binwidth=10, boundary=b, alpha=transparenz) 
  print(p)
  transparenz <- transparenz+0.1
}


#=============== JW: benötigt die Funktion multiplot()
# Die Funktion multiplot() ist nicht Bestandteil der Fallstudie und des Moduls.
# Sie wurde lediglich benutzt, um für das Skript Grafiken mit mehreren Plots zu erstellen.
# Die Definition der Funktion multiplot() finden Sie am Ende diese Skriptes.
pHist2 <-  function( grenze, alpha = 0.4){
  return (ggplot(df, aes(x)) + geom_histogram( binwidth=10, boundary=grenze, closed="right", alpha=alpha))
}

multiplot(pHist2(16,0.2),pHist2(18,0.3), pHist2(20,0.4),pHist2(22,0.5), pHist2(24,0.6), cols=2)


transparenz <- 0.2
p <- ggplot(df, aes(x)) 
for(b in seq(16, 24, by=2)) {
  p <- p + geom_histogram(binwidth=10, boundary=b, alpha=transparenz) 
  transparenz <- transparenz+0.1
}
p + labs(title = "Überlagerung der fünf Histogramme")


#######  3.3	Kerndichteschätzer geom_density() ############ 
ggplot(df, aes(x)) +   # Schlecht
  geom_histogram(binwidth=16, boundary=b, alpha=0.2) +
  geom_density(color="blue", size =1.2)



ggplot(df, aes(x, stat(density))) + 
  geom_histogram(binwidth=10, boundary=16, alpha=0.2) +
  geom_density(color="blue", size=1)

ggplot(df, aes(x, stat(density))) + 
  geom_histogram(binwidth=10, boundary=16, alpha=0.2) +
  geom_density(color="blue", size=1)
  geom_density(adjust=0.5, color="red", size =1) 
# adjust = 1/2 means use half of the default bandwidth.


transparenz <- 0.2
p <- ggplot(df, aes(x, stat(density))) 
for(b in seq(16, 24, by=2)) {
  p <- p + geom_histogram(binwidth=10, boundary=b, alpha=transparenz) 
  transparenz <- transparenz+0.1
}
p <- p + geom_density(color="blue", size=1)+
    geom_density(adjust=0.5, color="red", size =1)
p + 
  theme_bw() +
  labs(title = "Überlagerung der Histogramme mit Dichteschätzer", 
         subtitle ="Data Science, J. Willms")



### Kapitel 4:  Beispiel: Zentraler Grenzwertsatz ######
set.seed(1)
# Erzeuge einen zufälligen Wert x mit
# P(x=0) = 2/3 und P(x=1 = 1/3)
# Also ist x eine Realisation einer zweiwertigen Zufallsvariable
x1 <- sample(c(0,0,1),1)

# Die (theoretische) Varianz der zugrundeliegenden zweitwertigen 
# Zufallsvariablen X lautet
# Var(X) = 2/3 * 1/9 + 1/3 * 4/9 = 2/9
(varX <-  2/9)

n <- 300
# Erzeuge n solche zufälligen Datensätze
x <- sample(c(0,0,1),n, replace = TRUE)

# Teste:
head(x,200)
str(x)
length(x[x==1])  # sollte ca. n/3 sein
n/3
summary(x) 
mean(x)         # sollte ca. 1/3 sein
sd(x)
var(x)          # sollte ca. gleich varX sein
varX

# Als Nächstes betrachten wir den Durchschnitt unserer zufälligen Daten
(sn <- sum(x)/n)  # == mean(x)
all.equal(sn, mean(x))

# sn ist die Realisierung eine ZV (Zufallsvariable) Sn und für große n macht der zentrale Grenzwertsatz
# eine Aussage über die Verteilung von Sn (Sn sollte ungefähr normalverteilt sein)
# Dies wollen wir visuell mithilfe einer Simulation überprüfen
# Dazu müssen wir mehrfach n solche zufälligen Datensätze erzeugen, sn berechnen
# und grafisch darstellen

Anzahl <- 500    # gibt an, wie oft sn realisiert werden soll 
s <- replicate(Anzahl, sum(sample(c(0,0,1),n, replace = TRUE))/n)

head(s,100)
str(s)
summary(s) 
mean(s)     # sollte ca. 1/3 sein 
sd(s)
var(s)      # sollte ca. gleich varX/n sein
varX/n
var(s) - 2/(9*n) 
 
##### Aufgabe b): Welche Verteilung hat die Zufallsvariable n*Sn?   ####
##  Die Zufallsvariable n*Sn ist binomialverteit mitden Parametern n und 1/3
#___________________________________________________________________________


##### Aufgabe c): replicate()   ####
## Erzeugen Sie den Vektor s, ohne die Funktion replicate() zu benutzten
# (Hinweis: sapply())
set.seed(1)
x1 <- sample(c(0,0,1),1)
n <- 300
x <- sample(c(0,0,1),n, replace = TRUE)
Anzahl <- 500    # gibt an, wie oft sn realisiert werden soll 
s2 <- sapply(1:Anzahl, function(i){sum(sample(c(0,0,1),n, replace = TRUE))/n})
str(s2)

# Alten Zustand des Pseudo-Zufallsgenerators wiederherstellen
set.seed(1)
x1 <- sample(c(0,0,1),1)
n <- 300
x <- sample(c(0,0,1),n, replace = TRUE)
Anzahl <- 500    # gibt an, wie oft sn realisiert werden soll 
s <- replicate(Anzahl, sum(sample(c(0,0,1),n, replace = TRUE))/n)
str(s)
#____________________________________________________________



## Erzeuge Data Frame
df <- data.frame(x=s) 
str(df)

# Zeichne Histogramm
ggplot(df, aes(x, stat(density))) + 
  geom_histogram(bins=8, alpha=0.4) +  
  geom_density(color="blue", size =1.2)+
  labs(title = "Konvergenz gegen die Normalverteilung?", 
       subtitle ="anzahl=50, n=300,  J. Willms") +
  theme_bw() 



# Welche Aussage macht der zentrale Grenzwertsatz über die Verteilung der so 
# erzeugten sn-Werte?
# Antwort: sn sollte ungefähr normalverteilt sein; genauer ungefähr N(n/3,n*2/9 )-verteilt
ggplot(df, aes(x)) +
  geom_histogram(aes(x, stat(density)), bins=10, alpha=0.4)+
  geom_density(color="blue", size =1.2) +
  labs(title = "Konvergenz gegen die Normalverteilung?", subtitle ="anzahl=50, n=300,  J. Willms") +
  stat_function(fun = dnorm, args= list(mean=1/3, sd=sqrt(2/(9*n))), 
                colour = "red", size = 1.1)+
  theme_bw()


zeichne <- function(df,nGruppen, n=300){
  supStr <- sprintf("anzahl=%d, n=%d,  J. Willms", length(df$x), as.integer(n)) 
  p <- ggplot(df, aes(x)) +
    geom_histogram(aes(x, stat(density)), bins=nGruppen, alpha=0.4)+
    geom_density(color="blue", size =1.2) +
    labs(title = "Konvergenz gegen die Normalverteilung?", subtitle = supStr) +
    stat_function(fun = dnorm, args= list(mean=1/3, sd=sqrt(2/(9*n))), 
                  colour = "red", size = 1.1)+
    theme_bw()
  return(p)
}
 
# Alten Zustand des Pseudo-Zufallsgenerators wiederherstellen
# _______________________________  
set.seed(1)  
n <- 300
x <- sample(c(0,0,1),n, replace = TRUE)
Anzahl <- 50    # gibt an, wie oft sn realisiert werden soll 
s <- replicate(Anzahl, sum(sample(c(0,0,1),n, replace = TRUE))/n)
df <- data.frame(x=s)  
p1 <- zeichne(df,10,n)
p1
#  _______________________________ 
set.seed(1)  
n <- 300
x <- sample(c(0,0,1),n, replace = TRUE)
Anzahl <- 2000    # gibt an, wie oft sn realisiert werden soll 
s <- replicate(Anzahl, sum(sample(c(0,0,1),n, replace = TRUE))/n)
df <- data.frame(x=s)  
p2 <- zeichne(df,18,n)
p2
#  _______________________________ 
 
multiplot(p1,p2, cols=2)


#___________________________________________________________________________
### Aufgabe d)	######
# Für m = 2,3,4,5 überprüfen Sie in ähnlicher Weise durch eine Simulation, 
# wie sich die Summe n identisch verteilter und unabhängiger Zufallsvariablen verhält, 
# die N(0; 1) verteilt sind.
# Bemerkung: In der ursprünglichen Aufgabenstgellung wurde n statt m benutzt
m <- 4      
n <- 300 

# Zufallsgenerator für die k-fache Summe von N(0,1)-Verteilungen
rsumNormal <- function(n, k=2){
  res <- rnorm(n)
  for(i in 1:(k-1))
    res <- res + rnorm(n)
  return(res)
} 

x <- rsumNormal(n,m)
# Jedes Element in x ist Realisation einer Summe von m identisch verteilten N(0,1)-Variablen
# Teste:
head(x,50)
summary(x) 
mean(x)         # sollte ca. 0 sein
var(x)          # sollte ca. m

# Als Nächstes betrachten wir den Durchschnitt unserer zufälligen Daten
(sn <- sum(x)/n) # sollte annährend 0 sein
 
# sn ist die Realisierung eine ZV (Zufallsvariable), die laut zentralen Grenzwertsatz 
# N(0,m)- verteilt ist
# Dies wollen wir visuell mithilfe einer Simulation überprüfen
# Dazu müssen wir mehrfach n solche zufälligen Datensätze erzeugen, sn berechnen
# und grafisch darstellen

Anzahl <- 500    # gibt an, wie oft sn realisiert werden soll 
s <- replicate(Anzahl, sum(rsumNormal(n,m))/n)
str(s)
mean(s)     # sollte ca. 0 sein  
var(s) - m/n # sollte ca. 0 sein 

zeichne2 <- function(df,nGruppen, m,  n=300){
  supStr <- sprintf("anzahl=%d, n=%d,  Summe normalverteilter Werte", length(df$x), as.integer(n)) 
  p <- ggplot(df, aes(x)) +
    geom_histogram(aes(x, stat(density)), bins=nGruppen, alpha=0.4)+
    geom_density(color="blue", size =1.2) +
    labs(title = "Konvergenz gegen die Normalverteilung?", subtitle = supStr) +
    stat_function(fun = dnorm, args= list(mean=0, sd=sqrt(m/n)), 
                  colour = "red", size = 1.1)+
    theme_bw()
  return(p)
}

# Der Übersichtlichkeit halber werden alle Werte noch einmal neu erzeugt
set.seed(0)
m <- 2     
n <- 3
x <- rsumNormal(n,m)
Anzahl <- 500    # gibt an, wie oft sn realisiert werden soll 
s <- replicate(Anzahl, sum(rsumNormal(n,m))/n)
df <- data.frame(x=s) 
zeichne2(df,sqrt(nrow(df)),m,n) 
#___________________________________________________________________________


### Kapitel 5:	Quantilplot    #####

set.seed(0)  
s <- rnorm(400 ,mean=4, sd=2)  # N(4;4)-verteilt
ggplot(data.frame(s), aes(sample = s)) +
  stat_qq() + stat_qq_line()  +
  labs(title = "NP-PLot", subtitle = "N(4;4)-verteilt")
 

##### Aufgabe e)	Erstellen Sie NP-Plots ##### 
# für unterschiedliche Seeds und unterschiedliche Datensatzgrößen entsprechende 
# NP-Plots für Daten, die N(2; 9)-verteilt sind.
anzahl <- 400
for(seed in 11:14) {
  set.seed(seed)  
  s <- rnorm(anzahl, mean =2, sd=3)   
  p <- ggplot(data.frame(s), aes(sample = s)) +
    stat_qq() + stat_qq_line()  
  print(p)
}
#_______________________________________________________

### Daten aus dem vorherigen Kapitel
set.seed(1)  
n <- 300
x <- sample(c(0,0,1),n, replace = TRUE) 
s50 <- replicate(50, sum(sample(c(0,0,1),n, replace = TRUE))/n)
ggplot(data.frame(s50), aes(sample = s50))+
  stat_qq() + stat_qq_line() +
  labs(title = "NP-PLot", subtitle = "Anzahl=50")


set.seed(1) 
x <- sample(c(0,0,1),n, replace = TRUE) 
s2000 <- replicate(2000, sum(sample(c(0,0,1),n, replace = TRUE))/n)
ggplot(data.frame(s2000), aes(sample = s2000))+
  stat_qq() + stat_qq_line() +
  labs(title = "NP-PLot", subtitle = "Anzahl=2000")

# # Berechnung der Steigung
# steigung1 <- 0.11/4   # aus der Grafik entnommen
# steigung
# sqrt(2/(9*n)) 
# n
# 
# # Theor. Berechnung der Steigung
# q25     <-  quantile(s, 0.25, type = 5) 
# q75    <-  quantile(s, 0.75, type = 5) 
# norm25 <- qnorm(0.25) 
# norm75 <-  qnorm(0.75) 
# steigung  <-  (q25 - q75) / (norm25 - norm75) 
# steigung
# yAbschnitt    <-  q25 - slope * norm25
# yAbschnitt 




##_____________________________________________________
### multiplot() #######################
# Die Funktion multiplot() ist nicht Bestandteil der Fallstudie und des Moduls.
# Sie wurde lediglich benutzt, um für das Skript Grafiken mit mehreren Plots zu erstellen.

# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}







