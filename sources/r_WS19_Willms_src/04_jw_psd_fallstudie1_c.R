# J. Willms, August 2019
# Programmierung für statistische Datenanalyse
#
# Quelltexte: Fallstudien 1 (Kapitel 4)


###############  boxplot ####### 
?par
?paste
?summary
?boxplot

anzahl = 8
op <- par(mfrow=c(2,anzahl/2), col="blue")
n <- 100L
x=0:n 
for (i in 0:(anzahl-1)){
  x[n] <- x[n]+10*i
  x[n-1] <- x[n-1]+5*i
  print(paste("i = ",i, ":")) 
  print(summary(x)) 
  boxplot(x, main = paste("i = ",i), #range=3,
          xlab = paste("x[n-1] =", x[n-1], ", x[n] =", x[n]))
}

par(op) # stellt die ursprüngliche Grafikeinstellung wieder her


# LSG: range=3 (siehe oben)
# range	
# this determines how far the plot whiskers extend out from the box. 
# If range is positive, the whiskers extend to the most extreme data point 
# which is no more than range times the interquartile range from the box. 
# A value of zero causes the whiskers to extend to the data extremes.



 
###############  MNIST ####### 

# mnist <- dataset_mnist()
# Downloading data from
# https://storage.googleapis.com/tensorflow/tf-keras-datasets/mnist.npz:

# Siehe jw_mnist1.R

######   Nur label und ziffern einlesen #####

ziffern <- readRDS("ziffern.rds")
label<- readRDS("label.rds")
str(ziffern)

# a) Bilder pro Ziffer ###########
(nBilder <- dim(ziffern)[1])  # Anzahl der Bilder
(t <- table(label))
sum(t) #  == nBilder

(nBilderProZiffer  <- max(table(label)))  # 14: maximale Anzahl der Bilder pro Ziffer



# b) Alle Ziffern anzeigen ###########
iMax <-  5   # Anzahl  Reihen
jMax <-   20   # Anzahl Spalten

# Konvertieren: aus Schwarz <-> wird Weiß
for(i in 1:nBilder){
  ziffern[i,,] <- 255-ziffern[i,,]
}
bild <- matrix(0, nrow=iMax*28, ncol = jMax*28) # 5x20 Bilder soll Bild speichern

for(i in 1:iMax){
  for(j in 1:jMax){
    # Bilddaten in die Matrix bild kopieren
    bild[(1+(i-1)*28):(i*28), (1+(j-1)*28):(j*28)] <- ziffern[j+(i-1)*jMax,,]
  }
}
plot(as.raster(bild, max = 255))   
label


# c) Die einzelnen Ziffern anzeigen ###########
## 3x5 Raster

iMax <-  3   # Anzahl  Reihen
jMax <-  5   # Anzahl Spalten

ziffer <-  1   # muss für alle Ziffern gemacht werden
index <- which(label==ziffer)
#Anzahl der Bilder:
(nBilder <-  length(index))
bild <- matrix(255, nrow=iMax*28, ncol = jMax*28) 
for(nr in 1:length(index)){
  i <- (nr-1)%/%jMax+1
  j <- (nr-1)%%jMax+1
  bild[(1+(i-1)*28):(i*28), (1+(j-1)*28):(j*28)] <- ziffern[index[nr],,]
}
plot(as.raster(bild, max = 255))
label
str(bild)


