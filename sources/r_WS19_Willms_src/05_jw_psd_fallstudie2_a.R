# J. Willms, November 2019
# Skript: Programmierung für statistische Datenanalyse

# Quelltexte: Fallstudie IMDb (Kapitel 5)

###  1.1	imdb_Bewertungen ###### 
imdbData <- readRDS("imdbData.rds")
wortIndex <- readRDS("wortIndex.rds")
str(imdbData)
str(wortIndex)

imdb_Bewertungen <- imdbData$x
imdb_Ergebnisse <- imdbData$y
str(imdb_Ergebnisse)

str(imdb_Bewertungen) 
length(imdb_Bewertungen)
head(imdb_Bewertungen , n= 4)

### a)      ###############
sapply(imdb_Bewertungen, max)
max(sapply(imdb_Bewertungen, max))
min(sapply(imdb_Bewertungen, min))
 

###  1.2 wortIndex ######
length(wortIndex)
#str(wortIndex)    # zu viel Listenelemente
head(wortIndex , n= 4)
head(names(wortIndex), n=20)

# wortIndex: Benannte Liste mit 88584 Elementen
#  jedes Listenelement besteht aus einem Int-Wert (integer-Vektor der Länge 1)
wortIndex[[1]]
wortIndex$fawn

#### Häufigsten Worte 
wortIndex[wortIndex == 1]
wortIndex[wortIndex == 2]


#####  b) Größter und kleinster Index ####

# b1)	Was ist der kleinste und was ist der größte Wort-Indexwert?  
# b2) Ist jeder Wert  zwischen diesen beiden Größen ein Wort-Indexwert?

indexVektor <- unlist(wortIndex)
str(names(indexVektor))
max(indexVektor)
min(indexVektor)
range(indexVektor)
length(indexVektor)
length(unique(indexVektor)) 

# b)	Was ist der kleinste und was ist der größte Wort-Indexwert?  
# 1,  88584
# Ist jeder Wert  zwischen diesen beiden Größen ein Wort-Indexwert?
# JA


#####  c) topWorte[]  ####
# c)	Erstellen Sie einen character-Vektor topWorte, 
# der die hundert häufigsten Worte enthält, geordnet nach der Häufigkeit:
topWorte <- indexVektor[indexVektor <= 100]
topWorte <- sort(topWorte)
topWorte <-names(topWorte)  # topWorte ist nun ein unbenannter Vektor
topWorte

indexVektor[1]
str(indexVektor[1])
indexVektor["fawn"]    # ==> 34701    # gibt Index von "fawn" zurück
# Wir betrachten den named vector indexVektor nun als "Dictionary<string, int>" (bzw. map) 
# key = "fawn"   => value = Index von "fawn": c

# Da es eine 1:1 Beziehung zwischen key und value gibt können wir das "Dictionary"
# auch umdrehen:  key = "34701"   => value ="fawn"  


###  d) wortVektor   #####
wortVektor <-names(indexVektor)
names(wortVektor) <- indexVektor
wortVektor[1]
str(wortVektor[1])
wortVektor["34701"]

str(wortVektor)
head(wortVektor, n=20)
wortVektor[as.character(34701)]
unname(wortVektor[as.character(34701)])

wortVektor[as.character(34701)]  # ==> "fawn"
identical(unname(wortVektor[as.character(34701)]), "fawn")

indexVektor[1]
unname(indexVektor[1])


#### e) getWort()  ###############
getWort <- function(index) { 
    return(unname(wortVektor[as.character(index)]))
}

i <- 1
bewertung <- imdb_Bewertungen[[i]][-1] -3 

for(j in 1:10){
  print(getWort(bewertung[j]))
}


### f)  Decodieren I   ############
for(i in 1:5){ 
  bewertung <- imdb_Bewertungen[[i]][-1] -3 
  w <- getWort(bewertung)
  w[is.na(w)] <- "?"
  text <- paste(w, collapse = " ") 
  print(text)
  print("------------------------------")
}


### g)  Sortieren         #######################
# Alternativ: Mit Sortieren 
worte <- names(indexVektor)[order(indexVektor)]
str(worte[1])
str(worte)
head(worte, n=100)


##### h)  Decodieren II   ############
for(i in 1:5){ 
  bewertung <- imdb_Bewertungen[[i]][-1] -3 
  bewertung[bewertung < 1 | bewertung > length(worte)] <- NA
  w <- worte[bewertung]
  w[is.na(w)] <- "?"
  text <- paste(w, collapse = " ") 
  print(text)
  print("------------------------------")
}


##### i)  One-Hot-Codierung   ############
i <- 1
bewertung <- imdb_Bewertungen[[i]] 

nSpalten <- max(sapply(imdb_Bewertungen, max)) # siehe a)

length(imdb_Bewertungen)    # 100 : Anzahl der Zeilen in C 
 
C <- matrix(0, nrow = length(imdb_Bewertungen), ncol = nSpalten)

for (i in 1:length(imdb_Bewertungen)){
  C[i, imdb_Bewertungen[[i]]] <- 1
}

str(C)

C[1:8, 1:16]
C[1:8, 501:516]






