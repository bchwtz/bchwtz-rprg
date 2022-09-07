# J. Willms, August 2019
# Programmierung für statistische Datenanalyse
#
# Quelltexte: Fallstudie 3: Visualisierung mit ggplot3 (Kapitel 6)
# Ergänzungen

library(ggplot2)
#___________________________________________________________________________
## t-Verteilung mit n-1 Freiheitsgraden: 
## x "normierter" Mittelwert der Summe von iid mit  Mittelwert 0 und unbekannte Varianz
##      Bei der "Normierung" wurde statt der unbekannten Varianz, die durch
#       sd() geschätzte Streuung nutzt
## DANN: mean(x)/sd(x) * sqrt(length(x)) ist t-verteilt mit n-1 Freiheitsgraden

# n: Anzahl der Freiheitsgrade

rnormalMean <- function(n, sd=1) {
  x <- rnorm(n, sd=sd)
  return ( mean(x)/sd(x) * sqrt(length(x)))  
}


zeichne4 <- function(df,xsigma, n){ 
  supStr <- sprintf("anzahl=%d, n=%d,  Summe normalverteilter Werte", length(df$x), as.integer(n)) 
  p <- ggplot(df, aes(x)) +
    geom_histogram(aes(x, stat(density)), binwidth=xsigma/15, alpha=0.4)+
    geom_density(color="blue", size =1.2) +
    labs(title = "Konvergenz gegen die Normalverteilung?", subtitle = supStr) +
    stat_function(fun = dt, args= list(df=n-1), 
                  colour = "red", size = 1.1)+
    stat_function(fun = dt, args= list(df=n), 
                  colour = "green")+
    stat_function(fun = dnorm, args= list(sd=xsigma/sqrt(n)), 
                  colour = "cyan") +
    theme_bw()
  return(p)
}


set.seed(1)
n <- 4
xsigma <- 4  # "unbekannte" Streung
Anzahl <- 30    # gibt an, wie oft sn realisiert werden soll 
s <- replicate(Anzahl, rnormalMean(n, sd=xsigma))
zeichne4(data.frame(x=s), xsigma, n)
 


# mean(s)     # sollte ca. 0 sein  
# var(s) # sollte ca. 1 sein 
