## Fallstudie 2 - words:

library(ggplot2movies)
library(stringr)

# Erstellen Sie eine Liste aller Wörter, die in der Variablen "title" gespeichert sind.
words <- unlist(str_split(movies$title, pattern = " "))

# Zählen Sie die Häufigkeit der einzelnen Wörter.
table_words <- as.data.frame(table(words))

# Welches Wort is am häufigsten vertreten?
table_words[which.max(table_words$Freq),]

# Substituieren Sie das von Ihnen am häufigsten bestimmte Wort durch den
# unbestimmten Artikel "A".
sub_words <- gsub("The", replacement = "A", words)

# Extrahieren Sie aus dem Vektor der modifizierten Wörter alle diejenigen,
# die mindestens einen Buchstaben besitzen. (Dadurch werden Zahlen und Symbole, 
# die als einzelne Wörter (character) abgespeichert sind, entfernt.)
words_letters <- sub_words[str_detect(sub_words, "[A-Z][a-z]")]

# Zählen Sie erneut die Häufigkeit der einzelnen Wörter.
as.data.frame(table(words_letters))


# Suchen Sie zufälligerweise fünf Wörter aus und verbinden diese zu einem Titel.
# Macht der Titel Sinn?
words_sample <- sample(words_letters, size = 5, replace = FALSE)

paste(words_sample, collapse = " ")