# J. Willms, Nov 2019
# Modul: Programmierung für statistische Datenanalyse
#
# Quelltexte: R4DS: Exploratory Data Analysis
# siehe auch: https://r4ds.had.co.nz/exploratory-data-analysis.html






library(tidyverse)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 50))

 
# 7.3.4 Exercises
diamonds %>% select(x) %>% 
  ggplot() + 
  geom_histogram(mapping = aes(x = x), binwidth = 0.5)
  
diamonds  %>% 
  ggplot() + 
  geom_histogram(mapping = aes(x = x), binwidth = 0.5)

             

### 7.5 Covariation  ######
## 7.5.1 A categorical and continuous variable ######
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))


ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500, size = 1)

## JW: alternativ 
ggplot(diamonds, aes(x=price, stat(density))) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500, size = 1)


ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()


ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

str(reorder(mpg$class, mpg$hwy, FUN = median))



###  7.5.2 Two categorical variables  ####
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = fct_rev(color))) +
  xlab("Schliff") + ylab("Farbe (-> besser)")



diamonds %>%
  count(color, cut)

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))


### 7.5.3 Two continuous variables ##########

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))


ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 0.01)

ggplot(data = diamonds[1:100,]) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 0.01)


dd <- diamonds[1:10000,]

ggplot(data = dd) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 0.01)
dd

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))


ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

########  Zusammenfassung 7.5 ###############

### A  continuous variable bro

### A categorical and continuous variable
# geom_boxplot()  |  cat ~ cont:  Verteilung von cont

# geom_freqpoly() |  cat ~ "count"  |  price ~ count Überlagerung: color=cut
# geom_bar()      |  cat ~ "count"


### A categorical and categorical variable
#  geom_count()  | cat ~ cat:   count entspricht Punktgröße
#  count() %>%  geom_tile()   fill=n  

### A continuous and continuous variable
# geom_point()  |  cont ~ cont:   (Dichte der einzlnen Punkte)
# --- Using bins:
# geom_bin2d()                    (Farbe entspricht count)
# geom_hex()

# cont in cat umwandeln
# aes(group = cut_width(carat, 0.1))) 



######## 7.6 Patterns and models   ###############
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

 
mod <- lm(log(price) ~ log(carat), data = diamonds)
str(mod$residuals)


library(modelr)
diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

diamonds2

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))
ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))

##___________ JW_______ wieso log() ?

mod3 <- lm(price ~ carat, data = diamonds)
str(mod3$residuals)
diamonds3 <- diamonds %>% 
  add_residuals(mod3)  
diamonds3

ggplot(data = diamonds3) + 
  geom_point(mapping = aes(x = carat, y = resid))
ggplot(data = diamonds3) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))










