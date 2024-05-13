## ----setup, include=FALSE-----------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----packages, echo=TRUE, message=FALSE, warning=FALSE------
# load the packages needed
library(PASWR2)
library(ggplot2)
library(tidyverse)
library(lattice)


## ----data description---------------------------------------
help("TITANIC3")
data("TITANIC3")


## ----dimentions---------------------------------------------
glimpse(TITANIC3)


## -----------------------------------------------------------
TITANIC3 %>% tail(6)

TITANIC3 %>% head



## -----------------------------------------------------------
TITANIC <- TITANIC3 %>% mutate(survived = factor(survived, levels = 0:1, labels = c("No", "Yes")))


## -----------------------------------------------------------
summary(TITANIC)



## -----------------------------------------------------------
TITANIC %>% summary()



## ----part-a-------------------------------------------------
T1 <- xtabs(~survived + pclass, data = TITANIC)

T1 <- table(TITANIC$survived,TITANIC$pclass)

T1 <- TITANIC %>% select(survived, pclass) %>% table()

T1

prop.table(T1, margin = 2) # to produce the proportion per column (2), per row would be margin = 1


## ----part-b-------------------------------------------------
T2 <- TITANIC %>% select(pclass, sex, survived) %>% table()

T2

prop.table(T2)



## ----part-c-------------------------------------------------
# Finding summary statistics

median(TITANIC$age, na.rm = TRUE) # old style 
mean(TITANIC$age, na.rm = TRUE) # old style

# dplyr style
TITANIC %>% summarise(mean = mean(age, na.rm = TRUE), median = median(age, na.rm = TRUE))

# IQR(TITANIC$age, na.rm = TRUE) 

TITANIC %>% pull(age) %>% IQR(na.rm = TRUE) # pull() does extract the column from the data frame as vector object

# look at the density function to see if it is uni or bi-modal distribution
ggplot(data = TITANIC, aes(x = age)) +
geom_density(fill = "lightgreen") +
theme_bw()



## -----------------------------------------------------------
# mean summaries
TITANIC %>% group_by(sex, survived) %>% summarise(avg = mean(age, na.rm = TRUE))



## -----------------------------------------------------------
# sd summaries
TITANIC %>% group_by(sex, survived) %>% summarise(stdev = sd(age, na.rm = TRUE))



## -----------------------------------------------------------
# median summaries
TITANIC %>% group_by(sex, survived) %>% summarise(med = median(age, na.rm = TRUE))



## -----------------------------------------------------------
# IQR summaries
TITANIC %>% group_by(sex, survived) %>% summarise(IQR = IQR(age, na.rm = TRUE))



## -----------------------------------------------------------
# mean summaries
TITANIC %>% group_by(pclass, sex, survived) %>% summarise(avg = mean(age, na.rm = TRUE))

# median summaries

TITANIC %>% group_by(pclass, sex, survived) %>% summarise(med = median(age, na.rm = TRUE))

# standard deviation
TITANIC %>% group_by(sex, survived) %>% summarise(stdev = sd(age, na.rm = TRUE))

#IQR
TITANIC %>% group_by(sex, survived) %>% summarise(IQR = IQR(age, na.rm = TRUE))


## -----------------------------------------------------------
TITANIC %>% filter (sex =="female" & survived =="Yes" & pclass == "1st") %>% arrange(desc(age))


## -----------------------------------------------------------
TITANIC %>% filter(sex =="female" & survived =="Yes" & pclass == "1st") %>% arrange(desc(age))

TITANIC %>% filter (sex =="male" & survived =="Yes" & pclass == "1st") %>% arrange(desc(age))


## ----part extra---------------------------------------------

TITANIC %>%  ggplot(aes(x = survived)) +
  geom_bar(aes(fill = sex), stat = "count", position = "stack" ) +
  theme_bw()

TITANIC %>%  ggplot(aes(x = survived)) +
  geom_bar(aes(fill = pclass), stat = "count", position = "stack" ) +
  theme_bw()



## ----part 1 extra_credit------------------------------------
CLEAN <- na.omit(TITANIC)

#print the dimensions
dim(CLEAN)


## ----part 2 extra_credit------------------------------------
#get the number of missing values in columns
colNAs<- colSums(is.na(TITANIC))
(colNAs <- as.vector(colSums(is.na(TITANIC)))) # coerce to a vector 

rowNAs <- table(rowSums(is.na(TITANIC)))
(rowNAs <- as.vector(table(rowSums(is.na(TITANIC))))) # coerce to a vector


## ----save---------------------------------------------------
write.csv(CLEAN, file="TITANIC_CLEAN.csv", row.names=FALSE)


## -----------------------------------------------------------

set.seed(123)

library(leaflet)

# The code below will create list of 5 UNC university data points with lat & lng, name and school size

UNC_schools <- data.frame(name = c("NC State", "UNC Chapel Hill", "FSU", "ECU", "UNC Charlotte"),
                        size = c(30130, 28136, 6000, 25990, 25990),
                        lat = c(36.0373638, 35.9050353, 35.0726, 35.6073769, 35.2036325),
                        lng = c(-79.0355663, -79.0477533, -78.8924739, -77.3671566, -80.8401145))


# Use the data frame to draw map and circles proportional to the school sizes of the cities
UNC_schools %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(weight = 1, radius = sqrt(UNC_schools$size)*100)



## -----------------------------------------------------------
set.seed(234)

library(leaflet)

UNC_schools <- data.frame(name = c("NC State", "UNC Chapel Hill", "FSU", "ECU", "UNC Charlotte", "NC Central University", "UNC Pembroke" ),
                        size = c(30130, 28136, 6000, 25990, 25990, 7965, 5643),
                        lat = c(36.0373638, 35.9050353, 35.0726, 35.6073769, 35.2036325, 35.975266, 34.6899),
                        lng = c(-79.0355663, -79.0477533, -78.8924739, -77.3671566, -80.8401145, -78.899734, -79.2006))

UNC_schools %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(weight = 1, radius = sqrt(UNC_schools$size)*50)


## ----echo=FALSE---------------------------------------------
## DO NOT CHANGE ANYTHING IN THIS CODE CHUNK!
date()
sessionInfo()
R.Version()


