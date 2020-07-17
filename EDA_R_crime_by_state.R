## import library
library(ggplot2)
library(psych)

## read data
state_crime <- read.csv('F:/DS/crime_by_state.csv')
head(state_crime)
headTail(state_crime,6)
names(state_crime)

## Dimension of data
dim(state_crime)

## Count NaN in data
colSums(sapply(state_crime, is.na))

## Some data statistics summary
str(state_crime)
summary(state_crime)
## We have data from 2001 to 2012

## Scatterplot Matrix of variables
plot(state_crime)

## Filtering only 2001 data
crime_2001 <- filter(state_crime, Year==2001)

## Plotting State wise Murders in 2001
library(tidyverse)
ggplot(crime_2001, aes(x=STATE.UT, y=Murder))+
geom_bar(stat = "identity")+
ggtitle('State-wise Murders in 2001')

## Since it is difficult to read the State names, we shall consider the states
## with more murders
crime_2001_more <- filter(state_crime, Year==2001 & Murder>100)
ggplot(crime_2001_more, aes(x=STATE.UT, y=Murder))+
  geom_bar(stat = "identity")+
  ggtitle('State-wise Murders more than 100 in 2001')

## UP having more Murders. We shall delete Total (All_INDIA) 
count(state_crime, vars=c('STATE.UT'))
data <- state_crime[-c(456, 455, 454, 453, 452, 451, 450, 449, 448, 447, 446, 445),]
count(data, vars=c('STATE.UT'))

## Find the index numbers having Total (STATES)
ind <- which(data[,1] == 'TOTAL (STATES)')
ind

## Delete the rows having TOTAL (STATES)
data <- data[-c(337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348),]
headTail(data)

## We shall also delete TOTAL (UTs) rows
ind1 <- which(data[,1] == 'TOTAL (UTs)')
ind1
data <- data[-c(421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432),]
headTail(data)

## Now Plot State_wise murders in 2012
ind2 <- which(data[,2] == 2012)
data1 <- data[c(ind2),]
library(tidyverse)
ggplot(data1, aes(x=STATE.UT, y=Murder))+
  geom_bar(stat = "identity", color='blue', fill='orange')+
  ggtitle('State-wise Murders in 2012')

## Plotting Assault on women in AP
ind2 <- which(data[,1] == 'ANDHRA PRADESH')
data1 <- data[c(ind2),]
ggplot(data1, aes(x=Year, y=Assault.on.women))+
  geom_point()+
  ggtitle('Assault on women in AP')

## Plotting Dacoity
ggplot(data, aes(x=STATE.UT, y=Dacoity))+
  geom_violin(scale = "width", color='#56B4E9')+
  ggtitle('Dacoity')+
  stat_summary(fun=mean, geom="point", shape=23, size=2)

## 3 variables plot
ind2 <- which(data[,2] == 2010)
data1 <- data[c(ind2),]
ggplot(data1, aes(y=Year, x=STATE.UT, z=Robbery))+
  geom_contour()+
  ggtitle('Contour Plot')
## Unable to plot Contour Plot as the data is not suitable

## Histogram
ind2 <- which(data[,2] == 2010)
data1 <- data[c(ind2),]
ggplot(data1, aes(x=Hurt))+
  geom_histogram(bins = 20)+
  ggtitle('Histogram')