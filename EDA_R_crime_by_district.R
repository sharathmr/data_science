## import library
library(ggplot2)
library(psych)

## read data
district_crime <- read.csv('F:/DS/crime_by_district.csv')
head(district_crime)
headTail(district_crime,6)
names(district_crime)

## Dimension of data
dim(district_crime)

## Count NaN in data
colSums(sapply(district_crime, is.na))

## Some data statistics summary
str(district_crime)
summary(district_crime)
## We have data from 2001 to 2012

## Scatterplot Matrix of variables
plot(district_crime)

## Filtering only 2001 data
crime_2001 <- filter(district_crime, Year==2001)

## Plotting District wise Murders in 2001
library(tidyverse)
ggplot(crime_2001, aes(x=DISTRICT, y=Murder))+
  geom_bar(stat = "identity")+
  ggtitle('District-wise Murders in 2001')

## Since it is difficult to read the District names, we shall consider the Districts
## with less murders
crime_2001_less <- filter(district_crime, Year==2001 & Murder<2)
head(crime_2001_less)
ggplot(crime_2001_less, aes(x=DISTRICT, y=Murder))+
  geom_bar(stat = "identity")+
  ggtitle('District-wise Murders less than 2 in 2001')

## Find the index numbers having Total (STATES) and Districts
ind <- which(district_crime[,2] == 'TOTAL (DISTRICTs)')
ind
ind <- which(district_crime[,1] == 'TOTAL (STATES)')
ind

## Now Plot District_wise murders in 2001
ind2 <- which(district_crime[,3] == 2001 & district_crime[,1] == 'ARUNACHAL PRADESH')
data1 <- district_crime[c(ind2),]
library(tidyverse)
ggplot(data1, aes(x=DISTRICT, y=Murder))+
  geom_bar(stat = "identity", color='blue', fill='orange')+
  ggtitle('District-wise Murders in 2012')

## Plotting Assault on women in AP
ind2 <- which(district_crime[,1] == 'ANDHRA PRADESH')
data1 <- district_crime[c(ind2),]
ggplot(data1, aes(x=Year, y=Assault.on.women))+
  geom_point()+
  ggtitle('Assault on women in AP')

## Plotting Dacoity
ggplot(district_crime, aes(x=DISTRICT, y=Dacoity))+
  geom_violin(scale = "width", color='#56B4E9')+
  ggtitle('Dacoity')+
  stat_summary(fun=mean, geom="point", shape=23, size=2)

## 3 variables plot
ind2 <- which(district_crime[,3] == 2010 & district_crime[,1] == 'ANDHRA PRADESH')
data1 <- district_crime[c(ind2),]
ggplot(data1, aes(y=Dacoity, x=DISTRICT, z=Hurt))+
  geom_contour()+
  ggtitle('Contour Plot')
## Unable to plot Contour Plot

## Histogram
ind2 <- which(district_crime[,3] == 2010)
data1 <- district_crime[c(ind2),]
ggplot(data1, aes(x=Hurt))+
  geom_histogram(bins = 20)+
  ggtitle('Histogram')