# import library
library(ggplot2)
library(dplyr)

# load data
district_crime <- read.csv('F:/DS/crime_by_district.csv', stringsAsFactors = FALSE)
head(district_crime)

# Check data types
sapply(district_crime, typeof)
# No need to convert into factors

# Bar plot
crime_2001 <- filter(district_crime, Year==2001 & STATE.UT == 'ANDHRA PRADESH')
p1 <- ggplot(crime_2001, aes(x=DISTRICT, y=Murder))
p2 <- p1 + theme_classic() + geom_bar(stat='identity') +
  labs(x = 'District names', title = 'State wise Murders in AP in 2001')
p2

district_crime$Other.Crimes.Against.SCs
prop.table(table(district_crime$Other.Crimes.Against.SCs))

# Plotting for 3 data
p1 <- ggplot(crime_2001, aes(x=DISTRICT, y=Murder, fill=Hurt))
p2 <- p1 + theme_classic() + geom_bar(stat='identity') +
  labs(x = 'District names', title = 'State wise Murders and Hurts in AP in 2001')
p2

# Data 1 vs Data 2
crime_2002 <- filter(district_crime, Year==2002 & STATE.UT == 'KARNATAKA')
plot(crime_2002$Hurt, crime_2002$Protection.of.Civil.Rights..PCR..Act, 
     xlab = "hurt", ylab = "Protection of Civil Rights (PCR) Act")

# Histogram_facet
crime_guntur <- filter(district_crime, DISTRICT=='GUNTUR')
ggplot(crime_guntur, aes(x=Hurt )) +
  geom_histogram(stat = 'count')+
  theme_bw()+
  facet_wrap(~ Murder ~ Year) +
  labs(title=' Year-wise Hurts in Guntur')

# Density plot
ggplot(crime_guntur, aes(x=Murder)) +
  geom_density()+
  theme_bw()+
  labs(title=' Murders in Guntur')

# Violin plot
ggplot(crime_guntur, aes(x=Murder, y=Hurt)) +
  geom_violin(trim = FALSE)+
  theme_bw()+
  labs(title=' Murders vs Hurts in Guntur')

