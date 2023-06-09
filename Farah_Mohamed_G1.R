#reading data
alometry<- read.csv('G1_Allometry.csv')
alometry
str(alometry)

#Cleaning Data and numeric convertion for height values
alometry$height <-gsub(",",".",alometry$height )
alometry$height <-gsub('“',"",alometry$height )
alometry$height <-gsub('”',"",alometry$height )
alometry$height <- as.numeric(alometry$height)

#visualizing data for deep understanding
library(ggplot2)
ggplot(alometry, aes(x=height, y=diameter)) + geom_point()
#from the plot we found that height and diameter has direct relation so we fill messy data of heiht according to this relation
#recoding diameter variable to 3 categories according to its values
alometry$diameter[alometry$diameter < 30] ='low'
alometry$diameter[alometry$diameter >= 30 & alometry$diameter < 50] = 'medium'
alometry$diameter[alometry$diameter !='low' & alometry$diameter !='medium' ] ='high'

#Data Manipulation for NA values
med_low <- median(alometry[ alometry$diameter == 'low', 'height'], na.rm = T)
med_low
med_medium <- median(alometry[ alometry$diameter == 'medium', 'height'], na.rm = T)
med_medium
med_high  <- median(alometry[ alometry$diameter == 'high', 'height'], na.rm = T)
med_high
alometry[is.na(alometry$height) & alometry$diameter == 'high', 'height'] <-med_high
alometry[is.na(alometry$height) & alometry$diameter == 'medium', 'height'] <-med_medium
alometry[is.na(alometry$height) & alometry$diameter == 'low', 'height'] <-med_low
alometry$diameter <- as.factor(alometry$diameter)
diameterFactor <- factor(alometry$diameter, ordered = TRUE,levels=c("low", "medium", "high"))
#ensuring that there is no more NAs 
is.na(alometry)
complete.cases(alometry)
summary(alometry)
#visualizing data of height & diameter after data manipulation
ggplot(alometry, aes(x=height, y=diameter)) + geom_point()
  
#this diffrence between before and after is due to the conversion of diameter from numeric values to factor