setwd("D:/Documents/Hackathon/Case_Onsite_Modeling_Wine")


## load the necessay packages

library(ggplot2)
library(dplyr)
library(corrplot)

wine = read.csv("wine_dataset.csv", stringsAsFactors = FALSE)

str(wine)

wine$style<- as.factor(wine$style)
wine$style<-as.numeric(wine$style)

str(wine)
## Correaltion Matrix
data_mat<-cor(wine)
corrplot(data_mat, method = "circle")

## Top contributor in determing the quality of wine is 
##"alcohol","density","chlorides" and volatile_acidity


## ggplot() for alcohol vs density for both red and white wines.

ggplot(wine, aes(x = quality, y = alcohol, col = factor(style))) +
  geom_point()+geom_jitter()

ggplot(wine, aes(x = alcohol, y = density, col = factor(style))) +
  geom_point()+ # Copy from Plot 2
  geom_smooth(aes(group = 1), method = "lm",se =FALSE,linetype = 2)+geom_jitter()

ggplot(wine, aes(x = quality, y = chlorides, col = factor(style))) +
  geom_point()+geom_jitter(width = 0.6)+geom_boxplot()

ggplot(wine, aes(x = quality, y = volatile_acidity, col = factor(style))) +
  geom_jitter()+geom_smooth(method = "loess")

## Histogram

ggplot(wine, aes(quality, fill = factor(style), col = factor(style))) + 
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.4)+
  xlim(c(0,10))


## Classification: red or white wine





















