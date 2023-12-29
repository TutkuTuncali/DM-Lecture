sampledata <- iris # load the iris dataset
head(sampledata)# first 6 observations
str(sampledata) # structure of dataset

#Descriptives
min(sampledata$Sepal.Length)
max(sampledata$Sepal.Length)
range(sampledata$Sepal.Length) # min and max values

# defining a function to get the actual range
range_calculation <- function(x) { 
  range <- max(x) - min(x)
  return(range)
}
  range_calculation(sampledata$Sepal.Length)
  
mean(sampledata$Sepal.Length) #average
median(sampledata$Sepal.Length) #median
quantile(sampledata$Sepal.Length, 0.25) #first quartile
quantile(sampledata$Sepal.Length, 0.75) #third quartile

IQR(sampledata$Sepal.Length)

sd(sampledata$Sepal.Length)

#to compute the descriptives
#of multiple variables at the same time, use lapply()
#The command dat[, 1:4] selects the variables 1 to 4
lapply(sampledata[, 1:4], sd)
lapply(sampledata[, 1:4], mean)

summary(sampledata)

#if you need these descriptive statistics by group 
#use the by() function
by(sampledata, sampledata$Species, summary)

#an alternative to get descriptives in a detailed form  
install.packages("pastecs")
library(pastecs)
stat.desc(sampledata)

#creating 2 subgroups according to size of the flowers
sampledata$size <- ifelse(sampledata$Sepal.Length 
                    < median(sampledata$Sepal.Length),
                   "small", "big")

#contingency table w/ table function
table(sampledata$size)
table(sampledata$Species, sampledata$size)

#contingency table w/ xtabs function
xtabs(~ sampledata$Species + sampledata$size)

#contingency table w/ proportions
prop.table(table(sampledata$Species, sampledata$size))
# round to 2 digits with round()
round(prop.table(table(sampledata$Species,sampledata$size),1),2)

# ----- PLOTS -----
#mosaic plot for contingency table
mosaicplot(table(sampledata$Species, sampledata$size),
           color = TRUE,
           xlab = "Species", # label for x-axis
           ylab = "Size" # label for y-axis
)

#bar chart for categorical variables
barplot(table(sampledata$size)) # table() is mandatory
barplot(prop.table(table(sampledata$size)))

#ggplot
library(ggplot2)
ggplot(sampledata) +  aes(x = size) +  geom_bar()

#histogram
hist(sampledata$Sepal.Length)

#ggplot
ggplot(sampledata) + aes(x = Sepal.Length) + geom_histogram()
#changing bin width
ggplot(sampledata) + aes(x = Sepal.Length) + geom_histogram(bins = 12)

#boxplot
# The IQR criterion means that all observations 
#above third quartile+1.5xIQR or 
#below first quartile-1.5xIQR 
#are considered as potential outliers by R
boxplot(sampledata$Sepal.Length)
boxplot(sampledata$Sepal.Length ~ sampledata$Species)

#ggplot
ggplot(sampledata) + aes(x = Species, y = Sepal.Length) +
  geom_boxplot()

#dotplot
install.packages("lattice")
library(lattice)

dotplot(sampledata$Sepal.Length ~ sampledata$Species)

#scatterplot
plot(sampledata$Sepal.Length, sampledata$Petal.Length)

#ggplot
ggplot(sampledata) +  aes(x = Sepal.Length, y = Petal.Length) +
  geom_point()
#representing different species in different colors
ggplot(sampledata) +
  aes(x = Sepal.Length, y = Petal.Length, colour = Species) +
  geom_point() +   scale_color_hue()

#line chart
plot(sampledata$Sepal.Length, type = "l") # "l" for line

#Q-Q Plot
# Draw points on the qq-plot:
qqnorm(sampledata$Sepal.Length)
# Draw the reference line:
qqline(sampledata$Sepal.Length)

#ggplot
install.packages("car")
library(car) # package must be installed first
qqPlot(sampledata$Sepal.Length)

# an alternative
install.packages("ggpubr")
library(ggpubr)
ggqqplot(sampledata$Sepal.Length)

#QQ plots by size
qqPlot(sampledata$Sepal.Length, groups = sampledata$size)

#different groups in different colors
qplot(
  sample = Sepal.Length, data = sampledata,
  col = Species, shape = Species
)

#Density Plot = Frequency Polygon
plot(density(sampledata$Sepal.Length))

#ggplot
ggplot(sampledata) + aes(x = Sepal.Length) +  geom_density()

#correlation matrix
cm <- sampledata [, c(1:4)]
round(cor(cm), 2)

remove.packages("dplyr")
install.packages("dplyr")
library(dplyr)

remove.packages("vctrs")
install.packages("vctrs")

remove.packages("purrr")
install.packages("purrr")


#correlogram
install.packages("ggcorrplot")
install.packages("ggstatsplot")
library(ggcorrplot)
library(ggstatsplot)
ggstatsplot::ggcorrmat(
  data = cm,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)
