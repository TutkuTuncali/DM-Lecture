#Setting working directory
setwd("C:/Users/tutku/OneDrive/Documents/R Lectures/preprocessing")

install.packages("caTools") #Package installation
library(caTools)
library(readxl)

#github dataset import
Dataset <- read.csv("https://raw.githubusercontent.com/Daniel695/data/main/Dataset.csv", header=TRUE)

View(Dataset)

#Completing the missing value in the age column with the average
Dataset$Age = ifelse(is.na(Dataset$Age),
                     ave(Dataset$Age, FUN = function (x)mean(x, na.rm = TRUE)),
                     Dataset$Age)
View(Dataset)
#Completing the missing value in the Salary column with the average
Dataset$Salary = ifelse(is.na(Dataset$Salary),
                        ave(Dataset$Salary, FUN = function (x)mean(x, na.rm = TRUE)),
                        Dataset$Salary)
View(Dataset)

#Recoding categorical variables-Country
Dataset$Country = factor(Dataset$Country, 
                         levels = c('France','Spain','Germany'), 
                         labels = c(1.0, 2.0 , 3.0 ))

View(Dataset)

#Recoding categorical variables-Purchase
Dataset$Purchased = factor(Dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))
Dataset$Purchased[is.na(Dataset$Purchased)] <- 0
as.factor(Dataset$Purchased)

View(Dataset)

library(caTools)

set.seed(123) #for random selection -- Function creates reproducible results when writing code that 
#involves creating variables that take on random values like splits.

split = sample.split(Dataset$Purchased, SplitRatio = 0.8)

# Returns true if the observation goes to the training set, 
#false if it goes to the test set. Split ratio 0.8

#Definition of Training and test sets
training_set = subset(Dataset, split == TRUE)
test_set = subset(Dataset, split == FALSE)
training_set
test_set

#z-normalization for age and salary variables

training_set[, 2:3] = scale(training_set[, 2:3]) #2:3 shows which columns in the dataset will be processed
test_set[, 2:3] = scale(test_set[, 2:3])
training_set
test_set

#it can be useful to start off your batch script with the command 'rm(list=ls())' 
#just to make sure that you have a clean R environment before submitting the batch job.

rm(list = ls())

library(readxl)
data <- read_xlsx("data.xlsx")   #importing sample excel file in the same directory

View(data)

summary(data)

#log transformation

log_scale = log(as.data.frame(data))
View(log_scale)

#min-max transformation

install.packages("caret")
library(caret)
process <- preProcess(as.data.frame(data), method=c("range"))

norm_scale <- predict(process, as.data.frame(data))
View(norm_scale)

#robust scalar transformation

robust_scalar<- function(x)
{(x- median(x)) /(quantile(x,probs = .75)
                    -quantile(x,probs = .25))}

frame= (as.data.frame(data))
robust_scalar_data <- lapply(frame, robust_scalar)

head(robust_scalar_data)

#Mean Normalization

mean_norm_minmax <- function(x)
  {(x- mean(x)) /(max(x)-min(x))}

frame= (as.data.frame(data))

mean_normalize_data <- (lapply(frame, mean_norm_minmax))

head(mean_normalize_data)

#Unit length Normalization

unit_length <- function(x) {x / sqrt(sum(x^2))}

frame= (as.data.frame(data))
unit_length_data <- lapply(frame, unit_length)

head(unit_length_data)

#z-score Normalization

zscore_data <- scale(frame, center = TRUE, scale = TRUE)

head(zscore_data)

#Visualization

library(ggplot2)
ggplot() +   geom_point(data, mapping = 
                          aes(x=Experience, y=Salary), 
                        color='darkgreen')



#Converting the normalization scores into data frames for the graph 

minmax= (as.data.frame(norm_scale))
zscore= (as.data.frame(zscore_data))
robust= (as.data.frame(robust_scalar_data))
meannormal= (as.data.frame(mean_normalize_data))
unitlen= (as.data.frame(unit_length_data))
logarithm= (as.data.frame(log_scale))


#Comparison of the normalized scores

ggplot() + 
  geom_point(minmax, mapping = aes(x=Experience, y=Salary, color="Min-Max"))+
  geom_point(zscore, mapping = aes(x=Experience, y=Salary, color="Z-score")) +
  geom_point(robust, mapping = aes(x=Experience, y=Salary, color="Robust Scalar"))+ 
  geom_point(meannormal, mapping = aes(x=Experience, y=Salary, color="Mean Normalization")) + 
  geom_point(unitlen, mapping = aes(x=Experience, y=Salary, color="Unit Length Normalization")) + 
  #geom_point(logarithm, mapping = aes(x=Experience, y=Salary, color="Logarithm")) +
    scale_color_manual(name = "Comparison",
                     values = c( "Min-Max" = "blue", "Z-score" = "red", "Robust Scalar" = "darkgrey", "Mean Normalization" = "orange", 
                                 "Unit Length Normalization" = "black", "Logarithm" = "green"))

