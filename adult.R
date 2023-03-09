#Include Libraries
library(tidyverse)
library(stringr)
#Read in Data

#Set data source url
data <- url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")

#Colnames url
column_names <- url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names")

#Make vector of column names to add to "adult"
names <- read_delim(column_names, col_names = F, delim = ":" ,skip = 94)
names <- as.vector(names$X1) 
names <- names[2:length(names)]
names <- gsub(":.*","",names)
names <- c(names, "overunder50k")

#Read in observations as csv, column names not included on this url
adult <- read_csv(data, col_names = F, show_col_types = F) 

#Add column names to adult
colnames(adult) <- names

#Review Data
str(adult)
summary(adult)

adult[adult == "?"] <- NA

test <- count(adult, is.na(adult))

#adultClean <- adult %>%
#  filter(age>=25 & age<=65)

# filter the dataframe to include only individuals aged 25-65 
adult <- adult[adult$age >= 25 & adult$age <= 65, ] 

# drop missing values from the workclass and occupation columns 
adult <- adult[!is.na(adult$workclass) & !is.na(adult$occupation), ] 

# replace missing values in the native-country column with "unknown" 
adult$`native-country`[is.na(adult$`native-country`)] <- "unknown"
