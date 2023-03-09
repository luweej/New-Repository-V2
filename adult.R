#Include Libraries--------------------------------------------------------------
library(tidyverse)
library(stringr)
library(caret)
library(rpart)
library(rpart.plot)
library(kernlab)


#Read in data ------------------------------------------------------------------

  #Set data source url
  data <- url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
  
  #Colnames url
  column_names <- url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names")
  
  #Make vector of column names to add to "adult"
  names <- read_delim(column_names, col_names = F, delim = ":" ,skip = 94)
  names <- as.vector(names$X1) 
  names <- names[2:length(names)]
  names <- gsub(":.*","",names)
  names <- c(names, "class")
  
  #Read in observations as csv, column names not included on this url
  adult <- read_csv(data, col_names = F, show_col_types = F) 
  
  #Add column names to adult
  colnames(adult) <- names

#Review and format Data---------------------------------------------------------
  glimpse(adult)
  summary(adult)
  
  adult[adult == "?"] <- NA
  
  test <- count(adult, is.na(adult))
  
  # filter the dataframe to include only individuals aged 25-65 
  adult <- adult[adult$age >= 25 & adult$age <= 65, ] 
  
  # drop missing values from the workclass and occupation columns 
  adult <- adult[!is.na(adult$workclass) & !is.na(adult$occupation), ] 
  
  # replace missing values in the native-country column with "unknown" 
  adult$`native-country`[is.na(adult$`native-country`)] <- "unknown"
  
  #Check for NA values
  colSums(is.na(adult))
  
  #Convert character type variables to factors
  adult <- adult %>% 
    mutate(across(where(is.character), as.factor))
  
  # Convert character columns to numbers
  adultNumeric <- adult %>% 
    mutate(across(where(is.factor), as.numeric, .names = "{.col}_numeric"))
  
  
  
  
# Classification--------------------------------------------------------------------

  set.seed(11111)
  
  trainList <- createDataPartition(y=adult$class, p=.8,list=F)
  
  trainSet <- adult[trainList,]
  testSet <- adult[-trainList,]
  
  svmModel <- train(class ~., data = trainSet, method = "rpart",
                    preProc=c("center", "scale"))