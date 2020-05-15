#
# Copyright 2012 Dave Langer
#    
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#  	http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 



#
# This R source code file corresponds to video 1 of the YouTube series
# "Introduction to Data Science with R" located at the following URL:
#     http://www.youtube.com/watch?v=32o0DnuRjfg
#



# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
library(tibble)
test <- add_column(test, data.frame(Survived = rep("None", nrow(test))), .after = 1)

# Combine data sets
data.combined <- rbind(train, test)

# A bit about R data types (e.g., factors)
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Name <- as.factor(data.combined$Name)

# Check  for the missing data
visdat::vis_dat(data.combined)
#  Check the measure of data missing
naniar::vis_miss(data.combined)

# Take a look at gross survival rates
library(epiDisplay)
tab1(data.combined$Survived, col=c("chocolate","brown1","brown4"))


# Distribution across classes
tab1(data.combined$Pclass, col=c("chocolate","brown1","brown4"))


# Load up ggplot2 package to use for visualizations
library(ggplot2)


# Hypothesis - Rich folks survived at a higer rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


# Examine the first few names in the training data set
train$Name <- as.character(train$Name)
head(train$Name)


# How many unique names are there across both train & test?
length(unique(data.combined$Name))


# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(data.combined$Name)), "Name"])
# There are no duplicates in the data


# What is up with the 'Miss.' and 'Mr.' thing?
# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.
library(stringr)
Title <- as.factor(str_extract(string = data.combined$Name, pattern = "(Mr|Master|Mrs|Miss)\\."))
data.combined <- add_column(data.combined, Title, .after = 3)

# Any correlation with other variables (e.g., sibsp[Siblings/Spouse])?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]


# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(data.combined$Title == "Mrs."), ]
mrses[1:5,]


# Check out males to see if pattern continues
males <- data.combined[which(data.combined$Sex == "male"), ]
males[1:5,]

# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")