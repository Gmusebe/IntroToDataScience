# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
library(tibble)
test <- add_column(test, data.frame(Survived = rep("None", nrow(test))), .after = 1)

# Combine data sets
data.combined <- rbind(train, test)

# A bit about R data types
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Name <- as.factor(data.combined$Name)
data.combined$Sex <- as.factor(data.combined$Sex)

# Check  for the missing data
visdat::vis_dat(data.combined)
#  Check the measure of data missing
naniar::vis_miss(data.combined)

# Primary View
# Gross survival rates
library(epiDisplay)
tab1(data.combined$Survived, col=c("chocolate","brown1","brown4"))
# Most people perished than survived.

# Distribution of persons across classes
tab1(data.combined$Pclass, col=c("chocolate","brown1","brown4"))


library(ggplot2)
# Hypothesis - Rich folks survived at a higer rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


# Examine Names in the training data set
train$Name <- as.character(train$Name)
# How many unique names are there both in the train & test data?
length(unique(data.combined$Name))

# Titles Mr., Mrs., .....
library(stringr)
name.splits <- str_split(data.combined$Name, ",")
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles )

titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)
Title <- as.factor(titles)
data.combined <- add_column(data.combined, Title, .after = 3)

# Use the first 891 rows that  have surviavl labels
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# The distribution of females to males across train & test?
tab1(data.combined$Sex, col=c("chocolate","brown1"))


# Visualize the 3-way relationship of sex, pclass, and survival, compare to analysis of title
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")
# Survial rates deccreased across the Pclass among all the genders.


# OK, age and sex seem pretty important as derived from analysis of title, let's take a closer 
# look at the distibutions of age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])
# Most of the age data is missing in the train data


# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")
# There is a decrease in survival rates by gender across the Pclass


# SibSp variable, summarize the variable
summary(data.combined$SibSp)
length(unique(data.combined$SibSp))
# Change to factor
data.combined$SibSp <- as.factor(data.combined$SibSp)


# We believe title is predictive. Visualize Survival rates by SibSp, Pclass, and Title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat the Parch variable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
family.size <- as.factor(temp.sibsp + temp.parch + 1)
data.combined <- add_column(data.combined, family.size, .after = 9)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
# The greater the family size the less the survival rate.
# From the above analysis we will use the Pclass, Title(Combining Sex and Age), SibSp, Parch and family.size for the exploratory analysis.

# Fare
# Take a look at some of the high fares
indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")
View(data.combined[indexes,])


ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
Tickets <- unique(data.combined$Ticket)

for (i in 1:length(Tickets)) {
  current.ticket <- Tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined <- add_column(data.combined, ticket.party.size, .after = 10)
data.combined <- add_column(data.combined, avg.fare, .after = 12)

summary(data.combined$avg.fare)
data.combined[is.na(data.combined$avg.fare), ]

indexes <- with(data.combined, which(Pclass == "3" & Title == "Mr." & family.size == 1 &
                                       Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

data.combined[is.na(avg.fare), "avg.fare"] <- 7.840


library(tidyverse)
library(caret)
library(randomForest)

features <- c("Survived", "Pclass", "Title", "family.size", "ticket.party.size", "avg.fare")
Train.data <- droplevels(data.combined[1:891, features])
Test.data <- data.combined[892:1309, c("Pclass", "Title", "family.size", "ticket.party.size", "avg.fare")] 

train.Control <- trainControl(method = "LOOCV")
model <- train(Survived ~., data = Train.data, method = "rf", trControl = train.Control)
print(model)

predict <- model %>% predict(Test.data)

My_Pred <- data.frame(PassengerId = rep(892:1309), Survived = predict)

write.csv(My_Pred, file = "RF_SUB_20200602_6.csv", row.names = FALSE)