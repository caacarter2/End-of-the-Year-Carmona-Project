# End-of-the-Year-Carmona-Project
Using PGA Tour Data
#FinalProject for Carmona

#Linear and Logistic PGA Tour

#library(readxl)

PGATOUR<-read_excel("C:/Users/frogp/Downloads/CombinedPGATour.Carmona.420.xlsx")

View(PGATOUR)

error uploading dataset, change BBR to BPR

colnames(PGATOUR)[6]<- "BPR"

#Let's get a summary of the data prior to partitioning

summary(PGATOUR)

#convert characters into factors

PGATOUR$EQP <- as.factor(PGATOUR$EQP)

#open the dataset PGATOUR

#attach(PGATOUR)

#str(PGATOUR)

#head(PGATOUR)

range(PGATOUR$Wins)

library(tree)

library(caret)

#convert wins to "Yes" if wins are greater than or = 1

# if wins are less than 1 then it is "No"

victory <- ifelse(PGATOUR$Wins >=1, "Yes", "No")

#add the field victory to the PGATOUR dataset

PGATOUR <- data.frame(PGATOUR, victory)

#convert the field victory to factor

PGATOUR$victory <- as.factor(PGATOUR$victory)

head(PGATOUR)

#delete the numeric variable "Wins" from the data set

#remember that we replaced it with the categorical variable "victory"

PGATOUR <- PGATOUR[,-10]

names(PGATOUR)

str(PGATOUR)

#Split data set

set.seed(2)

train <- sample(1:nrow(PGATOUR), nrow(PGATOUR)*0.70)

validation <- -train

training_data <- PGATOUR[train,]

validation_data <- PGATOUR[validation,]

#create a variable to hold the actual values of the validation database

#this variable will be used latter for the confusion matrix

test_victory = victory[validation]

#create the classification tree with the training data set

#we are setting "victory" as the dependent variable

tree_model = tree(victory ~., training_data )

#plot the tree

plot(tree_model, type="uniform")

#show the text for the tree

#the pretty=0 option will show the actual values of the factors

text(tree_model, pretty=0)

#Check how the model is doing using the validation data

tree_pred <- predict(tree_model, validation_data, type="class")

mean(tree_pred != test_victory)

#create the confusion matrix with the prediction

test_victory <- as.factor(test_victory)

cm <- confusionMatrix(data=tree_pred, reference = test_victory)

cm

library(InformationValue)

library(caret)

PGATOUR_2<-PGATOUR

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(PGATOUR_2), replace=TRUE, prob=c(0.7,0.3))


train_2 <- PGATOUR_2[sample, ]


test_2 <- PGATOUR_2[!sample, ]


model <- glm(victory~SCAVG + GIR + BPR + PPR, data = train_2, family = "binomial")
summary(model)


predicted <- predict(model, test_2, type="response")


test_2$victory <- ifelse(test_2$victory=="Yes", 1, 0)


optimal <- optimalCutoff(test_2$victory, predicted)[1]


cm_2<-confusionMatrix(test_2$victory, predicted)
cm_2
confusionMatrix(cm_2)
#Can't get the confusion matrix to give me a table with accuracy sens, spec etc
#says predictedscores is missing
