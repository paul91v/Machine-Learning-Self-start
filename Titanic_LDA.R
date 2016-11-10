titanic = read.csv("G:/Kaggle titanic data/train.csv")
KaggleTest = read.csv("G:/Kaggle titanic data/test.csv")
KaggleTest$Survived = NA
full = rbind(titanic, KaggleTest)
View(full)
library(caret)
library(rpart)

#Summarizing the Data
dim(full)
sapply(full, class)

#Preparing the Data
#Counting the missing data
sapply(full, function(x)sum(is.na(x)))
full$Fare[is.na(full$Fare)] = median(full$Fare[Pclass = 3], na.rm = TRUE)
#Finding the Titles of all the passengers
full$Name = as.character(full$Name)
full$title = sapply(full$Name, function(x)strsplit(x, split = '[,.]')[[1]][2])
full$title = sub(' ', '', full$title)
table(full$title)
full$title[full$title %in% c('Don','Major','Sir','Col','Jonkheer')] = 'Sir'
full$title[full$title %in% c('Mlle','Miss','Ms')] = 'Miss'
full$title = factor(full$title)

#predicting the missing ages from building a Tree
missing_Age = rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + title, data = full[!is.na(full$Age),], method = "anova")
full$Age[is.na(full$Age)] = predict(missing_Age, full[is.na(full$Age),])

#Calculating Family Size
full$Survived = as.factor(full$Survived)
full$FamilySize = full$Parch + full$SibSp + 1

#Extracting FamilyNames
full$surname = sapply(full$Name, function(x)strsplit(x, split = '[,.]')[[1]][1])
full$FamilyID = paste(as.character(full$FamilySize), full$surname, sep = "")
full$FamilyID[full$FamilySize <= 3] <- 'Small'

#Splitting the data into test and training set
titanic = full[1:891,]
KaggleTest = full[892:1309,]
#trainIndex = sample(1:nrow(titanic), 0.75*nrow(titanic))
#titanic = titanic[trainIndex,]
#Testdata = titanic[-trainIndex,]
#(Removing Error checking on 07-11)

#Visualizing the Data
plot(titanic$Age, titanic$Survived)
prop.table(table(titanic$Survived, titanic$Sex))

#Evaluating LDA
control <- trainControl(method = 'cv', number = 20)
#(cv20 produced 0.799, cv5 0.77)
metric <- 'Accuracy'
#titanic.lda = train(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title, data = titanic,method = "lda", metric = metric, trControl = control)
titanic.cart = train(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + FamilySize, data = titanic,method = "rpart", metric = metric, trControl = control)
titanic.knn = train(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + FamilySize, data = titanic,method = "knn", metric = metric, trControl = control)
titanic.svm = train(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + FamilySize, data = titanic,method = "svmRadial", metric = metric, trControl = control)
titanic.rf = train(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + FamilySize, data = titanic,method = "rf", metric = metric, trControl = control)

result = resamples(list(cart = titanic.cart, knn = titanic.knn, svm = titanic.svm, rf = titanic.rf))
summary(result)
dotplot(result)
print(titanic.rf)

#Predicting on Test Data
#prediction = predict(titanic.rf, Testdata)
#confusionMatrix(prediction, Testdata$Survived)
#(removing error checking on 07-11)

#Calculating the values for Kaggle Test data
KagglePredict = predict(titanic.cart, KaggleTest)


#Exporting the Solution
KaggleSubmit = data.frame(KaggleTest$PassengerId, KagglePredict)
colnames(KaggleSubmit) = c("PassengerId","Survived")
write.csv(KaggleSubmit, file = "G:/Kaggle titanic data/titanic-cart-cv20-fe-09-11 .csv", row.names = FALSE)
