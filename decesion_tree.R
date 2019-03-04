install.packages("party")
library(party)
str(iris)
View(iris)
set.seed(1234) #To get reproducible result
ind <- sample(2,nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)
train_predict <- predict(iris_ctree,trainData,type="response")
table(train_predict,trainData$Species)
mean(train_predict != trainData$Species) * 100

test_predict <- predict(iris_ctree, newdata= testData,type="response")
table(test_predict, testData$Species)
mean(test_predict != testData$Species) * 100
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")
summary(iris_ctree)


##### comapany data set decision tree#####
install.packages("party")
library(party)
 company <- read.csv(file.choose())
View(company)
str(company)
hist(company$Sales)
high <- ifelse(company$Sales >=8,"yes","no")
company =data.frame(company,high)
tree.company <- ctree(high ~.-Sales ,data=company)
summary(tree.company)
plot(tree.company)
tree.company
View(company)

set.seed(101)
train=sample(1:nrow(company), 200)
View(train)
tree.company= ctree(high ~.-Sales,company, subset=train)
plot(tree.company)
print(tree.company)
text(tree.company)
tree.company
tree.pred = predict(tree.company,company[-train,])
with(company[-train,], table(tree.pred, high))

########  fraud check#######
install.packages("party")
library(party)
fraud <- read.csv(file.choose())
View(fraud)
str(fraud)
hist(fraud$Taxable.Income)
Risk <- ifelse(fraud$Taxable.Income <= 30000,"Risky","good")
fraud =data.frame(fraud,Risk)
tree.fraud <- ctree(Risk ~.-Taxable.Income ,data=fraud)
summary(tree.fraud)
plot(tree.fraud)
tree.fraud
View(fraud)

set.seed(101)
train=sample(1:nrow(fraud), 200)
View(train)
tree.fraud= ctree(Risk ~.-Sales,fraud, subset=train)
plot(tree.fraud)
print(tree.fraud)
text(tree.fraud)
tree.fraud
tree.pred = predict(tree.fraud,fraud[-train,])
with(fraud[-train,], table(tree.pred, Risk))

