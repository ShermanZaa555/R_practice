install.packages("caret")
install.packages("rpart.plot")

library(caret)
library(rpart.plot)

Titanic <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/Titanic.csv")
summary(Titanic)

Titanic <- Titanic[,c("PClass","Age","Sex","Survived")]
Titanic$Survived <- as.factor(ifelse(Titanic$Survived==0,"Died","Survived"))
Titanic$PClass <- as.factor(Titanic$PClass)
Titanic$Sex <- as.factor(Titanic$Sex)

str(Titanic)

summary(Titanic)

Titanic <- na.omit(Titanic)

set.seed(9999)

train <- createDataPartition(Titanic[,"Survived"],p=0.8,list=FALSE)
Titanic.trn <- Titanic[train,]
Titanic.tst <- Titanic[-train,]

ctrl <- trainControl(method = "cv", number = 10)

fit.cv <- train(Survived ~ ., data = Titanic.trn, method = "rpart",
                trControl = ctrl,
                tuneLength = 30)

pred <- predict(fit.cv,Titanic.tst)

confusionMatrix(table(Titanic.tst[,"Survived"],pred))

print(fit.cv)
plot(fit.cv)

plot(fit.cv$finalModel)
text(fit.cv$finalModel)

rpart.plot(fit.cv$finalModel)
rpart.plot(fit.cv$finalModel,fallen.leaves = FALSE)

install.packages("RColorBrewer")
install.packages("rattle")
library(RColorBrewer)
library(rattle)
library(rpart)
tree <- rpart(Survived ~., data=Titanic.trn,
                method="class")
fancyRpartPlot(tree)