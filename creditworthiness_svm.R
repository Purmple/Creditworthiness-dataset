library(ROCR)
require(tree)
require(randomForest)
require(e1071)


cw <- read.csv("creditworthiness.csv")
# cw.k <- cw %>% filter(credit.rating > 0)
cw.k <- subset(cw, credit.rating > 0)
cw.uk <- subset(cw, credit.rating == 0)
cw.train <- cw.k[1:(nrow(cw.k)/2), ]
cw.test <- cw.k[-(1:(nrow(cw.k)/2)), ]

tree.cw.train = tree(as.factor(credit.rating)~., data=cw.train)
tree.cw.train

median.cust = data.frame()
median.cust.data = 
  c(0,1,1,0,3,0,3,3,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
    3,3,3,3,3,3,3,3,3,3,3,3)
median.cust = rbind(median.cust, median.cust.data)
colnames(median.cust) = names(cw.k)[-46]

set.seed(101)

cust.pred = predict(tree.cw.train, median.cust, type="class")
cust.pred

tree.pred = predict(tree.cw.train, cw.test, type="class")
confusion = with(cw.test, table(tree.pred, credit.rating))
confusion

tree.acc = sum(diag(confusion))/sum(confusion)
tree.acc

# count of all classes in credit.rating
before.count = table(cw.train$credit.rating)
# probability of each class
before.prob = before.count/sum(before.count)
# entropy before split
before.ent = -sum(before.prob * log2(before.prob))
before.ent 

# functionary == 0
func0.count = table(cw.train$credit.rating[cw.train$functionary == 0])
func0.prob = func0.count/sum(func0.count)
func0.ent = -sum(func0.prob * log2(func0.prob))
func0.ent

# functionary == 1
func1.count = table(cw.train$credit.rating[cw.train$functionary == 1])
func1.prob = func1.count/sum(func1.count)
func1.ent = -sum(func1.prob * log2(func1.prob))
func1.ent

ent = (before.ent - (func0.ent * sum(func0.count) +
                        func1.ent * sum(func1.count)) /
        sum(sum(func0.count) + sum(func1.count)))
ent


# Random forest
rf.cw.train = randomForest(as.factor(credit.rating)~., data = cw.train)
rf.cw.train
rf.pred = predict(rf.cw.train, cw.test[,-46])

confusion.rf = with(cw.test, table(rf.pred, credit.rating))
confusion.rf
rf.acc = sum(diag(confusion.rf))/sum(confusion.rf)
rf.acc

# Fit to a model using randomForest after the tuning
rftuning.cw.train = randomForest(as.factor(credit.rating)~., data = cw.train, mtry
                                 = 15, ntree=500, stepFactor=2, improve=0.2)
rftuning.pred = predict(rftuning.cw.train, cw.test[,-46])

# Produce confusion matrix after the tuning
confusion.rftuning = with(cw.test, table(rftuning.pred, credit.rating))
confusion.rftuning
# Calculate the accuracy rate after the tuning
acc = sum(diag(confusion.rftuning))/sum(confusion.rftuning)
acc

# e1071 svm
svmfit = svm(as.factor(credit.rating)~., data = cw.train, kernel = "radial")
svmfit

svm.med.pred = predict(svmfit, median.cust, decision.values = TRUE)
svm.med.pred

# Predict the crefusion matrix for predicting the credit rating from the SVM on the test set
svm.pred = predict(svmfit, cw.test[,-46])

confusion.svm = with(cw.test, table(svm.pred, credit.rating))
confusion.svm

acc = sum(diag(confusion.svm))/sum(confusion.svm)
acc

summary(tune.svm(as.factor(credit.rating) ~ ., data = cw.train,
                 kernel = "radial",cost = 10^c(0:2), gamma = 10^c(-4:-1)))

# Fit a model using SVM
svmtuning = svm(as.factor(credit.rating) ~ ., data = cw.train, kernel = "radial", 
               cost=10, 
               gamma = 0.001)

svmtuning.pred = predict(svmtuning, cw.test[,-46])

confusion.svmtuning = with(cw.test, table(svmtuning.pred, credit.rating))
confusion.svmtuning

acc = sum(diag(confusion.svmtuning))/sum(confusion.svmtuning)
acc

# Naive Bayes
nb = naiveBayes(as.factor(credit.rating)~. ,data=cw.train)
nb.class.pred = predict(nb, median.cust, type='class')
nb.class.pred
nb.class.raw = predict(nb, median.cust, type='raw')
nb.class.raw 

nb.pred = predict(nb, cw.test[,-46])

confusion.nb = with(cw.test, table(nb.pred, credit.rating))
confusion.nb

acc = sum(diag(confusion.nb))/sum(confusion.nb)
acc

# Logistic Regression
glm.fit <- glm((credit.rating==1)~., data = cw.train, family = binomial)
options(width = 130)
summary(glm.fit)

# Fit an SVM model of your choice to the training set
summary(tune.svm((credit.rating==1) ~ ., data = cw.train,
                 kernel = "radial",cost = 10^c(-2:2), gamma = 10^c(-4:1), 
                 type='C'))
svm2 = svm(I(credit.rating == 1)~ ., data = cw.train, type = "C")
svm2 

# Predict the values on test set[SVM]
svm.fit.pred = predict(svm2, cw.test[,-46], decision.values =TRUE)
# Predict the values on test set[GLM]
glm.fit.pred = predict(glm.fit, cw.test[,-46])

# Make prediction using SVM
confusionSVM = prediction(-attr(svm.fit.pred, "decision.values"), 
                          cw.test$credit.rating == 1)

# Create rocs curve based on prediction
rocsSVM <- performance(confusionSVM, "tpr", "fpr")

#make prediction using Logistic Regression
confusionGLM = prediction(glm.fit.pred, cw.test$credit.rating == 1)
#create rocs curve based on prediction
rocsGLM <- performance(confusionGLM, "tpr", "fpr")

# Plot the graph
plot(rocsGLM, col=1)
plot(rocsSVM, col= 2 ,add=TRUE)
abline(0, 1, lty = 3)
# Add the legend to the graphs
legend(0.6, 0.6, c('glm','svm'), 1:2)
