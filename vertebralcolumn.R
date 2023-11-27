# Yazan Khalaf

# install packages

install.packages("tree")
install.packages("pastecs")
install.packages("randomForest")

#Load or active packages
library(tree)
library(pastecs)
library(randomForest)

#load/import data into R
data<- read.csv("backpain.csv")
class(data)

#review your data
View(data)
names(data)
head(data)
tail(data)

summary(data)
str(data)
pastecs::stat.desc(data)

cor(data$pelvic.incidence,data$pelvic.tilt) #checking the correlation between age and stroke for exploration
hist(data$pelvic.tilt)

#preprocessing
is.na(data) #check for null values
sum(is.na(data)) #count of null

#transform class attribute from character into 
#factor so that it can be used for prediction
data$normal.or.abnormal <- as.factor(data$normal.or.abnormal)

str(data)

#create test set, train set, and class label set
set.seed(123) #this set of random numbers
train.index <- sample(1:nrow(data),220)

train.set <- data[train.index,] #train w train.index
test.set <- data[-train.index,] #test the rest
normal.or.abnormal.test <- data$normal.or.abnormal[-train.index] 

#training the model
dtree.trained <- tree(normal.or.abnormal~., train.set)
summary(dtree.trained)
#terminal nodes = leaf nodes
#misclassification error = % of error
dtree.trained
plot(dtree.trained)
text(dtree.trained) # _ percent chance that values that reach here are assigned to "yes" or "no"

#testing predictions
dtree.tested <- predict(dtree.trained, test.set, type = 'class')
table(dtree.tested,normal.or.abnormal.test) # table comparing actual (high) vs. tested
accuracy.tested <- (45+15)/90
accuracy.tested 
# 66% accuracy shown
# opposite of accuracy = test error, so 33% error

#cross validation
#pruning = simplify, cutting branches off
dtree.validated <- cv.tree(dtree.trained, FUN = prune.misclass)
dtree.validated 
dtree.pruned <- prune.misclass(dtree.trained, best = 5)
plot(dtree.pruned)
text(dtree.pruned)
summary(dtree.pruned)

#test the pruned tree
dtree.pruned.test <- predict(dtree.pruned, test.set, type = "class")
table(dtree.pruned.test,normal.or.abnormal.test)
accuracy.pruned.test <- (49+17) / 90
accuracy.pruned.test
# almost same accuracy, less nodes
# 73% accuracy, 27% error, 4 nodes

#random forest, more accurate, multiple decision trees, average results
dtree.rf <- randomForest::randomForest(normal.or.abnormal~. , train.set)
dtree.rf
dtree.rf.test <- predict(dtree.rf, test.set, type = "class")
dtree.rf.test
table(dtree.rf.test, normal.or.abnormal.test)
accuracy.rf <- (53+19)/90
accuracy.rf

range(data$pelvic.tilt)
range(data$grade.of.spondylolisthesis)
data <- subset(data, select = -normal.or.abnormal) 
data <- scale(data) #standardizing numeric values
pastecs::stat.desc(data)
summary(data)

#of dissimilarity after scaling.
data.dist <- factoextra::get_dist(data) 
data.dist
factoextra::fviz_dist(data.dist)

data.dist <- factoextra::get_dist(data,method = 'pearson') #pearson correlation is used
factoextra::fviz_dist(data.dist)

#### clustering - kmeans ####
#you can determine the best values for k by using the within sum of squares measure.
factoextra::fviz_nbclust(data, kmeans, method = 'wss') #wss is total within sum of squares

set.seed(123)
k6 = kmeans(data, centers = 6, nstart = 25)
k6
k6$cluster 
k6$betweenss 
k6$withinss 

#try the values of 7 and 8 for k and see how the withinss and betweenss change.
set.seed(123)
k7 = kmeans(data, centers = 7, nstart = 25)
k7
k7$cluster
k7$betweenssS
k7$withinss

set.seed(123)
k8 = kmeans(data, centers = 8, nstart = 25)
k8
k8$cluster
k8$betweenss
k8$withinss


#Let's compare the betweenss for the k values of 6,7, 8

k6$withinss
k7$withinss
k8$withinss

fviz_cluster(k8, data = data)

