# https://github.com/RickPack/R-Dojo/blob/master/RDojo_MachLearn.R

# Created by Rick Pack and Chad Himes during the Triangle .NET User Group
#   "Introduction to R" dojo, led by
#   Kevin Feasel and Jamie Dixon.
# Vast majority of code from the tutorial by Jason Brownlee:
# "Your First Machine Learning Project in R
#   Step-By-Step (tutorial and template for future projects)"
#    http://machinelearningmastery.com/machine-learning-in-r-step-by-step/

# --------------------------------------------------
# To get started with Comet and R, please see:
# https://www.comet.ml/docs/r-sdk/getting-started/
#
# Specifically, you need to create a .comet.yml file
# or add your Comet API key to create_experiment()
# --------------------------------------------------

#install.packages("cometr")
#install.packages("caret")
#install.packages("ellipse")

library(cometr)
library(caret)
library(ellipse)

exp = create_experiment(
  keep_active = TRUE,
  log_output = TRUE,
  log_error = FALSE,
  log_code = TRUE,
  log_system_details = TRUE,
  log_git_info = TRUE
)

# attach the iris dataset to the environment
data(iris)

# rename the dataset
dataset <- iris

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)

# select 20% of the data for validation
validation <- dataset[-validation_index,]

# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

# split input and output
# 5th column is outcome variable "Species"
# 1-4th are two types of length and width measurements
x <- dataset[,1:4]
y <- dataset[,5]
# scatterplot matrix
# Scatterplot shows overlap of green (species "virginica")
#   and pink (species "versicolor")

png(file = "FeaturePlot.png")
featurePlot(x=x, y=y, plot="ellipse")
dev.off()
exp$upload_asset("FeaturePlot.png")

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)

exp$log_parameter("seed", 7)

fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

## b) nonlinear algorithms
## CART
#set.seed(7)
#fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
## kNN
#set.seed(7)
#fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
## c) advanced algorithms
## SVM
#set.seed(7)
#fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
## Random Forest
#set.seed(7)
#fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
#results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
#summary(results)

# estimate skill of LDA on the validation dataset
# Shows accuracy is 100% (1) for validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)

exp$add_tags(list("made with caret"))
exp$log_html("
<h1>Comet Caret Example</h1>

<p>This example demonstrates using the caret library on the iris dataset.</p>

<p>See the Output tab for confusion matrix.</p>

<ul>
<li><a href=https://github.com/comet-ml/cometr/blob/master/inst/train-examples/caret-example.R>github.com/comet-ml/cometr/inst/train-example/caret-example.R</a></li>
</ul>

<p>For help on the Comet R SDK, please see: <a href=https://www.comet.ml/docs/r-sdk/getting-started/>www.comet.ml/docs/r-sdk/getting-started/</a></p>

")

exp$log_other(key = "Created from", value = "cometr")

exp$print()
exp$stop()
