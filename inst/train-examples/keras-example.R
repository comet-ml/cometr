# Based on:
# https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_classification/

# --------------------------------------------------
# To get started with Comet and R, please see:
# https://www.comet.ml/docs/r-sdk/getting-started/
#
# Specifically, you need to create a .comet.yml file
# or add your Comet API key to create_experiment()
# --------------------------------------------------

# install.packages("cometr")
# devtools::install_github("rstudio/keras")
# install.packages("tidyr")

# library(tensorflow)
# install_keras()

library(cometr)
library(tidyr)
library(ggplot2)
library(keras)
library(reticulate)

exp <- create_experiment(
  keep_active = TRUE,
  log_output = TRUE,
  log_error = FALSE,
  log_code = TRUE,
  log_system_details = TRUE,
  log_git_info = TRUE
)

exp$add_tags(c("made with keras"))

fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

train_images <- train_images / 255
test_images <- test_images / 255

model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  optimizer = 'adam',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

epochs <- 5

exp$log_parameter("epochs", epochs)

LogMetrics <- R6::R6Class("LogMetrics",
  inherit = KerasCallback,
  public = list(
    losses = NULL,
    on_epoch_end = function(epoch, logs = list()) {
      # Had trouble logging directly to exp here
      # so we do it when we get back in R
      self$losses <- c(self$losses, c(epoch, logs[["loss"]]))
    }
  )
)

callback <- LogMetrics$new()

model %>% fit(train_images, train_labels, epochs = epochs, verbose = 2,
      callbacks = list(callback))

# Log the losses now:
losses <- matrix(callback$losses, nrow = 2)
for (i in 1:ncol(losses)) {
  exp$log_metric("loss", losses[2, i], step=losses[1, i])
}

score <- model %>% evaluate(test_images, test_labels, verbose = 0)

cat('Test loss:', score$loss, "\n")

cat('Test accuracy:', score$acc, "\n")

exp$log_metric("test_loss", score$loss)
exp$log_metric("test_accuracy", score$acc)

predictions <- model %>% predict(test_images)

class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat',
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

png(file = "FashionMNISTResults.png")

par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) {
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev))
  # subtract 1 as labels go from 0 to 9
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800'
  } else {
    color <- '#bb0000'
  }

  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}

dev.off()
exp$upload_asset("FashionMNISTResults.png")

exp$log_other(key = "Created by", value = "cometr")

exp$print()
exp$stop()