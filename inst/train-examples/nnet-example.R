# Based on:
# https://www.rdocumentation.org/packages/nnet/versions/7.3-13/topics/nnet

# --------------------------------------------------
# To get started with Comet and R, please see:
# https://www.comet.ml/docs/r-sdk/getting-started/
#
# Specifically, you need to create a .comet.yml file
# or add your Comet API key to create_experiment()
# --------------------------------------------------

#install.packages("cometr")

library(cometr)
library(nnet)
library(stringr)

exp <- create_experiment(
  keep_active = TRUE,
  log_output = TRUE,
  log_error = FALSE,
  log_code = TRUE,
  log_system_details = TRUE,
  log_git_info = TRUE
)

exp$add_tags(c("made with nnet"))

# sample the iris data:

sample_size <- 25 # of each iris type
total_size <- 50
total_size2 <- total_size * 2

exp$log_parameter("sample_size", sample_size)

ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
targets <- class.ind(c(
  rep("s", total_size),
  rep("c", total_size),
  rep("v", total_size))
)
samp <- c(
  sample(1:total_size, sample_size),
  sample((total_size + 1):(total_size * 2), sample_size),
  sample(((total_size * 2) + 1):(total_size * 3), sample_size)
)

weight_decay <- 5e-4
epochs <- 200
hidden_layer_size <- 2
initial_random_weight_range <- 0.1

exp$log_parameter("weight_decay", weight_decay)
exp$log_parameter("epochs", epochs)
exp$log_parameter("hidden_layer_size", hidden_layer_size)
exp$log_parameter("initial_random_weight_range", initial_random_weight_range)

ir1 <- NULL

train <- function() {
  ir1 <<- nnet(
    ir[samp,],
    targets[samp,],
    size = hidden_layer_size,
    rang = initial_random_weight_range,
    decay = weight_decay,
    maxit = epochs)
    ir1
}

output <- capture.output(train(), split = TRUE)
output <- strsplit(output, "\n")

# "initial  value 57.703088 "
for (match in str_match(output, "^initial\\s+value\\s+([-+]?[0-9]*\\.?[0-9]+)")[,2]) {
  if (!is.na(match)) {
     exp$log_metric("loss", match, step=0)
  }
}

# "iter  10 value 46.803951"
matrix = str_match(output, "^iter\\s+(\\d+)\\s+value\\s+([-+]?[0-9]*\\.?[0-9]+)")
for (i in 1:nrow(matrix)) {
  match = matrix[i,]
  if (!is.na(match[2])) {
     exp$log_metric("loss", match[3], step=match[2])
  }
}

test.cl <- function(true, pred) {
    true <- max.col(true)
    cres <- max.col(pred)
    table(true, cres)
}
cm = test.cl(targets[-samp,], predict(ir1, ir[-samp,]))

matrix <- sprintf("[%s,%s,%s]",
                  sprintf("[%s]", paste(cm[1,], collapse=",")),
                  sprintf("[%s]", paste(cm[2,], collapse=",")),
                  sprintf("[%s]", paste(cm[3,], collapse=",")))

title <- "Iris Confusion Matrix"
labels <- sprintf('["%s","%s","%s"]', "Setosa", "Versicolor", "Virginica")

template <- '{"version":1,"title":"%s","labels":%s,"matrix":%s,"rowLabel":"Actual Category","columnLabel":"Predicted Category","maxSamplesPerCell":25,"sampleMatrix":[],"type":"integer"}'

fp <- file("confusion_matrix.json")
writeLines(c(sprintf(template, title, labels, matrix)), fp)
close(fp)

exp$upload_asset("confusion_matrix.json", type = "confusion-matrix")

exp$log_html("
<h1>Comet nnet Example</h1>

<p>This example demonstrates using the nnet library on the iris dataset.</p>

<p>See the Output tab for confusion matrix.</p>

<ul>
<li><a href=https://github.com/comet-ml/cometr/blob/master/inst/train-examples/nnet-example.R>github.com/comet-ml/cometr/inst/train-example/nnet-example.R</a></li>
</ul>

<p>For help on the Comet R SDK, please see: <a href=https://www.comet.ml/docs/r-sdk/getting-started/>www.comet.ml/docs/r-sdk/getting-started/</a></p>
")

exp$log_other(key = "Created from", value = "cometr")

exp$print()
exp$stop()
