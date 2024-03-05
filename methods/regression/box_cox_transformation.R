library(openxlsx)

# Set working directory
setwd("C:/Users/dtass/OneDrive/Desktop/R work")

# Read data from Excel file
tryCatch({
  D <- read.xlsx(xlsxFile = "x^2.xlsx")
  print(D)
}, error = function(e) {
  stop("Error reading data: ", e$message)
})

# Plot data
tryCatch({
  plot(D$x, D$y)
}, error = function(e) {
  stop("Error plotting data: ", e$message)
})

# Create a linear model
tryCatch({
  model <- lm(D$y ~ D$x)
  print(summary(model))
}, error = function(e) {
  stop("Error creating linear model: ", e$message)
})

# Save residuals
tryCatch({
  RES <- model$residuals
  plot(RES)
}, error = function(e) {
  stop("Error saving residuals or plotting: ", e$message)
})

# QQ plot
tryCatch({
  qqnorm(RES)
  qqline(RES)
}, error = function(e) {
  stop("Error creating QQ plot: ", e$message)
})

# Shapiro test
tryCatch({
  shapiro.test(RES)
}, error = function(e) {
  stop("Error conducting Shapiro test: ", e$message)
})

# Box Cox transformation
tryCatch({
  library(MASS)
  box <- boxcox(object = model, lambda = seq(from = -2, to = 2, by = 0.01))
  lambda.hat <- box$x[box$y == max(box$y)]
  print(lambda.hat)
}, error = function(e) {
  stop("Error performing Box Cox transformation: ", e$message)
})

# Remake model with transformation
tryCatch({
  plot(D$x, D$y^lambda.hat)
  lam_model <- lm(D$y^lambda.hat ~ D$x)
  print(summary(lam_model))
}, error = function(e) {
  stop("Error remaking model with transformation: ", e$message)
})

# Save residuals of transformation
tryCatch({
  lam_res <- lam_model$residuals
  plot(lam_res)
}, error = function(e) {
  stop("Error saving residuals of transformation or plotting: ", e$message)
})

# QQ plot and Shapiro test for transformed residuals
tryCatch({
  qqnorm(lam_res)
  qqline(lam_res)
  shapiro.test(lam_res)
}, error = function(e) {
  stop("Error creating QQ plot or conducting Shapiro test for transformed residuals: ", e$message)
})
