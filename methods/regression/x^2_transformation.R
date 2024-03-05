library(openxlsx)

# Set working directory
setwd("C:/Users/dtass/OneDrive/Desktop/R work")

# Function to plot data
plot_data <- function(x, y) {
  plot(x, y)
}

# Function to fit a linear model and plot residuals
fit_linear_model <- function(x, y) {
  tryCatch({
    # Fit linear model
    model <- lm(y ~ x)
    # Summary of the model
    summary(model)
    # Save residuals
    RES <- model$residuals
    # Plot residuals
    plot(x, RES)
    # QQ plot
    qqnorm(RES)
    qqline(RES)
    # Shapiro test
    shapiro.test(RES)
  }, error = function(e) {
    message("Error fitting the linear model: ", e$message)
  })
}

# Function to fit a quadratic model and plot residuals
fit_quadratic_model <- function(x, y) {
  tryCatch({
    # Fit quadratic model
    model2 <- lm(y ~ x + I(x^2))
    # Summary of the model
    summary(model2)
    # Save residuals
    RES2 <- model2$residuals
    # QQ plot
    qqnorm(RES2)
    qqline(RES2)
    # Shapiro test
    shapiro.test(RES2)
    # Plot residuals
    plot(x, RES2)
    return(model2) # Return the model object
  }, error = function(e) {
    message("Error fitting the quadratic model: ", e$message)
  })
}

# Main script
tryCatch({
  # Read data
  D <- read.xlsx(xlsxFile = "box_cox_example.xlsx")
  # Display head and tail of data
  head(D)
  tail(D)
  # Plot data
  plot_data(D$x, D$y)
  
  # Fit linear model
  fit_linear_model(D$x, D$y)
  
  # Fit quadratic model
  model2 <- fit_quadratic_model(D$x, D$y)
  
  # Plot data with quadratic model curve
  plot_data(D$x, D$y)
  curve(expr = model2$coefficients[1] +
          model2$coefficients[2]*x +
          model2$coefficients[3]*x^2, col = "red", lty =
          "solid", lwd = 1, add = TRUE)
}, error = function(e) {
  message("Error: ", e$message)
})
