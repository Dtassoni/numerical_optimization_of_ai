# Set working directory with error handling
setwd_safe <- function(path) {
  if (dir.exists(path)) {
    setwd(path)
  } else {
    stop("The specified directory does not exist.")
  }
}

# Function to read data with error handling
read_data_safe <- function(file, widths, col_names) {
  if (file.exists(file)) {
    tryCatch({
      data <- read.fwf(file, widths, col.names = col_names)
      return(data)
    }, error = function(e) {
      stop("Error reading the file: ", e$message)
    })
  } else {
    stop("The specified file does not exist.")
  }
}

# Ensure necessary libraries are installed and loaded
load_library_safe <- function(library_name) {
  if (!require(library_name, character.only = TRUE)) {
    install.packages(library_name)
    library(library_name, character.only = TRUE)
  }
}

# Set working directory
setwd_safe("C:/Users/dtass/OneDrive/Desktop/R work")

# Define column widths and names
widths <- c(30, 8, 8, 8, 8, 8, 8, 8, 8)
col_names <- c("golfer", "drive", "fairway", "green", "putts", "sandshot", "sandsave", "prz", "logprz")

# Read data with error handling
data <- read_data_safe("lpga2008.txt", widths, col_names)

# Load necessary library with error handling
load_library_safe("SignifReg")

# Fit the full model with error handling
fit_model_safe <- function(data) {
  tryCatch({
    model <- lm(logprz ~ drive + fairway + green + putts + sandshot + sandsave, data = data)
    return(model)
  }, error = function(e) {
    stop("Error fitting the model: ", e$message)
  })
}

model_pre <- fit_model_safe(data)

# Define the scope for stepwise selection
scope <- logprz ~ drive + fairway + green + putts + sandshot + sandsave

# Perform stepwise selection with error handling
perform_stepwise_selection <- function(model, scope) {
  tryCatch({
    result <- SignifReg(model, scope = scope, criterion = "AIC", direction = "both", trace = TRUE)
    return(result)
  }, error = function(e) {
    stop("Error in stepwise selection: ", e$message)
  })
}

result <- perform_stepwise_selection(model_pre, scope)

# Output the result
print(result)
