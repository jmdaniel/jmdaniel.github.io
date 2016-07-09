
# ---------------------------------------------
# Coursera Data Product Course
# Module: HDB Reseller price predictive model
#         R function library
# Data: 2nd July 2016
# Author: Jean-Michel Coeur
# ---------------------------------------------
require(caret)

inventory_type <- NULL
inventory_storey <- NULL
inventory_lease <- NULL
inventory_town <- NULL

# Collect the data from the Singapore'Open Data Portal and clean it before analysis
getCleanHDBData <- function () {

  # Data are collected from Singapore's Open Data Portal
  url_data <- "https://data.gov.sg/dataset/7a339d20-3c57-4b11-a695-9348adfd7614/resource/83b2fc37-ce8c-4df4-968b-370fd818138b/download/resale-flat-prices-based-on-registration-date-from-march-2012-onwards.csv"
  file_name <- "resale-flat-prices-based-on-registration-date-from-march-2012-onwards.csv"
  
  # Download the file if not stored locally
  if (!file.exists(paste("./Data/", file_name, sep = ""))) 
    download.file(url_data, paste("./Data/", file_name, sep = ""), method="curl")
  
  hdb_resale <- read.csv(file = paste("./Data/", file_name, sep = ""))

  hdb_resale$month <- as.Date(paste(hdb_resale$month, "-01", sep=""), format = "%Y-%m-%d")
  # We restrict the data from 2014 onwards
  hdb_resale <- hdb_resale[hdb_resale$month > as.Date("2014-01-01", format = "%Y-%m-%d"), ]
  
  # Remove 'block', 'street_name' and 'floor_area_sqm', which depends on the flat_type and model
  hdb_resale <- hdb_resale[, -c(1, 4:5,7)]
  
  # We remove the flats with very little inventory of storey range: less than 0.4%
  freq_storey_range <- levels(hdb_resale$storey_range)[(table(hdb_resale$storey_range) / nrow(hdb_resale)) > 0.004]
  hdb_resale <- hdb_resale[hdb_resale$storey_range %in% freq_storey_range, ]
  hdb_resale$storey_range <- factor(hdb_resale$storey_range)
  
  # We remove the flat types that are rare: less than 0.4% of the total number of flats
  freq_flat_model <- levels(hdb_resale$flat_model)[(table(hdb_resale$flat_model) / nrow(hdb_resale)) > 0.004]
  hdb_resale <- hdb_resale[hdb_resale$flat_model %in% freq_flat_model, ]
  
  # Reset the number of factor levels to non 0 values
  hdb_resale$flat_model <- factor(hdb_resale$flat_model)
  hdb_resale$flat_type <- factor(hdb_resale$flat_type)
  
  # Refactor flat_model
  current_model <- levels(hdb_resale$flat_model)
  level_model <- c("Apartment/Maisonette", "Improved", "Apartment/Maisonette", "Model A", "Model A",
                   "New Generation", "Premium Apartment",
                   "Simplified", "Standard")
  
  replaceModLevel <- function(curlevel) {
    return (level_model[grep(curlevel, current_model)[1]])
  }
  
  hdb_resale$flat_model <- as.factor(sapply(hdb_resale$flat_model, FUN = replaceModLevel))
  
  # Rename the columns
  names(hdb_resale) <- c("town", "flat_type", "storey_range", "flat_model", 
                         "lease_start_date", "resale_price") 
  return (hdb_resale)
}

# Build a linear model
buildModel <- function(hdb_resale) {
  # We split the dataset between training set (75% of the data) and testing set (25% of the data)
  inTrain <- createDataPartition(y = hdb_resale$resale_price, p=0.75, list=FALSE)
  training <- hdb_resale[inTrain,]
  testing <- hdb_resale[-inTrain,]
  
  # We use a simple Linear Model that captures the interaction between town, model of flats and lease
  # These are correlated because the Singapore goverment built towns, introduces specific flat models at specific dates the past 50 years.
  fit_lmSimple <- lm(formula = resale_price ~ town + flat_model * lease_start_date + 
                       flat_type + storey_range, data = training)
  return(fit_lmSimple)
}

# Resale price Prediction function
predict_price <- function(fit_lmSimple, in_data) {
  # Parse the incoming vector into the predict dataframe
  input.data <- data.frame(town = in_data[1], flat_type = in_data[2], flat_model = in_data[3],
                           storey_range = in_data[4], lease_start_date = as.numeric(in_data[5]))
  # Predict with the confidence interval at 95%
  price_pred <- suppressWarnings(round(predict(fit_lmSimple, newdata = input.data, 
                                               interval = "confidence"), 0))
  # Build return dataframe with input data and predictive price with range
  result_df <- data.frame(input.data, Price = price_pred[1], Lower = price_pred[2], Upper = price_pred[3])
  
  return(result_df)
}

# Predict set of prices and return prediction dataframe: used for testing purpose
buildPredictResults <- function(fit_lm, price_predict_df) {
  
  ndata <- c("WOODLANDS","4 ROOM","Model A", "07 TO 09", 1985)
  new_pred <- predict_price(fit_lm, ndata)
  price_predict_df <- rbind(price_predict_df, new_pred)
  
  ndata <- c("WOODLANDS", "EXECUTIVE", "Apartment/Maisonette", "07 TO 09", 1984)
  new_pred <- predict_price(fit_lm, ndata)
  price_predict_df <- rbind(price_predict_df, new_pred)
  
  ndata <- c("ANG MO KIO", "EXECUTIVE", "Apartment/Maisonette", "07 TO 09", 1984)
  new_pred <- predict_price(fit_lm, ndata)
  price_predict_df <- rbind(price_predict_df, new_pred)
  
  return(price_predict_df)
}

buildInventory <- function(hdb_resale) {
  # Contingency tables - In use to present the only available choices to the user given the
  # dependency between the variables.
  inventory_type <- as.data.frame.matrix(table(hdb_resale$flat_model, hdb_resale$flat_type))
  inventory_storey <- as.data.frame.matrix(table(hdb_resale$flat_model, hdb_resale$storey_range))
  inventory_lease <- as.data.frame.matrix(table(hdb_resale$flat_model, hdb_resale$lease_start_date))
  inventory_town <- as.data.frame.matrix(table(hdb_resale$town, hdb_resale$lease_start_date))
}

# Contingency tables - Verification of available data
# Function that returns flat_type possibilities after choosing a model of HDB
flat_choices <- function(inventory, flat_choice) {
  # Compute list of flat_type availables
  l_flat_choices <- names(inventory)[!(inventory[flat_choice,] == 0)]
  # Return list 
  return (l_flat_choices)
}

# Get HDB Town coordinates
town_coord <- function () {
  hdb_coord <- read.csv(file = "./Data/hdb_coordinates.csv")
  return (hdb_coord)
}

#                     ----------- End of the R functions library ----------- 
