# Axel Eichelmann (s2030757, axeleichelmann)

# Place your function definitions that may be needed in analysis.R and report.Rmd
# in this file, including documentation.
# You can also include any needed library() calls here


library(shiny)
library(tidyverse)
library(dplyr)
library(StatCompLab)

data(ghcnd_stations, package = "StatCompLab")
data(ghcnd_values, package = "StatCompLab")

#Function which computes the test-stat for each station, w\ an option to randomize the season labels
test_stat <- function(randomise = FALSE) {
  PRCP_data <- mutate(ghcnd_values, Season = if_else(Month %in% c(1,2,3,10,11,12), 'Winter', 'Summer')) %>%
    filter(Element == "PRCP") %>% group_by(ID) %>% select(ID, Value, Season)
  if (randomise == TRUE) {
    for (i in ghcnd_stations$ID) {
      var <- PRCP_data$Season[PRCP_data$ID==i]
      PRCP_data$Season[PRCP_data$ID==i] <- sample(var, size = length(PRCP_data$Season[PRCP_data$ID == i]))
    }
  }
  Avg_Summer_Rf <- PRCP_data %>% summarize(Summer_rf = mean(Value[Season=='Summer']))
  Avg_Winter_Rf <- PRCP_data %>% summarize(Winter_rf = mean(Value[Season=='Winter']))
  table <- Avg_Summer_Rf %>% right_join(Avg_Winter_Rf, by = "ID")
  table <- mutate(table, t_value = abs(Summer_rf-Winter_rf))
  colnames(table) <- c("Station ID", "Avg. Summer Rf", "Avg. Winter Rf", "T-Value")
  table
}

# Create function which calculates the empirical non-zero proportion of rainfall in Winter and Summer
prcp_prob <- function(randomise = FALSE){
  PRCP_data <- mutate(ghcnd_values, Season = if_else(Month %in% c(1,2,3,10,11,12),'Winter','Summer')) %>%
    filter(Element == "PRCP") %>% group_by(ID) %>%
    mutate(Rainfall = if_else(Value == 0, 0, 1)) %>% select(ID, Rainfall, Season)
  if (randomise== TRUE){
    for (i in ghcnd_stations$ID) {
      var <- PRCP_data$Season[PRCP_data$ID==i]
      PRCP_data$Season[PRCP_data$ID==i] <- sample(var, size = length(PRCP_data$Season[PRCP_data$ID == i]))
    }
  }

  Summer_prob <- PRCP_data %>% summarise(summer_prob = sum(Rainfall[Season == 'Summer'])/length(Rainfall))
  Winter_prob <- PRCP_data %>% summarise(winter_prob = sum(Rainfall[Season == 'Winter'])/length(Rainfall))
  table <- right_join(Summer_prob, Winter_prob, by = "ID")
  table <- mutate(table, t_value = abs(summer_prob-winter_prob))
  colnames(table) <- c("Station ID", "Summer Rf Prob", "Winter Rf Prob", "T-Value")
  table
}

#version of data set with monthly average precipitation values in each year
monthly_avg_data <- ghcnd_values %>% filter(Element == "PRCP") %>% group_by(ID,Year,Month) %>%
                    mutate(monthly_avg = mean(Value)) %>% right_join(ghcnd_stations, by = "ID")

# average monthly precipitation across all years
graph_monthly_avg <- ghcnd_values %>% filter(Element == "PRCP") %>% group_by(ID, Month) %>%
                     mutate(monthly_mu = mean(Value)) %>% left_join(ghcnd_stations, by = "ID")

model <- lm(monthly_avg ~ Longitude+Latitude+Elevation + cos((DecYear-Year)*2*pi*365/300), data = monthly_avg_data)


# code for the station Cross Validation
complete_ghcnd <- ghcnd_values %>% filter(Element == "PRCP") %>%
                  right_join(ghcnd_stations, by = "ID") %>% group_by(ID)

ID_set <- ghcnd_stations$ID[-1]
cv_model_data <- complete_ghcnd %>% filter(ID %in% ID_set)
CV_model <- lm(Value ~ Longitude + Latitude + cos((DecYear-Year)*2*pi*365/300), data = cv_model_data)
predict_data <- complete_ghcnd %>% filter(ID == ghcnd_stations$ID[1])
pred_values <- data.frame(predict_data, pred = predict(CV_model, predict_data),
                                        pred_sd = predict(CV_model, predict_data, se.fit = TRUE))

for (i in 2:8){
  ID_set <- ghcnd_stations$ID[-i]
  cv_model_data <- complete_ghcnd %>% filter(ID %in% ID_set)
  CV_model <- lm(Value ~ Longitude + Latitude + cos((DecYear-Year)*2*pi*365/300), data = cv_model_data)
  predict_data <- complete_ghcnd %>% filter(ID == ghcnd_stations$ID[i])
  pred_values <- rbind(pred_values, data.frame(predict_data, pred = predict(CV_model, predict_data),
                                               pred_sd = predict(CV_model, predict_data, se.fit = TRUE)))
}

# code for the month cross validation
month_ghcnd <- ghcnd_values %>% filter(Element == "PRCP") %>%
  right_join(ghcnd_stations, by = "ID") %>% group_by(Month)

month_set <- c(1,2,3,4,5,6,7,8,9,10,11,12)[-1]
cv_model_data <- month_ghcnd %>% filter(Month %in% month_set)
CV_model <- lm(Value ~ Longitude + Latitude + cos((DecYear-Year)*2*pi*365/300), data = cv_model_data)
predict_data <- month_ghcnd %>% filter(Month == 1)
pred_month_values <- data.frame(predict_data, pred = predict(CV_model, predict_data),
                          pred_sd = predict(CV_model, predict_data, se.fit = TRUE))

for (i in 2:12){
  month_set <- c(1,2,3,4,5,6,7,8,9,10,11,12)[-i]
  cv_model_data <- month_ghcnd %>% filter(Month %in% month_set)
  CV_model <- lm(Value ~ Longitude + Latitude + cos((DecYear-Year)*2*pi*365/300), data = cv_model_data)
  predict_data <- month_ghcnd %>% filter(Month == i)
  pred_month_values <- rbind(pred_month_values,
                             data.frame(predict_data, pred = predict(CV_model, predict_data),
                                        pred_sd = predict(CV_model, predict_data, se.fit = TRUE)))
}
