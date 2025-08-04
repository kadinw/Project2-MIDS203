library(ggplot2)
library(tidyverse)
library(nnet)
library(dplyr)

#file_path <- "\Users\winni\Documents\MIDS\mids203HW\Project2\Sleep_health_and_lifestyle_dataset"

sleep_data <- read.csv("C:\\Users\\winni\\Documents\\MIDS\\mids203HW\\Project2\\Sleep_health_and_lifestyle_dataset.csv")


unique(sleep_data$Occupation)
table(sleep_data$Occupation)

colnames(sleep_data)
glimpse(sleep_data)

model <- multinom(Occupation ~ Sleep.Duration + Quality.of.Sleep + Stress.Level, data = sleep_data)
summary(model)

null_model <- multinom(Occupation ~ 1, data = sleep_data)
summary(null_model)



#Filter for jobs with significance
filtered_data <- sleep_data %>%
  filter(!Occupation %in% c("Software Engineer", "Manager", "Scientist", "Sales Representative"))

model_filtered <- multinom(Occupation ~ Sleep.Duration + Quality.of.Sleep + Stress.Level, 
                           data = filtered_data)
summary(model_filtered)

null_model_filter <- multinom(Occupation ~ 1, data = filtered_data)
summary(null_model_filter)
