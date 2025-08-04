library(ggplot2)
library(tidyverse)

#file_path <- "\Users\winni\Documents\MIDS\mids203HW\Project2\Sleep_health_and_lifestyle_dataset"

sleep_data <- read.csv("C:\\Users\\winni\\Documents\\MIDS\\mids203HW\\Project2\\Sleep_health_and_lifestyle_dataset.csv")


unique(sleep_data$Occupation)
table(sleep_data$Occupation)

# Sleep duration Doctor vs. All others
sleep_data$is_doctor <- ifelse(sleep_data$Occupation == "Doctor", 1, 0)
model <- lm(Sleep.Duration ~ is_doctor, data = sleep_data)

summary(model)

#Sleep duration Doctor vs. Nurses
doc_nurse_data <- sleep_data[sleep_data$Occupation %in% c("Doctor", "Nurse"), ]
doc_nurse_data$is_doctor <- ifelse(doc_nurse_data$Occupation == "Doctor", 1, 0)
model_dn <- lm(Sleep.Duration ~ is_doctor, data = doc_nurse_data)

summary(model_dn)

#sleep duration Nurses to all others
sleep_data$is_nurse <- ifelse(sleep_data$Occupation == "Nurse", 1, 0)
model_n <- lm(Sleep.Duration ~ is_nurse, data = sleep_data)

summary(model_n)

#sleep quality Doctor to all others
sleep_data$is_doctor <- ifelse(sleep_data$Occupation == "Doctor", 1, 0)
model_dq <- lm(Quality.of.Sleep ~ is_doctor, data = sleep_data)

summary(model_dq)

#sleep quality Nurse vs all others
sleep_data$is_nurse <- ifelse(sleep_data$Occupation == "Nurse", 1, 0)
model_nq <- lm(Quality.of.Sleep ~ is_nurse, data = sleep_data)

summary(model_nq)

#sleep quality Software Engineers
#sleep_data$is_swe <- ifelse(sleep_data$Occupation == "Software Engineer", 1, 0)
#model_sweq <- lm(Quality.of.Sleep ~ is_swe, data = sleep_data)

#summary(model_sweq)


#sleep duration Software Engineers
#sleep_data$is_swe <- ifelse(sleep_data$Occupation == "Software Engineer", 1, 0)
#model_swed <- lm(Sleep.Duration ~ is_swe, data = sleep_data)

#summary(model_swed)


#sleep quality Lawyers
sleep_data$is_l <- ifelse(sleep_data$Occupation == "Lawyer", 1, 0)
model_lq <- lm(Quality.of.Sleep ~ is_l, data = sleep_data)

summary(model_lq)


#sleep duration Lawyers
sleep_data$is_l <- ifelse(sleep_data$Occupation == "Lawyer", 1, 0)
model_ld <- lm(Sleep.Duration ~ is_l, data = sleep_data)

summary(model_ld)


#sleep quality Salesperson
sleep_data$is_sp <- ifelse(sleep_data$Occupation == "Salesperson", 1, 0)
model_spq <- lm(Quality.of.Sleep ~ is_sp, data = sleep_data)

summary(model_spq)


#sleep duration Salesperson
sleep_data$is_sp <- ifelse(sleep_data$Occupation == "Salesperson", 1, 0)
model_spd <- lm(Sleep.Duration ~ is_sp, data = sleep_data)

summary(model_spd)


#all Occupation compared in Sleep Quality
ggplot(sleep_data, aes(x = Occupation, y = Quality.of.Sleep)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Sleep Quality Across Occupations",
    x = "Occupation",
    y = "Quality of Sleep"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#all Occupation compared in Sleep Duration
ggplot(sleep_data, aes(x = Occupation, y = Sleep.Duration)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Sleep Duration Across Occupations",
    x = "Occupation",
    y = "Sleep Duration"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#all Occupation compared in Stress Level
ggplot(sleep_data, aes(x = Occupation, y = Stress.Level)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Stress Level Across Occupations",
    x = "Occupation",
    y = "Stress Level"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#all Occupation compared in Physical Activity Level
ggplot(sleep_data, aes(x = Occupation, y = Physical.Activity.Level)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Physical Activity Level by Occupation",
    x = "Occupation",
    y = "Physical Activity Level"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
