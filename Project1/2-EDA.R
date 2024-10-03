##################################################################
# Impact of weather on marathon performance across age and gender
# 2-Preliminary and supplementary exploratory data analysis
##################################################################

# Load necessary packages
library(tidyverse)
library(kableExtra)
library(knitr)
library(ggplot2)
library(naniar)
library(gtsummary)

# Define data path
data_path = "/Users/yanweitong/Documents/PHP2550-Data/Project1"

# Import datasets
main_data = read.csv(paste0(data_path, "/project1.csv"))
aqi_data = read.csv(paste0(data_path, "/aqi_values.csv"))
aqi_box_data = read.csv(paste0(data_path, "/aqi_values_latlon_box.csv"))
record_data = read.csv(paste0(data_path, "/course_record.csv"))


#-----------------------------------------------
# Data merge and cleaning
#-----------------------------------------------
# Merge main and record data sets
record_data <- record_data %>%
  mutate(Sex = ifelse(Gender == "F", 0, 1)) %>%
  mutate(Race_code = case_when(Race == "B"~0,
                               Race == "C"~1,
                               Race == "NY"~2,
                               Race == "TC"~3,
                               Race == "D" ~4))

merged_main <- main_data %>%
  left_join(record_data[, c("Year", "Sex", "CR", "Race_code")], 
            by = c("Race..0.Boston..1.Chicago..2.NYC..3.TC..4.D." = "Race_code", 
                   "Year" = "Year",
                   "Sex..0.F..1.M." = "Sex")) %>%
  dplyr::rename(Race_code = Race..0.Boston..1.Chicago..2.NYC..3.TC..4.D.,
         Sex = Sex..0.F..1.M.,
         Age = Age..yr.) %>%
  mutate(Gender = factor(ifelse(Sex == 0, "Female", "Male")))

#Clean up AQI by box
aqi_box_data = aqi_box_data %>% 
  distinct()

aqi_box_mean = aqi_box_data %>%
  group_by(marathon, date_local, parameter, sample_duration) %>%
  summarise(daily_mean = mean(arithmetic_mean, na.rm = TRUE)) %>%
  mutate(parameter_duration = paste0(parameter, "-", sample_duration)) 

aqi_box_pivot = aqi_box_mean[,c("marathon", "date_local", "parameter_duration", "daily_mean")] %>%
  pivot_wider(names_from = parameter_duration, values_from = daily_mean)

#Clean up AQI by CBSA
aqi_data = aqi_data %>% 
  distinct()

aqi_mean = aqi_data %>%
  group_by(marathon, date_local, parameter, sample_duration) %>%
  summarise(daily_mean = mean(arithmetic_mean, na.rm = TRUE)) %>%
  mutate(parameter_duration = paste0(parameter, "-", sample_duration)) 

aqi_pivot = aqi_mean[,c("marathon", "date_local", "parameter_duration", "daily_mean")] %>%
  pivot_wider(names_from = parameter_duration, values_from = daily_mean)


#-----------------------------------------------
# Exploratory plotting
#-----------------------------------------------

# Plot the data with smoothing and 95% CI
(best_time_gender_age = ggplot(merged_main, aes(x = Age, y = X.CR, color = Gender)) +
  geom_smooth(method = "loess", se = TRUE) +  # Loess smoothing with 95% CI
  labs(
    title = "Men vs Women",
    x = "Age (yrs)",
    y = "Best Time (%CR)"
  ) +
  scale_y_continuous(limits = c(0, 300)) +  # Adjust y-axis to match the example
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_blank(),  # Remove the legend title
    legend.position = "bottom"  # Move the legend to the bottom
  ))

