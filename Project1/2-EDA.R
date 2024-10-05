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
library(gt)
library(patchwork)
library(stargazer)
library(knitcitations)
library(mosaic)
library(summarytools)
library(npreg)
library(mgcv)

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
                Age = Age..yr.,
                SR = SR.W.m2) %>%
  mutate(Gender = factor(ifelse(Sex == 0, "Female", "Male"))) %>%
  mutate(Marathon = case_when(Race_code == 0 ~ "Boston",
                              Race_code == 1~ "NYC",
                              Race_code == 2 ~ "Chicago",
                              Race_code == 3 ~ "Twin Cities",
                              Race_code == 4 ~ "Grandmas")) %>%
  mutate(Flag = factor(Flag, 
                       levels = c("White", "Green", "Yellow", "Red", "Black")))  %>%
  mutate(FinishTime = as.numeric(as.difftime(CR, units = "mins") * (1+X.CR/100))) %>%
  mutate(Age_Group = cut(Age, breaks = c(0, 14, 19, 29, 39, 49, 59, 69, 79, 92),
                         labels = c("<= 14", "15-19", "20-29", "30-39", 
                                    "40-49", "50-59", "60-69", "70-79", ">= 80"), 
                         right = TRUE))



#Clean up AQI by box
aqi_box_data = aqi_box_data %>% 
  distinct()

aqi_box_mean = aqi_box_data %>%
  group_by(marathon, date_local, parameter, sample_duration) %>%
  summarise(daily_mean = mean(arithmetic_mean, na.rm = TRUE)) %>%
  mutate(parameter_duration = paste0(parameter, "-", sample_duration)) 

aqi_box_pivot = aqi_box_mean[,c("marathon", "date_local", "parameter_duration", "daily_mean")] %>%
  pivot_wider(names_from = parameter_duration, values_from = daily_mean)
# ! AQS data by bounding box will not be included in main analysis due to high missingness


#Clean up AQI by CBSA
aqi_data = aqi_data %>% 
  distinct()

AP_mean = aqi_data %>%
  group_by(marathon, date_local, parameter, sample_duration) %>%
  summarise(daily_mean = mean(arithmetic_mean, na.rm = TRUE)) %>%
  mutate(parameter_duration = paste0(parameter, "-", sample_duration)) %>%
  filter(parameter_duration %in% c("Sulfur dioxide-1 HOUR", "Ozone-1 HOUR",
                                   "Nitrogen dioxide (NO2)-1 HOUR", 
                                   "PM2.5 - Local Conditions-1 HOUR"))

AP_pivot = AP_mean[,c("marathon", "date_local", "parameter_duration", "daily_mean")] %>%
  pivot_wider(names_from = parameter_duration, values_from = daily_mean) %>%
  mutate(Year = year(date_local)) %>%
  rename("SO2" = "Sulfur dioxide-1 HOUR",
         "NO2" = "Nitrogen dioxide (NO2)-1 HOUR",
         "PM2.5" = "PM2.5 - Local Conditions-1 HOUR",
         "Ozone" = "Ozone-1 HOUR")


merged_main = merged_main %>% 
  left_join(AP_pivot, 
            by = c("Marathon" = "marathon",
                   "Year" = "Year")) %>% 
  mutate(Wind_s = scale(Wind),
         WBGT_s = scale(WBGT),
         SR_s = scale(SR),
         X.rh_s = scale(X.rh),
         Ozone_s = scale(Ozone),
         PM2.5_s = scale(PM2.5),
         SO2_s = scale(SO2),
         NO2_s = scale(NO2)
  )

# For course records and environmental parameters only
CR_merged = merged_main  %>%
  dplyr::select(Marathon, CR, Gender, WBGT, Flag, Wind,
                X.rh, SR, NO2, SO2, Ozone, PM2.5, WBGT_s, Wind_s,
                X.rh_s, SR_s, NO2_s, SO2_s, Ozone_s, PM2.5_s) %>%
  distinct() %>%
  mutate(ChipTime = as.numeric(as.difftime(CR, units = "mins")))

#-----------------------------------------------
# Exploratory plotting
#-----------------------------------------------

# Missingness
vis_miss(merged_main, sort_miss = TRUE) +
  labs(title = "Figure 1: Missing Data") +
  theme(axis.text.x =  element_text(angle = 75))

# Participant summary
merged_main %>%
  mutate(Marathon = case_when(Race_code == 0 ~ "Boston",
                          Race_code == 1~ "NYC",
                          Race_code == 2 ~ "Chicago",
                          Race_code == 3 ~ "Twin Cities",
                          Race_code == 4 ~ "Grandmas")) %>%
  dplyr::select(
    Marathon,
    Gender, 
    Age
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    by = Marathon,
    digits = all_continuous() ~ 2,
    missing = "no",
    type = list(
      Gender ~ "categorical"
    )
  ) %>%
  add_p() %>%
  modify_caption(caption = "Baseline Characteristics by Race, N = {N}") %>%
  as_kable_extra(
    booktabs = TRUE,
    longtable = TRUE,
    linesep = ""
  ) %>%
  kableExtra::kable_styling(
    position = "center",
    latex_options = c("striped", "repeat_header"),
    stripe_color = "gray!15"
  )


# Environmental summary
merged_main  %>%
  dplyr::select(
    Marathon,
    WBGT,
    DP,
    `Nitrogen dioxide (NO2)-1 HOUR`,
    `Sulfur dioxide-1 HOUR`,
    `Ozone-1 HOUR`,
    `PM2.5 - Local Conditions-1 HOUR`
  ) %>%
  distinct() %>%
  tbl_summary(
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    by = Marathon,
    digits = all_continuous() ~ 2,
    missing = "no",
    type = list(
      WBGT ~ "continuous2",
      DP ~"continuous2",
      `Sulfur dioxide-1 HOUR`  ~"continuous2",
      `Nitrogen dioxide (NO2)-1 HOUR` ~"continuous2",
      `Ozone-1 HOUR` ~"continuous2",
      `PM2.5 - Local Conditions-1 HOUR` ~"continuous2"
    ),
    label = list(
      DP = "DP (°C)",
      `Nitrogen dioxide (NO2)-1 HOUR` = "NO$_2$ (parts per billion)",
      `Sulfur dioxide-1 HOUR` = "SO$_2$ (parts per billion)",
      `Ozone-1 HOUR` = "O$_3$, (parts per million)",
      `PM2.5 - Local Conditions-1 HOUR` = "PM$_{2.5}, (μg/m$^3$)$"
      # ,
      # Mat_race = "Maternal race",
      # SGA = "Small for gestational age",
      # gender = "Infant gender",
      # ga = "Obstetrical gestational age (weeks)",
      # Del_method = "Delivery method",
      # mat_chorio = "Maternal Chorioamnionitis"
    )
  ) %>%
  modify_caption(caption = 
                   "Summary of weather and pollution parameters across five marathons, N = {N}") %>%
  as_kable_extra(
    booktabs = TRUE,
    longtable = TRUE,
    linesep = ""
  ) %>%
  kableExtra::kable_styling(
    position = "center",
    latex_options = c("striped", "repeat_header"),
    stripe_color = "gray!15"
  )

#dtSummmary()
distinct_environ = CR_merged[, c("WBGT",
                                 "Flag",
                                 "X.rh",
                                 "Wind",
                                 "SR",
                                 "NO2",
                                 "SO2",
                                 "Ozone",
                                 "PM2.5")] %>%
  distinct()

summary_tmp = dfSummary(
  distinct_environ,
  plain.ascii  = FALSE,
  style        = 'grid',
  graph.magnif = 0.85,
  varnumbers = FALSE,
  valid.col    = FALSE,
  tmp.img.dir  = "tmp",
  labels.col=TRUE, 
  display.labels=TRUE
)

print(summary_tmp, methods = "render", 
      Variable.label=TRUE,
      max.tbl.height = 100,
      headings=FALSE)

# Course record distribution
CR_dist_plot <- ggplot(CR_merged, aes(x = ChipTime, fill = Gender, Color = Gender)) +
  geom_histogram(
    position = "identity",
    binwidth = 2,
    alpha = 0.6
  ) +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +
  theme_minimal() +
  labs(title = "Figure 1: Course Record Distribution by Gender", 
       x = "Net race time of the course record (minute)", y = "Count") +
  theme(
    strip.text = element_text(face = "bold", size = 14),  
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = "bottom"
  )


# Plot the data with smoothing 
(best_time_gender_age = ggplot(merged_main, aes(x = Age, y = X.CR, color = Gender)) +
   geom_smooth(method = "loess", 
               se = TRUE, 
               size = 0.5) +  
  labs(
    title = "Impact of Gender",
    x = "Age (yrs)",
    y = "Best Time (%CR)"
  ) +
  scale_y_continuous(limits = c(0, 300)) +  
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred")) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_blank(), 
    legend.position = "right" 
  ))

(best_time_WBGT_age = ggplot(merged_main, aes(x = Age, y = X.CR, color = Flag)) +
    geom_smooth(method = "loess", 
                se = TRUE, 
                size = 0.5) +  
    labs(
      title = "Impact of WBGT Flag",
      x = "Age (yrs)",
      y = "Best Time (%CR)"
    ) +
    scale_y_continuous(limits = c(0, 300)) +  
    #scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred")) + 
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.title = element_blank(), 
      legend.position = "right" 
    ))

# Plot performance vs age by 10-yr age category
age_performance_summary <- merged_main  %>%
  group_by(Age_Group, Gender) %>%
  summarise(
    mean_XCR = mean(X.CR, na.rm = TRUE),
    sd_XCR = sd(X.CR, na.rm = TRUE),
    n = n(),
    se_XCR = sd_XCR / sqrt(n)  # Standard error
  ) %>%
  ungroup()

best_time_gender_age <- ggplot(age_performance_summary, aes(x = Age_Group, y = mean_XCR, color = Gender)) +
  geom_point(size = 1) +  
  geom_line(aes(group = Gender), size = 1) +  
  geom_errorbar(aes(ymin = mean_XCR - 1.96*se_XCR, ymax = mean_XCR + 1.96*se_XCR), width = 0.2) +  
  labs(
    title = "Men vs Women",
    x = "Age (yrs)",
    y = "Best Time (%CR)"
  ) +
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred")) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    legend.title = element_blank(), 
    legend.position = "right"
  )

print(best_time_gender_age)


# List of pollutant variables and their labels
pollutants <- c("Ozone", "NO2", "SO2", "PM2.5")
titles <- c("Ozone", "NO2", "SO2", "PM2.5")
units <- c(" (ppm)", " (ppb)", " (ppb)", "(μg/m^3)")

# Create a list of ggplot objects
plots <- lapply(seq_along(pollutants), function(i) {
  ggplot(merged_main, aes_string(x = pollutants[i], y = "X.CR")) +
    geom_smooth(method = "loess", se = TRUE, size = 0.5, color = "steelblue") +
    labs(title = titles[i], x = paste0(pollutants[i], " ",units[i]), y = "Best Time (%CR)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8)
    )
})

# Combine the four plots into a single row
combined_plot <- wrap_plots(plots, ncol = 4)
combined_plot


ggplot(merged_main, aes(x = Age, y = FinishTime, color = Flag, linetype = Gender)) +
  geom_line(stat = "smooth", method = "loess", se = FALSE) +
  labs(title = "Interaction of WBGT, Gender, and Age on Marathon Performance",
       x = "WBGT", y = "Finish Time (minutes)") +
  theme_minimal()


ggplot(merged_main, aes(x = NO2, y = X.CR, color = Gender)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, size = 1, color = "black") +
  facet_grid(Gender~Age_Group) +
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "darkred")) + 
  labs(title = "Impact of NO2 on Marathon Performance by Gender and Age",
       x = "NO2 (ppm)", y = "Percent off current course record") +
  theme_minimal() + 
  theme(
    legend.position = "none"
  )


dist_plot <- ggplot(merged_main %>% filter(!is.na(Flag)), aes(x = X.CR, fill = Gender)) +
  geom_histogram(position = "identity", binwidth = 10, alpha = 0.6, color = NA) +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +  # Colors for Gender
  facet_wrap(~Flag, scales = "free", nrow = 1) + 
  theme_minimal() + 
  labs(
    title = "Finish Time Distribution by Gender and Flag",
    x = "Percent off current course record",
    y = "Count"
  ) +
  theme(
    strip.text = element_text(face = "bold"),   # Facet titles bold
    plot.title = element_text(hjust = 0.5, size = 16),  # Centered title
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_blank(),  # Remove legend title
    legend.position = "right"
  )


dist_plot <- ggplot(merged_main %>% filter(Age >= 15), aes(x = log(FinishTime), fill = Gender)) +
  geom_histogram(position = "identity",binwidth = 20,  alpha = 0.6, color = NA) +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +  # Colors for Gender
  facet_wrap(~Age_Group, scales = "free", nrow = 2) + 
  theme_minimal() + 
  labs(
    title = "Finish Time Distribution by Gender and Age Group",
    x = "Percent off current course record",
    y = "Count"
  ) +
  theme(
    strip.text = element_text(face = "bold"),   # Facet titles bold
    plot.title = element_text(hjust = 0.5, size = 16),  # Centered title
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_blank(),  # Remove legend title
    legend.position = "right"
  )

dist_plot <- ggplot(merged_main %>% filter(Age >= 15), aes(x = FinishTime, fill = Gender)) +
  geom_histogram(position = "identity",binwidth = 20,  alpha = 0.6, color = NA) +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +  # Colors for Gender
  facet_wrap(~Age_Group, scales = "free_y", nrow = 2) + 
  theme_minimal() + 
  labs(
    title = "Finish Time Distribution by Gender and Age Group",
    x = "Percent off current course record",
    y = "Count"
  ) +
  theme(
    strip.text = element_text(face = "bold"),   # Facet titles bold
    plot.title = element_text(hjust = 0.5, size = 16),  # Centered title
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_blank(),  
    legend.position = "right"
  )

# Display the plot
print(dist_plot)



#-----------------------------------------------
# Correlation and regression
#-----------------------------------------------

all_environ_factors = c("Ozone", "PM2.5", "SO2", "NO2", "WBGT", "Wind", "DP", 
                        "Td..C", "Tw..C", "X.rh", "Tg..C", "SR.W.m2")
environ_data <- merged_main[, all_environ_factors]

cor_matrix <- cor(environ_data, use = "complete.obs", method = "pearson")
corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                   tl.col = "black", tl.srt = 45)



lm.fit = lm(FinishTime ~ Gender + Age + 
              Wind+ WBGT + SR + X.rh,
             #Ozone + PM2.5 + SO2 + NO2,
            data = merged_main)
summary(lm.fit)
lm.fit %>% tbl_regression()
stargazer(lm.fit)

lm.fit = lm(X.CR ~ Gender + Age + 
              Ozone + PM2.5 + SO2 + NO2,
            data = merged_main)
summary(lm.fit)
lm.fit %>% tbl_regression()


merged_main = merged_main %>% 
  mutate(Wind_s = scale(Wind),
         WBGT_s = scale(WBGT),
         SR_s = scale(SR),
         X.rh_s = scale(X.rh),
         Ozone_s = scale(Ozone),
         PM2.5_s = scale(PM2.5),
         SO2_s = scale(SO2),
         NO2_s = scale(NO2)
  )

lm.fit = lm(log(X.CR) ~ Gender + I(Age^2) +
              Wind_s+ WBGT_s + SR_s+ X.rh_s+
              Ozone_s + SO2_s + NO2_s + PM2.5_s,
            data = merged_main)
summary(lm.fit)
lm.fit %>% tbl_regression()
tidy(lm.fit)


#Spline
std_environ_factors = c("Wind_s", "WBGT_s", "SR_s", "X.rh_s",
                        "Ozone_s", "SO2_s", "NO2_s", "PM2.5_s")

std_x_labels <- list(
  "X.rh_s" = "% relative humidity",
  "Wind_s" = "Wind Speed",
  "WBGT_s" = "WBGT",
  "SR_s" = "SR",
  "NO2_s" = "NO2",
  "SO2_s" = "SO2",
  "Ozone_s" = "Ozone",
  "PM2.5_s" = "PM2.5"
)

# Wrapper funct to fit spline, create plot, and extract adjusted R-squared
create_spline_plot <- function(df, show_y_axis =TRUE, 
                               x_var, y_var = "X.CR") {
  
  model_data <- df %>%
    filter(!is.na(.data[[x_var]]) & !is.na(.data[[y_var]]) & 
             is.finite(.data[[x_var]]) & is.finite(.data[[y_var]]))
  
  spline_fit <- ss(model_data[[x_var]], model_data[[y_var]])
  
  adj_r2 <- summary(spline_fit)$adj.r.squared
  
  x_label <- paste0("Standardized ", std_x_labels[[x_var]] %||% x_var)
  ylab_text <- if (show_y_axis) "% off course record" else ""
  
  mar_setting <- if (show_y_axis) c(5, 4, 4, 2) else c(5, 1.5, 4, 2)
  par(mar = mar_setting)
  
  plot(spline_fit, xlab =" ", ylab = ylab_text, 
       main = x_label, cex.lab = if (show_y_axis) 1.5 else 0.1, 
       cex.main = 1.5)
  
  # Add annotation for adjusted R²
  legend(
    "topright",
    legend = paste0("Adj R² = ", round(adj_r2, 3)),
    bty = "n",
    text.col = "#CC3333",
    cex = 1.5
  )
}

# Create all plots
par(mfrow = c(2, 4), oma = c(0, 0, 3, 0))
for (i in seq_along(std_environ_factors)) {
  show_y_axis <- (i %% 4 == 1)
  create_spline_plot(merged_main, x_var = std_environ_factors[i], 
                     y_var = "X.CR",
                     show_y_axis = show_y_axis)
}
mtext("Figure: Spline Fits of Environmental Factors on % off Course Record", 
      outer = TRUE, cex = 1.5, font = 2)



# Fit GAM and extract necessary statistics
# Data frame to store results
gam_results <- data.frame(
  Variable = character(),
  n = numeric(),
  F_value = numeric(),
  P_value = numeric(),
  Adj_R2 = numeric(),
  Deviance_Explained = numeric(),
  stringsAsFactors = FALSE
)


get_gam_info <- function(df, x_var, y_var = "X.CR", family = Gamma(link = "log")) {
  
  # Filter data for valid values
  model_data <- df %>%
    filter(.data[[y_var]] > 0) %>%
    filter(!is.na(.data[[x_var]]) & !is.na(.data[[y_var]]) & 
             is.finite(.data[[x_var]]) & is.finite(.data[[y_var]]))
  
  gam_fit <- gam(as.formula(paste0(y_var, " ~ s(", x_var, ")")), 
                 data = model_data, family = family)
  
  gam_summary <- summary(gam_fit)
  n <- gam_summary$n
  
  # Extracting approximate F-value and p-value for the smooth term
  smooth_terms <- gam_summary$s.table
  
  F_value <- smooth_terms[1, "F"]
  p_value <- smooth_terms[1, "p-value"]
  
  # Adjusted R-squared and deviance explained
  adj_r2 <- paste0(round(gam_summary$r.sq, 4) * 100, "%")
  deviance_explained <- paste0(round(gam_summary$dev.expl, 4)*100, "%") 
  
  return(
    list(
      n = n,
      F_value = round(F_value, 4),
      P_value = round(p_value, 4),
      Adj_R2 = adj_r2,
      Deviance_Explained = deviance_explained
    )
  )
}

for (x_var in std_environ_factors) {
  gam_info <- get_gam_info(merged_main, x_var)
  
  gam_results <- rbind(gam_results, data.frame(
    Variable = std_x_labels[[x_var]] %||% x_var,
    n = gam_info$n,
    F_value = gam_info$F_value,
    Approx_P_value = gam_info$P_value,
    Adj_R2 = gam_info$Adj_R2, 
    Deviance_Explained = gam_info$Deviance_Explained
  ))
}

colnames(gam_results) <- c("Standardized variable", "n","Approxi. F-value",
                           "Approxi. p-value", "Adj. R-squared", "%Deviance explained")

gam_results %>%
  kable(caption = "GAM Results: %off Course Record vs. 
        Standardized Environmental Factors") %>%
  kable_styling(full_width = FALSE,
                latex_options = c("striped", "repeat_header"),
                stripe_color = "gray!15",
                font_size = 9,
                position = "center")

```
