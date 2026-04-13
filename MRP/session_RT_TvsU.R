#Import and analyze RT session data #append to 1RM script
#compare to 1RM/10RM
# Activate the project's renv environment
# renv::activate()

packages_to_install <- c("readxl", "writexl", "dplyr", "tidyr", "renv", "ggplot2", "openxlsx","ggrepel")
new_packages <- packages_to_install[!(packages_to_install %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
}
rm(packages_to_install, new_packages)

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(renv)
library(ggplot2)
library(openxlsx)
library(gridExtra)
library(ggrepel)

# renv::snapshot()

setwd("C:/MRP")
# Get the current date and format it as mmddyyyy
date <- format(Sys.Date(), "%m%d%Y")

# get the names of the sheets in the Excel file
excel_sheets("MRP.xlsx")
# Create the directory name with todays date 
output_directory <- paste( date, sep = "_","output")
# Check if the directory already exists; if not, create it
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}
# Read in the Excel file and perform all transformations in a single pipeline
mydata <- read_excel("MRP.xlsx", sheet = "Strength", col_names = TRUE) %>%
  select(1:3, 7:9) %>%
  rename(Subject = 1, Measure = 2, Exercise = 3, PRE = 4, MID = 5, POST = 6) %>%
  filter(!(Subject %in% c("SUB 5", "SUB 13"))) %>%
  mutate(
    PRE = as.numeric(PRE),
    MID = as.numeric(MID),
    POST = as.numeric(POST)
  )
# view the first few rows of the data
head(mydata)

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "session_raw_data.xlsx")
write_xlsx(mydata, file_name)

# Create 1RM table to compare to 
RM1_data <- mydata %>%
  filter(Measure == "1RM") %>%
  select(Subject, Exercise, PRE)

replacements <- c(
  "Overhead Press" = "OHP",
  "Lat Pull down" = "LatPull",
  "Leg Press" = "LegPress",
  "Bent Over Row" = "BentRow",
  "Seated Row" = "SeatedRow",
  "Lat Pulldown" = "LatPull"
)

RM1_data$Exercise <- recode(RM1_data$Exercise, !!!replacements)
unique(RM1_data$Exercise)

# Combine the Measure and Exercise columns into one
#Rename the exercises to be more amenable to R code
mydata$Exercise <- recode(mydata$Exercise, !!!replacements)
mydata$ex_measure <- paste(mydata$Exercise, mydata$Measure, sep = "_")

head(mydata)
#swap the new ex_measure column to the measure column location. Remove Exercise and ex_measure columns
mydata$Measure <- mydata$ex_measure
mydata$Exercise <- NULL
mydata$ex_measure <- NULL
head(mydata)

# Define the subjects that are trained and untrained
trained_subjects <- c(8, 17, 18, 19)
untrained_subjects <- c(2, 3, 10, 11, 14)

trained_subjects <- paste0("SUB ", trained_subjects)
untrained_subjects <- paste0("SUB ", untrained_subjects)

# filter trained
mydata <- mydata %>%
  filter(Subject %in% trained_subjects)

unique(mydata$Subject)

# Create a new table to store percent change results
percent_change <- data.frame(Measure = character(),
                             PRE_to_MID = numeric(),
                             MID_to_POST = numeric(),
                             PRE_to_POST = numeric(),
                             stringsAsFactors = FALSE)

# Loop through each Measure and calculate percent changes
for (measure in unique(mydata$Measure)) {
  # Subset data for current Measure
  measure_data <- filter(mydata, Measure == measure)
  
  # Calculate percent change from PRE to MID
  percent_change_PRE_to_MID <- ((mean(measure_data$MID) - mean(measure_data$PRE)) / mean(measure_data$PRE)) * 100
  
  # Calculate percent change from MID to POST
  percent_change_MID_to_POST <- ((mean(measure_data$POST) - mean(measure_data$MID)) / mean(measure_data$MID)) * 100
  
  # Calculate percent change from PRE to POST
  percent_change_PRE_to_POST <- ((mean(measure_data$POST) - mean(measure_data$PRE)) / mean(measure_data$PRE)) * 100
  
  # Append results to percent_change table
  percent_change <- rbind(percent_change, 
                          data.frame(Measure = measure,
                                     PRE_to_MID = percent_change_PRE_to_MID,
                                     MID_to_PRE = percent_change_MID_to_POST,
                                     PRE_to_POST = percent_change_PRE_to_POST,
                                     stringsAsFactors = FALSE))
}
# Print percent change table
percent_change
# > percent_change
#              Measure PRE_to_MID MID_to_PRE PRE_to_POST
# 1           OHP_1RM   18.18182   9.425785    29.32138
# 2       LatPull_1RM   13.53383  10.596026    25.56391
# 3      LegPress_1RM   20.59659  12.367491    35.51136
# 4      Deadlift_1RM   26.86567  12.009804    42.10199
# 5   Bench Press_1RM   11.06383   6.130268    17.87234
# 6       BentRow_1RM   13.79310   0.000000    13.79310
# 7          OHP_10RM   25.32110  10.248902    38.16514
# 8      LatPull_10RM   19.41748   7.317073    28.15534
# 9     LegPress_10RM   26.66667   9.716599    38.97436
# 10    Deadlift_10RM   29.10798  12.363636    45.07042
# 11 Bench Press_10RM   15.56886   8.808290    25.74850
# 12     BentRow_10RM   17.39130  14.814815    34.78261
# 13   SeatedRow_10RM   23.25581  14.339623    40.93023
# 14    SeatedRow_1RM   20.47619  13.043478    36.19048


#####RT session data 
data <- read_excel("MRP.xlsx", sheet = "Exercise Session RT", col_names = TRUE)
data$Exercise <- recode(data$Exercise, !!!replacements)
head(data)

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "RTsession_raw_data.xlsx")
write_xlsx(data, file_name)

# Filter the data and get unique Exercise and Subject in a single pipeline
data <- data %>%
  filter(Exercise != "BentRow", 
         !(Subject %in% c("SUB 4", "SUB 5", "SUB 13", "SUB 2")))

data <- data %>%
  filter(Subject %in% trained_subjects)

unique(data$Subject)

# Display unique values for Exercise and Subject after filtering
unique(data$Exercise)
unique(data$Subject)

#chart mean load over weeks
load_data <- filter(data, data$Measure == "lbs")
unique(load_data$Exercise)

# Conversion factor from lbs to kg
lbs_to_kg <- 0.45359237

# Columns containing the week values
week_columns <- c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6",
                  "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Week 12")

# Convert lbs values to kg for the specified week columns
load_data[week_columns] <- load_data[week_columns] * lbs_to_kg

# Rename the measure column from "lbs" to "kg"
load_data$Measure <- gsub("lbs", "kg", load_data$Measure)

# Reshape the data to long format
load_data_long <- load_data %>%
  pivot_longer(cols = starts_with("Week"),
               names_to = "Week",
               names_prefix = "Week ",
               values_to = "Value") %>% 
  drop_na()  # Remove rows with missing values
head(load_data_long)

load_data_long <- load_data_long %>%
  filter(as.numeric(Week) <= 10)
unique(load_data_long$Week)

# week_4_rows <- load_data_long %>%
#   filter(Week == 4)
# 
# print(week_4_rows)
# # View(week_4_rows)
# 
# exercise_counts <- week_4_rows %>%
#   group_by(Exercise) %>%
#   summarise(num_entries = sum(!is.na(Value)))
# 
# print(exercise_counts)

# Calculate the mean and standard deviation for each exercise-week combination
exercise_summary <- load_data_long %>%
  group_by(Exercise, Week) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE),
            sd_value = sd(Value, na.rm = FALSE))

exercise_summary_week4 <- exercise_summary %>%
  filter(Week == "4")

print(exercise_summary_week4)

# Convert the week column to factor with numeric levels in the desired order
exercise_summary$Week <- factor(exercise_summary$Week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

# Get unique exercise names
unique_exercises <- unique(exercise_summary$Exercise)

# Create a list to store plots
exercise_plots <- list()

# Loop through unique exercise names and create plots
for (exercise in unique_exercises) {
  plot_data <- exercise_summary %>%
    filter(Exercise == exercise)
  
  exercise_plot <- ggplot(plot_data, aes(x = Week, y = mean_value)) +
    geom_bar(stat = "identity", fill = "black") +
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                  width = 0.2) +
    labs(x = "Week", y = "kg",
         title = paste(exercise)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line())
  
  exercise_plots[[exercise]] <- exercise_plot
}

# Arrange and display the plots
# Adjust the number of columns as needed
arranged_plots <- grid.arrange(grobs = exercise_plots, ncol = 2)

# Save the arranged plots as a .png file
file_name_plot <- paste0(output_directory, "/", date, "TRAINED_arranged_plots_RTsession__load.png")
ggsave(file_name_plot, arranged_plots, width = 8.5, height = 11, units = "in", dpi = 1200)


######################################################################
#chart mean volume load over weeks
data <- read_excel("MRP.xlsx", sheet = "Exercise Session RT", col_names = TRUE)
data$Exercise <- recode(data$Exercise, !!!replacements)
head(data)

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "RTsession_raw_data.xlsx")
write_xlsx(data, file_name)

# Filter the data and get unique Exercise and Subject in a single pipeline
data <- data %>%
  filter(Exercise != "BentRow", 
         !(Subject %in% c("SUB 4", "SUB 5", "SUB 13", "SUB 2")))
data <- data %>%
  filter(Subject %in% trained_subjects)

unique(data$Subject)

# Display unique values for Exercise and Subject after filtering
unique(data$Exercise)
unique(data$Subject)

# Filter and gather the relevant columns
volume_load_data <- data %>%
  filter(Measure %in% c("lbs", "Reps")) %>%
  gather(Week, Value, starts_with("Week")) %>%
  spread(Measure, Value)

# Calculate the volume load for each week
volume_load_data <- volume_load_data %>%
  mutate(Volume_Load = lbs * Reps)

# Pivot the data back into a wide format
volume_load_wide <- volume_load_data %>%
  select(-lbs, -Reps) %>%
  spread(Week, Volume_Load)

# Print the new wide-format data
print(volume_load_wide)

# Conversion factor from lbs to kg
lbs_to_kg <- 0.45359237

# Columns containing the week values
week_columns <- c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6",
                  "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Week 12")

# Convert lbs values to kg for the specified week columns
volume_load_wide[week_columns] <- volume_load_wide[week_columns] * lbs_to_kg
print(volume_load_wide)

# Reshape the data to long format
volume_load_long <- volume_load_wide %>%
  pivot_longer(cols = starts_with("Week"),
               names_to = "Week",
               names_prefix = "Week ",
               values_to = "Value") %>% 
  drop_na()  # Remove rows with missing values

head(volume_load_long)

volume_load_long <- volume_load_long %>%
  filter(as.numeric(Week) <= 10)
unique(volume_load_long$Week)

# Calculate the mean and standard deviation for each exercise-week combination
exercise_summary <- volume_load_long %>%
  group_by(Exercise, Week) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE),
            sd_value = sd(Value, na.rm = FALSE))


# Convert the week column to factor with numeric levels in the desired order
exercise_summary$Week <- factor(exercise_summary$Week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

# Get unique exercise names
unique_exercises <- unique(exercise_summary$Exercise)

# Create a list to store plots
exercise_plots <- list()

# Loop through unique exercise names and create plots
for (exercise in unique_exercises) {
  plot_data <- exercise_summary %>%
    filter(Exercise == exercise)
  
  exercise_plot <- ggplot(plot_data, aes(x = Week, y = mean_value)) +
    geom_bar(stat = "identity", fill = "black") +
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                  width = 0.2) +
    labs(x = "Week", y = "Volume Load",
         title = paste(exercise)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line())
  
  exercise_plots[[exercise]] <- exercise_plot
}

# Arrange and display the plots
# Adjust the number of columns as needed
arranged_plots <- grid.arrange(grobs = exercise_plots, ncol = 2)
arranged_plots

# Save the arranged plots as a .png file
file_name_plot <- paste0(output_directory, "/", date, "TRAINED_RTsession_volume_load.png")
ggsave(file_name_plot, arranged_plots, width = 11, height = 8.5, units = "in", dpi = 1200)


######
# plot volume load as percent 1RM
#this object is generated at the beginning, maybe move the code 
RM1_data

# Join the load_data with RM1_data on Subject and Exercise
joined_data <- load_data %>%
  left_join(RM1_data, by = c("Subject", "Exercise"))

# Convert the values in columns for Week 1 to Week 12 to percentages
percentage_data <- joined_data %>%
  pivot_longer(cols = starts_with("Week"), names_to = "Week", values_to = "Value") %>%
  mutate(Percentage = (Value / PRE)) %>%
  select(-Value, -PRE) %>%
  pivot_wider(names_from = "Week", values_from = "Percentage")
percentage_data$Measure <- gsub("kg", "%1RM", percentage_data$Measure)

# Print the result
print(percentage_data)

data <- read_excel("MRP.xlsx", sheet = "Exercise Session RT", col_names = TRUE)
data$Exercise <- recode(data$Exercise, !!!replacements)
head(data)

# Filter the data and get unique Exercise and Subject in a single pipeline
data <- data %>%
  filter(Exercise != "BentRow", 
         !(Subject %in% c("SUB 4", "SUB 5", "SUB 13", "SUB 2")))

data <- data %>%
  filter(Subject %in% trained_subjects)

unique(data$Subject)
# Filter and gather the relevant columns
volume_load_data <- data %>%
  filter(Measure %in% c("lbs", "Reps")) %>%
  gather(Week, Value, starts_with("Week")) %>%
  spread(Measure, Value)

# Convert percentage_data to long format for easier merging
percentage_data_long <- tidyr::pivot_longer(percentage_data, cols = starts_with("Week"), names_to = "Week", values_to = "Percentage")

# Create merged_data data frame
merged_data <- dplyr::left_join(volume_load_data, percentage_data_long, by = c("Subject", "Exercise", "Week"))

# View the merged_data to confirm changes
print(merged_data)

# Calculate the volume load for each week
merged_data <-  merged_data %>%
  mutate(Percent_Volume_Load = Percentage * Reps)%>%
  filter(Week != c("Week 11", "Week 12"))%>%
  select(-c(lbs, Reps, Percentage, Measure))

# Calculate the mean and standard deviation for each exercise-week combination
exercise_summary <- merged_data %>%
  mutate(Week = as.numeric(gsub("Week ", "", Week)))  %>%
  group_by(Exercise, Week) %>%
  summarise(mean_value = mean(Percent_Volume_Load, na.rm = TRUE),
            sd_value = sd(Percent_Volume_Load, na.rm = FALSE))


# Convert the week column to factor with numeric levels in the desired order
exercise_summary$Week <- factor(exercise_summary$Week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

# Get unique exercise names
unique_exercises <- unique(exercise_summary$Exercise)

# Create a list to store plots
exercise_plots <- list()

# Loop through unique exercise names and create plots
for (exercise in unique_exercises) {
  plot_data <- exercise_summary %>%
    filter(Exercise == exercise)
  
  exercise_plot <- ggplot(plot_data, aes(x = Week, y = mean_value)) +
    geom_bar(stat = "identity", fill = "black") +
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                  width = 0.2) +
    labs(x = "Week", y = "Relative Volume Load",
         title = paste(exercise)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line())
  
  exercise_plots[[exercise]] <- exercise_plot
}
# Arrange and display the plots
# Adjust the number of columns as needed
arranged_plots <- grid.arrange(grobs = exercise_plots, ncol = 3)
arranged_plots

# Save the arranged plots as a .png file
file_name_plot <- paste0(output_directory, "/", date, "TRAINED_RTsession_relative_volume_load.png")
ggsave(file_name_plot, arranged_plots, width = 11, height = 8.5, units = "in", dpi = 1200)


#compare total week to week %change in volume to 10rm/1rm %change

# Filter the relevant data
exercise_data <- data %>%
  filter(Measure %in% c("lbs", "Reps"))

# Calculate the volume load for each week
exercise_data <- exercise_data %>%
  pivot_longer(cols = starts_with("Week"), names_to = "Week", values_to = "Value") %>%
  pivot_wider(names_from = "Measure", values_from = "Value") %>%
  mutate(Volume_Load = lbs * Reps)

# Calculate percentage change in volume load from one week to the next
exercise_data <- exercise_data %>%
  group_by(Exercise) %>%
  mutate(Percent_Change = (Volume_Load - lag(Volume_Load)) / lag(Volume_Load) * 100)

file_name <- paste0(output_directory, "/", date, "TRAINED_VL_exercise_percent_change.xlsx")
write_xlsx(exercise_data, file_name)

# # Calculate mean and standard deviation of percentage changes for each exercise
exercise_percent_changes <- exercise_data %>%
  group_by(Exercise) %>%
  summarise(
    Mean_Percent_Change = mean(Percent_Change, na.rm = TRUE),
    Stdev_Percent_Change = sd(Percent_Change, na.rm = TRUE),
    Sum_Percent_Change = sum(Percent_Change, na.rm = TRUE),
    Average_Percent_Change = sum(Percent_Change, na.rm = TRUE) / (n_distinct(Subject))
  )
print(exercise_percent_changes)

# Write the results to a CSV file
file_name <- paste0(output_directory, "/", date, "TRAINED_VL_exercise_percent_change_summary.xlsx")
write_xlsx(exercise_percent_changes, file_name)
# > exercise_percent_changes
# # A tibble: 6 × 5
# Exercise    Mean_Percent_Change     Stdev_Percent_Change    Sum_Percent_Change  Average_Percent_Change
# 1 Bench Press                4.66                 22.6               345.                   43.1
# 2 Deadlift                   3.27                 17.1               242.                   30.2
# 3 LatPull                    3.52                 12.9               260.                   32.5
# 4 LegPress                   6.67                 26.3               493.                   61.7
# 5 OHP                        3.86                 17.3               285.                   35.7
# 6 SeatedRow                  5.28                 16.4               391.                   48.9

# Calculate mean and standard deviation of percentage changes for each exercise and subject
exercise_percent_changes_subject <- exercise_data %>%
  group_by(Exercise, Subject) %>%
  summarise(
    Mean_Percent_Change = mean(Percent_Change, na.rm = TRUE),
    Stdev_Percent_Change = sd(Percent_Change, na.rm = TRUE),
    Sum_Percent_Change = sum(Percent_Change, na.rm = TRUE),
    First_Last_Percent_Change = (last(na.omit(Volume_Load)) - first(na.omit(Volume_Load))) / first(na.omit(Volume_Load)) * 100,
    Max_Min_Percent_Change = (max(Volume_Load , na.rm = TRUE) - min(Volume_Load , na.rm = TRUE)) / min(Volume_Load , na.rm = TRUE) * 100,
    Sum_Volume_Load = sum(Volume_Load[!is.na(Volume_Load)], na.rm = TRUE),
    Average_Percent_Change = sum(Volume_Load[!is.na(Volume_Load)], na.rm = TRUE) / sum(!is.na(Volume_Load))
  )

# Print the result
print(exercise_percent_changes_subject)
file_name <- paste0(output_directory, "/", date, "TRAINED_VL_exercise_percent_changes_bysubject.xlsx")
write_xlsx(exercise_percent_changes_subject, file_name)

#merge this table with 1RM % change pre/post per subject
# Read in the Excel file and perform all transformations in a single pipeline
mydata <- read_excel("MRP.xlsx", sheet = "Strength", col_names = TRUE) %>%
  select(1:3, 7:9) %>%
  rename(Subject = 1, Measure = 2, Exercise = 3, PRE = 4, MID = 5, POST = 6) %>%
  filter(!(Subject %in% c("SUB 5", "SUB 13"))) %>%
  mutate(
    PRE = as.numeric(PRE),
    MID = as.numeric(MID),
    POST = as.numeric(POST)
  )
# Define the replacements
replacements <- c(
  "Overhead Press" = "OHP",
  "Lat Pull down" = "LatPull",
  "Leg Press" = "LegPress",
  "Bent Over Row" = "BentRow",
  "Seated Row" = "SeatedRow",
  "Lat Pulldown" = "LatPull"
)
# Apply the replacements to the Exercise column
mydata$Exercise <- recode(mydata$Exercise, !!!replacements)
mydata <- mydata %>%
  filter(Exercise != "BentRow")

RM_percent_changes <- mydata %>%
  group_by(Exercise, Subject) %>%
  summarise(
    Pre_Post_Change_1RM = (last(POST[Measure == "1RM"]) - first(PRE[Measure == "1RM"])) / first(PRE[Measure == "1RM"]) * 100,
    Pre_Post_Change_10RM = (last(POST[Measure == "10RM"]) - first(PRE[Measure == "10RM"])) / first(PRE[Measure == "10RM"]) * 100
  )

combined_data <- left_join(exercise_percent_changes_subject, RM_percent_changes, by = c("Exercise", "Subject"))
print(combined_data)
file_name <- paste0(output_directory, "/", date, "TRAINED_Load_correlation.xlsx")
write_xlsx(combined_data, file_name)


# Using base R cor() function
cor_matrix_base <- cor(combined_data[, c("Mean_Percent_Change", "Sum_Percent_Change", "Sum_Volume_Load", "First_Last_Percent_Change", "Max_Min_Percent_Change", "Pre_Post_Change_1RM", "Pre_Post_Change_10RM")])
print(cor_matrix_base)
# Create an empty matrix for p-values
p_values_base <- matrix(NA, nrow(cor_matrix_base), ncol(cor_matrix_base))
# Loop through rows and columns to calculate p-values
for (i in 1:nrow(cor_matrix_base)) {
  for (j in 1:ncol(cor_matrix_base)) {
    if (i != j) {
      cor_test <- cor.test(cor_matrix_base[, i], cor_matrix_base[, j])
      p_values_base[i, j] <- cor_test$p.value
    }
  }
}

# Print correlation matrix and p-values
print(cor_matrix_base)
print(p_values_base)
#1RM p value below 0.05 for mean and sum, 0.07 for first last and max min 
####### Analysis 
# No correlation
#1RM is correlated with 10RM .63 
# exercise data is correlated with itself, which should be obvious 
# what about cumulative volume load? 
# 0.2 which is the highest

# List to store correlation matrices
correlation_matrices <- list()

# Loop through each unique exercise to compute the correlation matrix
for (exercise in unique(combined_data$Exercise)) {
  subset_data <- filter(combined_data, Exercise == exercise)
  correlation_matrix <- cor(subset_data[, -c(1, 2)], use = "complete.obs")  # Remove non-numeric columns
  correlation_matrices[[exercise]] <- correlation_matrix
}

library(broom)
# Define a function to calculate correlation and p-value
calculate_correlation <- function(df, var1, var2) {
  test_result <- cor.test(df[[var1]], df[[var2]])
  tidy_result <- tidy(test_result)
  return(tidy_result)
}

# List of variables to correlate
variables_to_correlate <- c("Mean_Percent_Change", "Stdev_Percent_Change", "Sum_Percent_Change", 
                            "First_Last_Percent_Change", "Max_Min_Percent_Change", "Sum_Volume_Load",
                            "Average_Percent_Change", "Pre_Post_Change_1RM", "Pre_Post_Change_10RM")

# Calculate correlations
correlation_results <- combined_data %>%
  group_by(Exercise) %>%
  do({
    exercise_data = .
    cor_list <- list()
    for (var1 in variables_to_correlate) {
      for (var2 in variables_to_correlate) {
        if (var1 != var2) {
          cor_result <- calculate_correlation(exercise_data, var1, var2)
          cor_result$var1 <- var1
          cor_result$var2 <- var2
          cor_list[[length(cor_list) + 1]] <- cor_result
        }
      }
    }
    bind_rows(cor_list)
  })

# View the results
print(correlation_results)
file_name <- paste0(output_directory, "/", date, "_TRAINED_correlation_results.xlsx")
write_xlsx(correlation_results, file_name)

# Create a new Excel workbook
wb <- createWorkbook()
# Add sheets to the workbook
addWorksheet(wb, "Correlation")
addWorksheet(wb, "p values")
addWorksheet(wb, "Correlation_per_exercise")
# Write correlation matrices to the Excel sheets
writeData(wb, sheet = "Correlation", x = cor_matrix_base, startCol = 1, startRow = 1)
writeData(wb, sheet = "Correlation_per_exercise", x = correlation_matrices, startCol = 1, startRow = 1)
writeData(wb, sheet = "p values", x = p_values_base, startCol = 1, startRow = 1)
file_name <- paste0(output_directory, "/", date, "_correlation_matrix.xlsx")
saveWorkbook(wb, file_name)












































