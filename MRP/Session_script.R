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
# Define color values
dark_blue_color <- "#00205B"   # Dark blue color
light_blue_color <- "#6699CC"  # Light blue color
third_color <- "#A0C9E0"       # Third color

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
  dplyr::select(1:3, 7:9) %>%
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
  dplyr::select(Subject, Exercise, PRE)

replacements <- c(
  "Overhead Press" = "OHP",
  "Lat Pull down" = "Lat Pull",
  "Leg Press" = "Leg Press",
  "Bent Over Row" = "Bent Row",
  "Seated Row" = "Seated Row",
  "Lat Pulldown" = "Lat Pull"
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
            sd_value = sd(Value, na.rm = TRUE))

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
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                  width = 0.2) +
    geom_bar(stat = "identity", fill = dark_blue_color) +
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
arranged_plots <- grid.arrange(grobs = exercise_plots, ncol = 3)

# Save the arranged plots as a .png file
file_name_plot <- paste0(output_directory, "/", date, "arranged_plots_RTsession__load.png")
ggsave(file_name_plot, arranged_plots, width = 7, height = 4, units = "in", dpi = 1200)


######################################################################

# Load data
data <- read_excel("MRP.xlsx", sheet = "Exercise Session RT", col_names = TRUE) %>%
  mutate(Exercise = dplyr::recode(Exercise, !!!replacements))

# Save the initial data with the current date
file_name <- paste0(output_directory, "/", Sys.Date(), "RTsession_raw_data.xlsx")
write_xlsx(data, file_name)

# Pre-filter data for efficiency
filtered_data <- data %>%
  filter(Exercise != "BentRow", !Subject %in% c("SUB 4", "SUB 5", "SUB 13", "SUB 2"))


# Filter for "lbs" and "Reps", then calculate Volume Load directly in long format
volume_load_long <- filtered_data %>%
  filter(Measure %in% c("lbs", "Reps")) %>%
  pivot_longer(cols = starts_with("Week"), names_to = "Week", values_to = "Value") %>%
  pivot_wider(names_from = Measure, values_from = Value) %>%
  mutate(Volume_Load = lbs * Reps) %>%
  select(-c(lbs, Reps)) %>%
  rename_with(~ gsub("Week ", "", .), starts_with("Week")) %>%
  mutate(across(starts_with("Week"), ~ .x * lbs_to_kg)) %>%
  drop_na() %>%
  filter(as.numeric(gsub("Week", "", Week)) <= 10)




#chart mean volume load over weeks
data <- read_excel("MRP.xlsx", sheet = "Exercise Session RT", col_names = TRUE)
data$Exercise <- dplyr::recode(data$Exercise, !!!replacements)
head(data)

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "RTsession_raw_data.xlsx")
write_xlsx(data, file_name)

# Filter the data and get unique Exercise and Subject in a single pipeline
data <- data %>%
  filter(Exercise != "BentRow", 
         !(Subject %in% c("SUB 4", "SUB 5", "SUB 13", "SUB 2")))

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
 dplyr::select(-lbs, -Reps) %>%
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
            sd_value = sd(Value, na.rm = TRUE))


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
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                  width = 0.2) +
    geom_bar(stat = "identity", fill = dark_blue_color) +
    labs(x = "Week", y = "Volume Load",
         title = paste(exercise)) +
        theme_minimal() +
    scale_y_continuous(limits = c(0, 3250), breaks = seq(0, 3250, by = 500)) +
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
file_name_plot <- paste0(output_directory, "/", date, "RTsession_volume_load.png")
ggsave(file_name_plot, arranged_plots, width = 7, height = 4, units = "in", dpi = 1200)


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
 dplyr::select(-Value, -PRE) %>%
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
 dplyr::select(-c(lbs, Reps, Percentage, Measure))

# Calculate the mean and standard deviation for each exercise-week combination
exercise_summary <- merged_data %>%
  mutate(Week = as.numeric(gsub("Week ", "", Week)))  %>%
  group_by(Exercise, Week) %>%
  summarise(mean_value = mean(Percent_Volume_Load, na.rm = TRUE),
            sd_value = sd(Percent_Volume_Load, na.rm = TRUE))


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
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                  width = 0.2) +
    geom_bar(stat = "identity", fill = dark_blue_color) +
     geom_hline(yintercept = 15, linetype="dashed", color = third_color, linewidth = 1) +  # Adding horizontal line at y = 15
     geom_hline(yintercept = 22.5, linetype="dashed", color = light_blue_color , linewidth = 1) +  # Adding horizontal line at y = 22.5
    
    labs(x = "Week", y = "Relative Volume Load",
         title = paste(exercise)) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 35), breaks = seq(0, 30, by = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
        #  axis.line = element_line()
        )
  
  exercise_plots[[exercise]] <- exercise_plot
}
# Arrange and display the plots
# Adjust the number of columns as needed
arranged_plots <- grid.arrange(grobs = exercise_plots, ncol = 3)
arranged_plots

# Save the arranged plots as a .png file
file_name_plot <- paste0(output_directory, "/", date, "RTsession_relative_volume_load_LINE.png")
ggsave(file_name_plot, arranged_plots, width = 8, height = 5, units = "in", dpi = 1200)


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

file_name <- paste0(output_directory, "/", date, "VL_exercise_percent_change.xlsx")
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
file_name <- paste0(output_directory, "/", date, "VL_exercise_percent_change_summary.xlsx")
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
file_name <- paste0(output_directory, "/", date, "VL_exercise_percent_changes_bysubject.xlsx")
write_xlsx(exercise_percent_changes_subject, file_name)

#merge this table with 1RM % change pre/post per subject
# Read in the Excel file and perform all transformations in a single pipeline
mydata <- read_excel("MRP.xlsx", sheet = "Strength", col_names = TRUE) %>%
 dplyr::select(1:3, 7:9) %>%
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
file_name <- paste0(output_directory, "/", date, "Load_correlation.xlsx")
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
file_name <- paste0(output_directory, "/", date, "_correlation_results.xlsx")
write_xlsx(correlation_results, file_name)


#Errors sometimes. Do it manually
# # Create a new Excel workbook
# wb <- createWorkbook()
# # Add sheets to the workbook
# addWorksheet(wb, "Correlation")
# addWorksheet(wb, "p values")
# addWorksheet(wb, "Correlation_per_exercise")
# # Write correlation matrices to the Excel sheets
# writeData(wb, sheet = "Correlation", x = cor_matrix_base, startCol = 1, startRow = 1)
# writeData(wb, sheet = "Correlation_per_exercise", x = correlation_matrices, startCol = 1, startRow = 1)
# writeData(wb, sheet = "p values", x = p_values_base, startCol = 1, startRow = 1)
# file_name <- paste0(output_directory, "/", date, "_correlation_matrix.xlsx")
# saveWorkbook(wb, file_name)


#import and analyze HIIT session data #append to VO2script
#Compare to Vo2max changed

# read in the Excel file
mydata <- read_excel("MRP.xlsx", sheet = "Exercise session HIIT", skip = 3, col_names = FALSE)
head(mydata)
colnames(mydata)

# Generate the new column names
new_column_names <- c("Subject", "Bout")
for (week in 1:12) {
  new_column_names <- c(new_column_names, paste0(week, "HR"), paste0(week, "RPE"))
}
colnames(mydata) <- new_column_names
# Print the data with new column names
print(mydata)

# Filter rows where Subjects did not complete 12 weeks of testing
mydata <- mydata %>%
  filter(Subject != "SUB 4" & Subject != "SUB 5" & Subject != "SUB 13")

unique(mydata$Subject)

# Convert the table to long format using gather
data_long <- gather(mydata, key, value, -Subject, -Bout, na.rm = TRUE)

# Separate the key column into week and measure columns
data_long <- separate(data_long, key, into = c("week", "measure"), sep = "(?<=[0-9])(?=[A-Za-z])")
# In this code, we use the regular expression pattern "(?<=[0-9])(?=[A-Za-z])" as the sep argument. This pattern uses positive 
# lookbehind (?<=[0-9]) to split the column where there is a numeric value before the character value, and 
# positive lookahead (?=[A-Za-z]) to split the column where there is a character value after the numeric value.
# This way, we can correctly separate the key column into week and measure columns, even for double-digit numeric values.
#Alternatively, the below code uses sep = # of characters. 
#data_long <- separate(data_long, key, into = c("week", "measure"), sep = 1)
# Print the updated data
print(data_long)
head(data_long)
unique(data_long$Bout)
unique(data_long$week)

#convert load with calibrated chart levels to Watts 
data_frame <- data.frame(
  Data = c(6, 9, 10, 11, 13, 14, 15, 16, 17, 17.5, 18, 19, 20, 21, 21, 22, 23, 24, 25),
  Watt = seq(120, 300, by = 10)
)

#Filter load Rows, remove measure column, remove duplicates with distinct
load_rows <- data_long %>%
  filter(Bout == "Load") %>%
 dplyr::select(-measure) %>%
  distinct()%>%
  mutate(value = as.numeric(value))

print(load_rows)

# Join the dataframes to convert Load resistance raw levels to Watts
converted_data <- load_rows %>%
  left_join(data_frame, by = c("value" = "Data"))
#This was necessary as some bouts in early weeks started far too light and we increased during the bout, 
#we averaged the levels, but this created non-integer values which must be interpolated from our calibrated chart 
converted_data <- converted_data %>%
  mutate(Watt_Interpolated = ifelse(Bout == "Load", approx(value, Watt, xout = value)$y, value))

load_rows <- converted_data %>%
 dplyr::select(-Watt, -value) %>%
  rename(Watts = Watt_Interpolated) %>%
  filter(as.numeric(week) <= 10)

print(load_rows)
unique(load_rows$week)

# Calculate the mean and standard deviation per week for the filtered data
load_summary <- load_rows %>%
  group_by(week) %>%
  summarise(mean_load = mean(Watts), sd_load = sd(Watts))

# Convert the week column to factor with numeric levels in the desired order
load_summary$week <- factor(load_summary$week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
# Create the plot
load_plot <- ggplot(load_summary, aes(x = week, y = mean_load)) +
  geom_col(fill = "darkgray", width = 0.7) +
  geom_errorbar(aes(ymin = mean_load - sd_load, ymax = mean_load + sd_load), width = 0.25) +
  labs(x = "Week", y = "Watts", title = "Mean Load per Week") +
  theme_minimal()

print(load_plot)

file_name_plot <- paste0(output_directory, "/", date, "HIIT_session_load.png")
ggsave(file_name_plot, load_plot, width = 11, height = 8.5, units = "in", dpi = 1200)


# Extract, filter, and transform data in a single pipeline
mean_load_rows <- data_long %>%
  filter(Bout %in% c("Mean", "Max")) %>%
 dplyr::select(-measure) %>%
  mutate(
    value = as.numeric(value),
    Bout = ifelse(Bout == "Mean", "meanHR", ifelse(Bout == "Max", "MaxHR", Bout))
  ) %>%
  filter(value >= 25)  # Filter values under 25

# Create a data frame
age_frame <- data.frame(
  Subject = c("SUB 3", "SUB 8", "SUB 10", "SUB 14", "SUB 17", "SUB 18", "SUB 2", "SUB 11", "SUB 19"),
  Age = c(37, 28.9, 29.1, 33.8, 22.8, 33.6, 38, 20.7, 31.2)
)

# This uses the inner_join to merge mean_load_rows and age_frame, and then mutate to add the new columns. 
merged_data_age <- mean_load_rows %>%
  inner_join(age_frame, by = "Subject") %>%
  mutate(
    APMHR = 220 - Age,
    Percentage_APMHR = (value / APMHR) * 100
  )

# Display the resulting data
print(merged_data_age)

# Define the subjects that are trained and untrained
trained_subjects <- c(8, 17, 18, 19)
untrained_subjects <- c(2, 3, 10, 11, 14)

trained_subjects <- paste0("SUB ", trained_subjects)
untrained_subjects <- paste0("SUB ", untrained_subjects)

mean_load_rows <- merged_data_age %>%
  filter( (as.numeric(week) <= 10)) %>%
  filter(!(week %in% c("11", "12"))) 
  #Trained
#  filter(Subject %in% untrained_subjects) 
#Untrained
#  filter(Subject %in% untrained_subjects) 


unique(mean_load_rows$week)
unique(mean_load_rows$Subject)

head(mean_load_rows)
# # Separate 'Load' and 'HR' values into separate tables
 load_table <- load_rows %>%
   filter(Bout == "Load") %>%
  dplyr::select(Subject, week, Watts)

# read in the data from the Excel file and create table of Wmax values 
mydata <- read_excel("MRP.xlsx", sheet = "vo2data", col_names = TRUE)

Wmax_data <- mydata %>%
  filter(Measure == "Wmax") %>%
 dplyr::select(Subject, PRE)

percentage_load_table <- load_table %>%
  left_join(Wmax_data, by = "Subject") %>%
  mutate(Percentage_Wmax = (Watts / PRE) * 100) %>%
 dplyr::select(Subject, week, Percentage_Wmax)

load_table <- percentage_load_table

hr_table <- mean_load_rows %>%
  filter(Bout == "MaxHR") %>%
 dplyr::select(Subject, week, Percentage_APMHR)

# Merge tables by 'Subject' and 'week'
merged_data <- full_join(load_table, hr_table, by = c("Subject", "week"))

# Fit a linear model to the data
model <- lm(Percentage_APMHR ~ Percentage_Wmax, data = merged_data)

# Compute the R-squared value
r2 <- summary(model)$r.squared

# Create the plot
load_hr_plot <- ggplot(merged_data, aes(x = Percentage_Wmax, y = Percentage_APMHR)) +
  geom_point(aes(color = as.numeric(week)), size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear regression line
  labs(x = "%Wmax", y = "%APMHR", color = "Week") +
  theme_minimal(base_size = 18) +  # Set base font size to 18
  scale_color_gradient(low = "blue", high = "red") + # Apply gradient color scale
  geom_hline(yintercept = c(85, 95), linetype="dashed", color = "black") +  # Add horizontal lines at 85 and 95
  annotate("text", x = Inf, y = Inf, vjust = 1, hjust = 1,
           label = paste("R^2 =", round(r2, 3)), # Add R^2 value as text
           size = 4) +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black"),  # Add black axis lines
    axis.ticks = element_line(color = "black")  # Add black tick marks
    #legend.position = c(0.8, 0.4)  # Adjust legend position (if needed)
  )

# Print the plot
print(load_hr_plot)


file_name_plot <- paste0(output_directory, "/", date, "_MAX_HIIT_session_HR_Load_Scatter.png")
ggsave(file_name_plot, load_hr_plot, width = 9, height = 6, units = "in", dpi = 1200)

# Create the plot
load_hr_plot <- ggplot(merged_data, aes(x = Percentage_Wmax, y = Percentage_APMHR)) +
  geom_point(aes(color = as.numeric(week))) + # Color dots by week as a numeric variable
  geom_smooth(method = "loess", se = FALSE, color = "red") + # Add Loess smoothing curve
  labs(x = "%Wmax", y = "%APMHR", title = "Relationship between HR and Load", color = "Week") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red") # Apply gradient color scale

# Print the plot
load_hr_plot

file_name_plot <- paste0(output_directory, "/", date, "_HIIT_session_HR_Load_Scatter_Loess.png")
ggsave(file_name_plot, load_hr_plot, width = 11, height = 8.5, units = "in", dpi = 1200)

# Calculate the ratio of HR to Load
merged_data <- merged_data %>%
  mutate(HR_Load_Ratio = Percentage_APMHR / Percentage_Wmax)
  #Trained
 # filter(Subject %in% untrained_subjects) 

# Calculate mean and standard deviation of HR_Load_Ratio across weeks
ratio_summary <- merged_data %>%
  group_by(week) %>%
  summarise(mean_ratio = mean(HR_Load_Ratio),
            sd_ratio = sd(HR_Load_Ratio))

# Convert the week column to factor with numeric levels in the desired order
ratio_summary$week <- factor(ratio_summary$week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# Fit linear model
lm_fit <- lm(mean_ratio ~ as.numeric(week), data = ratio_summary)
# Extract R-squared value
r2 <- summary(lm_fit)$r.squared

# Create the plot
mean_ratio_plot <- ggplot(ratio_summary, aes(x = week, y = mean_ratio, group = 1)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear regression line
  geom_errorbar(aes(ymin = mean_ratio - sd_ratio, ymax = mean_ratio + sd_ratio), width = 0.25) +
  labs(x = "Week", y = "HR/Load Ratio",
       title = "Mean HR/Load Ratio",
       group = "1") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  annotate("text", x = Inf, y = Inf, label = paste("R^2 =", round(r2, 3)),
           vjust = "top", hjust = "right") # Add R-squared value as text annotation
print(mean_ratio_plot)

# Save the plot as a PNG file
file_name_plot <- paste0(output_directory, "/", date, "_mean_ratio_plot.png")
ggsave(file_name_plot, mean_ratio_plot, width = 11, height = 8.5, units = "in", dpi = 1200)

#### max
hr_table <- mean_load_rows %>%
  filter(Bout == "MaxHR") %>%
 dplyr::select(Subject, week, Percentage_APMHR)
  #Trained
 # filter(Subject %in% untrained_subjects) 

# Merge tables by 'Subject' and 'week'
merged_data <- full_join(load_table, hr_table, by = c("Subject", "week"))

# Create the plot
max_load_hr_plot <- ggplot(merged_data, aes(x = Percentage_Wmax, y = Percentage_APMHR)) +
  geom_point(aes(color = as.numeric(week))) + # Color dots by week as a numeric variable
  geom_smooth(method = "loess", se = FALSE, color = "red") + # Add Loess smoothing curve
  labs(x = "%Wmax", y = "%APMHR", title = "Relationship between HR and Load", color = "Week") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red") # Apply gradient color scale
print(max_load_hr_plot)

# Calculate the ratio of HR to Load
merged_data <- merged_data %>%
  mutate(HR_Load_Ratio = Percentage_APMHR / Percentage_Wmax)%>%
  #Trained
  filter(Subject %in% untrained_subjects) 

# Calculate mean and standard deviation of HR_Load_Ratio across weeks
ratio_summary <- merged_data %>%
  group_by(week) %>%
  summarise(mean_ratio = mean(HR_Load_Ratio),
            sd_ratio = sd(HR_Load_Ratio))

# Convert the week column to factor with numeric levels in the desired order
ratio_summary$week <- factor(ratio_summary$week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# Fit linear model for ratio_plot
lm_fit_ratio <- lm(mean_ratio ~ as.numeric(week), data = ratio_summary)

# Extract R-squared value for ratio_plot
r2_ratio <- summary(lm_fit_ratio)$r.squared

# Create the plot
ratio_plot <- ggplot(ratio_summary, aes(x = week, y = mean_ratio, group = 1)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear regression line
  geom_errorbar(aes(ymin = mean_ratio - sd_ratio, ymax = mean_ratio + sd_ratio), width = 0.25) +
  labs(x = "Week\nUntrained", y = "HR/Load Ratio",
       title = "Max HR/Load Ratio",
       group = "1") +
  scale_y_continuous(limits = c(0.60, 1.15), breaks = seq(0.7, 1.2, by = 0.1)) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  annotate("text", x = Inf, y = Inf, label = paste("R^2 =", round(r2_ratio, 3)),
           vjust = "top", hjust = "right") # Add R-squared value as text annotation

# Display the plot
print(ratio_plot)
print(mean_ratio_plot)

# Save the ratio_plot as an image
file_name_plot <- paste0(output_directory, "/", date, "UNTRAINED_ratio_plot.png")
ggsave(file_name_plot, ratio_plot, width = 6, height = 4, units = "in", dpi = 1200)

#Barplot

# Create the bar plot
ratio_plot <- ggplot(ratio_summary, aes(x = week, y = mean_ratio, group = 1)) +
   geom_errorbar(aes(ymin = mean_ratio - sd_ratio, ymax = mean_ratio + sd_ratio), width = 0.25) +
  geom_bar(stat = "identity", fill = dark_blue_color) +  # Change to bar plot
  geom_smooth(method = "lm", se = FALSE, color = light_blue_color) + # Add linear regression line
  labs(x = "Week\nUntrained", y = "HR/Load Ratio",
       title = "Max HR/Load Ratio",
       group = "1") +
   scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1.2, by = 0.2)) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  annotate("text", x = Inf, y = Inf, label = paste("R^2 =", round(r2_ratio, 3)),
           vjust = "top", hjust = "right")  # Add R-squared value as text annotation

# Print the plot
print(ratio_plot)

# Save the ratio_plot as an image
file_name_plot <- paste0(output_directory, "/", date, "_BAR_UNTRAINED_ratio_plot.png")
ggsave(file_name_plot, ratio_plot, width = 6, height = 4, units = "in", dpi = 1200)



# Calculate mean and standard deviation of HR_Load_Ratio across weeks
ratio_summary_perSub <- merged_data %>%
  group_by(week,Subject) %>%
  summarise(mean_ratio = mean(HR_Load_Ratio))%>%
  #Trained
  filter(Subject %in% untrained_subjects) 

ratio_summary_perSub_plot <- ggplot(ratio_summary_perSub, aes(x = as.numeric(week), y = mean_ratio, group = Subject, color = Subject)) +
  geom_line() +
  geom_point() +
  labs(x = "Week", y = "Mean Ratio", title = "Mean Ratio Per Subject Over Weeks") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = "Subject") # Add legend title
print(ratio_summary_perSub_plot)

# Save the ratio_plot as an image
file_name_plot <- paste0(output_directory, "/", date, "_ratio_summary_perSub_plot.png")
ggsave(file_name_plot, ratio_summary_perSub_plot, width = 11, height = 8.5, units = "in", dpi = 1200)


# Filter the data to include only weeks 1 and 10
filtered_data <- ratio_summary_perSub %>%
  filter(week %in% c("1", "10"))

# Spread the data to wide format
wide_data <- filtered_data %>%
  spread(key = week, value = mean_ratio)

# Calculate the percentage change from week 1 to week 10
final_data <- wide_data %>%
  mutate(percent_change = -((`10` - `1`) / `1`) * 100) %>%
 dplyr::select(Subject, percent_change)

# Print the final result
print(final_data)

#Sub 3 manual calc. 
filter(ratio_summary_perSub, Subject == "SUB 3")
((0.89 - 1.08) / 1.08) * 100
value_to_assign <- 17.59259

# Assigning the value to SUB 3 in the percent_change column
final_data <- final_data %>%
  mutate(percent_change = if_else(Subject == "SUB 3", value_to_assign, percent_change))


mydata <- read_excel("MRP.xlsx", sheet = "vo2data", col_names = TRUE)
vo2data <- filter(mydata, Measure == "VO2Max")

vo2data <- vo2data %>%
  mutate(Difference_POST_PRE = POST - PRE) %>%
  mutate(percent_change = (Difference_POST_PRE / PRE) * 100) %>%
 dplyr::select(Subject, Measure, PRE, MID, POST, Difference_POST_PRE, percent_change)

# Merge final_data and vo2data on the "Subject" column
merged_data <- final_data %>%
  inner_join(vo2data, by = "Subject")
  # #Trained
  # filter(Subject %in% untrained_subjects) 

# Filter out the rows where Subject is "SUB 18"
#Sub 18 had a poor vo2max final test.  
merged_data <- merged_data %>% filter(Subject != "SUB 18")

# Fit a linear model to calculate R-squared
lm_fit <- lm(percent_change.y ~ percent_change.x, data = merged_data)
r2 <- summary(lm_fit)$r.squared

# Scatter plot comparing percent changes with linear regression line and R-squared annotation
percent_change_plot <- ggplot(merged_data, aes(x = percent_change.x, y = percent_change.y)) +
  geom_point(aes(color = Subject)) + # Color points by Subject
  geom_text_repel(aes(label = Subject), box.padding = 0.5, point.padding = 0.5) + # Use ggrepel to avoid overlaps # Add labels for each Subject
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear regression line
  annotate("text", x = Inf, y = Inf, label = paste("R^2 =", round(r2, 3)),
           vjust = 6, hjust = 2) + # Add R-squared value as text annotation with adjusted position
  labs(
    x = "Percent Change in mean_ratio (Week 1 to 10)",
    y = "Percent Change in VO2Max (PRE to POST)",
    title = "Comparison of Percent Changes"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) + # Remove gridlines
  theme(legend.position = "none")+
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1))) + # Expand x scale
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) # Expand y scale

# Print the plot
print(percent_change_plot)

# Save the mean_ratio_plot as an image
file_name_plot <- paste0(output_directory, "/", date, "_vo2max_postpreratio.png")
ggsave(file_name_plot, percent_change_plot, width = 11, height = 8.5, units = "in", dpi = 1200)
# r2 = 0.40567
#In the wrong direction...

#what about load? 
#What about RPE?
######################################


#################################################
mydata <- read_excel("MRP.xlsx", sheet = "Session HIFT", col_names = TRUE)
head(mydata)

# Filter rows where Subject is not equal to "SUB 4", "SUB 5", or "SUB 13"
mydata <- mydata %>%
  filter(Subject != "SUB 4" & Subject != "SUB 5" & Subject != "SUB 13" & Subject != "SUB 2")

head(mydata)
# Convert all week columns to numeric
mydata[, 3:ncol(mydata)] <- sapply(mydata[, 3:ncol(mydata)], as.numeric)

# Convert the data to a long format
mydata_long <- mydata %>%
  pivot_longer(
    cols = starts_with("Week"),
    names_to = "Week",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value)) %>%
  mutate(Week = as.factor(sub("Week ", "", Week)))

# Fit linear model to obtain R^2
lm_fit <- lm(Value ~ Week, data = mydata_long)
r2 <- summary(lm_fit)$r.squared

# Create the ggplot2 scatter plot
mydata_long$Week <- factor(mydata_long$Week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
scatter_plot <- ggplot(mydata_long, aes(x = as.numeric(Week), y = Value, color = Subject)) +  # Color by Subject
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = "blue", aes(group = 1)) +  # Add trendline here
  annotate("text", x = Inf, y = Inf, label = paste("R^2 = ", round(r2, 2)), 
           vjust = 2, hjust = 2, size = 5) +
  labs(title = "Scatterplot with Trendline and R^2", 
       x = "Week", 
       y = "Value") +
  theme_minimal()
print(scatter_plot)

file_name_plot <- paste0(output_directory, "/", date, "HIFT_Scatter.png")
ggsave(file_name_plot, scatter_plot, width = 11, height = 8.5, units = "in", dpi = 1200)

# rANOVA
model <- aov(Value ~ Week + Error(Subject/Week), data = mydata_long)
summary(model)
# > summary(model)
# 
# Error: Subject
# Df Sum Sq Mean Sq F value Pr(>F)
# Week       4 184593   46148   1.648  0.355
# Residuals  3  84022   28007               
# 
# Error: Subject:Week
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Week      10  35094    3509   4.191 0.000235 ***
#   Residuals 56  46898     837     


# Calculate the reference "Value" for week 1 for each subject
week1_reference <- mydata_long %>%
  filter(Week == 1) %>%
 dplyr::select(Subject, Week1_Value = Value)

# Join this reference back to the original data
mydata_long_with_reference <- left_join(mydata_long, week1_reference, by = "Subject")

# Calculate % change from week 1
mydata_long_with_reference <- mydata_long_with_reference %>%
  mutate(Percent_Change = ((Value - Week1_Value) / Week1_Value) * 100)

# View the data
print(mydata_long_with_reference)

# Create the summary table for mydata_long_with_reference
summary_table_with_pvalues_and_change <- mydata_long_with_reference %>%
  group_by(Week) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    stdev_value = sd(Value, na.rm = TRUE),
    mean_percent_change = mean(Percent_Change, na.rm = TRUE),
    stdev_percent_change = sd(Percent_Change, na.rm = TRUE),
    p_value_unpaired = t.test(Value, 
                           mydata_long_with_reference$Value[mydata_long_with_reference$Week == "1"], 
                           paired = FALSE, na.rm = TRUE)$p.value,  # Changed to unpaired
    p_value_paired = t.test(Value, Week1_Value, paired = TRUE, na.rm = TRUE)$p.value,
    p_value_percent_change = t.test(Percent_Change, mu = 0, na.rm = TRUE)$p.value
  )

# View the summary table
print(summary_table_with_pvalues_and_change)

file_name <- paste0(output_directory, "/", date, "HIFT_summary_pvalue.xlsx")
write_xlsx(summary_table_with_pvalues_and_change, file_name)

# Summary table with mean, standard deviation, and p-values
summary_table <- mydata_long %>%
  group_by(Week) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    stdev_value = sd(Value, na.rm = TRUE)
  )

summary_table <- summary_table %>%
  filter( (as.numeric(Week) <= 10)) %>%
  filter(!(Week %in% c("11", "12")))

# Convert the week column to factor with numeric levels in the desired order
summary_table$Week <- factor(summary_table$Week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# Plot
bar_plot <- ggplot(summary_table, aes(x = Week, y = mean_value)) +
  geom_errorbar(
    aes(ymin = mean_value - stdev_value, ymax = mean_value + stdev_value),
    width = 0.2
  ) +
  geom_bar(stat = "identity", fill = dark_blue_color) +
  geom_text(
    data = subset(summary_table, Week %in% c("3", "5", "6", "7", "9")),
    aes(y = mean_value + stdev_value + 2, label = "*"),
    position = position_dodge(width = 0.9),
    size = 10
  ) +
  labs(x = "Week", y = "Reps") +
  theme_bw()+
theme(
  panel.grid.major = element_blank(),  # Remove major grid lines
  panel.grid.minor = element_blank(),  # Remove minor grid lines
  panel.border = element_blank(),  # Remove panel border
  # axis.line = element_line(color = "black")
  )  # Add axes lines

print(bar_plot)

file_name_plot <- paste0(output_directory, "/", date, "HIFT_session.png")
ggsave(file_name_plot, bar_plot, width = 8, height = 5, units = "in", dpi = 1200)


mydata_long
# Define the subjects that are trained and untrained
trained_subjects <- c(8, 17, 18, 19)
untrained_subjects <- c(2, 3, 10, 11, 14)

trained_subjects <- paste0("SUB ", trained_subjects)
untrained_subjects <- paste0("SUB ", untrained_subjects)

mydata_long_T <- mydata_long %>%
  filter(Subject %in% trained_subjects) %>%
  filter( (as.numeric(Week) <= 10)) %>%
  filter(!(Week %in% c("11", "12")))

# Calculate the reference "Value" for week 1 for each subject
week1_reference <- mydata_long_T %>%
  filter(Week == 1) %>%
 dplyr::select(Subject, Week1_Value = Value)

# Join this reference back to the original data
mydata_long_with_reference <- left_join(mydata_long_T, week1_reference, by = "Subject")

# Calculate % change from week 1
mydata_long_with_reference <- mydata_long_with_reference %>%
  mutate(Percent_Change = ((Value - Week1_Value) / Week1_Value) * 100)

# View the data
print(mydata_long_with_reference)

# Create the summary table for mydata_long_with_reference
summary_table_with_pvalues_and_change <- mydata_long_with_reference %>%
  group_by(Week) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    stdev_value = sd(Value, na.rm = TRUE),
    mean_percent_change = mean(Percent_Change, na.rm = TRUE),
    stdev_percent_change = sd(Percent_Change, na.rm = TRUE),
    p_value_unpaired = t.test(Value, 
                              mydata_long_with_reference$Value[mydata_long_with_reference$Week == "1"], 
                              paired = FALSE, na.rm = TRUE)$p.value,  # Changed to unpaired
    p_value_paired = t.test(Value, Week1_Value, paired = TRUE, na.rm = TRUE)$p.value,
    p_value_percent_change = t.test(Percent_Change, mu = 0, na.rm = TRUE)$p.value
  )

# View the summary table
print(summary_table_with_pvalues_and_change)

file_name <- paste0(output_directory, "/", date, "trained_HIFT_summary_pvalue.xlsx")
write_xlsx(summary_table_with_pvalues_and_change, file_name)

# Summary table with mean, standard deviation, and p-values
summary_table <- mydata_long_T %>%
  group_by(Week) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    stdev_value = sd(Value, na.rm = TRUE)
  )

summary_table <- summary_table %>%
  filter( (as.numeric(Week) <= 10)) %>%
  filter(!(Week %in% c("11", "12")))

# Convert the week column to factor with numeric levels in the desired order
summary_table$Week <- factor(summary_table$Week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# Plot
bar_plot <- ggplot(summary_table, aes(x = Week, y = mean_value)) +
  geom_errorbar(
    aes(ymin = mean_value - stdev_value, ymax = mean_value + stdev_value),
    width = 0.2
  )+
  geom_bar(stat = "identity", fill = dark_blue_color) +
  
  labs(x = "Week", y = "Reps", title = "") +
  theme_bw(base_size = 18)+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
  )
print(bar_plot)

file_name_plot <- paste0(output_directory, "/", date, "trained_HIFT_session.png")
ggsave(file_name_plot, bar_plot, width = 11, height = 8.5, units = "in", dpi = 1200)


#Untrained
mydata_long_U <- mydata_long %>%
  filter(Subject %in% untrained_subjects) %>%
  filter( (as.numeric(Week) <= 10)) %>%
  filter(!(Week %in% c("8", "12")))

# Calculate the reference "Value" for week 1 for each subject
week1_reference <- mydata_long_U %>%
  filter(Week == 1) %>%
 dplyr::select(Subject, Week1_Value = Value)

# Join this reference back to the original data
mydata_long_with_reference <- left_join(mydata_long_U, week1_reference, by = "Subject")

# Calculate % change from week 1
mydata_long_with_reference <- mydata_long_with_reference %>%
  mutate(Percent_Change = ((Value - Week1_Value) / Week1_Value) * 100)

# View the data
print(mydata_long_with_reference)

# Create the summary table for mydata_long_with_reference
summary_table_with_pvalues_and_change <- mydata_long_with_reference %>%
  group_by(Week) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    stdev_value = sd(Value, na.rm = TRUE),
    mean_percent_change = mean(Percent_Change, na.rm = TRUE),
    stdev_percent_change = sd(Percent_Change, na.rm = TRUE),
    p_value_unpaired = t.test(Value, 
                              mydata_long_with_reference$Value[mydata_long_with_reference$Week == "1"], 
                              paired = FALSE, na.rm = TRUE)$p.value,  # Changed to unpaired
    p_value_paired = t.test(Value, Week1_Value, paired = TRUE, na.rm = TRUE)$p.value,
    p_value_percent_change = t.test(Percent_Change, mu = 0, na.rm = TRUE)$p.value
  )

# View the summary table
print(summary_table_with_pvalues_and_change)

file_name <- paste0(output_directory, "/", date, "untrained_HIFT_summary_pvalue.xlsx")
write_xlsx(summary_table_with_pvalues_and_change, file_name)

# Summary table with mean, standard deviation, and p-values
summary_table <- mydata_long_U %>%
  group_by(Week) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    stdev_value = sd(Value, na.rm = TRUE)
  )

summary_table <- summary_table %>%
  filter( (as.numeric(Week) <= 10)) %>%
  filter(!(Week %in% c("11", "12")))

# Convert the week column to factor with numeric levels in the desired order
summary_table$Week <- factor(summary_table$Week, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# Plot
bar_plot <- ggplot(summary_table, aes(x = Week, y = mean_value)) +
  geom_errorbar(
    aes(ymin = mean_value - stdev_value, ymax = mean_value + stdev_value),
    width = 0.2
  ) + 
  geom_bar(stat = "identity",  fill = dark_blue_color) +
  geom_text(
    data = subset(summary_table, Week %in% c("6")),
    aes(y = mean_value + stdev_value + 2, label = "*"),
    position = position_dodge(width = 0.9),
    size = 10
  ) +
  labs(x = "Week", y = "Reps", title = "") +
  theme_bw(base_size = 18) +
theme(
  panel.grid.major = element_blank(),  # Remove major grid lines
  panel.grid.minor = element_blank(),  # Remove minor grid lines
  panel.border = element_blank(),  # Remove panel border
)

print(bar_plot)

file_name_plot <- paste0(output_directory, "/", date, "untrained_HIFT_session.png")
ggsave(file_name_plot, bar_plot, width = 11, height = 8.5, units = "in", dpi = 1200)

