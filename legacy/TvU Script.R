# Function to install and load packages
install_and_load_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)}}}

required_packages <- c("readxl", "writexl", "dplyr", "tidyr" ,"openxlsx", "ggplot2", "openxlsx", "stringr", "car", "broom", "stringr", "cowplot", "ggpubr" ,"ggsignif")
# Call the function to install and load packages
install_and_load_packages(required_packages)

setwd("C:/MRP")
# Get the current date and format it as mmddyyyy
date <- format(Sys.Date(), "%m%d%Y")
# Define color values
dark_blue_color <- "#00205B"   # Dark blue color
light_blue_color <- "#6699CC"  # Light blue color
third_color <- "#A0C9E0"       # Third color

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

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "TvsU_Strength_raw_data.xlsx")
write_xlsx(mydata, file_name)

# T.test for subject characteristics between groups, not the best code but it works
data <- data.frame(
  Subject = c('SUB 3', 'SUB 10', 'SUB 14', 'SUB 2', 'SUB 11', 'SUB 8', 'SUB 17', 'SUB 18', 'SUB 19'),
  Group = c('untrained', 'untrained', 'untrained', 'untrained', 'untrained', 'trained', 'trained', 'trained', 'trained'),
  Age = c(37, 29.1, 33.8, 38, 20.7, 28.9, 22.8, 33.6, 31.2),
  Height = c(199, 173, 177, 165, 160, 166, 170, 183, 174),
  VO2Max = c(128.13, 99.66, 87.15, 102.27, 50.33, 67.57, 71.44, 72.56, 58.81),
  BodyFat = c(30.2, 27.9, 31.9, 43.8, 31.7, 15.2, 14.2, 4.3, 13.2),
  VO2Peak = c(30.6, 24.6, 22, 22.7, 21, 30.8, 49.8, 45, 39.5),
  Wmax = c(275, 200, 150, 175, 100, 200, 200, 250, 200)
  
)
#Where is weight?   Pull this from the data
# Separate the data into trained and untrained groups
trained <- data[data$Group == 'trained', ]
untrained <- data[data$Group == 'untrained', ]

#summary stats for data by group using dplyr 
data_summary <- data %>%
  group_by(Group) %>%
  summarize(
    Age_Mean = mean(Age, na.rm = TRUE),
    Age_SD = sd(Age, na.rm = TRUE),
    Height_Mean = mean(Height, na.rm = TRUE),
    Height_SD = sd(Height, na.rm = TRUE),
    VO2Max_Mean = mean(VO2Max, na.rm = TRUE),
    VO2Max_SD = sd(VO2Max, na.rm = TRUE),
    BodyFat_Mean = mean(BodyFat, na.rm = TRUE),
    BodyFat_SD = sd(BodyFat, na.rm = TRUE),
    VO2Peak_Mean = mean(VO2Peak, na.rm = TRUE),
    VO2Peak_SD = sd(VO2Peak, na.rm = TRUE),
    Wmax_Mean = mean(Wmax, na.rm = TRUE),
    Wmax_SD = sd(Wmax, na.rm = TRUE)
  )


# Perform t-tests for each variable
variables <- c('Age', 'Height', 'VO2Max', 'BodyFat', 'VO2Peak', 'Wmax')
t_test_results <- sapply(variables, function(var) {
  t.test(trained[[var]], untrained[[var]])$p.value
})
# View the results
t_test_results

# Define the replacements
replacements <- c(
  "Overhead Press" = "OHP",
  "Lat Pull down" = "Lat Pull",
  "Leg Press" = "Leg Press",
  "Bent Over Row" = "Bent Row",
  "Seated Row" = "Seated Row",
  "Lat Pulldown" = "Lat Pull"
)

# Apply the replacements to the Exercise column
mydata$Exercise <- dplyr::recode(mydata$Exercise, !!!replacements)
# Combine the Measure and Exercise columns into one
mydata$ex_measure <- paste(mydata$Exercise, mydata$Measure, sep = "_")
mydata$Measure <- mydata$ex_measure

# Define the subjects that are trained and untrained
trained_subjects <- c(8, 17, 18, 19)
untrained_subjects <- c(2, 3, 10, 11, 14)
trained_subjects <- paste0("SUB ", trained_subjects)
untrained_subjects <- paste0("SUB ", untrained_subjects)

# Add a new column "TrainingStatus" based on the subjects
mydata <- mydata %>%
  mutate(TrainingStatus = ifelse(Subject %in% trained_subjects, "trained", "untrained"))

# Remove BentRow entries from mydata
mydata <- mydata[mydata$Measure != "Bent Row_1RM" & mydata$Measure != "Bent Row_10RM", ]

# Combine the Measure and Exercise columns into a new column Measure
mydata_mutate <- mydata %>%
  filter(Exercise != "Bent Row") %>%
  unite("Measure", Measure, Exercise, sep = "_") 

mydata_reshaped <- mydata %>%
  pivot_longer(cols = c("PRE", "MID", "POST"), names_to = "Timepoint", values_to = "Weight")

# Group  data by 'Measure_Exercise' and perform the Shapiro-Wilk test for each group
normality_tests <- mydata_reshaped %>%
  group_by(ex_measure, TrainingStatus) %>%
  summarize(p_value = shapiro.test(Weight)$p.value)

filter(normality_tests, p_value < 0.05)

# Create a list of measures
measures <- unique(mydata_mutate$Measure)

# Open a file connection for writing
# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "TvsU_Strength_anova_results.txt")
fileConn <- file(file_name, "w")

# Loop through each measure and perform ANOVA
for (measure in measures) {
  # Subset the data for the current measure
  measure_data <- subset(mydata_mutate, Measure == measure)
  
  # Convert the data to long format
  long_data <- pivot_longer(measure_data, cols = c(PRE, MID, POST), names_to = "Timepoint", values_to = "Value")
  long_data$Timepoint <- factor(long_data$Timepoint, levels = c("PRE", "MID", "POST"))
  
  # Fit aov model with long format data
  aov_result <- aov(Value ~ Timepoint + Error(Subject/Timepoint), data = long_data)
  
  # Print ANOVA table with measure name
  cat("Measure:", measure, "\n")
  print(summary(aov_result))
  
  # Write ANOVA table with measure name to file
  cat("Measure:", measure, "\n", file = fileConn)
  writeLines(capture.output(print(summary(aov_result))), fileConn)
}
# Close the file connection
close(fileConn)





# Generate summary of mean and stdev grouped by measure. Additionally, output formatted mean±stdev for easy table
mydata_summary <- mydata %>%
  group_by(Measure, TrainingStatus) %>%
  summarize(
    PRE_Mean = mean(`PRE`, na.rm = TRUE),
    PRE_SD = sd(`PRE`, na.rm = TRUE),
    MID_Mean = mean(`MID`, na.rm = TRUE),
    MID_SD = sd(`MID`, na.rm = TRUE),
    POST_Mean = mean(`POST`, na.rm = TRUE),
    POST_SD = sd(`POST`, na.rm = TRUE),
    PRE_Mean_SD = sprintf("%.2f±%.2f", mean(`PRE`, na.rm = TRUE), sd(`PRE`, na.rm = TRUE)),
    MID_Mean_SD = sprintf("%.2f±%.2f", mean(`MID`, na.rm = TRUE), sd(`MID`, na.rm = TRUE)),
    POST_Mean_SD = sprintf("%.2f±%.2f", mean(`POST`, na.rm = TRUE), sd(`POST`, na.rm = TRUE))
  ) %>%
  mutate(
    cohens_d_pre_mid = (MID_Mean - PRE_Mean) / sqrt((PRE_SD^2 + MID_SD^2) / 2),
    cohens_d_pre_post = (POST_Mean - PRE_Mean) / sqrt((PRE_SD^2 + POST_SD^2) / 2),
    cohens_d_mid_post = (POST_Mean - MID_Mean) / sqrt((MID_SD^2 + POST_SD^2) / 2)
  ) %>%
  select(-PRE_Mean, -PRE_SD, -MID_Mean, -MID_SD, -POST_Mean, -POST_SD)


mydata_reshaped <- mydata %>%
  pivot_longer(cols = c("PRE", "MID", "POST"), names_to = "Timepoint", values_to = "Weight")

p_values <- mydata_reshaped %>%
  filter(Measure != "Bench Press_10RM" | TrainingStatus != "trained") %>%
  group_by(Measure, TrainingStatus) %>%
  summarize(
    p_val_pre_mid = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "MID"))$p.value,
    p_val_pre_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "POST"))$p.value,
    p_val_mid_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("MID", "POST"))$p.value
  )

summary_p_values <- left_join(mydata_summary, p_values, by = c("Measure", "TrainingStatus"))

# First, ensure that 'TrainingStatus' is a factor if it's not already
mydata_reshaped <- mydata_reshaped %>%
  mutate(TrainingStatus = as.factor(TrainingStatus))
# Now perform the comparison
comparison_results <- mydata_reshaped %>%
  group_by(Measure, Timepoint) %>%
  do(tidy(t.test(Weight ~ TrainingStatus, data = .))) %>%
  ungroup() %>%
  select(Measure, Timepoint, p.value)
# View the results
print(comparison_results)
filter(comparison_results, p.value < 0.1)
#No sig differences between groups at any timepoint or measure

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "_TvsU_summary_pval.xlsx")
write_xlsx(summary_p_values, file_name)

filtered_data <- mydata_reshaped %>%
  filter(Measure != "Bench Press_10RM" | TrainingStatus != "trained")

print(filtered_data)

filtered_out_data <- anti_join(mydata_reshaped, filtered_data)

print(filtered_out_data)

filtered_out_data %>%
  group_by(Timepoint) %>%
  summarize(PRE_Mean = mean(Weight, na.rm = TRUE),
            PRE_SD = sd(Weight, na.rm = TRUE),
            MID_Mean = mean(Weight, na.rm = TRUE),
            MID_SD = sd(Weight, na.rm = TRUE),
            POST_Mean = mean(Weight, na.rm = TRUE),
            POST_SD = sd(Weight, na.rm = TRUE),
            Mean_SD = sprintf("%.2f±%.2f", mean(Weight, na.rm = TRUE), sd(Weight, na.rm = TRUE)),)
###################Data shown below illustrates lack of variation

# # A tibble: 3 × 8
# Timepoint PRE_Mean PRE_SD MID_Mean MID_SD POST_Mean POST_SD Mean_SD    
# <chr>        <dbl>  <dbl>    <dbl>  <dbl>     <dbl>   <dbl> <chr>      
#   1 MID           53.3   16.7     53.3   16.7      53.3    16.7 53.30±16.72
# 2 POST          57.8   16.7     57.8   16.7      57.8    16.7 57.83±16.72
# 3 PRE           46.5   15.0     46.5   15.0      46.5    15.0 46.49±14.99

# filtered_out_data %>%
#   summarize(
#     p_val_pre_mid = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "MID"))$p.value,
#     p_val_pre_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "POST"))$p.value,
#     p_val_mid_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("MID", "POST"))$p.value
#   )
# Error in `summarize()`:
#   ℹ In argument: `p_val_mid_post = `$`(...)`.
# Caused by error in `t.test.default()`:
#  ########## ! data are essentially constant  ###################

plot_data <- filtered_out_data %>%
  group_by(Timepoint) %>%
  summarize(PRE_Mean = mean(Weight, na.rm = TRUE),
            PRE_SD = sd(Weight, na.rm = TRUE),
            MID_Mean = mean(Weight, na.rm = TRUE),
            MID_SD = sd(Weight, na.rm = TRUE),
            POST_Mean = mean(Weight, na.rm = TRUE),
            POST_SD = sd(Weight, na.rm = TRUE))
# Define the order of the x-axis
timepoint_order <- c("PRE", "MID", "POST")
# Plotting
benchpress10rm_plot <- ggplot(plot_data, aes(x = factor(Timepoint, levels = timepoint_order), y = PRE_Mean)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue", color = "black") +
  geom_errorbar(aes(ymin = PRE_Mean - PRE_SD, ymax = PRE_Mean + PRE_SD), width = 0.2, color = "black") +
  labs(title = "BenchPress 10RM in Trained",
       x = "Timepoint",
       y = "Weight") +
  theme_minimal()

file_name_plot <- paste0(output_directory, "/", date, "benchpress10rm_plot.png")
ggsave(file_name_plot, plot = benchpress10rm_plot, width = 6, height = 4, dpi = 1200)


################################################
# Group the data by Measure and Exercise, and summarize the percent change
# Define a function to calculate percent change
calculate_percent_change <- function(pre, mid, post) {
  data.frame(
    PRE_to_MID = ((mid - pre) / pre) * 100,
    MID_to_POST = ((post - mid) / mid) * 100,
    PRE_to_POST = ((post - pre) / pre) * 100
  )
}

# First Stage: Calculating mean and percent change
percent_change_summary <- mydata %>%
  group_by(Measure, Exercise, TrainingStatus) %>%
  summarise(
    PRE_Mean = mean(PRE, na.rm = TRUE),
    MID_Mean = mean(MID, na.rm = TRUE),
    POST_Mean = mean(POST, na.rm = TRUE),
    percent_change = list(calculate_percent_change(PRE_Mean, MID_Mean, POST_Mean)),
    .groups = "drop"
  ) %>%
  unnest(percent_change)

# First Stage: Calculating mean and percent change per subject
percent_change_per_subject <- mydata %>%
  group_by(Subject, Measure, Exercise, TrainingStatus) %>%  # Include Subject here
  summarise(
    PRE = mean(PRE, na.rm = TRUE),
    MID = mean(MID, na.rm = TRUE),
    POST = mean(POST, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(percent_change = list(calculate_percent_change(PRE, MID, POST))) %>%
  unnest(percent_change)

# Second Stage: Calculating p-values and Mean±SD
percent_change_summary_with_pvalues <- percent_change_per_subject %>% 
  filter(Exercise != "BentRow") %>% 
  group_by(Measure, Exercise, TrainingStatus) %>%
  summarise(
    p_val_pre_mid = t.test(PRE_to_MID, MID_to_POST, paired = TRUE, na.rm = TRUE)$p.value,
    p_val_pre_post = t.test(PRE_to_MID, PRE_to_POST, paired = TRUE, na.rm = TRUE)$p.value,
    p_val_mid_post = t.test(MID_to_POST, PRE_to_POST, paired = TRUE, na.rm = TRUE)$p.value,
    PRE_Mean_SD = sprintf("%.2f±%.2f", mean(PRE_to_MID, na.rm = TRUE), sd(PRE_to_MID, na.rm = TRUE)),
    MID_Mean_SD = sprintf("%.2f±%.2f", mean(MID_to_POST, na.rm = TRUE), sd(MID_to_POST, na.rm = TRUE)),
    POST_Mean_SD = sprintf("%.2f±%.2f", mean(PRE_to_POST, na.rm = TRUE), sd(PRE_to_POST, na.rm = TRUE)),
    .groups = "drop"
  )

# Merge the tables using left_join
merged_summary <- left_join(percent_change_summary, percent_change_summary_with_pvalues, 
                            by = c("Measure", "Exercise", "TrainingStatus"))
# Print the merged summary
print(merged_summary)

pivot_summary <- percent_change_per_subject %>%
  pivot_longer(cols = c(PRE_to_MID, MID_to_POST, PRE_to_POST),
               names_to = "Timepoint", values_to = "Value")

# Now perform the comparison
comparison_results <- pivot_summary %>%
  group_by(Measure, Timepoint) %>%
  do(tidy(t.test(Value ~ TrainingStatus, data = .))) %>%
  ungroup() %>%
  select(Measure, Timepoint, p.value)
# View the results
print(comparison_results)
filter(comparison_results, p.value < 0.1)
#> filter(comparison_results, p.value < 0.1)
# # A tibble: 2 × 3
# Measure         Timepoint   p.value
# <chr>           <chr>         <dbl>
#   1 Leg Press_1RM   PRE_to_MID   0.0223
# 2 Seated Row_10RM MID_to_POST  0.0769

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "TvsU_percent_change_summary.xlsx")
write_xlsx(merged_summary, file_name)

# Reorder the factor levels for the 'TrainingStatus' column
percent_change_summary$TrainingStatus <- factor(percent_change_summary$TrainingStatus, levels = c("untrained", "trained"))

# Calculate mean and standard deviation for the specified columns
summary_data <- percent_change_per_subject %>%
  group_by(Measure, TrainingStatus) %>%
  summarize(
    mean_PRE_to_MID = mean(PRE_to_MID),
    sd_PRE_to_MID = sd(PRE_to_MID),
    mean_MID_to_POST = mean(MID_to_POST),
    sd_MID_to_POST = sd(MID_to_POST),
    mean_PRE_to_POST = mean(PRE_to_POST),
    sd_PRE_to_POST = sd(PRE_to_POST)
  )

# Subset rows with "1RM" in the Measure column and retain TrainingStatus
measures_1RM <- summary_data %>%
  filter(str_detect(Measure, "1RM")) %>%
  mutate(Measure = str_replace(Measure, "_1RM", "")) # Remove "_1RM" suffix

# Print the updated data frame
print(measures_1RM)


# Create the plot for 1RM measures in descending order of magnitude
plot_1RM <- 
  ggplot(measures_1RM, aes(x = reorder(Measure, -mean_PRE_to_POST), y = mean_PRE_to_POST, fill = TrainingStatus)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  geom_errorbar(aes(ymin = mean_PRE_to_POST - sd_PRE_to_POST, ymax = mean_PRE_to_POST + sd_PRE_to_POST), position = "dodge", width = 0.9) +
  labs(x = "Exercise", y = "1RM %Δ",
       fill = "Training Status") +
  scale_fill_grey() +  # Sets the fill color to grayscale (black and white)
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "1RM_plot_TvsU.png")
ggsave(file_name_plot, plot = plot_1RM, width = 6, height = 4, dpi = 1200)

# Calculate mean and standard deviation for the specified columns
summary_data <- percent_change_per_subject %>%
  group_by(Measure, TrainingStatus) %>%
  summarize(
    mean_PRE_to_MID = mean(PRE_to_MID),
    sd_PRE_to_MID = sd(PRE_to_MID),
    mean_MID_to_POST = mean(MID_to_POST),
    sd_MID_to_POST = sd(MID_to_POST),
    mean_PRE_to_POST = mean(PRE_to_POST),
    sd_PRE_to_POST = sd(PRE_to_POST)
  )

# Subset rows with "1RM" in the Measure column and retain TrainingStatus
measures_1RM <- summary_data %>%
  filter(str_detect(Measure, "1RM")) %>%
  mutate(Measure = str_replace(Measure, "_1RM", "")) # Remove "_1RM" suffix
# Reorder the factor levels for the 'TrainingStatus' column
measures_1RM$TrainingStatus <- factor(measures_1RM$TrainingStatus, levels = c("untrained", "trained"))

# Create a new column for asterisks
measures_1RM$Asterisk <- ifelse((measures_1RM$Measure == "OHP" & measures_1RM$TrainingStatus == "trained") |
                                  (measures_1RM$Measure == "Seated Row" & measures_1RM$TrainingStatus == "untrained"), "", "*")

# Define color values
dark_blue_color <- "#00205B"  # Dark blue color
light_blue_color <- "#6699CC"  # Light blue color

# Reorder factor levels for Exercise variable
exercise_order <- c("Leg Press", "Deadlift", "Seated Row", "Lat Pull", "Bench Press", "OHP")
measures_1RM$Measure <- factor(measures_1RM$Measure, levels = exercise_order)


# Plot with markers for significant differences
plot_1RM_with_asterisks <- ggplot(measures_1RM, aes(x = Measure, y = mean_PRE_to_POST, fill = TrainingStatus)) +
  geom_errorbar(aes(ymin = mean_PRE_to_POST - sd_PRE_to_POST, ymax = mean_PRE_to_POST + sd_PRE_to_POST),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(y = mean_PRE_to_POST + sd_PRE_to_POST + 2, label = Asterisk), size = 10, color = "black",
            position = position_dodge(width = 0.9)) +
  labs(x = "", y = "1RM %Δ", fill = "Training Status") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color)) +  # Apply custom fill colors
  scale_y_continuous(limits = c(0, 130), 
                     breaks = seq(0, 120, 20), 
                     minor_breaks = seq(0, 120, 10), 
                     expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.8),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        text = element_text(size = 12),          # Set font size for general text
        axis.title = element_text(size = 12, color = "black"),    # Set font size for axis titles
        axis.text = element_text(size = 11, color = "black"),     # Set font size for axis text
        legend.title = element_text(size = 12),  # Set font size for legend title
        legend.text = element_text(size = 12))   # Set font size for legend text))

# Display the updated plot
print(plot_1RM_with_asterisks)
# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_SIG_1RM_plot_TvsU.png")
ggsave(file_name_plot, plot = plot_1RM_with_asterisks, width = 6, height = 4, dpi = 1200)


# Subset rows with "10RM" in the Measure column and retain TrainingStatus
measures_10RM <- percent_change_summary %>%
  filter(str_detect(Measure, "10RM")) %>%
  mutate(Measure = str_replace(Measure, "_10RM", "")) # Remove "_10RM" suffix

# Print the updated data frame
print(measures_10RM)
# Create the plot for 10RM measures in descending order of magnitude
plot_10RM <- 
  ggplot(measures_10RM, aes(x = reorder(Measure, -PRE_to_POST), y = PRE_to_POST, fill = TrainingStatus)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  labs(x = "Exercise", y = "10RM %Δ",
       fill = "Training Status") +
  scale_fill_grey() +  # Sets the fill color to grayscale (black and white)
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "10RM_plot_TvsU.png")
ggsave(file_name_plot, plot = plot_10RM, width = 6, height = 4, dpi = 1200)

# Subset rows with "10RM" in the Measure column and retain TrainingStatus
measures_10RM <- summary_data %>%
  filter(str_detect(Measure, "10RM")) %>%
  mutate(Measure = str_replace(Measure, "_10RM", "")) # Remove "_10RM" suffix

# Reorder the factor levels for the 'TrainingStatus' column
measures_10RM$TrainingStatus <- factor(measures_1RM$TrainingStatus, levels = c("untrained", "trained"))

# Create a new column for asterisks
measures_10RM$Asterisk <- ifelse((measures_1RM$Measure == "Bench Press" & measures_1RM$TrainingStatus == "trained") |
                                  (measures_1RM$Measure == "" & measures_1RM$TrainingStatus == "untrained"), "", "*")
# Reorder factor levels for Exercise variable
exercise_order <- c("Leg Press", "Deadlift", "Seated Row", "Lat Pull", "Bench Press",  "OHP")
measures_10RM$Measure <- factor(measures_10RM$Measure, levels = exercise_order)

# Define color values
dark_blue_color <- "#00205B"  # Dark blue color
light_blue_color <- "#6699CC"  # Light blue color

# Plot with markers for significant differences
plot_10RM_with_asterisks <- ggplot(measures_10RM, aes(x = Measure, y = mean_PRE_to_POST, fill = TrainingStatus)) +
  geom_errorbar(aes(ymin = mean_PRE_to_POST - sd_PRE_to_POST, ymax = mean_PRE_to_POST + sd_PRE_to_POST),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(y = mean_PRE_to_POST + sd_PRE_to_POST + 2, label = Asterisk), size = 10, color = "black",
            position = position_dodge(width = 0.9)) +
  labs(x = "", y = "10RM %Δ", fill = "Training Status") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color)) +  # Apply custom fill colors
  theme_minimal() +
  scale_y_continuous(limits = c(0, 130), 
                     breaks = seq(0, 120, 20), 
                     minor_breaks = seq(0, 120, 10), 
                     expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.8),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        text = element_text(size = 12),          # Set font size for general text
        axis.title = element_text(size = 12, color = "black"),    # Set font size for axis titles
        axis.text = element_text(size = 11, color = "black"),     # Set font size for axis text
        legend.title = element_text(size = 12),  # Set font size for legend title
        legend.text = element_text(size = 12))   # Set font size for legend text))

# Display the updated plot
print(plot_10RM_with_asterisks)

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_SIG_10RM_plot_TvsU.png")
ggsave(file_name_plot, plot = plot_10RM_with_asterisks, width = 6, height = 4, dpi = 1200)


plot_1RM
plot_10RM

# Combine the plots using plot_grid and include labels
combined_plot <- plot_grid(plot_1RM, plot_10RM, nrow = 1)

# Display the combined plot
print(combined_plot)
file_name_plot <- paste0(output_directory, "/", date, "strengthchange_TvsU_plot.png")
ggsave(file_name_plot, plot = combined_plot, width = 6, height = 4, dpi = 1200)

# Combine the plots using plot_grid and include labels
combined_plot <- plot_grid(plot_1RM_with_asterisks, plot_10RM_with_asterisks, nrow = 1)

# Display the combined plot
print(combined_plot)
file_name_plot <- paste0(output_directory, "/", date, "_SIG_strengthchange_TvsU_plot.png")
ggsave(file_name_plot, plot = combined_plot, width = 6, height = 4, dpi = 1200)


######NEEDS object from script rm1splot rm10splot

# Arrange and display plots using ggarrange
arranged_plots <- ggarrange(rm1splot, rm10splot, plot_1RM_with_asterisks, plot_10RM_with_asterisks,
                            ncol = 2, nrow = 2, common.legend = FALSE)
#
# Display the arranged plots
print(arranged_plots)
file_name_plot <- paste0(output_directory, "/", date, "_FOUR_PANEL.png")
ggsave(file_name_plot, plot = arranged_plots, width = 11, height =8.5, dpi = 1200)

# Arrange and display plots using ggarrange
arranged_plots <- ggarrange(rm1splot, rm10splot, plot_1RM_with_asterisks, plot_10RM_with_asterisks,
                            ncol = 2, nrow = 2, common.legend = FALSE)

# Create a wrapped text grob for the caption
caption_text <- "Figure 1.a. A minimal dose training program improves maximal (1RM) and submaximal (10RM) strength. b. Trained individuals exhibited a greater relative increase in strength. *Indicates significance (vs. PRE, p<0.05)"
# Wrap the caption text
wrapped_caption <- strwrap(caption_text, width = 140)  # Adjust 'width' to control the wrapping
wrapped_caption <- paste(wrapped_caption, collapse = "\n")



# Add labels and a wrapped caption using annotate_figure()
arranged_plots_with_labels <- annotate_figure(arranged_plots,
                                              top = text_grob("a", color = "black", size = 15, x = 0),
                                              left = text_grob("b", color = "black", size = 15, hjust = 0, vjust = 0.5),
                                              bottom = wrapped_caption) # Wrapped caption at the bottom

# Display the arranged plots with labels and caption
arranged_plots_with_labels


file_name_plot <- paste0(output_directory, "/", date, "_annotated__FOUR_PANEL.gif")
ggsave(file_name_plot, plot = arranged_plots_with_labels, width = 11, height = 8.5, dpi = 1200, bg = "white")

####################################################################################################

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
  "Lat Pull down" = "Lat Pull",
  "Leg Press" = "Leg Press",
  "Bent Over Row" = "Bent Row",
  "Seated Row" = "Seated Row",
  "Lat Pulldown" = "Lat Pull"
)

# Apply the replacements to the Exercise column
mydata$Exercise <- recode(mydata$Exercise, !!!replacements)

# Define the subjects that are trained and untrained
trained_subjects <- c(8, 17, 18, 19)
untrained_subjects <- c(2, 3, 10, 11, 14)

trained_subjects <- paste0("SUB ", trained_subjects)
untrained_subjects <- paste0("SUB ", untrained_subjects)

# Add a new column "TrainingStatus" based on the subjects
mydata <- mydata %>%
  mutate(TrainingStatus = ifelse(Subject %in% trained_subjects, "trained", "untrained"))

# Remove BentRow entries from mydata
mydata <- mydata[mydata$Measure != "BentRow_1RM" & mydata$Measure != "BentRow_10RM", ]

head(mydata)
str(mydata)
unique(mydata$Exercise)
mydata <- mydata %>%
  filter(Exercise != "Bent Row")

#Filter 10rm and 1rm to new tables
# Function to summarize and reshape the data
summarize_and_reshape <- function(data) {
  summary <- data %>%
    group_by(Exercise) %>%
    summarise(across(
      c(PRE, MID, POST),
      list(Mean = mean, SD = sd),
      .names = "{.col}_{.fn}")
    )
  
  summary_long <- summary %>%
    pivot_longer(
      cols = c(PRE_Mean:POST_SD),
      names_to = c("Time", ".value"),
      names_pattern = "(PRE|MID|POST)_(.*)"
    ) %>%
    mutate(Exercise = reorder(Exercise, -Mean),
           Time = factor(Time, levels = c("PRE", "MID", "POST")))
  return(summary_long)
}

# Filter 10rm and 1rm to new tables and apply the function
mydata_1rm_T <- mydata %>% filter(Measure == "1RM" & TrainingStatus == "trained") %>% summarize_and_reshape()
mydata_1rm_U <- mydata %>% filter(Measure == "1RM" & TrainingStatus == "untrained") %>% summarize_and_reshape()
mydata_10rm_T <- mydata %>% filter(Measure == "10RM" & TrainingStatus == "trained") %>% summarize_and_reshape()
mydata_10rm_U <- mydata %>% filter(Measure == "10RM" & TrainingStatus == "untrained") %>% summarize_and_reshape()

# Plot the data using ggplot2
ggplot(mydata_1rm_T, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "Exercise", y = "10RM", fill = "Time") +
  scale_fill_grey(start = 0.2, end = 0.8, name = "Time") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# create a vector indicating which bars are significantly different
sig_diff <- c(F,F,T,F,T,T,F,T,T,F,T,T,F,F,F,F,T,T)
#(BP, DL, LAT, LP ,OHP,SR)
# plot with markers for significant differences
rm1_T_plot <- 
  ggplot(mydata_1rm_T, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(y = Mean + SD + 2, label =ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "Trained", y = "1RM (kg)", fill = "Time") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  
  scale_y_continuous(limits = c(0, 165), 
                     breaks = seq(0, 160, 20), 
                     minor_breaks = seq(0, 160, 10), 
                     expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.8),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "1rm_T_plot_sig.png")
ggsave(file_name_plot, plot = rm1_T_plot, width = 6, height = 4, dpi = 1200)

sig_diff <- c(F,T,T,F,T,T,F,T,T,F,F,T,F,F,T,F,F,F)
#(BP, DL, LAT, LP ,OHP,SR)
rm1_U_plot<-
ggplot(mydata_1rm_U, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(y = Mean + SD + 2, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "Untrained", y = "1RM (kg)", fill = "Time") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  
  scale_y_continuous(limits = c(0, 165), 
                     breaks = seq(0, 160, 20), 
                     minor_breaks = seq(0, 160, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.8),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

file_name_plot <- paste0(output_directory, "/", date, "1rm_U_plot_sig.png")
ggsave(file_name_plot, plot = rm1_U_plot, width = 6, height = 4, dpi = 1200)

# # Combine the plots using plot_grid and include labels
# # combined_plot <- plot_grid(rm1_U_plot, rm1_T_plot, nrow = 1)
# 
# # Display the combined plot
# print(combined_plot)
# file_name_plot <- paste0(output_directory, "/", date, "1rm_TvsU_plot_sig.png")
# ggsave(file_name_plot, plot = combined_plot, width = 6, height = 4, dpi = 1200)

# Reorder factor levels for Exercise variable
exercise_order <- c("Leg Press", "Deadlift", "Seated Row", "Lat Pull", "Bench Press",  "OHP")
mydata_10rm_T$Exercise <- factor(mydata_10rm_T$Exercise, levels = exercise_order)

# create a vector indicating which bars are significantly different
sig_diff <- c(F,F,F,F,T,T,F,F,T,F,T,T,F,F,T,F,T,T)
#(BP, DL, LAT, LP ,OHP,SR)
# plot with markers for significant differences
rm10_T_plot <- 
  ggplot(mydata_10rm_T, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(y = Mean + SD + 2, label =ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "Trained", y = "10RM (kg)", fill = "Time") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  scale_y_continuous(limits = c(0, 165), 
                     breaks = seq(0, 160, 20), 
                     minor_breaks = seq(0, 160, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.8),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "10rm_T_plot_sig.png")
ggsave(file_name_plot, plot = rm10_T_plot, width = 6, height = 4, dpi = 1200)

sig_diff <- c(F,T,T,F,T,T,F,T,T,F,T,T,F,F,T,F,T,T)
#(BP, DL, LAT, LP ,OHP,SR)
rm10_U_plot<-
  ggplot(mydata_10rm_U, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(y = Mean + SD + 2, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "Untrained", y = "10RM (kg)", fill = "Time") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  scale_y_continuous(limits = c(0, 165), 
                     breaks = seq(0, 160, 20), 
                     minor_breaks = seq(0, 160, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.8),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

file_name_plot <- paste0(output_directory, "/", date, "10rm_U_plot_sig.png")
ggsave(file_name_plot, plot = rm10_U_plot, width = 6, height = 4, dpi = 1200)

# Combine the plots using plot_grid and include labels
combined_plot <- plot_grid(rm10_U_plot, rm10_T_plot, nrow = 1)
com

# Display the combined plot
print(combined_plot)
file_name_plot <- paste0(output_directory, "/", date, "10rm_TvsU_plot_sig.png")
ggsave(file_name_plot, plot = combined_plot, width = 6, height = 4, dpi = 1200)

############################################################################################################
#VO2 and other variables 

# read in the data from the Excel file
mydata <- read_excel("MRP.xlsx", sheet = "vo2data", col_names = TRUE)
str(mydata)
summary(mydata)
head(mydata)
unique(mydata$Measure)
unique(mydata$Subject)

# Define the subjects that are trained and untrained
trained_subjects <- c(8, 17, 18, 19)
untrained_subjects <- c(2, 3, 10, 11, 14)

trained_subjects <- paste0("SUB ", trained_subjects)
untrained_subjects <- paste0("SUB ", untrained_subjects)

mydata <- mydata[mydata$Subject %in% trained_subjects, ]

unique(mydata$Subject)

# Create a list of measures
measures <- c("VO2Max", "VO2Peak", "Body fat (%)", "Weight (Kg)", "Vertical Jump","6.5km*hr-1", "16km*hr-1",
              "12km*hr-1", "Wmax")
# Create the file name by appending the date to "anova_results.txt"
file_name <- paste0(output_directory, "/", date, "trained_anova_results.txt", sep = "_")

# Open the file connection
fileConn <- file(file_name, "w")
# Loop through each measure and perform ANOVA
for (measure in measures) {
  # Subset the data for the current measure
  measure_data <- subset(mydata, Measure == measure)
  
  # Convert the data to long format
  long_data <- pivot_longer(measure_data, cols = c(PRE, MID, POST), names_to = "Timepoint", values_to = "Value")
  
  # Fit aov model with long format data
  aov_result <- aov(Value ~ Timepoint + Error(Subject), data = long_data)
  
  # Print ANOVA table with measure name
  cat("Measure:", measure, "\n")
  print(summary(aov_result))
  
  # Write ANOVA table with measure name to file
  cat("Measure:", measure, "\n", file = fileConn)
  writeLines(capture.output(print(summary(aov_result))), fileConn)
}

# Close the file connection
close(fileConn)

# Generate summary of mean and stdev grouped by measure. Additionally, output formatted mean±stdev for easy table
mydata_summary <- mydata %>%
  group_by(Measure) %>%
  summarize(
    PRE_Mean = mean(`PRE`, na.rm = TRUE),
    PRE_SD = sd(`PRE`, na.rm = TRUE),
    MID_Mean = mean(`MID`, na.rm = TRUE),
    MID_SD = sd(`MID`, na.rm = TRUE),
    POST_Mean = mean(`POST`, na.rm = TRUE),
    POST_SD = sd(`POST`, na.rm = TRUE),
    PRE_Mean_SD = sprintf("%.2f±%.2f", mean(`PRE`, na.rm = TRUE), sd(`PRE`, na.rm = TRUE)),
    MID_Mean_SD = sprintf("%.2f±%.2f", mean(`MID`, na.rm = TRUE), sd(`MID`, na.rm = TRUE)),
    POST_Mean_SD = sprintf("%.2f±%.2f", mean(`POST`, na.rm = TRUE), sd(`POST`, na.rm = TRUE))
  ) %>%
  mutate(
    cohens_d_pre_mid = (MID_Mean - PRE_Mean) / sqrt((PRE_SD^2 + MID_SD^2) / 2),
    cohens_d_pre_post = (POST_Mean - PRE_Mean) / sqrt((PRE_SD^2 + POST_SD^2) / 2),
    cohens_d_mid_post = (POST_Mean - MID_Mean) / sqrt((MID_SD^2 + POST_SD^2) / 2)
  )

mydata_reshaped <- mydata %>%
  pivot_longer(cols = c("PRE", "MID", "POST"), names_to = "Timepoint", values_to = "Weight")

p_values <- mydata_reshaped %>%
  filter(Measure != "Bench Press_10RM") %>%
  group_by(Measure) %>%
  summarize(
    p_val_pre_mid = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "MID"))$p.value,
    p_val_pre_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "POST"))$p.value,
    p_val_mid_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("MID", "POST"))$p.value
  )

summary_p_values <- left_join(mydata_summary, p_values, by = c("Measure"))


# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "VO2_trained_summary.csv")
write.csv(summary_p_values, file = file_name, row.names = FALSE)

# Pivot the data into long format
long_data <- mydata %>% pivot_longer(cols = c(PRE, MID, POST), names_to = "Timepoint", values_to = "Value")

head(long_data)
# Group the data by measure and timepoint and calculate the mean and sd
summary_data <- long_data %>%
  group_by(Measure, Timepoint) %>%
  summarize(mean = mean(Value), sd = sd(Value), mean_sd = paste(round(mean, 2), "±", round(sd, 2)))

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "trained_summary.txt")

# Write the summary data to a text file with the current date in the file name
write.table(summary_data, file = file_name, sep = "\t", row.names = FALSE)
print(summary_data)

unique(long_data$Measure)

p_values <- long_data %>%
  group_by(Measure) %>%
  summarize(
    p_val_pre_mid = t.test(Value ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "MID"))$p.value,
    p_val_pre_post = t.test(Value ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "POST"))$p.value,
    p_val_mid_post = t.test(Value ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("MID", "POST"))$p.value
  )

file_name <- paste0(output_directory, "/", date, "VO2_T_p_values.csv")
write.csv(p_values, file_name, row.names = FALSE)

# Subset the data to only include VO2Max and VO2Peak
vo2_data <- subset(long_data, Measure %in% c("VO2Max", "VO2Peak"))

###vo2_data <- long_data 

# Change the levels of the Timepoint factor to set the desired order
vo2_data$Timepoint <- factor(vo2_data$Timepoint, levels = c("PRE", "MID", "POST"))

# Plot the data using ggplot2
ggplot(vo2_data, aes(x = Timepoint, y = Value, group = Subject, color = Measure)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Timepoint", y = "Value", color = "Measure") +
  theme_classic()
######

# Calculate the mean value for each timepoint and measure combination
vo2_means <- aggregate(Value ~ Measure + Timepoint, vo2_data, mean)

# Set the y-axis label to the desired format
y_axis_label <- expression(paste("VO"[2]," (", "ml·"~kg^-1~min^-1, ")"))


ggplot(vo2_means, aes(x = Timepoint, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Timepoint", y = y_axis_label, fill = "Measure") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_classic()

# Calculate the mean and standard deviation for each timepoint and measure combination
vo2_summary <- vo2_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Set the y-axis label to the desired format
y_axis_label <- expression(paste("VO"[2]," (", "ml·"~kg^-1~min^-1, ")"))

# Create the bar chart with error bars
ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "Timepoint", y = y_axis_label, fill = "Measure") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_classic()

# Create the plot
vo2_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "", y = y_axis_label, fill = "") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_classic() +
  theme(legend.position = c(0.1, 0.85),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# create a vector indicating which bars are significantly different
sig_diff <- c(F, F, F, F, F, F)

# Create the plot sig diff
vo2_T_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = mean + sd + 1, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "", y = y_axis_label, fill = "") +
  scale_fill_grey() +  # sets the fill color to grayscale
  scale_y_continuous(limits = c(0, 60), 
                     breaks = seq(0, 50, 10), 
                     minor_breaks = seq(0, 50, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = c(0.1, 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

vo2_T_summary<- vo2_summary
# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "vo2_T_plot.png")
ggsave(file_name_plot, plot = vo2_T_plot, width = 6, height = 4, dpi = 1200)

# Subset the data to only include running economy
WM_data <- subset(long_data, Measure %in% c("Wmax"))

# Change the levels of the Timepoint factor to set the desired order
WM_data$Timepoint <- factor(WM_data$Timepoint, levels = c("PRE", "MID", "POST"))

# Calculate the mean and standard deviation for each timepoint and measure combination
WM_summary <- WM_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# create a vector indicating which bars are significantly different
sig_diff <- c(F, F, T)
# Create the plot sig diff
WM_plot <- 
  ggplot(WM_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = mean + sd + 1, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "Trained", y = "Watts", fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  theme_bw() +
  theme(legend.position = c(0.1, 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_TRAINED_WM_plot.png")
ggsave(file_name_plot, plot = WM_plot, width = 6, height = 4, dpi = 1200)

###############################################################
mydata <- read_excel("MRP.xlsx", sheet = "vo2data", col_names = TRUE)
str(mydata)
summary(mydata)
head(mydata)
unique(mydata$Measure)
unique(mydata$Subject)

# Define the subjects that are trained and untrained
trained_subjects <- c(8, 17, 18, 19)
untrained_subjects <- c(2, 3, 10, 11, 14)

trained_subjects <- paste0("SUB ", trained_subjects)
untrained_subjects <- paste0("SUB ", untrained_subjects)

mydata <- mydata[mydata$Subject %in% untrained_subjects, ]
unique(mydata$Subject)
unique(mydata$Measure)


##"16km*hr-1", did not have enough values for comparison
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#   contrasts can be applied only to factors with 2 or more levels
# Create a list of measures
measures <- c("VO2Max", "VO2Peak", "Body fat (%)", 
              "Weight (Kg)", "Vertical Jump","6.5km*hr-1", 
              #"16km*hr-1",
              "12km*hr-1", "Wmax")
# Create the file name by appending the date to "anova_results.txt"
file_name <- paste0(output_directory, "/", date, "untrained_anova_results.txt", sep = "_")

# Open the file connection
fileConn <- file(file_name, "w")
# Loop through each measure and perform ANOVA
for (measure in measures) {
  # Subset the data for the current measure
  measure_data <- subset(mydata, Measure == measure)
  
  # Convert the data to long format
  long_data <- pivot_longer(measure_data, cols = c(PRE, MID, POST), names_to = "Timepoint", values_to = "Value")
  
  # Fit aov model with long format data
  aov_result <- aov(Value ~ Timepoint + Error(Subject), data = long_data)
  
  # Print ANOVA table with measure name
  cat("Measure:", measure, "\n")
  print(summary(aov_result))
  
  # Write ANOVA table with measure name to file
  cat("Measure:", measure, "\n", file = fileConn)
  writeLines(capture.output(print(summary(aov_result))), fileConn)
}

# Close the file connection
close(fileConn)

# Generate summary of mean and stdev grouped by measure. Additionally, output formatted mean±stdev for easy table
mydata_summary <- mydata %>%
  group_by(Measure) %>%
  summarize(
    PRE_Mean = mean(`PRE`, na.rm = TRUE),
    PRE_SD = sd(`PRE`, na.rm = TRUE),
    MID_Mean = mean(`MID`, na.rm = TRUE),
    MID_SD = sd(`MID`, na.rm = TRUE),
    POST_Mean = mean(`POST`, na.rm = TRUE),
    POST_SD = sd(`POST`, na.rm = TRUE),
    PRE_Mean_SD = sprintf("%.2f±%.2f", mean(`PRE`, na.rm = TRUE), sd(`PRE`, na.rm = TRUE)),
    MID_Mean_SD = sprintf("%.2f±%.2f", mean(`MID`, na.rm = TRUE), sd(`MID`, na.rm = TRUE)),
    POST_Mean_SD = sprintf("%.2f±%.2f", mean(`POST`, na.rm = TRUE), sd(`POST`, na.rm = TRUE))
  ) %>%
  mutate(
    cohens_d_pre_mid = (MID_Mean - PRE_Mean) / sqrt((PRE_SD^2 + MID_SD^2) / 2),
    cohens_d_pre_post = (POST_Mean - PRE_Mean) / sqrt((PRE_SD^2 + POST_SD^2) / 2),
    cohens_d_mid_post = (POST_Mean - MID_Mean) / sqrt((MID_SD^2 + POST_SD^2) / 2)
  )

mydata_reshaped <- mydata %>%
  pivot_longer(cols = c("PRE", "MID", "POST"), names_to = "Timepoint", values_to = "Weight")

p_values <- mydata_reshaped %>%
  filter(Measure != "Wmax") %>%
  group_by(Measure) %>%
  summarize(
    p_val_pre_mid = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "MID"))$p.value,
    p_val_pre_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "POST"))$p.value,
    p_val_mid_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("MID", "POST"))$p.value
  )

summary_p_values <- left_join(mydata_summary, p_values, by = c("Measure"))

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "VO2_untrained_summary.csv")
write.csv(summary_p_values, file = file_name, row.names = FALSE)


# Pivot the data into long format
long_data <- mydata %>% pivot_longer(cols = c(PRE, MID, POST), names_to = "Timepoint", values_to = "Value")

head(long_data)
# Group the data by measure and timepoint and calculate the mean and sd
summary_data <- long_data %>%
  group_by(Measure, Timepoint) %>%
  summarize(mean = mean(Value), sd = sd(Value), mean_sd = paste(round(mean, 2), "±", round(sd, 2)))


# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "untrained_summary.txt")

# Write the summary data to a text file with the current date in the file name
write.table(summary_data, file = file_name, sep = "\t", row.names = FALSE)
print(summary_data)

unique(long_data$Measure)

long_data <- filter(long_data, Measure != "Wmax")

p_values <- long_data %>%
  group_by(Measure) %>%
  summarize(
    p_val_pre_mid = t.test(Value ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "MID"))$p.value,
    p_val_pre_post = t.test(Value ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "POST"))$p.value,
    p_val_mid_post = t.test(Value ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("MID", "POST"))$p.value
  )

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "VO2_U_p_values.csv")
write.csv(p_values, file_name, row.names = FALSE)

# Subset the data to only include VO2Max and VO2Peak
vo2_data <- subset(long_data, Measure %in% c("VO2Max", "VO2Peak"))

# Change the levels of the Timepoint factor to set the desired order
vo2_data$Timepoint <- factor(vo2_data$Timepoint, levels = c("PRE", "MID", "POST"))

# Plot the data using ggplot2
ggplot(vo2_data, aes(x = Timepoint, y = Value, group = Subject, color = Measure)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Timepoint", y = "Value", color = "Measure") +
  theme_classic()
######

# Calculate the mean value for each timepoint and measure combination
vo2_means <- aggregate(Value ~ Measure + Timepoint, vo2_data, mean)

# Set the y-axis label to the desired format
y_axis_label <- expression(paste("VO"[2]," (", "ml·"~kg^-1~min^-1, ")"))

ggplot(vo2_means, aes(x = Timepoint, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Timepoint", y = y_axis_label, fill = "Measure") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_classic()

# Calculate the mean and standard deviation for each timepoint and measure combination
vo2_summary <- vo2_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Set the y-axis label to the desired format
y_axis_label <- expression(paste("VO"[2]," (", "ml·"~kg^-1~min^-1, ")"))

# Create the bar chart with error bars
ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "Timepoint", y = y_axis_label, fill = "Measure") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_classic()

# Create the plot
vo2_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "", y = y_axis_label, fill = "") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_classic() +
  theme(legend.position = c(0.1, 0.85),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# create a vector indicating which bars are significantly different
sig_diff <- c(F, T, T, F, F, F)

# Create the plot sig diff
vo2_U_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(y = mean + sd + 1, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "", y = y_axis_label, fill = "") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_bw() +
  theme(legend.position = c(0.1, 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "vo2_U_plot.png")
ggsave(file_name_plot, plot = vo2_U_plot, width = 6, height = 4, dpi = 1200)

# Create the plot sig diff
sig_diff <- c(F, T, T, F, F, F)

vo2_U_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = mean + sd + 1, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "Untrained", y = y_axis_label, fill = "") +
  scale_fill_grey() +  # sets the fill color to grayscale
scale_y_continuous(limits = c(0, 60), 
                   breaks = seq(0, 50, 10), 
                   minor_breaks = seq(0, 50, 10), 
                   expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

sig_diff_T <- c(F, F, F, F, F, F)

vo2_T_plot <- 
  ggplot(vo2_T_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = mean + sd + 1, label = ifelse(sig_diff_T, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "Trained", y = y_axis_label, fill = "") +
  scale_fill_grey() +  # sets the fill color to grayscale
  scale_y_continuous(limits = c(0, 60), 
                     breaks = seq(0, 50, 10), 
                     minor_breaks = seq(0, 50, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Combine the plots using plot_grid and include labels
combined_plot <- plot_grid(vo2_U_plot, vo2_T_plot, nrow = 1)

# Display the combined plot
print(combined_plot)
file_name_plot <- paste0(output_directory, "/", date, "VO2_TvsU_plot_sig.png")
ggsave(file_name_plot, plot = combined_plot, width = 8, height = 5, dpi = 1200)

  # Create the plot sig diff
  sig_diff <- c(F, T, T, F, F, F)

vo2_U_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = mean + sd + 1, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "Untrained", y = y_axis_label, fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color)) +
  scale_y_continuous(limits = c(0, 60), 
                     breaks = seq(0, 50, 10), 
                     minor_breaks = seq(0, 50, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

sig_diff_T <- c(F, F, F, F, F, F)

vo2_T_plot <- 
  ggplot(vo2_T_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = mean + sd + 1, label = ifelse(sig_diff_T, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "Trained", y = y_axis_label, fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color)) +
  scale_y_continuous(limits = c(0, 60), 
                     breaks = seq(0, 50, 10), 
                     minor_breaks = seq(0, 50, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Combine the plots using plot_grid and include labels
combined_plot <- plot_grid(vo2_U_plot, vo2_T_plot, nrow = 1)
combined_plot
# Display the combined plot
print(combined_plot)
file_name_plot <- paste0(output_directory, "/", date, "COLOR_VO2_TvsU_plot_sig.png")
ggsave(file_name_plot, plot = combined_plot, width = 8, height = 5, dpi = 1200)

# Subset the data to only include running economy
WM_data <- subset(long_data, Measure %in% c("Wmax"))

# Change the levels of the Timepoint factor to set the desired order
WM_data$Timepoint <- factor(WM_data$Timepoint, levels = c("PRE", "MID", "POST"))

# Calculate the mean and standard deviation for each timepoint and measure combination
WM_summary <- WM_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Create the plot sig diff
WM_plot <- 
  ggplot(WM_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "Untrained", y = "Watts", fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  theme_bw() +
  theme(legend.position = c(0.1, 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_UNTRAINED_WM_plot.png")
ggsave(file_name_plot, plot = WM_plot, width = 6, height = 4, dpi = 1200)


#Generate Wmax comparison plot
mydata <- read_excel("MRP.xlsx", sheet = "vo2data", col_names = TRUE)
str(mydata)
summary(mydata)
head(mydata)
unique(mydata$Measure)
unique(mydata$Subject)

# Define the subjects that are trained and untrained
trained_subjects <- c(8, 17, 18, 19)
untrained_subjects <- c(2, 3, 10, 11, 14)

trained_subjects <- paste0("SUB ", trained_subjects)
untrained_subjects <- paste0("SUB ", untrained_subjects)

# Add a new column "TrainingStatus" based on the subjects
mydata <- mydata %>%
  mutate(TrainingStatus = ifelse(Subject %in% trained_subjects, "trained", "untrained"))

long_data <- mydata %>% pivot_longer(cols = c(PRE, MID, POST), names_to = "Timepoint", values_to = "Value")

# Subset the data to only include running economy
WM_data <- subset(long_data, Measure %in% c("Wmax"))

# Change the levels of the Timepoint factor to set the desired order
WM_data$Timepoint <- factor(WM_data$Timepoint, levels = c("PRE", "MID", "POST"))
WM_data$TrainingStatus <- factor(WM_data$TrainingStatus, levels = c("untrained", "trained"))


# Calculate the mean and standard deviation for each timepoint and measure combination
WM_summary <- WM_data %>%
  group_by(Measure, Timepoint, TrainingStatus) %>%
  summarise(mean = mean(Value), sd = sd(Value))

sig_diff <- c(F, F, F, F, F, T)


# Create the plot sig diff
WM_plot <- 
  ggplot(WM_summary, aes(x = Timepoint, y = mean, fill = TrainingStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(y = mean + sd + 2, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "", y = "Watts", fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  theme_bw() +
  theme(legend.position = c(0.1, 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_COMPARISON_WM_plot.png")
ggsave(file_name_plot, plot = WM_plot, width = 6, height = 4, dpi = 1200)



# Subset the data to only include running economy
YY_data <- subset(long_data, Measure %in% c("YYIR"))

# Change the levels of the Timepoint factor to set the desired order
YY_data$Timepoint <- factor(YY_data$Timepoint, levels = c("PRE", "MID", "POST"))
YY_data$TrainingStatus <- factor(WM_data$TrainingStatus, levels = c("untrained", "trained"))


# Plot the data using ggplot2
ggplot(YY_data, aes(x = Timepoint, y = Value, group = Subject, color = Measure)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Timepoint", y = "Value", color = "Measure") +
  theme_classic()

# Calculate the mean and standard deviation for each timepoint and measure combination
YY_summary <- YY_data %>%
  group_by(Measure, Timepoint, TrainingStatus) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Create the plot sig diff
YY_plot <- 
  ggplot(YY_summary, aes(x = Timepoint, y = mean, fill = TrainingStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "", y = "Meters", fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  theme_bw() +
  theme(legend.position = c(0.1,.95),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_YY_plot.png")
ggsave(file_name_plot, plot = YY_plot, width = 6, height = 4, dpi = 1200)
