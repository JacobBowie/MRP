### Code is checked and edited 08232023 It works Just highlight and run 
set.seed(0308)
# Function to install and load packages
install_and_load_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)}}}

required_packages <- c("readxl", "writexl", "dplyr", "tidyr" ,"openxlsx", "ggplot2", "openxlsx", "stringr", "car", "broom", "knitr")
# Call the function to install and load packages
install_and_load_packages(required_packages)


# Get the current date and format it as mmddyyyy
date <- format(Sys.Date(), "%m%d%Y")
setwd("C:/MRP")
# Create the directory name with todays date 
output_directory <- paste( date, sep = "_","output")
if (!dir.exists(output_directory)) {
  dir.create(output_directory)}

# Read in the Excel file and perform all transformations in a single pipeline
mydata <- read_excel("MRP.xlsx", sheet = "Strength", col_names = TRUE) %>%
  select(1:3, 7:9) %>%
  rename(Subject = 1, Measure = 2, Exercise = 3, PRE = 4, MID = 5, POST = 6) %>%
  filter(!(Subject %in% c("SUB 5", "SUB 13"))) %>%  # Remove subjects 5 and 13
  mutate(
    PRE = as.numeric(PRE),
    MID = as.numeric(MID),
    POST = as.numeric(POST)  )

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "Strength_raw_data.xlsx")
write_xlsx(mydata, file_name)

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

unique(mydata$Exercise)
upper_exercises <- c("OHP", "Lat Pull", "Bent Row", "Seated Row")
lower_exercises <- c("Leg Press", "Deadlift")
head(mydata)

mydata_combined <- mydata %>%
  mutate(Measure_Exercise = paste(Measure, Exercise, sep = "_")) %>%
  filter(Measure_Exercise != "1RM_Bent Row" & Measure_Exercise != "10RM_Bent Row")

#Assessing Normality and homogeneity of variance
# Pivot the data into a longer format
mydata_long <- pivot_longer(mydata_combined, cols = c(PRE, MID, POST),
                            names_to = "Time", values_to = "Value")

# Group your data by 'Measure_Exercise' and perform the Shapiro-Wilk test for each group
normality_tests <- mydata_long %>%
  group_by(Measure_Exercise, Time) %>%
  summarize(p_value = shapiro.test(Value)$p.value)
# Print the results
print(normality_tests)
# Filter for sig non-normality 
filter(normality_tests, p_value < 0.05)

#At these specific timepoints for these values there is a non-normal distribution
#Seated row makes sense due to the difficulties in load
# OHP 10RM 
# > filter(normality_tests, p_value < 0.05)
# # A tibble: 4 × 3
# # Groups:   Measure_Exercise [3]
# Measure_Exercise Time  p_value
# <chr>            <chr>   <dbl>
#   1 10RM_OHP         MID    0.0432
# 2 10RM_OHP         PRE    0.0226
# 3 10RM_Seated Row  POST   0.0272
# 4 1RM_Seated Row   POST   0.0165

# Filter for sig non-normality 
mydata_long2 <- mydata_long %>%
  filter(Measure_Exercise %in% c("10RM_OHP", "1RM_Seated Row", "10RM_Seated Row"))
ggplot(mydata_long2, aes(x = Value, fill = Time)) +
  geom_histogram(binwidth = 5, position = "dodge") +  # Set position to "dodge" to unstack bars
  facet_wrap(~ Measure_Exercise, scales = "free") +
  labs(x = "Value", y = "Frequency") +
  ggtitle("Histogram Facetted by Measure_Exercise") +
  theme_minimal()



#Slight right skewness in the data 
# But, the data is not too far from normality
#OHP has a very tight range, with some very low values
# Actually that's probably what caused the violation, the extra weak subjects skewing the data 

# Perform Levene's Test for homogeneity of variances
leveneTest(Value ~ Time * Measure_Exercise, data = mydata_long)
# > leveneTest(Value ~ Time * Measure_Exercise, data = mydata_long)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group  35  0.6386 0.9452
#       285     
#Variances are homogenous! 

# Perform Levene's Test for each Measure
levene_results <- mydata_long %>%
  group_by(Measure_Exercise ) %>%
  do(tidy(leveneTest(Value ~ Time, data = .)))
print(levene_results)
# Variances by measure are homogenous!

# Create a histogram facetted by "Measure_Exercise" and colored by Time
ggplot(mydata_long, aes(x = Value, fill = Time)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  facet_wrap(~ Measure_Exercise, scales = "free") +  # Updated to allow for free scales if necessary
  labs(x = "Value", y = "Frequency") +
  ggtitle("Histogram Facetted by Measure_Exercise and Colored by Time") +
  theme_minimal()


# Step 1: Calculate mean, standard deviation, and mean±SD for PRE, MID, POST and calculate p-values
mydata_summary <- mydata_combined %>%
  group_by(Measure_Exercise) %>%
  summarise(
    PRE_Mean = mean(PRE, na.rm = TRUE),
    MID_Mean = mean(MID, na.rm = TRUE),
    POST_Mean = mean(POST, na.rm = TRUE),
    PRE_SD = sd(PRE, na.rm = TRUE),
    MID_SD = sd(MID, na.rm = TRUE),
    POST_SD = sd(POST, na.rm = TRUE),
    p_val_pre_mid = t.test(PRE, MID, paired = TRUE, na.rm = TRUE)$p.value,
    p_val_pre_post = t.test(PRE, POST, paired = TRUE, na.rm = TRUE)$p.value,
    p_val_mid_post = t.test(MID, POST, paired = TRUE, na.rm = TRUE)$p.value
  )
print(mydata_summary)
# Define the file name with the current date
file_name_summary <- paste0(output_directory, "/", date, "strength_summary_pval.xlsx")
write_xlsx(mydata_summary, file_name_summary)

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
  filter(Exercise != "Bent Row") %>% 
  group_by(Measure, Exercise) %>%
  summarise(
    PRE_Mean = mean(PRE, na.rm = TRUE),
    MID_Mean = mean(MID, na.rm = TRUE),
    POST_Mean = mean(POST, na.rm = TRUE),
    percent_change = list(calculate_percent_change(PRE_Mean, MID_Mean, POST_Mean)),
    .groups = "drop"
  ) %>%
  unnest(percent_change)

percent_change_by_upperlower <- mydata %>%
  mutate(Exercise_Type = ifelse(Exercise %in% upper_exercises, "Upper", "Lower")) %>%
  group_by(Measure, Exercise_Type) %>%
  summarise(
    PRE_Mean = mean(PRE, na.rm = TRUE),
    MID_Mean = mean(MID, na.rm = TRUE),
    POST_Mean = mean(POST, na.rm = TRUE),
    percent_change = list(calculate_percent_change(PRE_Mean, MID_Mean, POST_Mean)),
    .groups = "drop"
  ) %>%
  unnest(percent_change)

# > percent_change_by_upperlower
# # A tibble: 4 × 8
# Measure Exercise_Type PRE_Mean MID_Mean POST_Mean PRE_to_MID MID_to_POST PRE_to_POST
# <chr>   <chr>            <dbl>    <dbl>     <dbl>      <dbl>       <dbl>       <dbl>
#   1 10RM    Lower             56.5     70.4      77.7       24.7        10.4        37.6
# 2 10RM    Upper             44.8     54.6      60.6       22.0        11.0        35.4
# 3 1RM     Lower             76.3     91.9     102.        20.3        10.7        33.3
# 4 1RM     Upper             58.9     68.3      75.3       16.0        10.3        27.9
percent_change_per_subject_upperlower <- mydata %>%
  mutate(Exercise_Type = ifelse(Exercise %in% upper_exercises, "Upper", "Lower")) %>%
  group_by(Subject, Measure, Exercise_Type) %>%  # Include Subject here
  summarise(
    PRE = mean(PRE, na.rm = TRUE),
    MID = mean(MID, na.rm = TRUE),
    POST = mean(POST, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(percent_change = list(calculate_percent_change(PRE, MID, POST))) %>%
  unnest(percent_change)

#Ran t-tests between upper and lower at each timepoint no sig diff.  Code was ugly so excluded 
#lowest p-value 0.17, highest 0.96 mean like .6 so no sig diff between upper and lower

percent_change_summary_with_pvalues_upperlower <- percent_change_per_subject_upperlower %>% 
  # filter(Exercise != "Bent Row") %>% 
  group_by(Measure, Exercise_Type) %>%
  summarise(
    p_val_pre_mid = t.test(PRE_to_MID, MID_to_POST, paired = TRUE, na.rm = TRUE)$p.value,
    p_val_pre_post = t.test(PRE_to_MID, PRE_to_POST, paired = TRUE, na.rm = TRUE)$p.value,
    p_val_mid_post = t.test(MID_to_POST, PRE_to_POST, paired = TRUE, na.rm = TRUE)$p.value,
    PRE_Mean_SD = sprintf("%.2f±%.2f", mean(PRE_to_MID, na.rm = TRUE), sd(PRE_to_MID, na.rm = TRUE)),
    MID_Mean_SD = sprintf("%.2f±%.2f", mean(MID_to_POST, na.rm = TRUE), sd(MID_to_POST, na.rm = TRUE)),
    POST_Mean_SD = sprintf("%.2f±%.2f", mean(PRE_to_POST, na.rm = TRUE), sd(PRE_to_POST, na.rm = TRUE)),
    .groups = "drop"
  )

# # A tibble: 4 × 8
# Measure Exercise_Type p_val_pre_mid p_val_pre_post p_val_mid_post PRE_Mean_SD MID_Mean_SD POST_Mean_SD
# <chr>   <chr>                 <dbl>          <dbl>          <dbl> <chr>       <chr>       <chr>       
#   1 10RM    Lower               0.00249       0.000168       0.000339 27.28±13.42 10.80±4.72  41.36±18.59 
# 2 10RM    Upper               0.0277        0.000187       0.00260  27.54±18.51 11.88±4.71  42.96±23.82 
# 3 1RM     Lower               0.0255        0.00242        0.000602 22.12±11.74 11.34±7.56  36.21±18.17 
# 4 1RM     Upper               0.239         0.00167        0.000150 15.67±6.97  11.49±7.56  29.00±11.94 

# First Stage: Calculating mean and percent change per subject
percent_change_per_subject <- mydata %>%
  filter(Exercise != "Bent Row") %>% 
  group_by(Subject, Measure, Exercise) %>%  # Include Subject here
  summarise(
    PRE = mean(PRE, na.rm = TRUE),
    MID = mean(MID, na.rm = TRUE),
    POST = mean(POST, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(percent_change = list(calculate_percent_change(PRE, MID, POST))) %>%
  unnest(percent_change)

filter(percent_change_per_subject, Exercise == "Deadlift")
filter(percent_change_per_subject, Exercise == "OHP")


min_max_values <- percent_change_per_subject %>%
  group_by(Measure, Exercise) %>%
  summarise(
    Min_PRE_to_MID = min(PRE_to_MID, na.rm = TRUE),
    Max_PRE_to_MID = max(PRE_to_MID, na.rm = TRUE),
    Min_MID_to_POST = min(MID_to_POST, na.rm = TRUE),
    Max_MID_to_POST = max(MID_to_POST, na.rm = TRUE),
    Min_PRE_to_POST = min(PRE_to_POST, na.rm = TRUE),
    Max_PRE_to_POST = max(PRE_to_POST, na.rm = TRUE),
    .groups = "drop"
  )
print(min_max_values)

# Second Stage: Calculating p-values and Mean±SD
percent_change_summary_with_pvalues <- percent_change_per_subject %>% 
  filter(Exercise != "Bent Row") %>% 
  group_by(Measure, Exercise) %>%
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
                            by = c("Measure", "Exercise"))
print(merged_summary)

file_name <- paste0(output_directory, "/", date, "Strength_percent_change_summary.xlsx")
write_xlsx(merged_summary, file_name)
file_name <- paste0(output_directory, "/", date, "_per_subject_Strength_percent_change_summary.xlsx")
write_xlsx(percent_change_per_subject, file_name)

#remove bentrow for ANOVA 
# Combine the Measure and Exercise columns into a new column Measure
mydata_mutate <- mydata %>%
  filter(Exercise != "Bent Row") %>%
  unite("Measure", Measure, Exercise, sep = "_") 

# Create a list of measures
measures <- unique(mydata_mutate$Measure)

# Open a file connection for writing
# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "Strength_anova_results.txt")
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

######################################################################################################################

#make barplot
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

# Create a new dataframe with 1RM data only, filtering out "Bent Over Row" and removing the Measure column
mydata_1rm <- mydata %>%
  filter(Exercise != "Bent Over Row", str_detect(Measure, "1RM")) %>%
  select(-Measure)
mydata_1rm$Exercise <- dplyr::recode(mydata_1rm$Exercise, !!!replacements)
# Create a new dataframe with 10RM data only, filtering out "Bent Over Row" and removing the Measure column
mydata_10rm <- mydata %>%
  filter(Exercise != "Bent Over Row", str_detect(Measure, "10RM")) %>%
  select(-Measure)
mydata_10rm$Exercise <- dplyr::recode(mydata_10rm$Exercise, !!!replacements)

# Calculate means and standard deviations for each exercise and time point
mydata_10rm_summary <- mydata_10rm %>%
  group_by(Exercise) %>%
  summarise(across(
    c(PRE, MID, POST),
    list(Mean = mean, SD = sd), # Capitalize the Mean here
    .names = "{.col}_{.fn}")  )

# Reshape the data into a longer format
mydata_10rm_summary_long <- mydata_10rm_summary %>%
  pivot_longer(
    cols = c(PRE_Mean:POST_SD), # Update column names to match the new pattern
    names_to = c("Time", ".value"),
    names_pattern = "(PRE|MID|POST)_(.*)"
  )

#reorder so that the plot displays in order of magnitude and pre mid post. 
mydata_10rm_summary_long <- mydata_10rm_summary_long %>%
  mutate(Exercise = reorder(Exercise, -Mean))
mydata_10rm_summary_long$Time <- factor(mydata_10rm_summary_long$Time, levels = c("PRE", "MID", "POST"))

# Plot the data using ggplot2
rm10plot <- ggplot(mydata_10rm_summary_long, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  labs(x = "", y = "10RM (kg)", fill = "Time") +
  scale_fill_grey(start = 0.2, end = 0.8, name = "") +
  scale_y_continuous(limits = c(0, 135), 
                     breaks = seq(0, 140, 20), 
                     minor_breaks = seq(0, 140, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.75),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "10rm_plot.png")
ggsave(file_name_plot, plot = rm10plot, width = 6, height = 4, dpi = 1200)




# create a vector indicating which bars are significantly different
sig_diff_10s <- c(F, T, T, F, T, T, F, T, T, F, T, T, F, T, T, F, T, T)

# plot with markers for significant differences
rm10splot <- ggplot(mydata_10rm_summary_long, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(y = Mean + SD + 2, label = ifelse(sig_diff_10s, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "", y = "10RM (kg)", fill = "Time") +
  scale_fill_grey(start = 0.2, end = 0.8, name = "") +
  scale_y_continuous(limits = c(0, 145), 
                     breaks = seq(0, 140, 20), 
                     minor_breaks = seq(0, 140, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.75),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        text = element_text(size = 12),          # Set font size for general text
        axis.title = element_text(size = 12),    # Set font size for axis titles
        axis.text = element_text(size = 12),     # Set font size for axis text
        legend.title = element_text(size = 12),  # Set font size for legend title
        legend.text = element_text(size = 12))   # Set font size for legend text)

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_10rm_plot_sig.png")
ggsave(file_name_plot, plot = rm10splot, width = 6, height = 4, dpi = 1200)


# Define color values
dark_blue_color <- "#00205B"   # Dark blue color
light_blue_color <- "#6699CC"  # Light blue color
third_color <- "#A0C9E0"       # Third color

# plot with markers for significant differences COLOR
rm10splot <- ggplot(mydata_10rm_summary_long, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(y = Mean + SD + 2, label = ifelse(sig_diff_10s, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "", y = "10RM (kg)", fill = "Time") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +  # Apply custom fill colors 
  scale_y_continuous(limits = c(0, 165), 
                     breaks = seq(0, 160, 20), 
                     minor_breaks = seq(0, 160, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
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
print(rm10splot)

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_COLOR_10rm_plot_sig.png")
ggsave(file_name_plot, plot = rm10splot, width = 6, height = 4, dpi = 1200)


# Calculate means and standard deviations for each exercise and time point
mydata_1rm_summary <- mydata_1rm %>%
  group_by(Exercise) %>%
  summarise(across(
    c(PRE, MID, POST),
    list(Mean = mean, SD = sd), # Capitalize the Mean here
    .names = "{.col}_{.fn}")
  )

# Reshape the data into a longer format
mydata_1rm_summary_long <- mydata_1rm_summary %>%
  pivot_longer(
    cols = c(PRE_Mean:POST_SD), # Update column names to match the new pattern
    names_to = c("Time", ".value"),
    names_pattern = "(PRE|MID|POST)_(.*)"
  )

# Reorder so that the plot displays in order of magnitude and PRE, MID, POST.
mydata_1rm_summary_long <- mydata_1rm_summary_long %>%
  mutate(Exercise = reorder(Exercise, -Mean))
mydata_1rm_summary_long$Time <- factor(mydata_1rm_summary_long$Time, levels = c("PRE", "MID", "POST"))


# Plot the data using ggplot2
ggplot(mydata_1rm_summary_long, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean -SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  labs(x = "Exercise", y = "Mean 1rm", fill = "Time") +
  scale_fill_grey(start = 0.2, end = 0.8, name = "Time") +
  theme_bw()

rm1plot <- ggplot(mydata_1rm_summary_long, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  labs(x = "", y = "1rm (kg)", fill = "Time") +
  scale_fill_grey(start = 0.2, end = 0.8, name = "") +
  scale_y_continuous(limits = c(0, 135), 
                     breaks = seq(0, 140, 20), 
                     minor_breaks = seq(0, 140, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.75),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "1rm_plot.png")
ggsave(file_name_plot, plot = rm1plot, width = 6, height = 4, dpi = 1200)

# create a vector indicating which bars are significantly different
sig_diff_1s <- c(F, T, T, F, T, T, F, T, T, F, T, T, F, T, T, F, T, T)

# plot with markers for significant differences
rm1splot <- ggplot(mydata_1rm_summary_long, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(y = Mean + SD + 2, label = ifelse(sig_diff_1s, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "", y = "1RM (kg)", fill = "Time") +
  scale_fill_grey(start = 0.2, end = 0.8, name = "") +
  scale_y_continuous(limits = c(0, 160), 
                     breaks = seq(0, 150, 20), 
                     minor_breaks = seq(0, 150, 10), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.8),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        text = element_text(size = 12),          # Set font size for general text
        axis.title = element_text(size = 12),    # Set font size for axis titles
        axis.text = element_text(size = 12),     # Set font size for axis text
        legend.title = element_text(size = 12),  # Set font size for legend title
        legend.text = element_text(size = 12))   # Set font size for legend text)

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "1rm_plot_sig.png")
ggsave(file_name_plot, plot = rm1splot, width = 6, height = 4, dpi = 1200)


scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color))  # Apply custom fill colors +
  
  # plot with markers for significant differences
  rm1splot <- ggplot(mydata_1rm_summary_long, aes(x = Exercise, y = Mean, fill = Time)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(y = Mean + SD + 2, label = ifelse(sig_diff_1s, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "", y = "1RM (kg)", fill = "Time") +
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
        panel.border = element_blank(),
        text = element_text(size = 12),          # Set font size for general text
        axis.title = element_text(size = 12, color= "black"),    # Set font size for axis titles
        axis.text = element_text(size = 11, color = "black"),     # Set font size for axis text
        legend.title = element_text(size = 12),  # Set font size for legend title
        legend.text = element_text(size = 12))   # Set font size for legend text))

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_COLOR_1rm_plot_sig.png")
ggsave(file_name_plot, plot = rm1splot, width = 6, height = 4, dpi = 1200)


# Pivot the data into long format
mydata_long <- mydata_1rm %>%
  pivot_longer(cols = c(PRE, MID, POST), names_to = "Time", values_to = "Value")

# First, ensure that 'Time' is an ordered factor with levels in the correct order
mydata_long$Time <- factor(mydata_long$Time, levels = c("PRE", "MID", "POST"))

# Plot bars with points and lines conencting each subject to illustrate changes over time 
#I don't love the way this turned out, but it seems like getting points appropriately placed is a bit tricky for the prior plot 
ggplot() +
  geom_errorbar(data = mydata_1rm_summary_long, aes(x = Time, y = Mean, ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.75), width = 0.2) +
  geom_bar(data = mydata_1rm_summary_long, aes(x = Time, y = Mean, fill = Time), 
           stat = "identity", position = position_dodge(width = 0.75), width = 0.7) +
  geom_point(data = mydata_long, aes(x = Time, y = Value, color = Time, group = interaction(Subject, Exercise)), 
             position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.1), size = 1.5) +
  geom_line(data = mydata_long, aes(x = Time, y = Value, group = interaction(Subject, Exercise), color = Time), 
            position = position_dodge(width = 0.2)) +
  facet_wrap(~ Exercise, scales = "free_y") +
  scale_fill_manual(values = c("PRE" = "red", "MID" = "blue", "POST" = "green")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


#### nonsense ####
##############################################
# PART 2 VO2 data ####

# read in the data from the Excel file
mydata <- read_excel("MRP.xlsx", sheet = "vo2data", col_names = TRUE)
str(mydata)
summary(mydata)
head(mydata)
unique(mydata$Measure)
unique(mydata$Subject)

# Create a list of measures
measures <- c("VO2Max", "VO2Peak", "Body fat (%)", "Weight (Kg)", "Vertical Jump","6.5km*hr-1", "16km*hr-1",
              "12km*hr-1")
# Create the file name by appending the date to "anova_results.txt"
file_name <- paste0(output_directory, "/", date, "anova_results.txt")

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

#Assessing Normality and homogeneity of variance
# Group your data by 'Measure_Exercise' and perform the Shapiro-Wilk test for each group
normality_tests <- mydata_reshaped %>%
  filter(Weight != 0) %>%
  group_by(Measure, Timepoint) %>%
  summarize(p_value = shapiro.test(Weight)$p.value)
# Print the results
print(normality_tests)
# Filter for sig non-normality 
filter(normality_tests, p_value < 0.05)

# #At these specific timepoints for these values there is a non-normal distribution
# YYIr had a huge range!  Test for normality when grouped by Trained/Untrained since we use that for comparison 
# Huge right skew in 12kmhr 
# Long right tail with YYIR 
# > filter(normality_tests, p_value < 0.05)
# # A tibble: 3 × 3
# # Groups:   Measure [2]
# Measure   Timepoint p_value
# <chr>     <chr>       <dbl>
#   1 12km*hr-1 POST       0.0327
# 2 YYIR      MID        0.0234
# 3 YYIR      PRE        0.0204

# Filter for sig non-normality 
mydata_long2 <- mydata_reshaped %>%
  filter(Weight != 0) %>%
    filter(Measure %in% c("12km*hr-1", "YYIR")) 

ggplot(mydata_long2, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  facet_wrap(~ Measure, scales = "free") +
  labs(x = "Value", y = "Frequency") +
  ggtitle("Histogram Facetted by Measure_Exercise") +
  theme_minimal()

mydata_long <- pivot_longer(mydata_combined, cols = c(PRE, MID, POST),
                            names_to = "Time", values_to = "Value")
# Perform Levene's Test for homogeneity of variances
leveneTest(Weight ~ Timepoint *Measure, data = mydata_reshaped)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value    Pr(>F)    
# group  29  5.4395 4.129e-14 ***
#       225                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Variance is non-homogenous. As expected. Measures are diverse and not expected to have equal variance.

# Perform Levene's Test for each Measure
levene_results <- mydata_reshaped %>%
  group_by(Measure) %>%
  do(tidy(leveneTest(Weight ~ Timepoint, data = .)))

# Print results
print(levene_results)
filter(levene_results, p.value < 0.05)
#No sig differences in variance between timepoints for any measure.


p_values <- mydata_reshaped %>%
  group_by(Measure) %>%
  summarize(
    p_val_pre_mid = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "MID"))$p.value,
    p_val_pre_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "POST"))$p.value,
    p_val_mid_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("MID", "POST"))$p.value
  )

# Perform the filtering step separately
p_values %>%
  filter(if_any(starts_with("p_val"), ~ . < 0.05))

summary_p_values <- left_join(mydata_summary, p_values, by = c("Measure"))

# Define the file name with the current date
file_name <- paste0(output_directory, "/", date, "VO2_combined_summary.csv")
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

file_name <- paste0(output_directory, "/", date, "VO2__p_values.csv")
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

# Calculate the mean and standard deviation for each timepoint and measure combination
vo2_summary <- vo2_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Set the y-axis label to the desired format
y_axis_label <- expression(paste("VO"[2]," (", "ml·"~kg^-1~min^-1, ")"))
# Set the y-axis label to the desired format
y_axis_label <- expression(paste("VO"[2], " (", "ml" %*% kg^-1 %*% min^-1, ")"))

# Create the bar chart with error bars
ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Timepoint", y = y_axis_label, fill = "Measure") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_classic()

# Create the plot
vo2_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = y_axis_label, fill = "") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_classic() +
  theme(legend.position = c(0.1, 0.85),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# create a vector indicating which bars are significantly different
sig_diff <- c(F, T, T, F, F, F)

# Create the plot sig diff
vo2_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = mean + sd + 1, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "", y = y_axis_label, fill = "") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_bw() +
  theme(legend.position = c(0.1, 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())


# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "vo2_plot.png")
ggsave(file_name_plot, plot = vo2_plot, width = 6, height = 4, dpi = 1200)


# create a vector indicating which bars are significantly different
sig_diff <- c(F, T, T, F, F, F)


# consider a recode to use dots and lines!!! 

# Create the plot sig diff
vo2_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = mean + sd + 1, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "", y = y_axis_label, fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color)) +
  theme_bw() +
  theme(legend.position = c(0.1, 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())


# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_vo2_plot.png")
ggsave(file_name_plot, plot = vo2_plot, width = 8, height = 5, dpi = 1200)



# Generating plots for  on-sig variables just because 
# "Body fat (%)", "Weight (Kg)", "Vertical Jump","6.5km*hr-1", "16km*hr-1",
#               "12km*hr-1")


# Subset the data to only include body fat and weight 
BC_data <- subset(long_data, Measure %in% c("Body fat (%)", "Weight (Kg)"))

# Change the levels of the Timepoint factor to set the desired order
BC_data$Timepoint <- factor(BC_data$Timepoint, levels = c("PRE", "MID", "POST"))

# Plot the data using ggplot2
ggplot(BC_data, aes(x = Timepoint, y = Value, group = Subject, color = Measure)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Timepoint", y = "Value", color = "Measure") +
  theme_classic()

# Calculate the mean and standard deviation for each timepoint and measure combination
BC_summary <- BC_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Create the plot sig diff
BC_plot <- 
  ggplot(BC_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "", fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color)) +
  theme_bw() +
  theme(legend.position = c(0.1, 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_BC_plot.png")
ggsave(file_name_plot, plot = BC_plot, width = 6, height = 4, dpi = 1200)

# Subset the data to only include body fat and weight 
RE_data <- subset(long_data, Measure %in% c("6.5km*hr-1", "16km*hr-1", "12km*hr-1"))

# Change the levels of the Timepoint factor to set the desired order
RE_data$Timepoint <- factor(RE_data$Timepoint, levels = c("PRE", "MID", "POST"))
RE_data$Measure <- factor(RE_data$Measure, levels = c("6.5km*hr-1", "12km*hr-1", "16km*hr-1"))


# Plot the data using ggplot2
ggplot(RE_data, aes(x = Timepoint, y = Value, group = Subject, color = Measure)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Timepoint", y = "Value", color = "Measure") +
  theme_classic()

# Calculate the mean and standard deviation for each timepoint and measure combination
RE_summary <- RE_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Create the plot sig diff
RE_plot <- 
  ggplot(RE_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = y_axis_label, fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +  # Manual y-axis limits and tick marks
  theme_bw() +
  theme(legend.position = c(0.1, 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())


# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_RE_plot.png")
ggsave(file_name_plot, plot = RE_plot, width = 6, height = 4, dpi = 1200)

# Subset the data to only include running economy
VJ_data <- subset(long_data, Measure %in% c("Vertical Jump"))

# Change the levels of the Timepoint factor to set the desired order
VJ_data$Timepoint <- factor(VJ_data$Timepoint, levels = c("PRE", "MID", "POST"))


# Plot the data using ggplot2
ggplot(VJ_data, aes(x = Timepoint, y = Value, group = Subject, color = Measure)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Timepoint", y = "Value", color = "Measure") +
  theme_classic()

# Calculate the mean and standard deviation for each timepoint and measure combination
VJ_summary <- VJ_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Create the plot sig diff
VJ_plot <- 
  ggplot(VJ_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Inches", fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  theme_bw() +
  theme(legend.position = c(0.1, 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_VJ_plot.png")
ggsave(file_name_plot, plot = VJ_plot, width = 6, height = 4, dpi = 1200)


# Subset the data to only include running economy
WM_data <- subset(long_data, Measure %in% c("Wmax"))

# Change the levels of the Timepoint factor to set the desired order
WM_data$Timepoint <- factor(WM_data$Timepoint, levels = c("PRE", "MID", "POST"))


# Plot the data using ggplot2
ggplot(WM_data, aes(x = Timepoint, y = Value, group = Subject, color = Measure)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Timepoint", y = "Value", color = "Measure") +
  theme_classic()

# Calculate the mean and standard deviation for each timepoint and measure combination
WM_summary <- WM_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Create the plot sig diff
WM_plot <- 
  ggplot(WM_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Watts", fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_WM_plot.png")
ggsave(file_name_plot, plot = WM_plot, width = 6, height = 4, dpi = 1200)


# Subset the data to only include body fat and weight 
BC_data <- subset(long_data, Measure %in% c("Body fat (%)", "Weight (Kg)", "Vertical Jump"))

# Change the levels of the Timepoint factor to set the desired order
BC_data$Timepoint <- factor(BC_data$Timepoint, levels = c("PRE", "MID", "POST"))

# Plot the data using ggplot2
ggplot(BC_data, aes(x = Timepoint, y = Value, group = Subject, color = Measure)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Timepoint", y = "Value", color = "Measure") +
  theme_classic()

# Calculate the mean and standard deviation for each timepoint and measure combination
BC_summary <- BC_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Create the plot sig diff
BC_plot <- 
  ggplot(BC_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "", fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  theme_bw() +
  theme(legend.position = c(0.1, 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_COMBO_BC_VJ_plot.png")
ggsave(file_name_plot, plot = BC_plot, width = 6, height = 4, dpi = 1200)

# Subset the data to only include running economy
YY_data <- subset(long_data, Measure %in% c("YYIR"))

# Change the levels of the Timepoint factor to set the desired order
YY_data$Timepoint <- factor(YY_data$Timepoint, levels = c("PRE", "MID", "POST"))
# Define the subjects that are trained and untrained
trained_subjects <- c(8, 17, 18, 19)
untrained_subjects <- c(2, 3, 10, 11, 14)

trained_subjects <- paste0("SUB ", trained_subjects)
untrained_subjects <- paste0("SUB ", untrained_subjects)

# Add a new column "TrainingStatus" based on the subjects
YY_data <- YY_data %>%
  mutate(TrainingStatus = ifelse(Subject %in% trained_subjects, "trained", "untrained"))


# Calculate the mean and standard deviation for each timepoint and measure combination
YY_summary <- YY_data %>%
  group_by(Measure, Timepoint, TrainingStatus) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Create the plot sig diff
YY_plot <- 
  ggplot(YY_summary, aes(x = Timepoint, y = mean, fill = TrainingStatus)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Meters", fill = "") +
  scale_fill_manual(values = c(dark_blue_color, light_blue_color, third_color)) +
  theme_bw() +
  theme(legend.position = c(0.1, .95),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

# Save the plot to a png file at 1200 dpi
file_name_plot <- paste0(output_directory, "/", date, "_color_YY_plot.png")
ggsave(file_name_plot, plot = YY_plot, width = 6, height = 4, dpi = 1200)
