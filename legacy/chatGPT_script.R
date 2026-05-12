# load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# read in the data from the Excel file
mydata <- read_excel("C:/Users/Jacob Bowie/Downloads/MRP.xlsx", sheet = "0409 copy 1RM10RM TESTING", col_names = TRUE)

# select the relevant columns and clean up the column names
mydata <- mydata %>%
  select(1:3, 7:9) %>%
  slice(1:(nrow(mydata) - 24)) %>%
  rename(Subject...1 = `#`, Measure...2 = `Test Type`, Exercise...3 = `Exercise Type`,
         `PRE (kg)` = `Pre-Test (kg)`, `MID (kg)` = `Mid-Test (kg)`, `POST (kg)` = `Post-Test (kg)`)

# convert weight columns to numeric
mydata <- mydata %>%
  mutate(across(`PRE (kg)`:`POST (kg)`, as.numeric))

# summarize the data
mydata_summary <- mydata %>%
  group_by(Exercise, Measure) %>%
  summarize(PRE_Mean = mean(`PRE (kg)`, na.rm = TRUE),
            PRE_SD = sd(`PRE (kg)`, na.rm = TRUE),
            MID_Mean = mean(`MID (kg)`, na.rm = TRUE),
            MID_SD = sd(`MID (kg)`, na.rm = TRUE),
            POST_Mean = mean(`POST (kg)`, na.rm = TRUE),
            POST_SD = sd(`POST (kg)`, na.rm = TRUE))

# write the summary data to an Excel file
write_xlsx(mydata_summary, "summary.xlsx")

# reshape the data
mydata_reshaped <- mydata %>%
  pivot_longer(cols = c(`PRE (kg)`, `MID (kg)`, `POST (kg)`), names_to = "Timepoint", values_to = "Weight")

# run the repeated-measures ANOVA
my_anova <- aov(Weight ~ Exercise * Measure + Timepoint + Error(Subject/(Timepoint)), data = mydata_reshaped)

# Define the file name with the current date
file_name <- paste0(date, "_anova_results.txt")
# print the ANOVA results to the console and save them to a text file
sink(file_name)
summary(my_anova)
sink()

# calculate p-values for pairwise comparisons
p_values <- mydata_reshaped %>%
  filter(Exercise != "Bent Over Row") %>%
  group_by(Measure, Exercise) %>%
  summarize(
    p_val_pre_mid = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE (kg)", "MID (kg)"))$p.value,
    p_val_pre_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE (kg)", "POST (kg)"))$p.value,
    p_val_mid_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("MID (kg)", "POST (kg)"))$p.value
  )

# write the p-values to a CSV file
write.csv(p_values, "p_values.csv", row.names = FALSE)





library(tidyverse)

# Load the data
mydata <- read.csv("mydata.csv")

# Reshape the data
mydata_reshaped <- mydata %>%
  pivot_longer(cols = c("PRE (kg)", "MID (kg)", "POST (kg)"), names_to = "Timepoint", values_to = "Weight")

# Filter out Bent Over Row exercise
mydata_reshaped <- mydata_reshaped %>%
  filter(Exercise != "Bent Over Row")

# Perform normality tests
normality_tests <- mydata_reshaped %>%
  group_by(Measure, Exercise, Timepoint) %>%
  summarize(
    shapiro_p = shapiro.test(Weight)$p.value,
    kstest_p = ks.test(Weight, "pnorm")$p.value
  )

# Perform homogeneity test
homogeneity_test <- mydata_reshaped %>%
  group_by(Measure, Exercise) %>%
  summarize(
    levene_p = leveneTest(Weight ~ Timepoint, data = ., center = mean)$p.value
  )

# Perform pairwise t-tests
p_values <- mydata_reshaped %>%
  group_by(Measure, Exercise) %>%
  filter(n_distinct(Timepoint) == 3) %>%
  summarize(
    p_val_pre_mid = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE (kg)", "MID (kg)"))$p.value,
    p_val_pre_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE (kg)", "POST (kg)"))$p.value,
    p_val_mid_post = t.test(Weight ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("MID (kg)", "POST (kg)"))$p.value
  )

# Write the results to a file
write.table(normality_tests, file = "normality_tests.txt", sep = "\t", row.names = FALSE)
write.table(homogeneity_test, file = "homogeneity_test.txt", sep = "\t", row.names = FALSE)
write.table(p_values, file = "pairwise_t_tests.txt", sep = "\t", row.names = FALSE)


###############################################
#PART 2

# read in the data from the Excel file
mydata <- read_excel("MRP.xlsx", sheet = "vo2data", col_names = TRUE)
str(mydata)
summary(mydata)
head(mydata)
unique(mydata$Measure)

# Pivot the data into long format
long_data <- mydata %>% pivot_longer(cols = c(PRE, MID, POST), names_to = "Timepoint", values_to = "Value")

# Load the tidyr package for data manipulation
library(tidyr)

# Create a list of measures
measures <- c("VO2Max", "VO2Peak", "Body fat (%)", "Weight (Kg)", "Vertical Jump")

# Open a file connection for writing
fileConn <- file("anova_results.txt", "w")

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


# Group the data by measure and timepoint and calculate the mean and sd
summary_data <- long_data %>%
  group_by(Measure, Timepoint) %>%
  summarize(mean = mean(Value), sd = sd(Value), mean_sd = paste(round(mean, 2), "±", round(sd, 2)))

# Get the current date and format it as mmddyyyy
date <- format(Sys.Date(), "%m%d%Y")

# Define the file name with the current date
file_name <- paste0(date, "_summary.txt")

# Write the summary data to a text file with the current date in the file name
write.table(summary_data, file = file_name, sep = "\t", row.names = FALSE)
print(summary_data)

head(long_data)
unique(long_data$Measure)


p_values <- long_data %>%
  group_by(Measure) %>%
  summarize(
    p_val_pre_mid = t.test(Value ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "MID"))$p.value,
    p_val_pre_post = t.test(Value ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("PRE", "POST"))$p.value,
    p_val_mid_post = t.test(Value ~ Timepoint, paired = TRUE, subset = Timepoint %in% c("MID", "POST"))$p.value
  )

view(p_values)
write.csv(p_values, "VO2_p_values.csv", row.names = FALSE)



library(ggplot2)
library(ggplot2)

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
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "Timepoint", y = y_axis_label, fill = "Measure") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_classic()


# Create the plot
vo2_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
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
sig_diff <- c(F, T, T, F, F, T)


# Create the plot sig diff
vo2_plot <- 
  ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(y = mean + sd + 1, label = ifelse(sig_diff, "*", "")), 
            position = position_dodge(width = 0.9), size = 10) +
  labs(x = "", y = y_axis_label, fill = "") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_bw() +
  theme(legend.position = c(0.1, 0.85),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())


# Save the plot to a png file at 1200 dpi
ggsave("vo2_plot.png", plot = vo2_plot, width = 6, height = 4, dpi = 1200)





# Calculate the mean and standard deviation for each timepoint and measure combination
vo2_summary <- vo2_data %>%
  group_by(Measure, Timepoint) %>%
  summarise(mean = mean(Value), sd = sd(Value))

# Perform a paired t-test to compare the MID and PRE timepoints
vo2_ttest <- vo2_data %>%
  filter(Timepoint %in% c("MID", "PRE")) %>%
  group_by(Measure, Subject) %>%
  summarise(t = round(t.test(Value ~ Timepoint)$statistic, 2)) %>%
  ungroup() %>%
  filter(t > 2.5)  # only keep significant differences

# Set the y-axis label to the desired format
y_axis_label <- expression(paste("VO"[2]," (", "ml·"~kg^-1~min^-1, ")"))

# Create the bar chart with error bars and asterisks for significant differences
ggplot(vo2_summary, aes(x = Timepoint, y = mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "Timepoint", y = y_axis_label, fill = "Measure") +
  scale_fill_grey() +  # sets the fill color to grayscale
  theme_classic() +
  geom_text(data = vo2_ttest, aes(x = "MID", y = mean, label = "*"), size = 5, vjust = -1, hjust = 0.5, position = position_dodge(0.9))



