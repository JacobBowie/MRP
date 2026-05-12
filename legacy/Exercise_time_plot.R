# Load necessary library
# Load necessary libraries
library(tidyr)
library(ggplot2)
library(stringr)
library(ggpubr)
library(dplyr)

# Define the scenarios in a data frame
scenarios <- data.frame(
  Scenario = c("Minimal", "Intermediate", "Maximal"),
  Number_of_Exercises = c(8, 9, 10),
  Repetitions_per_Exercise = c(8, 10, 12),
  Sets_per_Exercise = c(1, 3, 6),  # Assuming 3 sets for Intermediate
  Time_per_Repetition_seconds = rep(4, 3),
  Rest_between_Sets_seconds = rep(120, 3),
  Rest_between_Exercises_seconds = rep(60, 3),  # 1 minute rest between exercises
  Frequency_per_Week = c(2, 3, 6)  # Assuming 3 days per week for Intermediate
)

# Compute the times based on the entries in the data frame
exercise_times <- scenarios %>%
  mutate(
    Time_Exercising_per_Session_seconds = Number_of_Exercises * Repetitions_per_Exercise * Sets_per_Exercise * Time_per_Repetition_seconds,
    Rest_Time_per_Session_seconds = (Number_of_Exercises * (Sets_per_Exercise - 1) * Rest_between_Sets_seconds) + 
      ((Number_of_Exercises - 1) * Rest_between_Exercises_seconds),
    Total_Time_per_Session_seconds = Time_Exercising_per_Session_seconds + Rest_Time_per_Session_seconds,
    # Convert seconds to minutes
    Time_Exercising_per_Session_minutes = Time_Exercising_per_Session_seconds / 60,
    Rest_Time_per_Session_minutes = Rest_Time_per_Session_seconds / 60,
    Total_Time_per_Session_minutes = Total_Time_per_Session_seconds / 60,
    Total_Time_per_Week_minutes_RT = Total_Time_per_Session_minutes * Frequency_per_Week
  )

# Print the computed exercise times
print(exercise_times)

# Define the aerobic training scenarios in a data frame
aerobic_scenarios <- data.frame(
  Scenario = c("Minimal", "Intermediate", "Maximal"),
  Minutes_per_Session = c(20, 30, 60),  # Assuming 45 minutes for Intermediate
  Frequency_per_Week = c(3, 5, 6)  # Assuming 5 days per week for Intermediate
)

# Compute the total time spent on aerobic training per week for each scenario
aerobic_times <- aerobic_scenarios %>%
  mutate(
    Total_Time_per_Week_minutes_AT = Minutes_per_Session * Frequency_per_Week
  )

# Rename the Frequency_per_Week column to make it unique
names(exercise_times)[names(exercise_times) == "Frequency_per_Week"] <- "Frequency_per_Week_RT"
names(aerobic_times)[names(aerobic_times) == "Frequency_per_Week"] <- "Frequency_per_Week_AT"

# Merge the aerobic and resistance training data frames for easier plotting
# Assume the rows correspond to the same individuals across scenarios
merged_data <- cbind(exercise_times, aerobic_times[, -1])  # Exclude the Scenario column from aerobic_times

# Prepare data for plotting
plot_data <- merged_data %>%
  pivot_longer(cols = starts_with("Total_Time"), names_to = "Exercise_Type", values_to = "Time_per_Week_minutes")

# Create the horizontal bar plot
ggplot(plot_data, aes(x = factor(Scenario, levels = c("Maximal", "Intermediate", "Minimal")), 
                      y = Time_per_Week_minutes, fill = Exercise_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    x = "Scenario",
    y = "Total Time per Week (minutes)",
    fill = "Exercise Type"
  ) +
  theme_minimal()

# Merge the aerobic and resistance training data frames for easier plotting
# Assume the rows correspond to the same individuals across scenarios
merged_data <- cbind(exercise_times, aerobic_times[, -1])  # Exclude the Scenario column from aerobic_times

# Prepare data for plotting
plot_data <- merged_data %>%
  pivot_longer(cols = starts_with("Total_Time"), names_to = "Exercise_Type", values_to = "Time_per_Week_minutes")

# Check column names
print(names(plot_data))

# Select only the necessary columns for the plot
plot_data_simplified <- plot_data %>% 
  dplyr::select(Scenario, Exercise_Type, Time_per_Week_minutes)


# View the simplified data frame
print(plot_data_simplified)

# Rename the entries in the Exercise_Type column
plot_data_renamed <- plot_data_simplified %>%
  mutate(
    Exercise_Type = str_replace(Exercise_Type, "RT", "Resistance"),
    Exercise_Type = str_replace(Exercise_Type, "AT", "Aerobic")
  )

# Remove entries not needed for plotting
plot_data_renamed <- plot_data_renamed %>%
  filter(str_detect(Exercise_Type, "Resistance|Aerobic"))

# View the renamed data frame
print(plot_data_renamed)
# Remove the Total_Time_ prefix from the Exercise_Type column
plot_data_final <- plot_data_renamed %>%
  mutate(
    Exercise_Type = str_replace(Exercise_Type, "Total_Time_per_Week_minutes_", "")
  )

# View the final data frame
print(plot_data_final)
# Rename Minimal to Sedentary and Maximal to Elite
plot_data_final <- plot_data_final %>%
  mutate(
    Scenario = case_when(
      Scenario == "Minimal" ~ "Sedentary",
      Scenario == "Maximal" ~ "Elite",
      TRUE ~ Scenario  # leaves other scenarios unchanged
    )
  )

# Create a new data frame for the MRP scenario
mrp_data <- data.frame(
  Scenario = c("MRP", "MRP", "Hadza", "Hadza"),
  Exercise_Type = c("Resistance", "Aerobic", "Resistance", "Aerobic"),
  Time_per_Week_minutes = c(20, 15, 0, 945)
)
# Create a new data frame for the MRP scenario
mrp_data <- data.frame(
  Scenario = c("MRP", "MRP"),
  Exercise_Type = c("Resistance", "Aerobic"),
  Time_per_Week_minutes = c(20, 15)
)

# Create a new data frame for the MRP scenario
TT_data <- data.frame(
  Scenario = c("MRP", "Sedentary", "Intermediate", "Elite"),
  Exercise_Type = "Transition",
  Time_per_Week_minutes = c(30, 150, 240, 360)
)

#Raichlen, DA, Pontzer, H, Harris, JA, et al. Physical activity patterns and biomarkers of cardiovascular disease risk in hunter-gatherers. Am J Hum Biol. 2017; 29:e22919. https://doi.org/10.1002/ajhb.22919
# Add the MRP scenario to the final data frame
plot_data_final <- bind_rows(plot_data_final, mrp_data, TT_data)
# View the updated data frame
print(plot_data_final)

# Create the cumulative horizontal bar plot
ggplot(plot_data_final, aes(x = factor(Scenario, levels = c("Hadza", "MRP","Sedentary", "Intermediate", "Elite")), 
                      y = Time_per_Week_minutes, fill = Exercise_Type)) +
  geom_bar(stat = "identity", position = "stack") +  # Use position = "stack" for cumulative bar plot
  coord_flip() +
  labs(
    x = "",
    y = "Total Time per Week (minutes)",
    fill = "Exercise Type"
  ) +
  theme_minimal()

# Create the cumulative horizontal bar plot with reversed order of levels
ggplot(plot_data_final, aes(x = factor(Scenario, levels = c("Hadza", "Elite", "Intermediate", "Sedentary", "MRP")), 
                            y = Time_per_Week_minutes, fill = Exercise_Type)) +
  geom_bar(stat = "identity", position = "stack") +  # Use position = "stack" for cumulative bar plot
  coord_flip() +
  labs(
    x = "",
    y = "Total Time per Week (minutes)",
    fill = "Exercise Type"
  ) +
  theme_bw()

# Create the cumulative horizontal bar plot with reversed order of levels
ggplot(plot_data_final, aes(x = factor(Scenario, levels = c( "Elite", "Intermediate", "Sedentary", "MRP")), 
                            y = Time_per_Week_minutes, fill = Exercise_Type)) +
  geom_bar(stat = "identity", position = "stack") +  # Use position = "stack" for cumulative bar plot
  coord_flip() +
  labs(
    x = "",
    y = "Total Time per Week (minutes)",
    fill = "Exercise Type"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = c(0.75, 0.75)  # Adjust the x-value to 0.7, keeping y-value at 0.5 (or adjust as needed)
  )
# Define color values
dark_blue_color <- "#00205B"   # Dark blue color
light_blue_color <- "#6699CC"  # Light blue color
third_color <- "#A0C9E0"       # Third color

# Create your plot
my_plot <- ggplot(plot_data_final, aes(x = factor(Scenario, levels = c( "Elite", "Intermediate", "Sedentary", "MRP")), 
                                       y = Time_per_Week_minutes, fill = Exercise_Type)) +
  geom_bar(stat = "identity", position = "stack") +  # Use position = "stack" for cumulative bar plot
  coord_flip() +
  labs(
    x = "",
    y = "Total Time per Week (minutes)",
    fill = "Exercise Type"
  ) +
  theme_bw(base_size = 18) +  # Set the base size to 18
  scale_fill_manual(values = c(dark_blue_color,light_blue_color, third_color)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  #  axis.line = element_line(color = "black"),
    legend.position = c(0.75, 0.75),  # Adjust the x-value to 0.7, keeping y-value at 0.5 (or adjust as needed)
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.text = element_text(color = "black")
  )
# Save your plot as a PNG file with a transparent background
ggsave(filename = "my_plot.png", plot = my_plot, width = 6, height = 4, units = "in", dpi = 300, bg = "transparent")

