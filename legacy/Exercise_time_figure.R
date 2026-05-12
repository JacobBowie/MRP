# Exercise Time Commitment Comparison Figure
# Compares weekly exercise time across training scenarios (Sedentary, Intermediate, Elite)
# against the MRP protocol, broken down by Resistance, Aerobic, and Transition time.
#
# Reference for Hadza data (commented out but available):
#   Raichlen DA, Pontzer H, Harris JA, et al. Physical activity patterns and biomarkers
#   of cardiovascular disease risk in hunter-gatherers. Am J Hum Biol. 2017;29:e22919.

library(tidyr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggsci)

# --- Resistance Training Scenarios ---
rt_scenarios <- tibble(
  Scenario = c("Sedentary", "Intermediate", "Elite"),
  n_exercises       = c(8, 9, 10),
  reps_per_exercise = c(8, 10, 12),
  sets_per_exercise = c(1, 3, 6),
  time_per_rep_s    = 4,
  rest_between_sets_s     = 120,
  rest_between_exercises_s = 60,
  sessions_per_week = c(2, 3, 6)
) %>%
  mutate(
    exercise_time_s = n_exercises * reps_per_exercise * sets_per_exercise * time_per_rep_s,
    rest_time_s     = n_exercises * (sets_per_exercise - 1) * rest_between_sets_s +
                      (n_exercises - 1) * rest_between_exercises_s,
    session_total_min = (exercise_time_s + rest_time_s) / 60,
    weekly_rt_min = session_total_min * sessions_per_week
  )

# --- Aerobic Training Scenarios ---
aerobic_scenarios <- tibble(
  Scenario = c("Sedentary", "Intermediate", "Elite"),
  min_per_session   = c(20, 30, 60),
  sessions_per_week = c(3, 5, 6)
) %>%
  mutate(weekly_aerobic_min = min_per_session * sessions_per_week)

# --- Transition Time (warmup, cooldown, changing, travel) ---
transition_scenarios <- tibble(
  Scenario = c("Sedentary", "Intermediate", "Elite"),
  weekly_transition_min = c(150, 240, 360)
)

# --- MRP Protocol ---
mrp_data <- tibble(
  Scenario = "MRP",
  weekly_rt_min = 20,
  weekly_aerobic_min = 15,
  weekly_transition_min = 30
)

# --- Combine ---
combined <- rt_scenarios %>%
  dplyr::select(Scenario, weekly_rt_min) %>%
  left_join(aerobic_scenarios %>% dplyr::select(Scenario, weekly_aerobic_min), by = "Scenario") %>%
  left_join(transition_scenarios, by = "Scenario") %>%
  bind_rows(mrp_data)

plot_data <- combined %>%
  pivot_longer(
    cols = c(weekly_rt_min, weekly_aerobic_min, weekly_transition_min),
    names_to = "component",
    values_to = "minutes"
  ) %>%
  mutate(
    component = case_when(
      component == "weekly_rt_min"         ~ "Resistance",
      component == "weekly_aerobic_min"    ~ "Aerobic",
      component == "weekly_transition_min" ~ "Transition"
    ),
    component = factor(component, levels = c("Resistance", "Aerobic", "Transition")),
    Scenario = factor(Scenario, levels = c("Elite", "Intermediate", "Sedentary", "MRP"))
  )

# --- Plot ---
npg_colors <- ggsci::pal_npg()(10)

exercise_time_plot <- ggplot(plot_data,
       aes(x = Scenario, y = minutes, fill = component)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c("Resistance" = npg_colors[1],
               "Aerobic"    = npg_colors[2],
               "Transition" = npg_colors[3]),
    name = "Component"
  ) +
  labs(
    x = NULL,
    y = "Total Time per Week (minutes)",
    title = "Weekly Exercise Time Commitment by Training Scenario"
  ) +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = c(0.75, 0.25),
    legend.background = element_rect(fill = "white", color = NA)
  )

print(exercise_time_plot)

# Save
ggsave("Exercise_time_comparison.png", exercise_time_plot,
       width = 8, height = 4.5, dpi = 300, bg = "white")

cat("Saved: Exercise_time_comparison.png\n")
cat("\nWeekly totals (minutes):\n")
combined %>%
  mutate(total = weekly_rt_min + weekly_aerobic_min + weekly_transition_min) %>%
  arrange(total) %>%
  print()
