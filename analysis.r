# Memory Recall Experiment Data Analysis
# This script analyzes data from the memory recall experiment with music conditions

# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(ggplot2)    # For creating plots
library(dplyr)      # For data manipulation
library(car)        # For ANOVA and additional statistical tests
library(corrplot)   # For correlation visualization
library(RColorBrewer) # For color palettes

# Set a seed for reproducibility
set.seed(123)

# Load the data
# Replace 'memory_recall_data.csv' with your actual file path
# If you're using RStudio, you can also use the file browser to import the data
data <- read.csv("memory_recall_data.csv", header = TRUE)

# Data preprocessing
# Convert variables to appropriate types
data$participantId <- as.factor(data$participantId)
data$musicCondition <- as.factor(data$musicCondition)
data$level <- as.factor(data$level)

# Check data structure
str(data)
summary(data)

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print("Missing values by column:")
print(missing_values)

# Remove rows with missing values if necessary
if(sum(missing_values) > 0) {
  data <- na.omit(data)
  print("Rows with missing values removed.")
}

# Calculate average accuracy and response time per participant per music condition
participant_summary <- data %>%
  group_by(participantId, musicCondition, musicalTraining) %>%
  summarize(
    avg_response_time = mean(responseTime),
    avg_accuracy = mean(accuracy),
    trials = n(),
    .groups = 'drop'
  )

# Print summary statistics
print("Summary statistics by music condition:")
condition_summary <- participant_summary %>%
  group_by(musicCondition) %>%
  summarize(
    mean_response_time = mean(avg_response_time),
    sd_response_time = sd(avg_response_time),
    mean_accuracy = mean(avg_accuracy),
    sd_accuracy = sd(avg_accuracy),
    n = n(),
    .groups = 'drop'
  )
print(condition_summary)

# ------------------------------------------------------------
# Visualizations
# ------------------------------------------------------------

# Create a directory for saving plots if it doesn't exist
if(!dir.exists("plots")) {
  dir.create("plots")
}

# 1. Histograms of response times by music condition
hist_response_time <- ggplot(participant_summary, aes(x = avg_response_time, fill = musicCondition)) +
  geom_histogram(alpha = 0.7, bins = 20, position = "identity") +
  facet_wrap(~ musicCondition) +
  labs(
    title = "Distribution of Response Times by Music Condition",
    x = "Average Response Time (seconds)",
    y = "Count",
    fill = "Music Condition"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

print(hist_response_time)
ggsave("plots/hist_response_time.png", hist_response_time, width = 10, height = 6)

# 2. Boxplots of response times by music condition
box_response_time <- ggplot(participant_summary, aes(x = musicCondition, y = avg_response_time, fill = musicCondition)) +
  geom_boxplot() +
  labs(
    title = "Response Time by Music Condition",
    x = "Music Condition",
    y = "Average Response Time (seconds)",
    fill = "Music Condition"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

print(box_response_time)
ggsave("plots/box_response_time.png", box_response_time, width = 8, height = 6)

# 3. Boxplots of accuracy by music condition
box_accuracy <- ggplot(participant_summary, aes(x = musicCondition, y = avg_accuracy, fill = musicCondition)) +
  geom_boxplot() +
  labs(
    title = "Recall Accuracy by Music Condition",
    x = "Music Condition",
    y = "Average Accuracy",
    fill = "Music Condition"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

print(box_accuracy)
ggsave("plots/box_accuracy.png", box_accuracy, width = 8, height = 6)

# 4. Violin plots to better visualize the distribution
violin_response_time <- ggplot(participant_summary, aes(x = musicCondition, y = avg_response_time, fill = musicCondition)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(
    title = "Response Time Distribution by Music Condition",
    x = "Music Condition",
    y = "Average Response Time (seconds)",
    fill = "Music Condition"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

print(violin_response_time)
ggsave("plots/violin_response_time.png", violin_response_time, width = 8, height = 6)

# 5. Scatter plot of response time vs. accuracy colored by music condition
scatter_rt_acc <- ggplot(participant_summary, aes(x = avg_response_time, y = avg_accuracy, color = musicCondition)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Response Time vs. Accuracy by Music Condition",
    x = "Average Response Time (seconds)",
    y = "Average Accuracy",
    color = "Music Condition"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

print(scatter_rt_acc)
ggsave("plots/scatter_rt_acc.png", scatter_rt_acc, width = 8, height = 6)

# 6. Bar chart of accuracy by music condition
bar_accuracy <- ggplot(condition_summary, aes(x = musicCondition, y = mean_accuracy, fill = musicCondition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_accuracy - sd_accuracy, ymax = mean_accuracy + sd_accuracy), width = 0.2) +
  labs(
    title = "Mean Accuracy by Music Condition",
    x = "Music Condition",
    y = "Mean Accuracy",
    fill = "Music Condition"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

print(bar_accuracy)
ggsave("plots/bar_accuracy.png", bar_accuracy, width = 8, height = 6)

# 7. Scatter plot of musical training vs. response time
scatter_training_rt <- ggplot(participant_summary, aes(x = musicalTraining, y = avg_response_time, color = musicCondition)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
  labs(
    title = "Effect of Musical Training on Response Time by Music Condition",
    x = "Musical Training (years)",
    y = "Average Response Time (seconds)",
    color = "Music Condition"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

print(scatter_training_rt)
ggsave("plots/scatter_training_rt.png", scatter_training_rt, width = 8, height = 6)

# ------------------------------------------------------------
# Statistical Analysis
# ------------------------------------------------------------

# 1. One-way ANOVA for response time
rt_anova <- aov(avg_response_time ~ musicCondition, data = participant_summary)
print("ANOVA for Response Time:")
print(summary(rt_anova))

# Post-hoc test if ANOVA is significant
if(summary(rt_anova)[[1]]$"Pr(>F)"[1] < 0.05) {
  print("Post-hoc Tukey HSD test for Response Time:")
  print(TukeyHSD(rt_anova))
}

# 2. One-way ANOVA for accuracy
acc_anova <- aov(avg_accuracy ~ musicCondition, data = participant_summary)
print("ANOVA for Accuracy:")
print(summary(acc_anova))

# Post-hoc test if ANOVA is significant
if(summary(acc_anova)[[1]]$"Pr(>F)"[1] < 0.05) {
  print("Post-hoc Tukey HSD test for Accuracy:")
  print(TukeyHSD(acc_anova))
}

# 3. Correlation analysis
# Create a correlation matrix for numerical variables
corr_vars <- participant_summary %>%
  select(musicalTraining, avg_response_time, avg_accuracy)

corr_matrix <- cor(corr_vars, use = "complete.obs")
print("Correlation Matrix:")
print(corr_matrix)

# Visualize the correlation matrix
png("plots/correlation_matrix.png", width = 800, height = 800)
corrplot(corr_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
         title = "Correlation Matrix")
dev.off()

# 4. Linear regression for response time
rt_model <- lm(avg_response_time ~ musicCondition + musicalTraining, data = participant_summary)
print("Linear Regression for Response Time:")
print(summary(rt_model))

# 5. Linear regression for accuracy
acc_model <- lm(avg_accuracy ~ musicCondition + musicalTraining, data = participant_summary)
print("Linear Regression for Accuracy:")
print(summary(acc_model))

# 6. Consider musical training as an interaction effect
rt_interaction_model <- lm(avg_response_time ~ musicCondition * musicalTraining, data = participant_summary)
print("Linear Regression with Interaction for Response Time:")
print(summary(rt_interaction_model))

acc_interaction_model <- lm(avg_accuracy ~ musicCondition * musicalTraining, data = participant_summary)
print("Linear Regression with Interaction for Accuracy:")
print(summary(acc_interaction_model))

# 7. ANCOVA analysis with musical training as covariate
rt_ancova <- aov(avg_response_time ~ musicCondition + musicalTraining, data = participant_summary)
print("ANCOVA for Response Time with Musical Training as Covariate:")
print(summary(rt_ancova))

acc_ancova <- aov(avg_accuracy ~ musicCondition + musicalTraining, data = participant_summary)
print("ANCOVA for Accuracy with Musical Training as Covariate:")
print(summary(acc_ancova))

# ------------------------------------------------------------
# Summary Report
# ------------------------------------------------------------

# Create a function to generate a summary report
generate_report <- function() {
  # Capture output to a file
  sink("memory_recall_experiment_results.txt")
  
  cat("==========================================================\n")
  cat("MEMORY RECALL EXPERIMENT: DATA ANALYSIS\n")
  cat("==========================================================\n\n")
  
  cat("STUDY OVERVIEW\n")
  cat("This study examined the effect of background music on memory recall performance.\n")
  cat("Music conditions: None, Familiar, and Unfamiliar\n\n")
  
  cat("DESCRIPTIVE STATISTICS\n")
  print(condition_summary)
  cat("\n")
  
  cat("ANOVA RESULTS\n")
  cat("Response Time ANOVA:\n")
  print(summary(rt_anova))
  cat("\n")
  
  cat("Accuracy ANOVA:\n")
  print(summary(acc_anova))
  cat("\n")
  
  cat("REGRESSION ANALYSIS\n")
  cat("Effect of music condition and musical training on response time:\n")
  print(summary(rt_model)$coefficients)
  cat("\n")
  
  cat("Effect of music condition and musical training on accuracy:\n")
  print(summary(acc_model)$coefficients)
  cat("\n")
  
  cat("CORRELATION ANALYSIS\n")
  print(corr_matrix)
  cat("\n")
  
  cat("CONCLUSIONS\n")
  
  # Automatically generate simple conclusions based on the results
  rt_pval <- summary(rt_anova)[[1]]$"Pr(>F)"[1]
  acc_pval <- summary(acc_anova)[[1]]$"Pr(>F)"[1]
  
  if(rt_pval < 0.05) {
    cat("- Background music significantly affected response time (p < ", 
        round(rt_pval, 3), ").\n", sep="")
  } else {
    cat("- Background music did not significantly affect response time (p = ", 
        round(rt_pval, 3), ").\n", sep="")
  }
  
  if(acc_pval < 0.05) {
    cat("- Background music significantly affected recall accuracy (p < ", 
        round(acc_pval, 3), ").\n", sep="")
  } else {
    cat("- Background music did not significantly affect recall accuracy (p = ", 
        round(acc_pval, 3), ").\n", sep="")
  }
  
  # Check effect of musical training
  rt_training_pval <- summary(rt_model)$coefficients["musicalTraining", "Pr(>|t|)"]
  acc_training_pval <- summary(acc_model)$coefficients["musicalTraining", "Pr(>|t|)"]
  
  if(rt_training_pval < 0.05) {
    cat("- Musical training significantly predicted response time (p < ", 
        round(rt_training_pval, 3), ").\n", sep="")
  } else {
    cat("- Musical training did not significantly predict response time (p = ", 
        round(rt_training_pval, 3), ").\n", sep="")
  }
  
  if(acc_training_pval < 0.05) {
    cat("- Musical training significantly predicted recall accuracy (p < ", 
        round(acc_training_pval, 3), ").\n", sep="")
  } else {
    cat("- Musical training did not significantly predict recall accuracy (p = ", 
        round(acc_training_pval, 3), ").\n", sep="")
  }
  
  cat("\n==========================================================\n")
  cat("END OF REPORT\n")
  cat("==========================================================\n")
  
  # Close the output file
  sink()
}

# Execute the report generation
generate_report()

# Save the participant summary to CSV for future reference
write.csv(participant_summary, "participant_summary.csv", row.names = FALSE)
write.csv(condition_summary, "condition_summary.csv", row.names = FALSE)

# Print final message
cat("\nAnalysis complete. Results saved to memory_recall_experiment_results.txt\n")
cat("Summary data saved to participant_summary.csv and condition_summary.csv\n")
cat("Plots saved to the 'plots' directory\n")