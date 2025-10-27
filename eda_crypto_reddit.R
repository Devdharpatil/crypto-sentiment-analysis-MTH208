# Cryptocurrency Sentiment Analysis - Exploratory Data Analysis
# Simplified version for immediate execution with current dataset

# ============================================================================
# SECTION 1: SETUP AND DATA LOADING
# ============================================================================

# Clear workspace
rm(list = ls())

# Load required libraries
cat("📦 Loading required packages...\n")

required_packages <- c(
  "tidyverse", "lubridate", "ggplot2", "scales", 
  "gridExtra", "viridis", "corrplot", "knitr"
)

# Install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("✅ All packages loaded successfully!\n\n")

# Set theme for all plots
theme_set(theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  ))

# Color palette
crypto_colors <- c("#F7931A", "#627EEA", "#C2A633")
names(crypto_colors) <- c("bitcoin", "ethereum", "dogecoin")

# ============================================================================
# SECTION 2: DATA LOADING AND PREPROCESSING
# ============================================================================

cat("📂 Loading Reddit cryptocurrency data...\n")

# Load the CSV file
reddit_data <- read_csv("reddit_cc.csv", show_col_types = FALSE)

cat(paste("✅ Loaded", nrow(reddit_data), "rows and", ncol(reddit_data), "columns\n"))

# Display structure
cat("\n📊 Data Structure:\n")
str(reddit_data)

# Convert timestamp to proper datetime
reddit_data <- reddit_data %>%
  mutate(
    timestamp = as_datetime(timestamp),
    date = as.Date(timestamp),
    hour = hour(timestamp),
    day_of_week = wday(timestamp, label = TRUE),
    is_post = title != "Comment"
  )

# Create text content column (combine title and body)
reddit_data <- reddit_data %>%
  mutate(
    text_content = if_else(
      is_post,
      paste(title, body, sep = " "),
      body
    ),
    text_length = nchar(text_content)
  )

cat("\n✅ Data preprocessing complete!\n")

# ============================================================================
# SECTION 3: DATA QUALITY ASSESSMENT
# ============================================================================

cat("\n" , rep("=", 70), "\n", sep = "")
cat("DATA QUALITY REPORT\n")
cat(rep("=", 70), "\n", sep = "")

# Basic statistics
cat("\n1. DATASET OVERVIEW:\n")
cat("   Total records:", nrow(reddit_data), "\n")
cat("   Date range:", format(min(reddit_data$date), "%Y-%m-%d"), "to", 
    format(max(reddit_data$date), "%Y-%m-%d"), "\n")
cat("   Duration:", as.numeric(max(reddit_data$date) - min(reddit_data$date)), "days\n")

# Content type breakdown
content_summary <- reddit_data %>%
  group_by(is_post) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    type = if_else(is_post, "Posts", "Comments"),
    percentage = round(count / sum(count) * 100, 1)
  )

cat("\n2. CONTENT TYPE:\n")
for (i in 1:nrow(content_summary)) {
  cat("  ", content_summary$type[i], ":", content_summary$count[i], 
      "(", content_summary$percentage[i], "%)\n")
}

# Missing values
missing_summary <- reddit_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing") %>%
  mutate(percentage = round(missing / nrow(reddit_data) * 100, 2)) %>%
  filter(missing > 0)

cat("\n3. MISSING VALUES:\n")
if (nrow(missing_summary) > 0) {
  print(missing_summary)
} else {
  cat("   ✅ No missing values detected\n")
}

# Engagement statistics
cat("\n4. ENGAGEMENT METRICS:\n")
cat("   Average score:", round(mean(reddit_data$score, na.rm = TRUE), 2), "\n")
cat("   Median score:", median(reddit_data$score, na.rm = TRUE), "\n")
cat("   Max score:", max(reddit_data$score, na.rm = TRUE), "\n")
cat("   Posts with high engagement (score > 100):", 
    sum(reddit_data$score > 100, na.rm = TRUE), "\n")

# ============================================================================
# SECTION 4: CREATE OUTPUT DIRECTORIES
# ============================================================================

# Create directories for outputs
dir.create("plots", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

cat("\n✅ Output directories created\n")

# ============================================================================
# SECTION 5: TEMPORAL ANALYSIS
# ============================================================================

cat("\n📈 Performing temporal analysis...\n")

# Daily post volume
daily_activity <- reddit_data %>%
  group_by(date, is_post) %>%
  summarise(
    count = n(),
    avg_score = mean(score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(type = if_else(is_post, "Posts", "Comments"))

# Plot 1: Daily activity over time
p1 <- ggplot(daily_activity, aes(x = date, y = count, color = type, fill = type)) +
  geom_area(alpha = 0.3, position = "identity") +
  geom_line(size = 1) +
  scale_color_manual(values = c("Posts" = "#E74C3C", "Comments" = "#3498DB")) +
  scale_fill_manual(values = c("Posts" = "#E74C3C", "Comments" = "#3498DB")) +
  labs(
    title = "Daily Reddit Activity Over Time",
    subtitle = "Posts and Comments Volume",
    x = "Date",
    y = "Number of Items",
    color = "Type",
    fill = "Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Hourly posting patterns
hourly_pattern <- reddit_data %>%
  filter(is_post) %>%
  group_by(hour) %>%
  summarise(count = n(), .groups = "drop")

p2 <- ggplot(hourly_pattern, aes(x = hour, y = count)) +
  geom_bar(stat = "identity", fill = "#F39C12", alpha = 0.8) +
  geom_line(color = "#E67E22", size = 1.2) +
  labs(
    title = "Posting Activity by Hour of Day",
    subtitle = "When do users post most?",
    x = "Hour (24-hour format)",
    y = "Number of Posts"
  ) +
  scale_x_continuous(breaks = seq(0, 23, 2))

# Plot 3: Day of week patterns
dow_pattern <- reddit_data %>%
  filter(is_post) %>%
  group_by(day_of_week) %>%
  summarise(
    count = n(),
    avg_score = mean(score, na.rm = TRUE),
    .groups = "drop"
  )

p3 <- ggplot(dow_pattern, aes(x = day_of_week, y = count)) +
  geom_col(fill = "#9B59B6", alpha = 0.8) +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) +
  labs(
    title = "Posting Activity by Day of Week",
    subtitle = "Which days are most active?",
    x = "Day of Week",
    y = "Number of Posts"
  )

# Combine temporal plots
temporal_combined <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1)

# Save temporal analysis
ggsave("plots/temporal_analysis.png", temporal_combined, 
       width = 12, height = 14, dpi = 300)

cat("✅ Temporal analysis plots saved to plots/temporal_analysis.png\n")

# ============================================================================
# SECTION 6: ENGAGEMENT ANALYSIS
# ============================================================================

cat("\n💬 Analyzing engagement patterns...\n")

# Score distribution
p4 <- ggplot(reddit_data %>% filter(score > 0 & score < 500), 
             aes(x = score, fill = is_post)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  scale_fill_manual(
    values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"),
    labels = c("TRUE" = "Posts", "FALSE" = "Comments")
  ) +
  labs(
    title = "Distribution of Engagement Scores",
    subtitle = "Scores between 0 and 500",
    x = "Score",
    y = "Frequency",
    fill = "Type"
  ) +
  theme(legend.position = "bottom")

# Score vs comments (for posts only)
posts_only <- reddit_data %>% filter(is_post)

p5 <- ggplot(posts_only %>% filter(score < 1000 & comms_num < 200), 
             aes(x = score, y = comms_num)) +
  geom_point(alpha = 0.3, color = "#E74C3C") +
  geom_smooth(method = "lm", se = TRUE, color = "#2C3E50") +
  labs(
    title = "Relationship: Post Score vs Number of Comments",
    subtitle = "Do high-scoring posts get more comments?",
    x = "Post Score (Upvotes)",
    y = "Number of Comments"
  )

# Combine engagement plots
engagement_combined <- gridExtra::grid.arrange(p4, p5, ncol = 1)

ggsave("plots/engagement_analysis.png", engagement_combined, 
       width = 12, height = 10, dpi = 300)

cat("✅ Engagement analysis plots saved to plots/engagement_analysis.png\n")

# ============================================================================
# SECTION 7: CONTENT ANALYSIS
# ============================================================================

cat("\n📝 Analyzing content characteristics...\n")

# Text length distribution
p6 <- ggplot(reddit_data %>% filter(text_length > 0 & text_length < 1000), 
             aes(x = text_length, fill = is_post)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"),
    labels = c("TRUE" = "Posts", "FALSE" = "Comments")
  ) +
  labs(
    title = "Distribution of Text Length",
    subtitle = "Character count for posts and comments",
    x = "Text Length (characters)",
    y = "Density",
    fill = "Type"
  )

# Top posts by score
top_posts <- reddit_data %>%
  filter(is_post) %>%
  arrange(desc(score)) %>%
  head(10) %>%
  mutate(
    title_short = str_trunc(title, 60),
    rank = row_number()
  )

p7 <- ggplot(top_posts, aes(x = reorder(title_short, score), y = score)) +
  geom_col(fill = "#F39C12", alpha = 0.8) +
  geom_text(aes(label = score), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Posts by Score",
    subtitle = "Most upvoted posts in the dataset",
    x = NULL,
    y = "Score (Upvotes)"
  ) +
  theme(axis.text.y = element_text(size = 8))

# Combine content plots
content_combined <- gridExtra::grid.arrange(p6, p7, ncol = 1)

ggsave("plots/content_analysis.png", content_combined, 
       width = 12, height = 10, dpi = 300)

cat("✅ Content analysis plots saved to plots/content_analysis.png\n")

# ============================================================================
# SECTION 8: SUMMARY STATISTICS
# ============================================================================

cat("\n📊 Generating summary statistics...\n")

# Overall summary
summary_stats <- list(
  dataset_info = tibble(
    metric = c("Total Records", "Posts", "Comments", "Date Range", "Duration (days)"),
    value = c(
      format(nrow(reddit_data), big.mark = ","),
      format(sum(reddit_data$is_post), big.mark = ","),
      format(sum(!reddit_data$is_post), big.mark = ","),
      paste(format(min(reddit_data$date), "%Y-%m-%d"), "to", 
            format(max(reddit_data$date), "%Y-%m-%d")),
      as.character(as.numeric(max(reddit_data$date) - min(reddit_data$date)))
    )
  ),

  engagement_stats = tibble(
    metric = c("Mean Score", "Median Score", "Max Score", "High Engagement Posts (>100)"),
    value = c(
      format(round(mean(reddit_data$score, na.rm = TRUE), 2), nsmall = 2),
      as.character(median(reddit_data$score, na.rm = TRUE)),
      as.character(max(reddit_data$score, na.rm = TRUE)),
      format(sum(reddit_data$score > 100, na.rm = TRUE), big.mark = ",")
    )
  ),

  temporal_stats = tibble(
    metric = c("Avg Daily Posts", "Most Active Hour", "Most Active Day", "Weekend Posts %"),
    value = c(
      format(round(nrow(posts_only) / as.numeric(max(reddit_data$date) - min(reddit_data$date)), 1), nsmall = 1),
      as.character(hourly_pattern$hour[which.max(hourly_pattern$count)]),
      as.character(dow_pattern$day_of_week[which.max(dow_pattern$count)]),
      paste0(round(sum(reddit_data$day_of_week %in% c("Sat", "Sun")) / nrow(reddit_data) * 100, 1), "%")
    )
  )
)

# Print summary
cat("\n", rep("=", 70), "\n", sep = "")
cat("SUMMARY STATISTICS\n")
cat(rep("=", 70), "\n", sep = "")

cat("\nDataset Information:\n")
print(kable(summary_stats$dataset_info))

cat("\nEngagement Statistics:\n")
print(kable(summary_stats$engagement_stats))

cat("\nTemporal Patterns:\n")
print(kable(summary_stats$temporal_stats))

# Save summary statistics
saveRDS(summary_stats, "results/summary_statistics.rds")
write.csv(summary_stats$dataset_info, "results/dataset_info.csv", row.names = FALSE)
write.csv(summary_stats$engagement_stats, "results/engagement_stats.csv", row.names = FALSE)

cat("\n✅ Summary statistics saved to results/ directory\n")

# ============================================================================
# SECTION 9: SAVE PROCESSED DATA
# ============================================================================

cat("\n💾 Saving processed data...\n")

# Save cleaned dataset
write_csv(reddit_data, "data/reddit_processed.csv")

# Save analysis results
eda_results <- list(
  data = reddit_data,
  daily_activity = daily_activity,
  hourly_pattern = hourly_pattern,
  dow_pattern = dow_pattern,
  top_posts = top_posts,
  summary_stats = summary_stats
)

saveRDS(eda_results, "results/eda_results.rds")

cat("✅ Processed data saved to data/reddit_processed.csv\n")
cat("✅ EDA results saved to results/eda_results.rds\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("🎉 EXPLORATORY DATA ANALYSIS COMPLETE!\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n📁 Generated Files:\n")
cat("   Plots:\n")
cat("      - plots/temporal_analysis.png\n")
cat("      - plots/engagement_analysis.png\n")
cat("      - plots/content_analysis.png\n")
cat("\n   Results:\n")
cat("      - results/eda_results.rds\n")
cat("      - results/summary_statistics.rds\n")
cat("      - results/dataset_info.csv\n")
cat("      - results/engagement_stats.csv\n")
cat("\n   Data:\n")
cat("      - data/reddit_processed.csv\n")

cat("\n🎯 Key Findings:\n")
cat("   • Dataset contains", nrow(reddit_data), "records over", 
    as.numeric(max(reddit_data$date) - min(reddit_data$date)), "days\n")
cat("   • Average", round(nrow(posts_only) / as.numeric(max(reddit_data$date) - min(reddit_data$date)), 1), 
    "posts per day\n")
cat("   • Peak posting hour:", hourly_pattern$hour[which.max(hourly_pattern$count)], ":00\n")
cat("   • Most active day:", as.character(dow_pattern$day_of_week[which.max(dow_pattern$count)]), "\n")
cat("   • Top post score:", max(reddit_data$score, na.rm = TRUE), "\n")

cat("\n✅ Ready for next steps:\n")
cat("   1. Sentiment analysis\n")
cat("   2. Price data integration\n")
cat("   3. Correlation analysis\n")
cat("   4. Shiny app development\n")

cat("\n", rep("=", 70), "\n", sep = "")
cat("EDA script completed successfully!\n")
cat(rep("=", 70), "\n", sep = "")
