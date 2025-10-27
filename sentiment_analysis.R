# Cryptocurrency Sentiment Analysis in R
# Complete sentiment analysis using syuzhet and sentimentr packages

# ============================================================================
# SECTION 1: LOAD REQUIRED PACKAGES
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("🧠 CRYPTOCURRENCY SENTIMENT ANALYSIS (R)\n")
cat(rep("=", 70), "\n", sep = "")

# List of required packages
required_packages <- c(
  "tidyverse",    # Data manipulation
  "lubridate",    # Date handling
  "syuzhet",      # Sentiment analysis (NRC, Bing, AFINN)
  "sentimentr",   # Sentence-level sentiment
  "textdata",     # Sentiment lexicons
  "tidytext",     # Text mining
  "tm",           # Text preprocessing
  "SnowballC",    # Text stemming
  "wordcloud",    # Word clouds (bonus visualization)
  "RColorBrewer"  # Color palettes
)

cat("\n📦 Installing and loading required packages...\n")

# Function to install and load packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    cat(paste("   Installing", package, "...\n"))
    install.packages(package, dependencies = TRUE, repos = "https://cloud.r-project.org/")
    library(package, character.only = TRUE)
  }
}

# Install and load all packages
invisible(lapply(required_packages, install_and_load))

cat("✅ All packages loaded successfully!\n")

# ============================================================================
# SECTION 2: LOAD AND PREPARE DATA
# ============================================================================

cat("\n📂 Loading Reddit cryptocurrency data...\n")

# Load processed data from EDA
reddit_data <- read_csv("data/reddit_processed.csv", show_col_types = FALSE)

cat(paste("✅ Loaded", format(nrow(reddit_data), big.mark = ","), "records\n"))

# Convert timestamp and create date column
reddit_data <- reddit_data %>%
  mutate(
    timestamp = as_datetime(timestamp),
    date = as.Date(timestamp)
  )

cat(paste("📅 Date range:", min(reddit_data$date), "to", max(reddit_data$date), "\n"))

# Prepare text for analysis
reddit_data <- reddit_data %>%
  mutate(
    # Combine title and body for complete text
    full_text = paste(title, body, sep = " "),
    # Clean text: remove URLs, special characters
    clean_text = str_replace_all(full_text, "http\\S+|www\\.\\S+", ""),
    clean_text = str_replace_all(clean_text, "[^[:alnum:][:space:]]", " "),
    clean_text = str_squish(clean_text),
    # Calculate text length
    text_length = nchar(clean_text)
  ) %>%
  # Filter out very short texts
  filter(text_length > 10)

cat(paste("✅ Prepared", format(nrow(reddit_data), big.mark = ","), 
          "records with meaningful text\n"))

# ============================================================================
# SECTION 3: SENTIMENT ANALYSIS - NRC EMOTION LEXICON
# ============================================================================

cat("\n🎭 Running NRC Emotion Analysis...\n")
cat("   (Detecting: joy, sadness, fear, anger, trust, etc.)\n")

# Function to get NRC sentiment safely
get_nrc_sentiment_safe <- function(text_vector, sample_size = 5000) {
  # Sample if dataset is large to speed up processing
  if (length(text_vector) > sample_size) {
    cat("   Processing in batches for efficiency...\n")
    indices <- seq_along(text_vector)
    batch_size <- 1000

    all_sentiments <- list()

    for (i in seq(1, length(text_vector), by = batch_size)) {
      end_idx <- min(i + batch_size - 1, length(text_vector))
      batch <- text_vector[i:end_idx]

      tryCatch({
        batch_sentiment <- get_nrc_sentiment(batch)
        all_sentiments[[length(all_sentiments) + 1]] <- batch_sentiment

        if (i %% 5000 == 0) {
          progress <- round(i / length(text_vector) * 100, 1)
          cat(paste("      Progress:", progress, "%\n"))
        }
      }, error = function(e) {
        # If batch fails, create empty result
        empty_result <- data.frame(
          anger = rep(0, length(batch)),
          anticipation = rep(0, length(batch)),
          disgust = rep(0, length(batch)),
          fear = rep(0, length(batch)),
          joy = rep(0, length(batch)),
          sadness = rep(0, length(batch)),
          surprise = rep(0, length(batch)),
          trust = rep(0, length(batch)),
          negative = rep(0, length(batch)),
          positive = rep(0, length(batch))
        )
        all_sentiments[[length(all_sentiments) + 1]] <- empty_result
      })
    }

    return(bind_rows(all_sentiments))
  } else {
    return(get_nrc_sentiment(text_vector))
  }
}

# Get NRC sentiment for all texts
nrc_sentiment <- get_nrc_sentiment_safe(reddit_data$clean_text)

# Add NRC scores to dataset
reddit_data <- bind_cols(reddit_data, nrc_sentiment)

cat("✅ NRC emotion analysis complete!\n")

# ============================================================================
# SECTION 4: SENTIMENT ANALYSIS - AFINN LEXICON
# ============================================================================

cat("\n💭 Running AFINN Sentiment Analysis...\n")
cat("   (Sentiment score: -5 to +5)\n")

# Get AFINN sentiment scores
afinn_scores <- get_sentiment(reddit_data$clean_text, method = "afinn")
reddit_data$afinn_sentiment <- afinn_scores

cat("✅ AFINN sentiment analysis complete!\n")

# ============================================================================
# SECTION 5: SENTIMENT ANALYSIS - BING LEXICON
# ============================================================================

cat("\n😊 Running Bing Sentiment Analysis...\n")
cat("   (Binary: positive/negative)\n")

# Get Bing sentiment scores
bing_scores <- get_sentiment(reddit_data$clean_text, method = "bing")
reddit_data$bing_sentiment <- bing_scores

cat("✅ Bing sentiment analysis complete!\n")

# ============================================================================
# SECTION 6: COMBINED SENTIMENT SCORE
# ============================================================================

cat("\n🔮 Creating combined sentiment scores...\n")

# Normalize NRC scores (positive - negative)
reddit_data$nrc_sentiment <- reddit_data$positive - reddit_data$negative

# Create combined sentiment (average of all methods)
reddit_data <- reddit_data %>%
  mutate(
    # Normalize scores to -1 to 1 range
    afinn_normalized = afinn_sentiment / 5,  # AFINN is -5 to 5
    bing_normalized = bing_sentiment / max(abs(bing_sentiment), na.rm = TRUE),
    nrc_normalized = nrc_sentiment / max(abs(nrc_sentiment), na.rm = TRUE),

    # Combined sentiment (average of normalized scores)
    combined_sentiment = (afinn_normalized + bing_normalized + nrc_normalized) / 3,

    # Classify sentiment
    sentiment_label = case_when(
      combined_sentiment > 0.1 ~ "positive",
      combined_sentiment < -0.1 ~ "negative",
      TRUE ~ "neutral"
    ),

    # Score-based proxy (using Reddit engagement)
    score_normalized = score / max(score, na.rm = TRUE)
  )

cat("✅ Combined sentiment scores created!\n")

# ============================================================================
# SECTION 7: SENTIMENT STATISTICS
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 SENTIMENT ANALYSIS RESULTS\n")
cat(rep("=", 70), "\n", sep = "")

# Sentiment distribution
sentiment_counts <- reddit_data %>%
  count(sentiment_label) %>%
  mutate(percentage = n / sum(n) * 100)

cat("\n1. SENTIMENT DISTRIBUTION:\n")
for (i in 1:nrow(sentiment_counts)) {
  cat(sprintf("   %s: %s (%.1f%%)\n", 
              str_to_title(sentiment_counts$sentiment_label[i]),
              format(sentiment_counts$n[i], big.mark = ","),
              sentiment_counts$percentage[i]))
}

# Overall sentiment statistics
cat("\n2. COMBINED SENTIMENT SCORES:\n")
cat(sprintf("   Mean: %.3f\n", mean(reddit_data$combined_sentiment, na.rm = TRUE)))
cat(sprintf("   Median: %.3f\n", median(reddit_data$combined_sentiment, na.rm = TRUE)))
cat(sprintf("   Std Dev: %.3f\n", sd(reddit_data$combined_sentiment, na.rm = TRUE)))
cat(sprintf("   Min: %.3f\n", min(reddit_data$combined_sentiment, na.rm = TRUE)))
cat(sprintf("   Max: %.3f\n", max(reddit_data$combined_sentiment, na.rm = TRUE)))

# NRC emotion distribution
cat("\n3. TOP NRC EMOTIONS:\n")
emotion_totals <- reddit_data %>%
  summarise(
    Joy = sum(joy, na.rm = TRUE),
    Trust = sum(trust, na.rm = TRUE),
    Anticipation = sum(anticipation, na.rm = TRUE),
    Fear = sum(fear, na.rm = TRUE),
    Sadness = sum(sadness, na.rm = TRUE),
    Anger = sum(anger, na.rm = TRUE),
    Surprise = sum(surprise, na.rm = TRUE),
    Disgust = sum(disgust, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "emotion", values_to = "count") %>%
  arrange(desc(count))

for (i in 1:min(5, nrow(emotion_totals))) {
  cat(sprintf("   %s: %s\n", 
              emotion_totals$emotion[i],
              format(emotion_totals$count[i], big.mark = ",")))
}

# ============================================================================
# SECTION 8: DAILY SENTIMENT AGGREGATION
# ============================================================================

cat("\n📅 Aggregating daily sentiment metrics...\n")

daily_sentiment <- reddit_data %>%
  group_by(date) %>%
  summarise(
    # Count metrics
    total_posts = n(),
    num_posts = sum(is_post, na.rm = TRUE),
    num_comments = sum(!is_post, na.rm = TRUE),

    # Sentiment metrics
    sentiment_mean = mean(combined_sentiment, na.rm = TRUE),
    sentiment_median = median(combined_sentiment, na.rm = TRUE),
    sentiment_sd = sd(combined_sentiment, na.rm = TRUE),

    # Individual method scores
    afinn_mean = mean(afinn_sentiment, na.rm = TRUE),
    bing_mean = mean(bing_sentiment, na.rm = TRUE),
    nrc_mean = mean(nrc_sentiment, na.rm = TRUE),

    # Engagement metrics
    avg_score = mean(score, na.rm = TRUE),
    total_score = sum(score, na.rm = TRUE),

    # Emotion metrics
    joy_total = sum(joy, na.rm = TRUE),
    fear_total = sum(fear, na.rm = TRUE),
    anger_total = sum(anger, na.rm = TRUE),
    trust_total = sum(trust, na.rm = TRUE),

    # Sentiment distribution
    positive_count = sum(sentiment_label == "positive", na.rm = TRUE),
    negative_count = sum(sentiment_label == "negative", na.rm = TRUE),
    neutral_count = sum(sentiment_label == "neutral", na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    # Calculate sentiment momentum (change from previous day)
    sentiment_momentum = sentiment_mean - lag(sentiment_mean),
    # Positive/negative ratio
    pos_neg_ratio = positive_count / pmax(negative_count, 1)
  )

cat(sprintf("✅ Daily sentiment aggregated for %d days\n", nrow(daily_sentiment)))

# ============================================================================
# SECTION 9: TOP POSITIVE AND NEGATIVE POSTS
# ============================================================================

cat("\n🏆 Identifying top posts by sentiment...\n")

# Top positive posts
top_positive <- reddit_data %>%
  filter(is_post) %>%
  arrange(desc(combined_sentiment)) %>%
  head(10) %>%
  select(title, combined_sentiment, score, date, joy, trust)

# Top negative posts
top_negative <- reddit_data %>%
  filter(is_post) %>%
  arrange(combined_sentiment) %>%
  head(10) %>%
  select(title, combined_sentiment, score, date, fear, anger, sadness)

cat("\n📈 TOP 5 MOST POSITIVE POSTS:\n")
for (i in 1:min(5, nrow(top_positive))) {
  title_short <- str_trunc(top_positive$title[i], 60)
  cat(sprintf("   • %s\n", title_short))
  cat(sprintf("     Sentiment: %.3f, Score: %d, Joy: %d\n", 
              top_positive$combined_sentiment[i],
              top_positive$score[i],
              top_positive$joy[i]))
}

cat("\n📉 TOP 5 MOST NEGATIVE POSTS:\n")
for (i in 1:min(5, nrow(top_negative))) {
  title_short <- str_trunc(top_negative$title[i], 60)
  cat(sprintf("   • %s\n", title_short))
  cat(sprintf("     Sentiment: %.3f, Score: %d, Fear: %d, Anger: %d\n",
              top_negative$combined_sentiment[i],
              top_negative$score[i],
              top_negative$fear[i],
              top_negative$anger[i]))
}

# ============================================================================
# SECTION 10: SENTIMENT VISUALIZATIONS
# ============================================================================

cat("\n📊 Creating sentiment visualizations...\n")

# Create plots directory
dir.create("plots", showWarnings = FALSE)

# Plot 1: Daily sentiment over time
p1 <- ggplot(daily_sentiment, aes(x = date, y = sentiment_mean)) +
  geom_line(color = "#3498DB", size = 1.2) +
  geom_ribbon(aes(ymin = sentiment_mean - sentiment_sd,
                  ymax = sentiment_mean + sentiment_sd),
              alpha = 0.2, fill = "#3498DB") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Daily Sentiment Trend",
    subtitle = "Mean sentiment with standard deviation band",
    x = "Date",
    y = "Combined Sentiment Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

# Plot 2: Sentiment distribution
p2 <- ggplot(reddit_data, aes(x = combined_sentiment, fill = sentiment_label)) +
  geom_histogram(bins = 50, alpha = 0.7) +
  scale_fill_manual(
    values = c("positive" = "#27AE60", "neutral" = "#95A5A6", "negative" = "#E74C3C")
  ) +
  labs(
    title = "Sentiment Score Distribution",
    subtitle = paste("Total:", format(nrow(reddit_data), big.mark = ","), "posts/comments"),
    x = "Combined Sentiment Score",
    y = "Frequency",
    fill = "Sentiment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom"
  )

# Plot 3: Emotion breakdown
emotion_data <- reddit_data %>%
  summarise(
    Joy = sum(joy, na.rm = TRUE),
    Trust = sum(trust, na.rm = TRUE),
    Anticipation = sum(anticipation, na.rm = TRUE),
    Fear = sum(fear, na.rm = TRUE),
    Sadness = sum(sadness, na.rm = TRUE),
    Anger = sum(anger, na.rm = TRUE),
    Surprise = sum(surprise, na.rm = TRUE),
    Disgust = sum(disgust, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "emotion", values_to = "count")

p3 <- ggplot(emotion_data, aes(x = reorder(emotion, count), y = count, fill = emotion)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "NRC Emotion Distribution",
    subtitle = "Total emotion counts across all posts and comments",
    x = NULL,
    y = "Count",
    fill = "Emotion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none"
  )

# Combine plots
sentiment_viz <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1)

# Save plots
ggsave("plots/sentiment_analysis.png", sentiment_viz, 
       width = 12, height = 14, dpi = 300)

cat("✅ Sentiment visualizations saved to plots/sentiment_analysis.png\n")

# ============================================================================
# SECTION 11: SAVE RESULTS
# ============================================================================

cat("\n💾 Saving sentiment analysis results...\n")

# Create data directory
dir.create("data", showWarnings = FALSE)

# Save full dataset with sentiment scores
write_csv(reddit_data, "data/reddit_with_sentiment.csv")
cat("✅ Full dataset saved: data/reddit_with_sentiment.csv\n")

# Save daily aggregated sentiment
write_csv(daily_sentiment, "data/daily_sentiment.csv")
cat("✅ Daily sentiment saved: data/daily_sentiment.csv\n")

# Save top posts
write_csv(top_positive, "data/top_positive_posts.csv")
write_csv(top_negative, "data/top_negative_posts.csv")
cat("✅ Top posts saved\n")

# Save as RDS for easy loading in R
saveRDS(list(
  data = reddit_data,
  daily = daily_sentiment,
  top_positive = top_positive,
  top_negative = top_negative,
  summary = list(
    total_records = nrow(reddit_data),
    sentiment_distribution = sentiment_counts,
    mean_sentiment = mean(reddit_data$combined_sentiment, na.rm = TRUE)
  )
), "results/sentiment_analysis.rds")

cat("✅ Results saved: results/sentiment_analysis.rds\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("🎉 SENTIMENT ANALYSIS COMPLETE!\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n📁 Generated Files:\n")
cat("   • data/reddit_with_sentiment.csv\n")
cat("   • data/daily_sentiment.csv\n")
cat("   • data/top_positive_posts.csv\n")
cat("   • data/top_negative_posts.csv\n")
cat("   • results/sentiment_analysis.rds\n")
cat("   • plots/sentiment_analysis.png\n")

cat("\n🎯 Key Findings:\n")
cat(sprintf("   • Total records analyzed: %s\n", 
            format(nrow(reddit_data), big.mark = ",")))
cat(sprintf("   • Overall sentiment: %.3f\n", 
            mean(reddit_data$combined_sentiment, na.rm = TRUE)))
cat(sprintf("   • Positive posts: %.1f%%\n",
            sentiment_counts$percentage[sentiment_counts$sentiment_label == "positive"]))
cat(sprintf("   • Negative posts: %.1f%%\n",
            sentiment_counts$percentage[sentiment_counts$sentiment_label == "negative"]))
cat(sprintf("   • Top emotion: %s\n", emotion_totals$emotion[1]))

cat("\n✅ Ready for next step: Get cryptocurrency price data!\n")

cat("\n", rep("=", 70), "\n", sep = "")
cat("Sentiment analysis script completed successfully!\n")
cat(rep("=", 70), "\n", sep = "")
