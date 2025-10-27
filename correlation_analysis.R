# Sentiment-Price Correlation Analysis
# Merges sentiment and price data, calculates correlations

# ============================================================================
# SECTION 1: LOAD REQUIRED PACKAGES
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 SENTIMENT-PRICE CORRELATION ANALYSIS\n")
cat(rep("=", 70), "\n", sep = "")
cat("Analyzing relationship between Reddit sentiment and crypto prices\n")
cat(rep("=", 70), "\n", sep = "")

# Required packages
required_packages <- c(
  "tidyverse",   # Data manipulation
  "lubridate",   # Date handling
  "corrplot",    # Correlation plots
  "ggplot2",     # Visualization
  "gridExtra",   # Multiple plots
  "scales",      # Scale formatting
  "viridis"      # Color palettes
)

cat("\n📦 Loading required packages...\n")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("   Installing", pkg, "...\n"))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("✅ All packages loaded successfully!\n")

# ============================================================================
# SECTION 2: LOAD DATA
# ============================================================================

cat("\n📂 Loading sentiment and price data...\n")

# Load daily sentiment data
sentiment_data <- read_csv("data/daily_sentiment.csv", show_col_types = FALSE)
cat(sprintf("✅ Loaded sentiment data: %d days\n", nrow(sentiment_data)))

# Load 2021 historical price data
price_data <- read_csv("data/crypto_prices_2021.csv", show_col_types = FALSE)
cat(sprintf("✅ Loaded price data: %d records\n", nrow(price_data)))

# Convert dates
sentiment_data$date <- as.Date(sentiment_data$date)
price_data$date <- as.Date(price_data$date)

cat(sprintf("\n📅 Data ranges:\n"))
cat(sprintf("   Sentiment: %s to %s\n", 
            min(sentiment_data$date), max(sentiment_data$date)))
cat(sprintf("   Prices: %s to %s\n", 
            min(price_data$date), max(price_data$date)))

# ============================================================================
# SECTION 3: MERGE SENTIMENT AND PRICE DATA
# ============================================================================

cat("\n🔗 Merging sentiment and price data...\n")

# Prepare sentiment data (already daily)
sentiment_prep <- sentiment_data %>%
  select(
    date, 
    sentiment_mean, 
    sentiment_sd,
    positive_count,
    negative_count,
    neutral_count,
    total_posts,
    joy_total,
    fear_total,
    anger_total,
    trust_total
  )

# Merge with each cryptocurrency
merged_data <- list()

for (crypto in c("btc", "eth", "doge")) {
  # Filter price data for this crypto
  crypto_prices <- price_data %>%
    filter(cryptocurrency == crypto) %>%
    select(date, price, price_change_pct, volatility_7d, volume_24h)

  # Merge with sentiment
  merged <- sentiment_prep %>%
    inner_join(crypto_prices, by = "date") %>%
    mutate(cryptocurrency = crypto)

  merged_data[[crypto]] <- merged

  cat(sprintf("   ✅ %s: %d matching days\n", toupper(crypto), nrow(merged)))
}

# Combine all cryptocurrencies
combined_data <- bind_rows(merged_data)

cat(sprintf("\n✅ Total merged records: %d\n", nrow(combined_data)))

# Save merged data
write_csv(combined_data, "data/sentiment_price_merged.csv")
cat("✅ Saved: data/sentiment_price_merged.csv\n")

# ============================================================================
# SECTION 4: CORRELATION ANALYSIS
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📈 CORRELATION ANALYSIS RESULTS\n")
cat(rep("=", 70), "\n", sep = "")

# Function to calculate correlations
calculate_correlations <- function(data, crypto_name) {
  cat(sprintf("\n### %s ###\n", crypto_name))

  # Select numeric variables
  cor_data <- data %>%
    select(
      sentiment_mean,
      positive_count,
      negative_count,
      joy_total,
      fear_total,
      anger_total,
      trust_total,
      price,
      price_change_pct,
      volatility_7d
    ) %>%
    na.omit()

  # Pearson correlation
  cor_pearson <- cor(cor_data, method = "pearson")

  # Spearman correlation (rank-based, more robust)
  cor_spearman <- cor(cor_data, method = "spearman")

  # Extract key correlations
  sentiment_price <- cor_pearson["sentiment_mean", "price"]
  sentiment_change <- cor_pearson["sentiment_mean", "price_change_pct"]
  pos_price <- cor_pearson["positive_count", "price"]
  neg_price <- cor_pearson["negative_count", "price"]
  fear_volatility <- cor_pearson["fear_total", "volatility_7d"]

  cat(sprintf("Sentiment vs Price: %.3f\n", sentiment_price))
  cat(sprintf("Sentiment vs Price Change: %.3f\n", sentiment_change))
  cat(sprintf("Positive Posts vs Price: %.3f\n", pos_price))
  cat(sprintf("Negative Posts vs Price: %.3f\n", neg_price))
  cat(sprintf("Fear vs Volatility: %.3f\n", fear_volatility))

  return(list(
    pearson = cor_pearson,
    spearman = cor_spearman,
    key_cors = c(
      sentiment_price = sentiment_price,
      sentiment_change = sentiment_change,
      pos_price = pos_price,
      neg_price = neg_price,
      fear_volatility = fear_volatility
    )
  ))
}

# Calculate for each cryptocurrency
correlations <- list()
for (crypto in c("btc", "eth", "doge")) {
  crypto_data <- combined_data %>% filter(cryptocurrency == crypto)
  correlations[[crypto]] <- calculate_correlations(
    crypto_data, 
    toupper(crypto)
  )
}

# ============================================================================
# SECTION 5: LEAD-LAG ANALYSIS
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("⏱️  LEAD-LAG ANALYSIS\n")
cat(rep("=", 70), "\n", sep = "")
cat("Does sentiment lead or lag price movements?\n\n")

for (crypto in c("btc", "eth", "doge")) {
  cat(sprintf("### %s ###\n", toupper(crypto)))

  crypto_data <- combined_data %>% 
    filter(cryptocurrency == crypto) %>%
    arrange(date)

  # Test different lags (1, 2, 3 days)
  for (lag_days in 1:3) {
    # Sentiment leads price (sentiment today vs price tomorrow)
    cor_lead <- cor(
      crypto_data$sentiment_mean[1:(nrow(crypto_data)-lag_days)],
      crypto_data$price_change_pct[(1+lag_days):nrow(crypto_data)],
      use = "complete.obs"
    )

    # Sentiment lags price (price today vs sentiment tomorrow)
    cor_lag <- cor(
      crypto_data$price_change_pct[1:(nrow(crypto_data)-lag_days)],
      crypto_data$sentiment_mean[(1+lag_days):nrow(crypto_data)],
      use = "complete.obs"
    )

    cat(sprintf("Lag %d day: Sentiment→Price %.3f | Price→Sentiment %.3f\n",
                lag_days, cor_lead, cor_lag))
  }
  cat("\n")
}

# ============================================================================
# SECTION 6: VISUALIZATIONS
# ============================================================================

cat("📊 Creating correlation visualizations...\n")

dir.create("plots", showWarnings = FALSE)

# Plot 1: Correlation heatmap for Bitcoin
btc_data <- combined_data %>% 
  filter(cryptocurrency == "btc") %>%
  select(
    Sentiment = sentiment_mean,
    `Pos Posts` = positive_count,
    `Neg Posts` = negative_count,
    Joy = joy_total,
    Fear = fear_total,
    Trust = trust_total,
    Price = price,
    `Price Δ%` = price_change_pct,
    Volatility = volatility_7d
  ) %>%
  na.omit()

cor_matrix <- cor(btc_data)

png("plots/correlation_heatmap.png", width = 12, height = 10, units = "in", res = 300)
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("#E74C3C", "white", "#27AE60"))(200),
         title = "Sentiment-Price Correlation Matrix (Bitcoin)",
         mar = c(0, 0, 2, 0))
dev.off()

cat("✅ Saved: plots/correlation_heatmap.png\n")

# Plot 2: Time series overlay - Sentiment vs Price
for (crypto in c("btc", "eth", "doge")) {
  crypto_data <- combined_data %>% filter(cryptocurrency == crypto)

  # Normalize for dual-axis plot
  crypto_data <- crypto_data %>%
    mutate(
      price_norm = (price - mean(price, na.rm = TRUE)) / sd(price, na.rm = TRUE),
      sentiment_norm = (sentiment_mean - mean(sentiment_mean, na.rm = TRUE)) / 
                       sd(sentiment_mean, na.rm = TRUE)
    )

  p <- ggplot(crypto_data, aes(x = date)) +
    geom_line(aes(y = sentiment_norm, color = "Sentiment"), linewidth = 1.2) +
    geom_line(aes(y = price_norm, color = "Price"), linewidth = 1.2, alpha = 0.7) +
    scale_color_manual(
      values = c("Sentiment" = "#3498DB", "Price" = "#E74C3C")
    ) +
    labs(
      title = sprintf("%s: Sentiment vs Price (Normalized)", toupper(crypto)),
      subtitle = "Both scaled to mean=0, sd=1 for comparison",
      x = "Date",
      y = "Normalized Value",
      color = "Variable"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      legend.position = "bottom"
    )

  ggsave(sprintf("plots/sentiment_price_%s.png", crypto), p,
         width = 12, height = 6, dpi = 300)
}

cat("✅ Saved: plots/sentiment_price_*.png (3 files)\n")

# Plot 3: Scatter plots
scatter_plots <- list()

crypto_list <- c("btc", "eth", "doge")
for (i in seq_along(crypto_list)) {
  crypto <- crypto_list[i]
  crypto_data <- combined_data %>% filter(cryptocurrency == crypto)

  p <- ggplot(crypto_data, aes(x = sentiment_mean, y = price_change_pct)) +
    geom_point(alpha = 0.6, size = 3, color = "#3498DB") +
    geom_smooth(method = "lm", color = "#E74C3C", se = TRUE) +
    labs(
      title = toupper(crypto),
      x = "Daily Sentiment",
      y = "Price Change (%)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  scatter_plots[[i]] <- p
}

combined_scatter <- gridExtra::grid.arrange(
  scatter_plots[[1]], scatter_plots[[2]], scatter_plots[[3]],
  ncol = 3,
  top = "Sentiment vs Price Change: Correlation Scatter Plots"
)

ggsave("plots/scatter_sentiment_price.png", combined_scatter,
       width = 15, height = 5, dpi = 300)

cat("✅ Saved: plots/scatter_sentiment_price.png\n")

# ============================================================================
# SECTION 7: SUMMARY STATISTICS
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📋 CORRELATION SUMMARY\n")
cat(rep("=", 70), "\n", sep = "")

summary_stats <- data.frame(
  Cryptocurrency = c("Bitcoin", "Ethereum", "Dogecoin"),
  Sentiment_Price = c(
    correlations$btc$key_cors["sentiment_price"],
    correlations$eth$key_cors["sentiment_price"],
    correlations$doge$key_cors["sentiment_price"]
  ),
  Sentiment_Change = c(
    correlations$btc$key_cors["sentiment_change"],
    correlations$eth$key_cors["sentiment_change"],
    correlations$doge$key_cors["sentiment_change"]
  ),
  Fear_Volatility = c(
    correlations$btc$key_cors["fear_volatility"],
    correlations$eth$key_cors["fear_volatility"],
    correlations$doge$key_cors["fear_volatility"]
  )
)

print(summary_stats)

write.csv(summary_stats, "results/correlation_summary.csv", row.names = FALSE)
cat("\n✅ Saved: results/correlation_summary.csv\n")

# Save detailed results
saveRDS(list(
  merged_data = combined_data,
  correlations = correlations,
  summary = summary_stats
), "results/correlation_analysis.rds")

cat("✅ Saved: results/correlation_analysis.rds\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("🎉 CORRELATION ANALYSIS COMPLETE!\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n📁 Generated Files:\n")
cat("   • data/sentiment_price_merged.csv\n")
cat("   • results/correlation_summary.csv\n")
cat("   • results/correlation_analysis.rds\n")
cat("   • plots/correlation_heatmap.png\n")
cat("   • plots/sentiment_price_btc.png\n")
cat("   • plots/sentiment_price_eth.png\n")
cat("   • plots/sentiment_price_doge.png\n")
cat("   • plots/scatter_sentiment_price.png\n")

cat("\n🎯 Key Findings:\n")
cat(sprintf("   • BTC Sentiment-Price correlation: %.3f\n",
            correlations$btc$key_cors["sentiment_price"]))
cat(sprintf("   • ETH Sentiment-Price correlation: %.3f\n",
            correlations$eth$key_cors["sentiment_price"]))
cat(sprintf("   • DOGE Sentiment-Price correlation: %.3f\n",
            correlations$doge$key_cors["sentiment_price"]))

cat("\n✅ Ready for:\n")
cat("   1. Shiny dashboard development\n")
cat("   2. Final report writing\n")
cat("   3. Presentation preparation\n")

cat("\n", rep("=", 70), "\n", sep = "")
cat("Analysis script completed!\n")
cat(rep("=", 70), "\n", sep = "")
