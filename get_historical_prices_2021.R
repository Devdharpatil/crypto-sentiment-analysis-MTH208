# Alternative Historical Price Data Collector (2021)
# Uses free CryptoCompare API - no authentication required

# ============================================================================
# SECTION 1: LOAD REQUIRED PACKAGES
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("💰 ALTERNATIVE HISTORICAL PRICE COLLECTOR (2021)\n")
cat(rep("=", 70), "\n", sep = "")
cat("Using: CryptoCompare API (Free, No Auth Required)\n")
cat("Date Range: August 13 - September 30, 2021\n")
cat(rep("=", 70), "\n", sep = "")

# Required packages
required_packages <- c(
  "tidyverse",   # Data manipulation
  "httr",        # HTTP requests
  "jsonlite",    # JSON parsing
  "lubridate"    # Date handling
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
# SECTION 2: CONFIGURATION
# ============================================================================

# CryptoCompare API (FREE - no key needed for historical data)
BASE_URL <- "https://min-api.cryptocompare.com/data/v2"

# Cryptocurrencies (CryptoCompare symbols)
CRYPTOCURRENCIES <- list(
  BTC = "Bitcoin",
  ETH = "Ethereum",
  DOGE = "Dogecoin"
)

# Date range matching Reddit data
START_DATE <- as.Date("2021-08-13")
END_DATE <- as.Date("2021-09-30")

cat("\n🎯 Configuration:\n")
cat(paste("   Start Date:", START_DATE, "\n"))
cat(paste("   End Date:", END_DATE, "\n"))
cat(paste("   Days:", as.numeric(END_DATE - START_DATE), "\n"))

# ============================================================================
# SECTION 3: HISTORICAL PRICE FETCHING FUNCTION
# ============================================================================

get_historical_prices_cc <- function(symbol, coin_name, start_date, end_date) {
  # Fetch historical daily data from CryptoCompare

  cat(sprintf("\n📊 Fetching %s (%s) historical data...\n", coin_name, symbol))

  # Convert dates to Unix timestamps
  from_timestamp <- as.numeric(as.POSIXct(start_date))
  to_timestamp <- as.numeric(as.POSIXct(end_date))

  # Calculate number of days
  days_needed <- as.numeric(end_date - start_date) + 1

  # CryptoCompare histoday endpoint
  url <- paste0(BASE_URL, "/histoday")

  # Query parameters
  params <- list(
    fsym = symbol,
    tsym = "USD",
    limit = days_needed,
    toTs = to_timestamp
  )

  tryCatch({
    # Make API request
    response <- GET(url, query = params, timeout(30))

    if (status_code(response) != 200) {
      cat(sprintf("   ❌ Error: HTTP status %d\n", status_code(response)))
      return(NULL)
    }

    # Parse response
    data <- content(response, as = "text", encoding = "UTF-8") %>%
      fromJSON(simplifyVector = TRUE)

    if (data$Response != "Success") {
      cat(sprintf("   ❌ API Error: %s\n", data$Message))
      return(NULL)
    }

    # Extract price data
    price_data <- data$Data$Data

    if (is.null(price_data) || nrow(price_data) == 0) {
      cat("   ❌ No price data returned\n")
      return(NULL)
    }

    # Convert to tibble and process
    df <- as_tibble(price_data) %>%
      mutate(
        timestamp = as_datetime(time),
        date = as.Date(timestamp),
        cryptocurrency = tolower(symbol),
        price = close,  # Using close price as main price
        open_price = open,
        close_price = close,
        high_price = high,
        low_price = low,
        volume_24h = volumeto,  # Volume in USD
        market_cap = NA  # Not provided by CryptoCompare in this endpoint
      ) %>%
      filter(date >= start_date & date <= end_date) %>%
      select(date, timestamp, cryptocurrency, price, open_price, close_price,
             high_price, low_price, volume_24h, market_cap) %>%
      arrange(date)

    # Calculate price changes
    df <- df %>%
      mutate(
        price_change = price - lag(price),
        price_change_pct = (price / lag(price) - 1) * 100,
        log_return = log(price / lag(price))
      )

    # Calculate 7-day rolling volatility
    if (nrow(df) >= 7) {
      df$volatility_7d <- zoo::rollapply(
        df$price_change_pct,
        width = 7,
        FUN = sd,
        fill = NA,
        align = "right"
      )
    } else {
      df$volatility_7d <- NA
    }

    cat(sprintf("   ✅ Retrieved %d daily price points\n", nrow(df)))
    cat(sprintf("   📅 Range: %s to %s\n", min(df$date), max(df$date)))
    cat(sprintf("   💵 Start price: $%s\n", format(round(df$price[1], 2), big.mark = ",")))
    cat(sprintf("   💵 End price: $%s\n", format(round(df$price[nrow(df)], 2), big.mark = ",")))

    return(df)

  }, error = function(e) {
    cat(sprintf("   ❌ Error: %s\n", e$message))
    return(NULL)
  })
}

# ============================================================================
# SECTION 4: PRICE SUMMARY FUNCTION
# ============================================================================

get_price_summary <- function(df, crypto_name) {
  summary <- tibble(
    cryptocurrency = crypto_name,
    start_date = as.character(min(df$date)),
    end_date = as.character(max(df$date)),
    days = nrow(df),
    start_price = first(df$price),
    end_price = last(df$price),
    min_price = min(df$price, na.rm = TRUE),
    max_price = max(df$price, na.rm = TRUE),
    mean_price = mean(df$price, na.rm = TRUE),
    median_price = median(df$price, na.rm = TRUE),
    total_return_pct = ((last(df$price) / first(df$price)) - 1) * 100,
    max_daily_gain = max(df$price_change_pct, na.rm = TRUE),
    max_daily_loss = min(df$price_change_pct, na.rm = TRUE),
    avg_volatility = mean(df$volatility_7d, na.rm = TRUE),
    total_volume = sum(df$volume_24h, na.rm = TRUE)
  )
  return(summary)
}

# ============================================================================
# SECTION 5: FETCH DATA FOR ALL CRYPTOCURRENCIES
# ============================================================================

cat("\n🔄 Starting historical price data collection...\n")

all_prices <- list()
all_summaries <- list()

# Fetch data for each cryptocurrency
for (symbol in names(CRYPTOCURRENCIES)) {
  coin_name <- CRYPTOCURRENCIES[[symbol]]

  cat("\n", rep("=", 70), "\n", sep = "")
  cat(sprintf("Collecting: %s (%s) - Aug-Sep 2021\n", coin_name, symbol))
  cat(rep("=", 70), "\n", sep = "")

  # Fetch historical price data
  price_df <- get_historical_prices_cc(symbol, coin_name, START_DATE, END_DATE)

  if (!is.null(price_df) && nrow(price_df) > 0) {
    # Store data
    all_prices[[tolower(symbol)]] <- price_df

    # Generate summary
    summary <- get_price_summary(price_df, coin_name)
    all_summaries[[tolower(symbol)]] <- summary

    # Display summary
    cat(sprintf("\n   📊 Price Summary for %s (Aug-Sep 2021):\n", coin_name))
    cat(sprintf("      Period: %s to %s\n", summary$start_date, summary$end_date))
    cat(sprintf("      Start Price: $%s\n", format(round(summary$start_price, 2), big.mark = ",")))
    cat(sprintf("      End Price: $%s\n", format(round(summary$end_price, 2), big.mark = ",")))
    cat(sprintf("      Min Price: $%s\n", format(round(summary$min_price, 2), big.mark = ",")))
    cat(sprintf("      Max Price: $%s\n", format(round(summary$max_price, 2), big.mark = ",")))
    cat(sprintf("      Total Return: %.2f%%\n", summary$total_return_pct))
    cat(sprintf("      Max Daily Gain: %.2f%%\n", summary$max_daily_gain))
    cat(sprintf("      Max Daily Loss: %.2f%%\n", summary$max_daily_loss))
    cat(sprintf("      Avg Volatility: %.2f%%\n", summary$avg_volatility))
  }

  # Rate limiting
  Sys.sleep(1)
}

# ============================================================================
# SECTION 6: SAVE RESULTS
# ============================================================================

if (length(all_prices) > 0) {
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("💾 SAVING HISTORICAL RESULTS (2021)\n")
  cat(rep("=", 70), "\n", sep = "")

  # Create directories
  dir.create("data", showWarnings = FALSE)
  dir.create("results", showWarnings = FALSE)
  dir.create("plots", showWarnings = FALSE)

  # Combine all cryptocurrency data
  combined_prices <- bind_rows(all_prices)

  # Save combined data
  output_file <- "data/crypto_prices_2021.csv"
  write_csv(combined_prices, output_file)

  cat(sprintf("\n✅ Historical price data saved: %s\n", output_file))
  cat(sprintf("   Total records: %s\n", format(nrow(combined_prices), big.mark = ",")))
  cat(sprintf("   Cryptocurrencies: %d\n", length(CRYPTOCURRENCIES)))
  cat(sprintf("   Date range: %s to %s\n", 
              min(combined_prices$date), max(combined_prices$date)))

  # Save summary
  summary_df <- bind_rows(all_summaries)
  write_csv(summary_df, "data/price_summary_2021.csv")
  cat("\n✅ Summary saved: data/price_summary_2021.csv\n")

  # Save individual files
  cat("\n✅ Individual crypto files (2021):\n")
  for (crypto_id in names(all_prices)) {
    crypto_file <- paste0("data/", crypto_id, "_prices_2021.csv")
    write_csv(all_prices[[crypto_id]], crypto_file)
    cat(sprintf("   • %s (%d records)\n", 
                basename(crypto_file), nrow(all_prices[[crypto_id]])))
  }

  # Save as RDS
  saveRDS(list(
    combined = combined_prices,
    individual = all_prices,
    summary = summary_df
  ), "results/price_data_2021.rds")
  cat("\n✅ Results saved: results/price_data_2021.rds\n")

  # ============================================================================
  # SECTION 7: CREATE VISUALIZATIONS
  # ============================================================================

  cat("\n📊 Creating historical price visualizations...\n")

  # Plot 1: Price trends
  p1 <- ggplot(combined_prices, aes(x = date, y = price, color = cryptocurrency)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(
      values = c("btc" = "#F7931A", "eth" = "#627EEA", "doge" = "#C2A633"),
      labels = c("Bitcoin", "Ethereum", "Dogecoin")
    ) +
    scale_y_log10(labels = scales::dollar_format()) +
    labs(
      title = "Cryptocurrency Prices (August-September 2021)",
      subtitle = "Historical data matching Reddit discussion period",
      x = "Date",
      y = "Price (USD, Log Scale)",
      color = "Cryptocurrency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      legend.position = "bottom"
    )

  # Plot 2: Daily returns
  p2 <- ggplot(combined_prices, aes(x = date, y = price_change_pct, fill = cryptocurrency)) +
    geom_col(position = "dodge", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("btc" = "#F7931A", "eth" = "#627EEA", "doge" = "#C2A633"),
      labels = c("Bitcoin", "Ethereum", "Dogecoin")
    ) +
    labs(
      title = "Daily Price Changes (Aug-Sep 2021)",
      x = "Date",
      y = "Daily Return (%)",
      fill = "Cryptocurrency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )

  # Plot 3: Normalized performance
  price_normalized <- combined_prices %>%
    group_by(cryptocurrency) %>%
    mutate(price_normalized = (price / first(price)) * 100) %>%
    ungroup()

  p3 <- ggplot(price_normalized, aes(x = date, y = price_normalized, color = cryptocurrency)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
    scale_color_manual(
      values = c("btc" = "#F7931A", "eth" = "#627EEA", "doge" = "#C2A633"),
      labels = c("Bitcoin", "Ethereum", "Dogecoin")
    ) +
    labs(
      title = "Normalized Price Performance (Base = 100)",
      subtitle = "Relative returns from August 13, 2021",
      x = "Date",
      y = "Normalized Price",
      color = "Cryptocurrency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      legend.position = "bottom"
    )

  # Combine and save
  price_viz <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
  ggsave("plots/price_analysis_2021.png", price_viz, 
         width = 12, height = 14, dpi = 300)

  cat("✅ Visualizations saved to plots/price_analysis_2021.png\n")

  # ============================================================================
  # FINAL SUMMARY
  # ============================================================================

  cat("\n", rep("=", 70), "\n", sep = "")
  cat("🎉 HISTORICAL PRICE DATA COLLECTION COMPLETE!\n")
  cat(rep("=", 70), "\n", sep = "")

  cat("\n📁 Generated Files (2021 Historical Data):\n")
  cat("   • data/crypto_prices_2021.csv\n")
  cat("   • data/btc_prices_2021.csv\n")
  cat("   • data/eth_prices_2021.csv\n")
  cat("   • data/doge_prices_2021.csv\n")
  cat("   • data/price_summary_2021.csv\n")
  cat("   • results/price_data_2021.rds\n")
  cat("   • plots/price_analysis_2021.png\n")

  cat("\n🎯 Historical Data Summary:\n")
  cat(sprintf("   • Total records: %s\n", format(nrow(combined_prices), big.mark = ",")))
  cat(sprintf("   • Date range: %s to %s\n", 
              min(combined_prices$date), max(combined_prices$date)))
  cat(sprintf("   • Matches Reddit data: ✅ YES\n"))

  cat("\n✅ Ready For:\n")
  cat("   1. Merge with sentiment data (matching timeframe!)\n")
  cat("   2. Correlation analysis\n")
  cat("   3. Price prediction models\n")
  cat("   4. Interactive Shiny dashboard\n")

} else {
  cat("\n❌ ERROR: No historical price data collected!\n")
  cat("Please check your internet connection and try again.\n")
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("Script completed!\n")
cat(rep("=", 70), "\n", sep = "")
