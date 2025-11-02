# Cryptocurrency Sentiment Analysis Dashboard
# Interactive Shiny application with About tab
# Author: Team 1
# Date: November 2025

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)

# ============================================================================
# LOAD DATA
# ============================================================================

sentiment_data <- read_csv("../data/reddit_with_sentiment.csv", show_col_types = FALSE)
daily_sentiment <- read_csv("../data/daily_sentiment.csv", show_col_types = FALSE)
price_data <- read_csv("../data/crypto_prices_2021.csv", show_col_types = FALSE)
merged_data <- read_csv("../data/sentiment_price_merged.csv", show_col_types = FALSE)

sentiment_data$date <- as.Date(sentiment_data$date)
daily_sentiment$date <- as.Date(daily_sentiment$date)
price_data$date <- as.Date(price_data$date)
merged_data$date <- as.Date(merged_data$date)

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Crypto Sentiment Analysis",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem(" Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem(" Sentiment Analysis", tabName = "sentiment", icon = icon("brain")),
      menuItem(" Price Analysis", tabName = "prices", icon = icon("chart-line")),
      menuItem(" Correlation", tabName = "correlation", icon = icon("link")),
      menuItem(" Top Posts", tabName = "posts", icon = icon("list")),
      menuItem(" Data Explorer", tabName = "data", icon = icon("table")),
      menuItem(" About", tabName = "about", icon = icon("info-circle"))
    ),
    hr(),
    dateRangeInput(
      "date_range",
      "Date Range:",
      start = min(daily_sentiment$date),
      end = max(daily_sentiment$date),
      min = min(daily_sentiment$date),
      max = max(daily_sentiment$date)
    ),
    selectInput(
      "crypto_select",
      "Cryptocurrency:",
      choices = c("Bitcoin" = "btc", "Ethereum" = "eth", "Dogecoin" = "doge"),
      selected = "btc"
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box-header { font-weight: bold; }
        .info-box { min-height: 90px; }
        .content-wrapper { background-color: #ecf0f5; }
      "))
    ),
    
    tabItems(
      # TAB 1: OVERVIEW
      tabItem(
        tabName = "overview",
        h2("Project Overview"),
        fluidRow(
          infoBox("Total Posts/Comments", format(nrow(sentiment_data), big.mark = ","),
                  icon = icon("comments"), color = "blue", width = 3),
          infoBox("Analysis Period", 
                  paste(min(daily_sentiment$date), "to", max(daily_sentiment$date)),
                  icon = icon("calendar"), color = "green", width = 3),
          infoBox("Avg Sentiment", sprintf("%.3f", mean(sentiment_data$combined_sentiment, na.rm = TRUE)),
                  icon = icon("smile"), color = "yellow", width = 3),
          infoBox("Cryptocurrencies", "BTC, ETH, DOGE",
                  icon = icon("bitcoin"), color = "orange", width = 3)
        ),
        fluidRow(
          box(title = "Sentiment Distribution", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("overview_sentiment_dist")),
          box(title = "Daily Activity", status = "success", solidHeader = TRUE, width = 6,
              plotlyOutput("overview_daily_activity"))
        ),
        fluidRow(
          box(title = "Key Correlation Findings", status = "warning", solidHeader = TRUE, width = 12,
              HTML('<div style="padding: 10px;">
                <h4>Sentiment-Price Correlations (Aug-Sep 2021)</h4>
                <div style="display: flex; justify-content: space-around; margin-top: 15px;">
                  <div style="text-align: center;">
                    <h3 style="color: #F7931A; margin: 0;">Bitcoin</h3>
                    <p style="font-size: 24px; font-weight: bold; margin: 5px 0;">0.351</p>
                    <p style="color: #666; margin: 0;">Moderate Positive</p>
                  </div>
                  <div style="text-align: center;">
                    <h3 style="color: #627EEA; margin: 0;">Ethereum</h3>
                    <p style="font-size: 24px; font-weight: bold; margin: 5px 0;">0.227</p>
                    <p style="color: #666; margin: 0;">Weak Positive</p>
                  </div>
                  <div style="text-align: center;">
                    <h3 style="color: #C2A633; margin: 0;">Dogecoin</h3>
                    <p style="font-size: 24px; font-weight: bold; margin: 5px 0;">0.149</p>
                    <p style="color: #666; margin: 0;">Weak Positive</p>
                  </div>
                </div>
                <hr>
                <p><strong>Fear-Volatility Correlation:</strong> 0.42 (All cryptos)</p>
              </div>'))
        )
      ),
      
      # TAB 2: SENTIMENT
      tabItem(
        tabName = "sentiment",
        h2("Sentiment Analysis"),
        fluidRow(
          box(title = "Daily Sentiment Trend", status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("sentiment_trend", height = "400px"))
        ),
        fluidRow(
          box(title = "Emotion Distribution", status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("emotion_dist")),
          box(title = "Sentiment by Type", status = "warning", solidHeader = TRUE, width = 6,
              plotlyOutput("sentiment_by_type"))
        )
      ),
      
      # TAB 3: PRICES
      tabItem(
        tabName = "prices",
        h2("Cryptocurrency Price Analysis"),
        fluidRow(
          box(title = "Price Trends (2021)", status = "success", solidHeader = TRUE, width = 12,
              plotlyOutput("price_trends", height = "400px"))
        ),
        fluidRow(
          box(title = "Daily Returns", status = "warning", solidHeader = TRUE, width = 6,
              plotlyOutput("price_returns")),
          box(title = "Volatility", status = "danger", solidHeader = TRUE, width = 6,
              plotlyOutput("price_volatility"))
        )
      ),
      
      # TAB 4: CORRELATION
      tabItem(
        tabName = "correlation",
        h2("Sentiment-Price Correlation"),
        fluidRow(
          box(title = "Sentiment vs Price (Normalized)", status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("correlation_overlay", height = "400px"))
        ),
        fluidRow(
          box(title = "Correlation Scatter", status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("correlation_scatter")),
          box(title = "Correlation Statistics", status = "success", solidHeader = TRUE, width = 6,
              verbatimTextOutput("correlation_stats"))
        )
      ),
      
      # TAB 5: TOP POSTS
      tabItem(
        tabName = "posts",
        h2("Top Posts by Sentiment"),
        fluidRow(
          box(title = "Most Positive Posts", status = "success", solidHeader = TRUE, width = 12,
              DTOutput("top_positive_table"))
        ),
        fluidRow(
          box(title = "Most Negative Posts", status = "danger", solidHeader = TRUE, width = 12,
              DTOutput("top_negative_table"))
        )
      ),
      
      # TAB 6: DATA EXPLORER
      tabItem(
        tabName = "data",
        h2("Data Explorer"),
        fluidRow(
          box(title = "Merged Sentiment-Price Data", status = "primary", solidHeader = TRUE, width = 12,
              DTOutput("data_table"))
        )
      ),
      
      # TAB 7: ABOUT (NEW)
      tabItem(
        tabName = "about",
        h2("About This Project"),
        fluidRow(
          box(
            title = "Project Information",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h3("Research Question"),
            p(strong("Central Question:"), "Does Reddit sentiment from r/CryptoCurrency correlate with cryptocurrency price movements?"),
            p("This project investigates whether social media sentiment can predict or explain cryptocurrency price changes during the August-September 2021 period."),
            hr(),
            
            h3("Data Sources"),
            h4("Reddit Data"),
            tags$ul(
              tags$li("40,918 posts and comments from r/CryptoCurrency subreddit"),
              tags$li("Date Range: August 13 - September 19, 2021 (38 days)"),
              tags$li(HTML('Source: <a href="https://www.kaggle.com/datasets/paultimothymooney/cryptocurrency-reddit" target="_blank">Kaggle - r/CryptoCurrency Dataset</a>'))
            ),
            h4("Price Data"),
            tags$ul(
              tags$li("Historical daily OHLC prices for Bitcoin, Ethereum, and Dogecoin"),
              tags$li("Date Range: August 13 - September 30, 2021 (49 days)"),
              tags$li(HTML('Source: <a href="https://www.cryptocompare.com/" target="_blank">CryptoCompare API</a>'))
            ),
            hr(),
            
            h3("Methodology"),
            tags$ol(
              tags$li(strong("Exploratory Data Analysis (EDA)"),
                      tags$ul(
                        tags$li("Temporal patterns and engagement metrics"),
                        tags$li("Content characteristics and post distributions"),
                        tags$li("User activity patterns")
                      )),
              tags$li(strong("Sentiment Analysis"),
                      tags$ul(
                        tags$li(strong("NRC Emotion Lexicon:"), "8 discrete emotions (joy, trust, fear, anger, etc.)"),
                        tags$li(strong("AFINN Sentiment:"), "Quantitative scoring (-5 to +5)"),
                        tags$li(strong("Bing Sentiment:"), "Binary positive/negative classification"),
                        tags$li(strong("Combined Score:"), "Averaged across all three methods")
                      )),
              tags$li(strong("Statistical Correlation Analysis"),
                      tags$ul(
                        tags$li("Pearson correlation coefficients"),
                        tags$li("Spearman rank correlations"),
                        tags$li("Lead-lag analysis (1-3 day lags)"),
                        tags$li("Linear regression modeling")
                      )),
              tags$li(strong("Visualization & Interactive Dashboard"),
                      tags$ul(
                        tags$li("15+ interactive plots using Plotly"),
                        tags$li("Real-time filtering by date range and cryptocurrency"),
                        tags$li("Dynamic data tables with search and sorting")
                      ))
            ),
            hr(),
            
            h3("Key Findings"),
            tags$ul(
              tags$li(strong("Bitcoin:"), "Moderate positive correlation (0.351) between sentiment and price"),
              tags$li(strong("Ethereum:"), "Weak positive correlation (0.227)"),
              tags$li(strong("Dogecoin:"), "Weak positive correlation (0.149)"),
              tags$li(strong("Fear-Volatility:"), "Strong correlation (0.42) - fear emotion predicts market volatility"),
              tags$li(strong("Causality:"), "Sentiment is primarily reactive rather than predictive")
            ),
            hr(),
            
            h3("Interactive Features"),
            tags$ul(
              tags$li(strong("Date Range Selector:"), "Filter all visualizations to specific time periods"),
              tags$li(strong("Cryptocurrency Selector:"), "Switch between Bitcoin, Ethereum, and Dogecoin"),
              tags$li(strong("Interactive Plots:"), "Hover for details, zoom, pan, and export functionality"),
              tags$li(strong("Sortable Tables:"), "Search, sort, and explore detailed data")
            ),
            hr(),
            
            h3("Technical Stack"),
            tags$ul(
              tags$li(strong("Language:"), "R (version 4.0+)"),
              tags$li(strong("Packages:"), "shiny, shinydashboard, tidyverse, plotly, DT, syuzhet, lubridate"),
              tags$li(strong("Visualization:"), "ggplot2, plotly"),
              tags$li(strong("Sentiment Analysis:"), "syuzhet (NRC, AFINN, Bing lexicons)")
            ),
            hr(),
            
            h3("Team & Course Information"),
            p(strong("Course:"), "MTH208 - Data Science Lab I"),
            p(strong("Institution:"), "Indian Institute of Technology Kanpur"),
            p(strong("Team:"), "Team 1"),
            p(strong("Date:"), "October-November 2025"),
            p(strong("GitHub:"), HTML('<a href="https://github.com/Devdharpatil/crypto-sentiment-analysis-MTH208" target="_blank">crypto-sentiment-analysis-MTH208</a>')),
            hr(),
            
            h3("Limitations & Future Work"),
            tags$ul(
              tags$li("Short time period (38 days) - limited to bearish market phase"),
              tags$li("Single platform (Reddit) - excludes Twitter, Telegram, Discord"),
              tags$li("Lexicon-based sentiment may miss sarcasm and context"),
              tags$li("Future work: Longer time series, multi-platform analysis, deep learning models")
            )
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  filtered_daily <- reactive({
    daily_sentiment %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
  })
  
  filtered_merged <- reactive({
    merged_data %>%
      filter(
        cryptocurrency == input$crypto_select,
        date >= input$date_range[1] & date <= input$date_range[2]
      )
  })
  
  # TAB 1: OVERVIEW
  output$overview_sentiment_dist <- renderPlotly({
    sentiment_counts <- sentiment_data %>%
      count(sentiment_label) %>%
      mutate(percentage = (n / sum(n)) * 100)
    
    plot_ly(
      sentiment_counts,
      labels = ~sentiment_label,
      values = ~n,
      type = "pie",
      marker = list(colors = c("negative" = "#E74C3C", "neutral" = "#95A5A6", "positive" = "#27AE60")),
      textinfo = "label+percent"
    ) %>% layout(title = "")
  })
  
  output$overview_daily_activity <- renderPlotly({
    plot_ly(filtered_daily(), x = ~date, y = ~total_posts, type = "scatter", mode = "lines",
            line = list(color = "#3498DB", width = 2)) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Number of Posts"))
  })
  
  # TAB 2: SENTIMENT
  output$sentiment_trend <- renderPlotly({
    plot_ly(filtered_daily(), x = ~date) %>%
      add_lines(y = ~sentiment_mean, name = "Mean Sentiment", line = list(color = "#3498DB", width = 2)) %>%
      add_ribbons(ymin = ~sentiment_mean - sentiment_sd, ymax = ~sentiment_mean + sentiment_sd,
                  name = "±1 SD", fillcolor = "rgba(52, 152, 219, 0.2)", line = list(color = "transparent")) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Sentiment Score"))
  })
  
  output$emotion_dist <- renderPlotly({
    emotion_totals <- filtered_daily() %>%
      summarise(
        Joy = sum(joy_total, na.rm = TRUE),
        Trust = sum(trust_total, na.rm = TRUE),
        Fear = sum(fear_total, na.rm = TRUE),
        Anger = sum(anger_total, na.rm = TRUE)
      ) %>%
      pivot_longer(everything(), names_to = "emotion", values_to = "count")
    
    plot_ly(emotion_totals, x = ~reorder(emotion, count), y = ~count, type = "bar",
            marker = list(color = "#9B59B6")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Total Count"))
  })
  
  output$sentiment_by_type <- renderPlotly({
    sent_by_type <- filtered_daily() %>%
      select(date, positive_count, negative_count, neutral_count) %>%
      pivot_longer(-date, names_to = "type", values_to = "count")
    
    plot_ly(sent_by_type, x = ~date, y = ~count, color = ~type, type = "scatter", mode = "lines",
            colors = c("#27AE60", "#E74C3C", "#95A5A6")) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Count"))
  })
  
  # TAB 3: PRICES
  output$price_trends <- renderPlotly({
    price_filtered <- price_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    plot_ly(price_filtered, x = ~date, y = ~price, color = ~cryptocurrency,
            type = "scatter", mode = "lines",
            colors = c("btc" = "#F7931A", "eth" = "#627EEA", "doge" = "#C2A633")) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Price (USD)", type = "log"))
  })
  
  output$price_returns <- renderPlotly({
    crypto_data <- price_data %>%
      filter(cryptocurrency == input$crypto_select, date >= input$date_range[1] & date <= input$date_range[2])
    
    plot_ly(crypto_data, x = ~date, y = ~price_change_pct, type = "bar",
            marker = list(color = ~ifelse(price_change_pct >= 0, "#27AE60", "#E74C3C"))) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Daily Return (%)"))
  })
  
  output$price_volatility <- renderPlotly({
    crypto_data <- price_data %>%
      filter(cryptocurrency == input$crypto_select, date >= input$date_range[1] & date <= input$date_range[2])
    
    plot_ly(crypto_data, x = ~date, y = ~volatility_7d, type = "scatter", mode = "lines",
            line = list(color = "#E74C3C", width = 2)) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "7-Day Volatility (%)"))
  })
  
  # TAB 4: CORRELATION
  output$correlation_overlay <- renderPlotly({
    data <- filtered_merged() %>%
      mutate(
        price_norm = (price - mean(price, na.rm = TRUE)) / sd(price, na.rm = TRUE),
        sentiment_norm = (sentiment_mean - mean(sentiment_mean, na.rm = TRUE)) / sd(sentiment_mean, na.rm = TRUE)
      )
    
    plot_ly(data, x = ~date) %>%
      add_lines(y = ~sentiment_norm, name = "Sentiment", line = list(color = "#3498DB", width = 2)) %>%
      add_lines(y = ~price_norm, name = "Price", line = list(color = "#E74C3C", width = 2)) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Normalized Value (z-score)"))
  })
  
  output$correlation_scatter <- renderPlotly({
    data <- filtered_merged() %>%
      select(sentiment_mean, price_change_pct) %>%
      na.omit()
    
    if (nrow(data) < 5) {
      plot_ly() %>% layout(title = "Not enough data points")
    } else {
      model <- lm(price_change_pct ~ sentiment_mean, data = data)
      data$fitted <- fitted(model)
      
      plot_ly(data, x = ~sentiment_mean, y = ~price_change_pct, type = "scatter", mode = "markers",
              marker = list(color = "#3498DB", size = 10, opacity = 0.6), name = "Data Points") %>%
        add_lines(x = ~sentiment_mean, y = ~fitted, line = list(color = "#E74C3C", width = 2), name = "Trend Line") %>%
        layout(xaxis = list(title = "Daily Sentiment"), yaxis = list(title = "Price Change (%)"))
    }
  })
  
  output$correlation_stats <- renderPrint({
    data <- filtered_merged() %>%
      select(sentiment_mean, price, price_change_pct) %>%
      na.omit()
    
    cat("Correlation Analysis\n====================\n\n")
    
    if (nrow(data) < 5) {
      cat("Not enough data points\n")
    } else {
      cor_sent_price <- cor(data$sentiment_mean, data$price, use = "complete.obs")
      cor_sent_change <- cor(data$sentiment_mean, data$price_change_pct, use = "complete.obs")
      
      cat(sprintf("Sentiment vs Price: %.4f\n", cor_sent_price))
      cat(sprintf("Sentiment vs Price Change: %.4f\n\n", cor_sent_change))
      cat(sprintf("Observations: %d\n\n", nrow(data)))
      
      model <- lm(price_change_pct ~ sentiment_mean, data = data)
      cat(sprintf("Regression R²: %.4f\n", summary(model)$r.squared))
      
      p_value <- summary(model)$coefficients[2, 4]
      cat(sprintf("P-value: %.4f", p_value))
      if (p_value < 0.05) cat(" (Significant!)\n") else cat(" (Not Significant)\n")
    }
  })
  
  # TAB 5: TOP POSTS
  output$top_positive_table <- renderDT({
    sentiment_data %>%
      filter(is_post) %>%
      arrange(desc(combined_sentiment)) %>%
      head(10) %>%
      select(Date = date, Title = title, Sentiment = combined_sentiment, Score = score) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE, dom = 't'), rownames = FALSE) %>%
      formatRound("Sentiment", 3)
  })
  
  output$top_negative_table <- renderDT({
    sentiment_data %>%
      filter(is_post) %>%
      arrange(combined_sentiment) %>%
      head(10) %>%
      select(Date = date, Title = title, Sentiment = combined_sentiment, Score = score) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE, dom = 't'), rownames = FALSE) %>%
      formatRound("Sentiment", 3)
  })
  
  # TAB 6: DATA TABLE
  output$data_table <- renderDT({
    filtered_merged() %>%
      select(Date = date, Sentiment = sentiment_mean, `Pos` = positive_count, `Neg` = negative_count,
             Price = price, `Δ%` = price_change_pct, Volatility = volatility_7d) %>%
      datatable(options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(c("Sentiment", "Price", "Δ%", "Volatility"), 2)
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
