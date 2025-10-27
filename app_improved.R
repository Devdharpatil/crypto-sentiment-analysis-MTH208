# Cryptocurrency Sentiment Analysis Dashboard (IMPROVED)
# Interactive Shiny application with bug fixes and enhancements

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)

# ============================================================================
# LOAD DATA
# ============================================================================

# Load all data files
sentiment_data <- read_csv("../data/reddit_with_sentiment.csv", show_col_types = FALSE)
daily_sentiment <- read_csv("../data/daily_sentiment.csv", show_col_types = FALSE)
price_data <- read_csv("../data/crypto_prices_2021.csv", show_col_types = FALSE)
merged_data <- read_csv("../data/sentiment_price_merged.csv", show_col_types = FALSE)

# Convert dates
sentiment_data$date <- as.Date(sentiment_data$date)
daily_sentiment$date <- as.Date(daily_sentiment$date)
price_data$date <- as.Date(price_data$date)
merged_data$date <- as.Date(merged_data$date)

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = "Crypto Sentiment Analysis",
    titleWidth = 300
  ),

  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("📊 Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("🧠 Sentiment Analysis", tabName = "sentiment", icon = icon("brain")),
      menuItem("💰 Price Analysis", tabName = "prices", icon = icon("chart-line")),
      menuItem("🔗 Correlation", tabName = "correlation", icon = icon("link")),
      menuItem("📈 Top Posts", tabName = "posts", icon = icon("list")),
      menuItem("📋 Data Explorer", tabName = "data", icon = icon("table"))
    ),

    hr(),

    # Date range filter
    dateRangeInput(
      "date_range",
      "Date Range:",
      start = min(daily_sentiment$date),
      end = max(daily_sentiment$date),
      min = min(daily_sentiment$date),
      max = max(daily_sentiment$date)
    ),

    # Cryptocurrency filter
    selectInput(
      "crypto_select",
      "Cryptocurrency:",
      choices = c("Bitcoin" = "btc", "Ethereum" = "eth", "Dogecoin" = "doge"),
      selected = "btc"
    )
  ),

  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box-header { font-weight: bold; }
        .info-box { min-height: 90px; }
        .content-wrapper { background-color: #ecf0f5; }
      "))
    ),

    tabItems(
      # ========== TAB 1: OVERVIEW ==========
      tabItem(
        tabName = "overview",
        h2("Project Overview"),

        fluidRow(
          infoBox(
            "Total Posts/Comments",
            format(nrow(sentiment_data), big.mark = ","),
            icon = icon("comments"),
            color = "blue",
            width = 3
          ),
          infoBox(
            "Analysis Period",
            paste(min(daily_sentiment$date), "to", max(daily_sentiment$date)),
            icon = icon("calendar"),
            color = "green",
            width = 3
          ),
          infoBox(
            "Avg Sentiment",
            sprintf("%.3f", mean(sentiment_data$combined_sentiment, na.rm = TRUE)),
            icon = icon("smile"),
            color = "yellow",
            width = 3
          ),
          infoBox(
            "Cryptocurrencies",
            "BTC, ETH, DOGE",
            icon = icon("bitcoin"),
            color = "orange",
            width = 3
          )
        ),

        fluidRow(
          box(
            title = "Sentiment Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("overview_sentiment_dist")
          ),
          box(
            title = "Daily Activity",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("overview_daily_activity")
          )
        ),

        fluidRow(
          box(
            title = "Key Correlation Findings",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            HTML("<div style=\'padding: 10px;\'>
                    <h4>Sentiment-Price Correlations (Aug-Sep 2021)</h4>
                    <div style=\'display: flex; justify-content: space-around; margin-top: 15px;\'>
                      <div style=\'text-align: center;\'>
                        <h3 style=\'color: #F7931A; margin: 0;\'>Bitcoin</h3>
                        <p style=\'font-size: 24px; font-weight: bold; margin: 5px 0;\'>0.351</p>
                        <p style=\'color: #666; margin: 0;\'>Moderate Positive</p>
                      </div>
                      <div style=\'text-align: center;\'>
                        <h3 style=\'color: #627EEA; margin: 0;\'>Ethereum</h3>
                        <p style=\'font-size: 24px; font-weight: bold; margin: 5px 0;\'>0.227</p>
                        <p style=\'color: #666; margin: 0;\'>Weak Positive</p>
                      </div>
                      <div style=\'text-align: center;\'>
                        <h3 style=\'color: #C2A633; margin: 0;\'>Dogecoin</h3>
                        <p style=\'font-size: 24px; font-weight: bold; margin: 5px 0;\'>0.149</p>
                        <p style=\'color: #666; margin: 0;\'>Weak Positive</p>
                      </div>
                    </div>
                    <hr>
                    <p><strong>Fear-Volatility Correlation:</strong> 0.42 (All cryptos) - Fear strongly predicts market volatility!</p>
                  </div>")
          )
        ),

        fluidRow(
          box(
            title = "Project Description",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            HTML("<p><strong>Research Question:</strong> Does Reddit sentiment predict cryptocurrency price movements?</p>
                  <p><strong>Data Sources:</strong></p>
                  <ul>
                    <li>Reddit r/CryptoCurrency: 40,918 posts/comments (Aug-Sep 2021)</li>
                    <li>CryptoCompare API: Historical BTC, ETH, DOGE prices (Aug-Sep 2021)</li>
                  </ul>
                  <p><strong>Methods:</strong> NRC Emotion Analysis, AFINN Sentiment, Pearson/Spearman Correlation</p>")
          )
        )
      ),

      # ========== TAB 2: SENTIMENT ANALYSIS ==========
      tabItem(
        tabName = "sentiment",
        h2("Sentiment Analysis"),

        fluidRow(
          box(
            title = "Daily Sentiment Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("sentiment_trend", height = "400px")
          )
        ),

        fluidRow(
          box(
            title = "Emotion Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("emotion_dist")
          ),
          box(
            title = "Sentiment by Type",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("sentiment_by_type")
          )
        )
      ),

      # ========== TAB 3: PRICE ANALYSIS ==========
      tabItem(
        tabName = "prices",
        h2("Cryptocurrency Price Analysis"),

        fluidRow(
          box(
            title = "Price Trends (2021)",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("price_trends", height = "400px")
          )
        ),

        fluidRow(
          box(
            title = "Daily Returns",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("price_returns")
          ),
          box(
            title = "Volatility",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("price_volatility")
          )
        )
      ),

      # ========== TAB 4: CORRELATION ==========
      tabItem(
        tabName = "correlation",
        h2("Sentiment-Price Correlation"),

        fluidRow(
          box(
            title = "Sentiment vs Price (Normalized)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("correlation_overlay", height = "400px")
          )
        ),

        fluidRow(
          box(
            title = "Correlation Scatter",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("correlation_scatter")
          ),
          box(
            title = "Correlation Statistics",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("correlation_stats")
          )
        )
      ),

      # ========== TAB 5: TOP POSTS ==========
      tabItem(
        tabName = "posts",
        h2("Top Posts by Sentiment"),

        fluidRow(
          box(
            title = "Most Positive Posts",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("top_positive_table")
          )
        ),

        fluidRow(
          box(
            title = "Most Negative Posts",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            DTOutput("top_negative_table")
          )
        )
      ),

      # ========== TAB 6: DATA EXPLORER ==========
      tabItem(
        tabName = "data",
        h2("Data Explorer"),

        fluidRow(
          box(
            title = "Merged Sentiment-Price Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_table")
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {

  # Reactive data filtering
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

  # ========== TAB 1: OVERVIEW PLOTS ==========

  output$overview_sentiment_dist <- renderPlotly({
    sentiment_counts <- sentiment_data %>%
      count(sentiment_label) %>%
      mutate(percentage = n / sum(n) * 100)

    plot_ly(
      sentiment_counts,
      labels = ~sentiment_label,
      values = ~n,
      type = "pie",
      marker = list(
        colors = c("negative" = "#E74C3C", "neutral" = "#95A5A6", "positive" = "#27AE60")
      ),
      textinfo = "label+percent",
      hovertemplate = paste("<b>%{label}</b><br>",
                            "Count: %{value:,}<br>",
                            "Percentage: %{percent}<extra></extra>")
    ) %>%
      layout(title = "")
  })

  output$overview_daily_activity <- renderPlotly({
    plot_ly(filtered_daily(), x = ~date, y = ~total_posts, type = "scatter", mode = "lines",
            line = list(color = "#3498DB", width = 2),
            hovertemplate = paste("<b>%{x}</b><br>",
                                  "Posts: %{y:,}<extra></extra>")) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Number of Posts")
      )
  })

  # ========== TAB 2: SENTIMENT PLOTS ==========

  output$sentiment_trend <- renderPlotly({
    plot_ly(filtered_daily(), x = ~date) %>%
      add_lines(y = ~sentiment_mean, name = "Mean Sentiment", 
                line = list(color = "#3498DB", width = 2),
                hovertemplate = paste("<b>%{x}</b><br>",
                                      "Sentiment: %{y:.3f}<extra></extra>")) %>%
      add_ribbons(
        ymin = ~sentiment_mean - sentiment_sd,
        ymax = ~sentiment_mean + sentiment_sd,
        name = "±1 SD",
        fillcolor = "rgba(52, 152, 219, 0.2)",
        line = list(color = "transparent"),
        hoverinfo = "skip"
      ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Sentiment Score"),
        hovermode = "x unified"
      )
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

    plot_ly(emotion_totals, x = ~reorder(emotion, count), y = ~count, 
            type = "bar", marker = list(color = "#9B59B6"),
            hovertemplate = paste("<b>%{x}</b><br>",
                                  "Count: %{y:,}<extra></extra>")) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Total Count")
      )
  })

  output$sentiment_by_type <- renderPlotly({
    sent_by_type <- filtered_daily() %>%
      select(date, positive_count, negative_count, neutral_count) %>%
      pivot_longer(-date, names_to = "type", values_to = "count")

    plot_ly(sent_by_type, x = ~date, y = ~count, color = ~type, type = "scatter",
            mode = "lines", colors = c("#27AE60", "#E74C3C", "#95A5A6"),
            hovertemplate = paste("<b>%{x}</b><br>",
                                  "%{data.name}: %{y}<extra></extra>")) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Count"),
        hovermode = "x unified"
      )
  })

  # ========== TAB 3: PRICE PLOTS ==========

  output$price_trends <- renderPlotly({
    price_filtered <- price_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])

    plot_ly(price_filtered, x = ~date, y = ~price, color = ~cryptocurrency,
            type = "scatter", mode = "lines",
            colors = c("btc" = "#F7931A", "eth" = "#627EEA", "doge" = "#C2A633"),
            hovertemplate = paste("<b>%{x}</b><br>",
                                  "%{data.name}: $%{y:,.2f}<extra></extra>")) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price (USD)", type = "log"),
        hovermode = "x unified"
      )
  })

  output$price_returns <- renderPlotly({
    crypto_data <- price_data %>%
      filter(
        cryptocurrency == input$crypto_select,
        date >= input$date_range[1] & date <= input$date_range[2]
      )

    plot_ly(crypto_data, x = ~date, y = ~price_change_pct, type = "bar",
            marker = list(color = ~ifelse(price_change_pct > 0, "#27AE60", "#E74C3C")),
            hovertemplate = paste("<b>%{x}</b><br>",
                                  "Return: %{y:.2f}%<extra></extra>")) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Daily Return (%)")
      )
  })

  output$price_volatility <- renderPlotly({
    crypto_data <- price_data %>%
      filter(
        cryptocurrency == input$crypto_select,
        date >= input$date_range[1] & date <= input$date_range[2]
      )

    plot_ly(crypto_data, x = ~date, y = ~volatility_7d, type = "scatter",
            mode = "lines", line = list(color = "#E74C3C", width = 2),
            hovertemplate = paste("<b>%{x}</b><br>",
                                  "Volatility: %{y:.2f}%<extra></extra>")) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "7-Day Volatility (%)")
      )
  })

  # ========== TAB 4: CORRELATION PLOTS ==========

  output$correlation_overlay <- renderPlotly({
    data <- filtered_merged() %>%
      mutate(
        price_norm = (price - mean(price, na.rm = TRUE)) / sd(price, na.rm = TRUE),
        sentiment_norm = (sentiment_mean - mean(sentiment_mean, na.rm = TRUE)) / 
                         sd(sentiment_mean, na.rm = TRUE)
      )

    plot_ly(data, x = ~date) %>%
      add_lines(y = ~sentiment_norm, name = "Sentiment", 
                line = list(color = "#3498DB", width = 2),
                hovertemplate = paste("<b>%{x}</b><br>",
                                      "Sentiment (z-score): %{y:.2f}<extra></extra>")) %>%
      add_lines(y = ~price_norm, name = "Price", 
                line = list(color = "#E74C3C", width = 2),
                hovertemplate = paste("<b>%{x}</b><br>",
                                      "Price (z-score): %{y:.2f}<extra></extra>")) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Normalized Value (z-score)"),
        hovermode = "x unified"
      )
  })

  output$correlation_scatter <- renderPlotly({
    # FIXED: Remove NA values before plotting
    data <- filtered_merged() %>%
      select(sentiment_mean, price_change_pct) %>%
      na.omit()

    # Only plot if we have enough data points
    if (nrow(data) < 5) {
      plot_ly() %>%
        layout(
          title = "Not enough data points for correlation plot",
          xaxis = list(title = "Daily Sentiment"),
          yaxis = list(title = "Price Change (%)")
        )
    } else {
      # Calculate regression line
      model <- lm(price_change_pct ~ sentiment_mean, data = data)
      data$fitted <- fitted(model)

      plot_ly(data, x = ~sentiment_mean, y = ~price_change_pct,
              type = "scatter", mode = "markers",
              marker = list(color = "#3498DB", size = 10, opacity = 0.6),
              name = "Data Points",
              hovertemplate = paste("Sentiment: %{x:.3f}<br>",
                                    "Price Change: %{y:.2f}%<extra></extra>")) %>%
        add_lines(x = ~sentiment_mean, y = ~fitted,
                  line = list(color = "#E74C3C", width = 2),
                  name = "Trend Line",
                  hoverinfo = "skip") %>%
        layout(
          xaxis = list(title = "Daily Sentiment"),
          yaxis = list(title = "Price Change (%)"),
          showlegend = TRUE
        )
    }
  })

  output$correlation_stats <- renderPrint({
    data <- filtered_merged() %>%
      select(sentiment_mean, price, price_change_pct, volatility_7d) %>%
      na.omit()

    cat("Correlation Analysis\n")
    cat("===================\n\n")

    if (nrow(data) < 5) {
      cat("Not enough data points for correlation analysis\n")
      cat(sprintf("Observations: %d (minimum 5 required)\n", nrow(data)))
    } else {
      cor_sent_price <- cor(data$sentiment_mean, data$price, use = "complete.obs")
      cor_sent_change <- cor(data$sentiment_mean, data$price_change_pct, use = "complete.obs")

      cat(sprintf("Sentiment vs Price: %.4f\n", cor_sent_price))
      cat(sprintf("Sentiment vs Price Change: %.4f\n", cor_sent_change))
      cat(sprintf("\nObservations: %d\n", nrow(data)))

      # Linear regression
      model <- lm(price_change_pct ~ sentiment_mean, data = data)
      cat(sprintf("\nRegression R²: %.4f\n", summary(model)$r.squared))

      p_value <- summary(model)$coefficients[2,4]
      cat(sprintf("P-value: %.4f", p_value))

      if (p_value < 0.05) {
        cat(" (Statistically Significant!)\n")
      } else {
        cat(" (Not Significant)\n")
      }
    }
  })

  # ========== TAB 5: TOP POSTS TABLES ==========

  output$top_positive_table <- renderDT({
    sentiment_data %>%
      filter(is_post) %>%
      arrange(desc(combined_sentiment)) %>%
      head(10) %>%
      select(Date = date, Title = title, Sentiment = combined_sentiment, Score = score) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE, dom = 't'),
        rownames = FALSE
      ) %>%
      formatRound("Sentiment", 3)
  })

  output$top_negative_table <- renderDT({
    sentiment_data %>%
      filter(is_post) %>%
      arrange(combined_sentiment) %>%
      head(10) %>%
      select(Date = date, Title = title, Sentiment = combined_sentiment, Score = score) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE, dom = 't'),
        rownames = FALSE
      ) %>%
      formatRound("Sentiment", 3)
  })

  # ========== TAB 6: DATA TABLE ==========

  output$data_table <- renderDT({
    filtered_merged() %>%
      select(
        Date = date,
        Sentiment = sentiment_mean,
        `Pos Posts` = positive_count,
        `Neg Posts` = negative_count,
        Price = price,
        `Price Δ%` = price_change_pct,
        Volatility = volatility_7d
      ) %>%
      datatable(
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE
      ) %>%
      formatRound(c("Sentiment", "Price", "Price Δ%", "Volatility"), 2)
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
