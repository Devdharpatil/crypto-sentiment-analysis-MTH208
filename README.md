# Cryptocurrency Sentiment Analysis Project

## Team 1 - MTH208 Data Science Lab I
**IIT Kanpur | October 2025**

---

## 📊 Project Overview

This project analyzes the relationship between Reddit sentiment and cryptocurrency price movements during August-September 2021, a period of significant market volatility. Using natural language processing and statistical correlation analysis, we investigate whether social media sentiment can predict or explain cryptocurrency price changes.

### Research Questions
1. **Does Reddit sentiment correlate with cryptocurrency prices?**
2. **Can social media sentiment predict price movements?**
3. **Which emotions (joy, fear, trust) are most associated with market volatility?**

---

## 🎯 Key Findings

### Sentiment-Price Correlations
- **Bitcoin (BTC):** 0.351 - **Moderate positive correlation**
- **Ethereum (ETH):** 0.227 - Weak-to-moderate positive correlation
- **Dogecoin (DOGE):** 0.149 - Weak positive correlation

### Fear-Volatility Relationship
- **All cryptocurrencies:** 0.42 - **Strong correlation** between fear emotion and market volatility
- **Interpretation:** Reddit users expressing fear is a strong predictor of increased price volatility

### Lead-Lag Analysis
- **Sentiment is more reactive than predictive** - Reddit users primarily respond to price changes rather than predicting them
- **2-day lag effect observed:** Sentiment from 2 days ago shows moderate predictive power for Dogecoin prices (correlation: 0.530)

### Market Context
Analysis period (Aug-Sep 2021) was a **bearish correction phase:**
- Bitcoin: -8.37% ($47,833 → $43,829)
- Ethereum: -9.73% ($3,324 → $3,001)
- Dogecoin: -28.83% ($0.29 → $0.20)

---

## 📁 Data Sources

### Reddit Data
- **Source:** Kaggle - r/CryptoCurrency Dataset
- **Link:** https://www.kaggle.com/datasets/paultimothymooney/cryptocurrency-reddit
- **Records:** 40,918 posts and comments
- **Period:** August 13 - September 19, 2021
- **Subreddit:** r/CryptoCurrency
- **Content:** Post titles, selftext, comments, scores, timestamps

### Historical Price Data
- **Source:** CryptoCompare API
- **Cryptocurrencies:** Bitcoin (BTC), Ethereum (ETH), Dogecoin (DOGE)
- **Period:** August 13 - September 30, 2021
- **Metrics:** OHLC prices, trading volume, 7-day rolling volatility
- **Records:** 147 daily price points (49 days × 3 cryptos)

---

## 🔬 Methodology

### 1. Data Acquisition
- Reddit data extracted from Kaggle CSV dataset
- Historical cryptocurrency prices fetched via CryptoCompare API
- Date alignment: Common period (Aug 13 - Sep 19, 2021)

### 2. Exploratory Data Analysis (EDA)
- **Temporal patterns:** Daily post volume, weekday vs weekend activity
- **Engagement metrics:** Score distributions, comment patterns
- **Content analysis:** Post length, title characteristics

### 3. Sentiment Analysis
**Three-Method Approach:**
- **NRC Emotion Lexicon:** Detects 8 emotions (joy, sadness, anger, fear, trust, anticipation, surprise, disgust)
- **AFINN:** Sentiment scoring from -5 (very negative) to +5 (very positive)
- **Bing:** Binary positive/negative classification

**Combined Sentiment Score:** Average of normalized scores from all three methods

**Results:**
- Overall sentiment: 0.150 (slightly positive)
- Positive posts: 41.6%
- Neutral posts: 45.2%
- Negative posts: 13.2%
- Dominant emotion: Trust (70,921 occurrences)

### 4. Statistical Correlation Analysis
- **Pearson correlation:** Linear relationship between sentiment and prices
- **Spearman correlation:** Rank-based, robust to outliers
- **Lead-lag analysis:** Time-shifted correlations (1-3 day lags)
- **Key findings:** Moderate BTC correlation (0.351), strong fear-volatility link (0.42)

### 5. Visualization & Dashboard
- Interactive Shiny dashboard with 6 tabs
- 15+ professional visualizations
- Real-time filtering by date and cryptocurrency
- Data export capabilities

---

## 🗂️ File Structure

```
project/
│
├── data/                           # Processed datasets
│   ├── reddit_processed.csv        # Cleaned Reddit data
│   ├── reddit_with_sentiment.csv   # Reddit data with sentiment scores
│   ├── daily_sentiment.csv         # Daily aggregated sentiment
│   ├── crypto_prices_2021.csv      # Combined historical prices
│   ├── btc_prices_2021.csv         # Bitcoin prices
│   ├── eth_prices_2021.csv         # Ethereum prices
│   ├── doge_prices_2021.csv        # Dogecoin prices
│   ├── sentiment_price_merged.csv  # Merged sentiment + prices
│   └── price_summary_2021.csv      # Price statistics
│
├── plots/                          # Visualizations (15 PNG files)
│   ├── temporal_analysis.png       # EDA: Time patterns
│   ├── engagement_analysis.png     # EDA: User engagement
│   ├── content_analysis.png        # EDA: Post characteristics
│   ├── sentiment_analysis.png      # Sentiment distributions
│   ├── price_analysis_2021.png     # Price trends & volatility
│   ├── correlation_heatmap.png     # Correlation matrix
│   ├── sentiment_price_btc.png     # BTC sentiment-price overlay
│   ├── sentiment_price_eth.png     # ETH sentiment-price overlay
│   ├── sentiment_price_doge.png    # DOGE sentiment-price overlay
│   └── scatter_sentiment_price.png # Correlation scatter plots
│
├── results/                        # Analysis outputs (RDS files)
│   ├── eda_results.rds             # EDA statistics
│   ├── sentiment_analysis.rds      # Sentiment analysis results
│   ├── price_data_2021.rds         # Historical price data
│   └── correlation_analysis.rds    # Correlation statistics
│
├── app/                            # Shiny dashboard
│   └── app.R                       # Interactive dashboard code
│
├── eda_crypto_reddit.R             # Exploratory Data Analysis script
├── sentiment_analysis.R            # Sentiment computation script
├── get_historical_prices_2021_alternative.R  # Price data collection
├── correlation_analysis.R          # Statistical correlation analysis
│
├── README.md                       # This file
└── FINAL_REPORT.pdf               # Detailed project report

```

---

## 🚀 How to Run

### Prerequisites
```r
# Install required R packages
install.packages(c(
  "tidyverse",      # Data manipulation
  "syuzhet",        # Sentiment analysis
  "sentimentr",     # Sentence-level sentiment
  "textdata",       # Sentiment lexicons
  "tidytext",       # Text mining
  "httr",           # HTTP requests
  "jsonlite",       # JSON parsing
  "corrplot",       # Correlation plots
  "shiny",          # Interactive dashboards
  "shinydashboard", # Dashboard layout
  "plotly",         # Interactive plots
  "DT",             # Data tables
  "gridExtra",      # Multiple plots
  "lubridate",      # Date handling
  "scales",         # Scale formatting
  "zoo"             # Rolling calculations
))
```

### Step-by-Step Execution

**1. Exploratory Data Analysis**
```r
setwd("path/to/project")
source("eda_crypto_reddit.R")
```
- Processes Reddit data
- Generates 3 EDA visualizations
- Creates summary statistics

**2. Sentiment Analysis**
```r
source("sentiment_analysis.R")
```
- Runs NRC, AFINN, Bing sentiment methods
- Creates combined sentiment scores
- Aggregates daily sentiment metrics
- Runtime: ~15-20 minutes (processes 40K+ texts)

**3. Historical Price Data Collection**
```r
source("get_historical_prices_2021_alternative.R")
```
- Fetches BTC, ETH, DOGE prices from CryptoCompare API
- Covers August-September 2021
- Calculates volatility and returns
- Runtime: ~3-5 minutes

**4. Correlation Analysis**
```r
source("correlation_analysis.R")
```
- Merges sentiment and price data
- Calculates Pearson/Spearman correlations
- Performs lead-lag analysis
- Generates correlation visualizations
- Runtime: ~2-3 minutes

**5. Launch Interactive Dashboard**
```r
shiny::runApp("app")
```
- Opens interactive dashboard in web browser
- 6 tabs with 15+ visualizations
- Real-time filtering and exploration
- Press ESC or close browser to stop

---

## 📊 Technologies & Libraries

### R Programming (v4.5+)
**Data Processing:**
- `tidyverse` - Data manipulation and visualization
- `lubridate` - Date/time handling
- `zoo` - Time series operations

**Sentiment Analysis:**
- `syuzhet` - NRC Emotion Lexicon, AFINN
- `sentimentr` - Sentence-level sentiment
- `tidytext` - Text mining and tokenization
- `textdata` - Sentiment lexicon data

**API & Data Collection:**
- `httr` - HTTP requests
- `jsonlite` - JSON parsing

**Statistical Analysis:**
- `corrplot` - Correlation matrices
- Base R `cor()`, `lm()` functions

**Visualization:**
- `ggplot2` - Static plots
- `plotly` - Interactive visualizations
- `gridExtra` - Multiple plot layouts

**Dashboard:**
- `shiny` - Interactive web applications
- `shinydashboard` - Dashboard framework
- `DT` - Interactive data tables

---

## 📈 Key Results & Interpretation

### 1. Sentiment-Price Relationship

**Bitcoin (0.351 correlation):**
- **Strongest sentiment-price relationship** among the three cryptocurrencies
- Moderate positive correlation suggests Reddit sentiment reflects market psychology
- As sentiment increases by 1 standard deviation, Bitcoin price tends to increase by ~0.35 SD

**Ethereum (0.227 correlation):**
- Weaker than Bitcoin but still positively correlated
- Less influenced by Reddit sentiment

**Dogecoin (0.149 correlation):**
- **Surprisingly weak** given its meme status
- Likely driven more by specific influencers (e.g., Elon Musk tweets) than general Reddit sentiment

### 2. Fear as Volatility Predictor

**Fear-Volatility correlation: 0.42 (all cryptos)**
- **Strongest finding of the study**
- When Reddit users express more fear, market volatility increases significantly
- Consistent with behavioral finance theory
- **Practical application:** Fear sentiment could serve as early warning signal for market turbulence

### 3. Sentiment Lags Price (Not Leads)

**Lead-Lag Analysis:**
- Price→Sentiment correlations consistently positive (0.1-0.4)
- Sentiment→Price correlations weaker and inconsistent
- **Interpretation:** Reddit users primarily **react to** price changes rather than predicting them
- **Exception:** 2-day lagged sentiment shows moderate predictive power for Dogecoin (0.530)

### 4. Academic Implications

**Main Conclusion:**
Social media sentiment reflects market psychology but is **not a strong standalone predictor** of cryptocurrency prices. For effective trading strategies, sentiment should be combined with:
- Technical indicators (moving averages, RSI, MACD)
- Fundamental analysis (adoption metrics, development activity)
- Macroeconomic factors (regulations, institutional adoption)

---

## 🎓 Academic Context

This project demonstrates practical application of:
- **Natural Language Processing (NLP)** - Text preprocessing, sentiment extraction
- **Time Series Analysis** - Price movements, volatility calculation, lead-lag relationships
- **Statistical Inference** - Correlation analysis, hypothesis testing
- **Data Visualization** - Effective communication of complex relationships
- **Reproducible Research** - Well-documented code, clear methodology

---

## 📅 Project Timeline

- **Week 1-2:** Data collection and preprocessing
- **Week 3:** Exploratory Data Analysis
- **Week 4:** Sentiment analysis implementation
- **Week 5:** Correlation analysis and statistical testing
- **Week 6:** Dashboard development
- **Week 7:** Report writing and presentation preparation

---

## 🔗 References

1. **Reddit Data Source:** Kaggle - r/CryptoCurrency Dataset
   - https://www.kaggle.com/datasets/paultimothymooney/cryptocurrency-reddit

2. **CryptoCompare API Documentation**
   - https://min-api.cryptocompare.com/documentation

3. **NRC Emotion Lexicon:**
   - Mohammad, S. M., & Turney, P. D. (2013). Crowdsourcing a word-emotion association lexicon. *Computational Intelligence*, 29(3), 436-465.

4. **AFINN Lexicon:**
   - Nielsen, F. Å. (2011). A new ANEW: Evaluation of a word list for sentiment analysis in microblogs. *ESWC2011 Workshop*.

5. **R Packages:**
   - Wickham, H., et al. (2019). *tidyverse: Easily Install and Load the 'Tidyverse'*
   - Jockers, M. (2015). *syuzhet: Extract Sentiment and Plot Arcs from Text*
   - Chang, W., et al. (2021). *shiny: Web Application Framework for R*

---

## 📄 License

This project is submitted as part of MTH208 course requirements at IIT Kanpur. All code and analysis are original work by Team 1.

---

**Last Updated:** October 26, 2025
