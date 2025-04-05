library(shiny)
library(ggplot2)
library(forecast)
library(zoo)
library(dplyr)
library(lubridate)
library(readr)
library(rugarch)

ui <- fluidPage(
  titlePanel("\U0001F4C8 Beginner-Friendly Time Series Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Your Time Series CSV", accept = ".csv"),
      uiOutput("date_column_ui"),
      uiOutput("value_column_ui"),
      selectInput("dataset", "Or Use Sample Dataset",
                  choices = c("AirPassengers", "nottem", "UKgas")),
      sliderInput("ma_window", "Moving Average Window:", min = 2, max = 12, value = 3),
      helpText("\U0001F4A1 Upload a CSV with a date column and one or more numeric value columns."),
      tableOutput("preview")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Original Series", 
                 plotOutput("origPlot"), 
                 helpText("This shows your original data over time. It gives a general overview of the pattern, like whether it's going up, down, or fluctuating.")),
        
        tabPanel("Trend", 
                 plotOutput("trendPlot"), 
                 helpText("This smooth line shows the overall direction your data is moving — upward (growth), downward (decline), or flat (stable).")),
        
        tabPanel("Seasonality", 
                 plotOutput("seasonalPlot"), 
                 helpText("Seasonality is when similar patterns repeat over regular time intervals. This plot helps spot things like higher sales every December.")),
        
        tabPanel("Cycle", 
                 plotOutput("cyclePlot"), 
                 helpText("Cycles are ups and downs in your data that don't repeat at regular intervals. They're more random and longer-term than seasonality.")),
        
        tabPanel("Differencing", 
                 plotOutput("diffPlot"), 
                 helpText("This removes trends from the data, making it easier to analyze patterns. If the result looks stable (no trend), your data might be 'stationary'.")),
        
        tabPanel("Seasonal Differencing", 
                 plotOutput("sdiffPlot"), 
                 helpText("This removes repeating seasonal patterns. Useful if your data has annual, quarterly, or monthly cycles.")),
        
        tabPanel("Moving Average", 
                 plotOutput("maPlot"), 
                 helpText("This smooths out short-term fluctuations and highlights longer-term trends by averaging nearby points.")),
        
        tabPanel("ACF & PACF",
                 plotOutput("acfPlot"), 
                 helpText("ACF (Autocorrelation Function) tells you if current values are related to past values. If the bars drop off slowly, your data is not stationary."),
                 
                 plotOutput("pacfPlot"), 
                 helpText("PACF shows how much a value is related to a past value after removing effects from other time lags."),
                 
                 plotOutput("diffAcfPlot"), 
                 helpText("ACF after differencing. If bars drop quickly, it means your differencing made the series stationary (a good thing for forecasting)."),
                 
                 plotOutput("diffPacfPlot"), 
                 helpText("PACF after differencing. Helps you choose how many lags to include in ARIMA.")),
        
        tabPanel("ARIMA Model", 
                 verbatimTextOutput("arimaSummary"), 
                 helpText("ARIMA is a popular forecasting model. This shows the automatically chosen version and its details.")),
        
        tabPanel("ARIMA Forecast", 
                 plotOutput("forecastPlot"), 
                 helpText("Forecast using ARIMA (good when there's no strong seasonal pattern). Future values are shown with confidence intervals.")),
        
        tabPanel("SARIMA Model", 
                 verbatimTextOutput("sarimaSummary"), 
                 helpText("SARIMA includes seasonality. This model is better when your data shows repeating seasonal effects.")),
        
        tabPanel("SARIMA Forecast", 
                 plotOutput("sarimaForecastPlot"), 
                 helpText("This plot shows a forecast from SARIMA — good for monthly/quarterly data with seasonality.")),
        
        tabPanel("ARCH/GARCH Model", 
                 verbatimTextOutput("garchSummary"), 
                 plotOutput("garchVolPlot"), 
                 helpText("GARCH models volatility (how much the data fluctuates). Useful for financial data or data with big jumps."))
      )
    )
  )
)

server <- function(input, output, session) {
  
  raw_data <- reactive({
    if (!is.null(input$file)) {
      read_csv(input$file$datapath, show_col_types = FALSE)
    } else {
      NULL
    }
  })
  
  output$preview <- renderTable({
    head(raw_data(), 5)
  })
  
  output$date_column_ui <- renderUI({
    df <- raw_data()
    if (!is.null(df)) {
      date_candidates <- names(df)[sapply(df, function(col) any(!is.na(parse_date_time(col, orders = c("ymd", "dmy", "mdy", "my", "Y"), quiet = TRUE))))]
      selectInput("date_col", "Select Date Column", choices = date_candidates)
    }
  })
  
  output$value_column_ui <- renderUI({
    df <- raw_data()
    if (!is.null(df)) {
      num_cols <- names(df)[sapply(df, is.numeric)]
      selectInput("value_col", "Select Value Column", choices = num_cols)
    }
  })
  
  ts_data <- reactive({
    if (!is.null(input$file)) {
      df <- raw_data()
      if (is.null(input$date_col) || is.null(input$value_col)) return(NULL)
      
      df[["Date"]] <- parse_date_time(df[[input$date_col]], orders = c("ymd", "dmy", "mdy", "my", "Y"), quiet = TRUE)
      df[["Value"]] <- df[[input$value_col]]
      
      df <- df %>% filter(!is.na(Date) & !is.na(Value)) %>% arrange(Date)
      if (nrow(df) < 2) {
        showNotification("Dataset must have at least 2 valid rows after filtering.", type = "error")
        return(NULL)
      }
      
      date_diff <- median(diff(df$Date), na.rm = TRUE)
      freq <- ifelse(date_diff <= 31, 12, 4)
      start_year <- year(min(df$Date, na.rm = TRUE))
      start_period <- if (freq == 12) month(min(df$Date, na.rm = TRUE)) else quarter(min(df$Date, na.rm = TRUE))
      
      ts_obj <- ts(df$Value, start = c(start_year, start_period), frequency = freq)
      return(ts_obj)
    } else {
      switch(input$dataset,
             "AirPassengers" = AirPassengers,
             "nottem" = nottem,
             "UKgas" = UKgas)
    }
  })
  
  output$origPlot <- renderPlot({ autoplot(ts_data()) + ggtitle("Original Time Series") + theme_minimal() })
  output$trendPlot <- renderPlot({ trend <- ma(ts_data(), order = 12); autoplot(ts_data(), series = "Original") + autolayer(trend, series = "Trend", color = "blue") + ggtitle("Trend Estimation") + theme_minimal() })
  output$seasonalPlot <- renderPlot({ stl_decomp <- stl(ts_data(), s.window = "periodic"); autoplot(stl_decomp$time.series[, "seasonal"]) + ggtitle("Seasonal Component") + theme_minimal() })
  output$cyclePlot <- renderPlot({ stl_decomp <- stl(ts_data(), s.window = "periodic"); autoplot(stl_decomp$time.series[, "remainder"]) + ggtitle("Cycle Component") + theme_minimal() })
  output$diffPlot <- renderPlot({ autoplot(diff(ts_data())) + ggtitle("Differenced Series") + theme_minimal() })
  output$sdiffPlot <- renderPlot({ autoplot(diff(ts_data(), lag = frequency(ts_data()))) + ggtitle("Seasonally Differenced Series") + theme_minimal() })
  output$maPlot <- renderPlot({ ma_ts <- ma(ts_data(), order = input$ma_window); autoplot(ts_data(), series = "Original") + autolayer(ma_ts, series = paste0(input$ma_window, "-Point MA"), color = "darkgreen") + ggtitle("Moving Average") + theme_minimal() })
  output$acfPlot <- renderPlot({ Acf(ts_data(), main = "ACF of Original") })
  output$pacfPlot <- renderPlot({ Pacf(ts_data(), main = "PACF of Original") })
  output$diffAcfPlot <- renderPlot({ Acf(diff(ts_data()), main = "ACF of Differenced") })
  output$diffPacfPlot <- renderPlot({ Pacf(diff(ts_data()), main = "PACF of Differenced") })
  output$arimaSummary <- renderPrint({ model <- auto.arima(ts_data(), seasonal = FALSE); summary(model) })
  output$forecastPlot <- renderPlot({ model <- auto.arima(ts_data(), seasonal = FALSE); fcast <- forecast(model, h = 12); autoplot(fcast) + ggtitle("ARIMA Forecast") + theme_minimal() })
  output$sarimaSummary <- renderPrint({ model <- auto.arima(ts_data(), seasonal = TRUE); summary(model) })
  output$sarimaForecastPlot <- renderPlot({ model <- auto.arima(ts_data(), seasonal = TRUE); fcast <- forecast(model, h = 12); autoplot(fcast) + ggtitle("SARIMA Forecast") + theme_minimal() })
  output$garchSummary <- renderPrint({
    ts_vals <- ts_data()
    if (length(ts_vals) < 20) return("Not enough data for GARCH modeling")
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(1, 1)))
    fit <- ugarchfit(spec, ts_vals)
    show(fit)
  })
  output$garchVolPlot <- renderPlot({
    ts_vals <- ts_data()
    if (length(ts_vals) < 20) return(NULL)
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(1, 1)))
    fit <- ugarchfit(spec, ts_vals)
    plot(sigma(fit), main = "Conditional Volatility (GARCH)", ylab = "Volatility")
  })
}

shinyApp(ui = ui, server = server)

