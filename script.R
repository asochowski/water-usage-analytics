library(shiny)
# Shiny setup
ui <- fluidPage(
  headerPanel("Water Usage Analytics"),
  sidebarPanel(
    fileInput("file", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    sliderInput(inputId = "num", label = "Choose a sensor", value = 1, min = 1, max = 3),
    selectInput("indicator", "Choose an indicator", list("None", "Simple Moving Average", "Exponential Moving Average",
                                               "Boellinger Bands", "Commodity Channel Index", "MACD", 
                                               "Rate of Change", "Stochastic Momentum Index", "Williams %R"))
  ),
  mainPanel(
    plotOutput("myplot"),
  )
)
server <- function(input, output) {
  output$myplot <- renderPlot({
    # Importing CSV data
    data<- read.csv(input$file$datapath,header=TRUE)
    print(data)
    # Gathering the list of unique dates in the CSV data
    unique_dates <- as.list(levels(unique(data[,1])))
    # Setting up the structure of the data frame
    data_frame1 <- do.call(rbind, lapply(unique_dates, as.data.frame))
    names(data_frame1) <- c("date")
    data_frame2 <- data.frame(high = 0, low = 0, open = 1:length(unique_dates), close = 0)
    data_frame3 <- cbind(data_frame1, data_frame2)
    data_frame4 <- data.frame(matrix(NA_real_, nrow = length(unique_dates), ncol = 25))
    data_frame <- cbind(data_frame3, data_frame4)
    averages <- cbind(data_frame1, data.frame(matrix(0, nrow = length(unique_dates), ncol = 2)))
    # Importing sensor data into the main data frame
    for (i in 1:(length(data[,1]))) {
      date <- data[[i,1]]
      for (j in 1:length(unique_dates)) {
        if (date == unique_dates[j]) {
          for (k in 1:24) {
            l <- 5 + k
            if (is.na(data_frame[j,l])) {
              # Xnum
              data_frame[j,l] <- 100 - data[i,1 + input$num]
              averages[j,2] <- averages[j,2] + data_frame[j,l]
              # Open
              if (l == 6) {
                data_frame[j,4] <- data_frame[j,l]
              }
              # High
              if (data_frame[j,2] < data_frame[j,l]) {
                data_frame[j,2] <- data_frame[j,l]
              }
              # Low
              if (data_frame[j,3] == 0 || (data_frame[j,3] > data_frame[j,l] && data_frame[j,l] != 0)) {
                data_frame[j,3] <- data_frame[j,l]
              }
              # Close
              data_frame[j,5] <- data_frame[j,l]
              averages[j,3] <- averages[j,3] + 1
              break
            }
          }
        }
      }
    }
    for (i in 1:length(unique_dates)) {
      data_frame[i,30] <- averages[i,2] / averages[i,3]
    }
    # Plotting candlestick chart
    x <- c(1:nrow(data_frame))
    colors <- ifelse(data_frame$close >= data_frame$open, "green3", "firebrick1")
    par(mar = c(4, 2, 2, 2), mfrow=c(2,1))
    layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE),
           heights=c(2,1))
    plot(data_frame$high, main = paste("Sensor", input$num, "and", input$indicator, sep = " "), xaxt = "n", xlab = "", ylab = "Water Potential", ylim = c(min(data_frame$low), max(data_frame$high)), type = "n")
    #par(new = T)
    #plot(data_frame$low, axes = FALSE, xlab = "", ylab = "", ylim=c(min(data_frame$low), max(data_frame$high)), type = "n")
    segments(x0 = x, y0 = data_frame$open, x1 = x, y1 = data_frame$close, col = colors, lwd = 5)
    segments(x0 = x, y0 = data_frame$low, x1 = x, y1 = data_frame$high, col = colors, lwd = 1)
    axis(1, at = x, labels = data_frame$date, las = 2)
    
    
    # Plotting SMA (5 and 21)
    if (input$indicator == "Simple Moving Average") {
      sma5 <- c(0)
      sma21 <- c(0)
      if (length(unique_dates) >= 5) {
        # Calculation of 5 day SMA
        avg <- 0.0
        for (i in 1:5) {
          avg <- avg + data_frame[i,5]
        }
        sma5[1] <- avg / 5.0
        print("TEST")
        print(sma5[1])
        for (i in 2:(length(unique_dates) - 4)) {
          add <- data_frame[i+4,5] - data_frame[i-1,5]
          avg <- avg + add
          sma5[i] <- (avg/5)
        }
        x <- c(1:(length(sma5)-1))
        z <- c(2:(length(sma5)))
        segments(x0 = x + 4, y0 = sma5[x], x1 = x + 5, y1 = sma5[x+1], lwd = 2, col = "orange")
      }
      if (length(unique_dates) >= 21) {
        # Calculation of 21 day SMA
        avg <- 0.0
        for (i in 1:21) { 
          avg <- avg + data_frame[i,5]
        }
        sma21[1] <- avg / 21.0
        print("TEST")
        print(sma5[1])
        for (i in 2:(length(unique_dates) - 20)) {
          add <- data_frame[i+20,5] - data_frame[i-1,5]
          avg <- avg + add
          sma21[i] <- (avg/21)
        }
        x <- c(1:(length(sma21)-1))
        z <- c(2:(length(sma21)))
        segments(x0 = x + 20, y0 = sma21[x], x1 = x + 21, y1 = sma21[x+1], lwd = 2, col = "lightslateblue")
      }
    }  
    
    
    # Plotting EMA (5 and 21)  
    else if (input$indicator == "Exponential Moving Average") {
      ema5 <- c(0)
      ema21 <- c(0)
      if (length(unique_dates) >= 5) {
        # Calculation of 5 day EMA
        ema <- 0.0
        for (i in 1:5) {
          ema <- ema + data_frame[i,5]
        }
        ema5[1] <- ema / 5.0
        for (i in 2:(length(unique_dates) - 4)) {
          ema5[i] <- data_frame[i+4,5] * (2/6) + ema5[i - 1] * (4/6)
        }
        x <- c(1:(length(ema5)-1))
        z <- c(2:(length(ema5)))
        segments(x0 = x + 4, y0 = ema5[x], x1 = x + 5, y1 = ema5[x+1], lwd = 2, col = "orange")
      }
      if (length(unique_dates) >= 21) {
        # Calculation of 21 day EMA
        ema <- 0.0
        for (i in 1:21) {
          ema <- ema + data_frame[i,5]
        }
        ema21[1] <- ema / 21.0
        for (i in 2:(length(unique_dates) - 20)) {
          ema21[i] <- data_frame[i+20,5] * (2/22) + ema21[i - 1] * (20/22)
        }
        x <- c(1:(length(ema21)-1))
        z <- c(2:(length(ema21)))
        segments(x0 = x + 20, y0 = ema21[x], x1 = x + 21, y1 = ema21[x+1], lwd = 2, col = "lightslateblue")
      }
    }
    
    
    # Plotting Boellinger Bands
    else if (input$indicator == "Boellinger Bands") {
      if (length(unique_dates) >= 20) {
        # Calculation of 20 day SMA
        sma20 <- c(0)
        avg <- 0.0
        for (i in 1:20) {
          avg <- avg + data_frame[i,5]
        }
        sma20[1] <- avg / 20.0
        for (i in 2:(length(unique_dates) - 19)) {
          add <- data_frame[i+19,5] - data_frame[i-1,5]
          avg <- avg + add
          sma20[i] <- (avg/20)
        }
        # Plotting of middle, upper, and lower bands
        x <- c(1:(length(sma20)-1))
        segments(x0 = x + 19, y0 = sma20[x], x1 = x + 20, y1 = sma20[x+1], lwd = 2, col = "gray")
        segments(x0 = x + 19, y0 = sma20[x]+2*sd(data_frame[(x):(x+19),5]), x1 = x + 20, y1 = sma20[x+1]+2*sd(data_frame[(x):(x + 19),5]), lwd = 2, col = "pink")
        segments(x0 = x + 19, y0 = sma20[x]-2*sd(data_frame[(x):(x+19),5]), x1 = x + 20, y1 = sma20[x+1]-2*sd(data_frame[(x):(x + 19),5]), lwd = 2, col = "pink")
      }
    }
    
    
    # Plotting Commodity Channel Index
    else if (input$indicator == "Commodity Channel Index") {
      if (length(unique_dates) >= 20) {
        tp <- c(0)
        # Calculation of "true price"
        for (i in 1:(length(unique_dates))) {
          tp[i] <- sum(c(data_frame[i,2],data_frame[i,3],data_frame[i,5]))/3
        }
        # Calculation of the 20 day SMA based off of the true price
        sma20 <- c(sum(tp[1:20])/20)
        for(i in 2:(length(unique_dates)-19)) {
          sma20[i] <- sma20[i-1] + (tp[1+19] - tp[i-1])/20
        }
        # Calculation of the daily mean average deviation of the true price from the SMA20
        mad <- c(0)
        for (j in 1:(length(unique_dates)-19)) {
          mad[j] <- 0
          for (i in 1:20) {
            mad[j] <- mad[j] + (abs(sma20[j] - tp[i+j-1]))/20 
          }
        }
        # Calculation of the CCI(20,0.015)
        cci <- c(0)
        for (i in 1:(length(unique_dates)-19)) {
          cci[i] <- (tp[i+19] - sma20[i])/(0.015*mad[i])
          print(cci[i])
        }
        par(mar = c(0,2,2,2))
        plot(tp, ylim = c(-250,250), xaxt = "n", type = "n", xlab="")
        x <- c(1:(length(sma20)-1))
        z <- c(-100,100)
        segments(x0 = 0, y0 = z, x1 = length(unique_dates), y1 = z, lwd = 1, col = "gray", xlab = "")
        segments(x0 = x + 19, y0 = cci[x], x1 = x + 20, y1 = cci[x+1], lwd = 2, col = "red")
      }
    }
    
    
    # Plotting of the MACD
    else if (input$indicator == "MACD") {
      ema12 <- c(0)
      ema26 <- c(0)
      if (length(unique_dates) >= 26) {
        # Calculation of 5 day EMA
        ema <- 0.0
        for (i in 1:12) {
          ema <- ema + data_frame[i,5]
        }
        ema12[1] <- ema / 12.0
        for (i in 2:(length(unique_dates) - 11)) {
          ema12[i] <- data_frame[i+11,5] * (2/13) + ema12[i - 1] * (11/13)
        }
        # Calculation of 21 day EMA
        ema <- 0.0
        for (i in 1:26) {
          ema <- ema + data_frame[i,5]
        }
        ema26[1] <- ema / 26.0
        for (i in 2:(length(unique_dates) - 25)) {
          ema26[i] <- data_frame[i+25,5] * (2/27) + ema26[i - 1] * (25/27)
        }
        # Calculation of MACD
        macd <- c(0)
        for (i in 1:length(ema26)) {
          macd[i] <- ema12[i+14] - ema26[i]
        }
        # Calculation of signal
        signal <- c(0)
        ema <- 0.0
        for (i in 1:9) {
          ema <- ema + macd[i]
        }
        signal[1] <- ema / 9.0
        for (i in 2:(length(ema26) - 8)) {
          signal[i] <- macd[i+8] * (2/10) + signal[i - 1] * (8/10)
        }
        # Calculation of histogram
        his <- c(0,0)
        for (i in 1:(length(signal))) {
          his[i] <- macd[i+8] - signal[i]
        }
        par(mar = c(0,2,2,2))
        plot(1:length(unique_dates), ylim = c(min(c(min(macd),min(his),min(signal))),max(c(max(macd),max(his),max(signal)))), xaxt = "n", type = "n", xlab="")
        # Plotting of MACD, Signal, and MACD Histogram
        x <- c(1:(length(macd)-1))
        z <- c(1:(length(signal)-1))
        segments(x0 = x + 25, y0 = macd[x], x1 = x + 26, y1 = macd[x+1], lwd = 2, col = "gray")
        segments(x0 = z + 25 + 9, y0 = signal[x], x1 = z + 26 + 9, y1 = signal[x+1], lwd = 2, col = "red")
        segments(x0 = 0, y0 = 0, x1 = length(unique_dates), y1 = 0, lwd = 1, col = "gray")
        segments(x0 = z + 25 + 9, y0 = 0, x1 = z + 25 + 9, y1 = his[z], lwd = 1, col = "black")
      }
    }
    
    
    # Plotting of Rate of Change
    else if (input$indicator == "Rate of Change") {
      if (length(unique_dates) >= 21) {
        roc21 <- c(0)
        for (i in 1:(length(unique_dates) - 20)) {
          roc21[i] <- 100 * (data_frame[i + 20, 5] - data_frame[i,5]) / data_frame[i,5]
        }
        x <- c(1:(length(roc21)-1))
        par(mar = c(0,2,2,2))
        plot(1:length(unique_dates), ylim = c(min(roc21),max(roc21)), xaxt = "n", type = "n", xlab="")
        segments(x0 = 0, y0 = 0, x1 = length(unique_dates), y1 = 0, lwd = 1, col = "gray")
        segments(x0 = x + 20, y0 = roc21[x], x1 = x + 21, y1 = roc21[x+1], lwd = 2, col = "red")
      }
    }
    
    
    # Plotting of Stochastic Momentum Index
    else if (input$indicator == "Stochastic Momentum Index") {
      if (length(unique_dates) >= 49) {
        high13 <- c(0)
        low13 <- c(0)
        for (i in 1:(length(unique_dates) - 12)) {
          high13[i] <- max(data_frame[i:(i + 12), 2])
          low13[i] <- min(data_frame[i:(i + 12), 3])
        }
        # Calculation of first 25 day EMA
        ema <- 0.0
        for (i in 1:25) {
          ema <- ema + data_frame[i+12,5] - 0.5 * (high13[i] + low13[i])
        }
        ema251 <- c()
        ema251[1] <- ema / 25.0
        for (i in 2:(length(low13) - 24)) {
          ema251[i] <- (data_frame[i+24+12,5] - 0.5 * (high13[i+24] + low13[i+24])) * (2/26) + ema251[i - 1] * (24/26)
        } 
        # EMA2.1
        ema <- 0.0
        for (i in 1:2) {
          ema <- ema +  ema251[i]
        }
        ema21 <- c()
        ema21[1] <- ema / 2.0
        for (i in 2:(length(ema251) - 1)) {
          ema21[i] <- (ema251[i + 1]) * (2/3) + ema21[i - 1] * (1/3)
        }
        ema <- 0.0
        for (i in 1:25) {
          ema <- ema + (high13[i] - low13[i])
        }
        ema252 <- c()
        ema252[1] <- ema / 25.0
        for (i in 2:(length(low13) - 24)) {
          ema252[i] <- ((high13[i+24] - low13[i+24])) * (2/26) + ema252[i - 1] * (24/26)
        } 
        # EMA2.1
        ema <- 0.0
        for (i in 1:2) {
          ema <- ema +  ema252[i]
        }
        ema22 <- c()
        ema22[1] <- ema / 2.0
        for (i in 2:(length(ema251) - 1)) {
          ema22[i] <- (ema252[i + 1]) * (2/3) + ema22[i - 1] * (1/3)
        }
        
        smi <- c()
        for (i in 1:(length(ema22))) {
          smi[i] <- 200 * ema21[i] / ema22
        }
        
        signal <- c()
        ema <- 0.0
        for (i in 1:9) {
          ema <- ema +  smi[i]
        }
        signal[1] <- ema / 9.0
        for (i in 2:(length(ema22) - 8)) {
          signal[i] <- (smi[i + 8]) * (2/9) + signal[i - 1] * (7/9)
        }
        x <- c(1:(length(smi)-1))
        z <- c(1:(length(signal)-1))
        par(mar = c(0,2,2,2))
        plot(1:length(unique_dates), ylim = c(min(smi),max(smi)), xaxt = "n", type = "n", xlab="")
        segments(x0 = 0, y0 = 0, x1 = length(unique_dates), y1 = 0, lwd = 1, col = "gray")
        segments(x0 = x + 39, y0 = smi[x], x1 = x + 40, y1 = smi[x+1], lwd = 2, col = "blue")
        segments(x0 = x + 48, y0 = signal[x], x1 = x + 49, y1 = signal[x+1], lwd = 2, col = "black")
      }
    }
    
    
    # Plotting of Williams %R
    else if (input$indicator == "Williams %R") {
      if (length(unique_dates) >= 14) {
        high14 <- c(0)
        low14 <- c(0)
        for (i in 1:(length(unique_dates) - 13)) {
          high14[i] <- max(data_frame[i:(i + 13), 2])
          low14[i] <- min(data_frame[i:(i + 13), 3])
        }
        williams <- c()
        for (i in 1:length(high14)) {
          williams[i] <- -100 * (high14[i] - data_frame[i+13,5]) / (high14[i] - low14[i])
        }
        x <- c(1:(length(williams)-1))
        par(mar = c(0,2,2,2))
        plot(1:length(unique_dates), ylim = c(min(williams),max(williams)), xaxt = "n", type = "n", xlab="")
        segments(x0 = 0, y0 = 0, x1 = length(unique_dates), y1 = 0, lwd = 1, col = "gray")
        segments(x0 = x + 13, y0 = williams[x], x1 = x + 14, y1 = williams[x+1], lwd = 2, col = "blue")
      }
    }
  })
}
shinyApp(ui = ui, server = server)