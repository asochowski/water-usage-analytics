library(shiny)
# Importing CSV data
data<- read.csv("data2.csv",header=TRUE)
# Gathering the list of unique dates in the CSV data
unique_dates <- as.list(levels(unique(data[,1])))
# Shiny setup
ui <- fluidPage(
  headerPanel("Candlesticks"),
  sidebarPanel(
    sliderInput(inputId = "num", label = "Choose a sensor", value = 1, min = 1, max = 3),
    selectInput("indicator", "Indicator", list("None", "Simple Moving Average", "Exponential Moving Average",
                                               "Boellinger Bands", "Parabolic Stop and Reverse", 
                                               "Average Directional Movement Index", "Commodity Channel Index",
                                               "MACD", "Rate of Change", "Relative Strength Index",
                                               "Stochastic Momentum Index", "William's %R"))
  ),
  mainPanel(
    plotOutput("myplot"),
  )
)
server <- function(input, output) {
  output$myplot <- renderPlot({
    # Setting up the structure of the data frame
    data_frame1 <- do.call(rbind, lapply(unique_dates, as.data.frame))
    names(data_frame1) <- c("date")
    data_frame2 <- data.frame(high = 0, low = 0, open = 1:length(unique_dates), close = 0)
    data_frame3 <- cbind(data_frame1, data_frame2)
    data_frame4 <- data.frame(matrix(NA_real_, nrow = length(unique_dates), ncol = 24))
    data_frame <- cbind(data_frame3, data_frame4)
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
              break
            }
          }
        }
      }
    }
    print(num)
    # Plotting candlestick chart
    x <- c(1:nrow(data_frame))
    colors <- ifelse(data_frame$close >= data_frame$open, "green3", "firebrick1")
    par(mar = c(6, 3, 2, 2))
    plot(data_frame$high, main = paste("Sensor", input$num, "and", input$indicator, sep = " "), xaxt = "n", xlab = "", ylab = "Water Potential", ylim = c(min(data_frame$low), max(data_frame$high)), type = "n")
    par(new = T)
    plot(data_frame$low, axes = FALSE, xlab = "", ylab = "", ylim=c(min(data_frame$low), max(data_frame$high)), type = "n")
    segments(x0 = x, y0 = data_frame$open, x1 = x, y1 = data_frame$close, col = colors, lwd = 5)
    segments(x0 = x, y0 = data_frame$low, x1 = x, y1 = data_frame$high, col = colors, lwd = 1)
    axis(1, at = x, labels = data_frame$date, las = 2)
    # Plotting SMA
    if (input$indicator == "Simple Moving Average") {
    }
  })
}
shinyApp(ui = ui, server = server)
