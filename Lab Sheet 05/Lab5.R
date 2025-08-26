 
setwd("C:\\Users\\it24100348\\Desktop\\IT24100348") 

Delivery_Times <- read.table("Exercise - Lab 05.txt", header = FALSE)
colnames(Delivery_Times) <- c("Time")
head(Delivery_Times)

str(Delivery_Times)
Delivery_Times$Time <- as.numeric(as.character(Delivery_Times$Time))
str(Delivery_Times)

breaks <- seq(20,70, by = 5)
hist( Delivery_Times$Time,
     breaks = breaks,
      right = FALSE,
      col = "skyblue",
      main = "Histogram of Delivery Times",
      xlab = "Delivery Times",
      ylab = "frequency",
      border = "black")

freq_table <- hist(Delivery_Times$Time,
                   breaks =  breaks,
                   right = FALSE,
                   plot = FALSE)

cum_freq <- cumsum(freq_table$counts)

plot(breaks[-1], cum_freq, type = "o",
     main = "cumulative frequency polygon (o give)",
     xlab = "Delivery Times",
     ylab = "cumulative frequency",
     col = "red",pch = 16)




