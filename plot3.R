plot3 <- function() {
  #Reading data from txt file
  data_file <- read.table("./exdata_data_household_power_consumption/household_power_consumption.txt", header = TRUE, sep = ";", 
                          colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), 
                          na.strings = "?", stringsAsFactors = FALSE )
  #specifying classes for Date and Time column
  data_file$Date <- as.Date(data_file$Date, format = "%d/%m/%Y")
  data_file$Time <- format(data_file$Time, format = "%H:%M:%S")
  
  
  #subsetting the data by dates "2007-02-01 & 2007-02-02"
  data_subset_Bydate <- subset(data_file, Date == "2007-02-01" | Date == "2007-02-02")
  
  #combining date and time column in to 'DateTime'.
  DateTime <- paste(data_subset_Bydate$Date, data_subset_Bydate$Time)
  
  #Converting DateTime in to POSIXct class
  DateTime <- as.POSIXct(DateTime)
  
  #combining new column with the dateset and removing old 
  #Date and Time column. 
  data_subset_Bydate <- cbind(DateTime, data_subset_Bydate)
  data_subset_Bydate <- data_subset_Bydate[, -c(2:3)]
  
# making the plot
png(filename = "plot3.png", width = 480, height = 480)
plot(x = data_subset_Bydate$DateTime, y = data_subset_Bydate$Sub_metering_1, 
     type = "l", xlab = "", ylab = "Energy sub metering", axes = TRUE)
lines(data_subset_Bydate$DateTime, data_subset_Bydate$Sub_metering_2, type = "l", col = "red")
lines(data_subset_Bydate$DateTime, data_subset_Bydate$Sub_metering_3, type = "l", col = "blue")
legend("topright", legend = c("sub_metering_1", "sub_metering_2", "sub_metering_3"), lty = 1, col = c("black", "red", "blue"))
dev.off()
}