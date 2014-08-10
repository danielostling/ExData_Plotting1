# danielostling:
# This program will read measurement data for power use over time and and plot 
# the energy of sub metering 1, 2 and 3 over time.


# danielostling: Read raw data from input file, drop columns not needed, and
# convert Date and Time columns from two separate character vectors to one 
# column "Date" holding an R date value instead.
#
# Input:
# vecCharFilename: Name of input file.
#
# Returns: A data frame holding the relevant data for plotting.
readData <- function(vecCharFilename) {
  # danielostling: Read the raw data from CSV. Note separator is ';', not ','.
  # Also convert "?" into R NA values.
  dfData <- read.csv(vecCharFilename,sep=";", na.strings=c("?"))
  
  # danielostling: Skip all data except 2007-02-01 -> 2007-02-02, and convert
  # the Date and Time columns into R datetime.
  dfData <- dfData[dfData$Date %in% c("1/2/2007", "2/2/2007"),]
  dfData$Date <- 
    strptime(paste(dfData$Date, dfData$Time), "%d/%m/%Y %H:%M:%S")
  
  # danielostling: Drop unneeded columns, and return.
  dfData <- 
    dfData[, c("Date", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]
  dfData
}

# danielostling: Plot the energy of sub meters 1, 2 and 3 over time. To get 
# X-axis labels in english, please note that the locale is hardcoded to 
# en_US.UTF-8.
#
# Input:
# dfData: Data frame hodling data to plot.
# vecCharFilename: Name of output file.
#
# Returns: Nothing.
plotData <- function(dfData, vecCharFilename) {
  # danielostling: Set locale to USA to get correct text for X-axis labels.
  Sys.setlocale(category="LC_ALL", locale="en_US.UTF-8")
  
  # danielostling: Set output to PNG file, plot, and deactivate output to PNG.
  png(filename=vecCharFilename, width=480, height=480)
  plot(dfData$Date, dfData$Sub_metering_1, type="l",
       xlab="",
       ylab="Energy sub metering",
       col="black")
  lines(dfData$Date, dfData$Sub_metering_2, type="l", col="red")
  lines(dfData$Date, dfData$Sub_metering_3, type="l", col="blue")
  legend("topright",
         lty=1,
         col=c("black", "red", "blue"),
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  dev.off()
}

# danielostling: Read the data to plot.
dfData <- readData("household_power_consumption.txt")

# danielostling: Plot.
plotData(dfData, "plot3.png")