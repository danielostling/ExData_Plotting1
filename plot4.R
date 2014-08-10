# danielostling:
# This program will read measurement data for power use over time and and plot 
# four sub-graphs:
# Top left: Global Active Power.
# Top right: Voltage.
# Bottom left: The energy of sub metering 1, 2 and 3.
# Bottom right: Global Reactive Power.

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
    dfData[, c("Date",
               "Global_active_power",
               "Global_reactive_power",
               "Voltage",
               "Sub_metering_1",
               "Sub_metering_2",
               "Sub_metering_3")]
  dfData
}


# danielostling: Plot four sub graphs, 
# Top left: Global Active Power.
# Top right: Voltage.
# Bottom left: The energy of sub metering 1, 2 and 3.
# Bottom right: Global Reactive Power.
#
# Input:
# dfData: Data frame hodling data to plot.
# vecCharFilename: Name of output file.
#
# Returns: Nothing.
plotGraphs <- function(dfData, vecCharFilename) {
  # danielostling: Set output to PNG file, plot, and deactivate output to PNG.
  png(filename=vecCharFilename, width=480, height=480)

  # danielostling: To get X-axis labels in english, please note that the locale 
  # is hardcoded to en_US.UTF-8.
  # danielostling: Set locale to USA to get correct text for X-axis labels.
  Sys.setlocale(category="LC_ALL", locale="en_US.UTF-8")

  # danielostling: Set the sub plot grid to 2 x 2.
  par(mfrow = c(2, 2))

  # danielostling: Plot the four sub plots.
  with(dfData, {
    plot(Date, Global_active_power, type="l", xlab="",
         ylab="Global Active Power")

    plot(Date, Voltage, type="l", xlab="datetime", ylab="Voltage")

    plot(Date, Sub_metering_1, type="l", xlab="", ylab="Energy sub metering",
         col="black")
    lines(Date, Sub_metering_2, type="l", col="red")
    lines(Date, Sub_metering_3, type="l", col="blue")
    legend("topright", lty=1, bty="n", col=c("black", "red", "blue"),
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

    plot(Date, Global_reactive_power, type="l", xlab="datetime",
         ylab="Global_reactive_power")
  })

  # danielostling: Close output device.
  dev.off()
}


# danielostling: Read the data to plot.
dfData <- readData("household_power_consumption.txt")

# danielostling: Plot.
plotGraphs(dfData, "plot4.png")