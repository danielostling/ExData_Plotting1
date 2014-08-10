# danielostling:
# This program will read measurement data for power use over time and and plot 
# a histogram showing the distribution of Global Active Power measurements.


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
  dfData <- dfData[, c("Date", "Global_active_power")]
  dfData
}


# danielostling: Plot observations of Global Active Power in histogram format.
#
# Input:
# dfData: Data frame hodling data to plot.
# vecCharFilename: Name of output file.
#
# Returns: Nothing.
plotData <- function(dfData, vecCharFilename) {
  # danielostling: Convert Global Active Power from character vector elements
  # to numeric elements.
  vecNumGAP <- as.numeric(dfData$Global_active_power)

  # danielostling: Set output to PNG file, plot, and deactivate output to PNG.
  png(filename=vecCharFilename, width=480, height=480)
  hist(vecNumGAP,
       main="Global Active Power",
       xlab="Global Active Power (kilowatts)",
       ylab="Frequency", 
       col="red")
  dev.off()
}

# danielostling: Read the data to plot.
dfData <- readData("household_power_consumption.txt")

# danielostling: Plot.
plotData(dfData, "plot1.png")
