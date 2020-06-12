workingpath <- "C:\\Users\\MED1\\Desktop\\Coursera\\project\\ExploratoryDataAnalysis"
setwd(workingpath)

## read_elec function reads the data file with read.table()
##   then extract the data between 2007-02-01 and 2007-02-02
read_elec <- function(filename){
	classes <- c(rep("character", 2), rep("numeric", 7))
	elec <- read.table(filename, sep=";", header=TRUE, 
						colClasses=classes, na.strings="?")
	
	date.range <- as.Date(c("2007/02/01", "2007/02/02"))
	elec$Date <- as.Date(strptime(elec$Date, "%d/%m/%Y"))
	
	id.date.range <- which(elec$Date %in% date.range)
	return(elec[id.date.range,])
}

## read the data file and extract the date 
##   by using previously defined function (read_elec())
filename <- "./data/household_power_consumption.txt"
elec.data <- read_elec(filename)

## make histogram & save as plot1.png file
png(file = "plot1.png")
hist(elec.data$Global_active_power, col="red",
	main="Global Active Power", xlab="Global Active Power(kilowatts)")
dev.off()