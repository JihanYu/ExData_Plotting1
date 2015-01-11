##########################################################################
##### plot1.R : to construct histogram of Global Active Power data   #####
##########################################################################

## read_epower() function - read the original data file
##   and extract data from the date 2007-02-01 and 2007-02-02 and return
read_epower <- function(filename){
	## Read the original data (read the date as character)
	classes <- c(rep("character", 2), rep("numeric", 7))
	cons <- read.table(filename, header=TRUE, 
			   colClasses=classes, sep=";", na.strings="?")

	## Convert the class of date : character -> Date
	daterange <- as.Date(c("2007/02/01", "2007/02/02"))
	cons$Date <- as.Date(strptime(cons$Date, "%d/%m/%Y"))

	## Find the row index corresponded to date range(2007/02/01~02)
	##    and make subgroup using the row index
	index_daterange <- which(cons$Date %in% daterange)
	epower_daterange <- cons[index_daterange,]
	return(epower_daterange)
}

## Current original data file
filename <- "./data/household_power_consumption.txt"  
## Read the data from the date 2007-02-01 and 2007-02-02
epower_data <- read_epower(filename)

## Construct histogram of Global active power data
hist(epower_data$Global_active_power, col="red", 
     main="Global Active Power", xlab="Global Active Power(kilowatts)",
     cex.main=.8, cex.lab=.8, cex.axis=.8)

