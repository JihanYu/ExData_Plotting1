##########################################################################
##### plot2.R : to construct line graph of Global Active Power data   ####
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

## seek_label() function - find the x-axis indices and abbreviated weekday
##   names corresponding to 2007-02-01 and 2007-02-02 and return them
seek_label <- function(epower_data){
	## Find the abbreviated weekday names corresponding to date_rage
	xrange <- format(as.POSIXct(epower_data$Date), format="%a")
	## Find the next day of date_range
	xrange_over1 <- format(as.POSIXct(epower_data$
				Date[length(epower_data$Date)]+1), format="%a")
	## Find the 1st indices of each day and their weekday names
	id_changedate <- table(xrange)[1]+1;  id_lastdate <- length(xrange)
	index_date <- c(1, id_changedate, id_lastdate)
	label_date <- c(xrange[1], xrange[id_changedate], xrange_over1)

	## Change the weekday names in English
	## In this computer's setting, the weekday names are expressed
	##    in Korean("일", "월"...). So, translate them in English
	weekday <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
	names(weekday) <- c("일", "월", "화", "수", "목", "금", "토")
	label_date <- weekday[label_date]

	## Make the indices and their weekday names to one vector
	names(index_date) <- label_date

	return(index_date)
}

## Current original data file
filename <- "./data/household_power_consumption.txt"  
## Read the data from the date 2007-02-01 and 2007-02-02
epower_data <- read_epower(filename)
## Read the weekdayname and their x-axis indices corrsponding to date range
index_date <- seek_label(epower_data)

## Construct line plot of Global active power data
plot(epower_data$Global_active_power, type="l", xlab="",
     ylab="Global Active Power(kilowatts)", xaxt='n',
     cex.lab=.8, cex.axis=.8)
axis(side=1, at=index_date, labels=names(index_date), cex.lab=.8, cex.axis=.8)

