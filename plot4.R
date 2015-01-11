##########################################################################
#### plot4.R : to construct 4 plots in a screen                       ####
####     (1) Global active power     (2) Voltage                      ####
####     (3) Energy sub metering     (4) Global reactive power        ####
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


## Make the 4 plots in a screen
par(mfrow=c(2,2))
## Plot (1, 1) - line graph of Global active power data
plot(epower_data$Global_active_power, type="l", xlab="",
     ylab="Global Active Power(kilowatts)", xaxt="n", cex.lab=.8, cex.axis=.8)
axis(side=1, at=index_date, labels=names(index_date), cex.lab=.8, cex.axis=.8)

## Plot (1, 2) - line graph of Voltage
plot(epower_data$Voltage, type="l", xlab="datetime", ylab="Voltage",
     xaxt="n", cex.lab=.8, cex.axis=.8)
axis(side=1, at=index_date, labels=names(index_date), cex.lab=.8, cex.axis=.8)

## Plot (2, 1) - 3 line graphs of energy sub metering(Sub_metering_1, 2, 3)
##               in a plot with legend
yrange <- c(0, max(epower_data$Sub_metering_1, epower_data$Sub_metering_2,
		   epower_data$Sub_metering_3, na.rm=TRUE))
plot(epower_data$Sub_metering_1, type="l", col="black",
     xlab="", ylab="Energy sub metering", ylim=yrange,
     xaxt="n", cex.lab=.8, cex.axis=.8)
par(new=TRUE)
plot(epower_data$Sub_metering_2, type="l", col="red", 
     xlab="", ylab="", ylim=yrange, xaxt="n", cex.lab=.8, cex.axis=.8)
par(new=TRUE)
plot(epower_data$Sub_metering_3, type="l", col="blue",
     xlab="", ylab="", ylim=yrange, xaxt="n", cex.lab=.8, cex.axis=.8)

legend("topright", col=c("black", "red", "blue"), lwd=1, box.col="white",
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex=.8)
axis(side=1, at=index_date, labels=names(index_date), cex.lab=.8, cex.axis=.8)
box()

## Plot (2, 2) - line graph of Global reactive power
plot(epower_data$Global_reactive_power, type="l", 
     xlab="datetime", ylab="Global_reactive_power", xaxt="n",
     cex.lab=.8, cex.axis=.8)
axis(side=1, at=index_date, labels=names(index_date), cex.lab=.8, cex.axis=.8)

