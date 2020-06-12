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

## ass_date finds the x-axis indices and abbreviated weekday names
##    corresponding to 2007-02-01 and 2007-02-02
ass_date <- function(date.df){
    # find the abbreviated weekday names
	day.name <- format(as.POSIXct(date.df$Date), format="%a")
	day.name.over1 <- format(as.POSIXct(date.df$Date[length(date.df$Date)] + 1), 
						format="%a")
	
	# find the 1st indices of eacy day and their weekday names
	id.changedate <- table(day.name)[1] + 1;
	id.lastdate <- length(day.name)
	id.date <- c(1, id.changedate, id.lastdate)
	label.date <- c(day.name[1], day.name[id.changedate], day.name.over1)
	
	# change the weekday names in English (Korean -> English)
	day.name <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
	names(day.name) <- c("일", "월", "화", "수", "목", "금", "토")
	label.date <- day.name[label.date]
	names(id.date) <- label.date
	
	return(id.date)
}

## read the data file and extract the date 
##   by using previously defined function (read_elec())
filename <- "./data/household_power_consumption.txt"
elec.data <- read_elec(filename)
## read the weekday names and their x-axis indices corresponding to date range
index.date <- ass_date(elec.data)


## make 4 plots in a screen
par(mfrow=c(2,2))
## plot (1,1) - line plot of Global active power
plot(elec.data$Global_active_power, type="l", 
	xlab="", ylab="Global Active Power(killowatts)", xaxt="n")
axis(side=1, at=index.date, labels=names(index.date))

## plot(1, 2) - line plot of Voltage
plot(elec.data$Voltage, type="l", xlab="datetime", ylab="Voltage", xaxt="n")

## plot(2, 1) - 3 line graphs of energy sub metering(Sub_metering_1 to 3)
y.range <- c(0, max(elec.data$Sub_metering_1, elec.data$Sub_metering_2,
					elec.data$Sub_metering_3, na.rm=TRUE))
plot(elec.data$Sub_metering_1, type="l", col="black", ylim=y.range,
	xlab="", ylab="Energy sub metering", xaxt="n")
par(new=TRUE)
plot(elec.data$Sub_metering_2, type="l", col="red", ylim=y.range,
	xlab="", ylab="", xaxt="n")
par(new=TRUE)
plot(elec.data$Sub_metering_3, type="l", col="blue", ylim=y.range,
	xlab="", ylab="", xaxt="n")

legend("topright", col=c("black", "red", "blue"), lwd=1,
	legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
axis(side=1, at=index.date, labels=names(index.date))

## plot(2,2) - line graph of Global reactive power
plot(elec.data$Global_reactive_power, type="l", 
	xlab="datetime", ylab="Global_reactive_power", xaxt="n")
axis(side=1, at=index.date, labels=names(index.date))

