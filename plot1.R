plot1 <- function(){
        d1 <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = '?' , colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
        d1$Date <- as.Date(d1$Date, "%d/%m/%Y")
        d2 <- subset(d1,Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
        d2 <- d2[complete.cases(d2),]
        comTime <- paste(d2$Date,d2$Time)
        c <- c("Date","Time")
        d2 <- d2[,!names(d2) %in% c]
        d2 <- cbind(comTime, d2)
        d2$comTime <- as.POSIXct(comTime)
        hist(d2$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
        dev.copy(png,"plot1.png",width=480,height=480)
        dev.off()
}