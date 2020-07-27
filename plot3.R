plot3 <- function(){
        d1 <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = '?' , colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
        d1$Date <- as.Date(d1$Date, "%d/%m/%Y")
        d2 <- subset(d1,Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
        d2 <- d2[complete.cases(d2),]
        comTime <- paste(d2$Date,d2$Time)
        c <- c("Date","Time")
        d2 <- d2[,!names(d2) %in% c]
        d2 <- cbind(comTime, d2)
        d2$comTime <- as.POSIXct(comTime)
        
        plot(d2$Sub_metering_1~d2$comTime, type="l",ylab = "Global Active Power (kilowatts)", xlab = "")
        lines(d2$Sub_metering_2~d2$comTime, col = "red")
        lines(d2$Sub_metering_3~d2$comTime, col = "blue")
        legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2,
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        dev.copy(png,"plot3.png",width=480,height=480)
        dev.off()
}