plot4 <- function(){
        d1 <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = '?' , colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
        d1$Date <- as.Date(d1$Date, "%d/%m/%Y")
        d2 <- subset(d1,Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
        d2 <- d2[complete.cases(d2),]
        comTime <- paste(d2$Date,d2$Time)
        c <- c("Date","Time")
        d2 <- d2[,!names(d2) %in% c]
        d2 <- cbind(comTime, d2)
        d2$comTime <- as.POSIXct(comTime)
        
        par(mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0))
        
        plot(d2$Global_active_power~d2$comTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
        plot(d2$Voltage~d2$comTime, type="l", ylab="Voltage (volt)", xlab="")
        plot(d2$Sub_metering_1~d2$comTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
        lines(d2$Sub_metering_2~d2$comTime,col='Red')
        lines(d2$Sub_metering_3~d2$comTime,col='Blue')
        legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        plot(d2$Global_reactive_power~d2$comTime, type="l", ylab="Global Rective Power (kilowatts)", xlab="")
        
        dev.copy(png,"plot4.png",width=480,height=480)
        dev.off()
}