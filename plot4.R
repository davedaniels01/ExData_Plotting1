

# Our overall goal here is simply to examine how household energy usage 
#    varies over a 2-day period in February, 2007. Your task is to reconstruct 
#   the following plots below, all of which were constructed using the base plotting system.

# First you will need to fork and clone the following GitHub repository: 
#   https://github.com/rdpeng/ExData_Plotting1

# For each plot you should

# Construct the plot and save it to a PNG file with a width of 480 pixels and a 
#   height of 480 pixels.

# Name each of the plot files as plot1.png, plot2.png, etc.

# Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding 
#   plot, i.e. code in plot1.R constructs the plot1.png plot. Your code file should include 
#   code for reading the data so that the plot can be fully reproduced. You must also 
#   include the code that creates the PNG file.

# Add the PNG file and R code file to your git repository

# When you are finished with the assignment, push your git repository to GitHub so 
#   that the GitHub version of your repository is up to date. There should be four PNG 
#   files and four R code files.


plot4 <- function() {


    # file column classes
    columnClasses <- c(rep("character", 2), rep("numeric", 7))
    
    # read data
    powerdata <- read.csv(file="household_power_consumption.txt", 
                          header=TRUE,
                          stringsAsFactors=FALSE, 
                          sep=";",
                          skipNul = FALSE,
                          colClasses = columnClasses,
                          na.strings=c("?"))
    
    
    # We will only be using data from the dates 2007-02-01 and 2007-02-02
    dataset <- powerdata[as.Date(powerdata$Date,'%d/%m/%Y')>= '2007-02-01' & 
                             as.Date(powerdata$Date,'%d/%m/%Y')<= '2007-02-02',]
    
    
    # assure data in Global_active_power column (for this plot)
    plot4data <- dataset[!is.na(dataset$Global_active_power), ]
    
    
    # create new column for "smart" date
    plot4data$DT <- as.POSIXct(paste(plot4data$Date, plot4data$Time), 
                               format='%d/%m/%Y %H:%M:%S')

    
    #-------------------------------------------------------------------    
    # PLOTS
    #-------------------------------------------------------------------    

    graphics.off()
    
    # define plotting grid (row based)
    par(mfrow=c(2,2))
    
    # set margins (global parameter)
    par(oma=c(1, 1, 1, 0))   
    par(mar=c(6, 5, 4, 3))
    par(cex=0.6)
    
    #-------------------------------------------------------------------    
        
    # plot 1 
    with (plot4data, 
          plot(DT, Global_active_power, 
               type="l",
               xlab="",
               ylab="Global Active Power"
          )
    )
    
    #-------------------------------------------------------------------    
    
    # plot 2
    with (plot4data, 
          plot(DT, Voltage, 
               type="l",
               xlab="datetime",
               ylab="Voltage"
          )
    )
    
    #-------------------------------------------------------------------    
    
    # plot 3
    with(plot4data, 
         plot(DT, Sub_metering_1, 
              type = "n",
              main = "", 
              xlab="",
              ylab="Energy sub metering"
         )
    )
    with(subset(plot4data), points(DT, Sub_metering_1, col = "black", type="l")) 
    with(subset(plot4data), points(DT, Sub_metering_2, col = "red", type="l")) 
    with(subset(plot4data), points(DT, Sub_metering_3, col = "blue", type="l")) 
    
    legend("topright",
           text.width=strwidth("  Sub_metering_1  "),
           xjust=0.5,
           cex=0.8,  # rel text size
           x.intersp=1.1,
           y.intersp=1.3,
           lty = c(1, 1, 1),
           pch = c(NA, NA, NA), 
           col = c("black", "red", "blue"), 
           bty="l",
           box.lwd=0,
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    #-------------------------------------------------------------------

    # plot 4
    with (plot4data, 
          plot(DT, Global_reactive_power, 
               type="l",
               xlab="datetime"
               #ylab=""
          )
    )
    
    #-------------------------------------------------------------------
    
    # save screen plot as a PNG file
    dev.copy(png,"plot4.png", width = 480, height = 480, type="quartz", bg="white")
    dev.off()

}


