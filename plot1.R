

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


plot1 <- function() {


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
    plot1data <- dataset[!is.na(dataset$Global_active_power), ]

    
    # set margins (global parameter)
    par(mar=c(6, 6, 4, 2))
    
    # default text size
    par(cex=0.8)
    
    # plot data
    hist(plot1data$Global_active_power, 
         col="red",
         main="Global Active Power",
         xlab="Global Active Power (kilowatts)"
    )
    
    # save screen plot as a PNG file
    dev.copy(png,"plot1.png", width = 480, height = 480, type="quartz", bg="white")
    dev.off()

}


