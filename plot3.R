plot3 <- function() {
    
    # First check to see if the file has already been downloaded and
    # is in the working directory
    
    if (!file.exists("exdata-data-NEI_data/Source_Classification_Code.rds")) {
        
        # If not, download and unzip the file
        filename<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(filename,destfile="exdata-data-NEI_data.zip",method="curl")
        unzip("exdata-data-NEI_data.zip")
            
    }
        
    ## Read in the data files
    ## This first line will likely take a few seconds. Be patient!
    NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
    SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
    
    
    # Of the four types of sources indicated by the type (point, nonpoint, 
    # onroad, nonroad) variable, which of these four sources have seen  
    # decreases in emissions from 1999–2008 for Baltimore City? Which have 
    # seen increases in emissions from 1999–2008? Use the ggplot2 plotting  
    # system to make a plot answer this question.
    
    # To answer this we will first need to subset the data for this 
    # specificed city 
    bcEmissions <- NEI[which(NEI$fips == "24510"),]
    
    # Next we will need to group by both year and type and do a sum
    yearlyTotalByType <- aggregate(bcEmissions$Emissions, by=list(bcEmissions$year, bcEmissions$type), FUN = sum, na.rm = TRUE)
    
    # Add logical column names to the new table
    colNames <- c("year","type","totalEmissions")
    colnames(yearlyTotalByType) <- colNames
    
    
    # Load ggplot2 library
    library(ggplot2)
    
    # Create plot:
    myplot <- qplot(year,totalEmissions,data=yearlyTotalByType,color=type)+geom_line()+labs(title = "Total Yearly Emissions for Baltimore City, MD by Type", x="Year",y="Total Emissions")

    # Open device
    png(filename="plot3.png",width=480,height=480,units="px")
    
    print(myplot)
    
    # Close device
    dev.off()
    
}