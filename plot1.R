plot1 <- function() {
    
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
    
    
    # Question: Have total emissions from PM2.5 decreased in the United  
    # States from 1999 to 2008? Using the base plotting system, make a plot  
    # showing the total PM2.5 emission from all sources for each of the  
    # years 1999, 2002, 2005, and 2008.
    
    # To answer this we will need to group by year and perform a sum of the
    # Emissions column.
    
    totalEmissions <- aggregate(NEI$Emissions, by=list(NEI$year), FUN = sum, na.rm = TRUE)
    
    # Add logical column names to the new table
    colNames <- c("year","totalEmissions")
    colnames(totalEmissions) <- colNames
    
    # Open device
    png(filename="plot1.png",width=480,height=480,units="px")
    
    # Create plot:
    plot(totalEmissions, ylab="Total Emissions (in millions)", xlab="Year", main="Total Yearly Emissions for All Locations", type="b",yaxt="n")
    axis(side=2,at=seq(max(totalEmissions$totalEmissions),min(totalEmissions$totalEmissions),length=4),labels=round(seq(max(totalEmissions$totalEmissions)/1000000,min(totalEmissions$totalEmissions)/1000000,length=4),digits=2))
    # Close device
    dev.off()
    
}