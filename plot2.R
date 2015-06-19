plot2 <- function() {
    
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
    
    
    # Have total emissions from PM2.5 decreased in the Baltimore City,  
    # Maryland (fips == "24510") from 1999 to 2008? Use the base 
    # plotting system to make a plot answering this question.
    
    # To answer this we will need to subset the data for this specificed 
    # city and then group by year and perform a sum of the Emissions column.
    
    bcEmissions <- NEI[which(NEI$fips == "24510"),]
    
    yearlyTotal <- aggregate(bcEmissions$Emissions, by=list(bcEmissions$year), FUN = sum, na.rm = TRUE)
    
    # Add logical column names to the new table
    colNames <- c("year","totalEmissions")
    colnames(yearlyTotal) <- colNames
    
    # Open device
    png(filename="plot2.png",width=480,height=480,units="px")
    
    # Create plot:
    plot(yearlyTotal, ylab="Total Emissions (in thousands)", xlab="Year", main="Total Yearly Emissions for Baltimore City, MD", type="b",yaxt="n")
    axis(side=2,at=seq(max(yearlyTotal$totalEmissions),min(yearlyTotal$totalEmissions),length=4),labels=round(seq(max(yearlyTotal$totalEmissions/1000),min(yearlyTotal$totalEmissions/1000),length=4),digits=2))
    # Close device
    dev.off()
    
}