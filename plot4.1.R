## THIS DOES THE SAME THING AS PLOT 4.  JUST CHECKING METHOD OF FILTERING.


plot4.1 <- function() {
    
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
    
    
    # Across the United States, how have emissions from coal combustion-related 
    # sources changed from 1999–2008?
    
    # Determining which sources were coal combustion-related is very subjective,
    # and after much contemplation and a thorough reading of the message boards
    # I determined that the most comprehensive filtering for this source is by
    # searching for "coal" and "comb" in the Short.Name column.
    
    # First attach the Short.Name to the NEI table using merge
    merged <- merge(NEI,SCC,by.x="SCC",by.y="SCC",all=FALSE)
    
    filterTest <- filter(merged, grepl("comb",merged$SCC.Level.One,ignore.case=TRUE)&grepl("coal",merged$SCC.Level.Three,ignore.case=TRUE))
    
    # Next we will need to group by year and do a sum
    yearlyTotal <- aggregate(filterTest$Emissions, by=list(filterTest$year), FUN = sum, na.rm = TRUE)
    
    # Add logical column names to the new table
    colNames <- c("year","totalEmissions")
    colnames(yearlyTotal) <- colNames

    # Open device
    png(filename="plot4.1.png",width=480,height=480,units="px")
    
    # Create plot:
    plot(yearlyTotal, ylab="Total Emissions (in hundred thousands)", xlab="Year", main="Total Yearly Emissions\nCoal Combustion-Related\nAll Locations", type="b",yaxt="n")
    axis(side=2,at=seq(max(yearlyTotal$totalEmissions),min(yearlyTotal$totalEmissions),length=4),labels=round(seq(max(yearlyTotal$totalEmissions/100000),min(yearlyTotal$totalEmissions/100000),length=4),digits=2))
    # Close device
    
    
    # Close device
    dev.off()
    
}