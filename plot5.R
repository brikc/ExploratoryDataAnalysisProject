plot5 <- function() {
    
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
    
    
    # How have emissions from motor vehicle sources changed from 1999–2008 
    # in Baltimore City?
    
    # To answer this we will need to subset the data for this specificed 
    # city as well as source types of "motor vehicle"
    # and then group by year and perform a sum of the Emissions column.
    
    # First attach the SCC data to the NEI table using merge so that we can
    # filter on source type for motor vehicle
    merged <- merge(NEI,SCC,by.x="SCC",by.y="SCC",all=FALSE)
    
    # Filter for only Baltimore records:
    bcEmissions <- merged[which(merged$fips == "24510"),]
    
    # Similar to question 4, this involved a lot of exploration of the  
    # data to decide what to filter on.  These filters all provided a very 
    # similar number of observations:
    # "vehicle" in EI.Sector - 1138
    # "veh" in Short.Name - 1185
    # Onroad in Data.Category - 1137
    
    # So, I settled on searching for "vehicle" EI.Sector using dplyr
    library(dplyr)
    bcVehicle <- filter(bcEmissions,grepl("vehicle",bcEmissions$EI.Sector,ignore.case=TRUE))
    
    yearlyTotal <- aggregate(bcVehicle$Emissions, by=list(bcVehicle$year), FUN = sum, na.rm = TRUE)
    
    # Add logical column names to the new table
    colNames <- c("year","totalEmissions")
    colnames(yearlyTotal) <- colNames
    
    # Open device
    png(filename="plot5.png",width=480,height=480,units="px")
    
    # Create plot:
    plot(yearlyTotal, ylab="Total Emissions", xlab="Year", main="Total Yearly Emissions\nFor Motor Vehicles\nfor Baltimore City, MD", type="b",yaxt="n")
    axis(side=2,at=seq(max(yearlyTotal$totalEmissions),min(yearlyTotal$totalEmissions),length=4),labels=round(seq(max(yearlyTotal$totalEmissions),min(yearlyTotal$totalEmissions),length=4),digits=2))
    # Close device
    dev.off()
    
}