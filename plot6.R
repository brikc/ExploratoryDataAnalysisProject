plot6 <- function() {
    
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
    
    
    # Compare emissions from motor vehicle sources in Baltimore City with 
    # emissions from motor vehicle sources in Los Angeles County,  
    # California (fips == "06037"). Which city has seen greater changes  
    # over time in motor vehicle emissions?
    
    # First attach the SCC data to the NEI table using merge so that we 
    # can filter on source type for motor vehicle
    merged <- merge(NEI,SCC,by.x="SCC",by.y="SCC",all=FALSE)
    
    # Filter for Baltimore and LA records:
    cityEmissions <- merged[which(merged$fips == "24510"|merged$fips == "06037"),]

    # Filter further for only motor vehcile emissions - same filter 
    # as question 5
    cityVehEmissions <- filter(cityEmissions,grepl("vehicle",cityEmissions$EI.Sector,ignore.case=TRUE))
    
    # Aggregate and sum the Baltimore City Data
    yearlyTotal <- aggregate(cityVehEmissions$Emissions, by=list(cityVehEmissions$year,cityVehEmissions$fips), FUN = sum, na.rm = TRUE)

    # Add logical column names to the new table
    colNames <- c("year","fips","totalEmissions")
    colnames(yearlyTotal) <- colNames
    
    # Create a dataframe of the city names to make the data pretty
    cities <- c("Los Angeles County","Baltimore City, MD")
    cityFips <- c("06037","24510")
    cityDF <-cbind(cityFips,cities)
    
    # Apply the city names to the total dataframe
    cityYearlyTotal <- merge(yearlyTotal,cityDF,by.x="fips",by.y="cityFips",all=TRUE)
    

    # Load ggplot2 library
    library(ggplot2)
    
    # When viewing the plot of the data points for the two cities, it's 
    # very difficult to tell which city experienced more change.  So I 
    # broke the data for the two cities into different plots using facets 
    # and graphed a line to better illustrate the relative level of change 
    # that each experienced.
    
    title = "Change in Total Yearly Motor Vehicle Emissions by City"
    myplot <- qplot(year,totalEmissions,data=cityYearlyTotal,color=cities)+labs(title = title, x="Year",y="Total Emissions")+facet_grid(cities~.,scales="free_y")+geom_smooth(method="lm",se=FALSE)
    
    # Open device
    png(filename="plot6.png",width=480,height=480,units="px")
    
    print(myplot)
    
    # Close device
    dev.off()
    
}