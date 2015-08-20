# Step 1, load the data.
# Note, assumption is that the RDS files are unzip and in a folder "data" in the working directory.
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Step 2 - Subset NEI to just Balitmore City, Maryland
subNEI <- NEI[NEI$fips == "24510",]

# Step 3 - Find the motor vehicle methods, using SCC$Data.Category and SCC$EI.Sector
# Having looked at the data I define a motor vehicle to be any vehicle that is classed as OnRoad
subSCC <-  SCC[SCC$Data.Category == "Onroad",] 
rowsToKeep <-  grep("vehicle", subSCC$EI.Sector, ignore.case=T) 
MotorVehicleSCC <- subSCC[rowsToKeep,]

# Step 4 - Subset NEI to just the CoalCombSCC values
motorSubNEI <- subNEI[subNEI$SCC %in% MotorVehicleSCC$SCC,]

# Step 5, use aggreegate to calculate the sums
agg <- with(motorSubNEI, aggregate(Emissions, by=list(year = year), FUN=sum))

# Step 6 draw plot
png("plot5.png", height = 480, width = 580, units = "px")
with(agg, plot(year, x, type="l", xlab="Year", ylab="PM2.5 Emissions (Tons)",
               col="red", lwd=3, main = "Motor Vehicle Emissions 1999-2008 (Baltimore City, Maryland)"))
dev.off()
