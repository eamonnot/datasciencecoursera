# Step 1, load the data.
# Note, assumption is that the RDS files are unzip and in a folder "data" in the working directory.
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Step 2 - Find the coal combustion methods, using SCC$EI.Sector
rowsToKeep <- grep("^fuel comb -(.*)- coal$", SCC$EI.Sector, ignore.case=T)
CoalCombSCC <- SCC[rowsToKeep,]

# Step 3 - Subset NEI to just the CoalCombSCC values
coalCombNEI <- NEI[NEI$SCC %in% CoalCombSCC$SCC,]

# Step 4, use aggreegate to calculate the sums
coalEmissions <- with(coalCombNEI,aggregate(Emissions, by=list(year = year), FUN = "sum"))

# Step 5, Plot the summary data
png(file="plot4.png", units = "px", height = 480, width = 580)
with(coalEmissions, plot(year, x, type="l", xlab="Year", ylab="PM2.5 Emissions (Tons)",
               col="red", lwd=3, main = "Coal Combustion Emissions 1999-2008 (United States)"))
dev.off()
