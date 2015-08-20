# Step 1, load the data.
# Note, assumption is that the RDS files are unzip and in a folder "data" in the working directory.
NEI <- readRDS("data/summarySCC_PM25.rds")

# Step 2, use aggreegate to calculate the sums
agg <- with(NEI,aggregate(Emissions, by=list(year = year), FUN = sum))

# Step 3, Plot the summary data
png(file="plot1.png", units = "px", height = 480, width = 580)
with(agg, plot(year, x, type="l", xlab="Year", ylab="PM2.5 Emissions (Tons)",
               col="red", lwd=3, main = "Overall Emissions 1999-2008 (United States)"))

dev.off()
