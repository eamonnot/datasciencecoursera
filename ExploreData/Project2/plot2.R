# Step 1, load the data.
# Note, assumption is that the RDS files are unzip and in a folder "data" in the working directory.
NEI <- readRDS("data/summarySCC_PM25.rds")

# Step 2. Subset to extract Baltimore City, Maryland data
subNEI <- NEI[NEI$fips == "24510",]

# Step 3, use aggreegate to calculate the sums
agg <- with(subNEI,aggregate(Emissions, by=list(year = year), FUN = sum))

# Step 4, Plot the summary data
png(file="plot2.png", units = "px", height = 480, width = 580)
with(agg, plot(year, x, type="l", xlab="Year", ylab="PM2.5 Emissions (Tons)",
               col="red", lwd=3, main = "Overall Emissions 1999-2008 (Baltimore City, Maryland)"))

dev.off()
