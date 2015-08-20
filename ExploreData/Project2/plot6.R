# Step 1, load the data.
# Note, assumption is that the RDS files are unzip and in a folder "data" in the working directory.
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Step 2 - Subset NEI to just Baltimore City and LA County
subNEI <- NEI[NEI$fips == "24510" | NEI$fips == "06037",]

# Step 3 - Find the motor vehicle methods, using SCC$Data.Category and SCC$EI.Sector
# Having looked at the data I define a motor vehicle to be any vehicle that is classed as OnRoad
subSCC <-  SCC[SCC$Data.Category == "Onroad",] 
rowsToKeep <-  grep("vehicle", subSCC$EI.Sector, ignore.case=T) 
MotorVehicleSCC <- subSCC[rowsToKeep,]

# Step 4 - Subset NEI to just the CoalCombSCC values
motorSubNEI <- subNEI[subNEI$SCC %in% MotorVehicleSCC$SCC,]

# Step 5, use aggreegate to calculate the yearly sums
agg <- with(motorSubNEI, aggregate(Emissions, by=list(year = year,fips = fips), FUN=sum))

# Step 6, As we want to compare changes, use 1999 as the benchmark and
# represent subsequent years as a percentage of this level for each city
Balt <- agg$x[agg$year=="1999" & agg$fips=="24510"]
LAC <- agg$x[agg$year=="1999" & agg$fips=="06037"]

aggBalt <- agg[agg$fips == "24510",]
aggLac <- agg[agg$fips == "06037",]
aggBalt$x <- (aggBalt$x / Balt) * 100
aggLac$x <- (aggLac$x / LAC) * 100

agg <- rbind(aggBalt, aggLac)

# Step 7 -  draw plot
library(ggplot2)
png("plot6.png", height=480, width=580, units="px") 
g <- ggplot(agg, aes(year,x, color=fips))
g <- g + geom_line(stat = "summary", fun.y = "sum", size=1) 
g <- g + labs(title="Changes in Motor Vehicle PM2.5 Emissions
              (Balitmore City vs. Los Angeles County)")
g <- g + labs(y = "Percent of 1999 Emissions Level")
g <- g + labs(x = "Year")
g <- g + scale_colour_discrete(name = "Cities", label= c("Los Angeles County","Baltimore"))
print(g)
dev.off()


