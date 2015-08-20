# Step 1, load the data.
# Note, assumption is that the RDS files are unzip and in a folder "data" in the working directory.
NEI <- readRDS("data/summarySCC_PM25.rds")

# Step 2, load the ggplot library
library(ggplot2)

# Step 3, Subset to just Baltimore City, Maryland
subNEI <- NEI[NEI$fips == "24510",]

# Step 4, use aggreegate to calculate the sums
agg <- with(subNEI, aggregate(Emissions, by=list(type = type,year = year), FUN=sum))

# Step 5 draw plot
png("plot3.png", units = "px", height = 480, width = 580)
g <- ggplot(data = agg, aes(year,x, colour=type, group=type)) 
g <- g + geom_line(stat = "summary", fun.y = "sum", size=1)
g <- g + labs(title="Detailed PM2.5 Emissions 1999-2008 (Balitmore City, Maryland)")
g <- g + labs(y = "PM2.5 Emissions (Ton)")
g <- g + labs(x = "Year")
print(g)
dev.off()
