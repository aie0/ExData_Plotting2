library("ggplot2")

# plots to file
plotToFile = function(fileName) {
    ggsave(file=fileName, dpi=72, width = 6.66, height = 6.66)
}

# loads and returns pm2.5 data of vehicle related emmisions filtering by U.S. county list
# (NEI, SCC)
loadData = function(fips, countyNames) {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")

    sectors = levels(SCC$EI.Sector)
    # filter vehicle related categories
    coalRelated = sectors[grep("Vehicles", sectors)]
    
    # filter vehicle related sources
    SCC = SCC[SCC$EI.Sector %in% coalRelated,]
    
    NEI = NEI[NEI$fips %in% fips,]    # filter by fips
    
    #filter vehicle related measurements 
    NEI = merge(NEI, SCC, by = "SCC")
    
    counties = data.frame(fips = fips, county = countyNames)
    
    # add county name
    NEI = merge(NEI, counties, by = "fips")
    list(NEI = NEI, SCC = SCC)
}

# renders a plot of total 2.5 emmision per year per emission type by received data
render = function(NEI, SCC) {
    yearEmmisionsByType = aggregate(NEI$Emissions, by=list(NEI$year, NEI$county), FUN = "sum")
    names(yearEmmisionsByType) = c("Year", "County", "Total")    
    MILLION = 1000000
    yLabel = "Emission"
    if(max(yearEmmisionsByType$Total) > MILLION) {
        yearEmmisionsByType$Total = yearEmmisionsByType$Total / MILLION
        yLabel = paste(yLabel, "(Millions of tons)")
    } else
        yLabel = paste(yLabel, "(tons)")
    qplot(Year, Total, data=yearEmmisionsByType, color=County, geom="line", ylab = yLabel, main="Total PM2.5 emission per year")
}

# renders a plot of total vehicle related 2.5 emmision per year in Baltimore City and Los Angeles into plot6.png
plot6 = function() {
    data = loadData(c("24510", "06037"), c("Baltimore City", "Los Angeles County"))
    render(data$NEI, data$SCC)
    plotToFile("plot6.png")
}