library("ggplot2")

# plots to file
plotToFile = function(fileName) {
    ggsave(file=fileName, dpi=72, width = 6.66, height = 6.66)
}

# loads and returns pm2.5 data filtering by U.S. county
# (NEI, SCC)
loadData = function(fips) {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    NEI = NEI[NEI$fips == fips,]    # filter by fips
    list(NEI = NEI, SCC = SCC)
}

# renders a plot of total 2.5 emmision per year per emission type by received data
render = function(NEI, SCC) {
    yearEmmisionsByType = aggregate(NEI$Emissions, by=list(NEI$year, NEI$type), FUN = "sum")
    names(yearEmmisionsByType) = c("Year", "Type", "Total")    
    MILLION = 1000000
    yLabel = "Emission"
    if(max(yearEmmisionsByType$Total) > MILLION) {
        yearEmmisionsByType$Total = yearEmmisionsByType$Total / MILLION
        yLabel = paste(yLabel, "(Millions of tons)")
    } else
        yLabel = paste(yLabel, "(tons)")
    qplot(Year, Total, data=yearEmmisionsByType, color=Type, geom="line", ylab = yLabel, main="Total PM2.5 emission per year")
}

# renders a plot of total 2.5 emmision per year in Baltimore City per emission type, Maryland into plot3.png
plot3 = function() {
    data = loadData("24510")    
    render(data$NEI, data$SCC)
    plotToFile("plot3.png")
}