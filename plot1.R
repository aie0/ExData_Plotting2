# starts plotting into png file with received name
startPlotToFile = function(name) {
    png(file = name, width = 480, height = 480)
}

# ends plotting
endPlotToFile = function() {
    dev.off()
}

# loads and returns pm2.5 data
# (NEI, SCC)
loadData = function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    list(NEI = NEI, SCC = SCC)
}

# renders a plot of total 2.5 emmision per year by received data
render = function(NEI, SCC) {
    yearEmmisions = aggregate(NEI$Emissions, by=list(NEI$year), FUN = "sum")
    names(yearEmmisions) = c("Year", "Total")    
    MILLION = 1000000
    yLabel = "Emission"
    if(max(yearEmmisions$Total) > MILLION) {
        yearEmmisions$Total = yearEmmisions$Total / MILLION
        yLabel = paste(yLabel, "(Millions of tons)")
    } else
        yLabel = paste(yLabel, "(tons)")
    plot(yearEmmisions, type = "l", ylab=yLabel, main="Total PM2.5 emission per year")
}

# renders a plot of total 2.5 emmision per year into plot1.png
plot1 = function() {
    data = loadData()
    startPlotToFile("plot1.png")
    render(data$NEI, data$SCC)
    endPlotToFile()
}