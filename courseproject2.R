## KHALIL H NAJAFI
## COURSE PROJECT 2 - MIXED PLOTS
## The following code fully creates several plots, Displaying PM2.5 trends


## Set working directory & Download dataset & Install/load libraries
rm(list=ls())
if (!dir.exists("./A2")) {
        dir.create("./A2")
}
setwd(file.path("./A2"))

temp <- tempfile()
zipURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(zipURL, dest = temp, method = "curl")
data <- readRDS(unz(temp, "summarySCC_PM25.rds"))
code <- readRDS(unz(temp, "Source_Classification_Code.rds"))
unlink(temp)
pkgs <- c("dplyr", "ggplot2")
if(length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(pkgs, rownames(installed.packages())))
}
library(dplyr)
library(ggplot2)

## PLOT 1

# Sum PM2.5 by year
data <- tbl_df(data)
annual <- data %>% group_by(year) %>% summarize(sum(Emissions)) %>% rename("PM2.5" = `sum(Emissions)`) %>% mutate(PM2.5m = PM2.5/10^6)

# Construct plot1
par(cex = 11/13)
with(annual, plot(year, PM2.5m, main = "US Total PM 2.5 Emissions by Year", xlab = "Year", ylab = "PM2.5 (millions of tons)", type = "n"))
lines(annual$year, annual$PM2.5m, col = "blue")
points(annual$year, annual$PM2.5m, pch = 21, col = "grey", bg = "lightslategrey")
mtext("All counties; all sources", 3, col = "darkslategrey", cex = 2/3)
dev.copy(png, file = "plot1.png") # create PNG 
dev.off()

## PLOT 2

# Subset to Baltimore data
bmo <- data %>% filter(fips == 24510) %>% group_by(year) %>% summarize(sum(Emissions)) %>% rename("PM2.5_Baltimore" = `sum(Emissions)`)

# Construct plot2
par(cex = 11/13)
with(bmo, plot(year, PM2.5_Baltimore, main = "Total PM 2.5 Emissions by Year", xlab = "Year", ylab = "PM2.5 (thousands of tons)", type = "n"))
lines(bmo$year, bmo$PM2.5_Baltimore, col = "red")
points(bmo$year, bmo$PM2.5_Baltimore, pch = 21, col = "grey", bg = "lightslategrey")
mtext("Baltimore, MD; all sources", 3, col = "darkslategrey", cex = 2/3)
dev.copy(png, file = "plot2.png") # create PNG
dev.off()

## PLOT 3

# Subset to Baltimore data, by year and type
bmotype <- data %>% filter(fips == 24510) %>% group_by(year, type) %>% summarize(sum(Emissions)) %>% rename("PM2.5" = `sum(Emissions)`)
bmotype$type <- as.factor(bmotype$type) # make 'type' factor variable

# Construct plot3
p <- ggplot(bmotype, aes(year, PM2.5))
plot3 <- p + geom_point(aes(colour = bmotype$type), size = 4, alpha = 3/4) + geom_smooth(aes(colour = bmotype$type)) + labs(title = "Baltimore Pollution by Type (99-08)", x = "Year", y = expression(PM[2.5] * " (thousands of tons)"), colour = "Type")
plot3 # Print plot
dev.copy(png, file = "plot3.png") # create PNG
dev.off()

## PLOT 4

# Find SCC for coal combustion related activity
# Using the EI.Sector variable as the source, to find all occurrences of 'coal'
# all results also include 'comb' (combustion) and therefore excluding this from the search is inconsequential
sub <- code[grep("*coall*", code$EI.Sector, ignore.case = T), ] 
# Subset dataset to only the matching SCCs related to coal combustion activity
# Group by year to construct change in total over observed years
coal <- data %>% filter(SCC %in% sub$SCC) %>% group_by(year) %>% summarize(sum(Emissions))

# Construct plot4
p <- ggplot(coal, aes(year, `sum(Emissions)`))
plot4 <- p + geom_point(colour = "darkgreen", size = 4, alpha = 1/3) + geom_smooth(colour = "darkgreen") + labs(title = "US Pollution from Coal Combustion-Related Activity", x = "Year", y = expression(PM[2.5] * " (tons)"))
plot4
dev.copy(png, file = "plot4.png") # create PNG
dev.off()

## PLOT 5

# Subset data to Baltimore only, and Motor Vehicle only and group by year
# NOTE: Motor Vehicle is determined by selecting 'ON-ROAD' type, as the SCCs contain an exhaustive
# list of sources within the definition of 'motor vehicle'
bmomv <- data %>% filter(fips == "24510" & type == "ON-ROAD") %>% group_by(year) %>% summarize(sum(Emissions))

# Construct plot5
p <- ggplot(bmomv, aes(year, `sum(Emissions)`))
plot5 <- p + geom_point(colour = "orange", size = 4, alpha = 1/3) + geom_smooth(colour = "orange") + labs(title = "Baltimore Pollution from Motor Vehicles", x = "Year", y = expression(PM[2.5] * " (tons)"))
plot5
dev.copy(png, file = "plot5.png") # create PNG
dev.off()

## PLOT 6

# Subset data to Baltimore OR Los Angeles, Motor Vehicle only, then group by year
# NOTE: Motor Vehicle is determined by selecting 'ON-ROAD' type, as the SCCs contain an exhaustive
# list of sources within the definition of 'motor vehicle'
bmo.la <- data %>% filter((fips == "24510" | fips == "06037") & type == "ON-ROAD") %>% group_by(fips, year) %>% summarize(sum(Emissions)) %>% rename("Emissions" = `sum(Emissions)`)
bmo.la$fips <- as.factor(bmo.la$fips)
levels(bmo.la$fips) <- c("Los Angeles, CA", "Baltimore, MD")

# Construct plot6
p <- ggplot(bmo.la, aes(x = year, y = Emissions, group = fips, col = fips))
plot6 <- p + geom_point(aes(colour = fips), size = 4, alpha = 1/3) + geom_smooth(aes(colour = fips), se = F) + labs(title = "Pollution by Motor Vehicles", x = "Year", y = expression(PM[2.5] * " (tons)"), colour = "City")
plot6.labels <- data.frame(
        Year = c(2006, 2006),
        Emissions = c(4100, 400),
        label = c("ΔLA = 170t", "ΔBaltimore = -259t"),
        fips = c("Los Angeles, CA", "Baltimore, MD"))
plot6 + geom_text(data = plot6.labels, aes(x = Year, y = Emissions, label = label), show_guide = F, size = 4)
dev.copy(png, file = "plot6.png") # create PNG
dev.off()