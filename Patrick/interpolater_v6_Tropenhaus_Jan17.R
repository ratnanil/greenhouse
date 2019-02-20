#######################################################################
# Interpolation Klimamonitoring: File Parser
# 
# laup, 10. 12. 2015; angepasst durch humo 16.12.2015 und 13.11.2017
#######################################################################

#######################################################################
#
# - Wichtig: Loggernamen-Konvention: tropFeb15_5_1_el_usb.txt muss tropFeb15_05_1_el_usb.txt werden, sonst gibts Probleme mit den Loggernamen.
# - EL-USB-Files haben andere Datumsangaben (2017-01-17 statt 13/12/2016): Skript umgeschrieben
#
#######################################################################

# http://rpubs.com/adam_dennett/10873
Sys.setenv(http_proxy="http://proxy.zhaw.ch:8080")
rm(list=ls(all.names=TRUE))


# Lade noetige libraries
library(reshape)
library(plyr)
library(stats)
library(sp)
library(tripack)
library(lattice)
library(akima)
library(ggplot2)
library(gstat)
library(maptools)
library(gridExtra)


#######################################################################
# set variables
#######################################################################

# Mode: IDW or kriging. IDW needs "thisIdp", kriging needs "fit"
mode <- "IDW"
thisIdp <- 2   # idp is the power of the weights for IDW: 1/d (1), 1/d^2 (2), 1/d^3 (3), ..

# mode <- "kriging"
# fit = fit.variogram(vgm, model = vgm(1, "Sph", 150, 1))

# What variable to read? Reading Celsius(?C) or Humidity(%rh) or dew point(?C)
variable <- "temp"
# variable <- "hum"
# variable <- "dew"


# define interval
## milder Wintertag
start_dateTime <- as.POSIXct(strptime("2017-02-03 01:00:00", "%Y-%m-%d %H:%M:%S"))
end_dateTime <- as.POSIXct(strptime("2017-02-04 01:00:00", "%Y-%m-%d %H:%M:%S"))

## kalter Wintertag
start_dateTime <- as.POSIXct(strptime("2017-01-25 01:00:00", "%Y-%m-%d %H:%M:%S"))
end_dateTime <- as.POSIXct(strptime("2017-01-26 01:00:00", "%Y-%m-%d %H:%M:%S"))

## ganzer Monat (18.1.-17.2.17)
start_dateTime <- as.POSIXct(strptime("2017-01-18 01:00:00", "%Y-%m-%d %H:%M:%S"))
end_dateTime <- as.POSIXct(strptime("2017-02-17 23:00:00", "%Y-%m-%d %H:%M:%S"))

start_dateTime_str <- paste("T", strftime(start_dateTime, format='%d%m%y%H%M%S'), sep="")
end_dateTime_str <- paste("T", strftime(end_dateTime, format='%d%m%y%H%M%S'), sep="")


# Set counter for length of measurement series. Not all series have the same length, I'll trim them so all have the same length
min_series <- 5000000


# Set the type of color range 
# range_type <- 3 # use global color range
# range_type=1: use instantaneous color range, i.e. spread  color range over just one time stamp
range_type <- 2 # use manually defined color range
# range_type=3: use global color range (sucht min / max Werte des gew?hlten Zeitpunktes und erstreckt ?ber die vorhanden Werte die Skala)
#
manual_min <- 11 # only used in range_type=2
manual_max <- 27 # only used in range_type=2


# read file containing Aussentemperatur and Solltemperatur (dieses File vorg?ngig anpassen!! Und unten im Code den Namen anpassen)
# This is the output of aussenSollParse.R
aussenSoll <- read.csv(file = 'Patrick/aussenSoll_Tropenhaus_Jan17.csv', header=TRUE, sep=",")

# Read the coordinates of the logger
stao <- read.csv('Patrick/TROP_staos.csv', header=TRUE, sep=';')


#######################################################################
# input / output
#######################################################################

# Set variables
file_list <- list.files(path='Patrick/el_usb', pattern="*.txt", full.names = TRUE,
                         recursive = TRUE, include.dirs = FALSE)
# Note: All files in folder have same length, i.e. same number of lines, i.e. same time span.

# Loop all files, read 'em and stitch them together
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE, sep=",")
    if (nrow(dataset) < min_series) {min_series <- nrow(dataset)}
    
    # Reading Celsius(?C) or Humidity(%rh) or dew point(?C)
    # dataset <- dataset[, c(1:3)] # Celsius(?C)
    # dataset <- dataset[, c(1:2,4)] # Humidity(%rh)
    # dataset <- dataset[, c(1:2,5)] # dew point(?C)
    
    if (variable == "temp") {
      dataset <- dataset[, c(1:3)] # Celsius(?C)
    } else if (variable == "hum") {
      dataset <- dataset[, c(1:2,4)] # Humidity(%rh)
    } else if (variable == "dew") {
      dataset <- dataset[, c(1:2,5)] # dew point(?C)
    } else
      print("Invalid variable has been set.")
      
    
    
    logger <- substr(file, start=nchar(file)-20, stop=nchar(file)-11)
    logger <- substr(logger, start=6, stop=10)
    logger <- paste("Log",logger, sep = "")
    logger_s <- toString(logger)
    
    names(dataset) <- c("messID", "dt_string", logger_s)
    
    dataset$datetime_lt <- strptime(dataset$dt_string, '%Y-%m-%d %H:%M:%S')  ## angepasst
    dataset$datetime_ct <- as.POSIXct(dataset$datetime_lt)
    dataset$dateTimeID <- paste("T", strftime(dataset$datetime_lt, format='%d%m%y%H%M%S'), sep="")
    dataset$day <- strftime(dataset$datetime_lt, format='%a %d %b %Y')
    dataset$hour <- strftime(dataset$datetime_lt, format='%H')
    
    
    dataset <- dataset[c(1,2,4:8,3)]
  
  }
  
  # if the merged dataset does exist, append to it
  else {
    temp_dataset <-read.csv(file, header=TRUE, sep=",")
    if (nrow(temp_dataset) < min_series) {min_series <- nrow(temp_dataset)}
    
    # temp_dataset <- temp_dataset[, c(1,3)]
    
    # Reading Celsius(?C) or Humidity(%rh) or dew point(?C)
    # temp_dataset <- temp_dataset[, c(1,3)] # Celsius(?C)
    # temp_dataset <- temp_dataset[, c(1,4)] # Humidity(%rh)
    # temp_dataset <- temp_dataset[, c(1,5)] # dew point(?C)
    
    if (variable == "temp") {
      temp_dataset <- temp_dataset[, c(1,3)] # Celsius(?C)
    } else if (variable == "hum") {
      temp_dataset <- temp_dataset[, c(1,4)] # Humidity(%rh)
    } else if (variable == "dew") {
      temp_dataset <- temp_dataset[, c(1,5)] # dew point(?C)
    } else
      print("Invalid variable has been set.")
    
    logger <- substr(file, start=nchar(file)-20, stop=nchar(file)-11)
    logger <- substr(logger, start=6, stop=10)
    logger <- paste("Log",logger, sep = "")
    logger_s <- toString(logger)
    
    names(temp_dataset) <- c("messID", logger_s)
    dataset <- merge(dataset, temp_dataset, by='messID', all.x=T)
    rm(temp_dataset)
  }
  
}

# Report min_series
print(paste("min_series =", min_series))
# trim dataset to min_series
dataset <- dataset[1:min_series,]


loggerNames <- names(dataset)[!is.element(names(dataset), c('messID','dt_string','datetime_lt','datetime_ct','dateTimeID','day','hour'))]
loggerNamesTemp <- loggerNames
loggerNames <- loggerNames[-1]
oldNameBase <- substr(loggerNames[1], start=1, stop=6)

merged <- c()
for (name in loggerNames){
  newNameBase <- substr(name, start=1, stop=6)
  if (oldNameBase == newNameBase) {
    first <- paste(oldNameBase,'_1',sep='')
    second <- paste(oldNameBase,'_2',sep='')
    dataset[, toString(newNameBase)] <- ( dataset[toString(first)] + dataset[toString(second)] ) / 2
    merged <- append(merged, toString(first))
    merged <- append(merged, toString(second))
  }
  
  oldNameBase <- newNameBase
}


dataset <- dataset[,!(names(dataset) %in% merged)]
dataset <- rename(dataset, c("dateTimeID"="aa_dateTimeID"))


ncol_df <- ncol(dataset)
data_GradC <- dataset[c(5, 8:ncol_df)]
data_GradC <- data_GradC[,order(names(data_GradC))]

# PROBLEM: Feb15_05 und Feb15_05_1
shortNames <- c()
for (name in names(data_GradC)){
  name <- substr(name, start=1, stop=6)
  shortNames <- append(shortNames, toString(name))
}
names(data_GradC) <- shortNames

dataset <- rename(dataset, c("aa_dateTimeID"="dateTimeID"))

# append aussenSoll
dataset <- merge(dataset, aussenSoll, by='dateTimeID', all.x=T)
dataset <- dataset[order(dataset$datetime_lt),] 

# http://stackoverflow.com/questions/6778908/r-transposing-a-data-frame
# first remember the names
n <- data_GradC$aa_dat
            

# transpose all but the first column (name)
data_GradC <- as.data.frame( t(data_GradC[,-1] ) )
colnames(data_GradC) <- n
data_GradC$logger_tag <- factor(row.names(data_GradC))

# join staos
data_GradC_stao <- merge(x = stao, y = data_GradC, by=c("logger_tag"))


# writing data
write.table(data_GradC_stao, "Patrick/out/data_GradC_stao.csv", sep=",", row.names=FALSE, col.names=TRUE, na="NA")
write.table(dataset, "Patrick/out/dataset.csv", sep=",", row.names=FALSE, col.names=TRUE, na="NA")



# Spatail stuff
coordinates(data_GradC_stao) = ~X + Y
plot(data_GradC_stao)


# Interpolation

# plot(dataStao)

## 1. Create a grid from the values in your points dataframe first get the
## range in data
# x.range <- as.integer(range(data_GradC_stao@coords[, 1]))
# y.range <- as.integer(range(data_GradC_stao@coords[, 2]))


## 2. Create a grid with a slightly larger extent
# plot(dataStao)
# use the locator to click 4 points beyond the extent of the plot and use
# those to set your x and y extents
# locator(4)

x.range <- as.integer(c(0, 16))  ### anpassen f?rs Tropenhaus (sind Meterangaben f?r den Frame)
y.range <- as.integer(c(0, 24.5))

## now expand your range to a grid with spacing that you'd like to use in
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1), y = seq(from = y.range[1], 
                                                                                to = y.range[2], by = 0.1))
## convert grid to SpatialPixel class
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

## test it out - this is a good way of checking that your sample points are
## all well within your grid. If they are not, try some different values in
## you r x and y ranges:
#plot(grd, cex = 1.5)
# points(data_GradC_stao, pch = 1, col = "red", cex = 1)
## title("Interpolation Grid and Sample Points")


# Find min max value across all colums
tempDf <- data.frame(data_GradC_stao)
tempDf_max <- (ncol(tempDf) - 1)
tempDf <- tempDf[c(8:tempDf_max)]
# tempDf <- tempDf[c(8:50)]

tempDf <- data.frame(tempDf, row.names=NULL)
min_value <- min(tempDf)
max_value <- max(tempDf)
remove(tempDf_max)
remove(tempDf)



# sucht Index f?r gew?hltes Zeitintervall
start_index <- which(n == start_dateTime_str)
end_index <- which(n == end_dateTime_str)
n <- n[start_index:end_index]

# create a temporary dataset with only numeric values, such that min and max can be computed
data_GradC_min_max <- data_GradC[n]
col_range_min <- min(data_GradC_min_max)
col_range_max <- max(data_GradC_min_max)
remove(data_GradC_min_max)




if (mode == "IDW") {

  for (timestamp in n) {
    
    # Inverse Distance Weighting
    # Set parameters
    dateTimeID <- as.name(timestamp)
    #dateTimeID <- quote(name) # use quote() and eval() to denote to column name, http://stackoverflow.com/questions/12603890/pass-column-name-in-data-table-using-variable-in-r
    idw <- idw(formula = eval(dateTimeID) ~ 1, idp=thisIdp, locations = data_GradC_stao, newdata = grd)
    
    timestamp_temp <- as.data.frame(data_GradC_stao[c(timestamp)])
    names(timestamp_temp) <- c("Value", "X", "Y")
    # Set the color range_type: 1 (just this timestamp, 2 (manual, limits set above), 3 (global, range of entire data set, col_range_min, col_range_max) )
    if(range_type==1){
      col_range_min <- min(timestamp_temp$Value) 
      col_range_max <- max(timestamp_temp$Value)
    } else if(range_type==2){
      col_range_min <- manual_min 
      col_range_max <- manual_max
    }
    
    
    # Visualisation
    idw.output = as.data.frame(idw)
    names(idw.output)[1:3] <- c("X", "Y", "Value")
    plot <- ggplot(data = idw.output, aes(x = X, y = Y)) #start with the base-plot 
    layer1 <- c(geom_tile(data = idw.output, aes(fill = Value)))
    layer2 <- c(geom_point(data = stao, aes(X, Y), colour = "black", size = 3))
    layer3 <- c(geom_text(data = stao, aes(label=logger_tag),hjust=-0.2, vjust=-0.2))
    ## (Patrick hatte diese Wahl drin) layer4 <- c(geom_text(data = timestamp_temp, aes(label=sprintf("%.2f", Value)),hjust=-0.2, vjust=+1.4))
    layer4 <- c(geom_text(data = timestamp_temp, aes(label=sprintf("%.2f", Value)),hjust=0.5, vjust=+1.4, size = 5))
    
    dateTimeID_str <- substr(dateTimeID, start=2, stop=nchar(dateTimeID))
    dateTimeID_lt <- strptime(dateTimeID_str, '%d%m%y%H%M%S')
    dateTime_str <- strftime(dateTimeID_lt, format='%d.%m.%Y %H:%M:%S')
    
    line <- dataset[dataset$dateTimeID==timestamp,]
    aussen1 <- line$aussen14_1_1
    aussen2 <- line$aussen14_1_2
    soll <- line$tempNum
    remove(line, timestamp_temp)
    
    titleString <- paste(variable, ", IDW, idp: ", thisIdp, ", ", dateTime_str, ",\n Aussentemperatur 1: ", aussen1, ",\n Aussentemperatur 2: ", aussen2, ",\n Solltemperatur: ", soll, sep="")
    title <- ggtitle(titleString)
    # plot + layer1 + layer2 + layer3 + layer4 + scale_fill_gradientn(colours = rainbow(50), limits=c(col_range_min, col_range_max)) + coord_fixed(ratio = 1) + title
  ## (das hatte Patrick gew?hlt) plot + layer1 + layer2 + layer3 + layer4 + scale_fill_gradient2(low = 'royalblue4', mid = 'snow2', high = 'red4', midpoint = ((col_range_min+col_range_max)/2), limits=c(col_range_min, col_range_max)) + coord_fixed(ratio = 1) + title
    plot + layer1 + layer2 + layer4 + scale_fill_gradient2(low = 'royalblue4', mid = 'snow2', high = 'red4', midpoint = ((col_range_min+col_range_max)/2), limits=c(col_range_min, col_range_max)) + coord_fixed(ratio = 1) + title
    # ggsaveFileName <- paste('/Users/laup/Documents/projects/interpol_temp/outData/foo.png",sep="")
    ggsaveFileName <- paste('S:/pools/n/N-IUNR-Allgemein/Zentren/Hortikultur/FS_Spezialkulturen/Projekte/Stadtgaertnerei_Zuerich/Temperaturmonitoring/Klimadaten/Auswertungen_Schlussbericht_2017/tropJan17/outData/',variable,"_IDW___",thisIdp,"_",dateTimeID, ".png",sep="")
    ggsave(ggsaveFileName, dpi=300, width=20, height=15, unit="cm", scale=1.5)  ### wird der Film zu schwer, hier die Bildgr?sse anpassen
    
  }

} else if(mode == "kriging") {

  for (timestamp in n) {
    
    dateTimeID <- as.name(timestamp)
    # Kriging
    vgm = variogram(eval(dateTimeID) ~ 1, data_GradC_stao)
    # vgm
    fit = fit.variogram(vgm, model = vgm(1, "Sph", 150, 1))
    # fit
    # plot(vgm, fit)
    kriged = krige(eval(dateTimeID) ~ 1, data_GradC_stao, grd, model = fit)
    
    timestamp_temp <- as.data.frame(data_GradC_stao[c(timestamp)])
    names(timestamp_temp) <- c("Value", "X", "Y")
    # Use timestamp_min instead of min_value when adjusting color scale "locally"
    timestamp_min <- min(timestamp_temp$Value) 
    timestamp_max <- max(timestamp_temp$Value)
    
    # Visualisation
    kriged.output = as.data.frame(kriged)
    names(kriged.output)[1:3] <- c("X", "Y", "Value")
    
    plot <- ggplot(data = kriged.output, aes(x = X, y = Y)) #start with the base-plot 
    layer1 <- c(geom_tile(data = kriged.output, aes(fill = Value)))
    layer2 <- c(geom_point(data = stao, aes(X, Y), colour = "black", size = 3))
    layer3 <- c(geom_text(data = stao, aes(label=logger_tag),hjust=-0.2, vjust=-0.2))
    layer4 <- c(geom_text(data = timestamp_temp, aes(label=sprintf("%.2f", Value)),hjust=-0.2, vjust=+1.4, size = 5))
    
    dateTimeID_str <- substr(dateTimeID, start=2, stop=nchar(dateTimeID))
    dateTimeID_lt <- strptime(dateTimeID_str, '%d%m%y%H%M%S')
    dateTime_str <- strftime(dateTimeID_lt, format='%d.%m.%Y %H:%M:%S')
    
    line <- dataset[dataset$dateTimeID==timestamp,]
    aussen1 <- line$aussen14_1_1
    aussen2 <- line$aussen14_1_2
    soll <- line$tempNum
    remove(line, timestamp_temp)
    
    titleString <- paste(variable, ", krige,", "vgm(1, Sph, 150, 1), ", thisIdp, ",", dateTime_str, ",\n aussen14_1_1: ", aussen1, ",\n aussen14_1_2: ", aussen2, ",\n soll:", soll, sep=" ")
    title <- ggtitle(titleString)
    # plot + layer1 + layer2 + layer3 + scale_fill_gradient(limits=c(dataset_min, dataset_max), low="blue", high="red") + coord_fixed(ratio = 1) + title
 ## (das hatte Patrick gew?hlt)    plot + layer1 + layer2 + layer3 + layer4 + scale_fill_gradient2(low = 'royalblue4', mid = 'snow2', high = 'red4', midpoint = ((col_range_min+col_range_max)/2), limits=c(col_range_min, col_range_max)) + coord_fixed(ratio = 1) + title
    plot + layer1 + layer2 + layer4 + scale_fill_gradient2(low = 'royalblue4', mid = 'snow2', high = 'red4', midpoint = ((col_range_min+col_range_max)/2), limits=c(col_range_min, col_range_max)) + coord_fixed(ratio = 1) + title
    # plot + layer1 + layer2 + layer3 + scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = ((min_value+max_value)/2), limits=c(min_value, max_value)) + coord_fixed(ratio = 1) + title
    
    ggsaveFileName <- paste('S:/pools/n/N-IUNR-Allgemein/Zentren/Hortikultur/FS_Spezialkulturen/Projekte/Stadtgaertnerei_Zuerich/Temperaturmonitoring/Klimadaten/Auswertungen_Schlussbericht_2017/tropJan17/outData/',variable,"_krige_",thisIdp,"_",dateTimeID, ".png",sep="")
    ggsave(ggsaveFileName, dpi=300, width=20, height=15, unit="cm", scale=1.5)
    
  }  

}

















