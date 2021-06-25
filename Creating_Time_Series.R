#### Creating a time series over 1993-2017 for PRC Trajectories ####
# Written by Elizabeth and Steph

# Load in packages 
library(ncdf4)
library(raster)
library(reshape2)
library(dplyr)
library(maps)
library(maptools)
library(mapdata)
library(ggplot2)
library(gganimate)
library(gifski)
library(cmocean)

# Load in files 
file_names <- list.files("/Users/elizabethsaraf/Desktop/PRC/total_year_out/Forward_depth", full.names = TRUE)
# Create a dataframe
output <- as.data.frame(matrix(NA,nrow=24,ncol=2)) # 24 rows bc 24 years 
colnames(output) <- c("year", "32") # Based on Previous analysis we are looking at south of 32 degrees 
counter = 1

# Creating a loop through all years 

for (f in 1:length(file_names)){
  #open netcdf files
  nc_file <- nc_open(file_names[f])
  #print(nc_file) range between 88 to 90 days with 601 particles released
  
  #grab year from each file
  Year <- as.character(unlist(strsplit(file_names[f],"_"))[8])
  print(Year)
  
  #----Extract variables from Netcdf-----
  #According to 'print(file)' above, there are 5 variables:
  # no trajectory file 
  id <- ncvar_get(nc_file,'trajectory') #ERROR:no trajectory variable outlined in these files 
  lat <- ncvar_get(nc_file,'lat') 
  lon <- ncvar_get(nc_file,'lon')
  #z <- ncvar_get(nc_file,'z') #ERROR: no Z available even though this is at depth
  time <- ncvar_get(nc_file,'time') #time is in seconds since origin date
  
  #----Convert variables to a dataframe------
  
  #First make dataframe for each variable
  lat_df <- melt(lat) #the 'melt' function does all the hard work to convert to a dataframe with 36594 rows (214 * 171) 
  colnames(lat_df) <- c("day", "lat") #give our dataframe columnnames that make sense
  # Removed "trajectory" label because not present in current file structure
  lon_df <- melt(lon)
  colnames(lon_df) <- c("day", "lon")
  time_df <- melt(time)
  colnames(time_df) <- c("day", "date")
  
  #Second merge dataframes together (not you can only merge two dataframes at once)
  df <- left_join(lon_df,lat_df, by=c("day","trajectory"))
  head(df)
  
  #Now let's add a date to the 'df' dataframe.
  #But first convert time to something sensible. 
  time_origin <- nc_file$var$time$units #get origin from netcdf file. Note that I could just manually type the origin date in, but it's better to get it from the file metadata so we can loop through files 
  time_origin <- unlist(strsplit(time_origin," "))[3] #split text string by "space", then unlist (strsplit function automatically returns a list), then take the third chunk which contains our date of interest
  time_df$date <- as.Date(time_df$date/86400, origin=time_origin) #convert to sensible time, with 86400 seconds in a day and origin from the netcdf file
  #Now add time to 'df'
  df <- left_join(df,time_df, by=c("day","trajectory"))
  head(df) #looks good!
  ## Paused
  
}
#Basic R code for making Time Series Plots

output <- output[order(output$month),] #month is out of order
#If running a forwards simulation looking at particles north of X degrees
# If running a backwards simulation, looking at particles south of X degrees
plot(output$month,output$prop_32, col="black", type="b", xlim=c(1,12), ylim=c(0,1), ylab="Proportion",xlab="Month", main="Proportion of Particles north of X degrees")
#lines(output$month,output$prop_32, col="green", type="b")
#lines(output$month,output$prop_34, col="red", type="b")
#lines(output$month,output$prop_36, col="blue", type="b")
#legend("topright",legend=c("30 degrees","32 degrees","34 degrees","36 degrees"),col=c("black","green","red","blue"), lty=c(1,1,1))

