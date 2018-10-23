# 3. Thermocline depth analysis ----
library(lubridate, lib.loc = 'r_libs/')
library(rLakeAnalyzer, lib.loc = 'r_libs/')
library(ggplot2, lib.loc = 'r_libs/')

# Read in the data
limnos <- read.csv('physical.csv')

# Data formatting & extraction
limnos$date <- as.POSIXct(limnos$date, format="%m/%d/%Y")
limnos$year <- year(limnos$date)
limnos$day <-  yday(limnos$date)
limnos$month <- month(limnos$date)

# Remove rows with NA to make life easier for now
limnos <- na.omit(limnos)

# Create a list of dataframes split by date
# so each date corresponds to a df that is an
# element of the list
limlist <- split(limnos, list(limnos$date))

# Create a function to calculate thermocline
# depth that can be applied to a list as long
# as depth is in the 5th column and temperature
# is in the 6th column of each list element (matrix
# or dataframe). This function creates a new
# variable (column) called 'thermo' in each df,
# and returns the original object modified in 
# place.
ltd <- function(x){
  x$thermo <- rLakeAnalyzer::thermo.depth(x[,6], x[,5])
  return(x)
}

# Create a test object that contains the 
test <- lapply(limlist, ltd)

# Stack all of the dfs in the list into
# a single, large df
out <- do.call(rbind, test)

# Make a plot of July thermocline depth 
# across years, and include predictions from
# a GAM for data viz.
# First, subset the data
out1 <- out[out$month==7,]

# Now, plot it with ggplot2 functions
plot <- ggplot(out1, aes(year, thermo)) +
  geom_point(pch=21,cex=5, col='gray40', bg='gray40') +
  geom_smooth(method='gam', level=.999,
              col='red', bg='tomato') + 
  xlab('Year') + xlim(2011, 2017) +
  ylab('July thermocline depth (m)') + ylim(5,10)
plot+theme(plot.margin=margin(5,5,10,10))