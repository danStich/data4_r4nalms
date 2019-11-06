
vermont <- read.csv('vermont.csv', stringsAsFactors = FALSE)


# Like this:
str(vermont)



names(vermont)



# First, we replace Secchi..m. with Secchi
names(vermont)[7] <- "Secchi"

# Next, we replace County..Borough.Parrish.
# with just County because we are working in a 
# single state in this case, not because we are
# County-centric :)
names(vermont)[11] <- "County"



# Replace names of both at once
# because that is cooler than one
# at a time.
names(vermont)[c(7,11)] <- c('Secchi', 'County')



# Print the names of the df
# to the console so we can 
# see them.
names(vermont)


library(rgdal)
VT <- rgdal::readOGR("VT_Data__State_Boundary.shp")


# Remove those points with missing longitude and latitude
d <- vermont[!is.na(vermont$Longitude) | !is.na(vermont$Latitude),]



# Load sp package
library(sp)

# Assign longitude and latitude to a 
# SpatialPoints-class obj
coord_dd = sp::SpatialPoints(cbind(d$Longitude, d$Latitude),
                             proj4string=CRS("+proj=longlat"))




# Get UTMs for the longitudes and latitudes using
# the coordinate system of our shape file
coord_utm <- sp::spTransform(coord_dd, CRS(proj4string(VT)))



# Assign the coordinates to new columns
# in our dataframe
d$x <- coord_utm@coords[,1]
d$y <- coord_utm@coords[,2]
sp::coordinates(d) <- ~ x + y



sp::proj4string(d) <- sp::proj4string(VT)


# We'll use the ggplot2 library
library(ggplot2)

# Make the plot
ggplot() + 
geom_polygon(data = fortify(VT),
             color = "black",
             fill = "gray40",
             aes(x=long, y=lat)) +
#coord_sf() +
geom_point(data = data.frame(d),
           mapping = aes(x = x, y = y)) +
labs(x="Easting", y="Northing") +
ggtitle("Vermont lakes") + 
theme(plot.title = element_text(hjust = .5),
      text = element_text(size = 10)
      ) +    
  
# Adjusting output width: distorts CRS  
# but can actually see the plot
coord_equal(ratio=.5)




# Perform a spatial intersect between
# the Vermont shapefile (polygon) and
# the SpatialPoints object.
# Note that order is important here.
ins <- sp::over(d, VT)

# Then, we can drop the points that 
# do not not intersect with the polygon,
# now saving over the original data set.
dd <- d[!is.na(ins[,1]),]

# Get the modified data back out of the
# SpatialPoints object
vermont <- dd@data


# We'll use the ggplot2 library
library(ggplot2)

# Make the plot
ggplot() + 
geom_polygon(data = fortify(VT),
             color = "black",
             fill = "gray60",
             aes(x=long, y=lat)) +
coord_sf() +  
geom_point(data = data.frame(dd),
           mapping = aes(x = x, y = y)) +
labs(x="Easting", y="Northing") +
ggtitle("Vermont lakes") + 
theme(plot.title = element_text(hjust = .5),
      text = element_text(size = 10)
      )


# Make the plot...again
ggplot() + 
geom_polygon(data = fortify(VT),
             color = "black",
             fill = "gray90",
             aes(x=long, y=lat)) +
coord_sf() +
geom_point(data = data.frame(dd),
           mapping = aes(x = x, y = y, color = dd$Secchi),
           alpha=.5, size=3) +
labs(x="", y="") +
ggtitle("Secchi depth (m)") + 
theme(plot.title = element_text(hjust = .5, face = "bold"),
      text = element_text(size = 14),
      legend.position = 'right',
      legend.title.align = 0,
      panel.background = element_blank(),
      axis.ticks.y = element_blank(),axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),axis.text.x = element_blank()
      ) +
scale_colour_gradientn("", colours=c("gray90","black"))



# Make the histogram
hist(vermont$Secchi, 
     col='gray87',
     yaxt='n', xaxt='n',
     xlab='Secchi depth (m)',
     main='')
axis(side=1, pos=0)
axis(side=2, las=2, pos=0)



# Make the histogram, this time
# subtracting the mean from each 
# value. Is the result normal?
hist(vermont$Secchi-mean(vermont$Secchi),
     col='gray87', 
     yaxt='n', xaxt='n',
     xlab='Error',
     main='')
axis(side=1, pos=0)
axis(side=2, las=2, pos=-5)



vermont$logSecchi <- log(vermont$Secchi)



# Make the histogram,
# subtracting the mean from each 
# value. Is the result normal?
hist(vermont$logSecchi-mean(vermont$logSecchi),
     col='gray87', 
     yaxt='n', xaxt='n',
     xlab='Error',
     main='')
axis(side=1, pos=0)
axis(side=2, las=2, pos=-2.3)



# Make the histogram.
hist(vermont$logSecchi,
     col='gray87', 
     yaxt='n', xaxt='n',
     xlab='Error',
     main='')
axis(side=1, pos=0)
axis(side=2, las=2, pos=-.75)



vermont <- vermont[vermont$GNIS.Class=="Lake" | vermont$GNIS.Class=="Reservoir", ]



# Wilcox test to assess the null hypothesis
# that there is no difference in Secchi between
# lakes and reservoirs.
wilcox.test(x=vermont$Secchi[vermont$GNIS.Class=='Lake'],
            y=vermont$Secchi[vermont$GNIS.Class=='Reservoir'])



# T-test to assess the null hypothesis
# that there is no difference in Secchi
# between lakes and reservoirs in Vermont.

# We use logSecchi to meet assumptions
# of normality.

# We can specify this one using a formula.
# To be conservative here, we will assume 
# that we have unequal variances using
# one of the optional arguments. Note that
# the default in R is to assume that variances
# are not equal, and this defaults to 
# a Welch's t-test that uses a calculated df
# to adjust the calculated test statistic.
t.test(logSecchi~GNIS.Class, data=vermont, equal=FALSE)


# T-test to assess the null hypothesis
# that there is no difference in Secchi
# between lakes and reservoirs in Vermont.

# We can specify this one using a formula.
mod <- t.test(logSecchi~GNIS.Class, data=vermont, equal=FALSE)



# Make a boxplot of Secchi by waterbody type

# We make the boxes narrower because
# flat, wide boxes look gross and 
# make people not want to use R for
# graphing even though it is awesome.
boxplot(Secchi~GNIS.Class,
        data = vermont,
        notch=TRUE,
        col='gray87',
        ylim=c(0, 12),
        xlab='Waterbody type',
        ylab='Secchi depth',
        boxwex = .25,
        outline=FALSE,
        yaxt='n',
        # Other parameters to make
        # it prettier
        pars = list(
          staplewex=0,
          whisklty=1,
          whisklwd=2,
          whiskcol='gray40',
          boxlwd=2,
          boxcol='gray40'
        )
        )
axis(side=2, las=2)



# Fit an ANOVA to test for differences in
# means between groups
mod <- lm(logSecchi~GNIS.Class, data=vermont)



# There are four plots that come out
# of this call. To see them all at 
# once, you need to set up the plotting
# window to accomodate that. Otherwise, 
# you have to hit ENTER (RETURN) to scroll
# through them one at a time.

par(mfrow=c(2, 2))

# The default plot function knows what
# to do with lm objects.

plot(mod)



# Get ANOVA summary for the model
anova(mod)



TukeyHSD( aov(mod))


# Change 
vermont$Year <- as.numeric(vermont$Year, 'Year ')



names(vermont)[18] <- 'Elevation'
lmod <- lm(logSecchi~Elevation, data=vermont)



# Look at residuals for model
# testing effect of latitude
# on logSecchi
par(mfrow=c(2,2))
plot(lmod)



summary(lmod)



# Make new values of Latitude
# that we can use to predict 
# logSecchi. If we want to use
# the default predict function,
# this has to be a variable
# named "Latitude" in a new 
# dataframe that we can give
# to R.

# To make these new values, we 
# use a sequence that goes from
# the minimum to the maximum of
# our observed values and avoid
# making predictions outside the
# observed range of Latitudes.

# We use a sequence to ensure 
# that we only get one of each
# value so we can make some nice
# line graphs of our predictions.

newd <- data.frame(
  Elevation = seq(
    from=min(vermont$Elevation, na.rm=T),
    to=max(vermont$Elevation, na.rm=T),
    by=.01
    )
  )



# Make predictions from the model using
# the new data.

# We specify interval='prediction' to
# get prediction intervals, but we could
# also ask for 'confidence' intervals.
preds <- predict(object = lmod,
                 newdata = newd,
                 interval = 'prediction'
                 )



# Take a look at the first few
# rows of the preds dataframe
head(preds)



# Get a dataframe with mean by elevation to clean
# things up a little
library(plyr)
means=ddply(vermont, 'Elevation', summarize, mu=mean(logSecchi))

# Make the base plot, axes, etc.
plot(x = means$Elevation,
     y = means$mu,
     xlab = 'Elevation (m)',
     ylab = expression(paste('log '[e],'Secchi depth (m)')),
     col="gray",
     ylim=c(0,max(vermont$logSecchi)),
     yaxt='n'
     )
# Add a rotated y-axis
axis(side = 2, las=2, at = log(c(1,5,10,15)), c(1,5,10,15))

# Plot a polygon
polygon(x=c(newd$Elevation, rev(newd$Elevation)),
        y=c(preds[,2], rev(preds[,3])),
        col='gray87', border=NA)

# Add the raw data over the top of the polygon
points(means$Elevation, means$mu, pch=21, bg='gray40', col='gray40')

# Now, add the lines for the mean, lower, and
# upper CIs from the model that we used
lines(newd$Elevation, preds[,1], lwd=1, lty=1, col='black')
lines(newd$Elevation, preds[,2], lwd=1, lty=2, col='black')
lines(newd$Elevation, preds[,3], lwd=1, lty=2, col='gray40')



otsego <- read.csv('physical.csv')



# Data formatting & extraction

# First, we convert the date column
# to a character string. We pass the
# result directly to the as.Date 
# function, and along with that we
# specify a format so R knows where it
# is looking for specific elements of
# the date info we are trying to pass.
otsego$date <- as.Date(
  as.character(otsego$date),
  format="%m/%d/%Y"
  )



# Remove NA values to make life easier
lim <- na.omit(otsego)



# Multiply depth column by -1 so depth will
# plot from top to bottom.
lim$depth = -1 *lim$depth


library(akima)



# Create a data frame containing the
# x, y, and z variables of interest
plotter = data.frame(x=lim$date, y=lim$depth, z=lim$temp)

# Sort it so we have ascending values of x and y
plotter = plotter[with(plotter, order(x, y)), ]

# Make a regularly spaced x, y, z grid using
# linear interpolation from the akima package
im = with(plotter,
          interp(x, y, z, duplicate='mean',
                 nx=length(unique(lim$date)),
                 ny=length(unique(lim$depth)))
          )



# Plot the isopleth
# filled.contour is the function that actually 
# makes the contour plot. This is the same function
# that is used in the wtr.heat.map function in the
# RLakeAnalyzer package, but it is executed from
# within a convenience wrapper there, so it is 
# hard to customize. 

# I tend to work with the filled.contour
# function from the graphics package (included
# in base R and loaded by default). This is
# just a preference driven by need for
# more flexibility.

# Set up plotting window margins
par(mar=c(4, 4, 2, 8)) 

# Make the graph
filled.contour(
  im$x, # Variable on x-axis (date)
  im$y, # Variable on y-axis (depth)
  im$z, # Response (wq parameter) 
  # Could also choose 'grey.colors' or 'terrain.colors'.
  # If you want the ramp to go the other way,
  # just delete the 'rev'. Note that you will
  # need to change the 26 in parentheses to match
  # the number of levels that you actually have or
  # want to display.
  col=topo.colors(26),  
  # I don't like in-figure titles.
  # You can add one, though. You will, however,
  # need to change the 'mar' argument in the call
  # to par above.
  main = expression(paste('Temperature (', degree, 'C)')),
  # Specify y-axis limits.
  ylim=c(min(im$y), max(im$y)),
  # Specify x-axis limits. In
  # this case, we are "zooming in"
  # on year 2017
  xlim=c(as.Date('2017/05/01'), max(im$x)), 
  # X-axis label
  xlab='Date', 
  # Y-axis label
  ylab='Depth (m)',
  # Axis options
  plot.axes = {  
        # This is how we include
        # countour lines
        contour(                            
        im$x,                             
        im$y,                             
        im$z,                             
        nlevels = 26,                     
        drawlabels = FALSE,               
        col = topo.colors(26),
        lwd = 1,                          
        lty = 2,                          
        add = TRUE
        )
    # Y-axis
    axis(2, at=seq(0,-50,-10),
         labels=seq(0,50,10)
    )
    # X-axis
    axis(1,
         at=seq(as.Date("2017/05/01"),
                by="2 months",
                length.out=16
                ), 
         labels = format(
           seq(as.Date("2017/05/01"),
               by="2 months",
               length.out=16
               ),
           "%b %Y"
           )
    )
  }                                     
)             

