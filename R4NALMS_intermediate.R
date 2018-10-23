ohio <- read.csv('ohio.csv', stringsAsFactors = FALSE)

# Like this:
str(ohio)

names(ohio)

# First, we replace Secchi..m. with Secchi
names(ohio)[7] <- "Secchi"

# Next, we replace County..Borough.Parrish.
# with just County because we are working in a 
# single state in this case, not becuase we are
# County-centric :)
names(ohio)[11] <- "County"

# Replace names of both at once
# because that is cooler than one
# at a time.
names(ohio)[c(7,11)] <- c('Secchi', 'County')

#Check that it worked!
names(ohio)

# Make the histogram
hist(ohio$Secchi, 
     col='gray87',
     yaxt='n', xaxt='n',
     xlab='Secchi depth (m)',
     main='')
axis(side=1, pos=0)
axis(side=2, las=2, pos=0)

# Make the histogram, this time
# subtracting the mean from each 
# value. Is the result normal?
hist(ohio$Secchi-mean(ohio$Secchi),
     col='gray87', 
     yaxt='n', xaxt='n',
     xlab='Error',
     main='')
axis(side=1, pos=0)
axis(side=2, las=2, pos=-1)

#Transformations
ohio$logSecchi <- log(ohio$Secchi)

# Make the histogram,
# subtracting the mean from each 
# value. Is the result normal?
hist(ohio$logSecchi-mean(ohio$logSecchi),
     col='gray87', 
     yaxt='n', xaxt='n',
     xlab='Error',
     main='')
axis(side=1, pos=0)
axis(side=2, las=2, pos=-3.5)

#####Is this code below different than the code above? Or is it supposed to be?
# Make the histogram.
hist(ohio$logSecchi-mean(ohio$logSecchi),
     col='gray87', 
     yaxt='n', xaxt='n',
     xlab='Error',
     main='')
axis(side=1, pos=0)
axis(side=2, las=2, pos=-3.5)


#Introductory statistics in R
ohio <- ohio[ohio$GNIS.Class=="Lake" | ohio$GNIS.Class=="Reservoir", ]

#Wilcoxon rank-sums test

# Wilcox test to assess the null hypothesis
# that there is no difference in Secchi between
# lakes and reservoirs.
wilcox.test(x=ohio$Secchi[ohio$GNIS.Class=='Lake'],
            y=ohio$Secchi[ohio$GNIS.Class=='Reservoir'])

#Parametric statistics in R

# T-test to assess the null hypothesis
# that there is no difference in Secchi
# between lakes and reservoirs in Ohio.

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
t.test(logSecchi~GNIS.Class, data=ohio, equal=FALSE)

# Make a boxplot of Secchi by waterbody type

# Specify notch=TRUE to get a visual
# approximation of significance by comparing
# the spread of the notches. In this case, we
# have a ton of data, so the notches are barely
# visible...something to think about when you
# are doing hypothesis testing with tons
# of data.

# We make the boxes narrower because
# flat, wide boxes look gross and 
# make people not want to use R for
# graphing even though it is awesome.
boxplot(logSecchi~GNIS.Class,
        data = ohio,
        notch=TRUE,
        col='gray87',
        ylab=expression(paste('log'[e],'Secchi depth')),
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

#Analysis of variance

# Fit an ANOVA to test for differences in
# means between groups
mod <- lm(logSecchi~GNIS.Class, data=ohio)

# There are four plots that come out
# of this call. To see them all at 
# once, you need to set up the plotting
# window to accomodate that. Otherwise, 
# you have to hit ENTER to scroll
# through them one at a time.

par(mfrow=c(2, 2))

# The default plot function knows what
# to do with lm objects.

plot(mod)

# Get ANOVA summary for the model
anova(mod)

TukeyHSD(aov(mod))

# Change 
ohio$Year <- as.numeric(ohio$Year, 'Year ')

lmod <- lm(logSecchi~Latitude, data=ohio)

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
  Latitude = seq(
    from=min(ohio$Latitude, na.rm=T),
    to=max(ohio$Latitude, na.rm=T),
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

# Apply the exponentiation 
# to each column in the 
# dataframe.
preds <- apply(X=preds, MARGIN=2, FUN=exp)

# Have a look at the first
# few rows of the df
head(preds)

plot(x = ohio$Latitude,
     y = ohio$Secchi,
     xlab = 'Latitude (decimal min)',
     ylab = 'Secchi depth (m)',
     pch=21,
     col=rgb(0.5,0.5,0.5,0.10),
     bg=rgb(0.5,0.5,0.5,0.10),
     ylim=c(0,10)
)

# Now, add the lines for the mean, lower, and
# upper CIs from the model that we used
lines(newd$Latitude, preds[,1], lwd=2, lty=1, col='blue')
lines(newd$Latitude, preds[,2], lwd=2, lty=2, col='red')
lines(newd$Latitude, preds[,3], lwd=2, lty=2, col='red')

# Fit a linear regression model to test
# effect of `year` on `logSecchi`
ymod <- lm(logSecchi~Year, data=ohio)
summary(ymod)

#Analysis of covariance
library(car)

# Fit the model and store it to an object
mainmod <- lm(formula=logSecchi~GNIS.Class + Year,
              data=ohio)

# Take a look at the summary of the model
library(car, lib.loc = 'r_libs/car')
Anova(mainmod)

summary(intmod)

#Response surfaces (isopleths)
otsego <- read.csv('physical.csv')

#Data formatting & extraction

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

library(akima, lib.loc = '/r_lib/akima')

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

#ta-da!