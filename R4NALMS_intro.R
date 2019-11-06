# Introductory R

# This is a comment. 
# We know because it is preceded 
# by a hashtag, or 'octothorpe'.

# R ignores comments so you have
# a way to write down what you have
# done or what you are doing.

# This is useful for sharing
# code or just figuring out
# what you did.

# Add 1 and 1 together
1 + 1

# Section heading ----
# Follow a comment with four dashes
# to insert a section heading

# Example (run the following lines):
a <- 1
A <- 2

# Are these two things equal?
a == A

 
# In Rstudio there are nifty little 
# markers to show this is broken once
# you have saved your source file.
1a <- 1 

# But this one works 
# Try it by typing 
# a1,
# print(a) or
# show(a) 
# in the console: 
a1 <- 1 

a <- 1
a <- 2
a

T == TRUE

a  <- 1

a <- c(1, 2, 3, 4, 5) # Make a vector of integers 1-5
print(a) # One way to look at our vector
show(a)  # Another way to look at it
a        # A third way to look at it
str(a)   # Look at the structure, integer class

# Define the same vector using a sequence
a <- seq(from=1, to=5, by=1) 
str(a)       

b <- c('a', 'b', 'c', 'd', 'e') # Make a character vector
b                               # Print it to the console
str(b)                          # Now it's a character vector
b <- as.factor(b)               # But we can change if we want
b                             
str(b)                          # Look at the data structure

as.numeric(b) 

# What did that do?
?as.numeric   

# The '==' compares the numeric vector to the factor one
c <- a == b 
c            
str(c)       

is.na(a)     # We can check for missing values
is.finite(a) # We can make sure that all values are finite
!is.na(a)    # The exclamation point means 'not'
a == 3       # We can see if specific elements meet a criterion
unique(b)    # We can just look at unique values

# This one just prints it
a[3] 

# This one stores it in a new object
f <- a[3]

b[b=='c'] 
which(b=='c')

a * .5   # Multiplication
a + 100  # Addition
a - 3    # Subtraction
a / 2    # Division
a ^ 2    # Exponentiation
exp(a)   # This is the same as 'e to the...'
log(a)   # Natural logarithm
log10(a) # Log base 10

b <-  as.character(b)
paste(b, 'AAAA', sep='')     # We can append text
paste('AAAA', b, sep='')     # We can do it the other way
paste('AAAA', b, sep='--')   # Add symbols to separate
gsub(pattern='c', replacement='AAAA', b) # We can replace text

e <- paste('AAAA', b, sep='') # Make a new object
e                            # Print to console
substr(e, start=5, stop=5)   # We can strip text (or dates, or times, etc.)

length(a) # A has a length of 5, try it and check it
a         # Yup, looks about right

cbind(a,e)

matrix(0, nrow=3, ncol=4)

mat <-  matrix(seq(1, 12), ncol=3, nrow=4)

ncol(mat)   # Number of columns
nrow(mat)   # Number of rows
length(mat) # Total number of entries
mat[2, 3]   # Value of row 2, column 3
str(mat)    

colnames(mat) <- c('first', 'second', 'third')
rownames(mat) <- c('This', 'is', 'a', 'matrix')
mat
str(mat) # Take a look to understand

# Make a new object 'a' from a sequence  
a <- seq(from=.5, to=10, by = .5)  
  
# Vector math: raise each 'a' to power of 2
b <- a^2                           

# Replicates values in object a # of times  
c <- rep(c('a','b','c','d'), 5)    

# Note, we don't use quotes for objects,
# but we do for character variables
d <- data.frame(a, b, c)           

d # Print the dataframe

# We can look at these
names(d)     # All of the names
names(d)[2]  # One at a time: note indexing, names(d) is a vector!!

# We can change the names
names(d) <- c('Increment', 'Squared', 'Class') # All at once- note quotes
names(d)                # Print it to see what this does
names(d)[3] <- 'Letter' # Or, change one at a time..
names(d)                # Print it again to see what changed
e <- d                  # We can also rename the entire dataframe
head(e)                 # Head shows first six rows by default
head(e, 10)             # Or, we can look at any other number that we want
e$Sqrt <-  sqrt(e$Increment) # We can make new columns in data frames like this!
e 


otsego <- read.csv('physical.csv')


ls()            # We can use ls() to see what is in our environment
head(otsego)    # Look at the first six rows of data in the object
nrow(otsego)    # How many rows does it have?
ncol(otsego)    # How many columns?
names(otsego)   # What are the column names?
str(otsego)     # Have a look at the data structure
summary(otsego) # Summarize the variables in the dataframe

otsego[12, 4]

otsego[12, 'depth']

mean(otsego$temp[otsego$depth==10.0] , na.rm=TRUE)

# Define a new function to calculate mean
myMean <- function(x){
  # If any of the observations are NA
  if(any(is.na(x))){
    # Then exclude those values of 
    # x from calculation
    x_bar = sum(x[!is.na(x)])/length(x[!is.na(x)])
      # Otherwise
      } else {
        # Calculate from all values of x
        x_bar = sum(x)/length(x)
      }
  
  return(x_bar)
  }

test <- myMean(otsego$temp[otsego$depth==10])

# Define a function to convert temperature
# in celcius to temperature in farenheit:
cToF <- function(cels){
  faren <- cels*(9/5) + 32
  return(faren)
}

# Test the function out.
# Here, we create a new variable in the
# otsego dataframe to hold the result
otsego$tempF <- cToF(otsego$temp)

# Look at the first few elements
head(otsego$tempF)

write.csv(x=otsego, file='physicalF.csv')

# Write the data set to a csv file.
# We specify the object, the filename, and
# we tell R we don't want rownames or quotes
# around character strings. Finally, we tell
# R to separate columns using a comma.

# We could do exactly the same thing, but use a ".txt"
# file extensions, and we would get a comma-separated
# text file if we needed one.
write.table(x=otsego, file='physicalF.csv', row.names=FALSE,
            quote=FALSE, sep=',')

save(otsego, file='otsego.rda')

myList <-  vector(mode='list', length=4)
myList[[1]] <- a
myList[[2]] <- c
myList[[3]] <- mat
myList[[4]] <- d

myList           # Print it - Cool, huh?
names(myList)    # No names on the fly like a df
names(myList) <-  c('a', 'c', 'mat', 'd')  # Give it names like a df
myList           # See how the names work now?
myList[[1]]      # We reference these differently [[]]
myList[[2]][5]   # But can still get into each object

mean(otsego$temp, na.rm=T) 
median(otsego$temp, na.rm=T)  
min(otsego$temp, na.rm=T)
max(otsego$temp, na.rm=T)
sd(otsego$temp, na.rm=T)  # Standard deviation
var(otsego$temp, na.rm=T) # Variance  
quantile(otsego$temp, probs=c(0.025, 0.975), na.rm=T) # Quantiles, with user-defined thresholds

summary(otsego)

# Summarize temperature by month in otsego data
aggregate(formula=temp~month, data=otsego, FUN=mean)

# Summarize temperature by month in otsego data
aggregate(formula=temp~month+depth, data=otsego, FUN=mean)

library(plyr)

# Summarize mean and sd by month and depth
summarydata <- ddply(
  .data = otsego,
  .variables = c('month', 'depth'),
  summarize,
  mean = mean(temp, na.rm = T),
  sd = sd(temp, na.rm = T)
)

head(x=summarydata, n=10)

write.table(summarydata, 'summary.csv', row.names = FALSE,
            quote=FALSE, sep=',')

# Make a simple boxplot of epilimnion temperatures by month
# Note that I pass this restriction using the depth variable
# in the Otsego data because I know (about) where the
# thermocline is in that lake.
boxplot(formula=temp~month, data=otsego[otsego$depth<8,])

# Make the plot
boxplot(
  formula=temp~month, data=otsego[otsego$depth<8,],
  col='gray87', # Box color: the best one
  xaxt='n',     # We'll make our own x-axis
  yaxt='n',     # Same for the y-axis
  xlab='Month', # Here is an x-axis label
  ylab=expression(paste('Temperature (', degree, 'C)'))
)

# Add x-axis. substr() takes the
# first 3 letters of month
axis(side=1,
     at=c(1:12),
     labels=substr(month.name, start=1, stop=3)
     )

# Add the y-axis, las rotates the labels
axis(side=2, las=2)

# Set up the plotting window
par(mar=c(5,5,1,1))
# Make the plot
boxplot(
  formula=temp~month, data=otsego[otsego$depth<8,],
  col='gray87', # Box color: the best one
  xaxt='n',     # We'll make our own x-axis
  yaxt='n',     # Same for the y-axis
  xlab='Month', # Here is an x-axis label
  ylab=expression(paste('Temperature (', degree, 'C)')),
  outline=FALSE,
  pars=list(
    staplewex=0,
    boxcol='gray60', # Another of the great grays
    boxlwd=2,
    medlwd=1, medcol='gray60',
    medpch=21, medbg='gray40', # Yet another...so many!
    whisklty=1, whisklwd=2, whiskcol='gray60'
  )
)

# Add x-axis
axis(side=1,
     at=c(1:12),
     labels=substr(month.name, start=1, stop=3)
     )

# Add the y-axis
axis(side=2, las=2)

# Subset the data so we are just working with 
# temps from the epilimnion in July
july <- otsego[otsego$month==7 & otsego$depth<8, ]

# Let's improve on the default
# scatter plot a little here.
plot(july$year, july$temp,
     xlab = 'Year', 
     ylab = expression(paste('Temperature (', degree, 'C)')),
     pch = 21,                 # Use a filled circle
     bg = rgb(.5,.5,.5,0.50),  # RGB colors for point background
     col = rgb(.5,.5,.5,0.50), # RGB colors for point outline
     cex = 1.5,                # Character expansion
     yaxt='n'                  # Don't plot y-axis...yet
     )
# Add the y-axis

# Again, adding some tweaks
hist(july$temp,
     xlim=c(17,27),
     col='gray87',
     xaxt='n',
     yaxt='n',
     main='',
     xlab=expression(paste('July temperature (', degree, 'C)'))
     )

# Add the x-axis
axis(side=1, pos=0)

# Add the x-axis
axis(side=2, pos=17, las=2)
