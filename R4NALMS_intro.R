# Add 1 and 1 together
1 + 1

# Section heading ----
# Follow a comment with four dashes
# to insert a section heading

# Example (run the following lines):
a = 1
A = 2
a == A

# In Rstudio there are nifty little 
# markers to show this is broken
1a = 1

# But this one works 
# Try it by typing 
# a1,
# print(a) or
# show(a) 
# in the console: 
a1 = 1 
print(a)
show(a)
show(A)

a = 1
a = 2
a

T == TRUE

a = 1
a

# Make a vector of integers 1-5
a = c(1, 2, 3, 4, 5) 
# One way to look at our vector
print(a) 
# Another way to look at it
show(a)  
a
# Look at the structure, integer class
str(a)   

# Define the same vector using a sequence
a = seq(from=1, to=5, by=1) 
str(a) 


# Make a character vector
b = c('a', 'b', 'c', 'd', 'e')
# Print it to the console
b       
# But we can change if we want
b = as.factor(b)               
b      

str(b)      


as.numeric(b) 

# What did that do?
?as.numeric   

# The '==' compares the numeric vector to the factor one
c <-  a == b 
c            
str(c)

is.na(a)
is.finite(a)
!is.na(a)
a == 3
unique(b)


a[3]
f <- a[3]
b[b=='c'] 
which(b=='c')
a*.5
a+100
a-3
a/2
a^2
exp(a)
log(a)

# Log base 10
log10(a) 
b = as.character(b)

# We can append text
paste(b, 'AAAA', sep='') 
# We can do it the other way
paste('AAAA', b, sep='') 
# We can replace text
gsub(pattern='c', replacement='AAAA', b)
# Make a new object
e = paste('AAAA', b, sep='') 
# Print to console
e
# We can strip text (or dates, or times, etc.)
substr(e, start=5, stop=5) 
# A has a length of 5, try it and check it
length(a)
# Yup, looks about right
a         
cbind(a,e)
matrix(0, nrow=3, ncol=4)
mat = matrix(seq(1, 12), ncol=3, nrow=4)
# Number of columns
ncol(mat)   
# Number of rows
nrow(mat)   
# Total number of entries
length(mat) 
# Value of row 2, column 3
mat[2, 3]   

str(mat)  
colnames(mat) = c('first', 'second', 'third')
rownames(mat) = c('This', 'is', 'a', 'matrix')
mat
# Take a look to understand
str(mat) 
mat*2

# Make a new object 'a' from a sequence  
a = seq(from=.5, to=10, by = .5)  

# Vector math: raise each 'a' to power of 2
b = a^2                           

# Replicates values in object a # of times  
c = rep(c('a','b','c','d'), 5)    

# Note, we don't use quotes for objects,
# but we do for character variables
d = data.frame(a, b, c)           
d

# We can look at these
# All of the names
names(d)     
# We can change the names
# All at once- note quotes
names(d) = c('Increment', 'Squared', 'Class') 
# Print it to see what this does
names(d)               

# Or, change one at a time..
names(d)[3] = 'Letter' 
# Print it again to see what changed
names(d)               

# We can also rename the entire dataframe
e = d                  
# Head shows first six rows by default
head(e)                
# Or, we can look at any other number that we want
head(e, 10)            
e$Sqrt = sqrt(e$Increment)
# We can make new columns in data frames like this!
e 


setwd("C:/Users/.../physical.csv")
# We can use ls() to see what is in our environment 
otsego <- read.csv('physical.csv')

ls()         
head(otsego)
# How many rows does it have?
nrow(otsego)    
# How many columns?
ncol(otsego)    
# Have a look at the data structure
str(otsego)     
# Summarize the variables in the dataframe
summary(otsego) 
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
test

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
load(file='otsego.rda')



myList = vector(mode='list', length=4)
myList[[1]] = a
myList[[2]] = c
myList[[3]] = mat
myList[[4]] = d

# Print it
# Cool, huh?
myList                                   

# No names on the fly like a df
names(myList)                            
# Give it names like a dataframe
names(myList) = c('a', 'c', 'mat', 'd')  
# See how the names work now?
myList                                   

# We reference these differently [[]]
myList[[1]]                              
# But can still get into each object
myList[[2]][5]                           
# Play around with the numbers to see what they do!
# Can also reference it this way!
myList$c[1]                              

mean(otsego$temp, na.rm=T) 
median(otsego$temp, na.rm=T) 
min(otsego$temp, na.rm=T)
max(otsego$temp, na.rm=T)
# Standard deviation
sd(otsego$temp, na.rm=T) 
# Variance  
var(otsego$temp, na.rm=T) 
quantile(otsego$temp, probs=c(0.025, 0.975), na.rm=T)
# Quantiles, with user-defined thresholds
summary(otsego)

# Summarize temperature by month in otsego data
aggregate(formula=temp~month, data=otsego, FUN=mean)

# Summarize temperature by month in otsego data
aggregate(formula=temp~month+depth, data=otsego, FUN=mean)

library(plyr, "r_libs")
        
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

# Set main graphical parameters
par(mfrow=c(1, 2))

# Make the default scatterplot
plot(july$year, july$temp)

# Now, make a better plot and add it
plot(july$year, july$temp,
     xlab='Year', 
     ylab=expression(paste('Temperature (', degree, 'C)')),
     pch = 21,               # Use a filled circle
     bg=rgb(.5,.5,.5,0.10),  # RGB colors for point background
     col=rgb(.5,.5,.5,0.10), # RGB colors for point outline
     yaxt='n'                # Don't plot y-axis...yet
)
# Add the y-axis
axis(side=2, las=2)

# Set up the graphing space
par(mfrow=c(1,2))

# Make the default histogram
hist(july$temp)

# Now, make a better one
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

