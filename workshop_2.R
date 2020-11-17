

# Read in some data

# R has to know where the file is
# You have to assign it to an object
otsego <- read.csv("data/physical.csv")

View(otsego)


# Quick summaries
# To access things inside dataframes, we use $
names(otsego)

otsego$do_mgl

summary(otsego$do_mgl)

# Oh, no!!
mean(otsego$do_mgl)

# Ignore the nas when we summarize
# The mean across years
mean(otsego$do_mgl, na.rm = TRUE)

do_sd <- sd(otsego$do_mgl, na.rm = TRUE)
do_sd

log_do = log(otsego$do_mgl)
log_do10 = log10(otsego$do_mgl)

otsego$log_do = log(otsego$do_mgl)

# Awesome summaries
# What if we wanted mean do by year and depth?
# Tidy time!
library(tidyverse)
library(dplyr)
library(ggplot2)
library(magrittr)

# We're going to use these to summarize data
# We want DO by month and depth

# Step-by-step
# Group the data
library(dplyr)
# This overwrites the orignal data
# BAAAD Dan - you can name it something new
otsego <- group_by(otsego, month, depth)
summarized_do = summarise(otsego,
                          avgs = mean(do_mgl)
                          )

#oh, there are still NA values
summarized_do = summarise(otsego,
                          avgs = mean(do_mgl, na.rm = TRUE)
                          )
View(summarized_do)

# IF ABLE TO INSTALL AND LOAD TIDYVERSE
# You can "chain" operations with "%>%"
tester <- otsego %>%
  group_by(year, depth) %>%
  summarize(avgs = mean(do_mgl, na.rm = TRUE))


# Awesome graphs

hist(otsego$do_mgl)


# Ggplot is better
# Here is a histogram
ggplot(otsego, aes(x = do_mgl)) +
  geom_histogram(bins = 20)

# Here is a boxplot
ggplot(otsego, aes(x = factor(depth), y = do_mgl)) +
  geom_boxplot()

# Here is a scatter plot
ggplot(otsego, aes(x = depth, y = do_mgl)) +
  geom_point()

# Here is a scatter plot
ggplot(otsego, aes(x = depth, y = do_mgl, fill = year, color = year)) +
  geom_jitter(width = 0.1)

ggplot(otsego, aes(x = depth, y = do_mgl, fill = year, color = year)) +
  geom_jitter(width = 0.1) +
  facet_wrap(~ year)



