library(tidyverse)

glimpse(diamonds)

ggplot(diamonds) + geom_bar(mapping = aes(x = cut))

# Categorical / Discrete Variable 

diamonds %>% count(cut)

diamonds %>% count(cut) %>% arrange(desc(n))

# Continuous Variable

ggplot(diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# Narrow bins (detailed)

ggplot(diamonds) + geom_histogram(aes(x = carat), binwidth = 0.1)

# Wide bins (broader trends)

ggplot(diamonds) + geom_histogram(aes(x = carat), binwidth = 1.0)


diamonds %>% count(cut_width(carat, 0.5))

# Creating a subset 'small' with diamonds under 3 carats to remove outliers.

small <- diamonds %>% filter(carat < 3)

ggplot(data = small, mapping = aes(x = carat)) + geom_histogram(binwidth = 0.1) 

# 'geom_freqpoly' is similar to a histogram but with lines. 'freqpoly' stands for
# 'frequency polygons'. The code below helps compare how diamond sizes vary across cuts.

ggplot(data = small, mapping = aes(x = carat, colour = cut)) + 
  geom_freqpoly(binwidth = 0.1)

# Using tiny bins (0.01 carats) to reveal subtle patterns, i.e., rounding effects in data.

ggplot(data = small, mapping = aes(x = carat)) + geom_histogram(binwidth = 0.01)

# Now, using the 'faithful' dataset

glimpse(faithful)

ggplot(data = faithful, mapping = aes(x = eruptions)) + geom_histogram(binwidth = 0.25)

ggplot(data = diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(data = diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

outliers <- diamonds %>% filter(y < 3 | y > 20) %>% select(price, x, y, z) %>% arrange(y)

outliers

# Missing Values

# DO NOT DO THIS (shown below)

diamonds2 <- diamonds %>% filter(between(y, 3, 20))

# Instead, mutate and use if-else statements. Essentially, create missing values.

diamonds2 <- diamonds %>% mutate(y = ifelse(y <3 | y > 20, NA, y))

diamonds2

?ifelse() # Typically used for single conditional statements.
?case_when() # Typically used for multiple conditional statements.

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + geom_point()

library(nycflights13)

glimpse(nycflights13::flights)

# Missing values in the departure time variable means that the flight was cancelled.
# But, we don't have such a variable, so we make one called 'cancelled'.

nycflights13::flights %>% mutate(cancelled = is.na(dep_time), 
                                 sched_hour = sched_dep_time %/% 100,
                                 sched_min = sched_dep_time %/% 100,
                                 sched_dep_time = sched_hour+sched_min /60) %>%
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(color = cancelled), binwidth = 1/4)


# COVARIATION
# If variation describes within a variable, covariation describes behaviour between
# Variables.

# Looking at Categorical vs Continuous variable

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(diamonds) + geom_bar(mapping = aes(x = cut))

# Giving the graph a density of one

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot()

# Using 'reorder' to make more sense of the data

ggplot(data = mpg) + geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median),
                                                y= hwy)) + coord_flip()


# Categorical vs Categorical Variables

ggplot(data = diamonds) + geom_count(mapping = aes(x = cut, y = color))

diamonds %>% count(color, cut)

# Using a simple heatmap to view the variation

diamonds %>% count(color, cut) %>% ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# Continuous vs Continuous Variables

# Lots of values in the visualization below

ggplot(data = diamonds) + geom_point(mapping = aes(x = carat, y = price))

# So use 'alpha = 1/100' instead for large datasets

ggplot(data = diamonds) + geom_point(mapping = aes(x = carat, y = price), alpha = 1/100)

library(hexbin)

ggplot(data = diamonds2) + geom_hex(mapping = aes(x = carat, y= price))

ggplot(data = diamonds2) + geom_bin2d(mapping = aes(x = carat, y= price))

# We do the following visualization because while technically the data is 
# continuous, by binning we're turning the data into categorical.

#ggplot(data = diamonds, mapping = aes(x= carat, y = price)) + 
#  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = small, mapping = aes(x= carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

# Using 'cut_number(carat, 20)' to grab 20 at a time

ggplot(data = small, mapping = aes(x= carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

# Patterns and Models

# Patterns in our data provide clues about the relationship between variables.
# If a systematic relationship exists between two variables, it will appear as a
# pattern in the data. Now ask the question whether this pattern is coincidental or
# random chance. How can we describe the relationship implied by the pattern?
# Is it positive or negative relationship? Linear or non-linear? Let's look at
# the faithful data to find out.

ggplot(data = faithful) + geom_point(mapping = aes(x = eruptions, y = waiting))

# This package is used to do some linear regression

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% add_residuals(mod) %>% mutate(resid = exp(resid))
?exp()

ggplot(data = diamonds2) + geom_point(mapping = aes(x = carat, y= resid))

ggplot(data = diamonds2) + geom_point(mapping = aes(x = carat, y= resid), alpha = 1/50)

# The following plot reinforce what's been done above. It takes the previous
# variation, and using the boxplot, makes the relationship between residuals and cut
# even.

ggplot(data = diamonds2) + geom_boxplot(mapping = aes(x = cut, y = resid))
