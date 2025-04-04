library(tidyverse)
library(nycflights13)
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
