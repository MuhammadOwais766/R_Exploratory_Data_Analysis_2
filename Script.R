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

