# Housekeeping
rm(list = ls())

setwd("C:/Decision Analysis") 

.libPaths("c:/Program Files/R")

# library shit
install.packages("datasauRus")
install.packages("gganimate")
install.packages("ggplot2")
install.packages("gifski")
install.packages("purrr")
install.packages("rmarkdown")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("magrittr")

library(datasauRus)
library(gganimate)
library(ggplot2)
library(gifski)
library(purrr)
library(rmarkdown)
library(tidyverse)
# part of the tidyverse:
library(dplyr)
library(magrittr)

### Data Wrangling -------------------------------------------------------------
data <-  read.csv("participants_data.csv")

names(data0)
data$continent_of_origin

# Change the selection to batch and age
select(data, batch, age)

# Change the selection without batch and age
select(data, -academic_parents, -working_hours_per_day)

# Change the selection to those who work more than 5 hours a day
filter(data, working_hours_per_day>5 & 
         letters_in_first_name >3)

# Rename the variable km_home_to_office as commute (new name = old name)
rename(data, commute = km_home_to_office)

# Mutate a new column named age_mean that is a function of the age 
# multiplied by the mean of all ages in the group
mutate(data, age_mean = age*
         mean(age))
names(data)

# Mutate new column named response_speed populated by 'slow' if it took you 
# more than a day to answer my email and 'fast' for others
mutate(data, response_speed = ifelse(days_to_email_response > 1, "slow", "fast"))

# Create a summary of the participants_mutate data with the mean number of siblings 
# and median years of study
summarize(data,
          mean(number_of_siblings),
          median(years_of_study))

# Use the magrittr pipe to summarize the mean days to email response, 
# median letters in first name, and maximum years of study by gender
data %>% 
  group_by(gender) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))

# Use the magrittr pipe to create a new column called commute, where those who travel 
# more than 10km to get to the office are called "commuter" and others are "local". 
# Summarize the mean days to email response, median letters in first name, and maximum years of study. 
data %>% 
  mutate(commute = ifelse(km_home_to_office > 10, "commuter", "local")) %>% 
  group_by(commute) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))

# Split the data frame by batch, fit a linear model formula (days to email response as 
# dependent and working hours as independent) to each batch, compute the summary, then extract the R^2.
data %>%
  split(.$batch) %>% 
  map(~ lm(days_to_email_response ~ 
             working_hours_per_day, 
           data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# Please work through the wrangling tasks we just went though. Use the diamonds data 
# and make the steps in long format (i.e. assigning each step to an object) and short 
# format with (i.e. with the magrittr pipeline):
# select: carat and price
# filter: only where carat is > 0.5
# rename: rename price as cost
# mutate: create a variable with 'expensive' if greater than mean of cost and 'cheap' otherwise
# group_by: split into cheap and expensive
# summarize: give some summary statistics of your choice

names(diamonds)

diamonds%>%
  filter(carat > 0.5)

diamonds%>%
  rename(cost = price)%>%
  mutate(mutation = ifelse(cost>mean(cost), "expensive", "cheap"))%>%
  group_by(mutation)

diamonds%>%
  summarise(price)

diamonds%>%
  split(.$carat)

### Data visualization ---------------------------------------------------------
# Change the barplot by creating a table of gender 
participants_barplot <- table(data$gender)

barplot(participants_barplot)

# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x) 
ggplot(data = data, 
       aes(x = letters_in_first_name, 
           y = days_to_email_response)) + 
  geom_point()

# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x) 
# with colors representing binary data 
# related to academic parents (color) 
# and working hours per day as bubble sizes (size).
ggplot(data = data, 
       aes(x = letters_in_first_name, 
           y = days_to_email_response,  
           color = academic_parents,
           size = working_hours_per_day))+
  geom_point()

names(data)

# Create a scatterplot of iris petal length (y) 
# as a function of sepal length (x) 
# with colors representing iris species (color) 
# and petal width as bubble sizes (size).
ggplot(data = iris, 
       aes(x = Sepal.Length, 
           y = Petal.Length, 
           color = Species, 
           size = Petal.Width))+ 
  geom_point()

# Create a plot with the diamonds data 
# of the carat (x) and the price (y)
plot1 <- ggplot(data = diamonds, 
                aes(x = carat, y = price, 
                    alpha = 0.2)) +
  geom_point()
plot(plot1)

# Create a plot with the diamonds data of the log of carat (x) 
# and the log of price (y)
ggplot(data = diamonds,
       aes(x = log(carat),
           y = log(price),
           alpha = 0.2)) +
  geom_point()

# Create a smaller diamonds data set (top 100 rows), create a scatterplot with carat on the x-axis 
# and price on the y-xis and with the color of the diamond as the color of the points. 
dsmall <- top_n(diamonds, n = 100)

ggplot(data = dsmall, aes(x = carat, 
                          y = price, 
                          color = color)) + 
  geom_point()

# Create a smaller diamonds data set (top 40 rows), create a scatterplot with carat on the x-axis 
# and price on the y-xis and with the cut of the diamond as the shapes for the points. 
dsmall <- top_n(diamonds, n = 40)

ggplot( data = dsmall, 
        aes(x = carat, 
            y = price, 
            shape = cut)) + 
  geom_point()

# Create a plot of the diamonds data with carat on the x-axis, price on the y-axis. 
# Use the inhibit function to set the alpha to 0.1 and color to blue.
ggplot(data = diamonds, 
       aes(x = carat, 
           y = price, 
           alpha = I(0.1), 
           color = "blue")) + 
  geom_point()

# Create a smaller data set of diamonds with 50 rows. Create a scatterplot and smoothed conditional 
# means overlay with carat on the x-axis and price on the y-axis.
dsmall <- top_n(diamonds, n = 50)

ggplot(data = dsmall, 
       aes(x = carat, 
           y = price))+
  geom_point()+
  geom_smooth(method = 'glm')

# Change the boxplot so that the x-axis is cut and the y-axis is price divided by carat
ggplot(data = diamonds, 
       aes(x = cut, 
           y = price/carat)) + 
  geom_boxplot()+
  geom_jitter()

# Change the alpha to 0.4 to make the scatter less transparent
ggplot(data = diamonds, 
       aes(x = cut, 
           y = price/carat, 
           alpha = I(0.4))) + 
  geom_boxplot()+ 
  geom_jitter()

# Change the density plot so that the x-axis is carat and the color is the diamond color
# and the alpha is set to 0.3 using the inhibit function
ggplot(data = diamonds, 
       aes(x = carat,
           color = color,
           alpha = I(0.3))) +
  geom_density()

# Create a plot of the mpg data with manufacturer as the color and a linear model 'lm'
# as the smooth method
ggplot(data = mpg, 
       aes(x = displ, 
           y = hwy,  
           color = manufacturer)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Change the title and labels as you see fit
ggplot(mtcars, 
       aes(mpg, 
           y = hp, 
           col = gear)) +
  geom_point() +
  ggtitle("Cars suck") +
  labs(x = "Howdy", 
       y = "Displayable", 
       col = "cool column")

# Change the title and labels as you see fit  
ggplot(data = mtcars) +
  aes(x = mpg) +
  labs(x = "the x label") +
  aes(y = hp) +
  labs(y = "the y label") +
  geom_point() +
  aes(col = gear) +
  labs(col = "legend title") +
  labs(title = "My Title")

# subset the data to numeric only with select_if
part_data <- select_if(data, 
                       is.numeric)
# use 'cor' to perform pearson correlation
# use 'round' to reduce correlation 
# results to 1 decimal
cormat <- round(cor(part_data), 
                digits = 1)
# use 'as.data.frame.table' to build a table
# with correlation values
melted_cormat <- as.data.frame.table(cormat, 
                                     responseName = "value")
# plot the result with 'geom-tile'
ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile()

# export shit 
png(file = "cortile.png", width = 7, height = 6, units = "in", res = 300)

ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

### Data animation -------------------------------------------------------------
names(datasaurus_dozen)

# Change the 'ease_aes()' option from default 'linear'
# to 'cubic-in-out' for a smoother appearance
ggplot(datasaurus_dozen, 
       aes(x = x,
           y = y))+
  geom_point()+
  theme_minimal() +
  transition_states(states = dataset) + 
  ease_aes("cubic-in-out")

# Change our 'dsmall' selection of diamonds data to 100 observations
dsmall <- top_n(diamonds, n = 100)

ggplot(data = dsmall, 
       aes(x = carat, 
           y = price, 
           color = color)) + 
  geom_line() +
  transition_reveal(carat) + 
  ease_aes("cubic-in-out") +
  labs(title = 'Diamond carat: {frame_along}')