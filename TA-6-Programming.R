
### Schedule for today

## 1) Go through the case_when functions: very useful for cleaning data
## 2) Explanation of for loops in R

# Always start with the libraries you plan to use in your script!
library(tidyverse)
library(gapminder)


##### case_when #####

## Very good for data cleaning / transforming

gapminder2 <- gapminder %>% 
  group_by(continent, year) %>% 
  summarize(lifeExp = mean(lifeExp),
            pop = sum(pop),
            gdpPercap = mean(gdpPercap))

## The arguments are evaluated in order, so you must
## proceed from the most specific to the most general

## A kind of "dummy variable" (is not a dummy variable though) indicating lifeExp > 70
gapminder3 <- gapminder2 %>%
  mutate(lifeExp_70 = case_when(lifeExp > 50 & lifeExp < 60 ~ "Between 50-60",
                                TRUE ~ "Other"))


## Graph
gapminder3 %>% 
  ggplot(aes(x = lifeExp_70)) +
  geom_bar() +
  facet_wrap(~year)

## Classification of gdp (not academic/theoretical) - Americas continent
gapminder4 <- gapminder %>% 
  filter(continent == "Americas")

gapminder5 <- gapminder4 %>%
  mutate(gdp_classification = case_when(gdpPercap < 3500 ~ "Very poor",
                                        gdpPercap >= 3500 & gdpPercap < 5500 ~ "Poor",
                                        gdpPercap >= 5500 & gdpPercap < 7500 ~ "Development",
                                        gdpPercap >= 7500 & gdpPercap < 10000 ~ "Rich",
                                        gdpPercap >= 10000 ~ "Very rich"),
         gdp_classification = reorder(gdp_classification, gdpPercap))

gapminder6 <- gapminder5 %>% 
  mutate(gdp_classification2 = case_when(gdp_classification %in% c("Rich", "Very rich") ~ "Rich",
                                         TRUE ~ "Not rich"))

gapminder5 %>% 
  ggplot(aes(x = gdp_classification)) +
  geom_bar() +
  facet_wrap(~year) +
  coord_flip()
  


##### For Loops #####

# Frequently, when programming, you will find yourself inclined to do the same thing many times.
# Here is a simple example:

print("Rio de Janeiro")
print("Tokyo")
print("Madrid")
print("Chicago")
print("New York")
print("Paris")


## 1.1 Theory

# There are four important parts of a for loop:

# OUTPUT - what the loop is going to return you (in most cases you will want a value)
# ITERABLE - usually called 'i', but it can be anything you want. This is the term that will be changing in each iteration of the for loop
# RANGE - this is the series of values over which the for loop will iterate. The iterable will be each value in this range one time.
# BODY - this is the code that runs for each iteration of the for loop. The body of code, encompassed in curly braces {}, runs once for every value of the range.

# R has way of indexing our values so he will know which element to select in each iteration

# for ("variable" in "a sequence") {
#     
#   "do this"
# 
#   "do more this"
# 
#   "do more that" 
#
#   "now, show me the final result"
#
# }

## 1.2 - Application 1

cities <- c("New York", "Tokyo", "Rio de Janeiro", "Madrid", "London", "Paris", "Chicago")


for (city in cities) {
  
  print(city)
  
}


## 1.3 - Application 2

## If we want to have more domain over the "for loop", we can decide the indexation ourselves

for (i in seq_along(cities)) {
  
  print(i)
  
}


## QUESTION ##

## Which loop is better: from application 1 or 2?

# ----

## However, many of R's functions are vectorized - What does that means?

## That means that most R functions that the function will operate on all elements of a vector
## without needing to loop through and act on each element one at a time.
## This makes writing code more concise, easy to read, and less error prone!

## 1.4 - Example

## Out two vectors
numbers <- c(1:7)

cities

## str_c is a function that joins two or more vectors "element-wise"
str_c(cities, numbers)



