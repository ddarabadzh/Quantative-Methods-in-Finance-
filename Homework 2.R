#####Problem 1#####
# Write a function, which uses a loop to calculate factorial.
# The base R function is called factorial and you should replicate its result.
# This is a function, which takes two numbers, multiplies them and returns
# the result as output:
# MultiplicationFunction <- function(inputNumber1, inputNumber2){
#   Result <- inputNumber1 * inputNumber2
#   return(Result)
# }
# MultiplicationFunction(5, 3)
# 
# Write a factorial function:

# FactorialFunction <- function(inputNumber){
#   ???
#     return(Result)
# }
#####Problem 1#####
fact_function = function(x){
  total = x
  while(x> 1){
    total = total * (x - 1)
    x = x -1
  }
  print(total)
}
fact_function(6)
#####Problem 2#####
#Write a function which takes a vector and returns its standard deviation.
#You should get the same results as the sd() function.
# SDFunction <- function(inputVector){
#   ???
#     return(Result)
# }
# ??? is not Result <- sd(inputVector)
#####Problem 2#####
inputVector = c(4, 5, 6, 12, 7)
SDFunction <- function(inputVector){
  avg = mean(inputVector)
  sum = 0
  quantity = length(inputVector)
  for (i in inputVector){
    sum = sum + ((i - avg) ^ 2)
  }
  result = sqrt(sum / quantity)
  print(result)
  inputVector = NULL
}
SDFunction()

#####Problem 3#####
# Read everything from https://r4ds.had.co.nz/transform.html, 
# in particular chapters 5.6/5.7

#Do all the exercises:
# 5.6.7 Exercises 
# 5.7.1 Exercises
#####Problem 3#####
library(tidyverse)
library(nycflights13)
#1 Brainstorm at least 5 different ways to assess the typical delay characteristics of a group of flights. Consider the following scenarios:
#a) A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time
#b) A flight is always 10 minutes late.
#c) A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.
#d} 99% of the time a flight is on time. 1% of the time it's 2 hours late.
# d would be best in terms of total time spent in delays, but it has the highest odds of affecting the plans passengers, whether that is a meeting or catching another flight.
# All scenarios would worsen the reputation of the company, but d) is most convenient to most customers. The other alternative would be b)
# 2 Come up with another approach that will give you the same output as not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) (without using count()).
not_canceled = flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_canceled %>%
  group_by(dest) %>%
  summarise(n = length(dest))
#3 Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) ) is slightly sub optimal. Why? Which is the most important column?
# a flight might depart, but land preemptively due to a malfunction or even crash, so the most important column is dep_delay, because if it is na, then the flight hasn't departed at all.
#4 Look at the number of cancelled flights per day. Is there a pattern? Is the proportion of cancelled flights related to the average delay?
daily_cancels = flights %>% 
  mutate(cancelled = (is.na(dep_delay) | is.na(arr_delay))) %>% 
  group_by(year, month, day) %>% 
  summarise(cancelled_amount = sum(cancelled), flights_amount=n())
library(ggplot2)
ggplot(daily_cancels) +
  geom_point(aes(x = flights_amount, y = cancelled_amount))
# the more flights there are, the more delays occur
#5 Which carrier has the worst delays? Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))
flights %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay)) ## F9 has the worst delays
filter(airlines, carrier == "F9")
#Frontier Airlines Inc.
#6 What does the sort argument to count() do. When might you use it?
#If set to TRUE, the sort argument in count() will arrange the outputs in descending order
# 5.7.1 Exercises
# 1 Refer back to the lists of useful mutate and filtering functions. Describe how each operation changes when you combine it with grouping.
#2 Which plane (tailnum) has the worst on-time record?
flights %>% 
  filter(!is.na(tailnum)) %>%
  mutate(on_time = (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), numberOfFlights = n()) %>%
  filter(min_rank(on_time) == 1) #I copied this code from the internet. I am clueless on how to accomplish the task
#3 What time of day should you fly if you want to avoid delays as much as possible?
flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)#morning flights usually depart early, with negative delay
#4 For each destination, compute the total minutes of delay. For each flight, compute the proportion of the total delay for its destination.
flights %>% 
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    totalArrDelay = sum(arr_delay),
    DelayProportion = arr_delay / totalArrDelay
  ) %>%
  arrange(desc(DelayProportion))
# I couldnt wrap my head around how to solve 5 and 6
#7Find all destinations that are flown by at least two carriers. Use that information to rank the carriers.
flights %>%
  group_by(dest)%>%
  mutate(NumOfCarriers = n_distinct(carrier)) %>%
  filter(NumOfCarriers > 1) %>%
  group_by(carrier) %>%
  summarize(NumOfDestinations = n_distinct(dest)) %>% 
  arrange(desc(NumOfDestinations))
#8For each plane, count the number of flights before the first delay of greater than 1 hour.
# I am incapable of solving this task
#####Problem 4#####
#Find the following:
#4.1 For each carrier what is the most common destination?
flights %>% 
  group_by(dest) %>%
  mutate(number_flights = n(tailnum))
#4.2 For each carrier what is the biggest delay?
flights %>%
  group_by(carrier) %>%
  summarize(largest_delay = max(arr_delay, na.rm = TRUE)) 

#4.3 Which are the three plane which have flown the most/least miles?
flights %>%
  group_by(tailnum) %>%
  transmute(total_distance = sum(distance))%>%
  arrange(total_distance) %>%
  head(3) #least miles
naless_flights = na.omit(flights) %>%
  group_by(tailnum) %>%
  transmute(total_distance = sum(distance))%>%
  arrange(total_distance) %>%
  tail(3) # I couldnt come up with a proper way to make the answers distinct 

#4.4 What are the first/last flights for each day in February 2013?
naless_flights_max = na.omit(flights) %>%
  filter(month == 2)%>%
  group_by(day) %>%
  summarize(max(dep_time))
naless_flights_min = na.omit(flights) %>%
  filter(month == 2)%>%
  group_by(day) %>%
  summarize(min(dep_time))
#4.5 Which company flew the most miles in March 2013? Which flew the least?
flights %>%
  filter(month == 3) %>%
  group_by(carrier)%>%
  summarize(total_dist = sum(distance))%>%
  arrange(total_dist)%>%
  #head(1). least -  YV            4122
  tail(1) #Most - UA         7235740

#4.6 Which month had the most delays over 60 minutes?
flights %>%
  filter(dep_delay > 60) %>%
  group_by(month)%>%
  summarize(delay_count = n())%>%
  arrange(desc(delay_count)) # July, with 3820 delays on departure, larger than 60 minutes
#4.7 What is the average time between two consecutive flights?
naless_flights_4_7 = na.omit(flights) %>%
  for(x in naless_flights_4_7){
    time_sum = time_sum + arr_time - dep_time(x+1)
  } %>%
  summarize(average_between_flights = time_sum / length(naless_flights_4_7)) # my code doesnt work here, but I don't know why exactly
#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation
#of the flight delays for each month and for each destination.
na.omit(flights) %>%
  group_by(month, dest) %>%
  summarize(SDFunction2 = sd(arr_delay))
#####Problem 4#####
