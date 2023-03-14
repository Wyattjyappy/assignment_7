Assignment #7


library(tidyverse)
library(knitr)
library(dslabs)

<br>
  
  ## Excercise: 2016 election result and polling
  
  For this exercise, we will explore the result of the 2016 US
presidential election as well as the polling data. We will use the
following three datasets in the `dslabs` package, and use `join`
function to connect them together. As a reminder, you can use `?` to
learn more about these datasets.

- `results_us_election_2016`: Election results (popular vote) and
electoral college votes from the 2016 presidential election.

- `polls_us_election_2016`: Poll results from the 2016 presidential
elections.

- `murders`: Gun murder data from FBI reports. It also contains the
population of each state.

We will also use [this
                  dataset](https://raw.githubusercontent.com/kshaffer/election2016/master/2016ElectionResultsByState.csv)
to get the exact numbers of votes for question 3.

<br>
  
  ### Question 1. What is the relationship between the population size and the number of electoral votes each state has?
  
  **1a.** Use a `join` function to combine the `murders` dataset, which
contains information on population size, and the
`results_us_election_2016` dataset, which contains information on the
number of electoral votes. Name this new dataset `q_1a`, and show its
first 6 rows.

results_us_election_2016

murders

q_1a <- murders %>% 
  left_join(results_us_election_2016)

q_1a %>% 
  head()

# 1a ----------------------------------------------------------------------



<br> <br>
  
  **1b.** Add a new variable in the `q_1a` dataset to indicate which
candidate won in each state, and remove the columns `abb`, `region`, and
`total`. Name this new dataset `q_1b`, and show its first 6 rows.

merge(results_us_election_2016, murders) %>% 
  select("state", "population", "electoral_votes", "clinton", "trump", "others") %>% 
  mutate(winner = airlines$name[match(carrier, airlines$carrier)])

murders %>% 
  left_join(results_us_election_2016) %>% 
  select("state", "population", "electoral_votes", "clinton", "trump", "others") %>% 
  mutate(winner = ("clinton" > "trump") %>% 
           head()
  
q_1a %>% 
  select("state", "population", "electoral_votes", "clinton", "trump", "others") %>% 
  mutate(winner = ("clinton" > "trump"))


# 1c ---------------------------------------------------------------------


q_1b %>% 
  ggplot(mapping = aes(x = population, y = electoral_votes, color = winner)) + 
  geom_smooth(mapping = aes(x = population, y = electoral_votes), se = 0.1) +
  geom_point()

q_1b %>% 
  ggplot(mapping = aes(x = population, y = electoral_votes, color = winner)) +
  geom_point() +
  geom_smooth(span = 0.3)
  
q_1b %>% 
  ggplot(mapping = aes(x = population, y = electoral_votes)) +
  geom_point(mapping = aes(color = winner)) +
  geom_smooth(mapping = aes(x = population, y = electoral_votes), method = lm, se = FALSE)

q_1b %>% 
  ggplot(mapping = aes(x = population, y = electoral_votes)) +
  geom_point(mapping = aes(color = winner)) +
  geom_smooth(mapping = aes(x = population, y = electoral_votes), method = lm, se = FALSE, color = "black", size = 0.1)



# Question 2a --------------------------------------------------------------

q_2b %>% 
  pivot_longer(c( population, electoral_votes), names_to = 'metric', values_to = 'value') 

q_2b %>% 
pivot_longer(cols = c(population, electoral_votes), names_to = "metric", values_to = "value")
  

# Question 2b -------------------------------------------------------------


q_1b %>% 
  pivot_longer(c( population, electoral_votes), names_to = 'metric', values_to = 'value') %>% 
  select(metric, winner, value) %>% 
  mutate(value = sum(population))

q_1b %>% 



mutate(winner = ifelse(clinton>trump, "clinton", "trump")) %>% 
  subset(select = -c(abb, region, total))

q_1b %>% 
  pivot_wider(names_from = value)

q_1b %>% 
  filter(metric, winner, value)

q_2b <- q_2a %>% 
  group_by(metric, winner) %>% 
  summarize(value = sum(value))

q_2b


# Question 2c -------------------------------------------------------------

geom_col(position = "fill")


q_2b %>% 
  ggplot(aes(metric)) +
  geom_bar()
 
q_2b %>% 
  ggplot(mapping = aes(x = metric, y = value, fill = winner)) +
  geom_col(position = "fill")
  


# Question 3a -------------------------------------------------------------

dataset3 %>% 
  summarize(sum(clintonVotes))

dataset3 %>%
  summarize(sum(trumpVotes))

dataset3 %>% 
  summarize(sum(othersVotes))

X2016ElectionResultsByState

dataset3 <- read_csv(https://raw.githubusercontent.com/kshaffer/election2016/master/2016ElectionResultsByState.csv)

q_3a <- dataset3 %>% 
  pivot_longer(c("clintonVotes", "trumpVotes", "othersVotes"),
               names_to = "candidate", values_to = "votes") %>% 
  group_by(candidate) %>% 
  summarise(popular_votes = sum(votes)) %>% 
  mutate(winner = "candidate")

q_3a <- dataset3 %>% 
  summarise(clinton = sum(clintonVotes),trump = sum(trumpVotes), others = sum(totalVotes - clintonVotes - trumpVotes)) %>% 
  pivot_longer(cols = c("clinton", "trump", "others"), names_to = "winner", values_to = "value") %>% 
  mutate(metric = "popular_votes") %>% 
  select(metric, winner, value)

q_3a


library(dplyr)
library(tidyr)
library(readr)

# Read in the data
results <- read_csv("https://raw.githubusercontent.com/kshaffer/election2016/master/2016ElectionResultsByState.csv")

# Summarize the data to get the popular vote counts
q_3a <- dataset3 %>% 
  summarise(clinton = sum(clintonVotes),trump = sum(trumpVotes), others = sum(totalVotes - clintonVotes - trumpVotes)) %>% 
  pivot_longer(cols = c("clinton", "trump", "others"), names_to = "winner", values_to = "value") %>% 
  mutate(metric = "popular_votes") %>% 
  select(metric, winner, value)

q_3a %>% 
  summarise(clinton = sum(clintonVotes),trump = sum(trumpVotes), others = sum(totalVotes - clintonVotes - trumpVotes)) %>% 
  pivot_longer(cols = c("clinton", "trump", "others"), names_to = "winner", values_to = "value") %>% 
  mutate(metric = "popular_votes") %>% 
  select(metric, winner, value)


# Question 3b -------------------------------------------------------------

q_2b

q_3a

q_3b <- q_2b %>% 
  full_join(q_3a)


# Question 3c -------------------------------------------------------------

q_3b %>% 
  ggplot(mapping = aes(x = metric, y = value, fill = winner)) +
  geom_col(position = "fill")


# Question 4a --------------------------------------------------------------

polls_us_election_2016

q_4a <- polls_us_election_2016 %>% 
  filter(pollster == "Ipsos") %>% 
  filter(state != "US") %>% 
  group_by(state) %>% 
  slice_max(enddate) %>% 
  select(state, adjpoll_clinton, adjpoll_trump)


# Question 4b -------------------------------------------------------------

q_1b

q_4a


q_4b <- q_1b %>% 
  left_join(q_4a) %>% 
  mutate(polling_margin = adjpoll_clinton - adjpoll_trump) %>% 
  mutate(actual_margin = clinton - trump) %>% 
  mutate(polling_error = polling_margin - actual_margin) %>% 
  mutate(predicted_winner = ifelse(clinton>trump, "clinton", "trump")) %>% 
  mutate(result = ifelse(winner == predicted_winner, "correct prediction", str_c("unexpected ", winner, " win"))) %>% 
  select(state, polling_error, result, electoral_votes)


# Question 4c -------------------------------------------------------------

q_4b %>% 
  ggplot(mapping = aes(x = polling_error, y = state, size = electoral_votes, color = result)) +
  geom_point()







  