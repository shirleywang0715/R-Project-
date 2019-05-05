# This sets the size of plots to a good default.
options(repr.plot.width = 5, repr.plot.height = 4)

# Loading in packages
library(readr)
library(dplyr)
library(ggplot2)

# Reading in the data
data <- read_csv("/Users/shirley/Documents/Data Learning/R Dataset/Candy Crush/candy_crush.csv")

# Printing out the first couple of rows
# .... YOUR CODE FOR TASK 2 ....
head(data)

print("Number of players:")
length(unique(data$player_id))
print("Period for which we have data:")
range(data$dt)

# Calculating level difficulty
difficulty <- 
  data %>%
  group_by(level) %>%
  summarise(attempts=sum(num_attempts),success=sum(num_success)) %>%
  mutate(p_win=success/attempts)

# Printing out the level difficulty
difficulty

# Plotting the level difficulty profile
difficulty %>%
  ggplot(aes(x=level,y=p_win))+
  geom_line()+
  scale_x_continuous(breaks = 1:15)+
  scale_y_continuous(label = scales::percent)

# Adding points and a dashed line
difficulty %>%
  ggplot(aes(x=level,y=p_win))+
  geom_line()+
  scale_x_continuous(breaks = 1:15)+
  scale_y_continuous(label = scales::percent)+
  geom_point()+
  geom_hline(yintercept = 0.5, linetype = 'dashed')

# Computing the standard error of p_win for each level
difficulty <- difficulty %>%
  mutate(error=sqrt(p_win * (1 - p_win) / attempts))

# Adding standard error bars
difficulty %>%
  ggplot(aes(x=level,y=p_win))+
  geom_line()+
  scale_x_continuous(breaks = 1:15)+
  scale_y_continuous(label = scales::percent)+
  geom_point()+
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  geom_errorbar(aes(ymin = p_win-error, ymax = p_win + error))

# The probability of completing the episode without losing a single time
p <- prod(difficulty$p_win)

# Printing it out
p

# Should our level designer worry about that a lot of 
# players will complete the episode in one attempt?
should_the_designer_worry = FALSE