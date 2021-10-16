install.packages("matrixStats")
library(matrixStats)
library(IntroProbR)
library(tidyverse)

# Question 1:
# In the early 1600s, Galileo was asked to explain why, although the number of
# triples of integers from 1 to 6 with sum 9 is the same as the number of such
# triples with sum 10, gamblers claimed that when three six-sided dice are 
# rolled, a 9 seemed to appear less often than a 10.

# Write a function to simulate the roll of three dice m times and evaluate
# the proportion of times that the sum is 9 and the proportion of times that
# the sum is 10 by simulating the roll m= 100,000 times. (It is necessary to
# write and run this function; do not provide an exact derivation.)

# According to your simulation, does 10 appear more often than 9?

why_Galileo <- function(m){
  die_roll <-  sample(1:6, 3*m, replace = TRUE)
  die_roll_matrix <- matrix(nrow = m, ncol = 3, data = die_roll)
  die_sum <- rowSums(die_roll_matrix)
  
  sum_is_nine <- die_sum == 9
  sum_nine_proportion <- sum(sum_is_nine)/m
  sum_is_ten <- die_sum == 10
  sum_ten_proportion <- sum(sum_is_ten)/m
  return(tibble(proportion_for_nine = sum_nine_proportion, 
                proportion_for_ten = sum_ten_proportion))
}

why_Galileo(100000)

# Question 2:
# In racquetball, a player continues to serve as long as she is winning;
# a point is scored only when a player is serving and wins the volley.
# The first player to reach 21 points wins the game. Assume you serve
# first and have a probability 0.6 of winning a volley when you serve, 
# and probability 0.5 when your opponent serves.

# Write a function, win_racquetball, that simulates a game of racquetball
# and returns TRUE if you win and FALSE if you lose. (Hint: use a loop. 
# At each serve, you will need to keep track of who is serving and whether you win. 
# Track both your points and your opponent's points, and stop the simulation when
# one of them reaches 21.)

# Use this function to estimate the probability that you will win a racquetball
# game, by running it m = 10,000 times. 

win_racquetball <- function(){
  my_points <- 0
  opponent_points <- 0
  i_serve <- TRUE
  
  while (my_points < 21 & opponent_points <21){
    if (i_serve){
      win_volley <- sample(c("Win", "Lose"), 1, prob = c(0.6, 0.4))
      if (win_volley == "Win"){
        my_points <- my_points + 1
      } else {
        i_serve <- FALSE
      }
    } else {
      win_volley_opp <- sample(c("Win", "Lose"), 1, prob = c(0.5, 0.5))
      if (win_volley_opp == "Win"){
        opponent_points <- opponent_points + 1
        i_serve <- FALSE
      } else {
        i_serve <- TRUE
      }
    }
  }
  return(my_points == 21)
}

probability_i_win <- function(m){
  wins <- 0
  for (game in 1:m){
    if (win_racquetball()){
      wins <- wins + 1
    }
  }
  probability <- wins/m
  return(probability)
}

probability_i_win(10000)

# Question 3:
# The following function will simulate the price of one share of a fictional 
# stock over n days (n = 30 by default) of trading. The share price starts at
# $100 and fluctuates over the following n-1 days. Answer the following
# questions by simulating the stock price m = 10,000 times.

stock_price <- function(n = 30, v0 = 100) {
  log_returns <- rnorm(n = n - 1, mean = 0, sd = 0.1)
  log_price <- cumsum(c(log(v0), log_returns))
  price <- exp(log_price)
  return(price)
} 

# (a) Estimate the probability that, at the end of 30 days, the stock price is 
# $120 or higher.

price_more_than_120 <- function(m){
  prices <- matrix(nrow = m, ncol = 30)
  for (i in 1:m){
    prices[i,] <- stock_price()
  }
  evaluate <- prices[, c(30)]
  higher_than_120 <- evaluate >= 120
  probability <- sum(higher_than_120)/m
  return(probability)
}

price_more_than_120(10000)

# (b) Estimate the probability after 30 days that the stock price closed at $120
# or higher at least once.

at_least_120_once <- function(m){
  prices <- matrix(nrow = m, ncol = 30)
  for (i in 1:m){
    prices[i,] <- stock_price()
  }
  price_at_least_120 <- prices >= 120
  month_has_120_above <- rowSums(price_at_least_120)>=1
  probability <- mean(month_has_120_above)
  return(probability)
}

at_least_120_once(10000)

# (c) Suppose you purchase one share of the stock at day one, for $100. You hold 
# the shares until either the share price reaches $120 or higher, or $90 or lower, 
# at which point you sell them all. If this doesn't happen, you sell all of the 
# shares at the end of the 30 days. There are no trading fees, etc. Find the
# following probabilities:

# i. the probability that you sell at a profit.

profit_prob <- function(m){
  prices <- matrix(nrow = m, ncol = 30)
  for (i in 1:m){
    prices[i,] <- stock_price()
  }
  evaluate <- prices <= 90 | prices >= 120 
  profit <- 0
  first_matrix <- matrix(nrow = m, ncol = 1)
  for (i in 1:m){
    find <- which(evaluate[i,])
    first <- ifelse(length(find)>0, min(find), 0)
    first_matrix[i,] <- first
    if (first_matrix[i,]< 30 & first_matrix[i,]!= 0){
      locate <- first_matrix[i,]
      price <- prices[i, locate]
      if (price>100){
        profit <- profit + 1
      }
    } else{
      if (prices[i,30]>100){
        profit <- profit + 1
      }
    }
  }
  probability <- profit/m
  return(probability)
}
profit_prob(10000)

# ii. the probability that you sell at a loss.

loss_prob <- function(m){
  prices <- matrix(nrow = m, ncol = 30)
  for (i in 1:m){
    prices[i,] <- stock_price()
  }
  evaluate <- prices <= 90 | prices >= 120 
  loss <- 0
  first_matrix <- matrix(nrow = m, ncol = 1)
  for (i in 1:m){
    find <- which(evaluate[i,])
    first <- ifelse(length(find)>0, min(find), 0)
    first_matrix[i,] <- first
    if (first_matrix[i,]< 30 & first_matrix[i,]!= 0){
      locate <- first_matrix[i,]
      price <- prices[i, locate]
      if (price<100){
        loss <- loss + 1
      }
    } else{
      if (prices[i,30]<100){
        loss <- loss + 1
      }
    }
  }
  probability <- loss/m
  return(probability)
}

loss_prob(10000)

# iii. the probability that you sell the stock at the end of 30 days and not 
# before.

sell_end_prob <- function(m){
  prices <- matrix(nrow = m, ncol = 30)
  for (i in 1:m){
    prices[i,] <- stock_price()
  }
  evaluate <- prices <= 90 | prices >= 120 
  end_of_30_days <- 0
  first_matrix <- matrix(nrow = m, ncol = 1)
  for (i in 1:m){
    find <- which(evaluate[i,])
    first <- ifelse(length(find)>0, min(find), 0)
    first_matrix[i,] <- first
    if (first_matrix[i,]== 30 | first_matrix[i,]== 0){
      end_of_30_days <- end_of_30_days + 1
      }
  }
  probability <- end_of_30_days/m
  return(probability)
}
options(scipen=10000)
sell_end_prob(10000)

# iv. your expected (mean) net profit (possibly negative).

expected_net_profit <- function(m){
  prices <- matrix(nrow = m, ncol = 30)
  for (i in 1:m){
    prices[i,] <- stock_price()
  }
  evaluate <- prices <= 90 | prices >= 120 
  net_profit <- 0
  first_matrix <- matrix(nrow = m, ncol = 1)
  for (i in 1:m){
    find <- which(evaluate[i,])
    first <- ifelse(length(find)>0, min(find), 0)
    first_matrix[i,] <- first
    if (first_matrix[i,]< 30 & first_matrix[i,]!= 0){
      locate <- first_matrix[i,]
      price <- prices[i, locate]
      net_profit <- net_profit + (price-100)
    }
    if (first_matrix[i,]== 0 | first_matrix[i,]== 30){
      price <- prices[i, 30]
      net_profit <- net_profit + (price-100)
      }
  }
  mean_net_profit <- net_profit/m
  return(mean_net_profit)
}
expected_net_profit(10000)
