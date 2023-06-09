# This code adapted from https://www.codingfinance.com/post/2018-05-31-portfolio-opt-in-r/

# Load packages

library(tidyverse) # To download the data 
library(tidyquant) # To download the data 
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series

# Download data for Scottish Mortgage Investment Trust PLC, Schroders, Coca-Cola HG, Centrica and Auto Trader Group plc

tick <- c('SMT.L', 'SDR.L', 'CCH.L', 'CNA.L', 'AUTO.L') 
price_data <- tq_get(tick, 
                     from = '2020-01-01', 
                     to = '2023-03-31', 
                     get ='stock.prices')

# Calculate daily returns

log_ret_tidy <- price_data %>% group_by(symbol) %>%
tq_transmute(select = adjusted, 
             mutate_fun = periodReturn, 
             period = 'daily', 
             col_rename = 'ret', 
             type = 'log')

# Inspect a few rows

head(log_ret_tidy)

# Rearrange data and turn it into time series

log_ret_xts <- log_ret_tidy %>% 
  spread(symbol, value = ret) %>%
  tk_xts()

# Inspect a few rows

head(log_ret_xts)

# Calculate mean daily returns

mean_ret <- colMeans(log_ret_xts) 
print(round(mean_ret, 5))

# Construct covariance matrix and annualise

cov_mat <- cov(log_ret_xts) * 252 
print(round(cov_mat,4))

# Demonstrate method using single portfolio

# Create random weights

wts <- runif(n = length(tick)) 
print(wts)

# Show that sum of weights is not necessarily 1 (it needs to be!)

print(sum(wts))

# Make sure sum of weights is 1

wts <- wts/sum(wts) 
print(wts) 
sum(wts)

# Calculate annualised portfolio returns

port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

# Calculate annualised portfolio risk

port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts)) 
print(port_risk)

# Calculate Sharpe ratio (assuming zero risk free-rate)

sharpe_ratio <- port_returns/port_risk 
print(sharpe_ratio)

# Using a 'loop' to do the above for 5,000 portfolios

num_port <- 5000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port, 
                  ncol = length(tick))

# Creating an empty vector to store Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port) 
for (i in seq_along(port_returns)) {

wts <- runif(length(tick)) 
wts <- wts/sum(wts)

# Storing weight in the matrix 

all_wts[i,] <- wts

# Portfolio returns

port_ret <- sum(wts * mean_ret) 
port_ret <- ((port_ret + 1)^252) - 1

# Storing Portfolio Returns values 

port_returns[i] <- port_ret

# Creating and storing portfolio risk 

port_sd <- sqrt(t(wts) %*% (cov_mat %*% wts)) 
port_risk[i] <- port_sd

# Creating and storing Portfolio Sharpe Ratios a ssuming 0% Risk free rate

sr <- port_ret/port_sd 
sharpe_ratio[i] <- sr

} 

# Storing the values in the table 
portfolio_values <- tibble(Return = port_returns, 
                           Risk = port_risk, 
                           SharpeRatio = sharpe_ratio)

# Converting matrix to a tibble and changing column names

all_wts <- tk_tbl(all_wts) 
colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together

portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

head(portfolio_values) 

# The minimum variance portfolio

min_var <-portfolio_values[which.min(portfolio_values$Risk),] 

# The tangency portfolio (the portfolio with highest sharpe ratio)

max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

# plot the weights of the min var portfolio

p <- min_var %>%
  gather(SMT.L, SDR.L, CCH.L, CNA.L, AUTO.L, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

# plot the weights of the tangency portfolio

p <- max_sr %>%
  gather(SMT.L, SDR.L, CCH.L, CNA.L, AUTO.L, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)


# visualise the efficient frontier

p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent, limits = c(0,0.4)) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'green') +
  annotate('text', x = 0.22, y = 0.06, label = "Tangency Portfolio") +
  annotate('text', x = 0.25, y = 0.02, label = "Minimum variance portfolio") +
  annotate(geom = 'segment', x = 0.24, xend = 0.26,  y = 0.012, 
           yend = 0.00, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.265, xend = 0.32,  y = 0.05, 
           yend = 0.037, color = 'red', arrow = arrow(type = "open"))

ggplotly(p)
