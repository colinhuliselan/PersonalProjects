## ----setup, include=FALSE--------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      fig.path = "README_figs/README-")


## ----import, message = FALSE, results = 'hold'-----------------------------------
# Import libraries
library(dplyr)
library(BTYD)
library(MLmetrics)
library(ggplot2)
library(gridExtra)

# Import data
df <- read.table('data/CDNOW_master.txt')
df[1:5, ]
cat('Number of rows: ', nrow(df), '\n',
    'Number of columns: ', ncol(df), sep = '')


## ----data cleaning---------------------------------------------------------------
# Change the column names
colnames(df) <- c('cust', 'date', 'quantity', 'sales')

# Convert date column to date type
df$date <- as.Date(as.character(df$date), format = "%Y%m%d")


## ----basic statistics, message = FALSE-------------------------------------------
# Basic statistics
df_basicstats <- df %>%
  summarise('Number of customers' = length(unique(cust)),
            'Number of transactions' = n(),
            'Begin period' = min(date),
            'End period' = max(date),
            'Avg. number of transactions per customer' = 
                round(n()  / length(unique(cust)), 2),
            'Avg. transaction value' = round(mean(sales), 2)) %>%
  t() %>%
  data.frame() %>%
  setNames('Statistic')

df_basicstats


## ----rfm variables, message = FALSE----------------------------------------------
# Setting our split date for the hold-out set
split_date <- as.Date('1997-12-31')

# Creating RFM variables
df_rfm <- df %>%
  group_by(cust) %>%
  mutate(returns = ifelse(max(date) > split_date, 1, 0)) %>%
  filter(date <= split_date) %>%
  summarise(recency = as.numeric(split_date - max(date)),
            frequency = n(),
            monetary_value = mean(sales),
            returns = as.factor(mean(returns)))
df_rfm[1:5, ]


## ----rfm summary, message = FALSE------------------------------------------------
summary(df_rfm[-1])


## ----hist plot, message = FALSE--------------------------------------------------
ggplot2::theme_set(theme_bw())

# Function for each individual plot
plot_column <- function(df, column){
  ggplot(data = df_rfm, aes_string(x = column, fill = 'returns', 
                                   color = 'returns')) + 
    geom_histogram(alpha=0.5)
}

# Making and arranging the plots
plot_list <- lapply(c('recency', 'frequency', 'monetary_value'), plot_column, 
                    df = df_rfm)
grid.arrange(grobs = plot_list,
             nrow = 3)


## ----df model, message = FALSE---------------------------------------------------
# Making a dataframe to be used by the BTYD package
# Note that for our problem and model, we only need the dates and the id's
df_model <- unique(df[c('cust', 'date')]) %>%
  group_by(cust) %>%
  mutate(returns = ifelse(max(date) > split_date, 1, 0)) %>%
  filter(date <= split_date) %>%
  summarise(x = n() - 1,
            t.x = as.numeric(max(date - min(date))),
            T.cal = as.numeric(max(split_date - date)),
            recency = as.numeric(split_date - max(date)),
            returns = mean(returns)) %>%
  as.data.frame()
df_model[1:5, ]


## ----btyd fit, message = FALSE---------------------------------------------------
# Fitting our models
pnbd_params <- pnbd.EstimateParameters(df_model)
bgnbd_params <- bgnbd.EstimateParameters(df_model)

cat('Pareto/NBD parameters (r, alpha, s, beta):',  round(pnbd_params, 2), '\n',
    'BG/NBD parameters (r, alpha, a, b):',  round(bgnbd_params, 2), sep = ' ')


## ----return probability, class.source = 'fold-hide', message = FALSE-------------
pnbd.ConditionalReturnProbability <- function (params, T.star, x, t.x, T.cal) 
{
  max.length <- max(length(x), length(t.x), length(T.cal))
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x)) 
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal)) 
    warning("Maximum vector length not a multiple of the length of T.cal")
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, 
                        "pnbd.DERT")
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x)) 
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  r <- params[1]
  alpha <- params[2]
  s <- params[3]
  beta <- params[4]
  # See equation (22) in Fader (2014) - Deriving the Conditional PMF...
  if (alpha >= beta){
    F1 <- gsl::hyperg_2F1(r + s + x, s + 1, r + s + x + 1, (alpha - beta)/(alpha + t.x))
    F2 <- gsl::hyperg_2F1(r + s + x, s, r + s + x + 1, (alpha - beta)/(alpha + T.cal))
    F3 <- gsl::hyperg_2F1(r + s + x, s, r + s + x + 1, (alpha - beta)/(alpha + T.cal + T.star))
    # Using a/b = exp(log(a) - log(b)) to rewrite fraction
    # Then using log(c+d) = log(c) + log(1 + d/c) to rewrite log(a) and log(b)
    l.num <- log( 1 + exp(log(r + x) + log(F3) - (r + s + x) * log(alpha + T.cal + T.star) -
                            log(s) - log(F1) + (r + s + x) * log(alpha + t.x)))
    l.denum <- log( 1 + exp(log(r+x) + log(F2) - (r + s + x) * log(alpha + T.cal) -
                              log(s) - log(F1) + (r + s + x) * log(alpha + t.x)))
  } else{
    F1 <- gsl::hyperg_2F1(r + s + x, r + x, r + s + x + 1, (beta - alpha)/(beta + t.x))
    F2 <- gsl::hyperg_2F1(r + s + x, r + x + 1, r + s + x + 1, (beta - alpha)/(beta + T.cal))
    F3 <- gsl::hyperg_2F1(r + s + x, r + x + 1, r + s + x + 1, (beta - alpha)/(beta + T.cal + T.star))
    l.num <- log( 1 + exp(log(r + x) + log(F3) - (r + s + x) * log(beta + T.cal + T.star) -
                            log(s) - log(F1) + (r + s + x) * log(beta + t.x)))
    l.denum <- log( 1 + exp(log(r+x) + log(F2) - (r + s + x) * log(beta + T.cal) -
                              log(s) - log(F1) + (r + s + x) * log(beta + t.x)))
  }
  return(1 - exp(l.num - l.denum))
}

bgnbd.ConditionalReturnProbability <- function(params, T.star, x, t.x, T.cal) 
{
  max.length <- max(length(x), length(t.x), length(T.cal))
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x)) 
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal)) 
    warning("Maximum vector length not a multiple of the length of T.cal")
  dc.check.model.params(c("r", "alpha", "a", "b"), params, 
                        "bgnbd.PAlive")
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x)) 
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)
  r = params[1]
  alpha = params[2]
  a = params[3]
  b = params[4]
  y = 0
  
  beta.ratio = function(a, b, x, y) {
    exp(lgamma(a) + lgamma(b) - lgamma(a + b) - lgamma(x) - 
          lgamma(y) + lgamma(x + y))
  }
  l.A <- log(beta.ratio(a + 1, b + x - 1, a, b)) + lgamma(r + x) - lgamma(r) + 
    r * log(alpha) - (r + x) * log(alpha + t.x)
  l.B <- log(beta.ratio(a, b + x + y, a, b)) + lgamma(r + x + y) - lgamma(r) - log(factorial(y)) +
    r * log(alpha) + y * log(T.star) - (r + x + y) * log(alpha + T.cal + T.star)
  l.num <- l.A + log(1 + exp(l.B - l.A))
  l.num[x == 0] <- l.B[x == 0]
  l.llh.1 <- log(beta.ratio(a, b + x, a, b)) + lgamma(r + x) + r * log(alpha) - 
    lgamma(r) - (r + x) * log(alpha + T.cal)
  l.llh.2 <- log(beta.ratio(a + 1, b + x - 1, a, b)) + lgamma(r + x) + r * log(alpha) -
    lgamma(r) - (r + x) * log(alpha + t.x)
  l.denum <- l.llh.1 + log(1 + exp(l.llh.2 - l.llh.1))
  l.denum[x == 0] <- l.llh.1[x == 0]
  return(1 - exp(l.num - l.denum))
}


## ----model predictions-----------------------------------------------------------
# Calculate length of our hold-out period in days
pred_days <- as.integer(max(df$date) - split_date)

# Predicting returns using our 2 models and the `return probability'
df_model$pnbd_return_prob <- pnbd.ConditionalReturnProbability(
                                    params = pnbd_params,
                                    T.star = pred_days,
                                    x = df_model$x,
                                    t.x = df_model$t.x,
                                    T.cal = df_model$T.cal)
df_model$pnbd_return_pred <- as.numeric(df_model$pnbd_return_prob > 0.5)

df_model$bgnbd_return_prob <- bgnbd.ConditionalReturnProbability(
                                    params = bgnbd_params,
                                    T.star = pred_days,
                                    x = df_model$x,
                                    t.x = df_model$t.x,
                                    T.cal = df_model$T.cal)
df_model$bgnbd_return_pred <- as.numeric(df_model$bgnbd_return_prob > 0.5)

# Predicting returns using P(alive)
df_model$pnbd_alive_prob <- pnbd.PAlive(params = pnbd_params,
                                        x = df_model$x,
                                        t.x = df_model$t.x,
                                        T.cal = df_model$T.cal)
df_model$pnbd_alive_pred <- as.numeric(df_model$pnbd_alive_prob > 0.5)

df_model$bgnbd_alive_prob <- bgnbd.PAlive(params = bgnbd_params,
                                          x = df_model$x,
                                          t.x = df_model$t.x,
                                          T.cal = df_model$T.cal)
df_model$bgnbd_alive_pred <- as.numeric(df_model$bgnbd_alive_prob > 0.5)


## ----majority--------------------------------------------------------------------
# Simple majority rule 
df_model$majority_pred <- 0


## --------------------------------------------------------------------------------
# Function for finding the best cut-off for our heuristic
find_cutoff <- function(recency, return){
  candidates <- seq(min(recency), max(recency))
  
  accuracy_cutoff <- function(cutoff, recency, return){
    prediction <- as.numeric(recency <= cutoff)
    return(sum(prediction == return) / length(return))
  }
  
  accuracies <- sapply(X = candidates, 
                      FUN = accuracy_cutoff, 
                      recency = recency, 
                      return = return)
  return(list('max' = max(candidates[which.max(accuracies)]),
              'scores' = cbind(candidates, accuracies)))
}

# Find the optimal cutoff and make predictions
optimal_cutoff <- find_cutoff(df_model$recency, df_model$returns)[[1]]
df_model$heuristic_pred <- as.numeric(df_model$recency <= optimal_cutoff)

cat('The optimal cut-off point lies at ', optimal_cutoff, ' days.', sep='')


## --------------------------------------------------------------------------------
# Function for evaluating predictions
evaluate_pred <- function(pred, actual, names = NULL){
  if (is.null(names)){
    names = c(colnames(pred))
  }
  measures <- c('Accuracy', 'TP', 'FP', 'TN', 'FN')
  output <- data.frame(matrix(NA, nrow = ncol(pred), ncol = length(measures)))
  colnames(output) <- measures
  rownames(output) <- names
  for (i in (1:ncol(pred))){
    predictions <- pred[i]
    TP <- as.numeric(sum(predictions == 1 & actual == 1))
    FP <- as.numeric(sum(predictions == 1 & actual == 0))
    TN <- as.numeric(sum(predictions == 0 & actual == 0))
    FN <- as.numeric(sum(predictions == 0 & actual == 1))
    output[i, 'Accuracy'] <- 100 * (TP + TN) / (TP + FP + TN + FN)
    output[i, c('TP', 'FP', 'TN', 'FN')] <- c(TP, FP, TN, FN)
  }
  return(output)
}

# Output
round(evaluate_pred(df_model[c('pnbd_return_pred', 'bgnbd_return_pred',
                         'pnbd_alive_pred', 'bgnbd_alive_pred',
                         'heuristic_pred', 'majority_pred')],
              df_model$returns), 2)


## --------------------------------------------------------------------------------
# Function for evaluating probabilities
evaluate_prob <- function(prob, actual, names = NULL){
  if (is.null(names)){
    names = c(colnames(prob))
  }
  actual <- as.vector(actual)
  measures <- c('AUROC', 'Log-loss')
  output <- data.frame(matrix(NA, nrow = ncol(prob), ncol = length(measures)))
  colnames(output) <- measures
  rownames(output) <- names
  for (i in (1:ncol(prob))){
    probabilities <- as.vector(prob[[i]])
    output[i, 'AUROC'] <- MLmetrics::AUC(probabilities, actual)
    output[i, 'Log-loss'] <- MLmetrics::LogLoss(probabilities, actual)
  }
  return(output)
}

# Output
round(evaluate_prob(df_model[c('pnbd_return_prob', 'bgnbd_return_prob')],
              df_model$returns), 4)


## ----convert script, echo = FALSE------------------------------------------------
knitr::purl("returning_customers.RMD")

