# Predicting which customers will return

> ## Tl;dr
> - Used the [Pareto/NBD](https://doi.org/10.1287/mnsc.33.1.1) and [BG/NBD](https://doi.org/10.1287/mksc.1040.0098) model to predict which customers will return over the next half year on the [CDNOW dataset](https://www.brucehardie.com/datasets/).
> - Models outperformed a heuristic basline based on when the customer last made a purchase.
> - Differences between models were small, with Pareto/NBD performing slightly better on AUROC and log-loss
> - Popular P(alive) statistic is not approporiate for this task. We provide an alternative 'return probability' in code.
> - Software/packages used: `R`, [`dplyr`](https://cran.r-project.org/web/packages/dplyr/index.html), [`BTYD`](https://CRAN.R-project.org/package=BTYD), [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/index.html)
> - See [`returning_customers.md`](returning_customers.md) for code and a detailed discussion.

## Motivation
Predicting a customer's purchase behaviour is a central subject within data science for (e-)commerce. In this project we look at one specific prediction, which is that of figuring out which of our customers will return over the next period. Here, accurate predictions could be of great value, for example in guiding marketing efforts. For instance we might want to target campaigns that promote activity to those customers who we expect to not return in the near future. On the other hand we might want to 'reward' those who will. 

## Data
We use the [CDNOW dataset](https://www.brucehardie.com/datasets/), which consists of transaction records for all ~70k purchases made by ~23k customers for a CD retailer. The data spans the period ranging from 1997-01-01 until 1998-06-30, of which we use the first year to train and the last half-year as a hold-out period.

## Models
In modelling purchase behaviour, the so-called 'buy-till-you-die' models are often the go-to option. We test two of these models, namely the [Pareto/NBD](https://doi.org/10.1287/mnsc.33.1.1) and [BG/NBD](https://doi.org/10.1287/mksc.1040.0098) model. To do this we use the [`BTYD`](https://CRAN.R-project.org/package=BTYD) package in `R`. Initially, many might be tempted to use the popular P(alive) result of these models to make our predictions, as it appears to be the logical option at the surface. Yet one should note that not each customer that is 'alive' will actually make a purchase in the next period. We provide a better but less-known alternative by supplying code for a `return probability' that *directly* relates to our event of a customer making at least one purchase over the next period.

We also make sure to test our models against an intuitive baseline. For these problems, managers often use a rule along the lines of "customers who have not made a purchase in the last *x* days, are not expected to return over the next period. In the absence of managers to retrieve this rule from, we 'cheat' and optimize such a rule based on the hold-out data directly. This makes it even more challenging for our models to beat this rule!

## Results
Our models beat the baseline when looking at the accuracy. Also, our return probability outperforms the P(alive) statistic convincingly. See the results below.
 
|    |  Accuracy | TP |  FP | TN | FN | 
|:-------|-------:|-------:|-------:|-------:|-------:|
|Pareto/NBD (Return prob.) | 81.94|2449|1332|16864|2925|
|BG/NBD (Return prob.)|82.06|2055|910|17286|3319|
|Pareto/NBD (P(alive)) 	|78.05	|3570	|3369	|14827	|1804|
|BG/NBD (P(alive))	|31.43	|4460	|15249	|2947	|914|
|Heuristic	|80.97	|2428	|1540	|16656	|2946|
|Majority rule	|77.20	|0	|0	|18196	|5374|

Differences between the Pareto/NBD and BG/NBD model seem minimal. Let's look at the AUROC and log-loss.

|   | AUROC | Log-loss |
|:-------|-------:|-------:|
|Pareto/NBD (Return prob.)|  0.7998 | 0.4208|
|BG/NBD (Return prob.) |0.7882 |  0.4244|

Again, differences are small. Yet, the Pareto/NBD might perform marginally better for our data set.

## Files in this repository
- [`returning_customers.md`](returning_customers.md): Detailed discussion of this project including code, data analysis etc..
- [`returning_customers.Rmd`](eturning_customers.Rmd): Used to generate [`returning_customers.md`](returning_customers.md).
- [`returning_customers.R`](returning_customers.R): Raw code.
- `data`: Contains the data used in this project.
- `README_figs`: Contains figures used in [`returning_customers.md`](returning_customers.md).


## References
[1] [Fader, Peter S. and Bruce G.,S. Hardie, (2001), "Forecasting Repeat Sales at CDNOW: A Case Study," Interfaces, 31 (May-June), Part 2 of 2, S94-S107.](https://doi.org/10.1287/mnsc.33.1.1)   
[2] [Fader, P. S., Hardie, B. G., & Lee, K. L. (2005). “Counting your customers” the easy way: An alternative to the Pareto/NBD model. Marketing science, 24(2), 275-284.](https://doi.org/10.1287/mksc.1040.0098)  
[3] https://www.brucehardie.com/datasets/  
[4] https://cran.r-project.org/web/packages/dplyr/index.html  
[5] https://CRAN.R-project.org/package=BTYD  
[6] https://cran.r-project.org/web/packages/ggplot2/index.html
