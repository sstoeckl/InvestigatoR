
<!-- README.md is generated from README.Rmd. Please edit that file -->

# InvestigatoR

<!-- badges: start -->
<!-- badges: end -->

The goal of InvestigatoR is to …

## Installation

You can install the development version of InvestigatoR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ericschumann12/InvestigatoR")
```

## Basic Workflow

This is a basic example which shows you how to solve a common problem:

``` r
library(InvestigatoR)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
## basic example code
```

First we load the complimentary dataset that comes with the package:

``` r
data("data_ml")
```

For a description, see…. The original datset was provided by Guillaume
Coqueret and Tony Guida with their book [Machine Learning for Factor
Investing](https://mlfactor.com).

Next we specify the set of features that should be used for return
prediction, specify some options for backtesting, such as whether the
return prediction should be done with a rolling window (TRUE), the
window size (“5 years”), the step size(“3 months”, this means, how often
do we reestimate the ML model), the offset (“1 year” to avoid any form
of data spillage).

``` r
return_label <- "R1M_Usd"
features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
rolling <- FALSE; window_size= "5 years"; step_size = "1 months"; offset = "1 month"; in_sample = TRUE
```

Next we specify the machine learning configuration. We can specify
multiple configurations, for example, one for a linear regression model
and one for a gradient boosting model. The configuration for the linear
regression model is empty, as we use the default configuration. The
configuration for the gradient boosting model specifies the number of
rounds, the maximum depth of the trees, the learning rate, and the
objective function. Other functions still need to be implemented.

``` r
 ml_config <- list(ols_pred = list(pred_func="ols_pred", config=list()),
                   xgb_pred = list(pred_func="xgb_pred", 
                                   config1=list(nrounds=10, max_depth=3, eta=0.3, objective="reg:squarederror"),
                                   config2=list(nrounds=10, max_depth=3, eta=0.1, objective="reg:squarederror")))
```

Finally, we call the backtesting function. The function returns a data
frame with the backtesting results. The data frame contains the
following columns: date, return_label, features, rolling, window_size,
step_size, offset, in_sample, ml_config, model, predicted returns,
actual realized returns, and errors for all predictions. The model
column contains the name of the model that was used for the prediction.
The prediction column contains the predicted returns. The actual column
contains the actual returns. The error column contains the difference
between the predicted and the actual returns.

``` r
 rp <- backtesting_returns(data=data_ml, return_prediction_object=NULL,
   return_label, features, rolling=FALSE, window_size, step_size, offset, in_sample, ml_config, append=FALSE, num_cores=NULL)
#> Currently processing model 1 of 2 
#> Specifically processing config 1 of 1
#> 
#> Attache Paket: 'purrr'
#> Das folgende Objekt ist maskiert 'package:base':
#> 
#>     %||%
#> Currently processing model 2 of 2 
#> Specifically processing config 1 of 2
#> Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
#> numbers without specifying argument 'seed'. There is a risk that those random
#> numbers are not statistically sound and the overall results might be invalid.
#> To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe
#> random numbers are produced via the L'Ecuyer-CMRG method. To disable this
#> check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".
#> Specifically processing config 2 of 2
#> Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
#> numbers without specifying argument 'seed'. There is a risk that those random
#> numbers are not statistically sound and the overall results might be invalid.
#> To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe
#> random numbers are produced via the L'Ecuyer-CMRG method. To disable this
#> check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".
```

Next we take this predictions and analyse thbeir statistical properties

``` r
rp$predictions |> head()
#> # A tibble: 6 × 5
#>   stock_id date        ols_1  xgb_1 xgb_2
#>      <int> <date>      <dbl>  <dbl> <dbl>
#> 1       13 2006-12-31 0.0305 0.0402 0.191
#> 2       13 2007-01-31 0.0308 0.0397 0.191
#> 3       13 2007-02-28 0.0300 0.0439 0.193
#> 4       17 2015-03-31 0.0336 0.110  0.223
#> 5       17 2015-04-30 0.0343 0.0734 0.222
#> 6       17 2015-05-31 0.0344 0.0987 0.214
rp_stats <- summary(rp)
print(rp_stats)
#>       MSE        RMSE      MAE        Hit_Ratio
#> ols_1 0.03158888 0.1777326 0.08071823 0.5352989
#> xgb_1 0.03142746 0.1772779 0.08193929 0.5529501
#> xgb_2 0.06060931 0.2461896 0.1848615  0.5525796
```

Next, we map those predictions into various portfolios (quantiles) and
analyse their performance. We specify various weight restrictions, such
as the minimum and maximum weight, the minimum and maximum cutoff
quantile, and the b parameter that adjusts the amount of investment per
leg (b=1 means, we go 100% long and short). We also specify the
predictions that should be used for the portfolio formation (e.g.,
ols_1, xgb_1, xgb_2).

``` r
pf_config <- list(predictions = c("ols_1","xgb_1","xgb_2"),
                  quantile_weight = list(pred_func="quantile_weights",
                    config1=list(quantiles = list(long = 0.20, short = 0.20),allow_short_sale = FALSE,
                      min_weight = -1,  max_weight = 1, b = 1),
                    config2=list(quantiles = list(long = 0.10, short = 0.10),allow_short_sale = FALSE,
                      min_weight = -1,  max_weight = 1, b = 1)))
```

Finally we run the portfolio formation process.

``` r
pf <- backtesting_portfolios(return_prediction_object = rp, pf_config = pf_config)
#> Currently processing weight model 1 of 1 
#> Specifically processing config 1 of 2 
#> Specifically processing config 2 of 2
```

Let us check the content of pf, and calculate some summary statistics

``` r
pf$weights |> head()
#> # A tibble: 6 × 8
#>   stock_id date       ols_1_quantile_weights_1 xgb_1_quantile_weights_1
#>      <int> <date>                        <dbl>                    <dbl>
#> 1       13 2006-12-31                  0.00418                  0.00418
#> 2       13 2007-01-31                  0.00418                  0.00418
#> 3       13 2007-02-28                  0.00418                  0.00418
#> 4       17 2015-03-31                  0.00418                  0.00418
#> 5       17 2015-04-30                  0.00418                  0.00418
#> 6       17 2015-05-31                  0.00418                  0.00418
#> # ℹ 4 more variables: xgb_2_quantile_weights_1 <dbl>,
#> #   ols_1_quantile_weights_2 <dbl>, xgb_1_quantile_weights_2 <dbl>,
#> #   xgb_2_quantile_weights_2 <dbl>
pf$portfolio_returns |> head()
#> # A tibble: 6 × 7
#>   date       ols_1_quantile_weights_1 xgb_1_quantile_weights_1
#>   <date>                        <dbl>                    <dbl>
#> 1 2006-12-31                0.0184                     0.0171 
#> 2 2007-01-31                0.0189                     0.0177 
#> 3 2007-02-28                0.0160                     0.0166 
#> 4 2015-03-31               -0.0000879                 -0.00412
#> 5 2015-04-30                0.00680                    0.00849
#> 6 2015-05-31               -0.00671                   -0.0102 
#> # ℹ 4 more variables: xgb_2_quantile_weights_1 <dbl>,
#> #   ols_1_quantile_weights_2 <dbl>, xgb_1_quantile_weights_2 <dbl>,
#> #   xgb_2_quantile_weights_2 <dbl>
pf_stats <- summary(pf)
print(pf_stats)
#> # A tibble: 6 × 6
#>   portfolio                  mean     sd    SR   VaR_5 turnover
#>   <chr>                     <dbl>  <dbl> <dbl>   <dbl>    <dbl>
#> 1 ols_1_quantile_weights_1 0.0280 0.0773 0.362 -0.0905     93.7
#> 2 ols_1_quantile_weights_2 0.0352 0.0904 0.389 -0.0995    120. 
#> 3 xgb_1_quantile_weights_1 0.0287 0.0753 0.382 -0.0915    110. 
#> 4 xgb_1_quantile_weights_2 0.0385 0.0937 0.411 -0.105     135. 
#> 5 xgb_2_quantile_weights_1 0.0282 0.0733 0.385 -0.0855     90.1
#> 6 xgb_2_quantile_weights_2 0.0383 0.0904 0.424 -0.0916    118.
```

Now we plot the corrsponding cumulative returns of the portfolios

``` r
plot(pf)
```

<img src="man/figures/README-plot_pf-1.png" width="100%" />
