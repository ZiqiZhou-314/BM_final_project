Qimin’s model building
================
Qimin Zhang
12/7/2019

``` r
data = read_csv("data/Lawsuit.csv") %>% 
  janitor::clean_names() %>% 
  dplyr::select(-id) %>% 
  mutate(dept = as.factor(dept),
         gender = as.factor(gender),
         clin = as.factor(clin),
         cert = as.factor(cert),
         rank = as.factor(rank))
```

Look at the distribution of all variables.

``` r
data %>%
  mutate(sal_average = (sal94 + sal95)/2) %>% 
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_density() +
    labs(
    title = "Density plot of all numeric variables"
  )
```

![](qimin_draft_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

We can see that they are all not normal. Let’s try log transformation.

``` r
data %>%
  mutate(sal_average = (sal94 + sal95)/2) %>%
  mutate(
    log_exper = log(exper),
    log_prate = log(prate),
    log_sal_average = log(sal_average),
    log_sal94 = log(sal94),
    log_sal95 = log(sal95)
  ) %>% 
  dplyr::select(-exper, -prate, -sal_average, -sal94, -sal95) %>% 
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_density() +
    labs(
    title = "Density plot of all log numeric variables"
  )
```

![](qimin_draft_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The salaries and exper seem to be normal, while prate is still not
normal.

``` r
lm(log(sal_average) ~ . , data = data %>% mutate(sal_average = (sal94 + sal95)/2) %>% dplyr::select(-sal94, -sal95)) %>% 
  step(direction='backward') %>% 
  summary()
```

    ## Start:  AIC=-1038.3
    ## log(sal_average) ~ dept + gender + clin + cert + prate + exper + 
    ##     rank
    ## 
    ##          Df Sum of Sq     RSS      AIC
    ## - gender  1    0.0166  4.4393 -1039.32
    ## - prate   1    0.0279  4.4506 -1038.66
    ## <none>                 4.4226 -1038.30
    ## - clin    1    0.2858  4.7085 -1023.96
    ## - rank    2    1.2646  5.6872  -976.66
    ## - cert    1    1.4402  5.8628  -966.73
    ## - exper   1    1.7384  6.1610  -953.78
    ## - dept    5    9.1712 13.5938  -755.23
    ## 
    ## Step:  AIC=-1039.32
    ## log(sal_average) ~ dept + clin + cert + prate + exper + rank
    ## 
    ##         Df Sum of Sq     RSS      AIC
    ## <none>                4.4393 -1039.32
    ## - prate  1    0.0421  4.4814 -1038.86
    ## - clin   1    0.2695  4.7088 -1025.94
    ## - rank   2    1.3973  5.8366  -971.90
    ## - cert   1    1.4549  5.8942  -967.33
    ## - exper  1    1.8444  6.2837  -950.63
    ## - dept   5    9.3583 13.7976  -753.35

    ## 
    ## Call:
    ## lm(formula = log(sal_average) ~ dept + clin + cert + prate + 
    ##     exper + rank, data = data %>% mutate(sal_average = (sal94 + 
    ##     sal95)/2) %>% dplyr::select(-sal94, -sal95))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.34228 -0.08136 -0.01292  0.08004  0.90901 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11.176162   0.129693  86.174  < 2e-16 ***
    ## dept2       -0.176674   0.029061  -6.079 4.51e-09 ***
    ## dept3        0.165536   0.038580   4.291 2.55e-05 ***
    ## dept4        0.141486   0.052228   2.709  0.00722 ** 
    ## dept5        0.490294   0.044850  10.932  < 2e-16 ***
    ## dept6        0.865105   0.061350  14.101  < 2e-16 ***
    ## clin1        0.156395   0.040226   3.888  0.00013 ***
    ## cert1        0.191677   0.021218   9.034  < 2e-16 ***
    ## prate       -0.025859   0.016831  -1.536  0.12571    
    ## exper        0.018316   0.001801  10.171  < 2e-16 ***
    ## rank2        0.136661   0.023193   5.892 1.23e-08 ***
    ## rank3        0.224767   0.025719   8.739 3.53e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1335 on 249 degrees of freedom
    ## Multiple R-squared:  0.9341, Adjusted R-squared:  0.9311 
    ## F-statistic: 320.6 on 11 and 249 DF,  p-value: < 2.2e-16

``` r
lm(log(sal_average) ~ dept + clin + cert + exper + rank + gender + gender*dept + gender*clin + gender*cert + gender*exper + gender*rank, data = data %>% mutate(sal_average = (sal94 + sal95)/2) %>% dplyr::select(-sal94, -sal95)) %>% 
  summary()
```

    ## 
    ## Call:
    ## lm(formula = log(sal_average) ~ dept + clin + cert + exper + 
    ##     rank + gender + gender * dept + gender * clin + gender * 
    ##     cert + gender * exper + gender * rank, data = data %>% mutate(sal_average = (sal94 + 
    ##     sal95)/2) %>% dplyr::select(-sal94, -sal95))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.32722 -0.07580 -0.00848  0.07071  0.83916 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   10.910077   0.047435 230.000  < 2e-16 ***
    ## dept2         -0.200083   0.044177  -4.529 9.35e-06 ***
    ## dept3          0.149171   0.051446   2.900 0.004085 ** 
    ## dept4          0.202252   0.051497   3.927 0.000112 ***
    ## dept5          0.537308   0.047515  11.308  < 2e-16 ***
    ## dept6          0.922282   0.075863  12.157  < 2e-16 ***
    ## clin1          0.237856   0.032668   7.281 4.76e-12 ***
    ## cert1          0.169028   0.034541   4.894 1.82e-06 ***
    ## exper          0.027516   0.004671   5.891 1.29e-08 ***
    ## rank2          0.141944   0.037446   3.791 0.000190 ***
    ## rank3          0.206462   0.052104   3.962 9.80e-05 ***
    ## gender1        0.129289   0.060919   2.122 0.034841 *  
    ## dept2:gender1  0.066863   0.058967   1.134 0.257971    
    ## dept3:gender1  0.093732   0.073790   1.270 0.205233    
    ## dept4:gender1  0.035246   0.074211   0.475 0.635259    
    ## dept5:gender1  0.031015   0.060957   0.509 0.611364    
    ## dept6:gender1  0.047254   0.087272   0.541 0.588698    
    ## clin1:gender1 -0.071685   0.045751  -1.567 0.118469    
    ## cert1:gender1  0.021934   0.045048   0.487 0.626765    
    ## exper:gender1 -0.010832   0.005076  -2.134 0.033875 *  
    ## rank2:gender1 -0.048680   0.049397  -0.985 0.325388    
    ## rank3:gender1 -0.020744   0.062122  -0.334 0.738730    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1321 on 239 degrees of freedom
    ## Multiple R-squared:  0.9381, Adjusted R-squared:  0.9326 
    ## F-statistic: 172.4 on 21 and 239 DF,  p-value: < 2.2e-16

``` r
lm(log(sal_average) ~ dept + clin + cert + exper + rank + gender + gender*exper, data = data %>% mutate(sal_average = (sal94 + sal95)/2) %>% dplyr::select(-sal94, -sal95)) %>% 
  summary()
```

    ## 
    ## Call:
    ## lm(formula = log(sal_average) ~ dept + clin + cert + exper + 
    ##     rank + gender + gender * exper, data = data %>% mutate(sal_average = (sal94 + 
    ##     sal95)/2) %>% dplyr::select(-sal94, -sal95))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.32130 -0.07860 -0.00987  0.07100  0.86910 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   10.903325   0.034805 313.272  < 2e-16 ***
    ## dept2         -0.165069   0.028755  -5.741 2.75e-08 ***
    ## dept3          0.189770   0.035827   5.297 2.60e-07 ***
    ## dept4          0.218603   0.035342   6.185 2.54e-09 ***
    ## dept5          0.546771   0.029045  18.825  < 2e-16 ***
    ## dept6          0.939830   0.034907  26.924  < 2e-16 ***
    ## clin1          0.208175   0.021470   9.696  < 2e-16 ***
    ## cert1          0.182166   0.020969   8.688 5.09e-16 ***
    ## exper          0.027774   0.003545   7.834 1.38e-13 ***
    ## rank2          0.118231   0.023648   5.000 1.09e-06 ***
    ## rank3          0.208036   0.026112   7.967 5.90e-14 ***
    ## gender1        0.128932   0.036912   3.493 0.000566 ***
    ## exper:gender1 -0.011728   0.003580  -3.276 0.001204 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1312 on 248 degrees of freedom
    ## Multiple R-squared:  0.9366, Adjusted R-squared:  0.9336 
    ## F-statistic: 305.4 on 12 and 248 DF,  p-value: < 2.2e-16

``` r
lm(log(sal_average) ~ dept + clin + cert + exper + rank + gender + gender*exper, data = data %>% mutate(sal_average = (sal94 + sal95)/2) %>% dplyr::select(-sal94, -sal95)) %>% 
  summary()
```

    ## 
    ## Call:
    ## lm(formula = log(sal_average) ~ dept + clin + cert + exper + 
    ##     rank + gender + gender * exper, data = data %>% mutate(sal_average = (sal94 + 
    ##     sal95)/2) %>% dplyr::select(-sal94, -sal95))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.32130 -0.07860 -0.00987  0.07100  0.86910 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   10.903325   0.034805 313.272  < 2e-16 ***
    ## dept2         -0.165069   0.028755  -5.741 2.75e-08 ***
    ## dept3          0.189770   0.035827   5.297 2.60e-07 ***
    ## dept4          0.218603   0.035342   6.185 2.54e-09 ***
    ## dept5          0.546771   0.029045  18.825  < 2e-16 ***
    ## dept6          0.939830   0.034907  26.924  < 2e-16 ***
    ## clin1          0.208175   0.021470   9.696  < 2e-16 ***
    ## cert1          0.182166   0.020969   8.688 5.09e-16 ***
    ## exper          0.027774   0.003545   7.834 1.38e-13 ***
    ## rank2          0.118231   0.023648   5.000 1.09e-06 ***
    ## rank3          0.208036   0.026112   7.967 5.90e-14 ***
    ## gender1        0.128932   0.036912   3.493 0.000566 ***
    ## exper:gender1 -0.011728   0.003580  -3.276 0.001204 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1312 on 248 degrees of freedom
    ## Multiple R-squared:  0.9366, Adjusted R-squared:  0.9336 
    ## F-statistic: 305.4 on 12 and 248 DF,  p-value: < 2.2e-16
