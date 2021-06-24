Gallup Relative Status Health Outcome Regression Analysis
================
Daniel O’Leary
1/11/2021

  - [Analysis](#analysis)
      - [Health outcomes](#health-outcomes)
          - [BMI](#bmi)
          - [Diabetes](#diabetes)
          - [High blood pressure](#high-blood-pressure)
          - [Depression](#depression)
          - [Obesity](#obesity)
          - [Self-reported health](#self-reported-health)
          - [Height](#height)
      - [Regression tables](#regression-tables)
          - [Demographic reference](#demographic-reference)
          - [Geographic reference](#geographic-reference)

``` r
contrasts(dfg_rs$sex) <- contr.sum(2)
contrasts(dfg_rs$employment_all) <- contr.sum(2)
contrasts(dfg_rs$race) <- contr.sum(5)
contrasts(dfg_rs$married) <- contr.sum(6)
```

``` r
# c <- contr.treatment(2)
# c
# my.coding <- matrix(rep(1/2, 2), ncol = 1)
# my.simple = c - my.coding
# my.simple
# 
# contrasts(dfg_rs$sex) <- my.simple
# contrasts(dfg_rs$employment_all) <- my.simple
# 
# 
# c <- contr.treatment(5)
# my.coding <- matrix(rep(1/5, 20), ncol = 4)
# my.simple = c - my.coding
# my.simple
# 
# contrasts(dfg_rs$race) <- my.simple
# 
# 
# c <- contr.treatment(6)
# my.coding <- matrix(rep(1/6, 30), ncol = 5)
# my.simple = c - my.coding
# my.simple
# 
# contrasts(dfg_rs$married) <- my.simple
```

# Analysis

## Health outcomes

### BMI

``` r
lm_bmi_geo <-
  lm(
    bmi_scale ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    data = dfg_rs 
  )

summary(lm_bmi_geo)
```

    ## 
    ## Call:
    ## lm(formula = bmi_scale ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6663 -0.6554 -0.1620  0.4666 21.5873 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     1.881e-02  2.716e-03   6.925 4.37e-12 ***
    ## raw_income_scale               -5.521e-02  8.777e-04 -62.906  < 2e-16 ***
    ## median_income_county_scale      1.020e-02  1.033e-03   9.874  < 2e-16 ***
    ## physicians_scale               -2.613e-02  7.994e-04 -32.685  < 2e-16 ***
    ## unweighted_pop_county_scale    -5.797e-03  8.508e-04  -6.814 9.53e-12 ***
    ## median_home_value_county_scale -6.362e-02  1.128e-03 -56.391  < 2e-16 ***
    ## land_area_2010_scale           -7.224e-03  7.361e-04  -9.814  < 2e-16 ***
    ## education_scale                -5.838e-02  8.013e-04 -72.852  < 2e-16 ***
    ## employment_all1                 1.765e-06  8.363e-04   0.002  0.99832    
    ## sex1                            9.727e-02  7.240e-04 134.354  < 2e-16 ***
    ## age_scale                       4.384e-02  9.417e-04  46.553  < 2e-16 ***
    ## race1                          -3.250e-02  1.733e-03 -18.759  < 2e-16 ***
    ## race2                           6.950e-02  3.902e-03  17.811  < 2e-16 ***
    ## race3                           2.690e-01  2.479e-03 108.489  < 2e-16 ***
    ## race4                          -3.551e-01  4.205e-03 -84.437  < 2e-16 ***
    ## married1                       -6.912e-02  1.943e-03 -35.581  < 2e-16 ***
    ## married2                        6.328e-02  1.518e-03  41.680  < 2e-16 ***
    ## married3                        8.619e-02  4.351e-03  19.811  < 2e-16 ***
    ## married4                        5.232e-02  2.070e-03  25.272  < 2e-16 ***
    ## married5                       -1.092e-01  2.421e-03 -45.118  < 2e-16 ***
    ## year2009                        1.250e-02  2.757e-03   4.535 5.76e-06 ***
    ## year2010                        1.514e-02  2.761e-03   5.481 4.22e-08 ***
    ## year2011                       -7.969e-03  2.743e-03  -2.905  0.00367 ** 
    ## year2012                       -2.304e-03  2.712e-03  -0.850  0.39552    
    ## year2013                        7.147e-03  3.303e-03   2.164  0.03048 *  
    ## year2014                        2.698e-02  3.328e-03   8.108 5.16e-16 ***
    ## year2015                        5.064e-02  3.316e-03  15.270  < 2e-16 ***
    ## year2016                        5.150e-02  3.291e-03  15.649  < 2e-16 ***
    ## year2017                        5.510e-02  3.402e-03  16.195  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9893 on 2006480 degrees of freedom
    ##   (1523761 observations deleted due to missingness)
    ## Multiple R-squared:  0.03919,    Adjusted R-squared:  0.03918 
    ## F-statistic:  2923 on 28 and 2006480 DF,  p-value: < 2.2e-16

``` r
lm_bmi_demo <-
  lm(
    bmi_scale ~
      income_scale +
      income_demo_ranger_sar_vars_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    data = dfg_rs 
  )

summary(lm_bmi_demo)
```

    ## 
    ## Call:
    ## lm(formula = bmi_scale ~ income_scale + income_demo_ranger_sar_vars_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8049 -0.6468 -0.1556  0.4645 21.5835 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)                        0.2369198  0.0028938   81.871  < 2e-16 ***
    ## income_scale                      -0.0600559  0.0008769  -68.487  < 2e-16 ***
    ## income_demo_ranger_sar_vars_scale  0.3050962  0.0015093  202.138  < 2e-16 ***
    ## physicians_scale                  -0.0236927  0.0007905  -29.971  < 2e-16 ***
    ## unweighted_pop_county_scale       -0.0071299  0.0008280   -8.611  < 2e-16 ***
    ## median_home_value_county_scale    -0.0564010  0.0008482  -66.496  < 2e-16 ***
    ## land_area_2010_scale              -0.0066891  0.0007280   -9.188  < 2e-16 ***
    ## education_scale                   -0.0680633  0.0007966  -85.439  < 2e-16 ***
    ## employment_all1                    0.0261333  0.0008435   30.983  < 2e-16 ***
    ## sex1                              -0.0701832  0.0010938  -64.165  < 2e-16 ***
    ## age_scale                          0.0969395  0.0009674  100.209  < 2e-16 ***
    ## race1                             -0.3284804  0.0022595 -145.377  < 2e-16 ***
    ## race2                              0.1643935  0.0038942   42.215  < 2e-16 ***
    ## race3                              0.3764825  0.0025128  149.829  < 2e-16 ***
    ## race4                             -0.5038444  0.0042292 -119.136  < 2e-16 ***
    ## married1                          -0.0102773  0.0019458   -5.282 1.28e-07 ***
    ## married2                           0.0184790  0.0015259   12.110  < 2e-16 ***
    ## married3                           0.0155915  0.0043236    3.606 0.000311 ***
    ## married4                          -0.0051254  0.0020702   -2.476 0.013295 *  
    ## married5                           0.0101865  0.0024681    4.127 3.67e-05 ***
    ## year2009                           0.0079002  0.0027300    2.894 0.003806 ** 
    ## year2010                           0.0183512  0.0027343    6.711 1.93e-11 ***
    ## year2011                           0.0080155  0.0027150    2.952 0.003154 ** 
    ## year2012                           0.0160084  0.0026812    5.971 2.37e-09 ***
    ## year2013                           0.0268266  0.0032658    8.214  < 2e-16 ***
    ## year2014                           0.0480121  0.0032873   14.605  < 2e-16 ***
    ## year2015                           0.0732908  0.0032767   22.368  < 2e-16 ***
    ## year2016                           0.0766554  0.0032450   23.623  < 2e-16 ***
    ## year2017                           0.0822996  0.0033445   24.608  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9793 on 2004987 degrees of freedom
    ##   (1525254 observations deleted due to missingness)
    ## Multiple R-squared:  0.05823,    Adjusted R-squared:  0.05822 
    ## F-statistic:  4428 on 28 and 2004987 DF,  p-value: < 2.2e-16

### Diabetes

``` r
lm_diab_geo <-
  glm(
    diabetes ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs 
  )

summary(lm_diab_geo)
```

    ## 
    ## Call:
    ## glm(formula = diabetes ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5683  -0.5577  -0.3961  -0.2848   3.2065  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)                    -1.8521412  0.0088297 -209.764  < 2e-16 ***
    ## raw_income_scale               -0.1724056  0.0029427  -58.588  < 2e-16 ***
    ## median_income_county_scale     -0.0090878  0.0034807   -2.611 0.009029 ** 
    ## physicians_scale               -0.0485484  0.0026268  -18.482  < 2e-16 ***
    ## unweighted_pop_county_scale     0.0074699  0.0028538    2.618 0.008856 ** 
    ## median_home_value_county_scale -0.0976523  0.0039510  -24.716  < 2e-16 ***
    ## land_area_2010_scale           -0.0270715  0.0024943  -10.853  < 2e-16 ***
    ## education_scale                -0.1186522  0.0024374  -48.679  < 2e-16 ***
    ## employment_all1                 0.2192279  0.0026246   83.528  < 2e-16 ***
    ## sex1                            0.1178431  0.0023025   51.180  < 2e-16 ***
    ## age_scale                       0.6582472  0.0031767  207.211  < 2e-16 ***
    ## race1                          -0.3682977  0.0058055  -63.440  < 2e-16 ***
    ## race2                           0.0839919  0.0119277    7.042 1.90e-12 ***
    ## race3                           0.2442004  0.0075894   32.177  < 2e-16 ***
    ## race4                          -0.0280962  0.0158254   -1.775 0.075835 .  
    ## married1                       -0.0746986  0.0065576  -11.391  < 2e-16 ***
    ## married2                        0.0254729  0.0047799    5.329 9.86e-08 ***
    ## married3                        0.2504305  0.0124060   20.186  < 2e-16 ***
    ## married4                        0.0945289  0.0060932   15.514  < 2e-16 ***
    ## married5                       -0.1765915  0.0066154  -26.694  < 2e-16 ***
    ## year2009                       -0.0300430  0.0088497   -3.395 0.000687 ***
    ## year2010                        0.0477351  0.0087650    5.446 5.15e-08 ***
    ## year2011                       -0.0005204  0.0087950   -0.059 0.952814    
    ## year2012                        0.0317652  0.0086688    3.664 0.000248 ***
    ## year2013                        0.0415014  0.0105446    3.936 8.29e-05 ***
    ## year2014                        0.0525743  0.0104629    5.025 5.04e-07 ***
    ## year2015                        0.0663087  0.0104136    6.368 1.92e-10 ***
    ## year2016                        0.0807603  0.0104069    7.760 8.47e-15 ***
    ## year2017                        0.0795982  0.0108910    7.309 2.70e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1540774  on 2057348  degrees of freedom
    ## Residual deviance: 1402159  on 2057320  degrees of freedom
    ##   (1472921 observations deleted due to missingness)
    ## AIC: 1402217
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
lm_diab_demo <-
  glm(
    diabetes ~
      income_scale +
      income_demo_ranger_sar_vars_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs 
  )

summary(lm_diab_demo)
```

    ## 
    ## Call:
    ## glm(formula = diabetes ~ income_scale + income_demo_ranger_sar_vars_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6788  -0.5770  -0.4024  -0.2391   3.5581  
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)                       -1.436095   0.009340 -153.765  < 2e-16 ***
    ## income_scale                      -0.184466   0.002850  -64.719  < 2e-16 ***
    ## income_demo_ranger_sar_vars_scale  0.684169   0.005317  128.682  < 2e-16 ***
    ## physicians_scale                  -0.043888   0.002624  -16.726  < 2e-16 ***
    ## unweighted_pop_county_scale        0.010573   0.002822    3.747 0.000179 ***
    ## median_home_value_county_scale    -0.102568   0.002980  -34.420  < 2e-16 ***
    ## land_area_2010_scale              -0.025277   0.002486  -10.167  < 2e-16 ***
    ## education_scale                   -0.132015   0.002457  -53.723  < 2e-16 ***
    ## employment_all1                    0.242641   0.002621   92.573  < 2e-16 ***
    ## sex1                              -0.295137   0.003910  -75.484  < 2e-16 ***
    ## age_scale                          0.958818   0.004254  225.368  < 2e-16 ***
    ## race1                             -1.029664   0.007803 -131.949  < 2e-16 ***
    ## race2                              0.286195   0.012205   23.449  < 2e-16 ***
    ## race3                              0.492984   0.007970   61.852  < 2e-16 ***
    ## race4                             -0.388120   0.016328  -23.771  < 2e-16 ***
    ## married1                           0.002798   0.006603    0.424 0.671701    
    ## married2                          -0.033110   0.004853   -6.823 8.90e-12 ***
    ## married3                           0.142632   0.012555   11.360  < 2e-16 ***
    ## married4                          -0.010334   0.006146   -1.681 0.092680 .  
    ## married5                           0.005479   0.006694    0.818 0.413112    
    ## year2009                          -0.035993   0.008869   -4.058 4.94e-05 ***
    ## year2010                           0.049434   0.008784    5.628 1.82e-08 ***
    ## year2011                           0.017856   0.008812    2.026 0.042733 *  
    ## year2012                           0.050140   0.008677    5.778 7.54e-09 ***
    ## year2013                           0.059125   0.010564    5.597 2.18e-08 ***
    ## year2014                           0.070312   0.010471    6.715 1.88e-11 ***
    ## year2015                           0.084913   0.010419    8.150 3.64e-16 ***
    ## year2016                           0.102020   0.010392    9.817  < 2e-16 ***
    ## year2017                           0.099815   0.010856    9.195  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1539573  on 2055691  degrees of freedom
    ## Residual deviance: 1382992  on 2055663  degrees of freedom
    ##   (1474578 observations deleted due to missingness)
    ## AIC: 1383050
    ## 
    ## Number of Fisher Scoring iterations: 6

### High blood pressure

``` r
lm_hbp_geo <-
  glm(
    hbp ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs
  )

summary(lm_hbp_geo)
```

    ## 
    ## Call:
    ## glm(formula = hbp ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3380  -0.8582  -0.5429   1.0478   2.8334  
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                    -0.552008   0.006437 -85.761  < 2e-16 ***
    ## raw_income_scale               -0.089050   0.002056 -43.309  < 2e-16 ***
    ## median_income_county_scale     -0.015169   0.002443  -6.208 5.35e-10 ***
    ## physicians_scale               -0.036110   0.001867 -19.343  < 2e-16 ***
    ## unweighted_pop_county_scale    -0.005151   0.002047  -2.516   0.0119 *  
    ## median_home_value_county_scale -0.073499   0.002719 -27.036  < 2e-16 ***
    ## land_area_2010_scale           -0.023356   0.001769 -13.201  < 2e-16 ***
    ## education_scale                -0.116412   0.001823 -63.868  < 2e-16 ***
    ## employment_all1                 0.131208   0.001867  70.271  < 2e-16 ***
    ## sex1                            0.116836   0.001676  69.716  < 2e-16 ***
    ## age_scale                       0.899923   0.002348 383.321  < 2e-16 ***
    ## race1                          -0.123597   0.004330 -28.547  < 2e-16 ***
    ## race2                          -0.001542   0.009293  -0.166   0.8682    
    ## race3                           0.511171   0.005856  87.293  < 2e-16 ***
    ## race4                          -0.167858   0.011520 -14.571  < 2e-16 ***
    ## married1                       -0.055290   0.004669 -11.842  < 2e-16 ***
    ## married2                       -0.048014   0.003481 -13.793  < 2e-16 ***
    ## married3                        0.160097   0.009656  16.580  < 2e-16 ***
    ## married4                        0.081271   0.004551  17.858  < 2e-16 ***
    ## married5                       -0.080651   0.005241 -15.388  < 2e-16 ***
    ## year2009                       -0.001080   0.006327  -0.171   0.8644    
    ## year2010                        0.025131   0.006324   3.974 7.07e-05 ***
    ## year2011                       -0.001588   0.006315  -0.251   0.8014    
    ## year2012                       -0.014917   0.006247  -2.388   0.0170 *  
    ## year2013                       -0.011400   0.007643  -1.492   0.1358    
    ## year2014                       -0.036799   0.007674  -4.795 1.62e-06 ***
    ## year2015                       -0.046528   0.007659  -6.075 1.24e-09 ***
    ## year2016                       -0.063468   0.007640  -8.307  < 2e-16 ***
    ## year2017                       -0.066063   0.007959  -8.300  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2649884  on 2056707  degrees of freedom
    ## Residual deviance: 2290188  on 2056679  degrees of freedom
    ##   (1473562 observations deleted due to missingness)
    ## AIC: 2290246
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
lm_hbp_demo <-
  glm(
    hbp ~
      income_scale +
      income_demo_ranger_sar_vars_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs
  )

summary(lm_hbp_demo)
```

    ## 
    ## Call:
    ## glm(formula = hbp ~ income_scale + income_demo_ranger_sar_vars_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3507  -0.8835  -0.5212   1.0586   2.9950  
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                       -0.3241326  0.0068589 -47.257  < 2e-16 ***
    ## income_scale                      -0.1016313  0.0020672 -49.164  < 2e-16 ***
    ## income_demo_ranger_sar_vars_scale  0.3536685  0.0037346  94.701  < 2e-16 ***
    ## physicians_scale                  -0.0345622  0.0018643 -18.539  < 2e-16 ***
    ## unweighted_pop_county_scale       -0.0023897  0.0020181  -1.184  0.23635    
    ## median_home_value_county_scale    -0.0834507  0.0020471 -40.765  < 2e-16 ***
    ## land_area_2010_scale              -0.0217967  0.0017619 -12.371  < 2e-16 ***
    ## education_scale                   -0.1227059  0.0018301 -67.049  < 2e-16 ***
    ## employment_all1                    0.1517026  0.0018942  80.090  < 2e-16 ***
    ## sex1                              -0.0843180  0.0027109 -31.103  < 2e-16 ***
    ## age_scale                          1.0136881  0.0027101 374.037  < 2e-16 ***
    ## race1                             -0.4663295  0.0056708 -82.233  < 2e-16 ***
    ## race2                              0.1066063  0.0094642  11.264  < 2e-16 ***
    ## race3                              0.6522460  0.0061281 106.436  < 2e-16 ***
    ## race4                             -0.3613162  0.0118450 -30.504  < 2e-16 ***
    ## married1                          -0.0114815  0.0047369  -2.424  0.01536 *  
    ## married2                          -0.0823933  0.0035315 -23.331  < 2e-16 ***
    ## married3                           0.0982545  0.0097467  10.081  < 2e-16 ***
    ## married4                           0.0249926  0.0045856   5.450 5.03e-08 ***
    ## married5                           0.0296319  0.0053290   5.561 2.69e-08 ***
    ## year2009                          -0.0056462  0.0063283  -0.892  0.37228    
    ## year2010                           0.0264024  0.0063242   4.175 2.98e-05 ***
    ## year2011                           0.0085648  0.0063145   1.356  0.17498    
    ## year2012                          -0.0039777  0.0062396  -0.637  0.52380    
    ## year2013                          -0.0005577  0.0076424  -0.073  0.94183    
    ## year2014                          -0.0255854  0.0076618  -3.339  0.00084 ***
    ## year2015                          -0.0338017  0.0076451  -4.421 9.81e-06 ***
    ## year2016                          -0.0499636  0.0076107  -6.565 5.21e-11 ***
    ## year2017                          -0.0528306  0.0079123  -6.677 2.44e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2647910  on 2055057  degrees of freedom
    ## Residual deviance: 2279166  on 2055029  degrees of freedom
    ##   (1475212 observations deleted due to missingness)
    ## AIC: 2279224
    ## 
    ## Number of Fisher Scoring iterations: 4

### Depression

``` r
lm_dep_geo <-
  glm(
    depression ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs 
  )

summary(lm_dep_geo)
```

    ## 
    ## Call:
    ## glm(formula = depression ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5151  -0.6524  -0.5052  -0.3618   2.7979  
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)                    -1.746650   0.007737 -225.748  < 2e-16 ***
    ## raw_income_scale               -0.310821   0.002541 -122.326  < 2e-16 ***
    ## median_income_county_scale     -0.056695   0.002925  -19.385  < 2e-16 ***
    ## physicians_scale                0.024271   0.002177   11.147  < 2e-16 ***
    ## unweighted_pop_county_scale    -0.016744   0.002397   -6.984 2.86e-12 ***
    ## median_home_value_county_scale  0.029714   0.003150    9.434  < 2e-16 ***
    ## land_area_2010_scale           -0.008148   0.002056   -3.963 7.41e-05 ***
    ## education_scale                -0.044300   0.002152  -20.583  < 2e-16 ***
    ## employment_all1                 0.319995   0.002218  144.246  < 2e-16 ***
    ## sex1                           -0.259397   0.002000 -129.728  < 2e-16 ***
    ## age_scale                      -0.187316   0.002515  -74.477  < 2e-16 ***
    ## race1                           0.333057   0.005295   62.903  < 2e-16 ***
    ## race2                           0.332713   0.010425   31.915  < 2e-16 ***
    ## race3                          -0.195225   0.007374  -26.475  < 2e-16 ***
    ## race4                          -0.446809   0.014964  -29.859  < 2e-16 ***
    ## married1                       -0.173688   0.004924  -35.277  < 2e-16 ***
    ## married2                       -0.345935   0.003903  -88.639  < 2e-16 ***
    ## married3                        0.541420   0.009588   56.470  < 2e-16 ***
    ## married4                        0.267503   0.004856   55.088  < 2e-16 ***
    ## married5                       -0.295873   0.006063  -48.801  < 2e-16 ***
    ## year2009                       -0.054047   0.007572   -7.138 9.48e-13 ***
    ## year2010                        0.005915   0.007570    0.781 0.434611    
    ## year2011                       -0.041486   0.007560   -5.487 4.08e-08 ***
    ## year2012                       -0.010840   0.007479   -1.449 0.147218    
    ## year2013                        0.032088   0.009044    3.548 0.000388 ***
    ## year2014                        0.048131   0.009079    5.301 1.15e-07 ***
    ## year2015                        0.085960   0.009054    9.494  < 2e-16 ***
    ## year2016                        0.116023   0.009036   12.840  < 2e-16 ***
    ## year2017                        0.204167   0.009246   22.081  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1887542  on 2057324  degrees of freedom
    ## Residual deviance: 1761266  on 2057296  degrees of freedom
    ##   (1472945 observations deleted due to missingness)
    ## AIC: 1761324
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
lm_dep_demo <-
  glm(
    depression ~
      income_scale +
      income_demo_ranger_sar_vars_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs 
  )

summary(lm_dep_demo)
```

    ## 
    ## Call:
    ## glm(formula = depression ~ income_scale + income_demo_ranger_sar_vars_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6489  -0.6385  -0.4902  -0.3619   2.9257  
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)                       -1.309897   0.008225 -159.260  < 2e-16 ***
    ## income_scale                      -0.334268   0.002415 -138.439  < 2e-16 ***
    ## income_demo_ranger_sar_vars_scale  0.619891   0.004246  145.986  < 2e-16 ***
    ## physicians_scale                   0.027065   0.002193   12.341  < 2e-16 ***
    ## unweighted_pop_county_scale       -0.008718   0.002386   -3.654 0.000258 ***
    ## median_home_value_county_scale    -0.008901   0.002431   -3.662 0.000251 ***
    ## land_area_2010_scale              -0.004070   0.002047   -1.988 0.046772 *  
    ## education_scale                   -0.058180   0.002183  -26.655  < 2e-16 ***
    ## employment_all1                    0.349459   0.002238  156.129  < 2e-16 ***
    ## sex1                              -0.601383   0.003074 -195.631  < 2e-16 ***
    ## age_scale                         -0.062151   0.002741  -22.672  < 2e-16 ***
    ## race1                             -0.278257   0.006739  -41.289  < 2e-16 ***
    ## race2                              0.531484   0.010603   50.127  < 2e-16 ***
    ## race3                              0.019590   0.007586    2.582 0.009810 ** 
    ## race4                             -0.740403   0.015191  -48.739  < 2e-16 ***
    ## married1                          -0.078192   0.004960  -15.764  < 2e-16 ***
    ## married2                          -0.422103   0.003994 -105.676  < 2e-16 ***
    ## married3                           0.403054   0.009739   41.384  < 2e-16 ***
    ## married4                           0.152590   0.004948   30.840  < 2e-16 ***
    ## married5                          -0.064043   0.006273  -10.209  < 2e-16 ***
    ## year2009                          -0.061729   0.007636   -8.084 6.25e-16 ***
    ## year2010                           0.010702   0.007634    1.402 0.160949    
    ## year2011                          -0.014590   0.007618   -1.915 0.055478 .  
    ## year2012                           0.016893   0.007530    2.243 0.024873 *  
    ## year2013                           0.059587   0.009108    6.542 6.07e-11 ***
    ## year2014                           0.074444   0.009138    8.146 3.75e-16 ***
    ## year2015                           0.115290   0.009114   12.650  < 2e-16 ***
    ## year2016                           0.147491   0.009072   16.258  < 2e-16 ***
    ## year2017                           0.231366   0.009252   25.008  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1885978  on 2055669  degrees of freedom
    ## Residual deviance: 1735223  on 2055641  degrees of freedom
    ##   (1474600 observations deleted due to missingness)
    ## AIC: 1735281
    ## 
    ## Number of Fisher Scoring iterations: 5

### Obesity

``` r
lm_obe_geo <-
  glm(
    obese ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs
  )

summary(lm_obe_geo)
```

    ## 
    ## Call:
    ## glm(formula = obese ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3255  -0.8158  -0.7205   1.3751   2.6130  
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)                    -1.129326   0.006689 -168.825  < 2e-16 ***
    ## raw_income_scale               -0.124041   0.002065  -60.074  < 2e-16 ***
    ## median_income_county_scale      0.025761   0.002490   10.346  < 2e-16 ***
    ## physicians_scale               -0.049332   0.001901  -25.955  < 2e-16 ***
    ## unweighted_pop_county_scale    -0.007460   0.002106   -3.542 0.000398 ***
    ## median_home_value_county_scale -0.143828   0.002867  -50.158  < 2e-16 ***
    ## land_area_2010_scale           -0.011568   0.001735   -6.667 2.60e-11 ***
    ## education_scale                -0.124227   0.001831  -67.830  < 2e-16 ***
    ## employment_all1                 0.007901   0.001926    4.102 4.10e-05 ***
    ## sex1                            0.089394   0.001680   53.218  < 2e-16 ***
    ## age_scale                       0.037159   0.002175   17.085  < 2e-16 ***
    ## race1                           0.005364   0.004582    1.171 0.241777    
    ## race2                           0.189181   0.009076   20.845  < 2e-16 ***
    ## race3                           0.502007   0.005896   85.147  < 2e-16 ***
    ## race4                          -0.817209   0.013439  -60.808  < 2e-16 ***
    ## married1                       -0.106821   0.004517  -23.647  < 2e-16 ***
    ## married2                        0.110174   0.003475   31.703  < 2e-16 ***
    ## married3                        0.133924   0.009605   13.944  < 2e-16 ***
    ## married4                        0.095042   0.004676   20.326  < 2e-16 ***
    ## married5                       -0.181237   0.005636  -32.159  < 2e-16 ***
    ## year2009                        0.035068   0.006426    5.457 4.83e-08 ***
    ## year2010                        0.040213   0.006428    6.256 3.95e-10 ***
    ## year2011                       -0.004577   0.006419   -0.713 0.475767    
    ## year2012                        0.007664   0.006337    1.209 0.226487    
    ## year2013                        0.036604   0.007665    4.775 1.79e-06 ***
    ## year2014                        0.076904   0.007668   10.029  < 2e-16 ***
    ## year2015                        0.116124   0.007622   15.235  < 2e-16 ***
    ## year2016                        0.128935   0.007582   17.006  < 2e-16 ***
    ## year2017                        0.130078   0.007851   16.568  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2324778  on 2006508  degrees of freedom
    ## Residual deviance: 2277022  on 2006480  degrees of freedom
    ##   (1523761 observations deleted due to missingness)
    ## AIC: 2277080
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
lm_obe_demo <-
  glm(
    smoke ~
      income_scale +
      income_demo_ranger_sar_vars_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs 
  )

summary(lm_obe_demo)
```

    ## 
    ## Call:
    ## glm(formula = smoke ~ income_scale + income_demo_ranger_sar_vars_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7738  -0.6300  -0.4621  -0.3116   3.2285  
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)                       -1.100930   0.008033 -137.054  < 2e-16 ***
    ## income_scale                      -0.258314   0.002414 -106.995  < 2e-16 ***
    ## income_demo_ranger_sar_vars_scale  0.708972   0.004723  150.103  < 2e-16 ***
    ## physicians_scale                  -0.022311   0.002279   -9.789  < 2e-16 ***
    ## unweighted_pop_county_scale       -0.005648   0.002513   -2.248   0.0246 *  
    ## median_home_value_county_scale    -0.102658   0.002631  -39.016  < 2e-16 ***
    ## land_area_2010_scale              -0.008613   0.002078   -4.145 3.40e-05 ***
    ## education_scale                   -0.446990   0.002225 -200.930  < 2e-16 ***
    ## employment_all1                    0.082899   0.002316   35.787  < 2e-16 ***
    ## sex1                              -0.215046   0.003191  -67.390  < 2e-16 ***
    ## age_scale                         -0.327717   0.002712 -120.862  < 2e-16 ***
    ## race1                             -0.482872   0.006744  -71.597  < 2e-16 ***
    ## race2                              0.587508   0.010009   58.697  < 2e-16 ***
    ## race3                              0.419735   0.006900   60.835  < 2e-16 ***
    ## race4                             -0.550502   0.013658  -40.305  < 2e-16 ***
    ## married1                          -0.082491   0.004878  -16.912  < 2e-16 ***
    ## married2                          -0.557903   0.004050 -137.742  < 2e-16 ***
    ## married3                           0.223775   0.009856   22.706  < 2e-16 ***
    ## married4                           0.185496   0.005072   36.570  < 2e-16 ***
    ## married5                          -0.028283   0.007072   -3.999 6.35e-05 ***
    ## year2009                          -0.018637   0.007570   -2.462   0.0138 *  
    ## year2010                          -0.018849   0.007600   -2.480   0.0131 *  
    ## year2011                          -0.069454   0.007590   -9.150  < 2e-16 ***
    ## year2012                          -0.058109   0.007525   -7.722 1.14e-14 ***
    ## year2013                          -0.066894   0.009156   -7.306 2.76e-13 ***
    ## year2014                          -0.113849   0.009274  -12.276  < 2e-16 ***
    ## year2015                          -0.156705   0.009394  -16.681  < 2e-16 ***
    ## year2016                          -0.144323   0.009361  -15.417  < 2e-16 ***
    ## year2017                          -0.146432   0.009637  -15.195  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1872702  on 2057584  degrees of freedom
    ## Residual deviance: 1677731  on 2057556  degrees of freedom
    ##   (1472685 observations deleted due to missingness)
    ## AIC: 1677789
    ## 
    ## Number of Fisher Scoring iterations: 5

### Self-reported health

``` r
lm_srh_geo <-
  lm(
    scale(sr_health) ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    data = dfg_rs %>% mutate(sr_health = ifelse(sr_health %in% c(1:5), 6 - sr_health, NA))
  )

summary(lm_srh_geo)
```

    ## 
    ## Call:
    ## lm(formula = scale(sr_health) ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs %>% 
    ##     mutate(sr_health = ifelse(sr_health %in% c(1:5), 6 - sr_health, 
    ##         NA)))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.11408 -0.65753  0.03361  0.71502  2.54129 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)                    -0.1316608  0.0036544  -36.029  < 2e-16 ***
    ## raw_income_scale                0.1735512  0.0008762  198.078  < 2e-16 ***
    ## median_income_county_scale      0.0128310  0.0010326   12.426  < 2e-16 ***
    ## physicians_scale                0.0144079  0.0007951   18.120  < 2e-16 ***
    ## unweighted_pop_county_scale    -0.0067239  0.0008438   -7.969 1.60e-15 ***
    ## median_home_value_county_scale  0.0098960  0.0011364    8.708  < 2e-16 ***
    ## land_area_2010_scale            0.0077501  0.0007297   10.620  < 2e-16 ***
    ## education_scale                 0.1456971  0.0007978  182.615  < 2e-16 ***
    ## employment_all1                -0.1351856  0.0008270 -163.467  < 2e-16 ***
    ## sex1                           -0.0393925  0.0007201  -54.700  < 2e-16 ***
    ## age_scale                      -0.0902529  0.0009372  -96.302  < 2e-16 ***
    ## race1                           0.1199903  0.0017271   69.477  < 2e-16 ***
    ## race2                          -0.0503065  0.0038984  -12.905  < 2e-16 ***
    ## race3                           0.0207076  0.0024715    8.379  < 2e-16 ***
    ## race4                          -0.0302576  0.0041784   -7.241 4.44e-13 ***
    ## married1                        0.0633679  0.0019301   32.831  < 2e-16 ***
    ## married2                        0.0772156  0.0015052   51.299  < 2e-16 ***
    ## married3                       -0.1629005  0.0042922  -37.953  < 2e-16 ***
    ## married4                       -0.0527593  0.0020630  -25.574  < 2e-16 ***
    ## married5                        0.0979744  0.0024005   40.814  < 2e-16 ***
    ## year2009                       -0.0115644  0.0036151   -3.199  0.00138 ** 
    ## year2010                       -0.0334589  0.0036163   -9.252  < 2e-16 ***
    ## year2011                       -0.0208711  0.0036039   -5.791 6.99e-09 ***
    ## year2012                       -0.0247833  0.0035853   -6.913 4.76e-12 ***
    ## year2013                       -0.0605011  0.0039839  -15.186  < 2e-16 ***
    ## year2014                       -0.0578373  0.0040002  -14.458  < 2e-16 ***
    ## year2015                       -0.0704859  0.0039953  -17.642  < 2e-16 ***
    ## year2016                       -0.0794063  0.0039775  -19.964  < 2e-16 ***
    ## year2017                       -0.1173578  0.0085662  -13.700  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9225 on 1759138 degrees of freedom
    ##   (1771103 observations deleted due to missingness)
    ## Multiple R-squared:  0.1436, Adjusted R-squared:  0.1436 
    ## F-statistic: 1.054e+04 on 28 and 1759138 DF,  p-value: < 2.2e-16

``` r
lm_srh_demo <-
  lm(
    scale(sr_health) ~
      income_scale +
      income_demo_ranger_sar_vars_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    data = dfg_rs %>% mutate(sr_health = ifelse(sr_health %in% c(1:5), 6 - sr_health, NA))
  )

summary(lm_srh_demo)
```

    ## 
    ## Call:
    ## lm(formula = scale(sr_health) ~ income_scale + income_demo_ranger_sar_vars_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs %>% 
    ##     mutate(sr_health = ifelse(sr_health %in% c(1:5), 6 - sr_health, 
    ##         NA)))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.16176 -0.65394  0.03618  0.71493  2.67557 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)                       -0.2447920  0.0037915  -64.564  < 2e-16 ***
    ## income_scale                       0.1907835  0.0008775  217.408  < 2e-16 ***
    ## income_demo_ranger_sar_vars_scale -0.1636799  0.0015058 -108.702  < 2e-16 ***
    ## physicians_scale                   0.0141831  0.0007906   17.939  < 2e-16 ***
    ## unweighted_pop_county_scale       -0.0086040  0.0008256  -10.421  < 2e-16 ***
    ## median_home_value_county_scale     0.0189972  0.0008592   22.111  < 2e-16 ***
    ## land_area_2010_scale               0.0064574  0.0007257    8.898  < 2e-16 ***
    ## education_scale                    0.1466787  0.0007976  183.892  < 2e-16 ***
    ## employment_all1                   -0.1432501  0.0008384 -170.854  < 2e-16 ***
    ## sex1                               0.0501097  0.0010906   45.949  < 2e-16 ***
    ## age_scale                         -0.1225623  0.0009691 -126.477  < 2e-16 ***
    ## race1                              0.2763680  0.0022634  122.103  < 2e-16 ***
    ## race2                             -0.1015785  0.0039109  -25.973  < 2e-16 ***
    ## race3                             -0.0366042  0.0025181  -14.536  < 2e-16 ***
    ## race4                              0.0493908  0.0042241   11.693  < 2e-16 ***
    ## married1                           0.0336669  0.0019438   17.320  < 2e-16 ***
    ## married2                           0.0927007  0.0015217   60.919  < 2e-16 ***
    ## married3                          -0.1178041  0.0042886  -27.469  < 2e-16 ***
    ## married4                          -0.0193888  0.0020739   -9.349  < 2e-16 ***
    ## married5                           0.0356729  0.0024601   14.501  < 2e-16 ***
    ## year2009                          -0.0096780  0.0035989   -2.689  0.00716 ** 
    ## year2010                          -0.0349198  0.0036001   -9.700  < 2e-16 ***
    ## year2011                          -0.0282451  0.0035867   -7.875 3.41e-15 ***
    ## year2012                          -0.0332471  0.0035662   -9.323  < 2e-16 ***
    ## year2013                          -0.0690765  0.0039621  -17.435  < 2e-16 ***
    ## year2014                          -0.0663552  0.0039758  -16.690  < 2e-16 ***
    ## year2015                          -0.0797288  0.0039711  -20.077  < 2e-16 ***
    ## year2016                          -0.0882054  0.0039481  -22.342  < 2e-16 ***
    ## year2017                          -0.1255390  0.0085218  -14.732  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9181 on 1757729 degrees of freedom
    ##   (1772512 observations deleted due to missingness)
    ## Multiple R-squared:  0.1516, Adjusted R-squared:  0.1516 
    ## F-statistic: 1.122e+04 on 28 and 1757729 DF,  p-value: < 2.2e-16

### Height

``` r
lm_ht_geo <-
  lm(
    height_scale ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    data = dfg_rs 
  )

summary(lm_ht_geo)
```

    ## 
    ## Call:
    ## lm(formula = height_scale ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5603 -0.4467 -0.0105  0.4360  7.9809 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)                    -0.1606306  0.0018526  -86.703  < 2e-16 ***
    ## raw_income_scale                0.0488055  0.0005982   81.593  < 2e-16 ***
    ## median_income_county_scale     -0.0080539  0.0007044  -11.434  < 2e-16 ***
    ## physicians_scale               -0.0074623  0.0005448  -13.698  < 2e-16 ***
    ## unweighted_pop_county_scale    -0.0052375  0.0005805   -9.022  < 2e-16 ***
    ## median_home_value_county_scale  0.0001315  0.0007692    0.171   0.8643    
    ## land_area_2010_scale            0.0072367  0.0005025   14.403  < 2e-16 ***
    ## education_scale                 0.0583664  0.0005463  106.841  < 2e-16 ***
    ## employment_all1                -0.0086778  0.0005695  -15.238  < 2e-16 ***
    ## sex1                            0.6873379  0.0004929 1394.574  < 2e-16 ***
    ## age_scale                      -0.1015436  0.0006426 -158.024  < 2e-16 ***
    ## race1                           0.1982299  0.0011823  167.664  < 2e-16 ***
    ## race2                           0.0995770  0.0026609   37.422  < 2e-16 ***
    ## race3                           0.1895693  0.0016913  112.087  < 2e-16 ***
    ## race4                          -0.3048071  0.0028705 -106.184  < 2e-16 ***
    ## married1                        0.0014673  0.0013244    1.108   0.2679    
    ## married2                        0.0112929  0.0010351   10.910  < 2e-16 ***
    ## married3                        0.0018012  0.0029685    0.607   0.5440    
    ## married4                        0.0212199  0.0014108   15.041  < 2e-16 ***
    ## married5                       -0.0308903  0.0016494  -18.728  < 2e-16 ***
    ## year2009                        0.0095713  0.0018801    5.091 3.57e-07 ***
    ## year2010                        0.0104873  0.0018820    5.572 2.51e-08 ***
    ## year2011                        0.0035096  0.0018694    1.877   0.0605 .  
    ## year2012                        0.0012180  0.0018481    0.659   0.5099    
    ## year2013                        0.0020383  0.0022516    0.905   0.3653    
    ## year2014                        0.0044410  0.0022678    1.958   0.0502 .  
    ## year2015                        0.0035358  0.0022609    1.564   0.1178    
    ## year2016                        0.0039190  0.0022432    1.747   0.0806 .  
    ## year2017                        0.0106135  0.0023193    4.576 4.74e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6797 on 2038678 degrees of freedom
    ##   (1491563 observations deleted due to missingness)
    ## Multiple R-squared:  0.5332, Adjusted R-squared:  0.5332 
    ## F-statistic: 8.317e+04 on 28 and 2038678 DF,  p-value: < 2.2e-16

``` r
lm_ht_demo <-
  lm(
    height_scale ~
      income_scale +
      income_demo_ranger_sar_vars_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_home_value_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    data = dfg_rs 
  )

summary(lm_ht_demo)
```

    ## 
    ## Call:
    ## lm(formula = height_scale ~ income_scale + income_demo_ranger_sar_vars_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_home_value_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5311 -0.4467 -0.0105  0.4358  8.0138 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)                       -0.1317662  0.0019926  -66.128  < 2e-16 ***
    ## income_scale                       0.0475461  0.0006032   78.821  < 2e-16 ***
    ## income_demo_ranger_sar_vars_scale  0.0357854  0.0010387   34.452  < 2e-16 ***
    ## physicians_scale                  -0.0073336  0.0005439  -13.484  < 2e-16 ***
    ## unweighted_pop_county_scale       -0.0038663  0.0005705   -6.778 1.22e-11 ***
    ## median_home_value_county_scale    -0.0050294  0.0005838   -8.615  < 2e-16 ***
    ## land_area_2010_scale               0.0074921  0.0005017   14.932  < 2e-16 ***
    ## education_scale                    0.0568613  0.0005483  103.702  < 2e-16 ***
    ## employment_all1                   -0.0042924  0.0005799   -7.401 1.35e-13 ***
    ## sex1                               0.6681047  0.0007508  889.861  < 2e-16 ***
    ## age_scale                         -0.0961062  0.0006666 -144.172  < 2e-16 ***
    ## race1                              0.1622016  0.0015573  104.156  < 2e-16 ***
    ## race2                              0.1109791  0.0026809   41.397  < 2e-16 ***
    ## race3                              0.2028680  0.0017307  117.216  < 2e-16 ***
    ## race4                             -0.3221840  0.0029142 -110.555  < 2e-16 ***
    ## married1                           0.0084984  0.0013390    6.347 2.20e-10 ***
    ## married2                           0.0047411  0.0010503    4.514 6.36e-06 ***
    ## married3                          -0.0041609  0.0029784   -1.397   0.1624    
    ## married4                           0.0144523  0.0014241   10.148  < 2e-16 ***
    ## married5                          -0.0174342  0.0016976  -10.270  < 2e-16 ***
    ## year2009                           0.0087470  0.0018796    4.654 3.26e-06 ***
    ## year2010                           0.0106820  0.0018814    5.678 1.37e-08 ***
    ## year2011                           0.0038486  0.0018680    2.060   0.0394 *  
    ## year2012                           0.0015441  0.0018451    0.837   0.4027    
    ## year2013                           0.0023096  0.0022478    1.027   0.3042    
    ## year2014                           0.0046995  0.0022620    2.078   0.0377 *  
    ## year2015                           0.0041795  0.0022552    1.853   0.0638 .  
    ## year2016                           0.0048759  0.0022331    2.183   0.0290 *  
    ## year2017                           0.0106817  0.0023019    4.640 3.48e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6793 on 2037136 degrees of freedom
    ##   (1493105 observations deleted due to missingness)
    ## Multiple R-squared:  0.5336, Adjusted R-squared:  0.5336 
    ## F-statistic: 8.324e+04 on 28 and 2037136 DF,  p-value: < 2.2e-16

## Regression tables

### Demographic reference

``` r
stargazer(
  lm_diab_demo, lm_hbp_demo, lm_obe_demo, lm_bmi_demo, lm_dep_demo, lm_srh_demo,
  ci = TRUE,
  type = "html", digits = 2,
  out = "regression_table_health_outcomes_demo.doc",
  dep.var.labels = c("diabetes", "high blood pressure", "obesity", "BMI", "depression", "self-reported health"),
  covariate.labels =
    c(
      "own income",
      "demographic reference \n median income",
      "ratio of physicians",
      "population",
      "housing cost",
      "land area",
      "education",
      "employment = employed",
      "sex = male",
      "age",
      "race = white",
      "race = African American",
      "race = Asian",
      "race = American Indian \n Alaska native",
      "marital status = single",
      "marital status = married",
      "marital status = separated",
      "marital status = divorced",
      "marital status = widowed"
    ),
  omit = c("year"),
  star.char = c("*", "**", "***"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = c("* p<0.05; ** p<0.01; *** p<0.001")
)
```

    ## 
    ## <table style="text-align:center"><tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="6"><em>Dependent variable:</em></td></tr>
    ## <tr><td></td><td colspan="6" style="border-bottom: 1px solid black"></td></tr>
    ## <tr><td style="text-align:left"></td><td>diabetes</td><td>high blood pressure</td><td>obesity</td><td>BMI</td><td>depression</td><td>self-reported health</td></tr>
    ## <tr><td style="text-align:left"></td><td><em>logistic</em></td><td><em>logistic</em></td><td><em>logistic</em></td><td><em>OLS</em></td><td><em>logistic</em></td><td><em>OLS</em></td></tr>
    ## <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td></tr>
    ## <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">own income</td><td>-0.18<sup>***</sup></td><td>-0.10<sup>***</sup></td><td>-0.26<sup>***</sup></td><td>-0.06<sup>***</sup></td><td>-0.33<sup>***</sup></td><td>0.19<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.19, -0.18)</td><td>(-0.11, -0.10)</td><td>(-0.26, -0.25)</td><td>(-0.06, -0.06)</td><td>(-0.34, -0.33)</td><td>(0.19, 0.19)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">median income</td><td>0.68<sup>***</sup></td><td>0.35<sup>***</sup></td><td>0.71<sup>***</sup></td><td>0.31<sup>***</sup></td><td>0.62<sup>***</sup></td><td>-0.16<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.67, 0.69)</td><td>(0.35, 0.36)</td><td>(0.70, 0.72)</td><td>(0.30, 0.31)</td><td>(0.61, 0.63)</td><td>(-0.17, -0.16)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">ratio of physicians</td><td>-0.04<sup>***</sup></td><td>-0.03<sup>***</sup></td><td>-0.02<sup>***</sup></td><td>-0.02<sup>***</sup></td><td>0.03<sup>***</sup></td><td>0.01<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.05, -0.04)</td><td>(-0.04, -0.03)</td><td>(-0.03, -0.02)</td><td>(-0.03, -0.02)</td><td>(0.02, 0.03)</td><td>(0.01, 0.02)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">population</td><td>0.01<sup>***</sup></td><td>-0.002</td><td>-0.01<sup>*</sup></td><td>-0.01<sup>***</sup></td><td>-0.01<sup>***</sup></td><td>-0.01<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.01, 0.02)</td><td>(-0.01, 0.002)</td><td>(-0.01, -0.001)</td><td>(-0.01, -0.01)</td><td>(-0.01, -0.004)</td><td>(-0.01, -0.01)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">housing cost</td><td>-0.10<sup>***</sup></td><td>-0.08<sup>***</sup></td><td>-0.10<sup>***</sup></td><td>-0.06<sup>***</sup></td><td>-0.01<sup>***</sup></td><td>0.02<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.11, -0.10)</td><td>(-0.09, -0.08)</td><td>(-0.11, -0.10)</td><td>(-0.06, -0.05)</td><td>(-0.01, -0.004)</td><td>(0.02, 0.02)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">land area</td><td>-0.03<sup>***</sup></td><td>-0.02<sup>***</sup></td><td>-0.01<sup>***</sup></td><td>-0.01<sup>***</sup></td><td>-0.004<sup>*</sup></td><td>0.01<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.03, -0.02)</td><td>(-0.03, -0.02)</td><td>(-0.01, -0.005)</td><td>(-0.01, -0.01)</td><td>(-0.01, -0.0001)</td><td>(0.01, 0.01)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">education</td><td>-0.13<sup>***</sup></td><td>-0.12<sup>***</sup></td><td>-0.45<sup>***</sup></td><td>-0.07<sup>***</sup></td><td>-0.06<sup>***</sup></td><td>0.15<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.14, -0.13)</td><td>(-0.13, -0.12)</td><td>(-0.45, -0.44)</td><td>(-0.07, -0.07)</td><td>(-0.06, -0.05)</td><td>(0.15, 0.15)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">employment = employed</td><td>0.24<sup>***</sup></td><td>0.15<sup>***</sup></td><td>0.08<sup>***</sup></td><td>0.03<sup>***</sup></td><td>0.35<sup>***</sup></td><td>-0.14<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.24, 0.25)</td><td>(0.15, 0.16)</td><td>(0.08, 0.09)</td><td>(0.02, 0.03)</td><td>(0.35, 0.35)</td><td>(-0.14, -0.14)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">sex = male</td><td>-0.30<sup>***</sup></td><td>-0.08<sup>***</sup></td><td>-0.22<sup>***</sup></td><td>-0.07<sup>***</sup></td><td>-0.60<sup>***</sup></td><td>0.05<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.30, -0.29)</td><td>(-0.09, -0.08)</td><td>(-0.22, -0.21)</td><td>(-0.07, -0.07)</td><td>(-0.61, -0.60)</td><td>(0.05, 0.05)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">age</td><td>0.96<sup>***</sup></td><td>1.01<sup>***</sup></td><td>-0.33<sup>***</sup></td><td>0.10<sup>***</sup></td><td>-0.06<sup>***</sup></td><td>-0.12<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.95, 0.97)</td><td>(1.01, 1.02)</td><td>(-0.33, -0.32)</td><td>(0.10, 0.10)</td><td>(-0.07, -0.06)</td><td>(-0.12, -0.12)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">race = white</td><td>-1.03<sup>***</sup></td><td>-0.47<sup>***</sup></td><td>-0.48<sup>***</sup></td><td>-0.33<sup>***</sup></td><td>-0.28<sup>***</sup></td><td>0.28<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-1.04, -1.01)</td><td>(-0.48, -0.46)</td><td>(-0.50, -0.47)</td><td>(-0.33, -0.32)</td><td>(-0.29, -0.27)</td><td>(0.27, 0.28)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">race = African American</td><td>0.29<sup>***</sup></td><td>0.11<sup>***</sup></td><td>0.59<sup>***</sup></td><td>0.16<sup>***</sup></td><td>0.53<sup>***</sup></td><td>-0.10<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.26, 0.31)</td><td>(0.09, 0.13)</td><td>(0.57, 0.61)</td><td>(0.16, 0.17)</td><td>(0.51, 0.55)</td><td>(-0.11, -0.09)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">race = Asian</td><td>0.49<sup>***</sup></td><td>0.65<sup>***</sup></td><td>0.42<sup>***</sup></td><td>0.38<sup>***</sup></td><td>0.02<sup>**</sup></td><td>-0.04<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.48, 0.51)</td><td>(0.64, 0.66)</td><td>(0.41, 0.43)</td><td>(0.37, 0.38)</td><td>(0.005, 0.03)</td><td>(-0.04, -0.03)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Alaska native</td><td>-0.39<sup>***</sup></td><td>-0.36<sup>***</sup></td><td>-0.55<sup>***</sup></td><td>-0.50<sup>***</sup></td><td>-0.74<sup>***</sup></td><td>0.05<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.42, -0.36)</td><td>(-0.38, -0.34)</td><td>(-0.58, -0.52)</td><td>(-0.51, -0.50)</td><td>(-0.77, -0.71)</td><td>(0.04, 0.06)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">marital status = single</td><td>0.003</td><td>-0.01<sup>*</sup></td><td>-0.08<sup>***</sup></td><td>-0.01<sup>***</sup></td><td>-0.08<sup>***</sup></td><td>0.03<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.01, 0.02)</td><td>(-0.02, -0.002)</td><td>(-0.09, -0.07)</td><td>(-0.01, -0.01)</td><td>(-0.09, -0.07)</td><td>(0.03, 0.04)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">marital status = married</td><td>-0.03<sup>***</sup></td><td>-0.08<sup>***</sup></td><td>-0.56<sup>***</sup></td><td>0.02<sup>***</sup></td><td>-0.42<sup>***</sup></td><td>0.09<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.04, -0.02)</td><td>(-0.09, -0.08)</td><td>(-0.57, -0.55)</td><td>(0.02, 0.02)</td><td>(-0.43, -0.41)</td><td>(0.09, 0.10)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">marital status = separated</td><td>0.14<sup>***</sup></td><td>0.10<sup>***</sup></td><td>0.22<sup>***</sup></td><td>0.02<sup>***</sup></td><td>0.40<sup>***</sup></td><td>-0.12<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.12, 0.17)</td><td>(0.08, 0.12)</td><td>(0.20, 0.24)</td><td>(0.01, 0.02)</td><td>(0.38, 0.42)</td><td>(-0.13, -0.11)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">marital status = divorced</td><td>-0.01</td><td>0.02<sup>***</sup></td><td>0.19<sup>***</sup></td><td>-0.01<sup>*</sup></td><td>0.15<sup>***</sup></td><td>-0.02<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.02, 0.002)</td><td>(0.02, 0.03)</td><td>(0.18, 0.20)</td><td>(-0.01, -0.001)</td><td>(0.14, 0.16)</td><td>(-0.02, -0.02)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">marital status = widowed</td><td>0.01</td><td>0.03<sup>***</sup></td><td>-0.03<sup>***</sup></td><td>0.01<sup>***</sup></td><td>-0.06<sup>***</sup></td><td>0.04<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.01, 0.02)</td><td>(0.02, 0.04)</td><td>(-0.04, -0.01)</td><td>(0.01, 0.02)</td><td>(-0.08, -0.05)</td><td>(0.03, 0.04)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Constant</td><td>-1.44<sup>***</sup></td><td>-0.32<sup>***</sup></td><td>-1.10<sup>***</sup></td><td>0.24<sup>***</sup></td><td>-1.31<sup>***</sup></td><td>-0.24<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-1.45, -1.42)</td><td>(-0.34, -0.31)</td><td>(-1.12, -1.09)</td><td>(0.23, 0.24)</td><td>(-1.33, -1.29)</td><td>(-0.25, -0.24)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>2,055,692</td><td>2,055,058</td><td>2,057,585</td><td>2,005,016</td><td>2,055,670</td><td>1,757,758</td></tr>
    ## <tr><td style="text-align:left">R<sup>2</sup></td><td></td><td></td><td></td><td>0.06</td><td></td><td>0.15</td></tr>
    ## <tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td></td><td></td><td></td><td>0.06</td><td></td><td>0.15</td></tr>
    ## <tr><td style="text-align:left">Log Likelihood</td><td>-691,495.80</td><td>-1,139,583.00</td><td>-838,865.50</td><td></td><td>-867,611.30</td><td></td></tr>
    ## <tr><td style="text-align:left">Akaike Inf. Crit.</td><td>1,383,050.00</td><td>2,279,224.00</td><td>1,677,789.00</td><td></td><td>1,735,281.00</td><td></td></tr>
    ## <tr><td style="text-align:left">Residual Std. Error</td><td></td><td></td><td></td><td>0.98 (df = 2004987)</td><td></td><td>0.92 (df = 1757729)</td></tr>
    ## <tr><td style="text-align:left">F Statistic</td><td></td><td></td><td></td><td>4,427.61<sup>***</sup> (df = 28; 2004987)</td><td></td><td>11,221.72<sup>***</sup> (df = 28; 1757729)</td></tr>
    ## <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="6" style="text-align:right"><sup>*</sup>p<0.05; <sup>**</sup>p<0.01; <sup>***</sup>p<0.001</td></tr>
    ## <tr><td style="text-align:left"></td><td colspan="6" style="text-align:right">* p<0.05; ** p<0.01; *** p<0.001</td></tr>
    ## </table>

### Geographic reference

``` r
stargazer(
  lm_diab_geo, lm_hbp_geo, lm_obe_geo, lm_bmi_geo, lm_dep_geo, lm_srh_geo,
  ci = TRUE,
  type = "html", digits = 2,
  out = "regression_table_health_outcomes_geo.doc",
  dep.var.labels = c("diabetes", "high blood pressure", "obesity", "BMI", "depression", "self-reported health"),
  covariate.labels =
    c(
      "own income",
      "geographic reference \n median income",
      "ratio of physicians",
      "population",
      "housing cost",
      "land area",
      "education",
      "employment = employed",
      "sex = male",
      "age",
      "race = white",
      "race = African American",
      "race = Asian",
      "race = American Indian \n Alaska native",
      "marital status = single",
      "marital status = married",
      "marital status = separated",
      "marital status = divorced",
      "marital status = widowed"
    ),
  omit = c("year"),
  star.char = c("*", "**", "***"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = c("* p<0.05; ** p<0.01; *** p<0.001")
)
```

    ## 
    ## <table style="text-align:center"><tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="6"><em>Dependent variable:</em></td></tr>
    ## <tr><td></td><td colspan="6" style="border-bottom: 1px solid black"></td></tr>
    ## <tr><td style="text-align:left"></td><td>diabetes</td><td>high blood pressure</td><td>obesity</td><td>BMI</td><td>depression</td><td>self-reported health</td></tr>
    ## <tr><td style="text-align:left"></td><td><em>logistic</em></td><td><em>logistic</em></td><td><em>logistic</em></td><td><em>OLS</em></td><td><em>logistic</em></td><td><em>OLS</em></td></tr>
    ## <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td></tr>
    ## <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">own income</td><td>-0.17<sup>***</sup></td><td>-0.09<sup>***</sup></td><td>-0.12<sup>***</sup></td><td>-0.06<sup>***</sup></td><td>-0.31<sup>***</sup></td><td>0.17<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.18, -0.17)</td><td>(-0.09, -0.09)</td><td>(-0.13, -0.12)</td><td>(-0.06, -0.05)</td><td>(-0.32, -0.31)</td><td>(0.17, 0.18)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">median income</td><td>-0.01<sup>**</sup></td><td>-0.02<sup>***</sup></td><td>0.03<sup>***</sup></td><td>0.01<sup>***</sup></td><td>-0.06<sup>***</sup></td><td>0.01<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.02, -0.002)</td><td>(-0.02, -0.01)</td><td>(0.02, 0.03)</td><td>(0.01, 0.01)</td><td>(-0.06, -0.05)</td><td>(0.01, 0.01)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">ratio of physicians</td><td>-0.05<sup>***</sup></td><td>-0.04<sup>***</sup></td><td>-0.05<sup>***</sup></td><td>-0.03<sup>***</sup></td><td>0.02<sup>***</sup></td><td>0.01<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.05, -0.04)</td><td>(-0.04, -0.03)</td><td>(-0.05, -0.05)</td><td>(-0.03, -0.02)</td><td>(0.02, 0.03)</td><td>(0.01, 0.02)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">population</td><td>0.01<sup>**</sup></td><td>-0.01<sup>*</sup></td><td>-0.01<sup>***</sup></td><td>-0.01<sup>***</sup></td><td>-0.02<sup>***</sup></td><td>-0.01<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.002, 0.01)</td><td>(-0.01, -0.001)</td><td>(-0.01, -0.003)</td><td>(-0.01, -0.004)</td><td>(-0.02, -0.01)</td><td>(-0.01, -0.01)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">housing cost</td><td>-0.10<sup>***</sup></td><td>-0.07<sup>***</sup></td><td>-0.14<sup>***</sup></td><td>-0.06<sup>***</sup></td><td>0.03<sup>***</sup></td><td>0.01<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.11, -0.09)</td><td>(-0.08, -0.07)</td><td>(-0.15, -0.14)</td><td>(-0.07, -0.06)</td><td>(0.02, 0.04)</td><td>(0.01, 0.01)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">land area</td><td>-0.03<sup>***</sup></td><td>-0.02<sup>***</sup></td><td>-0.01<sup>***</sup></td><td>-0.01<sup>***</sup></td><td>-0.01<sup>***</sup></td><td>0.01<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.03, -0.02)</td><td>(-0.03, -0.02)</td><td>(-0.01, -0.01)</td><td>(-0.01, -0.01)</td><td>(-0.01, -0.004)</td><td>(0.01, 0.01)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">education</td><td>-0.12<sup>***</sup></td><td>-0.12<sup>***</sup></td><td>-0.12<sup>***</sup></td><td>-0.06<sup>***</sup></td><td>-0.04<sup>***</sup></td><td>0.15<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.12, -0.11)</td><td>(-0.12, -0.11)</td><td>(-0.13, -0.12)</td><td>(-0.06, -0.06)</td><td>(-0.05, -0.04)</td><td>(0.14, 0.15)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">employment = employed</td><td>0.22<sup>***</sup></td><td>0.13<sup>***</sup></td><td>0.01<sup>***</sup></td><td>0.0000</td><td>0.32<sup>***</sup></td><td>-0.14<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.21, 0.22)</td><td>(0.13, 0.13)</td><td>(0.004, 0.01)</td><td>(-0.002, 0.002)</td><td>(0.32, 0.32)</td><td>(-0.14, -0.13)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">sex = male</td><td>0.12<sup>***</sup></td><td>0.12<sup>***</sup></td><td>0.09<sup>***</sup></td><td>0.10<sup>***</sup></td><td>-0.26<sup>***</sup></td><td>-0.04<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.11, 0.12)</td><td>(0.11, 0.12)</td><td>(0.09, 0.09)</td><td>(0.10, 0.10)</td><td>(-0.26, -0.26)</td><td>(-0.04, -0.04)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">age</td><td>0.66<sup>***</sup></td><td>0.90<sup>***</sup></td><td>0.04<sup>***</sup></td><td>0.04<sup>***</sup></td><td>-0.19<sup>***</sup></td><td>-0.09<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.65, 0.66)</td><td>(0.90, 0.90)</td><td>(0.03, 0.04)</td><td>(0.04, 0.05)</td><td>(-0.19, -0.18)</td><td>(-0.09, -0.09)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">race = white</td><td>-0.37<sup>***</sup></td><td>-0.12<sup>***</sup></td><td>0.01</td><td>-0.03<sup>***</sup></td><td>0.33<sup>***</sup></td><td>0.12<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.38, -0.36)</td><td>(-0.13, -0.12)</td><td>(-0.004, 0.01)</td><td>(-0.04, -0.03)</td><td>(0.32, 0.34)</td><td>(0.12, 0.12)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">race = African American</td><td>0.08<sup>***</sup></td><td>-0.002</td><td>0.19<sup>***</sup></td><td>0.07<sup>***</sup></td><td>0.33<sup>***</sup></td><td>-0.05<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.06, 0.11)</td><td>(-0.02, 0.02)</td><td>(0.17, 0.21)</td><td>(0.06, 0.08)</td><td>(0.31, 0.35)</td><td>(-0.06, -0.04)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">race = Asian</td><td>0.24<sup>***</sup></td><td>0.51<sup>***</sup></td><td>0.50<sup>***</sup></td><td>0.27<sup>***</sup></td><td>-0.20<sup>***</sup></td><td>0.02<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.23, 0.26)</td><td>(0.50, 0.52)</td><td>(0.49, 0.51)</td><td>(0.26, 0.27)</td><td>(-0.21, -0.18)</td><td>(0.02, 0.03)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Alaska native</td><td>-0.03</td><td>-0.17<sup>***</sup></td><td>-0.82<sup>***</sup></td><td>-0.36<sup>***</sup></td><td>-0.45<sup>***</sup></td><td>-0.03<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.06, 0.003)</td><td>(-0.19, -0.15)</td><td>(-0.84, -0.79)</td><td>(-0.36, -0.35)</td><td>(-0.48, -0.42)</td><td>(-0.04, -0.02)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">marital status = single</td><td>-0.07<sup>***</sup></td><td>-0.06<sup>***</sup></td><td>-0.11<sup>***</sup></td><td>-0.07<sup>***</sup></td><td>-0.17<sup>***</sup></td><td>0.06<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.09, -0.06)</td><td>(-0.06, -0.05)</td><td>(-0.12, -0.10)</td><td>(-0.07, -0.07)</td><td>(-0.18, -0.16)</td><td>(0.06, 0.07)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">marital status = married</td><td>0.03<sup>***</sup></td><td>-0.05<sup>***</sup></td><td>0.11<sup>***</sup></td><td>0.06<sup>***</sup></td><td>-0.35<sup>***</sup></td><td>0.08<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.02, 0.03)</td><td>(-0.05, -0.04)</td><td>(0.10, 0.12)</td><td>(0.06, 0.07)</td><td>(-0.35, -0.34)</td><td>(0.07, 0.08)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">marital status = separated</td><td>0.25<sup>***</sup></td><td>0.16<sup>***</sup></td><td>0.13<sup>***</sup></td><td>0.09<sup>***</sup></td><td>0.54<sup>***</sup></td><td>-0.16<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.23, 0.27)</td><td>(0.14, 0.18)</td><td>(0.12, 0.15)</td><td>(0.08, 0.09)</td><td>(0.52, 0.56)</td><td>(-0.17, -0.15)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">marital status = divorced</td><td>0.09<sup>***</sup></td><td>0.08<sup>***</sup></td><td>0.10<sup>***</sup></td><td>0.05<sup>***</sup></td><td>0.27<sup>***</sup></td><td>-0.05<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.08, 0.11)</td><td>(0.07, 0.09)</td><td>(0.09, 0.10)</td><td>(0.05, 0.06)</td><td>(0.26, 0.28)</td><td>(-0.06, -0.05)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">marital status = widowed</td><td>-0.18<sup>***</sup></td><td>-0.08<sup>***</sup></td><td>-0.18<sup>***</sup></td><td>-0.11<sup>***</sup></td><td>-0.30<sup>***</sup></td><td>0.10<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-0.19, -0.16)</td><td>(-0.09, -0.07)</td><td>(-0.19, -0.17)</td><td>(-0.11, -0.10)</td><td>(-0.31, -0.28)</td><td>(0.09, 0.10)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Constant</td><td>-1.85<sup>***</sup></td><td>-0.55<sup>***</sup></td><td>-1.13<sup>***</sup></td><td>0.02<sup>***</sup></td><td>-1.75<sup>***</sup></td><td>-0.13<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(-1.87, -1.83)</td><td>(-0.56, -0.54)</td><td>(-1.14, -1.12)</td><td>(0.01, 0.02)</td><td>(-1.76, -1.73)</td><td>(-0.14, -0.12)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>2,057,349</td><td>2,056,708</td><td>2,006,509</td><td>2,006,509</td><td>2,057,325</td><td>1,759,167</td></tr>
    ## <tr><td style="text-align:left">R<sup>2</sup></td><td></td><td></td><td></td><td>0.04</td><td></td><td>0.14</td></tr>
    ## <tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td></td><td></td><td></td><td>0.04</td><td></td><td>0.14</td></tr>
    ## <tr><td style="text-align:left">Log Likelihood</td><td>-701,079.50</td><td>-1,145,094.00</td><td>-1,138,511.00</td><td></td><td>-880,633.20</td><td></td></tr>
    ## <tr><td style="text-align:left">Akaike Inf. Crit.</td><td>1,402,217.00</td><td>2,290,246.00</td><td>2,277,080.00</td><td></td><td>1,761,324.00</td><td></td></tr>
    ## <tr><td style="text-align:left">Residual Std. Error</td><td></td><td></td><td></td><td>0.99 (df = 2006480)</td><td></td><td>0.92 (df = 1759138)</td></tr>
    ## <tr><td style="text-align:left">F Statistic</td><td></td><td></td><td></td><td>2,923.28<sup>***</sup> (df = 28; 2006480)</td><td></td><td>10,537.34<sup>***</sup> (df = 28; 1759138)</td></tr>
    ## <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="6" style="text-align:right"><sup>*</sup>p<0.05; <sup>**</sup>p<0.01; <sup>***</sup>p<0.001</td></tr>
    ## <tr><td style="text-align:left"></td><td colspan="6" style="text-align:right">* p<0.05; ** p<0.01; *** p<0.001</td></tr>
    ## </table>
