Gallup Relative Status Health Outcome Regression Analysis
================
Daniel O’Leary
1/11/2021

  - [Analysis](#analysis)
      - [Health outcomes](#health-outcomes)
          - [BMI](#bmi)
          - [Diabetes](#diabetes)
          - [High blood pressure](#high-blood-pressure)
          - [Obesity](#obesity)
          - [Self-reported health](#self-reported-health)
          - [Height](#height)

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
lm1 <-
  lm(
    bmi_scale ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
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

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = bmi_scale ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7289 -0.6558 -0.1623  0.4672 21.6228 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                               0.0185304  0.0027185   6.816 9.34e-12
    ## raw_income_scale                         -0.0544151  0.0008785 -61.942  < 2e-16
    ## median_income_county_scale                0.0526182  0.0018257  28.821  < 2e-16
    ## physicians_scale                         -0.0314087  0.0007839 -40.067  < 2e-16
    ## unweighted_pop_county_scale              -0.0060548  0.0008695  -6.964 3.32e-12
    ## median_monthly_housing_cost_county_scale -0.0933595  0.0019162 -48.721  < 2e-16
    ## land_area_2010_scale                     -0.0053712  0.0007393  -7.265 3.72e-13
    ## education_scale                          -0.0576914  0.0008022 -71.918  < 2e-16
    ## employment_all1                           0.0004596  0.0008368   0.549  0.58285
    ## sex1                                      0.0975401  0.0007241 134.701  < 2e-16
    ## age_scale                                 0.0428301  0.0009415  45.493  < 2e-16
    ## race1                                    -0.0334653  0.0017378 -19.258  < 2e-16
    ## race2                                     0.0648137  0.0039038  16.603  < 2e-16
    ## race3                                     0.2810246  0.0024763 113.486  < 2e-16
    ## race4                                    -0.3644829  0.0042014 -86.753  < 2e-16
    ## married1                                 -0.0698869  0.0019429 -35.970  < 2e-16
    ## married2                                  0.0633598  0.0015190  41.711  < 2e-16
    ## married3                                  0.0859394  0.0043516  19.749  < 2e-16
    ## married4                                  0.0532707  0.0020707  25.725  < 2e-16
    ## married5                                 -0.1080214  0.0024211 -44.616  < 2e-16
    ## year2009                                  0.0123180  0.0027576   4.467 7.94e-06
    ## year2010                                  0.0152956  0.0027620   5.538 3.06e-08
    ## year2011                                 -0.0013804  0.0027409  -0.504  0.61453
    ## year2012                                  0.0081857  0.0027052   3.026  0.00248
    ## year2013                                  0.0176142  0.0032974   5.342 9.20e-08
    ## year2014                                  0.0360929  0.0033207  10.869  < 2e-16
    ## year2015                                  0.0546287  0.0033143  16.483  < 2e-16
    ## year2016                                  0.0504270  0.0032974  15.293  < 2e-16
    ## year2017                                  0.0491390  0.0034202  14.367  < 2e-16
    ##                                             
    ## (Intercept)                              ***
    ## raw_income_scale                         ***
    ## median_income_county_scale               ***
    ## physicians_scale                         ***
    ## unweighted_pop_county_scale              ***
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     ***
    ## education_scale                          ***
    ## employment_all1                             
    ## sex1                                     ***
    ## age_scale                                ***
    ## race1                                    ***
    ## race2                                    ***
    ## race3                                    ***
    ## race4                                    ***
    ## married1                                 ***
    ## married2                                 ***
    ## married3                                 ***
    ## married4                                 ***
    ## married5                                 ***
    ## year2009                                 ***
    ## year2010                                 ***
    ## year2011                                    
    ## year2012                                 ** 
    ## year2013                                 ***
    ## year2014                                 ***
    ## year2015                                 ***
    ## year2016                                 ***
    ## year2017                                 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9895 on 2006483 degrees of freedom
    ##   (1523758 observations deleted due to missingness)
    ## Multiple R-squared:  0.03881,    Adjusted R-squared:  0.03879 
    ## F-statistic:  2893 on 28 and 2006483 DF,  p-value: < 2.2e-16

``` r
lm1 <-
  lm(
    bmi_scale ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
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

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = bmi_scale ~ raw_income_scale + median_income_demo_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6229 -0.6517 -0.1594  0.4654 21.7769 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.0491929  0.0027092   18.158
    ## raw_income_scale                         -0.0629740  0.0008775  -71.764
    ## median_income_demo_scale                  0.2159340  0.0016365  131.950
    ## physicians_scale                         -0.0311956  0.0007807  -39.959
    ## unweighted_pop_county_scale              -0.0145099  0.0008114  -17.883
    ## median_monthly_housing_cost_county_scale -0.0429967  0.0008213  -52.354
    ## land_area_2010_scale                     -0.0066310  0.0007324   -9.053
    ## education_scale                          -0.2219152  0.0014754 -150.413
    ## employment_all1                           0.0161428  0.0008423   19.165
    ## sex1                                      0.0511129  0.0008025   63.695
    ## age_scale                                 0.0456391  0.0009381   48.650
    ## race1                                    -0.0327980  0.0017231  -19.034
    ## race2                                     0.0632931  0.0038880   16.279
    ## race3                                     0.2646036  0.0024642  107.378
    ## race4                                    -0.3542430  0.0041842  -84.663
    ## married1                                 -0.0418738  0.0019474  -21.502
    ## married2                                  0.0449039  0.0015195   29.552
    ## married3                                  0.0608780  0.0043383   14.033
    ## married4                                  0.0284482  0.0020713   13.734
    ## married5                                 -0.0690226  0.0024285  -28.422
    ## year2009                                  0.0266503  0.0027483    9.697
    ## year2010                                  0.0194208  0.0027507    7.060
    ## year2011                                  0.0100553  0.0027307    3.682
    ## year2012                                 -0.0065069  0.0026964   -2.413
    ## year2013                                  0.0068875  0.0032846    2.097
    ## year2014                                  0.0284274  0.0033055    8.600
    ## year2015                                 -0.0093267  0.0033357   -2.796
    ## year2016                                 -0.0550949  0.0033793  -16.304
    ## year2017                                 -0.0438077  0.0034729  -12.614
    ##                                          Pr(>|t|)    
    ## (Intercept)                               < 2e-16 ***
    ## raw_income_scale                          < 2e-16 ***
    ## median_income_demo_scale                  < 2e-16 ***
    ## physicians_scale                          < 2e-16 ***
    ## unweighted_pop_county_scale               < 2e-16 ***
    ## median_monthly_housing_cost_county_scale  < 2e-16 ***
    ## land_area_2010_scale                      < 2e-16 ***
    ## education_scale                           < 2e-16 ***
    ## employment_all1                           < 2e-16 ***
    ## sex1                                      < 2e-16 ***
    ## age_scale                                 < 2e-16 ***
    ## race1                                     < 2e-16 ***
    ## race2                                     < 2e-16 ***
    ## race3                                     < 2e-16 ***
    ## race4                                     < 2e-16 ***
    ## married1                                  < 2e-16 ***
    ## married2                                  < 2e-16 ***
    ## married3                                  < 2e-16 ***
    ## married4                                  < 2e-16 ***
    ## married5                                  < 2e-16 ***
    ## year2009                                  < 2e-16 ***
    ## year2010                                 1.66e-12 ***
    ## year2011                                 0.000231 ***
    ## year2012                                 0.015815 *  
    ## year2013                                 0.036001 *  
    ## year2014                                  < 2e-16 ***
    ## year2015                                 0.005173 ** 
    ## year2016                                  < 2e-16 ***
    ## year2017                                  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9854 on 2005551 degrees of freedom
    ##   (1524690 observations deleted due to missingness)
    ## Multiple R-squared:  0.04671,    Adjusted R-squared:  0.0467 
    ## F-statistic:  3510 on 28 and 2005551 DF,  p-value: < 2.2e-16

### Diabetes

``` r
lm1 <-
  glm(
    diabetes ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs %>% mutate(diabetes = ifelse(diabetes == 1, 1, ifelse(diabetes == 2, 0, NA)))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## glm(formula = diabetes ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs %>% mutate(diabetes = ifelse(diabetes == 1, 
    ##         1, ifelse(diabetes == 2, 0, NA))))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5653  -0.5579  -0.3960  -0.2851   3.1807  
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error  z value
    ## (Intercept)                              -1.8573045  0.0088315 -210.303
    ## raw_income_scale                         -0.1716453  0.0029438  -58.307
    ## median_income_county_scale                0.0198819  0.0058925    3.374
    ## physicians_scale                         -0.0563165  0.0025959  -21.694
    ## unweighted_pop_county_scale              -0.0004787  0.0028708   -0.167
    ## median_monthly_housing_cost_county_scale -0.0998777  0.0061251  -16.306
    ## land_area_2010_scale                     -0.0258995  0.0025051  -10.339
    ## education_scale                          -0.1184489  0.0024395  -48.555
    ## employment_all1                           0.2195714  0.0026250   83.645
    ## sex1                                      0.1180691  0.0023023   51.284
    ## age_scale                                 0.6563318  0.0031738  206.798
    ## race1                                    -0.3654497  0.0058192  -62.801
    ## race2                                     0.0795281  0.0119284    6.667
    ## race3                                     0.2601616  0.0075786   34.328
    ## race4                                    -0.0434505  0.0158085   -2.749
    ## married1                                 -0.0764145  0.0065560  -11.656
    ## married2                                  0.0266799  0.0047810    5.580
    ## married3                                  0.2491043  0.0124045   20.082
    ## married4                                  0.0957229  0.0060922   15.712
    ## married5                                 -0.1740706  0.0066130  -26.323
    ## year2009                                 -0.0300995  0.0088483   -3.402
    ## year2010                                  0.0483952  0.0087635    5.522
    ## year2011                                  0.0089407  0.0087856    1.018
    ## year2012                                  0.0469505  0.0086470    5.430
    ## year2013                                  0.0575503  0.0105228    5.469
    ## year2014                                  0.0681349  0.0104352    6.529
    ## year2015                                  0.0764255  0.0103995    7.349
    ## year2016                                  0.0864797  0.0104158    8.303
    ## year2017                                  0.0807805  0.0109342    7.388
    ##                                          Pr(>|z|)    
    ## (Intercept)                               < 2e-16 ***
    ## raw_income_scale                          < 2e-16 ***
    ## median_income_county_scale               0.000741 ***
    ## physicians_scale                          < 2e-16 ***
    ## unweighted_pop_county_scale              0.867557    
    ## median_monthly_housing_cost_county_scale  < 2e-16 ***
    ## land_area_2010_scale                      < 2e-16 ***
    ## education_scale                           < 2e-16 ***
    ## employment_all1                           < 2e-16 ***
    ## sex1                                      < 2e-16 ***
    ## age_scale                                 < 2e-16 ***
    ## race1                                     < 2e-16 ***
    ## race2                                    2.61e-11 ***
    ## race3                                     < 2e-16 ***
    ## race4                                    0.005986 ** 
    ## married1                                  < 2e-16 ***
    ## married2                                 2.40e-08 ***
    ## married3                                  < 2e-16 ***
    ## married4                                  < 2e-16 ***
    ## married5                                  < 2e-16 ***
    ## year2009                                 0.000670 ***
    ## year2010                                 3.35e-08 ***
    ## year2011                                 0.308847    
    ## year2012                                 5.65e-08 ***
    ## year2013                                 4.52e-08 ***
    ## year2014                                 6.61e-11 ***
    ## year2015                                 2.00e-13 ***
    ## year2016                                  < 2e-16 ***
    ## year2017                                 1.49e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1540775  on 2057351  degrees of freedom
    ## Residual deviance: 1402521  on 2057323  degrees of freedom
    ##   (1472918 observations deleted due to missingness)
    ## AIC: 1402579
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
lm1 <-
  glm(
    diabetes ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs %>% mutate(diabetes = ifelse(diabetes == 1, 1, ifelse(diabetes == 2, 0, NA)))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## glm(formula = diabetes ~ raw_income_scale + median_income_demo_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs %>% mutate(diabetes = ifelse(diabetes == 1, 
    ##         1, ifelse(diabetes == 2, 0, NA))))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5712  -0.5629  -0.4012  -0.2705   3.2028  
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error  z value Pr(>|z|)
    ## (Intercept)                              -1.791670   0.008829 -202.936  < 2e-16
    ## raw_income_scale                         -0.185672   0.002958  -62.780  < 2e-16
    ## median_income_demo_scale                  0.385362   0.005526   69.740  < 2e-16
    ## physicians_scale                         -0.055371   0.002597  -21.323  < 2e-16
    ## unweighted_pop_county_scale              -0.002428   0.002722   -0.892   0.3725
    ## median_monthly_housing_cost_county_scale -0.079035   0.002725  -29.000  < 2e-16
    ## land_area_2010_scale                     -0.025438   0.002488  -10.225  < 2e-16
    ## education_scale                          -0.405251   0.004789  -84.612  < 2e-16
    ## employment_all1                           0.238229   0.002638   90.306  < 2e-16
    ## sex1                                      0.030547   0.002621   11.655  < 2e-16
    ## age_scale                                 0.720746   0.003412  211.255  < 2e-16
    ## race1                                    -0.378682   0.005801  -65.284  < 2e-16
    ## race2                                     0.071432   0.011963    5.971 2.36e-09
    ## race3                                     0.237399   0.007597   31.247  < 2e-16
    ## race4                                    -0.029572   0.015864   -1.864   0.0623
    ## married1                                 -0.043813   0.006569   -6.670 2.56e-11
    ## married2                                  0.003103   0.004794    0.647   0.5175
    ## married3                                  0.224872   0.012437   18.081  < 2e-16
    ## married4                                  0.060712   0.006109    9.938  < 2e-16
    ## married5                                 -0.135895   0.006627  -20.507  < 2e-16
    ## year2009                                 -0.017738   0.008846   -2.005   0.0449
    ## year2010                                  0.039897   0.008758    4.556 5.22e-06
    ## year2011                                 -0.005230   0.008794   -0.595   0.5521
    ## year2012                                 -0.003735   0.008676   -0.431   0.6668
    ## year2013                                  0.010619   0.010548    1.007   0.3141
    ## year2014                                  0.020052   0.010465    1.916   0.0554
    ## year2015                                 -0.063730   0.010572   -6.028 1.66e-09
    ## year2016                                 -0.127110   0.010806  -11.763  < 2e-16
    ## year2017                                 -0.129408   0.011385  -11.367  < 2e-16
    ##                                             
    ## (Intercept)                              ***
    ## raw_income_scale                         ***
    ## median_income_demo_scale                 ***
    ## physicians_scale                         ***
    ## unweighted_pop_county_scale                 
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     ***
    ## education_scale                          ***
    ## employment_all1                          ***
    ## sex1                                     ***
    ## age_scale                                ***
    ## race1                                    ***
    ## race2                                    ***
    ## race3                                    ***
    ## race4                                    .  
    ## married1                                 ***
    ## married2                                    
    ## married3                                 ***
    ## married4                                 ***
    ## married5                                 ***
    ## year2009                                 *  
    ## year2010                                 ***
    ## year2011                                    
    ## year2012                                    
    ## year2013                                    
    ## year2014                                 .  
    ## year2015                                 ***
    ## year2016                                 ***
    ## year2017                                 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1540005  on 2056392  degrees of freedom
    ## Residual deviance: 1396737  on 2056364  degrees of freedom
    ##   (1473877 observations deleted due to missingness)
    ## AIC: 1396795
    ## 
    ## Number of Fisher Scoring iterations: 5

### High blood pressure

``` r
lm1 <-
  glm(
    depression ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs %>% mutate(depression = ifelse(depression == 1, 1, ifelse(depression == 2, 0, NA)))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## glm(formula = depression ~ raw_income_scale + median_income_demo_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs %>% mutate(depression = ifelse(depression == 
    ##         1, 1, ifelse(depression == 2, 0, NA))))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6409  -0.6496  -0.5001  -0.3604   2.8547  
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error  z value
    ## (Intercept)                              -1.6727736  0.0077404 -216.111
    ## raw_income_scale                         -0.3271320  0.0025614 -127.716
    ## median_income_demo_scale                  0.3799494  0.0045003   84.427
    ## physicians_scale                          0.0320693  0.0021414   14.976
    ## unweighted_pop_county_scale              -0.0006429  0.0023092   -0.278
    ## median_monthly_housing_cost_county_scale -0.0380105  0.0023287  -16.322
    ## land_area_2010_scale                     -0.0038983  0.0020388   -1.912
    ## education_scale                          -0.3311530  0.0040510  -81.746
    ## employment_all1                           0.3454596  0.0022365  154.464
    ## sex1                                     -0.3431245  0.0022402 -153.168
    ## age_scale                                -0.1813459  0.0025604  -70.826
    ## race1                                     0.3152813  0.0052872   59.631
    ## race2                                     0.3249605  0.0104538   31.085
    ## race3                                    -0.2210826  0.0073743  -29.980
    ## race4                                    -0.4206438  0.0149625  -28.113
    ## married1                                 -0.1276872  0.0049365  -25.866
    ## married2                                 -0.3834387  0.0039342  -97.463
    ## married3                                  0.5047350  0.0096238   52.446
    ## married4                                  0.2259728  0.0048859   46.250
    ## married5                                 -0.2344570  0.0061148  -38.343
    ## year2009                                 -0.0298384  0.0075938   -3.929
    ## year2010                                  0.0100729  0.0075860    1.328
    ## year2011                                 -0.0290719  0.0075717   -3.840
    ## year2012                                 -0.0499817  0.0074919   -6.671
    ## year2013                                  0.0029845  0.0090481    0.330
    ## year2014                                  0.0170000  0.0090757    1.873
    ## year2015                                 -0.0447209  0.0091485   -4.888
    ## year2016                                 -0.1021145  0.0093310  -10.944
    ## year2017                                 -0.0135572  0.0095649   -1.417
    ##                                          Pr(>|z|)    
    ## (Intercept)                               < 2e-16 ***
    ## raw_income_scale                          < 2e-16 ***
    ## median_income_demo_scale                  < 2e-16 ***
    ## physicians_scale                          < 2e-16 ***
    ## unweighted_pop_county_scale              0.780703    
    ## median_monthly_housing_cost_county_scale  < 2e-16 ***
    ## land_area_2010_scale                     0.055873 .  
    ## education_scale                           < 2e-16 ***
    ## employment_all1                           < 2e-16 ***
    ## sex1                                      < 2e-16 ***
    ## age_scale                                 < 2e-16 ***
    ## race1                                     < 2e-16 ***
    ## race2                                     < 2e-16 ***
    ## race3                                     < 2e-16 ***
    ## race4                                     < 2e-16 ***
    ## married1                                  < 2e-16 ***
    ## married2                                  < 2e-16 ***
    ## married3                                  < 2e-16 ***
    ## married4                                  < 2e-16 ***
    ## married5                                  < 2e-16 ***
    ## year2009                                 8.52e-05 ***
    ## year2010                                 0.184235    
    ## year2011                                 0.000123 ***
    ## year2012                                 2.53e-11 ***
    ## year2013                                 0.741517    
    ## year2014                                 0.061051 .  
    ## year2015                                 1.02e-06 ***
    ## year2016                                  < 2e-16 ***
    ## year2017                                 0.156369    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1886512  on 2056368  degrees of freedom
    ## Residual deviance: 1753139  on 2056340  degrees of freedom
    ##   (1473901 observations deleted due to missingness)
    ## AIC: 1753197
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
lm1 <-
  glm(
    hbp ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs %>% mutate(hbp = ifelse(hbp == 1, 1, ifelse(hbp == 2, 0, NA)))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## glm(formula = hbp ~ raw_income_scale + median_income_demo_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs %>% mutate(hbp = ifelse(hbp == 1, 1, ifelse(hbp == 
    ##         2, 0, NA))))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3293  -0.8705  -0.5342   1.0529   2.8685  
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                              -0.520752   0.006436 -80.909  < 2e-16
    ## raw_income_scale                         -0.096309   0.002064 -46.671  < 2e-16
    ## median_income_demo_scale                  0.223930   0.004020  55.699  < 2e-16
    ## physicians_scale                         -0.042063   0.001835 -22.920  < 2e-16
    ## unweighted_pop_county_scale              -0.011373   0.001959  -5.805 6.44e-09
    ## median_monthly_housing_cost_county_scale -0.070340   0.001921 -36.615  < 2e-16
    ## land_area_2010_scale                     -0.021899   0.001762 -12.428  < 2e-16
    ## education_scale                          -0.286299   0.003558 -80.475  < 2e-16
    ## employment_all1                           0.144620   0.001883  76.811  < 2e-16
    ## sex1                                      0.067582   0.001897  35.627  < 2e-16
    ## age_scale                                 0.924158   0.002421 381.692  < 2e-16
    ## race1                                    -0.128504   0.004333 -29.656  < 2e-16
    ## race2                                    -0.009861   0.009320  -1.058 0.290034
    ## race3                                     0.514004   0.005867  87.603  < 2e-16
    ## race4                                    -0.172577   0.011550 -14.942  < 2e-16
    ## married1                                 -0.036848   0.004697  -7.846 4.30e-15
    ## married2                                 -0.062060   0.003493 -17.767  < 2e-16
    ## married3                                  0.142424   0.009681  14.712  < 2e-16
    ## married4                                  0.061269   0.004563  13.427  < 2e-16
    ## married5                                 -0.049199   0.005254  -9.363  < 2e-16
    ## year2009                                  0.010027   0.006318   1.587 0.112507
    ## year2010                                  0.023855   0.006313   3.779 0.000158
    ## year2011                                  0.006294   0.006307   0.998 0.318285
    ## year2012                                 -0.028041   0.006239  -4.494 6.98e-06
    ## year2013                                 -0.020921   0.007636  -2.740 0.006151
    ## year2014                                 -0.044608   0.007661  -5.823 5.78e-09
    ## year2015                                 -0.117919   0.007758 -15.199  < 2e-16
    ## year2016                                 -0.184240   0.007920 -23.264  < 2e-16
    ## year2017                                 -0.174107   0.008185 -21.271  < 2e-16
    ##                                             
    ## (Intercept)                              ***
    ## raw_income_scale                         ***
    ## median_income_demo_scale                 ***
    ## physicians_scale                         ***
    ## unweighted_pop_county_scale              ***
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     ***
    ## education_scale                          ***
    ## employment_all1                          ***
    ## sex1                                     ***
    ## age_scale                                ***
    ## race1                                    ***
    ## race2                                       
    ## race3                                    ***
    ## race4                                    ***
    ## married1                                 ***
    ## married2                                 ***
    ## married3                                 ***
    ## married4                                 ***
    ## married5                                 ***
    ## year2009                                    
    ## year2010                                 ***
    ## year2011                                    
    ## year2012                                 ***
    ## year2013                                 ** 
    ## year2014                                 ***
    ## year2015                                 ***
    ## year2016                                 ***
    ## year2017                                 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2648569  on 2055750  degrees of freedom
    ## Residual deviance: 2286133  on 2055722  degrees of freedom
    ##   (1474519 observations deleted due to missingness)
    ## AIC: 2286191
    ## 
    ## Number of Fisher Scoring iterations: 4

### Obesity

``` r
lm1 <-
  glm(
    obese ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs %>% mutate(obese = ifelse(obese == 1, 0, ifelse(obese == 0, 1, NA)))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## glm(formula = obese ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs %>% mutate(obese = ifelse(obese == 1, 0, ifelse(obese == 
    ##         0, 1, NA))))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3546  -0.8146  -0.7168   1.3727   2.4754  
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error  z value Pr(>|z|)
    ## (Intercept)                              -1.130781   0.006690 -169.022  < 2e-16
    ## raw_income_scale                         -0.122335   0.002065  -59.230  < 2e-16
    ## median_income_county_scale                0.099703   0.004259   23.412  < 2e-16
    ## physicians_scale                         -0.059412   0.001872  -31.738  < 2e-16
    ## unweighted_pop_county_scale              -0.012034   0.002118   -5.682 1.33e-08
    ## median_monthly_housing_cost_county_scale -0.183577   0.004475  -41.026  < 2e-16
    ## land_area_2010_scale                     -0.008873   0.001733   -5.121 3.04e-07
    ## education_scale                          -0.123001   0.001833  -67.102  < 2e-16
    ## employment_all1                           0.008700   0.001926    4.516 6.30e-06
    ## sex1                                      0.089831   0.001680   53.486  < 2e-16
    ## age_scale                                 0.034947   0.002173   16.081  < 2e-16
    ## race1                                     0.004182   0.004591    0.911    0.362
    ## race2                                     0.179123   0.009071   19.746  < 2e-16
    ## race3                                     0.526093   0.005887   89.370  < 2e-16
    ## race4                                    -0.834618   0.013420  -62.192  < 2e-16
    ## married1                                 -0.108220   0.004515  -23.970  < 2e-16
    ## married2                                  0.110380   0.003475   31.760  < 2e-16
    ## married3                                  0.132921   0.009601   13.845  < 2e-16
    ## married4                                  0.096927   0.004675   20.735  < 2e-16
    ## married5                                 -0.178678   0.005634  -31.712  < 2e-16
    ## year2009                                  0.034699   0.006423    5.402 6.58e-08
    ## year2010                                  0.040639   0.006426    6.325 2.54e-10
    ## year2011                                  0.008928   0.006411    1.392    0.164
    ## year2012                                  0.029325   0.006322    4.639 3.50e-06
    ## year2013                                  0.058978   0.007651    7.709 1.27e-14
    ## year2014                                  0.097186   0.007650   12.704  < 2e-16
    ## year2015                                  0.126504   0.007613   16.616  < 2e-16
    ## year2016                                  0.129895   0.007591   17.113  < 2e-16
    ## year2017                                  0.122188   0.007885   15.496  < 2e-16
    ##                                             
    ## (Intercept)                              ***
    ## raw_income_scale                         ***
    ## median_income_county_scale               ***
    ## physicians_scale                         ***
    ## unweighted_pop_county_scale              ***
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     ***
    ## education_scale                          ***
    ## employment_all1                          ***
    ## sex1                                     ***
    ## age_scale                                ***
    ## race1                                       
    ## race2                                    ***
    ## race3                                    ***
    ## race4                                    ***
    ## married1                                 ***
    ## married2                                 ***
    ## married3                                 ***
    ## married4                                 ***
    ## married5                                 ***
    ## year2009                                 ***
    ## year2010                                 ***
    ## year2011                                    
    ## year2012                                 ***
    ## year2013                                 ***
    ## year2014                                 ***
    ## year2015                                 ***
    ## year2016                                 ***
    ## year2017                                 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2324782  on 2006511  degrees of freedom
    ## Residual deviance: 2277944  on 2006483  degrees of freedom
    ##   (1523758 observations deleted due to missingness)
    ## AIC: 2278002
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
lm1 <-
  glm(
    obese ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year,
    family = "binomial",
    data = dfg_rs %>% mutate(obese = ifelse(obese == 1, 0, ifelse(obese == 0, 1, NA)))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## glm(formula = obese ~ raw_income_scale + median_income_demo_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = dfg_rs %>% mutate(obese = ifelse(obese == 1, 0, ifelse(obese == 
    ##         0, 1, NA))))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5669  -0.8180  -0.7077   1.3477   2.6764  
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error  z value Pr(>|z|)
    ## (Intercept)                              -1.082182   0.006701 -161.486  < 2e-16
    ## raw_income_scale                         -0.138017   0.002081  -66.321  < 2e-16
    ## median_income_demo_scale                  0.394472   0.003913  100.805  < 2e-16
    ## physicians_scale                         -0.059703   0.001877  -31.806  < 2e-16
    ## unweighted_pop_county_scale              -0.028144   0.002006  -14.033  < 2e-16
    ## median_monthly_housing_cost_county_scale -0.088773   0.001974  -44.971  < 2e-16
    ## land_area_2010_scale                     -0.011312   0.001736   -6.515 7.25e-11
    ## education_scale                          -0.424690   0.003516 -120.793  < 2e-16
    ## employment_all1                           0.034958   0.001946   17.968  < 2e-16
    ## sex1                                      0.004943   0.001878    2.632  0.00849
    ## age_scale                                 0.042741   0.002211   19.331  < 2e-16
    ## race1                                     0.005120   0.004581    1.118  0.26375
    ## race2                                     0.177064   0.009095   19.469  < 2e-16
    ## race3                                     0.499277   0.005897   84.668  < 2e-16
    ## race4                                    -0.820569   0.013442  -61.044  < 2e-16
    ## married1                                 -0.061941   0.004534  -13.660  < 2e-16
    ## married2                                  0.080649   0.003495   23.076  < 2e-16
    ## married3                                  0.090934   0.009625    9.448  < 2e-16
    ## married4                                  0.053137   0.004702   11.301  < 2e-16
    ## married5                                 -0.113934   0.005674  -20.079  < 2e-16
    ## year2009                                  0.061218   0.006444    9.501  < 2e-16
    ## year2010                                  0.049973   0.006439    7.761 8.45e-15
    ## year2011                                  0.029271   0.006429    4.553 5.28e-06
    ## year2012                                  0.002138   0.006343    0.337  0.73611
    ## year2013                                  0.039487   0.007670    5.148 2.63e-07
    ## year2014                                  0.083369   0.007665   10.876  < 2e-16
    ## year2015                                  0.010849   0.007718    1.406  0.15984
    ## year2016                                 -0.060460   0.007841   -7.711 1.25e-14
    ## year2017                                 -0.052912   0.008126   -6.511 7.44e-11
    ##                                             
    ## (Intercept)                              ***
    ## raw_income_scale                         ***
    ## median_income_demo_scale                 ***
    ## physicians_scale                         ***
    ## unweighted_pop_county_scale              ***
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     ***
    ## education_scale                          ***
    ## employment_all1                          ***
    ## sex1                                     ** 
    ## age_scale                                ***
    ## race1                                       
    ## race2                                    ***
    ## race3                                    ***
    ## race4                                    ***
    ## married1                                 ***
    ## married2                                 ***
    ## married3                                 ***
    ## married4                                 ***
    ## married5                                 ***
    ## year2009                                 ***
    ## year2010                                 ***
    ## year2011                                 ***
    ## year2012                                    
    ## year2013                                 ***
    ## year2014                                 ***
    ## year2015                                    
    ## year2016                                 ***
    ## year2017                                 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2323788  on 2005579  degrees of freedom
    ## Residual deviance: 2267129  on 2005551  degrees of freedom
    ##   (1524690 observations deleted due to missingness)
    ## AIC: 2267187
    ## 
    ## Number of Fisher Scoring iterations: 4

### Self-reported health

``` r
lm1 <-
  lm(
    scale(sr_health) ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
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

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(sr_health) ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs %>% 
    ##     mutate(sr_health = ifelse(sr_health %in% c(1:5), 6 - sr_health, 
    ##         NA)))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.10147 -0.65743  0.03355  0.71485  2.55368 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1331295  0.0036555  -36.419
    ## raw_income_scale                          0.1731691  0.0008768  197.512
    ## median_income_county_scale               -0.0051223  0.0018265   -2.804
    ## physicians_scale                          0.0143890  0.0007788   18.475
    ## unweighted_pop_county_scale              -0.0092225  0.0008621  -10.698
    ## median_monthly_housing_cost_county_scale  0.0275896  0.0019121   14.429
    ## land_area_2010_scale                      0.0069331  0.0007329    9.460
    ## education_scale                           0.1453128  0.0007985  181.982
    ## employment_all1                          -0.1354375  0.0008272 -163.721
    ## sex1                                     -0.0394452  0.0007201  -54.776
    ## age_scale                                -0.0902203  0.0009368  -96.308
    ## race1                                     0.1215618  0.0017318   70.196
    ## race2                                    -0.0490858  0.0038988  -12.590
    ## race3                                     0.0183442  0.0024680    7.433
    ## race4                                    -0.0290685  0.0041738   -6.965
    ## married1                                  0.0631991  0.0019298   32.748
    ## married2                                  0.0777486  0.0015056   51.638
    ## married3                                 -0.1631095  0.0042920  -38.003
    ## married4                                 -0.0528630  0.0020629  -25.626
    ## married5                                  0.0980923  0.0024001   40.870
    ## year2009                                 -0.0114402  0.0036149   -3.165
    ## year2010                                 -0.0333408  0.0036162   -9.220
    ## year2011                                 -0.0218239  0.0036013   -6.060
    ## year2012                                 -0.0263408  0.0035793   -7.359
    ## year2013                                 -0.0619783  0.0039782  -15.580
    ## year2014                                 -0.0585967  0.0039933  -14.674
    ## year2015                                 -0.0697201  0.0039925  -17.463
    ## year2016                                 -0.0769976  0.0039819  -19.337
    ## year2017                                 -0.1133533  0.0085729  -13.222
    ##                                          Pr(>|t|)    
    ## (Intercept)                               < 2e-16 ***
    ## raw_income_scale                          < 2e-16 ***
    ## median_income_county_scale                0.00504 ** 
    ## physicians_scale                          < 2e-16 ***
    ## unweighted_pop_county_scale               < 2e-16 ***
    ## median_monthly_housing_cost_county_scale  < 2e-16 ***
    ## land_area_2010_scale                      < 2e-16 ***
    ## education_scale                           < 2e-16 ***
    ## employment_all1                           < 2e-16 ***
    ## sex1                                      < 2e-16 ***
    ## age_scale                                 < 2e-16 ***
    ## race1                                     < 2e-16 ***
    ## race2                                     < 2e-16 ***
    ## race3                                    1.06e-13 ***
    ## race4                                    3.29e-12 ***
    ## married1                                  < 2e-16 ***
    ## married2                                  < 2e-16 ***
    ## married3                                  < 2e-16 ***
    ## married4                                  < 2e-16 ***
    ## married5                                  < 2e-16 ***
    ## year2009                                  0.00155 ** 
    ## year2010                                  < 2e-16 ***
    ## year2011                                 1.36e-09 ***
    ## year2012                                 1.85e-13 ***
    ## year2013                                  < 2e-16 ***
    ## year2014                                  < 2e-16 ***
    ## year2015                                  < 2e-16 ***
    ## year2016                                  < 2e-16 ***
    ## year2017                                  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9224 on 1759140 degrees of freedom
    ##   (1771101 observations deleted due to missingness)
    ## Multiple R-squared:  0.1437, Adjusted R-squared:  0.1437 
    ## F-statistic: 1.054e+04 on 28 and 1759140 DF,  p-value: < 2.2e-16

``` r
lm1 <-
  lm(
    scale(sr_health) ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
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

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(sr_health) ~ raw_income_scale + median_income_demo_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs %>% 
    ##     mutate(sr_health = ifelse(sr_health %in% c(1:5), 6 - sr_health, 
    ##         NA)))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.15185 -0.65671  0.03679  0.71400  2.58662 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1509910  0.0036537  -41.326
    ## raw_income_scale                          0.1776532  0.0008787  202.180
    ## median_income_demo_scale                 -0.1048152  0.0017135  -61.171
    ## physicians_scale                          0.0141369  0.0007779   18.173
    ## unweighted_pop_county_scale              -0.0085332  0.0008067  -10.578
    ## median_monthly_housing_cost_county_scale  0.0224671  0.0008208   27.373
    ## land_area_2010_scale                      0.0066694  0.0007282    9.159
    ## education_scale                           0.2248805  0.0015249  147.474
    ## employment_all1                          -0.1432505  0.0008360 -171.348
    ## sex1                                     -0.0176786  0.0008025  -22.029
    ## age_scale                                -0.0905503  0.0009357  -96.770
    ## race1                                     0.1232871  0.0017222   71.587
    ## race2                                    -0.0479040  0.0038945  -12.300
    ## race3                                     0.0249307  0.0024634   10.120
    ## race4                                    -0.0334930  0.0041694   -8.033
    ## married1                                  0.0491278  0.0019415   25.304
    ## married2                                  0.0875327  0.0015116   57.907
    ## married3                                 -0.1512826  0.0042917  -35.250
    ## married4                                 -0.0405519  0.0020705  -19.585
    ## married5                                  0.0792057  0.0024168   32.773
    ## year2009                                 -0.0184479  0.0036128   -5.106
    ## year2010                                 -0.0353268  0.0036124   -9.779
    ## year2011                                 -0.0271959  0.0035984   -7.558
    ## year2012                                 -0.0189197  0.0035775   -5.289
    ## year2013                                 -0.0561379  0.0039746  -14.124
    ## year2014                                 -0.0535077  0.0039878  -13.418
    ## year2015                                 -0.0363955  0.0040199   -9.054
    ## year2016                                 -0.0217203  0.0040651   -5.343
    ## year2017                                 -0.0628835  0.0086156   -7.299
    ##                                          Pr(>|t|)    
    ## (Intercept)                               < 2e-16 ***
    ## raw_income_scale                          < 2e-16 ***
    ## median_income_demo_scale                  < 2e-16 ***
    ## physicians_scale                          < 2e-16 ***
    ## unweighted_pop_county_scale               < 2e-16 ***
    ## median_monthly_housing_cost_county_scale  < 2e-16 ***
    ## land_area_2010_scale                      < 2e-16 ***
    ## education_scale                           < 2e-16 ***
    ## employment_all1                           < 2e-16 ***
    ## sex1                                      < 2e-16 ***
    ## age_scale                                 < 2e-16 ***
    ## race1                                     < 2e-16 ***
    ## race2                                     < 2e-16 ***
    ## race3                                     < 2e-16 ***
    ## race4                                    9.51e-16 ***
    ## married1                                  < 2e-16 ***
    ## married2                                  < 2e-16 ***
    ## married3                                  < 2e-16 ***
    ## married4                                  < 2e-16 ***
    ## married5                                  < 2e-16 ***
    ## year2009                                 3.29e-07 ***
    ## year2010                                  < 2e-16 ***
    ## year2011                                 4.10e-14 ***
    ## year2012                                 1.23e-07 ***
    ## year2013                                  < 2e-16 ***
    ## year2014                                  < 2e-16 ***
    ## year2015                                  < 2e-16 ***
    ## year2016                                 9.14e-08 ***
    ## year2017                                 2.90e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9215 on 1759042 degrees of freedom
    ##   (1771199 observations deleted due to missingness)
    ## Multiple R-squared:  0.1455, Adjusted R-squared:  0.1455 
    ## F-statistic: 1.07e+04 on 28 and 1759042 DF,  p-value: < 2.2e-16

### Height

``` r
lm1 <-
  lm(
    scale(HEIGHT) ~
      raw_income_scale +
      median_income_county_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
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

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(HEIGHT) ~ raw_income_scale + median_income_county_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5461 -0.4466 -0.0104  0.4361  7.9934 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1589904  0.0018538  -85.766
    ## raw_income_scale                          0.0491020  0.0005986   82.031
    ## median_income_county_scale                0.0045866  0.0012444    3.686
    ## physicians_scale                         -0.0064990  0.0005341  -12.169
    ## unweighted_pop_county_scale              -0.0023712  0.0005931   -3.998
    ## median_monthly_housing_cost_county_scale -0.0146024  0.0013059  -11.182
    ## land_area_2010_scale                      0.0078044  0.0005045   15.469
    ## education_scale                           0.0586744  0.0005467  107.317
    ## employment_all1                          -0.0084786  0.0005697  -14.883
    ## sex1                                      0.6873537  0.0004928 1394.653
    ## age_scale                                -0.1014158  0.0006423 -157.898
    ## race1                                     0.1966598  0.0011854  165.903
    ## race2                                     0.0990225  0.0026613   37.208
    ## race3                                     0.1901373  0.0016890  112.575
    ## race4                                    -0.3045173  0.0028673 -106.205
    ## married1                                  0.0017828  0.0013242    1.346
    ## married2                                  0.0106778  0.0010354   10.313
    ## married3                                  0.0020768  0.0029685    0.700
    ## married4                                  0.0211716  0.0014107   15.008
    ## married5                                 -0.0312147  0.0016492  -18.927
    ## year2009                                  0.0094922  0.0018801    5.049
    ## year2010                                  0.0103448  0.0018820    5.497
    ## year2011                                  0.0034369  0.0018674    1.840
    ## year2012                                  0.0011312  0.0018434    0.614
    ## year2013                                  0.0018655  0.0022473    0.830
    ## year2014                                  0.0037000  0.0022626    1.635
    ## year2015                                  0.0019968  0.0022588    0.884
    ## year2016                                  0.0014130  0.0022470    0.629
    ## year2017                                  0.0072321  0.0023310    3.103
    ##                                          Pr(>|t|)    
    ## (Intercept)                               < 2e-16 ***
    ## raw_income_scale                          < 2e-16 ***
    ## median_income_county_scale               0.000228 ***
    ## physicians_scale                          < 2e-16 ***
    ## unweighted_pop_county_scale              6.40e-05 ***
    ## median_monthly_housing_cost_county_scale  < 2e-16 ***
    ## land_area_2010_scale                      < 2e-16 ***
    ## education_scale                           < 2e-16 ***
    ## employment_all1                           < 2e-16 ***
    ## sex1                                      < 2e-16 ***
    ## age_scale                                 < 2e-16 ***
    ## race1                                     < 2e-16 ***
    ## race2                                     < 2e-16 ***
    ## race3                                     < 2e-16 ***
    ## race4                                     < 2e-16 ***
    ## married1                                 0.178200    
    ## married2                                  < 2e-16 ***
    ## married3                                 0.484158    
    ## married4                                  < 2e-16 ***
    ## married5                                  < 2e-16 ***
    ## year2009                                 4.45e-07 ***
    ## year2010                                 3.87e-08 ***
    ## year2011                                 0.065701 .  
    ## year2012                                 0.539436    
    ## year2013                                 0.406498    
    ## year2014                                 0.101988    
    ## year2015                                 0.376689    
    ## year2016                                 0.529460    
    ## year2017                                 0.001919 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6796 on 2038681 degrees of freedom
    ##   (1491560 observations deleted due to missingness)
    ## Multiple R-squared:  0.5332, Adjusted R-squared:  0.5332 
    ## F-statistic: 8.318e+04 on 28 and 2038681 DF,  p-value: < 2.2e-16

``` r
lm1 <-
  lm(
    scale(HEIGHT) ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      unweighted_pop_county_scale +
      median_monthly_housing_cost_county_scale +
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

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(HEIGHT) ~ raw_income_scale + median_income_demo_scale + 
    ##     physicians_scale + unweighted_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = dfg_rs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5272 -0.4467 -0.0104  0.4360  7.9740 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1526591  0.0018544  -82.325
    ## raw_income_scale                          0.0474336  0.0006001   79.038
    ## median_income_demo_scale                  0.0397190  0.0011201   35.460
    ## physicians_scale                         -0.0064154  0.0005339  -12.017
    ## unweighted_pop_county_scale              -0.0030518  0.0005556   -5.493
    ## median_monthly_housing_cost_county_scale -0.0101531  0.0005616  -18.080
    ## land_area_2010_scale                      0.0077932  0.0005017   15.533
    ## education_scale                           0.0285324  0.0010101   28.247
    ## employment_all1                          -0.0054769  0.0005756   -9.515
    ## sex1                                      0.6788522  0.0005478 1239.178
    ## age_scale                                -0.1008831  0.0006424 -157.043
    ## race1                                     0.1964027  0.0011798  166.466
    ## race2                                     0.0985446  0.0026604   37.041
    ## race3                                     0.1875046  0.0016870  111.144
    ## race4                                    -0.3027415  0.0028662 -105.625
    ## married1                                  0.0069352  0.0013321    5.206
    ## married2                                  0.0071020  0.0010396    6.832
    ## married3                                 -0.0024394  0.0029704   -0.821
    ## married4                                  0.0166031  0.0014163   11.723
    ## married5                                 -0.0241124  0.0016603  -14.523
    ## year2009                                  0.0120910  0.0018807    6.429
    ## year2010                                  0.0110415  0.0018813    5.869
    ## year2011                                  0.0054468  0.0018674    2.917
    ## year2012                                 -0.0017133  0.0018443   -0.929
    ## year2013                                 -0.0002868  0.0022470   -0.128
    ## year2014                                  0.0019237  0.0022607    0.851
    ## year2015                                 -0.0103867  0.0022819   -4.552
    ## year2016                                 -0.0190828  0.0023118   -8.254
    ## year2017                                 -0.0111196  0.0023757   -4.681
    ##                                          Pr(>|t|)    
    ## (Intercept)                               < 2e-16 ***
    ## raw_income_scale                          < 2e-16 ***
    ## median_income_demo_scale                  < 2e-16 ***
    ## physicians_scale                          < 2e-16 ***
    ## unweighted_pop_county_scale              3.96e-08 ***
    ## median_monthly_housing_cost_county_scale  < 2e-16 ***
    ## land_area_2010_scale                      < 2e-16 ***
    ## education_scale                           < 2e-16 ***
    ## employment_all1                           < 2e-16 ***
    ## sex1                                      < 2e-16 ***
    ## age_scale                                 < 2e-16 ***
    ## race1                                     < 2e-16 ***
    ## race2                                     < 2e-16 ***
    ## race3                                     < 2e-16 ***
    ## race4                                     < 2e-16 ***
    ## married1                                 1.93e-07 ***
    ## married2                                 8.39e-12 ***
    ## married3                                  0.41153    
    ## married4                                  < 2e-16 ***
    ## married5                                  < 2e-16 ***
    ## year2009                                 1.29e-10 ***
    ## year2010                                 4.38e-09 ***
    ## year2011                                  0.00354 ** 
    ## year2012                                  0.35290    
    ## year2013                                  0.89844    
    ## year2014                                  0.39482    
    ## year2015                                 5.32e-06 ***
    ## year2016                                  < 2e-16 ***
    ## year2017                                 2.86e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6794 on 2037734 degrees of freedom
    ##   (1492507 observations deleted due to missingness)
    ## Multiple R-squared:  0.5335, Adjusted R-squared:  0.5335 
    ## F-statistic: 8.324e+04 on 28 and 2037734 DF,  p-value: < 2.2e-16
