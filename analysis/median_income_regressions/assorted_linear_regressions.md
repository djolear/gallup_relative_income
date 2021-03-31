Gallup - Assorted Regression Results
================
Daniel O’Leary
3/30/2021

  - [Demographic Median Income](#demographic-median-income)
      - [Height](#height)
      - [Self-reported health](#self-reported-health)
      - [Life satisfaction](#life-satisfaction)
      - [BMI](#bmi)
  - [Geographic Median Income](#geographic-median-income)
      - [Height](#height-1)
      - [Self-reported health](#self-reported-health-1)
      - [Life satisfaction](#life-satisfaction-1)
      - [BMI](#bmi-1)

``` r
contrasts(dfg_rs$sex) <- contr.sum(2)
contrasts(dfg_rs$employment_all) <- contr.sum(2)
contrasts(dfg_rs$race) <- contr.sum(5)
contrasts(dfg_rs$married) <- contr.sum(6)
```

## Demographic Median Income

### Height

``` r
  lm1 <-
    lmer(
      height_scale ~
        raw_income_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## height_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4206596  4206997 -2103266  4206532  2038678 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0891 -0.6555 -0.0147  0.6409 11.7080 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance  Std.Dev. Corr 
    ##  fips_code (Intercept)      0.0016349 0.04043       
    ##            raw_income_scale 0.0001101 0.01049  -0.01
    ##  Residual                   0.4602786 0.67844       
    ## Number of obs: 2038710, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1595820  0.0027577  -57.867
    ## raw_income_scale                          0.0493141  0.0007163   68.847
    ## total_pop_county_scale                   -0.0130138  0.0042175   -3.086
    ## median_monthly_housing_cost_county_scale -0.0132139  0.0013834   -9.552
    ## land_area_2010_scale                      0.0025165  0.0010831    2.323
    ## physicians_scale                         -0.0030733  0.0010404   -2.954
    ## education_scale                           0.0578784  0.0005496  105.317
    ## employment_all1                          -0.0086490  0.0005699  -15.177
    ## sex1                                      0.6873207  0.0004923 1396.096
    ## age_scale                                -0.1010523  0.0006443 -156.837
    ## race1                                     0.1959373  0.0012094  162.007
    ## race2                                     0.0972750  0.0026692   36.443
    ## race3                                     0.1923291  0.0017373  110.703
    ## race4                                    -0.3049088  0.0028865 -105.633
    ## married1                                  0.0031333  0.0013248    2.365
    ## married2                                  0.0092546  0.0010363    8.931
    ## married3                                  0.0037620  0.0029657    1.268
    ## married4                                  0.0197535  0.0014099   14.010
    ## married5                                 -0.0320776  0.0016482  -19.462
    ## year2009                                  0.0095818  0.0018776    5.103
    ## year2010                                  0.0086944  0.0018799    4.625
    ## year2011                                  0.0025515  0.0018675    1.366
    ## year2012                                 -0.0006165  0.0018457   -0.334
    ## year2013                                  0.0013075  0.0022563    0.579
    ## year2014                                  0.0031639  0.0022674    1.395
    ## year2015                                  0.0015947  0.0022595    0.706
    ## year2016                                  0.0013456  0.0022395    0.601
    ## year2017                                  0.0076130  0.0023154    3.288

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.160    0.00276     -57.9 
    ##  2 fixed  <NA>  raw_income_scale                     0.0493   0.000716     68.8 
    ##  3 fixed  <NA>  total_pop_county_scale              -0.0130   0.00422      -3.09
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -0.0132   0.00138      -9.55
    ##  5 fixed  <NA>  land_area_2010_scale                 0.00252  0.00108       2.32
    ##  6 fixed  <NA>  physicians_scale                    -0.00307  0.00104      -2.95
    ##  7 fixed  <NA>  education_scale                      0.0579   0.000550    105.  
    ##  8 fixed  <NA>  employment_all1                     -0.00865  0.000570    -15.2 
    ##  9 fixed  <NA>  sex1                                 0.687    0.000492   1396.  
    ## 10 fixed  <NA>  age_scale                           -0.101    0.000644   -157.  
    ## # ... with 22 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2103266. 4206596. 4206997. 4206532.     2038678

``` r
  lm1 <-
    lmer(
      height_scale ~
        raw_income_scale +
        median_income_demo_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0808007 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: height_scale ~ raw_income_scale + median_income_demo_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code) + (1 + median_income_demo_scale |  
    ##     fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4202726  4203177 -2101327  4202654  2037727 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.1218 -0.6554 -0.0145  0.6414 11.6596 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              5.863e-06 0.002421      
    ##              raw_income_scale         5.777e-05 0.007600 0.73 
    ##  fips_code.1 (Intercept)              1.648e-03 0.040599      
    ##              median_income_demo_scale 1.727e-04 0.013143 -0.09
    ##  Residual                             4.598e-01 0.678059      
    ## Number of obs: 2037763, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1520925  0.0027694  -54.919
    ## raw_income_scale                          0.0477021  0.0006773   70.425
    ## median_income_demo_scale                  0.0408620  0.0012114   33.730
    ## total_pop_county_scale                   -0.0129540  0.0042269   -3.065
    ## median_monthly_housing_cost_county_scale -0.0129486  0.0013893   -9.320
    ## land_area_2010_scale                      0.0026472  0.0010852    2.439
    ## physicians_scale                         -0.0030604  0.0010443   -2.931
    ## education_scale                           0.0266764  0.0010140   26.309
    ## employment_all1                          -0.0055312  0.0005759   -9.604
    ## sex1                                      0.6785014  0.0005474 1239.465
    ## age_scale                                -0.1004922  0.0006447 -155.873
    ## race1                                     0.1951082  0.0012099  161.262
    ## race2                                     0.0966344  0.0026687   36.210
    ## race3                                     0.1896674  0.0017387  109.084
    ## race4                                    -0.3033281  0.0028881 -105.029
    ## married1                                  0.0083953  0.0013328    6.299
    ## married2                                  0.0054547  0.0010413    5.238
    ## married3                                 -0.0007373  0.0029676   -0.248
    ## married4                                  0.0149112  0.0014155   10.534
    ## married5                                 -0.0248526  0.0016598  -14.973
    ## year2009                                  0.0122800  0.0018781    6.539
    ## year2010                                  0.0093708  0.0018790    4.987
    ## year2011                                  0.0045845  0.0018674    2.455
    ## year2012                                 -0.0036627  0.0018468   -1.983
    ## year2013                                 -0.0010738  0.0022563   -0.476
    ## year2014                                  0.0009862  0.0022672    0.435
    ## year2015                                 -0.0117816  0.0022881   -5.149
    ## year2016                                 -0.0208047  0.0023192   -8.971
    ## year2017                                 -0.0128041  0.0023888   -5.360

    ## 
    ## Correlation matrix not shown by default, as p = 29 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0808007 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 36 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.152    0.00277     -54.9 
    ##  2 fixed  <NA>  raw_income_scale                     0.0477   0.000677     70.4 
    ##  3 fixed  <NA>  median_income_demo_scale             0.0409   0.00121      33.7 
    ##  4 fixed  <NA>  total_pop_county_scale              -0.0130   0.00423      -3.06
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0129   0.00139      -9.32
    ##  6 fixed  <NA>  land_area_2010_scale                 0.00265  0.00109       2.44
    ##  7 fixed  <NA>  physicians_scale                    -0.00306  0.00104      -2.93
    ##  8 fixed  <NA>  education_scale                      0.0267   0.00101      26.3 
    ##  9 fixed  <NA>  employment_all1                     -0.00553  0.000576     -9.60
    ## 10 fixed  <NA>  sex1                                 0.679    0.000547   1239.  
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2101327. 4202726. 4203177. 4202654.     2037727

``` r
  lm1 <-
    lmer(
      height_scale ~
        median_income_demo_scale * raw_income_scale +
        median_income_demo_scale * education_scale +
        median_income_demo_scale * employment_all +
        median_income_demo_scale * sex +
        median_income_demo_scale * age_scale +
        median_income_demo_scale * race +
        median_income_demo_scale * married +
        median_income_demo_scale * year +
        median_income_demo_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0125592 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: height_scale ~ median_income_demo_scale * raw_income_scale +  
    ##     median_income_demo_scale * education_scale + median_income_demo_scale *  
    ##     employment_all + median_income_demo_scale * sex + median_income_demo_scale *  
    ##     age_scale + median_income_demo_scale * race + median_income_demo_scale *  
    ##     married + median_income_demo_scale * year + median_income_demo_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + (1 + raw_income_scale |  
    ##     fips_code) + (1 + median_income_demo_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4200237  4200976 -2100059  4200119  2037704 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0908 -0.6563 -0.0151  0.6403 11.7708 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              6.448e-04 0.02539       
    ##              raw_income_scale         4.901e-05 0.00700  0.08 
    ##  fips_code.1 (Intercept)              9.945e-04 0.03154       
    ##              median_income_demo_scale 1.518e-04 0.01232  -0.11
    ##  Residual                             4.592e-01 0.67765       
    ## Number of obs: 2037763, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error  t value
    ## (Intercept)                               -0.1308835  0.0028307  -46.237
    ## median_income_demo_scale                   0.0315490  0.0022197   14.213
    ## raw_income_scale                           0.0482867  0.0006717   71.886
    ## education_scale                            0.0197362  0.0010521   18.758
    ## employment_all1                           -0.0057157  0.0005783   -9.883
    ## sex1                                       0.6780345  0.0005528 1226.454
    ## age_scale                                 -0.0997262  0.0006767 -147.378
    ## race1                                      0.1854860  0.0012548  147.816
    ## race2                                      0.0909816  0.0027056   33.627
    ## race3                                      0.1787107  0.0017742  100.728
    ## race4                                     -0.2873411  0.0031116  -92.346
    ## married1                                   0.0047417  0.0013579    3.492
    ## married2                                   0.0050613  0.0010634    4.760
    ## married3                                   0.0031033  0.0030499    1.018
    ## married4                                   0.0125513  0.0014373    8.733
    ## married5                                  -0.0246877  0.0019046  -12.962
    ## year2009                                   0.0125219  0.0018922    6.618
    ## year2010                                   0.0105259  0.0018871    5.578
    ## year2011                                   0.0059717  0.0018828    3.172
    ## year2012                                  -0.0029969  0.0018509   -1.619
    ## year2013                                  -0.0002655  0.0022593   -0.117
    ## year2014                                   0.0022451  0.0022753    0.987
    ## year2015                                  -0.0119071  0.0023172   -5.139
    ## year2016                                  -0.0242488  0.0024417   -9.931
    ## year2017                                  -0.0089248  0.0024698   -3.614
    ## total_pop_county_scale                    -0.0121769  0.0042114   -2.891
    ## median_monthly_housing_cost_county_scale  -0.0127393  0.0013850   -9.198
    ## land_area_2010_scale                       0.0025248  0.0010829    2.331
    ## physicians_scale                          -0.0028765  0.0010420   -2.760
    ## median_income_demo_scale:raw_income_scale -0.0038487  0.0006521   -5.902
    ## median_income_demo_scale:education_scale  -0.0139447  0.0006236  -22.363
    ## median_income_demo_scale:employment_all1   0.0029869  0.0006219    4.803
    ## median_income_demo_scale:sex1              0.0046897  0.0005390    8.701
    ## median_income_demo_scale:age_scale        -0.0002015  0.0006915   -0.291
    ## median_income_demo_scale:race1            -0.0017452  0.0012343   -1.414
    ## median_income_demo_scale:race2             0.0253349  0.0027393    9.249
    ## median_income_demo_scale:race3            -0.0129726  0.0017537   -7.397
    ## median_income_demo_scale:race4            -0.0564338  0.0030348  -18.596
    ## median_income_demo_scale:married1         -0.0111898  0.0013865   -8.071
    ## median_income_demo_scale:married2         -0.0001315  0.0010773   -0.122
    ## median_income_demo_scale:married3          0.0047695  0.0029385    1.623
    ## median_income_demo_scale:married4         -0.0024673  0.0015060   -1.638
    ## median_income_demo_scale:married5         -0.0003381  0.0018040   -0.187
    ## median_income_demo_scale:year2009          0.0071678  0.0020087    3.568
    ## median_income_demo_scale:year2010          0.0098845  0.0020061    4.927
    ## median_income_demo_scale:year2011          0.0163851  0.0020044    8.174
    ## median_income_demo_scale:year2012          0.0191974  0.0019980    9.608
    ## median_income_demo_scale:year2013          0.0159169  0.0024045    6.620
    ## median_income_demo_scale:year2014          0.0216707  0.0024514    8.840
    ## median_income_demo_scale:year2015          0.0182061  0.0023557    7.728
    ## median_income_demo_scale:year2016          0.0239775  0.0023150   10.358
    ## median_income_demo_scale:year2017          0.0065727  0.0022407    2.933

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0125592 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 59 x 6
    ##    effect group term                     estimate std.error statistic
    ##    <chr>  <chr> <chr>                       <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)              -0.131    0.00283     -46.2 
    ##  2 fixed  <NA>  median_income_demo_scale  0.0315   0.00222      14.2 
    ##  3 fixed  <NA>  raw_income_scale          0.0483   0.000672     71.9 
    ##  4 fixed  <NA>  education_scale           0.0197   0.00105      18.8 
    ##  5 fixed  <NA>  employment_all1          -0.00572  0.000578     -9.88
    ##  6 fixed  <NA>  sex1                      0.678    0.000553   1226.  
    ##  7 fixed  <NA>  age_scale                -0.0997   0.000677   -147.  
    ##  8 fixed  <NA>  race1                     0.185    0.00125     148.  
    ##  9 fixed  <NA>  race2                     0.0910   0.00271      33.6 
    ## 10 fixed  <NA>  race3                     0.179    0.00177     101.  
    ## # ... with 49 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2100059. 4200237. 4200976. 4200119.     2037704

### Self-reported health

``` r
  lm1 <-
    lmer(
      sr_health_scale ~
        raw_income_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: sr_health_scale ~ raw_income_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     physicians_scale + education_scale + employment_all + sex +  
    ##     age_scale + race + married + year + (1 + raw_income_scale |      fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4706249  4706645 -2353092  4706185  1759278 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7903 -0.7764 -0.0369  0.7118  6.9311 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance Std.Dev. Corr
    ##  fips_code (Intercept)      0.000000 0.00000      
    ##            raw_income_scale 0.002136 0.04622   NaN
    ##  Residual                   0.848865 0.92134      
    ## Number of obs: 1759310, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.1254593  0.0036509   34.364
    ## raw_income_scale                         -0.1913819  0.0015096 -126.777
    ## total_pop_county_scale                    0.0094034  0.0007967   11.803
    ## median_monthly_housing_cost_county_scale -0.0243090  0.0008472  -28.694
    ## land_area_2010_scale                     -0.0064187  0.0007373   -8.706
    ## physicians_scale                         -0.0139153  0.0007916  -17.578
    ## education_scale                          -0.1443120  0.0007986 -180.716
    ## employment_all1                           0.1345925  0.0008266  162.829
    ## sex1                                      0.0395478  0.0007195   54.965
    ## age_scale                                 0.0910080  0.0009367   97.159
    ## race1                                    -0.1209928  0.0017290  -69.980
    ## race2                                     0.0484737  0.0038993   12.431
    ## race3                                    -0.0174554  0.0024752   -7.052
    ## race4                                     0.0262743  0.0041766    6.291
    ## married1                                 -0.0606071  0.0019295  -31.411
    ## married2                                 -0.0770093  0.0015040  -51.204
    ## married3                                  0.1611770  0.0042891   37.578
    ## married4                                  0.0518395  0.0020617   25.144
    ## married5                                 -0.0989412  0.0023987  -41.248
    ## year2009                                  0.0116066  0.0036117    3.214
    ## year2010                                  0.0331346  0.0036130    9.171
    ## year2011                                  0.0223215  0.0035983    6.203
    ## year2012                                  0.0271122  0.0035764    7.581
    ## year2013                                  0.0629775  0.0039745   15.845
    ## year2014                                  0.0649184  0.0039870   16.283
    ## year2015                                  0.0709899  0.0039818   17.828
    ## year2016                                  0.0787407  0.0039600   19.884
    ## year2017                                  0.1153983  0.0085479   13.500

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm1)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          0.125    0.00365      34.4 
    ##  2 fixed  <NA>  raw_income_scale                    -0.191    0.00151    -127.  
    ##  3 fixed  <NA>  total_pop_county_scale               0.00940  0.000797     11.8 
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -0.0243   0.000847    -28.7 
    ##  5 fixed  <NA>  land_area_2010_scale                -0.00642  0.000737     -8.71
    ##  6 fixed  <NA>  physicians_scale                    -0.0139   0.000792    -17.6 
    ##  7 fixed  <NA>  education_scale                     -0.144    0.000799   -181.  
    ##  8 fixed  <NA>  employment_all1                      0.135    0.000827    163.  
    ##  9 fixed  <NA>  sex1                                 0.0395   0.000720     55.0 
    ## 10 fixed  <NA>  age_scale                            0.0910   0.000937     97.2 
    ## # ... with 22 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.921 -2353092. 4706249. 4706645. 4706185.     1759278

``` r
  lm1 <-
    lmer(
      sr_health_scale ~
        raw_income_scale +
        median_income_demo_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0105079 (tol = 0.002, component 1)

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: sr_health_scale ~ raw_income_scale + median_income_demo_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code) + (1 + median_income_demo_scale |  
    ##     fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4699236  4699682 -2349582  4699164  1759176 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8006 -0.7749 -0.0390  0.7100  6.8475 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              3.781e-03 0.061489      
    ##              raw_income_scale         1.209e-03 0.034773 -0.45
    ##  fips_code.1 (Intercept)              4.095e-05 0.006399      
    ##              median_income_demo_scale 4.274e-04 0.020673 -0.95
    ##  Residual                             8.444e-01 0.918913      
    ## Number of obs: 1759212, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.1493088  0.0046574   32.058
    ## raw_income_scale                         -0.1900001  0.0013455 -141.206
    ## median_income_demo_scale                  0.1020539  0.0018598   54.873
    ## total_pop_county_scale                    0.0293540  0.0058395    5.027
    ## median_monthly_housing_cost_county_scale -0.0327865  0.0020110  -16.303
    ## land_area_2010_scale                     -0.0069533  0.0015795   -4.402
    ## physicians_scale                         -0.0124202  0.0015245   -8.147
    ## education_scale                          -0.2200528  0.0015307 -143.764
    ## employment_all1                           0.1420697  0.0008363  169.884
    ## sex1                                      0.0184205  0.0008015   22.982
    ## age_scale                                 0.0928145  0.0009389   98.851
    ## race1                                    -0.1174479  0.0017685  -66.412
    ## race2                                     0.0479202  0.0039066   12.266
    ## race3                                    -0.0263140  0.0025453  -10.338
    ## race4                                     0.0281492  0.0041993    6.703
    ## married1                                 -0.0454407  0.0019422  -23.396
    ## married2                                 -0.0865121  0.0015135  -57.162
    ## married3                                  0.1474776  0.0042856   34.413
    ## married4                                  0.0394505  0.0020685   19.072
    ## married5                                 -0.0813450  0.0024151  -33.682
    ## year2009                                  0.0185680  0.0036056    5.150
    ## year2010                                  0.0349170  0.0036057    9.684
    ## year2011                                  0.0285784  0.0035943    7.951
    ## year2012                                  0.0210718  0.0035758    5.893
    ## year2013                                  0.0580136  0.0039817   14.570
    ## year2014                                  0.0599239  0.0039908   15.015
    ## year2015                                  0.0381592  0.0040232    9.485
    ## year2016                                  0.0248744  0.0040698    6.112
    ## year2017                                  0.0671972  0.0086080    7.806

    ## 
    ## Correlation matrix not shown by default, as p = 29 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0105079 (tol = 0.002, component 1)

``` r
tidy(lm1)
```

    ## # A tibble: 36 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          0.149    0.00466      32.1 
    ##  2 fixed  <NA>  raw_income_scale                    -0.190    0.00135    -141.  
    ##  3 fixed  <NA>  median_income_demo_scale             0.102    0.00186      54.9 
    ##  4 fixed  <NA>  total_pop_county_scale               0.0294   0.00584       5.03
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0328   0.00201     -16.3 
    ##  6 fixed  <NA>  land_area_2010_scale                -0.00695  0.00158      -4.40
    ##  7 fixed  <NA>  physicians_scale                    -0.0124   0.00152      -8.15
    ##  8 fixed  <NA>  education_scale                     -0.220    0.00153    -144.  
    ##  9 fixed  <NA>  employment_all1                      0.142    0.000836    170.  
    ## 10 fixed  <NA>  sex1                                 0.0184   0.000802     23.0 
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.919 -2349582. 4699236. 4699682. 4699164.     1759176

``` r
  lm1 <-
    lmer(
      sr_health_scale ~
        median_income_demo_scale * raw_income_scale +
        median_income_demo_scale * education_scale +
        median_income_demo_scale * employment_all +
        median_income_demo_scale * sex +
        median_income_demo_scale * age_scale +
        median_income_demo_scale * race +
        median_income_demo_scale * married +
        median_income_demo_scale * year +
        median_income_demo_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: sr_health_scale ~ median_income_demo_scale * raw_income_scale +  
    ##     median_income_demo_scale * education_scale + median_income_demo_scale *  
    ##     employment_all + median_income_demo_scale * sex + median_income_demo_scale *  
    ##     age_scale + median_income_demo_scale * race + median_income_demo_scale *  
    ##     married + median_income_demo_scale * year + median_income_demo_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + (1 + raw_income_scale |  
    ##     fips_code) + (1 + median_income_demo_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4695833  4696564 -2347858  4695715  1759153 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0035 -0.7767 -0.0437  0.7096  6.8471 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0037225 0.06101       
    ##              raw_income_scale         0.0010453 0.03233  -0.44
    ##  fips_code.1 (Intercept)              0.0000000 0.00000       
    ##              median_income_demo_scale 0.0003723 0.01930   NaN 
    ##  Residual                             0.8428383 0.91806       
    ## Number of obs: 1759212, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error  t value
    ## (Intercept)                                0.1237725  0.0047449   26.085
    ## median_income_demo_scale                   0.0728947  0.0041979   17.365
    ## raw_income_scale                          -0.1881610  0.0013093 -143.716
    ## education_scale                           -0.2128588  0.0015913 -133.763
    ## employment_all1                            0.1422287  0.0008414  169.044
    ## sex1                                       0.0195822  0.0008084   24.223
    ## age_scale                                  0.0903286  0.0009972   90.578
    ## race1                                     -0.1069352  0.0018257  -58.571
    ## race2                                      0.0593939  0.0039795   14.925
    ## race3                                     -0.0109453  0.0026003   -4.209
    ## race4                                      0.0201344  0.0044462    4.528
    ## married1                                  -0.0354170  0.0019918  -17.781
    ## married2                                  -0.0873140  0.0015509  -56.299
    ## married3                                   0.1236193  0.0044573   27.734
    ## married4                                   0.0405561  0.0021064   19.253
    ## married5                                  -0.0560732  0.0027994  -20.030
    ## year2009                                   0.0173776  0.0036202    4.800
    ## year2010                                   0.0348224  0.0036146    9.634
    ## year2011                                   0.0303388  0.0036108    8.402
    ## year2012                                   0.0234431  0.0035808    6.547
    ## year2013                                   0.0600851  0.0039858   15.075
    ## year2014                                   0.0620248  0.0040013   15.501
    ## year2015                                   0.0363279  0.0040498    8.970
    ## year2016                                   0.0211502  0.0041849    5.054
    ## year2017                                   0.0700729  0.0090810    7.716
    ## total_pop_county_scale                     0.0278580  0.0058530    4.760
    ## median_monthly_housing_cost_county_scale  -0.0347044  0.0020084  -17.280
    ## land_area_2010_scale                      -0.0067767  0.0015735   -4.307
    ## physicians_scale                          -0.0125905  0.0015216   -8.275
    ## median_income_demo_scale:raw_income_scale  0.0148960  0.0009692   15.369
    ## median_income_demo_scale:education_scale   0.0069995  0.0009299    7.527
    ## median_income_demo_scale:employment_all1  -0.0049885  0.0009101   -5.481
    ## median_income_demo_scale:sex1              0.0067389  0.0007951    8.475
    ## median_income_demo_scale:age_scale         0.0050500  0.0010279    4.913
    ## median_income_demo_scale:race1             0.0045553  0.0018165    2.508
    ## median_income_demo_scale:race2             0.0007423  0.0040227    0.185
    ## median_income_demo_scale:race3             0.0222988  0.0025947    8.594
    ## median_income_demo_scale:race4             0.0465318  0.0044222   10.522
    ## median_income_demo_scale:married1          0.0523270  0.0020380   25.676
    ## median_income_demo_scale:married2         -0.0028217  0.0015753   -1.791
    ## median_income_demo_scale:married3         -0.0520025  0.0042654  -12.192
    ## median_income_demo_scale:married4         -0.0236230  0.0022258  -10.613
    ## median_income_demo_scale:married5          0.0444091  0.0026708   16.627
    ## median_income_demo_scale:year2009          0.0071832  0.0038588    1.861
    ## median_income_demo_scale:year2010          0.0126628  0.0038560    3.284
    ## median_income_demo_scale:year2011          0.0188634  0.0038530    4.896
    ## median_income_demo_scale:year2012          0.0203911  0.0038461    5.302
    ## median_income_demo_scale:year2013          0.0233101  0.0042441    5.492
    ## median_income_demo_scale:year2014          0.0259146  0.0042885    6.043
    ## median_income_demo_scale:year2015          0.0314813  0.0041940    7.506
    ## median_income_demo_scale:year2016          0.0278470  0.0041532    6.705
    ## median_income_demo_scale:year2017          0.0088497  0.0078801    1.123

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm1)
```

    ## # A tibble: 59 x 6
    ##    effect group term                     estimate std.error statistic
    ##    <chr>  <chr> <chr>                       <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                0.124   0.00474      26.1 
    ##  2 fixed  <NA>  median_income_demo_scale   0.0729  0.00420      17.4 
    ##  3 fixed  <NA>  raw_income_scale          -0.188   0.00131    -144.  
    ##  4 fixed  <NA>  education_scale           -0.213   0.00159    -134.  
    ##  5 fixed  <NA>  employment_all1            0.142   0.000841    169.  
    ##  6 fixed  <NA>  sex1                       0.0196  0.000808     24.2 
    ##  7 fixed  <NA>  age_scale                  0.0903  0.000997     90.6 
    ##  8 fixed  <NA>  race1                     -0.107   0.00183     -58.6 
    ##  9 fixed  <NA>  race2                      0.0594  0.00398      14.9 
    ## 10 fixed  <NA>  race3                     -0.0109  0.00260      -4.21
    ## # ... with 49 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.918 -2347858. 4695833. 4696564. 4695715.     1759153

### Life satisfaction

``` r
  lm1 <-
    lmer(
      ladder_now_scale ~
        raw_income_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: ladder_now_scale ~ raw_income_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     physicians_scale + education_scale + employment_all + sex +  
    ##     age_scale + race + married + year + (1 + raw_income_scale |      fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5444243  5444643 -2722089  5444179  2001166 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1968 -0.6165  0.1020  0.6492  2.8296 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance Std.Dev. Corr 
    ##  fips_code (Intercept)      0.002303 0.04799       
    ##            raw_income_scale 0.001251 0.03537  -0.63
    ##  Residual                   0.887792 0.94223       
    ## Number of obs: 2001198, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.2300709  0.0033128  -69.449
    ## raw_income_scale                          0.2278584  0.0013021  174.990
    ## total_pop_county_scale                   -0.0281208  0.0041256   -6.816
    ## median_monthly_housing_cost_county_scale -0.0263221  0.0015983  -16.468
    ## land_area_2010_scale                      0.0082043  0.0013810    5.941
    ## physicians_scale                          0.0090900  0.0012899    7.047
    ## education_scale                           0.0560988  0.0007704   72.819
    ## employment_all1                          -0.0129970  0.0007986  -16.275
    ## sex1                                     -0.0740315  0.0006901 -107.274
    ## age_scale                                 0.0928025  0.0009028  102.799
    ## race1                                    -0.0628912  0.0016922  -37.165
    ## race2                                    -0.0744779  0.0037565  -19.827
    ## race3                                     0.0345605  0.0024379   14.177
    ## race4                                    -0.0331442  0.0040229   -8.239
    ## married1                                  0.0673763  0.0018537   36.347
    ## married2                                  0.1246342  0.0014484   86.050
    ## married3                                 -0.2295596  0.0041489  -55.330
    ## married4                                 -0.1107590  0.0019829  -55.856
    ## married5                                  0.1041590  0.0023205   44.886
    ## year2009                                  0.1725526  0.0027221   63.390
    ## year2010                                  0.1811520  0.0026158   69.252
    ## year2011                                  0.1888168  0.0025960   72.732
    ## year2012                                  0.1657121  0.0025654   64.595
    ## year2013                                  0.1879958  0.0031298   60.066
    ## year2014                                  0.2005614  0.0031453   63.765
    ## year2015                                  0.2055081  0.0031357   65.539
    ## year2016                                  0.1934066  0.0031070   62.250
    ## year2017                                  0.2061974  0.0032068   64.300

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.230    0.00331     -69.4 
    ##  2 fixed  <NA>  raw_income_scale                     0.228    0.00130     175.  
    ##  3 fixed  <NA>  total_pop_county_scale              -0.0281   0.00413      -6.82
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -0.0263   0.00160     -16.5 
    ##  5 fixed  <NA>  land_area_2010_scale                 0.00820  0.00138       5.94
    ##  6 fixed  <NA>  physicians_scale                     0.00909  0.00129       7.05
    ##  7 fixed  <NA>  education_scale                      0.0561   0.000770     72.8 
    ##  8 fixed  <NA>  employment_all1                     -0.0130   0.000799    -16.3 
    ##  9 fixed  <NA>  sex1                                -0.0740   0.000690   -107.  
    ## 10 fixed  <NA>  age_scale                            0.0928   0.000903    103.  
    ## # ... with 22 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.942 -2722089. 5444243. 5444643. 5444179.     2001166

``` r
  lm1 <-
    lmer(
      ladder_now_scale ~
        raw_income_scale +
        median_income_demo_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0876039 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: ladder_now_scale ~ raw_income_scale + median_income_demo_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code) + (1 + median_income_demo_scale |  
    ##     fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5427915  5428365 -2713922  5427843  2000222 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4706 -0.6166  0.1036  0.6514  2.8535 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0014993 0.03872       
    ##              raw_income_scale         0.0010936 0.03307  -0.72
    ##  fips_code.1 (Intercept)              0.0005816 0.02412       
    ##              median_income_demo_scale 0.0002434 0.01560  -0.12
    ##  Residual                             0.8816313 0.93895       
    ## Number of obs: 2000258, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.2612118  0.0032761  -79.732
    ## raw_income_scale                          0.2334072  0.0012760  182.914
    ## median_income_demo_scale                 -0.1788452  0.0016687 -107.178
    ## total_pop_county_scale                   -0.0265691  0.0040167   -6.615
    ## median_monthly_housing_cost_county_scale -0.0260071  0.0015657  -16.611
    ## land_area_2010_scale                      0.0078054  0.0013580    5.748
    ## physicians_scale                          0.0083290  0.0012683    6.567
    ## education_scale                           0.1940821  0.0014174  136.931
    ## employment_all1                          -0.0267723  0.0008047  -33.268
    ## sex1                                     -0.0354549  0.0007644  -46.383
    ## age_scale                                 0.0904833  0.0009005  100.483
    ## race1                                    -0.0591220  0.0016871  -35.044
    ## race2                                    -0.0722089  0.0037440  -19.287
    ## race3                                     0.0456226  0.0024314   18.764
    ## race4                                    -0.0397676  0.0040126   -9.911
    ## married1                                  0.0437294  0.0018593   23.519
    ## married2                                  0.1415536  0.0014513   97.536
    ## married3                                 -0.2102279  0.0041391  -50.791
    ## married4                                 -0.0898041  0.0019850  -45.241
    ## married5                                  0.0729254  0.0023296   31.303
    ## year2009                                  0.1607193  0.0027147   59.204
    ## year2010                                  0.1782167  0.0026070   68.362
    ## year2011                                  0.1797904  0.0025882   69.465
    ## year2012                                  0.1790549  0.0025591   69.968
    ## year2013                                  0.1985366  0.0031200   63.633
    ## year2014                                  0.2102697  0.0031354   67.063
    ## year2015                                  0.2646862  0.0031662   83.598
    ## year2016                                  0.2913102  0.0032092   90.772
    ## year2017                                  0.2983108  0.0032992   90.419

    ## 
    ## Correlation matrix not shown by default, as p = 29 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0876039 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 36 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.261    0.00328     -79.7 
    ##  2 fixed  <NA>  raw_income_scale                     0.233    0.00128     183.  
    ##  3 fixed  <NA>  median_income_demo_scale            -0.179    0.00167    -107.  
    ##  4 fixed  <NA>  total_pop_county_scale              -0.0266   0.00402      -6.61
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0260   0.00157     -16.6 
    ##  6 fixed  <NA>  land_area_2010_scale                 0.00781  0.00136       5.75
    ##  7 fixed  <NA>  physicians_scale                     0.00833  0.00127       6.57
    ##  8 fixed  <NA>  education_scale                      0.194    0.00142     137.  
    ##  9 fixed  <NA>  employment_all1                     -0.0268   0.000805    -33.3 
    ## 10 fixed  <NA>  sex1                                -0.0355   0.000764    -46.4 
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.939 -2713922. 5427915. 5428365. 5427843.     2000222

``` r
  lm1 <-
    lmer(
      ladder_now_scale ~
        median_income_demo_scale * raw_income_scale +
        median_income_demo_scale * education_scale +
        median_income_demo_scale * employment_all +
        median_income_demo_scale * sex +
        median_income_demo_scale * age_scale +
        median_income_demo_scale * race +
        median_income_demo_scale * married +
        median_income_demo_scale * year +
        median_income_demo_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.023627 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: ladder_now_scale ~ median_income_demo_scale * raw_income_scale +  
    ##     median_income_demo_scale * education_scale + median_income_demo_scale *  
    ##     employment_all + median_income_demo_scale * sex + median_income_demo_scale *  
    ##     age_scale + median_income_demo_scale * race + median_income_demo_scale *  
    ##     married + median_income_demo_scale * year + median_income_demo_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + (1 + raw_income_scale |  
    ##     fips_code) + (1 + median_income_demo_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5421807  5422545 -2710845  5421689  2000199 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4282 -0.6158  0.1010  0.6506  2.9435 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0011980 0.03461       
    ##              raw_income_scale         0.0011446 0.03383  -0.78
    ##  fips_code.1 (Intercept)              0.0008518 0.02919       
    ##              median_income_demo_scale 0.0001651 0.01285  -0.33
    ##  Residual                             0.8789593 0.93753       
    ## Number of obs: 2000258, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error t value
    ## (Intercept)                               -2.966e-01  3.361e-03 -88.251
    ## median_income_demo_scale                  -1.885e-01  3.055e-03 -61.691
    ## raw_income_scale                           2.314e-01  1.287e-03 179.814
    ## education_scale                            2.069e-01  1.469e-03 140.918
    ## employment_all1                           -2.815e-02  8.078e-04 -34.849
    ## sex1                                      -3.558e-02  7.713e-04 -46.135
    ## age_scale                                  9.337e-02  9.434e-04  98.968
    ## race1                                     -5.592e-02  1.749e-03 -31.971
    ## race2                                     -7.334e-02  3.794e-03 -19.332
    ## race3                                      4.261e-02  2.477e-03  17.199
    ## race4                                     -2.264e-02  4.319e-03  -5.241
    ## married1                                   4.900e-02  1.895e-03  25.853
    ## married2                                   1.471e-01  1.482e-03  99.273
    ## married3                                  -2.162e-01  4.264e-03 -50.694
    ## married4                                  -7.866e-02  2.014e-03 -39.058
    ## married5                                   5.015e-02  2.664e-03  18.826
    ## year2009                                   1.567e-01  2.734e-03  57.335
    ## year2010                                   1.729e-01  2.616e-03  66.101
    ## year2011                                   1.718e-01  2.608e-03  65.876
    ## year2012                                   1.765e-01  2.562e-03  68.874
    ## year2013                                   1.956e-01  3.121e-03  62.684
    ## year2014                                   2.080e-01  3.144e-03  66.170
    ## year2015                                   2.614e-01  3.201e-03  81.662
    ## year2016                                   2.898e-01  3.369e-03  86.004
    ## year2017                                   2.721e-01  3.401e-03  80.012
    ## total_pop_county_scale                    -2.871e-02  3.903e-03  -7.354
    ## median_monthly_housing_cost_county_scale  -2.805e-02  1.544e-03 -18.168
    ## land_area_2010_scale                       8.221e-03  1.348e-03   6.099
    ## physicians_scale                           6.999e-03  1.256e-03   5.572
    ## median_income_demo_scale:raw_income_scale  1.272e-02  9.136e-04  13.927
    ## median_income_demo_scale:education_scale   2.420e-02  8.696e-04  27.824
    ## median_income_demo_scale:employment_all1   1.471e-04  8.626e-04   0.171
    ## median_income_demo_scale:sex1              7.849e-03  7.500e-04  10.465
    ## median_income_demo_scale:age_scale         8.889e-06  9.618e-04   0.009
    ## median_income_demo_scale:race1             3.796e-02  1.713e-03  22.163
    ## median_income_demo_scale:race2             6.008e-03  3.826e-03   1.570
    ## median_income_demo_scale:race3            -1.676e-02  2.440e-03  -6.867
    ## median_income_demo_scale:race4            -2.557e-02  4.204e-03  -6.082
    ## median_income_demo_scale:married1         -2.231e-02  1.922e-03 -11.608
    ## median_income_demo_scale:married2          1.863e-02  1.487e-03  12.530
    ## median_income_demo_scale:married3         -2.082e-02  4.057e-03  -5.131
    ## median_income_demo_scale:married4          3.117e-02  2.106e-03  14.799
    ## median_income_demo_scale:married5         -2.046e-02  2.523e-03  -8.109
    ## median_income_demo_scale:year2009         -3.679e-02  2.897e-03 -12.701
    ## median_income_demo_scale:year2010         -4.394e-02  2.778e-03 -15.818
    ## median_income_demo_scale:year2011         -5.409e-02  2.772e-03 -19.515
    ## median_income_demo_scale:year2012         -6.674e-02  2.762e-03 -24.164
    ## median_income_demo_scale:year2013         -2.757e-02  3.321e-03  -8.300
    ## median_income_demo_scale:year2014         -2.743e-02  3.380e-03  -8.113
    ## median_income_demo_scale:year2015         -2.887e-02  3.251e-03  -8.882
    ## median_income_demo_scale:year2016         -3.323e-02  3.193e-03 -10.407
    ## median_income_demo_scale:year2017          2.743e-02  3.088e-03   8.883

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.023627 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 59 x 6
    ##    effect group term                     estimate std.error statistic
    ##    <chr>  <chr> <chr>                       <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)               -0.297   0.00336      -88.3
    ##  2 fixed  <NA>  median_income_demo_scale  -0.188   0.00306      -61.7
    ##  3 fixed  <NA>  raw_income_scale           0.231   0.00129      180. 
    ##  4 fixed  <NA>  education_scale            0.207   0.00147      141. 
    ##  5 fixed  <NA>  employment_all1           -0.0282  0.000808     -34.8
    ##  6 fixed  <NA>  sex1                      -0.0356  0.000771     -46.1
    ##  7 fixed  <NA>  age_scale                  0.0934  0.000943      99.0
    ##  8 fixed  <NA>  race1                     -0.0559  0.00175      -32.0
    ##  9 fixed  <NA>  race2                     -0.0733  0.00379      -19.3
    ## 10 fixed  <NA>  race3                      0.0426  0.00248       17.2
    ## # ... with 49 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.938 -2710845. 5421807. 5422545. 5421689.     2000199

### BMI

``` r
  lm1 <-
    lmer(
      bmi_scale ~
        raw_income_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## bmi_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5645629  5646029 -2822782  5645565  2006480 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7817 -0.6609 -0.1629  0.4716 21.9436 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance  Std.Dev. Corr 
    ##  fips_code (Intercept)      0.0059793 0.07733       
    ##            raw_income_scale 0.0004899 0.02213  -0.32
    ##  Residual                   0.9741350 0.98698       
    ## Number of obs: 2006512, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error t value
    ## (Intercept)                               0.0037955  0.0046029   0.825
    ## raw_income_scale                         -0.0551088  0.0011488 -47.972
    ## total_pop_county_scale                   -0.0321206  0.0075331  -4.264
    ## median_monthly_housing_cost_county_scale -0.0515998  0.0023745 -21.731
    ## land_area_2010_scale                     -0.0049639  0.0017586  -2.823
    ## physicians_scale                         -0.0158059  0.0017072  -9.258
    ## education_scale                          -0.0525866  0.0008064 -65.208
    ## employment_all1                           0.0002408  0.0008368   0.288
    ## sex1                                      0.0975366  0.0007229 134.924
    ## age_scale                                 0.0446001  0.0009444  47.226
    ## race1                                    -0.0348819  0.0017758 -19.643
    ## race2                                     0.0651414  0.0039166  16.632
    ## race3                                     0.2739800  0.0025550 107.232
    ## race4                                    -0.3634055  0.0042284 -85.945
    ## married1                                 -0.0675754  0.0019431 -34.778
    ## married2                                  0.0615606  0.0015196  40.510
    ## married3                                  0.0843657  0.0043452  19.416
    ## married4                                  0.0525929  0.0020685  25.426
    ## married5                                 -0.1098326  0.0024185 -45.413
    ## year2009                                  0.0120303  0.0027522   4.371
    ## year2010                                  0.0162143  0.0027572   5.881
    ## year2011                                  0.0005592  0.0027406   0.204
    ## year2012                                  0.0118013  0.0027091   4.356
    ## year2013                                  0.0245634  0.0033144   7.411
    ## year2014                                  0.0445972  0.0033303  13.391
    ## year2015                                  0.0661906  0.0033179  19.950
    ## year2016                                  0.0666275  0.0032896  20.254
    ## year2017                                  0.0710763  0.0034040  20.880

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          3.80e-3  0.00460      0.825
    ##  2 fixed  <NA>  raw_income_scale                    -5.51e-2  0.00115    -48.0  
    ##  3 fixed  <NA>  total_pop_county_scale              -3.21e-2  0.00753     -4.26 
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -5.16e-2  0.00237    -21.7  
    ##  5 fixed  <NA>  land_area_2010_scale                -4.96e-3  0.00176     -2.82 
    ##  6 fixed  <NA>  physicians_scale                    -1.58e-2  0.00171     -9.26 
    ##  7 fixed  <NA>  education_scale                     -5.26e-2  0.000806   -65.2  
    ##  8 fixed  <NA>  employment_all1                      2.41e-4  0.000837     0.288
    ##  9 fixed  <NA>  sex1                                 9.75e-2  0.000723   135.   
    ## 10 fixed  <NA>  age_scale                            4.46e-2  0.000944    47.2  
    ## # ... with 22 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.987 -2822782. 5645629. 5646029. 5645565.     2006480

``` r
  lm1 <-
    lmer(
      bmi_scale ~
        raw_income_scale +
        median_income_demo_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## bmi_scale ~ raw_income_scale + median_income_demo_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     physicians_scale + education_scale + employment_all + sex +  
    ##     age_scale + race + married + year + (1 + raw_income_scale |  
    ##     fips_code) + (1 + median_income_demo_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5625500  5625951 -2812714  5625428  2005544 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6778 -0.6601 -0.1606  0.4721 22.1610 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0048492 0.06964       
    ##              raw_income_scale         0.0004320 0.02078  -0.51
    ##  fips_code.1 (Intercept)              0.0008374 0.02894       
    ##              median_income_demo_scale 0.0007422 0.02724  0.98 
    ##  Residual                             0.9653878 0.98254       
    ## Number of obs: 2005580, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.0536783  0.0043466   12.349
    ## raw_income_scale                         -0.0651291  0.0011292  -57.675
    ## median_income_demo_scale                  0.2203877  0.0018461  119.382
    ## total_pop_county_scale                   -0.0147839  0.0066467   -2.224
    ## median_monthly_housing_cost_county_scale -0.0471888  0.0022695  -20.793
    ## land_area_2010_scale                     -0.0042988  0.0017109   -2.513
    ## physicians_scale                         -0.0152904  0.0016629   -9.195
    ## education_scale                          -0.2156664  0.0014806 -145.659
    ## employment_all1                           0.0163033  0.0008424   19.354
    ## sex1                                      0.0514604  0.0008013   64.222
    ## age_scale                                 0.0480859  0.0009412   51.089
    ## race1                                    -0.0377631  0.0017689  -21.348
    ## race2                                     0.0621449  0.0038999   15.935
    ## race3                                     0.2618050  0.0025465  102.808
    ## race4                                    -0.3526167  0.0042147  -83.663
    ## married1                                 -0.0388446  0.0019476  -19.945
    ## married2                                  0.0411220  0.0015212   27.032
    ## married3                                  0.0604080  0.0043311   13.948
    ## married4                                  0.0273454  0.0020687   13.218
    ## married5                                 -0.0722382  0.0024262  -29.774
    ## year2009                                  0.0260595  0.0027421    9.503
    ## year2010                                  0.0198119  0.0027452    7.217
    ## year2011                                  0.0105114  0.0027294    3.851
    ## year2012                                 -0.0046981  0.0026994   -1.740
    ## year2013                                  0.0112682  0.0032999    3.415
    ## year2014                                  0.0321515  0.0033157    9.697
    ## year2015                                 -0.0047785  0.0033453   -1.428
    ## year2016                                 -0.0497960  0.0033907  -14.686
    ## year2017                                 -0.0376243  0.0034949  -10.766

    ## 
    ## Correlation matrix not shown by default, as p = 29 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## unable to evaluate scaled gradient
    ## Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

``` r
tidy(lm1)
```

    ## # A tibble: 36 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          0.0537   0.00435      12.3 
    ##  2 fixed  <NA>  raw_income_scale                    -0.0651   0.00113     -57.7 
    ##  3 fixed  <NA>  median_income_demo_scale             0.220    0.00185     119.  
    ##  4 fixed  <NA>  total_pop_county_scale              -0.0148   0.00665      -2.22
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0472   0.00227     -20.8 
    ##  6 fixed  <NA>  land_area_2010_scale                -0.00430  0.00171      -2.51
    ##  7 fixed  <NA>  physicians_scale                    -0.0153   0.00166      -9.19
    ##  8 fixed  <NA>  education_scale                     -0.216    0.00148    -146.  
    ##  9 fixed  <NA>  employment_all1                      0.0163   0.000842     19.4 
    ## 10 fixed  <NA>  sex1                                 0.0515   0.000801     64.2 
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.983 -2812714. 5625500. 5625951. 5625428.     2005544

``` r
  lm1 <-
    lmer(
      bmi_scale ~
        median_income_demo_scale * raw_income_scale +
        median_income_demo_scale * education_scale +
        median_income_demo_scale * employment_all +
        median_income_demo_scale * sex +
        median_income_demo_scale * age_scale +
        median_income_demo_scale * race +
        median_income_demo_scale * married +
        median_income_demo_scale * year +
        median_income_demo_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0172521 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## bmi_scale ~ median_income_demo_scale * raw_income_scale + median_income_demo_scale *  
    ##     education_scale + median_income_demo_scale * employment_all +  
    ##     median_income_demo_scale * sex + median_income_demo_scale *  
    ##     age_scale + median_income_demo_scale * race + median_income_demo_scale *  
    ##     married + median_income_demo_scale * year + median_income_demo_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + (1 + raw_income_scale |  
    ##     fips_code) + (1 + median_income_demo_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5616015  5616753 -2807948  5615897  2005521 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8487 -0.6589 -0.1615  0.4712 22.6291 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0019704 0.04439       
    ##              raw_income_scale         0.0004318 0.02078  -0.82
    ##  fips_code.1 (Intercept)              0.0037779 0.06146       
    ##              median_income_demo_scale 0.0007717 0.02778  0.45 
    ##  Residual                             0.9607844 0.98020       
    ## Number of obs: 2005580, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error  t value
    ## (Intercept)                                0.0562273  0.0044363   12.674
    ## median_income_demo_scale                   0.2161226  0.0033014   65.463
    ## raw_income_scale                          -0.0632947  0.0011300  -56.012
    ## education_scale                           -0.2276777  0.0015333 -148.487
    ## employment_all1                            0.0151704  0.0008444   17.966
    ## sex1                                       0.0485952  0.0008077   60.164
    ## age_scale                                  0.0471157  0.0009860   47.783
    ## race1                                     -0.0398272  0.0018317  -21.743
    ## race2                                      0.0598159  0.0039471   15.154
    ## race3                                      0.2618307  0.0025946  100.913
    ## race4                                     -0.3406497  0.0045327  -75.154
    ## married1                                  -0.0353089  0.0019808  -17.825
    ## married2                                   0.0410253  0.0015507   26.456
    ## married3                                   0.0351696  0.0044429    7.916
    ## married4                                   0.0254627  0.0020969   12.143
    ## married5                                  -0.0364371  0.0027797  -13.108
    ## year2009                                   0.0272823  0.0027572    9.895
    ## year2010                                   0.0208046  0.0027517    7.561
    ## year2011                                   0.0132772  0.0027466    4.834
    ## year2012                                  -0.0026551  0.0027004   -0.983
    ## year2013                                   0.0113495  0.0032985    3.441
    ## year2014                                   0.0310414  0.0033214    9.346
    ## year2015                                  -0.0086707  0.0033823   -2.564
    ## year2016                                  -0.0532983  0.0035631  -14.959
    ## year2017                                  -0.0129129  0.0036073   -3.580
    ## total_pop_county_scale                    -0.0154329  0.0066812   -2.310
    ## median_monthly_housing_cost_county_scale  -0.0458605  0.0022762  -20.148
    ## land_area_2010_scale                      -0.0045510  0.0017129   -2.657
    ## physicians_scale                          -0.0153769  0.0016655   -9.232
    ## median_income_demo_scale:raw_income_scale  0.0037864  0.0009552    3.964
    ## median_income_demo_scale:education_scale  -0.0155893  0.0009104  -17.124
    ## median_income_demo_scale:employment_all1  -0.0159168  0.0009071  -17.548
    ## median_income_demo_scale:sex1              0.0522446  0.0007867   66.407
    ## median_income_demo_scale:age_scale         0.0299316  0.0010076   29.705
    ## median_income_demo_scale:race1             0.0225707  0.0018084   12.481
    ## median_income_demo_scale:race2            -0.0091448  0.0039938   -2.290
    ## median_income_demo_scale:race3            -0.0147066  0.0025681   -5.727
    ## median_income_demo_scale:race4             0.0047462  0.0044236    1.073
    ## median_income_demo_scale:married1          0.0803933  0.0020215   39.769
    ## median_income_demo_scale:married2         -0.0298024  0.0015714  -18.966
    ## median_income_demo_scale:married3         -0.0580313  0.0042777  -13.566
    ## median_income_demo_scale:married4         -0.0312676  0.0021950  -14.245
    ## median_income_demo_scale:married5          0.0403288  0.0026307   15.330
    ## median_income_demo_scale:year2009          0.0008319  0.0029247    0.284
    ## median_income_demo_scale:year2010          0.0009547  0.0029231    0.327
    ## median_income_demo_scale:year2011          0.0090533  0.0029207    3.100
    ## median_income_demo_scale:year2012          0.0142919  0.0029103    4.911
    ## median_income_demo_scale:year2013          0.0017591  0.0035024    0.502
    ## median_income_demo_scale:year2014          0.0022830  0.0035719    0.639
    ## median_income_demo_scale:year2015         -0.0091682  0.0034316   -2.672
    ## median_income_demo_scale:year2016         -0.0010574  0.0033737   -0.313
    ## median_income_demo_scale:year2017         -0.0776572  0.0032656  -23.780

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0172521 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 59 x 6
    ##    effect group term                     estimate std.error statistic
    ##    <chr>  <chr> <chr>                       <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                0.0562  0.00444       12.7
    ##  2 fixed  <NA>  median_income_demo_scale   0.216   0.00330       65.5
    ##  3 fixed  <NA>  raw_income_scale          -0.0633  0.00113      -56.0
    ##  4 fixed  <NA>  education_scale           -0.228   0.00153     -148. 
    ##  5 fixed  <NA>  employment_all1            0.0152  0.000844      18.0
    ##  6 fixed  <NA>  sex1                       0.0486  0.000808      60.2
    ##  7 fixed  <NA>  age_scale                  0.0471  0.000986      47.8
    ##  8 fixed  <NA>  race1                     -0.0398  0.00183      -21.7
    ##  9 fixed  <NA>  race2                      0.0598  0.00395       15.2
    ## 10 fixed  <NA>  race3                      0.262   0.00259      101. 
    ## # ... with 49 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.980 -2807948. 5616015. 5616753. 5615897.     2005521

## Geographic Median Income

### Height

``` r
  lm1 <-
    lmer(
      height_scale ~
        raw_income_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## height_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4206596  4206997 -2103266  4206532  2038678 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0891 -0.6555 -0.0147  0.6409 11.7080 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance  Std.Dev. Corr 
    ##  fips_code (Intercept)      0.0016349 0.04043       
    ##            raw_income_scale 0.0001101 0.01049  -0.01
    ##  Residual                   0.4602786 0.67844       
    ## Number of obs: 2038710, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1595820  0.0027577  -57.867
    ## raw_income_scale                          0.0493141  0.0007163   68.847
    ## total_pop_county_scale                   -0.0130138  0.0042175   -3.086
    ## median_monthly_housing_cost_county_scale -0.0132139  0.0013834   -9.552
    ## land_area_2010_scale                      0.0025165  0.0010831    2.323
    ## physicians_scale                         -0.0030733  0.0010404   -2.954
    ## education_scale                           0.0578784  0.0005496  105.317
    ## employment_all1                          -0.0086490  0.0005699  -15.177
    ## sex1                                      0.6873207  0.0004923 1396.096
    ## age_scale                                -0.1010523  0.0006443 -156.837
    ## race1                                     0.1959373  0.0012094  162.007
    ## race2                                     0.0972750  0.0026692   36.443
    ## race3                                     0.1923291  0.0017373  110.703
    ## race4                                    -0.3049088  0.0028865 -105.633
    ## married1                                  0.0031333  0.0013248    2.365
    ## married2                                  0.0092546  0.0010363    8.931
    ## married3                                  0.0037620  0.0029657    1.268
    ## married4                                  0.0197535  0.0014099   14.010
    ## married5                                 -0.0320776  0.0016482  -19.462
    ## year2009                                  0.0095818  0.0018776    5.103
    ## year2010                                  0.0086944  0.0018799    4.625
    ## year2011                                  0.0025515  0.0018675    1.366
    ## year2012                                 -0.0006165  0.0018457   -0.334
    ## year2013                                  0.0013075  0.0022563    0.579
    ## year2014                                  0.0031639  0.0022674    1.395
    ## year2015                                  0.0015947  0.0022595    0.706
    ## year2016                                  0.0013456  0.0022395    0.601
    ## year2017                                  0.0076130  0.0023154    3.288

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.160    0.00276     -57.9 
    ##  2 fixed  <NA>  raw_income_scale                     0.0493   0.000716     68.8 
    ##  3 fixed  <NA>  total_pop_county_scale              -0.0130   0.00422      -3.09
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -0.0132   0.00138      -9.55
    ##  5 fixed  <NA>  land_area_2010_scale                 0.00252  0.00108       2.32
    ##  6 fixed  <NA>  physicians_scale                    -0.00307  0.00104      -2.95
    ##  7 fixed  <NA>  education_scale                      0.0579   0.000550    105.  
    ##  8 fixed  <NA>  employment_all1                     -0.00865  0.000570    -15.2 
    ##  9 fixed  <NA>  sex1                                 0.687    0.000492   1396.  
    ## 10 fixed  <NA>  age_scale                           -0.101    0.000644   -157.  
    ## # ... with 22 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2103266. 4206596. 4206997. 4206532.     2038678

``` r
  lm1 <-
    lmer(
      height_scale ~
        raw_income_scale +
        median_income_county_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_county_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: height_scale ~ raw_income_scale + median_income_county_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code) + (1 + median_income_county_scale |  
    ##     fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4206599  4207050 -2103264  4206527  2038674 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0892 -0.6555 -0.0147  0.6410 11.7080 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev.  Corr 
    ##  fips_code   (Intercept)                1.306e-03 0.0361421      
    ##              raw_income_scale           1.102e-04 0.0104987 -0.02
    ##  fips_code.1 (Intercept)                3.197e-04 0.0178814      
    ##              median_income_county_scale 8.989e-08 0.0002998 -1.00
    ##  Residual                               4.603e-01 0.6784380      
    ## Number of obs: 2038710, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1591113  0.0027598  -57.654
    ## raw_income_scale                          0.0492757  0.0007166   68.766
    ## median_income_county_scale                0.0056028  0.0025613    2.187
    ## total_pop_county_scale                   -0.0110829  0.0042909   -2.583
    ## median_monthly_housing_cost_county_scale -0.0184598  0.0027683   -6.668
    ## land_area_2010_scale                      0.0024505  0.0010830    2.263
    ## physicians_scale                         -0.0030645  0.0010395   -2.948
    ## education_scale                           0.0578926  0.0005496  105.336
    ## employment_all1                          -0.0086268  0.0005700  -15.136
    ## sex1                                      0.6873235  0.0004923 1396.097
    ## age_scale                                -0.1010524  0.0006443 -156.837
    ## race1                                     0.1958559  0.0012100  161.863
    ## race2                                     0.0972480  0.0026693   36.432
    ## race3                                     0.1924516  0.0017383  110.711
    ## race4                                    -0.3049552  0.0028866 -105.647
    ## married1                                  0.0031324  0.0013248    2.364
    ## married2                                  0.0092305  0.0010363    8.907
    ## married3                                  0.0037878  0.0029657    1.277
    ## married4                                  0.0197513  0.0014099   14.009
    ## married5                                 -0.0321026  0.0016482  -19.477
    ## year2009                                  0.0095765  0.0018776    5.100
    ## year2010                                  0.0086911  0.0018799    4.623
    ## year2011                                  0.0025298  0.0018675    1.355
    ## year2012                                 -0.0006448  0.0018457   -0.349
    ## year2013                                  0.0011718  0.0022571    0.519
    ## year2014                                  0.0028041  0.0022732    1.234
    ## year2015                                  0.0009323  0.0022794    0.409
    ## year2016                                  0.0002276  0.0022967    0.099
    ## year2017                                  0.0059830  0.0024323    2.460

    ## 
    ## Correlation matrix not shown by default, as p = 29 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm1)
```

    ## # A tibble: 36 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.159    0.00276     -57.7 
    ##  2 fixed  <NA>  raw_income_scale                     0.0493   0.000717     68.8 
    ##  3 fixed  <NA>  median_income_county_scale           0.00560  0.00256       2.19
    ##  4 fixed  <NA>  total_pop_county_scale              -0.0111   0.00429      -2.58
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0185   0.00277      -6.67
    ##  6 fixed  <NA>  land_area_2010_scale                 0.00245  0.00108       2.26
    ##  7 fixed  <NA>  physicians_scale                    -0.00306  0.00104      -2.95
    ##  8 fixed  <NA>  education_scale                      0.0579   0.000550    105.  
    ##  9 fixed  <NA>  employment_all1                     -0.00863  0.000570    -15.1 
    ## 10 fixed  <NA>  sex1                                 0.687    0.000492   1396.  
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2103264. 4206599. 4207050. 4206527.     2038674

``` r
  lm1 <-
    lmer(
      height_scale ~
        median_income_county_scale * raw_income_scale +
        median_income_county_scale * education_scale +
        median_income_county_scale * employment_all +
        median_income_county_scale * sex +
        median_income_county_scale * age_scale +
        median_income_county_scale * race +
        median_income_county_scale * married +
        median_income_county_scale * year +
        median_income_county_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_county_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: height_scale ~ median_income_county_scale * raw_income_scale +  
    ##     median_income_county_scale * education_scale + median_income_county_scale *  
    ##     employment_all + median_income_county_scale * sex + median_income_county_scale *  
    ##     age_scale + median_income_county_scale * race + median_income_county_scale *  
    ##     married + median_income_county_scale * year + median_income_county_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + (1 + raw_income_scale |  
    ##     fips_code) + (1 + median_income_county_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4206450  4207189 -2103166  4206332  2038651 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0925 -0.6554 -0.0148  0.6409 11.7051 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev.  Corr 
    ##  fips_code   (Intercept)                1.486e-03 3.855e-02      
    ##              raw_income_scale           1.101e-04 1.049e-02 -0.02
    ##  fips_code.1 (Intercept)                1.428e-04 1.195e-02      
    ##              median_income_county_scale 1.062e-09 3.259e-05 -1.00
    ##  Residual                               4.602e-01 6.784e-01      
    ## Number of obs: 2038710, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                               Estimate Std. Error  t value
    ## (Intercept)                                 -1.571e-01  2.798e-03  -56.149
    ## median_income_county_scale                  -7.230e-04  3.259e-03   -0.222
    ## raw_income_scale                             4.914e-02  7.202e-04   68.234
    ## education_scale                              5.778e-02  5.526e-04  104.552
    ## employment_all1                             -8.575e-03  5.712e-04  -15.011
    ## sex1                                         6.872e-01  4.926e-04 1395.150
    ## age_scale                                   -1.012e-01  6.455e-04 -156.786
    ## race1                                        1.941e-01  1.267e-03  153.168
    ## race2                                        9.371e-02  2.705e-03   34.639
    ## race3                                        1.913e-01  1.784e-03  107.258
    ## race4                                       -2.966e-01  3.221e-03  -92.074
    ## married1                                     2.856e-03  1.329e-03    2.148
    ## married2                                     9.297e-03  1.041e-03    8.930
    ## married3                                     4.562e-03  2.996e-03    1.523
    ## married4                                     1.980e-02  1.419e-03   13.953
    ## married5                                    -3.266e-02  1.668e-03  -19.581
    ## year2009                                     9.746e-03  1.890e-03    5.156
    ## year2010                                     8.796e-03  1.895e-03    4.642
    ## year2011                                     2.521e-03  1.876e-03    1.344
    ## year2012                                    -6.342e-04  1.853e-03   -0.342
    ## year2013                                     1.127e-03  2.263e-03    0.498
    ## year2014                                     2.800e-03  2.279e-03    1.229
    ## year2015                                     8.530e-04  2.286e-03    0.373
    ## year2016                                     2.073e-04  2.312e-03    0.090
    ## year2017                                     6.738e-03  2.479e-03    2.718
    ## total_pop_county_scale                      -1.072e-02  4.306e-03   -2.490
    ## median_monthly_housing_cost_county_scale    -1.901e-02  2.823e-03   -6.733
    ## land_area_2010_scale                         2.258e-03  1.083e-03    2.084
    ## physicians_scale                            -3.077e-03  1.040e-03   -2.958
    ## median_income_county_scale:raw_income_scale  1.088e-03  7.226e-04    1.505
    ## median_income_county_scale:education_scale  -1.669e-03  5.771e-04   -2.892
    ## median_income_county_scale:employment_all1   7.315e-04  5.868e-04    1.247
    ## median_income_county_scale:sex1             -4.196e-03  5.014e-04   -8.368
    ## median_income_county_scale:age_scale        -3.202e-03  6.637e-04   -4.824
    ## median_income_county_scale:race1             7.993e-03  1.180e-03    6.771
    ## median_income_county_scale:race2            -1.090e-02  2.761e-03   -3.948
    ## median_income_county_scale:race3             9.001e-03  1.739e-03    5.176
    ## median_income_county_scale:race4            -1.150e-02  2.590e-03   -4.439
    ## median_income_county_scale:married1          1.051e-03  1.370e-03    0.767
    ## median_income_county_scale:married2         -8.927e-04  1.064e-03   -0.839
    ## median_income_county_scale:married3          3.039e-03  3.046e-03    0.998
    ## median_income_county_scale:married4          7.381e-04  1.492e-03    0.495
    ## median_income_county_scale:married5         -4.373e-03  1.746e-03   -2.504
    ## median_income_county_scale:year2009          1.662e-03  1.999e-03    0.832
    ## median_income_county_scale:year2010          9.759e-04  2.000e-03    0.488
    ## median_income_county_scale:year2011          2.211e-04  1.969e-03    0.112
    ## median_income_county_scale:year2012          2.096e-03  1.933e-03    1.084
    ## median_income_county_scale:year2013          9.958e-05  2.332e-03    0.043
    ## median_income_county_scale:year2014          1.713e-03  2.336e-03    0.733
    ## median_income_county_scale:year2015         -2.143e-04  2.308e-03   -0.093
    ## median_income_county_scale:year2016         -9.595e-04  2.243e-03   -0.428
    ## median_income_county_scale:year2017         -2.755e-03  2.256e-03   -1.221

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm1)
```

    ## # A tibble: 59 x 6
    ##    effect group term                        estimate std.error statistic
    ##    <chr>  <chr> <chr>                          <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                -0.157     0.00280    -56.1  
    ##  2 fixed  <NA>  median_income_county_scale -0.000723  0.00326     -0.222
    ##  3 fixed  <NA>  raw_income_scale            0.0491    0.000720    68.2  
    ##  4 fixed  <NA>  education_scale             0.0578    0.000553   105.   
    ##  5 fixed  <NA>  employment_all1            -0.00857   0.000571   -15.0  
    ##  6 fixed  <NA>  sex1                        0.687     0.000493  1395.   
    ##  7 fixed  <NA>  age_scale                  -0.101     0.000646  -157.   
    ##  8 fixed  <NA>  race1                       0.194     0.00127    153.   
    ##  9 fixed  <NA>  race2                       0.0937    0.00271     34.6  
    ## 10 fixed  <NA>  race3                       0.191     0.00178    107.   
    ## # ... with 49 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2103166. 4206450. 4207189. 4206332.     2038651

### Self-reported health

``` r
  lm1 <-
    lmer(
      sr_health_scale ~
        raw_income_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: sr_health_scale ~ raw_income_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     physicians_scale + education_scale + employment_all + sex +  
    ##     age_scale + race + married + year + (1 + raw_income_scale |      fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4706249  4706645 -2353092  4706185  1759278 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7903 -0.7764 -0.0369  0.7118  6.9311 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance Std.Dev. Corr
    ##  fips_code (Intercept)      0.000000 0.00000      
    ##            raw_income_scale 0.002136 0.04622   NaN
    ##  Residual                   0.848865 0.92134      
    ## Number of obs: 1759310, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.1254593  0.0036509   34.364
    ## raw_income_scale                         -0.1913819  0.0015096 -126.777
    ## total_pop_county_scale                    0.0094034  0.0007967   11.803
    ## median_monthly_housing_cost_county_scale -0.0243090  0.0008472  -28.694
    ## land_area_2010_scale                     -0.0064187  0.0007373   -8.706
    ## physicians_scale                         -0.0139153  0.0007916  -17.578
    ## education_scale                          -0.1443120  0.0007986 -180.716
    ## employment_all1                           0.1345925  0.0008266  162.829
    ## sex1                                      0.0395478  0.0007195   54.965
    ## age_scale                                 0.0910080  0.0009367   97.159
    ## race1                                    -0.1209928  0.0017290  -69.980
    ## race2                                     0.0484737  0.0038993   12.431
    ## race3                                    -0.0174554  0.0024752   -7.052
    ## race4                                     0.0262743  0.0041766    6.291
    ## married1                                 -0.0606071  0.0019295  -31.411
    ## married2                                 -0.0770093  0.0015040  -51.204
    ## married3                                  0.1611770  0.0042891   37.578
    ## married4                                  0.0518395  0.0020617   25.144
    ## married5                                 -0.0989412  0.0023987  -41.248
    ## year2009                                  0.0116066  0.0036117    3.214
    ## year2010                                  0.0331346  0.0036130    9.171
    ## year2011                                  0.0223215  0.0035983    6.203
    ## year2012                                  0.0271122  0.0035764    7.581
    ## year2013                                  0.0629775  0.0039745   15.845
    ## year2014                                  0.0649184  0.0039870   16.283
    ## year2015                                  0.0709899  0.0039818   17.828
    ## year2016                                  0.0787407  0.0039600   19.884
    ## year2017                                  0.1153983  0.0085479   13.500

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm1)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          0.125    0.00365      34.4 
    ##  2 fixed  <NA>  raw_income_scale                    -0.191    0.00151    -127.  
    ##  3 fixed  <NA>  total_pop_county_scale               0.00940  0.000797     11.8 
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -0.0243   0.000847    -28.7 
    ##  5 fixed  <NA>  land_area_2010_scale                -0.00642  0.000737     -8.71
    ##  6 fixed  <NA>  physicians_scale                    -0.0139   0.000792    -17.6 
    ##  7 fixed  <NA>  education_scale                     -0.144    0.000799   -181.  
    ##  8 fixed  <NA>  employment_all1                      0.135    0.000827    163.  
    ##  9 fixed  <NA>  sex1                                 0.0395   0.000720     55.0 
    ## 10 fixed  <NA>  age_scale                            0.0910   0.000937     97.2 
    ## # ... with 22 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.921 -2353092. 4706249. 4706645. 4706185.     1759278

``` r
  lm1 <-
    lmer(
      sr_health_scale ~
        raw_income_scale +
        median_income_county_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_county_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0108476 (tol = 0.002, component 1)

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: sr_health_scale ~ raw_income_scale + median_income_county_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code) + (1 + median_income_county_scale |  
    ##     fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4703152  4703598 -2351540  4703080  1759274 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8305 -0.7762 -0.0362  0.7106  6.8811 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)                2.555e-03 0.050547      
    ##              raw_income_scale           1.245e-03 0.035284 -0.47
    ##  fips_code.1 (Intercept)                8.327e-06 0.002886      
    ##              median_income_county_scale 1.551e-03 0.039384 -0.89
    ##  Residual                               8.463e-01 0.919971      
    ## Number of obs: 1759310, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.1205996  0.0045095   26.744
    ## raw_income_scale                         -0.1857831  0.0013445 -138.181
    ## median_income_county_scale               -0.0076264  0.0039713   -1.920
    ## total_pop_county_scale                    0.0255457  0.0052184    4.895
    ## median_monthly_housing_cost_county_scale -0.0343216  0.0040456   -8.484
    ## land_area_2010_scale                     -0.0061070  0.0015677   -3.896
    ## physicians_scale                         -0.0130461  0.0015063   -8.661
    ## education_scale                          -0.1419087  0.0008025 -176.825
    ## employment_all1                           0.1343634  0.0008273  162.414
    ## sex1                                      0.0397986  0.0007188   55.365
    ## age_scale                                 0.0923248  0.0009394   98.277
    ## race1                                    -0.1158869  0.0017702  -65.464
    ## race2                                     0.0492078  0.0039112   12.581
    ## race3                                    -0.0206541  0.0025467   -8.110
    ## race4                                     0.0225430  0.0042006    5.367
    ## married1                                 -0.0595292  0.0019301  -30.843
    ## married2                                 -0.0766989  0.0015061  -50.926
    ## married3                                  0.1590666  0.0042857   37.116
    ## married4                                  0.0515728  0.0020607   25.027
    ## married5                                 -0.0999188  0.0023975  -41.676
    ## year2009                                  0.0117111  0.0036077    3.246
    ## year2010                                  0.0330493  0.0036095    9.156
    ## year2011                                  0.0235747  0.0035983    6.552
    ## year2012                                  0.0285788  0.0035796    7.984
    ## year2013                                  0.0639129  0.0039878   16.027
    ## year2014                                  0.0653055  0.0040053   16.305
    ## year2015                                  0.0713613  0.0040200   17.752
    ## year2016                                  0.0797515  0.0040568   19.659
    ## year2017                                  0.1178578  0.0086449   13.633

    ## 
    ## Correlation matrix not shown by default, as p = 29 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0108476 (tol = 0.002, component 1)

``` r
tidy(lm1)
```

    ## # A tibble: 36 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          0.121    0.00451      26.7 
    ##  2 fixed  <NA>  raw_income_scale                    -0.186    0.00134    -138.  
    ##  3 fixed  <NA>  median_income_county_scale          -0.00763  0.00397      -1.92
    ##  4 fixed  <NA>  total_pop_county_scale               0.0255   0.00522       4.90
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0343   0.00405      -8.48
    ##  6 fixed  <NA>  land_area_2010_scale                -0.00611  0.00157      -3.90
    ##  7 fixed  <NA>  physicians_scale                    -0.0130   0.00151      -8.66
    ##  8 fixed  <NA>  education_scale                     -0.142    0.000803   -177.  
    ##  9 fixed  <NA>  employment_all1                      0.134    0.000827    162.  
    ## 10 fixed  <NA>  sex1                                 0.0398   0.000719     55.4 
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.920 -2351540. 4703152. 4703598. 4703080.     1759274

``` r
  lm1 <-
    lmer(
      sr_health_scale ~
        median_income_county_scale * raw_income_scale +
        median_income_county_scale * education_scale +
        median_income_county_scale * employment_all +
        median_income_county_scale * sex +
        median_income_county_scale * age_scale +
        median_income_county_scale * race +
        median_income_county_scale * married +
        median_income_county_scale * year +
        median_income_county_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_county_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: sr_health_scale ~ median_income_county_scale * raw_income_scale +  
    ##     median_income_county_scale * education_scale + median_income_county_scale *  
    ##     employment_all + median_income_county_scale * sex + median_income_county_scale *  
    ##     age_scale + median_income_county_scale * race + median_income_county_scale *  
    ##     married + median_income_county_scale * year + median_income_county_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + (1 + raw_income_scale |  
    ##     fips_code) + (1 + median_income_county_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4701438  4702168 -2350660  4701320  1759251 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8621 -0.7775 -0.0388  0.7104  6.8836 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)                0.0026735 0.05171       
    ##              raw_income_scale           0.0006112 0.02472  -0.60
    ##  fips_code.1 (Intercept)                0.0002273 0.01508       
    ##              median_income_county_scale 0.0005018 0.02240  -1.00
    ##  Residual                               0.8458039 0.91968       
    ## Number of obs: 1759310, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                               Estimate Std. Error  t value
    ## (Intercept)                                  1.124e-01  4.490e-03   25.043
    ## median_income_county_scale                  -1.826e-02  5.380e-03   -3.393
    ## raw_income_scale                            -1.767e-01  1.207e-03 -146.322
    ## education_scale                             -1.408e-01  8.077e-04 -174.303
    ## employment_all1                              1.324e-01  8.294e-04  159.638
    ## sex1                                         3.985e-02  7.193e-04   55.403
    ## age_scale                                    9.313e-02  9.415e-04   98.919
    ## race1                                       -1.118e-01  1.844e-03  -60.661
    ## race2                                        5.245e-02  3.960e-03   13.246
    ## race3                                       -1.612e-02  2.608e-03   -6.182
    ## race4                                        7.137e-03  4.636e-03    1.539
    ## married1                                    -5.743e-02  1.937e-03  -29.641
    ## married2                                    -7.554e-02  1.514e-03  -49.902
    ## married3                                     1.547e-01  4.336e-03   35.678
    ## married4                                     4.956e-02  2.077e-03   23.860
    ## married5                                    -9.825e-02  2.430e-03  -40.426
    ## year2009                                     1.291e-02  3.626e-03    3.561
    ## year2010                                     3.361e-02  3.630e-03    9.257
    ## year2011                                     2.555e-02  3.612e-03    7.074
    ## year2012                                     3.040e-02  3.592e-03    8.463
    ## year2013                                     6.594e-02  3.998e-03   16.494
    ## year2014                                     6.735e-02  4.014e-03   16.778
    ## year2015                                     7.356e-02  4.027e-03   18.266
    ## year2016                                     8.106e-02  4.062e-03   19.956
    ## year2017                                     1.162e-01  8.962e-03   12.965
    ## total_pop_county_scale                       2.606e-02  5.041e-03    5.170
    ## median_monthly_housing_cost_county_scale    -3.058e-02  3.973e-03   -7.699
    ## land_area_2010_scale                        -5.911e-03  1.530e-03   -3.863
    ## physicians_scale                            -1.211e-02  1.477e-03   -8.202
    ## median_income_county_scale:raw_income_scale  1.830e-02  1.200e-03   15.257
    ## median_income_county_scale:education_scale   4.828e-03  8.498e-04    5.681
    ## median_income_county_scale:employment_all1  -2.296e-02  8.577e-04  -26.774
    ## median_income_county_scale:sex1              4.994e-03  7.379e-04    6.767
    ## median_income_county_scale:age_scale         5.999e-03  9.762e-04    6.146
    ## median_income_county_scale:race1            -1.511e-02  1.745e-03   -8.659
    ## median_income_county_scale:race2            -1.374e-02  4.063e-03   -3.381
    ## median_income_county_scale:race3            -5.023e-06  2.573e-03   -0.002
    ## median_income_county_scale:race4             1.819e-02  3.830e-03    4.750
    ## median_income_county_scale:married1          2.083e-02  2.014e-03   10.345
    ## median_income_county_scale:married2          2.519e-03  1.559e-03    1.616
    ## median_income_county_scale:married3         -2.115e-02  4.437e-03   -4.766
    ## median_income_county_scale:married4         -1.892e-02  2.196e-03   -8.615
    ## median_income_county_scale:married5          1.304e-02  2.561e-03    5.092
    ## median_income_county_scale:year2009          1.248e-02  3.822e-03    3.265
    ## median_income_county_scale:year2010          5.874e-03  3.823e-03    1.536
    ## median_income_county_scale:year2011          1.497e-02  3.793e-03    3.946
    ## median_income_county_scale:year2012          7.714e-03  3.762e-03    2.051
    ## median_income_county_scale:year2013          1.172e-02  4.153e-03    2.822
    ## median_income_county_scale:year2014          9.595e-03  4.157e-03    2.308
    ## median_income_county_scale:year2015          1.196e-02  4.136e-03    2.891
    ## median_income_county_scale:year2016          1.573e-02  4.081e-03    3.856
    ## median_income_county_scale:year2017          1.990e-02  8.032e-03    2.478

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm1)
```

    ## # A tibble: 59 x 6
    ##    effect group term                       estimate std.error statistic
    ##    <chr>  <chr> <chr>                         <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                  0.112   0.00449      25.0 
    ##  2 fixed  <NA>  median_income_county_scale  -0.0183  0.00538      -3.39
    ##  3 fixed  <NA>  raw_income_scale            -0.177   0.00121    -146.  
    ##  4 fixed  <NA>  education_scale             -0.141   0.000808   -174.  
    ##  5 fixed  <NA>  employment_all1              0.132   0.000829    160.  
    ##  6 fixed  <NA>  sex1                         0.0399  0.000719     55.4 
    ##  7 fixed  <NA>  age_scale                    0.0931  0.000941     98.9 
    ##  8 fixed  <NA>  race1                       -0.112   0.00184     -60.7 
    ##  9 fixed  <NA>  race2                        0.0525  0.00396      13.2 
    ## 10 fixed  <NA>  race3                       -0.0161  0.00261      -6.18
    ## # ... with 49 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.920 -2350660. 4701437. 4702168. 4701319.     1759251

### Life satisfaction

``` r
  lm1 <-
    lmer(
      ladder_now_scale ~
        raw_income_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: ladder_now_scale ~ raw_income_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     physicians_scale + education_scale + employment_all + sex +  
    ##     age_scale + race + married + year + (1 + raw_income_scale |      fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5444243  5444643 -2722089  5444179  2001166 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1968 -0.6165  0.1020  0.6492  2.8296 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance Std.Dev. Corr 
    ##  fips_code (Intercept)      0.002303 0.04799       
    ##            raw_income_scale 0.001251 0.03537  -0.63
    ##  Residual                   0.887792 0.94223       
    ## Number of obs: 2001198, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.2300709  0.0033128  -69.449
    ## raw_income_scale                          0.2278584  0.0013021  174.990
    ## total_pop_county_scale                   -0.0281208  0.0041256   -6.816
    ## median_monthly_housing_cost_county_scale -0.0263221  0.0015983  -16.468
    ## land_area_2010_scale                      0.0082043  0.0013810    5.941
    ## physicians_scale                          0.0090900  0.0012899    7.047
    ## education_scale                           0.0560988  0.0007704   72.819
    ## employment_all1                          -0.0129970  0.0007986  -16.275
    ## sex1                                     -0.0740315  0.0006901 -107.274
    ## age_scale                                 0.0928025  0.0009028  102.799
    ## race1                                    -0.0628912  0.0016922  -37.165
    ## race2                                    -0.0744779  0.0037565  -19.827
    ## race3                                     0.0345605  0.0024379   14.177
    ## race4                                    -0.0331442  0.0040229   -8.239
    ## married1                                  0.0673763  0.0018537   36.347
    ## married2                                  0.1246342  0.0014484   86.050
    ## married3                                 -0.2295596  0.0041489  -55.330
    ## married4                                 -0.1107590  0.0019829  -55.856
    ## married5                                  0.1041590  0.0023205   44.886
    ## year2009                                  0.1725526  0.0027221   63.390
    ## year2010                                  0.1811520  0.0026158   69.252
    ## year2011                                  0.1888168  0.0025960   72.732
    ## year2012                                  0.1657121  0.0025654   64.595
    ## year2013                                  0.1879958  0.0031298   60.066
    ## year2014                                  0.2005614  0.0031453   63.765
    ## year2015                                  0.2055081  0.0031357   65.539
    ## year2016                                  0.1934066  0.0031070   62.250
    ## year2017                                  0.2061974  0.0032068   64.300

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.230    0.00331     -69.4 
    ##  2 fixed  <NA>  raw_income_scale                     0.228    0.00130     175.  
    ##  3 fixed  <NA>  total_pop_county_scale              -0.0281   0.00413      -6.82
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -0.0263   0.00160     -16.5 
    ##  5 fixed  <NA>  land_area_2010_scale                 0.00820  0.00138       5.94
    ##  6 fixed  <NA>  physicians_scale                     0.00909  0.00129       7.05
    ##  7 fixed  <NA>  education_scale                      0.0561   0.000770     72.8 
    ##  8 fixed  <NA>  employment_all1                     -0.0130   0.000799    -16.3 
    ##  9 fixed  <NA>  sex1                                -0.0740   0.000690   -107.  
    ## 10 fixed  <NA>  age_scale                            0.0928   0.000903    103.  
    ## # ... with 22 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.942 -2722089. 5444243. 5444643. 5444179.     2001166

``` r
  lm1 <-
    lmer(
      ladder_now_scale ~
        raw_income_scale +
        median_income_county_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_county_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: ladder_now_scale ~ raw_income_scale + median_income_county_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code) + (1 + median_income_county_scale |  
    ##     fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5445011  5445462 -2722470  5444939  2001162 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2103 -0.6172  0.1017  0.6489  2.8392 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev.  Corr
    ##  fips_code   (Intercept)                4.365e-76 2.089e-38     
    ##              raw_income_scale           1.173e-03 3.425e-02 1.00
    ##  fips_code.1 (Intercept)                0.000e+00 0.000e+00     
    ##              median_income_county_scale 3.062e-03 5.534e-02  NaN
    ##  Residual                               8.882e-01 9.424e-01     
    ## Number of obs: 2001198, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.2200594  0.0027863  -78.980
    ## raw_income_scale                          0.2279787  0.0013026  175.019
    ## median_income_county_scale                0.0267179  0.0030472    8.768
    ## total_pop_county_scale                   -0.0019883  0.0009968   -1.995
    ## median_monthly_housing_cost_county_scale -0.0493631  0.0026616  -18.547
    ## land_area_2010_scale                      0.0030801  0.0008481    3.632
    ## physicians_scale                          0.0114144  0.0010582   10.787
    ## education_scale                           0.0566136  0.0007697   73.557
    ## employment_all1                          -0.0130299  0.0007986  -16.315
    ## sex1                                     -0.0740465  0.0006902 -107.277
    ## age_scale                                 0.0926869  0.0009021  102.743
    ## race1                                    -0.0640469  0.0016882  -37.939
    ## race2                                    -0.0744618  0.0037562  -19.824
    ## race3                                     0.0358776  0.0024271   14.782
    ## race4                                    -0.0341342  0.0040237   -8.483
    ## married1                                  0.0673716  0.0018533   36.353
    ## married2                                  0.1247788  0.0014483   86.154
    ## married3                                 -0.2297419  0.0041492  -55.371
    ## married4                                 -0.1109049  0.0019829  -55.930
    ## married5                                  0.1041878  0.0023207   44.895
    ## year2009                                  0.1725785  0.0027227   63.384
    ## year2010                                  0.1814199  0.0026163   69.341
    ## year2011                                  0.1888368  0.0025964   72.731
    ## year2012                                  0.1660717  0.0025660   64.719
    ## year2013                                  0.1882661  0.0031277   60.193
    ## year2014                                  0.1995707  0.0031495   63.365
    ## year2015                                  0.2029579  0.0031514   64.403
    ## year2016                                  0.1881884  0.0031552   59.644
    ## year2017                                  0.1977055  0.0033224   59.507

    ## 
    ## Correlation matrix not shown by default, as p = 29 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm1)
```

    ## # A tibble: 36 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.220    0.00279     -79.0 
    ##  2 fixed  <NA>  raw_income_scale                     0.228    0.00130     175.  
    ##  3 fixed  <NA>  median_income_county_scale           0.0267   0.00305       8.77
    ##  4 fixed  <NA>  total_pop_county_scale              -0.00199  0.000997     -1.99
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0494   0.00266     -18.5 
    ##  6 fixed  <NA>  land_area_2010_scale                 0.00308  0.000848      3.63
    ##  7 fixed  <NA>  physicians_scale                     0.0114   0.00106      10.8 
    ##  8 fixed  <NA>  education_scale                      0.0566   0.000770     73.6 
    ##  9 fixed  <NA>  employment_all1                     -0.0130   0.000799    -16.3 
    ## 10 fixed  <NA>  sex1                                -0.0740   0.000690   -107.  
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.942 -2722470. 5445011. 5445462. 5444939.     2001162

``` r
  lm1 <-
    lmer(
      ladder_now_scale ~
        median_income_county_scale * raw_income_scale +
        median_income_county_scale * education_scale +
        median_income_county_scale * employment_all +
        median_income_county_scale * sex +
        median_income_county_scale * age_scale +
        median_income_county_scale * race +
        median_income_county_scale * married +
        median_income_county_scale * year +
        median_income_county_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_county_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0242667 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: ladder_now_scale ~ median_income_county_scale * raw_income_scale +  
    ##     median_income_county_scale * education_scale + median_income_county_scale *  
    ##     employment_all + median_income_county_scale * sex + median_income_county_scale *  
    ##     age_scale + median_income_county_scale * race + median_income_county_scale *  
    ##     married + median_income_county_scale * year + median_income_county_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + (1 + raw_income_scale |  
    ##     fips_code) + (1 + median_income_county_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5443219  5443958 -2721551  5443101  2001139 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2386 -0.6162  0.1028  0.6491  2.9020 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)                0.0012291 0.03506       
    ##              raw_income_scale           0.0010368 0.03220  -0.82
    ##  fips_code.1 (Intercept)                0.0008108 0.02847       
    ##              median_income_county_scale 0.0001226 0.01107  0.14 
    ##  Residual                               0.8874151 0.94203       
    ## Number of obs: 2001198, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                               Estimate Std. Error  t value
    ## (Intercept)                                 -0.2213038  0.0033652  -65.762
    ## median_income_county_scale                   0.0282009  0.0041857    6.737
    ## raw_income_scale                             0.2242556  0.0013057  171.750
    ## education_scale                              0.0559540  0.0007740   72.294
    ## employment_all1                             -0.0120567  0.0008000  -15.071
    ## sex1                                        -0.0737330  0.0006903 -106.819
    ## age_scale                                    0.0921920  0.0009039  101.992
    ## race1                                       -0.0645239  0.0017717  -36.419
    ## race2                                       -0.0752738  0.0038031  -19.793
    ## race3                                        0.0284701  0.0024991   11.392
    ## race4                                       -0.0240118  0.0044902   -5.348
    ## married1                                     0.0665152  0.0018588   35.783
    ## married2                                     0.1244746  0.0014540   85.607
    ## married3                                    -0.2297766  0.0041825  -54.938
    ## married4                                    -0.1090371  0.0019942  -54.676
    ## married5                                     0.1035893  0.0023462   44.152
    ## year2009                                     0.1700304  0.0027391   62.075
    ## year2010                                     0.1779490  0.0026350   67.534
    ## year2011                                     0.1859359  0.0026062   71.343
    ## year2012                                     0.1634588  0.0025744   63.494
    ## year2013                                     0.1856468  0.0031383   59.154
    ## year2014                                     0.1974488  0.0031589   62.506
    ## year2015                                     0.2013099  0.0031650   63.606
    ## year2016                                     0.1870985  0.0031909   58.636
    ## year2017                                     0.1998650  0.0034072   58.659
    ## total_pop_county_scale                      -0.0233535  0.0040716   -5.736
    ## median_monthly_housing_cost_county_scale    -0.0432993  0.0034286  -12.629
    ## land_area_2010_scale                         0.0078302  0.0013595    5.760
    ## physicians_scale                             0.0090343  0.0012769    7.075
    ## median_income_county_scale:raw_income_scale -0.0086497  0.0012757   -6.781
    ## median_income_county_scale:education_scale  -0.0002584  0.0008068   -0.320
    ## median_income_county_scale:employment_all1   0.0138385  0.0008207   16.862
    ## median_income_county_scale:sex1              0.0066250  0.0007015    9.444
    ## median_income_county_scale:age_scale        -0.0084119  0.0009284   -9.061
    ## median_income_county_scale:race1             0.0200458  0.0016486   12.159
    ## median_income_county_scale:race2             0.0155021  0.0038725    4.003
    ## median_income_county_scale:race3            -0.0305904  0.0024340  -12.568
    ## median_income_county_scale:race4            -0.0055744  0.0036077   -1.545
    ## median_income_county_scale:married1         -0.0115936  0.0019108   -6.067
    ## median_income_county_scale:married2         -0.0014132  0.0014831   -0.953
    ## median_income_county_scale:married3         -0.0068970  0.0042435   -1.625
    ## median_income_county_scale:married4          0.0101807  0.0020948    4.860
    ## median_income_county_scale:married5         -0.0008013  0.0024542   -0.326
    ## median_income_county_scale:year2009         -0.0245696  0.0028972   -8.480
    ## median_income_county_scale:year2010         -0.0275146  0.0027812   -9.893
    ## median_income_county_scale:year2011         -0.0268392  0.0027361   -9.809
    ## median_income_county_scale:year2012         -0.0172720  0.0026877   -6.426
    ## median_income_county_scale:year2013         -0.0102536  0.0032388   -3.166
    ## median_income_county_scale:year2014         -0.0033166  0.0032401   -1.024
    ## median_income_county_scale:year2015         -0.0153183  0.0032029   -4.783
    ## median_income_county_scale:year2016         -0.0123410  0.0031133   -3.964
    ## median_income_county_scale:year2017         -0.0187037  0.0031302   -5.975

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0242667 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 59 x 6
    ##    effect group term                       estimate std.error statistic
    ##    <chr>  <chr> <chr>                         <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                 -0.221   0.00337     -65.8 
    ##  2 fixed  <NA>  median_income_county_scale   0.0282  0.00419       6.74
    ##  3 fixed  <NA>  raw_income_scale             0.224   0.00131     172.  
    ##  4 fixed  <NA>  education_scale              0.0560  0.000774     72.3 
    ##  5 fixed  <NA>  employment_all1             -0.0121  0.000800    -15.1 
    ##  6 fixed  <NA>  sex1                        -0.0737  0.000690   -107.  
    ##  7 fixed  <NA>  age_scale                    0.0922  0.000904    102.  
    ##  8 fixed  <NA>  race1                       -0.0645  0.00177     -36.4 
    ##  9 fixed  <NA>  race2                       -0.0753  0.00380     -19.8 
    ## 10 fixed  <NA>  race3                        0.0285  0.00250      11.4 
    ## # ... with 49 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.942 -2721551. 5443219. 5443957. 5443101.     2001139

### BMI

``` r
  lm1 <-
    lmer(
      bmi_scale ~
        raw_income_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## bmi_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5645629  5646029 -2822782  5645565  2006480 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7817 -0.6609 -0.1629  0.4716 21.9436 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance  Std.Dev. Corr 
    ##  fips_code (Intercept)      0.0059793 0.07733       
    ##            raw_income_scale 0.0004899 0.02213  -0.32
    ##  Residual                   0.9741350 0.98698       
    ## Number of obs: 2006512, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error t value
    ## (Intercept)                               0.0037955  0.0046029   0.825
    ## raw_income_scale                         -0.0551088  0.0011488 -47.972
    ## total_pop_county_scale                   -0.0321206  0.0075331  -4.264
    ## median_monthly_housing_cost_county_scale -0.0515998  0.0023745 -21.731
    ## land_area_2010_scale                     -0.0049639  0.0017586  -2.823
    ## physicians_scale                         -0.0158059  0.0017072  -9.258
    ## education_scale                          -0.0525866  0.0008064 -65.208
    ## employment_all1                           0.0002408  0.0008368   0.288
    ## sex1                                      0.0975366  0.0007229 134.924
    ## age_scale                                 0.0446001  0.0009444  47.226
    ## race1                                    -0.0348819  0.0017758 -19.643
    ## race2                                     0.0651414  0.0039166  16.632
    ## race3                                     0.2739800  0.0025550 107.232
    ## race4                                    -0.3634055  0.0042284 -85.945
    ## married1                                 -0.0675754  0.0019431 -34.778
    ## married2                                  0.0615606  0.0015196  40.510
    ## married3                                  0.0843657  0.0043452  19.416
    ## married4                                  0.0525929  0.0020685  25.426
    ## married5                                 -0.1098326  0.0024185 -45.413
    ## year2009                                  0.0120303  0.0027522   4.371
    ## year2010                                  0.0162143  0.0027572   5.881
    ## year2011                                  0.0005592  0.0027406   0.204
    ## year2012                                  0.0118013  0.0027091   4.356
    ## year2013                                  0.0245634  0.0033144   7.411
    ## year2014                                  0.0445972  0.0033303  13.391
    ## year2015                                  0.0661906  0.0033179  19.950
    ## year2016                                  0.0666275  0.0032896  20.254
    ## year2017                                  0.0710763  0.0034040  20.880

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm1)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          3.80e-3  0.00460      0.825
    ##  2 fixed  <NA>  raw_income_scale                    -5.51e-2  0.00115    -48.0  
    ##  3 fixed  <NA>  total_pop_county_scale              -3.21e-2  0.00753     -4.26 
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -5.16e-2  0.00237    -21.7  
    ##  5 fixed  <NA>  land_area_2010_scale                -4.96e-3  0.00176     -2.82 
    ##  6 fixed  <NA>  physicians_scale                    -1.58e-2  0.00171     -9.26 
    ##  7 fixed  <NA>  education_scale                     -5.26e-2  0.000806   -65.2  
    ##  8 fixed  <NA>  employment_all1                      2.41e-4  0.000837     0.288
    ##  9 fixed  <NA>  sex1                                 9.75e-2  0.000723   135.   
    ## 10 fixed  <NA>  age_scale                            4.46e-2  0.000944    47.2  
    ## # ... with 22 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.987 -2822782. 5645629. 5646029. 5645565.     2006480

``` r
  lm1 <-
    lmer(
      bmi_scale ~
        raw_income_scale +
        median_income_county_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_county_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## bmi_scale ~ raw_income_scale + median_income_county_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     physicians_scale + education_scale + employment_all + sex +  
    ##     age_scale + race + married + year + (1 + raw_income_scale |  
    ##     fips_code) + (1 + median_income_county_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5645585  5646036 -2822757  5645513  2006476 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7859 -0.6609 -0.1629  0.4715 21.9417 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)                0.0051550 0.07180       
    ##              raw_income_scale           0.0004869 0.02207  -0.37
    ##  fips_code.1 (Intercept)                0.0000000 0.00000       
    ##              median_income_county_scale 0.0005512 0.02348   NaN 
    ##  Residual                               0.9741469 0.98699       
    ## Number of obs: 2006512, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error t value
    ## (Intercept)                               0.0066110  0.0044727   1.478
    ## raw_income_scale                         -0.0552334  0.0011472 -48.144
    ## median_income_county_scale                0.0283203  0.0042605   6.647
    ## total_pop_county_scale                   -0.0200958  0.0071959  -2.793
    ## median_monthly_housing_cost_county_scale -0.0781604  0.0045183 -17.299
    ## land_area_2010_scale                     -0.0055489  0.0017363  -3.196
    ## physicians_scale                         -0.0170909  0.0016914 -10.104
    ## education_scale                          -0.0525685  0.0008064 -65.185
    ## employment_all1                           0.0003062  0.0008368   0.366
    ## sex1                                      0.0975492  0.0007229 134.941
    ## age_scale                                 0.0445714  0.0009444  47.197
    ## race1                                    -0.0351185  0.0017764 -19.770
    ## race2                                     0.0650260  0.0039169  16.602
    ## race3                                     0.2743961  0.0025562 107.346
    ## race4                                    -0.3635143  0.0042288 -85.962
    ## married1                                 -0.0675853  0.0019431 -34.783
    ## married2                                  0.0615066  0.0015197  40.473
    ## married3                                  0.0844083  0.0043453  19.425
    ## married4                                  0.0525974  0.0020685  25.428
    ## married5                                 -0.1098869  0.0024185 -45.435
    ## year2009                                  0.0120225  0.0027522   4.368
    ## year2010                                  0.0161953  0.0027573   5.874
    ## year2011                                  0.0005133  0.0027412   0.187
    ## year2012                                  0.0116560  0.0027101   4.301
    ## year2013                                  0.0236579  0.0033164   7.134
    ## year2014                                  0.0426279  0.0033426  12.753
    ## year2015                                  0.0627400  0.0033571  18.689
    ## year2016                                  0.0610254  0.0034022  17.937
    ## year2017                                  0.0630708  0.0036412  17.321

    ## 
    ## Correlation matrix not shown by default, as p = 29 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm1)
```

    ## # A tibble: 36 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          6.61e-3  0.00447      1.48 
    ##  2 fixed  <NA>  raw_income_scale                    -5.52e-2  0.00115    -48.1  
    ##  3 fixed  <NA>  median_income_county_scale           2.83e-2  0.00426      6.65 
    ##  4 fixed  <NA>  total_pop_county_scale              -2.01e-2  0.00720     -2.79 
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -7.82e-2  0.00452    -17.3  
    ##  6 fixed  <NA>  land_area_2010_scale                -5.55e-3  0.00174     -3.20 
    ##  7 fixed  <NA>  physicians_scale                    -1.71e-2  0.00169    -10.1  
    ##  8 fixed  <NA>  education_scale                     -5.26e-2  0.000806   -65.2  
    ##  9 fixed  <NA>  employment_all1                      3.06e-4  0.000837     0.366
    ## 10 fixed  <NA>  sex1                                 9.75e-2  0.000723   135.   
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.987 -2822757. 5645585. 5646036. 5645513.     2006476

``` r
  lm1 <-
    lmer(
      bmi_scale ~
        median_income_county_scale * raw_income_scale +
        median_income_county_scale * education_scale +
        median_income_county_scale * employment_all +
        median_income_county_scale * sex +
        median_income_county_scale * age_scale +
        median_income_county_scale * race +
        median_income_county_scale * married +
        median_income_county_scale * year +
        median_income_county_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_county_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg_rs
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## bmi_scale ~ median_income_county_scale * raw_income_scale + median_income_county_scale *  
    ##     education_scale + median_income_county_scale * employment_all +  
    ##     median_income_county_scale * sex + median_income_county_scale *  
    ##     age_scale + median_income_county_scale * race + median_income_county_scale *  
    ##     married + median_income_county_scale * year + median_income_county_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + (1 + raw_income_scale |  
    ##     fips_code) + (1 + median_income_county_scale | fips_code)
    ##    Data: dfg_rs
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5642741  5643480 -2821312  5642623  2006453 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7710 -0.6601 -0.1628  0.4715 21.9563 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)                0.0050943 0.07137       
    ##              raw_income_scale           0.0004267 0.02066  -0.39
    ##  fips_code.1 (Intercept)                0.0000000 0.00000       
    ##              median_income_county_scale 0.0005060 0.02249   NaN 
    ##  Residual                               0.9727880 0.98630       
    ## Number of obs: 2006512, groups:  fips_code, 3131
    ## 
    ## Fixed effects:
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                  0.0045799  0.0044940   1.019
    ## median_income_county_scale                   0.0408272  0.0052019   7.848
    ## raw_income_scale                            -0.0532368  0.0011395 -46.720
    ## education_scale                             -0.0536358  0.0008103 -66.192
    ## employment_all1                             -0.0005600  0.0008381  -0.668
    ## sex1                                         0.0983124  0.0007228 136.012
    ## age_scale                                    0.0460992  0.0009455  48.759
    ## race1                                       -0.0347030  0.0018585 -18.673
    ## race2                                        0.0651681  0.0039665  16.430
    ## race3                                        0.2718342  0.0026197 103.764
    ## race4                                       -0.3625855  0.0047164 -76.877
    ## married1                                    -0.0657291  0.0019486 -33.732
    ## married2                                     0.0619682  0.0015256  40.618
    ## married3                                     0.0799249  0.0043863  18.221
    ## married4                                     0.0531211  0.0020811  25.526
    ## married5                                    -0.1079529  0.0024462 -44.131
    ## year2009                                     0.0123166  0.0027688   4.448
    ## year2010                                     0.0166584  0.0027772   5.998
    ## year2011                                     0.0005648  0.0027513   0.205
    ## year2012                                     0.0115717  0.0027187   4.256
    ## year2013                                     0.0232347  0.0033238   6.990
    ## year2014                                     0.0418385  0.0033500  12.489
    ## year2015                                     0.0616659  0.0033656  18.322
    ## year2016                                     0.0604346  0.0034217  17.662
    ## year2017                                     0.0627559  0.0036961  16.979
    ## total_pop_county_scale                      -0.0175140  0.0071143  -2.462
    ## median_monthly_housing_cost_county_scale    -0.0858968  0.0046020 -18.665
    ## land_area_2010_scale                        -0.0053599  0.0017284  -3.101
    ## physicians_scale                            -0.0176258  0.0016835 -10.470
    ## median_income_county_scale:raw_income_scale  0.0069403  0.0011357   6.111
    ## median_income_county_scale:education_scale  -0.0104384  0.0008479 -12.311
    ## median_income_county_scale:employment_all1  -0.0164006  0.0008612 -19.043
    ## median_income_county_scale:sex1              0.0266740  0.0007361  36.237
    ## median_income_county_scale:age_scale         0.0303040  0.0009727  31.153
    ## median_income_county_scale:race1            -0.0005243  0.0017348  -0.302
    ## median_income_county_scale:race2            -0.0022431  0.0040496  -0.554
    ## median_income_county_scale:race3            -0.0192154  0.0025591  -7.509
    ## median_income_county_scale:race4             0.0146603  0.0037960   3.862
    ## median_income_county_scale:married1          0.0122187  0.0020095   6.081
    ## median_income_county_scale:married2         -0.0116826  0.0015598  -7.490
    ## median_income_county_scale:married3         -0.0180406  0.0044605  -4.045
    ## median_income_county_scale:married4         -0.0021390  0.0021883  -0.977
    ## median_income_county_scale:married5          0.0024535  0.0025615   0.958
    ## median_income_county_scale:year2009          0.0020902  0.0029279   0.714
    ## median_income_county_scale:year2010          0.0011713  0.0029313   0.400
    ## median_income_county_scale:year2011          0.0011422  0.0028873   0.396
    ## median_income_county_scale:year2012         -0.0038133  0.0028371  -1.344
    ## median_income_county_scale:year2013         -0.0058719  0.0034224  -1.716
    ## median_income_county_scale:year2014         -0.0102054  0.0034339  -2.972
    ## median_income_county_scale:year2015         -0.0125314  0.0033960  -3.690
    ## median_income_county_scale:year2016         -0.0128169  0.0033143  -3.867
    ## median_income_county_scale:year2017         -0.0112702  0.0033590  -3.355

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm1)
```

    ## # A tibble: 59 x 6
    ##    effect group term                        estimate std.error statistic
    ##    <chr>  <chr> <chr>                          <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                 0.00458   0.00449      1.02 
    ##  2 fixed  <NA>  median_income_county_scale  0.0408    0.00520      7.85 
    ##  3 fixed  <NA>  raw_income_scale           -0.0532    0.00114    -46.7  
    ##  4 fixed  <NA>  education_scale            -0.0536    0.000810   -66.2  
    ##  5 fixed  <NA>  employment_all1            -0.000560  0.000838    -0.668
    ##  6 fixed  <NA>  sex1                        0.0983    0.000723   136.   
    ##  7 fixed  <NA>  age_scale                   0.0461    0.000945    48.8  
    ##  8 fixed  <NA>  race1                      -0.0347    0.00186    -18.7  
    ##  9 fixed  <NA>  race2                       0.0652    0.00397     16.4  
    ## 10 fixed  <NA>  race3                       0.272     0.00262    104.   
    ## # ... with 49 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.986 -2821312. 5642741. 5643480. 5642623.     2006453
