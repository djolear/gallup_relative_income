Gallup - Assorted Regression Results
================
Daniel O’Leary
3/30/2021

  - [Demographic Median Income](#demographic-median-income)
      - [Height](#height)
          - [Data selection](#data-selection)
          - [Baseline Model](#baseline-model)
          - [Main effect model](#main-effect-model)
          - [Interactive model](#interactive-model)
          - [Model comparison](#model-comparison)
      - [Self-reported health](#self-reported-health)
          - [Data selection](#data-selection-1)
          - [Baseline Model](#baseline-model-1)
          - [Main effect model](#main-effect-model-1)
          - [Interactive model](#interactive-model-1)
          - [Model comparison](#model-comparison-1)
      - [Life satisfaction](#life-satisfaction)
          - [Data selection](#data-selection-2)
          - [Baseline Model](#baseline-model-2)
          - [Main effect model](#main-effect-model-2)
          - [Interactive model](#interactive-model-2)
          - [Model comparison](#model-comparison-2)
      - [BMI](#bmi)
          - [Data selection](#data-selection-3)
          - [Baseline Model](#baseline-model-3)
          - [Main effect model](#main-effect-model-3)
          - [Interactive model](#interactive-model-3)
          - [Model comparison](#model-comparison-3)
  - [Geographic Median Income](#geographic-median-income)
      - [Height](#height-1)
          - [Data selection](#data-selection-4)
          - [Baseline Model](#baseline-model-4)
          - [Main effect model](#main-effect-model-4)
          - [Interactive model](#interactive-model-4)
          - [Model comparison](#model-comparison-4)
      - [Self-reported health](#self-reported-health-1)
          - [Data selection](#data-selection-5)
          - [Baseline Model](#baseline-model-5)
          - [Main effect model](#main-effect-model-5)
          - [Interactive model](#interactive-model-5)
          - [Model comparison](#model-comparison-5)
      - [Life satisfaction](#life-satisfaction-1)
          - [Data selection](#data-selection-6)
          - [Baseline Model](#baseline-model-6)
          - [Main effect model](#main-effect-model-6)
          - [Interactive model](#interactive-model-6)
          - [Model comparison](#model-comparison-6)
      - [BMI](#bmi-1)
          - [Data selection](#data-selection-7)
          - [Baseline Model](#baseline-model-7)
          - [Main effect model](#main-effect-model-7)
          - [Interactive model](#interactive-model-7)
          - [Model comparison](#model-comparison-7)

``` r
contrasts(dfg_rs$sex) <- contr.sum(2)
contrasts(dfg_rs$employment_all) <- contr.sum(2)
contrasts(dfg_rs$race) <- contr.sum(5)
contrasts(dfg_rs$married) <- contr.sum(6)
```

## Demographic Median Income

### Height

#### Data selection

``` r
dfg_current <-
  dfg_rs %>%
  filter_at(
    vars(
      height_scale,
      median_income_demo_scale,
      education_scale,
      raw_income_scale,
      median_home_value_county_scale,
      median_monthly_housing_cost_county_scale,
      physicians_scale,
      total_pop_county_scale,
      land_area_2010_scale,
      race,
      sex,
      age_scale,
      married,
      employment_all,
      year
    ),
    all_vars(!is.na(.))
  )
```

#### Baseline Model

``` r
  lm0 <-
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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm0)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## height_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code)
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4204201  4204602 -2102069  4204137  2037728 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0907 -0.6556 -0.0147  0.6410 11.7074 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance  Std.Dev. Corr 
    ##  fips_code (Intercept)      0.0016366 0.04046       
    ##            raw_income_scale 0.0001099 0.01048  -0.01
    ##  Residual                   0.4601801 0.67837       
    ## Number of obs: 2037760, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1596266  0.0027586  -57.866
    ## raw_income_scale                          0.0492885  0.0007162   68.816
    ## total_pop_county_scale                   -0.0129541  0.0042199   -3.070
    ## median_monthly_housing_cost_county_scale -0.0132025  0.0013840   -9.540
    ## land_area_2010_scale                      0.0025143  0.0010832    2.321
    ## physicians_scale                         -0.0030641  0.0010407   -2.944
    ## education_scale                           0.0579049  0.0005497  105.348
    ## employment_all1                          -0.0086025  0.0005699  -15.095
    ## sex1                                      0.6873140  0.0004924 1395.890
    ## age_scale                                -0.1010392  0.0006445 -156.767
    ## race1                                     0.1960484  0.0012095  162.089
    ## race2                                     0.0972382  0.0026694   36.428
    ## race3                                     0.1923034  0.0017375  110.678
    ## race4                                    -0.3048416  0.0028864 -105.612
    ## married1                                  0.0031051  0.0013249    2.344
    ## married2                                  0.0092159  0.0010364    8.892
    ## married3                                  0.0037384  0.0029661    1.260
    ## married4                                  0.0197192  0.0014101   13.984
    ## married5                                 -0.0319445  0.0016488  -19.374
    ## year2009                                  0.0095663  0.0018774    5.096
    ## year2010                                  0.0086875  0.0018797    4.622
    ## year2011                                  0.0025364  0.0018673    1.358
    ## year2012                                 -0.0006293  0.0018455   -0.341
    ## year2013                                  0.0013064  0.0022561    0.579
    ## year2014                                  0.0031640  0.0022672    1.396
    ## year2015                                  0.0015959  0.0022593    0.706
    ## year2016                                  0.0013551  0.0022392    0.605
    ## year2017                                  0.0080714  0.0023206    3.478

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm0)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.160    0.00276     -57.9 
    ##  2 fixed  <NA>  raw_income_scale                     0.0493   0.000716     68.8 
    ##  3 fixed  <NA>  total_pop_county_scale              -0.0130   0.00422      -3.07
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -0.0132   0.00138      -9.54
    ##  5 fixed  <NA>  land_area_2010_scale                 0.00251  0.00108       2.32
    ##  6 fixed  <NA>  physicians_scale                    -0.00306  0.00104      -2.94
    ##  7 fixed  <NA>  education_scale                      0.0579   0.000550    105.  
    ##  8 fixed  <NA>  employment_all1                     -0.00860  0.000570    -15.1 
    ##  9 fixed  <NA>  sex1                                 0.687    0.000492   1396.  
    ## 10 fixed  <NA>  age_scale                           -0.101    0.000645   -157.  
    ## # ... with 22 more rows

``` r
glance(lm0)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2102069. 4204201. 4204602. 4204137.     2037728

#### Main effect model

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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0478979 (tol = 0.002, component 1)

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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4202719  4203170 -2101324  4202647  2037724 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.1218 -0.6554 -0.0145  0.6414 11.6596 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              1.354e-04 0.01163       
    ##              raw_income_scale         5.776e-05 0.00760  0.15 
    ##  fips_code.1 (Intercept)              1.519e-03 0.03897       
    ##              median_income_demo_scale 1.727e-04 0.01314  -0.09
    ##  Residual                             4.598e-01 0.67806       
    ## Number of obs: 2037760, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1520843  0.0027694  -54.916
    ## raw_income_scale                          0.0477006  0.0006773   70.423
    ## median_income_demo_scale                  0.0408648  0.0012114   33.733
    ## total_pop_county_scale                   -0.0129521  0.0042269   -3.064
    ## median_monthly_housing_cost_county_scale -0.0129514  0.0013893   -9.322
    ## land_area_2010_scale                      0.0026465  0.0010852    2.439
    ## physicians_scale                         -0.0030618  0.0010443   -2.932
    ## education_scale                           0.0266753  0.0010140   26.308
    ## employment_all1                          -0.0055321  0.0005759   -9.605
    ## sex1                                      0.6785011  0.0005474 1239.464
    ## age_scale                                -0.1004896  0.0006447 -155.869
    ## race1                                     0.1951030  0.0012099  161.257
    ## race2                                     0.0966296  0.0026687   36.208
    ## race3                                     0.1896623  0.0017387  109.080
    ## race4                                    -0.3033092  0.0028881 -105.021
    ## married1                                  0.0084003  0.0013328    6.303
    ## married2                                  0.0054531  0.0010413    5.237
    ## married3                                 -0.0007378  0.0029676   -0.249
    ## married4                                  0.0149097  0.0014155   10.533
    ## married5                                 -0.0248549  0.0016598  -14.975
    ## year2009                                  0.0122781  0.0018781    6.538
    ## year2010                                  0.0093718  0.0018790    4.988
    ## year2011                                  0.0045792  0.0018674    2.452
    ## year2012                                 -0.0036652  0.0018468   -1.985
    ## year2013                                 -0.0010765  0.0022564   -0.477
    ## year2014                                  0.0009835  0.0022672    0.434
    ## year2015                                 -0.0117849  0.0022881   -5.151
    ## year2016                                 -0.0208084  0.0023192   -8.972
    ## year2017                                 -0.0128076  0.0023888   -5.361

    ## 
    ## Correlation matrix not shown by default, as p = 29 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0478979 (tol = 0.002, component 1)
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
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0130   0.00139      -9.32
    ##  6 fixed  <NA>  land_area_2010_scale                 0.00265  0.00109       2.44
    ##  7 fixed  <NA>  physicians_scale                    -0.00306  0.00104      -2.93
    ##  8 fixed  <NA>  education_scale                      0.0267   0.00101      26.3 
    ##  9 fixed  <NA>  employment_all1                     -0.00553  0.000576     -9.61
    ## 10 fixed  <NA>  sex1                                 0.679    0.000547   1239.  
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2101324. 4202719. 4203170. 4202647.     2037724

#### Interactive model

``` r
  lm2 <-
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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.048951 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm2)
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4200230  4200969 -2100056  4200112  2037701 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0908 -0.6563 -0.0151  0.6403 11.7708 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              2.067e-05 0.004547      
    ##              raw_income_scale         4.900e-05 0.007000 0.47 
    ##  fips_code.1 (Intercept)              1.619e-03 0.040233      
    ##              median_income_demo_scale 1.518e-04 0.012322 -0.08
    ##  Residual                             4.592e-01 0.677651      
    ## Number of obs: 2037760, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error  t value
    ## (Intercept)                               -0.1308765  0.0028307  -46.234
    ## median_income_demo_scale                   0.0315518  0.0022197   14.215
    ## raw_income_scale                           0.0482852  0.0006717   71.885
    ## education_scale                            0.0197351  0.0010521   18.757
    ## employment_all1                           -0.0057166  0.0005783   -9.885
    ## sex1                                       0.6780342  0.0005528 1226.453
    ## age_scale                                 -0.0997235  0.0006767 -147.373
    ## race1                                      0.1854818  0.0012549  147.812
    ## race2                                      0.0909776  0.0027056   33.625
    ## race3                                      0.1787065  0.0017742  100.726
    ## race4                                     -0.2873255  0.0031116  -92.340
    ## married1                                   0.0047467  0.0013579    3.496
    ## married2                                   0.0050597  0.0010634    4.758
    ## married3                                   0.0031027  0.0030499    1.017
    ## married4                                   0.0125499  0.0014373    8.732
    ## married5                                  -0.0246898  0.0019046  -12.963
    ## year2009                                   0.0125201  0.0018922    6.617
    ## year2010                                   0.0105270  0.0018871    5.578
    ## year2011                                   0.0059665  0.0018828    3.169
    ## year2012                                  -0.0029995  0.0018509   -1.621
    ## year2013                                  -0.0002682  0.0022593   -0.119
    ## year2014                                   0.0022423  0.0022753    0.985
    ## year2015                                  -0.0119105  0.0023172   -5.140
    ## year2016                                  -0.0242524  0.0024417   -9.933
    ## year2017                                  -0.0089279  0.0024698   -3.615
    ## total_pop_county_scale                    -0.0121751  0.0042115   -2.891
    ## median_monthly_housing_cost_county_scale  -0.0127420  0.0013851   -9.200
    ## land_area_2010_scale                       0.0025240  0.0010830    2.331
    ## physicians_scale                          -0.0028779  0.0010421   -2.762
    ## median_income_demo_scale:raw_income_scale -0.0038482  0.0006521   -5.901
    ## median_income_demo_scale:education_scale  -0.0139443  0.0006236  -22.362
    ## median_income_demo_scale:employment_all1   0.0029864  0.0006219    4.802
    ## median_income_demo_scale:sex1              0.0046886  0.0005390    8.698
    ## median_income_demo_scale:age_scale        -0.0002004  0.0006915   -0.290
    ## median_income_demo_scale:race1            -0.0017471  0.0012343   -1.415
    ## median_income_demo_scale:race2             0.0253327  0.0027393    9.248
    ## median_income_demo_scale:race3            -0.0129742  0.0017537   -7.398
    ## median_income_demo_scale:race4            -0.0564270  0.0030348  -18.593
    ## median_income_demo_scale:married1         -0.0111888  0.0013865   -8.070
    ## median_income_demo_scale:married2         -0.0001312  0.0010773   -0.122
    ## median_income_demo_scale:married3          0.0047694  0.0029385    1.623
    ## median_income_demo_scale:married4         -0.0024676  0.0015060   -1.638
    ## median_income_demo_scale:married5         -0.0003388  0.0018040   -0.188
    ## median_income_demo_scale:year2009          0.0071690  0.0020087    3.569
    ## median_income_demo_scale:year2010          0.0098886  0.0020061    4.929
    ## median_income_demo_scale:year2011          0.0163876  0.0020044    8.176
    ## median_income_demo_scale:year2012          0.0191983  0.0019980    9.609
    ## median_income_demo_scale:year2013          0.0159177  0.0024045    6.620
    ## median_income_demo_scale:year2014          0.0216716  0.0024514    8.840
    ## median_income_demo_scale:year2015          0.0182068  0.0023557    7.729
    ## median_income_demo_scale:year2016          0.0239780  0.0023150   10.358
    ## median_income_demo_scale:year2017          0.0065730  0.0022407    2.933

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.048951 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm2)
```

    ## # A tibble: 59 x 6
    ##    effect group term                     estimate std.error statistic
    ##    <chr>  <chr> <chr>                       <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)              -0.131    0.00283     -46.2 
    ##  2 fixed  <NA>  median_income_demo_scale  0.0316   0.00222      14.2 
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
glance(lm2)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2100056. 4200230. 4200969. 4200112.     2037701

#### Model comparison

``` r
anova(lm0, lm1 , lm2)
```

    ## Data: dfg_current
    ## Models:
    ## lm0: height_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm0:     land_area_2010_scale + physicians_scale + education_scale + 
    ## lm0:     employment_all + sex + age_scale + race + married + year + 
    ## lm0:     (1 + raw_income_scale | fips_code)
    ## lm1: height_scale ~ raw_income_scale + median_income_demo_scale + 
    ## lm1:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm1:     land_area_2010_scale + physicians_scale + education_scale + 
    ## lm1:     employment_all + sex + age_scale + race + married + year + 
    ## lm1:     (1 + raw_income_scale | fips_code) + (1 + median_income_demo_scale | 
    ## lm1:     fips_code)
    ## lm2: height_scale ~ median_income_demo_scale * raw_income_scale + 
    ## lm2:     median_income_demo_scale * education_scale + median_income_demo_scale * 
    ## lm2:     employment_all + median_income_demo_scale * sex + median_income_demo_scale * 
    ## lm2:     age_scale + median_income_demo_scale * race + median_income_demo_scale * 
    ## lm2:     married + median_income_demo_scale * year + median_income_demo_scale + 
    ## lm2:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm2:     land_area_2010_scale + physicians_scale + (1 + raw_income_scale | 
    ## lm2:     fips_code) + (1 + median_income_demo_scale | fips_code)
    ##     npar     AIC     BIC   logLik deviance  Chisq Df Pr(>Chisq)    
    ## lm0   32 4204201 4204602 -2102069  4204137                         
    ## lm1   36 4202719 4203170 -2101324  4202647 1489.8  4  < 2.2e-16 ***
    ## lm2   59 4200230 4200969 -2100056  4200112 2535.5 23  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Self-reported health

#### Data selection

``` r
dfg_current <-
  dfg_rs %>%
  filter_at(
    vars(
      sr_health_scale,
      median_income_demo_scale,
      education_scale,
      raw_income_scale,
      median_home_value_county_scale,
      median_monthly_housing_cost_county_scale,
      physicians_scale,
      total_pop_county_scale,
      land_area_2010_scale,
      race,
      sex,
      age_scale,
      married,
      employment_all,
      year
    ),
    all_vars(!is.na(.))
  )
```

#### Baseline Model

``` r
  lm0 <-
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
      data = dfg_current
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm0)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: sr_health_scale ~ raw_income_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     physicians_scale + education_scale + employment_all + sex +  
    ##     age_scale + race + married + year + (1 + raw_income_scale |      fips_code)
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4705969  4706365 -2352952  4705905  1759178 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7903 -0.7764 -0.0369  0.7118  6.9311 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance Std.Dev. Corr
    ##  fips_code (Intercept)      0.000000 0.00000      
    ##            raw_income_scale 0.002137 0.04623   NaN
    ##  Residual                   0.848859 0.92134      
    ## Number of obs: 1759210, groups:  fips_code, 3129
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.1254539  0.0036509   34.362
    ## raw_income_scale                         -0.1913795  0.0015098 -126.760
    ## total_pop_county_scale                    0.0094023  0.0007967   11.801
    ## median_monthly_housing_cost_county_scale -0.0242985  0.0008472  -28.681
    ## land_area_2010_scale                     -0.0064271  0.0007373   -8.717
    ## physicians_scale                         -0.0139128  0.0007916  -17.575
    ## education_scale                          -0.1443206  0.0007986 -180.722
    ## employment_all1                           0.1345922  0.0008266  162.825
    ## sex1                                      0.0395488  0.0007195   54.965
    ## age_scale                                 0.0910148  0.0009367   97.161
    ## race1                                    -0.1209926  0.0017290  -69.978
    ## race2                                     0.0484615  0.0038994   12.428
    ## race3                                    -0.0174456  0.0024752   -7.048
    ## race4                                     0.0262666  0.0041767    6.289
    ## married1                                 -0.0606177  0.0019295  -31.416
    ## married2                                 -0.0770050  0.0015040  -51.200
    ## married3                                  0.1611441  0.0042892   37.570
    ## married4                                  0.0518266  0.0020618   25.137
    ## married5                                 -0.0988881  0.0023988  -41.224
    ## year2009                                  0.0116062  0.0036117    3.214
    ## year2010                                  0.0331327  0.0036130    9.170
    ## year2011                                  0.0223172  0.0035983    6.202
    ## year2012                                  0.0271117  0.0035764    7.581
    ## year2013                                  0.0629785  0.0039745   15.846
    ## year2014                                  0.0649181  0.0039870   16.283
    ## year2015                                  0.0709891  0.0039818   17.828
    ## year2016                                  0.0787390  0.0039600   19.884
    ## year2017                                  0.1155829  0.0085750   13.479

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm0)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          0.125    0.00365      34.4 
    ##  2 fixed  <NA>  raw_income_scale                    -0.191    0.00151    -127.  
    ##  3 fixed  <NA>  total_pop_county_scale               0.00940  0.000797     11.8 
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -0.0243   0.000847    -28.7 
    ##  5 fixed  <NA>  land_area_2010_scale                -0.00643  0.000737     -8.72
    ##  6 fixed  <NA>  physicians_scale                    -0.0139   0.000792    -17.6 
    ##  7 fixed  <NA>  education_scale                     -0.144    0.000799   -181.  
    ##  8 fixed  <NA>  employment_all1                      0.135    0.000827    163.  
    ##  9 fixed  <NA>  sex1                                 0.0395   0.000720     55.0 
    ## 10 fixed  <NA>  age_scale                            0.0910   0.000937     97.2 
    ## # ... with 22 more rows

``` r
glance(lm0)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.921 -2352952. 4705969. 4706365. 4705905.     1759178

#### Main effect model

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
      data = dfg_current
    )
```

    ## boundary (singular) fit: see ?isSingular

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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4699234  4699680 -2349581  4699162  1759174 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7954 -0.7749 -0.0390  0.7099  6.8473 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0038374 0.06195       
    ##              raw_income_scale         0.0012212 0.03495  -0.47
    ##  fips_code.1 (Intercept)              0.0000000 0.00000       
    ##              median_income_demo_scale 0.0004181 0.02045   NaN 
    ##  Residual                             0.8444035 0.91891       
    ## Number of obs: 1759210, groups:  fips_code, 3129
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.1495909  0.0046613   32.092
    ## raw_income_scale                         -0.1900993  0.0013471 -141.116
    ## median_income_demo_scale                  0.1020464  0.0018577   54.930
    ## total_pop_county_scale                    0.0301879  0.0058433    5.166
    ## median_monthly_housing_cost_county_scale -0.0328453  0.0020136  -16.312
    ## land_area_2010_scale                     -0.0070406  0.0015797   -4.457
    ## physicians_scale                         -0.0123033  0.0015259   -8.063
    ## education_scale                          -0.2200246  0.0015306 -143.752
    ## employment_all1                           0.1420746  0.0008363  169.888
    ## sex1                                      0.0184307  0.0008015   22.995
    ## age_scale                                 0.0928088  0.0009389   98.847
    ## race1                                    -0.1174608  0.0017684  -66.422
    ## race2                                     0.0479066  0.0039066   12.263
    ## race3                                    -0.0263177  0.0025452  -10.340
    ## race4                                     0.0280826  0.0041994    6.687
    ## married1                                 -0.0454394  0.0019422  -23.395
    ## married2                                 -0.0865224  0.0015135  -57.168
    ## married3                                  0.1474968  0.0042856   34.417
    ## married4                                  0.0394540  0.0020685   19.073
    ## married5                                 -0.0813768  0.0024151  -33.695
    ## year2009                                  0.0185681  0.0036056    5.150
    ## year2010                                  0.0349257  0.0036057    9.686
    ## year2011                                  0.0285817  0.0035943    7.952
    ## year2012                                  0.0210799  0.0035759    5.895
    ## year2013                                  0.0580329  0.0039818   14.575
    ## year2014                                  0.0599446  0.0039909   15.020
    ## year2015                                  0.0381739  0.0040233    9.488
    ## year2016                                  0.0248910  0.0040699    6.116
    ## year2017                                  0.0672097  0.0086081    7.808

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
    ##  1 fixed  <NA>  (Intercept)                          0.150    0.00466      32.1 
    ##  2 fixed  <NA>  raw_income_scale                    -0.190    0.00135    -141.  
    ##  3 fixed  <NA>  median_income_demo_scale             0.102    0.00186      54.9 
    ##  4 fixed  <NA>  total_pop_county_scale               0.0302   0.00584       5.17
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0328   0.00201     -16.3 
    ##  6 fixed  <NA>  land_area_2010_scale                -0.00704  0.00158      -4.46
    ##  7 fixed  <NA>  physicians_scale                    -0.0123   0.00153      -8.06
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
    ## 1 0.919 -2349581. 4699234. 4699680. 4699162.     1759174

#### Interactive model

``` r
  lm2 <-
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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0033062 (tol = 0.002, component 1)

``` r
summary(lm2)
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4695826  4696556 -2347854  4695708  1759151 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0031 -0.7767 -0.0437  0.7097  6.8471 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0034787 0.05898       
    ##              raw_income_scale         0.0010334 0.03215  -0.43
    ##  fips_code.1 (Intercept)              0.0002340 0.01530       
    ##              median_income_demo_scale 0.0003765 0.01940  -0.42
    ##  Residual                             0.8428392 0.91806       
    ## Number of obs: 1759210, groups:  fips_code, 3129
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error  t value
    ## (Intercept)                                0.1236577  0.0047406   26.085
    ## median_income_demo_scale                   0.0730171  0.0041984   17.392
    ## raw_income_scale                          -0.1880733  0.0013072 -143.873
    ## education_scale                           -0.2128916  0.0015913 -133.783
    ## employment_all1                            0.1422283  0.0008414  169.046
    ## sex1                                       0.0195784  0.0008084   24.218
    ## age_scale                                  0.0903225  0.0009972   90.574
    ## race1                                     -0.1069284  0.0018258  -58.566
    ## race2                                      0.0594231  0.0039794   14.933
    ## race3                                     -0.0109493  0.0026001   -4.211
    ## race4                                      0.0200866  0.0044465    4.517
    ## married1                                  -0.0354144  0.0019918  -17.780
    ## married2                                  -0.0873085  0.0015509  -56.296
    ## married3                                   0.1236197  0.0044573   27.734
    ## married4                                   0.0405515  0.0021064   19.251
    ## married5                                  -0.0560625  0.0027994  -20.027
    ## year2009                                   0.0173801  0.0036202    4.801
    ## year2010                                   0.0348195  0.0036146    9.633
    ## year2011                                   0.0303540  0.0036108    8.406
    ## year2012                                   0.0234514  0.0035808    6.549
    ## year2013                                   0.0600519  0.0039857   15.067
    ## year2014                                   0.0619939  0.0040013   15.494
    ## year2015                                   0.0362919  0.0040497    8.962
    ## year2016                                   0.0211053  0.0041848    5.043
    ## year2017                                   0.0700621  0.0090810    7.715
    ## total_pop_county_scale                     0.0275357  0.0058359    4.718
    ## median_monthly_housing_cost_county_scale  -0.0347838  0.0020047  -17.351
    ## land_area_2010_scale                      -0.0065564  0.0015732   -4.168
    ## physicians_scale                          -0.0128499  0.0015197   -8.455
    ## median_income_demo_scale:raw_income_scale  0.0149464  0.0009692   15.421
    ## median_income_demo_scale:education_scale   0.0069612  0.0009299    7.486
    ## median_income_demo_scale:employment_all1  -0.0049844  0.0009101   -5.477
    ## median_income_demo_scale:sex1              0.0067448  0.0007951    8.483
    ## median_income_demo_scale:age_scale         0.0050129  0.0010278    4.877
    ## median_income_demo_scale:race1             0.0044306  0.0018166    2.439
    ## median_income_demo_scale:race2             0.0006976  0.0040227    0.173
    ## median_income_demo_scale:race3             0.0223622  0.0025946    8.619
    ## median_income_demo_scale:race4             0.0466479  0.0044223   10.548
    ## median_income_demo_scale:married1          0.0522986  0.0020380   25.662
    ## median_income_demo_scale:married2         -0.0028331  0.0015753   -1.798
    ## median_income_demo_scale:married3         -0.0519591  0.0042654  -12.182
    ## median_income_demo_scale:married4         -0.0236070  0.0022258  -10.606
    ## median_income_demo_scale:married5          0.0444398  0.0026708   16.639
    ## median_income_demo_scale:year2009          0.0071865  0.0038589    1.862
    ## median_income_demo_scale:year2010          0.0126749  0.0038560    3.287
    ## median_income_demo_scale:year2011          0.0188711  0.0038530    4.898
    ## median_income_demo_scale:year2012          0.0203958  0.0038461    5.303
    ## median_income_demo_scale:year2013          0.0233097  0.0042441    5.492
    ## median_income_demo_scale:year2014          0.0259284  0.0042885    6.046
    ## median_income_demo_scale:year2015          0.0314907  0.0041941    7.508
    ## median_income_demo_scale:year2016          0.0278624  0.0041532    6.709
    ## median_income_demo_scale:year2017          0.0088151  0.0078801    1.119

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0033062 (tol = 0.002, component 1)

``` r
tidy(lm2)
```

    ## # A tibble: 59 x 6
    ##    effect group term                     estimate std.error statistic
    ##    <chr>  <chr> <chr>                       <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                0.124   0.00474      26.1 
    ##  2 fixed  <NA>  median_income_demo_scale   0.0730  0.00420      17.4 
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
glance(lm2)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.918 -2347854. 4695826. 4696556. 4695708.     1759151

#### Model comparison

``` r
anova(lm0, lm1 , lm2)
```

    ## Data: dfg_current
    ## Models:
    ## lm0: sr_health_scale ~ raw_income_scale + total_pop_county_scale + 
    ## lm0:     median_monthly_housing_cost_county_scale + land_area_2010_scale + 
    ## lm0:     physicians_scale + education_scale + employment_all + sex + 
    ## lm0:     age_scale + race + married + year + (1 + raw_income_scale | 
    ## lm0:     fips_code)
    ## lm1: sr_health_scale ~ raw_income_scale + median_income_demo_scale + 
    ## lm1:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm1:     land_area_2010_scale + physicians_scale + education_scale + 
    ## lm1:     employment_all + sex + age_scale + race + married + year + 
    ## lm1:     (1 + raw_income_scale | fips_code) + (1 + median_income_demo_scale | 
    ## lm1:     fips_code)
    ## lm2: sr_health_scale ~ median_income_demo_scale * raw_income_scale + 
    ## lm2:     median_income_demo_scale * education_scale + median_income_demo_scale * 
    ## lm2:     employment_all + median_income_demo_scale * sex + median_income_demo_scale * 
    ## lm2:     age_scale + median_income_demo_scale * race + median_income_demo_scale * 
    ## lm2:     married + median_income_demo_scale * year + median_income_demo_scale + 
    ## lm2:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm2:     land_area_2010_scale + physicians_scale + (1 + raw_income_scale | 
    ## lm2:     fips_code) + (1 + median_income_demo_scale | fips_code)
    ##     npar     AIC     BIC   logLik deviance  Chisq Df Pr(>Chisq)    
    ## lm0   32 4705969 4706365 -2352952  4705905                         
    ## lm1   36 4699234 4699680 -2349581  4699162 6742.7  4  < 2.2e-16 ***
    ## lm2   59 4695826 4696556 -2347854  4695708 3454.2 23  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Life satisfaction

#### Data selection

``` r
dfg_current <-
  dfg_rs %>%
  filter_at(
    vars(
      ladder_now_scale,
      median_income_demo_scale,
      education_scale,
      raw_income_scale,
      median_home_value_county_scale,
      median_monthly_housing_cost_county_scale,
      physicians_scale,
      total_pop_county_scale,
      land_area_2010_scale,
      race,
      sex,
      age_scale,
      married,
      employment_all,
      year
    ),
    all_vars(!is.na(.))
  )
```

#### Baseline Model

``` r
  lm0 <-
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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm0)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: ladder_now_scale ~ raw_income_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     physicians_scale + education_scale + employment_all + sex +  
    ##     age_scale + race + married + year + (1 + raw_income_scale |      fips_code)
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5441426  5441826 -2720681  5441362  2000223 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1971 -0.6165  0.1020  0.6492  2.8295 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance Std.Dev. Corr 
    ##  fips_code (Intercept)      0.002301 0.04797       
    ##            raw_income_scale 0.001249 0.03534  -0.63
    ##  Residual                   0.887680 0.94217       
    ## Number of obs: 2000255, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.2300990  0.0033136  -69.441
    ## raw_income_scale                          0.2278622  0.0013019  175.028
    ## total_pop_county_scale                   -0.0281042  0.0041294   -6.806
    ## median_monthly_housing_cost_county_scale -0.0263251  0.0015990  -16.463
    ## land_area_2010_scale                      0.0081962  0.0013812    5.934
    ## physicians_scale                          0.0090941  0.0012902    7.049
    ## education_scale                           0.0560736  0.0007705   72.772
    ## employment_all1                          -0.0129864  0.0007987  -16.260
    ## sex1                                     -0.0740741  0.0006902 -107.316
    ## age_scale                                 0.0927089  0.0009031  102.658
    ## race1                                    -0.0628075  0.0016924  -37.112
    ## race2                                    -0.0745552  0.0037568  -19.845
    ## race3                                     0.0345827  0.0024382   14.184
    ## race4                                    -0.0331264  0.0040230   -8.234
    ## married1                                  0.0672767  0.0018540   36.287
    ## married2                                  0.1246002  0.0014487   86.011
    ## married3                                 -0.2294395  0.0041496  -55.291
    ## married4                                 -0.1107821  0.0019833  -55.857
    ## married5                                  0.1039986  0.0023216   44.797
    ## year2009                                  0.1725618  0.0027219   63.397
    ## year2010                                  0.1811689  0.0026157   69.263
    ## year2011                                  0.1888237  0.0025959   72.739
    ## year2012                                  0.1657178  0.0025652   64.601
    ## year2013                                  0.1879997  0.0031296   60.071
    ## year2014                                  0.2005664  0.0031452   63.770
    ## year2015                                  0.2055155  0.0031355   65.545
    ## year2016                                  0.1934156  0.0031068   62.256
    ## year2017                                  0.2064412  0.0032141   64.230

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm0)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.230    0.00331     -69.4 
    ##  2 fixed  <NA>  raw_income_scale                     0.228    0.00130     175.  
    ##  3 fixed  <NA>  total_pop_county_scale              -0.0281   0.00413      -6.81
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -0.0263   0.00160     -16.5 
    ##  5 fixed  <NA>  land_area_2010_scale                 0.00820  0.00138       5.93
    ##  6 fixed  <NA>  physicians_scale                     0.00909  0.00129       7.05
    ##  7 fixed  <NA>  education_scale                      0.0561   0.000771     72.8 
    ##  8 fixed  <NA>  employment_all1                     -0.0130   0.000799    -16.3 
    ##  9 fixed  <NA>  sex1                                -0.0741   0.000690   -107.  
    ## 10 fixed  <NA>  age_scale                            0.0927   0.000903    103.  
    ## # ... with 22 more rows

``` r
glance(lm0)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.942 -2720681. 5441426. 5441826. 5441362.     2000223

#### Main effect model

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
      data = dfg_current
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
    ## Formula: ladder_now_scale ~ raw_income_scale + median_income_demo_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code) + (1 + median_income_demo_scale |  
    ##     fips_code)
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5427907  5428357 -2713917  5427835  2000219 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4706 -0.6166  0.1036  0.6514  2.8535 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0015113 0.03888       
    ##              raw_income_scale         0.0010936 0.03307  -0.71
    ##  fips_code.1 (Intercept)              0.0005696 0.02387       
    ##              median_income_demo_scale 0.0002434 0.01560  -0.13
    ##  Residual                             0.8816312 0.93895       
    ## Number of obs: 2000255, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.2612084  0.0032761  -79.731
    ## raw_income_scale                          0.2334083  0.0012761  182.914
    ## median_income_demo_scale                 -0.1788454  0.0016687 -107.177
    ## total_pop_county_scale                   -0.0265678  0.0040166   -6.614
    ## median_monthly_housing_cost_county_scale -0.0260106  0.0015657  -16.613
    ## land_area_2010_scale                      0.0078046  0.0013580    5.747
    ## physicians_scale                          0.0083269  0.0012683    6.565
    ## education_scale                           0.1940842  0.0014174  136.933
    ## employment_all1                          -0.0267730  0.0008048  -33.269
    ## sex1                                     -0.0354564  0.0007644  -46.385
    ## age_scale                                 0.0904847  0.0009005  100.485
    ## race1                                    -0.0591295  0.0016871  -35.048
    ## race2                                    -0.0722153  0.0037440  -19.288
    ## race3                                     0.0456161  0.0024314   18.761
    ## race4                                    -0.0397420  0.0040126   -9.904
    ## married1                                  0.0437302  0.0018593   23.519
    ## married2                                  0.1415536  0.0014513   97.536
    ## married3                                 -0.2102276  0.0041391  -50.791
    ## married4                                 -0.0898045  0.0019850  -45.241
    ## married5                                  0.0729238  0.0023296   31.303
    ## year2009                                  0.1607231  0.0027147   59.205
    ## year2010                                  0.1782251  0.0026070   68.365
    ## year2011                                  0.1797961  0.0025882   69.467
    ## year2012                                  0.1790585  0.0025591   69.969
    ## year2013                                  0.1985399  0.0031200   63.634
    ## year2014                                  0.2102731  0.0031354   67.064
    ## year2015                                  0.2646896  0.0031662   83.599
    ## year2016                                  0.2913138  0.0032092   90.773
    ## year2017                                  0.2983146  0.0032992   90.420

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
    ##  1 fixed  <NA>  (Intercept)                         -0.261    0.00328     -79.7 
    ##  2 fixed  <NA>  raw_income_scale                     0.233    0.00128     183.  
    ##  3 fixed  <NA>  median_income_demo_scale            -0.179    0.00167    -107.  
    ##  4 fixed  <NA>  total_pop_county_scale              -0.0266   0.00402      -6.61
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0260   0.00157     -16.6 
    ##  6 fixed  <NA>  land_area_2010_scale                 0.00780  0.00136       5.75
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
    ## 1 0.939 -2713917. 5427907. 5428357. 5427835.     2000219

#### Interactive model

``` r
  lm2 <-
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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0454095 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm2)
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5421799  5422537 -2710841  5421681  2000196 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4282 -0.6158  0.1010  0.6506  2.9435 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0011784 0.03433       
    ##              raw_income_scale         0.0011446 0.03383  -0.79
    ##  fips_code.1 (Intercept)              0.0008715 0.02952       
    ##              median_income_demo_scale 0.0001651 0.01285  -0.32
    ##  Residual                             0.8789593 0.93753       
    ## Number of obs: 2000255, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error t value
    ## (Intercept)                               -2.966e-01  3.361e-03 -88.250
    ## median_income_demo_scale                  -1.885e-01  3.055e-03 -61.689
    ## raw_income_scale                           2.314e-01  1.287e-03 179.813
    ## education_scale                            2.069e-01  1.469e-03 140.919
    ## employment_all1                           -2.815e-02  8.078e-04 -34.850
    ## sex1                                      -3.558e-02  7.713e-04 -46.136
    ## age_scale                                  9.337e-02  9.434e-04  98.969
    ## race1                                     -5.593e-02  1.749e-03 -31.974
    ## race2                                     -7.334e-02  3.794e-03 -19.333
    ## race3                                      4.260e-02  2.477e-03  17.197
    ## race4                                     -2.262e-02  4.319e-03  -5.237
    ## married1                                   4.900e-02  1.895e-03  25.853
    ## married2                                   1.471e-01  1.482e-03  99.274
    ## married3                                  -2.162e-01  4.264e-03 -50.694
    ## married4                                  -7.866e-02  2.014e-03 -39.058
    ## married5                                   5.015e-02  2.664e-03  18.825
    ## year2009                                   1.567e-01  2.734e-03  57.336
    ## year2010                                   1.729e-01  2.616e-03  66.104
    ## year2011                                   1.718e-01  2.608e-03  65.878
    ## year2012                                   1.765e-01  2.562e-03  68.876
    ## year2013                                   1.956e-01  3.121e-03  62.685
    ## year2014                                   2.080e-01  3.144e-03  66.171
    ## year2015                                   2.614e-01  3.201e-03  81.663
    ## year2016                                   2.898e-01  3.369e-03  86.005
    ## year2017                                   2.722e-01  3.401e-03  80.013
    ## total_pop_county_scale                    -2.871e-02  3.903e-03  -7.354
    ## median_monthly_housing_cost_county_scale  -2.805e-02  1.544e-03 -18.170
    ## land_area_2010_scale                       8.220e-03  1.348e-03   6.099
    ## physicians_scale                           6.997e-03  1.256e-03   5.570
    ## median_income_demo_scale:raw_income_scale  1.272e-02  9.135e-04  13.926
    ## median_income_demo_scale:education_scale   2.420e-02  8.696e-04  27.825
    ## median_income_demo_scale:employment_all1   1.458e-04  8.626e-04   0.169
    ## median_income_demo_scale:sex1              7.849e-03  7.500e-04  10.465
    ## median_income_demo_scale:age_scale         1.136e-05  9.618e-04   0.012
    ## median_income_demo_scale:race1             3.796e-02  1.713e-03  22.161
    ## median_income_demo_scale:race2             6.005e-03  3.826e-03   1.570
    ## median_income_demo_scale:race3            -1.676e-02  2.440e-03  -6.869
    ## median_income_demo_scale:race4            -2.556e-02  4.204e-03  -6.080
    ## median_income_demo_scale:married1         -2.230e-02  1.922e-03 -11.605
    ## median_income_demo_scale:married2          1.863e-02  1.487e-03  12.529
    ## median_income_demo_scale:married3         -2.082e-02  4.057e-03  -5.131
    ## median_income_demo_scale:married4          3.116e-02  2.106e-03  14.798
    ## median_income_demo_scale:married5         -2.046e-02  2.523e-03  -8.110
    ## median_income_demo_scale:year2009         -3.680e-02  2.897e-03 -12.701
    ## median_income_demo_scale:year2010         -4.394e-02  2.778e-03 -15.817
    ## median_income_demo_scale:year2011         -5.410e-02  2.772e-03 -19.517
    ## median_income_demo_scale:year2012         -6.674e-02  2.762e-03 -24.165
    ## median_income_demo_scale:year2013         -2.757e-02  3.321e-03  -8.301
    ## median_income_demo_scale:year2014         -2.743e-02  3.380e-03  -8.114
    ## median_income_demo_scale:year2015         -2.888e-02  3.251e-03  -8.883
    ## median_income_demo_scale:year2016         -3.323e-02  3.193e-03 -10.408
    ## median_income_demo_scale:year2017          2.743e-02  3.088e-03   8.882

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0454095 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm2)
```

    ## # A tibble: 59 x 6
    ##    effect group term                     estimate std.error statistic
    ##    <chr>  <chr> <chr>                       <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)               -0.297   0.00336      -88.2
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
glance(lm2)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.938 -2710841. 5421799. 5422537. 5421681.     2000196

#### Model comparison

``` r
anova(lm0, lm1 , lm2)
```

    ## Data: dfg_current
    ## Models:
    ## lm0: ladder_now_scale ~ raw_income_scale + total_pop_county_scale + 
    ## lm0:     median_monthly_housing_cost_county_scale + land_area_2010_scale + 
    ## lm0:     physicians_scale + education_scale + employment_all + sex + 
    ## lm0:     age_scale + race + married + year + (1 + raw_income_scale | 
    ## lm0:     fips_code)
    ## lm1: ladder_now_scale ~ raw_income_scale + median_income_demo_scale + 
    ## lm1:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm1:     land_area_2010_scale + physicians_scale + education_scale + 
    ## lm1:     employment_all + sex + age_scale + race + married + year + 
    ## lm1:     (1 + raw_income_scale | fips_code) + (1 + median_income_demo_scale | 
    ## lm1:     fips_code)
    ## lm2: ladder_now_scale ~ median_income_demo_scale * raw_income_scale + 
    ## lm2:     median_income_demo_scale * education_scale + median_income_demo_scale * 
    ## lm2:     employment_all + median_income_demo_scale * sex + median_income_demo_scale * 
    ## lm2:     age_scale + median_income_demo_scale * race + median_income_demo_scale * 
    ## lm2:     married + median_income_demo_scale * year + median_income_demo_scale + 
    ## lm2:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm2:     land_area_2010_scale + physicians_scale + (1 + raw_income_scale | 
    ## lm2:     fips_code) + (1 + median_income_demo_scale | fips_code)
    ##     npar     AIC     BIC   logLik deviance   Chisq Df Pr(>Chisq)    
    ## lm0   32 5441426 5441826 -2720681  5441362                          
    ## lm1   36 5427907 5428357 -2713917  5427835 13527.4  4  < 2.2e-16 ***
    ## lm2   59 5421799 5422537 -2710841  5421681  6153.5 23  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### BMI

#### Data selection

``` r
dfg_current <-
  dfg_rs %>%
  filter_at(
    vars(
      bmi_scale,
      median_income_demo_scale,
      education_scale,
      raw_income_scale,
      median_home_value_county_scale,
      median_monthly_housing_cost_county_scale,
      physicians_scale,
      total_pop_county_scale,
      land_area_2010_scale,
      race,
      sex,
      age_scale,
      married,
      employment_all,
      year
    ),
    all_vars(!is.na(.))
  )
```

#### Baseline Model

``` r
  lm0 <-
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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm0)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## bmi_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code)
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5642872  5643272 -2821404  5642808  2005545 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7820 -0.6609 -0.1629  0.4716 21.9441 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance  Std.Dev. Corr 
    ##  fips_code (Intercept)      0.0059823 0.07735       
    ##            raw_income_scale 0.0004886 0.02211  -0.32
    ##  Residual                   0.9740737 0.98695       
    ## Number of obs: 2005577, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error t value
    ## (Intercept)                               0.0039270  0.0046033   0.853
    ## raw_income_scale                         -0.0551183  0.0011485 -47.992
    ## total_pop_county_scale                   -0.0321326  0.0075338  -4.265
    ## median_monthly_housing_cost_county_scale -0.0515314  0.0023750 -21.698
    ## land_area_2010_scale                     -0.0049576  0.0017587  -2.819
    ## physicians_scale                         -0.0158431  0.0017076  -9.278
    ## education_scale                          -0.0526149  0.0008066 -65.228
    ## employment_all1                           0.0002248  0.0008369   0.269
    ## sex1                                      0.0975830  0.0007231 134.959
    ## age_scale                                 0.0448518  0.0009448  47.474
    ## race1                                    -0.0349009  0.0017761 -19.651
    ## race2                                     0.0651899  0.0039170  16.643
    ## race3                                     0.2739929  0.0025555 107.219
    ## race4                                    -0.3634612  0.0042286 -85.954
    ## married1                                 -0.0675632  0.0019434 -34.765
    ## married2                                  0.0614446  0.0015200  40.425
    ## married3                                  0.0842859  0.0043461  19.393
    ## married4                                  0.0524692  0.0020689  25.360
    ## married5                                 -0.1094809  0.0024196 -45.247
    ## year2009                                  0.0120054  0.0027521   4.362
    ## year2010                                  0.0161628  0.0027572   5.862
    ## year2011                                  0.0005374  0.0027405   0.196
    ## year2012                                  0.0117722  0.0027091   4.345
    ## year2013                                  0.0245475  0.0033143   7.406
    ## year2014                                  0.0445640  0.0033302  13.382
    ## year2015                                  0.0661581  0.0033178  19.941
    ## year2016                                  0.0666015  0.0032895  20.247
    ## year2017                                  0.0723209  0.0034119  21.197

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm0)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          3.93e-3  0.00460      0.853
    ##  2 fixed  <NA>  raw_income_scale                    -5.51e-2  0.00115    -48.0  
    ##  3 fixed  <NA>  total_pop_county_scale              -3.21e-2  0.00753     -4.27 
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -5.15e-2  0.00237    -21.7  
    ##  5 fixed  <NA>  land_area_2010_scale                -4.96e-3  0.00176     -2.82 
    ##  6 fixed  <NA>  physicians_scale                    -1.58e-2  0.00171     -9.28 
    ##  7 fixed  <NA>  education_scale                     -5.26e-2  0.000807   -65.2  
    ##  8 fixed  <NA>  employment_all1                      2.25e-4  0.000837     0.269
    ##  9 fixed  <NA>  sex1                                 9.76e-2  0.000723   135.   
    ## 10 fixed  <NA>  age_scale                            4.49e-2  0.000945    47.5  
    ## # ... with 22 more rows

``` r
glance(lm0)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.987 -2821404. 5642872. 5643272. 5642808.     2005545

#### Main effect model

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
      data = dfg_current
    )
```

    ## boundary (singular) fit: see ?isSingular

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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5625543  5625994 -2812736  5625471  2005541 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6750 -0.6601 -0.1607  0.4719 22.1626 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr
    ##  fips_code   (Intercept)              0.0000000 0.00000      
    ##              raw_income_scale         0.0003842 0.01960   NaN
    ##  fips_code.1 (Intercept)              0.0057906 0.07610      
    ##              median_income_demo_scale 0.0007276 0.02697  0.31
    ##  Residual                             0.9654249 0.98256      
    ## Number of obs: 2005577, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.0466165  0.0045944   10.146
    ## raw_income_scale                         -0.0645110  0.0011198  -57.611
    ## median_income_demo_scale                  0.2201037  0.0018452  119.285
    ## total_pop_county_scale                   -0.0225919  0.0074935   -3.015
    ## median_monthly_housing_cost_county_scale -0.0512700  0.0023473  -21.842
    ## land_area_2010_scale                     -0.0050747  0.0017293   -2.935
    ## physicians_scale                         -0.0138177  0.0016858   -8.196
    ## education_scale                          -0.2156300  0.0014807 -145.631
    ## employment_all1                           0.0163951  0.0008424   19.463
    ## sex1                                      0.0514395  0.0008013   64.194
    ## age_scale                                 0.0480965  0.0009413   51.098
    ## race1                                    -0.0376748  0.0017684  -21.304
    ## race2                                     0.0620441  0.0038992   15.912
    ## race3                                     0.2619936  0.0025439  102.988
    ## race4                                    -0.3527661  0.0042149  -83.694
    ## married1                                 -0.0389028  0.0019474  -19.977
    ## married2                                  0.0410843  0.0015213   27.007
    ## married3                                  0.0604934  0.0043309   13.968
    ## married4                                  0.0273017  0.0020687   13.198
    ## married5                                 -0.0722675  0.0024262  -29.786
    ## year2009                                  0.0260166  0.0027422    9.487
    ## year2010                                  0.0197634  0.0027453    7.199
    ## year2011                                  0.0107465  0.0027297    3.937
    ## year2012                                 -0.0042717  0.0027001   -1.582
    ## year2013                                  0.0119354  0.0033012    3.615
    ## year2014                                  0.0328057  0.0033169    9.890
    ## year2015                                 -0.0042515  0.0033468   -1.270
    ## year2016                                 -0.0491492  0.0033926  -14.487
    ## year2017                                 -0.0366044  0.0034975  -10.466

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
    ##  1 fixed  <NA>  (Intercept)                          0.0466   0.00459      10.1 
    ##  2 fixed  <NA>  raw_income_scale                    -0.0645   0.00112     -57.6 
    ##  3 fixed  <NA>  median_income_demo_scale             0.220    0.00185     119.  
    ##  4 fixed  <NA>  total_pop_county_scale              -0.0226   0.00749      -3.01
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0513   0.00235     -21.8 
    ##  6 fixed  <NA>  land_area_2010_scale                -0.00507  0.00173      -2.93
    ##  7 fixed  <NA>  physicians_scale                    -0.0138   0.00169      -8.20
    ##  8 fixed  <NA>  education_scale                     -0.216    0.00148    -146.  
    ##  9 fixed  <NA>  employment_all1                      0.0164   0.000842     19.5 
    ## 10 fixed  <NA>  sex1                                 0.0514   0.000801     64.2 
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.983 -2812736. 5625543. 5625994. 5625471.     2005541

#### Interactive model

``` r
  lm2 <-
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
      data = dfg_current
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm2)
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5616060  5616798 -2807971  5615942  2005518 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8488 -0.6590 -0.1615  0.4711 22.6299 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr
    ##  fips_code   (Intercept)              0.0000000 0.00000      
    ##              raw_income_scale         0.0003869 0.01967   NaN
    ##  fips_code.1 (Intercept)              0.0058527 0.07650      
    ##              median_income_demo_scale 0.0007584 0.02754  0.30
    ##  Residual                             0.9608208 0.98021      
    ## Number of obs: 2005577, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error  t value
    ## (Intercept)                                0.0487701  0.0046832   10.414
    ## median_income_demo_scale                   0.2159057  0.0033008   65.410
    ## raw_income_scale                          -0.0626237  0.0011220  -55.816
    ## education_scale                           -0.2276511  0.0015334 -148.465
    ## employment_all1                            0.0152662  0.0008444   18.079
    ## sex1                                       0.0485715  0.0008077   60.134
    ## age_scale                                  0.0471321  0.0009861   47.798
    ## race1                                     -0.0396852  0.0018313  -21.670
    ## race2                                      0.0597467  0.0039465   15.139
    ## race3                                      0.2619738  0.0025923  101.057
    ## race4                                     -0.3408385  0.0045332  -75.186
    ## married1                                  -0.0353322  0.0019807  -17.838
    ## married2                                   0.0409873  0.0015508   26.430
    ## married3                                   0.0352405  0.0044428    7.932
    ## married4                                   0.0254325  0.0020969   12.129
    ## married5                                  -0.0365078  0.0027797  -13.134
    ## year2009                                   0.0272302  0.0027572    9.876
    ## year2010                                   0.0207495  0.0027518    7.540
    ## year2011                                   0.0135156  0.0027470    4.920
    ## year2012                                  -0.0022265  0.0027011   -0.824
    ## year2013                                   0.0119978  0.0032998    3.636
    ## year2014                                   0.0316966  0.0033227    9.539
    ## year2015                                  -0.0081690  0.0033838   -2.414
    ## year2016                                  -0.0526753  0.0035648  -14.776
    ## year2017                                  -0.0119339  0.0036098   -3.306
    ## total_pop_county_scale                    -0.0240273  0.0075357   -3.188
    ## median_monthly_housing_cost_county_scale  -0.0499116  0.0023548  -21.196
    ## land_area_2010_scale                      -0.0052613  0.0017312   -3.039
    ## physicians_scale                          -0.0140409  0.0016883   -8.316
    ## median_income_demo_scale:raw_income_scale  0.0040846  0.0009552    4.276
    ## median_income_demo_scale:education_scale  -0.0156684  0.0009104  -17.210
    ## median_income_demo_scale:employment_all1  -0.0158842  0.0009071  -17.511
    ## median_income_demo_scale:sex1              0.0522450  0.0007868   66.406
    ## median_income_demo_scale:age_scale         0.0299299  0.0010076   29.703
    ## median_income_demo_scale:race1             0.0225143  0.0018082   12.452
    ## median_income_demo_scale:race2            -0.0091254  0.0039935   -2.285
    ## median_income_demo_scale:race3            -0.0148300  0.0025677   -5.776
    ## median_income_demo_scale:race4             0.0048651  0.0044240    1.100
    ## median_income_demo_scale:married1          0.0803436  0.0020215   39.745
    ## median_income_demo_scale:married2         -0.0297910  0.0015714  -18.959
    ## median_income_demo_scale:married3         -0.0579748  0.0042776  -13.553
    ## median_income_demo_scale:married4         -0.0312998  0.0021950  -14.260
    ## median_income_demo_scale:married5          0.0403489  0.0026306   15.338
    ## median_income_demo_scale:year2009          0.0008303  0.0029247    0.284
    ## median_income_demo_scale:year2010          0.0009496  0.0029232    0.325
    ## median_income_demo_scale:year2011          0.0090983  0.0029207    3.115
    ## median_income_demo_scale:year2012          0.0143457  0.0029103    4.929
    ## median_income_demo_scale:year2013          0.0018576  0.0035025    0.530
    ## median_income_demo_scale:year2014          0.0023695  0.0035720    0.663
    ## median_income_demo_scale:year2015         -0.0091282  0.0034317   -2.660
    ## median_income_demo_scale:year2016         -0.0010833  0.0033738   -0.321
    ## median_income_demo_scale:year2017         -0.0776309  0.0032658  -23.771

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm2)
```

    ## # A tibble: 59 x 6
    ##    effect group term                     estimate std.error statistic
    ##    <chr>  <chr> <chr>                       <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                0.0488  0.00468       10.4
    ##  2 fixed  <NA>  median_income_demo_scale   0.216   0.00330       65.4
    ##  3 fixed  <NA>  raw_income_scale          -0.0626  0.00112      -55.8
    ##  4 fixed  <NA>  education_scale           -0.228   0.00153     -148. 
    ##  5 fixed  <NA>  employment_all1            0.0153  0.000844      18.1
    ##  6 fixed  <NA>  sex1                       0.0486  0.000808      60.1
    ##  7 fixed  <NA>  age_scale                  0.0471  0.000986      47.8
    ##  8 fixed  <NA>  race1                     -0.0397  0.00183      -21.7
    ##  9 fixed  <NA>  race2                      0.0597  0.00395       15.1
    ## 10 fixed  <NA>  race3                      0.262   0.00259      101. 
    ## # ... with 49 more rows

``` r
glance(lm2)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.980 -2807971. 5616060. 5616798. 5615942.     2005518

#### Model comparison

``` r
anova(lm0, lm1 , lm2)
```

    ## Data: dfg_current
    ## Models:
    ## lm0: bmi_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm0:     land_area_2010_scale + physicians_scale + education_scale + 
    ## lm0:     employment_all + sex + age_scale + race + married + year + 
    ## lm0:     (1 + raw_income_scale | fips_code)
    ## lm1: bmi_scale ~ raw_income_scale + median_income_demo_scale + total_pop_county_scale + 
    ## lm1:     median_monthly_housing_cost_county_scale + land_area_2010_scale + 
    ## lm1:     physicians_scale + education_scale + employment_all + sex + 
    ## lm1:     age_scale + race + married + year + (1 + raw_income_scale | 
    ## lm1:     fips_code) + (1 + median_income_demo_scale | fips_code)
    ## lm2: bmi_scale ~ median_income_demo_scale * raw_income_scale + median_income_demo_scale * 
    ## lm2:     education_scale + median_income_demo_scale * employment_all + 
    ## lm2:     median_income_demo_scale * sex + median_income_demo_scale * 
    ## lm2:     age_scale + median_income_demo_scale * race + median_income_demo_scale * 
    ## lm2:     married + median_income_demo_scale * year + median_income_demo_scale + 
    ## lm2:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm2:     land_area_2010_scale + physicians_scale + (1 + raw_income_scale | 
    ## lm2:     fips_code) + (1 + median_income_demo_scale | fips_code)
    ##     npar     AIC     BIC   logLik deviance   Chisq Df Pr(>Chisq)    
    ## lm0   32 5642872 5643272 -2821404  5642808                          
    ## lm1   36 5625543 5625994 -2812736  5625471 17336.8  4  < 2.2e-16 ***
    ## lm2   59 5616060 5616798 -2807971  5615942  9529.5 23  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Geographic Median Income

### Height

#### Data selection

``` r
dfg_current <-
  dfg_rs %>%
  filter_at(
    vars(
      height_scale,
      median_income_county_scale,
      education_scale,
      raw_income_scale,
      median_home_value_county_scale,
      median_monthly_housing_cost_county_scale,
      physicians_scale,
      total_pop_county_scale,
      land_area_2010_scale,
      race,
      sex,
      age_scale,
      married,
      employment_all,
      year
    ),
    all_vars(!is.na(.))
  )
```

#### Baseline Model

``` r
  lm0 <-
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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm0)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## height_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code)
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4206589  4206990 -2103263  4206525  2038675 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0891 -0.6555 -0.0147  0.6409 11.7080 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance  Std.Dev. Corr 
    ##  fips_code (Intercept)      0.0016349 0.04043       
    ##            raw_income_scale 0.0001101 0.01049  -0.01
    ##  Residual                   0.4602785 0.67844       
    ## Number of obs: 2038707, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1595742  0.0027578  -57.864
    ## raw_income_scale                          0.0493125  0.0007163   68.844
    ## total_pop_county_scale                   -0.0130119  0.0042175   -3.085
    ## median_monthly_housing_cost_county_scale -0.0132165  0.0013834   -9.554
    ## land_area_2010_scale                      0.0025158  0.0010831    2.323
    ## physicians_scale                         -0.0030746  0.0010404   -2.955
    ## education_scale                           0.0578790  0.0005496  105.318
    ## employment_all1                          -0.0086501  0.0005699  -15.179
    ## sex1                                      0.6873209  0.0004923 1396.095
    ## age_scale                                -0.1010497  0.0006443 -156.832
    ## race1                                     0.1959321  0.0012094  162.002
    ## race2                                     0.0972703  0.0026692   36.441
    ## race3                                     0.1923242  0.0017373  110.700
    ## race4                                    -0.3048904  0.0028865 -105.625
    ## married1                                  0.0031380  0.0013248    2.369
    ## married2                                  0.0092532  0.0010363    8.929
    ## married3                                  0.0037617  0.0029657    1.268
    ## married4                                  0.0197523  0.0014099   14.009
    ## married5                                 -0.0320802  0.0016482  -19.464
    ## year2009                                  0.0095799  0.0018776    5.102
    ## year2010                                  0.0086954  0.0018799    4.626
    ## year2011                                  0.0025461  0.0018675    1.363
    ## year2012                                 -0.0006188  0.0018457   -0.335
    ## year2013                                  0.0013051  0.0022563    0.578
    ## year2014                                  0.0031614  0.0022674    1.394
    ## year2015                                  0.0015921  0.0022595    0.705
    ## year2016                                  0.0013431  0.0022395    0.600
    ## year2017                                  0.0076107  0.0023154    3.287

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm0)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                         -0.160    0.00276     -57.9 
    ##  2 fixed  <NA>  raw_income_scale                     0.0493   0.000716     68.8 
    ##  3 fixed  <NA>  total_pop_county_scale              -0.0130   0.00422      -3.09
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -0.0132   0.00138      -9.55
    ##  5 fixed  <NA>  land_area_2010_scale                 0.00252  0.00108       2.32
    ##  6 fixed  <NA>  physicians_scale                    -0.00307  0.00104      -2.96
    ##  7 fixed  <NA>  education_scale                      0.0579   0.000550    105.  
    ##  8 fixed  <NA>  employment_all1                     -0.00865  0.000570    -15.2 
    ##  9 fixed  <NA>  sex1                                 0.687    0.000492   1396.  
    ## 10 fixed  <NA>  age_scale                           -0.101    0.000644   -157.  
    ## # ... with 22 more rows

``` r
glance(lm0)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2103263. 4206589. 4206990. 4206525.     2038675

#### Main effect model

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
      data = dfg_current
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4206592  4207043 -2103260  4206520  2038671 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0891 -0.6555 -0.0147  0.6409 11.7080 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev.  Corr 
    ##  fips_code   (Intercept)                1.629e-03 4.037e-02      
    ##              raw_income_scale           1.103e-04 1.050e-02 -0.02
    ##  fips_code.1 (Intercept)                4.955e-09 7.039e-05      
    ##              median_income_county_scale 5.278e-11 7.265e-06 0.20 
    ##  Residual                               4.603e-01 6.784e-01      
    ## Number of obs: 2038707, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.1591284  0.0027625  -57.603
    ## raw_income_scale                          0.0492733  0.0007166   68.756
    ## median_income_county_scale                0.0055799  0.0025633    2.177
    ## total_pop_county_scale                   -0.0111150  0.0042969   -2.587
    ## median_monthly_housing_cost_county_scale -0.0184473  0.0027702   -6.659
    ## land_area_2010_scale                      0.0024449  0.0010831    2.257
    ## physicians_scale                         -0.0030619  0.0010397   -2.945
    ## education_scale                           0.0578931  0.0005496  105.336
    ## employment_all1                          -0.0086279  0.0005700  -15.138
    ## sex1                                      0.6873237  0.0004923 1396.097
    ## age_scale                                -0.1010494  0.0006443 -156.832
    ## race1                                     0.1958516  0.0012100  161.858
    ## race2                                     0.0972435  0.0026693   36.431
    ## race3                                     0.1924470  0.0017383  110.709
    ## race4                                    -0.3049362  0.0028866 -105.638
    ## married1                                  0.0031371  0.0013248    2.368
    ## married2                                  0.0092289  0.0010363    8.905
    ## married3                                  0.0037877  0.0029657    1.277
    ## married4                                  0.0197500  0.0014099   14.008
    ## married5                                 -0.0321052  0.0016482  -19.479
    ## year2009                                  0.0095744  0.0018776    5.099
    ## year2010                                  0.0086922  0.0018799    4.624
    ## year2011                                  0.0025254  0.0018675    1.352
    ## year2012                                 -0.0006446  0.0018458   -0.349
    ## year2013                                  0.0011737  0.0022571    0.520
    ## year2014                                  0.0028071  0.0022732    1.235
    ## year2015                                  0.0009359  0.0022794    0.411
    ## year2016                                  0.0002319  0.0022967    0.101
    ## year2017                                  0.0059871  0.0024324    2.461

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
    ##  1 fixed  <NA>  (Intercept)                         -0.159    0.00276     -57.6 
    ##  2 fixed  <NA>  raw_income_scale                     0.0493   0.000717     68.8 
    ##  3 fixed  <NA>  median_income_county_scale           0.00558  0.00256       2.18
    ##  4 fixed  <NA>  total_pop_county_scale              -0.0111   0.00430      -2.59
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0184   0.00277      -6.66
    ##  6 fixed  <NA>  land_area_2010_scale                 0.00244  0.00108       2.26
    ##  7 fixed  <NA>  physicians_scale                    -0.00306  0.00104      -2.94
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
    ## 1 0.678 -2103260. 4206592. 4207043. 4206520.     2038671

#### Interactive model

``` r
  lm2 <-
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
      data = dfg_current
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm2)
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4206443  4207182 -2103163  4206325  2038648 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.0925 -0.6554 -0.0148  0.6409 11.7051 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev.  Corr 
    ##  fips_code   (Intercept)                1.318e-03 3.630e-02      
    ##              raw_income_scale           1.101e-04 1.049e-02 -0.02
    ##  fips_code.1 (Intercept)                3.107e-04 1.763e-02      
    ##              median_income_county_scale 9.570e-10 3.094e-05 -1.00
    ##  Residual                               4.602e-01 6.784e-01      
    ## Number of obs: 2038707, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                               Estimate Std. Error  t value
    ## (Intercept)                                 -0.1570998  0.0027980  -56.146
    ## median_income_county_scale                  -0.0007319  0.0032589   -0.225
    ## raw_income_scale                             0.0491423  0.0007202   68.232
    ## education_scale                              0.0577786  0.0005526  104.553
    ## employment_all1                             -0.0085756  0.0005712  -15.013
    ## sex1                                         0.6872227  0.0004926 1395.150
    ## age_scale                                   -0.1012084  0.0006455 -156.782
    ## race1                                        0.1940840  0.0012672  153.160
    ## race2                                        0.0937058  0.0027055   34.636
    ## race3                                        0.1913348  0.0017840  107.253
    ## race4                                       -0.2965745  0.0032215  -92.061
    ## married1                                     0.0028605  0.0013294    2.152
    ## married2                                     0.0092957  0.0010411    8.929
    ## married3                                     0.0045617  0.0029956    1.523
    ## married4                                     0.0198033  0.0014193   13.952
    ## married5                                    -0.0326637  0.0016680  -19.582
    ## year2009                                     0.0097440  0.0018901    5.155
    ## year2010                                     0.0087972  0.0018948    4.643
    ## year2011                                     0.0025159  0.0018756    1.341
    ## year2012                                    -0.0006364  0.0018527   -0.344
    ## year2013                                     0.0011249  0.0022635    0.497
    ## year2014                                     0.0027981  0.0022793    1.228
    ## year2015                                     0.0008508  0.0022859    0.372
    ## year2016                                     0.0002053  0.0023123    0.089
    ## year2017                                     0.0067360  0.0024787    2.718
    ## total_pop_county_scale                      -0.0107203  0.0043063   -2.489
    ## median_monthly_housing_cost_county_scale    -0.0190067  0.0028228   -6.733
    ## land_area_2010_scale                         0.0022571  0.0010835    2.083
    ## physicians_scale                            -0.0030787  0.0010401   -2.960
    ## median_income_county_scale:raw_income_scale  0.0010878  0.0007226    1.505
    ## median_income_county_scale:education_scale  -0.0016689  0.0005771   -2.892
    ## median_income_county_scale:employment_all1   0.0007327  0.0005868    1.249
    ## median_income_county_scale:sex1             -0.0041952  0.0005014   -8.367
    ## median_income_county_scale:age_scale        -0.0032039  0.0006638   -4.827
    ## median_income_county_scale:race1             0.0079982  0.0011805    6.775
    ## median_income_county_scale:race2            -0.0108918  0.0027607   -3.945
    ## median_income_county_scale:race3             0.0090073  0.0017390    5.180
    ## median_income_county_scale:race4            -0.0115183  0.0025900   -4.447
    ## median_income_county_scale:married1          0.0010464  0.0013702    0.764
    ## median_income_county_scale:married2         -0.0008917  0.0010639   -0.838
    ## median_income_county_scale:married3          0.0030393  0.0030463    0.998
    ## median_income_county_scale:married4          0.0007393  0.0014925    0.495
    ## median_income_county_scale:married5         -0.0043712  0.0017464   -2.503
    ## median_income_county_scale:year2009          0.0016640  0.0019986    0.833
    ## median_income_county_scale:year2010          0.0009749  0.0020003    0.487
    ## median_income_county_scale:year2011          0.0002227  0.0019687    0.113
    ## median_income_county_scale:year2012          0.0020979  0.0019332    1.085
    ## median_income_county_scale:year2013          0.0001018  0.0023320    0.044
    ## median_income_county_scale:year2014          0.0017150  0.0023363    0.734
    ## median_income_county_scale:year2015         -0.0002117  0.0023080   -0.092
    ## median_income_county_scale:year2016         -0.0009567  0.0022428   -0.427
    ## median_income_county_scale:year2017         -0.0027517  0.0022558   -1.220

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm2)
```

    ## # A tibble: 59 x 6
    ##    effect group term                        estimate std.error statistic
    ##    <chr>  <chr> <chr>                          <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                -0.157     0.00280    -56.1  
    ##  2 fixed  <NA>  median_income_county_scale -0.000732  0.00326     -0.225
    ##  3 fixed  <NA>  raw_income_scale            0.0491    0.000720    68.2  
    ##  4 fixed  <NA>  education_scale             0.0578    0.000553   105.   
    ##  5 fixed  <NA>  employment_all1            -0.00858   0.000571   -15.0  
    ##  6 fixed  <NA>  sex1                        0.687     0.000493  1395.   
    ##  7 fixed  <NA>  age_scale                  -0.101     0.000646  -157.   
    ##  8 fixed  <NA>  race1                       0.194     0.00127    153.   
    ##  9 fixed  <NA>  race2                       0.0937    0.00271     34.6  
    ## 10 fixed  <NA>  race3                       0.191     0.00178    107.   
    ## # ... with 49 more rows

``` r
glance(lm2)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.678 -2103163. 4206443. 4207182. 4206325.     2038648

#### Model comparison

``` r
anova(lm0, lm1 , lm2)
```

    ## Data: dfg_current
    ## Models:
    ## lm0: height_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm0:     land_area_2010_scale + physicians_scale + education_scale + 
    ## lm0:     employment_all + sex + age_scale + race + married + year + 
    ## lm0:     (1 + raw_income_scale | fips_code)
    ## lm1: height_scale ~ raw_income_scale + median_income_county_scale + 
    ## lm1:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm1:     land_area_2010_scale + physicians_scale + education_scale + 
    ## lm1:     employment_all + sex + age_scale + race + married + year + 
    ## lm1:     (1 + raw_income_scale | fips_code) + (1 + median_income_county_scale | 
    ## lm1:     fips_code)
    ## lm2: height_scale ~ median_income_county_scale * raw_income_scale + 
    ## lm2:     median_income_county_scale * education_scale + median_income_county_scale * 
    ## lm2:     employment_all + median_income_county_scale * sex + median_income_county_scale * 
    ## lm2:     age_scale + median_income_county_scale * race + median_income_county_scale * 
    ## lm2:     married + median_income_county_scale * year + median_income_county_scale + 
    ## lm2:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm2:     land_area_2010_scale + physicians_scale + (1 + raw_income_scale | 
    ## lm2:     fips_code) + (1 + median_income_county_scale | fips_code)
    ##     npar     AIC     BIC   logLik deviance    Chisq Df Pr(>Chisq)    
    ## lm0   32 4206589 4206990 -2103263  4206525                           
    ## lm1   36 4206592 4207043 -2103260  4206520   4.7288  4     0.3163    
    ## lm2   59 4206443 4207182 -2103163  4206325 195.3441 23     <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Self-reported health

#### Data selection

``` r
dfg_current <-
  dfg_rs %>%
  filter_at(
    vars(
      sr_health_scale,
      median_income_county_scale,
      education_scale,
      raw_income_scale,
      median_home_value_county_scale,
      median_monthly_housing_cost_county_scale,
      physicians_scale,
      total_pop_county_scale,
      land_area_2010_scale,
      race,
      sex,
      age_scale,
      married,
      employment_all,
      year
    ),
    all_vars(!is.na(.))
  )
```

#### Baseline Model

``` r
  lm0 <-
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
      data = dfg_current
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm0)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: sr_health_scale ~ raw_income_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     physicians_scale + education_scale + employment_all + sex +  
    ##     age_scale + race + married + year + (1 + raw_income_scale |      fips_code)
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4706244  4706640 -2353090  4706180  1759276 
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
    ## Number of obs: 1759308, groups:  fips_code, 3129
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.1254552  0.0036509   34.363
    ## raw_income_scale                         -0.1913863  0.0015096 -126.778
    ## total_pop_county_scale                    0.0094031  0.0007967   11.802
    ## median_monthly_housing_cost_county_scale -0.0243071  0.0008472  -28.692
    ## land_area_2010_scale                     -0.0064180  0.0007373   -8.705
    ## physicians_scale                         -0.0139137  0.0007916  -17.576
    ## education_scale                          -0.1443121  0.0007986 -180.716
    ## employment_all1                           0.1345931  0.0008266  162.829
    ## sex1                                      0.0395490  0.0007195   54.967
    ## age_scale                                 0.0910075  0.0009367   97.158
    ## race1                                    -0.1209899  0.0017290  -69.978
    ## race2                                     0.0484769  0.0038993   12.432
    ## race3                                    -0.0174523  0.0024752   -7.051
    ## race4                                     0.0262629  0.0041767    6.288
    ## married1                                 -0.0606087  0.0019295  -31.412
    ## married2                                 -0.0770090  0.0015040  -51.204
    ## married3                                  0.1611770  0.0042891   37.578
    ## married4                                  0.0518399  0.0020617   25.144
    ## married5                                 -0.0989404  0.0023987  -41.248
    ## year2009                                  0.0116065  0.0036117    3.214
    ## year2010                                  0.0331329  0.0036130    9.170
    ## year2011                                  0.0223179  0.0035983    6.202
    ## year2012                                  0.0271125  0.0035764    7.581
    ## year2013                                  0.0629780  0.0039745   15.845
    ## year2014                                  0.0649189  0.0039870   16.283
    ## year2015                                  0.0709906  0.0039818   17.829
    ## year2016                                  0.0787414  0.0039600   19.884
    ## year2017                                  0.1153989  0.0085479   13.500

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm0)
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
glance(lm0)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.921 -2353090. 4706244. 4706640. 4706180.     1759276

#### Main effect model

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
      data = dfg_current
    )
```

    ## boundary (singular) fit: see ?isSingular

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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4703203  4703649 -2351566  4703131  1759272 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8035 -0.7758 -0.0363  0.7105  6.8777 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance Std.Dev. Corr 
    ##  fips_code   (Intercept)                0.000000 0.00000       
    ##              raw_income_scale           0.001178 0.03432   NaN 
    ##  fips_code.1 (Intercept)                0.002337 0.04834       
    ##              median_income_county_scale 0.001708 0.04133  -0.16
    ##  Residual                               0.846379 0.91999       
    ## Number of obs: 1759308, groups:  fips_code, 3129
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                               0.1142796  0.0045614   25.053
    ## raw_income_scale                         -0.1855496  0.0013378 -138.697
    ## median_income_county_scale               -0.0116820  0.0040170   -2.908
    ## total_pop_county_scale                    0.0223577  0.0055197    4.051
    ## median_monthly_housing_cost_county_scale -0.0367658  0.0040802   -9.011
    ## land_area_2010_scale                     -0.0056943  0.0015812   -3.601
    ## physicians_scale                         -0.0140811  0.0015257   -9.229
    ## education_scale                          -0.1419920  0.0008026 -176.926
    ## employment_all1                           0.1345100  0.0008273  162.583
    ## sex1                                      0.0398053  0.0007189   55.372
    ## age_scale                                 0.0923618  0.0009395   98.313
    ## race1                                    -0.1157732  0.0017697  -65.420
    ## race2                                     0.0492269  0.0039107   12.588
    ## race3                                    -0.0208692  0.0025445   -8.202
    ## race4                                     0.0224165  0.0042008    5.336
    ## married1                                 -0.0596601  0.0019299  -30.913
    ## married2                                 -0.0767068  0.0015062  -50.929
    ## married3                                  0.1592830  0.0042855   37.168
    ## married4                                  0.0515830  0.0020606   25.033
    ## married5                                 -0.1000834  0.0023975  -41.745
    ## year2009                                  0.0116897  0.0036078    3.240
    ## year2010                                  0.0330249  0.0036096    9.149
    ## year2011                                  0.0240377  0.0035986    6.680
    ## year2012                                  0.0290984  0.0035801    8.128
    ## year2013                                  0.0642522  0.0039887   16.108
    ## year2014                                  0.0658300  0.0040066   16.430
    ## year2015                                  0.0719148  0.0040221   17.880
    ## year2016                                  0.0807494  0.0040619   19.880
    ## year2017                                  0.1197548  0.0086509   13.843

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
    ##  1 fixed  <NA>  (Intercept)                          0.114    0.00456      25.1 
    ##  2 fixed  <NA>  raw_income_scale                    -0.186    0.00134    -139.  
    ##  3 fixed  <NA>  median_income_county_scale          -0.0117   0.00402      -2.91
    ##  4 fixed  <NA>  total_pop_county_scale               0.0224   0.00552       4.05
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0368   0.00408      -9.01
    ##  6 fixed  <NA>  land_area_2010_scale                -0.00569  0.00158      -3.60
    ##  7 fixed  <NA>  physicians_scale                    -0.0141   0.00153      -9.23
    ##  8 fixed  <NA>  education_scale                     -0.142    0.000803   -177.  
    ##  9 fixed  <NA>  employment_all1                      0.135    0.000827    163.  
    ## 10 fixed  <NA>  sex1                                 0.0398   0.000719     55.4 
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.920 -2351565. 4703203. 4703649. 4703131.     1759272

#### Interactive model

``` r
  lm2 <-
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
      data = dfg_current
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm2)
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4701433  4702163 -2350657  4701315  1759249 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8621 -0.7775 -0.0388  0.7104  6.8836 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)                0.0026734 0.05170       
    ##              raw_income_scale           0.0006113 0.02472  -0.60
    ##  fips_code.1 (Intercept)                0.0002275 0.01508       
    ##              median_income_county_scale 0.0005017 0.02240  -1.00
    ##  Residual                               0.8458042 0.91968       
    ## Number of obs: 1759308, groups:  fips_code, 3129
    ## 
    ## Fixed effects:
    ##                                               Estimate Std. Error  t value
    ## (Intercept)                                  0.1124378  0.0044902   25.041
    ## median_income_county_scale                  -0.0182915  0.0053803   -3.400
    ## raw_income_scale                            -0.1766748  0.0012074 -146.323
    ## education_scale                             -0.1407779  0.0008077 -174.303
    ## employment_all1                              0.1324069  0.0008294  159.639
    ## sex1                                         0.0398545  0.0007193   55.404
    ## age_scale                                    0.0931304  0.0009415   98.918
    ## race1                                       -0.1118419  0.0018438  -60.657
    ## race2                                        0.0524597  0.0039599   13.248
    ## race3                                       -0.0161166  0.0026078   -6.180
    ## race4                                        0.0071160  0.0046361    1.535
    ## married1                                    -0.0574299  0.0019374  -29.642
    ## married2                                    -0.0755420  0.0015138  -49.902
    ## married3                                     0.1546981  0.0043359   35.678
    ## married4                                     0.0495580  0.0020770   23.860
    ## married5                                    -0.0982471  0.0024303  -40.425
    ## year2009                                     0.0129131  0.0036264    3.561
    ## year2010                                     0.0336047  0.0036303    9.257
    ## year2011                                     0.0255460  0.0036120    7.073
    ## year2012                                     0.0303968  0.0035916    8.463
    ## year2013                                     0.0659425  0.0039979   16.494
    ## year2014                                     0.0673525  0.0040142   16.779
    ## year2015                                     0.0735612  0.0040270   18.267
    ## year2016                                     0.0810671  0.0040618   19.958
    ## year2017                                     0.1161959  0.0089617   12.966
    ## total_pop_county_scale                       0.0260461  0.0050413    5.167
    ## median_monthly_housing_cost_county_scale    -0.0305398  0.0039728   -7.687
    ## land_area_2010_scale                        -0.0059088  0.0015304   -3.861
    ## physicians_scale                            -0.0121063  0.0014766   -8.199
    ## median_income_county_scale:raw_income_scale  0.0183036  0.0011997   15.257
    ## median_income_county_scale:education_scale   0.0048287  0.0008498    5.682
    ## median_income_county_scale:employment_all1  -0.0229641  0.0008577  -26.775
    ## median_income_county_scale:sex1              0.0049933  0.0007379    6.767
    ## median_income_county_scale:age_scale         0.0060003  0.0009762    6.147
    ## median_income_county_scale:race1            -0.0151136  0.0017449   -8.662
    ## median_income_county_scale:race2            -0.0137413  0.0040635   -3.382
    ## median_income_county_scale:race3            -0.0000082  0.0025734   -0.003
    ## median_income_county_scale:race4             0.0182072  0.0038298    4.754
    ## median_income_county_scale:married1          0.0208355  0.0020139   10.346
    ## median_income_county_scale:married2          0.0025184  0.0015588    1.616
    ## median_income_county_scale:married3         -0.0211477  0.0044368   -4.766
    ## median_income_county_scale:married4         -0.0189219  0.0021963   -8.616
    ## median_income_county_scale:married5          0.0130370  0.0025606    5.091
    ## median_income_county_scale:year2009          0.0124812  0.0038225    3.265
    ## median_income_county_scale:year2010          0.0058753  0.0038229    1.537
    ## median_income_county_scale:year2011          0.0149649  0.0037931    3.945
    ## median_income_county_scale:year2012          0.0077149  0.0037620    2.051
    ## median_income_county_scale:year2013          0.0117231  0.0041530    2.823
    ## median_income_county_scale:year2014          0.0095976  0.0041574    2.309
    ## median_income_county_scale:year2015          0.0119583  0.0041357    2.891
    ## median_income_county_scale:year2016          0.0157371  0.0040807    3.856
    ## median_income_county_scale:year2017          0.0199069  0.0080317    2.479

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm2)
```

    ## # A tibble: 59 x 6
    ##    effect group term                       estimate std.error statistic
    ##    <chr>  <chr> <chr>                         <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                  0.112   0.00449      25.0 
    ##  2 fixed  <NA>  median_income_county_scale  -0.0183  0.00538      -3.40
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
glance(lm2)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.920 -2350657. 4701433. 4702163. 4701315.     1759249

#### Model comparison

``` r
anova(lm0, lm1 , lm2)
```

    ## Data: dfg_current
    ## Models:
    ## lm0: sr_health_scale ~ raw_income_scale + total_pop_county_scale + 
    ## lm0:     median_monthly_housing_cost_county_scale + land_area_2010_scale + 
    ## lm0:     physicians_scale + education_scale + employment_all + sex + 
    ## lm0:     age_scale + race + married + year + (1 + raw_income_scale | 
    ## lm0:     fips_code)
    ## lm1: sr_health_scale ~ raw_income_scale + median_income_county_scale + 
    ## lm1:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm1:     land_area_2010_scale + physicians_scale + education_scale + 
    ## lm1:     employment_all + sex + age_scale + race + married + year + 
    ## lm1:     (1 + raw_income_scale | fips_code) + (1 + median_income_county_scale | 
    ## lm1:     fips_code)
    ## lm2: sr_health_scale ~ median_income_county_scale * raw_income_scale + 
    ## lm2:     median_income_county_scale * education_scale + median_income_county_scale * 
    ## lm2:     employment_all + median_income_county_scale * sex + median_income_county_scale * 
    ## lm2:     age_scale + median_income_county_scale * race + median_income_county_scale * 
    ## lm2:     married + median_income_county_scale * year + median_income_county_scale + 
    ## lm2:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm2:     land_area_2010_scale + physicians_scale + (1 + raw_income_scale | 
    ## lm2:     fips_code) + (1 + median_income_county_scale | fips_code)
    ##     npar     AIC     BIC   logLik deviance  Chisq Df Pr(>Chisq)    
    ## lm0   32 4706244 4706640 -2353090  4706180                         
    ## lm1   36 4703203 4703649 -2351565  4703131 3048.9  4  < 2.2e-16 ***
    ## lm2   59 4701433 4702163 -2350657  4701315 1816.2 23  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Life satisfaction

#### Data selection

``` r
dfg_current <-
  dfg_rs %>%
  filter_at(
    vars(
      ladder_now_scale,
      median_income_county_scale,
      education_scale,
      raw_income_scale,
      median_home_value_county_scale,
      median_monthly_housing_cost_county_scale,
      physicians_scale,
      total_pop_county_scale,
      land_area_2010_scale,
      race,
      sex,
      age_scale,
      married,
      employment_all,
      year
    ),
    all_vars(!is.na(.))
  )
```

#### Baseline Model

``` r
  lm0 <-
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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm0)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: ladder_now_scale ~ raw_income_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     physicians_scale + education_scale + employment_all + sex +  
    ##     age_scale + race + married + year + (1 + raw_income_scale |      fips_code)
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5444235  5444635 -2722085  5444171  2001163 
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
    ## Number of obs: 2001195, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.2300666  0.0033128  -69.448
    ## raw_income_scale                          0.2278595  0.0013021  174.990
    ## total_pop_county_scale                   -0.0281197  0.0041256   -6.816
    ## median_monthly_housing_cost_county_scale -0.0263255  0.0015983  -16.470
    ## land_area_2010_scale                      0.0082034  0.0013810    5.940
    ## physicians_scale                          0.0090880  0.0012899    7.046
    ## education_scale                           0.0561003  0.0007704   72.821
    ## employment_all1                          -0.0129977  0.0007986  -16.276
    ## sex1                                     -0.0740331  0.0006901 -107.277
    ## age_scale                                 0.0928040  0.0009028  102.801
    ## race1                                    -0.0628991  0.0016922  -37.170
    ## race2                                    -0.0744846  0.0037565  -19.828
    ## race3                                     0.0345537  0.0024379   14.174
    ## race4                                    -0.0331176  0.0040230   -8.232
    ## married1                                  0.0673777  0.0018537   36.347
    ## married2                                  0.1246339  0.0014484   86.050
    ## married3                                 -0.2295594  0.0041489  -55.330
    ## married4                                 -0.1107596  0.0019829  -55.856
    ## married5                                  0.1041574  0.0023205   44.885
    ## year2009                                  0.1725563  0.0027221   63.391
    ## year2010                                  0.1811603  0.0026158   69.255
    ## year2011                                  0.1888220  0.0025961   72.734
    ## year2012                                  0.1657153  0.0025654   64.597
    ## year2013                                  0.1879988  0.0031298   60.067
    ## year2014                                  0.2005644  0.0031453   63.766
    ## year2015                                  0.2055110  0.0031357   65.540
    ## year2016                                  0.1934095  0.0031070   62.250
    ## year2017                                  0.2062006  0.0032068   64.301

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm0)
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
glance(lm0)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.942 -2722085. 5444234. 5444635. 5444170.     2001163

#### Main effect model

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
      data = dfg_current
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5445003  5445453 -2722466  5444931  2001159 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2103 -0.6172  0.1017  0.6489  2.8392 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance Std.Dev. Corr
    ##  fips_code   (Intercept)                0.000000 0.00000      
    ##              raw_income_scale           0.001173 0.03426   NaN
    ##  fips_code.1 (Intercept)                0.000000 0.00000      
    ##              median_income_county_scale 0.003062 0.05534   NaN
    ##  Residual                               0.888185 0.94244      
    ## Number of obs: 2001195, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error  t value
    ## (Intercept)                              -0.2200537  0.0027863  -78.978
    ## raw_income_scale                          0.2279799  0.0013026  175.018
    ## median_income_county_scale                0.0267289  0.0030472    8.772
    ## total_pop_county_scale                   -0.0019868  0.0009968   -1.993
    ## median_monthly_housing_cost_county_scale -0.0493752  0.0026616  -18.551
    ## land_area_2010_scale                      0.0030801  0.0008481    3.632
    ## physicians_scale                          0.0114130  0.0010582   10.785
    ## education_scale                           0.0566152  0.0007697   73.559
    ## employment_all1                          -0.0130306  0.0007986  -16.316
    ## sex1                                     -0.0740481  0.0006902 -107.279
    ## age_scale                                 0.0926884  0.0009021  102.745
    ## race1                                    -0.0640554  0.0016882  -37.944
    ## race2                                    -0.0744688  0.0037562  -19.825
    ## race3                                     0.0358711  0.0024271   14.779
    ## race4                                    -0.0341072  0.0040237   -8.477
    ## married1                                  0.0673732  0.0018533   36.353
    ## married2                                  0.1247784  0.0014483   86.154
    ## married3                                 -0.2297416  0.0041492  -55.371
    ## married4                                 -0.1109055  0.0019829  -55.930
    ## married5                                  0.1041861  0.0023207   44.894
    ## year2009                                  0.1725821  0.0027227   63.386
    ## year2010                                  0.1814281  0.0026163   69.344
    ## year2011                                  0.1888418  0.0025964   72.732
    ## year2012                                  0.1660746  0.0025660   64.720
    ## year2013                                  0.1882687  0.0031277   60.194
    ## year2014                                  0.1995729  0.0031495   63.366
    ## year2015                                  0.2029595  0.0031514   64.403
    ## year2016                                  0.1881891  0.0031552   59.645
    ## year2017                                  0.1977053  0.0033224   59.507

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
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -0.0494   0.00266     -18.6 
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
    ## 1 0.942 -2722465. 5445003. 5445453. 5444931.     2001159

#### Interactive model

``` r
  lm2 <-
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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.119454 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm2)
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5443211  5443949 -2721547  5443093  2001136 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2386 -0.6162  0.1028  0.6491  2.9020 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)                0.0012054 0.03472       
    ##              raw_income_scale           0.0010368 0.03220  -0.83
    ##  fips_code.1 (Intercept)                0.0008344 0.02889       
    ##              median_income_county_scale 0.0001226 0.01107  0.14 
    ##  Residual                               0.8874150 0.94203       
    ## Number of obs: 2001195, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                               Estimate Std. Error  t value
    ## (Intercept)                                 -0.2212923  0.0033652  -65.758
    ## median_income_county_scale                   0.0282126  0.0041858    6.740
    ## raw_income_scale                             0.2242566  0.0013057  171.751
    ## education_scale                              0.0559554  0.0007740   72.296
    ## employment_all1                             -0.0120574  0.0008000  -15.072
    ## sex1                                        -0.0737346  0.0006903 -106.822
    ## age_scale                                    0.0921936  0.0009039  101.993
    ## race1                                       -0.0645375  0.0017717  -36.426
    ## race2                                       -0.0752860  0.0038031  -19.796
    ## race3                                        0.0284585  0.0024991   11.387
    ## race4                                       -0.0239641  0.0044904   -5.337
    ## married1                                     0.0665168  0.0018588   35.784
    ## married2                                     0.1244742  0.0014540   85.607
    ## married3                                    -0.2297763  0.0041825  -54.937
    ## married4                                    -0.1090377  0.0019942  -54.676
    ## married5                                     0.1035874  0.0023462   44.151
    ## year2009                                     0.1700337  0.0027391   62.076
    ## year2010                                     0.1779566  0.0026350   67.537
    ## year2011                                     0.1859406  0.0026062   71.344
    ## year2012                                     0.1634614  0.0025744   63.495
    ## year2013                                     0.1856489  0.0031383   59.155
    ## year2014                                     0.1974503  0.0031589   62.507
    ## year2015                                     0.2013104  0.0031650   63.606
    ## year2016                                     0.1870978  0.0031909   58.635
    ## year2017                                     0.1998633  0.0034072   58.658
    ## total_pop_county_scale                      -0.0233465  0.0040716   -5.734
    ## median_monthly_housing_cost_county_scale    -0.0433194  0.0034287  -12.634
    ## land_area_2010_scale                         0.0078290  0.0013595    5.759
    ## physicians_scale                             0.0090321  0.0012769    7.073
    ## median_income_county_scale:raw_income_scale -0.0086487  0.0012757   -6.780
    ## median_income_county_scale:education_scale  -0.0002600  0.0008068   -0.322
    ## median_income_county_scale:employment_all1   0.0138391  0.0008207   16.862
    ## median_income_county_scale:sex1              0.0066261  0.0007015    9.445
    ## median_income_county_scale:age_scale        -0.0084136  0.0009284   -9.062
    ## median_income_county_scale:race1             0.0200554  0.0016487   12.165
    ## median_income_county_scale:race2             0.0155107  0.0038725    4.005
    ## median_income_county_scale:race3            -0.0305824  0.0024340  -12.565
    ## median_income_county_scale:race4            -0.0056074  0.0036078   -1.554
    ## median_income_county_scale:married1         -0.0115953  0.0019108   -6.068
    ## median_income_county_scale:married2         -0.0014128  0.0014831   -0.953
    ## median_income_county_scale:married3         -0.0068972  0.0042435   -1.625
    ## median_income_county_scale:married4          0.0101813  0.0020948    4.860
    ## median_income_county_scale:married5         -0.0007994  0.0024542   -0.326
    ## median_income_county_scale:year2009         -0.0245728  0.0028972   -8.481
    ## median_income_county_scale:year2010         -0.0275222  0.0027812   -9.896
    ## median_income_county_scale:year2011         -0.0268417  0.0027361   -9.810
    ## median_income_county_scale:year2012         -0.0172749  0.0026877   -6.427
    ## median_income_county_scale:year2013         -0.0102571  0.0032388   -3.167
    ## median_income_county_scale:year2014         -0.0033204  0.0032401   -1.025
    ## median_income_county_scale:year2015         -0.0153225  0.0032029   -4.784
    ## median_income_county_scale:year2016         -0.0123453  0.0031133   -3.965
    ## median_income_county_scale:year2017         -0.0187078  0.0031302   -5.977

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.119454 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm2)
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
glance(lm2)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.942 -2721547. 5443211. 5443949. 5443093.     2001136

#### Model comparison

``` r
anova(lm0, lm1 , lm2)
```

    ## Data: dfg_current
    ## Models:
    ## lm0: ladder_now_scale ~ raw_income_scale + total_pop_county_scale + 
    ## lm0:     median_monthly_housing_cost_county_scale + land_area_2010_scale + 
    ## lm0:     physicians_scale + education_scale + employment_all + sex + 
    ## lm0:     age_scale + race + married + year + (1 + raw_income_scale | 
    ## lm0:     fips_code)
    ## lm1: ladder_now_scale ~ raw_income_scale + median_income_county_scale + 
    ## lm1:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm1:     land_area_2010_scale + physicians_scale + education_scale + 
    ## lm1:     employment_all + sex + age_scale + race + married + year + 
    ## lm1:     (1 + raw_income_scale | fips_code) + (1 + median_income_county_scale | 
    ## lm1:     fips_code)
    ## lm2: ladder_now_scale ~ median_income_county_scale * raw_income_scale + 
    ## lm2:     median_income_county_scale * education_scale + median_income_county_scale * 
    ## lm2:     employment_all + median_income_county_scale * sex + median_income_county_scale * 
    ## lm2:     age_scale + median_income_county_scale * race + median_income_county_scale * 
    ## lm2:     married + median_income_county_scale * year + median_income_county_scale + 
    ## lm2:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm2:     land_area_2010_scale + physicians_scale + (1 + raw_income_scale | 
    ## lm2:     fips_code) + (1 + median_income_county_scale | fips_code)
    ##     npar     AIC     BIC   logLik deviance  Chisq Df Pr(>Chisq)    
    ## lm0   32 5444234 5444635 -2722085  5444170                         
    ## lm1   36 5445003 5445453 -2722465  5444931    0.0  4          1    
    ## lm2   59 5443211 5443949 -2721547  5443093 1837.9 23     <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### BMI

#### Data selection

``` r
dfg_current <-
  dfg_rs %>%
  filter_at(
    vars(
      bmi_scale,
      median_income_county_scale,
      education_scale,
      raw_income_scale,
      median_home_value_county_scale,
      median_monthly_housing_cost_county_scale,
      physicians_scale,
      total_pop_county_scale,
      land_area_2010_scale,
      race,
      sex,
      age_scale,
      married,
      employment_all,
      year
    ),
    all_vars(!is.na(.))
  )
```

#### Baseline Model

``` r
  lm0 <-
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
      data = dfg_current
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(lm0)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## bmi_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + physicians_scale + education_scale +  
    ##     employment_all + sex + age_scale + race + married + year +  
    ##     (1 + raw_income_scale | fips_code)
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5645603  5646004 -2822770  5645539  2006477 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7817 -0.6609 -0.1629  0.4716 21.9438 
    ## 
    ## Random effects:
    ##  Groups    Name             Variance  Std.Dev. Corr 
    ##  fips_code (Intercept)      0.0059794 0.07733       
    ##            raw_income_scale 0.0004899 0.02213  -0.32
    ##  Residual                   0.9741268 0.98698       
    ## Number of obs: 2006509, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error t value
    ## (Intercept)                               0.0037680  0.0046029   0.819
    ## raw_income_scale                         -0.0551056  0.0011488 -47.969
    ## total_pop_county_scale                   -0.0321338  0.0075331  -4.266
    ## median_monthly_housing_cost_county_scale -0.0515836  0.0023745 -21.724
    ## land_area_2010_scale                     -0.0049601  0.0017586  -2.821
    ## physicians_scale                         -0.0157980  0.0017072  -9.254
    ## education_scale                          -0.0525909  0.0008064 -65.214
    ## employment_all1                           0.0002443  0.0008368   0.292
    ## sex1                                      0.0975391  0.0007229 134.928
    ## age_scale                                 0.0445919  0.0009444  47.217
    ## race1                                    -0.0348555  0.0017758 -19.628
    ## race2                                     0.0651641  0.0039166  16.638
    ## race3                                     0.2740035  0.0025550 107.241
    ## race4                                    -0.3634962  0.0042284 -85.965
    ## married1                                 -0.0675878  0.0019431 -34.784
    ## married2                                  0.0615639  0.0015196  40.512
    ## married3                                  0.0843658  0.0043452  19.416
    ## married4                                  0.0525966  0.0020685  25.427
    ## married5                                 -0.1098238  0.0024185 -45.410
    ## year2009                                  0.0120269  0.0027522   4.370
    ## year2010                                  0.0161954  0.0027572   5.874
    ## year2011                                  0.0005604  0.0027406   0.204
    ## year2012                                  0.0117991  0.0027091   4.355
    ## year2013                                  0.0245624  0.0033144   7.411
    ## year2014                                  0.0445963  0.0033303  13.391
    ## year2015                                  0.0661901  0.0033178  19.950
    ## year2016                                  0.0666264  0.0032896  20.254
    ## year2017                                  0.0710742  0.0034040  20.880

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
tidy(lm0)
```

    ## # A tibble: 32 x 6
    ##    effect group term                                estimate std.error statistic
    ##    <chr>  <chr> <chr>                                  <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                          3.77e-3  0.00460      0.819
    ##  2 fixed  <NA>  raw_income_scale                    -5.51e-2  0.00115    -48.0  
    ##  3 fixed  <NA>  total_pop_county_scale              -3.21e-2  0.00753     -4.27 
    ##  4 fixed  <NA>  median_monthly_housing_cost_county~ -5.16e-2  0.00237    -21.7  
    ##  5 fixed  <NA>  land_area_2010_scale                -4.96e-3  0.00176     -2.82 
    ##  6 fixed  <NA>  physicians_scale                    -1.58e-2  0.00171     -9.25 
    ##  7 fixed  <NA>  education_scale                     -5.26e-2  0.000806   -65.2  
    ##  8 fixed  <NA>  employment_all1                      2.44e-4  0.000837     0.292
    ##  9 fixed  <NA>  sex1                                 9.75e-2  0.000723   135.   
    ## 10 fixed  <NA>  age_scale                            4.46e-2  0.000944    47.2  
    ## # ... with 22 more rows

``` r
glance(lm0)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.987 -2822770. 5645603. 5646004. 5645539.     2006477

#### Main effect model

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
      data = dfg_current
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5645560  5646010 -2822744  5645488  2006473 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7859 -0.6609 -0.1629  0.4715 21.9418 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)                0.0051555 0.07180       
    ##              raw_income_scale           0.0004869 0.02207  -0.37
    ##  fips_code.1 (Intercept)                0.0000000 0.00000       
    ##              median_income_county_scale 0.0005507 0.02347   NaN 
    ##  Residual                               0.9741387 0.98698       
    ## Number of obs: 2006509, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error t value
    ## (Intercept)                               0.0065825  0.0044729   1.472
    ## raw_income_scale                         -0.0552300  0.0011472 -48.142
    ## median_income_county_scale                0.0282963  0.0042606   6.641
    ## total_pop_county_scale                   -0.0201174  0.0071963  -2.796
    ## median_monthly_housing_cost_county_scale -0.0781220  0.0045186 -17.289
    ## land_area_2010_scale                     -0.0055449  0.0017363  -3.193
    ## physicians_scale                         -0.0170828  0.0016915 -10.099
    ## education_scale                          -0.0525729  0.0008064 -65.191
    ## employment_all1                           0.0003096  0.0008368   0.370
    ## sex1                                      0.0975517  0.0007229 134.945
    ## age_scale                                 0.0445633  0.0009444  47.189
    ## race1                                    -0.0350920  0.0017764 -19.755
    ## race2                                     0.0650487  0.0039168  16.607
    ## race3                                     0.2744190  0.0025562 107.355
    ## race4                                    -0.3636042  0.0042288 -85.983
    ## married1                                 -0.0675974  0.0019431 -34.789
    ## married2                                  0.0615100  0.0015197  40.476
    ## married3                                  0.0844083  0.0043453  19.425
    ## married4                                  0.0526010  0.0020685  25.429
    ## married5                                 -0.1098782  0.0024185 -45.432
    ## year2009                                  0.0120190  0.0027522   4.367
    ## year2010                                  0.0161765  0.0027573   5.867
    ## year2011                                  0.0005148  0.0027412   0.188
    ## year2012                                  0.0116539  0.0027101   4.300
    ## year2013                                  0.0236575  0.0033164   7.133
    ## year2014                                  0.0426286  0.0033426  12.753
    ## year2015                                  0.0627423  0.0033571  18.689
    ## year2016                                  0.0610292  0.0034022  17.938
    ## year2017                                  0.0630758  0.0036412  17.323

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
    ##  1 fixed  <NA>  (Intercept)                          6.58e-3  0.00447      1.47 
    ##  2 fixed  <NA>  raw_income_scale                    -5.52e-2  0.00115    -48.1  
    ##  3 fixed  <NA>  median_income_county_scale           2.83e-2  0.00426      6.64 
    ##  4 fixed  <NA>  total_pop_county_scale              -2.01e-2  0.00720     -2.80 
    ##  5 fixed  <NA>  median_monthly_housing_cost_county~ -7.81e-2  0.00452    -17.3  
    ##  6 fixed  <NA>  land_area_2010_scale                -5.54e-3  0.00174     -3.19 
    ##  7 fixed  <NA>  physicians_scale                    -1.71e-2  0.00169    -10.1  
    ##  8 fixed  <NA>  education_scale                     -5.26e-2  0.000806   -65.2  
    ##  9 fixed  <NA>  employment_all1                      3.10e-4  0.000837     0.370
    ## 10 fixed  <NA>  sex1                                 9.76e-2  0.000723   135.   
    ## # ... with 26 more rows

``` r
glance(lm1)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.987 -2822744. 5645560. 5646010. 5645488.     2006473

#### Interactive model

``` r
  lm2 <-
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
      data = dfg_current
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lm2)
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
    ##    Data: dfg_current
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  5642716  5643454 -2821299  5642598  2006450 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7710 -0.6601 -0.1628  0.4715 21.9564 
    ## 
    ## Random effects:
    ##  Groups      Name                       Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)                0.0050948 0.07138       
    ##              raw_income_scale           0.0004267 0.02066  -0.39
    ##  fips_code.1 (Intercept)                0.0000000 0.00000       
    ##              median_income_county_scale 0.0005056 0.02249   NaN 
    ##  Residual                               0.9727797 0.98630       
    ## Number of obs: 2006509, groups:  fips_code, 3130
    ## 
    ## Fixed effects:
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                  0.0045355  0.0044941   1.009
    ## median_income_county_scale                   0.0408263  0.0052021   7.848
    ## raw_income_scale                            -0.0532336  0.0011395 -46.717
    ## education_scale                             -0.0536398  0.0008103 -66.197
    ## employment_all1                             -0.0005567  0.0008381  -0.664
    ## sex1                                         0.0983147  0.0007228 136.016
    ## age_scale                                    0.0460912  0.0009455  48.750
    ## race1                                       -0.0346612  0.0018585 -18.650
    ## race2                                        0.0652062  0.0039665  16.439
    ## race3                                        0.2718720  0.0026197 103.778
    ## race4                                       -0.3627366  0.0047165 -76.907
    ## married1                                    -0.0657407  0.0019486 -33.738
    ## married2                                     0.0619716  0.0015256  40.621
    ## married3                                     0.0799245  0.0043863  18.221
    ## married4                                     0.0531244  0.0020811  25.527
    ## married5                                    -0.1079444  0.0024462 -44.128
    ## year2009                                     0.0123136  0.0027688   4.447
    ## year2010                                     0.0166419  0.0027772   5.992
    ## year2011                                     0.0005673  0.0027513   0.206
    ## year2012                                     0.0115706  0.0027187   4.256
    ## year2013                                     0.0232350  0.0033238   6.991
    ## year2014                                     0.0418397  0.0033500  12.489
    ## year2015                                     0.0616688  0.0033656  18.323
    ## year2016                                     0.0604389  0.0034217  17.663
    ## year2017                                     0.0627610  0.0036961  16.980
    ## total_pop_county_scale                      -0.0175341  0.0071147  -2.464
    ## median_monthly_housing_cost_county_scale    -0.0858583  0.0046023 -18.655
    ## land_area_2010_scale                        -0.0053559  0.0017285  -3.099
    ## physicians_scale                            -0.0176173  0.0016835 -10.465
    ## median_income_county_scale:raw_income_scale  0.0069385  0.0011357   6.109
    ## median_income_county_scale:education_scale  -0.0104351  0.0008479 -12.307
    ## median_income_county_scale:employment_all1  -0.0164039  0.0008612 -19.047
    ## median_income_county_scale:sex1              0.0266709  0.0007361  36.233
    ## median_income_county_scale:age_scale         0.0303111  0.0009727  31.161
    ## median_income_county_scale:race1            -0.0005528  0.0017348  -0.319
    ## median_income_county_scale:race2            -0.0022708  0.0040496  -0.561
    ## median_income_county_scale:race3            -0.0192422  0.0025591  -7.519
    ## median_income_county_scale:race4             0.0147657  0.0037961   3.890
    ## median_income_county_scale:married1          0.0122299  0.0020095   6.086
    ## median_income_county_scale:married2         -0.0116850  0.0015598  -7.491
    ## median_income_county_scale:married3         -0.0180410  0.0044605  -4.045
    ## median_income_county_scale:married4         -0.0021422  0.0021883  -0.979
    ## median_income_county_scale:married5          0.0024456  0.0025614   0.955
    ## median_income_county_scale:year2009          0.0020933  0.0029279   0.715
    ## median_income_county_scale:year2010          0.0011880  0.0029313   0.405
    ## median_income_county_scale:year2011          0.0011443  0.0028873   0.396
    ## median_income_county_scale:year2012         -0.0038120  0.0028371  -1.344
    ## median_income_county_scale:year2013         -0.0058690  0.0034224  -1.715
    ## median_income_county_scale:year2014         -0.0102024  0.0034339  -2.971
    ## median_income_county_scale:year2015         -0.0125280  0.0033959  -3.689
    ## median_income_county_scale:year2016         -0.0128133  0.0033143  -3.866
    ## median_income_county_scale:year2017         -0.0112675  0.0033590  -3.354

    ## 
    ## Correlation matrix not shown by default, as p = 52 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
tidy(lm2)
```

    ## # A tibble: 59 x 6
    ##    effect group term                        estimate std.error statistic
    ##    <chr>  <chr> <chr>                          <dbl>     <dbl>     <dbl>
    ##  1 fixed  <NA>  (Intercept)                 0.00454   0.00449      1.01 
    ##  2 fixed  <NA>  median_income_county_scale  0.0408    0.00520      7.85 
    ##  3 fixed  <NA>  raw_income_scale           -0.0532    0.00114    -46.7  
    ##  4 fixed  <NA>  education_scale            -0.0536    0.000810   -66.2  
    ##  5 fixed  <NA>  employment_all1            -0.000557  0.000838    -0.664
    ##  6 fixed  <NA>  sex1                        0.0983    0.000723   136.   
    ##  7 fixed  <NA>  age_scale                   0.0461    0.000945    48.8  
    ##  8 fixed  <NA>  race1                      -0.0347    0.00186    -18.7  
    ##  9 fixed  <NA>  race2                       0.0652    0.00397     16.4  
    ## 10 fixed  <NA>  race3                       0.272     0.00262    104.   
    ## # ... with 49 more rows

``` r
glance(lm2)
```

    ## # A tibble: 1 x 6
    ##   sigma    logLik      AIC      BIC deviance df.residual
    ##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
    ## 1 0.986 -2821299. 5642716. 5643454. 5642598.     2006450

#### Model comparison

``` r
anova(lm0, lm1 , lm2)
```

    ## Data: dfg_current
    ## Models:
    ## lm0: bmi_scale ~ raw_income_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm0:     land_area_2010_scale + physicians_scale + education_scale + 
    ## lm0:     employment_all + sex + age_scale + race + married + year + 
    ## lm0:     (1 + raw_income_scale | fips_code)
    ## lm1: bmi_scale ~ raw_income_scale + median_income_county_scale + total_pop_county_scale + 
    ## lm1:     median_monthly_housing_cost_county_scale + land_area_2010_scale + 
    ## lm1:     physicians_scale + education_scale + employment_all + sex + 
    ## lm1:     age_scale + race + married + year + (1 + raw_income_scale | 
    ## lm1:     fips_code) + (1 + median_income_county_scale | fips_code)
    ## lm2: bmi_scale ~ median_income_county_scale * raw_income_scale + median_income_county_scale * 
    ## lm2:     education_scale + median_income_county_scale * employment_all + 
    ## lm2:     median_income_county_scale * sex + median_income_county_scale * 
    ## lm2:     age_scale + median_income_county_scale * race + median_income_county_scale * 
    ## lm2:     married + median_income_county_scale * year + median_income_county_scale + 
    ## lm2:     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ## lm2:     land_area_2010_scale + physicians_scale + (1 + raw_income_scale | 
    ## lm2:     fips_code) + (1 + median_income_county_scale | fips_code)
    ##     npar     AIC     BIC   logLik deviance    Chisq Df Pr(>Chisq)    
    ## lm0   32 5645603 5646004 -2822770  5645539                           
    ## lm1   36 5645560 5646010 -2822744  5645488   51.191  4  2.036e-10 ***
    ## lm2   59 5642716 5643454 -2821299  5642598 2890.216 23  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
