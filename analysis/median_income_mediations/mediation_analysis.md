Gallup - Relative Income - Mediation Analysis
================
Daniel O’Leary
5/20/2021

  - [Setup](#setup)
      - [Load pacakges](#load-pacakges)
      - [Load data](#load-data)
      - [Select relevant data](#select-relevant-data)
      - [Functions](#functions)
  - [Analysis](#analysis)
      - [Mediation Analysis 1: Well-Being
        Composites](#mediation-analysis-1-well-being-composites)
          - [Mediator models](#mediator-models)
          - [Linear Outcome models](#linear-outcome-models)
              - [Fruit and vegetable
                consumption](#fruit-and-vegetable-consumption)
              - [Eat healthy all day
                yesterday](#eat-healthy-all-day-yesterday)
              - [Smoking](#smoking)
          - [Actual Outcome models](#actual-outcome-models)
              - [Fruit and vegetable
                consumption](#fruit-and-vegetable-consumption-1)
              - [Eat healthy all day
                yesterday](#eat-healthy-all-day-yesterday-1)
              - [Smoking](#smoking-1)
          - [Indirect effects](#indirect-effects)
              - [Fruit and vegetable
                consumption](#fruit-and-vegetable-consumption-2)
              - [Eat healthy all day
                yesterday](#eat-healthy-all-day-yesterday-2)
              - [Smoking](#smoking-2)
      - [Mediation Analysis 2: Well-Being Composites with Financial and
        Social
        Subsets](#mediation-analysis-2-well-being-composites-with-financial-and-social-subsets)
          - [Mediator models](#mediator-models-1)
          - [Linear Outcome models](#linear-outcome-models-1)
              - [Fruit and vegetable
                consumption](#fruit-and-vegetable-consumption-3)
              - [Eat healthy all day
                yesterday](#eat-healthy-all-day-yesterday-3)
              - [Smoking](#smoking-3)
          - [Actual Outcome models](#actual-outcome-models-1)
              - [Fruit and vegetable
                consumption](#fruit-and-vegetable-consumption-4)
              - [Eat healthy all day
                yesterday](#eat-healthy-all-day-yesterday-4)
              - [Smoking](#smoking-4)
          - [Indirect effects](#indirect-effects-1)
              - [Fruit and vegetable
                consumption](#fruit-and-vegetable-consumption-5)
              - [Eat healthy all day
                yesterday](#eat-healthy-all-day-yesterday-5)
              - [Smoking](#smoking-5)

# Setup

## Load pacakges

## Load data

## Select relevant data

``` r
data <-
  dfg_rs %>%
  filter_at(
    vars(
      eat_healthy,
      fruits_veggies_scale,
      smoke,
      median_income_demo_scale,
      education_scale,
      raw_income_scale,
      median_home_value_county_scale,
      median_monthly_housing_cost_county_scale,
      physicians_scale,
      PURPOSE_scale,
      COMMUNITY_scale,
      FINANCIAL_scale,
      SOCIAL_scale,
      PHYSICAL_scale,
      enough_money_scale,
      comp_satis_std_liv_scale,
      social_subset_scale,
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
  ) %>%
  dplyr::select(
    eat_healthy,
    fruits_veggies_scale,
    smoke,
    median_income_demo_scale,
    education_scale,
    raw_income_scale,
    median_home_value_county_scale,
    median_monthly_housing_cost_county_scale,
    physicians_scale,
    PURPOSE_scale,
    COMMUNITY_scale,
    FINANCIAL_scale,
    SOCIAL_scale,
    PHYSICAL_scale,
    enough_money_scale,
    comp_satis_std_liv_scale,
    social_subset_scale,
    total_pop_county_scale,
    land_area_2010_scale,
    age_scale,
    race,
    sex,
    married,
    employment_all,
    year,
    fips_code
  ) %>%
  #dummy_cols(., select_columns = c("sex", "race", "married", "employment_all")) %>%
  mutate(across(eat_healthy:land_area_2010_scale, as.numeric)) %>%
  mutate(across(sex:year, as.factor)) 
```

## Functions

``` r
extract_med_coef <- function(lm) {
  
  # save lm object to variable
  coefs <- summary(lm)
  
  # extract coefficients
  coefs <- coefs$coefficients
  
  # select coefficient for relative income
  ri_coef <- coefs[3, 1]
  
  # return value
  return(ri_coef)
}

extract_outcome_coefs_med1 <- function(lm) {
    
  # save lm object to variable
  coefs <- summary(lm)
  
  # extract coefficients
  coefs <- coefs$coefficients
  
  # select coefficients for mediators
  coefs <- data.frame(t(coefs[2:6, 1]))
  
  # return values
  return(coefs)
}

extract_outcome_coefs_med2 <- function(lm) {
    
  # save lm object to variable
  coefs <- summary(lm)
  
  # extract coefficients
  coefs <- coefs$coefficients
  
  # select coefficients for mediators
  coefs <- data.frame(t(coefs[2:7, 1]))
  
  # return values
  return(coefs)
}

calculate_indirect_effects_med1 <- function(lm, coef_pur, coef_fin, coef_com, coef_soc) {
  coefs_out <- extract_outcome_coefs_med1(lm)

  ie_pur <- coefs_out$PURPOSE_scale * coef_pur
  ie_fin <- coefs_out$FINANCIAL_scale * coef_fin
  ie_com <- coefs_out$COMMUNITY_scale * coef_com
  ie_soc <- coefs_out$SOCIAL_scale * coef_soc

  total_effect = ie_pur + ie_fin + ie_com + ie_soc + coefs_out$median_income_demo_scale

  res <-
    data.frame(
      "mediatior" = c("purpose", "financial", "community", "social"),
      "indirect_effects" = c(ie_pur, ie_fin, ie_com, ie_soc)
    )

  res <-
    res %>% 
    mutate(
      prop_mediated = indirect_effects / total_effect
    )
  
  return(res)
}

calculate_indirect_effects_med2 <- function(lm, coef_pur, coef_em, coef_cs, coef_com, coef_soc) {
  coefs_out <- extract_outcome_coefs_med2(lm)

  ie_pur <- coefs_out$PURPOSE_scale * coef_pur
  ie_em <- coefs_out$enough_money_scale * coef_em
  ie_cs <- coefs_out$comp_satis_std_liv_scale * coef_cs
  ie_com <- coefs_out$COMMUNITY_scale * coef_com
  ie_soc <- coefs_out$social_subset_scale * coef_soc

  total_effect = ie_pur + ie_em + ie_cs + ie_com + ie_soc + coefs_out$median_income_demo_scale

  res <-
    data.frame(
      "mediatior" = c("purpose", "enough_money", "comp_satis_living", "community", "social_subset"),
      "indirect_effects" = c(ie_pur, ie_em, ie_cs, ie_com, ie_soc)
    )

  res <-
    res %>% 
    mutate(
      prop_mediated = indirect_effects / total_effect
    )
  
  return(res)
}
```

# Analysis

## Mediation Analysis 1: Well-Being Composites

### Mediator models

``` r
lm_pur <-
  lmer(
    PURPOSE_scale ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## boundary (singular) fit: see ?isSingular

``` r
lm_fin <-
  lmer(
    FINANCIAL_scale ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0033421 (tol = 0.002, component 1)

``` r
lm_com <-
  lmer(
    COMMUNITY_scale ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0185736 (tol = 0.002, component 1)

``` r
lm_soc <-
  lmer(
    SOCIAL_scale ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00349173 (tol = 0.002, component 1)

### Linear Outcome models

#### Fruit and vegetable consumption

``` r
lm_fv <-
  lmer(
    fruits_veggies_scale ~
      PURPOSE_scale +
      FINANCIAL_scale + 
      COMMUNITY_scale +
      SOCIAL_scale +
      median_income_demo_scale +
      raw_income_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## boundary (singular) fit: see ?isSingular

#### Eat healthy all day yesterday

``` r
lm_eh <-
  lmer(
    eat_healthy ~
      PURPOSE_scale +
      FINANCIAL_scale + 
      COMMUNITY_scale +
      SOCIAL_scale +
      median_income_demo_scale +
      raw_income_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## boundary (singular) fit: see ?isSingular

#### Smoking

``` r
lm_sm <-
  lmer(
    smoke ~
      PURPOSE_scale +
      FINANCIAL_scale + 
      COMMUNITY_scale +
      SOCIAL_scale +
      median_income_demo_scale +
      raw_income_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0114542 (tol = 0.002, component 1)

### Actual Outcome models

#### Fruit and vegetable consumption

``` r
summary(lm_fv)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## fruits_veggies_scale ~ PURPOSE_scale + FINANCIAL_scale + COMMUNITY_scale +  
    ##     SOCIAL_scale + median_income_demo_scale + raw_income_scale +  
    ##     physicians_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + education_scale + employment_all +  
    ##     sex + age_scale + race + married + year + (1 + median_income_demo_scale |  
    ##     fips_code) + (1 + raw_income_scale | fips_code)
    ##    Data: data
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##       AIC       BIC    logLik  deviance  df.resid 
    ## 1503354.9 1503736.4 -751643.5 1503286.9    551040 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.57899 -0.74888  0.05951  0.90164  2.24639 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr
    ##  fips_code   (Intercept)              0.0000000 0.00000      
    ##              median_income_demo_scale 0.0003001 0.01732   NaN
    ##  fips_code.1 (Intercept)              0.0013165 0.03628      
    ##              raw_income_scale         0.0001744 0.01320  0.26
    ##  Residual                             0.8945643 0.94581      
    ## Number of obs: 551074, groups:  fips_code, 3028
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error t value
    ## (Intercept)                              -0.1978655  0.0050296 -39.340
    ## PURPOSE_scale                             0.1007981  0.0017922  56.243
    ## FINANCIAL_scale                          -0.0160057  0.0016342  -9.794
    ## COMMUNITY_scale                           0.0101011  0.0015962   6.328
    ## SOCIAL_scale                              0.1127093  0.0016745  67.308
    ## median_income_demo_scale                 -0.0010268  0.0027518  -0.373
    ## raw_income_scale                         -0.0084428  0.0017863  -4.726
    ## physicians_scale                          0.0032167  0.0020922   1.537
    ## total_pop_county_scale                   -0.0083832  0.0042731  -1.962
    ## median_monthly_housing_cost_county_scale  0.0096736  0.0022457   4.308
    ## land_area_2010_scale                      0.0051720  0.0020834   2.482
    ## education_scale                           0.0180789  0.0025386   7.122
    ## employment_all1                           0.0242976  0.0031229   7.780
    ## sex2                                      0.2160919  0.0029106  74.243
    ## age_scale                                 0.0986375  0.0017375  56.769
    ## race2                                     0.1754059  0.0103168  17.002
    ## race3                                     0.1382703  0.0046460  29.761
    ## race4                                     0.1548876  0.0085460  18.124
    ## race5                                     0.0735837  0.0047779  15.401
    ## married2                                  0.0648361  0.0039655  16.350
    ## married3                                  0.0755416  0.0097398   7.756
    ## married4                                  0.0257857  0.0051986   4.960
    ## married5                                  0.0483768  0.0061449   7.873
    ## married8                                  0.0694141  0.0062694  11.072
    ## year2015                                 -0.0092812  0.0036788  -2.523
    ## year2016                                 -0.0176775  0.0038084  -4.642
    ## year2017                                 -0.0003163  0.0038746  -0.082

    ## 
    ## Correlation matrix not shown by default, as p = 27 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

#### Eat healthy all day yesterday

``` r
lma_eh <-
  glmer(
    eat_healthy ~
      PURPOSE_scale +
      FINANCIAL_scale + 
      COMMUNITY_scale +
      SOCIAL_scale +
      median_income_demo_scale +
      raw_income_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    control = glmerControl(optimizer = "bobyqa"),
    family = "binomial",
    data = data 
  )
```

    ## Warning in commonArgs(par, fn, control, environment()): maxfun < 10 *
    ## length(par)^2 is not recommended.

    ## boundary (singular) fit: see ?isSingular

``` r
summary(lma_eh)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: eat_healthy ~ PURPOSE_scale + FINANCIAL_scale + COMMUNITY_scale +  
    ##     SOCIAL_scale + median_income_demo_scale + raw_income_scale +  
    ##     physicians_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + education_scale + employment_all +  
    ##     sex + age_scale + race + married + year + (1 + median_income_demo_scale |  
    ##     fips_code) + (1 + raw_income_scale | fips_code)
    ##    Data: data
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##       AIC       BIC    logLik  deviance  df.resid 
    ##  636868.2  637238.4 -318401.1  636802.2    551041 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.9294 -0.9458  0.4502  0.7053  2.9980 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev.  Corr
    ##  fips_code   (Intercept)              0.000e+00 0.0000000     
    ##              median_income_demo_scale 5.144e-04 0.0226811  NaN
    ##  fips_code.1 (Intercept)              8.127e-03 0.0901507     
    ##              raw_income_scale         2.446e-07 0.0004946 1.00
    ## Number of obs: 551074, groups:  fips_code, 3028
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                               1.024979   0.012116  84.597  < 2e-16
    ## PURPOSE_scale                             0.223694   0.004138  54.060  < 2e-16
    ## FINANCIAL_scale                           0.092362   0.003797  24.327  < 2e-16
    ## COMMUNITY_scale                           0.095935   0.003697  25.948  < 2e-16
    ## SOCIAL_scale                              0.244597   0.003874  63.132  < 2e-16
    ## median_income_demo_scale                 -0.030854   0.006505  -4.743 2.10e-06
    ## raw_income_scale                         -0.146100   0.003992 -36.596  < 2e-16
    ## physicians_scale                          0.025222   0.005092   4.954 7.28e-07
    ## total_pop_county_scale                   -0.009527   0.010652  -0.894 0.371110
    ## median_monthly_housing_cost_county_scale  0.078213   0.005409  14.459  < 2e-16
    ## land_area_2010_scale                      0.036384   0.005541   6.566 5.17e-11
    ## education_scale                           0.057039   0.006185   9.222  < 2e-16
    ## employment_all1                          -0.216028   0.007487 -28.853  < 2e-16
    ## sex2                                     -0.148996   0.006951 -21.436  < 2e-16
    ## age_scale                                 0.522122   0.004188 124.680  < 2e-16
    ## race2                                     0.194478   0.024440   7.957 1.76e-15
    ## race3                                    -0.031891   0.010805  -2.952 0.003162
    ## race4                                     0.423267   0.019972  21.193  < 2e-16
    ## race5                                     0.376513   0.011356  33.157  < 2e-16
    ## married2                                 -0.034327   0.009148  -3.752 0.000175
    ## married3                                  0.067829   0.022569   3.005 0.002652
    ## married4                                 -0.066257   0.012204  -5.429 5.66e-08
    ## married5                                 -0.084012   0.015702  -5.350 8.77e-08
    ## married8                                  0.010748   0.014321   0.751 0.452943
    ## year2015                                 -0.026367   0.008819  -2.990 0.002792
    ## year2016                                 -0.040352   0.009155  -4.407 1.05e-05
    ## year2017                                  0.003478   0.009268   0.375 0.707413
    ##                                             
    ## (Intercept)                              ***
    ## PURPOSE_scale                            ***
    ## FINANCIAL_scale                          ***
    ## COMMUNITY_scale                          ***
    ## SOCIAL_scale                             ***
    ## median_income_demo_scale                 ***
    ## raw_income_scale                         ***
    ## physicians_scale                         ***
    ## total_pop_county_scale                      
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     ***
    ## education_scale                          ***
    ## employment_all1                          ***
    ## sex2                                     ***
    ## age_scale                                ***
    ## race2                                    ***
    ## race3                                    ** 
    ## race4                                    ***
    ## race5                                    ***
    ## married2                                 ***
    ## married3                                 ** 
    ## married4                                 ***
    ## married5                                 ***
    ## married8                                    
    ## year2015                                 ** 
    ## year2016                                 ***
    ## year2017                                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 27 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular
    ## maxfun < 10 * length(par)^2 is not recommended.

#### Smoking

``` r
lma_sm <-
  glmer(
    smoke ~
      PURPOSE_scale +
      FINANCIAL_scale + 
      COMMUNITY_scale +
      SOCIAL_scale +
      median_income_demo_scale +
      raw_income_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    control = glmerControl(optimizer = "bobyqa"),
    family = "binomial",
    data = data 
  )
```

    ## Warning in commonArgs(par, fn, control, environment()): maxfun < 10 *
    ## length(par)^2 is not recommended.

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00277388 (tol = 0.002, component 1)

``` r
summary(lma_sm)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## smoke ~ PURPOSE_scale + FINANCIAL_scale + COMMUNITY_scale + SOCIAL_scale +  
    ##     median_income_demo_scale + raw_income_scale + physicians_scale +  
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale +  
    ##     land_area_2010_scale + education_scale + employment_all +  
    ##     sex + age_scale + race + married + year + (1 + median_income_demo_scale |  
    ##     fips_code) + (1 + raw_income_scale | fips_code)
    ##    Data: data
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##       AIC       BIC    logLik  deviance  df.resid 
    ##  425733.5  426103.7 -212833.7  425667.5    551041 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1112 -0.4454 -0.3115 -0.2114  9.8637 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0184968 0.13600       
    ##              median_income_demo_scale 0.0035068 0.05922  -0.12
    ##  fips_code.1 (Intercept)              0.0001582 0.01258       
    ##              raw_income_scale         0.0049030 0.07002  0.77 
    ## Number of obs: 551074, groups:  fips_code, 3028
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                              -1.556257   0.015672 -99.303  < 2e-16
    ## PURPOSE_scale                             0.011993   0.005177   2.317 0.020524
    ## FINANCIAL_scale                          -0.284508   0.004652 -61.155  < 2e-16
    ## COMMUNITY_scale                          -0.076634   0.004596 -16.675  < 2e-16
    ## SOCIAL_scale                             -0.062178   0.004881 -12.738  < 2e-16
    ## median_income_demo_scale                  0.136453   0.008425  16.196  < 2e-16
    ## raw_income_scale                         -0.164548   0.005920 -27.795  < 2e-16
    ## physicians_scale                         -0.039243   0.006993  -5.612 2.01e-08
    ## total_pop_county_scale                   -0.017137   0.016152  -1.061 0.288712
    ## median_monthly_housing_cost_county_scale -0.085488   0.007577 -11.283  < 2e-16
    ## land_area_2010_scale                     -0.015459   0.007163  -2.158 0.030922
    ## education_scale                          -0.555569   0.007862 -70.664  < 2e-16
    ## employment_all1                          -0.101713   0.009423 -10.794  < 2e-16
    ## sex2                                     -0.279049   0.009195 -30.348  < 2e-16
    ## age_scale                                -0.250437   0.005316 -47.110  < 2e-16
    ## race2                                     0.314703   0.026781  11.751  < 2e-16
    ## race3                                    -0.049379   0.013267  -3.722 0.000198
    ## race4                                    -0.326538   0.030607 -10.669  < 2e-16
    ## race5                                    -0.582979   0.014934 -39.036  < 2e-16
    ## married2                                 -0.263782   0.011734 -22.480  < 2e-16
    ## married3                                  0.428706   0.024209  17.709  < 2e-16
    ## married4                                  0.371835   0.014254  26.086  < 2e-16
    ## married5                                  0.004302   0.019001   0.226 0.820898
    ## married8                                  0.448731   0.016239  27.632  < 2e-16
    ## year2015                                 -0.084358   0.011211  -7.525 5.29e-14
    ## year2016                                 -0.103021   0.011621  -8.865  < 2e-16
    ## year2017                                 -0.113012   0.011906  -9.492  < 2e-16
    ##                                             
    ## (Intercept)                              ***
    ## PURPOSE_scale                            *  
    ## FINANCIAL_scale                          ***
    ## COMMUNITY_scale                          ***
    ## SOCIAL_scale                             ***
    ## median_income_demo_scale                 ***
    ## raw_income_scale                         ***
    ## physicians_scale                         ***
    ## total_pop_county_scale                      
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     *  
    ## education_scale                          ***
    ## employment_all1                          ***
    ## sex2                                     ***
    ## age_scale                                ***
    ## race2                                    ***
    ## race3                                    ***
    ## race4                                    ***
    ## race5                                    ***
    ## married2                                 ***
    ## married3                                 ***
    ## married4                                 ***
    ## married5                                    
    ## married8                                 ***
    ## year2015                                 ***
    ## year2016                                 ***
    ## year2017                                 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 27 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.00277388 (tol = 0.002, component 1)
    ## maxfun < 10 * length(par)^2 is not recommended.

### Indirect effects

``` r
coef_pur <- extract_med_coef(lm_pur)
coef_fin <- extract_med_coef(lm_fin)
coef_com <- extract_med_coef(lm_com)
coef_soc <- extract_med_coef(lm_soc)
```

#### Fruit and vegetable consumption

``` r
fv_ie <- calculate_indirect_effects_med1(lm_fv, coef_pur, coef_fin, coef_com, coef_soc)
fv_ie
```

    ##   mediatior indirect_effects prop_mediated
    ## 1   purpose     -0.016281069    0.49277055
    ## 2 financial      0.002407100   -0.07285443
    ## 3 community     -0.001063645    0.03219277
    ## 4    social     -0.017075442    0.51681342

#### Eat healthy all day yesterday

``` r
eh_ie <- calculate_indirect_effects_med1(lm_eh, coef_pur, coef_fin, coef_com, coef_soc)
eh_ie
```

    ##   mediatior indirect_effects prop_mediated
    ## 1   purpose     -0.007467538     0.4624365
    ## 2 financial     -0.002915744     0.1805610
    ## 3 community     -0.001990699     0.1232765
    ## 4    social     -0.007536226     0.4666900

#### Smoking

``` r
sm_ie <- calculate_indirect_effects_med1(lm_sm, coef_pur, coef_fin, coef_com, coef_soc)
sm_ie
```

    ##   mediatior indirect_effects prop_mediated
    ## 1   purpose    -0.0002240553   -0.01221120
    ## 2 financial     0.0060245196    0.32834119
    ## 3 community     0.0011089640    0.06043943
    ## 4    social     0.0011827712    0.06446199

## Mediation Analysis 2: Well-Being Composites with Financial and Social Subsets

### Mediator models

``` r
lm_em <-
  lmer(
    enough_money_scale ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## boundary (singular) fit: see ?isSingular

``` r
lm_cs <-
  lmer(
    comp_satis_std_liv_scale ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0107649 (tol = 0.002, component 1)

``` r
lm_soc <-
  lmer(
    social_subset_scale ~
      raw_income_scale +
      median_income_demo_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  ) 
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0222604 (tol = 0.002, component 1)

### Linear Outcome models

#### Fruit and vegetable consumption

``` r
lm_fv <-
  lmer(
    fruits_veggies_scale ~
      PURPOSE_scale +
      enough_money_scale + 
      comp_satis_std_liv_scale +
      COMMUNITY_scale +
      social_subset_scale +
      median_income_demo_scale +
      raw_income_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## boundary (singular) fit: see ?isSingular

#### Eat healthy all day yesterday

``` r
lm_eh <-
  lmer(
    eat_healthy ~
      PURPOSE_scale +
      enough_money_scale + 
      comp_satis_std_liv_scale +
      COMMUNITY_scale +
      social_subset_scale +
      median_income_demo_scale +
      raw_income_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## boundary (singular) fit: see ?isSingular

#### Smoking

``` r
lm_sm <-
  lmer(
    smoke ~
      PURPOSE_scale +
      enough_money_scale + 
      comp_satis_std_liv_scale +
      COMMUNITY_scale +
      social_subset_scale +
      median_income_demo_scale +
      raw_income_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = data 
  )
```

    ## boundary (singular) fit: see ?isSingular

### Actual Outcome models

#### Fruit and vegetable consumption

``` r
summary(lm_fv)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## fruits_veggies_scale ~ PURPOSE_scale + enough_money_scale + comp_satis_std_liv_scale +  
    ##     COMMUNITY_scale + social_subset_scale + median_income_demo_scale +  
    ##     raw_income_scale + physicians_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     education_scale + employment_all + sex + age_scale + race +  
    ##     married + year + (1 + median_income_demo_scale | fips_code) +  
    ##     (1 + raw_income_scale | fips_code)
    ##    Data: data
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##       AIC       BIC    logLik  deviance  df.resid 
    ## 1505723.3 1506116.0 -752826.6 1505653.3    551039 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.51122 -0.74817  0.06052  0.90734  2.23972 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr
    ##  fips_code   (Intercept)              0.0000000 0.00000      
    ##              median_income_demo_scale 0.0003982 0.01995   NaN
    ##  fips_code.1 (Intercept)              0.0000000 0.00000      
    ##              raw_income_scale         0.0002584 0.01607   NaN
    ##  Residual                             0.8991543 0.94824      
    ## Number of obs: 551074, groups:  fips_code, 3028
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error t value
    ## (Intercept)                              -0.186950   0.004702 -39.761
    ## PURPOSE_scale                             0.120068   0.001821  65.930
    ## enough_money_scale                       -0.011031   0.001669  -6.609
    ## comp_satis_std_liv_scale                 -0.008495   0.001807  -4.701
    ## COMMUNITY_scale                           0.015345   0.001624   9.449
    ## social_subset_scale                       0.082590   0.001728  47.788
    ## median_income_demo_scale                 -0.004438   0.002781  -1.596
    ## raw_income_scale                         -0.011363   0.001814  -6.263
    ## physicians_scale                          0.003070   0.001663   1.846
    ## total_pop_county_scale                   -0.000650   0.001472  -0.441
    ## median_monthly_housing_cost_county_scale  0.010614   0.001602   6.625
    ## land_area_2010_scale                      0.005928   0.001392   4.260
    ## education_scale                           0.022807   0.002546   8.960
    ## employment_all1                           0.004872   0.003122   1.561
    ## sex2                                      0.214559   0.002913  73.657
    ## age_scale                                 0.099892   0.001731  57.714
    ## race2                                     0.179467   0.010308  17.410
    ## race3                                     0.141153   0.004553  31.003
    ## race4                                     0.158125   0.008526  18.546
    ## race5                                     0.076396   0.004685  16.306
    ## married2                                  0.065728   0.003984  16.500
    ## married3                                  0.083491   0.009767   8.549
    ## married4                                  0.033462   0.005212   6.420
    ## married5                                  0.054900   0.006154   8.922
    ## married8                                  0.073294   0.006285  11.661
    ## year2015                                 -0.008758   0.003687  -2.375
    ## year2016                                 -0.016540   0.003817  -4.334
    ## year2017                                 -0.001335   0.003881  -0.344

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

#### Eat healthy all day yesterday

``` r
lma_eh <-
  glmer(
    eat_healthy ~
      PURPOSE_scale +
      enough_money_scale + 
      comp_satis_std_liv_scale +
      COMMUNITY_scale +
      social_subset_scale +
      median_income_demo_scale +
      raw_income_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    control = glmerControl(optimizer = "bobyqa"),
    family = "binomial",
    data = data 
  )
```

    ## Warning in commonArgs(par, fn, control, environment()): maxfun < 10 *
    ## length(par)^2 is not recommended.

    ## boundary (singular) fit: see ?isSingular

#### Smoking

``` r
lma_sm <-
  glmer(
    smoke ~
      PURPOSE_scale +
      enough_money_scale + 
      comp_satis_std_liv_scale +
      COMMUNITY_scale +
      social_subset_scale +
      median_income_demo_scale +
      raw_income_scale +
      physicians_scale +
      total_pop_county_scale +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      education_scale +
      employment_all +
      sex +
      age_scale +
      race +
      married + 
      year +
      (1 + median_income_demo_scale | fips_code) +
      (1 + raw_income_scale | fips_code),
    control = glmerControl(optimizer = "bobyqa"),
    family = "binomial",
    data = data 
  )
```

    ## Warning in commonArgs(par, fn, control, environment()): maxfun < 10 *
    ## length(par)^2 is not recommended.

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(lma_sm)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## smoke ~ PURPOSE_scale + enough_money_scale + comp_satis_std_liv_scale +  
    ##     COMMUNITY_scale + social_subset_scale + median_income_demo_scale +  
    ##     raw_income_scale + physicians_scale + total_pop_county_scale +  
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale +  
    ##     education_scale + employment_all + sex + age_scale + race +  
    ##     married + year + (1 + median_income_demo_scale | fips_code) +  
    ##     (1 + raw_income_scale | fips_code)
    ##    Data: data
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##       AIC       BIC    logLik  deviance  df.resid 
    ##  427323.4  427704.9 -213627.7  427255.4    551040 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9931 -0.4487 -0.3135 -0.2116  9.7938 
    ## 
    ## Random effects:
    ##  Groups      Name                     Variance  Std.Dev. Corr 
    ##  fips_code   (Intercept)              0.0184254 0.13574       
    ##              median_income_demo_scale 0.0034186 0.05847  -0.12
    ##  fips_code.1 (Intercept)              0.0001164 0.01079       
    ##              raw_income_scale         0.0052301 0.07232  0.82 
    ## Number of obs: 551074, groups:  fips_code, 3028
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error  z value Pr(>|z|)
    ## (Intercept)                              -1.613803   0.015713 -102.705  < 2e-16
    ## PURPOSE_scale                            -0.012022   0.005245   -2.292 0.021890
    ## enough_money_scale                       -0.212681   0.004896  -43.440  < 2e-16
    ## comp_satis_std_liv_scale                  0.003147   0.004957    0.635 0.525433
    ## COMMUNITY_scale                          -0.090645   0.004690  -19.326  < 2e-16
    ## social_subset_scale                      -0.088539   0.004944  -17.909  < 2e-16
    ## median_income_demo_scale                  0.141446   0.008416   16.807  < 2e-16
    ## raw_income_scale                         -0.194955   0.005915  -32.957  < 2e-16
    ## physicians_scale                         -0.038545   0.006976   -5.525 3.29e-08
    ## total_pop_county_scale                   -0.014275   0.016141   -0.884 0.376486
    ## median_monthly_housing_cost_county_scale -0.082061   0.007563  -10.850  < 2e-16
    ## land_area_2010_scale                     -0.015687   0.007123   -2.202 0.027640
    ## education_scale                          -0.543305   0.007847  -69.238  < 2e-16
    ## employment_all1                          -0.033336   0.009404   -3.545 0.000393
    ## sex2                                     -0.252871   0.009154  -27.625  < 2e-16
    ## age_scale                                -0.274702   0.005280  -52.031  < 2e-16
    ## race2                                     0.332178   0.026706   12.439  < 2e-16
    ## race3                                    -0.041424   0.013225   -3.132 0.001735
    ## race4                                    -0.344378   0.030604  -11.253  < 2e-16
    ## race5                                    -0.575715   0.014919  -38.590  < 2e-16
    ## married2                                 -0.241667   0.011777  -20.520  < 2e-16
    ## married3                                  0.449305   0.024147   18.607  < 2e-16
    ## married4                                  0.371832   0.014239   26.113  < 2e-16
    ## married5                                 -0.016427   0.018982   -0.865 0.386815
    ## married8                                  0.476306   0.016210   29.384  < 2e-16
    ## year2015                                 -0.086630   0.011189   -7.743 9.74e-15
    ## year2016                                 -0.105171   0.011598   -9.068  < 2e-16
    ## year2017                                 -0.119521   0.011885  -10.056  < 2e-16
    ##                                             
    ## (Intercept)                              ***
    ## PURPOSE_scale                            *  
    ## enough_money_scale                       ***
    ## comp_satis_std_liv_scale                    
    ## COMMUNITY_scale                          ***
    ## social_subset_scale                      ***
    ## median_income_demo_scale                 ***
    ## raw_income_scale                         ***
    ## physicians_scale                         ***
    ## total_pop_county_scale                      
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     *  
    ## education_scale                          ***
    ## employment_all1                          ***
    ## sex2                                     ***
    ## age_scale                                ***
    ## race2                                    ***
    ## race3                                    ** 
    ## race4                                    ***
    ## race5                                    ***
    ## married2                                 ***
    ## married3                                 ***
    ## married4                                 ***
    ## married5                                    
    ## married8                                 ***
    ## year2015                                 ***
    ## year2016                                 ***
    ## year2017                                 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 28 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## unable to evaluate scaled gradient
    ## Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
    ## maxfun < 10 * length(par)^2 is not recommended.

### Indirect effects

``` r
coef_pur <- extract_med_coef(lm_pur)
coef_em <- extract_med_coef(lm_em)
coef_cs <- extract_med_coef(lm_cs)
coef_com <- extract_med_coef(lm_com)
coef_soc <- extract_med_coef(lm_soc)
```

#### Fruit and vegetable consumption

``` r
fv_ie <- calculate_indirect_effects_med2(lm_fv, coef_pur, coef_em, coef_cs, coef_com, coef_soc)
fv_ie
```

    ##           mediatior indirect_effects prop_mediated
    ## 1           purpose     -0.019393615    0.58406727
    ## 2      enough_money      0.001687324   -0.05081625
    ## 3 comp_satis_living      0.001521019   -0.04580772
    ## 4         community     -0.001615803    0.04866229
    ## 5     social_subset     -0.010965324    0.33023686

#### Eat healthy all day yesterday

``` r
eh_ie <- calculate_indirect_effects_med2(lm_eh, coef_pur, coef_em, coef_cs, coef_com, coef_soc)
eh_ie
```

    ##           mediatior indirect_effects prop_mediated
    ## 1           purpose    -0.0086964702     0.5399220
    ## 2      enough_money    -0.0029139831     0.1809152
    ## 3 comp_satis_living    -0.0005651573     0.0350879
    ## 4         community    -0.0021585295     0.1340127
    ## 5     social_subset    -0.0046587786     0.2892411

#### Smoking

``` r
sm_ie <- calculate_indirect_effects_med2(lm_sm, coef_pur, coef_em, coef_cs, coef_com, coef_soc)
sm_ie
```

    ##           mediatior indirect_effects prop_mediated
    ## 1           purpose     0.0003008572    0.01647633
    ## 2      enough_money     0.0039727040    0.21756358
    ## 3 comp_satis_living     0.0001507799    0.00825740
    ## 4         community     0.0012860716    0.07043121
    ## 5     social_subset     0.0016541876    0.09059094
