Gallup - Relative Income - Mediation Analysis (Traditional Regression)
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
  lm(
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
      year,
    data = data 
  )

lm_fin <-
  lm(
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
      year,
    data = data 
  )

lm_com <-
  lm(
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
      year,
    data = data 
  )

lm_soc <-
  lm(
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
      year,
    data = data 
  )
```

### Linear Outcome models

#### Fruit and vegetable consumption

``` r
lm_fv <-
  lm(
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
      year,
    data = data 
  )
```

#### Eat healthy all day yesterday

``` r
lm_eh <-
  lm(
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
      year,
    data = data 
  )
```

#### Smoking

``` r
lm_sm <-
  lm(
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
      year,
    data = data 
  )
```

### Actual Outcome models

#### Fruit and vegetable consumption

``` r
summary(lm_fv)
```

    ## 
    ## Call:
    ## lm(formula = fruits_veggies_scale ~ PURPOSE_scale + FINANCIAL_scale + 
    ##     COMMUNITY_scale + SOCIAL_scale + median_income_demo_scale + 
    ##     raw_income_scale + physicians_scale + total_pop_county_scale + 
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale + 
    ##     education_scale + employment_all + sex + age_scale + race + 
    ##     married + year, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.42983 -0.70981  0.05653  0.85553  2.14255 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                              -0.1955601  0.0046790 -41.795  < 2e-16
    ## PURPOSE_scale                             0.1003502  0.0017916  56.012  < 2e-16
    ## FINANCIAL_scale                          -0.0161807  0.0016338  -9.904  < 2e-16
    ## COMMUNITY_scale                           0.0102790  0.0015882   6.472 9.68e-11
    ## SOCIAL_scale                              0.1129848  0.0016744  67.479  < 2e-16
    ## median_income_demo_scale                  0.0016775  0.0026552   0.632  0.52754
    ## raw_income_scale                         -0.0074648  0.0016804  -4.442 8.90e-06
    ## physicians_scale                          0.0029238  0.0016245   1.800  0.07190
    ## total_pop_county_scale                    0.0011627  0.0014252   0.816  0.41461
    ## median_monthly_housing_cost_county_scale  0.0091047  0.0015450   5.893 3.80e-09
    ## land_area_2010_scale                      0.0058554  0.0013763   4.255 2.09e-05
    ## education_scale                           0.0176514  0.0025345   6.965 3.30e-12
    ## employment_all1                           0.0240066  0.0031215   7.691 1.46e-14
    ## sex2                                      0.2165141  0.0029106  74.388  < 2e-16
    ## age_scale                                 0.0994239  0.0017339  57.343  < 2e-16
    ## race2                                     0.1765656  0.0102885  17.161  < 2e-16
    ## race3                                     0.1347732  0.0045369  29.706  < 2e-16
    ## race4                                     0.1565825  0.0084897  18.444  < 2e-16
    ## race5                                     0.0695652  0.0046506  14.958  < 2e-16
    ## married2                                  0.0637705  0.0039549  16.125  < 2e-16
    ## married3                                  0.0752485  0.0097411   7.725 1.12e-14
    ## married4                                  0.0257776  0.0051949   4.962 6.98e-07
    ## married5                                  0.0477495  0.0061406   7.776 7.50e-15
    ## married8                                  0.0707348  0.0062700  11.282  < 2e-16
    ## year2015                                 -0.0095989  0.0036796  -2.609  0.00909
    ## year2016                                 -0.0179466  0.0038090  -4.712 2.46e-06
    ## year2017                                 -0.0005675  0.0038735  -0.147  0.88352
    ##                                             
    ## (Intercept)                              ***
    ## PURPOSE_scale                            ***
    ## FINANCIAL_scale                          ***
    ## COMMUNITY_scale                          ***
    ## SOCIAL_scale                             ***
    ## median_income_demo_scale                    
    ## raw_income_scale                         ***
    ## physicians_scale                         .  
    ## total_pop_county_scale                      
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     ***
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
    ## married5                                 ***
    ## married8                                 ***
    ## year2015                                 ** 
    ## year2016                                 ***
    ## year2017                                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9467 on 551047 degrees of freedom
    ## Multiple R-squared:  0.07172,    Adjusted R-squared:  0.07167 
    ## F-statistic:  1637 on 26 and 551047 DF,  p-value: < 2.2e-16

#### Eat healthy all day yesterday

``` r
lma_eh <-
  glm(
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
      year,
    family = "binomial",
    data = data 
  )

summary(lma_eh)
```

    ## 
    ## Call:
    ## glm(formula = eat_healthy ~ PURPOSE_scale + FINANCIAL_scale + 
    ##     COMMUNITY_scale + SOCIAL_scale + median_income_demo_scale + 
    ##     raw_income_scale + physicians_scale + total_pop_county_scale + 
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale + 
    ##     education_scale + employment_all + sex + age_scale + race + 
    ##     married + year, family = "binomial", data = data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6733  -1.1328   0.6089   0.9000   2.1739  
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                               1.027337   0.011209  91.656  < 2e-16
    ## PURPOSE_scale                             0.222894   0.004129  53.978  < 2e-16
    ## FINANCIAL_scale                           0.092195   0.003789  24.330  < 2e-16
    ## COMMUNITY_scale                           0.095162   0.003671  25.923  < 2e-16
    ## SOCIAL_scale                              0.244674   0.003867  63.268  < 2e-16
    ## median_income_demo_scale                 -0.031223   0.006411  -4.870 1.11e-06
    ## raw_income_scale                         -0.146877   0.003979 -36.911  < 2e-16
    ## physicians_scale                          0.023113   0.003868   5.975 2.30e-09
    ## total_pop_county_scale                    0.002356   0.003396   0.694 0.487779
    ## median_monthly_housing_cost_county_scale  0.078073   0.003696  21.123  < 2e-16
    ## land_area_2010_scale                      0.018565   0.003379   5.494 3.93e-08
    ## education_scale                           0.057582   0.006167   9.337  < 2e-16
    ## employment_all1                          -0.214810   0.007472 -28.749  < 2e-16
    ## sex2                                     -0.148252   0.006941 -21.359  < 2e-16
    ## age_scale                                 0.523334   0.004172 125.426  < 2e-16
    ## race2                                     0.194786   0.024338   8.003 1.21e-15
    ## race3                                    -0.038000   0.010520  -3.612 0.000304
    ## race4                                     0.423296   0.019847  21.328  < 2e-16
    ## race5                                     0.381922   0.011045  34.579  < 2e-16
    ## married2                                 -0.039901   0.009108  -4.381 1.18e-05
    ## married3                                  0.064908   0.022541   2.880 0.003982
    ## married4                                 -0.070990   0.012176  -5.830 5.54e-09
    ## married5                                 -0.090938   0.015672  -5.802 6.54e-09
    ## married8                                  0.012291   0.014307   0.859 0.390282
    ## year2015                                 -0.026484   0.008809  -3.007 0.002641
    ## year2016                                 -0.040358   0.009144  -4.413 1.02e-05
    ## year2017                                  0.003273   0.009251   0.354 0.723529
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
    ## race3                                    ***
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
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 708225  on 551073  degrees of freedom
    ## Residual deviance: 637015  on 551047  degrees of freedom
    ## AIC: 637069
    ## 
    ## Number of Fisher Scoring iterations: 4

#### Smoking

``` r
lma_sm <-
  glm(
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
      year,
    family = "binomial",
    data = data 
  )

summary(lma_sm)
```

    ## 
    ## Call:
    ## glm(formula = smoke ~ PURPOSE_scale + FINANCIAL_scale + COMMUNITY_scale + 
    ##     SOCIAL_scale + median_income_demo_scale + raw_income_scale + 
    ##     physicians_scale + total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, family = "binomial", 
    ##     data = data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8078  -0.6032  -0.4325  -0.2988   3.0532  
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error  z value Pr(>|z|)
    ## (Intercept)                              -1.549827   0.013939 -111.186  < 2e-16
    ## PURPOSE_scale                             0.012135   0.005161    2.351 0.018702
    ## FINANCIAL_scale                          -0.284349   0.004637  -61.321  < 2e-16
    ## COMMUNITY_scale                          -0.079879   0.004554  -17.540  < 2e-16
    ## SOCIAL_scale                             -0.061200   0.004866  -12.576  < 2e-16
    ## median_income_demo_scale                  0.140900   0.008084   17.429  < 2e-16
    ## raw_income_scale                         -0.161562   0.005200  -31.068  < 2e-16
    ## physicians_scale                         -0.029039   0.005068   -5.730 1.00e-08
    ## total_pop_county_scale                   -0.017315   0.004675   -3.704 0.000212
    ## median_monthly_housing_cost_county_scale -0.092313   0.005009  -18.429  < 2e-16
    ## land_area_2010_scale                     -0.012556   0.004332   -2.898 0.003753
    ## education_scale                          -0.558577   0.007827  -71.367  < 2e-16
    ## employment_all1                          -0.103844   0.009387  -11.063  < 2e-16
    ## sex2                                     -0.277568   0.009167  -30.279  < 2e-16
    ## age_scale                                -0.246110   0.005283  -46.584  < 2e-16
    ## race2                                     0.306902   0.026525   11.570  < 2e-16
    ## race3                                    -0.049055   0.012787   -3.836 0.000125
    ## race4                                    -0.328567   0.030416  -10.802  < 2e-16
    ## race5                                    -0.603805   0.014378  -41.995  < 2e-16
    ## married2                                 -0.269752   0.011669  -23.118  < 2e-16
    ## married3                                  0.425239   0.024121   17.630  < 2e-16
    ## married4                                  0.369352   0.014196   26.019  < 2e-16
    ## married5                                  0.004253   0.018934    0.225 0.822293
    ## married8                                  0.446386   0.016187   27.577  < 2e-16
    ## year2015                                 -0.085152   0.011180   -7.616 2.61e-14
    ## year2016                                 -0.104216   0.011587   -8.994  < 2e-16
    ## year2017                                 -0.113456   0.011866   -9.562  < 2e-16
    ##                                             
    ## (Intercept)                              ***
    ## PURPOSE_scale                            *  
    ## FINANCIAL_scale                          ***
    ## COMMUNITY_scale                          ***
    ## SOCIAL_scale                             ***
    ## median_income_demo_scale                 ***
    ## raw_income_scale                         ***
    ## physicians_scale                         ***
    ## total_pop_county_scale                   ***
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     ** 
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
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 480338  on 551073  degrees of freedom
    ## Residual deviance: 426070  on 551047  degrees of freedom
    ## AIC: 426124
    ## 
    ## Number of Fisher Scoring iterations: 5

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
    ## 1   purpose     -0.016871416    0.54198123
    ## 2 financial      0.002482999   -0.07976443
    ## 3 community     -0.001126982    0.03620344
    ## 4    social     -0.017291220    0.55546711

#### Eat healthy all day yesterday

``` r
eh_ie <- calculate_indirect_effects_med1(lm_eh, coef_pur, coef_fin, coef_com, coef_soc)
eh_ie
```

    ##   mediatior indirect_effects prop_mediated
    ## 1   purpose     -0.007754095     0.4641185
    ## 2 financial     -0.002974739     0.1780519
    ## 3 community     -0.002061354     0.1233816
    ## 4    social     -0.007627244     0.4565260

#### Smoking

``` r
sm_ie <- calculate_indirect_effects_med1(lm_sm, coef_pur, coef_fin, coef_com, coef_soc)
sm_ie
```

    ##   mediatior indirect_effects prop_mediated
    ## 1   purpose    -0.0001974213   -0.00947238
    ## 2 financial     0.0061835858    0.29669182
    ## 3 community     0.0012034078    0.05774016
    ## 4    social     0.0011770723    0.05647657

## Mediation Analysis 2: Well-Being Composites with Financial and Social Subsets

### Mediator models

``` r
lm_em <-
  lm(
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
      year,
    data = data 
  )

lm_cs <-
  lm(
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
      year,
    data = data 
  )

lm_soc <-
  lm(
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
      year,
    data = data 
  ) 
```

### Linear Outcome models

#### Fruit and vegetable consumption

``` r
lm_fv <-
  lm(
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
      year,
    data = data 
  )
```

#### Eat healthy all day yesterday

``` r
lm_eh <-
  lm(
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
      year,
    data = data 
  )
```

#### Smoking

``` r
lm_sm <-
  lm(
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
      year,
    data = data 
  )
```

### Actual Outcome models

#### Fruit and vegetable consumption

``` r
summary(lm_fv)
```

    ## 
    ## Call:
    ## lm(formula = fruits_veggies_scale ~ PURPOSE_scale + enough_money_scale + 
    ##     comp_satis_std_liv_scale + COMMUNITY_scale + social_subset_scale + 
    ##     median_income_demo_scale + raw_income_scale + physicians_scale + 
    ##     total_pop_county_scale + median_monthly_housing_cost_county_scale + 
    ##     land_area_2010_scale + education_scale + employment_all + 
    ##     sex + age_scale + race + married + year, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.38938 -0.70993  0.05756  0.86136  2.12742 
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                              -0.186346   0.004696 -39.682  < 2e-16
    ## PURPOSE_scale                             0.119722   0.001821  65.748  < 2e-16
    ## enough_money_scale                       -0.011016   0.001669  -6.599 4.15e-11
    ## comp_satis_std_liv_scale                 -0.008559   0.001807  -4.736 2.18e-06
    ## COMMUNITY_scale                           0.015494   0.001623   9.545  < 2e-16
    ## social_subset_scale                       0.082689   0.001728  47.842  < 2e-16
    ## median_income_demo_scale                 -0.001410   0.002662  -0.530   0.5963
    ## raw_income_scale                         -0.009494   0.001674  -5.672 1.41e-08
    ## physicians_scale                          0.003217   0.001628   1.976   0.0482
    ## total_pop_county_scale                    0.001709   0.001428   1.197   0.2315
    ## median_monthly_housing_cost_county_scale  0.010135   0.001548   6.547 5.88e-11
    ## land_area_2010_scale                      0.005806   0.001379   4.210 2.55e-05
    ## education_scale                           0.022630   0.002545   8.893  < 2e-16
    ## employment_all1                           0.004824   0.003122   1.545   0.1223
    ## sex2                                      0.214877   0.002913  73.770  < 2e-16
    ## age_scale                                 0.100053   0.001730  57.824  < 2e-16
    ## race2                                     0.179776   0.010308  17.441  < 2e-16
    ## race3                                     0.140961   0.004544  31.018  < 2e-16
    ## race4                                     0.159826   0.008508  18.785  < 2e-16
    ## race5                                     0.073031   0.004660  15.671  < 2e-16
    ## married2                                  0.064956   0.003980  16.319  < 2e-16
    ## married3                                  0.083032   0.009767   8.501  < 2e-16
    ## married4                                  0.033499   0.005211   6.428 1.29e-10
    ## married5                                  0.055489   0.006152   9.020  < 2e-16
    ## married8                                  0.073730   0.006285  11.731  < 2e-16
    ## year2015                                 -0.008850   0.003687  -2.400   0.0164
    ## year2016                                 -0.016642   0.003817  -4.360 1.30e-05
    ## year2017                                 -0.001292   0.003881  -0.333   0.7392
    ##                                             
    ## (Intercept)                              ***
    ## PURPOSE_scale                            ***
    ## enough_money_scale                       ***
    ## comp_satis_std_liv_scale                 ***
    ## COMMUNITY_scale                          ***
    ## social_subset_scale                      ***
    ## median_income_demo_scale                    
    ## raw_income_scale                         ***
    ## physicians_scale                         *  
    ## total_pop_county_scale                      
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     ***
    ## education_scale                          ***
    ## employment_all1                             
    ## sex2                                     ***
    ## age_scale                                ***
    ## race2                                    ***
    ## race3                                    ***
    ## race4                                    ***
    ## race5                                    ***
    ## married2                                 ***
    ## married3                                 ***
    ## married4                                 ***
    ## married5                                 ***
    ## married8                                 ***
    ## year2015                                 *  
    ## year2016                                 ***
    ## year2017                                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9486 on 551046 degrees of freedom
    ## Multiple R-squared:  0.06797,    Adjusted R-squared:  0.06792 
    ## F-statistic:  1488 on 27 and 551046 DF,  p-value: < 2.2e-16

#### Eat healthy all day yesterday

``` r
lma_eh <-
  glm(
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
      year,
    family = "binomial",
    data = data 
  )
```

#### Smoking

``` r
lma_sm <-
  glm(
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
      year,
    family = "binomial",
    data = data 
  )

summary(lma_sm)
```

    ## 
    ## Call:
    ## glm(formula = smoke ~ PURPOSE_scale + enough_money_scale + comp_satis_std_liv_scale + 
    ##     COMMUNITY_scale + social_subset_scale + median_income_demo_scale + 
    ##     raw_income_scale + physicians_scale + total_pop_county_scale + 
    ##     median_monthly_housing_cost_county_scale + land_area_2010_scale + 
    ##     education_scale + employment_all + sex + age_scale + race + 
    ##     married + year, family = "binomial", data = data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7644  -0.6072  -0.4353  -0.2989   3.0474  
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error  z value Pr(>|z|)
    ## (Intercept)                              -1.607010   0.013978 -114.970  < 2e-16
    ## PURPOSE_scale                            -0.011735   0.005228   -2.245 0.024789
    ## enough_money_scale                       -0.212792   0.004881  -43.592  < 2e-16
    ## comp_satis_std_liv_scale                  0.003104   0.004940    0.628 0.529783
    ## COMMUNITY_scale                          -0.093880   0.004648  -20.198  < 2e-16
    ## social_subset_scale                      -0.087693   0.004929  -17.792  < 2e-16
    ## median_income_demo_scale                  0.145985   0.008078   18.073  < 2e-16
    ## raw_income_scale                         -0.191505   0.005157  -37.137  < 2e-16
    ## physicians_scale                         -0.028198   0.005055   -5.578 2.44e-08
    ## total_pop_county_scale                   -0.015831   0.004664   -3.394 0.000688
    ## median_monthly_housing_cost_county_scale -0.088683   0.004998  -17.743  < 2e-16
    ## land_area_2010_scale                     -0.012953   0.004316   -3.001 0.002693
    ## education_scale                          -0.546160   0.007810  -69.928  < 2e-16
    ## employment_all1                          -0.035697   0.009366   -3.811 0.000138
    ## sex2                                     -0.251424   0.009125  -27.554  < 2e-16
    ## age_scale                                -0.270249   0.005246  -51.516  < 2e-16
    ## race2                                     0.324659   0.026443   12.278  < 2e-16
    ## race3                                    -0.040460   0.012742   -3.175 0.001496
    ## race4                                    -0.346248   0.030412  -11.385  < 2e-16
    ## race5                                    -0.596027   0.014366  -41.489  < 2e-16
    ## married2                                 -0.247888   0.011711  -21.167  < 2e-16
    ## married3                                  0.445887   0.024047   18.542  < 2e-16
    ## married4                                  0.369498   0.014176   26.064  < 2e-16
    ## married5                                 -0.016336   0.018910   -0.864 0.387677
    ## married8                                  0.474091   0.016152   29.352  < 2e-16
    ## year2015                                 -0.087415   0.011156   -7.835 4.67e-15
    ## year2016                                 -0.106383   0.011562   -9.201  < 2e-16
    ## year2017                                 -0.119949   0.011843  -10.129  < 2e-16
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
    ## total_pop_county_scale                   ***
    ## median_monthly_housing_cost_county_scale ***
    ## land_area_2010_scale                     ** 
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
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 480338  on 551073  degrees of freedom
    ## Residual deviance: 427663  on 551046  degrees of freedom
    ## AIC: 427719
    ## 
    ## Number of Fisher Scoring iterations: 5

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
    ## 1           purpose     -0.020128293    0.64660590
    ## 2      enough_money      0.001708083   -0.05487086
    ## 3 comp_satis_living      0.001573417   -0.05054482
    ## 4         community     -0.001698774    0.05457179
    ## 5     social_subset     -0.011173109    0.35892751

#### Eat healthy all day yesterday

``` r
eh_ie <- calculate_indirect_effects_med2(lm_eh, coef_pur, coef_em, coef_cs, coef_com, coef_soc)
eh_ie
```

    ##           mediatior indirect_effects prop_mediated
    ## 1           purpose    -0.0090376843    0.54094732
    ## 2      enough_money    -0.0029738056    0.17799606
    ## 3 comp_satis_living    -0.0005637229    0.03374143
    ## 4         community    -0.0022341427    0.13372380
    ## 5     social_subset    -0.0047487074    0.28423216

#### Smoking

``` r
sm_ie <- calculate_indirect_effects_med2(lm_sm, coef_pur, coef_em, coef_cs, coef_com, coef_soc)
sm_ie
```

    ##           mediatior indirect_effects prop_mediated
    ## 1           purpose     0.0003339872   0.016024888
    ## 2      enough_money     0.0040520514   0.194419637
    ## 3 comp_satis_living     0.0001619527   0.007770578
    ## 4         community     0.0013864032   0.066520381
    ## 5     social_subset     0.0016747461   0.080355232
