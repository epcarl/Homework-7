Lab 7
================
Erik Carlson, Emily Vasquez, Emmanuel Mendez, Joe Correa

## Econ B2000, Econometrics

\~ In this HW assignment we use several models to try and predict if a
person from our selected subset has health insurance. We use logit,
Random Forest, Support Vector Machines, and Elastic Net.

``` r
load("NHIS_2014.RData")
```

\~ The code below tells R to include the NA values of the money earned
last year variable, when running regressions.

``` r
data_use1$earn_lastyr <- as.factor(data_use1$ERNYR_P)
levels(data_use1$earn_lastyr) <- c("0","$01-$4999","$5000-$9999","$10000-$14999","$15000-$19999","$20000-$24999","$25000-$34999","$35000-$44999","$45000-$54999","$55000-$64999","$65000-$74999","$75000 and over",NA,NA,NA)
```

\~ Our selected subgroup is Adults between ages 25 up to and including
55. We wanted to measure if a persons health status has an impact on
whether or not that person has health insurance. We chose people with at
least good health for our subset.

``` r
pick_use2 <- (data_use1$AGE_P >25) & (data_use1$AGE_P <= 55) & ((data_use1$person_healthstatus=="Excellent")|(data_use1$person_healthstatus=="Very good")|(data_use1$person_healthstatus=="Good"))
data_use2 <- subset(data_use1, pick_use2)
data_use2$person_healthstatus <- droplevels(data_use2$person_healthstatus)
```

\~ We also thought it would be interesting to test the impact of how
much a person spends on medical in a given year, and whether that has
any impact on that person having health insurance. We predict that
someone who spends more in a given year on medical is more likely to
have health insurance( higher probability). The logit model below shows
that the more you spent on medical the more likely you are to have
health insurance. This could be due to that if you dont have health
insurance to cover some expense you dont go to the doctor. Keep in mind
we are using at least good health in our subset. Also

``` r
model_logit1 <- glm(NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + RaceOther  
                    + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                    + married + widowed + divorc_sep + veteran_stat + REGION + region_born +sptn_medical,
                    family = binomial, data = data_use2)
summary(model_logit1)
```

    ## 
    ## Call:
    ## glm(formula = NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + 
    ##     RaceOther + Hispanic + educ_hs + educ_smcoll + educ_as + 
    ##     educ_bach + educ_adv + married + widowed + divorc_sep + veteran_stat + 
    ##     REGION + region_born + sptn_medical, family = binomial, data = data_use2)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0706  -0.6201  -0.4152  -0.2350   2.9960  
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                    0.1103608  0.3426007   0.322 0.747357    
    ## AGE_P                         -0.0110564  0.0175834  -0.629 0.529481    
    ## I(AGE_P^2)                    -0.0001925  0.0002188  -0.880 0.378978    
    ## female                        -0.2930933  0.0291953 -10.039  < 2e-16 ***
    ## AfAm                          -0.1995465  0.0450504  -4.429 9.45e-06 ***
    ## Asian                         -0.1119950  0.0985926  -1.136 0.255983    
    ## RaceOther                      0.2719196  0.0934435   2.910 0.003614 ** 
    ## Hispanic                       0.2559893  0.0484008   5.289 1.23e-07 ***
    ## educ_hs                       -0.3168698  0.0410877  -7.712 1.24e-14 ***
    ## educ_smcoll                   -0.6391260  0.0481720 -13.268  < 2e-16 ***
    ## educ_as                       -0.8770048  0.0551346 -15.907  < 2e-16 ***
    ## educ_bach                     -1.5572347  0.0553621 -28.128  < 2e-16 ***
    ## educ_adv                      -2.1871876  0.0840741 -26.015  < 2e-16 ***
    ## married                       -0.7648380  0.0336990 -22.696  < 2e-16 ***
    ## widowed                        0.0895452  0.1514945   0.591 0.554468    
    ## divorc_sep                    -0.0920553  0.0492854  -1.868 0.061790 .  
    ## veteran_stat                  -0.6051974  0.0780939  -7.750 9.22e-15 ***
    ## REGIONMidwest                  0.3131024  0.0535858   5.843 5.13e-09 ***
    ## REGIONSouth                    0.7095207  0.0465303  15.249  < 2e-16 ***
    ## REGIONWest                     0.2824213  0.0489855   5.765 8.15e-09 ***
    ## region_bornMex Cent Am Caribb  1.1101202  0.0522640  21.241  < 2e-16 ***
    ## region_bornS Am                0.8947440  0.1068870   8.371  < 2e-16 ***
    ## region_bornEur                 0.2785126  0.1356564   2.053 0.040065 *  
    ## region_bornformer USSR         0.9334674  0.2561024   3.645 0.000267 ***
    ## region_bornAfrica              0.7754194  0.1352663   5.733 9.89e-09 ***
    ## region_bornMidE                0.7231996  0.2138967   3.381 0.000722 ***
    ## region_bornIndia subc          0.5450733  0.1611891   3.382 0.000721 ***
    ## region_bornAsia                0.8301855  0.1468424   5.654 1.57e-08 ***
    ## region_bornSE Asia             0.2505186  0.1352114   1.853 0.063912 .  
    ## region_bornElsewhere           0.0507258  0.2201526   0.230 0.817772    
    ## region_bornunknown             0.1256527  0.2595225   0.484 0.628266    
    ## sptn_medicalunder 500         -0.2981740  0.0419023  -7.116 1.11e-12 ***
    ## sptn_medical500-1999          -0.2944762  0.0445922  -6.604 4.01e-11 ***
    ## sptn_medical2000-2999         -0.2629952  0.0618480  -4.252 2.12e-05 ***
    ## sptn_medical3000-4999         -0.4766052  0.0789476  -6.037 1.57e-09 ***
    ## sptn_medical5000+             -0.2438582  0.0740674  -3.292 0.000993 ***
    ## sptn_medicalrefused           -0.7318731  0.2241667  -3.265 0.001095 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 37893  on 39413  degrees of freedom
    ## Residual deviance: 31333  on 39377  degrees of freedom
    ##   (836 observations deleted due to missingness)
    ## AIC: 31407
    ## 
    ## Number of Fisher Scoring iterations: 5

\~ The graph below shows the coefficients for each explanatory variable.
The data shows that someone born abroad is less likely to have health
insurance. This could be due to things such as immigration status and/or
income levels. Higher education levels also show a higher likelihood to
have health insurance. Our subset did not include the people with bad
health statuses so that may allow for different interpretations. People
with higher incomes also have the ability to buy healthier foods, which
are generally more expensive.

``` r
library(coefplot)
```

    ## Warning: package 'coefplot' was built under R version 3.6.3

    ## Warning: package 'ggplot2' was built under R version 3.6.3

``` r
library(ggplot2)
coefplot(model_logit1, innerCI=2, outerCI=0, intercept = FALSE, title = "Logit Model", color = "red", lab = "Explantory Variables")
```

![](HW_7_pub_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
data_use1_orig<-data_use1
rm(data_use1)
data_use1<-data_use2
```

\~The code below tells R to include the NA values of the spent on
medical variable for the previous year, when running the regressions.

``` r
levels(data_use1$sptn_medical)<-c("zero", "under 500" ,"500-1999" , "2000-2999" ,"3000-4999" ,"5000+"  ,  "refused","Is NA")
data_use1$sptn_medical[(is.na(data_use1$sptn_medical)==TRUE)]<-"Is NA"
```

\~ The code below prints the output string text and factors the non
dummy variables and changes them to dummy variables.

``` r
d_region <- data.frame(model.matrix(~ data_use1$REGION))
d_region_born <- data.frame(model.matrix(~ factor(data_use1$region_born)))  # snips any with zero in the subgroup
d_sptn_medical <- data.frame(model.matrix(~ factor(data_use1$sptn_medical)))
dat_for_analysis_sub <- data.frame(
  data_use1$NOTCOV,
  data_use1$AGE_P,
  data_use1$female,
  data_use1$AfAm,
  data_use1$Asian,
  data_use1$RaceOther,
  data_use1$Hispanic,
  data_use1$educ_hs,
  data_use1$educ_smcoll,
  data_use1$educ_as,
  data_use1$educ_bach,
  data_use1$educ_adv,
  data_use1$married,
  data_use1$widowed,
  data_use1$divorc_sep,
  d_region[,2:4],
  d_region_born[,2:12], # need [] since model.matrix includes intercept term
  d_sptn_medical[,2:8])

names(dat_for_analysis_sub) <- c("NOTCOV",
                                 "Age",
                                 "female",
                                 "AfAm",
                                 "Asian",
                                 "RaceOther",
                                 "Hispanic",
                                 "educ_hs",
                                 "educ_smcoll",
                                 "educ_as",
                                 "educ_bach",
                                 "educ_adv",
                                 "married",
                                 "widowed",
                                 "divorc_sep",
                                 "Region.Midwest",
                                 "Region.South",
                                 "Region.West",
                                 "born.Mex.CentAm.Carib",
                                 "born.S.Am",
                                 "born.Eur",
                                 "born.f.USSR",
                                 "born.Africa",
                                 "born.MidE",
                                 "born.India.subc",
                                 "born.Asia",
                                 "born.SE.Asia",
                                 "born.elsewhere",
                                 "born.unknown",
                                 "sptn.under_500",
                                 "sptn.500_1999",
                                 "sptn.2000_2999",
                                 "sptn.3000_4999",
                                 "sptn.5000_gr",
                                 "sptn.refused",
                                 "sptn.Is_NA")
```

\~ This code creates a common data object that is standardized and
splits it into the training and test data. There are 34,218 in the
training data and 6032 in the test data. We added in the amounts spent
on medical including the people who refused to answer as well as the NA
values.

``` r
require("standardize")
```

    ## Warning: package 'standardize' was built under R version 3.6.3

``` r
set.seed(654321)
NN <- length(dat_for_analysis_sub$NOTCOV)
#restrict_1 <- as.logical(round(runif(NN,min=0,max=0.6))) # use fraction as training data
restrict_1 <- (runif(NN) < 0.15)
summary(restrict_1)
```

    ##    Mode   FALSE    TRUE 
    ## logical   34218    6032

``` r
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)
sobj <- standardize(NOTCOV ~ Age + female + AfAm + Asian + RaceOther + Hispanic + 
                      educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv + 
                      married + widowed + divorc_sep + 
                      Region.Midwest + Region.South + Region.West + 
                      born.Mex.CentAm.Carib + born.S.Am + born.Eur + born.f.USSR + 
                      born.Africa + born.MidE + born.India.subc + born.Asia + 
                      born.SE.Asia + born.elsewhere + born.unknown + sptn.under_500+
                      sptn.500_1999 + sptn.2000_2999 + sptn.3000_4999 + sptn.5000_gr +sptn.refused + sptn.Is_NA , dat_train, family = binomial)
s_dat_test <- predict(sobj, dat_test)
```

\~ The code below runs the linear probability model on our subset using
the explanatory variables we added. The logit model predicts that 82% of
our set has health insurance. The actual correct number is 81.25% which
is not too far off. There are 1256 people who the model correctly
predicted do not have health insurance. We have 840 false positives
those who the model says are not covered but are indeed covered. 5159
false negatives are those who the model says are covered but are
actually not covered. The 26963 are those who are correctly predicted to
have health insurance.

``` r
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
```

    ## 
    ## Call:
    ## lm(formula = sobj$formula, data = sobj$data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.68810 -0.20051 -0.09439  0.01997  1.10138 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             0.248692   0.121129   2.053 0.040104 *  
    ## Age                    -0.030798   0.004777  -6.447 1.23e-10 ***
    ## female1                -0.009273   0.004577  -2.026 0.042808 *  
    ## AfAm1                  -0.023290   0.007322  -3.181 0.001476 ** 
    ## Asian1                 -0.020382   0.015090  -1.351 0.176850    
    ## RaceOther1              0.001607   0.017878   0.090 0.928376    
    ## Hispanic1               0.019699   0.008269   2.382 0.017231 *  
    ## educ_hs1               -0.022889   0.008131  -2.815 0.004892 ** 
    ## educ_smcoll1           -0.057207   0.008892  -6.434 1.34e-10 ***
    ## educ_as1               -0.057226   0.009510  -6.018 1.87e-09 ***
    ## educ_bach1             -0.097954   0.008701 -11.258  < 2e-16 ***
    ## educ_adv1              -0.101453   0.009947 -10.200  < 2e-16 ***
    ## married1               -0.061277   0.005565 -11.011  < 2e-16 ***
    ## widowed1                0.002047   0.023098   0.089 0.929400    
    ## divorc_sep1            -0.032536   0.008679  -3.749 0.000179 ***
    ## Region.Midwest1         0.011506   0.007728   1.489 0.136609    
    ## Region.South1           0.042583   0.007075   6.019 1.86e-09 ***
    ## Region.West1            0.012820   0.007325   1.750 0.080119 .  
    ## born.Mex.CentAm.Carib1  0.106007   0.009636  11.001  < 2e-16 ***
    ## born.S.Am1              0.048321   0.020713   2.333 0.019688 *  
    ## born.Eur1               0.018551   0.019123   0.970 0.332036    
    ## born.f.USSR1            0.065062   0.050949   1.277 0.201654    
    ## born.Africa1            0.006840   0.024240   0.282 0.777799    
    ## born.MidE1              0.080970   0.031798   2.546 0.010909 *  
    ## born.India.subc1        0.033174   0.022530   1.472 0.140945    
    ## born.Asia1              0.054018   0.022880   2.361 0.018262 *  
    ## born.SE.Asia1           0.040515   0.019957   2.030 0.042392 *  
    ## born.elsewhere1         0.039077   0.029297   1.334 0.182311    
    ## born.unknown1          -0.007375   0.038635  -0.191 0.848621    
    ## sptn.under_5001        -0.026807   0.007788  -3.442 0.000581 ***
    ## sptn.500_19991         -0.032861   0.008117  -4.049 5.22e-05 ***
    ## sptn.2000_29991        -0.027134   0.010245  -2.649 0.008106 ** 
    ## sptn.3000_49991        -0.038965   0.011967  -3.256 0.001137 ** 
    ## sptn.5000_gr1          -0.020787   0.011741  -1.770 0.076717 .  
    ## sptn.refused1          -0.019100   0.034385  -0.555 0.578589    
    ## sptn.Is_NA1            -0.032288   0.017186  -1.879 0.060336 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3514 on 5996 degrees of freedom
    ## Multiple R-squared:  0.1687, Adjusted R-squared:  0.1638 
    ## F-statistic: 34.76 on 35 and 5996 DF,  p-value: < 2.2e-16

``` r
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
```

    ## Warning: contrasts dropped from factor female

    ## Warning: contrasts dropped from factor AfAm

    ## Warning: contrasts dropped from factor Asian

    ## Warning: contrasts dropped from factor RaceOther

    ## Warning: contrasts dropped from factor Hispanic

    ## Warning: contrasts dropped from factor educ_hs

    ## Warning: contrasts dropped from factor educ_smcoll

    ## Warning: contrasts dropped from factor educ_as

    ## Warning: contrasts dropped from factor educ_bach

    ## Warning: contrasts dropped from factor educ_adv

    ## Warning: contrasts dropped from factor married

    ## Warning: contrasts dropped from factor widowed

    ## Warning: contrasts dropped from factor divorc_sep

    ## Warning: contrasts dropped from factor Region.Midwest

    ## Warning: contrasts dropped from factor Region.South

    ## Warning: contrasts dropped from factor Region.West

    ## Warning: contrasts dropped from factor born.Mex.CentAm.Carib

    ## Warning: contrasts dropped from factor born.S.Am

    ## Warning: contrasts dropped from factor born.Eur

    ## Warning: contrasts dropped from factor born.f.USSR

    ## Warning: contrasts dropped from factor born.Africa

    ## Warning: contrasts dropped from factor born.MidE

    ## Warning: contrasts dropped from factor born.India.subc

    ## Warning: contrasts dropped from factor born.Asia

    ## Warning: contrasts dropped from factor born.SE.Asia

    ## Warning: contrasts dropped from factor born.elsewhere

    ## Warning: contrasts dropped from factor born.unknown

    ## Warning: contrasts dropped from factor sptn.under_500

    ## Warning: contrasts dropped from factor sptn.500_1999

    ## Warning: contrasts dropped from factor sptn.2000_2999

    ## Warning: contrasts dropped from factor sptn.3000_4999

    ## Warning: contrasts dropped from factor sptn.5000_gr

    ## Warning: contrasts dropped from factor sptn.refused

    ## Warning: contrasts dropped from factor sptn.Is_NA

``` r
pred_model_lpm1 <- (pred_vals_lpm > 0.5)
pred_lm<-table(pred = pred_model_lpm1, true = dat_test$NOTCOV)
print(pred_lm)
```

    ##        true
    ## pred        0     1
    ##   FALSE 27230  5437
    ##   TRUE    573   978

``` r
(pred_lm[1,1]+pred_lm[2,2])/sum(pred_lm)
```

    ## [1] 0.8243614

``` r
# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
```

    ## 
    ## Call:
    ## glm(formula = sobj$formula, family = binomial, data = sobj$data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8115  -0.6002  -0.4052  -0.2268   2.9867  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)            -0.47907    0.94828  -0.505 0.613421    
    ## Age                    -0.25865    0.03967  -6.520 7.03e-11 ***
    ## female1                -0.08155    0.03753  -2.173 0.029790 *  
    ## AfAm1                  -0.15251    0.05945  -2.565 0.010309 *  
    ## Asian1                 -0.13496    0.13249  -1.019 0.308356    
    ## RaceOther1              0.00659    0.12741   0.052 0.958750    
    ## Hispanic1               0.13486    0.06246   2.159 0.030846 *  
    ## educ_hs1               -0.08718    0.05333  -1.635 0.102067    
    ## educ_smcoll1           -0.31190    0.06295  -4.954 7.26e-07 ***
    ## educ_as1               -0.31744    0.06914  -4.591 4.41e-06 ***
    ## educ_bach1             -0.80939    0.07437 -10.883  < 2e-16 ***
    ## educ_adv1              -0.95516    0.10550  -9.053  < 2e-16 ***
    ## married1               -0.45674    0.04302 -10.618  < 2e-16 ***
    ## widowed1                0.10125    0.17176   0.590 0.555512    
    ## divorc_sep1            -0.19023    0.06558  -2.901 0.003721 ** 
    ## Region.Midwest1         0.11233    0.07144   1.572 0.115842    
    ## Region.South1           0.37127    0.06233   5.957 2.58e-09 ***
    ## Region.West1            0.14694    0.06475   2.269 0.023244 *  
    ## born.Mex.CentAm.Carib1  0.58345    0.06719   8.684  < 2e-16 ***
    ## born.S.Am1              0.39280    0.14971   2.624 0.008698 ** 
    ## born.Eur1               0.22553    0.16661   1.354 0.175841    
    ## born.f.USSR1            0.66974    0.40046   1.672 0.094435 .  
    ## born.Africa1            0.04726    0.21988   0.215 0.829825    
    ## born.MidE1              0.70101    0.24189   2.898 0.003754 ** 
    ## born.India.subc1        0.31709    0.21500   1.475 0.140263    
    ## born.Asia1              0.48714    0.19472   2.502 0.012358 *  
    ## born.SE.Asia1           0.35109    0.17368   2.022 0.043228 *  
    ## born.elsewhere1         0.35843    0.22769   1.574 0.115441    
    ## born.unknown1          -0.01869    0.30038  -0.062 0.950395    
    ## sptn.under_5001        -0.16203    0.05480  -2.957 0.003111 ** 
    ## sptn.500_19991         -0.21818    0.05935  -3.677 0.000236 ***
    ## sptn.2000_29991        -0.18211    0.08195  -2.222 0.026261 *  
    ## sptn.3000_49991        -0.33490    0.11364  -2.947 0.003209 ** 
    ## sptn.5000_gr1          -0.10149    0.09628  -1.054 0.291819    
    ## sptn.refused1          -0.05745    0.27060  -0.212 0.831860    
    ## sptn.Is_NA1            -0.19402    0.13328  -1.456 0.145470    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 5687.6  on 6031  degrees of freedom
    ## Residual deviance: 4672.7  on 5996  degrees of freedom
    ## AIC: 4744.7
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
```

    ## Warning: contrasts dropped from factor female

    ## Warning: contrasts dropped from factor AfAm

    ## Warning: contrasts dropped from factor Asian

    ## Warning: contrasts dropped from factor RaceOther

    ## Warning: contrasts dropped from factor Hispanic

    ## Warning: contrasts dropped from factor educ_hs

    ## Warning: contrasts dropped from factor educ_smcoll

    ## Warning: contrasts dropped from factor educ_as

    ## Warning: contrasts dropped from factor educ_bach

    ## Warning: contrasts dropped from factor educ_adv

    ## Warning: contrasts dropped from factor married

    ## Warning: contrasts dropped from factor widowed

    ## Warning: contrasts dropped from factor divorc_sep

    ## Warning: contrasts dropped from factor Region.Midwest

    ## Warning: contrasts dropped from factor Region.South

    ## Warning: contrasts dropped from factor Region.West

    ## Warning: contrasts dropped from factor born.Mex.CentAm.Carib

    ## Warning: contrasts dropped from factor born.S.Am

    ## Warning: contrasts dropped from factor born.Eur

    ## Warning: contrasts dropped from factor born.f.USSR

    ## Warning: contrasts dropped from factor born.Africa

    ## Warning: contrasts dropped from factor born.MidE

    ## Warning: contrasts dropped from factor born.India.subc

    ## Warning: contrasts dropped from factor born.Asia

    ## Warning: contrasts dropped from factor born.SE.Asia

    ## Warning: contrasts dropped from factor born.elsewhere

    ## Warning: contrasts dropped from factor born.unknown

    ## Warning: contrasts dropped from factor sptn.under_500

    ## Warning: contrasts dropped from factor sptn.500_1999

    ## Warning: contrasts dropped from factor sptn.2000_2999

    ## Warning: contrasts dropped from factor sptn.3000_4999

    ## Warning: contrasts dropped from factor sptn.5000_gr

    ## Warning: contrasts dropped from factor sptn.refused

    ## Warning: contrasts dropped from factor sptn.Is_NA

``` r
pred_model_logit1 <- (pred_vals > 0.5)
pred_table<-table(pred = pred_model_logit1, true = dat_test$NOTCOV)
print(pred_table)
```

    ##        true
    ## pred        0     1
    ##   FALSE 26963  5159
    ##   TRUE    840  1256

``` r
(pred_table[1,1]+pred_table[2,2])/sum(pred_table)
```

    ## [1] 0.8246829

``` r
#0.8125 have health insurance
```

\~ The code below runs the random forest model on our subset using the
explanatory variables we added. The random forest model predicts that
82.73% of our set has health insurance. The actual correct number is
81.25% which is not too far off. There are 1091 people who the model
correctly predicted do not have health insurance. We have 583 false
positives those who the model says are not covered but are indeed
covered. 5324 false negatives are those who the model says are covered
but are actually not covered. The 27220 are those who are correctly
predicted to have health insurance. The graph below has the mexican and
central american region as the variable with the highest degree of
accuracy when predicting whether or not a person has health insurance.
Age is a good predictor in this model at reducing classification errors,
likewise with education levels.

``` r
require('randomForest')
```

    ## Warning: package 'randomForest' was built under R version 3.6.3

``` r
set.seed(54321)
model_randFor <- randomForest(as.factor(NOTCOV) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
```

    ## 
    ## Call:
    ##  randomForest(formula = as.factor(NOTCOV) ~ ., data = sobj$data,      importance = TRUE, proximity = TRUE) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 5
    ## 
    ##         OOB estimate of  error rate: 16.93%
    ## Confusion matrix:
    ##      0   1 class.error
    ## 0 4826 120  0.02426203
    ## 1  901 185  0.82965009

``` r
round(importance(model_randFor),2)
```

    ##                           0     1 MeanDecreaseAccuracy MeanDecreaseGini
    ## Age                    3.86 15.91                12.43           140.38
    ## female                 1.49  5.79                 4.53            27.54
    ## AfAm                   6.00  1.28                 6.77            18.22
    ## Asian                 -0.27 -0.02                -0.29             9.81
    ## RaceOther              1.95  1.92                 2.74             8.87
    ## Hispanic              -9.01 30.37                26.00            61.35
    ## educ_hs                8.98  1.77                 9.73            23.91
    ## educ_smcoll           10.62  9.83                15.44            18.04
    ## educ_as                8.37  6.23                10.72            15.08
    ## educ_bach             14.57 26.33                26.07            32.40
    ## educ_adv              11.37 26.32                22.96            20.41
    ## married               10.52 15.59                16.71            44.77
    ## widowed               -4.67  8.96                 0.09             5.01
    ## divorc_sep             9.81 -3.97                 8.00            15.38
    ## Region.Midwest        -1.93  3.26                 0.11            14.15
    ## Region.South           5.85 18.89                17.58            25.45
    ## Region.West            2.72  3.56                 5.49            18.79
    ## born.Mex.CentAm.Carib -6.01 45.53                28.68            97.65
    ## born.S.Am              0.99  8.81                 5.52             7.18
    ## born.Eur              -7.21 -1.27                -7.18             5.16
    ## born.f.USSR           -4.78 -0.91                -4.96             1.61
    ## born.Africa           -3.16 -1.39                -3.58             3.48
    ## born.MidE             -0.12  0.97                 0.32             3.98
    ## born.India.subc       -0.68  1.32                -0.09             3.74
    ## born.Asia              0.38 -1.38                -0.15             4.85
    ## born.SE.Asia          -1.95  3.11                -0.72             6.09
    ## born.elsewhere        -1.23  1.15                -0.65             3.31
    ## born.unknown          -1.43 -1.39                -1.79             2.12
    ## sptn.under_500         8.39 -6.32                 4.02            19.13
    ## sptn.500_1999          8.64 -0.01                 7.64            20.18
    ## sptn.2000_2999         3.86 -4.43                 1.18            12.36
    ## sptn.3000_4999        -1.80  7.84                 2.62             9.48
    ## sptn.5000_gr           0.95 -2.45                -0.41            10.86
    ## sptn.refused          -2.12 -0.39                -2.12             3.25
    ## sptn.Is_NA            -3.73 -2.55                -4.48             7.04

``` r
varImpPlot(model_randFor)
```

![](HW_7_pub_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# look at confusion matrix for this too
pred_model1 <- predict(model_randFor,  s_dat_test)
pred_rf<-table(pred = pred_model1, true = dat_test$NOTCOV)
print(pred_rf)
```

    ##     true
    ## pred     0     1
    ##    0 27220  5324
    ##    1   583  1091

``` r
(pred_rf[1,1]+pred_rf[2,2])/sum(pred_rf)
```

    ## [1] 0.8273716

``` r
require(e1071)
tuned_parameters <- tune.svm(as.factor(NOTCOV) ~ ., data = sobj$data, gamma = 10^(-3:0), cost = 10^(-2:1)) 
summary(tuned_parameters)
```

The code below runs the support vector machines probability model with
tuned parameters on our subset using the explanatory variables we added.
The support vector machines model predicts that 82.59% of our set has
health insurance. The actual correct number is 81.25% which is not too
far off. There are 1061 people who the model correctly predicted do not
have health insurance. We have 603 false positives those who the model
says are not covered but are indeed covered. 5354 false negatives are
those who the model says are covered but are actually not covered. The
27200 are those who are correctly predicted to have health insurance.

``` r
#This is with tuned parameters and the tuned parameters.rds file is for the support vector machine method. It takes less time and spits out a better result.
require(e1071)
```

    ## Warning: package 'e1071' was built under R version 3.6.3

``` r
tuned_parameters <- readRDS("tuned_parameters.rds")
best.linear = tuned_parameters$best.model
tune.test = predict(best.linear, s_dat_test)
pred_tuned<-table(pred=tune.test, true=dat_test$NOTCOV)
pred_tuned
```

    ##     true
    ## pred     0     1
    ##    0 27200  5354
    ##    1   603  1061

``` r
(pred_tuned[1,1]+pred_tuned[2,2])/sum(pred_tuned)
```

    ## [1] 0.8259103

The code below runs the support vector machines probability model
without tuned parameters on our subset using the explanatory variables
we added. The support vector machines model predicts that 80.22% of our
set has health insurance. The actual correct number is 81.25% which is
not too far off. There are 1703 people who the model correctly predicted
do not have health insurance. We have 2054 false positives those who the
model says are not covered but are indeed covered. 4712 false negatives
are those who the model says are covered but are actually not covered.
The 25749 are those who are correctly predicted to have health
insurance.

``` r
#Without tuned parameters

svm.model <- svm(as.factor(NOTCOV) ~ ., data = sobj$data, cost = 10, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
pred_svm<-table(pred = svm.pred, true = dat_test$NOTCOV)
pred_svm
```

    ##     true
    ## pred     0     1
    ##    0 25749  4712
    ##    1  2054  1703

``` r
(pred_svm[1,1]+pred_svm[2,2])/sum(pred_svm)
```

    ## [1] 0.8022678

The code below runs the elastic net probability model on our subset
using the explanatory variables we added. The elastic net model predicts
that 65.46% of our set has health insurance. The actual correct number
is 81.25% so this model has the lowest accuracy for predicting our
subset. There are 4707 people who the model correctly predicted do not
have health insurance. We have 10108 false positives those who the model
says are not covered but are indeed covered. 1708 false negatives are
those who the model says are covered but are actually not covered. The
17695 are those who are correctly predicted to to have health insurance.
The graph below shows the explanatory variables graphed with their
coefficients and lasso L1 norm values. The 2 that have the biggest
predicting in terms of accuracy are educ adv and the region mexico and
central america and carribean birthplaces.

``` r
# Elastic Net
require(glmnet)
```

    ## Warning: package 'glmnet' was built under R version 3.6.3

``` r
model1_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV) 
# default is alpha = 1, lasso

par(mar=c(4.5,4.5,1,4))
plot(model1_elasticnet)
vnat=coef(model1_elasticnet)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
axis(4, at=vnat,line=-.5,label=names(sobj$data[,-1]),las=1,tick=FALSE, cex.axis=0.5) 
```

![](HW_7_pub_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
plot(model1_elasticnet, xvar = "lambda")
```

![](HW_7_pub_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
plot(model1_elasticnet, xvar = "dev", label = TRUE)
```

![](HW_7_pub_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
print(model1_elasticnet)
```

    ## 
    ## Call:  glmnet(x = as.matrix(sobj$data[, -1]), y = sobj$data$NOTCOV) 
    ## 
    ##    Df  %Dev   Lambda
    ## 1   0  0.00 0.110500
    ## 2   1  1.41 0.100700
    ## 3   1  2.57 0.091770
    ## 4   1  3.54 0.083610
    ## 5   1  4.34 0.076190
    ## 6   2  5.05 0.069420
    ## 7   2  5.70 0.063250
    ## 8   3  6.49 0.057630
    ## 9   3  7.33 0.052510
    ## 10  4  8.14 0.047850
    ## 11  4  8.88 0.043600
    ## 12  5  9.49 0.039720
    ## 13  6 10.30 0.036190
    ## 14  6 10.97 0.032980
    ## 15  7 11.54 0.030050
    ## 16  8 12.11 0.027380
    ## 17  8 12.59 0.024950
    ## 18  8 13.00 0.022730
    ## 19  8 13.33 0.020710
    ## 20  8 13.61 0.018870
    ## 21  8 13.84 0.017200
    ## 22  8 14.03 0.015670
    ## 23  8 14.19 0.014280
    ## 24  9 14.33 0.013010
    ## 25 12 14.53 0.011850
    ## 26 13 14.80 0.010800
    ## 27 15 15.04 0.009840
    ## 28 16 15.28 0.008966
    ## 29 16 15.46 0.008169
    ## 30 17 15.62 0.007444
    ## 31 18 15.75 0.006782
    ## 32 18 15.86 0.006180
    ## 33 19 15.96 0.005631
    ## 34 19 16.05 0.005131
    ## 35 20 16.12 0.004675
    ## 36 20 16.18 0.004259
    ## 37 21 16.24 0.003881
    ## 38 23 16.30 0.003536
    ## 39 23 16.35 0.003222
    ## 40 26 16.39 0.002936
    ## 41 27 16.45 0.002675
    ## 42 27 16.50 0.002437
    ## 43 27 16.55 0.002221
    ## 44 28 16.59 0.002024
    ## 45 28 16.63 0.001844
    ## 46 30 16.66 0.001680
    ## 47 30 16.69 0.001531
    ## 48 30 16.71 0.001395
    ## 49 31 16.74 0.001271
    ## 50 32 16.76 0.001158
    ## 51 33 16.78 0.001055
    ## 52 34 16.79 0.000961
    ## 53 34 16.80 0.000876
    ## 54 34 16.82 0.000798
    ## 55 34 16.82 0.000727
    ## 56 34 16.83 0.000663
    ## 57 34 16.84 0.000604
    ## 58 34 16.84 0.000550
    ## 59 34 16.85 0.000501
    ## 60 35 16.85 0.000457
    ## 61 35 16.85 0.000416
    ## 62 35 16.86 0.000379
    ## 63 35 16.86 0.000346
    ## 64 35 16.86 0.000315
    ## 65 35 16.86 0.000287
    ## 66 35 16.86 0.000261
    ## 67 35 16.86 0.000238
    ## 68 35 16.86 0.000217
    ## 69 35 16.87 0.000198
    ## 70 35 16.87 0.000180
    ## 71 35 16.87 0.000164
    ## 72 35 16.87 0.000150
    ## 73 35 16.87 0.000136
    ## 74 35 16.87 0.000124
    ## 75 35 16.87 0.000113
    ## 76 35 16.87 0.000103
    ## 77 35 16.87 0.000094
    ## 78 35 16.87 0.000086

``` r
cvmodel1_elasticnet = cv.glmnet(data.matrix(sobj$data[,-1]),data.matrix(sobj$data$NOTCOV)) 
cvmodel1_elasticnet$lambda.min
```

    ## [1] 0.0005501301

``` r
log(cvmodel1_elasticnet$lambda.min)
```

    ## [1] -7.505356

``` r
coef(cvmodel1_elasticnet, s = "lambda.min")
```

    ## 36 x 1 sparse Matrix of class "dgCMatrix"
    ##                                  1
    ## (Intercept)           -0.032879231
    ## Age                   -0.030550760
    ## female                 0.017751813
    ## AfAm                   0.043120226
    ## Asian                  0.023256750
    ## RaceOther             -0.002618294
    ## Hispanic              -0.041600041
    ## educ_hs                0.036647861
    ## educ_smcoll            0.105393573
    ## educ_as                0.105152922
    ## educ_bach              0.187166429
    ## educ_adv               0.193490882
    ## married                0.121093310
    ## widowed                .          
    ## divorc_sep             0.061968428
    ## Region.Midwest        -0.015902123
    ## Region.South          -0.078643866
    ## Region.West           -0.018979581
    ## born.Mex.CentAm.Carib -0.212929635
    ## born.S.Am             -0.087710860
    ## born.Eur              -0.031489040
    ## born.f.USSR           -0.114614922
    ## born.Africa           -0.006089186
    ## born.MidE             -0.153240591
    ## born.India.subc       -0.045413543
    ## born.Asia             -0.086277505
    ## born.SE.Asia          -0.061771922
    ## born.elsewhere        -0.069974284
    ## born.unknown           0.007089628
    ## sptn.under_500         0.043585882
    ## sptn.500_1999          0.055672472
    ## sptn.2000_2999         0.043250933
    ## sptn.3000_4999         0.066864276
    ## sptn.5000_gr           0.030442027
    ## sptn.refused           0.021368051
    ## sptn.Is_NA             0.050835008

``` r
pred1_elasnet <- predict(model1_elasticnet, newx = data.matrix(s_dat_test), s = cvmodel1_elasticnet$lambda.min)
pred_model1_elasnet <- (pred1_elasnet < mean(pred1_elasnet)) 
pred_en<-table(pred = pred_model1_elasnet, true = dat_test$NOTCOV)
pred_en
```

    ##        true
    ## pred        0     1
    ##   FALSE 17695  1708
    ##   TRUE  10108  4707

``` r
(pred_en[1,1]+pred_en[2,2])/sum(pred_en)
```

    ## [1] 0.6546847

``` r
model2_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV, alpha = 0) 
# or try different alpha values to see if you can improve
```

We removed some of the explanatory variables and ran the random forest
regression again to see if there is a stronger correlation with ceratin
explanatory variables.

``` r
require("standardize")
set.seed(654321)
NNN <- length(dat_for_analysis_sub$NOTCOV)
restrict_2 <- (runif(NNN) < 0.15)
summary(restrict_2)
```

    ##    Mode   FALSE    TRUE 
    ## logical   34218    6032

``` r
dat_train2 <- subset(dat_for_analysis_sub, restrict_2)
dat_test2 <- subset(dat_for_analysis_sub, !restrict_2)
sobj2 <- standardize(NOTCOV ~ Age + female + Region.Midwest + Region.South + Region.West + born.Mex.CentAm.Carib + born.S.Am + born.Eur + born.f.USSR + born.Africa + born.MidE + born.India.subc + born.Asia + born.SE.Asia + born.elsewhere + born.unknown, dat_train, family = binomial)
s_dat_test2 <- predict(sobj2, dat_test2)
```

``` r
require('randomForest')
set.seed(54321)
model_randFor2 <- randomForest(as.factor(NOTCOV) ~ ., data = sobj2$data, importance=TRUE, proximity=TRUE)
print(model_randFor2)
```

    ## 
    ## Call:
    ##  randomForest(formula = as.factor(NOTCOV) ~ ., data = sobj2$data,      importance = TRUE, proximity = TRUE) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 4
    ## 
    ##         OOB estimate of  error rate: 17.61%
    ## Confusion matrix:
    ##      0   1 class.error
    ## 0 4851  95  0.01920744
    ## 1  967 119  0.89042357

``` r
round(importance(model_randFor2),2)
```

    ##                           0     1 MeanDecreaseAccuracy MeanDecreaseGini
    ## Age                   18.76 20.69                24.60            63.81
    ## female                 6.39 -0.41                 5.04             9.67
    ## Region.Midwest        -7.39  5.31                -2.45             5.70
    ## Region.South           2.02 16.20                16.31            12.37
    ## Region.West           -4.98  7.11                 1.94             6.49
    ## born.Mex.CentAm.Carib 36.55 41.98                42.20           146.18
    ## born.S.Am              7.63 15.70                14.38             7.34
    ## born.Eur              -3.52  4.16                -1.19             3.47
    ## born.f.USSR           -1.75  1.70                -1.04             1.62
    ## born.Africa           -5.14  2.61                -3.72             2.58
    ## born.MidE             -5.91 -1.89                -6.12             3.40
    ## born.India.subc       -1.62  3.31                 0.15             4.04
    ## born.Asia             -4.09 -2.35                -4.73             3.54
    ## born.SE.Asia          -2.19  4.49                 0.03             4.30
    ## born.elsewhere        -5.92  9.17                -0.42             3.05
    ## born.unknown          -0.63 -3.55                -1.75             2.74

``` r
varImpPlot(model_randFor2)
```

![](HW_7_pub_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
pred_model2 <- predict(model_randFor2, s_dat_test2)
pred_rf<-table(pred = pred_model2, true = dat_test2$NOTCOV)
print(pred_rf)
```

    ##     true
    ## pred     0     1
    ##    0 27299  5634
    ##    1   504   781

``` r
(pred_rf[1,1]+pred_rf[2,2])/sum(pred_rf)
```

    ## [1] 0.8206207

We tried to understand whether a person has health insurance through the
lens of age, being a female, race, education martial status, region born
and how much is spent on medical insurance. Our subset only includes
those with good health and beyond.We previously used the technique of
linear probability model with logit, which is able to successfully
predict whether or not a person is covered with about 82 percent
accuracy. We analyze these variables through the lens of Random Forest
which is also able to successfully predict whether or not a person is
covered with about 82 percent accuracy. With an average of 82 percent
accuracy for the previous models we also see an average truly false
ratio to false negative ratio of 5:1 and a positive to false positive
ratio of .5:1 .With Support Vector machine the accuracy is drops to
about 80 percent . Lastly in the Elastic Net model we see a larger drop
in accuracy to only about 65 percent. However we see a decrease in the
number of both false and true positives we see a 10:1 ratio of truly
false to false negative and about 2:1 ratio of positives to false
positives. The explanatory variable that stood out the most out to us
was the where you were born, specifically being Hispanic born in
Mexico/Central or America/Caribbean were strong predictors of this.
