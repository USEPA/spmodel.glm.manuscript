

``` r
#=========================================================#
# Load Software ####
#=========================================================#

library(spmodel)
library(ggplot2, verbose = FALSE)
library(dplyr, verbose = FALSE)
library(emmeans, verbose = FALSE)
library(car)

#=========================================================#
# Section 3: Modeling moose presence in Alaska, USA ####
#=========================================================#

head(moose)
```

```
## Simple feature collection with 6 features and 4 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 281896.4 ymin: 1518398 xmax: 311325.3 ymax: 1541016
## Projected CRS: NAD83 / Alaska Albers
## # A tibble: 6 × 5
##    elev strat count presence           geometry
##   <dbl> <chr> <dbl> <fct>           <POINT [m]>
## 1  469. L         0 0        (293542.6 1541016)
## 2  362. L         0 0        (298313.1 1533972)
## 3  173. M         0 0        (281896.4 1532516)
## 4  280. L         0 0        (298651.3 1530264)
## 5  620. L         0 0        (311325.3 1527705)
## 6  164. M         0 0        (291421.5 1518398)
```

``` r
head(moose_preds)
```

```
## Simple feature collection with 6 features and 2 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 291839.8 ymin: 1436192 xmax: 401239.6 ymax: 1512103
## Projected CRS: NAD83 / Alaska Albers
## # A tibble: 6 × 3
##    elev strat           geometry
##   <dbl> <chr>        <POINT [m]>
## 1  143. L     (401239.6 1436192)
## 2  324. L     (352640.6 1490695)
## 3  158. L     (360954.9 1491590)
## 4  221. M     (291839.8 1466091)
## 5  209. M     (310991.9 1441630)
## 6  218. L     (304473.8 1512103)
```

``` r
#==========================================#
# Section 3.1: Model Fitting ####
#==========================================#

spbin <- spglm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose,
  spcov_type = "exponential"
)

summary(spbin)
```

```
## 
## Call:
## spglm(formula = presence ~ elev + strat, family = binomial, data = moose, 
##     spcov_type = "exponential")
## 
## Deviance Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.7535 -0.8005  0.3484  0.7893  1.5797 
## 
## Coefficients (fixed):
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -2.465713   1.486212  -1.659 0.097104 .  
## elev         0.006036   0.003525   1.712 0.086861 .  
## stratM       1.439273   0.420591   3.422 0.000622 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Pseudo R-squared: 0.06275
## 
## Coefficients (exponential spatial covariance):
##        de        ie     range 
## 5.145e+00 1.294e-03 4.199e+04 
## 
## Coefficients (Dispersion for binomial family):
## dispersion 
##          1
```

``` r
tidy(spbin, conf.int = TRUE)
```

```
## # A tibble: 3 × 7
##   term        estimate std.error statistic  p.value  conf.low conf.high
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>     <dbl>     <dbl>
## 1 (Intercept) -2.47      1.49        -1.66 0.0971   -5.38        0.447 
## 2 elev         0.00604   0.00353      1.71 0.0869   -0.000873    0.0129
## 3 stratM       1.44      0.421        3.42 0.000622  0.615       2.26
```

``` r
#==========================================#
# Section 3.2: Model Comparison ####
#==========================================#

bin <- spglm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose,
  spcov_type = "none"
)

bin_glm <- glm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose,
)
round(coef(bin), digits = 4)
```

```
## (Intercept)        elev      stratM 
##     -0.4247     -0.0003      0.8070
```

``` r
round(coef(bin_glm), digits = 4)
```

```
## (Intercept)        elev      stratM 
##     -0.4247     -0.0003      0.8070
```

``` r
round(sqrt(diag(vcov(bin))), digits = 4)
```

```
## (Intercept)        elev      stratM 
##      0.4208      0.0019      0.2906
```

``` r
round(sqrt(diag(vcov(bin_glm))), digits = 4)
```

```
## (Intercept)        elev      stratM 
##      0.4208      0.0019      0.2906
```

``` r
glance(spbin)
```

```
## # A tibble: 1 × 10
##       n     p  npar value   AIC  AICc   BIC logLik deviance pseudo.r.squared
##   <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>            <dbl>
## 1   218     3     3  676.  682.  683.  693.  -338.     176.           0.0627
```

``` r
glance(bin)
```

```
## # A tibble: 1 × 10
##       n     p  npar value   AIC  AICc   BIC logLik deviance pseudo.r.squared
##   <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>            <dbl>
## 1   218     3     0  708.  708.  708.  708.  -354.     294.           0.0280
```

``` r
anova(spbin, bin)
```

```
## Likelihood Ratio Test
## 
## Response: presence
##              Df   Chi2 Pr(>Chi2)    
## spbin vs bin  3 31.546 6.525e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
loocv(spbin)
```

```
## # A tibble: 1 × 3
##        bias  MSPE RMSPE
##       <dbl> <dbl> <dbl>
## 1 0.0000206 0.156 0.394
```

``` r
loocv(bin)
```

```
## # A tibble: 1 × 3
##       bias  MSPE RMSPE
##      <dbl> <dbl> <dbl>
## 1 -1.23e-9 0.240 0.490
```

``` r
spbin2 <- update(spbin, spcov_type = "gaussian")
glances(spbin, spbin2)
```

```
## # A tibble: 2 × 11
##   model      n     p  npar value   AIC  AICc   BIC logLik deviance pseudo.r.squared
##   <chr>  <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>            <dbl>
## 1 spbin2   218     3     3  674.  680.  680.  690.  -337.     198.           0.0698
## 2 spbin    218     3     3  676.  682.  683.  693.  -338.     176.           0.0627
```

``` r
loocv(spbin)
```

```
## # A tibble: 1 × 3
##        bias  MSPE RMSPE
##       <dbl> <dbl> <dbl>
## 1 0.0000206 0.156 0.394
```

``` r
loocv(spbin2)
```

```
## # A tibble: 1 × 3
##        bias  MSPE RMSPE
##       <dbl> <dbl> <dbl>
## 1 -0.000261 0.146 0.382
```

``` r
#==========================================#
# Section 3.3: Model Diagnostics ####
#==========================================#

augment(spbin)
```

```
## Simple feature collection with 218 features and 8 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 269085 ymin: 1416151 xmax: 419057.4 ymax: 1541016
## Projected CRS: NAD83 / Alaska Albers
## # A tibble: 218 × 9
##    presence  elev strat .fitted .resid    .hat  .cooksd .std.resid           geometry
##  * <fct>    <dbl> <chr>   <dbl>  <dbl>   <dbl>    <dbl>      <dbl>        <POINT [m]>
##  1 0         469. L       -1.95 -0.516 0.0476  0.00465      -0.528 (293542.6 1541016)
##  2 0         362. L       -2.70 -0.361 0.0123  0.000548     -0.363 (298313.1 1533972)
##  3 0         173. M       -1.96 -0.514 0.00455 0.000405     -0.516 (281896.4 1532516)
##  4 0         280. L       -3.15 -0.290 0.00413 0.000117     -0.291 (298651.3 1530264)
##  5 0         620. L       -1.19 -0.728 0.168   0.0427       -0.798 (311325.3 1527705)
##  6 0         164. M       -1.71 -0.576 0.00534 0.000598     -0.578 (291421.5 1518398)
##  7 0         164. M       -1.60 -0.606 0.00576 0.000714     -0.608 (287298.3 1518035)
##  8 0         186. L       -2.50 -0.397 0.00439 0.000233     -0.398 (279050.9 1517324)
##  9 0         362. L       -1.88 -0.532 0.0239  0.00237      -0.539 (346145.9 1512479)
## 10 0         430. L       -1.54 -0.623 0.0497  0.00713      -0.639 (321354.6 1509966)
## # ℹ 208 more rows
```

``` r
varcomp(spbin)
```

```
## # A tibble: 3 × 2
##   varcomp            proportion
##   <chr>                   <dbl>
## 1 Covariates (PR-sq)   0.0627  
## 2 de                   0.937   
## 3 ie                   0.000236
```

``` r
#==========================================#
# Section 3.4: Model Diagnostics ####
#==========================================#

predict(spbin, newdata = moose_preds)[1:5]
```

```
##           1           2           3           4           5 
##  0.06664165 -0.79069107 -1.60387940 -0.83159357  1.38183928
```

``` r
predict(spbin, newdata = moose_preds, type = "response")[1:5]
```

```
##         1         2         3         4         5 
## 0.5166542 0.3120203 0.1674401 0.3033082 0.7992862
```

``` r
predict(spbin, newdata = moose_preds, interval = "prediction")[1:5, ]
```

```
##           fit        lwr       upr
## 1  0.06664165 -2.0374370 2.1707203
## 2 -0.79069107 -3.4758514 1.8944692
## 3 -1.60387940 -4.0953329 0.8875741
## 4 -0.83159357 -3.0704818 1.4072947
## 5  1.38183928 -0.7692107 3.5328893
```

``` r
augment(spbin, newdata = moose_preds, interval = "prediction")
```

```
## Simple feature collection with 100 features and 5 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 269386.2 ymin: 1418453 xmax: 419976.2 ymax: 1541763
## Projected CRS: NAD83 / Alaska Albers
## # A tibble: 100 × 6
##     elev strat .fitted .lower  .upper           geometry
##  * <dbl> <chr>   <dbl>  <dbl>   <dbl>        <POINT [m]>
##  1  143. L      0.0666 -2.04   2.17   (401239.6 1436192)
##  2  324. L     -0.791  -3.48   1.89   (352640.6 1490695)
##  3  158. L     -1.60   -4.10   0.888  (360954.9 1491590)
##  4  221. M     -0.832  -3.07   1.41   (291839.8 1466091)
##  5  209. M      1.38   -0.769  3.53   (310991.9 1441630)
##  6  218. L     -2.59   -5.20   0.0177 (304473.8 1512103)
##  7  127. L     -2.73   -5.24  -0.220  (339011.1 1459318)
##  8  122. L     -2.32   -4.74   0.0920 (342827.3 1463452)
##  9  191  L     -1.17   -4.01   1.66   (284453.8 1502837)
## 10  105. L     -0.905  -3.05   1.24   (391343.9 1483791)
## # ℹ 90 more rows
```

``` r
#=========================================================#
# Section 4: Additional Applications ####
#=========================================================#

#==========================================#
# Section 4.1: Modeling moose counts in Alaska, USA ####
#==========================================#

sppois <- spglm(
  formula = count ~ elev + strat,
  family = poisson,
  data = moose,
  spcov_type = "spherical"
)
spnb <- update(sppois, family = nbinomial)

BIC(sppois, spnb)
```

```
##        df      BIC
## sppois  3 1344.574
## spnb    4 1343.105
```

``` r
sppois_anis <- update(sppois, anisotropy = TRUE)
spnb_anis <- update(spnb, anisotropy = TRUE)

BIC(sppois, spnb, sppois_anis, spnb_anis)
```

```
##             df      BIC
## sppois       3 1344.574
## spnb         4 1343.105
## sppois_anis  5 1341.143
## spnb_anis    6 1339.714
```

``` r
#==========================================#
# Section 4.2: Modeling lake conductivity in Southwest, USA ####
#==========================================#

spgam <- spglm(
  formula = exp(log_cond) ~ temp * state + origin,
  family = "Gamma",
  data = lake,
  spcov_type = "cauchy",
  partition_factor = ~ year
)

summary(spgam)
```

```
## 
## Call:
## spglm(formula = exp(log_cond) ~ temp * state + origin, family = "Gamma", 
##     data = lake, spcov_type = "cauchy", partition_factor = ~year)
## 
## Deviance Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.35762 -0.20796 -0.03706  0.17869  1.10616 
## 
## Coefficients (fixed):
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    3.59325    0.50058   7.178 7.06e-13 ***
## temp           0.15182    0.03006   5.051 4.39e-07 ***
## stateCO       -0.03214    0.56098  -0.057  0.95432    
## stateNV        0.75664    0.66851   1.132  0.25771    
## stateUT       -0.19696    0.55916  -0.352  0.72466    
## originNATURAL  0.08313    0.21988   0.378  0.70538    
## temp:stateCO   0.13679    0.04808   2.845  0.00444 ** 
## temp:stateNV   0.01882    0.05820   0.323  0.74645    
## temp:stateUT   0.20015    0.04846   4.131 3.62e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Pseudo R-squared: 0.7061
## 
## Coefficients (cauchy spatial covariance):
##        de        ie     range     extra 
## 2.069e-02 2.952e-01 4.119e+06 5.645e-01 
## 
## Coefficients (Dispersion for Gamma family):
## dispersion 
##      3.761
```

``` r
anova(spgam)
```

```
## Analysis of Variance Table
## 
## Response: exp(log_cond)
##             Df    Chi2 Pr(>Chi2)    
## (Intercept)  1 51.5270 7.062e-13 ***
## temp         1 25.5146 4.390e-07 ***
## state        3  3.0747 0.3802528    
## origin       1  0.1429 0.7053819    
## temp:state   3 19.7668 0.0001897 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
vif(spgam)
```

```
##                  GVIF Df GVIF^(1/(2*Df))
## temp         4.691914  1        2.166083
## state      127.082397  3        2.242234
## origin       1.264940  1        1.124695
## temp:state  76.387383  3        2.059856
```

``` r
pairs(emmeans(spgam, ~ state | temp))
```

```
## temp = 7.63:
##  contrast estimate    SE  df z.ratio p.value
##  AZ - CO    -1.012 0.337 Inf  -3.004  0.0142
##  AZ - NV    -0.900 0.348 Inf  -2.584  0.0480
##  AZ - UT    -1.331 0.326 Inf  -4.082  0.0003
##  CO - NV     0.112 0.258 Inf   0.434  0.9727
##  CO - UT    -0.319 0.223 Inf  -1.427  0.4822
##  NV - UT    -0.431 0.244 Inf  -1.763  0.2915
## 
## Results are averaged over the levels of: origin 
## Degrees-of-freedom method: asymptotic 
## Results are given on the log (not the response) scale. 
## P value adjustment: tukey method for comparing a family of 4 estimates
```

``` r
emtrends(spgam, ~ state, var = "temp")
```

```
##  state temp.trend     SE  df asymp.LCL asymp.UCL
##  AZ         0.152 0.0301 Inf    0.0929     0.211
##  CO         0.289 0.0370 Inf    0.2161     0.361
##  NV         0.171 0.0504 Inf    0.0718     0.270
##  UT         0.352 0.0372 Inf    0.2791     0.425
## 
## Results are averaged over the levels of: origin 
## Degrees-of-freedom method: asymptotic 
## Results are given on the exp (not the response) scale. 
## Confidence level used: 0.95
```

``` r
#==========================================#
# Section 4.3: Modeling harbor seal trends in Alaska, USA ####
#==========================================#

is_decreasing <- seal$log_trend < 0
spbin <- spgautor(
  formula = is_decreasing ~ 1,
  family = binomial,
  data = seal,
  spcov_type = "car",
  random = ~ stock
)

tidy(spbin, conf.int = TRUE)
```

```
## # A tibble: 1 × 7
##   term        estimate std.error statistic p.value conf.low conf.high
##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
## 1 (Intercept)    0.340     0.673     0.506   0.613   -0.979      1.66
```

``` r
emmeans(spbin, ~ 1, type = "response")
```

```
##  1        prob    SE  df asymp.LCL asymp.UCL
##  overall 0.584 0.164 Inf     0.273      0.84
## 
## Degrees-of-freedom method: asymptotic 
## Confidence level used: 0.95 
## Intervals are back-transformed from the logit scale
```

``` r
seal
```

```
## Simple feature collection with 149 features and 4 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: 913618.8 ymin: 855730.2 xmax: 1221859 ymax: 1145054
## Projected CRS: NAD83 / Alaska Albers
## # A tibble: 149 × 5
##    log_trend stock                                                                                     geometry    trend sampled
##  *     <dbl> <fct>                                                                                <POLYGON [m]>    <dbl> <fct>  
##  1  NA       8     ((1035002 1054710, 1035002 1054542, 1035002 1053542, 1035002 1052542, 1035002 1051624, 10... NA       no     
##  2  -0.282   8     ((1037002 1039492, 1037006 1039490, 1037017 1039492, 1037035 1039496, 1037045 1039499, 10... -0.282   yes    
##  3  -0.00121 8     ((1070158 1030216, 1070185 1030207, 1070187 1030207, 1070211 1030207, 1070275 1030205, 10... -0.00121 yes    
##  4   0.0354  8     ((1054906 1034826, 1054931 1034821, 1054936 1034822, 1055001 1034828, 1055002 1034828, 10...  0.0354  yes    
##  5  -0.0160  8     ((1025142 1056940, 1025184 1056889, 1025222 1056836, 1025256 1056780, 1025275 1056747, 10... -0.0160  yes    
##  6   0.0872  8     ((1026035 1044623, 1026037 1044605, 1026072 1044610, 1026083 1044612, 1026112 1044616, 10...  0.0872  yes    
##  7  -0.266   8     ((1100345 1060709, 1100287 1060706, 1100228 1060706, 1100170 1060711, 1100112 1060718, 11... -0.266   yes    
##  8   0.0743  8     ((1030247 1029637, 1030248 1029637, 1030265 1029642, 1030328 1029656, 1030393 1029667, 10...  0.0743  yes    
##  9  NA       8     ((1043093 1020553, 1043097 1020550, 1043101 1020550, 1043166 1020557, 1043231 1020559, 10... NA       no     
## 10  -0.00961 8     ((1116002 1024542, 1116002 1023542, 1116002 1022542, 1116002 1021554, 1115988 1021564, 11... -0.00961 yes    
## # ℹ 139 more rows
```

``` r
predict(spbin, type = "response", interval = "prediction")[1:5, ]
```

```
##          fit       lwr       upr
## 1  0.6807677 0.3863736 0.8783808
## 9  0.5945680 0.2467634 0.8678078
## 13 0.6189055 0.2974432 0.8616799
## 15 0.6040102 0.2921802 0.8493132
## 18 0.6375700 0.3356282 0.8596641
```

``` r
#==========================================#
# Section 4.4: Modeling voter turnout in Texas, USA ####
#==========================================#

spbeta_geo <- spglm(
  formula = turnout ~ log_income,
  family = "beta",
  data = texas,
  spcov_type = "matern"
)

spbeta_auto <- spgautor(
  formula = turnout ~ log_income,
  family = "beta",
  data = texas,
  spcov_type = "car",
  cutoff = 1e5
)

AIC(spbeta_geo, spbeta_auto)
```

```
##             df       AIC
## spbeta_geo   5 -44.53113
## spbeta_auto  3 -22.46104
```

``` r
spbeta_full_ml <- update(spbeta_geo, estmethod = "ml")
spbeta_red_ml <- update(spbeta_geo, estmethod = "ml", formula = turnout ~ 1)
anova(spbeta_full_ml, spbeta_red_ml)
```

```
## Likelihood Ratio Test
## 
## Response: turnout
##                                 Df   Chi2 Pr(>Chi2)    
## spbeta_red_ml vs spbeta_full_ml  1 23.155 1.494e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
AIC(spbeta_full_ml, spbeta_red_ml)
```

```
##                df       AIC
## spbeta_full_ml  7 -31.25900
## spbeta_red_ml   6 -10.10354
```

``` r
#=========================================================#
# Figures ####
#=========================================================#

library(patchwork)
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#==========================================#
# Figure 1 ####
#==========================================#

h <- seq(0, 1, length.out = 1000)
range1 <- 0.2
dat1 <- data.frame(Distance = h, Correlation = exp(-h/range1), Range = "0.2")
range2 <- 0.5
dat2 <- data.frame(Distance = h, Correlation = exp(-h/range2), Range = "0.5")
range3 <- 0.8
dat3 <- data.frame(Distance = h, Correlation = exp(-h/range3), Range = "0.8")
dat <- bind_rows(dat1, dat2, dat3)
dat$Range <- factor(dat$Range, levels = c("0.8", "0.5", "0.2"))
figure1 <- ggplot(dat, aes(x = Distance, y = Correlation, color = Range, linetype = Range)) +
  geom_line(linewidth = 1.2) +
  scale_color_viridis_d() +
  theme_bw(base_size = 14)
figure1
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

``` r
#==========================================#
# Figure 2 ####
#==========================================#

h <- seq(0, 1, length.out = 1000)
range <- 0.5
dat1 <- data.frame(Distance = h, Correlation = exp(-h/range), Type = "Exponential")
dat2 <- data.frame(Distance = h, Correlation = exp(-(h/range)^2), Type = "Gaussian")
dat3 <- data.frame(Distance = h, Correlation = (1 - 1.5 * h/range + 0.5 * (h/range)^3) * (h <= range), Type = "Spherical")
dat <- bind_rows(dat1, dat2, dat3)
figure2 <- ggplot(dat, aes(x = Distance, y = Correlation, color = Type, linetype = Type)) +
  geom_line(linewidth = 1.2) +
  scale_color_viridis_d() +
  theme_bw(base_size = 14)
figure2
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

``` r
#==========================================#
# Figure 3 ####
#==========================================#

moose_comb <- bind_rows(moose |> mutate(samp = "yes"), moose_preds |> mutate(samp = "no"))
figure3 <- ggplot(moose_comb, aes(color = presence, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_manual(name = "moose", values = okabe[1:2], breaks = c(1, 0), labels = c("yes", "no")) +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_bw(base_size = 16) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  ) +
  geom_sf(size = 2, data = moose_comb %>% filter(samp == "no"), color = "red", shape = 17)
figure3
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

``` r
#==========================================#
# Figure 4 ####
#==========================================#

spbin <- spglm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose,
  spcov_type = "exponential"
)
spbin_aug <- augment(spbin)

p1 <- ggplot(spbin_aug, aes(color = .hat)) +
  geom_sf(size = 1) +
  scale_color_viridis_c(option = "E", name = ".hat") +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p2 <- ggplot(spbin_aug, aes(color = .std.resid)) +
  geom_sf(size = 1) +
  scale_color_viridis_c(option = "A", limits = c(-2, 2)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p1 + p2
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

``` r
#==========================================#
# Figure 5 ####
#==========================================#


plot(spbin, which = 4)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png)

``` r
plot(spbin, which = 7)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-6.png)

``` r
#==========================================#
# Figure 6 ####
#==========================================#

preds_aug <- augment(
  spbin,
  newdata = moose_preds,
  type.predict = "response",
  interval = "prediction"
)
spbin_aug <- augment(spbin, type.predict = "response", interval = "prediction")
comb_aug <- bind_rows(spbin_aug %>% mutate(Type = "Fitted"), preds_aug %>% mutate(Type = "Pred"))

figure6 <- ggplot(comb_aug, aes(color = .fitted, shape = Type)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "H", name = "Prob", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw()
figure6
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-7.png)

``` r
#==========================================#
# Figure 7 ####
#==========================================#

p1 <- ggplot(preds_aug, aes(color = .lower)) +
  geom_sf(pch = 17) +
  scale_color_viridis_c(option = "H", name = "Lower", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p2 <- ggplot(preds_aug, aes(color = .upper)) +
  geom_sf(pch = 17) +
  scale_color_viridis_c(option = "H", name = "Upper", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p1 + p2
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-8.png)

``` r
#==========================================#
# Figure 8 ####
#==========================================#

figure8 <- ggplot(moose_comb, aes(color = count, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_binned(breaks = c(0, 1, 2, 5, 10, 20, 40), type = "viridis") +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_bw(base_size = 14) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  geom_sf(size = 2, data = moose_comb %>% filter(samp == "no"), color = "red", shape = 17)
figure8
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-9.png)

``` r
#==========================================#
# Figure 9 ####
#==========================================#

spnbin <- spglm(
  formula = count ~ elev + strat,
  family = nbinomial,
  data = moose,
  spcov_type = "spherical",
  anisotropy = TRUE
)

spnbin_iso <- update(spnbin, anisotropy = FALSE)


plot(spnbin_iso, which = 8)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-10.png)

``` r
plot(spnbin, which = 8)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-11.png)

``` r
#==========================================#
# Figure 10 ####
#==========================================#

seal$trend <- seal$log_trend
seal$sampled <- factor(if_else(is.na(seal$trend), "no", "yes"), levels = c("yes", "no"))
p1 <- ggplot(seal, aes(fill = sampled, color = stock)) +
  geom_sf(size = 2) +
  theme_bw(base_size = 14) +
  scale_fill_manual(name = "sampled", values = okabe[1:2], breaks = c("yes", "no"), labels = c("yes", "no")) +
  scale_color_manual(name = "stock", values = c("grey40", "grey80"), breaks = c(8, 10), labels = c(8, 10)) +
  scale_y_continuous(breaks = seq(57, 59, length.out = 5)) +
  scale_x_continuous(breaks = seq(-139, -134, length.out = 3))

p2 <- ggplot(seal, aes(fill = trend)) +
  geom_sf(size = 2) +
  theme_bw(base_size = 14) +
  scale_fill_viridis_c(option = "D", begin = 0.5, limits = c(min(seal$trend), max(seal$trend))) +
  scale_y_continuous(breaks = seq(57, 59, length.out = 5)) +
  scale_x_continuous(breaks = seq(-139, -134, length.out = 3))

p1 + p2
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-12.png)

``` r
#==========================================#
# Figure 11 ####
#==========================================#

texas$samp <- factor(if_else(is.na(texas$turnout), "no", "yes"), levels = c("yes", "no"))
figure11 <- ggplot(texas, aes(color = turnout, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "D") +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = seq(-94, -106, length.out = 3)) +
  geom_sf(size = 2, data = texas %>% filter(samp == "no"), color = "red", shape = 17)
figure11
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-13.png)

``` r
#=========================================================#
# Session Information ####
#=========================================================#

sessionInfo()
```

```
## R version 4.4.1 (2024-06-14 ucrt)
## Platform: x86_64-w64-mingw32/x64
## Running under: Windows 11 x64 (build 22631)
## 
## Matrix products: default
## 
## 
## locale:
## [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.utf8    
## 
## time zone: America/Los_Angeles
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] patchwork_1.2.0 car_3.1-3       carData_3.0-5   emmeans_1.10.3  spmodel_0.11.0  here_1.0.1      lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4    
## [11] purrr_1.0.2     readr_2.1.5     tidyr_1.3.1     tibble_3.2.1    ggplot2_3.5.1   tidyverse_2.0.0
## 
## loaded via a namespace (and not attached):
##  [1] gtable_0.3.5       xfun_0.52          lattice_0.22-6     tzdb_0.4.0         vctrs_0.6.5        tools_4.4.1        generics_0.1.4     parallel_4.4.1    
##  [9] proxy_0.4-27       highr_0.11         pkgconfig_2.0.3    Matrix_1.7-0       KernSmooth_2.23-24 lifecycle_1.0.4    compiler_4.4.1     farver_2.1.2      
## [17] textshaping_0.4.0  munsell_0.5.1      litedown_0.7       class_7.3-22       Formula_1.2-5      pillar_1.11.0      classInt_0.4-11    abind_1.4-5       
## [25] mime_0.12          commonmark_1.9.5   tidyselect_1.2.1   mvtnorm_1.2-5      stringi_1.8.4      sf_1.0-21          labeling_0.4.3     rprojroot_2.0.4   
## [33] grid_4.4.1         colorspace_2.1-0   cli_3.6.3          magrittr_2.0.3     utf8_1.2.4         e1071_1.7-16       withr_3.0.2        scales_1.3.0      
## [41] timechange_0.3.0   estimability_1.5.1 ragg_1.3.2         hms_1.1.3          coda_0.19-4.1      evaluate_0.24.0    knitr_1.48         viridisLite_0.4.2 
## [49] markdown_2.0       rlang_1.1.4        Rcpp_1.0.12        xtable_1.8-4       glue_1.7.0         DBI_1.2.3          rstudioapi_0.16.0  R6_2.5.1          
## [57] systemfonts_1.1.0  units_0.8-7
```

``` r
# R code spun into .md and .html using knitr::spin("code.R")
```

