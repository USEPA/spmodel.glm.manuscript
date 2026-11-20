

``` r
#=========================================================#
# Section 3: Modeling moose presence in Alaska, USA ####
#=========================================================#

library("spmodel")

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
## -1.7537 -0.7985  0.3467  0.7873  1.5767 
## 
## Coefficients (fixed):
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -2.474023   1.498874  -1.651 0.098823 .  
## elev         0.006062   0.003540   1.712 0.086862 .  
## stratM       1.441894   0.421669   3.419 0.000627 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Pseudo R-squared: 0.06276
## 
## Coefficients (exponential spatial covariance):
##        de        ie     range 
## 5.244e+00 5.244e-04 4.214e+04 
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
##   term        estimate std.error statistic  p.value conf.low conf.high
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
## 1 (Intercept) -2.47      1.50        -1.65 0.0988   -5.41e+0    0.464 
## 2 elev         0.00606   0.00354      1.71 0.0869   -8.77e-4    0.0130
## 3 stratM       1.44      0.422        3.42 0.000627  6.15e-1    2.27
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
tidy(bin)
```

```
## # A tibble: 3 × 5
##   term         estimate std.error statistic p.value
##   <chr>           <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept) -0.425      0.421      -1.01  0.313  
## 2 elev        -0.000317   0.00186    -0.171 0.865  
## 3 stratM       0.807      0.291       2.78  0.00549
```

``` r
bin_glm <- glm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose,
)
broom::tidy(bin_glm)
```

```
## # A tibble: 3 × 5
##   term         estimate std.error statistic p.value
##   <chr>           <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept) -0.425      0.421      -1.01  0.313  
## 2 elev        -0.000317   0.00186    -0.171 0.865  
## 3 stratM       0.807      0.291       2.78  0.00549
```

``` r
bin_ml <- update(bin, estmethod = "ml")
glance(bin_ml)[, c("logLik", "AIC", "BIC", "deviance")]
```

```
## # A tibble: 1 × 4
##   logLik   AIC   BIC deviance
##    <dbl> <dbl> <dbl>    <dbl>
## 1  -147.  300.  310.     294.
```

``` r
broom::glance(bin_glm)[, c("logLik", "AIC", "BIC", "deviance")]
```

```
## # A tibble: 1 × 4
##   logLik   AIC   BIC deviance
##    <dbl> <dbl> <dbl>    <dbl>
## 1  -147.  300.  310.     294.
```

``` r
glance(spbin)
```

```
## # A tibble: 1 × 10
##       n     p  npar value   AIC  AICc   BIC logLik deviance
##   <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>
## 1   218     3     3  276.  282.  282.  292.  -138.     175.
## # ℹ 1 more variable: pseudo.r.squared <dbl>
```

``` r
glance(bin)
```

```
## # A tibble: 1 × 10
##       n     p  npar value   AIC  AICc   BIC logLik deviance
##   <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>
## 1   218     3     0  307.  307.  307.  307.  -154.     294.
## # ℹ 1 more variable: pseudo.r.squared <dbl>
```

``` r
loocv(spbin)
```

```
## # A tibble: 1 × 3
##        bias  MSPE RMSPE
##       <dbl> <dbl> <dbl>
## 1 0.0000406 0.155 0.394
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
kcv(spbin)
```

```
## # A tibble: 1 × 3
##       bias  MSPE RMSPE
##      <dbl> <dbl> <dbl>
## 1 -0.00112 0.158 0.397
```

``` r
kcv(bin)
```

```
## # A tibble: 1 × 3
##           bias  MSPE RMSPE
##          <dbl> <dbl> <dbl>
## 1 0.0000000422 0.240 0.490
```

``` r
spbin2 <- update(spbin, spcov_type = "gaussian")
glances(spbin, spbin2)
```

```
## # A tibble: 2 × 11
##   model      n     p  npar value   AIC  AICc   BIC logLik deviance
##   <chr>  <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>
## 1 spbin2   218     3     3  273.  279.  279.  289.  -136.     198.
## 2 spbin    218     3     3  276.  282.  282.  292.  -138.     175.
## # ℹ 1 more variable: pseudo.r.squared <dbl>
```

``` r
loocv(spbin)
```

```
## # A tibble: 1 × 3
##        bias  MSPE RMSPE
##       <dbl> <dbl> <dbl>
## 1 0.0000406 0.155 0.394
```

``` r
loocv(spbin2)
```

```
## # A tibble: 1 × 3
##        bias  MSPE RMSPE
##       <dbl> <dbl> <dbl>
## 1 -0.000237 0.146 0.382
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
##    presence  elev strat .fitted .resid    .hat  .cooksd .std.resid
##  * <fct>    <dbl> <chr>   <dbl>  <dbl>   <dbl>    <dbl>      <dbl>
##  1 0         469. L       -1.96 -0.513 0.0473  0.00457      -0.525
##  2 0         362. L       -2.71 -0.358 0.0122  0.000535     -0.360
##  3 0         173. M       -1.97 -0.511 0.00451 0.000397     -0.512
##  4 0         280. L       -3.16 -0.288 0.00408 0.000114     -0.289
##  5 0         620. L       -1.20 -0.725 0.167   0.0422       -0.794
##  6 0         164. M       -1.72 -0.574 0.00532 0.000591     -0.576
##  7 0         164. M       -1.61 -0.604 0.00574 0.000707     -0.606
##  8 0         186. L       -2.51 -0.396 0.00438 0.000230     -0.396
##  9 0         362. L       -1.89 -0.529 0.0238  0.00233      -0.536
## 10 0         430. L       -1.55 -0.621 0.0497  0.00706      -0.637
## # ℹ 208 more rows
## # ℹ 1 more variable: geometry <POINT [m]>
```

``` r
varcomp(spbin)
```

```
## # A tibble: 3 × 2
##   varcomp            proportion
##   <chr>                   <dbl>
## 1 Covariates (PR-sq)  0.0628   
## 2 de                  0.937    
## 3 ie                  0.0000937
```

``` r
#==========================================#
# Section 3.4: Prediction ####
#==========================================#

predict(spbin, newdata = moose_preds)[1:5]
```

```
##           1           2           3           4           5 
##  0.06989524 -0.79411725 -1.61106131 -0.83935088  1.39081663
```

``` r
predict(spbin, newdata = moose_preds, type = "response")[1:5]
```

```
##         1         2         3         4         5 
## 0.5174667 0.3112853 0.1664413 0.3016715 0.8007226
```

``` r
predict(spbin, newdata = moose_preds, interval = "prediction")[1:5, ]
```

```
##           fit        lwr       upr
## 1  0.06989524 -2.0456011 2.1853915
## 2 -0.79411725 -3.4952212 1.9069867
## 3 -1.61106131 -4.1166330 0.8945103
## 4 -0.83935088 -3.0915348 1.4128330
## 5  1.39081663 -0.7720673 3.5537005
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
##  1  143. L      0.0699 -2.05   2.19   (401239.6 1436192)
##  2  324. L     -0.794  -3.50   1.91   (352640.6 1490695)
##  3  158. L     -1.61   -4.12   0.895  (360954.9 1491590)
##  4  221. M     -0.839  -3.09   1.41   (291839.8 1466091)
##  5  209. M      1.39   -0.772  3.55   (310991.9 1441630)
##  6  218. L     -2.60   -5.22   0.0234 (304473.8 1512103)
##  7  127. L     -2.74   -5.26  -0.216  (339011.1 1459318)
##  8  122. L     -2.33   -4.77   0.0978 (342827.3 1463452)
##  9  191  L     -1.17   -4.02   1.68   (284453.8 1502837)
## 10  105. L     -0.907  -3.06   1.25   (391343.9 1483791)
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
## sppois  3 943.7197
## spnb    4 941.8881
```

``` r
sppois_anis <- update(sppois, anisotropy = TRUE)
spnb_anis <- update(spnb, anisotropy = TRUE)

BIC(sppois, spnb, sppois_anis, spnb_anis)
```

```
##             df      BIC
## sppois       3 943.7197
## spnb         4 941.8881
## sppois_anis  5 940.4199
## spnb_anis    6 938.9993
```

``` r
#==========================================#
# Section 4.2: Modeling lake conductivity in Southwest, USA ####
#==========================================#

spgam <- spglm(
  formula = cond ~ temp * state + origin,
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
## spglm(formula = cond ~ temp * state + origin, family = "Gamma", 
##     data = lake, spcov_type = "cauchy", partition_factor = ~year)
## 
## Deviance Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.25870 -0.18421 -0.03273  0.16951  1.00464 
## 
## Coefficients (fixed):
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    3.56516    0.50509   7.058 1.68e-12 ***
## temp           0.15281    0.03015   5.068 4.02e-07 ***
## stateCO       -0.01340    0.56220  -0.024  0.98098    
## stateNV        0.77487    0.66791   1.160  0.24600    
## stateUT       -0.17698    0.55945  -0.316  0.75174    
## originNATURAL  0.08565    0.22142   0.387  0.69890    
## temp:stateCO   0.13488    0.04817   2.800  0.00511 ** 
## temp:stateNV   0.01781    0.05808   0.307  0.75911    
## temp:stateUT   0.19723    0.04833   4.081 4.49e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Pseudo R-squared: 0.7093
## 
## Coefficients (cauchy spatial covariance):
##        de        ie     range     extra 
## 2.722e-02 3.218e-01 6.062e+06 2.663e-01 
## 
## Coefficients (Dispersion for Gamma family):
## dispersion 
##      4.095
```

``` r
anova(spgam)
```

```
## Analysis of Variance Table
## 
## Response: cond
##             Df    Chi2 Pr(>Chi2)    
## (Intercept)  1 49.8221 1.683e-12 ***
## temp         1 25.6843 4.021e-07 ***
## state        3  3.0926 0.3775687    
## origin       1  0.1496 0.6989043    
## temp:state   3 19.3168 0.0002351 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
car::vif(spgam)
```

```
##                  GVIF Df GVIF^(1/(2*Df))
## temp         4.725774  1        2.173885
## state      126.219690  3        2.239690
## origin       1.270228  1        1.127044
## temp:state  75.860886  3        2.057483
```

``` r
pairs(emmeans::emmeans(spgam, ~ state | temp))
```

```
## temp = 7.63:
##  contrast estimate    SE  df z.ratio p.value
##  AZ - CO    -1.016 0.337 Inf  -3.015  0.0137
##  AZ - NV    -0.911 0.348 Inf  -2.614  0.0443
##  AZ - UT    -1.329 0.327 Inf  -4.068  0.0003
##  CO - NV     0.106 0.258 Inf   0.410  0.9768
##  CO - UT    -0.312 0.224 Inf  -1.395  0.5022
##  NV - UT    -0.418 0.245 Inf  -1.705  0.3210
## 
## Results are averaged over the levels of: origin 
## Degrees-of-freedom method: asymptotic 
## Results are given on the log (not the response) scale. 
## P value adjustment: tukey method for comparing a family of 4 estimates
```

``` r
emmeans::emtrends(spgam, ~ state, var = "temp")
```

```
##  state temp.trend     SE  df asymp.LCL asymp.UCL
##  AZ         0.153 0.0302 Inf    0.0937     0.212
##  CO         0.288 0.0370 Inf    0.2151     0.360
##  NV         0.171 0.0502 Inf    0.0722     0.269
##  UT         0.350 0.0369 Inf    0.2777     0.422
## 
## Results are averaged over the levels of: origin 
## Degrees-of-freedom method: asymptotic 
## Confidence level used: 0.95
```

``` r
#==========================================#
# Section 4.3: Modeling harbor seal trends in Alaska, USA ####
#==========================================#

seal$is_decreasing <- seal$trend < 1
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
## 1 (Intercept)    0.335     0.626     0.534   0.593   -0.893      1.56
```

``` r
emmeans(spbin, ~ 1, type = "response")
```

```
##  1        prob    SE  df asymp.LCL asymp.UCL
##  overall 0.583 0.152 Inf     0.291     0.827
## 
## Degrees-of-freedom method: asymptotic 
## Confidence level used: 0.95 
## Intervals are back-transformed from the logit scale
```

``` r
seal
```

```
## Simple feature collection with 149 features and 5 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: 913618.8 ymin: 855730.2 xmax: 1221859 ymax: 1145054
## Projected CRS: NAD83 / Alaska Albers
## # A tibble: 149 × 6
##    log_trend  trend stock                       geometry is_decreasing
##  *     <dbl>  <dbl> <fct>                  <POLYGON [m]> <lgl>        
##  1  NA       NA     8     ((1035002 1054710, 1035002 10… NA           
##  2  -0.282    0.754 8     ((1037002 1039492, 1037006 10… TRUE         
##  3  -0.00121  0.999 8     ((1070158 1030216, 1070185 10… TRUE         
##  4   0.0354   1.04  8     ((1054906 1034826, 1054931 10… FALSE        
##  5  -0.0160   0.984 8     ((1025142 1056940, 1025184 10… TRUE         
##  6   0.0872   1.09  8     ((1026035 1044623, 1026037 10… FALSE        
##  7  -0.266    0.767 8     ((1100345 1060709, 1100287 10… TRUE         
##  8   0.0743   1.08  8     ((1030247 1029637, 1030248 10… FALSE        
##  9  NA       NA     8     ((1043093 1020553, 1043097 10… NA           
## 10  -0.00961  0.990 8     ((1116002 1024542, 1116002 10… TRUE         
## # ℹ 139 more rows
## # ℹ 1 more variable: sampled <fct>
```

``` r
predict(spbin, type = "response", interval = "prediction")[1:5, ]
```

```
##          fit       lwr       upr
## 1  0.6790592 0.3810180 0.8791211
## 9  0.5859764 0.2358680 0.8664793
## 13 0.6136317 0.2888352 0.8613150
## 15 0.5976789 0.2831359 0.8482017
## 18 0.6334075 0.3281146 0.8594164
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
## spbeta_geo   5 -465.8884
## spbeta_auto  3 -443.4700
```

``` r
spbeta_full_ml <- update(spbeta_geo, estmethod = "ml")
spbeta_reduced_ml <- update(spbeta_full_ml, formula = turnout ~ 1)
anova(spbeta_full_ml, spbeta_reduced_ml)
```

```
## Likelihood Ratio Test
## 
## Response: turnout
##                                     Df  Chi2 Pr(>Chi2)    
## spbeta_reduced_ml vs spbeta_full_ml  1 19.17 1.196e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
AIC(spbeta_full_ml, spbeta_reduced_ml)
```

```
##                   df       AIC
## spbeta_full_ml     7 -467.6174
## spbeta_reduced_ml  6 -450.4478
```

``` r
#=========================================================#
# Figures ####
#=========================================================#

library("ggplot2", verbose = FALSE)
library("dplyr", verbose = FALSE)
library("patchwork")

okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#==========================================#
# Figure 1a: Correlation by range ####
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
#==========================================#
# Figure 1b: Correlation types ####
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
correlations <- (figure1 | figure2) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )
correlations
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

``` r
#==========================================#
# Figure 2 ####
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

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

``` r
#==========================================#
# Figure 3 ####
#==========================================#

spbin <- spglm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose,
  spcov_type = "exponential"
)
spbin_aug <- augment(spbin)

p1 <- ggplot(spbin_aug, aes(color = .hat)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "E", name = ".hat") +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p2 <- ggplot(spbin_aug, aes(color = .std.resid)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "A", limits = c(-2, 2)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p1
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

``` r
p2
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

``` r
#==========================================#
# Figure 4 ####
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
# Figure 5 ####
#==========================================#

preds_aug <- augment(
  spbin,
  newdata = moose_preds,
  type.predict = "response",
  interval = "prediction"
)
spbin_aug <- augment(spbin, type.predict = "response", interval = "prediction")
```

```
## Warning: interval = "prediction" is ignored when newdata is not
## supplied, because a prediction interval requires a location that has
## not been observed. Supply newdata for prediction intervals, or use
## interval = "confidence" for an interval around the fitted mean at the
## observed locations.
```

``` r
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
# Figure 6 ####
#==========================================#

p1 <- ggplot(preds_aug, aes(color = .lower)) +
  geom_sf(size = 2, pch = 17) +
  scale_color_viridis_c(option = "H", name = "Lower", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p2 <- ggplot(preds_aug, aes(color = .upper)) +
  geom_sf(size = 2, pch = 17) +
  scale_color_viridis_c(option = "H", name = "Upper", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p1
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-8.png)

``` r
p2
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-9.png)

``` r
#==========================================#
# Figure 7 ####
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

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-10.png)

``` r
#==========================================#
# Figure 8 ####
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

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-11.png)

``` r
plot(spnbin, which = 8)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-12.png)

``` r
#==========================================#
# Figure 9 ####
#==========================================#

seal$sampled <- factor(if_else(is.na(seal$log_trend), "no", "yes"), levels = c("yes", "no"))
p1 <- ggplot(seal, aes(fill = sampled, color = stock)) +
  geom_sf(size = 1) +
  theme_bw(base_size = 14) +
  scale_fill_manual(name = "sampled", values = okabe[1:2], breaks = c("yes", "no"), labels = c("yes", "no")) +
  scale_color_manual(name = "stock", values = c("grey40", "grey80"), breaks = c(8, 10), labels = c(8, 10)) +
  scale_y_continuous(breaks = seq(57, 59, length.out = 5)) +
  scale_x_continuous(breaks = seq(-139, -134, length.out = 3))

p2 <- ggplot(seal, aes(fill = log_trend)) +
  geom_sf(size = 1) +
  theme_bw(base_size = 14) +
  scale_fill_viridis_c(name = "logtrend", option = "D", begin = 0.5, limits = c(min(seal$log_trend), max(seal$log_trend))) +
  scale_y_continuous(breaks = seq(57, 59, length.out = 5)) +
  scale_x_continuous(breaks = seq(-139, -134, length.out = 3))

p1
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-13.png)

``` r
p2
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-14.png)

``` r
#==========================================#
# Figure 10 ####
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

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-15.png)

``` r
#=========================================================#
# Session Information ####
#=========================================================#

sessionInfo()
```

```
## R version 4.4.1 (2024-06-14 ucrt)
## Platform: x86_64-w64-mingw32/x64
## Running under: Windows 11 x64 (build 26200)
## 
## Matrix products: default
## 
## 
## locale:
## [1] LC_COLLATE=English_United States.utf8 
## [2] LC_CTYPE=English_United States.utf8   
## [3] LC_MONETARY=English_United States.utf8
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.utf8    
## 
## time zone: America/Los_Angeles
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods  
## [7] base     
## 
## other attached packages:
##  [1] lubridate_1.9.4 forcats_1.0.1   stringr_1.5.2   purrr_1.1.0    
##  [5] readr_2.1.5     tidyr_1.3.1     tibble_3.3.0    tidyverse_2.0.0
##  [9] patchwork_1.3.2 here_1.0.2      car_3.1-3       carData_3.0-5  
## [13] emmeans_1.10.3  dplyr_1.2.0     ggplot2_4.0.2   spmodel_0.15.0 
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.2.1   viridisLite_0.4.2  farver_2.1.2      
##  [4] S7_0.2.0           fastmap_1.2.0      digest_0.6.36     
##  [7] timechange_0.3.0   estimability_1.5.1 mime_0.12         
## [10] lifecycle_1.0.5    qpdf_1.4.1         sf_1.0-21         
## [13] processx_3.8.4     magrittr_2.0.4     compiler_4.4.1    
## [16] rlang_1.1.7        tools_4.4.1        utf8_1.2.4        
## [19] yaml_2.3.10        knitr_1.50         askpass_1.2.0     
## [22] labeling_0.4.3     classInt_0.4-11    xml2_1.4.0        
## [25] RColorBrewer_1.1-3 pkgload_1.5.3      abind_1.4-5       
## [28] KernSmooth_2.23-24 withr_3.0.2        hunspell_3.0.5    
## [31] grid_4.4.1         xtable_1.8-4       e1071_1.7-16      
## [34] scales_1.4.0       tinytex_0.57       cli_3.6.3         
## [37] mvtnorm_1.2-5      rmarkdown_2.31     ragg_1.5.0        
## [40] generics_0.1.4     tzdb_0.4.0         commonmark_1.9.5  
## [43] DBI_1.2.3          proxy_0.4-27       parallel_4.4.1    
## [46] vctrs_0.7.1        Matrix_1.7-0       litedown_0.7      
## [49] callr_3.7.6        hms_1.1.3          Formula_1.2-5     
## [52] systemfonts_1.3.1  units_0.8-7        spelling_2.3.1    
## [55] pak_0.9.0          glue_1.7.0         ps_1.7.6          
## [58] stringi_1.8.4      gtable_0.3.6       pillar_1.11.1     
## [61] htmltools_0.5.8.1  R6_2.5.1           textshaping_0.4.0 
## [64] rprojroot_2.1.1    evaluate_0.24.0    lattice_0.22-6    
## [67] markdown_2.0       rbibutils_2.3      backports_1.5.0   
## [70] broom_1.0.10       renv_1.0.7         class_7.3-22      
## [73] Rcpp_1.0.12        coda_0.19-4.1      xfun_0.52         
## [76] rticles_0.27       pdftools_3.8.0     pkgconfig_2.0.3
```

``` r
# R code spun into .md and .html using knitr::spin("code.R")
```

