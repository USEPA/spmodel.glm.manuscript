







The \code{spglm()} (for point-referenced data) and \code{spgautor()} (for areal data) functions in \pkg{spmodel} fit SPGLMs using the Laplace approximation outlined in Section$~$\ref{sec-spglm}. Both \code{spglm()} and \code{spgautor()} generally require the following four arguments: `formula`, a formula that describes the relationship between the response variable and explanatory variables; `family`, the response distribution (which can be `binomial`, `poisson`, `nbinomial`, `Gamma`, `inverse.gaussian`, or `beta`); `data`, the data frame that holds the variables in `formula` as well as spatial locations; and `spcov_type`, the spatial covariance type. The first three arguments are shared by [glm]{.fct}; thus, the transition from GLMs to SPGLMs requires only one additional argument: `spcov_type`. The \code{spglm()} spatial covariance types measure dependence as a function of Euclidean distance among observations; an example is the exponential spatial covariance:
  \begin{equation}\label{eq-spcov-exp}
\boldsymbol{\Sigma} = \sigma^2_{de} \exp(-\mathbf{H}/\phi) + \sigma^2_{ie}\mathbf{I},
\end{equation}
where $\mathbf{H}$ is a matrix of pairwise distances among all observations. \code{spglm()} currently supports 18 distinct spatial covariance functions.

The \code{spgautor()} spatial covariance types measure dependence as a function of neighborhood distance among observations; an example is the simultaneous autoregressive covariance matrix:
  \begin{equation}\label{eq-spcov-sar}
\boldsymbol{\Sigma} = \sigma^2_{de} [(\mathbf{I} - \phi \mathbf{W})(\mathbf{I} - \phi \mathbf{W})^T]^{-1} + \sigma^2_{ie}\mathbf{I},
\end{equation}
where $\mathbf{W}$ is a matrix that represents the neighborhood structure among all observations. \code{spgautor()} currently supports two distinct spatial covariance functions.

In the rest of this section, we use \pkg{spmodel} to study binary, count, skewed, and proportion response variables that are either point-referenced or areal. We use \pkg{spmodel} for all parts of the data analysis, from estimation to inference to model diagnostics to prediction. We first describe core \pkg{spmodel} functionality in an application to binary data, while additional analyses highlight count, skewed, and proportion data as well as some additional \pkg{spmodel} features. Before proceeding, load \pkg{spmodel} into the current \proglang{R} session:
  ```{r, eval = FALSE}
library("spmodel")
```


## Binary data

The `moose` data in \pkg{spmodel} contain information on moose presence in Alaska. They are an `sf` object, a special data frame that is supplemented with spatial information using the [sf]{.pkg} package [@pebesma2018sf]. The first few rows look like:

  ```{r}
head(moose)
```

There are five columns: `elev`, the numeric site elevation (meters); `strat` a stratification variable for sampling with two levels, `"L"` and `"M"`, which are categorized by landscape metrics at each site; `count`, the number of moose at each site; `presence`, a factor that indicates whether at least one moose was observed at each site (`0` implies no moose; `1` implies at least one moose); and `geometry`, the NAD83 projected coordinate of each site. The `moose_preds` data in \pkg{spmodel} contain spatial locations at which predictions of moose presence are desired. They are also an `sf` object with the same projection and measurements for `elev` and `strat`. Figure$~$\ref{fig-moose-data} shows the `presence` variable in `moose` as well as the spatial locations of both `moose` and `moose_preds`. Moose are most common in the southwestern and eastern parts of the domain and least common in the northwest.

\begin{figure}
\centering
\includegraphics[width = 0.70\linewidth]{figures/figure-1.png}
\caption{Moose presence in Alaska. Circles represent moose presence or absence (based on color) and triangles represent locations at which moose presence probability predictions are desired.}
\label{fig-moose-data}
\end{figure}

To study the effect of elevation, stratum, and their interaction on moose presence while accounting for spatial correlation, we fit a SPGLM for binary data (i.e., a spatial logistic regression model) using \code{spglm()}:

  ```{r}
spbin <- spglm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose,
  spcov_type = "exponential"
)
```

Summarizing the model object yields a summary similar to that provided by the familiar [glm]{.fct}:
  ```{r}
summary(spbin)
```

The summary contains the original function call, a summary of residuals, the fixed effects coefficients table, the spatial covariance parameter estimates, and additional model information like the pseudo R-squared, which quantifies the variability in the model attributable to the fixed effects. While useful, this summary information is hard to work with, as it is printed directly to the \proglang{R} console. The [broom]{.pkg} package from the tidymodels [@kuhn2022tidy] ecosystem has functions to provide helpful model output in the form of tibbles (i.e., data frames) that are easily manipulated. \pkg{spmodel} has methods for the [tidy]{.fct}, [glance]{.fct}, and [augment]{.fct} functions from [broom]{.pkg}. The first [broom]{.pkg} function is [tidy]{.fct}, which tidies the model output:
  ```{r}
tidy(spbin)
```

The estimates and standard errors returned are on the log odds link (Table$~$\ref{table-links}) scale ([coef]{.fct} and [vcov]{.fct} may also be used). The output provides evidence that elevation is positively associated with moose presence in the `"L"` stratum ($p-$value $<0.05$) and, at zero elevation, moose are more likely in the `"M"` stratum than the `"L"` stratum ($p-$value $<0.01$). This output provides marginal evidence that the effect of elevation on moose presence varies across strata ($p-$value $\approx 0.1$).  The model effectively quantifies the impact of elevation on moose presence for moose in the `"L"` strata, but an analogous statement for moose in the `"M"` strata requires more context. We could refit the model treating `"M"` as the reference group instead of `"L"`:
  ```{r}
moose$strat2 <- factor(moose$strat, levels = c("M", "L"))
update(spbin, formula = presence ~ elev + strat2 + elev:strat2) |>
  summary()
```

A simpler solution, especially if there categorical variables with many levels, is to leverage [emmeans]{.pkg}. [emmeans]{.pkg} is an \proglang{R} package for estimating marginal means of model objects. The [emtrends]{.fct} function in [emmeans]{.pkg} characterizes the effect of a continuous variable (here, `elev`) for each level of a categorical variable (here, `strat`):

  ```{r, eval = FALSE}
library("emmeans")
```

```{r}
emtrends(spbin, "strat", "elev")
```

The asymptotic confidence intervals show that there is more evidence of an association between elevation and moose presence in the `"L"` stratum than in the `"M"` stratum. Notice that `elev.trend` for the `"L"` stratum matches the `elev` effect when `"L"` is the reference group, and similarly for `elev.trend` when `"M"` is the reference group.

The second [broom]{.pkg} function is [glance]{.fct}, which glances at the model fit:
  ```{r}
glance(spbin)
```

[glance]{.fct} returns several useful statistics like the sample size (`n`), number of fixed effects (`p`), number of covariance parameters (`npar`), several likelihood-based statistics (e.g., `AIC`, `AICc`, `BIC`), and pseudo R-squared.

\begin{figure}
\centering
\includegraphics[width = 1\linewidth]{figures/figure-2.png}
\caption{Spatial logistic regression model diagnostics from [augment]{.fct}. The leverage (i.e., hat) values (left) and standardized residuals (right).}
\label{fig-sp-diagnostic}
\end{figure}

The third [broom]{.pkg} function is [augment]{.fct}, which augments the model data with diagnostics:
  ```{R}
#| prompt: true
head(augment(spbin))
```

The [augment]{.fct} function returns the fitted $\mathbf{w}$ values (`.fitted`), deviance residuals (`.resid`), leverage (i.e., hat) values (`.hat`), Cook's distance (`.cooksd`), and standardized residuals (`.std.resid`). When the data are an `sf` object, [augment]{.fct} returns another `sf` object, helpful for visualizing model diagnostics spatially as in Figure$~$\ref{fig-sp-diagnostic}. Leverage measures the unusualness of an observation's set of explanatory variables, while Cook's distances measures how influential an observation is on the resulting model fit [@montgomery2021introduction]. Model diagnostics are also accessible as vectors using the appropriate generic function (e.g., [fitted]{.fct}, [residuals]{.fct}).

Similar to [glm]{.fct} model objects, [plot]{.fct} can be used to visualize diagnostics (Figure$~$\ref{fig-sp-diagnostic2}):
```{r, eval = FALSE}
plot(spbin, which = c(4, 7))
```

\begin{figure}
\centering
\includegraphics[width = 1\linewidth]{figures/figure-3.png}
\caption{Spatial logistic regression model diagnostics from [plot]{.fct}. The Cook's distance values (left) and the fitted spatial covariance as a function of distance (right).}
\label{fig-sp-diagnostic2}
\end{figure}

Components of model variation are partitioned using [varcomp]{.fct}:
  ```{r}
varcomp(spbin)
```
The fixed effects explain roughly 9% of model variation, while the spatially dependent variance explains most of the remaining variability.


\begin{figure}
\centering
\includegraphics[width = 0.70\linewidth]{figures/figure-4.png}
\caption{Moose presence probability fitted values and predictions. Fitted values are represeneted by circles and predictions by triangles.}
\label{fig-moose-fit}
\end{figure}

We make predictions of the log odds of moose probability presence at each site in `moose_preds` using [predict]{.fct}:
  ```{r}
head(predict(spbin, newdata = moose_preds))
```

[augment]{.fct} may also be used to augment the prediction data with predictions:
  ```{r}
head(augment(
  spbin,
  newdata = moose_preds,
  type.predict = "response",
  interval = "prediction"
))
```

Here, we requested predictions on the probability (i.e., response) scale (Figure$~$\ref{fig-moose-fit}) alongside lower and upper bounds of a 95% (see `level`) prediction interval (Figure$~$\ref{fig-moose-int}).

\begin{figure}
\centering
\includegraphics[width = 1\linewidth]{figures/figure-5.png}
\caption{Moose presence probability prediction intervals. 95\% prediction interval lower bound (left) and 95\% prediction interval upper bound (right).}
\label{fig-moose-int}
\end{figure}

Thus far we have heuristically argued, based on first principles, that there are benefits to incorporating spatial correlation for GLMs applied to spatial data. Now we provide some empirical evidence to support this claim by comparing the fits of the SPGLM and a GLM. If `spcov_type = "none"`, the resulting model fit is nearly identical to that from [glm]{.fct}:

  ```{r}
bin <- spglm(
  formula = presence ~ elev + strat + elev:strat,
  family = binomial,
  data = moose,
  spcov_type = "none"
)
bin_glm <- glm(
  formula = presence ~ elev + strat + elev:strat,
  family = binomial,
  data = moose
)
data.frame(
  est_none = coef(bin),
  est_glm = coef(bin_glm),
  se_none = sqrt(diag(vcov(bin))),
  se_glm = sqrt(diag(vcov(bin_glm)))
) |>
  apply(2, round, digits = 4)
```

The advantage of using \code{spglm()} to fit a model with `spcov_type = "none"` is that it provides access to other \pkg{spmodel} functions for model objects (e.g., [glances]{.fct} below) and accounts for the additional terms in the likelihood from Equation$~$\ref{eq-marginal6}. These additional terms make the likelihood for \code{spglm()} and [glm]{.fct} different, though the models convey the same information. A glance at the spatial and nonspatial models reveals:
  ```{r}
glances(spbin, bin)
```

The spatial model has a notably lower AIC, AICc, BIC, and deviance, suggesting it is the superior model. Another model comparison approach is leave-one-out cross validation. In leave-one-out cross validation, separately each observation is held out, a model is fit to the remaining data, and a prediction is made for the mean of the held out observation on the response scale. Then, statistics like leave-one-out bias, mean-squared-prediction error (MSPE), and the square root of MSPE (RMSPE) may be computed:
  ```{r}
loocv(spbin) |>
  apply(2, round, digits = 4)
loocv(bin) |>
  apply(2, round, digits = 4)
```

Both models are nearly unbiased, but the spatial model has an approximately 39% lower MSPE, suggesting the probability predictions tend to be much closer to the observed presence values.

A third model comparison tool is area under the receiver operating characteristic (AUROC) curve. The AUROC curve ranges from zero to one and conveys a model's classification performance over all possible probability thresholds [@james2013introduction]. Larger values of AUROC indicate a more accurate model:
```{r}
AUROC(spbin)
AUROC(bin)
```

All three performance metrics (likelihood-based statistics, leave-one-out statistics, and AUROC) prefer the spatial model.

## Count data


\begin{figure}
\centering
\includegraphics[width = 0.70\linewidth]{figures/figure-6.png}
\caption{Moose counts in Alaska. Circles represent moose counts (based on color) and triangles represent locations at which mean count predictions are desired.}
\label{fig-moose-data-count}
\end{figure}

The `count` variable in `moose` contains the number of moose observed at a site (Figure$~$\ref{fig-moose-data-count}). Count data are often modeled using Poisson or negative binomial regression with the log link function. The Poisson regression model assumes each datum's underlying latent mean equals its variance, while the negative binomial accommodates overdispersion (where the variance is greater than the mean) at the cost of estimating an extra parameter.

So far our spatial models have made an implicit assumption of geometric isotropy. A spatial covariance is geometrically isotropic if its dependence decays with distance equally in all directions. A spatial covariance is geometrically anisotropic if its dependence decays differently in different directions. The geometric anisotropy's directionality and strength are controlled by rotation and scale parameters that are applied to the original coordinates, creating a transformed set of coordinates whose spatial covariance is geometrically isotropic. Geometrically anisotropic models are fit by specifying `anisotropy`:

```{r}
sppois <- spglm(
  formula = count ~ elev + strat + elev:strat,
  family = poisson,
  data = moose,
  spcov_type = "gaussian",
  anisotropy = TRUE
)

spnbin <- update(sppois, family = nbinomial)
```

Because the models have the same support (i.e., both non-negative count models), we can use likelihood-based statistics to compare them:
```{r}
glances(sppois, spnbin, sort_by = "AIC") |>
  subset(select = c(model, npar, AIC, AICc, BIC))
```

The negative binomial model has a slightly lower AIC and AICc, while the Poisson model has a slightly lower BIC. This is reasonable given the BIC penalizes additional parameters (here, an overdispersion parameter) more heavily than AIC and AICc. The leave-out-out MSPE prefers the negative binomial model:
```{r}
loocv(sppois) |>
  apply(2, round, digits = 4)
loocv(spnbin) |>
  apply(2, round, digits = 4)
```

A likelihood-based comparison between the negative binomial anisotropic model and the negative binomial isotropic model suggests that the anisotropic model is preferred (lower AIC, AICc, and BIC):
```{r}
spnbin_iso <- update(spnbin, anisotropy = FALSE)
glances(spnbin_iso, spnbin, sort_by = "AIC") |>
  subset(select = c(model, npar, AIC, AICc, BIC))
```

[plot]{.fct} returns the spatial covariance as a function of direction (Figure$~$\ref{fig-tropy}):
```{r}
plot(spnbin_iso, which = 8)
plot(spnbin, which = 8)
```

\begin{figure}
\centering
\includegraphics[width = 1\linewidth]{figures/figure-7.png}
\caption{Level curves of equal correlation for the negative binomial moose count models. The ellipse is centered at zero distance in the x-direction and y-direction, and points along the ellipse have equal levels of correlation.  In the isotropic level curve (left), spatial covariance decays equally in all directions. In the anistropic level curve (right), spatial covariance decays fastest in the northeast-southwest direction and slowest in the northwest-southeast direction (this pattern can be seen in the observed counts).}
\label{fig-tropy}
\end{figure}

Earlier we used [tidy]{.fct} to tidy the model's fixed effects, but we can also use tidy to tidy the spatial covariance parameters:
  ```{r}
tidy(spnbin, effects = "spcov")
```

The `rotate` parameter is the number of radians in $[0, \pi]$ the ellipse is rotated and the `scale` parameter is the ratio in $(0, 1]$ of the minor axis length to the major axis length. The `is_known` column indicates whether the parameter was assumed known during optimization, controlled by specifying the `spcov_initial` argument.

## Skewed data

The `seal` data in \pkg{spmodel} is an `sf` object with data on harbor seal trends in Alaska. The `log_trend` variable is the logarithm of a seal abundance temporal trend measure at the site (based on historical data), and the `stock` variable is a factor with two levels, `8` and `10`, where each level represents one of twelve seal stocks (i.e., breeds) in Alaska. The `seal` geometry is an areal polygon geometry and hence, spatial autoregressive models based on neighborhood distance are appropriate.

SPGLMs for areal data are are fit in \pkg{spmodel} using \code{spgautor()}, which has similar syntax as \code{spglm()} but contains arguments to control the weight matrix ($\mathbf{W}$ in Equation$~$\ref{eq-spcov-sar}) and whether or not row-standardization [@ver2018spatial] is applied.  By default, polygons are neighbors if they share a boundary (i.e., Queen's contiguity; see @pebesma2023spatial) and row standardization is assumed. Weight matrices may be provided via the `W` argument and row standardization may be ignored via the `row_st` arguent. Following @ver2018spatial, polygons without a neighbor are given their own (independent) variance parameter called `extra`.

The trend data were originally logged to remove skew [@ver2018spatial], but we will exponentiate `log_trend` (Figure$~$\ref{fig-seal}) and model this skew directly using a SPGLM:
```{r}
seal$trend <- exp(seal$log_trend)
```

The `trend` variable has several missing (`NA`) values, which represent polygons at which predictions of `trend` are desired. To make predictions using spatial autoregressive models, the prediction locations must be known prior to model fitting because these locations affect the neighborhood structure of the observed data [@ver2018spatial]. This restriction is notably different than SPGLMs for point-referenced data (i.e., geostatistical models), which completely separates the estimation and prediction steps.

\begin{figure}
\centering
\includegraphics[width = 1\linewidth]{figures/figure-8.png}
\caption{Seal trend distribution in Alaska. Observed and missing seal polygons by stock (left) and observed seal trends (right).}
\label{fig-seal}
\end{figure}

\pkg{spmodel} supports the gamma and inverse Gaussian families for modeling skewed, positive response variables. \pkg{spmodel} also supports nonspatial random effects specified via the `random` argument, which uses a similar formula syntax as [nlme]{.pkg} [@pinheiro2006mixed] and [lme4]{.pkg} [@bates2015lme4]. Using likelihood-based statistics, we compare two models fit using the simultaneous autoregressive covariance and the Gamma and inverse Gaussian families. Both models have a random effect for seal stock, which builds additional correlation into the model for two polygons from the same stock:
```{r}
spgam <- spgautor(
  formula = trend ~ 1,
  family = Gamma,
  data = seal,
  spcov_type = "sar",
  random = ~ stock
)
spinvg <- update(spgam, family = inverse.gaussian)
glances(spgam, spinvg, sort_by = "AIC") |>
  subset(select = c(model, npar, AIC, AICc, BIC))
```

The inverse Gaussian model has a lower AIC, AICc, and BIC, which indicates it is a better fit than the gamma model.

We may [tidy]{.fct} the estimated stock random effect variance:
```{r}
tidy(spinvg, effects = "randcov")
```

The locations to predict `trend` (`NA` values) are stored in the `newdata` element of `spinvg` and used for prediction:
```{r, results="hide"}
predict(spinvg, type = "response", interval = "prediction")
```

If using [augment]{.fct} for prediction, `newdata` must be specified:
```{r}
head(augment(
  spinvg,
  newdata = spinvg$newdata,
  type.predict = "response",
  interval = "prediction"
))
```

## Proportion data

We end with two examples of beta regression for proportion data [@ferrari2004beta]. First, we model the nitrogen percentage in a caribou foraging experiment. Second, we model the proportion of voter turnout by Texas county in the United States (US) 1980 presidential election.

\begin{figure}
\centering
\includegraphics[width = 0.70\linewidth]{figures/figure-9.png}
\caption{Caribou data from a spatial experimental design measuring the percentage of nitrogen (z) in soil and testing two factors: tarp type and water presence.}
\label{fig-caribou}
\end{figure}

The `caribou` data in \pkg{spmodel} are a data frame from a caribou foraging experiment in Alaska meant to study the impact of water and tarp cover on the percentage of nitrogen in surrounding plants (Figure$~$\ref{fig-caribou}). @lenart2002climate studied these data treating nitrogen percentage as a continuous variable, but here we treat nitrogen percentage (`z`) as a proportion:

```{r}
spbeta <- spglm(
    formula = z/100 ~ water + tarp + water:tarp,
    family = "beta",
    data = caribou,
    spcov_type = "matern",
    xcoord = x,
    ycoord = y
)
```

The nitrogen percentage is dynamically scaled in `formula` from (0, 100) to (0, 1) so that it is a proportion. Nitrogen percentage is modeled as a function of `water` (two levels: `"Y"` for water and `"N"` for no water), `tarp` (three levels: `"clear"` for a clear tarp, `"none"` for no tarp, and `"shade"` for a shaded tarp), and their interaction, which lets the effect of water presence vary across tarp type. `caribou` is a data frame (not an `sf` object), so we supply the x-coordinate and y-coordinate directly via `xcoord` and `ycoord`, and, consistent with the tidyverse approach [@wickham2019welcome], column names in `data` do not need to be quoted when referenced (but can be quoted).

A summary of `spbeta` returns a coefficients table that provides parameter estimates relative to a reference group. When factors have more than two levels, it is not straightforward to use these contrasts to determine overall significance of the factor. The `anova()` function tests marginal (i.e., Type III sums of squares) significance of factors using the general linear hypothesis test for spatial (i.e., correlated) data [@schabenberger2017statistical]:

```{r}
tidy(anova(spbeta))
```

These results suggest there is some evidence that the effect of water on nitrogen percentage differ depending on the type of tarp used (0.01 < $p~$value < 0.1).

Sometimes averages or contrasts between factor levels that are not in the reference group are of interest. We again leverage \pkg{spmodel}'s built-in support for [emmeans]{.pkg} and use it to obtain the averages of each factor combination on the link (here, logit) scale:
  ```{r}
spemm <- emmeans(spbeta, ~ water + tarp)
spemm
```

Delta method [@ver2012invented] standard errors are used when averages on the response (here, proportion) scale are desired:
  ```{r}
update(spemm, type = "response")
```

Pairwise contrasts use a Tukey $p~$value adjustment [@tukey1949comparing] by default. Here, we request no $p~$value adjustment:
  ```{r}
head(pairs(spemm, adjust = "none"))
```

\begin{figure}
\centering
\includegraphics[width = 0.70\linewidth]{figures/figure-10.png}
\caption{Proportion of voter turnout in Texas for the 1980 presidential election. Circles represent voter turnout (based on color) and triangles represent locations at which voter turnout predictions are desired.}
\label{fig-texas}
\end{figure}

We now model the `elect80` data in [spData]{.pkg} [@bivand2024spdata], which contains voter turnout data by county in the 1980 US Presidential election [@pace1997quick]. The `texas` data in \pkg{spmodel} contains a subset of these data in the state of Texas. These data are point-referenced, but we may still use autoregressive models if neighborhood distance is determined using county centroids (i.e., counties whose centroid distance is less than some cutoff are defined as neighbors). The response variable of interest, `turnout`, is the proportion of registered voters in the county who voted in the election (Figure$~$\ref{fig-texas}).

```{r}
spgautor_mods <- spgautor(
  formula = turnout ~ log_income,
  family = beta,
  data = texas,
  spcov_type = c("car", "sar"),
  cutoff = 2e5,
  estmethod = "ml"
)
```

We model voter turnout as a function of log income using both the conditional and simultaneous autoregressive models with a neighbor distance cutoff of 200 kilometers and the maximum likelihood estimation method. When a vector is provided to `spcov_type` in \code{spgautor()} (or \code{spglm()}), a model is fit for each spatial covariance type and stored in a list with name equal to the respective type. Then it is simple to glance at each model fit:
  ```{r}
glances(spgautor_mods) |>
  subset(select = c(model, npar, AIC, AICc, BIC))
```

The conditional autoregressive model has the best fit (in terms of AIC, AICc, and BIC). In this model, there is significant evidence `log_income` is positively related to average voter turnout ($p~$value < 0.001):
  ```{r}
tidy(spgautor_mods$car)
```

Another way to assess the impact of `log_income` on `turnout` is a likelihood ratio test:
  ```{r}
reduced_car <- update(spgautor_mods$car, formula = turnout ~ 1)
tidy(anova(reduced_car, spgautor_mods$car))
```

Likelihood ratio tests compare the fit of a "full" model compared to a "reduced" model that is completely nested within the full model. If there is evidence the full model explains significantly more information than the reduced model, the likelihood ratio test $p~$value will be small. Similar to the summary output from the general linear hypothesis test, the likelihood ratio suggests `log_income` is related to average voter turnout ($p~$value < 0.001).

The default estimation method in \pkg{spmodel} is REML, but note that these models used maximum likelihood (ML). ML is very similar to REML -- the difference is that for ML, the fixed effects $\boldsymbol{\beta}$ are not integrated out of Equation$~$\ref{eq-marginal} but are rather back-substituted. While REML typically performs better for fixed effect estimation and prediction [@zimmerman2024spatial], ML allows likelihood-based comparisons (e.g., AIC) for models with simultaneously varying fixed effect and covariance structures, while REML likelihood-based comparisons are only valid for models sharing the same fixed effect structure (though @gurka2006selecting provides some evidence that this restriction may be unnecessary).

The point-referenced `texas` and `caribou` data may be analyzed using \code{spgautor()} or \code{spglm()} and comparisons across these structures can be made using likelihood-based statistics (as long as the supports of the response distribution are the same). Put another way, likelihood-based statistics can be used to determine whether geostatistical (distance-based) or autoregressive (neighbor-based) structures perform best when the data are point-referenced.

# Discussion {#sec-discussion}


SPGLMs are fit in \pkg{spmodel} using a novel application of the Laplace approximation that marginalizes over the latent (i.e., unobserved) mean, $\mathbf{w}$, and the fixed effects, $\boldsymbol{\beta}$. The approach is quite flexible and accommodates any general response distribution and covariance structure.  @ver2024marginal show that the approach, as implemented in \pkg{spmodel}, generally yields unbiased estimators with proper interval coverage and often outperforms the Bayesian approach from [spBayes]{.pkg}, the INLA approach from [R-INLA]{.pkg}, and the automatic differentiation approach from [glmmTMB]{.pkg}.

\pkg{spmodel}'s \code{spglm()} and \code{spgautor()} functions are similar in structure and syntax as the base-\proglang{R} [glm]{.fct} function, easing the transition from GLMs to SPGLMs. These functions support six response distributions (Table$~$\ref{table-links}) and 20 spatial covariance functions. \pkg{spmodel} provides several additional features that accommodate geometric anisotropy, nonspatial random effects, fixing spatial covariance parameters at known values, data having thousands of observations (following @ver2023indexing), incorporating spatial dependence in machine learning (e.g., random forests; @breiman2001random), simulating spatially dependent data (e.g., [sprbinom]{.fct}, [sprpois]{.fct}), and several others. Learn more at [https://CRAN.R-project.org/package=spmodel](https://CRAN.R-project.org/package=spmodel) and links therein.

# Computational details {.unnumbered}

The results in this paper were obtained using \proglang{R} 4.4.0 with the
\pkg{spmodel} 0.9.0 package. Figures were created using the [ggplot2]{.pkg} 3.5.1 package [@wickham2016ggplot2] and base \proglang{R}.

# Data and code availability {.unnumbered}

All writing and code associated with this manuscript is available for viewing and download on GitHub at [https://github.com/USEPA/spmodel.glm.manuscript](https://github.com/USEPA/spmodel.glm.manuscript). All data used are part of the \pkg{spmodel} \proglang{R} package available for download from CRAN at [https://CRAN.R-project.org/package=spmodel](https://CRAN.R-project.org/package=spmodel).


# Acknowledgments {.unnumbered}

We would like to thank initial reviewers and editors for feedback that has greatly improved the manuscript.

The views expressed in this manuscript are those of the authors and do not necessarily represent the views or policies of the U.S. Environmental Protection Agency or the National Oceanic and Atmospheric Administration. Any mention of trade names, products, or services does not imply
an endorsement by the U.S. government, the U.S. Environmental Protection Agency, or the National Oceanic and Atmospheric
Administration. The U.S. Environmental Protection Agency and the National Oceanic and Atmospheric Administration do not endorse
any commercial products, services or enterprises.
