---
documentclass: jss
author:
    # use this syntax to add text on several lines
    # To add another line, use \AND at the end of the previous one as above
    # use a different affiliation in adress field (differently formated here)
  - name: Michael Dumelle
    orcid: 0000-0002-3393-5529
    affiliation: |
      | United States
      | Environmental Protection Agency
    address: |
      | Office of Research and Development, Pacific Ecological Systems Division
      | Current Address: Office of Water, Office of Wetlands, Oceans and Watersheds
      | 200 SW 35th St
      | Corvallis, OR, 97330
    email: \email{Dumelle.Michael@epa.gov}
  - name: Jay M. Ver Hoef
    orcid: 0000-0003-4302-6895
    affiliation: |
      | Alaska Fisheries
      | Science Center
  - name: Matt Higham
    orcid: 0009-0006-4217-625X
    affiliation: |
      | St. Lawrence University
title:
  # If you use tex in the formatted title, also supply version without
  # For running headers, if needed
  formatted: "Spatial Generalized Linear Models in \\proglang{R} Using \\pkg{spmodel}"
  plain:     "Spatial Generalized Linear Models in R Using spmodel"
  short:     "Spatial Generalized Linear Models in \\proglang{R} Using \\pkg{spmodel}"
abstract: |
  Non-Gaussian data are common in practice and include binary, count,
  skewed, and proportion structures. Often, non-Gaussian data are modeled
  using a generalized linear model (GLM). GLMs typically assume that
  observations are independent of one another. This is an impractical
  assumption for spatial data, as nearby observations tend to be more
  similar than distant ones. The \pkg{spmodel} package in \proglang{R}
  provides a suite of tools for fitting spatial generalized linear models
  (SPGLMs) to non-Gaussian data and making spatial predictions
  (i.e.,~kriging). SPGLMs for point-referenced ($x$~ and $y$~coordinates) support are
  fit using the \code{spglm()} function, while SPGLMs for areal (lattice,
  polygon) support are fit using the \code{spgautor()} function. Both
  \code{spglm()} and \code{spgautor()} maximize a novel Laplace likelihood
  that, under the default restricted maximum likelihood (REML) approach,
  marginalizes over the fixed effects and latent mean while formally
  incorporating spatial covariance among observations. The inputs
  and outputs of \code{spglm()} and \code{spgautor()} closely resemble the
  \code{glm()} function from base \proglang{R}, easing the transition from
  GLMs to SPGLMs. \pkg{spmodel} provides and builds upon several commonly
  used helper functions for model building like \code{summary()},
  \code{plot()}, and \code{fitted()}, among others. Spatial predictions of
  the latent mean at unobserved locations are obtained using
  \code{predict()} or \code{augment()}. \pkg{spmodel} accommodates myriad
  advanced modeling features like geometric anisotropy, nonspatial random
  effects, analysis of variance, and more. Throughout, we use
  \pkg{spmodel} to fit SPGLMs to moose presence and counts in Alaska,
  United States (US), skewed conductivity data in the Southwestern US,
  harbor seal abundance trends in Alaska, US, and voter turnout rates in
   Texas, US. 
keywords:
  formatted: [autoregressive model, geostatistical model, Poisson regression, link function, logistic regression, overdispersion, spatial covariance, spatial dependence]
  plain:     [autoregressive model, geostatistical model, Poisson regression, link function, logistic regression, overdispersion, spatial covariance, spatial dependence]
preamble: |
  \usepackage{amsmath,amsfonts,amssymb}
  \usepackage{bm, bbm}
  \usepackage{lineno}
  \usepackage{caption, subcaption}
  \usepackage{autonum}
  \usepackage{adjustbox}
output: rticles::jss_article
editor_options: 
  chunk_output_type: console
---



<!-- \linenumbers -->


\newpage

# Introduction {#sec-intro}

In practice, non-Gaussian data (e.g.,~binary, count, skewed, and
proportion data) are ubiquitous. Non-Gaussian data are often
modeled using a generalized linear
model (GLM) regression framework
[@nelder1972generalized; @mccullagh1989generalized]. In a GLM, an
\(n \times 1\) response variable \(\mathbf{y}\) belongs to a statistical
distribution (e.g.,~binomial, Poisson) with some mean and variance.
Often, the analysis goal is to study the impact of
explanatory variables on the mean of \(\mathbf{y}\) through a
GLM. In this context, the latent (i.e.,~unobserved) mean of
\(\mathbf{y}\), \(\boldsymbol{\mu}\), is linked to these explanatory
variables via a link function: 

\begin{align}\label{eq-glm}
f(\boldsymbol{\mu}|\mathbf{X}, \boldsymbol{\beta}) \equiv \mathbf{w} = \mathbf{X} \boldsymbol{\beta},
\end{align}

 where for a sample size \(n\), \(f(\cdot)\) is a link
function that connects \(\boldsymbol{\mu}\) to \(\mathbf{w}\),
\(\mathbf{X}\) is the \(n \times p\) design matrix of explanatory
variables, and \(\boldsymbol{\beta}\) is the \(p \times 1\) vector of
fixed effects. While the mean is typically constrained in some way
(e.g.,~if a probability, between zero and one), the link function
generally makes \(\mathbf{w}\) unconstrained. Common link functions
include the log odds (i.e.,~logit) link for binary and proportion data
and the log link for count and skewed data. Equation~\ref{eq-glm}
can also be written in terms of the inverse link function,
\(f^{-1}(\cdot)\): 

\begin{align}\label{eq-glm2}
\boldsymbol{\mu}|\mathbf{X}, \boldsymbol{\beta} \equiv f^{-1}(\mathbf{w}) = f^{-1}(\mathbf{X} \boldsymbol{\beta}).
\end{align}



The GLM fixed effects (\(\boldsymbol{\beta}\)) are typically estimated
via maximum likelihood, often via the iteratively
reweighted least squares (IRWLS) algorithm [@chambers1992S],
the typical approach used by the \code{glm()} function in the
\proglang{R} programming language [@rcore2024]. GLMs add an
additional layer of complexity compared to linear regression models, as
the left-hand side of Equation~\ref{eq-glm} is a function of the
mean of \(\mathbf{y}\) rather than \(\mathbf{y}\) itself (as in linear
regression models).

The standard GLM assumes the elements of \(\mathbf{y}\) are independent.
This independence assumption is typically impractical for spatial data.
For spatial data, nearby observations tend to be more similar than
distant observations [@tobler1970computer], which leads to positive
spatial covariance among observations. The consequences of ignoring
spatial covariance in statistical models for spatial data can be severe
and include imprecise parameter estimates as well as misleading standard
errors that inflate Type-I error rates and decrease power
[@legendre1993spatial].

An approach for handling spatial data using a GLM is to assume the
elements of \(\mathbf{w}\) exhibit both spatial and nonspatial
variability. This is achieved by adding to Equation~\ref{eq-glm} two
random effects, \(\boldsymbol{\tau}\) and \(\boldsymbol{\epsilon}\). The
random effect \(\boldsymbol{\tau}\) is an \(n \times 1\) column vector
of spatially dependent Gaussian random errors. We assume that
\( \E(\boldsymbol{\tau}) = \boldsymbol{0}\) and
\(\COV(\boldsymbol{\tau}) = \sigma^2_\tau \mathbf{R}\), where
\( \E(\cdot)\) and \(\COV(\cdot)\) denote expectation and
covariance, respectively. The variance parameter \(\sigma^2_\tau\)
controls the magnitude of spatial covariance and is often called a
partial sill. The matrix \(\mathbf{R}\) is an \(n \times n\) spatial
correlation matrix that depends on a range parameter controlling the
distance-decay rate of the spatial correlation. One example of a spatial
covariance matrix is the ``exponential'', which is given by


\begin{align}\label{eq-spcov-exp}
  \COV(\boldsymbol{\tau}) = \sigma^2_{\tau} \mathbf{R}_{exp} =  \sigma^2_{\tau} \exp(-\mathbf{H}/\phi),
\end{align}


where \(\mathbf{H}\) is a matrix of pairwise distances among
the elements of \(\mathbf{y}\) and \(\phi\) is the range parameter. From
Equation~\ref{eq-spcov-exp}, as the distance between two elements of
\(\mathbf{y}\) increases, the spatial covariance decreases, which
reflects intuition. Moreover, as the range parameter, \(\phi\),
increases, the strength of spatial dependence increases
(Figure~\ref{fig-correlations}(a)). The random effect
\(\boldsymbol{\epsilon}\) is an \(n \times 1\) column vector of
independent Gaussian random errors. We assume that
\( \E(\boldsymbol{\epsilon}) = \boldsymbol{0}\) and
\(\COV(\boldsymbol{\epsilon}) = \sigma^2_\epsilon \mathbf{I}\), where
\(\mathbf{I}\) is an \(n \times n\) identity matrix. The variance
parameter \(\sigma^2_\epsilon\) controls the magnitude of nonspatial
variability (i.e.,~fine-scale variation) and is often called a nugget.
Often in spatial statistics, quantities are explicitly referenced with
respect to \(\mathbf{s}\), a vector of spatial coordinates indexing the
observation [@cressie1993statistics]. For example, \(\mathbf{y}\)
and \(\mathbf{X}\) may instead be written \(\mathbf{y}(\mathbf{s})\) and
\(\mathbf{X}(\mathbf{s})\), respectively. We acknowledge the utility of
this nomenclature but drop the explicit dependence on \(\mathbf{s}\) for
simplicity of notation moving forward.

\begin{figure}[t!]
\centering
\includegraphics[width = 1\linewidth]{figures/figure-1.pdf}
\caption{Spatial correlation functions. (a) Exponential correlation functions with varying range parameters. (b) Exponential, Gaussian, and spherical correlation functions with range parameters equal to 0.5.}
\label{fig-correlations}
\end{figure}

Through inclusion of \(\boldsymbol{\tau}\) and
\(\boldsymbol{\epsilon}\), the spatial GLM (SPGLM) can be written as


\begin{align}\label{eq-spglm}
f(\boldsymbol{\mu}|\mathbf{X}, \boldsymbol{\beta}, \boldsymbol{\tau}, \boldsymbol{\epsilon}) \equiv \mathbf{w} = \mathbf{X} \boldsymbol{\beta} + \boldsymbol{\tau} + \boldsymbol{\epsilon}.
\end{align}

 Assuming independence between \(\boldsymbol{\tau}\) and
\(\boldsymbol{\epsilon}\), it follows that 

\begin{align}\label{eq-spcov}
 \COV(\boldsymbol{\tau} + \boldsymbol{\epsilon}) = \COV(\boldsymbol{\tau}) + \COV(\boldsymbol{\epsilon}) = \sigma^2_{\tau}\mathbf{R} + \sigma^2_{\epsilon} \mathbf{I}.
\end{align}

 Henceforth, we refer to \(\sigma^2_{\tau}\) as
\(\sigma^2_{de}\) (for spatially dependent error variance) and
\(\sigma^2_{\epsilon}\) as \(\sigma^2_{ie}\) (for independent error
variance). The parameters \(\sigma^2_{de}\), \(\sigma^2_{ie}\), and
\(\phi\), in addition to any other parameters in \(\mathbf{R}\), compose
\(\boldsymbol{\theta}\), the covariance parameter vector.

Fitting and using SPGLMs is challenging both conceptually and
computationally [@bolker2009generalized]. Recently, however, there
have been numerous, significant advances in the \proglang{R} software that
have made these models more accessible to practitioners. The \pkg{brms}
[@burkner2017brms], \pkg{carBayes} [@lee2013carbayes],
\pkg{ngspatial} [@hughes2020ngspatial], \pkg{R-INLA}
[@lindgren2015bayesian], \pkg{inlabru} [@bachl2019inlabru],
\pkg{spBayes} [@finley2007spbayes], \pkg{spOccupancy}
[@doser2022spoccupancy], \pkg{spAbundance}
[@doser2024spabundance], and \pkg{spNNGP} [@finley2002spnngp]
packages take a Bayesian approach, either directly sampling from
posterior distributions of parameters (e.g.,~using MCMC) or
approximating them. A benefit of Bayesian approaches is that prior
information can be incorporated and uncertainty quantification of
parameter estimates is straightforward. However, Bayesian approaches,
especially those using MCMC, can be computationally expensive. In order
to reduce computation time, many of these packages (e.g.,~\pkg{R-INLA})
work with the precision matrix instead of the covariance matrix so that
computationally expensive matrix inversion is not required. Working with
precision matrices, however, can be more restrictive and less intuitive
than working with covariance matrices. The \pkg{FRK}
[@sainsbury2024modeling], \pkg{glmmTMB} [@brooks2017glmmtmb],
\pkg{hglm} [@ronnegard2010hglm], \pkg{mgcv}
[@wood2017generalized], and \pkg{spaMM} [@rousset2014spamm]
packages directly use Laplace, quasi-likelihood, or reduced-rank
approaches to estimate parameters. These direct approaches tend to be
computationally efficient, as they don't rely on MCMC sampling. In
contrast to the Bayesian approach, a drawback of these direct approaches
is that prior information cannot be formally incorporated and covariance
parameter uncertainty is more challenging to quantify. The \pkg{sdmTMB}
[@anderson2025sdmtmb] package combines elements of \pkg{R-INLA},
\pkg{glmmTMB}, and Gaussian Markov random fields to fit a wide variety
of SPGLMs, while \pkg{tinyVAST} [@thorson2025tinyVAST] extends some
of these models to multivariate or (dynamic) structural equation models.

Building from @evangelou2011estimation and
@bonat2016practical, @ver2024marginal proposed a novel REML
approach for fitting SPGLMs that leverages the Laplace approximation
while marginalizing over both the latent \(\mathbf{w}\) and the fixed
effects (\(\boldsymbol{\beta}\)). This approach performed well in a
variety of simulation settings, generally having appropriate confidence
interval coverage for the fixed effects and prediction interval coverage
for \(\mathbf{w}\) at new locations. It also performed similarly to the
Bayesian SPGLM approach in \pkg{spBayes} and the automatic
differentiation SPGLM approach in \pkg{glmmTMB} but was much faster. At
small sample sizes, the approach outperformed the approximate Bayesian
SPGLM approach in \pkg{R-INLA} and had similar computational times. For
moderate sample sizes, it performed similarly to \pkg{R-INLA}, though
\pkg{R-INLA} was faster. The novel Laplace approach is particularly
attractive for two reasons. First, it is general enough that it can be
applied to any covariance structure (not just spatial). Second, after
estimating the covariance parameters, analytical solutions exist for the
fixed effects (and their standard errors) as well as predictions of the
latent \(\mathbf{w}\) at new locations (and their standard errors).

The \pkg{spmodel} \proglang{R} package [@dumelle2023spmodel]
recently released a full set of modeling tools for SPGLMs fit using the
novel Laplace approach described by @ver2024marginal. These
modeling tools are approachable and mirror the familiar \code{glm()}
syntax from base \proglang{R}, making the transition from GLMs to SPGLMs
relatively seamless. The \code{spglm()} function fits SPGLMs for
point-referenced support (e.g.,~$x$~ and $y$~coordinates representing point
locations in a field; these models are sometimes called
"geostatistical" models), while the \code{spgautor()} function fits
SPGLMs for areal support (e.g.,~polygon boundaries representing
geographic subsets of a region; these models are sometimes called
"autoregressive" models). For both point-referenced and areal
supports, \pkg{spmodel} supports the binomial distribution for binary
data, Poisson and negative binomial distributions for count data, Gamma
and inverse Gaussian distributions for skewed data, and the beta
distribution for proportion data. There are 20 different spatial
covariance structures available including the exponential, Gaussian, and
spherical for point-referenced support (Figure~\ref{fig-correlations}(b)) and
the conditional autoregressive and simultaneous autoregressive
structures for areal support. \pkg{spmodel} provides commonly used model
summaries, visualizations, and diagnostics (e.g.,~fitted values) using
standard \proglang{R} methods like \code{summary()},
\code{plot()}, and \code{fitted()}, among others. \pkg{spmodel} also has
tools to predict \(\mathbf{w}\) at new locations and quantify
uncertainty in those predictions using \code{predict()} and
\code{augment()}. This core functionality, combined with several
advanced features we describe throughout the manuscript, enables
\pkg{spmodel} to introduce novel, important SPGLM modeling tools
previously missing from the existing \proglang{R} ecosystem.

Of the existing \proglang{R} packages for SPGLMs, \pkg{spmodel} (version
0.11.1) is arguably most similar to \pkg{sdmTMB} (version 0.7.4) in
terms of scope and feel. Both packages use similar syntax to
\code{glm()}, accommodate flexible \code{formula} arguments (e.g.,
transformations, splines), handle spatial covariance that decays at
different rates in different directions (i.e.,~geometric anisotropy),
incorporate nonspatial random effects, support other \proglang{R}
packages for modeling like \pkg{broom}
[@robinson2021broom; @kuhn2022tidy], \pkg{emmeans}
[@lenth2024emmeans], and \pkg{car} [@fox2019car], and have
tools for model summaries, prediction, and simulating data. There are
some notable differences between the two packages, however. \pkg{sdmTMB}
supports several additional GLM distributions like the Tweedie, supports
Hurdle models, and can incorporate prior information through Bayesian
applications. \pkg{sdmTMB} also provides tools for working with temporal
data and spatiotemporal data and provides enhanced visualizations of the
model's marginal effects. \pkg{sdmTMB} does require a preprocessing step
of constructing a mesh prior to model fitting (using the stochastic
partial differential equation approach), and the density of the mesh can
affect model results and computational complexity. On the other hand,
\pkg{spmodel} does not require the construction of a mesh prior to model
fitting. \pkg{spmodel} supports 20 different spatial covariances and
models them directly, rather than using a precision matrix approximation
to the Matérn spatial covariance as in \pkg{sdmTMB}. \pkg{spmodel} can
model data using neighborhood distance and autoregressive models, rather
than relying on the polygon centroid (as in \pkg{sdmTMB}), which may not
be within the polygon's boundaries. \pkg{spmodel} provides experimental
design tools (e.g.,~analysis of variance, contrasts), supports \pkg{sf}
objects in modeling and prediction functions [@pebesma2018sf], has
several specialized model diagnostics like leverage values and Cook's
distances, and has analytic solutions for fixed effect and prediction
standard errors. Other similarities and differences do exist between
\pkg{sdmTMB} and \pkg{spmodel}, and both packages continue to evolve.
Overall, we believe that these packages are complementary and enhance
the suite of SPGLM tools accessible to practitioners.

The rest of this article is organized as follows. In
Section~\ref{sec-spglm}, we provide some background for the SPGLM
fitting and prediction routines in \pkg{spmodel}. In
Section~\ref{sec-applications}, we provide an overview of core SPGLM
functionality in \pkg{spmodel} by modeling moose presence in Alaska,
United States (US). In Section~\ref{sec-applications2}, we model
moose counts in Alaska, US; skewed lake conductivity in the Southwestern
US; harbor seal abundance trend behavior in Alaska, US; and voter
turnout rates in Texas, US. And in Section~\ref{sec-discussion}, we
end with a discussion synthesizing \pkg{spmodel}'s contributions to
SPGLMs in \proglang{R}.

# The spatial generalized linear model and marginalization {#sec-spglm}

The novel Laplace approach implemented in \pkg{spmodel} formally
maximizes a hierarchical GLM likelihood
[@lee1996hierarchical; @wood2017generalized], making
likelihood-based statistics for model comparison like AIC
[@akaike1974new], AICc [@hoeting2006model], BIC
[@schwarz1978estimating], deviance
[@mccullagh1989generalized], and likelihood ratio tests available.
These types of statistics are not directly available for
quasi-likelihood [@wedderburn1974quasi; @breslow1993approximate] or
pseudo-likelihood approaches [@wolfinger1993generalized], which
only specify the first two moments of a distribution. Next, we describe
a brief overview of the approach and how it can be used for several
primary data analysis tasks [@tredennick2021practical] like model
comparison, parameter estimation, inference, model diagnostics, and
prediction.

## Formulating the hierarchical likelihood {#formulating-the-hierarchical-likelihood}

Conditional on the latent vector \(\mathbf{w}\) and dispersion parameter
\(\varphi\), the response follows the GLM family with mean
\(f^{-1}(\mathbf{w})\):

\[
  \mathbf{y} | \mathbf{w}, \varphi \sim g(f^{-1}(\mathbf{w}), \varphi).
\]

Here, \([\mathbf{y} | f^{-1}(\mathbf{w}), \varphi]\) denotes the
corresponding density (e.g.,~binomial or Poisson), and the response
components are conditionally independent given \(\mathbf{w}\). The
dispersion parameter \(\varphi\) controls the response variance when
the family has a free dispersion parameter.

The latent vector follows a multivariate Gaussian distribution with
mean determined by the design matrix and fixed effects:

\[
  \mathbf{w} | \mathbf{X}, \boldsymbol{\beta}, \boldsymbol{\theta}
  \sim \operatorname{MVN}(\mathbf{X}\boldsymbol{\beta},
  \boldsymbol{\Sigma}(\boldsymbol{\theta})).
\]

Its density is denoted by
\([\mathbf{w} | \mathbf{X}, \boldsymbol{\beta}, \boldsymbol{\theta}]\).
The covariance matrix \(\boldsymbol{\Sigma}(\boldsymbol{\theta})\)
encodes the spatial and nonspatial covariance parameters
\(\boldsymbol{\theta}\).

Under maximum likelihood (ML), the fixed effects are retained while the
latent vector is integrated out. This gives the marginal likelihood
[@bonat2016practical]:

\begin{align}\label{eq-marginal-ml}
  [\mathbf{y} | \mathbf{X}, \boldsymbol{\beta}, \varphi,
  \boldsymbol{\theta}] = \int_{\mathbf{w}}
  [\mathbf{y} | f^{-1}(\mathbf{w}), \varphi]
  [\mathbf{w} | \mathbf{X}, \boldsymbol{\beta}, \boldsymbol{\theta}]
  d\mathbf{w}.
\end{align}

Under restricted maximum likelihood (REML), the fixed effects are also
integrated out of the Gaussian component [@ver2024marginal]. This
produces the restricted Gaussian density for \(\mathbf{w}\) given
\(\mathbf{X}\) and \(\boldsymbol{\theta}\) [@patterson1971recovery]:

\begin{align}\label{eq-reml-def}
[\mathbf{w} | \mathbf{X}, \boldsymbol{\theta}] = \frac{\exp(-\frac{1}{2}(\mathbf{w} - \mathbf{X}\tilde{\boldsymbol{\beta}})^\top \boldsymbol{\Sigma}^{-1} (\mathbf{w} - \mathbf{X}\tilde{\boldsymbol{\beta}}))}{(2 \pi)^{(n - p)/2} |\boldsymbol{\Sigma}|^{1/2}|\mathbf{X}^\top \boldsymbol{\Sigma}^{-1} \mathbf{X}|^{1/2}},
\end{align}

where
\(\tilde{\boldsymbol{\beta}} = (\mathbf{X}^\top \boldsymbol{\Sigma}^{-1} \mathbf{X})^{-1} \mathbf{X}^\top \boldsymbol{\Sigma}^{-1} \mathbf{w}\),
\(\boldsymbol{\Sigma}\) denotes the covariance matrix (of
\(\mathbf{w}\)), and \(|\cdot|\) denotes the determinant.
The resulting REML likelihood integrates over the latent vector using
this restricted Gaussian density:

\begin{align}\label{eq-marginal}
  [\mathbf{y} | \mathbf{X}, \varphi, \boldsymbol{\theta}] =
  \int_{\mathbf{w}} [\mathbf{y} | f^{-1}(\mathbf{w}), \varphi]
  [\mathbf{w} | \mathbf{X}, \boldsymbol{\theta}] d\mathbf{w}.
\end{align}

Next, let


\begin{align}\label{eq-marginal03}
  \ell_\mathbf{w} = \log([\mathbf{y} | f^{-1}(\mathbf{w}), \varphi] [\mathbf{w} | \mathbf{X}, \boldsymbol{\theta}])
\end{align}

 and rewrite Equation~\ref{eq-marginal} as


\begin{align}\label{eq-marginal3}
  [\mathbf{y}|\mathbf{X}, \varphi, \boldsymbol{\theta}] = \int_{\mathbf{w}} \exp(\ell_\mathbf{w}) d\mathbf{w}.
\end{align}

 A second-order Taylor series expansion of
\(\ell_\mathbf{w}\) around a point \(\mathbf{w}^{*}\) yields


\begin{align}\label{eq-marginal4}
  [\mathbf{y}|\mathbf{X}, \varphi, \boldsymbol{\theta}] \approx \int_{\mathbf{w}} \exp(\ell_{\mathbf{w}^{*}} + \mathbf{g}^\top(\mathbf{w} - \mathbf{w}^{*}) + \frac{1}{2}(\mathbf{w} - \mathbf{w}^{*})^\top \mathbf{G} (\mathbf{w} - \mathbf{w}^{*}))d\mathbf{w},
\end{align}

 where \(\mathbf{g}\) and \(\mathbf{G}\) are the gradient and
Hessian, respectively, of \(\ell_\mathbf{w}\) with respect to
\(\mathbf{w}\). The forms of \(\mathbf{g}\) and \(\mathbf{G}\) are given by

\begin{align}
  \mathbf{g} = \mathbf{d} - \mathbf{P}\mathbf{w}
  \qquad \mathbf{G} = \mathbf{D} - \mathbf{P}
\end{align}

where \(\mathbf{d}\) and \(\mathbf{D}\) are the gradient and Hessian
contributions from the conditional log-likelihood, respectively, and
\(\mathbf{P}\) is the restricted precision matrix defined as

\begin{align}
  \mathbf{P} = \boldsymbol{\Sigma}^{-1} - \boldsymbol{\Sigma}^{-1}\mathbf{X}
  (\mathbf{X}^\top \boldsymbol{\Sigma}^{-1}\mathbf{X})^{-1}
  \mathbf{X}^\top \boldsymbol{\Sigma}^{-1}.
\end{align}

If \(\mathbf{w}^{*}\) is a value for which \(\mathbf{g} = \mathbf{0}\),

\begin{align}\label{eq-marginal5}
  [\mathbf{y}|\mathbf{X}, \varphi, \boldsymbol{\theta}] \approx \exp(\ell_{\mathbf{w}^{*}}) \int_{\mathbf{w}} \exp(-\frac{1}{2}(\mathbf{w} - \mathbf{w}^{*})^\top (-\mathbf{G}) (\mathbf{w} - \mathbf{w}^{*}))d\mathbf{w}.
\end{align}

 The integral in Equation~\ref{eq-marginal5} can be
solved by leveraging properties of the normalizing constant of a
multivariate Gaussian distribution. Thus, rewriting
\(\exp(\ell_{\mathbf{w}^{*}})\) yields 

\begin{align}\label{eq-marginal6}
  [\mathbf{y}|\mathbf{X}, \varphi, \boldsymbol{\theta}] \approx [\mathbf{y} | f^{-1}(\mathbf{w}^{*}), \varphi] [\mathbf{w}^{*} | \mathbf{X}, \boldsymbol{\theta}] (2 \pi)^{n/2}|-\mathbf{G}_{\mathbf{w}^{*}}|^{-1/2},
\end{align}

 which can be directly evaluated. Equation~\ref{eq-marginal6} gives the
REML Laplace approximation. Under ML, \(\boldsymbol{\beta}\) is retained
in the likelihood. To locate \(\mathbf{w}^{*}\), it is profiled out of
the Gaussian component, giving the score \(\mathbf{g}=\mathbf{d}-\mathbf{P}\mathbf{w}\)
and profiled Hessian \(\mathbf{G}_{\mathrm{profile}}=\mathbf{D}-\mathbf{P}\).
The Hessian in the Laplace determinant is different: because ML
integrates over \(\mathbf{w}\) with \(\boldsymbol{\beta}\) held fixed,
it uses \(\mathbf{G}_{\mathrm{ML}}^{*}=\mathbf{D}_{\mathbf{w}^{*}}-\boldsymbol{\Sigma}^{-1}\).
For REML, where \(\boldsymbol{\beta}\) is integrated out first, the
determinant uses \(\mathbf{G}_{\mathrm{REML}}^{*}=\mathbf{D}_{\mathbf{w}^{*}}-\mathbf{P}\).
Thus, under ML the profiled Hessian is used to locate the mode, while
the conditional Hessian is used in the Laplace determinant
[@ver2024marginal].

For either method, evaluating the marginal likelihood involves an outer
optimization over the covariance parameters \(\boldsymbol{\theta}\) and
dispersion parameter \(\varphi\), with an inner optimization to find
\(\mathbf{w}^{*}\) at each candidate parameter value. Under ML,
\(\boldsymbol{\beta}\) is also retained and estimated; under REML it is
integrated out and estimated afterward, as described below. At the
fitted parameter values, we denote the latent-mode estimate by
\(\hat{\mathbf{w}}\). @ver2024marginal provide further details.

## Estimating fixed effects {#sec:fixed}

We can estimate the fixed effects using generalized least squares (GLS)
principles, a common practice for linear models estimated using
restricted maximum likelihood methods. Had we observed \(\mathbf{w}\), a
GLS estimator for \(\boldsymbol{\beta}\) is given by


\begin{align}\label{eq-gls1}
  \hat{\boldsymbol{\beta}} = (\mathbf{X}^\top \boldsymbol{\Sigma}^{-1}\mathbf{X})^{-1}\mathbf{X}^\top \boldsymbol{\Sigma}^{-1}\mathbf{w} = \mathbf{B}\mathbf{w},
\end{align}

 where
\(\mathbf{B} = (\mathbf{X}^\top \boldsymbol{\Sigma}^{-1}\mathbf{X})^{-1}\mathbf{X}^\top \boldsymbol{\Sigma}^{-1}\).
While we do not know \(\mathbf{w}\), we do estimate it via
\(\hat{\mathbf{w}}\), and thus it is reasonable to define
\(\hat{\boldsymbol{\beta}} = \mathbf{B}\hat{\mathbf{w}}\). Following
@ver2024marginal, assume that conditional on \(\mathbf{w}\),
\(\hat{\mathbf{w}}\) is multivariate normal with mean
\(\mathbf{w}\) and covariance approximately equal to
\(-\mathbf{G}_{\mathbf{w}^{*}}^{-1}\), the inverse observed
information evaluated at the latent mode. Thus,
\(\E(\hat{\mathbf{w}})\) is given by

\begin{align}
   \E(\hat{\mathbf{w}}) =  \E( \E(\hat{\mathbf{w}} | \mathbf{w}))
   =  \E(\mathbf{w}) 
   = \mathbf{X}\boldsymbol{\beta}
\end{align}

 and \(\COV(\hat{\mathbf{w}})\) is given by


\begin{align}
  \COV(\hat{\mathbf{w}}) & =  \E(\COV(\hat{\mathbf{w}} | \mathbf{w})) + \COV( \E(\hat{\mathbf{w}} | \mathbf{w})) \\
  & \approx -\mathbf{G}_{\mathbf{w}^{*}}^{-1} + \boldsymbol{\Sigma}
\end{align}

Under these assumptions, and treating the covariance parameters as
fixed, \(\hat{\boldsymbol{\beta}}\) is unbiased for
\(\boldsymbol{\beta}\):


\begin{align}
   \E(\hat{\boldsymbol{\beta}})  =  \E(\mathbf{B}\hat{\mathbf{w}}) 
  = \mathbf{B} \E(\hat{\mathbf{w}}) =  (\mathbf{X}^\top \boldsymbol{\Sigma}^{-1}\mathbf{X})^{-1}(\mathbf{X}^\top \boldsymbol{\Sigma}^{-1} \mathbf{X})\boldsymbol{\beta} = \boldsymbol{\beta}.
\end{align}

 Moreover, it follows that 

\begin{align}
  \COV(\hat{\boldsymbol{\beta}}) & = \COV(\mathbf{B}\hat{\mathbf{w}}) \\
  & = \mathbf{B} \COV(\hat{\mathbf{w}}) \mathbf{B}^\top\\
  & = \mathbf{B} (-\mathbf{G}^{-1} + \boldsymbol{\Sigma}) \mathbf{B}^\top \\
  & = \mathbf{B}(-\mathbf{G})^{-1}\mathbf{B}^\top + \mathbf{B}\boldsymbol{\Sigma}\mathbf{B}^\top \\
  & = \mathbf{B}(-\mathbf{G})^{-1}\mathbf{B}^\top + (\mathbf{X}^\top \boldsymbol{\Sigma}^{-1}\mathbf{X})^{-1}.
\end{align}

 In practice, the covariance formula is evaluated at
\(\hat{\boldsymbol{\theta}}\), the estimated covariance parameter
vector.

These results are important because they justify analytic (i.e.,
closed-form) solutions for \(\hat{\boldsymbol{\beta}}\) and its
associated variance. Analytic solutions are useful because they bypass
the need for sampling-based strategies to evaluate the mean and variance
of \(\hat{\boldsymbol{\beta}}\), a common technique for other approaches
to SPGLMs like Bayesian MCMC that can be computationally intensive.

## Inspecting model diagnostics {#sec:diagnostics}

Inspecting model diagnostics is an important step of the modeling
process that can yield valuable insights into model behavior and unusual
observations. @montgomery2021introduction contextualize three
components of unusual observations: outliers, leverage, and influence.
An observation is an outlier if it has an extreme response value
relative to expectation. The response GLM residuals simply compare the
observation to its fitted latent mean: 

\begin{align}
  \mathbf{r}_{r} = \mathbf{y} - f^{-1}(\hat{\mathbf{w}})
\end{align}

 Because observations often have a unique support in a GLM
(e.g.,~only two possible response values for binary data) and the
variance of an observation generally depends on its mean, response
residuals lack some utility. Deviance, Pearson, and standardized
residuals are all different functions of response residuals that are
appropriately scaled to behave more like response residuals from a
standard linear model. For example, deviance residuals are given by


\begin{align}
  \mathbf{r}_{d} = \operatorname{sgn}(\mathbf{r}_{r})\sqrt{\mathbf{d}},
\end{align}

 where \(\mathbf{d}\) is a vector of individual deviances.
The sum of the squared deviance residuals equals the sum of the elements
of \(\mathbf{d}\), known as the deviance of the model fit. The deviance
of the model fit quantifies twice the difference in log likelihoods
between the saturated model that fits every observation perfectly
(i.e.,~\(\mu_{s,i} = y_i\) for all \(i\)) and the
fitted model [@myers2012generalized]. Deviance is often used as a
fit statistic, whereby lower values of deviance imply a better model fit
(given the observed data). Scaling the response residuals by the
reciprocal square root of the variance function (for the appropriate
response distribution) yields the Pearson residuals. In \pkg{spmodel},
standardized residuals are formed from deviance residuals, with
additional scaling for the family-specific deviance rescaling factor
\(\kappa_i\) and leverage: \(r_{s,i} = r_{d,i}/\sqrt{\kappa_i(1 -
L_{ii})}\) [@faraway2016extending].

An observation has high leverage if its combination of explanatory
variables is far away from other observations. In a linear model, the
leverage (i.e.,~hat) values are the diagonal of the leverage (i.e.,
projection, hat) matrix,
\(\mathbf{L} = \mathbf{X}(\mathbf{X}^\top \mathbf{X})^{-1}\mathbf{X}^\top\).
In a GLM, the leverage matrix is given by 

\begin{align}
  \mathbf{L} = \mathbf{V}^{1/2} \mathbf{X} (\mathbf{X}^\top \mathbf{V} \mathbf{X})^{-1} \mathbf{X}^\top \mathbf{V}^{1/2}.
\end{align}

 where \(\mathbf{V}\) is a diagonal matrix with \(i\)th
diagonal element equal to the expected information. The larger the value of
\(\mathbf{L}_{ii}\), the more severe the leverage from the \(i\)th
observation.

An observation is influential if it has a sizable impact on model fit.
Influence is measured using Cook's distance
[@cook1979influential; @cook1982residuals], which is given for a GLM
by 

\begin{align}
  \mathbf{c} = \frac{\mathbf{r}^2_{s}}{\text{tr}(\mathbf{L})} \frac{diag(\mathbf{L})}{(\mathbf{1} - diag(\mathbf{L}))},
\end{align}

 where \(\mathbf{r}^2_{s}\) are the standardized residuals
and \(diag(\mathbf{L})\) indicates the diagonal elements of the leverage
matrix. The larger the value of \(\text{c}_i\), the more severe the
influence from the \(i\)th observation.
@montgomery2021introduction provide guidance for interpreting
these types of statistics, including cutoffs to consider when
identifying extreme residual, leverage, or influence values.

In a linear model, the \(R^2\) (R-squared) statistic quantifies the
proportion of variability in the data captured by the explanatory
variables. It is calculated as one minus the ratio of the error sum of
squares to the total sum of squares [@rencher2008linear]. In a GLM,
there are many ways to define a statistic that emulates the
aforementioned meaning of \(R^2\) from the linear model
[@smith2013comparison]. This statistic is called a pseudo R-squared
(\(PR^2\)). One \(PR^2\) for GLMs simply replaces the sums of squares
ratio from the linear model with the deviance ratio: 

\begin{align}
  PR^2 = 1 - \frac{deviance_{error}}{deviance_{total}},
\end{align}

 where \(deviance_{error}\) is the deviance of the fitted
model (sometimes called the error or residual deviance) and
\(deviance_{total}\) is the deviance of the intercept-only model
(sometimes called the total or null deviance). In practice,
\(deviance_{total}\) is derived by computing \(\hat{\mathbf{w}}\) when
\(\mathbf{X} \equiv \mathbf{1}\) (a column of all ones), given
\(\hat{\boldsymbol{\theta}}\) and \(\hat{\varphi}\) from the fitted
model. Like \(R^2\), \(PR^2\) can be adjusted to account for the numbers
of parameters estimated in a model. Because the \(deviance_{total}\)
denominator changes across fitted models (as the values of
\(\hat{\boldsymbol{\theta}}\) and \(\hat{\varphi}\) change), this
statistic should not be used as a model comparison tool. Rather, it
should be used as an informative diagnostic tool that is unique to each
model fit and describes how much variability from that model is
attributable to the explanatory variables.

## Predicting at new locations {#predicting-at-new-locations}

We may also predict values of the latent mean (on the link scale) at new
locations by leveraging the spatial covariance between observed
locations and new locations (spatial prediction is also called Kriging;
see @cressie1990origins). Like in Section~\ref{sec:fixed},
suppose first that we observed \(\mathbf{w}\) and we want to make
predictions at \(\mathbf{u}\), a vector of latent means at the new
locations that follows the same SPGLM from Equation~\ref{eq-spglm}
and has design matrix, \(\mathbf{X}_{\mathbf{u}}\). The vector
\((\mathbf{w}, \mathbf{u})^\top\) has expectation
\((\mathbf{X}\boldsymbol{\beta}, \mathbf{X}_\mathbf{u}\boldsymbol{\beta})^\top\)
and covariance matrix
\(\begin{bmatrix} \boldsymbol{\Sigma} & \boldsymbol{\Sigma}_{\mathbf{w}\mathbf{u}} \\ \boldsymbol{\Sigma}_{\mathbf{u}\mathbf{w}} & \boldsymbol{\Sigma}_{\mathbf{u}\mathbf{u}} \end{bmatrix}\),
where \(\boldsymbol{\Sigma} = \COV(\mathbf{w}, \mathbf{w})\),
\(\boldsymbol{\Sigma}_{\mathbf{w} \mathbf{u}} = \COV(\mathbf{w}, \mathbf{u})\),
\(\boldsymbol{\Sigma}_{\mathbf{u} \mathbf{w}} = \boldsymbol{\Sigma}_{\mathbf{w} \mathbf{u}}^\top\)
and
\(\boldsymbol{\Sigma}_{\mathbf{u}, \mathbf{u}} = \COV(\mathbf{u}, \mathbf{u})\).
Thus we may derive the conditional distribution of
\(\mathbf{u}|\mathbf{w}\), which has the following properties:


\begin{align}
   \E(\mathbf{u} | \mathbf{w}) & = \mathbf{X}_{\mathbf{u}} \boldsymbol{\beta} + \boldsymbol{\Sigma}_{\mathbf{u}, \mathbf{w}}\boldsymbol{\Sigma}^{-1}(\mathbf{w} - \mathbf{X}\boldsymbol{\beta}), \\
  \COV(\mathbf{u} | \mathbf{w}) & = \boldsymbol{\Sigma}_{\mathbf{u}, \mathbf{u}} - \boldsymbol{\Sigma}_{\mathbf{u}, \mathbf{w}} \boldsymbol{\Sigma}^{-1} \boldsymbol{\Sigma}_{\mathbf{w}, \mathbf{u}}.
\end{align}


Recall, however, that we do not actually observe
\(\mathbf{w}\) and instead compute \(\hat{\mathbf{w}}\); so, to predict
\(\mathbf{u}\) and quantify its uncertainty, we must again leverage the
laws of total expectation and variance. @ver2024marginal show
that \(\hat{\mathbf{u}}\) and its associated variance are given by:


\begin{align}
  \hat{\mathbf{u}} & = \mathbf{X}_{\mathbf{u}} \hat{\boldsymbol{\beta}} + \boldsymbol{\Sigma}_{\mathbf{u}, \mathbf{w}}\boldsymbol{\Sigma}^{-1}(\hat{\mathbf{w}} - \mathbf{X}\hat{\boldsymbol{\beta}}) \\
  \COV(\hat{\mathbf{u}}) & = \boldsymbol{\Sigma}_{\mathbf{u}, \mathbf{u}} - \boldsymbol{\Sigma}_{\mathbf{u}, \mathbf{w}} \boldsymbol{\Sigma}^{-1} \boldsymbol{\Sigma}_{\mathbf{w}, \mathbf{u}} + \mathbf{K}(\mathbf{X}^\top \boldsymbol{\Sigma}^{-1}\mathbf{X})^{-1}\mathbf{K}^\top + \boldsymbol{\Lambda}(-\mathbf{G})^{-1}\boldsymbol{\Lambda}^\top,
\end{align}

 
where
\(\mathbf{K} = \mathbf{X}_{\mathbf{u}} - \boldsymbol{\Sigma}_{\mathbf{u}, \mathbf{w}} \boldsymbol{\Sigma}^{-1} \mathbf{X}\)
and
\(\boldsymbol{\Lambda} = \mathbf{X}_{\mathbf{u}}\mathbf{B} + \boldsymbol{\Sigma}_{\mathbf{u}, \mathbf{w}}\boldsymbol{\Sigma}^{-1}(\mathbf{I} - \mathbf{X}\mathbf{B})\)
where \(\mathbf{I}\) is the \(n \times n\) identity matrix. As with
\(\hat{\boldsymbol{\beta}}\), these covariance matrices are evaluated at
\(\hat{\boldsymbol{\theta}}\) in practice.

# Modeling moose presence in Alaska, USA {#sec-applications}

The \code{moose} data in \pkg{spmodel} contain information on moose
(Alces alces) presence in the Togiak region of Alaska, USA. \code{moose}
is an \code{sf} object, a special data frame that is supplemented with
spatial information using the \pkg{sf} package in \proglang{R}
[@pebesma2018sf]. After loading \pkg{spmodel}, the first few rows
of \code{moose} look like:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> library("spmodel")
R> head(moose)
\end{CodeInput}
\begin{CodeOutput}
Simple feature collection with 6 features and 4 fields
Geometry type: POINT
Dimension:     XY
Bounding box:  xmin: 281896.4 ymin: 1518398 xmax: 311325.3 ymax: 1541016
Projected CRS: NAD83 / Alaska Albers
# A tibble: 6 x 5
   elev strat count presence           geometry
  <dbl> <chr> <dbl> <fct>           <POINT [m]>
1  469. L         0 0        (293542.6 1541016)
2  362. L         0 0        (298313.1 1533972)
3  173. M         0 0        (281896.4 1532516)
4  280. L         0 0        (298651.3 1530264)
5  620. L         0 0        (311325.3 1527705)
6  164. M         0 0        (291421.5 1518398)
\end{CodeOutput}
\end{CodeChunk}
```
There are five columns in \texttt{moose}: \code{elev}, the numeric site
elevation (meters); \code{strat} a stratification variable for sampling
with two levels (\code{"L"} and \code{"M"}) categorized by landscape
metrics at each site; \code{count}, the number of moose at each site;
\code{presence}, a factor that indicates whether at least one moose was
observed at each site (\code{0} implies no moose; \code{1} implies at
least one moose); and \code{geometry}, the NAD83/Alaska Albers (EPSG:
3338) projected coordinate of each site. These data have
point-referenced support because each observation occurs at point
coordinates represented by a \code{POINT} geometry. Moose are most
prevalent in the southwestern and eastern parts of the Togiak region
(Figure~\ref{fig-moose-data}).

\begin{figure}[t!]
\centering
\includegraphics[width = 0.70\linewidth, trim=0 10 0 0,clip]{figures/figure-3.pdf}
\caption{Moose presence in Alaska. Circles represent moose presence or absence (based on color) and triangles represent locations at which moose presence probability predictions are desired.}
\label{fig-moose-data}
\end{figure}

The \code{moose_preds} data in \pkg{spmodel} is a \code{sf} object with
point locations at which moose presence predictions are desired. Like
\code{moose}, \code{moose_preds} contains \code{elev} and \code{strat}
for each site:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> head(moose_preds)
\end{CodeInput}
\begin{CodeOutput}
Simple feature collection with 6 features and 2 fields
Geometry type: POINT
Dimension:     XY
Bounding box:  xmin: 291839.8 ymin: 1436192 xmax: 401239.6 ymax: 1512103
Projected CRS: NAD83 / Alaska Albers
# A tibble: 6 x 3
   elev strat           geometry
  <dbl> <chr>        <POINT [m]>
1  143. L     (401239.6 1436192)
2  324. L     (352640.6 1490695)
3  158. L     (360954.9 1491590)
4  221. M     (291839.8 1466091)
5  209. M     (310991.9 1441630)
6  218. L     (304473.8 1512103)
\end{CodeOutput}
\end{CodeChunk}
```
## Model Fitting {#model-fitting}

SPGLMs in \pkg{spmodel} for point-referenced support are fit using the
\code{spglm()} function. The \code{spglm()} function requires four
arguments: \code{formula}, the relationship between the response and
explanatory variables; \code{family}, the response distribution assumed
for the response variable; \code{data}, the data frame that contains the
variables in \code{formula}, and \code{spcov_type}, the type of spatial
covariance. The \code{formula}, \code{family}, and \code{data} arguments
are familiar because they are the three required arguments to
\code{glm()} for nonspatial GLMs. So the transition from \code{glm()} to
\code{spglm()} simply requires one additional argument:
\code{spcov_type}. When \code{data} is not an \code{sf} object,
\code{spglm()} also requires the \code{xcoord} and \code{ycoord}
arguments, which indicate the columns in \code{data} that represent the
projected $x$~ and $y$~coordinates, respectively.

We use \code{spglm()} to fit a SPGLM (i.e.,~here, a spatial logistic
regression model) quantifying the effect of elevation and strata on
moose presence:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> spbin <- spglm(
+    formula = presence ~ elev + strat,
+    family = binomial,
+    data = moose,
+    spcov_type = "exponential"
+  )
\end{CodeInput}
\end{CodeChunk}
```
The \code{summary()} function returns a model summary with relevant
information like the function call, deviance residuals, a coefficients
table of fixed effects, the pseudo R-squared, spatial covariance
parameters, and the GLM dispersion parameter (fixed at one in logistic
regression):
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> summary(spbin)
\end{CodeInput}
\begin{CodeOutput}

Call:
spglm(formula = presence ~ elev + strat, family = binomial, data = moose, 
    spcov_type = "exponential")

Deviance Residuals:
    Min      1Q  Median      3Q     Max 
-1.7537 -0.7985  0.3467  0.7873  1.5767 

Coefficients (fixed):
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -2.474023   1.498874  -1.651 0.098823 .  
elev         0.006062   0.003540   1.712 0.086862 .  
stratM       1.441894   0.421669   3.419 0.000627 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Pseudo R-squared: 0.06276

Coefficients (exponential spatial covariance):
       de        ie     range 
5.244e+00 5.244e-04 4.214e+04 

Coefficients (Dispersion for binomial family):
dispersion 
         1 
\end{CodeOutput}
\end{CodeChunk}
```
The model provides some evidence that elevation is positively associated
with the log odds of moose presence (\(p~\)value \(\approx\) 0.087),
after controlling for strata. The model also provides strong evidence
that moose have a higher log odds of presence in the \code{"M"} strata
compared to the \code{"L"} strata (\(p~\)value \textless{} 0.001), after
controlling for elevation.

The fixed effects coefficients table from \code{summary()} is often of
primary scientific interest, but it is not immediately usable when
printed directly to the \proglang{R} console. The \code{tidy()} function
tidies this table, turning it into a data frame (i.e.,~a tibble) with
standard column names:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> tidy(spbin, conf.int = TRUE)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 3 x 7
  term        estimate std.error statistic  p.value conf.low conf.high
  <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
1 (Intercept) -2.47      1.50        -1.65 0.0988   -5.41e+0    0.464 
2 elev         0.00606   0.00354      1.71 0.0869   -8.77e-4    0.0130
3 stratM       1.44      0.422        3.42 0.000627  6.15e-1    2.27  
\end{CodeOutput}
\end{CodeChunk}
```
## Model Comparison {#model-comparison}

The strength of spatial covariance in the data affects how beneficial an
SPGLM is relative to a GLM. When the spatial covariance is strong, the
SPGLM should notably outperform the GLM. When the spatial covariance is
weak, the SPGLM and GLM should perform similarly. We can quantify the
benefits of incorporating spatial covariance for a particular data set
by comparing the fit of a SPGLM to a GLM. We can fit a GLM in
\pkg{spmodel} by specifying \code{spcov_type = "none"}:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> bin <- spglm(
+    formula = presence ~ elev + strat,
+    family = binomial,
+    data = moose,
+    spcov_type = "none"
+  )
R> tidy(bin)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 3 x 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -0.425      0.421      -1.01  0.313  
2 elev        -0.000317   0.00186    -0.171 0.865  
3 stratM       0.807      0.291       2.78  0.00549
\end{CodeOutput}
\begin{CodeInput}
R> bin_glm <- glm(
+    formula = presence ~ elev + strat,
+    family = binomial,
+    data = moose,
+  )
R> broom::tidy(bin_glm)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 3 x 5
  term         estimate std.error statistic p.value
  <chr>           <dbl>     <dbl>     <dbl>   <dbl>
1 (Intercept) -0.425      0.421      -1.01  0.313  
2 elev        -0.000317   0.00186    -0.171 0.865  
3 stratM       0.807      0.291       2.78  0.00549
\end{CodeOutput}
\end{CodeChunk}
```
Using \code{spglm()} instead of \code{glm()} ensures that \pkg{spmodel}
helper functions are available and that each of the \code{spglm()}
models uses the same hierarchical likelihood
(Equation~\ref{eq-marginal}):
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> glance(spbin)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 10
      n     p  npar value   AIC  AICc   BIC logLik deviance
  <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>
1   218     3     3  276.  282.  282.  292.  -138.     175.
# i 1 more variable: pseudo.r.squared <dbl>
\end{CodeOutput}
\begin{CodeInput}
R> glance(bin)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 10
      n     p  npar value   AIC  AICc   BIC logLik deviance
  <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>
1   218     3     0  307.  307.  307.  307.  -154.     294.
# i 1 more variable: pseudo.r.squared <dbl>
\end{CodeOutput}
\end{CodeChunk}
```
The likelihood-based statistics AIC, AICc, BIC, and deviance are much
lower for the SPGLM, indicating a better fit relative to the GLM. When 
using maximum likelihood estimation, the likelihoods from `spglm()`
and `glm()` are approximately equal:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> bin_ml <- update(bin, estmethod = "ml")
R> glance(bin_ml)[, c("logLik", "AIC", "BIC", "deviance")]
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 4
  logLik   AIC   BIC deviance
   <dbl> <dbl> <dbl>    <dbl>
1  -147.  300.  310.     294.
\end{CodeOutput}
\begin{CodeInput}
R> broom::glance(bin_glm)[, c("logLik", "AIC", "BIC", "deviance")]
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 4
  logLik   AIC   BIC deviance
   <dbl> <dbl> <dbl>    <dbl>
1  -147.  300.  310.     294.
\end{CodeOutput}
\end{CodeChunk}
```

Instead of relying on likelihood-based statistics, models can be
compared using a cross-validation procedure
[@james2013introduction]. The \code{loocv()} function performs
leave-one-out cross validation, comparing the predicted mean (on the
response scale) to the observed response variable for each hold-out
observation, recomputing estimates of \(\boldsymbol{\beta}\) in each
iteration. Performing leave-one-out cross validation tends to be more
computationally efficient than fitting the model, as leave-one-out cross
validation requires only one set of products involving the inverse
covariance matrix (a primary computational burden), while fitting
traditional models requires these products for each optimization
iteration. After performing leave-one-out cross validation, statistics
like bias, mean-squared-prediction error (MSPE), and the square root of
MSPE (RMSPE) can be used to evaluate models:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> loocv(spbin)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 3
       bias  MSPE RMSPE
      <dbl> <dbl> <dbl>
1 0.0000406 0.155 0.394
\end{CodeOutput}
\begin{CodeInput}
R> loocv(bin)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 3
      bias  MSPE RMSPE
     <dbl> <dbl> <dbl>
1 -1.23e-9 0.240 0.490
\end{CodeOutput}
\end{CodeChunk}
```

Both models have negligible bias, but the SPGLM has much lower MSPE and
RMSPE than the GLM, indicating more accurate predictions for these data.
The cross-validation metrics also prefer the SPGLM to the GLM. k-fold
cross validation is also
available, which uses five folds by default:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> kcv(spbin)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 3
     bias  MSPE RMSPE
    <dbl> <dbl> <dbl>
1 0.00252 0.158 0.398
\end{CodeOutput}
\begin{CodeInput}
R> kcv(bin)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 3
          bias  MSPE RMSPE
         <dbl> <dbl> <dbl>
1 0.0000000933 0.240 0.490
\end{CodeOutput}
\end{CodeChunk}
```

Again, the spatial model is preferred.

We may also want to compare the fit of two SPGLMs with different spatial
covariance structures. For example, we may want to compare the fit of an
SPGLM with the \texttt{"exponential"} spatial covariance to the fit of a
SPGLM with the \texttt{"gaussian"} spatial covariance:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> spbin2 <- update(spbin, spcov_type = "gaussian")
R> glances(spbin, spbin2)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 2 x 11
  model      n     p  npar value   AIC  AICc   BIC logLik deviance
  <chr>  <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>
1 spbin2   218     3     3  273.  279.  279.  289.  -136.     198.
2 spbin    218     3     3  276.  282.  282.  292.  -138.     175.
# i 1 more variable: pseudo.r.squared <dbl>
\end{CodeOutput}
\begin{CodeInput}
R> loocv(spbin)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 3
       bias  MSPE RMSPE
      <dbl> <dbl> <dbl>
1 0.0000406 0.155 0.394
\end{CodeOutput}
\begin{CodeInput}
R> loocv(spbin2)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 3
       bias  MSPE RMSPE
      <dbl> <dbl> <dbl>
1 -0.000237 0.146 0.382
\end{CodeOutput}
\end{CodeChunk}
```
The SPGLM with the \code{"exponential"} spatial covariance
(\code{spbin}) has a slightly lower (better) deviance but slightly
higher (worse) AIC, AICc, and BIC than the SPGLM with the
\code{"gaussian"} spatial covariance (\code{spbin2}). Both SPGLMs have
similar leave-one-out cross validation metrics, though the SPGLM with
the \code{"gaussian"} spatial covariance has slightly lower (better)
RMSPE. For practical purposes, these models fit similarly.

Frequently in spatial statistics, the difference in model fit between
the best spatial model and worst spatial model is much smaller than the
difference in model fit between the worst spatial model and the
nonspatial model, implying that accounting for some form of spatial
covariance is very beneficial. Two spatial covariance functions to
consider starting with are the exponential and Gaussian, which have
 quite different origin behaviors (Figure~\ref{fig-correlations}(b)), something
@stein1999interpolation argues is important to characterize
accurately.

## Model Diagnostics {#model-diagnostics}

\pkg{spmodel} provides a suite of tools for model diagnostics. One is
\code{augment()}, which augments the data used in the model with several
model diagnostics (introduced in Section~\ref{sec:diagnostics}):
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> augment(spbin)
\end{CodeInput}
\begin{CodeOutput}
Simple feature collection with 218 features and 8 fields
Geometry type: POINT
Dimension:     XY
Bounding box:  xmin: 269085 ymin: 1416151 xmax: 419057.4 ymax: 1541016
Projected CRS: NAD83 / Alaska Albers
# A tibble: 218 x 9
   presence  elev strat .fitted .resid    .hat  .cooksd .std.resid
 * <fct>    <dbl> <chr>   <dbl>  <dbl>   <dbl>    <dbl>      <dbl>
 1 0         469. L       -1.96 -0.513 0.0473  0.00457      -0.525
 2 0         362. L       -2.71 -0.358 0.0122  0.000535     -0.360
 3 0         173. M       -1.97 -0.511 0.00451 0.000397     -0.512
 4 0         280. L       -3.16 -0.288 0.00408 0.000114     -0.289
 5 0         620. L       -1.20 -0.725 0.167   0.0422       -0.794
 6 0         164. M       -1.72 -0.574 0.00532 0.000591     -0.576
 7 0         164. M       -1.61 -0.604 0.00574 0.000707     -0.606
 8 0         186. L       -2.51 -0.396 0.00438 0.000230     -0.396
 9 0         362. L       -1.89 -0.529 0.0238  0.00233      -0.536
10 0         430. L       -1.55 -0.621 0.0497  0.00706      -0.637
# i 208 more rows
# i 1 more variable: geometry <POINT [m]>
\end{CodeOutput}
\end{CodeChunk}
```
The fitted values (\code{.fitted}) can be returned on either the link
(\(\hat{\mathbf{w}}\)) or response (\(f^{-1}(\hat{\mathbf{w}})\)) scale
and the residuals (\code{.resid}) can be deviance, Pearson, or response
residuals. The default fitted values are on the link scale and the
default residuals are deviance residuals. Also returned by
\code{augment()} are the leverage (\code{.hat}), Cook's distance
(\code{.cooksd}), and standardized residuals (\code{.std.resid}). A
benefit of using \code{augment()} when \code{data} is an \code{sf}
object is that the output is also an \code{sf} object, which makes it
straightforward to create spatial diagnostic plots
(Figure~\ref{fig-sp-diagnostic}). Standard \proglang{R} helpers
(e.g.,~\code{fitted()}, \code{residuals()}) are also available to
extract model diagnostics from the model object.

\begin{figure}[t!]
\centering
\includegraphics[width = 1\linewidth, trim=0 20 0 20,clip]{figures/figure-4.pdf}
\caption{Moose presence model diagnostics, including leverage values (left) and standardized residuals (right).}
\label{fig-sp-diagnostic}
\end{figure}

The \code{plot()} function can also be used to return similar
diagnostics as from \code{lm()} and \code{glm()}, with additional tools
for diagnosing spatial covariance. For example, we can inspect Cook's
distance values and the fitted spatial covariance as a function of
distance with (Figure~\ref{fig-sp-diagnostic2}):
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> plot(spbin, which = c(4, 7))
\end{CodeInput}
\end{CodeChunk}
```
\begin{figure}[t!]
\centering
\includegraphics[width = 1\linewidth, trim=0 0 0 20,clip]{figures/figure-5.pdf}
\caption{Moose presence model diagnostics, including Cook's distance (left) and the fitted spatial covariance as a function of distance (right).}
\label{fig-sp-diagnostic2}
\end{figure}
The \code{varcomp()} function partitions model variability into several
different components, helping to elucidate the model's structure:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> varcomp(spbin)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 3 x 2
  varcomp            proportion
  <chr>                   <dbl>
1 Covariates (PR-sq)  0.0628   
2 de                  0.937    
3 ie                  0.0000937
\end{CodeOutput}
\end{CodeChunk}
```
The pseudo R-squared (\(PR^2\)) is reported in the first row. The
remaining variability (\(1 - PR^2\)) is allocated proportionally to
\code{de} and \code{ie} according to \(\sigma^2_{de}\) and
\(\sigma^2_{ie}\). This variability partitioning is a useful tool that
helps quantify how much the explanatory variables, residual spatial
variance, and residual nonspatial variance contribute to model fit; as
with \(PR^2\), it should not be used for model comparison, but rather as
a helpful model diagnostic.

## Prediction {#prediction}

We can predict the probability of moose presence at the locations in
\code{moose_preds} using \code{predict()}:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> predict(spbin, newdata = moose_preds)[1:5]
\end{CodeInput}
\begin{CodeOutput}
          1           2           3           4           5 
 0.06989524 -0.79411725 -1.61106131 -0.83935088  1.39081663 
\end{CodeOutput}
\end{CodeChunk}
```
By default, predictions are returned on the link scale, but this can be
changed to the response scale via \code{type}:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> predict(spbin, newdata = moose_preds, type = "response")[1:5]
\end{CodeInput}
\begin{CodeOutput}
        1         2         3         4         5 
0.5174667 0.3112853 0.1664413 0.3016715 0.8007226 
\end{CodeOutput}
\end{CodeChunk}
```
\begin{figure}[t!]
\centering
\includegraphics[width = 0.70\linewidth]{figures/figure-6.pdf}
\caption{Moose presence probability fitted values ($f^{-1}(\hat{\mathbf{w}})$) and predictions. Fitted values are represented by circles and predictions by triangles.}
\label{fig-moose-fit}
\end{figure}
Moose are far more common in the eastern and southwestern portion of the
domain (Figure~\ref{fig-moose-fit}). Prediction intervals for the
log odds of moose presence (on the link scale) are returned by
supplying \code{interval}:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> predict(spbin, newdata = moose_preds, interval = "prediction")[1:5, ]
\end{CodeInput}
\begin{CodeOutput}
          fit        lwr       upr
1  0.06989524 -2.0456011 2.1853915
2 -0.79411725 -3.4952212 1.9069867
3 -1.61106131 -4.1166330 0.8945103
4 -0.83935088 -3.0915348 1.4128330
5  1.39081663 -0.7720673 3.5537005
\end{CodeOutput}
\end{CodeChunk}
```

Predictions intervals on the response scale (\code{type = "response"}) are either transformed from the link scale (the default) or derived directly using the delta method (\code{delta = TRUE}) [@oehlert1992note; @ver2012invented]:

We can alternatively use \code{augment()} to augment the prediction data
with predictions. Arguments to \code{predict()} can also be passed to
\code{augment()}:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> augment(spbin, newdata = moose_preds, interval = "prediction")
\end{CodeInput}
\begin{CodeOutput}
Simple feature collection with 100 features and 5 fields
Geometry type: POINT
Dimension:     XY
Bounding box:  xmin: 269386.2 ymin: 1418453 xmax: 419976.2 ymax: 1541763
Projected CRS: NAD83 / Alaska Albers
# A tibble: 100 x 6
    elev strat .fitted .lower  .upper           geometry
 * <dbl> <chr>   <dbl>  <dbl>   <dbl>        <POINT [m]>
 1  143. L      0.0699 -2.05   2.19   (401239.6 1436192)
 2  324. L     -0.794  -3.50   1.91   (352640.6 1490695)
 3  158. L     -1.61   -4.12   0.895  (360954.9 1491590)
 4  221. M     -0.839  -3.09   1.41   (291839.8 1466091)
 5  209. M      1.39   -0.772  3.55   (310991.9 1441630)
 6  218. L     -2.60   -5.22   0.0234 (304473.8 1512103)
 7  127. L     -2.74   -5.26  -0.216  (339011.1 1459318)
 8  122. L     -2.33   -4.77   0.0978 (342827.3 1463452)
 9  191  L     -1.17   -4.02   1.68   (284453.8 1502837)
10  105. L     -0.907  -3.06   1.25   (391343.9 1483791)
# i 90 more rows
\end{CodeOutput}
\end{CodeChunk}
```
By using \code{augment()} when \code{newdata} is a \code{sf} object,
predictions and their corresponding uncertainties are readily available
for spatial mapping (Figure~\ref{fig-moose-int}).
\begin{figure}[t!]
\centering
\includegraphics[width = 1\linewidth, trim=0 20 0 20,clip]{figures/figure-7.pdf}
\caption{Moose presence 95\% prediction interval lower bounds (left) and upper bounds (right).}
\label{fig-moose-int}
\end{figure}

# Additional applications {#sec-applications2}

Throughout the remainder of this section, we briefly highlight some
additional \pkg{spmodel} capabilities for SPGLMs. In
Section~\ref{sec-moose-count}, we fit Poisson and negative binomial
models with and without geometric anisotropy for the point-referenced
moose count data. In Section~\ref{sec-lake}, we fit a Gamma model to
the point-referenced lake conductivity data, showing how to fit a model
with a partition factor, perform a spatial analysis of variance (ANOVA)
using general linear hypothesis tests [@schabenberger2017statistical],
and estimate contrasts for models with interactions. In
Section~\ref{sec-seal}, we fit a binomial model to the areal harbor
seal trend data with a nonspatial random effect. Finally in
Section~\ref{sec-texas}, we fit beta models to Texas voter turnout
data, which can be modeled using point-referenced or areal support, and
use maximum likelihood to compare two models with different explanatory
variables. Table~\ref{tab:additional} outlines, for each
application, the section number, data set, family (i.e.,~response
distribution), geometry type (point-referenced or areal support), and
additional \pkg{spmodel} features highlighted.
\begin{table}
    \centering
    \begin{tabular}{|l|llll|}
    \hline
    Section & Data & Family & Geometry & Additional Features \\
    \hline
    \hline
    4.1 & Moose Counts & Poisson & Point & Geometric Anisotropy  \\
    &  & NBinomial &  &  \\
    \hline
    4.2 & Lake Conductivity & Gamma & Point & Partition Factor  \\
    & & & & ANOVA \\
    & & & & Contrasts \\
    \hline
    4.3 & Harbor Seals & Binomial & Areal & Nonspatial Random Effects  \\
    \hline
    4.4 & Texas Voter Turnout & Beta & Point & Likelihood-Ratio Test  \\
    & & & Areal & \\
    \hline 
    \end{tabular}
\caption{Section number, data set, family, geometry type, and additional features for each application.}
\label{tab:additional}
\end{table}
## Modeling moose counts in Alaska, USA {#sec-moose-count}

In addition to moose presence, moose counts are also recorded in
\code{moose} (Figure~\ref{fig-moose-data-count}). The Poisson and
negative binomial response distributions can be used to model SPGLMs for
count data. The Poisson distribution mean is equal to its variance,
while the negative binomial has an extra parameter to accommodate
overdispersion (where the variance is larger than the mean). Using a
spherical spatial covariance function, we may fit both a Poisson and
negative binomial SPGLM by changing the \code{family} argument:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> sppois <- spglm(
+    formula = count ~ elev + strat,
+    family = poisson,
+    data = moose,
+    spcov_type = "spherical"
+  )
R> spnb <- update(sppois, family = nbinomial)
\end{CodeInput}
\end{CodeChunk}
```
\begin{figure}[t!]
\centering
\includegraphics[width = 0.70\linewidth, trim=0 0 0 0,clip]{figures/figure-8.pdf}
\caption{Moose counts in Alaska. Circles represent moose counts (based on color) and triangles represent locations at which mean count predictions are desired.}
\label{fig-moose-data-count}
\end{figure}
Because the Poisson and negative binomial distributions have the same
response support (nonnegative integers), we can compare them using AIC,
AICc, or BIC [@burnham2002model]:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> BIC(sppois, spnb)
\end{CodeInput}
\begin{CodeOutput}
       df      BIC
sppois  3 943.7197
spnb    4 941.8881
\end{CodeOutput}
\end{CodeChunk}
```
Implicit in our spatial covariance functions thus far has been an
assumption of geometric isotropy. A spatial covariance function is
geometrically isotropic if it decays with distance at the same rate in
all directions (Figure~\ref{fig-tropy}; left). A spatial covariance
is geometrically anisotropic if it decays with distance at different
rates in different directions (Figure~\ref{fig-tropy}; right).
Geometric anisotropy is formally incorporated by rotating and scaling
original coordinates, yielding transformed coordinates that are
geometrically isotropic: 


\begin{align}
  \begin{bmatrix}
    x^* \\
    y^*
  \end{bmatrix} = 
  \begin{bmatrix}
    1 & 0 \\
    0 & 1 / \omega
  \end{bmatrix}
  \begin{bmatrix}
    \cos(\alpha) & \sin(\alpha) \\
    -\sin(\alpha) & \cos(\alpha)
  \end{bmatrix}  
  \begin{bmatrix}
    x \\
    y
  \end{bmatrix}.
\end{align}

 
The parameters \(\omega\) and \(\alpha\) control the
scaling and rotation, respectively, of the major and minor axes of a
level curve of equal spatial covariance (Figure~\ref{fig-tropy}).
Using these transformed coordinates, the partial sill
(\(\sigma^2_{de}\)), nugget (\(\sigma^2_{ie}\)), and range (\(\phi\))
parameters are estimated. We accommodate geometric anisotropy by
supplying \code{anisotropy}:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> sppois_anis <- update(sppois, anisotropy = TRUE)
R> spnb_anis <- update(spnb, anisotropy = TRUE)
\end{CodeInput}
\end{CodeChunk}
```
According to BIC, the spatial negative binomial model with geometric
anisotropy performs best:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> BIC(sppois, spnb, sppois_anis, spnb_anis)
\end{CodeInput}
\begin{CodeOutput}
            df      BIC
sppois       3 943.7197
spnb         4 941.8881
sppois_anis  5 940.4199
spnb_anis    6 938.9993
\end{CodeOutput}
\end{CodeChunk}
```
The \code{plot()} function can be used to visualize the anisotropy
(Figure~\ref{fig-tropy}):
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> plot(spnb, which = 8)
R> plot(spnb_anis, which = 8)
\end{CodeInput}
\end{CodeChunk}
```
From Figure~\ref{fig-tropy}, the spatial covariance is strongest in
a northwest-southeast direction (2.729 radians) and weakest in the
northeast-southwest direction (0.413 radians), which is intuitive given
the similar patterns in moose counts from
Figure~\ref{fig-moose-data-count}.
\begin{figure}[t!]
\centering
\includegraphics[width = 1\linewidth, trim=0 0 0 20,clip]{figures/figure-9.pdf}
\caption{Level curves of equal spatial covariance for the negative binomial moose count models. The ellipse is centered at zero distance in the $x$~direction and $y$~direction, and points along the ellipse have equal levels of spatial covariance.  In the isotropic level curve (left), spatial covariance decays equally in all directions. In the anisotropic level curve (right), spatial covariance decays fastest in the northeast-southwest direction (0.413 radians) and slowest in the northwest-southeast direction (2.729 radians).}
\label{fig-tropy}
\end{figure}

## Modeling lake conductivity in Southwest, USA {#sec-lake}

The \code{lake} data in \pkg{spmodel} contains climate and chemical
data for several lakes in four southwestern states in the United States:
Arizona, Colorado, Nevada, and Utah. We desire an SPGLM that
characterizes the effect of temperature, state, and lake origin (whether
the lake is naturally occurring or human made) on lake conductivity.
Conductivity is a measure of dissolved ions (measured here in water),
which is important for various physical, chemical, and biological
processes. Chemical data are often heavily right-skewed, so we model
them using an SPGLM assuming a Gamma distribution for the response. The
\code{cond} variable in \code{lake} measures conductivity on its
original scale:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> spgam <- spglm(
+   formula = cond ~ temp * state + origin, 
+    family = "Gamma",
+    data = lake,
+    spcov_type = "cauchy",
+    partition_factor = ~ year
+  )
\end{CodeInput}
\end{CodeChunk}
```
We model conductivity as a function of temperature, state, and lake
origin, and we allow the effect of temperature to vary by state
(\code{temp:state} interaction). The \code{year} partition factor
(specified via \code{partition_factor}) restricts spatial covariance to
be nonzero only for observations sampled during the same year. Data were
collected in 2012 and 2017, so this partition factor assumes
observations in 2012 are independent of observations in 2017. While we
used the partition factor here illustratively, more generally, the
utility of partition factors can be highly context dependent.

When categorical variables have more than two levels, the default
reference group contrasts are not well-suited to assess the variable's
overall significance:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> summary(spgam)
\end{CodeInput}
\begin{CodeOutput}

Call:
spglm(formula = cond ~ temp * state + origin, family = "Gamma", 
    data = lake, spcov_type = "cauchy", partition_factor = ~year)

Deviance Residuals:
     Min       1Q   Median       3Q      Max 
-1.25870 -0.18421 -0.03273  0.16951  1.00464 

Coefficients (fixed):
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)    3.56516    0.50509   7.058 1.68e-12 ***
temp           0.15281    0.03015   5.068 4.02e-07 ***
stateCO       -0.01340    0.56220  -0.024  0.98098    
stateNV        0.77487    0.66791   1.160  0.24600    
stateUT       -0.17698    0.55945  -0.316  0.75174    
originNATURAL  0.08565    0.22142   0.387  0.69890    
temp:stateCO   0.13488    0.04817   2.800  0.00511 ** 
temp:stateNV   0.01781    0.05808   0.307  0.75911    
temp:stateUT   0.19723    0.04833   4.081 4.49e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Pseudo R-squared: 0.7093

Coefficients (cauchy spatial covariance):
       de        ie     range     extra 
2.722e-02 3.218e-01 6.062e+06 2.663e-01 

Coefficients (Dispersion for Gamma family):
dispersion 
     4.095 
\end{CodeOutput}
\end{CodeChunk}
```
A more effective approach is to use an analysis of variance (ANOVA),
which is well-suited to assess the overall significance of each
variable:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> anova(spgam)
\end{CodeInput}
\begin{CodeOutput}
Analysis of Variance Table

Response: cond
            Df    Chi2 Pr(>Chi2)    
(Intercept)  1 49.8221 1.683e-12 ***
temp         1 25.6843 4.021e-07 ***
state        3  3.0926 0.3775687    
origin       1  0.1496 0.6989043    
temp:state   3 19.3168 0.0002351 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
\end{CodeOutput}
\end{CodeChunk}
```
The main effect for temperature and the temperature by state interaction
are highly significant (\(p~\)value \textless{} 0.001), while the main
effects for state and lake origin are not significant.

Variance inflation factors assess the degree to which standard errors
\(\hat{\boldsymbol{\beta}}\) are inflated due to covariance among the
columns of \(\mathbf{X}\). Generalized variance inflation factors can
capture the variance inflation for subsets of \(\mathbf{X}\) that may
include categorical variables with more than two levels
[@fox1992generalized]:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> car::vif(spgam)
\end{CodeInput}
\begin{CodeOutput}
                 GVIF Df GVIF^(1/(2*Df))
temp         4.725774  1        2.173885
state      126.219690  3        2.239690
origin       1.270228  1        1.127044
temp:state  75.860886  3        2.057483
\end{CodeOutput}
\end{CodeChunk}
```
The GVIF\(^{1/2df}\) values for \code{temp}, \code{state}, and
\code{temp:state} are just greater than two, which suggests moderate
multicollinearity for these terms -- unsurprising given the
\code{temp:state} interaction in the model. The GVIF\(^{1/2df}\) for
\code{origin} is close to one, which suggests little to no
multicollinearity for this term.

Because of the interaction between \code{temp} and \code{state},
contrasts that assess mean differences among states should condition
upon a specific temperature value. By default, \pkg{emmeans} uses the
mean temperature value (here, 7.63) to assess contrasts:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> pairs(emmeans::emmeans(spgam, ~ state | temp))
\end{CodeInput}
\begin{CodeOutput}
temp = 7.63:
 contrast estimate    SE  df z.ratio p.value
 AZ - CO    -1.016 0.337 Inf  -3.015  0.0137
 AZ - NV    -0.911 0.348 Inf  -2.614  0.0443
 AZ - UT    -1.329 0.327 Inf  -4.068  0.0003
 CO - NV     0.106 0.258 Inf   0.410  0.9768
 CO - UT    -0.312 0.224 Inf  -1.395  0.5022
 NV - UT    -0.418 0.245 Inf  -1.705  0.3210

Results are averaged over the levels of: origin 
Degrees-of-freedom method: asymptotic 
Results are given on the log (not the response) scale. 
P value adjustment: tukey method for comparing a family of 4 estimates 
\end{CodeOutput}
\end{CodeChunk}
```
Again, because of the interaction between \code{temp} and \code{state},
we should assess temperature trends separately for each state:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> emmeans::emtrends(spgam, ~ state, var = "temp")
\end{CodeInput}
\begin{CodeOutput}
 state temp.trend     SE  df asymp.LCL asymp.UCL
 AZ         0.153 0.0302 Inf    0.0937     0.212
 CO         0.288 0.0370 Inf    0.2151     0.360
 NV         0.171 0.0502 Inf    0.0722     0.269
 UT         0.350 0.0369 Inf    0.2777     0.422

Results are averaged over the levels of: origin 
Degrees-of-freedom method: asymptotic 
Confidence level used: 0.95 
\end{CodeOutput}
\end{CodeChunk}
```
## Modeling harbor seal trends in Alaska, USA {#sec-seal}

The \code{seal} data in \pkg{spmodel} contains harbor seal abundance
trends for two different harbor seal stocks (genetically distinct
populations). While the \code{moose} and \code{lake} data have
point-referenced support, the \code{seal} data have areal support. Each
polygon in the \code{seal} data represents a distinct harbor seal
haulout region (Figure~\ref{fig-seal}). A haulout region is an area
of coastal rocks that harbor seals go to rest, molt, and give birth.

\begin{figure}[t!]
\centering
\includegraphics[width = 1\linewidth, trim=0 10 0 10,clip]{figures/figure-10.pdf}
\caption{Seal trend distribution in Alaska. Observed and missing seal polygons by stock (left) and observed log seal trends (right).}
\label{fig-seal}
\end{figure}

For each polygon, a Poisson regression was used to quantify the mean
trend in abundance over approximately 30 years [@ver2018spatial]. The
\code{trend} variable is the ratio of mean abundance at the end of the
period to mean abundance at the beginning; values below one indicate a
decrease, while values above one indicate an increase. We use a binomial
SPGLM to quantify the likelihood that mean abundance trends are
decreasing:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> seal$is_decreasing <- seal$trend < 1
R> spbin <- spgautor(
+    formula = is_decreasing ~ 1,
+    family = binomial,
+    data = seal,
+    spcov_type = "car",
+    random = ~ stock
+  )
\end{CodeInput}
\end{CodeChunk}
```
To model spatial dependence, we used a conditional autoregressive
function. Conditional and simultaneous autoregressive functions
characterize spatial distance through neighborhood relationships (rather
than Euclidean distance) and have \code{spcov_type} values of
\code{"car"} and \code{"sar"}, respectively. By default, Queen's
distance is used to determine whether two sites are neighbors, though
custom neighborhood matrices can be passed via the \code{W} argument.
Row standardization is also assumed by default; this can be changed via
the \code{row_st} argument. Using the \code{random} argument, we also
specified a nonspatial random effect for seal stock, which implies seals
belonging to the same stock share extra covariance. The \code{random}
argument uses similar syntax as \pkg{lme4} [@bates2015lme4] and
\pkg{nlme} [@pinheiro2006mixed] to specify nonspatial random
effects.

Tidying the model reveals the estimates and confidence intervals on the
log odds scale:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> tidy(spbin, conf.int = TRUE)
\end{CodeInput}
\begin{CodeOutput}
# A tibble: 1 x 7
  term        estimate std.error statistic p.value conf.low conf.high
  <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
1 (Intercept)    0.335     0.626     0.534   0.593   -0.893      1.56
\end{CodeOutput}
\end{CodeChunk}
```
Back-transforming the confidence interval to the probability scale
yields:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> emmeans(spbin, ~ 1, type = "response")
\end{CodeInput}
\begin{CodeOutput}
 1        prob    SE  df asymp.LCL asymp.UCL
 overall 0.583 0.152 Inf     0.291     0.827

Degrees-of-freedom method: asymptotic 
Confidence level used: 0.95 
Intervals are back-transformed from the logit scale 
\end{CodeOutput}
\end{CodeChunk}
```
The \code{SE} column is the standard error on the response scale
obtained from the delta method.

In contrast to point-referenced data, prediction locations for areal
data must be specified at the time of model fitting, as they affect the
spatial covariance function's neighborhood structure. Prediction
locations whose response values have an \code{NA} (i.e.,~missing) value
are converted into a \code{newdata} object that is stored in the model
output. For example, rows one and nine are locations without seal
trends, meaning they are not used in model fitting but are desired for
prediction:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> seal
\end{CodeInput}
\begin{CodeOutput}
Simple feature collection with 149 features and 5 fields
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 913618.8 ymin: 855730.2 xmax: 1221859 ymax: 1145054
Projected CRS: NAD83 / Alaska Albers
# A tibble: 149 x 6
   log_trend  trend stock                       geometry is_decreasing
 *     <dbl>  <dbl> <fct>                  <POLYGON [m]> <lgl>        
 1  NA       NA     8     ((1035002 1054710, 1035002 10~ NA           
 2  -0.282    0.754 8     ((1037002 1039492, 1037006 10~ TRUE         
 3  -0.00121  0.999 8     ((1070158 1030216, 1070185 10~ TRUE         
 4   0.0354   1.04  8     ((1054906 1034826, 1054931 10~ FALSE        
 5  -0.0160   0.984 8     ((1025142 1056940, 1025184 10~ TRUE         
 6   0.0872   1.09  8     ((1026035 1044623, 1026037 10~ FALSE        
 7  -0.266    0.767 8     ((1100345 1060709, 1100287 10~ TRUE         
 8   0.0743   1.08  8     ((1030247 1029637, 1030248 10~ FALSE        
 9  NA       NA     8     ((1043093 1020553, 1043097 10~ NA           
10  -0.00961  0.990 8     ((1116002 1024542, 1116002 10~ TRUE         
# i 139 more rows
# i 1 more variable: sampled <fct>
\end{CodeOutput}
\end{CodeChunk}
```
Then, \code{predict()} can be called without having to specify
\code{newdata}:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> predict(spbin, type = "response", interval = "prediction")[1:5, ]
\end{CodeInput}
\begin{CodeOutput}
         fit       lwr       upr
1  0.6790592 0.3810180 0.8791211
9  0.5859764 0.2358680 0.8664793
13 0.6136317 0.2888352 0.8613150
15 0.5976789 0.2831359 0.8482017
18 0.6334075 0.3281146 0.8594164
\end{CodeOutput}
\end{CodeChunk}
```
We could have alternatively used a (geostatistical) SPGLM via
\code{spglm()}. When areal data are used with \code{spglm()}, the
centroids of each polygon are used as the point-referenced coordinates.

## Modeling voter turnout in Texas, USA {#sec-texas}

\begin{figure}[t!]
\centering
\includegraphics[width = 0.70\linewidth,trim=0 8 0 0,clip]{figures/figure-11.pdf}
\caption{Proportion of voter turnout in Texas for the 1980 presidential election. Circles represent voter turnout (based on color) and triangles represent locations at which voter turnout predictions are desired.}
\label{fig-texas}
\end{figure}

The \code{texas} data in \pkg{spmodel} contains voter turnout data for
Texas counties in the 1980 United States Presidential Election
[@bivand2024spdata]. The data have point-referenced support, with
polygon centroids representing the spatial location of each county
(Figure~\ref{fig-texas}). Beta regression is a GLM used to model
rate and proportion data in the (0, 1) interval
[@ferrari2004beta; @cribari2010beta]. We model voter turnout rates
as a function of mean log income of county residents using a
geostatistical SPGLM assuming a beta distributed response variable:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> spbeta_geo <- spglm(
+    formula = turnout ~ log_income,
+    family = "beta", 
+    data = texas,
+    spcov_type = "matern"
+  )
\end{CodeInput}
\end{CodeChunk}
```
Alternatively, we could fit an autoregressive SPGLM by assuming the data
have areal support and constructing a neighborhood matrix that treats
counties as neighbors if the distance between their centroids is less
than \code{cutoff}:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> spbeta_auto <- spgautor(
+    formula = turnout ~ log_income,
+    family = "beta", 
+    data = texas,
+    spcov_type = "car",
+    cutoff = 1e5
+  )
\end{CodeInput}
\end{CodeChunk}
```
According to AIC, the geostatistical SPGLM is preferred:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> AIC(spbeta_geo, spbeta_auto)
\end{CodeInput}
\begin{CodeOutput}
            df       AIC
spbeta_geo   5 -465.8884
spbeta_auto  3 -443.4700
\end{CodeOutput}
\end{CodeChunk}
```
The default estimation method in \pkg{spmodel} for SPGLMs is restricted
maximum likelihood (REML), while maximum likelihood (ML) can also be
used. A benefit of REML is that it can yield unbiased estimates of
covariance parameters [@cressie1993asymptotic]. A drawback of REML
is that likelihood-based statistics are often considered invalid when
the models have different explanatory variable or fixed effect
structures [@wolfinger1993covariance], though
others, including @gurka2006selecting and @kubokawa2011conditional,
provide some evidence to the contrary. In
contrast to REML estimators, ML estimators are generally biased for
covariance parameters, though in practice this bias tends to be small.
Moreover, when using ML, likelihood-based comparisons are valid for
models with different explanatory variable or fixed effect structures.
Using ML, we can evaluate the significance of log income on voter
turnout using a likelihood ratio test (for nested models):
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> spbeta_full_ml <- update(spbeta_geo, estmethod = "ml")
R> spbeta_reduced_ml <- update(spbeta_full_ml, formula = turnout ~ 1)
R> anova(spbeta_full_ml, spbeta_reduced_ml)
\end{CodeInput}
\begin{CodeOutput}
Likelihood Ratio Test

Response: turnout
                                    Df  Chi2 Pr(>Chi2)    
spbeta_reduced_ml vs spbeta_full_ml  1 19.17 1.196e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
\end{CodeOutput}
\end{CodeChunk}
```
The likelihood ratio test provides strong evidence that log income is
significantly related to voter turnout (\(p~\)value \textless{} 0.001).
Alternatively, we could have instead used a different likelihood-based
statistic like AIC:
```{=latex}
\begin{CodeChunk}
\begin{CodeInput}
R> AIC(spbeta_full_ml, spbeta_reduced_ml)
\end{CodeInput}
\begin{CodeOutput}
                  df       AIC
spbeta_full_ml     7 -467.6174
spbeta_reduced_ml  6 -450.4478
\end{CodeOutput}
\end{CodeChunk}
```
The AIC also prefers the full model, suggesting that log income is
important for predicting voter turnout.

# Discussion {#sec-discussion}

SPGLMs are fit in \pkg{spmodel} using a novel application of the Laplace
approximation. Under restricted maximum likelihood (REML), the default
estimation method, it marginalizes over both the latent (i.e., unobserved)
random effects and the fixed effects. Maximum likelihood (ML) is also
available and marginalizes over the latent random effects while retaining
the fixed effects. \pkg{spmodel}'s
\code{spglm()} (for point-referenced support) and \code{spgautor()} (for
areal support) fit SPGLMs that are similar in structure and syntax as
base \proglang{R}'s \code{glm()} function, easing the transition for
practitioners from GLMs to SPGLMs. The \code{spglm()} and
\code{spgautor()} functions support six response distributions for
binary, count, and skewed data and 20 spatial covariance functions.
\pkg{spmodel} has a suite of tools for data visualization, inference,
model diagnostics, and prediction, providing a framework that can be
used for all stages of a data analysis. There are many additional
\pkg{spmodel} features that are not covered here, including fitting
multiple models simultaneously, fixing spatial covariance and dispersion
parameters at known values, fitting models to large non-Gaussian data
having thousands of observations via spatial indexing
[@ver2023indexing], conditionally simulating realized surfaces based on a model fit [@zimmerman2024spatial], simulating
spatially dependent data (e.g.,~\code{spbinom()}, \code{sprpois()},
etc.), and more. Further details are provided by
\url{https://CRAN.R-project.org/package=spmodel} and links therein.

# Data and code availability {.unnumbered}
The results in this manuscript were obtained using  \proglang{R} 4.6.1 with the
\pkg{spmodel}	0.14.0 package. Figures were created using the {ggplot2}
4.0.3 package [@wickham2016ggplot2] and base \proglang{R}.

# Acknowledgments {.unnumbered}
We would like to genuinely thank the associate editor, anonymous
reviewers, and editorial staff for significant support and feedback that
greatly improved the manuscript.

The views expressed in this article are those of the author(s) and do
not necessarily represent the views or policies of the U.S. government,
U.S. Environmental Protection Agency or the National Oceanic and
Atmospheric Administration. Mention of trade names or commercial
products does not constitute endorsement or recommendation for use.

\bibliography{ref.bib}


