# spmodel.glm. manuscript

### Supplement to "Spatial GeneralizedLinear Models in R using spmodel"

##### Michael Dumelle<sup>1</sup>, Jay M. Ver Hoef<sup>2</sup>, Matt Higham<sup>3</sup>

##### <sup>1</sup>United States Environmental Protection Agency, Corvallis, OR, USA
##### <sup>2</sup>NOAA Fisheries (NMFS) Alaska Fisheries Science Center, Marine Mammal Laboratory, Seattle, WA, USA
##### <sup>3</sup>St. Lawrence University Department of Math, Computer Science, and Statistics, Canton, NY, USA

##### For correspondence, please email Michael Dumelle at Dumelle.Michael@epa.gov

### Abstract

  Non-Gaussian data are common in practice and include binary, count, skewed, and proportion data types. Often, non-Gaussian data are modeled using a generalized linear model (GLM). GLMs typically assume that observations are independent of one another. This is an impractical assumption for spatial data, as nearby observations tend to be more similar than distant ones. The `spmodel` package in **R** provides a suite of tools for fitting spatial generalized linear models (SPGLMs) to non-Gaussian data and making spatial predictions (i.e., Kriging). SPGLMs for point-referenced (x- and y-coordinates) support are fit using the `spglm()` function, while SPGLMs for areal (lattice, polygon) support are fit using the `spgautor()` function. Both `spglm()` and `spgautor()` maximize a novel Laplace likelihood which marginalizes over the model's fixed effects and latent mean while formally incorporating spatial covariance among observations. The inputs and outputs of `spglm()` and `spgautor()` closely resemble the `glm()` function from base **R**, easing the transition from GLMs to SPGLMs. `spmodel` provides and builds upon several commonly used helper functions for model building like `summary()`, `plot()`, and `fitted()`, among others. Spatial predictions of the latent mean at unobserved locations are obtained using `predict()` or `augment()`. `spmodel` accommodates myriad advanced modeling features like geometric anisotrpoy, nonspatial random effects, analysis of variance, and more. Throughout, we use `spmodel` to fit SPGLMs to moose presence and counts in Alaska, United States (US), skewed conductivity data in the Southwestern US, harbor seal abundance trends in Alaska, US, and voter turnout rates in Texas, US.

### Package Overview

This supplementary R package contains all files used in the creation of this document. Next we discuss how to use the package to access these files.

### Installation

To install the supplementary R package, run
```r
remotes::install_github("USEPA/spmodel.glm.manuscript", ref = "main", dependencies = TRUE)
```

The package must be installed to view any of the files we discuss throughout this `README` on your machine. This package does not have to be installed to view any of these files if you want to look at them interactively using this GitHub repository.

### Data Availability

All data are available upon download of the **spmodel** **R** package.

### Manuscript

The files required to reproduce the manuscript are available at the file path found by running
```r
system.file("manuscript", package = "spmodel.glm.manuscript")
```

### Figures

Figures are available at the file path found by running
```r
system.file("manuscript/figures", package = "spmodel.glm.manuscript")
```

### Replication Code

Replication code is available at the file path found by running
```r
system.file("manuscript/code", package = "spmodel.glm.manuscript")
```

### Disclaimer

The views expressed in this article are those of the author(s) and do not necessarily represent the views or policies of the U.S. government, U.S. Environmental Protection Agency or the National Oceanic and Atmospheric Administration. Mention of trade names or commercial products does not constitute endorsement or recommendation for use.
