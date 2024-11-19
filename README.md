# spmodel.glm. manuscript

### Supplement to "Spatial generalized linear models in R using spmodel"

##### Michael Dumelle<sup>1</sup>, Matt Higham<sup>2</sup>, Jay M. Ver Hoef<sup>3</sup>

##### <sup>1</sup>United States Environmental Protection Agency, Corvallis, OR, USA
##### <sup>2</sup>St. Lawrence University Department of Math, Computer Science, and Statistics, Canton, NY, USA
##### <sup>3</sup>NOAA Fisheries (NMFS) Alaska Fisheries Science Center, Marine Mammal Laboratory, Seattle, WA, USA

##### For correspondence, please email Michael Dumelle at Dumelle.Michael@epa.gov

### Abstract

  Generalized linear models (GLMs) describe a non-normal response variable that may be binary, count, skewed, or a proportion. Typically, observations in a GLM are assumed independent of one another. For spatial data, this independence assumption is impractical, as nearby locations tend to be more similar than locations far apart. The **spmodel** **R** package provides tools to fit GLMs that incorporate spatial autocorrelation (i.e., spatial generalized linear models, or SPGLMs). SPGLMs are fit in **spmodel** using a novel application of the Laplace approximation via `spglm()` for point-referenced data or `spgautor()` for areal (i.e., lattice), data. `spglm()` and `spgautor()` closely resemble `glm()` from base **R** but include arguments that control the spatial autocorrelation structure. **spmodel** has many helper functions for model inspection and diagnostics, some of which leverage other **R** packages like **broom** and **emmeans**. It is also possible to use fitted models to make predictions of the latent spatial-mean process at unobserved locations. **spmodel** provides many additional features like accommodating geometric anisotropy and nonspatial random effects, simulating spatially autocorrelated data, and more. Here we use **spmodel** to illustrate the modeling of binary, count, skewed and proportion response variables from several point-referenced and areal data sets. 

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

The views expressed in this manuscript are those of the authors and do not necessarily represent the views or policies of the U.S. Environmental Protection Agency. Any mention of trade names, products, or services does not imply an endorsement by the U.S. government or the U.S. Environmental Protection Agency. The U.S. Environmental Protection Agency does not endorse any commercial products, services, or enterprises.
