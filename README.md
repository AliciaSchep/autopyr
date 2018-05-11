[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

# autopyr

**This is a highly experimental, proof-of-concept package for generating a pipe-able r api from a python package using the powers of reticulate**

The goal of autopyr is to enable auto-generating an r api from a python module. The package so far has been designed with one particular python module in mind: [Altair](https://altair-viz.github.io/index.html) and the [altair](https://github.com/ijlyttle/altair) r package. See [discussion in altair repo](https://github.com/ijlyttle/altair/issues/15) for more on goals.  Note: The type of api generated may not be appropriate for many python modules; Altair class methods seem to generate a new object, rather than modifying the object, which is different from many Python classes...

## Example

``` r
library(altair) # Loads "alt" object
library(autopyr)
r_api <- generate_r_api(alt, prefix = "alt")

# An Altair specific edit...
r_api <- gsub("Undefined", "alt$Undefined", r_api)

# write out
r_file_path <- file.path(rprojroot::find_package_root_file(), "R","r_api.R")
cat(r_api, file = r_file_path)
```

To do:

- [x] Add generation of functions
- [ ] Add generation of top-level objects
- [x] Add auto-generation of documentation
- [ ] Improve documentation: add links to doc websites optionally
- [ ] S3 generics -- add named arguments if present for all methods
- [ ] Different prefixes for methods versus classes versus functions (because can have same name...)
- [ ] Refactor some of the ugliness... 
- [ ] Get auto-generated package to pass R CMD check???




