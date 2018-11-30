# snaWeb
An R package for finding and building social networks for a website.

Please read the vignette

    vignette("snaWeb", package = "snaWeb")

for details on the package and the examples for use of the package.

## Development

* Please use the `sna/tools/R/makefile` for building, testing, checking, and
  installing the developmental version of the package.

* Functions built in the R/ directory will be documented via roxygen comments.

* Version numbering: major.minor.maintenance.development

    * major: Used with API and underlying code is solid for release
    * minor: new features, major bug patches
    * maintenance: bug fixes, improved documentation
    * development: only used to denote that the current version of the source
      code is in development and not ready for release.

* Please tag major.minor.maintenance version of the package.  Only tag
  development versions if needed.

* Vignettes:  Built using `knitr::spin`.  Edit the .R file(s), do not edit the
  .Rmd files.

