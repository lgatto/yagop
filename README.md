# Yet Another Gene Ontology Package [![Build Status](https://travis-ci.org/lgato/yagop.svg?branch=master)](https://travis-ci.org/lgatto/yagop)

## Current functionality


```r
library("yagop")
ls("package:yagop")
```

```
## [1] "goNamespace" "plotGO"      "validGO"
```

## Anticipated functionality

Given a vector of GO ids, return a list of children, descendants,
parents and ancestors. For example:



```r
##' @title Get the children of GO identifiers
##' @param x A \code{vector} of GO identifiers
##' @param simplify A \code{logical} of length 1 defining if the
##'     return value should be simplified to a vector if \code{x} is
##'     of length 1. Default is \code{TRUE}.
##' @param namespace An optional GO namespace to search for
##'     children. Either of \code{"CC"}, \code{"MF"} or
##'     \code{"BP"}. If missing, all namespace will be searched.
##' @return If \code{x} is of length 1 and \code{simplify} is
##'     \code{TRUE}), a \code{vector}, otherwise a \code{list} of
##'     length equal to \code{length{x}}.
##' @author Laurent Gatto
##' @export
##' @examples
##' goids <- c("GO:0005739", "GO:0005773", "GO:0005783", "GO:0032588")
##' chldrn <- children(goids)
##' ## remove those without any children
##' chldrn <- children[!is.na(cildrn)]
children <- function(x, simplify = TRUE, namespace) {
    if (!missing(namespace))
        namespace <- match.arg(namspace)
}
```
