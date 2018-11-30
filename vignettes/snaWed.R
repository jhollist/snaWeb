## ----style, echo = FALSE, results = 'asis'-------------------------------
# BiocStyle::markdown(css.files = c('custom.css'))

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(snaWeb);
#library(networkD3);

## ----network-------------------------------------------------------------
onaNetwork <- snaWeb::buildNetwork(sites='["http://npr.org"]',searchtype = "related")
onaNetwork <- snaWeb:::graphMetrics(onaNetwork)

## ----visualization, echo=FALSE-------------------------------------------

networkD3::forceNetwork(
  Links = data.frame(from=onaNetwork$edges$node_from-1,to=onaNetwork$edges$node_to-1),
  Nodes = data.frame(id=onaNetwork$nodes$id-1,label=onaNetwork$nodes$name),
  Source="from", Target="to",NodeID = "label",Group = "label",fontSize=12, zoom=T, legend=F,
  opacity = 0.8, charge=-300, width = 600, height = 400
)


