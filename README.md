# textgraph

## Overview
`textgraph` is a R package for building and analysing large-scale text co-occurrence graphs. It provides functionality to turn document-feature data into (weighted) feature co-occurrence graphs, 
analyse their contents via **seeded random walks**, **topic clustering** and **temporal topic clustering**, as well as numerous helper functions to prepare, analyse and explore the results.

Currently, the package provides two main workflows:
1) Retrieving terms or entities functionally equivalent to previously extracted seed terms, and retrieve associated documents. This can be compared to certain types of **seeded topic modeling**;
2) Retrieving clusters of related terms or entities via community detection algorithms, either statically for a single network or dynamically for a number of temporal network snapshots. This can be compared to **unsupervised topic modeling** approaches.

`textgraph` is built to facilitate the analysis of large-scale graphs built from millions of documents. Whenever feasible, functions can be parallelized through the [furrr](https://github.com/DavisVaughan/furrr) framework. The majority of graph operations
is handled via the [igraph](https://github.com/igraph/rigraph/) library. Random Walk functionality is provided via [RandomWalkRestartMH](https://github.com/alberto-valdeolivas/RandomWalkRestartMH/tree/master), while dynamic topics are facilitated via
[Memory Community Matching](https://github.com/philipplorenz/memory_community_matching).

## Installation
`textgraph` is not currently on CRAN. Therefore, it needs to be installed directly from Github.
```R
# install.packages("remotes")
remotes::install_github("TimBMK/textgraph")
```

## Usage
The vignette provides a throrough overview of the workflows, with explanations and example data.
```R
vignette("textgraph")
```

## Bugs
Please report any and all bugs [here](https://github.com/TimBMK/textgraph/issues).
