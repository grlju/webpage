---
authors:
- admin
date: "2021-11-29T00:00:00Z"
draft: false
featured: false
lastmod: "2021-11-29T00:00:00Z"
projects:
- content/project/rmmc
subtitle: An R package for calculating multimarket contact
summary: Over the last few months I have worked on creating a package
  that can help with calculating various measures of multimarket
  contact. This is a short post to illustrate how it can be used.
tags:
- multimarket contact
title: rmmc
---

Multimarket contact is the situation when firms meet in more than one
market. To capture this situation in empirical research a number of
different measures have been proposed. The R package `rmmc` can be used
as a central repository of functions that makes it easy to access the
various functions that have been used.

This post provides a short illustration about how this package can be
used.

First, install start by installing the package. Currently is only
available on github.

``` r
library(devtools)
```

    ## Loading required package: usethis

``` r
install_github("grlju/rmmc")
```

    ## Skipping install of 'rmmc' from a github remote, the SHA1 (d59f6245) has not changed since last install.
    ##   Use `force = TRUE` to force installation

Let’s load some example data provide with the package

``` r
library(rmmc)
data <- rmmc:::data
head(data)
```

    ##    year    id m1 m2 m3 m4
    ## 1: 2000 firm1  1  1  1  0
    ## 2: 2001 firm1  1  0  1  0
    ## 3: 2002 firm1  1  1  1  1
    ## 4: 2003 firm1  1  1  1  1
    ## 5: 2000 firm2  0  0  0  1
    ## 6: 2001 firm2  0  0  1  1

The `data.table` (the package `data.table` package is a dependency) has
information on three firms (firm1-firm3) across three years (2000-2003)
and four markets in which these firms have activities (m1-m4). This
example dataset also illustrates the layout that the package expects. If
there are multiple observations for each entity the data should be in
long format.

Calculating the multimarket contact between these firms can now easily
be accomplished with the various functions.

Let’s use a simple count measure. There can be multiple level at which
we can think that multimarket contact exists and is important. Let’s
focus on the dyadic level first. That is, the level of contact between
two different entities (i.e. firms in our example).

``` r
count_dyad <- mmc_count(data, firm_col = "id", date_col = "year", level = "dyad", market_cols = c("m1", "m2", "m3", "m4"))
head(count_dyad)
```

    ##    year    id N_i  id.y N_j mmc_ij
    ## 1: 2000 firm1   3 firm2   1      0
    ## 2: 2000 firm1   3 firm3   3      2
    ## 3: 2000 firm1   3 firm4   1      1
    ## 4: 2000 firm2   1 firm1   3      0
    ## 5: 2000 firm2   1 firm3   3      1
    ## 6: 2000 firm2   1 firm4   1      0

This calculates a count of the dyadic contact between the firms in data.
For instance, firm1 and firm2 have no contact in year 2000 but firm1 and
firm3 have 2 contacts in the same year.
