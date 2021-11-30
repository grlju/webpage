---
authors:
- admin
date: "2021-11-30T00:00:00Z"
draft: false
featured: false
image:
  placement: 2
  preview_only: false
lastmod: November 30, 2021
projects:
- content/project/knights
summary: Matching the Corporate Knights to WRDS can be tricky. This is a
  short post to illustrate how it can be done
tags:
- data wrangling
- name matching
title: Corporate Knights to WRDS matching
---

### Background

In one of the projects I am currently working on, we had to link
information from the [Corporate
Knights](https://www.corporateknights.com/rankings/global-100-rankings/)
dataset with information about the company that we obtained from
[WRDS](https://wrds-www.wharton.upenn.edu/). This proved being a bit
trickier than I expected so I wanted to create a short post to share our
approach so that others may benefit from reusing some of the code. The
main issue to overcome was that [Corporate
Knights](https://www.corporateknights.com/rankings/global-100-rankings/)
don’t have an identifier that can readily be matched to identifiers in
[WRDS](https://wrds-www.wharton.upenn.edu/).

Please note that the dataset I am using are subscription based so I
cannot provide them, instead I will be providing the code and you can
just point the code to files in your system once you have obtained
access to the data from the data providers.

### Let’s begin

I will be using a few packages, I usually call them at the beginning of
my script

``` r
require(readxl)
require(data.table)
require(stringr)
require(textclean)
```

Let’s load the knights data first, if this is in your current working
directory you can use replace the file name, if not you will have to
adjust the name and file location so it points to the right place on
your system. I am using the `here` package and `projects` in `R studio`
so I can just read from my project with ease. I like clean variable
names so I am using `janitor::make_clean_names` of course this is not
necessary.
