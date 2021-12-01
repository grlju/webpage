---
authors:
- admin
date: "2021-11-30T00:00:00Z"
draft: false
featured: false
image:
  placement: 2
  preview_only: false
lastmod: "2021-11-30T00:00:00Z"
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

``` r
global100 <- setDT(read_excel("global100.xlsx", .name_repair = janitor::make_clean_names))
```

### Clean up countries

Country identifiers across the two datasets are not the same, so we will
first create an country variable that will be allow us to merger on
country. In addition, the Knights dataset uses U.S. in some years and
United States in others, to make sure we get consistent country names I
convert them to iso3 codes before merging them back to the file using a
variable called fic (using this name will make things easier later on).
The `countrycode` package is a great help in doing this.

``` r
global100[country == "U.S.", country := "United States"]
countries <- data.table(country = unique(global100[, country]))
countries[, fic := countrycode::countryname(country, destination = "iso3c")]
head(countries)
```

    ##           country fic
    ## 1:         Sweden SWE
    ## 2:        Germany DEU
    ## 3:    Switzerland CHE
    ## 4: United Kingdom GBR
    ## 5:        Ireland IRL
    ## 6:          Spain ESP

``` r
# Merger countries back to the global100 data.table.
global100 <- countries[global100, on = "country"]
```

### Standardize names

Now the real fun begins. Matching on company names rather than on other
identifiers should always be approached with caution because names
change over time (as do some identifiers, think CUSIP) but also because
names may be written differently in different databases. To help with
the matching we used the Compustat Global Names dataset (G_NAMES) and
North American names (NAMES). I have another post of how to find an
download this dataset \[here\]. Once we have this downloaded to our
system we can load it.

``` r
g_names <- setDT(readRDS("G_NAMES2020.rda"))
names <- setDT(readRDS("NAMES2020.rda"))
names <- names[, .SD, .SDcols = c(intersect(names(names), names(g_names)))]
g_names <- rbindlist(list(g_names, names), fill=TRUE)
head(g_names)
```

    ##     gvkey                         conm   sedol         isin fic costat  sic  naics  gsubind   gind year1 year2
    ## 1: 001166         ASM INTERNATIONAL NV 5165294 NL0000334118 NLD      A 3559 333242 45301010 453010  1996  2020
    ## 2: 001491             HADERA PAPER LTD 6026099 IL0006320183 ISR      A 2621 322121 15103020 151030  1995  2020
    ## 3: 001855 ATLAS CONS MINING & DEV CORP BD0R0N4 PHY0434M1265 PHL      A 1000    212 15104025 151040  2000  2019
    ## 4: 001932     BRITISH AMER TOBACCO PLC 0287580 GB0002875804 GBR      A 2111 312230 30203010 302030  1989  2020
    ## 5: 001945                BOC GROUP PLC 0108120 GB0001081206 GBR      I 2810 325120 15101040 151010  1989  2005
    ## 6: 002000        BANCO CENTRAL HISPANO 5498592 ES0113260634 ESP      I 6020 522110 40101010 401010  1987  1998

When matching these names, it is useful to do some standardization
first. I applied a range standardization steps. First, I removed
characters that convey little meaning about the name. I applied the same
standardization steps to the Knights and the WRDS dataset

``` r
# Knights
# replace text between parentheses
global100[, company_name_stnd := str_replace(company_name, "\\([^\\)]+\\)", " ")]
# convert to uppercase as WRDS names are in upper case
global100[, company_name_stnd := toupper(company_name_stnd)]
# remove special characters ().,
global100[, company_name_stnd := str_remove_all(company_name_stnd, "\\(|\\)|\\.|\\,")]
# replace quotation marks
global100[, company_name_stnd := str_replace_all(company_name_stnd, "\"", " ")]
# remove excess whitespaces
global100[, company_name_stnd := str_squish(company_name_stnd)]


# WRDS
# replace text between parentheses
g_names[, company_name_stnd := str_replace(conm, "\\([^\\)]+\\)", " ")]
# convert to uppercase as WRDS names are in upper case
g_names[, company_name_stnd := toupper(company_name_stnd)]
# remove special characters ().,
g_names[, company_name_stnd := str_remove_all(company_name_stnd, "\\(|\\)|\\.|\\,")]
# replace quotation marks
g_names[, company_name_stnd := str_replace_all(company_name_stnd, "\"", " ")]
# remove excess whitespaces
g_names[, company_name_stnd := str_squish(company_name_stnd)]
```

Second we extracted information about the type of entity it is (I have a
post on getting entity types \[here\]), and removed this information
from the name.

``` r
# extract entity type information
# load entity types
type <- readRDS("type.rda")
# Knights
# extract all entity types
global100[, entity_type := list(str_extract_all(company_name_stnd, type))]
# For some names multiple entity types are extracted (e.g. if the company name abbreviation matched and entity type), in this case use the last on as it is likely to be the actual abbreviation
global100[, entity_type := sapply(entity_type, function(x) tail(x, 1))]
# if not entity type is extracted it creates and empty character list, remove it
global100[, entity_type1 := lapply(entity_type, unlist), by = 1:nrow(global100)][, entity_type := NULL]
setnames(global100, "entity_type1", "entity_type")
# remove the entity type from the standardized name
global100[!is.na(entity_type), company_name_stnd := str_replace(company_name_stnd, entity_type, "")]
# remove excess whitespaces
global100[, company_name_stnd := str_squish(company_name_stnd)]
global100[, entity_type := str_squish(entity_type)]

# WRDS
# extract all entity types
g_names[, entity_type := list(str_extract_all(company_name_stnd, type))]
# For some names multiple entity types are extracted (e.g. if the company name abbreviation matched and entity type), in this case use the last on as it is likely to be the actual abbreviation
g_names[, entity_type := sapply(entity_type, function(x) tail(x, 1))]
# if not entity type is extracted it creates and empty character list, remove it
g_names[, entity_type1 := lapply(entity_type, unlist), by = 1:nrow(g_names)][, entity_type := NULL]
setnames(g_names, "entity_type1", "entity_type")
# remove the entity type from the standardized name
g_names[!is.na(entity_type), company_name_stnd := str_replace(company_name_stnd, entity_type, "")]
# remove excess whitespaces
g_names[, company_name_stnd := str_squish(company_name_stnd)]
g_names[, entity_type := str_squish(entity_type)]
```

Third, we converted elements of names to standard abbreviations taken
from CRSP abbreviations (I also have a post about how to get these
\[here\](content/post/crspabb))

``` r
# convert to abbreviations
# load abbreviation data
name_abbrev <- readRDS("name_abbrev.rda")
# Knights
global100[, company_name_stnd := mgsub(company_name_stnd, name_abbrev[,long], name_abbrev[,abbr], trim = TRUE)]
global100[, entity_type := mgsub(entity_type, name_abbrev[,long], name_abbrev[,abbr], trim = TRUE)]
# WRDS
g_names[, company_name_stnd := mgsub(company_name_stnd, name_abbrev[,long], name_abbrev[,abbr], trim = TRUE)]
g_names[, entity_type := mgsub(entity_type, name_abbrev[,long], name_abbrev[,abbr], trim = TRUE)]
```

### Match names

Now that our names and countries are more or less standardized, I used
the `BRL` package to do a matching on the names, entity types and
country.

``` r
# Load the BRL package
require(BRL)
# create temporary data.tables with relevant information
df1 <- unique(g_names[, .(company_name_stnd, entity_type, fic, gvkey)])
df2 <- unique(global100[, .(company_name_stnd, entity_type, fic)])
# it is useful to set a different name for global dataset so after the match it is clear which variable came from which dataset
setnames(df1, c("company_name_stnd", "entity_type", "fic"), c("company_name_stnd_g", "entity_type_g", "fic_g"))

# do the matching
Zhat <- BRL(df1, df2,
            flds1 = c("company_name_stnd_g", "entity_type_g", "fic_g"),
            flds2 = c("company_name_stnd", "entity_type", "fic"),
            types = c("lv", "lv", "lv"))
```

    ## Warning in compareRecords(df1, df2, flds, flds1, flds2, types, breaks): The fields 'company_name_stnd_g' 'entity_type_g' 'fic_g' in
    ## 'df1' are being compared with the fields 'company_name_stnd' 'entity_type' 'fic' in 'df2'

``` r
# create the matched set
matched <- cbind( df1[Zhat[Zhat<=nrow(df1)],], df2[Zhat<=nrow(df1),])
head(matched)
```

    ##    company_name_stnd_g entity_type_g fic_g  gvkey   company_name_stnd entity_type fic
    ## 1:                 AAK            AB   SWE 215060                 AAK          AB SWE
    ## 2:           AAREAL BK            AG   DEU 251978           AAREAL BK          AG DEU
    ## 3:                 ABB           LTD   CHE 210418                 ABB         LTD CHE
    ## 4: ABERDEEN ASSET MGMT          <NA>   GBR 200015 ABERDEEN ASSET MGMT         PLC GBR
    ## 5:           ACCENTURE           PLC   IRL 143357           ACCENTURE         PLC IRL
    ## 6:             ACCIONA            SA   ESP 102712             ACCIONA          SA ESP

``` r
# a quick visual inspections confirms that all returned matches are valid, invalid matches can be dropped.
```

Of course, as with most matching on names it is likely that not all
firms get a match with this process. In my case 182 out of 212 firms
returned a match (I only have data between 2013 and 2019). Thus I
manually matched all the unmatched based on names (looking up missing on
the web) to account for name changes/M&A, etc.

``` r
# keep working on the unmatched
# create data.table with those global names that have not been matched. This can be used as a look up table in the manual matching. Often names are too dissimilar for the computer but a human can easily see the match.
unmatched_g <- df1[!gvkey %in% matched[, gvkey]]

# create data.table of the unmatched so one can systematically work through all of them.
unmatched <- df2[!company_name_stnd %in% matched[, company_name_stnd]]

# create data.table of with matched and unmatched so found matches can be updated there.
fullmatch <- matched[df2, on = intersect(names(df2), names(matched))]

# Manual matching
fullmatch[company_name_stnd == "AEROPORTS DE PARIS", gvkey := "278142"]
fullmatch[company_name_stnd == "AUSTRALIA & NEW ZEALD BKG GRP", gvkey := "015889"]
fullmatch[company_name_stnd == "BIOGEN IDEC", gvkey := "024468"]
# BIOGEN IDEC changed name to BIOGEN Inc
fullmatch[company_name_stnd == "BOMBARDIER", gvkey := "014078"]
fullmatch[company_name_stnd == "BRIT LD", gvkey := "102653"]
fullmatch[company_name_stnd == "CDN IMPERIAL BK OF COMM", gvkey := "015581"]
fullmatch[company_name_stnd == "CDN TIRE", gvkey := "002703"]
fullmatch[company_name_stnd == "CEMIG", gvkey := "222357"]
fullmatch[company_name_stnd == "COCA-COLA ENTERPRISES", gvkey := "003144"]
fullmatch[company_name_stnd == "ELI LILLY &", gvkey := "006730"]
fullmatch[company_name_stnd == "ENCANA", gvkey := "011781"]
# Encana changed name to Ovintiv Inc
fullmatch[company_name_stnd == "ESSILOR INTL", gvkey := "101248"]
fullmatch[company_name_stnd == "FRAPORT AG FRANKFURT ARPT SVCS WORLDWIDE", gvkey := "245839"]
fullmatch[company_name_stnd == "H & M HENNES & MAURITZ", gvkey := "102276"]
fullmatch[company_name_stnd == "INDUSTRIA DE DISENO TEXTIL", gvkey := "245663"]
fullmatch[company_name_stnd == "INGERSOLL-R&", gvkey := "030098"]
fullmatch[company_name_stnd == "JOHNSON CTLS", gvkey := "006268"]
fullmatch[company_name_stnd == "MCCORMICK &", gvkey := "007146"]
fullmatch[company_name_stnd == "MERCK & CO", gvkey := "007257"]
fullmatch[company_name_stnd == "METSO", gvkey := "339015"]
fullmatch[company_name_stnd == "NATURA COSMETICOS", gvkey := "270509"]
fullmatch[company_name_stnd == "NESTE", gvkey := "272746"]
fullmatch[company_name_stnd == "NESTLE", gvkey := "016603"]
fullmatch[company_name_stnd == "PNC FINL SVCS", gvkey := "008245"]
fullmatch[company_name_stnd == "PEUGEOT", gvkey := "101276"]
# Peugeot does no longer exists merger for Fiat Chrysler Automobiles and Peugeot (PSA Group) created Stellantis using gvkey for Stellantis
# Fiat Chrysler Automobiles is still held under own gvkey. This also changed fic from France to Netherlands
fullmatch[company_name_stnd == "ROYAL KPN", gvkey := "061440"]
fullmatch[company_name_stnd == "SSE", gvkey := "103342"]
fullmatch[company_name_stnd == "STATOIL", gvkey := "220546"]
# name change to Equinor
fullmatch[company_name_stnd == "SVENSKA CELLULOSA SCA", gvkey := "012368"]
fullmatch[company_name_stnd == "TAIWAN SEMICONDUCTOR", gvkey := "201395"]
fullmatch[company_name_stnd == "UCB", gvkey := "100751"]
fullmatch[company_name_stnd == "VMWARE", gvkey := "178083"]
fullmatch[company_name_stnd == "ELECITE DE FRANCE", gvkey := "220920"]
fullmatch[company_name_stnd == "ABERDEEN ASSET MGMT", gvkey := "278298"]
fullmatch[company_name_stnd == "ALLERGAN", gvkey := "027845"]
fullmatch[company_name_stnd == "TIM HORTONS", gvkey := "022402"]

# merge back to the Knights data
global100 <- fullmatch[global100, on = intersect(names(fullmatch), names(global100))]
```

And that is it! Now we have linked the [Corporate
Knights](https://www.corporateknights.com/rankings/global-100-rankings/)
dataset to [WRDS](https://wrds-www.wharton.upenn.edu/) and we can use
the gvkey identifier to obtain additional information about these
companies.
