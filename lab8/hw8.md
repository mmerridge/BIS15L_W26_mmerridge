---
title: "Homework 8"
author: "Maison Erridge"
date: "2026-02-05"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---

## Instructions
Answer the following questions and/or complete the exercises in RMarkdown. Please embed all of your code and push the final work to your repository. Your report should be organized, clean, and run free from errors. Remember, you must remove the `#` for any included code chunks to run.  

## Load the libraries

``` r
library("tidyverse")
library("janitor")
#library("naniar")
options(scipen = 999)
```

## About the Data
For this assignment we are going to work with a data set from the [United Nations Food and Agriculture Organization](https://www.fao.org/fishery/en/collection/capture) on world fisheries. These data were downloaded and cleaned using the `fisheries_clean.Rmd` script.  

Load the data `fisheries_clean.csv` as a new object titled `fisheries_clean`.

``` r
fisheries_clean <- read_csv("data/fisheries_clean.csv")
```

1. Explore the data. What are the names of the variables, what are the dimensions, are there any NA's, what are the classes of the variables, etc.? You may use the functions that you prefer.

``` r
names(fisheries_clean)
```

```
## [1] "period"          "continent"       "geo_region"      "country"        
## [5] "scientific_name" "common_name"     "taxonomic_code"  "catch"          
## [9] "status"
```

``` r
glimpse(fisheries_clean)
```

```
## Rows: 1,055,015
## Columns: 9
## $ period          <dbl> 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, …
## $ continent       <chr> "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia"…
## $ geo_region      <chr> "Southern Asia", "Southern Asia", "Southern Asia", "So…
## $ country         <chr> "Afghanistan", "Afghanistan", "Afghanistan", "Afghanis…
## $ scientific_name <chr> "Osteichthyes", "Osteichthyes", "Osteichthyes", "Ostei…
## $ common_name     <chr> "Freshwater fishes NEI", "Freshwater fishes NEI", "Fre…
## $ taxonomic_code  <chr> "1990XXXXXXXX106", "1990XXXXXXXX106", "1990XXXXXXXX106…
## $ catch           <dbl> 100, 100, 100, 100, 100, 200, 200, 200, 200, 200, 200,…
## $ status          <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",…
```

``` r
structure(fisheries_clean)
```

```
## # A tibble: 1,055,015 × 9
##    period continent geo_region    country     scientific_name common_name       
##     <dbl> <chr>     <chr>         <chr>       <chr>           <chr>             
##  1   1950 Asia      Southern Asia Afghanistan Osteichthyes    Freshwater fishes…
##  2   1951 Asia      Southern Asia Afghanistan Osteichthyes    Freshwater fishes…
##  3   1952 Asia      Southern Asia Afghanistan Osteichthyes    Freshwater fishes…
##  4   1953 Asia      Southern Asia Afghanistan Osteichthyes    Freshwater fishes…
##  5   1954 Asia      Southern Asia Afghanistan Osteichthyes    Freshwater fishes…
##  6   1955 Asia      Southern Asia Afghanistan Osteichthyes    Freshwater fishes…
##  7   1956 Asia      Southern Asia Afghanistan Osteichthyes    Freshwater fishes…
##  8   1957 Asia      Southern Asia Afghanistan Osteichthyes    Freshwater fishes…
##  9   1958 Asia      Southern Asia Afghanistan Osteichthyes    Freshwater fishes…
## 10   1959 Asia      Southern Asia Afghanistan Osteichthyes    Freshwater fishes…
## # ℹ 1,055,005 more rows
## # ℹ 3 more variables: taxonomic_code <chr>, catch <dbl>, status <chr>
```

2. Convert the following variables to factors: `period`, `continent`, `geo_region`, `country`, `scientific_name`, `common_name`, `taxonomic_code`, and `status`.

``` r
fish_factors <- fisheries_clean %>% 
  mutate(across(c("period", "continent", "geo_region","country","scientific_name","common_name","taxonomic_code","status"), as.factor))
```

##3. SKIP Are there any missing values in the data? If so, which variables contain missing values and how many are missing for each variable?


4. How many countries are represented in the data?
249 countries

``` r
fish_factors %>% 
  count(country)
```

```
## # A tibble: 249 × 2
##    country                 n
##    <fct>               <int>
##  1 Afghanistan            74
##  2 Albania              2836
##  3 Algeria              2766
##  4 American Samoa       2565
##  5 Andorra                54
##  6 Angola               4831
##  7 Anguilla              238
##  8 Antigua and Barbuda   887
##  9 Argentina            9246
## 10 Armenia               199
## # ℹ 239 more rows
```

5. The variables `common_name` and `taxonomic_code` both refer to species. How many unique species are represented in the data based on each of these variables? Are the numbers the same or different?
There are less common names than taxonomic codes - weird

``` r
names(fish_factors)
```

```
## [1] "period"          "continent"       "geo_region"      "country"        
## [5] "scientific_name" "common_name"     "taxonomic_code"  "catch"          
## [9] "status"
```

``` r
fish_factors %>% 
  summarize(n_species_common_name=n_distinct(common_name))
```

```
## # A tibble: 1 × 1
##   n_species_common_name
##                   <int>
## 1                  3390
```


``` r
fish_factors %>% 
  summarize(n_species_taxonomic_code=n_distinct(taxonomic_code))
```

```
## # A tibble: 1 × 1
##   n_species_taxonomic_code
##                      <int>
## 1                     3722
```

6. In 2023, what were the top five countries that had the highest overall catch?
China, Indonesia, India, Russian Federation, and the United States of America

``` r
names(fish_factors)
```

```
## [1] "period"          "continent"       "geo_region"      "country"        
## [5] "scientific_name" "common_name"     "taxonomic_code"  "catch"          
## [9] "status"
```

``` r
fish_factors %>% 
  filter(period==2023) %>% 
  group_by(country) %>% 
  summarize(total_catch=sum(catch, na.rm=T)) %>% 
  arrange(desc(total_catch)) %>% 
  slice_head(n=5)
```

```
## # A tibble: 5 × 2
##   country                  total_catch
##   <fct>                          <dbl>
## 1 China                      13424705.
## 2 Indonesia                   7820833.
## 3 India                       6177985.
## 4 Russian Federation          5398032 
## 5 United States of America    4623694
```



7. In 2023, what were the top 10 most caught species? To keep things simple, assume `common_name` is sufficient to identify species. What does `NEI` stand for in some of the common names? How might this be concerning from a fisheries management perspective?
Marine fishes NEI, Freshwater fishes NEI, Alaska polluck, Skipjack tuna, Anchoveta, Blue whiting, pacific sardine, Yellowfin tuna, Atlatic herring, Scads NEI. NEI stands for "not elsewhere included" and is bad from a fisheries management perspective since it masks true data and can make it hard for sustainable management.

``` r
fish_factors %>% 
  filter(period==2023) %>% 
  group_by(common_name) %>% 
  summarize(total_catch=sum(catch, na.rm=T)) %>% 
  arrange(desc(total_catch)) %>% 
  slice_head(n=10)
```

```
## # A tibble: 10 × 2
##    common_name                    total_catch
##    <fct>                                <dbl>
##  1 Marine fishes NEI                 8553907.
##  2 Freshwater fishes NEI             5880104.
##  3 Alaska pollock(=Walleye poll.)    3543411.
##  4 Skipjack tuna                     2954736.
##  5 Anchoveta(=Peruvian anchovy)      2415709.
##  6 Blue whiting(=Poutassou)          1739484.
##  7 Pacific sardine                   1678237.
##  8 Yellowfin tuna                    1601369.
##  9 Atlantic herring                  1432807.
## 10 Scads NEI                         1344190.
```

8. For the species that was caught the most above (not NEI), which country had the highest catch in 2023?
Russian Federation

``` r
fish_factors %>% 
  filter(period==2023,common_name=="Alaska pollock(=Walleye poll.)") %>% 
  group_by(country) %>% 
  summarize(total_catch=sum(catch, na.rm=T)) %>% 
  arrange(desc(total_catch)) %>% 
  slice_head(n=1)
```

```
## # A tibble: 1 × 2
##   country            total_catch
##   <fct>                    <dbl>
## 1 Russian Federation     1893924
```

9. How has fishing of this species changed over the last decade (2013-2023)? Create a  plot showing total catch by year for this species.
it wouldnt load so i commented it


``` r
names(fish_factors)
```

```
## [1] "period"          "continent"       "geo_region"      "country"        
## [5] "scientific_name" "common_name"     "taxonomic_code"  "catch"          
## [9] "status"
```


``` r
#fish_factors %>% 
#  group_by(common_name, period) %>% 
#  filter(common_name=="Alaska pollock(=Walleye poll.)",(period<2023 | period>2013)) %>% 
#  summarize(total_catch=sum(catch, na.rm=T)) %>% 
#  ggplot(aes(x=period,y=total_catch))+
#  geom_point()+
#  geom_smooth(method=lm,se=T)+
#  labs(title="Title",x="Year",y="Total Catch")
```

10. Perform one exploratory analysis of your choice. Make sure to clearly state the question you are asking before writing any code.
What is the mean, median, and standard devision for each catch?

``` r
names(fish_factors)
```

```
## [1] "period"          "continent"       "geo_region"      "country"        
## [5] "scientific_name" "common_name"     "taxonomic_code"  "catch"          
## [9] "status"
```


``` r
fish_factors %>% 
  group_by(common_name, catch) %>% 
  summarize(mean_catch=mean(catch, na.rm=T),
            median_catch=median(catch, na.rm=T),
            sd_catch=sd(catch, na.rm=T),
            n=n())
```

```
## `summarise()` has grouped output by 'common_name'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 373,220 × 6
## # Groups:   common_name [3,390]
##    common_name catch mean_catch median_catch sd_catch     n
##    <fct>       <dbl>      <dbl>        <dbl>    <dbl> <int>
##  1 Aba          100        100          100        NA     1
##  2 Aba          102        102          102        NA     1
##  3 Aba          120        120          120        NA     1
##  4 Aba          126        126          126        NA     1
##  5 Aba          134        134          134        NA     1
##  6 Aba          148        148          148        NA     1
##  7 Aba          176        176          176        NA     1
##  8 Aba          190        190          190        NA     1
##  9 Aba          204        204          204        NA     1
## 10 Aba          210.       210.         210.       NA     1
## # ℹ 373,210 more rows
```

## Knit and Upload
Please knit your work as an .html file and upload to Canvas. Homework is due before the start of the next lab. No late work is accepted. Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  
