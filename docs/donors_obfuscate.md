Rationale
=========

To preserve confidentiality while allowing public collaboration, we
require obfuscated donor and service data for model-building. The
obfuscated data should have similar distributional (geospatial and
demographic) properties to the real data.

Method
======

-   Data are resonably anonymised already.
-   Apply a random offset (up to +/- 6 months) to date variables.
-   Don’t set a RNG seed.

``` r
library(lubridate)
library(readxl)
library(here)
library(tidyverse)
```

Simple string normalisation:

``` r
str_normalise <- function(str){
  str %>%
    toupper %>%
    str_remove_all("\\W")
}
```

Import and clean real dataset (not pushed to Github):

``` r
donors_raw <- read_csv(here("data/raw/donors.csv"), col_types = "ccccccc")

donors <- donors_raw %>%
  mutate(gender = factor(gender),
         
         state = str_normalise(state) %>% 
           fct_collapse(ACT = c("ACT", "AUSTRALIANCAPITALTERRITORY"),
                        NSW = c("NSW", "NEWSOUTHWALES"),
                        QLD = c("QLD", "QUEENSLAND"),
                        SA = c("SA", "SOUTHAUSTRALIA"),
                        TAS = c("TAS", "TASMANIA"),
                        VIC = c("VIC", "VICTORIA"),
                        WA = c("WA", "WESTERNAUSTRALIA")) %>%
           fct_other(keep = c("ACT", "NSW", "QLD", "SA", "TAS", "VIC", "WA"),
                     other_level = NA_character_),
         
         postcode = case_when(state == "NT" & str_detect(postcode, "^8[0-9]{2}$") ~
                                paste0("0", postcode),
                              str_detect(postcode, "^[0-9]{4}$") ~ postcode,
                              TRUE ~ NA_character_) %>%
           factor) %>%
  mutate_at(vars(dateofbirth, FirstContributionDate, LastContributionDate),
            function(v) {
              str_remove(v, " .*$") %>%
                ymd %>%
                na_if(ymd("1900-01-01")) %>%
                if_else(. > ymd("2018-11-24"), as.Date(NA), .)
            })

summary(donors)
```

    ##       id                state          postcode      dateofbirth        
    ##  Length:93242       NSW    :37133   2000   :  680   Min.   :1900-02-11  
    ##  Class :character   QLD    :17031   2650   :  390   1st Qu.:1959-08-20  
    ##  Mode  :character   VIC    :11658   4350   :  375   Median :1975-04-01  
    ##                     WA     : 9705   2170   :  372   Mean   :1971-12-28  
    ##                     SA     : 6068   2770   :  357   3rd Qu.:1987-07-27  
    ##                     NA     : 5065   (Other):85939   Max.   :2018-10-03  
    ##                     (Other): 6582   NA's   : 5129   NA's   :36032       
    ##             gender      FirstContributionDate LastContributionDate
    ##  Female        :47011   Min.   :1984-01-06    Min.   :2014-01-02  
    ##  Male          :36835   1st Qu.:2009-11-30    1st Qu.:2015-12-23  
    ##  Not Applicable: 1238   Median :2015-01-15    Median :2017-07-15  
    ##  Unknown       : 8155   Mean   :2012-01-28    Mean   :2017-03-25  
    ##  NA's          :    3   3rd Qu.:2016-12-25    3rd Qu.:2018-10-15  
    ##                         Max.   :2018-11-19    Max.   :2018-11-19  
    ##                         NA's   :1

Obfuscate with date offset:

``` r
n <- nrow(donors)
range <- 365
offset_days <- runif(n, min =  -range / 2, max = range / 2)

donors_obf <- donors %>%
  mutate_at(vars(dateofbirth, FirstContributionDate, LastContributionDate), 
            ~ . + offset_days)

summary(donors_obf)
```

    ##       id                state          postcode      dateofbirth        
    ##  Length:93242       NSW    :37133   2000   :  680   Min.   :1899-09-01  
    ##  Class :character   QLD    :17031   2650   :  390   1st Qu.:1959-08-24  
    ##  Mode  :character   VIC    :11658   4350   :  375   Median :1975-03-31  
    ##                     WA     : 9705   2170   :  372   Mean   :1971-12-28  
    ##                     SA     : 6068   2770   :  357   3rd Qu.:1987-07-19  
    ##                     NA     : 5065   (Other):85939   Max.   :2018-09-29  
    ##                     (Other): 6582   NA's   : 5129   NA's   :36032       
    ##             gender      FirstContributionDate LastContributionDate
    ##  Female        :47011   Min.   :1983-08-19    Min.   :2013-07-11  
    ##  Male          :36835   1st Qu.:2009-12-09    1st Qu.:2015-12-31  
    ##  Not Applicable: 1238   Median :2015-02-13    Median :2017-07-29  
    ##  Unknown       : 8155   Mean   :2012-01-28    Mean   :2017-03-25  
    ##  NA's          :    3   3rd Qu.:2017-01-14    3rd Qu.:2018-08-03  
    ##                         Max.   :2019-05-12    Max.   :2019-05-17  
    ##                         NA's   :1

Write out for future use:

``` r
saveRDS(donors_obf, here("data/clean/donors_obf.rds"))
write_csv(donors_obf, here("data/clean/donors_obf.csv"))
```
