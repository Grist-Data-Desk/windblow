---
title: "Any way the wind blows"
author: "Clayton Aldern for Grist / The Houston Chronicle / The Beaumont Enterprise"
subtitle: "Comparing kiln shutdowns to wind direction"
date: "February 2023"
output:
  html_document:
    df_print: paged
    highlight: kate
    toc: yes
    toc_float: yes
    keep_md: yes
assets:
  css:
  - http://fonts.googleapis.com/css?family=Raleway
  - http://fonts.googleapis.com/css?family=Merriweather
always_allow_html: yes
---
<style>
body{
  font-family: 'Merriweather', sans-serif;
  font-size: 16px;
  line-height: 24px;
}

h1,h2,h3,h4 {
  font-family: 'Raleway', sans-serif;
}
</style>
---

This notebook accompanies Naveena Sadasivam and Clayton Aldern's Grist [investigation](https://grist.org/project/accountability/koch-oxbow-port-arthur-texas-clean-air-act-pollution/) into Oxbow Calcining's Port Arthur petcoke facility. Naveena reported and wrote the story; Clay authored this repository.

## Setup

```r
knitr::opts_chunk$set(fig.path = here::here("img/png/"),
                      dev = "png",
                      dpi = 300)
library(here)
library(tidyverse)
library(skimr)
library(lubridate)
library(circular)
library(mlr3verse)
library(mlr3learners)
library(mlr3pipelines)
library(iml)
library(ggplot2)

# make sure you're working with the most recent version of {mlr3spatiotempcv} if you're working in the experimental chunk at the end.
# remotes::install_github("mlr-org/mlr3spatiotempcv")
# library(mlr3spatiotempcv)
```

```r
PALETTE = c("#d3c8ff",
            "#ac00e8",
            "#f5515b",
            "#3c3830")
BINS = 36 #bins
INCREMENT_WIND = 8 #mph
INCREMENT_SO2 = 5 #ppb
CLASS_MIN = 60 #instances
AUTOCORR_WINDOW = 10 #hours
N_KILNS_OFF = 1 #kilns
```
### Load and format data

Here's the dataset we grabbed from that 5,000-page PDF.

```r
w <- read_csv(here('dat/clean/csv/windspeedData.csv'),
              col_types = cols(
                Date = col_character(),
                Count = col_double(),
                `Sulfur Dioxide` = col_character(),
                `Wind Speed – Scalar` = col_double(),
                `Wind Direction – Resultant` = col_double(),
                `Peak Wind Gust` = col_double(),
                `Outdoor Temperature (ºF)` = col_double(),
                Grouping = col_character()
                )
              )
```

```
## Warning: One or more parsing issues, see `problems()` for details
```

```r
colnames(w) <- c("date",
                 "count",
                 "so2",
                 "speed",
                 "dir",
                 "peak",
                 "temp",
                 "grouping")

w$date <- w$date %>% parse_date_time(orders = c("%m/%d/%y %H:%M",
                                                "%m/%d/%Y %H:%M"))
w$so2 <- w$so2 %>% gsub("‐","-",.) %>% as.numeric()
```
Let's add some flags based on the Grouping variable corresponding to whether a kiln is off at a given point in time. We also want to filter the dataset to the time period after 'experiments' have ended.

```r
w <- w %>% mutate(K2Off = ifelse(grepl("K2Off",
                                        grouping,
                                        fixed = T),
                                  T,
                                  F))

w <- w %>% mutate(K3Off = ifelse(grepl("K3Off",
                                        grouping,
                                        fixed = T),
                                  T,
                                  F))

w <- w %>% mutate(K4Off = ifelse(grepl("K4Off",
                                        grouping,
                                        fixed = T),
                                  T,
                                  F))

w <- w %>% mutate(K5Off = ifelse(grepl("K5Off",
                                        grouping,
                                        fixed = T),
                                  T,
                                  F))

w <- w %>% mutate(anyOff = ifelse(grepl("Off",
                                        grouping,
                                        fixed = T),
                                  T,
                                  F))

w <- w %>% mutate(allOff = ifelse(grepl("K2Off‐K3Off‐K4Off‐K5Off",
                                        grouping,
                                        fixed = T),
                                  T,
                                  F))

w <- w %>% mutate(K3K5Off = ifelse(grepl("K2HS‐K3Off‐K4HS‐K5Off",
                                        grouping,
                                        fixed = T),
                                  T,
                                  F))

stash <- w #in case we need a copy of the original df later; it's not too big
w <- w %>% filter(date > as.Date("2018-08-01")) #i.e. post-'experiments'

wK2Off <- w %>% filter(K2Off)
wK3Off <- w %>% filter(K3Off)
wK4Off <- w %>% filter(K4Off)
wK5Off <- w %>% filter(K5Off)
wAnyOff <- w %>% filter(anyOff)
wAllOn <- w %>% filter(!anyOff)
wAllOff <- w %>% filter(allOff)
wK3K5Off <- w %>% filter(K3K5Off)
wNotK4Off <- w %>% filter(K2Off | K3Off | K5Off)
```
## Analysis

And let's take a peek at some windrose plots.

```r
par(mfrow=c(4,2))

windrose(circular(wAllOn$dir,
                  units = "degrees",
                  template = "geographics"),
         wAllOn$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         right = TRUE,
         main = paste0("All Kilns On, n = ",nrow(wAllOn)))

windrose(circular(wAllOff$dir,
                  units = "degrees",
                  template = "geographics"),
         wAllOff$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("All Kilns Off, n = ",nrow(wAllOff)))

windrose(circular(wAnyOff$dir,
                  units = "degrees",
                  template = "geographics"),
         wAnyOff$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("Any Kiln(s) Off, n = ",nrow(wAnyOff)))

windrose(circular(wK3K5Off$dir,
                  units = "degrees",
                  template = "geographics"),
         wK3K5Off$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("K3 and K5 Off, n = ",nrow(wK3K5Off)))

windrose(circular(wK2Off$dir,
                  units = "degrees",
                  template = "geographics"),
         wK2Off$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("K2 Off, n = ",nrow(wK2Off)))

windrose(circular(wK3Off$dir,
                  units = "degrees",
                  template = "geographics"),
         wK3Off$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("K3 Off, n = ",nrow(wK3Off)))

windrose(circular(wK4Off$dir,
                  units = "degrees",
                  template = "geographics"),
         wK4Off$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("K4 Off, n = ",nrow(wK4Off)))

windrose(circular(wK5Off$dir,
                  units = "degrees",
                  template = "geographics"),
         wK5Off$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("K5 Off, n = ",nrow(wK5Off)))
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/windrose-speeds-1.png)<!-- -->
Let's also look at windroses that use SO2 figures as the magnitude:

```r
par(mfrow=c(4,2))

windrose(circular(wAllOn$dir,
                  units = "degrees",
                  template = "geographics"),
         wAllOn$so2,
         bins = BINS,
         increment = INCREMENT_SO2,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("All Kilns On, n = ",nrow(wAllOn)))

windrose(circular(wAllOff$dir,
                  units = "degrees",
                  template = "geographics"),
         wAllOff$so2,
         bins = BINS,
         increment = INCREMENT_SO2,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("All Kilns Off, n = ",nrow(wAllOff)))

windrose(circular(wAnyOff$dir,
                  units = "degrees",
                  template = "geographics"),
         wAnyOff$so2,
         bins = BINS,
         increment = INCREMENT_SO2,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("Any Kiln(s) Off, n = ",nrow(wAnyOff)))

windrose(circular(wK3K5Off$dir,
                  units = "degrees",
                  template = "geographics"),
         wK3K5Off$so2,
         bins = BINS,
         increment = INCREMENT_SO2,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("K3 and K5 Off, n = ",nrow(wK3K5Off)))

windrose(circular(wK2Off$dir,
                  units = "degrees",
                  template = "geographics"),
         wK2Off$so2,
         bins = BINS,
         increment = INCREMENT_SO2,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("K2 Off, n = ",nrow(wK2Off)))

windrose(circular(wK3Off$dir,
                  units = "degrees",
                  template = "geographics"),
         wK3Off$so2,
         bins = BINS,
         increment = INCREMENT_SO2,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("K3 Off, n = ",nrow(wK3Off)))

windrose(circular(wK4Off$dir,
                  units = "degrees",
                  template = "geographics"),
         wK4Off$so2,
         bins = BINS,
         increment = INCREMENT_SO2,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("K4 Off, n = ",nrow(wK4Off)))

windrose(circular(wK5Off$dir,
                  units = "degrees",
                  template = "geographics"),
         wK5Off$so2,
         bins = BINS,
         increment = INCREMENT_SO2,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         main = paste0("K5 Off, n = ",nrow(wK5Off)))
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/windrose-SO2-1.png)<!-- -->
Okay. So we're looking at a non-uniform wind distribution (expected) that appears to differ meaningfully between kiln states. We'll want to run some proper stats to understand these differences (and ultimately model states as a function of environmental conditions and readings at the monitor).

One immediate question, given the windroses above, is what the heck is going on with Kiln 4 (K4)? When is it down? Why does its windrose look different from other kiln states?

### We Don't Talk About K4

The tl;dr on K4 is that it's down when it's being rebuilt:

```r
ggplot(w,aes(date,K4Off)) + geom_point()
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/K4-time-1.png)<!-- -->
By the way, something interesting happens pre/post-rebuild:

```r
par(mfrow=c(3,2))

windrose(circular(filter(stash,year(date)==2017 & !anyOff)$dir,
                  units = "degrees",
                  template = "geographics"),
         filter(stash,year(date)==2017 & !anyOff)$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         right = TRUE,
         main = paste0("All Kilns On, 2017, n = ",nrow(filter(stash,year(date)==2017 & !anyOff))))

windrose(circular(filter(stash,year(date)==2017 & K4Off)$dir,
                  units = "degrees",
                  template = "geographics"),
         filter(stash,year(date)==2017 & K4Off)$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         right = TRUE,
         main = paste0("K4 Off, 2017, n = ",nrow(filter(stash,year(date)==2017 & K4Off))))

windrose(circular(filter(stash,year(date)==2018 & !anyOff)$dir,
                  units = "degrees",
                  template = "geographics"),
         filter(stash,year(date)==2018 & !anyOff)$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         right = TRUE,
         main = paste0("All Kilns On, 2018, n = ",nrow(filter(stash,year(date)==2018 & !anyOff))))

windrose(circular(filter(stash,year(date)==2018 & K4Off)$dir,
                  units = "degrees",
                  template = "geographics"),
         filter(stash,year(date)==2018 & K4Off)$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         right = TRUE,
         main = paste0("K4 Off, 2018, n = ",nrow(filter(stash,year(date)==2018 & K4Off))))

windrose(circular(filter(stash,year(date)==2019 & !anyOff)$dir,
                  units = "degrees",
                  template = "geographics"),
         filter(stash,year(date)==2019 & !anyOff)$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         right = TRUE,
         main = paste0("All Kilns On, 2019, n = ",nrow(filter(stash,year(date)==2019 & !anyOff))))

windrose(circular(filter(stash,year(date)==2019 & K4Off)$dir,
                  units = "degrees",
                  template = "geographics"),
         filter(stash,year(date)==2019 & K4Off)$speed,
         bins = BINS,
         increment = INCREMENT_WIND,
         ticks = F,
         label.freq = T,
         fill.col = PALETTE,
         right = TRUE,
         main = paste0("K4 Off, 2019, n = ",nrow(filter(stash,year(date)==2019 & K4Off))))
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/K4-1.png)<!-- -->
i.e. the southerly bias from 2017 appears to disappear.

Really, though, the main takeaway here is that we're going to exclude K4 from the rest of our analysis -- not by excluding the rows in question, but by ignoring its contribution to kiln states.

Let's keep going. We can move away from the windrose plots and try to understand the underlying statistics of those distributions. First, a teeny bit of trig:

```r
w$dir <- cos( w$dir * (pi/180) )
```
After taking the cosine above, wind direction ranges from -1 to 1, with negative values corresponding to winds blowing closer to due north (southerly winds). We got lucky with windrose conventions here, in that due north (southerly wind at 180º) corresponds to π radians. With that correspondence in mind, we can think of the windrose as the unit circle on its side. The trigonometric transformation basically grabs the x-coordinate from the unit circle.

### Downsampling

Anyway. We're taking 5-minute measurements; presumably there's some temporal autocorrelation here that'll bias a model.

```r
w %>% select(so2) %>% drop_na() %>% acf(lag.max = 120)
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/acf-1.png)<!-- -->
Yeah, quite a bit. We'll want to keep an eye out for that effect during modeling. To address it, we'll downsample the data to the hourly level (which we need to do anyway to match the resolution of the NOAA data later on) and then downsample it a second time using intervals of random length. We'll need some helper functions for that...

```r
# random intervals to deal with temporal autocorrelation
get_intervals <- function(df) {
  set.seed(206)
  iv <- rep(NA, nrow(df))
  i <- 1
  j <- 1
  while (j <= nrow(df)) {
    il <- round(runif(1,1,AUTOCORR_WINDOW))
    iv[j:(j+il-1)] <- rep(i,il)
    i <- i + 1
    j <- j + il
  }
  iv <- iv[1:nrow(df)]
  return(iv)
}

# easy slope over an interval
slope <- function(x) {
  last(x) - first(x)
}
```
Let's start with the hourly resolution:

```r
wdf <- w %>% 
  group_by(grouping) %>% 
  filter(n() >= CLASS_MIN) %>% 
  select(grouping,
         date,
         so2:peak) %>%
  ungroup() %>%
  mutate_if(is.numeric,
            ~replace_na(.,mean(., na.rm = TRUE))) %>%
  drop_na() %>%
  mutate(y = year(date),
         m = month(date), 
         d = day(date),
         h = hour(date)) %>%
  group_by(y,m,d,h) %>%
  summarise(across(grouping:date, first),
            across(so2:peak, mean, na.rm = TRUE)) %>%
  ungroup() %>%
  select(grouping:peak)
```

```
## `summarise()` has grouped output by 'y', 'm', 'd'. You can override using the
## `.groups` argument.
```

```r
wdf$grouping <- as.factor(wdf$grouping)

# BINOMIAL
# levels(wdf$grouping) <- as.integer(str_count(levels(wdf$grouping), 'Off') >= N_KILNS_OFF)
levels(wdf$grouping) <- c(0,1,0,1,1,1,1,1,1,1) # one or more kilns are off (ignoring K4)

# MULTINOMIAL
# levels(wdf$grouping) <- as.integer(str_count(levels(wdf$grouping), 'Off'))

glimpse(wdf)
```

```
## Rows: 8,039
## Columns: 6
## $ grouping <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ date     <dttm> 2018-08-01 00:05:00, 2018-08-01 01:00:00, 2018-08-01 02:00:0…
## $ so2      <dbl> 0.28181818, 3.10833333, 0.58333333, 0.30000000, 0.19166667, 0…
## $ speed    <dbl> 2.554545, 1.033333, 2.416667, 1.058333, 1.958333, 2.125000, 7…
## $ dir      <dbl> -0.31303854, 0.24811950, 0.71309555, 0.53478780, 0.50171133, …
## $ peak     <dbl> 3.445455, 1.600000, 3.566667, 1.800000, 3.091667, 3.808333, 1…
```
Great. We have some semblance of a clean dataset.

### Descriptive statistics

Before going any further, a few tests and charts to wrap our heads around the differences between the kiln states in question:

```r
wdf %>% group_by(grouping) %>% select(so2,speed,dir,peak) %>% skim() %>% ungroup()
```

```
## Adding missing grouping variables: `grouping`
```


Table: Data summary

|                         |           |
|:------------------------|:----------|
|Name                     |Piped data |
|Number of rows           |8039       |
|Number of columns        |5          |
|_______________________  |           |
|Column type frequency:   |           |
|numeric                  |4          |
|________________________ |           |
|Group variables          |grouping   |


**Variable type: numeric**

|skim_variable |grouping | n_missing| complete_rate|  mean|   sd|    p0|   p25|   p50|   p75|  p100|hist  |
|:-------------|:--------|---------:|-------------:|-----:|----:|-----:|-----:|-----:|-----:|-----:|:-----|
|so2           |0        |         0|             1|  0.94| 2.07| -1.48|  0.22|  0.48|  0.97| 45.55|▇▁▁▁▁ |
|so2           |1        |         0|             1|  0.79| 2.15| -1.23|  0.12|  0.36|  0.76| 49.17|▇▁▁▁▁ |
|speed         |0        |         0|             1|  8.69| 4.20|  0.30|  5.72|  8.23| 11.56| 25.56|▃▇▅▁▁ |
|speed         |1        |         0|             1|  8.96| 4.09|  0.53|  5.94|  8.28| 11.50| 27.14|▃▇▃▁▁ |
|dir           |0        |         0|             1| -0.08| 0.69| -1.00| -0.81| -0.05|  0.57|  1.00|▇▂▂▅▆ |
|dir           |1        |         0|             1| -0.31| 0.66| -1.00| -0.89| -0.59|  0.29|  1.00|▇▂▂▂▂ |
|peak          |0        |         0|             1| 12.62| 6.04|  0.30|  8.31| 11.91| 16.62| 36.21|▃▇▅▁▁ |
|peak          |1        |         0|             1| 12.80| 5.97|  1.13|  8.47| 11.87| 16.47| 42.55|▅▇▃▁▁ |

```r
ks.test(filter(wdf,grouping==0)$so2, filter(wdf,grouping==1)$so2)
```

```
## Warning in ks.test.default(filter(wdf, grouping == 0)$so2, filter(wdf, grouping
## == : p-value will be approximate in the presence of ties
```

```
## 
## 	Asymptotic two-sample Kolmogorov-Smirnov test
## 
## data:  filter(wdf, grouping == 0)$so2 and filter(wdf, grouping == 1)$so2
## D = 0.12241, p-value < 2.2e-16
## alternative hypothesis: two-sided
```

```r
ks.test(filter(wdf,grouping==0)$speed, filter(wdf,grouping==1)$speed)
```

```
## Warning in ks.test.default(filter(wdf, grouping == 0)$speed, filter(wdf, : p-
## value will be approximate in the presence of ties
```

```
## 
## 	Asymptotic two-sample Kolmogorov-Smirnov test
## 
## data:  filter(wdf, grouping == 0)$speed and filter(wdf, grouping == 1)$speed
## D = 0.035616, p-value = 0.01873
## alternative hypothesis: two-sided
```

```r
ks.test(filter(wdf,grouping==0)$dir, filter(wdf,grouping==1)$dir)
```

```
## 
## 	Asymptotic two-sample Kolmogorov-Smirnov test
## 
## data:  filter(wdf, grouping == 0)$dir and filter(wdf, grouping == 1)$dir
## D = 0.16816, p-value < 2.2e-16
## alternative hypothesis: two-sided
```

```r
ks.test(filter(wdf,grouping==0)$peak, filter(wdf,grouping==1)$peak)
```

```
## Warning in ks.test.default(filter(wdf, grouping == 0)$peak, filter(wdf, : p-
## value will be approximate in the presence of ties
```

```
## 
## 	Asymptotic two-sample Kolmogorov-Smirnov test
## 
## data:  filter(wdf, grouping == 0)$peak and filter(wdf, grouping == 1)$peak
## D = 0.02324, p-value = 0.2731
## alternative hypothesis: two-sided
```
Broadly, we can say we're looking at two different distributions. Kiln-off states tend to be associated with lower SO2 readings, higher wind speeds, and southerly (north-blowing) winds.

One quick question: What's happening in the 24 hours pre/post shutdown? That is, on median, what conditions precede and proceed the decision to shut down a kiln?

```r
offEffect <- wdf %>% 
  mutate(turnOff = ifelse(lag(grouping)==0 & grouping==1,1,0))
offEffect$turnOff <- as.factor(offEffect$turnOff)

SOff <- 24 #hrs per slice
offEffect <- offEffect %>% 
  slice(rep(which(turnOff == 1), each=SOff) + 0:(SOff-1))
i <- 1
for (g in 1:sum(offEffect$turnOff==1)) {
  offEffect[i:(i+SOff-1),"g"] <- g
  offEffect[i:(i+SOff-1),"x"] <- 0:(SOff-1)
  g <- g+1
  i <- i+SOff
}
offEffect <- offEffect %>%
  group_by(g) %>%
  mutate(effect = (so2 - first(so2))) %>%
  mutate(effectPct = (so2 - first(so2)) / first(so2)) %>%
  ungroup()

preEffect <- wdf %>% 
  mutate(turnOff = ifelse(lag(grouping)==0 & grouping==1,1,0))
preEffect$turnOff <- as.factor(preEffect$turnOff)

SPre <- 24 #hrs per slice
preEffect <- preEffect %>% 
  slice(rep(which(turnOff == 1), each=SPre) + 0:-(SPre-1))
i <- 1
for (g in 1:sum(offEffect$turnOff==1)) {
  preEffect[i:(i+SPre-1),"g"] <- g
  preEffect[i:(i+SPre-1),"x"] <- 0:-(SPre-1)
  g <- g+1
  i <- i+SPre
}
preEffect <- preEffect %>%
  group_by(g) %>%
  mutate(effect = (so2 - first(so2))) %>%
  mutate(effectPct = (so2 - first(so2)) / first(so2)) %>%
  filter(x!=0) %>%
  ungroup()

windowEffect <- rbind(preEffect,offEffect)

m <- windowEffect %>%
  group_by(x) %>%
  summarize(mn = median(effectPct))
ggplot(m, aes(x = x, y = mn)) + 
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 0, linetype='dashed') +
  scale_y_continuous(labels = scales::percent) +
  labs(x = 'Hours pre/post shutdown',
       y = 'SO2, median percent change (relative to kiln shutdown)') +
  geom_segment(aes(x = 1,
                   y = max(mn)/2,
                   xend = 5,
                   yend = max(mn)/2),
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_segment(aes(x = -1,
                   y = max(mn)/2,
                   xend = -5,
                   yend = max(mn)/2),
               arrow = arrow(length = unit(0.25, "cm"))) +
  theme_minimal()
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/transition-periods-1.png)<!-- -->
Which would seem to suggest that, on median, the shutdowns tend to occur in advance of an uptick in SO2 concentration measured at the monitor.

## Modeling

### Clean-up

Alrighty, let's finish cleaning up the dataframe for modeling (most notably by downsampling across random intervals and by adding a few variables computed within those intervals).

```r
wdf$interval <- get_intervals(wdf)
wdf <- wdf %>%
  mutate(across(so2:peak, ~ .x, .names = '{col}_sd')) %>%
  mutate(across(so2:peak, ~ .x, .names = '{col}_slope')) %>%
  group_by(interval) %>%
  summarise(across(grouping:date, first),
            across(so2:peak, mean, na.rm = TRUE),
            across(so2_sd:peak_sd, sd, na.rm = TRUE),
            across(so2_slope:peak_slope, slope)) %>%
  mutate_if(is.numeric,
            ~replace_na(.,mean(., na.rm = TRUE))) %>%
  drop_na() %>%
  ungroup() %>%
  select(grouping:peak_slope)

summary(wdf$grouping)/nrow(wdf)
```

```
##         0         1 
## 0.6425218 0.3574782
```

```r
glimpse(wdf)
```

```
## Rows: 1,491
## Columns: 14
## $ grouping    <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ date        <dttm> 2018-08-01 00:05:00, 2018-08-01 09:00:00, 2018-08-01 14:0…
## $ so2         <dbl> 1.422979798, 1.211666667, 0.274410702, 0.829166667, 0.8500…
## $ speed       <dbl> 3.507912, 6.828333, 6.955996, 6.945833, 9.125000, 6.258333…
## $ dir         <dbl> 0.48958364, 0.60194877, 0.61003904, 0.57954596, 0.41617230…
## $ peak        <dbl> 5.243013, 10.268333, 9.910714, 9.512500, 13.950000, 8.7166…
## $ so2_sd      <dbl> 1.52490699, 0.84437682, 0.17666083, 0.13552880, 0.51406779…
## $ speed_sd    <dbl> 2.6657450, 1.2179445, 1.7925753, 1.9857915, 1.4486425, 0.9…
## $ dir_sd      <dbl> 0.360216019, 0.245313235, 0.273004040, 0.143799560, 0.1590…
## $ peak_sd     <dbl> 3.7177276, 1.2942099, 2.7244682, 3.7771287, 2.0571152, 1.1…
## $ so2_slope   <dbl> 3.46818182, -2.06666667, -0.23333333, 0.19166667, 0.000000…
## $ speed_slope <dbl> 2.3204545, 2.2000000, -0.6250000, 2.8083333, 0.0000000, -1…
## $ dir_slope   <dbl> 1.197112697, -0.642407517, 0.661535144, -0.203363288, 0.00…
## $ peak_slope  <dbl> 4.5545455, 2.9500000, -2.4083333, 5.3416667, 0.0000000, -2…
```
### Linear model digression

By the way, it's worth looking at a quick linear model of SO2 as a function of kiln states and some of these environmental variables:

```r
wrdf <- wdf %>% select(c(so2,grouping,speed,dir)) %>% filter(so2 > 0)
wrdf$grouping <- as.integer(wrdf$grouping)
wrdf <- wrdf %>% mutate(highWind = ifelse(speed > 8,1,0))
wrdf <- wrdf %>% mutate(nearNorth = ifelse(dir < -.9,1,0))
summary(lm(so2 ~ grouping + highWind*nearNorth, wrdf))
```

```
## 
## Call:
## lm(formula = so2 ~ grouping + highWind * nearNorth, data = wrdf)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.446 -0.542 -0.273  0.222 33.365 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         0.95576    0.14313   6.678 3.52e-11 ***
## grouping           -0.16332    0.09515  -1.716   0.0863 .  
## highWind            0.12138    0.09794   1.239   0.2154    
## nearNorth           0.04422    0.22162   0.200   0.8419    
## highWind:nearNorth  1.58154    0.26757   5.911 4.30e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.659 on 1361 degrees of freedom
## Multiple R-squared:  0.09615,	Adjusted R-squared:  0.09349 
## F-statistic: 36.19 on 4 and 1361 DF,  p-value: < 2.2e-16
```
Basically, fewer kilns on = less SO2 observed at the monitor; high winds blowing north = more SO2 observed at the monitor. (We'd expect as much.)

### Random forest

Okay -- so can we recover kiln state as a function of the monitor? And deal with that temporal autocorrelation? We'll train a [random forest](https://en.wikipedia.org/wiki/Random_forest) model of kiln state. Random forests consist of populations of decision trees composed of random subsets of our training set and random subsets of our predictors. We construct a final model by summing across the forest of decision trees.

```r
set.seed(253)
task <- TaskClassif$new(id = "wind",
                        backend = wdf,
                        target = "grouping")
taskNoDate <- task$select(setdiff(task$feature_names, "date"))

OVER <- ceiling(sum(wdf$grouping==0)/sum(wdf$grouping==1))
po_over <- po("classbalancing",
             id = "oversample",
             adjust = "minor",
             reference = "minor",
             shuffle = FALSE,
             ratio = OVER)

learner <- lrn("classif.ranger",
               num.trees = to_tune(2, 250, logscale = FALSE),
               predict_type = "prob")
learner_over <- as_learner(po_over %>>% learner)

WHICH_TASK <- taskNoDate
WHICH_LEARNER <- learner_over
TRAIN_PCT <- 0.1
RSMP_PCT <- 0.1

train_set <- sample(WHICH_TASK$nrow,
                    TRAIN_PCT * WHICH_TASK$nrow)
test_set <- setdiff(seq_len(WHICH_TASK$nrow),
                    train_set)
acf(wdf$so2)
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/model-params-1.png)<!-- -->

```r
acf(wdf[sort(train_set),"so2"])
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/model-params-2.png)<!-- -->

```r
m <- AutoTuner$new(
  learner = WHICH_LEARNER,
  resampling = rsmp("holdout", ratio = RSMP_PCT),
  measure = msr("classif.specificity"),
  terminator = trm("evals", n_evals = 5),
  tuner = tnr("random_search"))
m$train(WHICH_TASK, row_ids = train_set)
```

```r
prediction <- m$predict(WHICH_TASK, row_ids = test_set)
m$model
```

```
## $learner
## <GraphLearner:oversample.classif.ranger>
## * Model: list
## * Parameters: oversample.ratio=2, oversample.reference=minor,
##   oversample.adjust=minor, oversample.shuffle=FALSE,
##   classif.ranger.num.threads=1, classif.ranger.num.trees=56
## * Packages: mlr3, mlr3pipelines, mlr3learners, ranger
## * Predict Type: prob
## * Feature types: logical, integer, numeric, character, factor, ordered,
##   POSIXct
## * Properties: featureless, hotstart_backward, hotstart_forward,
##   importance, loglik, missings, multiclass, oob_error,
##   selected_features, twoclass, weights
## 
## $tuning_instance
## <TuningInstanceSingleCrit>
## * State:  Optimized
## * Objective: <ObjectiveTuning:oversample.classif.ranger_on_wind>
## * Search Space:
##                          id    class lower upper nlevels
## 1: classif.ranger.num.trees ParamInt     2   250     249
## * Terminator: <TerminatorEvals>
## * Result:
##    classif.ranger.num.trees classif.specificity
## 1:                       56           0.5869565
## * Archive:
##    classif.ranger.num.trees classif.specificity
## 1:                      186           0.4782609
## 2:                       56           0.5869565
## 3:                      108           0.4130435
## 4:                      141           0.5217391
## 5:                      184           0.5217391
```

```r
prediction$confusion
```

```
##         truth
## response   0   1
##        0 598 267
##        1 264 213
```

```r
measure <- msrs(c("classif.acc",
                  "classif.sensitivity",
                  "classif.specificity",
                  "classif.fbeta"))
prediction$score(measure)
```

```
##         classif.acc classif.sensitivity classif.specificity       classif.fbeta 
##           0.6043219           0.6937355           0.4437500           0.6925304
```
Okay, so this a pretty poor model of kiln behavior. But it's not worthless, and we dealt with a fair amount of the autocorrelation. Basically, when the model guesses "kilns are off," it's correct about 44.7 percent of the time, and over the test set, it catches about 44.4 percent of the cases. Given that the cases in question appear in about 35.8 percent of the dataset, a random guesser (that just guessed "OFF" 35.8 percent of the time) would have only captured ~11.8 percent of cases in the test set, given their temporal distribution. (You can't see it, but the numbers in that last sentence were generated on the fly by a random binomial distribution model we ran at the time this document was compiled.) We're doing better than the random guesser.

So what's going on inside the model?

```r
x <- wdf %>% select(so2:peak_slope)
model <- Predictor$new(m, data = x, y = wdf$grouping)
f <- names(x)
effect <- FeatureEffects$new(model)
plot(effect, features = f)
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/feature-importance-1.png)<!-- -->

```r
impo <- FeatureImp$new(model, loss = "ce")
impo$plot(features = f)
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/feature-importance-2.png)<!-- -->
('1' in the effect plot above -- not the feature importance chart -- refers to the OFF state.) Check out that direction plot -- big effect of southerly winds (i.e. wind blowing to the north) on the probability of the OFF state. OFF also appears to be related to higher wind speeds and instances in which the wind is shifting into a southerly direction. The SO2 figures are important but trickier to interpret. Part of the reason that's true is there are (presumably) two effects captured at once here: Rising SO2 may trigger a change in the firm's behavior *and* a change in the firm's behavior may spur change in recordings at the monitor. Anecdotally, that interpretation maps on to the v-shaped `so2_slope` effect and the swoosh-shaped `so2` effect.

Now let's load in and transform the new data from TCEQ, run it through the model, and see what (if any) inferences we can make about the missing kiln states.

```r
wNso2 <- read_delim(here('dat/raw/txt/TCEQ5min.txt'),
                 col_types = cols(
                   Date = col_character(),
                   Time = col_time(),
                   `1071_Sulfur Dioxide_1` = col_character()
                )
              )
colnames(wNso2) <- c("day",
                     "time",
                     "so2")
wNso2$date <- parse_date_time(paste(wNso2$day,
                                    wNso2$time),
                              orders = "%Y%m%d %H:%M:%S")
wNso2$so2 <- wNso2$so2 %>% gsub("‐","-",.) %>% as.numeric()
```

```
## Warning in wNso2$so2 %>% gsub("‐", "-", .) %>% as.numeric(): NAs introduced by
## coercion
```

```r
wNso2 <- wNso2 %>% 
  mutate_if(is.numeric,
            ~replace_na(.,mean(., na.rm = TRUE))) %>%
  drop_na() %>%
  mutate(y = year(date),
         m = month(date), 
         d = day(date),
         h = hour(date)) %>%
  group_by(y,m,d,h) %>%
  summarise(across(date, first),
            across(so2, mean, na.rm = TRUE)) %>%
  ungroup() %>%
  select(date,
         so2) %>%
  filter(date > as.Date("2019-07-05"))
```

```
## `summarise()` has grouped output by 'y', 'm', 'd'. You can override using the
## `.groups` argument.
```

```r
wNwind <- read_csv(here('dat/clean/csv/PortArthurWind.csv'),
                   skip = 1)
```

```
## Rows: 282271 Columns: 18
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (4): MM, DD, hh, mm
## dbl (14): YY, WDIR, WSPD, GST, WVHT, DPD, APD, MWD, PRES, ATMP, WTMP, DEWP, ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
wNwind$date <- parse_date_time(paste0(wNwind$YY,
                                      wNwind$MM,
                                      wNwind$DD," ",
                                      wNwind$hh,
                                      wNwind$mm),
                           orders = "%Y%m%d %H%M")
is.na(wNwind$WSPD) <- wNwind$WSPD == 99
is.na(wNwind$WDIR) <- wNwind$WDIR == 999
is.na(wNwind$GST) <- wNwind$GST == 99
wNwind <- wNwind %>% select(date,WSPD,WDIR,GST,YY,MM,DD,hh) %>%
  mutate_if(is.numeric,
            ~replace_na(.,mean(., na.rm = TRUE))) %>%
  drop_na() %>%
  group_by(YY,MM,DD,hh) %>%
  summarise(across(date, first),
            across(WSPD:GST, mean, na.rm = TRUE)) %>%
  ungroup() %>%
  select(date,
         WSPD,
         WDIR,
         GST) %>%
  filter(date > as.Date("2019-07-05"))
```

```
## `summarise()` has grouped output by 'YY', 'MM', 'DD'. You can override using
## the `.groups` argument.
```

```r
colnames(wNwind) <- c("date",
                      "speed",
                      "dir",
                      "peak")

wNdf <- wNso2 %>% merge(wNwind)

wNdf$dir <- cos( wNdf$dir * (pi/180) )
wNdf$speed <- wNdf$speed * 2.23694
wNdf$peak <- wNdf$peak * 2.23694

wNdf$interval <- get_intervals(wNdf)
wNdf <- wNdf %>%
  mutate(across(so2:peak, ~ .x, .names = '{col}_sd')) %>%
  mutate(across(so2:peak, ~ .x, .names = '{col}_slope')) %>%
  group_by(interval) %>%
  summarise(across(date, first),
            across(so2:peak, mean, na.rm = TRUE),
            across(so2_sd:peak_sd, sd),
            across(so2_slope:peak_slope, slope)) %>%
  mutate_if(is.numeric,
            ~replace_na(.,mean(., na.rm = TRUE))) %>%
  drop_na() %>%
  ungroup() %>%
  select(date:peak_slope)
```
Moment of faith:

```r
newPrediction <- m$predict_newdata(wNdf,
                                   task = WHICH_TASK)
```
Kind of anti-climactic, huh? Here's what happened: Recall that working with the test data, the classifier made 'OFF' predictions 35.5 percent of the time. (It was correct only 44.7 percent of the time, but captured 44.4 percent of OFF cases.)

Working with the *new* data from TCEQ, the classifier made 'OFF' predictions 34.4 percent of the time. Infer what you will...

And take a look at those datasets stacked up against one another:

```r
vizdf <- wNdf %>%
  mutate(grouping = 2) %>%
  rbind(wdf) %>%
  group_by(grouping)
skim(vizdf)
```


Table: Data summary

|                         |         |
|:------------------------|:--------|
|Name                     |vizdf    |
|Number of rows           |5905     |
|Number of columns        |14       |
|_______________________  |         |
|Column type frequency:   |         |
|numeric                  |12       |
|POSIXct                  |1        |
|________________________ |         |
|Group variables          |grouping |


**Variable type: numeric**

|skim_variable |grouping | n_missing| complete_rate|  mean|   sd|     p0|   p25|   p50|   p75|  p100|hist  |
|:-------------|:--------|---------:|-------------:|-----:|----:|------:|-----:|-----:|-----:|-----:|:-----|
|so2           |0        |         0|             1|  0.95| 1.57|  -1.16|  0.28|  0.56|  1.09| 26.22|▇▁▁▁▁ |
|so2           |1        |         0|             1|  0.78| 1.92|  -1.20|  0.17|  0.41|  0.85| 35.74|▇▁▁▁▁ |
|so2           |2        |         0|             1|  0.39| 0.99|  -0.80|  0.06|  0.22|  0.50| 41.30|▇▁▁▁▁ |
|speed         |0        |         0|             1|  8.69| 3.88|   0.30|  6.04|  8.18| 11.26| 24.28|▃▇▅▁▁ |
|speed         |1        |         0|             1|  8.89| 3.76|   1.53|  6.16|  8.21| 11.14| 21.51|▃▇▅▂▁ |
|speed         |2        |         0|             1|  7.16| 3.42|   0.36|  4.79|  6.63|  8.88| 36.52|▇▅▁▁▁ |
|dir           |0        |         0|             1| -0.07| 0.66|  -0.99| -0.74| -0.04|  0.53|  0.97|▇▃▃▅▆ |
|dir           |1        |         0|             1| -0.31| 0.64|  -0.99| -0.86| -0.56|  0.25|  0.98|▇▂▂▂▂ |
|dir           |2        |         0|             1| -0.24| 0.61|  -1.00| -0.81| -0.36|  0.29|  0.99|▇▃▃▃▃ |
|peak          |0        |         0|             1| 12.62| 5.60|   0.30|  8.66| 11.85| 16.37| 34.50|▂▇▅▁▁ |
|peak          |1        |         0|             1| 12.70| 5.51|   2.37|  8.70| 11.74| 15.80| 33.26|▅▇▃▂▁ |
|peak          |2        |         0|             1| 10.89| 4.94|   0.68|  7.42| 10.24| 13.55| 52.20|▇▆▁▁▁ |
|so2_sd        |0        |         0|             1|  0.55| 1.35|   0.00|  0.09|  0.21|  0.51| 16.76|▇▁▁▁▁ |
|so2_sd        |1        |         0|             1|  0.44| 0.93|   0.00|  0.08|  0.18|  0.49| 10.53|▇▁▁▁▁ |
|so2_sd        |2        |         0|             1|  0.21| 0.64|   0.00|  0.04|  0.09|  0.21| 16.32|▇▁▁▁▁ |
|speed_sd      |0        |         0|             1|  1.45| 0.99|   0.00|  0.75|  1.32|  1.85|  7.05|▇▆▁▁▁ |
|speed_sd      |1        |         0|             1|  1.45| 0.93|   0.01|  0.83|  1.26|  1.82|  7.65|▇▃▁▁▁ |
|speed_sd      |2        |         0|             1|  1.36| 0.89|   0.00|  0.78|  1.22|  1.71|  8.49|▇▂▁▁▁ |
|dir_sd        |0        |         0|             1|  0.16| 0.16|   0.00|  0.04|  0.11|  0.21|  0.90|▇▂▁▁▁ |
|dir_sd        |1        |         0|             1|  0.16| 0.16|   0.00|  0.04|  0.11|  0.22|  0.89|▇▂▁▁▁ |
|dir_sd        |2        |         0|             1|  0.25| 0.24|   0.00|  0.06|  0.19|  0.38|  1.31|▇▂▂▁▁ |
|peak_sd       |0        |         0|             1|  2.06| 1.43|   0.00|  1.06|  1.87|  2.59| 11.16|▇▃▁▁▁ |
|peak_sd       |1        |         0|             1|  2.06| 1.35|   0.01|  1.15|  1.85|  2.52| 11.76|▇▃▁▁▁ |
|peak_sd       |2        |         0|             1|  1.90| 1.27|   0.00|  1.05|  1.70|  2.39| 11.90|▇▂▁▁▁ |
|so2_slope     |0        |         0|             1| -0.09| 2.17| -28.64| -0.24|  0.00|  0.24| 21.69|▁▁▇▁▁ |
|so2_slope     |1        |         0|             1| -0.10| 1.35| -13.64| -0.24|  0.00|  0.15|  6.73|▁▁▁▇▁ |
|so2_slope     |2        |         0|             1|  0.00| 1.22| -45.33| -0.10|  0.00|  0.08| 21.74|▁▁▁▇▁ |
|speed_slope   |0        |         0|             1|  0.02| 3.36| -14.99| -1.76|  0.00|  1.53| 15.75|▁▂▇▁▁ |
|speed_slope   |1        |         0|             1|  0.03| 3.17| -11.89| -1.66|  0.00|  1.76| 12.07|▁▂▇▂▁ |
|speed_slope   |2        |         0|             1|  0.01| 3.10| -19.64| -1.56|  0.00|  1.58| 18.16|▁▁▇▁▁ |
|dir_slope     |0        |         0|             1| -0.01| 0.44|  -1.59| -0.14|  0.00|  0.10|  1.96|▁▂▇▁▁ |
|dir_slope     |1        |         0|             1|  0.00| 0.42|  -1.68| -0.13|  0.00|  0.11|  1.74|▁▁▇▁▁ |
|dir_slope     |2        |         0|             1|  0.00| 0.57|  -1.99| -0.19|  0.00|  0.14|  1.99|▁▂▇▁▁ |
|peak_slope    |0        |         0|             1|  0.01| 4.85| -22.27| -2.55|  0.00|  2.23| 24.64|▁▂▇▁▁ |
|peak_slope    |1        |         0|             1|  0.04| 4.58| -20.68| -2.35|  0.00|  2.61| 16.77|▁▁▇▃▁ |
|peak_slope    |2        |         0|             1|  0.02| 4.43| -24.31| -2.17|  0.00|  2.21| 27.20|▁▂▇▁▁ |


**Variable type: POSIXct**

|skim_variable |grouping | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|:--------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|date          |0        |         0|             1|2018-08-01 00:05:00 |2019-07-01 10:00:00 |2018-12-19 10:30:00 |      958|
|date          |1        |         0|             1|2018-08-06 12:00:00 |2019-07-04 10:00:00 |2019-03-11 02:00:00 |      533|
|date          |2        |         0|             1|2019-07-05 01:00:00 |2022-04-30 16:00:00 |2020-11-21 19:00:00 |     4414|

```r
ggplot(vizdf, aes(x = grouping, y = dir)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/summary-stats-1.png)<!-- -->

```r
ggplot(vizdf, aes(x = grouping, y = speed)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/summary-stats-2.png)<!-- -->

```r
ggplot(vizdf, aes(x = grouping, y = so2)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")
```

![](/Users/claytonaldern/Developer/gristy/windblow/img/png/summary-stats-3.png)<!-- -->
Okay!

```r
# work in progress
# requires dev version of {mlr3spatiotempcv}

# set.seed(206)
# cv = rsmp("repeated_cv", folds = 5, repeats = 2)
# cv$instantiate(taskNoDate)
# rNT = mlr3::resample(
#   task = taskNoDate, learner = learner,
#   resampling = cv)
# 
# taskT = task$set_col_roles("date", roles = "time")
# cvT = rsmp("sptcv_cstf", folds = 5)
# cvT$instantiate(taskT)
# rT = mlr3::resample(
#   task = taskT, learner = learner,
#   resampling = cvT)
# 
# rNT$aggregate(measures = measure)
# rT$aggregate(measures = measure)
```
(That section is under development but would also seem to suggest we've dealt sufficiently with the temporal autocorrelation problem.)

```r
sessionInfo()
```

```
## R version 4.2.0 (2022-04-22)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS Big Sur/Monterey 10.16
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
##  [1] grid      parallel  tools     compiler  splines   stats     graphics 
##  [8] grDevices utils     datasets  methods   base     
## 
## other attached packages:
##   [1] patchwork_1.1.1        bslib_0.3.1            munsell_0.5.0         
##   [4] digest_0.6.29          reprex_2.0.1           bbotk_0.5.3           
##   [7] mlr3misc_0.10.0        data.table_1.14.2      readxl_1.4.0          
##  [10] rmarkdown_2.14         tzdb_0.3.0             utf8_1.2.2            
##  [13] uuid_1.1-0             crayon_1.5.2           mlr3cluster_0.1.3     
##  [16] modelr_0.1.8           future.apply_1.9.0     withr_2.5.0           
##  [19] haven_2.5.0            pillar_1.8.1           mgcv_1.8-40           
##  [22] DBI_1.1.3              generics_0.1.2         R6_2.5.1              
##  [25] magrittr_2.0.3         parallelly_1.32.0      tidyselect_1.1.2      
##  [28] labeling_0.4.2         prediction_0.3.14      archive_1.1.5         
##  [31] evaluate_0.15          lattice_0.20-45        pkgconfig_2.0.3       
##  [34] rlang_1.0.6            repr_1.1.4             mlr3viz_0.5.9         
##  [37] boot_1.3-28            palmerpenguins_0.1.0   checkmate_2.1.0       
##  [40] highr_0.9              stringi_1.7.6          mlr3fselect_0.7.1     
##  [43] sass_0.4.1             mlr3data_0.6.0         yaml_2.3.5            
##  [46] mlr3tuningspaces_0.2.0 hms_1.1.2              mlr3filters_0.5.0     
##  [49] lgr_0.4.3              vroom_1.5.7            scales_1.2.0          
##  [52] future_1.26.1          lifecycle_1.0.3        rvest_1.0.2           
##  [55] globals_0.15.0         mlr3measures_0.4.1     xfun_0.31             
##  [58] nlme_3.1-157           vctrs_0.5.1            jquerylib_0.1.4       
##  [61] cellranger_1.1.0       Rcpp_1.0.9             glue_1.6.2            
##  [64] gtable_0.3.0           htmltools_0.5.2        cli_3.5.0             
##  [67] fastmap_1.1.0          assertthat_0.2.1       Matrix_1.4-1          
##  [70] backports_1.4.1        httr_1.4.4             dbplyr_2.1.1          
##  [73] cluster_2.1.3          broom_0.8.0            Metrics_0.1.4         
##  [76] jsonlite_1.8.4         knitr_1.39             codetools_0.2-18      
##  [79] xml2_1.3.3             ranger_0.13.1          mvtnorm_1.1-3         
##  [82] fansi_1.0.3            bit64_4.0.5            bit_4.0.4             
##  [85] mlr3tuning_0.13.1      farver_2.1.0           listenv_0.8.0         
##  [88] rstudioapi_0.13        clue_0.3-61            fs_1.5.2              
##  [91] base64enc_0.1-3        rprojroot_2.0.3        clusterCrit_1.2.8     
##  [94] ellipsis_0.3.2         colorspace_2.0-3       paradox_0.9.0         
##  [97] iml_0.11.0             mlr3pipelines_0.4.1    mlr3learners_0.5.3    
## [100] mlr3verse_0.2.5        mlr3_0.13.3            circular_0.4-95       
## [103] lubridate_1.8.0        skimr_2.1.4            forcats_0.5.1         
## [106] stringr_1.4.0          dplyr_1.0.9            purrr_0.3.4           
## [109] readr_2.1.2            tidyr_1.2.0            tibble_3.1.8          
## [112] ggplot2_3.3.6          tidyverse_1.3.1        here_1.0.1            
## 
## loaded via a namespace (and not attached):
## [1] ggdist_3.1.1         distributional_0.3.0
```

\
<div class="tocify-extend-page" data-unique="tocify-extend-page" style="height: 0;"></div>
