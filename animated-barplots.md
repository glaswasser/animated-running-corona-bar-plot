Animated Barplots
================
Alexander Heinz

# ANIMATED BARPLOTS

I really like these animated barplots and was wondering if it’s possible
in R to create such beautiful animations. I dug the web and
stackoverflow and R-bloggers had answers for me. It is indeed possible
to create nice animation movies out of plots in R and save them as GIF
or mp4\!

# Goals:

  - create a bar plot as known from “data is beautiful” (youtube
    <https://www.youtube.com/channel/UCkWbqlDAyJh2n8DN5X6NZyg>)
  - use n-COVID-19 data to see the top countries animated
  - smoothen it up\!

This is largely inspired and / or copied from the following two
sources:

<https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other>

<https://www.r-bloggers.com/how-to-create-bar-race-animation-charts-in-r/>

I added as much comments in the code as possible. Have fun recreating /
improving\!

Let’s start by loading the data and do some data wrangling:

``` r
# to get nCov2019 dataset:
#install.packages("remotes")
remotes::install_github("YuLab-SMU/nCov2019")

library(nCov2019) # the dataset
library(plotly) # the animation library
library(tidyverse) # useful for wrangling
library(gganimate) # for the animations
library(gifski)
library(magrittr)
library(magick)

#### LOAD DATA ####
res <- query()
```

    ## last update: 2021-03-14 
    ## Gloabl total  120382782  cases; and  2664281  deaths
    ## Gloabl total affect country or areas: 221
    ## Gloabl total recovered cases: 269199
    ## last update: 1 
    ## Total Candidates Programs : 51 
    ## Total Candidates Programs : 54

``` r
y =res$historical
d <- y["global"]
# get global data:

# FORMAT DATA
formatted <- d %>% 
  group_by(date) %>% # group by the date
  mutate(rank = min_rank(-cases) * 1, # create a rank for every day 
         Value_lbl = paste0(" ", round(cases))) %>%  # create labels for the values
  group_by(country) %>% 
  filter(rank <= 10) %>% # get the top 10 ranks every day
  filter(date > as.Date("2020-01-01")) %>%  # filter custom date
  ungroup()

#tail(formatted)
formatted$date = as.Date(formatted$date)
```

The animation:

``` r
# animate
animated <- 
  # create a ggplot with ..
  ggplot(formatted, aes(rank, group = country, 
                        fill = as.factor(country), color = as.factor(country))) +
  # the aesthetics for the bars: y position, height, alpha is the opacity
  geom_tile(aes(y = cases/2,
                height = cases,
                width = 0.9), alpha = 0.8, color = NA) +
  # the labels for the countries in front, the paste0 needs to be there for the right position
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.1, hjust = 1) +
  # the labels for the numbers. somehow this only works with the labels for rounded numbers like that.
  geom_text(aes(y=cases, label = paste0(Value_lbl), hjust = 0)) +
  # flip the coordinate system, clip off for the right display
  coord_flip(clip = "off", expand = FALSE) +
  # reverse the x-scale to have the largest bar on top
  scale_x_reverse() +
  # to make the background grid lines moving (view_follow creates warnings but can be ignored)
  scale_y_continuous(labels = scales::comma) +
  view_follow() +
  # this is for removing redundant labels etc:
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        # aesthetics for the title, subtitle etc.
        plot.title=element_text(size=20, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=15, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2, 2, 2, 4, "cm")) +
  # the transition between the states is defined here
  transition_states(date, transition_length = 25, state_length = 5) +
  # to make the countries overtake each other more smoothly:
  # more options to smoothen the transitions
  enter_grow() +
  exit_shrink() +
  ease_aes("linear") +
  labs(title = 'Cumulative confirmed cases on {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "Tianzhi Wu, Erqiang Hu, Xijin Ge*, Guangchuang Yu*. 
         Open-source analytics tools for studying the COVID-19 coronavirus outbreak. medRxiv, 2020.02.25.20027433. 
         doi: https://doi.org/10.1101/2020.02.25.20027433") 

# this is the animation here
#animated
#animate(animated, 100, fps = 25, duration = 20, width = 800, height = 600)

animate(animated, duration = 75, fps = 25, renderer = magick_renderer())
```

![](animated-barplots_files/figure-gfm/unnamed-chunk-2-1.gif)<!-- -->

``` r
#animate(animated)
#animate(animated, 100, fps = 30, duration = 100,  width = 1200, height = 1000,
#        renderer = gifski_renderer("confirmed_cases_new.gif"))
# create a mp4:
#animate(animated, 200, fps = 30,  width = 1200, height = 1000,
#        renderer = ffmpeg_renderer()) -> for_mp4anim_save("animation.mp4", animation = for_mp4 )


rm(list = ls()); gc()
```

    ##           used (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells 1218348 65.1    2975874 159.0         NA  2975874 159.0
    ## Vcells 2208251 16.9   10146329  77.5      16384 10146329  77.5
