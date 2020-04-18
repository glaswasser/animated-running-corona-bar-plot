library(shiny)
library(nCov2019)
library(plotly)
library(tidyverse)
library(gganimate)

## DESCRIPTION: 
# a running corona-barchart


#### LOAD DATA ####
y <- load_nCov2019(lang = 'en', source='github')

# get global data:
d = y['global']

# FORMAT DATA
formatted <- d %>% 
  group_by(time) %>% #group by the date
  mutate(rank = min_rank(-cum_confirm) * 1, # create a rank for every day 
         Value_lbl = paste0(" ", round(cum_confirm))) %>%  # create labels for the 
  group_by(country) %>% 
  filter(rank <= 10) %>%#
  filter(time > as.Date("2020-02-15")) %>% 
  ungroup()

# animate
animated <- 
  # create a ggplot with ..
  ggplot(formatted, aes(rank, group = country, 
                        fill = as.factor(country), color = as.factor(country))) +
  # the aesthetics for the bars: y position, height, alpha is the opacity
  geom_tile(aes(y = cum_confirm/2,
                height = cum_confirm,
                width = 0.9), alpha = 0.8, color = NA) +
  # the labels for the countries in front, the paste0 needs to be there for the right position
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.1, hjust = 1) +
  # the labels for the numbers. somehow this only works with the labels for rounded numbers like that.
  geom_text(aes(y=cum_confirm, label = paste0(Value_lbl), hjust = 0)) +
  # flip the coordinate system, clip off for the right display
  coord_flip(clip = "off", expand = FALSE) +
  # reverse the x-scale to have the largest bar on plot
  scale_x_reverse() +
  # to make the background grid lines moving (view_follow creates warnings but can be ignored)
  scale_y_continuous(labels = scales::comma) +
  view_follow(fixed_x = TRUE)  +
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
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2, 2, 2, 4, "cm")) +
  # the transition between the states is defined here
  transition_states(time, transition_length = 4, state_length = 1) +
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
animated

# alternatively, use
animate(animated, 100, fps = 25, duration = 20, width = 800, height = 600)



# create a a gif:
animate(animated, 100, fps = 25, duration = 20,  width = 1200, height = 1000,
        renderer = gifski_renderer("confirmed_cases.gif"))



# create a mp4:
animate(animated, 200, fps = 30,  width = 1200, height = 1000,
        renderer = ffmpeg_renderer()) -> for_mp4anim_save("animation.mp4", animation = for_mp4 )

