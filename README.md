# trackr
This R package provides tools to analyze and visualize animal video tracking data.

### Goals
##### Offer a framework for the analysis of tracking data that is independent to the tracking software used.
There are many tracking solutions available (both proprietary and free software), and many come with their own analysis tools. I believe it is more efficient to have one strong analysis framework for analysis, that can read in data from many trackers. This saves developers from having to offer analysis tools, and the user from re-learning if they make a switch. It can also enable wider adoption of trackers without native analysis tools, such as *idTracker*.
##### Offer a way to analyze animal tracking data within R.
`R` currently offers some of the best statistics and visualization packages out there, and `trackr` output easily works with them. `R` is also the main (and often only) language that many end users (biologists) already know, making it a logical choice in many cases.

### Installation
You can install this package from within your R session using `devtools`. You currently also need my fork of `multidplyr` for it to work, as there are currently some bugs (the fork is up-to-date and compatible with the main `multidplyr` repository).
````{r}
# Install devtools if necessary
# install.packages('devtools')
devtools::install_github('Ax3man/multidplyr')
devtools::install_github('Ax3man/trackr')
````

### Currently supported trackers

- [Ctrax](http://ctrax.sourceforge.net/)
- [idTracker](http://www.idtracker.es/)

Support for other trackers can be added. Please file an [issue](https://github.com/Ax3man/trackr/issues).

### Performance

`R` is not typically known for lightning fast performance. However, `trackr` makes use of several packages in the *Hadleyverse* that greatly speed things up. Specifically, it uses `readr` to read in large data files and `dplyr` for data manipulation. On top of that, the two largest tables in the tracks object (see `?as_tracks`) are kept on worker nodes so all calculations are in parallel. Keeping the tables on the workers minimizes data copying (eliminating the added cost of parallel computations).

### Example code
As a quick example, let's read in some data of male guppy pairs.
````{r}
library(trackr)
library(dplyr)

# Read in data
tr <- read_idTracker(folder = 'data')
tr <- as_tracks(tr, frame_rate = 30, resolution = 1080)

# We can get a quick overview by plotting the first ten thousand frames for all four trials
# (Note that we use the %>% (pipe) operator from margrittr (and dplyr).)
filter(tr, frame < 10000) %>% plot()
````
![plot1](http://i.imgur.com/zgxRpSz.png)

All plotting functions output ggplot objects, so they can be easily manipulated:
````{r}
filter(tr, frame < 10000) %>% plot() + ggplot2::scale_color_manual(values = c('purple', 'limegreen'))
````
![plot1b](http://i.imgur.com/gbWvcA1.png)

Since these are quite long trials, I want to look how the behavior changes for one trial, let's say between minutes 20 and 60. We can use filter, just like in `dplyr`.
````{r}
filter(tr, trial == 'trial2-1', frame %in% 36000:108000) %>% 
  plot_time_facets(time_bins = 10, nrow = 2)
````
It looks like some periods are a bit more active than others.

![plot2](http://i.imgur.com/ABR8siE.png)
Ok, so we have our trajectories, and most of it seems quite ok. Let's calculate some per frame statistics.

````{r}
tr <- tr %>%
  add_speed() %>%
  add_acceleration() %>%
  add_turn()
````
Summmary statistics work very similar to `dplyr`. We do have add some new tables to our `tracks` object though (see `?as_tracks`).
````{r}
tr <- tr %>% 
  expand_tracks(group = FALSE, animal = TRUE, soc = FALSE, trial = TRUE) %>%
  summarize(mean_speed = mean(speed, na.rm = TRUE),
                       sum_abs_turn = sum(abs(turn), na.rm = TRUE))

tr$animal

Source: local data frame [8 x 4]

     trial animal mean_speed sum_abs_turn
    (fctr) (fctr)      (dbl)        (dbl)
1 trial2-2      1 0.15063087     121350.9
2 trial2-2      2 0.16505333     135011.6
3 trial2-1      1 0.05598333     287765.2
4 trial2-1      2 0.07007088     287118.0
5 trial3-2      1 0.03191847     371926.9
6 trial3-2      2 0.03815877     392532.3
7 trial3-1      1 0.10114426     182766.9
8 trial3-1      2 0.10817431     171243.5
````
