# trackr
This R package provides tools to analyze and visualize animal video tracking data.

### Goals
##### Offer a framework for the analysis of tracking data that is independent to the tracking software used.
There are many tracking solutions available (both proprietary and free software), and many come with their own analysis tools. I believe it is more efficient to have one strong analysis framework for analysis, that can read in data from many trackers. This saves developers from having to offer analysis tools, and the user from re-learning if they make a switch. It can also enable wider adoption of trackers without native analysis tools, such as *idTracker*.
##### Offer a way to analyze animal tracking data within R.
`R` currently offers some of the best statistics and visualization packages out there, and `trackr` output easily works with them. `R` is also the main (and often only) language that many end users (biologists) already know, making it a logical choice in many cases.

### Installation
You can install this package from within your R session using `devtools`. 
````{r}
# Install devtools if necessary
# install.packages('devtools')
devtools::install_github('Ax3man/trackr')
````

### Currently supported trackers

- [Ctrax](http://ctrax.sourceforge.net/)
- [idTracker](http://www.idtracker.es/)

Support for other trackers can be added. Please file an [issue](https://github.com/Ax3man/trackr/issues).

### Performance

`R` is not typically known for lightning fast performance. However, `trackr` makes use of several packages that  speed things up, and make code writing easier. Specifically, it uses `data.table` to read in large data files and `dplyr` for data manipulation.

### Example code
As a quick example, let's read in some data of male guppy pairs.
````{r}
library(trackr)
library(dplyr)

# You would read in your data like this:
# guppies <- read_idTracker(folder = 'data')
# But 'guppies' is included with the package, so you can now run:
tr <- as_tracks(guppies, frame_rate = 30, resolution = 1080)
````
We can get a quick overview by plotting the tracks.

````{r}
plot(tr)
````
![plot1](http://i.imgur.com/ISihYqC.png)

It's easy to apply filters to the data before plotting, and we can use the
pipe operator (`%>%`) to chain commands together:

````{r}
filter(tr, frame %in% 10000:11000, drop = TRUE) %>% plot()
````
![plot1b](http://i.imgur.com/2ehU7o7.png)

All plotting functions output ggplot objects, so they can be easily manipulated:
````{r}
plot(tr) +
  ggplot2::facet_grid(trial ~ animal) +
  ggplot2::scale_color_manual(values = c('purple', 'limegreen'))
````
![plot1c](http://i.imgur.com/XPVNlv2.png)

If you'd like to see  how the behavior changes over the course of a trial, you can use facet by time instead.
````{r}
plot_time_facets(tr)
````
![plot2](http://i.imgur.com/PWsmB7F.png)
It looks like some periods are a bit more active than others.

Ok, so we have our trajectories, and most of it seems quite ok. Let's calculate some per frame statistics. We use the `mutate` syntax from `dplyr`.

````{r}
tr <- mutate(tr,
             speed = speed(),
             acc = acceleration(),
             turn = turn())
````
Summmary statistics also work very similar to `dplyr`.
````{r}
tr <- summarize(tr, 
                mean_speed = mean(speed, na.rm = TRUE),
                sum_abs_turn = sum(abs(turn), na.rm = TRUE))

# See summary statistics per animal:
tr$animal

# Source: local data frame [4 x 4]
# 
#    trial animal mean_speed sum_abs_turn
#   (fctr) (fctr)      (dbl)        (dbl)
# 1      a      1   2.092943     23557.73
# 2      a      2   2.569784     25572.35
# 3      b      1   2.675133     15169.35
# 4      b      2   4.018834     14620.87

# See summary statistics per trial:
tr$trial

# Source: local data frame [4 x 4]
# Groups: trial [?]
# 
#    trial mean_speed sum_abs_turn
#   (fctr)      (dbl)        (dbl)
# 1      a   2.331364     49130.07
# 2      b   3.346984     29790.21

````
