Introduction
------------

The [Fish acoustic receiver network](http://lifewatch.be/en/fish-acoustic-receiver-network) is a EU Lifewatch supported infrastructure to track the distribution, movements and habitat use of migratory fish. Among other species, we tag and study eels to get data-based knowledge about their migration behaviour. By using this information we can propose effective policy measures to support this criticially endangered species.

Interested in the fascinating live of the eel and eager to understand what we're talking about when mentioning `transmitters` and `receivers`? Check out the animated video starring affiliated Phd student Pieterjan Verhelst:

[![Tagging research on eel in Belgium](https://img.youtube.com/vi/7YQVgl3QPyY/0.jpg)](https://www.youtube.com/watch?v=7YQVgl3QPyY)

Pieter-Jan provided an interesting subset of the fish tracking data. In this blog post, we'll do some explorative data analysis and show a research application of the [wateRinfo R package](https://inbo.github.io/wateRinfo/) which was recently accepted through the rOpenSci onboarding process.

Eel tracking data
-----------------

Let's start with the tracking data of the eels and check the favorite places of two of our tagged eels to hang out in the Scheldt estuary.

``` r
library(tidyverse)
library(lubridate)
library(leaflet)
```

The eel tracking data contains the residence time intervals between the `Arrival` and `Departure` of an eel, supplied with a `Transmitter`, at a certain `Receiver` station. Each `Receiver` station does have a `latitude`, `longitude` and a `station` name:

    ## # A tibble: 6 x 10
    ##   Date       Transmitter Receiver latitude longitude station
    ##   <date>     <chr>       <chr>       <dbl>     <dbl> <chr>  
    ## 1 2016-10-11 A69-1601-5… VR2W-11…     51.0      3.78 s-2    
    ## 2 2016-10-11 A69-1601-5… VR2W-11…     51.0      3.86 s-Wett…
    ## 3 2016-10-12 A69-1601-5… VR2W-11…     51.0      3.93 s-Wich…
    ## 4 2016-10-12 A69-1601-5… VR2W-11…     51.0      3.93 s-Wich…
    ## 5 2016-10-13 A69-1601-5… VR2W-11…     51.0      3.93 s-Wich…
    ## 6 2016-10-13 A69-1601-5… VR2W-11…     51.0      3.93 s-Wich…
    ## # ... with 4 more variables: Arrival <dttm>, Departure <dttm>,
    ## #   Detections <int>, residencetime <int>

For each interval, the number of raw detections of the `Receiver` for each `Transmitter` is provided together with the derived `residencetime` of the `Transmitter`, i.e. the eel. As such, we can calculate the eels *favourite place to hang out* by calculating the total residence time around each receiver.

On a map, this looks like:

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-5-1.png)

Apparently, the eel carrying transmitter `A69-1601-52622` likes to be around the municipality Hamme and a section between the municipalities Wetteren and Wichelen, whereas eel `A69-1601-52644` prefers to swim around different sections of the Scheldt river.

For the remainder of this post, let's give both eels a more compelling name than the Transmitter code their carrying. So the eel with transmitter `A69-1601-52622` is officially named **Greg** (the roots of the famous cyclist [Greg Van Aevermaet](https://en.wikipedia.org/wiki/Greg_Van_Avermaet) is in Hamme as well) and the eel with transmitter `A69-1601-52644` will be called **Olivier**.

The map overview with total residence time lacks temporal information. We do not know *when* the eels were passing these receiever stations. Let's reconfigure adn plot the eel tracking data to see their route in the Scheldt river as function of time:

``` r
eels <- eels %>%
    mutate(eel_name = recode(Transmitter, 
                             `A69-1601-52622` = "Greg", 
                             `A69-1601-52644` = "Olivier"))
```

    ## # A tibble: 6 x 17
    ##   Date       Transmitter Receiver latitude.x longitude.x station
    ##   <date>     <chr>       <chr>         <dbl>       <dbl> <chr>  
    ## 1 2016-10-11 A69-1601-5… VR2W-11…       51.0        3.78 s-2    
    ## 2 2016-10-11 A69-1601-5… VR2W-11…       51.0        3.86 s-Wett…
    ## 3 2016-10-12 A69-1601-5… VR2W-11…       51.0        3.93 s-Wich…
    ## 4 2016-10-12 A69-1601-5… VR2W-11…       51.0        3.93 s-Wich…
    ## 5 2016-10-13 A69-1601-5… VR2W-11…       51.0        3.93 s-Wich…
    ## 6 2016-10-13 A69-1601-5… VR2W-11…       51.0        3.93 s-Wich…
    ## # ... with 11 more variables: Arrival <dttm>, Departure <dttm>,
    ## #   Detections <int>, residencetime <int>, eel_name <chr>,
    ## #   reference_station <chr>, distance_to_refence_station <dbl>,
    ## #   station_id <chr>, longitude.y <dbl>, latitude.y <dbl>,
    ## #   station_type <chr>

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-10-1.png)

For both eel, we picked up the signal around Ghent/Merelbeke and they resided for a while in the Scheldt river before migration towards the sea. Once left, they start their long journey towards the Sargasso Sea. However, both eel clearly seem to behave differently while moving towards the sea.

Similar to the map representation, Greg is residential for longer periods on a few specific places, whereas Olivier travels around much more along the river section between Temse and Merelbeke.

The periodic movement pattern of Olivier during the second half of november 2016 is of particular interest, as it looks like a tidal frequency. It would be interesting to compare the movement pattern with real water level data from the Scheldt river... that's what the [wateRinfo](https://inbo.github.io/wateRinfo/) package is all about.

**Note:** To represent the data along a straight line (y-axis), we calculated the distances from each station to a reference station close to the sea (`ws-DL7`) . We used a `costDistance` function to derive the distances along the Scheldt river. Interested? See [the code repository](https://github.com/stijnvanhoey/wateRinfo_post) for more the details on the derivation.

### Tidal data from Waterinfo

[Waterinfo.be](http://waterinfo.be), managemed by the [Flemisch Environmental agency (VMM)](https://en.vmm.be/) and [Flanders Hydraulics Research (WL)](https://www.waterbouwkundiglaboratorium.be/en/home), is a great resource for a wide set of abiotic variables. The water height of the Scheldt river is among these variables, providing detailed information of [tidal behaviour](https://www.waterinfo.be/default.aspx?path=NL/Thema/Getij_Actueel).

However, downloading the data from the website is both tedious (too much clicking) and not repeatable for future analysis. The goal of our [wateRinfo package](https://github.com/inbo/wateRinfo) is to facilitate access to the variety of data by providing an R interface to download time series data. We will use the package to access tidal/waterheight data.

For installation instructions, see [the package documentation](https://inbo.github.io/wateRinfo/). As wateRinfo is not registered on CRAN, you will need the `devtools` package to install the wateRinfo package. Once installed, load the library:

``` r
library(wateRinfo)
```

As water levels in the Scheldt river are not among their core supported variables, we use the tidal stations overview as currently discussed on the wateRinfo [GitHub issues](https://github.com/inbo/wateRinfo/issues/11#issuecomment-442901628). This list of station identifiers is precompiled to derive all tidal VMM station timeseries:

The following list provides an overview of the time series identifiers of the relevant measurement stations along the Scheldt River with a 10 minute interval:

    ## # A tibble: 6 x 17
    ##   ts_value_id datetime            req_timestamp station_latitude
    ##         <int> <dttm>              <chr>                    <dbl>
    ## 1           1 2018-11-29 15:30:00 <NA>                      51.1
    ## 2           3 2018-11-29 15:30:00 <NA>                      51.1
    ## 3           4 2018-11-29 15:32:00 <NA>                      51.1
    ## 4           7 2018-11-29 15:30:00 <NA>                      51.1
    ## 5           9 2018-11-29 15:30:00 <NA>                      51.0
    ## 6          17 2018-11-29 15:33:00 <NA>                      51.1
    ## # ... with 13 more variables: station_longitude <dbl>, station_id <chr>,
    ## #   station_no <chr>, station_name <chr>, dataprovider <chr>,
    ## #   dataowner <chr>, significant <int>, Portal_Bekken <chr>,
    ## #   station_no_short <chr>, ts_id <int>, ts_name <chr>,
    ## #   parametertype_name <chr>, stationparameter_name <chr>

Or shown on a map:

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-14-1.png)

[This vignette](https://inbo.github.io/wateRinfo/articles/download_timeseries_batch.html) of the wateRinfo package provides the example code to download the data for multiple stations at once using the `tidyverse verbs`. We use a similar approach, but instead of manually providing the start (`from`) and end (`to`) data, we get these from the eels tracking data period:

``` r
start_tracking_period <- min(eels$Date)
end_tracking_period <- max(eels$Date)

tidal_data <- ts_id_tide %>%
    filter(ts_id < 100000000) %>% # hack to exclude unworking ids
    group_by(ts_id) %>%
    do(get_timeseries_tsid(.$ts_id,
                           from = start_tracking_period,
                           to = end_tracking_period,
                           datasource = 4)) %>%
    ungroup() %>%
    left_join(ts_id_tide, by = "ts_id")
```

In just a few lines R code, we downloaded for each of the tidal measurement stations the data for the required time period. Make sure to check out the other tutorials as well on the [wateRinfo package website](https://inbo.github.io/wateRinfo/index.html).

The water level is expressed in meter TAW (or mTAW, meter above mean sea level). By combining `dplyr` and `ggplot2`, we can plot the data of the station in Dendermonde during November 2016:

``` r
tidal_plot <- tidal_data %>% 
    filter(station_name %in% c("Dendermonde tij/Zeeschelde")) %>%
    filter(Timestamp > "2016-10-31", Timestamp <= "2016-11-30") %>%
    ggplot() +
        geom_line(aes(x = Timestamp, y = Value), color = "#2b8cbe") +
        scale_x_datetime(date_labels = "%Y-%m-%d",
                         date_breaks = "7 days") +
        facet_grid(rows = vars(station_name)) +
        theme_minimal() + ylab("Water level (mTAW)") + xlab("")
tidal_plot
```

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-17-1.png)

**Note:** TAW means [*Tweede Algemene Waterpassing*](https://nl.wikipedia.org/wiki/Tweede_Algemene_Waterpassing), a reference height for Belgium

To highlight the periods of low and high tide in the figure, we want to define the moment of low and high water level peaks. We could do this by extracting the local maxima of the tidal time series or by assuming a regular interval, but these low and high water level peaks are also available as data source from waterinfo.be, using the `HWLW` keyword in the `ts_name` time serie name. So, similar to downloading the 10 minutes time series, we can download only the peaks using the wateRinfo package.

For the Dendermonde station the high and low flow peaks are provided by the following time series:

    ## # A tibble: 1 x 17
    ##   ts_value_id datetime            req_timestamp station_latitude
    ##         <int> <dttm>              <chr>                    <dbl>
    ## 1           9 2018-11-29 15:30:00 <NA>                      51.0
    ## # ... with 13 more variables: station_longitude <dbl>, station_id <chr>,
    ## #   station_no <chr>, station_name <chr>, dataprovider <chr>,
    ## #   dataowner <chr>, significant <int>, Portal_Bekken <chr>,
    ## #   station_no_short <chr>, ts_id <int>, ts_name <chr>,
    ## #   parametertype_name <chr>, stationparameter_name <chr>

Which can be used again to download the data for a chosen period:

``` r
peaks_tide <- ts_id_high_tide %>% group_by(ts_id) %>%
    do(get_timeseries_tsid(.$ts_id,
                           from = min(eels$Date),
                           to = max(eels$Date),
                           datasource = 4)) %>%
    ungroup() %>%
    left_join(ts_id_high_tide, by = "ts_id") %>%
    select(Timestamp, Value, station_no, station_name) %>%
    rename(tide_value = Value)
head(peaks_tide)
```

    ## # A tibble: 6 x 4
    ##   Timestamp           tide_value station_no    station_name              
    ##   <dttm>                   <dbl> <chr>         <chr>                     
    ## 1 2016-10-11 05:50:00       0.95 04zes47a-1066 Dendermonde tij/Zeeschelde
    ## 2 2016-10-11 11:23:00       4.43 04zes47a-1066 Dendermonde tij/Zeeschelde
    ## 3 2016-10-11 18:49:00       0.76 04zes47a-1066 Dendermonde tij/Zeeschelde
    ## 4 2016-10-12 00:21:00       4.71 04zes47a-1066 Dendermonde tij/Zeeschelde
    ## 5 2016-10-12 07:38:00       0.86 04zes47a-1066 Dendermonde tij/Zeeschelde
    ## 6 2016-10-12 12:58:00       4.56 04zes47a-1066 Dendermonde tij/Zeeschelde

By rewrangling the data, we can translate this to tidal start and end periods:

``` r
tide_periods <- peaks_tide %>%
    mutate(high_water = if_else(tide_value > 3, "end", "start")) %>%
    mutate(tide_id = sort(rep(seq(1, nrow(peaks_tide)/2 + 1), 2))[1:nrow(peaks_tide)]) %>%
    select(Timestamp, tide_id, high_water) %>%
    spread(key = high_water, value = Timestamp)
head(tide_periods)
```

    ## # A tibble: 6 x 3
    ##   tide_id end                 start              
    ##     <int> <dttm>              <dttm>             
    ## 1       1 2016-10-11 11:23:00 2016-10-11 05:50:00
    ## 2       2 2016-10-12 00:21:00 2016-10-11 18:49:00
    ## 3       3 2016-10-12 12:58:00 2016-10-12 07:38:00
    ## 4       4 2016-10-13 01:32:00 2016-10-12 20:27:00
    ## 5       5 2016-10-13 14:05:00 2016-10-13 08:55:00
    ## 6       6 2016-10-14 02:35:00 2016-10-13 21:35:00

The derived periods can be used as a background color to highlight each period between low and high tide:

``` r
tidal_plot + geom_rect(data = tide_periods %>% filter(start > "2016-10-31", end <= "2016-11-30"), 
              aes(xmin = start, xmax = end, ymin = 0, ymax = Inf), 
              alpha = 0.15)
```

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-22-1.png)

Still, to represent the data of all the individual stations together with the tracks, we'll have to figure out an alternative way. One way to combine all tidal data in a single plot is to use its respective location in the Scheldt river and translate the water height in color intensity instead of lines, resulting in a

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-24-1.png)

Each horizontal blue-shaded line is a time series downloaded from waterinfo.be and the color intensity represents the water level as function of time.

As such, we have all the bits and pieces together to combine the tidal data set with the tracking data of Olivier during the second half of november 2016 and verify if Olivier is using the tide to move around.

Is Olivier using the tide?
--------------------------

By extending the previous plot with the tracking data of Olivier, we can verify the eel movement pattern and the possible link with the water level data:

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-28-1.png)

Looking at the graph, Olivier seems to be *lazy* and following the tidal movement for a while before setting of to the sea. When the water comes inland from the sea (higher water levels), the eel follows the water upstream. When the water returns to the sea again to low tide, the eel moves again to the direction of the sea.

For a more scientific approach to check the selective tidal stream transport of eel, check out the [scientific publication](https://www.sciencedirect.com/science/article/pii/S0272771418304530?via%3Dihub) by Pieter-Jan.

The previous figure inspired us to use the [ggridges package](https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html) to provide an alternative representation to the color intensity representation of the water levels. Instead, for each location the water level is plotted as function of time. Applying this for a small subset of the data makes the tidal behaviour of the eel even more clear:

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-29-1.png)

During high tide Olivier likes to be in Dendermonde (more upstream), whereas during low tide, moving downstream to Sint-Amands.

And what about Greg?
--------------------

So, Greg seemed to be much more resident on chosen locations. But what if we confront its tracks with the tidal data as well?

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-31-1.png)

Actually, Greg is detected on the same location during high tide, but disappears for short periods during low tide. This indicates that Greg maybe exhibits the same pattern as Olivier after all. It could be that we are just missing the exact location of Greg during low tide periods because of the lack of a receiver at that particular location. In contrast to our [bird tracking data](https://www.gbif.org/dataset/83e20573-f7dd-4852-9159-21566e1e691e) we can not rely on a continous GPS logging for the fishtracking project, so we depend on the presence of receivers to actually measure the presence of the eel.

By linking the eels tracking data with the tidal data, we get more insight to demistify the behaviour of these very interesting species. With the [wateRinfo](https://inbo.github.io/wateRinfo/) package, we hope to support researchers in answering a lot more questions.

For the full code of this blogpost, check out [this repository](https://github.com/stijnvanhoey/wateRinfo_post).

**Acknowledgements:**

Thanks Greg and Olivier for being our study objects and hopefully you had a save journey towards the sargasso sea. Thanks to Pieter-Jan Verhelst for the providing the data.

We also would like to thank [Laura DeCicco](https://github.com/ldecicco-USGS) for reviewing the wateRinfo package as well as the editor [Karthik Ram](https://github.com/karthik).
