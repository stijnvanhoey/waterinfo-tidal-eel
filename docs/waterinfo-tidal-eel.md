Use wateRinfo package to understand eel behaviour in the Scheldt river
================
2018-12-20

Introduction
------------

The [Fish acoustic receiver network](http://lifewatch.be/en/fish-acoustic-receiver-network) is a EU Lifewatch supported infrastructure to track the distribution, movements and habitat use of migratory fish. Among other species, we tag and study eels to get data-based knowledge about their migration behaviour. By using this information we can propose effective policy measures to support this criticially endangered species.

Interested in the fascinating live of the eel and eager to understand what we're talking about when mentioning `transmitters` and `receivers`? Check out the animated video starring affiliated Phd student Pieterjan Verhelst:

[![Tagging research on eel in Belgium](https://img.youtube.com/vi/7YQVgl3QPyY/0.jpg)](https://www.youtube.com/watch?v=7YQVgl3QPyY)

Pieter-Jan provided an interesting subset of the fish tracking data. In this blog post, we'll do some explorative data analysis and show a research application of the [wateRinfo R package](https://inbo.github.io/wateRinfo/) which was recently accepted through the rOpenSci onboarding process.

Eel tracking data
-----------------

Let's start with the tracking data of the eel and check the favorite places of our tagged eel to hang out in the Scheldt estuary.

``` r
library(tidyverse)
library(lubridate)
library(leaflet)
```

The eel tracking data contains the residence time intervals between the `Arrival` and `Departure` of an eel, supplied with a `Transmitter`, at a certain `Receiver` station. Each `Receiver` station does have a `latitude`, `longitude` and a `station` name:

| Date       | Transmitter    | Receiver    |  latitude|  longitude| station    | Arrival             | Departure           |  Detections|  residencetime|
|:-----------|:---------------|:------------|---------:|----------:|:-----------|:--------------------|:--------------------|-----------:|--------------:|
| 2016-10-19 | A69-1601-52644 | VR2W-112297 |  51.00164|    3.85695| s-Wetteren | 2016-10-19 23:44:00 | 2016-10-19 23:48:00 |           4|            240|
| 2016-10-19 | A69-1601-52644 | VR2W-112287 |  51.00588|    3.77876| s-2        | 2016-10-19 16:07:00 | 2016-10-19 19:12:00 |          54|          11100|
| 2016-10-20 | A69-1601-52644 | VR2W-122322 |  51.02032|    3.96965| s-2a       | 2016-10-20 13:18:00 | 2016-10-20 13:23:00 |           3|            300|
| 2016-10-20 | A69-1601-52644 | VR2W-122322 |  51.02032|    3.96965| s-2a       | 2016-10-20 02:21:00 | 2016-10-20 02:29:00 |           5|            480|
| 2016-10-20 | A69-1601-52644 | VR2W-115438 |  51.01680|    3.92527| s-Wichelen | 2016-10-20 01:01:00 | 2016-10-20 01:09:00 |           6|            480|
| 2016-10-20 | A69-1601-52644 | VR2W-122322 |  51.02032|    3.96965| s-2a       | 2016-10-20 05:52:00 | 2016-10-20 06:00:00 |           5|            480|

For each interval, the number of raw detections of the `Receiver` for each `Transmitter` is provided together with the derived `residencetime` of the `Transmitter`, i.e. the eel. As such, we can calculate the eel *favourite place to hang out* by calculating the total residence time around each receiver.

On a map, this looks like:

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-5-1.png)

Apparently, the eel carrying transmitter `A69-1601-52622` likes to be around the municipality Hamme and a section between the municipalities Wetteren and Wichelen.

For the remainder of this post, let's give a more compelling name than the Transmitter code she is carrying. So, the eel with transmitter `A69-1601-52622` is officially named **Buttercup**.

The map overview with total residence time lacks temporal information. We do not know *when* the eel are passing these receiver stations. Let's reconfigure adn plot the eel tracking data to see their route in the Scheldt river as function of time:

``` r
eel <- eel %>%
    mutate(eel_name = recode(Transmitter, 
                             `A69-1601-52644` = "Buttercup"))
```

| Date       | Transmitter    | Receiver    |  latitude.x|  longitude.x| station    | Arrival             | Departure           |  Detections|  residencetime| eel\_name | reference\_station |  distance\_to\_refence\_station| station\_id |  longitude.y|  latitude.y| station\_type |
|:-----------|:---------------|:------------|-----------:|------------:|:-----------|:--------------------|:--------------------|-----------:|--------------:|:----------|:-------------------|-------------------------------:|:------------|------------:|-----------:|:--------------|
| 2016-10-19 | A69-1601-52644 | VR2W-112297 |    51.00164|      3.85695| s-Wetteren | 2016-10-19 23:44:00 | 2016-10-19 23:48:00 |           4|            240| Buttercup | ws-DL7             |                        149062.2| VR2W-112297 |      3.85695|    51.00164| receiver      |
| 2016-10-19 | A69-1601-52644 | VR2W-112287 |    51.00588|      3.77876| s-2        | 2016-10-19 16:07:00 | 2016-10-19 19:12:00 |          54|          11100| Buttercup | ws-DL7             |                        155620.9| VR2W-112287 |      3.77876|    51.00588| receiver      |
| 2016-10-20 | A69-1601-52644 | VR2W-122322 |    51.02032|      3.96965| s-2a       | 2016-10-20 13:18:00 | 2016-10-20 13:23:00 |           3|            300| Buttercup | ws-DL7             |                        138957.3| VR2W-122322 |      3.96965|    51.02032| receiver      |
| 2016-10-20 | A69-1601-52644 | VR2W-122322 |    51.02032|      3.96965| s-2a       | 2016-10-20 02:21:00 | 2016-10-20 02:29:00 |           5|            480| Buttercup | ws-DL7             |                        138957.3| VR2W-122322 |      3.96965|    51.02032| receiver      |
| 2016-10-20 | A69-1601-52644 | VR2W-115438 |    51.01680|      3.92527| s-Wichelen | 2016-10-20 01:01:00 | 2016-10-20 01:09:00 |           6|            480| Buttercup | ws-DL7             |                        143593.5| VR2W-115438 |      3.92527|    51.01680| receiver      |
| 2016-10-20 | A69-1601-52644 | VR2W-122322 |    51.02032|      3.96965| s-2a       | 2016-10-20 05:52:00 | 2016-10-20 06:00:00 |           5|            480| Buttercup | ws-DL7             |                        138957.3| VR2W-122322 |      3.96965|    51.02032| receiver      |

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-10-1.png)

We picked up the signal around Ghent/Merelbeke and Buttercup resided for a while in the Scheldt river before migration towards the sea. Once left, they start their long journey towards the Sargasso Sea.

The periodic movement pattern of Buttercup during the second half of november 2016 is of particular interest, as it looks like a tidal frequency. It would be interesting to compare the movement pattern with real water level data from the Scheldt river... that's what the [wateRinfo](https://inbo.github.io/wateRinfo/) package is all about.

**Note:** To represent the data along a straight line (y-axis), we calculated the distances from each station to a reference station close to the sea (`ws-DL7`) . We used a `costDistance` function to derive the distances along the Scheldt river. Interested? See [the code repository](https://github.com/stijnvanhoey/wateRinfo_post) for more the details on the derivation.

### Tidal data from Waterinfo

[Waterinfo.be](http://waterinfo.be), managed by the [Flemisch Environmental agency (VMM)](https://en.vmm.be/) and [Flanders Hydraulics Research (WL)](https://www.waterbouwkundiglaboratorium.be/en/home), is a great resource for a wide set of abiotic variables. The water height of the Scheldt river is among these variables, providing detailed information of [tidal conditions](https://www.waterinfo.be/default.aspx?path=NL/Thema/Getij_Actueel).

However, downloading the data from the website is both tedious (too much clicking) and not repeatable for future analysis. The goal of our [wateRinfo package](https://github.com/inbo/wateRinfo) is to facilitate access to the variety of data by providing an R interface to download time series data. We will use the package to access tidal/waterheight data.

For installation instructions, see [the package documentation](https://inbo.github.io/wateRinfo/). As wateRinfo is not registered on CRAN, you will need the `devtools` package to install the wateRinfo package. Once installed, load the library:

``` r
library(wateRinfo)
```

As water levels in the Scheldt river are not among their core supported variables, we use the tidal stations overview as currently discussed on the wateRinfo [GitHub issues](https://github.com/inbo/wateRinfo/issues/11#issuecomment-442901628). This list of station identifiers is precompiled to derive all tidal VMM/WL station timeseries:

The following list provides an overview of the time series identifiers of the relevant measurement stations along the Scheldt River with a 10 minute interval:

|  ts\_value\_id| datetime            | req\_timestamp |  station\_latitude|  station\_longitude| station\_id | station\_no   | station\_name              | dataprovider | dataowner |  significant| Portal\_Bekken        | station\_no\_short |    ts\_id| ts\_name | parametertype\_name | stationparameter\_name |
|--------------:|:--------------------|:---------------|------------------:|-------------------:|:------------|:--------------|:---------------------------|:-------------|:----------|------------:|:----------------------|:-------------------|---------:|:---------|:--------------------|:-----------------------|
|              1| 2018-11-29 15:30:00 | NA             |           51.05264|            4.197217| 0430087     | 04zes42a-1066 | Sint-Amands tij/Zeeschelde | MOW-HIC      | HIC       |            0| Beneden-Scheldebekken | zes42a-1066        |  55419010| Pv.10    | W                   | W                      |
|              3| 2018-11-29 15:30:00 | NA             |           51.10964|            4.173916| 0430029     | 04dur01a-1066 | Tielrode tij/Durme         | MOW-HIC      | HIC       |            0| Beneden-Scheldebekken | dur01a-1066        |  55565010| Pv.10    | W                   | W                      |
|              7| 2018-11-29 15:30:00 | NA             |           51.12281|            4.218673| 0430081     | 04zes36a-1066 | Temse tij/Zeeschelde       | MOW-HIC      | HIC       |            0| Beneden-Scheldebekken | zes36a-1066        |  55493010| Pv.10    | W                   | W                      |
|              9| 2018-11-29 15:30:00 | NA             |           51.03456|            4.103857| 0419418     | 04zes47a-1066 | Dendermonde tij/Zeeschelde | MOW-HIC      | HIC       |            0| Beneden-Scheldebekken | zes47a-1066        |  54186010| Pv.10    | W                   | W                      |
|             17| 2018-11-29 15:33:00 | NA             |           51.14328|            4.330275| 0430078     | 04zes28a-1066 | Hemiksem tij/Zeeschelde    | MOW-HIC      | HIC       |            0| Beneden-Scheldebekken | zes28a-1066        |  54493010| Pv.10    | W                   | W                      |
|             18| 2018-11-29 15:30:00 | NA             |           51.00467|            4.007578| 0430091     | 04zes49a-1066 | Schoonaarde tij/Zeeschelde | MOW-HIC      | HIC       |            0| Beneden-Scheldebekken | zes49a-1066        |  55355010| Pv.10    | W                   | W                      |

Or shown on a map:

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-14-1.png)

[This vignette](https://inbo.github.io/wateRinfo/articles/download_timeseries_batch.html) of the wateRinfo package provides the example code to download the data for multiple stations at once using the `tidyverse verbs`. We use a similar approach, but instead of manually providing the start (`from`) and end (`to`) data, we get these from the eels tracking data period:

``` r
start_tracking_period <- min(eel$Date)
end_tracking_period <- max(eel$Date)

tidal_data <- ts_id_tide %>%
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

As such, we have all the bits and pieces together to combine the tidal data set with the tracking data of Buttercup during the second half of november 2016 and verify if Buttercup is using the tide to move around.

Is Buttercup using the tide?
----------------------------

To represent the data of all the individual stations together with the tracks, we'll have to figure out an alternative way. One way to combine all tidal data in a single plot is to use its respective location in the Scheldt river and translate the water height in ridges instead of lines, as provided by the [`ggridges` package](https://cran.r-project.org/web/packages/ggridges/vignettes/gallery.html):

``` r
library(ggridges)
```

By extending the plot of the tracking data of Buttercup with the water levels measured in tidal measurement statsions, we can verify the eel movement pattern and the possible link with the water level data:

![](waterinfo-tidal_files/figure-markdown_github/unnamed-chunk-22-1.png)

Using the ridges, for each location the water level is plotted as function of time at that specific location.

Looking at the graph, Buttercup seems to be *lazy* and following the tidal movement for a while before setting of to the sea. When the water comes inland from the sea (higher water levels), Buttercup follows the water upstream. When the water returns to the sea again to low tide, Buttercup moves again to the direction of the sea.

By linking the eels tracking data with the tidal data, we get more insight to demistify the behaviour of these very interesting species. With the [wateRinfo](https://inbo.github.io/wateRinfo/) package, we hope to support researchers in answering a lot more questions.

For a more scientific approach to check the selective tidal stream transport of eel, check out the [scientific publication](https://www.sciencedirect.com/science/article/pii/S0272771418304530?via%3Dihub) by Pieter-Jan.

For the full code of this blogpost, check out [this repository](https://github.com/stijnvanhoey/wateRinfo_post).

**Acknowledgements:**

Thanks Buttercup for being our study objects and hopefully you had a save journey towards the sargasso sea. Thanks to Pieter-Jan Verhelst for the providing the data.

We also would like to thank [Laura DeCicco](https://github.com/ldecicco-USGS) for reviewing the wateRinfo package as well as the editor [Karthik Ram](https://github.com/karthik).
