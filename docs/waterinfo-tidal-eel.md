---
slug: "waterinfo-tidal-eel"
title: wateRinfo - Downloading tidal data to understand the behaviour of a migrating eel
authors:
  - name: Stijn Van Hoey
    url: https://orcid.org/0000-0001-6413-3185
    twitter: SVanHoey
  - name: Peter Desmet
    url: https://orcid.org/0000-0002-8442-8025
    twitter: peterdesmet
date: 2019-01-15
categories: blog
tags:
  - software-peer-review
  - r
  - package
  - water
  - data-access
  - tidal-data
  - fishes
  - movement-data
  - acoustic-telemetry
output:
#  html_document:
#    df_print: kable
  md_document:
    df_print: kable
    preserve_yaml: true
    variant: markdown_github
always_allow_html: yes
knit: (function(input_file, encoding) {
  rmarkdown::render(input_file, encoding = encoding, output_dir = "../docs") })
---

> Do you know what that sound is, Highness? Those are the Shrieking Eels
> — if you don’t believe me, just wait. They always grow louder when
> they’re about to feed on human flesh. If you swim back now, I promise,
> no harm will come to you. I doubt you will get such an offer from the
> Eels.
>
> *Vizzini, [The Princess Bride](https://youtu.be/KGcat9tGZVU)*

European eels ([*Anguilla
anguilla*](https://en.wikipedia.org/wiki/Anguilla_anguilla)) have it
tough. Not only are they depicted as monsters in movies, they are
[critically
endangered](https://doi.org/10.2305/IUCN.UK.2014-1.RLTS.T60344A45833138.en)
in real life. One of the many aspects that is contributing to their
decline is the reduced connectivity between their freshwater and marine
habitats. Eels are catadromous: they live in freshwater, but migrate to
the Sargasso Sea to spawn, a route that is blocked by numerous human
structures (shipping locks, sluices, pumping stations, etc.). [Pieterjan
Verhelst](http://www.marinebiology.ugent.be/node/68491) studies the
impact of these structures on the behaviour of eels, making use of the
[fish acoustic receiver
network](http://lifewatch.be/en/fish-acoustic-receiver-network) that was
established as part of the Belgian LifeWatch observatory. This animated
video gives a quick introduction to his research and the receiver
network:

[![Tagging research on eel in
Belgium](https://img.youtube.com/vi/7YQVgl3QPyY/0.jpg)](https://www.youtube.com/watch?v=7YQVgl3QPyY)

In this blog post, we’ll explore if the migration of one eel is
influenced by the tide. It’s a research use case for our R package
[wateRinfo](https://ropensci.github.io/wateRinfo/), which was recently
[peer reviewed](https://github.com/ropensci/software-review/issues/255)
(thanks to reviewer [Laura DeCicco](https://github.com/ldecicco-USGS)
and editor [Karthik Ram](https://github.com/karthik) for their
constructive feedback!) and accepted as a community-contributed package
to [rOpenSci](https://github.com/ropensci/wateRinfo).

### Meet Princess Buttercup

Pieterjan provided us the [tracking
data](https://github.com/stijnvanhoey/waterinfo-tidal-eel/blob/master/data/eel_track.csv)
for eel with transmitter `A69-1601-52622`. Let’s call her **Princess
Buttercup**, after the princess that almost got eaten by the Shrieking
Eels in the classic and immensly quotable movie [The Princess
Bride](https://www.imdb.com/title/tt0093779/quotes/).

``` r
eel <- read_csv(here("data", "eel_track.csv"))
```

Her tracking data consists of the residence time interval (`arrival`
until `departure`) at each `receiver` station that detected her along
the [Scheldt](https://en.wikipedia.org/wiki/Scheldt) river. It also
contains the number of `detections` and calculated `residencetime` (in
seconds), as well as the `station` name, `latitude` and `longitude`.

| date       | transmitter    | receiver    |  latitude|  longitude| station    | arrival             | departure           |  detections|  residencetime|
|:-----------|:---------------|:------------|---------:|----------:|:-----------|:--------------------|:--------------------|-----------:|--------------:|
| 2016-10-19 | A69-1601-52644 | VR2W-112297 |  51.00164|    3.85695| s-Wetteren | 2016-10-19 23:44:00 | 2016-10-19 23:48:00 |           4|            240|
| 2016-10-19 | A69-1601-52644 | VR2W-112287 |  51.00588|    3.77876| s-2        | 2016-10-19 16:07:00 | 2016-10-19 19:12:00 |          54|          11100|
| 2016-10-20 | A69-1601-52644 | VR2W-122322 |  51.02032|    3.96965| s-2a       | 2016-10-20 13:18:00 | 2016-10-20 13:23:00 |           3|            300|
| 2016-10-20 | A69-1601-52644 | VR2W-122322 |  51.02032|    3.96965| s-2a       | 2016-10-20 02:21:00 | 2016-10-20 02:29:00 |           5|            480|
| 2016-10-20 | A69-1601-52644 | VR2W-115438 |  51.01680|    3.92527| s-Wichelen | 2016-10-20 01:01:00 | 2016-10-20 01:09:00 |           6|            480|
| 2016-10-20 | A69-1601-52644 | VR2W-122322 |  51.02032|    3.96965| s-2a       | 2016-10-20 05:52:00 | 2016-10-20 06:00:00 |           5|            480|

Using the `latitude`, `longitude` and total `residencetime` for each
station, we can map where Princess Buttercup likes to hang out:

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-88f2da5c39d8ee4b42b6">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addCircleMarkers","args":[[51.00588,51.03855,51.14319,51.00164,51.04231,51.19069,51.09258,51.3552573,51.0168,51.12287,51.11414,51.03955,51.3482621,51.4071859,51.3970414,51.02032,51.0434,51.07363,51.07363,51.3544901,51.03203,51.05593,51.437785,51.3648799,51.3465544,51.235423,51.14222,51.40221,51.19068,51.352186],[3.77876,4.1175,4.33078,3.85695,4.0945,4.32954,4.17083,3.7609896,3.92527,4.2184,4.24123,4.16605,3.8147323,3.7080883,3.7153436,3.96965,4.04783,4.19007,4.19007,3.795027,4.18166,4.20024,3.470347,3.765539,3.800172,4.396673,4.32646,4.02998,4.33602,3.781008],[3.08333333333333,10.7416666666667,0.266666666666667,2.81666666666667,11.525,1.75,1.70833333333333,0.00833333333333333,5.31666666666667,0.358333333333333,3.26666666666667,0.533333333333333,0.0666666666666667,0.3,0.216666666666667,12.0916666666667,21.2666666666667,6.69166666666667,0.966666666666667,3.18333333333333,7.2,11.5083333333333,0.283333333333333,0.0333333333333333,0.383333333333333,0.2,1.83333333333333,0.00833333333333333,0.466666666666667,4.85],null,null,{"interactive":true,"className":"","stroke":false,"color":"#e66101","weight":5,"opacity":0.5,"fill":true,"fillColor":"#e66101","fillOpacity":0.6},null,null,["VR2W-112287","VR2W-112292","VR2W-112295","VR2W-112297","VR2W-113521","VR2W-113528","VR2W-115430","VR2W-115434","VR2W-115438","VR2W-115441","VR2W-115442","VR2W-115444","VR2W-115447","VR2W-120876","VR2W-120882","VR2W-122322","VR2W-122325","VR2W-122336","VR2W-122339","VR2W-122346","VR2W-122363","VR2W-122367","VR2W-123817","VR2W-124071","VR2W-126202","VR2W-127716","VR2W-127719","VR2W-127734","VR2W-127738","VR2W-129566"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[51.00164,51.437785],"lng":[3.470347,4.396673]}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

### Moving up and down the Scheldt river

To get a better sense of her journey along the river, we add a
`distance_to_sea` (in meters) for the stations, by joining the tracking
data with a [distance reference
file](https://github.com/stijnvanhoey/waterinfo-tidal-eel/blob/master/data/distance_from_sea.csv)[1].
We can now plot her movement over time and distance:

![](/Users/peter_desmet/Coding/Repositories/peterdesmet/waterinfo-tidal-eel/docs/waterinfo-tidal-eel_files/figure-markdown_github/plot_track-1.png)

Princess Buttercup’s signal was picked up by receivers in Merelbeke
(near Ghent) shortly after she was captured and released there on
October 11. She resided in a 40 km stretch of the river (between
Wetteren and Sint-Amands) for about a month before migrating towards the
sea and starting the long journey towards the Sargasso Sea. The periodic
movement pattern up and down the river during the second half of
November is of particular interest: it looks like tidal frequency[2]. It
would be interesting to compare the movement pattern with real water
level data from the Scheldt river… which is where our wateRinfo package
comes in.

### Getting tidal data with the wateRinfo package

[Waterinfo.be](http://waterinfo.be), managed by the [Flanders
Environment Agency (VMM)](https://en.vmm.be/) and [Flanders Hydraulics
Research](https://www.waterbouwkundiglaboratorium.be), is a website
where one can find real-time water and weather related environmental
variables for Flanders (Belgium), such as rainfall, air pressure,
discharge, and water level. The website also provides an
[API](https://www.waterinfo.be/download/9f5ee0c9-dafa-46de-958b-7cac46eb8c23?dl=0)
to download time series of these measurements as open data, but
compositing the download URL with the proper system codes can be
challenging. To facilitate users in searching for stations and
variables, subsequently downloading data of interest and incorporating
waterinfo.be data access in repeatable workflows, we developed the R
package [wateRinfo](https://github.com/ropensci/wateRinfo) to do just
that. See the [package
documentation](https://ropensci.github.io/wateRinfo/) for more
information on how to install and get started with the package.

Timeseries in waterinfo.be (identified by a `ts_id`) are a combination
of a variable, location (`station_id`) and measurement frequency (15min
by default). For example:

``` r
library(wateRinfo)
get_stations("water_level") %>%
  head()
```

|    ts\_id|  station\_latitude|  station\_longitude| station\_id | station\_no | station\_name                      | stationparameter\_name | parametertype\_name | ts\_unitsymbol | dataprovider |
|---------:|------------------:|-------------------:|:------------|:------------|:-----------------------------------|:-----------------------|:--------------------|:---------------|:-------------|
|  92956042|           51.27835|            4.189765| 39483       | L04\_00H    | Kieldrecht/Noordzuidverbinding     | H                      | H                   | m              | VMM          |
|  31683042|           50.87379|            4.697672| 21995       | K08\_015    | Leuven/Dijle1e arm/stuw K3         | Hopw                   | H                   | m              | VMM          |
|  20682042|           51.17875|            3.211181| 20301       | K02\_WM004  | StMichiels/Kerkebeek/Rooster       | Hopw                   | H                   | m              | VMM          |
|  96495042|           51.18228|            2.986944| 10413       | L01\_42E    | Oudenburg/Magdalenakreek           | H                      | H                   | m              | VMM          |
|  25074042|           50.96749|            5.167799| 20788       | K09\_002    | Schulen/Inlaat A/WB\_Schulensbroek | Hafw                   | H                   | m              | VMM          |
|   2006042|           50.96803|            2.885085| 10432       | L01\_49A    | Merkem/Steenbeek                   | H                      | H                   | m              | VMM          |

At the time of writing (see [this
issue](https://github.com/ropensci/wateRinfo/issues/11)), the stations
measuring [water levels in the Scheldt tidal
zone](https://www.waterinfo.be/default.aspx?path=NL/Thema/Getij_Actueel)
are not yet included by the API under the core variable `water_level`
and are thus not yet available via `get_stations("water_level")`. We
therefore rely on a [precompiled list of tidal time series
identifiers](https://github.com/stijnvanhoey/waterinfo-tidal-eel/blob/master/data/tidal_zone_ts_ids.csv)
(`tidal_zone_ts_ids`):

``` r
tidal_zone_ts_ids <- read_csv(here("data", "tidal_zone_ts_ids.csv"))
```

From which we select the 10-min frequency tidal timeseries in the
Scheldt river:

|    ts\_id| station\_id | station\_name                | portal\_bekken        |
|---------:|:------------|:-----------------------------|:----------------------|
|  55419010| 0430087     | Sint-Amands tij/Zeeschelde   | Beneden-Scheldebekken |
|  55565010| 0430029     | Tielrode tij/Durme           | Beneden-Scheldebekken |
|  55493010| 0430081     | Temse tij/Zeeschelde         | Beneden-Scheldebekken |
|  54186010| 0419418     | Dendermonde tij/Zeeschelde   | Beneden-Scheldebekken |
|  54493010| 0430078     | Hemiksem tij/Zeeschelde      | Beneden-Scheldebekken |
|  55355010| 0430091     | Schoonaarde tij/Zeeschelde   | Beneden-Scheldebekken |
|  53989010| 0419484     | Antwerpen tij/Zeeschelde     | Beneden-Scheldebekken |
|  54606010| 0430071     | Kallosluis tij/Zeeschelde    | Beneden-Scheldebekken |
|  56088010| 0430062     | Prosperpolder tij/Zeeschelde | Beneden-Scheldebekken |
|  54936010| 0430068     | Liefkenshoek tij/Zeeschelde  | Beneden-Scheldebekken |
|  55059010| 0430242     | Melle tij/Zeeschelde         | Beneden-Scheldebekken |

[This wateRinfo
vignette](https://ropensci.github.io/wateRinfo/articles/download_timeseries_batch.html)
shows how to download data for multiple stations at once using
`wateRinfo` and [`dplyr`](https://dplyr.tidyverse.org/). We use a
similar approach, but instead of manually providing the start and end
date, we get these from Princess Buttercup’s tracking data:

``` r
tidal_data <-
  tidal_zone_ts_ids %>%
  group_by(ts_id) %>%
  
  # Download tidal data for each ts_id (time series id)
  do(get_timeseries_tsid(
      .$ts_id,
      from = min(eel$date), # Start of eel tracking data
      to = max(eel$date),   # End of eel tracking data
      datasource = 4
  )) %>%
  
  # Join data back with tidal_zone_ts_id metadata
  ungroup() %>%
  left_join(tidal_zone_ts_ids, by = "ts_id")
```

In just a few lines of code, we downloaded the tidal data for each
measurement station for the required time period. 👌

The water level is expressed in `mTAW`[3] (meter above mean sea level).
Let’s plot the data for a station (here Dendermonde in November 2016) to
have a look:

![](/Users/peter_desmet/Coding/Repositories/peterdesmet/waterinfo-tidal-eel/docs/waterinfo-tidal-eel_files/figure-markdown_github/plot_tide_dendermonde-1.png)

We now have all the pieces to verify if Princess Buttercup *was* surfing
the tide back and forth.

### Is Princess Buttercup surfing the tide?

Let’s add the tidal data to Princess Buttercup’s journey plot we created
before. The first step is to join the tidal data with the same distance
reference file to know the distance from each tidal station to the sea:

``` r
tidal_data <-
  tidal_data %>%
  left_join(
    distance_from_sea %>% select(station, distance_from_sea, municipality),
    by = c("station_name" = "station")
  ) %>%
  filter(station_name != "Hemiksem tij/Zeeschelde") # Exclude (probably erroneous) data from Hemiksem
```

To avoid visual clutter, we’ll use ridges (from
[`ggridges`](https://cran.r-project.org/web/packages/ggridges/vignettes/gallery.html))
to display the tidal data for each station over time:

![](/Users/peter_desmet/Coding/Repositories/peterdesmet/waterinfo-tidal-eel/docs/waterinfo-tidal-eel_files/figure-markdown_github/plot_track_and_tide-1.png)

Looking at the plot, Princess Buttercup seems to be “lazy” and drift
with the tide. Rising water levels push her upstream, while decreasing
water levels bring her closer to sea again. On November 22 (see also
previous plot), she embarks on her migration for real.

Conclusion
----------

In this blogpost we used the wateRinfo package to gain some insight in
the movement/migration behaviour of an individual eel. We hope the
package can support many more research questions and that you [have fun
storming the
castle](https://www.imdb.com/title/tt0093779/quotes/qt0482745).

-   For a more in depth study of eel migration behaviour in response to
    the tide, see [Verhelst et
    al. (2018)](https://doi.org/10.1016/j.ecss.2018.08.025).
-   For more info on the package, see [the package
    website](https://ropensci.github.io/wateRinfo/).
-   For the full code of this blogpost, see [this
    repository](https://github.com/stijnvanhoey/waterinfo-tidal-eel).

[1] To represent the data along a straight line (y-axis), we calculated
the distance along the river from each station to a reference station
close to the sea (`ws-DL7`), using a `costDistance` function. See [this
script](https://github.com/stijnvanhoey/waterinfo-tidal-eel/blob/master/src/stations_distances.ipynb)
for more the details on the calculation.

[2] The Scheldt is under tidal influence from its river mouth all the
way to Ghent (160km upstream) where it is stopped by sluices. The tide
goes much further than the freshwater-saltwater boundary of the river.

[3] TAW means [*Tweede Algemene
Waterpassing*](https://nl.wikipedia.org/wiki/Tweede_Algemene_Waterpassing),
a reference height for Belgium.
