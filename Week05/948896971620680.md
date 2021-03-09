Spatial Analytics Assignment 5
================
Johan Kresten Horsmans & Gustav Aarup Lauridsen
3/4/2021

# Kazanlak Valley Analysis

## Task 2: Where in the landscape are the mounds located?

The location of settlements is usually easy to predict as humans need
safety and easy access to water and food resources. These range from
nearby arable soil to pasturage to trading centers. Choices regarding
the mortuary realm are much harder to establish as they are guided by
the social norms of the living rather than the natural environment. Some
environmental conditions, however, play a role, such as intervisibility,
which can be an important factor for societies that use monuments to the
dead for territorial signalling. Before such specific analysis, it is,
however, a good idea to get a general sense of where in the landscape
are mounds located.

In order to produce a formal assessment of mound location, you can start
by using most common aspects of topography, such as elevation, slope,
aspect, and water proximity. Choose one or more of these variables.
Calculate the distribution of classes of each environmental variable
through the entire region (defining, en route, what you consider to be a
“region”?) by extracting values from the digital elevation model and
binning them in several classes. Then, calculate site frequencies within
classes of the environmental variable by sampling mound locations in the
raster and evaluate whether the observed pattern could be a product of
the distribution of environmental categories themselves.

A example workflow with elevations could look like this:

  - extract elevation values from the entire landscape and bin them in
    elevation categories (e.g. 400-500m, 500-600m, 600-700m, etc.).
    Consider: what defines a landscape for the purpose of this study?
    You may wish to crop the Aster to a smaller but representative study
    area but make sure you justify your selection
  - extract elevation values from observed mound locations and review
    their distribution in the same bins
  - calculate the expected number of mounds per bin if mounds were
    located through the landscape randomly
  - compare the expected number with the observed one

<!-- end list -->

``` r
#Loading packages and setting a seed (to ensure reproducible results):
library(pacman)
p_load(sf, raster, dplyr, tmap, ggplot2)

set.seed(123125)
```

#### We start off by loading the data:

``` r
Aster <- raster("data/Aster.tif") #Loading raster containing heights of the landscape.
mounds<-st_read("data/KAZ_mounds.shp") #loading raster containing burial mounds. 
```

    ## Reading layer `KAZ_mounds' from data source `/home/cds-au618771/cds-spatial/cds-spatial-w5/Week05/HW05/data/KAZ_mounds.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 773 features and 5 fields
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: 352481.3 ymin: 4712325 xmax: 371282.4 ymax: 4730029
    ## CRS:            32635

#### We plot the loaded the data:

``` r
tm_shape(Aster) + tm_raster() #Plotting heights.
```

    ## stars object downsampled to 1022 by 979 cells. See tm_shape manual (argument raster.downsample)

    ## Variable(s) "NA" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.

![](Assignment-5_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
tm_shape(mounds) + tm_dots() #Plotting mounds.
```

![](Assignment-5_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

#### We ensure that the two raster share the same CRS:

``` r
crs(Aster)
```

    ## CRS arguments:
    ##  +proj=utm +zone=35 +datum=WGS84 +units=m +no_defs +ellps=WGS84
    ## +towgs84=0,0,0

``` r
crs(mounds)
```

    ## CRS arguments:
    ##  +proj=utm +zone=35 +datum=WGS84 +units=m +no_defs +ellps=WGS84
    ## +towgs84=0,0,0

#### Since they share the same CRS, no transformation is needed. We then proceed to plot the mounds on top of the elevation raster:

``` r
tm_shape(Aster) + 
  tm_raster() + tm_shape(mounds) + tm_dots()
```

    ## stars object downsampled to 1022 by 979 cells. See tm_shape manual (argument raster.downsample)

    ## Variable(s) "NA" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.

![](Assignment-5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### We see that the mounds are scattered across a very small part of the landscape. As such, we wish to crop the elevation raster to fit the locations of the mounds.

``` r
#Converting the mound-points to a single multi-point object, named 'mounds1'.
mounds1 <- st_union(mounds)

#Inspect the mounds1-object to ensure that the transformation worked.
head(mounds1)
```

    ## Geometry set for 1 feature 
    ## geometry type:  MULTIPOINT
    ## dimension:      XY
    ## bbox:           xmin: 352481.3 ymin: 4712325 xmax: 371282.4 ymax: 4730029
    ## CRS:            32635

    ## MULTIPOINT ((352481.3 4724776), (352488.8 47247...

``` r
#Confirm that we went from 6 features to 1 feature (sanity check).
length(mounds)
```

    ## [1] 6

``` r
length(mounds1)
```

    ## [1] 1

``` r
#Create a bounding box around the mounds.
mounds_hull <- st_convex_hull(mounds1)

#Plot the mounds on top of the hull to ensure that the transformation worked.
plot(mounds_hull, col = "red")
plot(mounds1, add = TRUE)
```

![](Assignment-5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

#### We proceed to crop the elevation raster:

``` r
mounds_hull_trans <- as_Spatial(mounds_hull) #Converting the hull from a "sfc_POLYGON" to a "SpatialPolygon". 

cropped_Aster <- crop(Aster, mounds_hull_trans) #Cropping the elevation raster.
```

#### Plotting the mounds on top of the cropped elevation raster:

``` r
tm_shape(cropped_Aster) + 
  tm_raster(title = "Elevations") + tm_shape(mounds) + tm_dots() + 
  tm_layout(main.title = "Mound elevations", 
          legend.title.size = 1,
          legend.text.size = 0.6,
          legend.position = c("left","bottom"),
          legend.bg.color = "white")
```

![](Assignment-5_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

#### Extracting the heights of the individual mounds:

``` r
mounds_heights <- extract(cropped_Aster, mounds) %>% na.omit()

#Ensuring that we haven't discarded any mounds (sanity check):
length(st_geometry(mounds))
```

    ## [1] 773

``` r
length(mounds_heights)
```

    ## [1] 771

We see that two mounds have been removed since they were NA’s

#### We then plot a histogram of the mound height distributions:

``` r
hist(mounds_heights)
```

![](Assignment-5_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

#### We change the breaks of the legend in the previous map to be more informative in realtion to the mound’s elevations:

``` r
tm_shape(cropped_Aster) + 
  tm_raster(title = "Elevations", breaks=c(300, 350, 400, 450, 500, 550, 600)) + tm_shape(mounds) + tm_dots() + 
  tm_layout(main.title = "Mound elevations",
          legend.title.size = 1,
          legend.text.size = 0.6,
          legend.position = c("left","bottom"),
          legend.bg.color = "white")
```

    ## Warning: Values have found that are less than the lowest break

    ## Warning: Values have found that are higher than the highest break

![](Assignment-5_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

#### We wish to evaluate whether the mounds are scattered randomly accross heights. We do this by taking a sample of 771 heights from the environment and the doing a t-test between these heights and the heights of the mounds:

``` r
Aster_samples <- sample(cropped_Aster, 771) #Sampling elevations.

t.test(Aster_samples, mounds_heights) #Performing t-test,
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  Aster_samples and mounds_heights
    ## t = 0.98913, df = 891.87, p-value = 0.3229
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -5.146948 15.606092
    ## sample estimates:
    ## mean of x mean of y 
    ##  447.6615  442.4319

``` r
sd(mounds_heights) #Computing standard deviation for mounds.
```

    ## [1] 39.87137

``` r
sd(Aster_samples) #Computing standard deviation for elevation raster samples.
```

    ## [1] 141.287

The 771 mounds recorded, had an elevation of (M = 442.43, SD = 39.87)
compared to the 771 randomly sampled elevations from the same raster (M
= 447.66, SD = 141.31) showing no significant difference t(891.87) =
0.98913, p = 0.3229.

#### We plot the distributions of the mound-elevations and the landscape elevations:

``` r
elevations <- data.frame(elevation = c(mounds_heights, Aster_samples), dist = c(rep("mounds", 771),rep("samples", 771))) 

ggplot(elevations,aes(x=elevation, fill=dist)) + geom_density(alpha=0.25)
```

![](Assignment-5_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggplot(elevations,aes(x=elevation, fill=dist)) + geom_histogram(alpha=0.25)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Assignment-5_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

Although there was no significant difference between the sampled
landscape elevations and the actual elevations of the mounds, we see
that no mound was placed above \~ 650 meters, indicating that mounds in
practice aren’t scattered randomly as the t-test would suggest.
