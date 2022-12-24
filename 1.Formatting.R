## Written by: Hanna Jackson hmj2@sfu.ca

## ~~~~~~~~~~~~~~ Load Data ~~~~~~~~~~~~~~
trapinfo    <- read.csv("raw_data/TrapInfo.csv",   strip.white = TRUE)
trapbumbles <- read.csv("raw_data/TrapBumbles.csv",strip.white = TRUE)
GLA         <- read.csv("raw_data/CanopyCover.csv",strip.white = TRUE)
veg         <- read.csv("raw_data/Vegetation.csv", stringsAsFactors = FALSE)


## ~~~~~~~~~~~~~~ Formatting  ~~~~~~~~~~~~~~~
GLA <- na.omit(GLA)

## Make round ID and site ID for all data frames.
## SiteID: we do this by pasting FireYear and Plot together. This
##    gives us an ID with 26 unique values, one for each of the 26
##    sites.
## RoundID: Here we want each unique visit to have a unique ID so we
##    paste FireYear and Plot (like we just did with siteID) but this
##    time we also add Round. This gives us 52 unique RoundIDs, one
##    for each visit to each site.   

trapbumbles[,'siteID']  <- paste(trapbumbles$FireYear, trapbumbles$Plot,                    sep=".", collapse = NULL)
trapbumbles[,'roundID'] <- paste(trapbumbles$FireYear, trapbumbles$Plot, trapbumbles$Round, sep=".", collapse = NULL)

trapinfo[,'siteID']     <- paste(trapinfo$FireYear,    trapinfo$Plot,                       sep=".", collapse = NULL)
trapinfo[,'roundID']    <- paste(trapinfo$FireYear,    trapinfo$Plot,    trapinfo$Round,    sep=".", collapse = NULL)

veg[,'siteID']          <- paste(veg$FireYear,         veg$Plot,                            sep=".",  collapse = NULL)
veg[,'roundID']         <- paste(veg$FireYear,         veg$Plot,         veg$Round,         sep=".",  collapse = NULL)



## Make list of species, useful for later 
species.names <- unique(trapbumbles$Species)[!is.na(unique(trapbumbles$Species))]

