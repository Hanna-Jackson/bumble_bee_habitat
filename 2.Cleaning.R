## Written by: Hanna Jackson hmj2@sfu.ca

## ~~~~~~~~~~~~~~ Fix plant common names ~~~~~~~~~~~~~~

## As with all manual entry, errors are bound to arise. Rather than
## manually try to figure out which ones are wrong in excel, we use R!
## This means that we keep track of all the changes we make, just in
## case we mess up and need to reverse our changes

## Mistakes that we fix include inconsistent capitalization, spelling,
## hypenation and alternate common names that are synonymous. 

## First we make a matrix with two columns, where column one contains
## the incorrect name and collum two contains the correct name. 

## Going from     incorrect names      ->        correct names 
names <- matrix(c("arnica"                     , "Arnica",
                  "one flowered wintergreen"   , "One-flowered wintergreen",
                  "rose"                       , "Rose",
                  "snowberry"                  , "Snowberry",
                  "White flowered Rhododendron", "White-flowered rhododendron",
                  "White Spirea"               , "White spirea",
                  "Yarrow "                    , "Yarrow",
                  "Green pyrola"               , "Green-flowered pyrola",
                  "sitka burnet"               , "Sitka burnet",
                  "White Tiny Willowherb"      , "Little willowherb",
                  "small white hawkweed"       , "White hawkweed",
                  "Alpine yellow Hawkweed"     , "Alpine hawkweed",
                  "pink wintergreen"           , "Pink pyrola",
                  "Orange hawkweed"            , "Orange agoseris",
                  "Alpine American Speedwell"  , "Alpine American speedwell",
                  "alpine buttercup"           , "Alpine buttercup",
                  "alpine hawkweed"            , "Alpine hawkweed",
                  "Arabidopsis lyrata"         , "Long capsule arabidopsis",
                  "arctic lupin"               , "Arctic lupin",
                  "Arrowleaf Senecio"          , "Arrowleaf senecio",
                  "tiny vaccinium"             , "Dwarf bilberry",
                  "tiny vaccinium "            , "Dwarf bilberry",
                  "Larger vaccinium"           , "Black huckleberry",
                  "wooly hawkweed"             , "Wooly hawkweed",
                  "Branched Yellow Hawkweed"   , "Wooly hawkweed",
                  "buttercup"                  , "Buttercup",
                  "Douglas Aster"              , "Douglas aster",
                  "Douglas Knotweed"           , "Douglas knotweed",
                  "Drummond's Cinquefoil"      , "Drummond's cinquefoil",
                  "Dwarf Fireweed"             , "Dwarf fireweed",
                  "False Hellebore"            , "False hellebore",
                  "Marsh Marigold"             , "Marsh marigold",
                  "Northern Golden-rod"        , "Northern goldenrod",
                  "one sided pyrola "          , "One-sided pyrola",
                  "buttercup"                  , "Buttercup",
                  "paintbrush"                 , "Common paintbrush",
                  "Pearly Everlasting"         , "Pearly everlasting",
                  "Pinedrops "                 , "Pinedrops",
                  "pink mountain heather"      , "Pink mountain heather",
                  "white mountain heather"     , "White mountain heather",
                  "Red Columbine"              , "Red columbine",
                  "Rock Harlequin"             , "Rock harlequin",
                  "Scrambled Eggs"             , "Scrambled eggs",
                  "sibbaldia"                  , "Sibbaldia",
                  "Small flowered penstemon"   , "Small-flowered penstemon",
                  "white hawkweed"             , "White hawkweed",
                  "Yellow Mountain Heather"    , "Yellow mountain heather",
                  "yellow violet"              , "Yellow violet"),
                byrow = TRUE,
                ncol  = 2
                )

## Implement those changes in the data

## This loops through each row of that matrix and then...
##   1. Finds which rows in the dataset have the name that's in column
##       1 (incorrect)
##   2. Overwrites all instances of the wrong with the correct name
##       from our matrix

for (ii in 1:nrow(names)){
  change <- which(veg['PlantCommonName'] == names[ii,1])
  veg[change,'PlantCommonName'] <- names[ii,2]
}

## Check that it worked if you want to 
table(veg[,'PlantCommonName'])



## ~~~~~~~~~~~~~~ Next fix the plant latin names ~~~~~~~~~~~~~~

## The process here is the same as the one described above for common
## names, just with latin names instead of common names. 
names <- matrix(c("Hieracium sp.",        "Hieracium triste",
                  "Pyrola uniflora ",     "Moneses uniflora",
                  "Agoseris glauca ",     "Agoseris glauca",
                  "Lupinus arcticus ",    "Lupinus arcticus",
                  "Parnassia fimbrata",   "Parnassia fimbriata",
                  "Pterospora andromeda", "Pterospora andromedea"),
                byrow = TRUE,
                ncol  = 2
                )

## Implement those changes in the data 
for (ii in 1:nrow(names)){
  change <- which(veg[,'PlantLatinName'] == names[ii,1])
  veg[change,'PlantLatinName'] <- names[ii,2]
}

## Check that worked 
table(veg[,"PlantLatinName"])


## Testing out the name changes
## This is a quick sanity check - if everything is going as expected, we
## should have the same number of unique latin and common names. If
## this condition doesnt hold, the code will hault and throw an
## error (better to know than to continue on in ignorance!) 

if (length(table(veg[,"PlantLatinName"])) != length(table(veg[,"PlantCommonName"])) ){
 stop("ERROR: Differnt number of common names and latin names")
}

