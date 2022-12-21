# Johnson et al. Positive impact of post-fire environment on bumble bees not explained by habitat variables in a remote forested ecosystem
---
Hello and thank you for taking the time to look at the data and code for our study! If you run into any problems, or have any questions or concerns please feel free to email me at hmj2@sfu.ca (especially if you're planning to use the data!)
Best of luck!
- Hanna Jackson, on behalf of all coauthors 
___

## Introduction/Summary 
We investigated the effect of burn status (whether a site had previously been burned by a wildfire), floral abundance, floral richness and canopy cover on bumble bee species' occupancy in Tweedsmuir Provincial  Park Western Canada. To do this we used an **Occupancy Model**, which can be conceptualized as two, linked Generalized Linear Models, where we model both site **occupancy probability** and a quantity called **detection probability**, which is roughly equivalent to sampling bias. In our study we model environmental covariates to both occupancy and detection probability to try to correct our estimates of occupancy probability for biased sampling (imperfect detection probability). 

We recorded data during two sampling visits ('rounds') to each of our 26 sites, 13 of which had previously been burned and 13 of which had not. During these visits we collected bumble bees in blue vane traps, recorded floral abundance and species richness, and took photos of the canopy cover to later calculate canopy openness. 

We found that whether or not a site was burned was a significant predictor of bumble bee species' occupancy, and that the other habitat variables were not. Instead, we found that floral species richness and canopy cover significantly affected detection probability, indicating that these variables impact how easy it is to sample bumble bees, not whether or not they are actually there. For more details, see the full study. 


First, we describe briefly how our code is set up. . We encourage anyone who wants to use our data to look through R files 1 though 4 (`1.Formatting.R`, `2.Cleaning.R`, `3.Site_Visit_Summaries.R`, and `4.Make_final_data.R`), as these files clean the data (including fixing typos in species names etc) and make summaries of it that will undoubtedly be helpful for any study. 

In the latter section, we describe the data that we have in terms that hopefully make it easy for researchers doing studies of other kinds to use our data. 


## 1. The Code! 
The code is written so that the code that executes actions is all stored in many separate R files, and the 'home base' `0.Main.R` uses the `source()` function to run code in those files. 

This means that you can load and prep the data, run the model, and plot the figures all from `0.Main.R` in very few lines of code. If you want to know what is happening under the hood, open up the sourced files and see what is in them! 

I try to comment my code in a helpful and "tutorial-like" manner so that people who are unfamiliar with my style of coding or with occupancy modeling can understand. I *especially* encourage readers to look at the model file `Model_Main.R`, as this is the heart of the analysis. 

Further instructions are in `0.Main.R`, so open that file up first and start there!



## 2. Description of the Data: 
Due to the hierarchical nature of our sampling design, we have some information at the **site level** (one observation for each of our 26 sites) and some information at the **visit level** (2 observations for each of our 26 sites). 


### Site-Level Data
#### Burn Status 
Each site has recorded status that indicates whether it is a previously burned site or not. 
#### Canopy Openness 
Each site has a measured value of canopy openness (see methods for details) 


### Visit-Level Data 
#### Bumble bee records 
For each site visit we have records of each bumble bee that was caught in a **blue vane trap** or a **sweep net**. For each individual we have recorded both its species and caste, and in the case of sweep-net data we also have which flower species it was netted from. 
#### Floral records 
At each visit to each site, we recorded the plants found inside our transects (see methods for details) and the number of flowers on each of those plants. 


### File Structure 
The code as written is meant to be used with a main working directory (folder) containing these 4 sub folders: (note: replace `my_directory` with the name of the folder you're using on your own computer)
1. `my_directory/data_for_run` : As written, when the code is run (R files 1 through 4) it will save the final data objects here that will be used for 
2. `my_directory/models` : This is where you should put the R code that contains each of the models that you want to run
3. `my_directory/raw_data` : This is where you should put the .csv files you download from this Dryad repository: `Trapinfo.csv`, `TrapBumbles.csv`, `NetBumbles.csv`, `Vegetation.csv` and `CanopyCover.csv`. See below for more information on interpreting each of these files. 
4. `my_directory/saved` : This is where the completed model run results will be saved to! (as the name implies) 
These files can be set up manually, or you can just run the code at the beginning of `0.Main.R` and the files will be made for you if they do not already exist in your working directory. 


### TrapInfo.csv 
**SUMMARY**: In this file, each row is an individual trap that was put out at a given site on a given visit to said site. 

The columns in this dataset are: 
1. `FireYear`: The year that the fire burned in that site. (If this is an unburned site, the fire year indicates the year the fire that this site was adjacent with burned)
2. `Plot`: This is a unique indicator of site within each `FireYear`. This means that each unique value of `FireYear` AND `Plot` indicates a unique site. 
3. `SiteLat`: The latitude of the site 
4. `SiteLon`: The longitude of the site 
5. `SiteType`: Whether the trap is at a burned or unburned location. 
6. `TrapJulian`: The day of the year (1 being January 1st) that the trap was placed
7. `Trap`: An ID that is unique only in that `FireYear`-`Plot`-`Round` 
8. `TrapLat`: The latitude of the trap
9. `TrapLon`: The longitude of the trap
10. `Hoursout`: How many hours that trap was out at
11. `Round`: Indicates whether this is the first or second visit to this site
12. `Bear`: On one instance, our trap was disturbed by a bear and we indicate this with a 1 in this column. 


### TrapBumbles.csv 
SUMMARY: This dataset contains the bumble bees that were caught in the traps. Each row is a unique caste of a unique species for each trap at each site visit (`Round`). The important information here is stored in the `Abundance` column which tells you how many individuals of that caste of that species were found in that trap at that site during that visit (`Round`).

The columns in this dataset are: 
0. Columns:  `FireYear`, `SiteType`, `Plot`, `Round`, and `Trap` are the same as above.
1. `Species`: The species of bumble bee (specific epithet) 
2. `Caste`: The caste of the bumble bee (Queen, worker, or male) 
3. `Abundance`: The number of individuals of that caste of that species were found in that trap at that site during that visit (`Round`).


### NetBumbles.csv
SUMMARY: This dataset contains records of bumble bees captured by netting. All rows and columns can be interpreted the same as the previous section (`TrapBumbles.csv`) with the only difference being that we have added  a column called `PlantSpecies` that indicates which plant species the individuals were found on. 
As a result, each row represents the abundance of individuals of a given caste of a given species on a given flower species found in at a given site on a given visit (`Round`). 

The columns in this dataset are: 
0. Columns:  `FireYear`, `Plot`, `Round`, `Species`, `Caste` and `Abundance` are the same as above.
1. `PlantSpecies`:  The species of plant that the bumble bee(s) were netted from


### Vegetation.csv
SUMMARY: This dataset contains information about the sites themselves during each visit (`Round`). 

The columns in this dataset are: 
0. Columns `FireYear`,`Plot`,`SiteType`, and `Round` are the same as above
1. `Day`: The day that the measurements of this site during this round were taken. 
2. `Month`:  The month that the measurements of this site during this round were taken. 
3. `Transect`: Which transect these values are for - the North, South, East or West transect at that site. 
4. `PlantCommonName`: The common name of the plant 
5. `PlantLatinName`: The latin name of the plant 
6. `FlowerCount`: The number of flowers of that species 
7. `PlantCount`: The number of individual plants of that species 
8. `OutsideTransectCount`: The number of additional species found outside the transect area 


### CanopyCover.csv 
SUMMARY: This dataset contains information about the canopy openness at each of the sites

The columns in this dataset are: 
0. Columns `FireYear` and `Plot`are the same as above
1. `CanopyOpenness`: The % canopy openness calculated from one canopy photo of the site
2. `Date`: The data the photo was taken in DDMMYY format 


### PlantBumbleInteractions.csv 

Need to conform with Sarah what this is 



