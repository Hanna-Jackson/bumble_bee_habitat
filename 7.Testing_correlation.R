## Written by: Hanna Jackson hmj2@sfu.ca

## ~~~~~~~~~~~~~~~ Load ~~~~~~~~~~~~~~~

## Load only the objects we need (these were saved in "4.Make_final_data.R")
load("data_for_run/JAGS.arr.RData",  verbose = TRUE)
load('data_for_run/JAGS.visit.RData',verbose = TRUE)
load('data_for_run/JAGS.site.RData', verbose = TRUE)
source("5.Prep_For_Run.R")



## ~~~~~~~~~~~~~~~ Site level Pearson's Correlation ~~~

cor.test(JAGS.site$canopyopenness, JAGS.site$openflowerabundsite) ## 0.1151194  
cor.test(JAGS.site$canopyopenness, JAGS.site$flowerSRsite)        ## 0.4364826 
cor.test(JAGS.site$flowerSRsite,   JAGS.site$openflowerabundsite) ## 0.5619696
cor.test(JAGS.site$sitetype,       JAGS.site$canopyopenness)
cor.test(JAGS.site$sitetype,       JAGS.site$openflowerabundsite)
cor.test(JAGS.site$sitetype,       JAGS.site$flowerSRsite)


## ~~~~~~~~~~~~~~~ Visit level Pearson's Correlation ~~~~~~~~~~~~~~~~ 

visit <- visit.flat
vars  <- c('sitetype','canopyopenness', 'openflowerabundvisit','flowerSRvisit')

cor.test(JAGS.site$flowerSRsite, JAGS.site$openflowerabundsite)   ## 0.5619696
cor.test(visit$flowerSRvisit,  visit$openflowerabundvisit)   ## 0.5619696

cor.test(JAGS.site$canopyopenness, JAGS.site$flowerSRsite)        ## 0.4364826
cor.test(visit$canopyopenness, visit$flowerSRvisit)        ## 0.4364826

cor.test(JAGS.site$canopyopenness, JAGS.site$openflowerabundsite) ## 0.1151194  
cor.test(visit$canopyopenness, visit$openflowerabundvisit) ## 0.1151194  

cor.test(JAGS.site$sitetype, JAGS.site$canopyopenness)
##cor.test(visit$sitetype, visit$canopyopenness) ## Both site level predictors, so don't need to do this one

cor.test(JAGS.site$sitetype, JAGS.site$openflowerabundsite)
cor.test(visit$sitetype,       visit$openflowerabundvisit)

cor.test(JAGS.site$sitetype, JAGS.site$flowerSRsite)
cor.test(visit$sitetype,       visit$flowerSRvisit)



## ~~~~~~~~~~~~~ VIF ~~~~~~~~~~~~~~
source("VIF.R")

## Measurement of how colinear they are and how that will inflate
## estaimted predictors


## ~~~ VIF analysis for sit-level predictors ~~~
vars <- c('sitetype','canopyopenness', 'openflowerabundsite','flowerSRsite')
corvif(JAGS.site[,vars])


## ~~~ VIF analysis for visit-level predictors ~~~
vars <- c('sitetype','canopyopenness', 'openflowerabundvisit','flowerSRvisit')
corvif(visit.flat[,vars])

vars <- c('canopyopenness', 'openflowerabundvisit','flowerSRvisit')
corvif(JAGS.visit.flat[,vars])


