## Written by: Hanna Jackson hmj2@sfu.ca

## ~~~ Welcome to Main.R! ~~~

## This is the hub of everything, and the only place we will be
## running any code from! Here, we will source other files that
## contain the code that actually executes the data cleaning, setting
## up, running of our models and plotting of the figures.
## Hopefully this way, you can get my code working for you quickly
## without having to dig through endless code. 

## My goal with this code is to help someone not familiar with my
## style of coding or with  occupancy modeling through it. I hope this
## has more of a tutorial style! 

## This code is also posted to
## github.com/Hanna-Jackson/bumble_bee_habitat for ease of
## viewing/accessing. 


## ~~~~~~~~~~~~~ Setting up ~~~~~~~~~~~~

## First, set your working directory to the folder on your computer
## that you want to use for this project. For me, that looks like
## this, but replace it with your own! 

setwd("~/Dropbox/Bumble Bee Habitat/DB-Bb Habitat")
message("working directory: ",getwd())

## For my code to work as normal you're going to need a specific file
## structure that we'll make now:

## The following code will look for the 4 folders you should have in
## your working directory and if they dont exist, it will create them!

## check if sub directory exists
for (ii in c("raw_data","data_for_run","models","saved")){
  if ( isFALSE(file.exists(ii)) ){
    dir.create(ii)
    message("Created folder called: ", ii)
  } 
}

## ~~~ IMPORTANT: Downloading the data ~~~ 
## Next you'll need to put the data into the raw_data folder manually
## by downloading the data from the paper's Data Dryad or github page
## (https://github.com/Hanna-Jackson/bumble_bee_habitat). 



## ~~~~~~~~~~~~~ Prepare Data ~~~~~~~~~~~~~

## Source the 4 data prep files. To see what this does, go look at those files! 
source("1.Formatting.R")
source("2.Cleaning.R")
source("3.Site_Visit_Summaries.R")
source("4.Make_final_data.R")

## If you just ran those 4 source calls, you dont need to run this
## next 2 load calls, but they're useful to have if you later want to
## load the important objects without re-making everything. 

## Load the objects we need (these were saved in "4.Make_final_data.R")
load("data_for_run/JAGS.arr.RData",  verbose = TRUE)
load('data_for_run/JAGS.visit.RData',verbose = TRUE)
load('data_for_run/JAGS.site.RData', verbose = TRUE)

## Prepare the data for running model - packages it up the way JAGS wants it
source("5.Prep_For_Run.R")



## ~~~~~~~~~~~~~ Decide the parameters of the model run ~~~~~~~~~~~~~

## This is the file name of all the models presented in the paper.
## This code just makes a vector of file names where the name of each
## element describes which model (main text or appendix models)

models <- c(maintext = "Model_Main",## Main text model
            A1       = "Model_A1",  ## No burn status on occupancy
            A2       = "Model_A2",  ## No burn status on occupancy or detection
            A3       = "Model_A3",  ## No floral spp rich on occupancy
            A4       = "Model_A4",  ## No floral spp rich on occ or det
            A5       = "Model_A5",  ## No floral spp rich + interaction between burn status and floral abundance on occupancy
            A6       = "Model_A6")  ## Add an effect of being in the Southern region on occupancy

## Choose the name (i.e. "maintext" or "A3") of the model you want
## from that in the quotes on the first line (here by default Ive put
## "maintext")
## Also input run parameters such as number of itterations, length of
## burnin time,how much to thin the output and the  number of chains
## to run

args <- list(model   = models["maintext"],
             niter   = 110000,
             nburnin =  10000,
             nthin   =    100,
             nchains =      3,
             notes   = "Eco Evo Reviwes V2")



## ~~~~~~~~~~~~~ Run the model and save it ~~~~~~~~~~~~

## Source the chosen model file from the models folder. This will load
##    the model object into our workspace.

## In order for this to work, you will need to have JAGS installed on
## your computer, which you can download at this link:
## https://mcmc-jags.sourceforge.io 

##    (Note: the sprintf() function is just replacing the '%s' in the string
##    I've given it with whater is in args$model, which is the file name
##    of our model we chose earlier! (I love sprintf, it is so
##    useful!)

source(sprintf("models/%s.R", args$model))
message('Using Model', args$model) 

## Run the model! 
message('\nRunning model\n ', args$model, ' with ', args$niter, '\n')
bugs <- jags(data       = my.data,  
             inits      = my.inits,
             parameters.to.save = my.params, 
             model.file = my.model,
             n.iter     = args$niter, 
             n.burnin   = args$nburnin,
             n.thin     = args$nthin,
             n.chains   = args$nchains,
             working.directory = NULL)


## ~~~~~~~~~~~~~~ Save the run ~~~~~~~~~~~~~~

save(my.data, bugs, args, my.params,
     file = sprintf('saved/modeloutput-%s.RData',args$model))
message('Saved Model Run: as ', sprintf('saved/modeloutput-%s',args$model))


## ~~~~~~~~~~~~~~ Look at the results ~~~~~~~~~~~

summ    <- bugs$BUGSoutput$summary
columns <- c('mean','sd','2.5%','97.5%', 'Rhat')
summ[,c("mean","2.5%",'97.5%')]
summ[,columns]



## ~~~~~~~~~~~~~~ Plotting ~~~~~~~~~~~~~~~~

## Next we need to plot our model run!

## You can use the run you just ran that's already in your R session
## memmory, or you can load a precious one by again choosing which
## model we want to use by putting the model name in the quotes. Here
## I've started us off by using the maintext model: 

load(sprintf('saved/modeloutput-%s.RData',models['maintext']))

## Next source a file that sets up some plotting code: 
source("6.Initialize_For_Plotting.R")


## ~~~ And now to actually plotting! ~~~ 
## Just like we sourced code earlier in the data prep phase, we're
## going to source files that contain the actual plotting code.
## The only difference is that this time instead of the source command
## processing a lot of data, it's just going to create and load into
## our workshapce a huge plotting function. We can then call that
## function to plot our model. 

## The only catch to that is that we won't call these functions
## directly, we're going to call them via the pdf.f function that is
## in our workspace from when we sourced
## 6.Initialize_For_Plotting.R.

## All this pdf.f() function does is let us choose the file path and
## name, as well as the width, height, and any other plotting
## parameters that are in the function itself. When you run it it will
## save a pdf of the figure to 

## Sidenote: 
##    If you are running multiple models and want to compare output from
##    them, I suggest replacing the 'file' argument with this: 
##         sprintf("~/Desktop/distofdata-%s.pdf",args$model)
##    This will replace the %s with the value stored in args$model, which
##    is loaded in when you load a new model run. This will add the model
##    name to the figure name, and will help with maintaining sanity
##    while trying to compare model runs.           



## ~~~ Figure 3: Data Boxplots ~~~  
source("Fig3_Data_Boxplots.R")
pdf.f(f = plot.data.boxplots,            ## The function loaded in when we sourced
      file = "~/Desktop/distofdata.pdf", ## The file path you want your figure to save to 
      width = 7,                         ## PDF width
      height = 7)                        ## PDF height 

## ~~~ Figure 4: Effect sizes ~~~
source("Fig4_Effect_Sizes.R")
pdf.f(f=make.effect.size.fig,       
      file = "~/Desktop/effectsize.pdf",  
      width =7,                         
      height = 5)

## ~~~ Figure 5: Occupancy vs Covariates ~~~
source("Fig5_Occupancy.R")
pdf.f(f = make.occ.figure,       
      file = "~/Desktop/occ.pdf",  
      width = 8,                         
      height = 8)  

## ~~~ Figure 6: Detection vs Covariates ~~~
source("Fig6_Detection.R")
pdf.f(f = make.det.fig,       
      file = "~/Desktop/det.pdf",  
      width = 11.3,                         
      height = 7)



## ~~~~~~~~~~ Appendix Figures ~~~~~~~~~~~

## ~~~ Figure A1: Site correlations ~~~
source("FigA1_Site_Correlations.R")
pdf.f(f = plot.site.data,
      file = "~/Desktop/distofsite.pdf",
      width = 12, height = 12)

## ~~~ Figure A2: Visit correlations ~~~
source("FigA2_Visit_Correlations.R")
pdf.f(f = plot.visit.data,
      file = "~/Desktop/distofvisit.pdf",
      width = 20, height = 20)




