## Written by: Hanna Jackson hmj2@sfu.ca

## ~~~ Figure A2: Visit correlations ~~~
plot.visit.data <- function(){

  ## Set plotting options
  par(oma = c(6.5, 6.5, 2, 2),   ## Outer margins (bottom, left, top, right)                               
      mar = c(1,2.3 , 0.5, 0.5), ## Inner margins (bottom, left, top, right)                               
      mgp = c(2, 1.3, 0),        ## The margin line for the axis title, axis labels and axis line          
      tcl = 0,                   ## The length of tick marks as a fraction of the height of a line of text 
      cex.axis = 2.5,            ## Axis text size                                                         
      pty = 's'                  ## square plotting region                                                                                          
      )

  ## Making an index for which sites are each types 
  fire.ind   <- which(visit.flat$sitetype == 1)
  forest.ind <- which(visit.flat$sitetype == 0)

  ## Make a function that we will call to plot all of our boxplot pannels
  add.boxplot <- function(x,y, x.axis, y.axis, xlabel, ylabel){
    boxplot(y~x,
            col = c(fire.col, forest.col),
            ylab = "",
            xlab = "",
            xaxt = 'n',
            yaxt = 'n')
    
    ## Add a stripchart on top of the boxplot 
    stripchart(y[which(visit.flat$sitetype==0)], at = 1, add=TRUE, vertical=TRUE,
               method = "jitter", jitter = 0.045,
               pch = 22, cex = 1.05,
               bg = dark.fire.col)

    stripchart(y[which(visit.flat$sitetype==1)], at = 2, add=TRUE, vertical=TRUE,
               method = "jitter", jitter = 0.045,
               pch = 22, cex = 1.05,
               bg = dark.forest.col)

    ## If we specify that the x axis is required, this will plot it
    if(x.axis){
      axis(side=1,
           labels= c("Burned","Unburned"),
           at = c(1,2))
      mtext(text = xlabel,
            line = 4.5,
            side = 1,
            cex = 1.75) 
    }

    ## Add the y axis if that pannel needs it 
    if(y.axis){
      axis(side=2,
           las = 1)
      mtext(text = ylabel,
            line = 5.5,
            side = 2,
            cex = 1.75)
    }

    ## Calls a function that adds the correlation and p value between
    ## the two values (function defined below) 
    add.correlation.text(x=x,y=y)
  }


  add.scatter <- function(x, y, x.axis, xlabel){
    ## Setup the scatter plot
    plot(y ~ x,
         col=visit.flat$sitetypecol,
         pch = 16,
         cex= 1.8,
         xaxt = 'n',
         yaxt = 'n',
         ylab = "",
         xlab = ""
         )

    ## Add x axis if needed
    if(x.axis){
      axis(side=1)
      mtext(text = xlabel,
            line = 4.5,
            side = 1,
            cex = 1.75)
      
    }

    ## Only add the trendline if the p-value is significant
    if(cor.test(x,y)$p.value < 0.05){
      abline(lm(y~x))
    }

    ## Add the text with the correlation and the p-value from the
    ## function we will specify in the next section:  
    add.correlation.text(x=x,y=y)
  }

  
  ## And this is the function that those two previous functions call
  ## that adds correlation and p value texts 
  add.correlation.text <- function(x,y){
    
    p.val <- cor.test(x,y)$p.value
    
    if (p.val < 0.01){
      p.string <- c("p<0.01")
    } else if(p.val < 0.05){
      p.string <- c("p<0.05")
    } else if(p.val > 0.05){
      p.string <- paste0("p=",round(cor.test(x,y)$p.value, 2))
    }
    
    xmin <- par("usr")[1]
    xmax <- par("usr")[2]
    ymin <- par("usr")[3]
    ymax <- par("usr")[4]
    
    text(x = xmin + (xmax-xmin)*0.83,
         y = ymin + (ymax-ymin)*0.95,
         p.string,
         cex=2)
    
    text(x = xmin + (xmax-xmin)*0.80,
         y = ymin + (ymax-ymin)*0.90,
         paste0( "   cor=", round(cor.test(x,y)$estimate, 2)),
         cex=2
         )
  }


  
  ## ~~~~ Here the actual plotting begins: ~~~~
  
  layout(matrix(c(1, 0, 0, 0, 0,
                  2, 6, 0, 0, 0,
                  3, 7,10, 0, 0,
                  4, 8,11,13, 0,
                  5, 9,12,14,15), nrow = 5, byrow=TRUE))

  ## We're going to call the scatterplot and boxplot functions we just
  ##    created, but each time we call it we're going to input
  ##    different data each time! Efficiency! 

  ## The nice thing about the way we set this up, is we can easily
  ## specify which data we want to plot and which axes we want it to
  ## include for each pannel! 


  
  ## ~~~~~ Boxplots ~~~~~~

  ## Pannel 1: Sitetype vs Canopy openness
  add.boxplot(x=visit.flat$sitetype,
              y=visit.flat$canopyopenness,
              x.axis=FALSE,
              y.axis=TRUE,
              xlabel = NA,
              ylabel = "Canopyopenness (% open)"
              )
  
  ## Pannel 2: Sitetype vs Open flower abundance
  add.boxplot(x=visit.flat$sitetype,
              y=visit.flat$openflowerabundvisit,
              x.axis=FALSE,
              y.axis=TRUE,
              xlabel = NA,
              ylabel = "Log(open flower abundance)"
              )
  
  ## Pannel 3: Sitetype vs Flower SR
  add.boxplot(x=visit.flat$sitetype,
              y=visit.flat$flowerSRvisit,
              x.axis=FALSE,
              y.axis=TRUE,
              xlabel = "Burn Status",
              ylabel = "Flower species richness"
              )

  ## Pannel 4: Sitetype vs Julian Day
  add.boxplot(x=visit.flat$sitetype,
              y=visit.flat$trapjulian,
              x.axis=FALSE,
              y.axis=TRUE,
              xlabel = "Burn Status",
              ylabel = "Julian Day"
              )

  ## Pannel 5: Sitetype vs Hours out
  add.boxplot(x=visit.flat$sitetype,
              y=visit.flat$hoursout,
              x.axis=TRUE,
              y.axis=TRUE,
              xlabel = "Burn Status",
              ylabel = "Total trap hours (# of traps * hrs out)"
              )


  ## ~~~~~ Scatterplots ~~~~~ 
  ## Pannel 6: Canopy openness vs flower abundance
  add.scatter(x=visit.flat$canopyopenness,
              y=visit.flat$openflowerabundvisit,
              x.axis = FALSE,
              )

  ## Pannel 7: Canopy openness vs flower SR
  add.scatter(x=visit.flat$canopyopenness,
              y=visit.flat$flowerSRvisit,
              x.axis = FALSE,
              xlabel = "Canopy openness (% open)"
              )
  
  ## Pannel 8: Canopy openness vs Julian
  add.scatter(x=visit.flat$canopyopenness,
              y=visit.flat$trapjulian,
              x.axis = FALSE,
              xlabel = "Canopy openness (% open)"
              ) 
  
  ## Pannel 9: Canopy openness vs Hours 
  add.scatter(x=visit.flat$canopyopenness,
              y=visit.flat$hoursout,
              x.axis = TRUE,
              xlabel = "Canopy openness (% open)"
              ) 
  
  ## Pannel 10: Flower abundance vs flower SR
  add.scatter(x=visit.flat$openflowerabundvisit,
              y=visit.flat$flowerSR,
              x.axis = FALSE,
              xlabel = "Log(open flower abundance)"
              )

  ## Pannel 11: Flower abundance vs julian
  add.scatter(x=visit.flat$openflowerabundvisit,
              y=visit.flat$trapjulian,
              x.axis = FALSE,
              xlabel = "Log(open flower abundance)"
              )

  ## Pannel 12: Abundance vs hours
  add.scatter(x=visit.flat$openflowerabundvisit,
              y=visit.flat$hoursout,
              x.axis = TRUE,
              xlabel = "Log(open flower abundance)"
              )
  ## Pannel 13: SR vs Julian
  add.scatter(x=visit.flat$flowerSRvisit,
              y=visit.flat$trapjulian,
              x.axis = FALSE,
              xlabel = "Log(open flower abundance)"
              )
  
  ## Pannel 14: SR vs Hours
  add.scatter(x=visit.flat$flowerSRvisit,
              y=visit.flat$hoursout,
              x.axis = TRUE,
              xlabel = "Flower species richness"
              )
  
  ## Pannel 15: Julian vs Hours 
  add.scatter(x=visit.flat$trapjulian,
              y=visit.flat$hoursout,
              x.axis = TRUE,
              xlabel = "Julian day"
              )
}
















