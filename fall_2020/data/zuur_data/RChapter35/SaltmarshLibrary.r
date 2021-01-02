#    R code for: Chapter 35 in:
#    Analysing Ecological Data. (2007). Zuur,  Ieno and Smith. Springer,  680p.
#    This file was produced by Alain Zuur (highstat@highstat.com)
#    www.highstat.com

#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License,  or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.


myplotN1Trellis <- function(f1, MainTitle, Titlexas, 
                                Titleyas, xaxisrange, 
                                yaxisrange, df, 
                                CoplotType, SEUP, SELOW){
     trellis.device(new = F)
     background <- trellis.par.get("background")
     background$col <- "white"
     trellis.par.set("background", background)
     xyplot(f1, col = 1, main = MainTitle, layout = c(2, 4), 
            xlab = Titlexas, ylab = Titleyas, 
            scales = list(alternating = T, 
                          x = list(relation = xaxisrange), 
                          y = list(relation = yaxisrange)), 
            panel = function(x, y, subscripts){
                    I1 <- y != 0
                    x1 <- x[I1]
                    y1 <- y[I1]
                    I2 <- order(x1)
                    y2 <- y1[I2]
                    x2 <- x1[I2]
                    mypanel1(x2, y2, df, CoplotType)
                       se.up1 <- SEUP[subscripts]
                       se.lo1 <- SELOW[subscripts]
                       se.up2 <- se.up1[I1]
                       se.lo2 <- se.lo1[I1]
                       llines(x2, se.up2[I2], lty = 2, col = 1)
                       llines(x2, se.lo2[I2], lty = 2, col = 1)
            }
            )
}


mydeviceTrellis <- function(){
     if (!require("lattice", character.only = TRUE)) {
         SetCustomError(latticeLiberr)
     }
     trellis.device(new = F)
     background <- trellis.par.get("background")
     background$col <- "white"
     trellis.par.set("background", background)
}


mypanel1 <-function(x, y,spanw,CoplotType,...) {
    if (CoplotType == "lines only" )  { 
        panel.xyplot(x, y,col=1,type="l") 
    }
    if (CoplotType != "lines only" )  { 
        panel.xyplot(x, y,col=1,type="p") 
    }
    I3<-!is.na(y) & !is.na(x)
    y1<-y[I3]
    x1<-x[I3]
    if (CoplotType == "smoothing lines & points" ) {
        tmp<-lowess(x1, y1, f=spanw)
        llines(tmp$x, tmp$y, col=1) 
    }
    if (CoplotType == "regression lines & points" ) {
        tmp<-lm(y1~x1)
        I4<-order(x1)
        llines(x1[I4], tmp$fitted[I4], col=1) 
    }
}
