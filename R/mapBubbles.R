#' Create a Map
#' 
#' Produce a map with bubbles for points.
#' 
#' @param dat The data.
#' @param depths The default is \code{NULL}.
#' @param plotMap A logical value specifying if the map should be plotted.
#' @param maxValue todo: document the argument.
#' @param maxSize todo: document the argument.
#' @param circle.fg The color for the foreground of the bubles.
#' @param circle.bg The color for the background of the bubles.
#' @param type  todo: document the argument.
#' @param zeroPch todo: document the argument.
#' @param zeroCex todo: document the argument.
#' @param zeroCol todo: document the argument.
#' @param legendPos todo: document the argument.
#' @param legendBreaks todo: document the argument.
#' @param legendType todo: document the argument.
#' @param legendTitle todo: document the argument.
#' @param legendCex todo: document the argument.
#' @param legendTitleCex todo: document the argument.
#' @param ... Arguments passed to \code{plotMap}.
#' @import graphics
#' @export
mapBubbles <-
function(dat,depths=NULL,plotMap=T,
            maxValue=max(dat$Z),maxSize=0.3,circle.fg="black",circle.bg=grDevices::rgb(0,0,0,0.3),type="surface",
            zeroPch=13,zeroCex=0.8,zeroCol=grDevices::gray(0.5),
            legendPos="bottomright",legendBreaks=NULL,legendType="vert",legendTitle=NULL,legendCex=0.8,legendTitleCex=legendCex,...) 
{
#type=surface is sqrt
    if(plotMap) {
        plotMap(westCoastLL,tck=c(-0.02),...)
        if(exists("westCoastLL")) {
            PBSmapping::addLines(westCoastLL)
        } else {
            print("Run data(westCoastLL) before calling this function to plot West Coast shoreline")
        }
        if(exists("WCstatesInlandPBS")) {
            PBSmapping::addLines(WCstatesInlandPBS)
        } else {
            print("Run data(WCstatesInlandPBS) before calling this function to plot state boundaries")
        }
        if(!is.null(depths)) {
            for(i in 1:length(depths)) {
                PBSmapping::addLines(depths[[i]],col=grDevices::gray(0.5))
            }
        }
    }
    ind0 <- dat$Z<=0
    ind <- dat$Z>0
    graphics::points(dat[ind0,"Longitude"],dat[ind0,"Latitude"],pch=zeroPch,cex=zeroCex,col=zeroCol)
    #symbols(dat[ind0,"Longitude"],dat[ind0,"Latitude"],squares=rep(1,sum(ind0)),add=T,fg=fgZero,bg=fgZero,inches=zeroSize)
    tmp <- dat[ind,]
    event <- PBSmapping::as.EventData(data.frame(EID=1:nrow(tmp),X=tmp$Longitude,Y=tmp$Latitude,Z=tmp$Z))
    .addBubbles.ach(event,type=type,z.max=maxValue,max.size=maxSize,symbol.fg=circle.fg,symbol.bg=circle.bg,symbol.zero=F,
                legend.pos=legendPos,legend.breaks=legendBreaks,legend.type=legendType,legend.title=legendTitle,legend.cex=legendCex,legend.title.cex=legendTitleCex)
    #symbols(c(-1000,dat[ind,]$Longitude),c(-1000,dat[ind,]$Latitude),circles=sqrt(c(maxValue,dat[ind,]$Z)),inches=maxSize,add=T,fg=circle.fg,bg=circle.bg)
    
    invisible()
}
