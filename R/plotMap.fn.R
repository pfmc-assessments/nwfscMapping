#' Create entire West Coast Map
#' 
#' @param dir directory where the output file will be saved
#' @param dat data-frame of the data that has been by the PullCatch.fn
#' @param area option are northsouth, state, inpfc, or ...
#' @param byYear todo: document
#' @param plotData where to plot the catch rates on the map
#' @param dopng TRUE/FALSE whether to create a png file that will be saved in the printfolder
#'
#' @author Chantel Wetzel
#' @export
#' @import ggplot2 colorRamps


WestCoastMap.fn <- function(dir = NULL, dat, area = "coast", byYear = FALSE, plotData = TRUE, dopng = TRUE)  {

	if (dopng) {
      	if(!is.null(dir)){stop("Directory needs to be set.")}
      	plotdir <- file.path(dir, paste("plots", sep=""))
      	plotdir.isdir <- file.info(plotdir)$isdir
      	if(is.na(plotdir.isdir) | !plotdir.isdir){
      	  	dir.create(plotdir)
      	}
      	if (!byYear) { grDevices::png(paste0(dir, "/plots/", area, "_map.png"), height=7, width=5, units="in",res=300) }
      	if ( byYear) { grDevices::png(paste0(dir, "/plots/", area, "_map.png"), height=7, width=7, units="in",res=300) }
    }

	# Get the state information to plot map
    states <- ggplot2::map_data("state")
    west_coast <- subset(states, region %in% c("washington", "oregon", "california"))
	map.df <- ggplot2::fortify(west_coast)

	# Filter the data
	pos.cat <- dat[dat$cpue_kg_km2 > 0 & dat$Latitude_dd >= min(map.df$lat),]
	neg     <- dat[dat$cpue_kg_km2 == 0 & dat$Latitude_dd >= min(map.df$lat),]
	mid     <- mean(pos.cat$cpue_kg_km2) + 0.20*max(pos.cat$cpue_kg_km2)

	# Define the theme for plotting
	if(!byYear){
		plot_format = theme(
                      panel.grid = element_blank(), 
                      panel.background = element_rect(fill = 'white'),
                      legend.position = "right"
                      )
	}

	if(byYear){
		plot_format = theme(
                      panel.grid = element_blank(), 
                      panel.background = element_rect(fill = 'white'),
                      axis.title.x = element_blank(),
         			  axis.text.x=element_blank(),
         			  axis.ticks.x = element_blank(),
         			  legend.position = "right"
                      )
	}

	color = colorRamps::matlab.like(20)
	color = color[seq(0, 20, 5)]

	if(!byYear){
	    g <- ggplot(data = west_coast) + 
  	         geom_polygon(aes_string(x = "long", y = "lat", group = "group"), fill = "lemonchiffon", color = "black") + 
  	         geom_point(data = neg, aes_string(x = "Longitude_dd", y = "Latitude_dd", color = "cpue_kg_km2", size = "cpue_kg_km2"), pch = 1, col = "lightgrey", alpha = 0.15) +
  	         geom_point(data = pos.cat, aes_string(x = "Longitude_dd", y = "Latitude_dd", color = "cpue_kg_km2", size = "cpue_kg_km2"), pch = 16, alpha = 0.7) +
  	         scale_size_area(max_size = 12, name = "CPUE kg/km2") +  
             scale_color_gradient2(midpoint = mid, low=color[2], mid=color[3], high=color[4], space ="Lab", name = "CPUE kg/km2") +
  	         plot_format + 
             xlab("Longitude") + ylab("Latitude") +
             labs(title = "                               US West Coast") +
             plot_format +
  	         coord_fixed(1.3) 
  	}

  	if(byYear){
	    g <- ggplot(data = west_coast) + 
  	         geom_polygon(aes_string(x = "long", y = "lat", group = "group"), fill = "lemonchiffon", color = "black") + 
  	         geom_point(data = neg, aes_string(x = "Longitude_dd", y = "Latitude_dd", color = "cpue_kg_km2", size = "cpue_kg_km2"), pch = 1, col = "lightgrey", alpha = 0.15) +
  	         geom_point(data = pos.cat, aes_string(x = "Longitude_dd", y = "Latitude_dd", color = "cpue_kg_km2", size = "cpue_kg_km2"), pch = 16, alpha = 0.7) +
  	         scale_size_area(max_size = 12, name = "CPUE kg/km2") +  
             scale_color_gradient2(midpoint = mid, low=color[2], mid=color[3], high=color[4], space ="Lab", name = "CPUE kg/km2") +
  	         plot_format + 
             xlab("Longitude") + ylab("Latitude") +
             labs(title = "                               US West Coast") +
             plot_format +
             facet_wrap(~Year) +
  	         coord_fixed(1.3) 
  	}
  	print(g)

	if (dopng) { grDevices::dev.off()}


    #roundDown <- function(x) 10^floor(log10(x))

    #water = "lightcyan"
    #land  = "lemonchiffon"
    #inc   = 0.15    #adjust this to modify the maximum size of the circles
    #sm.inc= 0.02
	#maxVal = ceiling(max(dat$cpue_kg_km2))
	#plotmaxVal = roundDown(maxVal)
	#valseq = c(plotmaxVal, plotmaxVal/10, plotmaxVal/100, plotmaxVal/1000)
	#valseq = c(maxVal, valseq[valseq >=  1])
	#ind0   = dat$cpue_kg_km2 <= 0
	#ind    = dat$cpue_kg_km2 > 0
	#x.in = c(c(-140, -110), rev(c(-140, -110))) #dimension for polygon water
	#y.in = c(25, 25, 55, 55)

	#if(area == "state"){
	#	longs = c(-125.9, -123.2, -125.9, -122, -125.5, -117)
	#	lats  = c(46.0, 48.6, 42.0, 46.1, 31.8, 42)
	#	main.names = c("Washington", "Oregon", "California") 
	#	layout(mat = matrix(c(1,4,2,3), nrow = 2),
	#		   widths = c(1, 1), #c(0.5, 0.5, 2, 2),
	#   		   heights = c(1.5, 2)) #c(0.5, 0.5, 0.75, 0.25))
	#	par(mar = c(2, 2, 2, 2), oma = c(0,0,0,0))
	#}

	#if(area == "northsouth"){
	#	longs = c(-123.5, -123.45, -125.9, -116.5)
	#	lats  = c(42, 48.6, 31.8, 38)
	#	main.names = c("North", "South") 
	#	layout(mat = matrix(c(1, 3, 2, 2), nrow = 2),
	#		   widths = c(1, 1),
	#   		   heights = c(1, 1))
	#	par(mar = c(1, 1, 1, 1), oma = c(2,2,2,2))
	#}

	#if(area == "inpfc"){

	#}

	#if(area == "allan"){
	#	longs = c(-122, -116.6, -125.2, -121.2, -125.9, -123.2)
	#	lats  = c(31.8, 36.0, 36.0, 43.0, 43.0, 48.6)
	#	main.names = c("Conception", "Monterey/Eureka", "Columbia/Vancouver") 
	#	layout(mat = matrix(c(1,4,2,2,3,3), nrow = 2),
	#		   widths = c(1.4, 1.4, 1.2, 1.2, 0.5, 0.5),
	#   		   heights = c(0.58, 0.42, 1, 1, 1, 1))
	#	par(mar = c(2, 2, 2, 2), oma = c(0,0,0,0))
	#}

	#cl <- rnaturalearth::ne_states(country = c("United States of America"))
	#cl <- ne_coastline(country = c("United States of America"))

	#
	#for (a in 1:length(main.names)){
	#	xx = seq(1,11, 2)[a]
	#	sp::plot(cl, xlim = c(longs[xx], longs[xx+1]), ylim = c(lats[xx], lats[xx+1]), axes = FALSE, tck=c(-0.02), cex = 1.0, main = main.names[a])
	#	polygon(x.in, y.in, col = water)
	#	plot(cl, xlim = c(longs[xx], longs[xx+1]), ylim = c(lats[xx], lats[xx+1]), col = land, add = TRUE)
	#	if (plotData == TRUE){
	#		symbols(dat[ind0,]$Longitude_dd, dat[ind0,]$Latitude_dd, squares = rep(1,sum(ind0)), add = TRUE, fg = gray(0.9), bg = gray(0.9), inches = sm.inc)
	#		symbols(dat[ind,]$Longitude_dd, dat[ind,]$Latitude_dd, circles = sqrt(dat[ind,]$cpue_kg_km2), inches = inc, add = TRUE)
	#	}
	#	if(plotData & a == length(main.names)){
	#		plot(1, 1, type="n", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
	#		x <- rep(1 , length(valseq)+1)
	#		ymax = 1.4; ymin = 1
	#		y <- seq(ymax, ymin, -(ymax-ymin)/(length(valseq)))#c(1.65, 1.58, 1.47, 1.4, 1.34)
	#		z <- c(valseq,0)
	#		symbols(x, y, circles = sqrt(z), inches = inc, add = TRUE, xpd = TRUE, fg = c(0, rep(1,length(z))))
	#		text(x, y, c("kg/km2", as.character(z)), pos = 2, offset = 2, xpd = TRUE)
	#		symbols(1, (ymin-0.10), squares = 1, inches = 0.02, add = TRUE, xpd = TRUE, fg = gray(0.9), bg = gray(0.9))
	#		text(1, (ymin-0.10),"0", pos = 2, offset = 2, xpd = TRUE)
	#		#lines(c(0.98, 1.02), c(1.15,1.15), xpd = TRUE, col = gray(0.5))
	#	}
	#}

	#if (dopng) { dev.off()}
	
}