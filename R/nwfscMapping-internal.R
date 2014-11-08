.addBubbles.ach <-
function (events, type = c("perceptual", "surface", "volume"),
    z.max = NULL, max.size = 0.8, symbol.zero = "+", symbol.fg = rgb(0,
        0, 0, 0.6), symbol.bg = rgb(0, 0, 0, 0.3), legend.pos = "bottomleft",
    legend.breaks = NULL, show.actual = FALSE, legend.type = c("nested",
        "horiz", "vert"), legend.title = "Abundance", legend.cex = 0.8,legend.title.cex=legend.cex+0.2,
    ...)
{
    #A sligth modification from PBSmapping addBubbles to call a modified legend function (fixes an error)
    events <- .validateEventData(events)
    if (is.character(events))
        stop(paste("Invalid EventData 'events'.\n", events, sep = ""))
    if (!is.element("Z", names(events)))
        stop("EventData is missing required column 'Z'.\n")
    type <- match.arg(type)
    if (!is.null(legend.pos))
        legend.type <- match.arg(legend.type)
    if (is.null(z.max) || is.na(z.max))
        z.max <- max(events$Z, na.rm = TRUE)
    if (is.null(legend.breaks) || is.na(legend.breaks))
        legend.breaks <- pretty(range(events$Z), 3)[-1]
    if (show.actual)
        legend.breaks <- signif(legend.breaks/max(legend.breaks) *
            max(events$Z, na.rm = TRUE), 3)
    usr.xdiff <- par("usr")[2] - par("usr")[1]
    usr.ydiff <- par("usr")[4] - par("usr")[3]
    stand.rad <- (max.size/2)/par("pin")[1] * usr.xdiff
    events <- events[order(events$Z, decreasing = TRUE), ]
    type <- match.arg(type)
    switch(type, volume = {
        radii <- ((events$Z/z.max)^(1/3)) * stand.rad
        radii.leg <- ((legend.breaks/z.max)^(1/3)) * stand.rad
    }, surface = {
        radii <- sqrt(events$Z/z.max) * stand.rad
        radii.leg <- sqrt(legend.breaks/z.max) * stand.rad
    }, perceptual = {
        radii <- ((events$Z/z.max)^0.57) * stand.rad
        radii.leg <- ((legend.breaks/z.max)^0.57) * stand.rad
    })
    isZero <- unlist(lapply(events$Z, all.equal, current = 0)) ==
        "TRUE"
    symbols(events$X[!isZero], events$Y[!isZero], circles = radii[!isZero],
        inches = FALSE, bg = symbol.bg, fg = symbol.fg, add = TRUE)
    if (any(isZero) && (!is.logical(symbol.zero) || symbol.zero)) {
        if (is.logical(symbol.zero))
            symbol.zero <- "+"
        dots <- list(...)
        if (!is.null(dots$pch))
            stop("Specify 'pch' through 'symbol.zero'")
        points(events$X[isZero], events$Y[isZero], pch = symbol.zero,
            ...)
    }
    if (!is.null(legend.pos)) {
        if (!any(isZero))
            symbol.zero <- FALSE
        .addBubblesLegend.ach(radii.leg, usr.xdiff, usr.ydiff, symbol.zero,
            symbol.fg, symbol.bg, legend.pos, legend.breaks,
            legend.type, legend.title, legend.cex,legend.title.cex, ...)
    }
    invisible()
}
.addBubblesLegend.ach <-
function (radii.leg, usr.xdiff, usr.ydiff, symbol.zero, symbol.fg,
    symbol.bg, legend.pos, legend.breaks, legend.type, legend.title,
    legend.cex,legend.title.cex=legend.cex+0.2, ...)
{
    #fixes an error in the vertical legend
    ratio.y.x = (usr.ydiff/par("pin")[2])/(usr.xdiff/par("pin")[1])
    gap.x <- par("cxy")[1] * legend.cex/2
    gap.y <- par("cxy")[2] * legend.cex/2
    radii.leg.y <- radii.leg * ratio.y.x
    leg.tex.w <- strwidth(legend.breaks, units = "user") * legend.cex
    title.w = strwidth(legend.title)
    max.tex.w <- max(leg.tex.w)
    switch(legend.type, nested = {
        legend.height <- 2 * max(radii.leg.y) + 3 * gap.y
        legend.width <- 2 * max(radii.leg) + gap.x + max.tex.w
    }, horiz = {
        legend.height <- 2 * max(radii.leg.y) + 3 * gap.y
        legend.width <- 2 * sum(radii.leg) + (length(legend.breaks) -
            1) * gap.x
    }, vert = {
        legend.height <- 2 * sum(radii.leg.y) + (length(legend.breaks) -
            1) * gap.y + 3 * gap.y
        legend.width <- 2 * max(radii.leg) + gap.x + max.tex.w
    })
    if (title.w > legend.width) {
        w.adj <- (title.w - legend.width)/2
    }
    else {
        w.adj <- 0
    }
    if (class(legend.pos) == "numeric") {
        legend.loc <- legend.pos
    }
    else {
        corners <- c("bottomleft", "bottomright", "topleft",
            "topright")
        if (legend.pos %in% corners) {
            legend.loc <- switch(legend.pos, bottomleft = c(par("usr")[1] +
                0.025 * usr.xdiff + w.adj, par("usr")[3] + 0.025 *
                usr.ydiff + legend.height), bottomright = c(par("usr")[2] -
                (0.025 * usr.xdiff + legend.width + w.adj), par("usr")[3] +
                0.025 * usr.ydiff + legend.height), topleft = c(par("usr")[1] +
                0.025 * usr.xdiff + w.adj, par("usr")[4] - 0.025 *
                usr.ydiff), topright = c(par("usr")[2] - (0.025 *
                usr.xdiff + legend.width + w.adj), par("usr")[4] -
                0.025 * usr.ydiff))
        }
    }
    switch(legend.type, nested = {
        legend.loc[1] <- legend.loc[1] + max(radii.leg)
        legend.loc[2] <- legend.loc[2] - legend.height
        r <- rev(radii.leg)
        bb <- rev(legend.breaks)
        x.text.leg <- legend.loc[1] + max(r) + gap.x + max.tex.w
        for (i in 1:length(r)) {
            symbols(legend.loc[1], legend.loc[2] + r[i] * ratio.y.x,
                circles = r[i], inches = FALSE, add = TRUE, bg = symbol.bg,
                fg = symbol.fg)
            lines(c(legend.loc[1], legend.loc[1] + r[1] + gap.x),
                rep(legend.loc[2] + 2 * r[i] * ratio.y.x, 2))
            text(x.text.leg, legend.loc[2] + 2 * r[i] * ratio.y.x,
                bb[i], adj = c(1, 0.5), cex = legend.cex)
        }
        x.title.leg <- legend.loc[1] - max(radii.leg) + (legend.width/2)
        text(x.title.leg, legend.loc[2] + legend.height, legend.title,
            adj = c(0.5, 0.5), cex = legend.title.cex, col = "black")
        zlab <- c(x.title.leg, legend.loc[2] + legend.height/4)
    }, horiz = {
        legend.loc[2] <- legend.loc[2] + max(radii.leg.y) - legend.height
        offset <- vector()
        for (i in 1:length(radii.leg)) offset[i] <- 2 * sum(radii.leg[1:i]) -
            radii.leg[i] + (i - 1) * gap.x
        symbols(legend.loc[1] + offset, rep(legend.loc[2], length(radii.leg)),
            circles = radii.leg, inches = FALSE, bg = symbol.bg,
            fg = symbol.fg, add = TRUE)
        text(legend.loc[1] + offset, legend.loc[2] + radii.leg.y +
            gap.y, legend.breaks, adj = c(0.5, 0.5), cex = legend.cex)
        text(legend.loc[1] + legend.width/2, legend.loc[2] +
            legend.height - max(radii.leg.y), legend.title, adj = c(0.5,
            0.5), cex = legend.title.cex, col = "black")
        zlab <- c(legend.loc[1], legend.loc[2] - legend.height/8)
    }, vert = {
        if (any(legend.pos == c("bottomleft", "topleft"))) legend.loc[1] <- legend.loc[1] +
            0.05 * usr.xdiff
        offset <- vector()
        for (i in 1:length(legend.breaks)) offset[i] <- gap.y +
            2 * sum(radii.leg.y[1:i]) - radii.leg.y[i] + i *
            gap.y
        symbols(rep(legend.loc[1], length(legend.breaks)), legend.loc[2] -
            offset, circles = radii.leg, bg = symbol.bg, fg = symbol.fg,
            inches = FALSE, add = TRUE)
        x.text.leg <- legend.loc[1] + max(radii.leg) + gap.x +
            max.tex.w
        text(rep(x.text.leg, length(legend.breaks)), legend.loc[2] -
            offset, legend.breaks, cex = legend.cex, adj = c(1,0.5), col = "black")
        text(legend.loc[1] + legend.width/2 - max(radii.leg),
            legend.loc[2], legend.title, adj = c(0.5, 0.5), cex = legend.title.cex, col = "black")
        x.title.leg <- legend.loc[1] - max(radii.leg) + (legend.width/2)  #ach
        zlab <- c(x.title.leg, legend.loc[2])
    })
    if (!is.logical(symbol.zero))
        legend(zlab[1], zlab[2], legend = "zero", pch = symbol.zero,
            xjust = 0, yjust = 1, bty = "n", cex = 0.8, x.intersp = 0.5)
    invisible()
}
.lat.to.km <-
function(lat){
# lat in degrees
    lat.rad <- (lat * pi)/180
    return(111.14 - 0.56 * cos(2 * lat.rad))
}
.long.to.km <-
function(lat){
# lat in degrees
    lat.rad <- (lat * pi)/180
    return(111.41 * cos(lat.rad) - 0.1 * cos(3 * lat.rad))
}


