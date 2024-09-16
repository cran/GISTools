thematic.map <- function(data, var.names, colorStyle = NULL, na.pos = "bottomright", bglyrs, bgStyle,
                         scaleBar.pos = "bottomright", mtitle = NULL, htitle = NULL,
                         legend = "Legend", legend.pos = "topright", cuts = 5, cutter = quantileCuts, horiz = FALSE, digits=2,...) {
  
  # Validate input
  bbox.data <- NULL
  dataCoordinates <- data.frame(x = c(0, 1), y = c(0, 0))
  
  if (missing(data)) {
    stop("A Spatial*DataFrame or sf object should be specified")
  } else {
    if (inherits(data, "Spatial")) {
      a.names <- names(data)
      bbox.data <- bbox(data)
      dataCoordinates$x <- bbox.data[1, ]
      dataCoordinates$y <- bbox.data[2, ]
      bbox.data <- t(bbox.data)
      sp.df <- TRUE
    } else if (inherits(data, "sf")) {
      a.names <- names(data)
      bbox.data <- st_bbox(data)
      dataCoordinates$x <- bbox.data[c("xmin", "xmax")]
      dataCoordinates$y <- bbox.data[c("ymin", "ymax")]
      bbox.data <- matrix(bbox.data[c(1,3,2,4)], nrow=2)
      sp.df <- FALSE
    } else {
      stop("The data should be either a Spatial*DataFrame or sf object")
    }
  }
  
  if (missing(var.names)) {
    stop("Attributes to be mapped should be specified")
  }
  
  vars <- var.names[var.names %in% a.names]
  
  if (length(vars) == 0) {
    stop("Correct attributes should be specified!")
  }
  
  var.n <- length(vars)
  
  if (is.null(mtitle)) {
    mtitle <- paste("Map of", vars, sep=" ")
  } else {
    if (length(mtitle) != var.n) {
      stop("Length of mtitle should match the number of variables")
    }
  }
  
  if (is.null(htitle)) {
    htitle <- paste("Histogram of", vars, sep=" ")
  } else {
    if (length(htitle) != var.n) {
      stop("Length of htitle should match the number of variables")
    }
  }
  
  colorfun <- FALSE
  if (is.null(colorStyle)) {
    colorStyle <- rep("red", var.n)
  } else if (is.function(colorStyle)) {
    colorfun <- TRUE
  } else {
    if (length(colorStyle) != var.n) {
      colorStyle <- rep(colorStyle, length.out = var.n)
    }
  }
  
  bgd <- FALSE
  if (!missing(bglyrs) && length(bglyrs) > 0) {
    bgd <- TRUE
    if (!is.list(bglyrs)) {
      bglyrs <- list(bglyrs)
    }
    nbglyr <- length(bglyrs)
    for (i in seq_along(bglyrs)) {
      if (inherits(bglyrs[[i]], "Spatial"))
      {
         bbox1 <-  bbox(bglyrs[[i]])
         bbox.data[1, 1] <- min(bbox.data[1, 1], bbox1[1,1])
         bbox.data[1, 2] <- min(bbox.data[1, 2], bbox1[1,2])
         bbox.data[2, 1] <- max(bbox.data[2, 1], bbox1[2,1])
         bbox.data[2, 2] <- max(bbox.data[2, 2], bbox1[2,2])
      }
       else
       {
          bbox1 <- st_bbox(bglyrs[[i]])
          bbox.data[1, 1] <- min(bbox.data[1, 1], bbox1[1])
         bbox.data[1, 2] <- min(bbox.data[1, 2], bbox1[2])
         bbox.data[2, 1] <- max(bbox.data[2, 1], bbox1[3])
         bbox.data[2, 2] <- max(bbox.data[2, 2], bbox1[4])
       }  
    }
    if(missing(bgStyle))
      bgStyle <- list(col="grey", cex=1, lwd=1, pch=16, lty=1)
    bgStyle$col <- rep(bgStyle$col, nbglyr)
    bgStyle$cex <- rep(bgStyle$cex, nbglyr)
    bgStyle$lwd <- rep(bgStyle$lwd, nbglyr)
    bgStyle$pch <- rep(bgStyle$pch, nbglyr)
    bgStyle$lty <- rep(bgStyle$lty, nbglyr)
  }
  
  # Setup margins and layout
  dev.new(width = 11 * var.n, height = 16)
  opar <- par(oma = c(0, 0, 0, 0), mar = c(0.5, 0, 0, 0), family = "serif")
  on.exit(par(opar))
  layout(matrix(1:(2 * var.n), nrow = 2, ncol = var.n, byrow = FALSE),
         widths = rep(c(4, 4), var.n), heights = rep(c(6, 3), var.n))
  
  for (mNums in seq_along(vars)) {
    var.data <- if (sp.df) data[[vars[mNums]]] else data[[vars[mNums]]]
    var.range <- range(var.data, na.rm = TRUE)
    var.norm <- (var.data - var.range[1]) / diff(var.range)
    brk = round(cutter(var.data, n = cuts), digits=digits)

    # Handle color scaling
    colorStyle.mNums <- if (colorfun) colorStyle else colorStyle[[mNums]]
    colSet <- if (is.character(colorStyle.mNums)) {
      
      switch(colorStyle.mNums,
             "red" = brewer.pal(cuts, 'Reds')[1 + findInterval(var.data, brk)],
             "blue" = brewer.pal(cuts, 'Blues')[1 + findInterval(var.data, brk)],
             "green" = brewer.pal(cuts, 'Greens')[1 + findInterval(var.data, brk)],
             brewer.pal(cuts, 'Greys')[1 + findInterval(var.data, brk)]) # Default black
    } else if (is.function(colorStyle.mNums)) {
      colSet <- colorStyle.mNums(cuts)[1 + findInterval(var.data, brk)]  # Use gradient function
    } else {
      stop("colorStyle should be either a color name or a function returning colors")
    }
    # Plot map
    opar <- par(mar = c(0.5, 0.5, 1, 0.5)) 
    on.exit(par(opar))
    if (bgd) {
      if(inherits(bglyrs[[1]], "Spatial"))
          plot(bglyrs[[1]], col = "white", xlim = bbox.data[, 1], ylim = bbox.data[, 2])
        else
          plot(st_geometry(bglyrs[[1]]), col = "white", xlim = bbox.data[, 1], ylim = bbox.data[, 2])
      for (i in seq_along(bglyrs)) {
        if (inherits(bglyrs[[i]], "Spatial")) 
           plot(bglyrs[[i]], pch = bgStyle$pch[i], col = bgStyle$col[i], cex = bgStyle$cex[i], 
                lwd=bgStyle$lwd[i],lty=bgStyle$lty[i],add = TRUE)
        else 
           plot(st_geometry(bglyrs[[i]]), pch = bgStyle$pch[i], col = bgStyle$col[i], 
                cex = bgStyle$cex[i], lwd=bgStyle$lwd[i],lty=bgStyle$lty[i], add = TRUE)
      }
      if (sp.df) {
         plot(data, pch = 16, col = colSet, cex = var.norm * 2, lwd= var.norm * 2,add = TRUE)
       } else {
        plot(st_geometry(data), col = colSet, cex = var.norm * 2, lwd= var.norm * 2, add = TRUE)
      }
    } else {
      if (sp.df) {
        plot(data, pch = 16, col = colSet, cex = var.norm * 2, lwd= var.norm * 2, xlim = bbox.data[,1], ylim = bbox.data[, 2])
      } else {
        plot(st_geometry(data),pch = 16,  col = colSet, cex = var.norm * 2, lwd= var.norm * 2, xlim = bbox.data[,1], ylim = bbox.data[,2])
      }
    }
    
    # Map title
    title(main = mtitle[mNums], font.main = 2, cex.main = 1.5)
    
    # Draw North Arrow
    x_width <- dataCoordinates$x[2] - dataCoordinates$x[1]
    y_width <- dataCoordinates$y[2] - dataCoordinates$y[1]
    north_arrow_pos <- switch(tolower(na.pos),
                              "topright" = c(max(dataCoordinates$x) - x_width * 0.06, max(dataCoordinates$y) - y_width * 0.06),
                              "topleft" = c(min(dataCoordinates$x) + x_width * 0.06, max(dataCoordinates$y) - y_width * 0.06),
                              "bottomright" = c(max(dataCoordinates$x) - x_width * 0.06, min(dataCoordinates$y) + y_width * 0.06),
                              "bottomleft" = c(min(dataCoordinates$x) + x_width * 0.06, min(dataCoordinates$y) + y_width * 0.06),
                              NULL)
    if (!is.null(north_arrow_pos)) {
      north.arrow(north_arrow_pos[1], north_arrow_pos[2], miles2ft(0.2), cex.lab = 0.8, col = "black")
    }
    
    # Draw Scale Bar
    scale_bar_pos <- switch(tolower(scaleBar.pos),
                            "topright" = c(max(dataCoordinates$x) - x_width * 0.06, max(dataCoordinates$y) + y_width * 0.06),
                            "topleft" = c(min(dataCoordinates$x) + x_width * 0.06, max(dataCoordinates$y) + y_width * 0.06),
                            "bottomright" = c(max(dataCoordinates$x) - x_width * 0.06, min(dataCoordinates$y) + y_width * 0.06),
                            "bottomleft" = c(min(dataCoordinates$x) + x_width * 0.06, min(dataCoordinates$y) + y_width * 0.06),
                            NULL)
    if (!is.null(scale_bar_pos)) {
      map.scale(scale_bar_pos[1], scale_bar_pos[2], miles2ft(1), "Miles", 1, 0.5)
    }
    
    # Draw Legend
    if(colorfun)
      legend_colors <- colorStyle.mNums(cuts)
    else
      legend_colors <- sort(unique(colSet),decreasing =T)
    
    lx <- length(brk)
    legend_labels <- character(lx + 1)
    legend_labels[1] <- paste("under", sprintf("%g", brk[1]))
    for (i in 1:(lx - 1)) legend_labels[i + 1] <- paste(sprintf("%g", brk[i]), "to", sprintf("%g", brk[i + 1]))
    legend_labels[lx + 1] <- paste("over", sprintf("%g", brk[lx]))
    
    maxwidth <- max(strwidth(legend_labels))
    legend(tolower(legend.pos), legend = legend_labels, fill = legend_colors, col = legend_colors,
           title = legend, horiz = horiz, text.width = maxwidth, cex = 1, bty = "n",...)
    # Draw Histogram
    opar <- par(mar = c(0.5, 6, 0.5, 6))
    on.exit(par(opar))
    hist(var.data, col = legend_colors, breaks = c(min(var.data),brk,max(var.data)),
         main = htitle[mNums], xlab = "", ylab = "")
  }
}