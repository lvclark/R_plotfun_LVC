# Lindsay V. Clark, March 30, 2016
# Feel free to use, modify, or distribute this code under the terms of GNU General Public License v.3.

# functions to make floating pie charts that are square or triangular.  Can put them
# anywhere on plot, like with plotrix's floating.pie function.

require(plotrix)

## Example use
# plot(1:10, 1:10)
# squarePie(6, 2, c(2,1,4), width = 1)
# trianglePie(2, 6, c(1,1,1,5), width = 1, col = c("red", "green", "blue", "black"))

# x and y are the center coordinates for the square.
# slicesizes is a vector of the relative sizes of the slices; does not have to sum to 1,
# and can contain zeros.
squarePie <- function(x, y, slicesizes, width, col = rainbow(length(slicesizes)), lwd = 1){
    if(length(x) != 1 || length(y) != 1){
        stop("squarePie only creates one pie chart at a time.  Use a for loop or mapply to make multiple pie charts.")
    }
    if(length(slicesizes) != length(col)){
        stop("slicesizes and col must be the same length.")
    }
    Xhalfwidth <- width/2
    Yhalfwidth <- width/2 * getYmult()
    left <- x - Xhalfwidth
    right <- x + Xhalfwidth
    top <- y + Yhalfwidth
    bottom <- y - Yhalfwidth

    # eliminate zeroes (floating.pie does not do this)
    nonzero <- slicesizes > 0
    if(all(!nonzero)) stop("At least one slice size must be above zero.")
    slicesizes <- slicesizes[nonzero]
    col <- col[nonzero]

    # convert slice sizes to radians
    sliceRad <- 2 * pi * slicesizes / sum(slicesizes)
    # variable to keep track of our rotation around the pie
    radTot <- 0

    # function to calculate the side of a square that an angle hits and the point on that side
    sqside <- function(rad){
        if((rad >= 0 && rad < pi/4) || (rad >= 7*pi/4 && rad <= 2*pi + 0.00001)){
            side <- 1 # right side
            x1 <- right
            y1 <- y + Yhalfwidth * sin(rad)/cos(rad)
        }
        if(rad >= pi/4 && rad < 3*pi/4){
            side <- 2 # top side
            x1 <- x + Xhalfwidth * cos(rad)/sin(rad)
            y1 <- top
        }
        if(rad >= 3*pi/4 && rad < 5*pi/4){
            side <- 3 # left side
            x1 <- left
            y1 <- y - Yhalfwidth * sin(rad)/cos(rad)
        }
        if(rad >= 5*pi/4 && rad < 7*pi/4){
            side <- 4 # bottom side
            x1 <- x - Xhalfwidth * cos(rad)/sin(rad)
            y1 <- bottom
        }
        return(list(side = side, x = x1, y = y1))
    }

    sliceVertices <- list()
    # calculate polygon vertices for each slice
    for(s in 1:length(sliceRad)){
        sliceVertices[[s]] <- list(x = x, y = y) # each slice starts from the center
        rad1 <- radTot                         # first angle
        radTot <- rad2 <- radTot + sliceRad[s] # second angle
        # which side of the square does each line intersect?
        side1 <- sqside(rad1)
        side2 <- sqside(rad2)
        # add the first point
        sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, side1$x)
        sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, side1$y)
        # add corners as needed
        if(side1$side == 1 && side2$side == 2){
          sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, right)
          sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, top)
        }
        if(side1$side == 1 && side2$side == 3){
          sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, right, left)
          sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, top, top)
        }
        if(side1$side == 1 && side2$side == 4){
          sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, right, left, left)
          sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, top, top, bottom)
        }
        if(side1$side == 1 && side2$side == 1 && side1$y >= side2$y){
          sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, right, left, left, right)
          sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, top, top, bottom, bottom)
        }
        if(side1$side == 2 && side2$side == 3){
          sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, left)
          sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, top)
        }
        if(side1$side == 2 && side2$side == 4){
          sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, left, left)
          sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, top, bottom)
        }
        if(side1$side == 2 && side2$side == 1){
          sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, left, left, right)
          sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, top, bottom, bottom)
        }
        if(side1$side == 3 && side2$side == 4){
          sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, left)
          sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, bottom)
        }
        if(side1$side == 3 && side2$side == 1){
          sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, left, right)
          sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, bottom, bottom)
        }
        if(side1$side == 4 && side2$side == 1){
          sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, right)
          sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, bottom)
        }
        # add the second point
        sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, side2$x)
        sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, side2$y)

        # draw the polygon
        polygon(x = sliceVertices[[s]]$x, y = sliceVertices[[s]]$y,
                col = col[s], lwd = lwd)
    }
    return(invisible(list(sliceVertices, col)))
}

trianglePie <- function(x, y, slicesizes, width, col = rainbow(length(slicesizes)), lwd = 1){
    if(length(x) != 1 || length(y) != 1){
        stop("trianglePie only creates one pie chart at a time.  Use a for loop or mapply to make multiple pie charts.")
    }
    if(length(slicesizes) != length(col)){
        stop("slicesizes and col must be the same length.")
    }

    # eliminate zeroes (floating.pie does not do this)
    nonzero <- slicesizes > 0
    if(all(!nonzero)) stop("At least one slice size must be above zero.")
    slicesizes <- slicesizes[nonzero]
    col <- col[nonzero]

    # convert slice sizes to radians
    sliceRad <- 2 * pi * slicesizes / sum(slicesizes)
    # variable to keep track of our rotation around the pie
    radTot <- 0

    ymult <- getYmult() # y/x aspect ratio for plotting device

    # vertices of the triangle
    top <- list(x = x, y = y + width/sqrt(3) * ymult)
    right <- list(x = x + width/2, y = y - width/4 * ymult)
    left <- list(x = x - width/2, y = y - width/4 * ymult)

    # function to calculate which side of the triangle and at what point an angle intersects
    trside <- function(rad){
        # slope and intercept to describe the line coming from the center
        slope <- ymult * sin(rad)/cos(rad)
        int <- y - slope*x
        # identify side, calculate slope and intercept of that side
        if((rad >= 0 && rad < pi/2) || (rad >= 11/6 * pi)){
            side <- 1 # right side; slope of -2
            # y-intercept for the line that is the right side
            slopeSide <- (top$y - right$y)/(top$x - right$x)
            intSide <- right$y - slopeSide*right$x
        }
        if(rad >= pi/2 && rad < 7*pi/6){
            side <- 2 # left side; slope of 2
            # y-intercept for the line that is the left side
            slopeSide <- (top$y - left$y)/(top$x - left$x)
            intSide <- left$y - slopeSide*left$x
        }
        if(rad >= 7*pi/6 && rad < 11*pi/6){
            side <- 3 # bottom; slope of 0; intercept of left$y
            slopeSide <- 0
            intSide <- left$y
        }
        # calculate x-coordinate of intersection
        x1 <- (intSide - int)/(slope - slopeSide)
        # calculate y-coordinate of interesection
        y1 <- slope*x1 + int

        # check for rounding errors with vertical lines
        if(y1 < right$y - 0.1 || y1 > top$y + 0.1){
            if(side %in% c(1,2)){
                x1 <- top$x
                y1 <- top$y
            } else {
                x1 <- x
                y1 <- left$y
            }
        }

        return(list(side = side, x = x1, y = y1))
    }

    sliceVertices <- list()
    # calculate polygon vertices for each slice
    for(s in 1:length(sliceRad)){
        sliceVertices[[s]] <- list(x = x, y = y) # each slice starts from the center
        rad1 <- radTot               # first angle
        radTot <- rad2 <- radTot + sliceRad[s] # second angle
        # which side of the square does each line intersect?
        side1 <- trside(rad1)
        side2 <- trside(rad2)
        # add the first point
        sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, side1$x)
        sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, side1$y)
        # add triangle vertices as needed
        if(side1$side == 1 && side2$side == 2){
            sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, top$x)
            sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, top$y)
        }
        if(side1$side == 1 && side2$side == 3){
            sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, top$x, left$x)
            sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, top$y, left$y)
        }
        if(side1$side == 1 && side2$side == 1 && side1$y >= side2$y){
            sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, top$x, left$x, right$x)
            sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, top$y, left$y, right$y)
        }
        if(side1$side == 2 && side2$side == 3){
            sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, left$x)
            sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, left$y)
        }
        if(side1$side == 2 && side2$side == 1){
            sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, left$x, right$x)
            sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, left$y, right$y)
        }
        if(side1$side == 3 && side2$side == 1){
            sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, right$x)
            sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, right$y)
        }
        # add the last point
        sliceVertices[[s]]$x <- c(sliceVertices[[s]]$x, side2$x)
        sliceVertices[[s]]$y <- c(sliceVertices[[s]]$y, side2$y)

        # draw the polygon
        polygon(x = sliceVertices[[s]]$x, y = sliceVertices[[s]]$y,
                col = col[s], lwd = lwd)
    }
    return(invisible(list(sliceVertices, col)))
}
