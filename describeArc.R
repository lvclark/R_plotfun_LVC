# function to extract radius and angles for arc plotting.
# click center of circle first, then two end points of arc.
# Example use: describe.arc() # You will then need to click on three points on your plot.

describe.arc <- function(pts=locator(3)){
    require(plotrix)
    x1 <- pts$x[2]-pts$x[1]
    x2 <- pts$x[3]-pts$x[1]
    y1 <- pts$y[2]-pts$y[1]
    y2 <- pts$y[3]-pts$y[1]
    dist1 <- (x1^2 + y1^2)^0.5
    dist2 <- (x2^2 + y2^2)^0.5
    radius <- (dist1+dist2)/2
    angle1 <- atan2(y1,x1)
    angle2 <- atan2(y2,x2)
    if(angle1 < 0 || angle2 < 0){
        angle1 <- angle1 + 2*pi
        angle2 <- angle2 + 2*pi
    }
    print(paste("Center:", pts$x[1], pts$y[1]))
    print(paste("Radius:", radius))
    print(paste("Angle 1:", angle1))
    print(paste("Angle 2:", angle2))
    draw.arc(pts$x[1], pts$y[1], radius, angle1, angle2)
}
