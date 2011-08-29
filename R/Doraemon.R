Doraemon <- function(){
## Building block
  ang.l <- seq(1 * pi/2, 3 * pi/2, 0.01)
  ang.r <- seq(3 * pi/2, 5 * pi/2, 0.01)
  x.head <- 20 * cos(seq(-pi/4, pi+pi/4, 0.01))
  y.head <- 20 * sin(seq(-pi/4, pi+pi/4, 0.01))
  x.face <- 15 * cos(seq(-pi/4, pi+pi/4, 0.01))
  y.face <- 15 * sin(seq(-pi/4, pi+pi/4, 0.01)) * .9
  x.mouth <- 10 * cos(seq(5 * pi/4, 7 * pi/4, 0.01))
  y.mouth <- 10 * sin(seq(5 * pi/4, 7 * pi/4, 0.01))
  x.eye <- 5 * cos(seq(0, 2 * pi, 0.01))
  y.eye <- 5 * sin(seq(0, 2 * pi, 0.01))
  x.nose <- 2 * cos(seq(0, 2 * pi, 0.01))
  y.nose <- 2 * sin(seq(0, 2 * pi, 0.01))
  x.neck <- c(-15, 15, 15, -15)
  y.neck <- c(-0, 0, 2, 2)
  x.truck <- c(-15, -20, -18, -15, -15, -15, -2, -1, 0, 1, 2, 15, 15, 15, 18, 20, 15)
  y.truck <- c(0, -5, -9, -7, -6, -20, -20, -18.5, -18, -18.5, -20, -20, -6, -7, -9, -5, 0)
  x.chest <- 10 * cos(seq(3 * pi/4, 9 * pi/4, 0.01))
  y.chest <- 10 * sin(seq(3 * pi/4, 9 * pi/4, 0.01))
  x.pack <- 8 * cos(seq(pi, 2 * pi, 0.01))
  y.pack <- 8 * sin(seq(pi, 2 * pi, 0.01))
  x.neck <- c(.5 * cos(ang.l)-15, .5 * cos(ang.r)+15)
  y.neck <- c(sin(ang.l), sin(ang.r))
  x.foot1 <- c(cos(ang.l)-15, cos(ang.r)-2)
  y.foot1 <- c(1.5 * sin(ang.l), 1.5 * sin(ang.r))
  x.foot2 <- c(cos(ang.l)-15, cos(ang.r)-2)+17
  y.foot2 <- c(1.5 * sin(ang.l), 1.5 * sin(ang.r))

## Draw "DingDang": Chinese transformation of "Doraemon".
  DingDang <- function(a = 10){
    par(mar = rep(2, 4))
    plot(NA, NA, xlim = c(-50, 50), ylim = c(-70, 30), type = 'n', axes = FALSE, xlab = '', ylab = '')
    polygon(x.head, y.head, col = 'dodgerblue2')
    polygon(x.face, y.face-4, col = 'white')
    lines(x.mouth, y.mouth)
    polygon(x.eye-5, y.eye+8, col = 'white')
    polygon(x.eye+5, y.eye+8, col = 'white')
    lines(c(0, 0), c(3, -10))
    polygon(x.nose, y.nose+3, col = 'red')
    lines(c(-12, -4), c(0, -1));lines(c(4, 12), c(-1, 0))
    lines(c(-12, -4), c(-2, -2));lines(c(4, 12), c(-2, -2))
    lines(c(-12, -4), c(-4, -3));lines(c(4, 12), c(-3, -4))
    polygon(x.neck, y.neck-15, col = 'red3')
    polygon(x.truck, y.truck-16, col = 'dodgerblue2')
    polygon(.6 * x.eye-20, .6 * y.eye-24, col = 'white')
    polygon(.6 * x.eye+20, .6 * y.eye-24, col = 'white')
    polygon(x.chest, y.chest-23, col = 'white')
    polygon(x.pack, y.pack-23, col = 'white')
    polygon(x.foot1, y.foot1-37, col = 'white')
    polygon(x.foot2, y.foot2-37, col = 'white')
    polygon(.5 * x.eye, .5 * y.eye-18, col = 'gold')
    points(0, -18.5, pch = 16)
    lines(c(0, 0), c(-18.5, -20.5), lwd = 2)
  }
 
## Animation
  mousemove <- function(buttons, x, y) {
    plx <- grconvertX(x, "ndc", "user")
    ply <- grconvertY(y, "ndc", "user")   
    DingDang()

    ang1 <- atan((ply - 8)/(plx + 5)); if(plx + 5 < 0) ang1 <- pi + ang1
    ang2 <- atan((ply - 8)/(plx - 5)); if(plx - 5 < 0) ang2 <- pi + ang2

    x1 <- 3 * cos(ang1) - 5;
    y1 <- 3 * sin(ang1) + 8
    if(sqrt((ply-8)^2+(plx+5)^2) <= 3) {x1 <- plx; y1 <- ply}
    x2 <- 3 * cos(ang2)+5;
    y2 <- 3 * sin(ang2)+8
    if(sqrt((ply-8)^2 + (plx-5)^2) <= 3) {x2 <- plx; y2 <- ply}  
      
    points(x1, y1, pch = 16, cex = 1)
    points(x2, y2, pch = 16, cex = 1)

    if(plx >- 2.5 & plx < 2.5 & ply > -20.5 & ply < -15.5 ) {
      alarm();
      Sys.sleep(.5)
    }
    NULL
  }

  par(mar = rep(2, 4))
  getGraphicsEvent(onMouseMove = mousemove)
}


