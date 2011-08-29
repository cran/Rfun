CountGrayKnots <- function(){
  par(mar = rep(2, 4), bg = 'black')
  plot(c(0, 10), c(0,10), type = 'n', xlab = '', ylab = '', axes = FALSE, main = 'Count the gray knots! :-)', col.main = 'white')
  grid(lty = 'solid', lwd = 8, col = 'gray80')
  dat = expand.grid(seq(0, 10, 2), seq(0, 10, 2))
  points(dat, pch = 16, cex = 2, col = 'white')
  par(bg = "white")
}

