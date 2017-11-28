#' plot flower plot
#'
#' This function plot a flower plot
#' @param sample a vector of sample names
#' @param value a vector of values
#' @param start init angle
#' @param a ellipse x length
#' @param b ellipse y length
#' @param ellipse_col flower colors
#' @param circle_col circle colors
#' @param circle_text_cex circle text cex
#' @export
#' @examples
#' flower_plot(1:12, 1:12)

flower_plot <- function(sample, value, start = 90, a = 0.5, b = 2,
                        ellipse_col = rgb(135, 206, 235, 150, max = 255),
                        circle_col = rgb(0, 162, 214, max = 255),
                        circle_text_cex = 1.5
) {
  par( bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1,1,1,1))
  plot(c(0,10),c(0,10),type="n")
  n   <- length(sample)
  deg <- 360 / n
  res <- lapply(1:n, function(t){
    plotrix::draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180),
                 y = 5 + sin((start + deg * (t - 1)) * pi / 180),
                 col = ellipse_col,
                 border = ellipse_col,
                 a = a, b = b, angle = deg * (t - 1))
    text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
         y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
         value[t]
    )

    if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) - start,
           adj = 1,
           cex = circle_text_cex
      )

    } else {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) + start,
           adj = 0,
           cex = circle_text_cex
      )
    }
  })
  plotrix::draw.circle(x = 5, y = 5, r = 1.3, col = circle_col, border = circle_col)
}

point_plot <- function(){
  plot(c(1,5), c(1,5))
}

sum_total <- function(){
  return(sum(1:5))
}
