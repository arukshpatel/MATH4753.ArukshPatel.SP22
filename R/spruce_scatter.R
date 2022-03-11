#' Spruce Scatter Plot
#'
#' @param title : Boolean = TRUE if want title to be present
#' @param data : Data Frame = Data Frame of Spruce
#'
#' @return Outputs a spruce scatter plot
#' @export
#'
#' @examples \dontrun{spruce_scatter(spruce.df, title = TRUE)}
spruce_scatter <- function(data, title = FALSE) {

  mainTitle = NULL;

  y_max = max(data[2])
  x_max = max(data[1])

  if(title == TRUE)
  {
    mainTitle = "Height vs Breast Height Diameter"
  }

  plot(Height~BHDiameter,bg="Blue",
       pch = 21,
       cex = 1.2,
       ylim = c(0, y_max),
       xlim = c(0, x_max),
       main = mainTitle, data=data)
}
