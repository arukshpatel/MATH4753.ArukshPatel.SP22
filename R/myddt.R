#' myddt
#'
#' @param df = DDT Data Frame
#' @param cond = Conditional statement to which to the data should be filtered by
#' @param xAxis = Dependent Variable for your graph -- DEFAULT: WEIGHT
#' @param yAxis = Independent Variable for your graph -- DEFAULT: LENGTH
#' @param col = Color of the points on graph -- DEFAULT: RIVER
#' @param title = Title of Graph -- DEFAULT: ARUKSH PATEL
#'
#' @return List consisting of the filtered data, ggplot graph, relative and absolute path of the CSV files.
#' @export LvsWfor{LIST-OF-SPECIES-IN-FILTERED-DATA}.csv = Output of the filtered data
#'
#' @examples \dontrun{myddt(df = ddt.df, cond = RIVER == "TRM", title = "TRM River Graph")}
#'

myddt <- function(df, cond, xAxis = "WEIGHT", yAxis = "LENGTH", col = "RIVER", title = "Aruksh Patel")
{
  usethis::use_package("ggplot2")
  usethis::use_package("dplyr")

  dfData = df %>% dplyr::filter({{cond}})

  graph = ggplot2::ggplot(dfData, ggplot2::aes_string(y = yAxis, x = xAxis)) +
    ggplot2::geom_point(ggplot2::aes_string(color = col)) +
    ggplot2::ggtitle(title) +
    ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

  print(graph)

  listOfSpecies = unique(dfData$SPECIES)

  speciesFileName = ''

  if(length(listOfSpecies) == 1)
  {
    speciesFileName = listOfSpecies[1]
  } else {

    for(species in listOfSpecies)
    {
      speciesFileName = paste(speciesFileName, species, sep = '-')
    }
  }

  fileName = paste("./output/LvsWfor", speciesFileName, ".csv", sep = "")

  write.csv(dfData, file = fileName)

  absoluteFilePath = paste(getwd(), substring(fileName, 2), sep = "")

  return(list('dfData'= as.data.frame(dfData),
              'ggplot' = graph,
              'RelativeFilePath' = fileName,
              'AbsoluteFilePath' = absoluteFilePath))
}
