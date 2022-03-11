\name{myddt}
\alias{myddt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\bold{myddt}
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function to filter, quadratic graph, and export DDT data
}
\usage{
myddt(df, cond, xAxis = "WEIGHT", yAxis = "LENGTH", col = "RIVER", title = "Aruksh Patel")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
   DDT Data Frame
%%     ~~Describe \code{df} here~~
}
  \item{cond}{
  Conditional statement to which to the data should be filtered by
%%     ~~Describe \code{cond} here~~
}
  \item{xAxis}{
  Dependent Variable for your graph -- DEFAULT: WEIGHT
%%     ~~Describe \code{xAxis} here~~
}
  \item{yAxis}{
  Independent Variable for your graph -- DEFAULT: LENGTH
%%     ~~Describe \code{yAxis} here~~
}
  \item{col}{
  Color of the points on graph -- DEFAULT: RIVER
%%     ~~Describe \code{col} here~~
}
  \item{title}{
  Title of Graph -- DEFAULT: ARUKSH PATEL
%%     ~~Describe \code{title} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
NA
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{dfData}{Filtered Data as data frame}
\item{ggplot}{ggplot2 graph}
\item{RelativeFilePath}{Filepath relative to the R script}
\item{AbsoluteFilePath}{Absolute Filepath in the system}

}
\references{
%% ~put references to the literature/web site here ~
ggplot2 and dplyr
}
\author{
%%  ~~who you are~~
Aruksh Patel
}
\note{
%%  ~~further notes~~
NA
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
NA
}
\examples{
\dontrun{

myddt(df = ddt.df, cond = RIVER == "TRM", title = "TRM River Graph")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
