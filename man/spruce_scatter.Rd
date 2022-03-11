\name{spruce_scatter}
\alias{spruce_scatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
spruce_scatter
}
\description{
NA
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spruce_scatter(data, title = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
}
\details{
NA%%  ~~ If necessary, more details than the description above ~~
}
\value{
NA
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
NA
%% ~put references to the literature/web site here ~
}
\author{
NA%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
NA%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, title = FALSE)
{
    mainTitle = NULL
    y_max = max(data[2])
    x_max = max(data[1])
    if (title == TRUE) {
        mainTitle = "Height vs Breast Height Diameter"
    }
    plot(Height ~ BHDiameter, bg = "Blue", pch = 21, cex = 1.2,
        ylim = c(0, y_max), xlim = c(0, x_max), main = mainTitle,
        data = data)
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
