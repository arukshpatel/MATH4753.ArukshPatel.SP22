\name{myf}
\alias{myf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
myf
}
\description{
NA
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
myf(x, xk, coef)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{xk}{
%%     ~~Describe \code{xk} here~~
}
  \item{coef}{
%%     ~~Describe \code{coef} here~~
}
}
\details{
NA
%%  ~~ If necessary, more details than the description above ~~
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
NA%%  ~~further notes~~
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
function (x, xk, coef)
{
    coef[1] + coef[2] * (x) + coef[3] * (x - xk) * (x - xk >
        0)
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
