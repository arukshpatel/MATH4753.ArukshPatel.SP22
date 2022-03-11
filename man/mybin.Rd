\name{mybin}
\alias{mybin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
\bold{mybin}
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
NA
}
\usage{
mybin(iter = 100, n = 10, p = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{iter}{
%%     ~~Describe \code{iter} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
n/a
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
n/a
}
\author{
%%  ~~who you are~~
na
}
\note{
%%  ~~further notes~~
n/a
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
n/a%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (iter = 100, n = 10, p = 0.5)
{
    sam.mat = matrix(NA, nr = n, nc = iter, byrow = TRUE)
    succ = c()
    for (i in 1:iter) {
        sam.mat[, i] = sample(c(1, 0), n, replace = TRUE, prob = c(p,
            1 - p))
        succ[i] = sum(sam.mat[, i])
    }
    succ.tab = table(factor(succ, levels = 0:n))
    barplot(succ.tab/(iter), col = rainbow(n + 1), main = "Binomial simulation",
        xlab = "Number of successes")
    succ.tab/iter
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
