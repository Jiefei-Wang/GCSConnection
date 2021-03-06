% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FolderClass.R
\name{names,FolderClass-method}
\alias{names,FolderClass-method}
\title{The Names of an Object}
\usage{
\S4method{names}{FolderClass}(x)
}
\arguments{
\item{x}{an \R object.}
}
\value{
For \code{names}, \code{NULL} or a character vector of the same length
  as \code{x}.  (\code{NULL} is given if the object has no names,
  including for objects of types which cannot have names.)  For an
  environment, the length is the number of objects in the environment
  but the order of the names is arbitrary.

  For \code{names<-}, the updated object.  (Note that the value of
  \code{names(x) <- value} is that of the assignment, \code{value}, not
  the return value from the left-hand side.)
}
\description{
Functions to get or set the names of an object.
}
\details{
\code{names} is a generic accessor function, and \code{names<-} is a
  generic replacement function.  The default methods get and set
  the \code{"names"} attribute of a vector (including a list) or
  pairlist.

  For an \code{\link[base]{environment}} \code{env}, \code{names(env)} gives
  the names of the corresponding list, i.e.,
  \code{names(as.list(env, all.names = TRUE))} which are also given by
  \code{\link[base]{ls}(env, all.names = TRUE, sorted = FALSE)}.  If the
  environment is used as a hash table, \code{names(env)} are its
  \dQuote{keys}.

  If \code{value} is shorter than \code{x}, it is extended by character
  \code{NA}s to the length of \code{x}.

  It is possible to update just part of the names attribute via the
  general rules: see the examples.  This works because the expression
  there is evaluated as \code{z <- "names<-"(z, "[<-"(names(z), 3, "c2"))}.

  The name \code{""} is special: it is used to indicate that there is no
  name associated with an element of a (atomic or generic) vector.
  Subscripting by \code{""} will match nothing (not even elements which
  have no name).

  A name can be character \code{NA}, but such a name will never be
  matched and is likely to lead to confusion.

  Both are \link[base]{primitive} functions.
}
\examples{
# print the names attribute of the islands data set
names(islands)

# remove the names attribute
names(islands) <- NULL
islands
rm(islands) # remove the copy made

z <- list(a = 1, b = "c", c = 1:3)
names(z)
# change just the name of the third element.
names(z)[3] <- "c2"
z

z <- 1:3
names(z)
## assign just one name
names(z)[2] <- "b"
z

\dontshow{## "show" the equivalence claimed above:
  for(e in c(baseenv(), globalenv()))
  stopifnot(identical(names(e), ls(e, all.names=TRUE, sorted=FALSE)),
            identical(names(e), names(as.list(e, all.names=TRUE))))
}
}
\references{
Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
\code{\link{slotNames}}, \code{\link[base]{dimnames}}.
}
