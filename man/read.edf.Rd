\name{read.edf}
\alias{read.edf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import EDF/EDF+
}
\description{
Imports EDF/EDF+ files.
}
\usage{
read.edf(file)
}
\arguments{
  \item{file}{
An EDF/EDF+ file.
}
}
\value{
A edf object. Contains header, signal, data and annotation information.
}
\author{
Ryan M. Hope
}

\examples{
#read.edf("~/eegsample.edf")
}

\keyword{ file }