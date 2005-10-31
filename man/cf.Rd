\name{cf}
\docType{data}
\alias{cf}
\title{Counterfactual Replication Data from King and Zeng 2006b}
\description{
     This data set is one of two that together allow the
     replication of the analysis in section 2.4 of King and Zeng 2006b.   
     It contains data on 122 \emph{counterfactuals} derived by King and Zeng 2006b
     from the factual Doyle and Sambanis 2000 data set, \code{f}.  It should
     be used in conjunction with the latter.
}
\usage{data(cf)}
\format{A data frame with dimensions 122-by-11.  Columns are covariates and 
    rows data points (or units).  The covariates are as in \code{f} with the exception
    of the key causal variable, 'untype4', which is transformed to 
    1 - 'untype4'.} 
\source{King and Zeng 2006b}
\references{
     King, Gary and Langche Zeng.  2006b.  "When Can History Be Our Guide?  The
     Pitfalls of Counterfactual Inference."  \emph{International Studies Quarterly},
     forthcoming.

     Doyle, Michael W. and Nicholas Sambanis.  2000.  "International Peacebuilding:
     A Theoretical and Quantitative Analysis."  \emph{American Political Science 
     Review} 94, no.4: 779--801.
}
\keyword{datasets}