% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gclassificatin_func.R
\name{reduce_tree}
\alias{reduce_tree}
\title{A function that creates new (reduced) tree based on an attribute that must
not be NA}
\usage{
reduce_tree(data_tree, attribute)
}
\arguments{
\item{a}{data.tree object that provides the source}

\item{a}{list/vector of nodes that build up the tree}
}
\value{
a data.tree object
}
\description{
A function that creates new (reduced) tree based on an attribute that must
not be NA
}
\examples{
test_tree_reduced = reduce_tree(base_tree_b, attribute = 'station_id')
}
\author{
Stefan Lüdtke
}
