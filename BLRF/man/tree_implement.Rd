% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_implement.R
\name{tree_implement}
\alias{tree_implement}
\title{Build Little Random Forest (LRS) for One Subsample.}
\usage{
tree_implement(
  formula,
  subsample,
  r,
  n,
  n_var,
  split,
  control = tree::tree.control(nobs = n, mincut = 5, minsize = 10, mindev = 0.01)
)
}
\arguments{
\item{formula}{an object of class "formula".}

\item{subsample}{data.frame.}

\item{r}{numeric. Number of trees.}

\item{n}{numeric. Number of observations in the original data.}

\item{n_var}{numeric. Number of variables to randomly subset to build one tree.}

\item{split}{character string. Can be "deviance" or "gini".}

\item{control}{control object based on tree::tree.control() function from "tree" package.}
}
\value{
list of tree object.
}
\description{
Build Little Random Forest (LRS) for One Subsample.
}
