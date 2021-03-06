% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_values.R
\name{plot_cells_heatmap}
\alias{plot_cells_heatmap}
\title{Plot heatmap of cell counts}
\usage{
plot_cells_heatmap(dat, by, fill, limits = NULL, plot.name)
}
\arguments{
\item{dat}{data.frame returned by \code{count_NAs} or \code{count_extremes()}.
It should have three columns: grouping variable (subj or item),
name of the cell (region+fixationtype by default),
number of observations in the cell}

\item{by}{quosure. name of the grouping variable (normally, first column
in \code{dat})}

\item{fill}{quosure. name of the column w/count of observations (normally,
third column in \code{dat})}

\item{limits}{vector of two integers: lowest and highest N of observations
possible in the cell to calibrate color coding. If NULL (default),
the limits will be set automatically based on the data.}

\item{plot.name}{character}
}
\value{
heatmap plot of the cell counts
}
\description{
A generic function to plot heatmaps of cell counts. Shouldn't be used directly
to plot counts produced by \code{\link{count_cells}} and related functions, since
those counts may lack the data from certain subsets of the data, if those
subsets lack the relevant kind of values (e.g. NAs or extreme values).
Instead, \code{\link{plot_cells_count}}, or convenience wrappers around it
should be called, since they take care of subsets with 0 observations
by artificially adding these 0s back to the counts.
}
