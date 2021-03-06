% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/naming_functions.R
\name{make_filename_postfix}
\alias{make_filename_postfix}
\title{Function assembling tags and values into a single string, separated with specified
delimiters. The string will be used a postfix, attached to the project name.}
\usage{
make_filename_postfix(tags, values, tag.delim = ".", component.delim = "_")
}
\arguments{
\item{tags}{list. List with a "tagging schema": list elements contain tags for
specific components of the name. E.g. list(markup = "mk",
analysis = "an").}

\item{values}{list. List with a "values schema": lsit elements contain
values for the components of the name associated with a specific
analysis. E.g. list(markup = "like-parker", analysis = "variant4")}

\item{tag.delim}{character. The character which will be used as a delimiter
between component tags and values}

\item{component.delim}{character. The character which will be used as a delimiter
between components tags}
}
\value{
string. Filename postfix, starting with the naming component delimiter.
E.g. _mk.parker-like_an.variant4
}
\description{
Function assembling tags and values into a single string, separated with specified
delimiters. The string will be used a postfix, attached to the project name.
}
