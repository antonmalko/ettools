% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/naming_functions.R
\name{save_named_data_file}
\alias{save_named_data_file}
\title{A function to save .Rdata files named according to a naming schema.}
\usage{
save_named_data_file(..., proj.name, tags, values, data.type, tag.delim = ".",
  component.delim = "_", output.dir = ".")
}
\arguments{
\item{...}{the names of the objects to be saved (as symbols or character strings).
(as in base::save)}

\item{proj.name}{character. Project name. Will be used as the first substring
in the file name}

\item{tags}{list. List with a "tagging schema": list elements contain tags for
specific components of the name. E.g. list(markup = "mk",
analysis = "an").}

\item{values}{list. List with a "values schema": lsit elements contain
values for the components of the name associated with a specific
analysis. E.g. list(markup = "like-parker", analysis = "variant4")}

\item{data.type}{optional string specifying type of the data stored in the file.
it will be added after the project name but before the tags. E.g., if
\code{data.type = "data"}, and the tags and values are as in the
examples above, the name could be project1_data_mk.parker-like_an.variant4}

\item{tag.delim}{character. Delimiter between tags and values.}

\item{component.delim}{character. Delimiter between filename components}

\item{output.dir}{character path to the output folder}
}
\value{
Full path to the created file
}
\description{
A function to save .Rdata files named according to a naming schema.
}
