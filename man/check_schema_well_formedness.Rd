% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/naming_functions.R
\name{check_schema_well_formedness}
\alias{check_schema_well_formedness}
\title{A basic validation function for a schema. Checks whether the schema
is a list, whether there is jsut a single non-verctor item assigned to each
element of the list, and whether all list elements are of type 'character'.}
\usage{
check_schema_well_formedness(schema)
}
\arguments{
\item{schema}{Schema to be checked}
}
\value{
Nothing. Throws an error if the naming schema does not satisfy
the basic conditions listed above
}
\description{
A basic validation function for a schema. Checks whether the schema
is a list, whether there is jsut a single non-verctor item assigned to each
element of the list, and whether all list elements are of type 'character'.
}
