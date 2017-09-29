# The naming system works as following. At the initialization of the project,
# one creates a list indicating which subparts a name of the file could in
# principle have. The names of the list components correspond to longer descriptions
# of the filename parts, and the values of the list correspond to shorter tags
# which would actually be used in the filename. E.g.
#
# tagging.schema <- list(markup = "mk",
#                        preprocessing = "prep",
#                        analysis = "an")
#
# Then, within each analysis script, another list is created. The names should be
# a subset of the names of tagging.schema; and the values correspond to specific
# parameters used in this script. E.g.
#
# values.schema <- list(markup = "variant1",
#                       analysis = "without-subj15")
#
# Then these two lists would be combined in a filename like this:
# projectname_mk.variant1_an_without-subj15
#
# The delimeters between the naming components and between the tag and the value
# are customizable. Obviously, the values and tags should not contain characters
# used as delimiters. E.g., if your tags and values are delimited by ".", you
# cannot set a tag like "mk.prep".


check_schema_well_formedness <- function(schema){
  #' A basic validation function for a schema. Checks whether the schema
  #' is a list, whether there is jsut a single non-verctor item assigned to each
  #' element of the list, and whether all list elements are of type 'character'.
  #' @param schema Schema to be checked
  #' @return Nothing. Throws an error if the naming schema does not satisfy
  #'        the basic conditions listed above

  # check that the naming schema is a list
  if (!is.list(schema)){
    stop("A naming components schema should be a list!")
  }

  # check that every naming schema component is character
  if (!all(sapply(schema, is.character))) {
    stop(paste0("Cannot use the naming components schema! ",
         "Some of the components are not of type `character`"))
  }

  # check that all components have only one value
  if (!all(sapply(schema, length) == 1)) {
    stop(paste0("Cannot use the naming components schema! ",
         "Some of the components have more or fewer than one corresponding tag"))
  }

}

check_for_duplicates <- function(x, x.name = "x"){
  #' A validation helper which throws an error if a vector contains duplicates
  #' @param x vector to be checked for duplicates
  #' @param x.name name for x (useful since this function is called from a wrapper,
  #'        check_schema_for_duplicates, which may want to let know which
  #'        schema has duplicates in it)
  #' @return Throws an error if the tested vector contains duplicates
  if (anyDuplicated(x)){
    stop(paste0("Duplicates found in ", x.name,
                ": ", x[duplicated(x)]))
  }
}

check_schema_for_duplicates <- function(schema, check.values.for.dups = FALSE){
  #' A validation function for schemas. Checks whether names of the list element
  #' contain duplicates, throws an error if they do. A wrapper around
  #' `check_for_duplicates`.
  #' @param schema List with a schema to be checked for duplicates
  #' @param check.values.for.dups logical. If FALSE, only names are checked
  #'        (usuful for checking values schemas - values can be duplicated);
  #'        if TRUE, names and values of the list elements are checked
  #'        (useful for checking tagging schemas - tags should be unique)

  check_for_duplicates(names(schema),
                       paste(deparse(substitute(schema)), "(schema tags)"))

  if (check.values.for.dups){
    check_for_duplicates(as.character(schema),
                         paste(deparse(substitute(schema)), "(schema values)"))
  }
}


check_for_delims <- function(x, delim, x.description, delim.description){
  #' Helper function throwing an error if a string contains characters used
  #' as delimiters in a schema.
  #' @param x character. String to be checked for delimiters
  #' @param delim character. Delimiter character
  #' @param x.description Description of the string being checked. Will be
  #'        substituted instead of X to the error message: "The following X
  #'        contains the character used as Y (see next param)"
  #' @param delim.description Descriprion of delimiter being checked. Will be
  #'        substituted instead of Y in the above error message.
  #' @return Nothing

  if  (!is.character(x)){
    stop("The first argument must be a character!")
  }

  if (!is.character(delim) | !is.character(x.description) |
      !is.character(delim.description)) {
    stop("All of delim, x.description, delim.description must be character!")
  }

  if (length(delim) > 1) {
    stop("Only a single delimiting character can be checked at a time!")
  }

  if (any(grepl(delim, x, fixed = TRUE))){
      stop(paste0("The following items in ", x.description, " contain(s) the character used as ",
                delim.description, " ('", delim , "'): ",
                paste (x[grepl(delim, x, fixed = TRUE)], collapse = ", ")))
  }
}

check_schema_for_delims <- function(schema,
                                    tag.delim,
                                    component.delim){
  #' A validation function for a schema checking that names and values of list elements
  #' do not contain characters which will be used as delimiters in the file name
  #' @param schema list. List with a schema to be checked
  #' @param tag.delim character. The character which will be used as a delimiter
  #'        between component tags and values
  #' @param component.delim character. The character which will be used as a delimiter
  #'        between components tags
  #' @return Nothing. Throws an error if the naming schema tags or values contain
  #'        delimiter characters
  #'

  nms <- names(schema)
  values <- sapply(schema, function(x){x[[1]]})

  check_for_delims(nms, tag.delim,
                   "schema names", "tag delimiter")

  check_for_delims(nms, component.delim,
                   "schema names", "component delimiter")

  check_for_delims(values, tag.delim,
                   "schema values", "tag delimiter")

  check_for_delims(values, component.delim,
                   "schema values", "component delimiter")
}

validate_schema <- function(schema,
                            tag.delim,
                            component.delim,
                            check.values.for.dups = FALSE){
  #' General validation function for a naming schema. Just a wrapper,
  #' gathering other validation function in a single place
  #' @param schema  list. List with a schema to be checked
  #' @param tag.delim character. The character which will be used as a delimiter
  #'        between component tags and values
  #' @param component.delim character. The character which will be used as a delimiter
  #'        between components tags
  #' @param check.values.for.dups logical. If TRUE, only names are checked
  #'        (usuful for checking values schemas - values can be duplicater);
  #'        if FALSE, names and values of the list elements are checked
  #'        (useful for checking tagging schemas - tags should be unique)
  #' @return Nothing. Throws an error if the naming schema has some problems


  check_schema_well_formedness(schema)
  check_schema_for_duplicates(schema, check.values.for.dups)
  check_schema_for_delims(schema, tag.delim, component.delim)

}

validate_proj_name <- function(proj.name,
                               tag.delim,
                               component.delim){
  #' A validation function for project name checking that it
  #' does not contain characters which will be used as delimiters
  #' in the file name
  #' @param proj.name project name to be validated
  #' @param tag.delim character. The character which will be used as a delimiter
  #'        between component tags and values
  #' @param component.delim character. The character which will be used as a delimiter
  #'        between components tags
  #' @return Nothing. Throws an error if the naming project name contains
  #'        delimiter characters

  check_for_delims(proj.name, component.delim,
                   "project name", "component delimiter")

  check_for_delims(proj.name, tag.delim,
                   "project name", "tag delimiter")
}

make_filename_postfix <- function(tags, values,
                                   tag.delim = ".",
                                   component.delim = "_"){
  #' Function assembling tags and values into a single string, separated with specified
  #' delimiters. The string will be used a postfix, attached to the project name.
  #' @param tags list. List with a "tagging schema": list elements contain tags for
  #'        specific components of the name. E.g. list(markup = "mk",
  #'        analysis = "an").
  #' @param values list. List with a "values schema": lsit elements contain
  #'        values for the components of the name associated with a specific
  #'        analysis. E.g. list(markup = "like-parker", analysis = "variant4")
  #' @param tag.delim character. The character which will be used as a delimiter
  #'        between component tags and values
  #' @param component.delim character. The character which will be used as a delimiter
  #'        between components tags
  #' @return string. Filename postfix, starting with the naming component delimiter.
  #'         E.g. _mk.parker-like_an.variant4

  validate_schema(tags, tag.delim, component.delim, check.values.for.dups = TRUE)
  validate_schema(values, tag.delim, component.delim, check.values.for.dups = FALSE)

  output <- ""

  for (name.component in names(values)){

    if (!name.component %in% names(tags)){
      stop(paste0("No tag found for name component `", name.component, "` !"))
    }

    if (!is.na(values[[name.component]])){
      output <- paste0(output, component.delim, tags[[name.component]], tag.delim,
                       values[[name.component]])
    }
    # else {
    #   warning(paste0("Value for name component `", name.component, "` is NA! ",
    #                  "Ignoring it"))
    # }
  }

  return(output)

}

make_filename <- function(proj.name,
                           tags, values,
                           tag.delim = ".",
                           component.delim = "_"){

  #' A function to create a filename based on tagging and value schemas.
  #'
  #' @param proj.name character. Project name; the filename will start with it
  #' @param tags list. List with a "tagging schema": list elements contain tags for
  #'        specific components of the name. E.g. list(markup = "mk",
  #'        analysis = "an").
  #' @param values list. List with a "values schema": lsit elements contain
  #'        values for the components of the name associated with a specific
  #'        analysis. E.g. list(markup = "like-parker", analysis = "variant4")
  #' @param tag.delim character. The character which will be used as a delimiter
  #'        between component tags and values
  #' @param component.delim character. The character which will be used as a delimiter
  #'        between components tags
  #' @return string. Filename, starting with project name
  #'         E.g. project1_mk.parker-like_an.variant4
  #' @export

  filename_postfix <- make_filename_postfix(tags, values, tag.delim, component.delim)
  validate_proj_name(proj.name, tag.delim, component.delim)

  return(paste0(proj.name, filename_postfix))
}

save_named_data_file <- function(..., proj.name,
                                 tags, values,
                                 tag.delim = ".",  component.delim = "_",
                                 output.dir = "."){

  #' A function to save .Rdata files named according to a naming schema.
  #' @param ... the names of the objects to be saved (as symbols or character strings).
  #'        (as in base::save)
  #' @param proj.name character. Project name. Will be used as the first substring
  #'        in the file name
  #' @param tags list. List with a "tagging schema": list elements contain tags for
  #'        specific components of the name. E.g. list(markup = "mk",
  #'        analysis = "an").
  #' @param values list. List with a "values schema": lsit elements contain
  #'        values for the components of the name associated with a specific
  #'        analysis. E.g. list(markup = "like-parker", analysis = "variant4")
  #' @param tag.delim character. Delimiter between tags and values.
  #' @param component.delim character. Delimiter between filename components
  #' @param output.dir character path to the output folder
  #' @return Full path to the created file
  #' @export

  file_name <- make_filename(proj.name,
                             tags, values,
                             tag.delim, component.delim)
  save(..., file = file.path(output.dir,
                             paste0(file_name, ".RData")))
  return(file.path(output.dir,
                   paste0(file_name, ".RData")))

}

save_named_table <- function(x, proj.name,
                             tags, values,
                             tag.delim = ".",  component.delim = "_",
                             output.dir = dirs$data.csv,
                             type = "csv",
                             file.extension = ".txt", ...){

  #' A function to save tabular files named according to a naming schema.
  #' @param x the names of the object to be saved (as symbols or character strings).
  #'        (as in base::save)
  #' @param proj.name character. Project name. Will be used as the first substring
  #'        in the file name
  #' @param tags list. List with a "tagging schema": list elements contain tags for
  #'        specific components of the name. E.g. list(markup = "mk",
  #'        analysis = "an").
  #' @param values list. List with a "values schema": lsit elements contain
  #'        values for the components of the name associated with a specific
  #'        analysis. E.g. list(markup = "like-parker", analysis = "variant4")
  #' @param tag.delim character. Delimiter between tags and values.
  #' @param component.delim character. Delimiter between filename components
  #' @param output.dir character
  #' @param type one of "table", "csv", "csv2" - depending on that, the function
  #'        called differs: utils::write.table, utils::write.csv or utils::write.csv2
  #' @param file.extension character. Extenstion of the output file. Only used
  #'        if type = "table", otherwise it is set to ".csv"
  #' @param ... other params to be passed to write.table function
  #' @return Full path to the created file
  #' @export

  file_name <- make_filename(proj.name,
                             tags, values,
                             tag.delim, component.delim)

  file_name_and_path <- file.path(output.dir, file_name)

  function.name <- paste0("write.", type)

  switch(type,
         table = utils::write.table(x = x, file = paste0(file_name_and_path, file.extension), ...),
         csv = utils::write.csv(x = x, file = paste0(file_name_and_path, ".csv"), ...),
         csv2 = utils::write.csv2(x = x, file = paste0(file_name_and_path, ".csv"), ...))

  return(paste0(file_name_and_path, file.extension))
}

