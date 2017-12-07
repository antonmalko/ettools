normalize_axes_labels <- function(s, revert = FALSE){
  #' A function to make nice axes lables for ggplot.
  #'
  #' The function will insert new lines symbol \code{(\\n)} replacing "_", ".",
  #' and before "(", so that long labels can be displayed on multiple lines
  #'
  #' @param s vector of labels to be normalized
  #' @param revert if FALSE, the symbols _,.,( are replaced with new lines. If TRUE,
  #' the operation is reverted; see "Details"
  #'
  #' @details
  #'
  #' Literally undoing the normalization is obviously not possible, since there is
  #' no way of knowing which symbols were replaced with nelines. Thus, the newlines
  #' not adjacent to "(" are replaced with "_"; the newlines preceding "(" are just
  #' deleted.
  #'
  #' @return vector of normalized labels if \code{revert = FALSE}; vector of labels
  #' without newlines if \code{revert = TRUE}
  #'
  #' @export
  #' @examples
  #' labels <- c("critical_region", "spillover.region", "region(another)")
  #' normalize_axes_labels(labels)
  #' # [1] "critical\nregion"  "spillover\nregion" "region\n(another)"


  if (!(is.character(s) | is.factor(s))){
    stop ("s must be a character or a factor!")
  }

  if (!revert){
    if (is.character(s)) {
      s <- gsub("_|\\.", "\n",s)
      s <- gsub("\\(", "\n(", s)
    }

    if (is.factor(s)){
      levels(s) <- gsub("_|\\.", "\n", levels(s))
      levels(s) <- gsub("\\(", "\n(", levels(s))
    }

    return(s)
  } else { # if we want to roll back such changes
    if (is.character(s)){
      s <- gsub("\n", "_", s) # just replace with underscore, since
      # we don't know what was there originally
      s <- gsub("\n\\(", "(", s)
    }
    if (is.factor(s)){
      levels(s) <- gsub("\n", "_", levels(s))
      levels(s) <- gsub("\n\\(", "(", levels(s))
    }
    return(s)
  }
}

name_regions <- function(dat, reg.names,
                         region.col = region, reg.names.col = reg.name,
                         type.col,
                         missing.prefix = "Reg_"){

  #' Assign meaningful names to regions in eye-tracking data
  #'
  #' The function adds a column to the data.frame with region names. It can
  #' assign different region names to the same regions identifiers,
  #' if this differs for subsets of stimuli. E.g. if you have two type of stimuli,
  #' and for one of them critical region is the fourth one, and for the other -
  #' the fifth one, the function will be able to take this into account.
  #'
  #' @param dat data.frame to add region names to
  #' @param reg.names either a vector of region names or a list with vectors of
  #'        regions names. See "Details" on the exact format and difference
  #'        between the two scenarios
  #' @param region.col unquoted name of the column with original regions identifiers,
  #'        which have to be assigned new names
  #' @param reg.names.col unquoted name of the coumn with region names which will
  #'        be added to the data; "reg.name" by default
  #' @param type.col unquoted name of the column specifying stimuli types, if
  #'        it is necessary to assign different names to the same region identifiers
  #'        depending on the stimuli subset. See "Details".
  #' @param missing.prefix  what to prefix to region identifiers for which the user didn't
  #'        specify names explicitly. Defaults to "Reg_", i.e. if region identifiers
  #'        are numbers, it would generate something like "Reg_1".
  #'
  #' @return data.frame with the added \code{reg.name} column
  #'
  #' @details
  #'
  #' The function has two scenarios of use. The first one: you only have one type of
  #' stimuli in the data, or, alternatively, you have several types of stimuli,
  #' but for each type the regions are identical. E.g. for all types of stimuli
  #' the fourth region is the critical one, and the fifth one is the spillover.
  #' In this case, \code{reg.names} argument should be a named vector, where names represent
  #' new names you want to assign to the regions, and values - region identifiers.
  #' E.g. \code{reg.names <- c(critical = 4, spillover = 5)}. \code{type.col} should be
  #' left out.
  #'
  #' The second scenario: you have several types of stimuli, and regions identifiers
  #' are not shared between types. E.g. for Type 1 critical region is the fourth
  #' one, and for Type 2 critical region is the third one. In this case,
  #' \code{reg.names} should be a list, where the name of each list component
  #' corresponds to one of the stimuli types, and the list components themselves
  #' are named vector like in the first scenario. E.g:
  #'
  #' \code{reg.names <- list(Type1 = c(critical = 4, spillover = 5),
  #'                         Type2 = c(critical = 3, spillover = 4))}
  #'
  #'  In this case, the \code{type.col} should be the unquoted name of the column
  #'  containing the names of stimuli types; these names should be the same as
  #'  in the list above.
  #'
  #'  In either case, you can specify only some region names. In that case, the
  #'  regions for which you didn't specify a name, will be given a name like
  #'  "Reg_REGION.IDENTIFIER", e.g. "Reg_a" or "Reg_1".
  #'
  #' @export


  if (missing(type.col) & is.list(reg.names)){
    stop("Reg.name should only be a list if type.col argument is specified!")
  }

  if (!missing(type.col) & !is.list(reg.names)){
    stop("Reg.name should be a list if type.col argument is specified!")
  }

  type.col <- rlang::enquo(type.col)
  region.col <- rlang::enquo(region.col)
  reg.names.col <- rlang::enquo(reg.names.col)


  if (is.list(reg.names)){

    stim_types_in_data <- unique(dat[[rlang::quo_name(type.col)]])

    if (!all(names(reg.names) %in% stim_types_in_data)){
      warning(paste0("Some stimuli types specified in reg.names argument are missing ",
                     "from the data: ", names(reg.names)[!names(reg.names) %in% stim_types_in_data]))
    }

    res <-  dat %>%
      dplyr::group_by(rlang::UQ(type.col)) %>%
      dplyr::do(., name_regions_in_subset(., reg.names = reg.names[[unique(.data$type)]],
                                   region.col = rlang::UQ(region.col),
                                   reg.names.col = rlang::UQ(reg.names.col),
                                   missing.prefix = missing.prefix)) %>%
      dplyr::ungroup()


    report <- res %>%
      dplyr::group_by(rlang::UQ(type.col), rlang::UQ(region.col), reg.name) %>%
      dplyr::summarize() %>%
      dplyr::ungroup()

    message("The following region names were assigned:\n")
    print(report, n = nrow(report))
    cat("\n\n")
    return(res)


  } else {
    res <-  name_regions_in_subset(dat, reg.names = reg.names,
                                   region.col = rlang::UQ(region.col),
                                   reg.names.col = rlang::UQ(reg.names.col),
                                   missing.prefix = missing.prefix)

    report <- res %>%
      dplyr::group_by(rlang::UQ(region.col), reg.name) %>%
      dplyr::summarize() %>%
      dplyr::ungroup()

    message("The following region names were assigned:\n")
    print(report, n = nrow(report))
    cat("\n\n")
    return(res)
  }



}

name_regions_in_subset <- function(dat, reg.names,
                                   region.col, reg.names.col = reg.name,
                                   missing.prefix = "Reg_"){

  #' Assign meaningful region names to a subset of data with a single stimuli type
  #'
  #' A helper function to assign region names to a single type of stimuli.
  #' If data only has one stimuli, this function is called directly to assign
  #' region names; otherwise, it is called repeatedly for each subset of data
  #' corresponding to a stimuli type.
  #'
  #' @param dat data.frame to add region names to
  #' @param reg.names named character vector, where names correspond to the new
  #'        region names to be assigned, and the values correspond to current
  #'        region identifiers
  #' @param region.col unquoted name of the column containing current region
  #'        identifiers
  #' @param reg.names.col unquoted name of the coumn with region names which will
  #'        be added to the data; "reg.name" by default
  #' @param missing.prefix  what to prefix to region identifiers for which the user didn't
  #'        specify names explicitly. Defaults to "Reg_", i.e. if region identifiers
  #'        are numbers, it would generate something like "Reg_1".
  #'
  #' @return data.frame with the added \code{reg.name} column

  region.col <- rlang::enquo(region.col)
  reg.names.col <- rlang::enquo(reg.names.col)

  reg.names <- inflate_region_names(reg.names = reg.names,
                                     regions = dat[[rlang::quo_name(region.col)]],
                                     missing.prefix = missing.prefix)

  dat[rlang::quo_name(reg.names.col)] <- names(reg.names[dat[[rlang::quo_name(region.col)]]])

  return(dat)

}


inflate_region_names <- function(reg.names, regions,
                                  missing.prefix = "Reg_"){

  #' Make sure that all regions get a name
  #'
  #' The function ensures that if the user didn't specify some region names
  #' explicitly, some kind of label would still be generated.
  #'
  #' @param reg.names named character vector, where names correspond to the new
  #'        region names to be assigned, and the values correspond to current
  #'        region identifiers
  #' @param regions vector of unique region identifiers in the data
  #' @param missing.prefix  what to prefix to region identifiers for which the user didn't
  #'        specify names explicitly. Defaults to "Reg_", i.e. if region identifiers
  #'        are numbers, it would generate something like "Reg_1".
  #'
  #' @return named character vector analogous to \code{reg.names}, but with added
  #'         names for regions for which the user didn't specify names

  if (!all(reg.names %in% regions)){
    warning(paste0("Some of the specified regions names correspond to regions ",
                   "which are absent from the data: ",
                   paste(names(reg.names[!reg.names %in% regions]),
                         reg.names[!reg.names %in% regions], collapse = ", ")))
  }

  dummy.regions <- unique(regions)
  names(dummy.regions) <- paste0(missing.prefix, dummy.regions)
  names(dummy.regions)[dummy.regions %in% reg.names] <- names(reg.names)
  return(dummy.regions)
}

get_subj_info <- function(dat){
  #' A function to extract basic info about subjects from an eye-tracking data
  #' returned by EyePy scripts.
  #' @param dat data frame containing eytracking data
  #' @return list with two components: `n.subj` - number of subjects in the data;
  #'  `subj.missing` - a vector with IDs of missing subjects; NA if no subjects
  #'  are missing. The missing subjects are simply numbers missing from a continuous
  #'  vector of numbers from 1 to highest subject ID. E.g. if we have subjects
  #'  1,2,3,5, the subject 4 will be reported missing.
  #' @export

  if (!is.data.frame(dat)){
    stop("`data` argument must be of type data.frame!")
  }

  if (!"subj" %in% colnames(dat)){
    stop("The data doesn't seem to contain `subj` column with subj IDs!")
  }

  if (!is.numeric(dat$subj)){
    stop("The subjects IDs in `subj` column should be numeric!")
  }

  subj <- unique(dat$subj)[order(unique(dat$subj))]
  # N of subjects
  n.subj <- length(subj)
  # are there any subjects which are missing?
  subj.max <- 1:max(subj) # highest subject index, e.g. 4 for [1,2,4]
  subj.missing <- subj.max[which(!subj.max %in% subj)] # i.e. it would be 3, if subj = [1,2,4]
  if (length(subj.missing) == 0) subj.missing <- NA

  return(list(n.subj = n.subj,
              subj.missing = subj.missing))
}

get_quest_acc <- function(dat, subj.col = subj, correct.col = is.correct){

  #' Function to summarize question response accuracy
  #' @param dat data containing subject identifiers and information about
  #'  question answering accuracy. The data has to contain just one region
  #'  and one eye-tracking measure in case of eye-tracking data.
  #' @param subj.col unquoted name of the column containing subject identifiers
  #' @param correct.col unquoted name of the column containing indicator of whether
  #'   the question was answered correctly
  #' @return a dataframe with 4 columns: subject number, total number of questions
  #' (will be different if you removed some trials), number of correctly answered
  #' questions, proportion of correct answers.
  #' @export

  subj.col <- rlang::enquo(subj.col)
  correct.col <- rlang::enquo(correct.col)

  res <- dat %>%
    dplyr::group_by(rlang::UQ(subj.col)) %>%
    dplyr::summarize(n.quest = length(which(!is.na(rlang::UQ(correct.col)))),
               n.correct = sum(rlang::UQ(correct.col), na.rm = TRUE),
               prop.correct = mean(rlang::UQ(correct.col), na.rm = TRUE))
  return(res)
}

preserve_env <- function(env){
  #' Copy objects from one environment to another, creating a copy of an environment
  #'
  #' @param env source environment
  #' @return copy of the source environment
  #' @export

  as.environment(as.list(env, all.names=TRUE))
}

restore_env <- function(source.env, target.env){
  #' Assign all objects from one environment to another.
  #'
  #' WARNING: objects in the target environments which have the same names as objects
  #' in the source environments will be overwritten!
  #' @param source.env source environment
  #' @param target.env target environment
  #' @return Nothing, function is called for its side effects

  source.env <- as.list(source.env, all.names = TRUE)

  for (obj_name in names(source.env)){
    assign(x = obj_name, value = source.env[[obj_name]], envir = target.env)
  }

}

preserve_global_env <- function(){
  #' Copy all objects in the global environment
  #'
  #' Convenience wrapper around \code{\link{preserve_env}}
  #' @return copy of the global environment
  #' @export
  preserve_env(env = globalenv())
}

restore_global_env <- function(source.env){
  #' Restore contents of global environment from a saved state
  #'
  #' Convenience wrapper around \code{\link{restore_env}}.
  #' **WARNING**: objects in the target environments which have the same names as objects
  #' in the source environments will be overwritten!
  #' @param source.env preserved copy of global environment from which it is to be restored
  #' @return Nothing, function is called for its side effects
  #' @export

  restore_env(source.env = source.env, target.env = globalenv())
}

get_today <- function(format = "%d%b%y"){
  #' A function which returns today's date in the requested format.
  #' @param format. Format to use on today's date (see \code{\link[base]{strptime}}).
  #'       Default format: 02Apr16
  #' @return today's date as character
  #' @export
  return(format(Sys.Date(), format = format))
}

