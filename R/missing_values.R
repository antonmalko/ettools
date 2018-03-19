# ---- Helper functions ----

check_columns_existence <- function(dat, columns){
  #' Check whether specified columns exist in a data frame
  #'
  #' @param dat data.frame to check for the existence of columns
  #' @param columns a vector of column names (should be "character")
  #' @return Throws an error if some of the columns are not present in the
  #' data.frame. The error text indicates which columns are missing

  if (!all(columns %in% colnames(dat))){
    stop(paste("Some columns are not found in the data: ",
               paste(columns[!columns %in% colnames(dat)])))
  }
}


check_rois_mois_existence <- function(dat, rois, mois, region.col, measure.col){

  #' Check whether eye-tracking data contains regions of interest (rois) and
  #' measures of interest (mois).
  #'
  #' @param dat data.frame to check the existence of rois and mois
  #' @param rois,mois vectors of region and measures identifiers, which will be
  #'  checked against \code{region.col} and \code{measure.col} respectively.
  #'  The type of entries in the vector should be the same as in the corresponding
  #'  columns
  #' @param region.col,measure.col name of column containing region
  #' and eye-tracking measures identifiers respectively. See "Details" for more
  #' info.
  #'
  #' @details
  #' The type of \code{region.col} and \code{measure.col} should be either
  #' "character" or "quosure" (see documentation for "quosure" in \code{rlang}
  #' package)
  #' @return Throws an error if some of the rois and mois are not present in the
  #' data.frame; the error text indicates which rois/mois are missing.
  #'

  regions.in.the.data <- dat[[rlang::quo_name(region.col)]]
  measures.in.the.data <- dat[[rlang::quo_name(measure.col)]]

  if (any(!rois %in% regions.in.the.data)){
    stop(paste0("Some rois are not found in the data: ",
                paste(rois[!rois %in% regions.in.the.data], collapse = ", ")))
  }

  if (any(!mois %in% measures.in.the.data)){
    stop(paste0("Some measures of interest are not found in the data: ",
                paste(mois[!mois %in% measures.in.the.data], collapse = ",")))
  }

}


zero_inflate_cells_count <- function(dat, by,
                                     cells.count, rois, mois){
  #' Make sure that every roi + moi combination has counts
  #'
  #' This function ensures that every combination of region and measure
  #' contains counts. Basically, it takes care fo the situation when a given
  #' region-measure doesn't contain cells of the required kind (e.g. NAs or
  #' extreme observations). In such cases this function adds 0 as count to make
  #' sure that all region-measure combination are present in the summary plot.
  #'
  #' @param dat data.frame containing information about \code{by} column
  #' @param by  quosure indicating which column we are aggregating along. Usually
  #' it is the subject or the item column.
  #' @param cells.count data.frame with cells count data produced by \code{count_cells}.
  #' @param rois,mois vectors of identifiers for regions and measures, in which
  #' cells have to be counted
  #'
  #' @return data.frame containing the original cells counts as well as added
  #' 0 counts
  #' @importFrom dplyr %>%

  by <- rlang::quo_name(by)

  # if there are no missing cells for some combination of region and measure,
  # insert 0s; otherwise that column will be completely missing

  # expand.grid varies the first column fastest - this is what we need, but the
  # order is wrong: e.g. we want spillover_ff, not, ff_spillover...
  reg.meas.combs <- expand.grid(mois, rois)
  # so we exchange columns
  reg.meas.combs <- reg.meas.combs[c(2,1)]
  # then paste them together; mdply adds a new column with the result,
  # and we only need that, so we only take the third column
  reg.meas.combs <- plyr::mdply(reg.meas.combs, paste, sep="_")[[3]]

  # create a dummy data.frame with all necessary region+measure combinations
  # filled with 0s
  dummy.df <- data.frame(tmp = unique(dat[[by]]),
                         cell = rep(reg.meas.combs,
                                    each = length(unique(dat[[by]]))),
                         count = 0,
                         stringsAsFactors = FALSE)
  colnames(dummy.df)[1] <- by

  # convert to character, otherwise bind_rows_ will throw a warning.
  # It's harmless, but we'd better avoi it not to worry the end user unduly
  cells.count$cell <- as.character(cells.count$cell)

  cells.count <- dplyr::bind_rows(dummy.df, cells.count) %>%
    dplyr::group_by_(by, "cell") %>%
    dplyr::summarize(count = sum(count)) %>%
    dplyr::ungroup()

  cells.count$cell <- factor(cells.count$cell,
                             levels = reg.meas.combs)

  return(cells.count)

}

# ----- Counting functions ----

count_cells <- function(dat, by, ..., cast.formula,
                        value.name = "count") {

  #' Count number of observations of specific kind
  #'
  #' A generic function to count number of certain observations (e.g. NAs or
  #' extreme values) by cells (e.g. combination of region and eye-tracking measure).
  #'
  #' @param dat data.frame containing data to process
  #' @param by unquoted column name for the main grouping variable. See "Details"
  #' @param ... unqouted column names - other grouping variables. See "Details".
  #' @param cast.formula character. A formula for \code{dcast()} (from 'reshape2' package).
  #'                LHS is the grouping variable, RHS defines cells
  #' @param value.name character. name of the column containing counts in the
  #' result data.frame
  #'
  #' @details
  #' The function assumes that \code{dat} argument contains the data which has
  #' already been subsetted to only contain values of interest. E.g., if you
  #' want to count NAs, before passing the data set to this function, you need
  #' to filter all non-NAs out. The package contains two specific convenience
  #' functions which would do the subseetting for you (the names are self-explanatory):
  #' \code{count_NAs} and \code{count_extremes}.
  #'
  #' The \code{by} argument would typically contain subject or item column name.
  #' this is the main grouping variable, which will be displayed on the y axis
  #' in the summary plots.
  #'
  #' The column names in \code{...} argument define which columns will act as
  #' grouping variables for \code{dplyr::group_by}, thus defining the smallest
  #' subset of the data in which the observations should be counted. In a typical
  #' use for eye-tracking data, there will be two components here: region and
  #' measure columns names. In general, the values in the first item will be
  #' varied slowest, the values in the last item will be varied fastest.
  #'
  #' Thus, e.g. (if \code{by = subj} and \code{... = region.col, measure,col}),
  #' the observations will be counted in each region, in each measure,
  #' for each subject.
  #'
  #'@return
  #'
  #' data.frame with 3 columns (default names):
  #'
  #' 1. Name of the \code{by} argument, typically subj or item; contains
  #'   the unique values from the corresponding column in \code{dat}
  #' 2. \code{cell}; contains data subset identifiers. In a typical use,
  #'   if used with regions and measures, it would be region_measure, e.g. "critical_tt"
  #' 3. \code{count} count of observations in each subset of the data.
  #'
  #'@seealso \code{\link{count_NAs}}, \code{\link{count_extremes}}
  #'
  #' @importFrom dplyr %>%
  #' @importFrom rlang !!
  #' @importFrom rlang !!!

  grouping_vars <- rlang::list2(...)
  id.var <- rlang::quo_name(by)

  if (!is.data.frame(dat)){
    stop("`dat` argument must be a data.frame")
  }

  if (!plyr::is.formula(cast.formula)){
    stop("`cast.formula` argument must be a formula")
  }

  if (!is.character(value.name)){
    stop("`value.name` should be a character")
  }

  if (!is.character(id.var)){
    stop("`id.var` should be a character")
  }

  if (nrow(dat) == 0){
    warning("Cannot count cells: the data is empty")
    res <- data.frame(a = numeric(),
                      b = character(),
                      c = numeric())
    colnames(res) <- c(id.var, "cell", value.name)
    return(res)
  }


  res <-  dat %>%
    dplyr::group_by(!!!grouping_vars, !!by) %>%
    dplyr::summarize(res =n()) %>%
    reshape2::dcast(cast.formula, value.var = "res", fill = 0) %>%
    reshape2::melt(id.vars = id.var, variable.name = "cell", value.name = value.name)

  return(res)
}


count_NAs <- function(dat, by,
                      rois, mois,
                      value.col, region.col, measure.col){

  #' Convenience wrapper for count_cells() for counting NAs.
  #'
  #' Count the number of NAs in the data briken down by region, measure and
  #' some third factor, usually subject or item.
  #'
  #' @param dat data.frame containing data to process
  #' @param by quosure. column name for the main grouping variable. See "Details"
  #' @param rois,mois vectors of identifiers for regions and measures, in which
  #' cells have to be counted. Any of these can be left unsepcified, in this
  #' case, all regions and/or measure form the data will be used.
  #' @param value.col quosure. column name containing the NAs (potentially along
  #' with non-NA values. Typically it will be the column containing the reaction
  #' times or something alike).
  #' @param region.col quosure. column name containing region identifiers
  #' @param measure.col quosure. column name containing measure identifiers
  #'
  #' @details
  #' The \code{by} argument would typically contain subject or item column name.
  #' this is the main grouping variable, which varies slowest and will be
  #' displayed on the y axis in the summary plots.
  #'
  #' @return data.frame with 3 columns analogous to \code{\link{count_cells}}
  #' (i.e. containing the main grouping variable identifier (typically subject or item),
  #'  the combination of region and measure identifying subsets of data, and NAs counts)
  #' @export
  #' @importFrom dplyr %>%

  if (missing(rois)){
    rois <- unique(dat[[rlang::quo_name(region.col)]])
  }

  if (missing(mois)){
    rois <- unique(dat[[rlang::quo_name(measure.col)]])
  }


  check_columns_existence(dat = dat,
                          columns = c(rlang::quo_name(by),
                            rlang::quo_name(value.col),
                            rlang::quo_name(region.col),
                            rlang::quo_name(measure.col)))

  check_rois_mois_existence (dat = dat, rois = rois, mois = mois,
                             region.col = region.col,
                             measure.col = measure.col)

  filtered.dat <- dat %>%
    dplyr::filter(!stats::complete.cases(rlang::UQ(value.col)))

  if (nrow(filtered.dat) == 0){
    warning(paste0("There are no NAs in column `", rlang::quo_name(value.col), "`"))
    res <- data.frame(a = numeric(),
                      cell = character(),
                      count = numeric())
    colnames(res)[1] <- rlang::quo_name(by)
  } else {
    filtered.dat <- filtered.dat%>%
      dplyr::filter(rlang::UQ(region.col) %in% rois & rlang::UQ(measure.col) %in% mois)

    res <- count_cells(filtered.dat, by = by,
                       region.col,
                       measure.col,
                       cast.formula = stats::formula(paste(rlang::quo_name(by),"~" ,rlang::quo_name(region.col),
                                                    " + ", rlang::quo_name(measure.col))))
  }

  res <- zero_inflate_cells_count(dat = dat, by = by,
                                  cells.count = res,
                                  rois = rois, mois = mois)

  return(res)
}

count_extremes <- function(dat, by,
                           rois, mois,
                           value.col, region.col, measure.col,
                           max.cutoff, min.cutoff = 0){
  #' Convenience wrapper for \code{\link{count_cells}} for counting NAs.
  #'
  #' Count the number of NAs in the data briken down by region, measure and
  #' some third factor, usually subject or item.
  #'
  #' @param dat data.frame containing data to process
  #' @param by quosure with column name for the main grouping variable. See "Details"
  #' @param rois,mois vectors of identifiers for regions and measures, in which
  #' cells have to be counted. Any of these can be left unsepcified, in this
  #' case, all regions and/or measure form the data will be used.
  #' @param value.col quosure with column name containing the NAs (potentially along
  #' with non-NA values. Typically it will be the column containing the reaction
  #' times or something alike).
  #' @param region.col quosure with column name containing region identifiers
  #' @param measure.col quosure with column name containing measure identifiers
  #' @param max.cutoff numeric value, indicating the upper threshold; any values
  #' above this threshold will be considered "extreme" and counted
  #' @param min.cutoff numeric value, indicating the lower threshold. Defaults to 0
  #'
  #' @details
  #' The \code{by} argument would typically contain subject or item column name.
  #' this is the main grouping variable, which varies slowest and will be
  #' displayed on the y axis in the summary plots.
  #'
  #' @return data.frame with 4 columns. The first 3 are analogous to
  #' \code{\link{count_cells}}
  #' (i.e. they contain the main grouping variable identifier (typically subject or item),
  #' the combination of region and measure identifying subsets of data,
  #' and extreme values counts), and the fourth one, called "direction", indicates
  #' whether the value is extremely high (then the corresponding row will contain
  #' "above.max" in the"direction" column) or extremely low ("below.min" in the
  #' "direction" column).
  #' @export
  #' @importFrom dplyr %>%

  if (missing(rois)){
    rois <- unique(dat[[rlang::quo_name(region.col)]])
  }

  if (missing(mois)){
    rois <- unique(dat[[rlang::quo_name(measure.col)]])
  }

  check_columns_existence(dat = dat,
                          columns = c(rlang::quo_name(by),
                            rlang::quo_name(value.col),
                            rlang::quo_name(region.col),
                            rlang::quo_name(measure.col)))

  check_rois_mois_existence (dat = dat, rois = rois, mois = mois,
                             region.col = region.col,
                             measure.col = measure.col)


  dat <- dat %>%
    dplyr::filter(rlang::UQ(region.col) %in% rois & rlang::UQ(measure.col) %in% mois)

  high.extremes <- dat %>%
    dplyr::filter(rlang::UQ(value.col) > max.cutoff)


  low.extremes <- dat %>%
    dplyr::filter(rlang::UQ(value.col) < min.cutoff)


  if (nrow(high.extremes) == 0){
    warning(paste0("There are no observations higher than ", max.cutoff))
    high.extremes <- data.frame(a = numeric(),
                                cell = character(),
                                count = numeric(),
                                direction = character())
    colnames(high.extremes)[1] <- rlang::quo_name(by)
  } else {
    high.extremes <- count_cells(high.extremes,
                                 by,
                                 region.col,
                                 measure.col,
                                 cast.formula = stats::formula(paste(rlang::quo_name(by),"~",
                                                              rlang::quo_name(region.col), " + ",
                                                              rlang::quo_name(measure.col))))
  }

  if (nrow(low.extremes) == 0){
    warning(paste0("There are no observations lower than ", min.cutoff))
    low.extremes <- data.frame(a = numeric(),
                               cell = character(),
                               count = numeric(), direction = character())
    colnames(low.extremes)[1] <- rlang::quo_name(by)
  } else {
    low.extremes <- count_cells(low.extremes, by,
                                region.col,
                                measure.col,
                                cast.formula = stats::formula(paste(rlang::quo_name(by),"~",
                                                             rlang::quo_name(region.col), " + ",
                                                             rlang::quo_name(measure.col))))
  }

  high.extremes <- zero_inflate_cells_count(dat = dat, by = by,
                                            cells.count = high.extremes,
                                            rois = rois, mois = mois)

  low.extremes <- zero_inflate_cells_count(dat = dat, by = by,
                                            cells.count = low.extremes,
                                             rois = rois, mois = mois)

  high.extremes$direction <- "above.max"
  low.extremes$direction <- "below.min"

  res <- rbind(high.extremes, low.extremes)

  return(res)
}

# ---- Plotting functions ----

plot_cells_heatmap <- function(dat, by, fill, limits = NULL,
                               plot.name){
  #' Plot heatmap of cell counts
  #'
  #' A generic function to plot heatmaps of cell counts. Shouldn't be used directly
  #' to plot counts produced by \code{\link{count_cells}} and related functions, since
  #' those counts may lack the data from certain subsets of the data, if those
  #' subsets lack the relevant kind of values (e.g. NAs or extreme values).
  #' Instead, \code{\link{plot_cells_count}}, or convenience wrappers around it
  #' should be called, since they take care of subsets with 0 observations
  #' by artificially adding these 0s back to the counts.
  #'
  #' @param dat data.frame returned by \code{count_NAs} or \code{count_extremes()}.
  #' It should have three columns: grouping variable (subj or item),
  #'                  name of the cell (region+fixationtype by default),
  #'                  number of observations in the cell
  #' @param by quosure. name of the grouping variable (normally, first column
  #'       in \code{dat})
  #' @param fill quosure. name of the column w/count of observations (normally,
  #'         third column in \code{dat})
  #' @param limits vector of two integers: lowest and highest N of observations
  #'            possible in the cell to calibrate color coding. If NULL (default),
  #'            the limits will be set automatically based on the data.
  #' @param plot.name character
  #' @return heatmap plot of the cell counts

  by <- rlang::quo_name(by)
  fill <- rlang::quo_name(fill)

  check_columns_existence(dat = dat,
                          columns = c(by,
                                      fill))

  # + scale_y_reverse - make the 1st subject/item appear on top;
  #   + breaks - make sure that each subject has its number shown
  out <- ggplot2::ggplot(dat, ggplot2::aes_string(x = "cell", y = by)) +
    ggplot2::geom_tile(ggplot2::aes_string(fill = fill), color = "white") +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue",
                        breaks = seq(min(limits), max(limits), by = 1)) +
    ggplot2::geom_text(ggplot2::aes_string(label = fill))+
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::ggtitle (plot.name)

  if (is.numeric(dat[[by]])){
    out <- out + ggplot2::scale_y_reverse(breaks = sort(unique(dat[[by]])))
  }

  return(out)

}



plot_cells_count <- function(dat, by,
                             cells.count, rois, mois, fill = count,
                             plot.name = "Give a sensible name to the plot!"){

  #' Plot heatmaps of cell counts
  #'
  #' A wrapper around \code{\link{plot_cells_heatmap}} which is aware of regions
  #' and measures of interest. Because of that it a) adds 0 counts to region + measure
  #' combinations which lacked the relevant kind of value (e.g. extreme or NAs);
  #' b) visually delimits ROIs in the plot for visual convenience.
  #'
  #' @param dat data.frame containing information about \code{by} column
  #' @param by quosure indicating which column we are aggregating along. Usually
  #' it is the subject or the item column.
  #' @param cells.count data.frame with cells count data produced by \code{\link{count_cells}}.
  #'        The function expects that the data.frame has column "cell", containing
  #'        the names of data subsets for which counts were performed (e.g
  #'        region+measure combination) and column "count", containing the counts
  #'        of the relevant values in each cell.
  #' @param rois,mois vectors of identifiers for regions and measures, in which
  #' cells have to be counted
  #' @param fill unquoted name of the column with cell counts
  #' @param plot.name character
  #'
  #' @return heatmap plot of the cell counts (expanded compared to the plot
  #' returned by \code{\link{plot_cells_heatmap}} - with added zero counts and ROIs
  #' delimitation)
  #' @export


  if (!rlang::is_quosure(by)) {
    stop("`by` argument should be a quosure")
  }

  check_columns_existence(dat = cells.count,
                          columns = c("cell",
                                      "count"))

  fill <- rlang::enquo(fill)

  cells.count$cell <- normalize_axes_labels(cells.count$cell)

  # determine the max number for fill scale
  fill.upper.limit <- max(cells.count$count)

  # How many regions and measures? This is used below in geom_vline() statement
  # to figure out, how close should the separating vertical lines inserted
  n.mois <- length(mois)
  n.rois <- length(rois)


  cells.plot <- plot_cells_heatmap(cells.count, by = by, fill = fill,
                                   limits = c(0, fill.upper.limit),
                                   plot.name = plot.name)

  if (n.rois > 1){
    # Insert separating lines between regions for visual convenience. Start after
    # first n + 0.5 measures (+0.5 in order to insert the line in between two columns
    # of figures), insert them every n, with an upper limit on n =
    # n_of_regions * n_of_measures.
    cells.plot <- cells.plot +
      ggplot2::geom_vline(xintercept = seq(from = n.mois+0.5, to = n.rois*n.mois, by = n.mois))
  }

  return(cells.plot)
}

# ---- User-level functions ----

report_NAs_count <- function(dat, by = subj,
                             rois, mois,
                             value.col = value,
                             region.col = reg.name, measure.col = fixationtype,
                             plot.name = "Give a sensible name to the plot!"){

  #' Count and plot NAs by regions and measures of interest
  #'
  #' @param dat data.frame containing the data where the NAs should be counted
  #' @param by unquoted column name for the main grouping variable. See "Details"
  #' @param rois,mois vectors of region and measures identifiers, for which counts
  #' should be produced. The type of entries in the vector should be the same as in
  #' \code{region.col} and \code{measure.col} respectively. Any of these can be left
  #' unsepcified, in this case, all regions and/or measure form the data will be used.
  #' @param value.col unquoted name of the column containing NAs which should
  #' be counted (typically it would be the column containing reaction times
  #' or something similar)
  #' @param region.col unquoted name of the column containing region ids
  #' @param measure.col unquoted name of the column containing measure ids
  #' @param plot.name character. name for the plot visualizing the counts
  #'
  #' @details
  #' The \code{by} argument would typically contain subject or item column name.
  #' this is the main grouping variable, which varies slowest and will be
  #' displayed on the y axis in the summary plots.
  #'
  #' @return list with the following components:
  #' * counts data.frame containing count data
  #' * cells.plot heatmap visualization of the counts
  #' @export
  #' @importFrom dplyr %>%

  value.col <- rlang::enquo(value.col)
  region.col <- rlang::enquo(region.col)
  measure.col <- rlang::enquo(measure.col)
  by <- rlang::enquo(by)

  if (missing(rois)){
    rois <- unique(dat[[rlang::quo_name(region.col)]])
  }

  if (missing(mois)){
    mois <- unique(dat[[rlang::quo_name(measure.col)]])
  }

  cells.count <- dat %>%
    dplyr::ungroup() %>%
    dplyr::do(., count_NAs(.,
                    rois = rois, mois = mois,
                    value.col = value.col, region.col = region.col,
                       measure.col = measure.col,
                       by = by))

  cells.plot <- plot_cells_count(dat = dat,
                                 cells.count = cells.count,
                                 rois = rois, mois = mois,
                  by = by, plot.name = plot.name)



  return(list(counts = cells.count,
              plot = cells.plot))
}

report_extremes_count <- function(dat, by = subj,
                                  rois, mois,
                                  value.col = value,
                                  region.col = reg.name, measure.col = fixationtype,
                                  max.cutoff, min.cutoff = 0,
                                  plot.name = "Give a sensible name to the plot!"){

  #' Count and plot extreme observation by regions and measures of interest
  #'
  #' @param dat data.frame containing information about \code{by} column
  #' @param by quosure indicating which column we are aggregating along. Usually
  #' it is the subject or the item column.
  #' @param rois,mois vectors of identifiers for regions and measures, in which
  #' cells have to be counted. The type of entries in the vector should be the same as in
  #' \code{region.col} and \code{measure.col} respectively.
  #' Any of these can be left unsepcified, in this case, all regions and/or measure
  #' form the data will be used.
  #' @param value.col unquoted name of the column containing NAs which should
  #' be counted (typically it would be the column containing reaction times
  #' or something similar)
  #' @param region.col unquoted name of the column containing region ids
  #' @param measure.col unquoted name of the column containing measure ids
  #' @param max.cutoff numeric value, indicating the upper threshold; any values
  #' above this threshold will be considered "extreme" and counted
  #' @param min.cutoff numeric value, indicating the lower threshold. Defaults to 0
  #' @param plot.name Name of the heatmap plot with extreme values counts
  #'
  #' @return list with 3 components:
  #' * \code{counts} - data.frame with counts data
  #' * \code{count.plots} - list with 3 components
  #'     * \code{high.plot} - plot with counts of extreme observations above \code{max.cutoff}
  #'     * \code{low.plot} - plot with counts of extreme observations below \code{min.cutoff}
  #'     * \code{all.plot} - plot with combined counts of both high and low extreme values
  #' * \code{values.plots} - list with 2 components
  #'     * \code{high.plot} - plot with numerical values of extreme observations above \code{max.cutoff}
  #'     * \code{low.plot} - plot with numerical values of extreme observations below \code{min.cutoff}
  #' @export
  #' @importFrom dplyr %>%


  value.col <- rlang::enquo(value.col)
  region.col <- rlang::enquo(region.col)
  measure.col <- rlang::enquo(measure.col)
  by <- rlang::enquo(by)

  if (missing(rois)){
    rois <- unique(dat[[rlang::quo_name(region.col)]])
  }

  if (missing(mois)){
    mois <- unique(dat[[rlang::quo_name(measure.col)]])
  }

  # count extreme values
  cells.count <-  dat %>%
    dplyr::do(., count_extremes(., by = by,
                         rois = rois, mois = mois,
                         value.col = value.col,
                         region.col = region.col,
                         measure.col = measure.col,
                         max.cutoff = max.cutoff,
                         min.cutoff = min.cutoff))

  count.plots <- list()
  values.plots <- list()

  # plot values above the max cutoff
  count.plots$high <- cells.count %>%
    dplyr::filter(direction == "above.max") %>%
    plot_cells_count(., dat = dat,
                     rois = rois, mois = mois,
                     by = by, plot.name = paste0(plot.name, "_above_", max.cutoff))

  # plot values below the min cutoff
  count.plots$low <- cells.count %>%
    dplyr::filter(direction == "below.min") %>%
    plot_cells_count(., dat = dat,
                     rois = rois, mois = mois,
                     by = by, plot.name = paste0(plot.name, "_below_", min.cutoff))

  # combine the two plots above in one
  count.plots$all <- cells.count %>%
    dplyr::group_by_(by, "cell") %>%
    dplyr::summarize(count = sum(count)) %>%
    dplyr::ungroup() %>%
    plot_cells_count(., dat = dat,
                     rois = rois, mois = mois,
                     by = by, plot.name = paste0(plot.name, "_above_", max.cutoff,
                                                 "_and_below_", min.cutoff))

  # plot numerical values above the max cutoff
  values.plots$high <- dat %>%
    dplyr::filter(rlang::UQ(region.col) %in% rois & rlang::UQ(measure.col) %in% mois) %>%
    dplyr::filter(rlang::UQ(value.col) > max.cutoff) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes_string(y = rlang::quo_name(value.col),
                                            x = rlang::quo_name(measure.col)))+
    ggplot2::facet_wrap(stats::formula(paste("~", rlang::quo_name(region.col))))

  # plot numerical values below the min cutoff
  values.plots$low <- dat %>%
    dplyr::filter(rlang::UQ(region.col) %in% rois & rlang::UQ(measure.col) %in% mois) %>%
    dplyr::filter(rlang::UQ(value.col) < min.cutoff) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes_string(y = rlang::quo_name(value.col),
                                            x = rlang::quo_name(measure.col)))+
    ggplot2::facet_wrap(stats::formula(paste("~", rlang::quo_name(region.col))))

  return(list(counts = cells.count,
              count.plots = count.plots,
              values.plots = values.plots))
}



