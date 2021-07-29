addFlagAndStatus = function(Sample, status_value){
  # get index of sample
  sample_index = which(star_df[,"Sample"] == Sample)
  # update the status column
  sample_df[sample_index, 'status'] =
    ifelse(is.na(sample_df[[sample_index, 'status']]), status_value,
           sample_df[[sample_index, 'status']] + status_value)
  # update statusDecomp
  sample_df[sample_index, "statusDecomp"] =
    statusDecomp(sample_df[[sample_index, 'status']])
}

#' @description Decompose a value in the column of the output of quality_assess_2 to bit.
#'              eg 18 = 2 + 16 or the powers of 2 1.0 and 4.0. See templates/qc_config.yaml
#' @param status an integer that represents the sum of powers of 2
#' @return a string of powers of 2 representing the bit, eg 1,4
#' @cite yiming kang \url{https://github.com/yiming-kang/rnaseq_pipe/blob/master/tools/utils.py}
decomposeStatus2Bit = function(status){

  #TODO can use negative numbers for "passing reasons"!!

  status_decomp = list()

  if(is.na(status)){
    status_decomp = "status_NA"
  } else if(status == 0){
    status_decomp = "passing_sample"
  } else if(status > 0){
    for(i in seq(floor(log2(status)),0)){
      if ((status -2**i) >= 0){
        status_decomp = append(status_decomp, i)
        status = status - 2**i
      }
    }
    status_decomp = paste(sort(unlist(status_decomp)), collapse=",")
  }
  status_decomp
}


parseComparatives = function(value1, comparative, value2){
  switch (comparative,
          ">"  = value1 > value2,
          ">=" = value1 >= value2,
          "<"  = value1 < value2,
          "<=" = value1 <= value2,
          stop("comparative not recognized")
  )
}


#' calculate lower/upper fence as a default threshold on qc metrics
#'
#' @description calculate lower/upper inner and outer fences, and number of NAs
#'              in metric vector. inner fence defined as Q1/Q3 -/+ 1.5*IQR,
#'              outer fence is the same but 3*IQR
#'
#' @param metric_vector a vector of values on which to calculate lower/upper fence
#' @return a list with the following slots: message, which stores a message
#'         regarding the NA count, lower_inner, lower_outer, and upper_inner,
#'         upper_outer which both store the fence values
outlierFence = function(metric_vector){

  metric_quantiles = quantile(metric_vector, c(.25, .75))

  metric_iqr = IQR(metric_vector, na.rm = TRUE)
  metric_iqr_inner_fence = 1.5*metric_iqr
  metric_iqr_outer_fence = 3*metric_iqr

  na_msg = paste0("Note: there are ",as.character(sum(is.na(metric_vector))),
                  " NAs in the argument vector.")

  list(message     = na_msg,
       lower_inner = metric_quantiles[[1]] - metric_iqr_inner_fence,
       lower_outer = metric_quantiles[[1]] - metric_iqr_outer_fence,
       upper_inner = metric_quantiles[[2]] + metric_iqr_inner_fence,
       upper_outer = metric_quantiles[[2]] + metric_iqr_outer_fence
  )

}

# source: https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R
# copied from
# https://gist.github.com/dgrtwo/eb7750e74997891d7c20

# somewhat hackish solution to:
# https://twitter.com/EamonCaddigan/status/646759751242620928
# based mostly on copy/pasting from ggplot2 geom_violin source:
# https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r

# TODO use this tutorial
# https://datavizpyr.com/how-to-make-violinplot-with-data-points-in-r/
# and the functions below to make half violion plots with points below

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)

            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
          },

          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))

            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))

            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])

            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },

          draw_key = draw_key_polygon,

          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),

          required_aes = c("x", "y")
  )

#' create a joined data table based on the column selectors in the UI
createMergedTable = function(ui_selected_columns, id_field,sample_df, status_df, qc_database_path){
  selected_columns =
    if(is.null(ui_selected_columns)){
      id_field
    } else{
      c(id_field, ui_selected_columns)
    }

  tables_with_selected_columns = status_df %>%
    filter(metric %in% selected_columns) %>%
    pull(source) %>%
    unique()

  tables_with_selected_columns = c(sample_df, tables_with_selected_columns)

  # subset the input table for only the selected fields from that table
  subsetDatabaseTables = function(db_table){

    relevant_fields =
      colnames(db_table)[colnames(db_table) %in% selected_columns]

    db_table %>% dplyr::select(relevant_fields)

  }

  # get relevant tables
  qc_database_connection = dbConnect(RSQLite::SQLite(), qc_database_path)
  selected_table_list = lapply(tables_with_selected_columns, function(x)
    dbReadTable(qc_database_connection, x,
                check.names = FALSE))
  dbDisconnect(qc_database_connection)

  selected_table_list = lapply(selected_table_list, subsetDatabaseTables)

  # cite: https://stackoverflow.com/a/34393416/9708266
  selected_table_list %>% reduce(left_join, by=ID_FIELD)
}
