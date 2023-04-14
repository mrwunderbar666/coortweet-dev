#' group_stats
#' 
#' Calculate coordinated group statistics: 
#' total unique users per group, total posts in per group,
#' average time delta per group
#' 
#' @param x a result data.table generated by `detect_coordinated_groups`
#' @return a data.table with summary statistics for each group
#' 
#' @import data.table
#' @export 
#' 
#' 

group_stats <- function(x) {
    variable = time_delta = id_user = NULL
    x_melted <- data.table::melt(x,
        id.vars = c("object_id", "time_delta"),
        measure.vars = patterns("^content_id", "^id_user"),
        value.name = c("content_id", "id_user")
    )

    x_melted[, variable := NULL]
    x_melted <- unique(x_melted)

    x_summary <- x_melted[, .(users = length(unique(id_user)), posts = .N, mean_time_delta = mean(time_delta)), by = 'object_id']
    return(x_summary)
}