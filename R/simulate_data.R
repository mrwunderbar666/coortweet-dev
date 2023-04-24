#' simulate_data
#'
#' Create a simulated input and output of detect_coordinated_groups function
#'
#' @param n_users_coord the desired number of coordinated users
#' @param n_users_noncoord the desired number of non-coordinated users
#' @param n_objects the desired number of objects
#' @param min_repetition the minimum number of repeated coordinated action to define two user as coordinated
#' @param time_window the time window of coordination
#'
#' @return a list with two data frames: a data frame with the columns required by the function detect_coordinated_groups (`object_id`, `id_user`, `content_id`, `timestamp_share`)
#' and the output table of the same detect_coordinated_groups function and columns: `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`, `time_delta`.
#'
#' @importFrom stringi stri_rand_strings
#'
#' @export
#'

simulate_data <- function(
    n_users_coord = 5,
    n_users_noncoord = 4,
    n_objects = 5,
    min_repetition = 3,
    time_window = 10) {
    # create sets of IDs and timestamps for both coordianted and non-coordinated users ---------------------------

    # user_IDs ----

    # Create a set of user_IDs of length n_users_coord + n_user_non_coord
    # and extract coord users and non coord users
    total_users <- n_users_coord + n_users_noncoord
    random_numbers <- sample(999999999, total_users, replace = FALSE)
    user_IDs <- paste0("user_", random_numbers)

    user_IDs_coord <- user_IDs[1:n_users_coord]
    user_IDs_noncoord <- user_IDs[(n_users_coord + 1):(n_users_coord + n_users_noncoord)]

    # Share IDs ----

    # Function to generate random IDs for shares
    generate_random_strings <- function(n = 1) {
        a <- do.call(paste0, replicate(15, sample(LETTERS, n, TRUE), FALSE))
        paste0(a, sprintf("%04d", sample(999999999, n, TRUE)), sample(LETTERS, n, TRUE))
    }

    # object_IDs ----

    # Create a set of object_IDs of size n_objects
    object_IDs <- unique(stringi::stri_rand_strings(n_objects,
        length = sample(c(5:10), n_objects, replace = TRUE),
        pattern = "[A-Za-z]"
    ))

    # timestamps ----

    # Create a set of timestamps of length 1 week
    start_date <- as.Date(sample(as.Date("2000-01-01"):as.Date("2020-12-31"), 1), origin = "1970-01-01")
    end_date <- start_date + 7
    timestamps <- as.numeric(seq.POSIXt(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by = "1 sec"))


    # Coordinated Behavior - Main structure ---------------------------------------------------------------------------------------

    # symmetric matrix of size n = n_users_coord and names in user_IDs_coord
    # populated with random integers with minimum value min_repetition representing the coordinated shares
    coord_matrix <- matrix(0,
        nrow = n_users_coord, ncol = n_users_coord,
        dimnames = list(user_IDs_coord, user_IDs_coord)
    )

    coord_matrix[upper.tri(coord_matrix)] <- sample(min_repetition:(min_repetition * 2),
        size = sum(upper.tri(coord_matrix)), replace = TRUE
    )

    # to data.frame
    coord_matrix[lower.tri(coord_matrix)] <- NA
    diag(coord_matrix) <- NA
    df_coord <- na.omit(data.frame(as.table(coord_matrix)))
    df_coord <- df_coord[rep(seq_len(nrow(df_coord)), df_coord$Freq), ]
    df_coord$Freq <- NULL
    rownames(df_coord) <- NULL
    colnames(df_coord) <- c("user_X", "user_Y")

    # add object ID
    df_coord$object_ID <- sample(object_IDs, size = nrow(df_coord), replace = TRUE)


    # Non-Coordinated Behavior - Main structure ----------------------------------------------------------------------------------

    # Use the same object_IDs  as above
    # Use the same range_time set as before

    # symmetric matrix of size n = n_users_noncoord and names in user_IDs_noncoord
    noncoord_matrix <- matrix(0,
        nrow = n_users_noncoord, ncol = n_users_noncoord,
        dimnames = list(user_IDs_noncoord, user_IDs_noncoord)
    )

    # HERE: could be a problem.
    # the range 1:10 is arbitrary,
    # rather have a sampling from a poisson distribution
    noncoord_matrix[upper.tri(noncoord_matrix)] <- sample(1:10,
        size = sum(upper.tri(noncoord_matrix)), replace = TRUE
    )

    # to data.frame
    noncoord_matrix[lower.tri(noncoord_matrix)] <- NA
    diag(noncoord_matrix) <- NA
    df_noncoord <- na.omit(data.frame(as.table(noncoord_matrix)))
    df_noncoord <- df_noncoord[rep(seq_len(nrow(df_noncoord)), df_noncoord$Freq), ]
    df_noncoord$Freq <- NULL
    rownames(df_noncoord) <- NULL
    colnames(df_noncoord) <- c("user_X", "user_Y")

    # add object ID
    df_noncoord$object_ID <- sample(object_IDs, size = nrow(df_noncoord), replace = TRUE)


    # Timestamps -------------------------------------------------------------------------------------------------

    # there are 604801 timestamps in the set (should be enough for most cases)

    # reserve the first third to coordinated shares
    timestamps_coord <- timestamps[1:length(timestamps) / 3]

    # the remaining ones to non-coordinated shares
    timestamps_noncoord <- setdiff(timestamps, timestamps_coord)
    # also prune the timestamps that might cause any overlapping with the coordinated timestamps and create unwanted links
    timestamps_noncoord <- timestamps_noncoord[time_window:length(timestamps_noncoord)]


    # assign coordinated timestamps:
    # extract timestamps from the timestamps_coord set using systematic sampling where the
    # sampling interval is higher than time_interval, to avoid unplanned links between users
    # assing the sampled timestamp to users' A shares,
    # then assign to users' B shares a timestamps equal to the A interval + noise lower than time_window

    sampling_interval <- time_window + 1 # the sampling interval is equal to time_window + n to avoid overlaps
    indices <- seq(from = 1, to = length(timestamps_coord), by = sampling_interval) # generate the indices
    indices <- indices[1:nrow(df_coord)]
    df_coord$share_time_A <- timestamps_coord[indices] # extract the sampled values and assign to the first set of shares

    # add share time to B adding number of seconds < time_interval
    noise <- time_window - sample.int(time_window, nrow(df_coord), replace = TRUE)
    df_coord$share_time_B <- df_coord$share_time_A + noise

    df_coord$delta <- abs(df_coord$share_time_A - df_coord$share_time_B)

    # mark as coordinated
    df_coord$coordinated <- as.logical("TRUE")

    # non-coordinated timestamps:
    # extract timestamps from coordinated timestamp set and with intervals exceeding time_window)

    sampling_interval <- time_window * 3 # the sampling interval is equal to 3*time_window
    indices <- seq(from = 1, to = length(timestamps_noncoord), by = sampling_interval) # generate the indices
    indices <- indices[1:nrow(df_noncoord)]
    df_noncoord$share_time_A <- timestamps_noncoord[indices]

    # the noise is equal to 1 + time_window + noise [0-time_window] and ensure the difference between timestamps is > time_window
    noise <- 1 + time_window + sample.int(time_window, nrow(df_noncoord), replace = TRUE)
    df_noncoord$share_time_B <- df_noncoord$share_time_A + noise

    df_noncoord$delta <- abs(df_noncoord$share_time_A - df_noncoord$share_time_B)

    # mark as non coordinated
    df_noncoord$coordinated <- as.logical("FALSE")

    output_table <- rbind(df_coord, df_noncoord)

    # shares IDs -------------------------------------------------------------------------------------------------

    total_id_needed <- nrow(output_table) * 2
    share_ids <- generate_random_strings(n = total_id_needed)

    output_table$share_id_A <- share_ids[1:(length(share_ids) / 2)]
    output_table$share_id_B <- share_ids[(1 + (length(share_ids) / 2)):length(share_ids)]

    rm(list = setdiff(ls(), "output_table"))

    # subset and rename accordingly to the detect_coordinated_groups output
    output_table <- output_table[, c("object_ID", "share_id_A", "share_id_B", "delta", "user_X", "user_Y", "coordinated", "share_time_A", "share_time_B")]
    colnames(output_table) <- c("object_id", "content_id", "content_id_y", "time_delta", "id_user", "id_user_y", "coordinated", "share_time_A", "share_time_B")


    # Input data  ------------------------------------------------------------------------------------------------
    # (Created by reshaping the output_table)
    input_dataset <-
        unique(data.frame(
            object_id = c(output_table$object_id, output_table$object_id),
            id_user = c(output_table$id_user, output_table$id_user_y),
            content_id = c(output_table$content_id, output_table$content_id_y),
            timestamp_share = c(output_table$share_time_A, output_table$share_time_B),
            stringsAsFactors = FALSE
        ))

    # drop unecessary columns share_time_A/share_time_B
    output_table <- output_table[, -c(8:9)]

    output_list <- list(input_dataset, output_table)

    rm(list = setdiff(ls(), "output_list"))
    gc()

    return(output_list)
}
