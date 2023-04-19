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
    time_window = 10){


# Coordinated Behavior ---------------------------------------------------------------------------------------

# user_IDs:
# Create a set of user_IDs of length n_users_coord
random_numbers <- sample(10000:999999999, n_users_coord, replace = FALSE)
user_IDs <- paste0("user_", random_numbers)


# Share IDs:
# Function to generate random IDs for shares
generate_random_strings <- function(n = 1) {
    a <- do.call(paste0, replicate(15, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

# object_IDs:
# Create a set of object_IDs of size n_objects
object_IDs <- unique(stringi::stri_rand_strings(n_objects, length = sample(c(5:10), n_objects, replace = TRUE),
                                      pattern = "[A-Za-z]"))

# timestamps:
# Create a set of timestamps of length 1 week
start_date <- as.Date(sample(as.Date("2000-01-01"):as.Date("2020-12-31"), 1), origin = "1970-01-01")
end_date <- start_date + 7
timestamps <- seq.POSIXt(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by = "1 sec")


# symmetric matrix of size n = n_users_coord and names in user_IDs
# pupulated with random integers with minimum value min_repetition representing the coordinated shares
coord_matrix <- matrix(0, nrow = n_users_coord, ncol = n_users_coord,
                       dimnames = list(user_IDs, user_IDs))

coord_matrix[upper.tri(coord_matrix)] <- sample(min_repetition:(min_repetition*2),
                                                size = sum(upper.tri(coord_matrix)), replace = TRUE)

# to data.frame
coord_matrix[lower.tri(coord_matrix)] <- NA
diag(coord_matrix) <- NA
df <- na.omit(data.frame(as.table(coord_matrix)))
df <- df[rep(seq_len(nrow(df)), df$Freq), ]
df$Freq <- NULL
rownames(df) <- NULL
colnames(df) <- c("user_X", "user_Y")

# add objects ID
df$object_ID <- sample(object_IDs, size = nrow(df), replace = TRUE)

# add timestamps
# extract random timestamps from the timestamps set to users' A shares
# then assign to users' B shares a random timestamp lower than time_window
timestamps <- as.numeric(timestamps)
df$share_time_A <- sample(timestamps, nrow(df))
noise <- time_window - sample.int(time_window, nrow(df), replace = TRUE)
df$share_time_B <- df$share_time_A + noise

df$delta <- abs(df$share_time_A-df$share_time_B)

# add shares IDs
set.seed(123)
df$share_id_A <- generate_random_strings(n = nrow(df))
set.seed(456)
df$share_id_B <- generate_random_strings(n = nrow(df))

# mark as coordinated
df$coordinated <- as.logical("TRUE")
df_coord <- df


rm(list = setdiff(ls(), c("df_coord",
                          "generate_random_strings",
                          "object_IDs",
                          "timestamps",
                          "n_users_coord",
                          "n_users_noncoord",
                          "n_objects",
                          "min_repetition",
                          "time_window")))


# Non-Coordinated Behavior  ----------------------------------------------------------------------------------
# same as above with slightly differences

random_numbers <- sample(10000:999999999, n_users_noncoord, replace = FALSE)
user_IDs <- paste0("user_", random_numbers)

# Use the same object_IDss  as above
# Use the same range_time set as before

# matrix
noncoord_matrix <- matrix(0, nrow = n_users_noncoord, ncol = n_users_noncoord,
                       dimnames = list(user_IDs, user_IDs))

noncoord_matrix[upper.tri(noncoord_matrix)] <- sample(1:10,
                                                      size = sum(upper.tri(noncoord_matrix)), replace = TRUE)

# to data.frame
noncoord_matrix[lower.tri(noncoord_matrix)] <- NA
diag(noncoord_matrix) <- NA
df <- na.omit(data.frame(as.table(noncoord_matrix)))
df <- df[rep(seq_len(nrow(df)), df$Freq), ]
df$Freq <- NULL
rownames(df) <- NULL
colnames(df) <- c("user_X", "user_Y")

df$object_ID <- sample(object_IDs, size = nrow(df), replace = TRUE)


# timestamps (exceeding time_window)
df$share_time_A <- sample(timestamps, nrow(df))

noise <- time_window + sample.int(60*60*24, nrow(df), replace = TRUE)
df$share_time_B <- df$share_time_A + noise

df$delta <- abs(df$share_time_A-df$share_time_B)

# shares ID
set.seed(789)
df$share_id_A <- generate_random_strings(n = nrow(df))
set.seed(101112)
df$share_id_B <- generate_random_strings(n = nrow(df))

df$coordinated <- as.logical("FALSE")

rm(list = setdiff(ls(), c("df_coord", "df")))


# Merge Coordinated and Non-Coordinated datasets  ------------------------------------------------------------
output_table <- rbind(df_coord, df)

# subset and rename accordingly to the detect_coordinated_groups output
output_table <- output_table[,c("object_ID", "share_id_A", "share_id_B", "delta", "user_X", "user_Y", "coordinated", "share_time_A", "share_time_B")]
colnames(output_table) <- c("object_id", "content_id", "content_id_y", "time_delta", "id_user", "id_user_y", "coordinated", "share_time_A", "share_time_B")

rm(list = setdiff(ls(), "output_table"))


# Input data  ------------------------------------------------------------------------------------------------
# (Created by reshaping the output_table)
input_dataset <-
    unique(data.frame(object_id = c(output_table$object_id, output_table$object_id),
                                   id_user = c(output_table$id_user, output_table$id_user_y),
                                   content_id = c(output_table$content_id, output_table$content_id_y),
                                   timestamp_share = c(output_table$share_time_A, output_table$share_time_B),
                             stringsAsFactors = FALSE))

# drop unecessary columns share_time_A/share_time_B
output_table <- output_table[, -c(8:9)]

output_list <- list(input_dataset, output_table)

rm(list = setdiff(ls(), "output_list"))
gc()

return(output_list)
}
