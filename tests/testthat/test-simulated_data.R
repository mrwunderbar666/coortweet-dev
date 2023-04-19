library(data.table)

sim_test <- function(
      n_users_coord = 5,
      n_users_noncoord = 4,
      n_objects = 5,
      min_repetition = 3,
      time_window = 10) {

      sim <- simulate_data(n_users_coord = n_users_coord,
      n_users_noncoord = n_users_noncoord,
      n_objects = n_objects,
      min_repetition = min_repetition,
      time_window = time_window)


    simulated_result <- data.table(sim[[2]])
    simulated_result <- simulated_result[coordinated == TRUE]
    simulated_result <- simulated_result[, coordinated := NULL]


  result <- detect_coordinated_groups(sim[[1]], time_window = time_window, min_repetition = min_repetition)


  result_stats <- group_stats(result)
  simulated_stats <- group_stats(simulated_result)
  result_stats_users <- user_stats(result)
  simulated_stats_users <- user_stats(simulated_result)

  return(list(result_stats, simulated_stats, result_stats_users, simulated_stats_users))

      }

test_that("simulated data works", {

  # run 10 tests
  for (i in 1:10) {
    n_users_coord = sample(1:100, size = 1)
    n_users_noncoord = sample(1:100, size = 1)
    n_objects = sample(1:100, size = 1)
    min_repetition = sample(1:10, size = 1)
    time_window = sample(1:120, size = 1)

    res <- sim_test(n_users_coord = n_users_coord,
                    n_users_noncoord = n_users_noncoord,
                    n_objects = n_objects,
                    min_repetition = min_repetition,
                    time_window = time_window
                    )

    expect_equal(res[[1]], res[[2]])
    expect_equal(res[[2]], res[[3]])
  }
}
)
