test_db_delete_data <- function(data,
                                expected_returned,
                                expected_state,
                                where = c("id1", "id2"),
                                returning = SQL("*")) {
  test_db_f(
    f = db_delete_data,
    data = data,
    expected_returned = expected_returned,
    expected_state = expected_state,
    where = where,
    returning = returning
  )
}


prepare_table()

test_that("empty data work", {
  state_before <- get_tbl()

  test_db_delete_data(
    data = df[0, ],
    expected_returned = state_before[0, ],
    expected_state = state_before
  )
})

test_that("partial delete works", {
  state_before <- get_tbl()
  # delete first row
  state_new <- state_before
  new_row <- create_new_row(state_before)
  state_new <- rbind(state_before[1, ], new_row)

  test_db_delete_data(
    data = state_new,
    expected_returned = state_before[1, ],
    expected_state = state_before[-1, ]
  )
})

test_that("where works", {
  prepare_table(df)
  state_before <- get_tbl()
  # delete third row
  state_new <- state_before[1:3, ]
  colnames(state_new)[2] <- "id_2"

  test_db_delete_data(
    data = state_new,
    where = list("id1", id2 = "id_2", SQL("target.id1 > 1")),
    expected_returned = state_before[3, ],
    expected_state = state_before[-3, ]
  )
})
