test_db_delete_data <- function(data,
                                expected_returned,
                                expected_state,
                                where = c("id1", "id2"),
                                returning = sql("*")) {
  expect_equivalent(
    db_delete_data(
      data = data,
      table = test_table,
      con = con,
      where = where,
      returning = returning
    ),
    expected_returned
  )

  expect_equivalent(
    get_tbl(),
    expected_state
  )
}


create_new_row <- function(state_before, value2 = NULL) {
  new_row <- state_before[1, ]
  new_row$id1 <- max(state_before$id1) + 100
  new_row$value1 <- max(state_before$value1) + 100

  if (!is.null(value2)) {
    new_row$value2 <- value2
  }

  new_row
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
    where = list("id1", id2 = "id_2", sql("target.id1 > 1")),
    expected_returned = state_before[3, ],
    expected_state = state_before[-3, ]
  )
})
