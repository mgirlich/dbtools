test_db_insert_data <- function(data,
                                expected_returned,
                                expected_state,
                                insert_cols = NULL,
                                returning = SQL("*"),
                                trans = TRUE,
                                batch_size = 50e3) {
  test_db_f(
    f = db_insert_data,
    data = data,
    expected_returned = expected_returned,
    expected_state = expected_state,
    insert_cols = insert_cols,
    returning = returning,
    trans = trans,
    batch_size = batch_size
  )
}


prepare_table()

test_that("empty data work", {
  state_before <- get_tbl()

  test_db_insert_data(
    data = df[0, ],
    expected_returned = df[0, ],
    expected_state = state_before
  )
})

test_that("new data works", {
  prepare_table(df[0, ])

  test_db_insert_data(
    data = df,
    expected_returned = df,
    expected_state = df
  )
})

test_that("insert cols work", {
  prepare_table()
  state_before <- get_tbl()

  new_row <- create_new_row(state_before, value2 = NA_character_)
  state_new <- rbind(state_before, new_row)

  test_db_insert_data(
    data = setNames(new_row, c("id1", "id2", "renamed_col", "value2")),
    expected_returned = new_row,
    expected_state = state_new,
    insert_cols = c("id1", "id2", "value1" = "renamed_col")
  )
})

test_that("trans and batch_size work", {
  prepare_table(df[0, ])

  test_db_insert_data(
    data = df[1:2, ],
    expected_returned = 2,
    expected_state = df[1:2, ],
    returning = NULL,
    trans = FALSE,
    batch_size = 1
  )
})
