test_db_update_data <- function(data,
                                expected_returned,
                                expected_state,
                                update = c("value1", "value2"),
                                where = c("id1", "id2" = "id_2"),
                                returning = NULL) {
  test_db_f(
    f = db_update_data,
    data = data,
    expected_returned = expected_returned,
    expected_state = expected_state,
    update = update,
    where = where,
    returning = returning
  )
}

test_that("db_update_data works", {
  skip_if(is_sqlite(con_memdb()))

  prepare_table()
  state_new <- df
  state_new$value1[1:3] <- state_new$value1[1:3] + 10
  input <- state_new
  colnames(input)[2] <- "id_2"

  test_db_update_data(
    input,
    expected_returned = 4,
    expected_state = state_new
  )

  prepare_table()
  test_db_update_data(
    input,
    expected_returned = input,
    expected_state = state_new,
    returning = sql("*")
  )
})
