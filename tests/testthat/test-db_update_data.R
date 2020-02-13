test_db_update_data <- function(data = df_local,
                                update = c("id", "chr"),
                                where = c("name", "id" = "id2"),
                                returning = NULL) {
  db_update_data(
    data = data,
    table = test_table,
    con = con,
    update = update,
    where = where,
    returning = returning
  )
}

update <- list("value1", value2 = "value_2", value3 = sql("now()"))
where <- list("id1", id2 = "id_2", sql("id2 > 1"))
returning <- list("id1", value = "value1", time = sql("now()"))

test_that("update, where and returning work", {
  skip("not yet implemented")
  prepare_table()
  expect_equal(test_db_update_data(update = def_update), 2)

  expect_equal(
    get_tbl(),
    df[1:2, ] %>%
      dplyr::transmute(
        name,
        id = id2,
        chr = chr_upper
      )
  )
})


test_that("from with dataframe works", {
  skip("not yet implemented")
})
