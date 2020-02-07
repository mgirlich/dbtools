batch_wise_db <- function(data,
                          con,
                          .f,
                          trans,
                          returning = NULL,
                          batch_size = 50e3) {
  .f <- rlang::as_function(.f)
  e <- expr(
    batch_wise(
      data,
      batch_size,
      function(chunk) {
        sql <- .f(chunk)
        get_or_execute(con, sql, returning = returning)
      }
    )
  )

  if (is_true(trans)) {
    ret <- dbWithTransaction(con, eval(e))
  } else {
    ret <- eval(e)
  }

  if (is_null(returning)) {
    sum(unlist(ret))
  } else {
    vec_rbind(!!!ret)
  }
}

batch_wise <- function(data, batch_size, .f) {
  .f <- rlang::as_function(.f)

  if (is_null(batch_size)) {
    .f(data)
  } else {
    row_count <- nrow(data)
    batch_count <- ceiling(row_count / batch_size)
    ret <- list()
    for (i in 1:batch_count) {
      start <- ((i - 1) * batch_size) + 1
      end <- start + batch_size - 1
      if (end > row_count) {
        end <- row_count
      }
      ret[[length(ret) + 1]] <- .f(data[start:end,, drop=FALSE])
    }

    ret
  }
}

get_or_execute <- function(conn, sql, returning) {
  if (is_null(returning)) {
    dbExecute(con, sql)
  } else {
    dbGetQuery(conn, sql)
  }
}
