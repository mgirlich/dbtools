batch_wise_db <- function(data,
                          con,
                          .f,
                          trans,
                          returning = NULL,
                          batch_size = 50e3) {
  .f <- rlang::as_function(.f)
  f_chunk <- function(chunk) {
    sql <- .f(chunk)
    get_or_execute(con, sql, returning = returning)
  }

  ret <- maybe_trans(
    con,
    batch_wise(
      data,
      batch_size,
      f_chunk
    ),
    trans = trans
  )

  if (is_null(returning)) {
    sum(unlist(ret))
  } else {
    vec_rbind(!!!ret)
  }
}

batch_wise <- function(data, batch_size, .f) {
  if (is_null(batch_size) || vec_size(data) == 0) {
    # wrap in list so that it has the same behaviour as if batched
    list(.f(data))
  } else {
    row_count <- nrow(data)
    batch_count <- ceiling(row_count / batch_size)
    ret <- vector("list", batch_count)

    for (i in 1:batch_count) {
      start <- ((i - 1) * batch_size) + 1
      end <- min(start + batch_size - 1, row_count)
      ret[[i]] <- .f(data[start:end, , drop = FALSE])
      # TODO use `vec_slice()` instead?
    }

    ret
  }
}

get_or_execute <- function(con, sql, returning) {
  if (is_null(returning)) {
    DBI::dbExecute(con, sql)
  } else {
    DBI::dbGetQuery(con, sql)
  }
}

# nocov start
maybe_trans <- function(con, code, trans) {
  if (is_false(trans)) {
    return(code)
  }

  # the following code is mostly copied from DBI::dbWithTransaction()
  # the rollback_because function is adapted so that the original error
  # is kept

  ## needs to be a closure, because it accesses conn
  rollback_because <- function(e) {
    call <- DBI::dbRollback(con)
    if (identical(call, FALSE)) {
      abort2(
        e,
        paste(
          "Failed to rollback transaction.",
          "Tried to roll back because an error",
          "occurred:", conditionMessage(e)
        )
      )
    }
    if (inherits(e, "error")) {
      abort2(e)
    }
  }

  ## check if each operation is successful
  call <- DBI::dbBegin(con)
  if (identical(call, FALSE)) {
    abort("Failed to begin transaction")
  }
  tryCatch(
    {
      res <- force(code)
      call <- DBI::dbCommit(con)
      if (identical(call, FALSE)) {
        abort("Failed to commit transaction")
      }
      res
    },
    dbi_abort = rollback_because,
    error = rollback_because
  )
}

abort2 <- function(e, message = NULL) {
  abort(
    message %||% conditionMessage(e) %||% "",
    class = class(e),
    error = e,
    trace = e$trace
  )
}
# nocov end
