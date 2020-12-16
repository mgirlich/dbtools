
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dbtools)](https://CRAN.R-project.org/package=dbtools)
<!-- badges: end -->

`dbtools` provides helpers to insert, update, or delete in a database
table with the rows of a data frame. The main functions are:

-   `db_insert_data()` to insert rows,
-   `db_update_data()` to update rows
-   `db_insert_missing_data()` to insert new rows,
-   `db_upsert_data()` to insert new and update existing rows,
-   `db_delete_data()` to delete rows.

While they are simple to use they also provide these features:

-   *batch operations* to increase the speed,
-   *custom SQL code* to have even better control,
-   *nesting transactions* with the help of `with_transaction()`,
-   *returning* the inserted/updated rows,
-   *upserts/insert missing* also possible without a unique index.

And of course they can easily be used with the pipe `%>%`.

## Installation

<!-- You can install the released version of dbtools from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("dbtools") -->
<!-- ``` -->
<!-- ### Development version -->
<!-- To get a bug fix, or use a feature from the development version, you can install dbtools from GitHub. -->

`dbtools` is not yet on CRAN and can only be installed from GitHub.

    # install.packages("devtools")
    devtools::install_github("mgirlich/dbtools")

## Insert (Missing) Data

Insert records

    library(dbtools)

    if (DBI::dbExistsTable(con_memdb(), "dbtools")) {
      DBI::dbRemoveTable(con_memdb(), "dbtools")
    }

    dbplyr::db_copy_to(
      con = con_memdb(),
      table = "dbtools",
      values = data.frame(
        id = integer(),
        value = character(),
        update_counter = integer(),
        updated_at = character()
      )[0, ],
      overwrite = TRUE,
      unique_indexes = "id"
    )
    #> [1] "dbtools"

    db_insert_data(
      data.frame(
        id = 1,
        value = c("a"),
        update_counter = 0,
        updated_at = Sys.time()
      ),
      table = "dbtools",
      con = con_memdb()
    )
    #> [1] 1

    DBI::dbReadTable(con_memdb(), "dbtools")
    #>   id value update_counter           updated_at
    #> 1  1     a              0 2020-11-26T11:43:47Z

If you want to insert data where the `id` is already in the table you
get an error because of the unique constraint on `id`:

    db_insert_data(
      data.frame(
        id = 1,
        value = c("a"),
        update_counter = 0,
        updated_at = Sys.time()
      ),
      table = "dbtools",
      con = con_memdb()
    )
    #> Error: UNIQUE constraint failed: dbtools.id

To only insert data where the `id` is not yet found in the table use
`db_insert_missing_data()`

    db_insert_missing_data(
      data = data.frame(
        id = 2:3,
        value = c("b", "c"),
        updated_at = Sys.time(),
        update_counter = c(2, 3)
      ),
      table = "dbtools",
      con = con_memdb(),
      conflict_target = "id"
    )
    #> [1] 2

    DBI::dbReadTable(con_memdb(), "dbtools")
    #>   id value update_counter           updated_at
    #> 1  1     a              0 2020-11-26T11:43:47Z
    #> 2  2     b              2 2020-11-26T11:43:48Z
    #> 3  3     c              3 2020-11-26T11:43:48Z

For more information on how to handle conflicts see the [Conflicts and
Unique Columns section](#conflicts).

## Update Data

    db_update_data(
      data = data.frame(
        id = 1:2,
        value = c("x", "y"),
        updated_at = Sys.time()
      ),
      table = "dbtools",
      con = con_memdb(),
      update = c("value", "updated_at"),
      where = "id"
    )
    #> [1] 2

    DBI::dbReadTable(con_memdb(), "dbtools")
    #>   id value update_counter           updated_at
    #> 1  1     x              0 2020-11-26T11:43:48Z
    #> 2  2     y              2 2020-11-26T11:43:48Z
    #> 3  3     c              3 2020-11-26T11:43:48Z

## Custom SQL

Let’s say you only want to update rows with an `update_counter` of at
most 2 and then you also want to increase the update counter by 1 for
the updated rows. This can easily be done by passing SQL code generated
with `sql()`:

    db_update_data(
    # sql_update(
      data = data.frame(
        id = 1:3,
        value = "z",
        updated_at = Sys.time()
      ),
      table = "dbtools",
      con = con_memdb(),
      update = list(
        "value",
        update_counter = sql("update_counter + 1"),
        "updated_at"
      ),
      where = list(
        "id",
        sql("update_counter <= 2")
      )
    )
    #> [1] 2

    DBI::dbReadTable(con_memdb(), "dbtools")
    #>   id value update_counter           updated_at
    #> 1  1     z              1 2020-11-26T11:43:48Z
    #> 2  2     z              3 2020-11-26T11:43:48Z
    #> 3  3     c              3 2020-11-26T11:43:48Z

## Data From Another Database Table

Instead of using local data to update/insert/upsert/delete you can also
another database table by providing its name to the `data` argument:

    if (DBI::dbExistsTable(con_memdb(), "dbtools2")) {
      DBI::dbRemoveTable(con_memdb(), "dbtools2")
    }

    dbplyr::db_copy_to(
      con = con_memdb(),
      table = "dbtools2",
      values = data.frame(
        dbtools_id = 1
      ),
      overwrite = TRUE
    )
    #> [1] "dbtools2"

    DBI::dbReadTable(con_memdb(), "dbtools")
    #>   id value update_counter           updated_at
    #> 1  1     z              1 2020-11-26T11:43:48Z
    #> 2  2     z              3 2020-11-26T11:43:48Z
    #> 3  3     c              3 2020-11-26T11:43:48Z
    DBI::dbReadTable(con_memdb(), "dbtools2")
    #>   dbtools_id
    #> 1          1

    db_delete_data(
      "dbtools2",
      "dbtools",
      con_memdb(),
      where = c(id = "dbtools_id")
    )
    #> [1] 1

    DBI::dbReadTable(con_memdb(), "dbtools")
    #>   id value update_counter           updated_at
    #> 1  2     z              3 2020-11-26T11:43:48Z
    #> 2  3     c              3 2020-11-26T11:43:48Z

## Conflicts and Unique Columns

When a table has a unique constraint on some columns (or it should have
one but for some reason it doesn’t) and you try to insert data some rows
might violate this unique constraint. This is referred to as conflict
target. There are two types of conflict targets in `dbtools`:

-   `sql_unique_cols()` to specify a set of unique columns,
-   `sql_constraint()` to specify the name of an existing unique
    constraint.

Mind that `sql_constraint()` only works in newer versions of databases.
If your database version does not yet support the `ON CONFLICT` clause
you have to use `sql_unique_cols()` and use the `mode = "old"`.

## Generate SQL

You can also easily get the underlying SQL code with the corresponding
`sql_*()` function:

-   `sql_insert()`
-   `sql_insert_missing()`
-   `sql_update()`
-   `sql_delete()`

There are also some helper functions you might find useful:

-   `sql_values()` to generate a `VALUES` clause from the data frame
