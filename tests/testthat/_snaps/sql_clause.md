# `sql_clause_data()` handles dataframes

    Code
      sql_clause_data(con, mtcars_df[1:2, ], "values")
    Output
      <SQL> `values` (`rowname`, `mpg`, `cyl`, `disp`) AS (
        VALUES
          ('Mazda RX4', 21, 6, 160),
          ('Mazda RX4 Wag', 21, 6, 160)
      )

# `sql_values()` works for empty dataframes and SQLite

    Code
      sql_values(con, mtcars_df[0, ])
    Output
      <SQL> SELECT NULL, NULL, NULL, NULL WHERE FALSE

# `sql_values()` works for empty dataframes and PostgreSQL

    Code
      sql_values(con_pg, mtcars_df[0, ])
    Output
      <SQL> SELECT NULL::text, NULL::float8, NULL::float8, NULL::float8 WHERE FALSE

# `sql_clause_cte_table()` can handle different input types

    Code
      chr_cols
    Output
      <SQL> `table` (`col 1`, `col 2`) AS (
        SELECT * FROM `source`
      )

# `sql_clause_set()` works

    Code
      sql_clause_set(con, sql(x = "a"))
    Output
      <SQL> SET
        `x` = a

# `sql_clause_set()` checks input type

    `updates` must be sql.

---

    `updates` must be sql.

# `sql_clause_set()` checks for names

    `updates` must be named.

---

    `updates` must be named.

# `sql_clause_returning()` works

    Code
      sql_clause_returning(con, c(x = "a", "b"))
    Output
      <SQL> RETURNING `a` AS `x`, `b`

---

    Code
      sql_clause_returning(con, ident(x = "a", "b"))
    Output
      <SQL> RETURNING `a` AS `x`, `b`

---

    Code
      sql_clause_returning(con, sql(time = "now()", "b + 1"))
    Output
      <SQL> RETURNING now() AS `time`, b + 1

---

    Code
      sql_clause_returning(con, list(time = sql("now()"), x = "a", "b"))
    Output
      <SQL> RETURNING now() AS `time`, `a` AS `x`, `b`

