# full update works

    Code
      update_sql
    Output
      <SQL> WITH `source` (`id1`, `id_2`, `value1`, `value_2`) AS (
        VALUES
          (1, 'a', 101, 1),
          (2, 'c', 103, 3)
      )
      UPDATE `sql_update_1` AS `target`
      SET
        `value1` = `source`.`value1`,
        `value2` = `source`.`value_2`
      FROM `source`
      WHERE ((`target`.`id1` = `source`.`id1`) AND (`target`.`id2` = `source`.`id_2`))

# `data` can be a SQL table

    Code
      sql_update(data = "my_tbl", table = "dbtools_test", con = con, update = c(
        "value1", "value2"), where = c("id1", "id2"))
    Output
      <SQL> UPDATE `dbtools_test` AS `target`
      SET
        `value1` = `source`.`value1`,
        `value2` = `source`.`value2`
      FROM `source`
      WHERE ((`target`.`id1` = `source`.`id1`) AND (`target`.`id2` = `source`.`id2`))

# `data` can be a zero row df

    Code
      sql_update(data = mtcars[0, ], table = "dbtools_test", con = con, update = c(
        "value1", "value2"), where = c("id1", "id2"))
    Output
      <SQL> WITH `source` (`mpg`, `cyl`, `disp`, `hp`, `drat`, `wt`, `qsec`, `vs`, `am`, `gear`, `carb`) AS (
        SELECT NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL WHERE FALSE
      )
      UPDATE `dbtools_test` AS `target`
      SET
        `value1` = `source`.`value1`,
        `value2` = `source`.`value2`
      FROM `source`
      WHERE ((`target`.`id1` = `source`.`id1`) AND (`target`.`id2` = `source`.`id2`))

# sql_update can handle lists in `update`

    Code
      f_update(update = list("value", target_id = "source_id", value1 = sql(
        "target.value1 + 1")), where = c("id"))
    Output
      <SQL> WITH `source` (`id1`, `id2`, `value1`, `value2`) AS (
        VALUES
          (1, 'a', 10, 'A'),
          (1, 'b', 20, 'AA'),
          (2, 'b', 11, 'B'),
          (3, 'c', 11, 'B')
      )
      UPDATE `dbtools_test` AS `target`
      SET
        `value` = `source`.`value`,
        `target_id` = `source`.`source_id`,
        `value1` = target.value1 + 1
      FROM `source`
      WHERE (`target`.`id` = `source`.`id`)

# sql_update can handle lists in `where`

    Code
      f_update(update = c("value"), where = list("value", target_id = "source_id",
        sql("target.value1 + 1")))
    Output
      <SQL> WITH `source` (`id1`, `id2`, `value1`, `value2`) AS (
        VALUES
          (1, 'a', 10, 'A'),
          (1, 'b', 20, 'AA'),
          (2, 'b', 11, 'B'),
          (3, 'c', 11, 'B')
      )
      UPDATE `dbtools_test` AS `target`
      SET
        `value` = `source`.`value`
      FROM `source`
      WHERE ((`target`.`value` = `source`.`value`) AND (`target`.`target_id` = `source`.`source_id`) AND (target.value1 + 1))

# sql_update can use returning

    Code
      f_update(update = c("value1"), where = c("id1"), returning = c("id1", "id2"))
    Error <dbtools_error_invalid_input>
      `returning` doesn't work for SQLite

---

    Code
      f_update(update = c("value1", "value2"), where = c("id1", target_col = "data_col"),
      returning = sql(time = "now()", "id2"))
    Error <dbtools_error_invalid_input>
      `returning` doesn't work for SQLite

