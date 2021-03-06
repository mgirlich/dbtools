# sql_insert_nc works

    Code
      f_insert(insert_cols = NULL, returning = NULL, return_all = FALSE)
    Output
      <SQL> WITH `source` (`id1`, `id2`, `value1`, `value2`) AS (
        VALUES
          (1.0, 'a', 10.0, 'A'),
          (1.0, 'b', 20.0, 'AA'),
          (2.0, 'b', 11.0, 'B'),
          (3.0, 'c', 11.0, 'B')
      )
      INSERT INTO `dbtools_test` AS `target` (`id1`, `id2`, `value1`, `value2`)
      SELECT `id1`, `id2`, `value1`, `value2`
      FROM `source`

# `insert_cols` works

    Code
      f_insert(insert_cols = c("id1", "value1"), returning = NULL, return_all = FALSE)
    Output
      <SQL> WITH `source` (`id1`, `id2`, `value1`, `value2`) AS (
        VALUES
          (1.0, 'a', 10.0, 'A'),
          (1.0, 'b', 20.0, 'AA'),
          (2.0, 'b', 11.0, 'B'),
          (3.0, 'c', 11.0, 'B')
      )
      INSERT INTO `dbtools_test` AS `target` (`id1`, `value1`)
      SELECT `id1`, `value1`
      FROM `source`

# `returning` works

    Code
      f_insert(insert_cols = NULL, returning = sql("*"), return_all = FALSE)
    Output
      <SQL> WITH `source` (`id1`, `id2`, `value1`, `value2`) AS (
        VALUES
          (1.0, 'a', 10.0, 'A'),
          (1.0, 'b', 20.0, 'AA'),
          (2.0, 'b', 11.0, 'B'),
          (3.0, 'c', 11.0, 'B')
      )
      INSERT INTO `dbtools_test` AS `target` (`id1`, `id2`, `value1`, `value2`)
      SELECT `id1`, `id2`, `value1`, `value2`
      FROM `source`
      RETURNING *

# do nothing on conflict works

    Code
      f_insert(conflict = sql_do_nothing(sql_unique_cols("id1", "id2")), insert_cols = NULL,
      returning = sql("*"), return_all = FALSE)
    Output
      <SQL> WITH `source` (`id1`, `id2`, `value1`, `value2`) AS (
        VALUES
          (1.0, 'a', 10.0, 'A'),
          (1.0, 'b', 20.0, 'AA'),
          (2.0, 'b', 11.0, 'B'),
          (3.0, 'c', 11.0, 'B')
      )
      INSERT INTO `dbtools_test` AS `target` (`id1`, `id2`, `value1`, `value2`)
      SELECT `id1`, `id2`, `value1`, `value2`
      FROM `source`
      WHERE NOT EXISTS (
        SELECT 1
          FROM `dbtools_test` AS `target`
         WHERE ((`target`.`id1` = `source`.`id1`) AND (`target`.`id2` = `source`.`id2`))
      )
      RETURNING *

# do update on conflict works

    Code
      f_insert(con = con_pg(), conflict = sql_do_update(sql_unique_cols("id1", "id2"),
      c("value1")), insert_cols = NULL, returning = sql("*"), return_all = FALSE)
    Output
      <SQL> WITH "source" ("id1", "id2", "value1", "value2") AS (
        VALUES
          (1.0, 'a', 10.0, 'A'),
          (1.0, 'b', 20.0, 'AA'),
          (2.0, 'b', 11.0, 'B'),
          (3.0, 'c', 11.0, 'B')
      ),
      "insert_action" AS (
        INSERT INTO "dbtools_test" AS "target" ("id1", "id2", "value1", "value2")
        SELECT "id1", "id2", "value1", "value2"
        FROM "source"
        WHERE NOT EXISTS (
          SELECT 1
            FROM "dbtools_test" AS "target"
           WHERE (("target"."id1" = "source"."id1") AND ("target"."id2" = "source"."id2"))
        )
        RETURNING *
      )
      UPDATE "dbtools_test" AS "target"
      SET
        "value1" = "source"."value1"
      FROM "source"
      WHERE (("target"."id1" = "source"."id1") AND ("target"."id2" = "source"."id2"))

