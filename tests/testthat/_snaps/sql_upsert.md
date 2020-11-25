# `sql_upsert()` works in new mode

    Code
      upsert_sql
    Output
      <SQL> WITH `source` (`rowname`, `mpg`, `cyl`, `disp`) AS (
        VALUES
          ('Datsun 710', 22.8, 6.0, 108.0),
          ('Hornet 4 Drive', 21.4, 8.0, 258.0),
          ('Hornet Sportabout', 18.7, 10.0, 360.0)
      )
      INSERT INTO `sql_upsert_1` AS `target` (`rowname`, `mpg`, `cyl`)
      SELECT `rowname`, `mpg`, `cyl`
      FROM `source`
      WHERE (true)
      ON CONFLICT ("rowname")
      DO UPDATE SET
        `cyl` = `EXCLUDED`.`cyl`,
        `mpg` = -`EXCLUDED`.`mpg`

# `sql_upsert()` works in old mode

    Code
      upsert_sql
    Output
      <SQL> WITH "source" ("rowname", "mpg", "cyl", "disp") AS (
        VALUES
          ('Datsun 710', 22.8, 6.0, 108.0),
          ('Hornet 4 Drive', 21.4, 8.0, 258.0),
          ('Hornet Sportabout', 18.7, 10.0, 360.0)
      ),
      "insert_action" AS (
        INSERT INTO "sql_upsert_2" AS "target" ("rowname", "mpg", "cyl")
        SELECT "rowname", "mpg", "cyl"
        FROM "source"
        WHERE NOT EXISTS (
          SELECT 1
            FROM "sql_upsert_2" AS "target"
           WHERE ("target"."rowname" = "source"."rowname")
        )
        RETURNING *
      )
      UPDATE "sql_upsert_2" AS "target"
      SET
        "cyl" = "source"."cyl",
        "mpg" = -"source"."mpg"
      FROM "source"
      WHERE ("target"."rowname" = "source"."rowname")

