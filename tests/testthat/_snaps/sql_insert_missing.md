# `sql_insert_missing()` works in new mode

    Code
      insert_missing_sql
    Output
      <SQL> WITH `source` (`rowname`, `mpg`, `cyl`, `disp`) AS (
        VALUES
          ('Datsun 710', 22.8, 4, 108),
          ('Hornet 4 Drive', 21.4, 6, 258),
          ('Hornet Sportabout', 18.7, 8, 360)
      )
      INSERT INTO `sql_insert_missing_1` AS `target` (`rowname`, `mpg`, `cyl`, `disp`)
      SELECT `rowname`, `mpg`, `cyl`, `disp`
      FROM `source`
      WHERE (true)
      ON CONFLICT ("rowname")
      DO NOTHING

# `sql_insert_missing()` works in old mode

    Code
      insert_missing_sql
    Output
      <SQL> WITH `source` (`rowname`, `mpg`, `cyl`, `disp`) AS (
        VALUES
          ('Datsun 710', 22.8, 4, 108),
          ('Hornet 4 Drive', 21.4, 6, 258),
          ('Hornet Sportabout', 18.7, 8, 360)
      )
      INSERT INTO `sql_insert_missing_2` AS `target` (`rowname`, `mpg`, `cyl`, `disp`)
      SELECT `rowname`, `mpg`, `cyl`, `disp`
      FROM `source`
      WHERE NOT EXISTS (
        SELECT 1
          FROM `sql_insert_missing_2` AS `target`
         WHERE (`target`.`rowname` = `source`.`rowname`)
      )

