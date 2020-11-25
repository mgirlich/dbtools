# sql_delete works

    Code
      delete_sql
    Output
      <SQL> WITH `source` (`rowname`, `mpg`, `cyl`, `disp`) AS (
        VALUES
          ('Datsun 710', 22.8, 4.0, 108.0),
          ('Hornet 4 Drive', 21.4, 6.0, 258.0),
          ('Hornet Sportabout', 18.7, 8.0, 360.0)
      )
      DELETE FROM `sql_delete_1` AS `target`
      WHERE EXISTS (
        SELECT 1
          FROM `source`
         WHERE ((`target`.`mpg` = `source`.`mpg`) AND (`source`.`cyl` > 4))
      )

