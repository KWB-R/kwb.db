#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("isMySQL() works", {

  expect_error(
    kwb.db:::isMySQL()
    # No source file (*.mdb, *.accdb, *.xls or *.xlsx) or name of ODBC data source given.
  )

})

