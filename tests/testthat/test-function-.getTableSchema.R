#
# This test file has been generated by kwb.test::create_test_files()
#

test_that(".getTableSchema() works", {

  expect_error(
    kwb.db:::.getTableSchema()
    # No source file (*.mdb, *.accdb, *.xls or *.xlsx) or name of ODBC data source given.
  )

})

