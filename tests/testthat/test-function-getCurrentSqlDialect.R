#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("getCurrentSqlDialect() works", {

  expect_error(
    kwb.db:::getCurrentSqlDialect()
    # Please use setCurrentSqlDialect to set the current SQL dialect first!
  )

})

