#
# This test file has been generated by kwb.test::create_test_files()
#

test_that(".getForeignKeyName() works", {

  expect_error(
    kwb.db:::.getForeignKeyName()
    # argument "tbl" is missing, with no default
  )

})

