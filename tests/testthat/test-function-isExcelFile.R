#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("isExcelFile() works", {

  expect_error(
    kwb.db:::isExcelFile()
    # argument "filepath" is missing, with no default
  )

})

