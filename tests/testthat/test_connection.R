context("Test connection")

file <-
  "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
test_that("Public data access, text mode", {
  con <-
    gcs_connection(description = file,
                   open = "r",
                   credentials = "")
  expect_error(readLines(con), NA)
  close(con)
})

test_that("Public data access, binary mode", {
  con <-
    gcs_connection(description = file,
                   open = "rb",
                   credentials = "")
  expect_error(readBin(con, raw(), n = 10L), NA)
  close(con)
})

test_that("seek connection", {
  con <-
    gcs_connection(description = file,
                   open = "r",
                   credentials = "")
  res <- readLines(con, n = 1L)
  expect_equal(seek(con, 0), nchar(res) + 1L)
  res2 <- readLines(con, n = 1L)
  expect_equal(res, res2)
  close(con)
})

test_that("credential access", {
  expect_error(gcs_get_cloud_auth(), NA)
  expect_error(gcs_cloud_auth(NULL), NA)
  expect_null(gcs_get_cloud_auth())
})

test_that("buffer size", {
  n <- 64L
  expect_error(gcs_input_stream_buff(n), NA)
  expect_warning(gcs_output_stream_buff(n))
  expect_equal(gcs_get_input_stream_buff(), n)
  expect_false(gcs_get_output_stream_buff()==n)
})
