context("Test connection")
## clear credentials
gcs_cloud_auth(json_file = NULL)

URI <-
  "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"


test_that("four ways to find the file", {
  info <- decompose_google_uri(URI)
  expect_error(
    con <- 
      gcs_connection(
        description = URI,
        open = "r"),
    NA)
  close(con)
  
  expect_error(
    con <- 
      gcs_connection(
        description = paste0(info$full_path_vector, collapse = "/"),
        open = "r"),
    NA)
  close(con)
  
  
  expect_error(
    con <- 
      gcs_connection(
        description = paste0(info$path_vector, collapse = "/"),
        bucket = info$bucket,
        open = "r"),
    NA)
  close(con)
  
  expect_error(con <- 
                 gcs_connection(
                   description = URI,
                   bucket = info$bucket,
                   open = "r"))
})



test_that("Public data access, text mode", {
  con <-
    gcs_connection(description = URI,
                   open = "r")
  expect_error(readLines(con), NA)
  close(con)
})

test_that("Public data access, binary mode", {
  con <-
    gcs_connection(description = URI,
                   open = "rb")
  expect_error(readBin(con, raw(), n = 10L), NA)
  close(con)
  
  ## Auto determine the access mode
  con <-
    gcs_connection(description = URI)
  expect_error(readBin(con, raw(), n = 10L), NA)
  close(con)
})




test_that("seek connection", {
  con <-
    gcs_connection(description = URI,
                   open = "r")
  res <- readLines(con, n = 1L)
  expect_equal(seek(con, 0), nchar(res) + 1L)
  res2 <- readLines(con, n = 1L)
  expect_equal(res, res2)
  close(con)
})



test_that("buffer size", {
  n <- 64L
  expect_warning(gcs_set_read_buff(n))
  expect_warning(gcs_set_write_buff(n))
  expect_equal(gcs_get_read_buff(), n)
  expect_false(gcs_get_write_buff()==n)
})
