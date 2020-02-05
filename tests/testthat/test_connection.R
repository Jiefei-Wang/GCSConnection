context("Test connection")
## clear credentials
gcs_cloud_auth()

file <-
  "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
test_that("Public data access, text mode", {
  con <-
    gcs_connection(description = file,
                   open = "r")
  expect_error(readLines(con), NA)
  close(con)
})

test_that("Public data access, binary mode", {
  con <-
    gcs_connection(description = file,
                   open = "rb")
  expect_error(readBin(con, raw(), n = 10L), NA)
  close(con)
  
  ## Auto determine the access mode
  con <-
    gcs_connection(description = file)
  expect_error(readBin(con, raw(), n = 10L), NA)
  close(con)
})

test_that("folder class",{
  x<-gcs_dir("genomics-public-data")
  
  expect_true(is(x$`#file_names#`, "character"))
  expect_true(is(x$`#file_sizes#`, "character"))
  expect_true(is(x$`#refresh_list#`, "function"))
  expect_true(is(x$`NA12878.chr20.sample.DeepVariant-0.7.2.vcf`, "FileClass"))
  expect_true(is(x$`1000-genomes-phase-3/`, "FolderClass"))
  
  expect_true(is(x$`#refresh_list#`(), "FolderClass"))
})

test_that("file class",{
  x<-gcs_dir("genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf")
  
  con <- x$get_connection(open = "rb")
  expect_error(readBin(con, raw(), n = 10L), NA)
  close(con)
  
  tmp <- tempdir()
  tmp_file <- paste0(tmp,"/test")
  x$copy_to(tmp_file)
  file_info <- file.info(tmp_file)
  expect_equal(file_info$size,as.numeric(x$file_size))
  
  expect_true(is(x$delete,"function"))
})


test_that("seek connection", {
  con <-
    gcs_connection(description = file,
                   open = "r")
  res <- readLines(con, n = 1L)
  expect_equal(seek(con, 0), nchar(res) + 1L)
  res2 <- readLines(con, n = 1L)
  expect_equal(res, res2)
  close(con)
})

test_that("credential access", {
  expect_error(gcs_get_cloud_auth(), NA)
  expect_error(gcs_cloud_auth(NULL), NA)
  expect_error(gcs_get_cloud_auth(), NA)
})

test_that("buffer size", {
  n <- 64L
  expect_warning(gcs_set_read_buff(n))
  expect_warning(gcs_set_write_buff(n))
  expect_equal(gcs_get_read_buff(), n)
  expect_false(gcs_get_write_buff()==n)
})
