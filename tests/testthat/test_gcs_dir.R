context("Test gcs_dir")

## clear credentials
gcs_cloud_auth()

URI <-
    "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
test_that("gcs_dir: empty path",{
    expect_error(x <- gcs_dir(NULL), NA)
})

test_that("gcs_dir: get information of the folders",{
    ## four ways to specify a bucket
    expect_error(gcs_dir("gs://genomics-public-data"), NA)
    expect_error(gcs_dir("genomics-public-data"), NA)
    expect_error(gcs_dir("gs://genomics-public-data/"), NA)
    expect_error(gcs_dir("genomics-public-data/"), NA)
    
    ## four ways to specify a folder
    expect_error(gcs_dir("gs://genomics-public-data/clinvar"), NA)
    expect_error(gcs_dir("genomics-public-data/clinvar"), NA)
    expect_error(gcs_dir("gs://genomics-public-data/clinvar/"), NA)
    expect_error(gcs_dir("genomics-public-data/clinvar/"), NA)
    
    ## Check object type
    x<-gcs_dir("genomics-public-data")
    expect_true(is(x$`NA12878.chr20.sample.DeepVariant-0.7.2.vcf`, "FileClass"))
    expect_true(is(x$`1000-genomes-phase-3/`, "FolderClass"))
})

test_that("gcs_dir: go to path",{
    expect_error(x <- gcs_dir("gs://genomics-public-data/clinvar/"), NA)
    ## Go to parent folders
    expect_error(x[["../"]], NA)
    expect_error(x[["../.."]], NA)
    expect_error(x[["../../genomics-public-data"]], NA)
    expect_error(x[["../../genomics-public-data/clinvar"]], NA)
    
    ## use ~ to go to bucket root
    expect_error(x[["~"]], NA)
    expect_error(x[["~/"]], NA)
    expect_equal(.file_names(x[[".."]]), .file_names(x[["~"]]), NA)
    
    ## Five ways to specify path
    expect_error(name1 <- .file_names(x[["../1000-genomes/"]]), NA)
    expect_error(name2 <- .file_names(x[["../1000-genomes"]]), NA)
    expect_error(name3 <- .file_names(x[["./../1000-genomes"]]), NA)
    expect_error(name4 <- .file_names(x$`..`$`1000-genomes`), NA)
    expect_equal(name1,name2)
    expect_equal(name1,name3)
    expect_equal(name1,name4)
    
    ## File class
    b <- x[["../1000-genomes/README"]]
    expect_true(is(b, "FileClass"))
    ## Go backward
    expect_error(b$`..`, NA)
    expect_error(b[[".."]], NA)
    expect_error(b[["~"]], NA)
})



test_that("gcs_dir: build connection/download file/delete file",{
    x <- gcs_dir(URI)
    
    ## build connection
    con <- x$get_connection(open = "rb")
    expect_error(readBin(con, raw(), n = 10L), NA)
    close(con)
    
    ## download file
    tmp_path <- tempdir()
    tmp_file_path <- paste0(tmp_path, "/test.vcf")
    x$copy_to(tmp_file_path)
    file_info <- file.info(tmp_file_path)
    expect_equal(file_info$size, as.numeric(x$file_size))
    
    ## delete file
    expect_true(is(x$delete,"function"))
})

