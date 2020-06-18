## This unit tests only test some special functions for the classes
## For more generic tests, see the file `test_gcs_dir.R`
context("Test FileClass and FolderClass")

## clear credentials
gcs_cloud_auth()

test_that("Classes: as character",{
    x <- gcs_dir("gs://genomics-public-data/clinvar")
    expect_equal(as.character(x), "gs://genomics-public-data/clinvar/")
    expect_equal(as(x, "character"), "gs://genomics-public-data/clinvar/")
    
    x1 <- x$README.txt
    expect_equal(as.character(x1), "gs://genomics-public-data/clinvar/README.txt")
    expect_equal(as(x1, "character"), "gs://genomics-public-data/clinvar/README.txt")
})



test_that("Classes: gcs_connection",{
    x <- gcs_dir("gs://genomics-public-data/clinvar/README.txt")
    expect_error(con <- gcs_connection(x), NA)
    close(con)
})


test_that("Classes: gcs_cp",{
    x <- gcs_dir("gs://genomics-public-data/")
    
    tmp_path <- tempdir()
    file_names <- .file_names(x)
    file_names <- file_names[.is_files(x)]
    for(i in file_names){
        file_path <- paste0(tmp_path,"/",i)
        if(file.exists(file_path))
            file.remove(file_path)
    }
    
    gcs_cp(x, tmp_path, recursive = FALSE)
    expect_true(all(file_names %in% list.files(tmp_path)))
})


test_that("Classes: gcs_dir",{
    x <- gcs_dir("gs://genomics-public-data/clinvar")
    expect_equal(x, gcs_dir(x))
})



