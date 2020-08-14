context("Test gcs_dir")

## clear credentials
gcs_cloud_auth(json_file = NULL)

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


test_that("gcs_dir: basic accessor function for file class",{
    x <- gcs_dir(URI)
    
    ## access basic information of a file class
    expect_equal(x$full_path_vector, 
                 c("genomics-public-data", 
                   "NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
    ))
    expect_equal(x$bucket_name, "genomics-public-data")
    expect_equal(x$file_name, "NA12878.chr20.sample.DeepVariant-0.7.2.vcf")
    expect_equal(x$file_size,"7194")
    expect_equal(x$file_type, "text/vcard")
    expect_equal(x$uri, URI)
    expect_equal(x$url, "https://console.cloud.google.com/storage/browser/genomics-public-data/?prefix=NA12878.chr20.sample.DeepVariant-0.7.2.vcf")
    expect_equal(x$lastModified, "2019-04-23T19:58:29.107Z")
    expect_equal(x$billing_project, NULL)
    
    ## Case sensitivity test
    expect_equal(x$uri, x$URI)
    expect_equal(x$lastModified, x$lastmodified)
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


test_that("gcs_dir: convert to character",{
    ## Folder class to character
    uri <- "gs://genomics-public-data/clinvar/"
    x <- gcs_dir(uri)
    expect_equal(as.character(x), uri)
    
    ## File class to character
    x <- gcs_dir(URI)
    expect_equal(as.character(x), URI)
})


test_that("gcs_dir: work with gcs_connection",{
    x <- gcs_dir(URI)
    expect_error(con <- gcs_connection(x), NA)
    close(con)
})


test_that("gcs_dir: work with gcs_cp",{
    ## download file
    x <- gcs_dir(URI)
    tmp_path <- tempdir()
    tmp_file_path <- paste0(tmp_path, "/test.vcf")
    if(file.exists(tmp_file_path))
        file.remove(tmp_file_path)
    gcs_cp(x, tmp_file_path)
    file_info <- file.info(tmp_file_path)
    expect_equal(file_info$size, as.numeric(x$file_size))
    
    ## download Folder
    x <- gcs_dir("gs://genomics-public-data")
    file_names <- names(x)[.file_types(x)=="file"]
    tmp_path <- tempdir()
    for(i in file_names){
        file_path <- paste0(tmp_path,"/",i)
        if(file.exists(file_path))
            file.remove(file_path)
    }
    gcs_cp(x, tmp_path, recursive = FALSE)
    expect_true(all(file_names%in%list.files(tmp_path)))
})


test_that("gcs_dir: work with gcs_dir",{
    x <- gcs_dir(URI)
    expect_error(y <- gcs_dir(x), NA)
})

