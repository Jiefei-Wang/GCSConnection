context("Test utils")
## clear credentials
gcs_cloud_auth()

URI <-
    "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"

test_that("gcs_dir: get information of the folders",{
    ## four ways to specify a bucket
    expect_error(gcs_dir("gs://genomics-public-data"),NA)
    expect_error(gcs_dir("genomics-public-data"),NA)
    expect_error(gcs_dir("gs://genomics-public-data/"),NA)
    expect_error(gcs_dir("genomics-public-data/"),NA)
    
    ## four ways to specify a folder
    expect_error(gcs_dir("gs://genomics-public-data/clinvar"),NA)
    expect_error(gcs_dir("genomics-public-data/clinvar"),NA)
    expect_error(gcs_dir("gs://genomics-public-data/clinvar/"),NA)
    expect_error(gcs_dir("genomics-public-data/clinvar/"),NA)
    
    ## Check object type
    x<-gcs_dir("genomics-public-data")
    expect_true(is(x$`NA12878.chr20.sample.DeepVariant-0.7.2.vcf`, "FileClass"))
    expect_true(is(x$`1000-genomes-phase-3/`, "FolderClass"))
})

test_that("gcs_dir: go to path",{
    expect_error(x <- gcs_dir("gs://genomics-public-data/clinvar/"),NA)
    ## Go to cloud root
    expect_error(x[["../"]],NA)
    expect_error(x[["../.."]],NA)
    expect_error(x[["../../genomics-public-data"]],NA)
    expect_error(x[["../../genomics-public-data/clinvar"]],NA)
    
    ## Five ways to specify path
    expect_error(name1 <- .file_names(x[["../1000-genomes/"]]),NA)
    expect_error(name2 <- .file_names(x[["../1000-genomes"]]),NA)
    expect_error(name3 <- .file_names(x[["./../1000-genomes"]]),NA)
    expect_error(name4 <- .file_names(x[["/../1000-genomes"]]),NA)
    expect_error(name5 <- .file_names(x$`..`$`1000-genomes`),NA)
    expect_equal(name1,name2)
    expect_equal(name1,name3)
    expect_equal(name1,name4)
    expect_equal(name1,name5)
    
    b <- x[["/../1000-genomes/README"]]
    expect_true(is(b, "FileClass"))
    ## Go backward
    expect_error(b$`..`,NA)
    expect_error(b$`..`$`..`,NA)
    
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
    expect_equal(file_info$size,as.numeric(x$file_size))
    
    ## delete file
    expect_true(is(x$delete,"function"))
})


test_that("gcs_cp: download file", {
    x <- gcs_dir(URI)
    
    ## Destination is a file path
    tmp_path <- tempdir()
    tmp_file_path <- paste0(tmp_path, "/test.vcf")
    if(file.exists(tmp_file_path))
        file.remove(tmp_file_path)
    
    gcs_cp(from = URI, to = tmp_file_path)
    ## Check result
    expect_true(file.exists(tmp_file_path))
    expect_true(as.numeric(x$file_size) == file.size(tmp_file_path))
    
    if(file.exists(tmp_file_path))
        file.remove(tmp_file_path)
    
    ## Destination is a folder path
    ## The file name is the same as the file name in the cloud
    for(end in c("","/")){
        tmp_folder_path <- paste0(tmp_path, end)
        gcs_cp(from = URI, to = tmp_folder_path)
        ## Check result
        tmp_file_path <- file.path(tmp_path,x$file_name)
        expect_true(file.exists(tmp_file_path))
        expect_true(as.numeric(x$file_size) == file.size(tmp_file_path))
        
        if(file.exists(tmp_file_path))
            file.remove(tmp_file_path)
    }
})

test_that("gcs_cp: download folder", {
    folder_uri <- "gs://genomics-public-data/"
    uri_info <- gcs_dir(folder_uri)
    file_names <- names(uri_info)[.file_types(uri_info)=="file"]
    
    ## Destination folder path can end with or without /
    for(end in c("","/")){
        tmp_path <- tempdir()
        tmp_folder_path <- paste0(tmp_path, end)
        
        for(i in file_names){
            file_path <- paste0(tmp_path,"/",i)
            if(file.exists(file_path))
                file.remove(file_path)
        }
        
        gcs_cp(folder_uri, tmp_folder_path, recursive = FALSE)
        expect_true(all(file_names%in%list.files(tmp_folder_path)))
    }
})
