context("Test gcs_cp")

## clear credentials
gcs_cloud_auth()

URI <- "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"

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

    expect_true(
        as.numeric(x$file_size) == file.size(tmp_file_path)
    )

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
