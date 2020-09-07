Credentials
===========

Auto authentication
-------------------

You need to have a service account credentials to authenticate with
Google Cloud Storage API. The credentials determines which data sources
are accessable to you. Please follow the instructions on [Google
Authentication](https://cloud.google.com/docs/authentication/production)
to create your credentials file and download it to your local
machine(Note that it can be done in the Cloud Console). The package will
search for the credentials file from the environment variable
`GOOGLE_APPLICATION_CREDENTIALS` when it is loaded. If it fails to find
the credentials, it will look for the environment variable
`GCS_AUTH_FILE` instead. If both environment variables are empty, you
will only be able to access public data. If you set the environment
variables after the package is loaded, you can rerun the auto
authentication by `gcs_cloud_auth()`.

Manual authentication
---------------------

The function `gcs_cloud_auth` provides two ways to authenticate with
Google Cloud. When you have a service account credentials, you can
authenticate with Google Cloud by calling
`gcs_cloud_auth(credentials_file_path)`. The second method is to get
credentials from [Cloud SDK](https://cloud.google.com/sdk/). The package
is able to interact with the command-line tool gcloud. Once the account
has been set in the `gcloud` program, the package can be authenticated
by `gcs_cloud_auth(gcloud = TRUE)`. By default, the package will use the
first account in gcloud to authenticate with Google. If there are
multiple accounts in gcloud, you can switch the account by
`gcs_cloud_auth(gcloud = TRUE, email = "your email address")`.

Anouymous access
----------------

Not all the cloud buckets require a credentials to access. You can read
public data without authenticating with Google. By default, if the
package fails to find the credentials from the environment variables, it
will use anouymous access method to query data from google cloud.
Otherwise, you can switch to the anouymous mode by calling
`gcs_cloud_auth(json_file = NULL)`.

Once the credentials is set, you can check your token and authenticate
type via:

    gcs_get_cloud_auth()
    #> Token:               NULL 
    #> Billing project ID:  NULL 
    #> Auth source:         JSON file

Manually create connections
===========================

Creating a connection to a file in a bucket is simple, you can simply
provide an URL to `gcs_connection`. Below is an example to create a read
connection to a public dataset on Google Cloud Platform.

    ## equivalent:
    ## file <- "genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
    file <- "gs://genomics-public-data/NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
    con <- gcs_connection(description = file, open = "r")
    readLines(con, n = 2L)
    #> [1] "##fileformat=VCFv4.2"                                 
    #> [2] "##FILTER=<ID=PASS,Description=\"All filters passed\">"
    close(con)

Note that you do not need a credentials for accessing the public data.
the character “r” in the parameter `open` is an abbreviation of read,
please see `?gcs_connection` for more details.

It is also possible to specify file and bucket name separately. For
example:

    file_name <- "NA12878.chr20.sample.DeepVariant-0.7.2.vcf"
    bucket_name <- "genomics-public-data"
    con <- gcs_connection(
        description = file_name, open = "r", bucket = bucket_name
    )
    readLines(con, n = 2L)
    #> [1] "##fileformat=VCFv4.2"                                 
    #> [2] "##FILTER=<ID=PASS,Description=\"All filters passed\">"
    close(con)

The above code can create the same connection as the previous example.
Note that you cannot provide both URI and bucket name to
`gcs_connection`.

For the write connection, it can be made by specifying an appropriate
open mode in the `gcs_connection` function. Please note that due to the
limitation of the Google Cloud Storage, the write connection is not
seekable, which means you cannot use the `seek` function to

navigate through the file. When the write connection is created. It
always starts in the beginning of the file. If there exists a file with
the same name, the old file will be deleted. After the write connection
is closed, the file will become immutable and no further change on the
file can be made.

create connections from the file manager
========================================

Besides manually creating a connection, the package provides a simple
and convinent S4 class to manage files. You can list all files in a
bucket/folder by

    ## These are equivalent
    ## x <- gcs_dir("gs://genomics-public-data/clinvar/")
    ## x <- gcs_dir("genomics-public-data/clinvar/")

    x <- gcs_dir("gs://genomics-public-data/clinvar/")
    x
    #> 4 items in the folder `genomics-public-data/clinvar/`:
    #> --------------------
    #>                     Name   Size
    #> 1             README.txt   227B
    #> 2      disease_names.txt  2.5MB
    #> 3    variant_summary.txt 89.3MB
    #> 4 variant_summary.txt.gz  8.3MB
    #> --------------------
    #> Total Size :  99.9MB

Note that for listing files in a bucket, the trailing slash does not
make any differences. However, if you want to list files in a folder
inside a bucket, it is recommended to explicitly add a `/` at the end.
The trailing slash is not mandatory, but if no trailing slash presents,
the package need one extra communication with the cloud to determine
whether the path is a file or a folder, which can double your time cost.

In fact, there is no hierarchical structure in a bucket, all files are
in the same level. By using the slash delimiter, the cloud makes the
files appear as though they are stored in folders. Though the cloud does
not have any restriction on the use of `/` in the file name, a good
practice is to not use `/` at the end of a file name for it can cause
unnecessary confusion.

Once obtaining a list of files, you can changes your current directory
or view the detail of a file through `$` or `[[` operator

    ## equivalent: x$README.txt
    myfile <- x[["README.txt"]]
    myfile
    #> File:   clinvar/README.txt 
    #> Bucket: genomics-public-data 
    #> Size:   227B  
    #> Type:   text/plain 
    #> Last modified: 2018-10-16T15:11:31.610Z 
    #> URI:    gs://genomics-public-data/clinvar/README.txt 
    #> URL:    https://console.cloud.google.com/storage/browser/genomics-public-data/?prefix=clinvar%2FREADME.txt 
    #> Billing project:

You can also use `..` to go to the parent folder, `~` to go to the
bucket root.

    ## equivalent: myfile$`..`
    myfile[[".."]]
    #> 4 items in the folder `genomics-public-data/clinvar/`:
    #> --------------------
    #>                     Name   Size
    #> 1             README.txt   227B
    #> 2      disease_names.txt  2.5MB
    #> 3    variant_summary.txt 89.3MB
    #> 4 variant_summary.txt.gz  8.3MB
    #> --------------------
    #> Total Size :  99.9MB

    ## myfile$`~`
    myfile[["~"]]
    #> 17 items in the bucket `genomics-public-data`:
    #> --------------------
    #>                                          Name   Size
    #> 1  NA12878.chr20.sample.DeepVariant-0.7.2.vcf  7.2KB
    #> 2                    NA12878.chr20.sample.bam 55.8KB
    #> 3                                      README   379B
    #> 4                       1000-genomes-phase-3/      *
    #> 5                               1000-genomes/      *
    #> 6                                    clinvar/      *
    #> 7                               cwl-examples/      *
    #> 8                     ftp-trace.ncbi.nih.gov/      *
    #> 9                              gatk-examples/      *
    #> 10                    linkage-disequilibrium/      *
    #> 11                          platinum-genomes/      *
    #> 12                             precision-fda/      *
    #> 13                                references/      *
    #> 14                                 resources/      *
    #> 15           simons-genome-diversity-project/      *
    #> 16                                 test-data/      *
    #> 17                                      ucsc/      *
    #> --------------------
    #> Total Size :  63.4KB

The connection can be made by

    ## Equivalent: gcs_connection(myfile)
    con <- myfile$get_connection(open = "r")
    con
    #> A connection with                                                                                   
    #> description "gs://genomics-public-data/clinvar/README.txt(Billing project enabled)"
    #> class       "google cloud storage"                                                 
    #> mode        "r"                                                                    
    #> text        "text"                                                                 
    #> opened      "opened"                                                               
    #> can read    "yes"                                                                  
    #> can write   "no"
    close(con)

Note that you can pass a file object to the function `gcs_connection`.
The function `gcs_cp` supports both file and folder objects. You can
query file info, copy, delete files through via the file manager

    ## Get file name
    myfile$file_name
    #> [1] "README.txt"

    ## copy file
    ## For the destination, you can specify a path to the file, 
    ## or a path to a folder.
    destination <-tempdir()
    myfile$copy_to(destination)
    file.exists(file.path(destination,myfile$file_name))
    #> [1] TRUE

    ## Delete file, the function is excutable 
    ## only when you have the right to delete the file.
    ## Use `quiet = TRUE` to suppress the confirmation before deleting the file.
    # x$README.txt$delete(quiet = FALSE)

Note that you cannot delete a folder since beforementioned file
hierarchy issue. Once all files in a folder has been deleted, the folder
will be removed.

Requester pays
==============

Some buckets have requester pays enabled, which means you are
responsible for the cost of accessing the data in the bucket. Therefore,
you must have a valid billing project for reading the data in the
bucket. By default, if you use JSON file to authenticate with Google
Cloud, the billing project is the project listed in the JSON file, which
is the project that you use to generate the JSON file. If you use
gcloud, the billing project is the default project in your config file.
You can view and change the default billing project via

    gcs_get_billing_project()
    #> character(0)
    gcs_set_billing_project("your project ID")

For accessing the data in the bucket, you also need to pass the argument
`billing_project = TRUE` to the gcs functions that you want to use. For
example, if `uri` is a path to a file in a requester pays bucket, you
can view its information via `gcs_dir(uri, billing_project = TRUE)`.
Please keep in mind that you will `get charged` if you send a billing
project to a bucket which do not have requester pays enabled. Therefore,
the default behavior of all gcs functions is to access the bucket
without a billing project. If you want your billing project being
involved in every gcs function, you can alter this setting by

    gcs_set_requester_pays(TRUE)
    gcs_get_requester_pays()
    #> [1] TRUE

Buffer size
===========

For reducing the number of network requests and speeding up the
performance of the connection, there is a buffer associated with each
connection. The default size is 1Mb for a buffer. You can set or get the
buffer size via `gcs_read_buff`, `gcs_write_buff`, `gcs_get_read_buff`
and `gcs_get_write_buff` functions:

    gcs_get_read_buff()
    #> [1] 1048576
    gcs_get_write_buff()
    #> [1] 1048576

Please note the minimum buffer size for a write connection is 256 Kb.
Creating a connection with small buffer size may impact the performance.
