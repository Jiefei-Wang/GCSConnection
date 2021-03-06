context("Test credentials")

test_that("credentials access", {
    ## clear out all credentials
    expect_error(gcs_cloud_auth(NULL), NA)
    expect_error(gcs_get_cloud_auth(), NA)

    ## The default method
    expect_error(gcs_cloud_auth(), NA)
    expect_error(gcs_get_cloud_auth(), NA)
})


test_that("Billing project", {
    expect_error(gcs_set_billing_project("test"), NA)
    expect_equal(gcs_get_billing_project(), "test")
    expect_error(gcs_cloud_auth(), NA)
})

test_that("requester pays", {
    expect_true(gcs_is_requester_pays("bioconductor_rp"))
    expect_false(gcs_is_requester_pays("genomics-public-data"))
})
