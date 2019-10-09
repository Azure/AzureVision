context("Custom Vision training")

custvis_url <- Sys.getenv("AZ_TEST_CUSTOMVISION_URL")
custvis_key <- Sys.getenv("AZ_TEST_CUSTOMVISION_KEY")
storage <- Sys.getenv("AZ_TEST_STORAGE_ACCT")
custvis_sas <- Sys.getenv("AZ_TEST_CUSTOMVISION_SAS")

if(custvis_url == "" || custvis_key == "" || storage == "" || custvis_sas == "")
    skip("Tests skipped: resource details not set")

projname <- paste0(sample(letters, 10, TRUE), collapse="")
cans <- paste0(storage, "customvision/", 1:5, ".jpg", custvis_sas)
cartons <- paste0(storage, "customvision/", 33:37, ".jpg", custvis_sas)
tags <- rep(c("cans", "cartons"), each=5)

endp <- customvision_training_endpoint(custvis_url, key=custvis_key)

test_that("Custom Vision training endpoint works",
{
    expect_is(endp, c("customvision_training_endpoint", "cognitive_endpoint"))

    expect_true(is_empty(list_projects(endp)))

    proj <- create_classification_project(endp, projname)
    expect_is(proj, "classification_project")

    img_ids <- add_images(proj, c(cans, cartons), tags)
    expect_type(img_ids, "character")

    img_df <- list_images(proj, "tagged", as="dataframe")
    expect_is(img_df, "data.frame")

    img_df <- img_df[match(img_ids, img_df$id), ]
    img_tags <- do.call(rbind.data.frame, img_df$tags)$tagName
    expect_identical(img_tags, tags)

    mod <- train_model(proj)
    expect_is(mod, "customvision_model")

    show <- show_model(mod)
    expect_type(show, "list")

    perf <- show_training_performance(mod)
    expect_type(perf, "list")
})


delete_project(endp, projname, confirm=FALSE)
