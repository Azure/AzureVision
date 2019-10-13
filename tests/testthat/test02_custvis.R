context("Custom Vision project and image creation")

custvis_url <- Sys.getenv("AZ_TEST_CUSTOMVISION_URL")
custvis_key <- Sys.getenv("AZ_TEST_CUSTOMVISION_KEY")
storage <- Sys.getenv("AZ_TEST_STORAGE_ACCT")
custvis_sas <- Sys.getenv("AZ_TEST_CUSTOMVISION_SAS")

if(custvis_url == "" || custvis_key == "" || storage == "" || custvis_sas == "")
    skip("Tests skipped: resource details not set")

projname <- paste0(sample(letters, 10, TRUE), collapse="")


test_that("Custom Vision project creation works",
{
    endp <- customvision_training_endpoint(custvis_url, key=custvis_key)
    expect_is(endp, c("customvision_training_endpoint", "cognitive_endpoint"))

    expect_true(is_empty(list_projects(endp)))

    proj <- create_classification_project(endp, projname)
    expect_is(proj, "classification_project")

    expect_true(!is_empty(list_projects(endp)))
})

test_that("Adding and tagging images works",
{
    endp <- customvision_training_endpoint(custvis_url, key=custvis_key)
    proj <- get_project(endp, projname)

    cans <- paste0(storage, "customvision/", 1:5, ".jpg", custvis_sas)
    cartons <- paste0(storage, "customvision/", 33:37, ".jpg", custvis_sas)
    tags <- rep(c("can", "carton"), each=5)

    img_ids <- add_images(proj, c(cans, cartons), tags)
    expect_type(img_ids, "character")

    img_df <- list_images(proj, "tagged", as="dataframe")
    expect_is(img_df, "data.frame")

    img_df <- img_df[match(img_ids, img_df$id), ]
    img_tags <- do.call(rbind.data.frame, img_df$tags)$tagName
    expect_identical(img_tags, tags)

    img_loc <- add_images(proj, paste0("../resources/", c("can1.jpg", "carton1.jpg")))
    expect_type(img_loc, "character")

    untagged_ids <- list_images(proj, "untagged")
    expect_type(untagged_ids, "character")
    expect_identical(sort(untagged_ids), sort(img_loc))

    tagged_ids <- tag_uploaded_images(proj, list(c("can", "object"), c("carton", "object")), img_loc)
    expect_identical(tagged_ids, img_loc)

    tags <- list_tags(proj)
    expect_identical(sort(tags), c("can", "carton", "object"))

    tagdf <- add_negative_tag(proj, "negtag")
    expect_true("negtag" %in% tagdf$name)

    tags <- list_tags(proj)
    expect_true("negtag" %in% tags)

    untagged_ids <- untag_uploaded_images(proj, list_images(proj))
    expect_type(untagged_ids, "character")

    expect_true(is_empty(list_images(proj, "tagged")))
})


endp <- customvision_training_endpoint(custvis_url, key=custvis_key)
delete_project(endp, projname, confirm=FALSE)
