context("Custom Vision object detection")

custvis_url <- Sys.getenv("AZ_TEST_CUSTOMVISION_URL")
custvis_key <- Sys.getenv("AZ_TEST_CUSTOMVISION_KEY")
storage <- Sys.getenv("AZ_TEST_STORAGE_ACCT")
custvis_sas <- Sys.getenv("AZ_TEST_CUSTOMVISION_SAS")

if(custvis_url == "" || custvis_key == "" || storage == "" || custvis_sas == "")
    skip("Tests skipped: resource details not set")

projname <- paste0(sample(letters, 10, TRUE), collapse="")


test_that("Object detection project creation works",
{
    endp <- customvision_training_endpoint(custvis_url, key=custvis_key)
    expect_is(endp, c("customvision_training_endpoint", "cognitive_endpoint"))

    expect_true(is_empty(list_projects(endp)))

    proj <- create_object_detection_project(endp, projname)
    expect_is(proj, "object_detection_project")

    expect_true(!is_empty(list_projects(endp)))
})

test_that("Adding and removing regions works",
{
    endp <- customvision_training_endpoint(custvis_url, key=custvis_key)
    proj <- get_project(endp, projname)

    imgs <- paste0(storage, "objectdetection/", 1:128, ".jpg", custvis_sas)
    regs <- readRDS("../resources/regions.rds")
    keep <- sapply(regs, function(df) all(df$tag %in% c("can", "carton")))

    img_ids <- add_images(proj, imgs[keep], regs[keep])
    expect_type(img_ids, "character")

    noreg_ids <- remove_image_regions(proj, img_ids)
    expect_type(noreg_ids, "character")
})


endp <- customvision_training_endpoint(custvis_url, key=custvis_key)
delete_project(endp, projname, confirm=FALSE)
