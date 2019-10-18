context("Custom Vision training and prediction")

custvis_url <- Sys.getenv("AZ_TEST_CUSTOMVISION_URL")
custvis_key <- Sys.getenv("AZ_TEST_CUSTOMVISION_KEY")
storage <- Sys.getenv("AZ_TEST_STORAGE_ACCT")
custvis_sas <- Sys.getenv("AZ_TEST_CUSTOMVISION_SAS")
custvis_pred_resid <- Sys.getenv("AZ_TEST_CUSTOMVISION_PRED_RESID")
custvis_pred_url <- Sys.getenv("AZ_TEST_CUSTOMVISION_PRED_URL")
custvis_pred_key <- Sys.getenv("AZ_TEST_CUSTOMVISION_PRED_KEY")

if(custvis_url == "" || custvis_key == "" || storage == "" || custvis_sas == "" ||
   custvis_pred_resid == "" || custvis_pred_url == "" || custvis_pred_key == "")
    skip("Tests skipped: resource details not set")

projname <- paste0(sample(letters, 10, TRUE), collapse="")
cans <- paste0(storage, "customvision/", 1:5, ".jpg", custvis_sas)
cartons <- paste0(storage, "customvision/", 33:37, ".jpg", custvis_sas)
tags <- rep(c("can", "carton"), each=5)

endp <- customvision_training_endpoint(custvis_url, key=custvis_key)

test_that("Model training works",
{
    expect_is(endp, c("customvision_training_endpoint", "cognitive_endpoint"))
    expect_true(is_empty(list_projects(endp)))

    proj <- create_classification_project(endp, projname, export_target="standard")
    expect_is(proj, "classification_project")

    Sys.sleep(2)
    img_ids <- add_images(proj, c(cans, cartons), tags)
    expect_type(img_ids, "character")

    img_df <- list_images(proj, "tagged", as="dataframe")
    expect_is(img_df, "data.frame")

    img_df <- img_df[match(img_ids, img_df$id), ]
    img_tags <- do.call(rbind.data.frame, img_df$tags)$tagName
    expect_identical(img_tags, tags)

    Sys.sleep(2)
    mod <- train_model(proj)
    expect_is(mod, "customvision_model")

    show <- show_model(mod)
    expect_type(show, "list")

    perf <- show_training_performance(mod)
    expect_type(perf, "list")
})

test_that("Training endpoint prediction and export works",
{
    proj <- get_project(endp, projname)
    mod <- get_model(proj)
    expect_is(mod, "customvision_model")

    pred1 <- predict(mod, cans)
    expect_type(pred1, "character")
    expect_identical(length(pred1), length(cans))

    pred2 <- predict(mod, "../resources/can1.jpg", type="prob")
    expect_is(pred2, "matrix")
    expect_type(pred2, "double")
    expect_identical(dim(pred2), c(1L, 2L))

    pred3 <- predict(mod, cans[1], type="list")
    expect_is(pred3, "list")
    expect_true(all(sapply(pred3, is.data.frame)))

    expect_error(predict(mod, c(cans, "../resources/can1.jpg")))

    exp_url <- export_model(mod, "tensorflow", download=FALSE)
    expect_true(is_url(exp_url))
})

test_that("Prediction endpoint works",
{
    proj <- get_project(endp, projname)
    mod <- get_model(proj)

    expect_silent(publish_model(mod, projname, custvis_pred_resid))

    pred_endp <- customvision_prediction_endpoint(custvis_pred_url, key=custvis_pred_key)
    expect_is(pred_endp, "customvision_prediction_endpoint")

    svc <- classification_service(pred_endp, proj, projname)
    pred1 <- predict(svc, cans)
    expect_type(pred1, "character")
    expect_identical(length(pred1), length(cans))
})


mod <- get_model(get_project(endp, projname))
unpublish_model(mod, confirm=FALSE)
delete_project(endp, projname, confirm=FALSE)
