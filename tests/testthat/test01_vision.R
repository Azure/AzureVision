context("Computer Vision")

vision_url <- Sys.getenv("AZ_TEST_COMPUTERVISION_URL")
vision_key <- Sys.getenv("AZ_TEST_COMPUTERVISION_KEY")
storage <- Sys.getenv("AZ_TEST_STORAGE_ACCT")

if(vision_url == "" || vision_key == "" || storage == "")
    skip("Tests skipped: resource details not set")


test_that("Computer Vision endpoint works",
{
    endp <- computervision_endpoint(vision_url, key=vision_key)
    expect_is(endp, c("computervision_endpoint", "cognitive_endpoint"))

    res_doms <- list_domains(endp)
    expect_is(res_doms, "computervision_response")

    img <- "../resources/bill.jpg"

    res_analyze <- analyze(endp, img)
    expect_is(res_analyze, "computervision_response")

    res_analyze_celeb <- analyze(endp, img, domain="celebrities")
    expect_is(res_analyze_celeb, "computervision_response")

    res_analyze_tags <- analyze(endp, img, feature_types="tags")
    expect_is(res_analyze_celeb, "computervision_response")

    res_desc <- describe(endp, img)
    expect_is(res_desc, "computervision_response")

    res_desc_lang <- describe(endp, img, language="es")
    expect_is(res_desc, "computervision_response")

    res_detobj <- detect_objects(endp, img)
    expect_is(res_detobj, "computervision_response")

    res_detface <- detect_faces(endp, img)
    expect_is(res_detface, "computervision_response")

    res_area <- area_of_interest(endp, img)
    expect_is(res_area, "computervision_response")

    res_tag <- tag(endp, img)
    expect_is(res_tag, "data.frame")

    res_cat <- categorize(endp, img)
    expect_is(res_cat, "data.frame")

    res_thumb <- make_thumbnail(endp, img, width=50, height=50)
    expect_type(res_thumb, "raw")
})
