context("Computer Vision")

vision_url <- Sys.getenv("AZ_TEST_COMPUTERVISION_URL")
vision_key <- Sys.getenv("AZ_TEST_COMPUTERVISION_KEY")
storage <- Sys.getenv("AZ_TEST_STORAGE_ACCT")

if(vision_url == "" || vision_key == "" || storage == "")
    skip("Tests skipped: resource details not set")


test_that("Computer Vision endpoint works with URL",
{
    endp <- computervision_endpoint(vision_url, key=vision_key)
    expect_is(endp, c("computervision_endpoint", "cognitive_endpoint"))

    res_doms <- list_computervision_domains(endp)
    expect_type(res_doms, "character")

    img <- httr::parse_url(storage)
    img$path <- "cognitive/bill.jpg"
    img <- httr::build_url(img)

    res_analyze <- analyze(endp, img)
    expect_is(res_analyze, "list")
    expect_is(res_analyze$categories, "data.frame")

    res_analyze_celeb <- analyze(endp, img, domain="celebrities")
    expect_is(res_analyze_celeb$categories, "data.frame")
    expect_is(res_analyze_celeb$categories$detail, "data.frame")

    res_analyze_tags <- analyze(endp, img, feature_types="tags")
    expect_is(res_analyze_tags$tags, "data.frame")

    res_analyze_faces <- analyze(endp, img, feature_types="faces")
    expect_is(res_analyze_faces$faces, "data.frame")

    res_desc <- describe(endp, img)
    expect_is(res_desc, "list")
    expect_type(res_desc$tags, "character")
    expect_is(res_desc$captions, "data.frame")

    res_desc_lang <- describe(endp, img, language="es")
    expect_is(res_desc_lang, "list")
    expect_type(res_desc_lang$tags, "character")
    expect_is(res_desc_lang$captions, "data.frame")

    res_detobj <- detect_objects(endp, img)
    expect_is(res_detobj, "data.frame")

    res_area <- area_of_interest(endp, img)
    expect_type(res_area, "integer")

    res_tag <- tag(endp, img)
    expect_is(res_tag, "data.frame")

    res_cat <- categorize(endp, img)
    expect_is(res_cat, "data.frame")

    text_img <- httr::parse_url(storage)
    text_img$path <- "cognitive/gettysburg.png"
    text_img <- httr::build_url(text_img)

    res_text <- read_text(endp, text_img)
    expect_is(res_text, "list")
    expect_type(res_text[[1]], "character")

    res_thumb <- make_thumbnail(endp, img, outfile=NULL, width=50, height=50)
    expect_type(res_thumb, "raw")
})

test_that("Computer Vision endpoint works with local file",
{
    endp <- computervision_endpoint(vision_url, key=vision_key)

    img <- "../resources/bill.jpg"

    res_analyze <- analyze(endp, img)
    expect_is(res_analyze, "list")
})

