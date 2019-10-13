add_image_regions <- function(project, image_ids, regions)
{
    tag_ids <- local({
        tagdf <- list_tags(project, as="dataframe")[c("name", "id")]
        structure(tagdf$id, names=tagdf$name)
    })

    regions <- mapply(
        function(region_df, img)
        {
            if(is.null(region_df$tag_id))
            {
                region_df$tagId <- tag_ids[region_df$tag]
                region_df$tag <- NULL
            }
            else names(region_df)[names(region_df) == "tag_id"] <- "tagId"

            region_df$imageId <- img
            region_df
        },
        regions, image_ids, SIMPLIFY=FALSE
    )

    body <- list(regions=do.call(rbind, regions))
    do_training_op(project, "images/regions", body=body, http_verb="POST", simplifyDataFrame=TRUE)$created
}


utils::globalVariables(c("id"))

remove_image_regions <- function(project, image_ids, region_ids=NULL)
{
    if(is_empty(region_ids))
    {
        region_dflst <- subset(list_images(project, "tagged", as="dataframe"), id %in% image_ids)$regions
        region_ids <- do.call(rbind, region_dflst)$regionId
    }

    opts <- list(regionIds=paste0(region_ids, collapse=","))
    do_training_op(project, "images/regions", options=opts, http_verb="DELETE")

    if(missing(image_ids))
        invisible(NULL)
    else invisible(image_ids)
}


identify_regions <- function(project, image)
{
    image_id <- if(is.character(image) && is_guid(image))
        image
    else add_images(project, image)

    res <- do_training_op(project, file.path("images", image_id, "regionproposals"), http_verb="POST",
                          simplifyDataFrame=TRUE, flatten=TRUE)
    names(res$proposals)[-1] <- c("left", "top", "width", "height")
    res
}
