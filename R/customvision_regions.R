#' Add and remove regions from images
#'
#' @param project A Custom Vision object detection project.
#' @param image_ids For `add_image_regions` and `remove_image_regions`, the IDs of the images for which to add or remove images.
#' @param image For `identify_regions`, an image for which to identify possible regions in which an object exists. This can be the ID of an image that was previously uploaded to the project; if not, the image is uploaded. Otherwise, see `add_images` for how to specify an image to upload.
#' @param regions For `add_image_regions`, the regions to add. See 'Details' below.
#' @param region_ids For `remove_image_regions`, a vector of region IDs. This is an alternative to image ID for specifying the regions to remove; if this is provided, `image_ids` is not used.
#' @details
#' The regions to add should be specified as a list of data frames, with one list component per image. Each data frame should have one row per region to add, and the following columns:
#' - `left`, `top`, `width`, `height`: the location and dimensions of the region bounding box.
#' - Either `tag`, the name of the tag to associate with the region, or `tag_id`, the GUID of the tag. All tags must have previously been added to the project, either via [`add_tags`] or [`add_image_tags`].
#' Any other columns in the data frame will be ignored.
#'
#' @return
#' For `add_image_regions`, a data frame containing the details on the added regions.
#'
#' For `remove_image_regions`, the value of `image_ids` invisibly, if this argument was provided; NULL otherwise.
#' @rdname customvision_image_regions
#' @export
add_image_regions <- function(project, image_ids, regions)
{
    if(!all(sapply(image_ids, is_guid)))
        stop("Must provide GUIDs of images to add regions to", call.=FALSE)

    tagdf <- list_tags(project, as="dataframe")[c("name", "id")]
    region_tags <- unique_tags(lapply(regions, `[[`, "tag"))
    if(!all(region_tags %in% tagdf$name))
        tagdf <- rbind(tagdf, add_tags(project, setdiff(region_tags, tagdf$name)))

    tag_ids <- tagdf$id
    names(tag_ids) <- tagdf$name

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

    regions <- do.call(rbind, regions)
    lst <- list()
    while(nrow(regions) > 0)
    {
        idx <- seq_len(min(nrow(regions), 64))
        body <- list(regions=regions[idx, ])
        res <- do_training_op(project, "images/regions", body=body, http_verb="POST", simplifyDataFrame=TRUE)$created
        lst <- c(lst, list(res))
        regions <- regions[-idx, ]
    }
    do.call(rbind, lst)
}


#' @rdname customvision_image_regions
#' @export
remove_image_regions <- function(project, image_ids, region_ids=NULL)
{
    if(!missing(image_ids) && !all(sapply(image_ids, is_guid)))
        stop("Must provide GUIDs of images to be tagged", call.=FALSE)

    if(is_empty(region_ids))
    {
        region_dflst <- subset(list_images(project, "tagged", as="dataframe"), id %in% image_ids)$regions
        region_ids <- do.call(rbind, region_dflst)$regionId
    }

    while(!is_empty(region_ids))
    {
        idx <- seq_len(min(length(region_ids), 64))
        opts <- list(regionIds=paste0(region_ids[idx], collapse=","))
        do_training_op(project, "images/regions", options=opts, http_verb="DELETE")
        region_ids <- region_ids[-idx]
    }

    if(missing(image_ids))
        invisible(NULL)
    else invisible(image_ids)
}


#' @rdname customvision_image_regions
#' @export
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
