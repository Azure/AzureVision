#' Add, retrieve and remove tags for a project
#'
#' @param project A Custom Vision project.
#' @param tags For `add_tags`, a vector of strings to treat as tags.
#' @param name,id For `get_tag`, the name (text string) for a tag, and its ID. Provide one or the other, but not both.
#' @param negative_name For `add_negative_tag`, the label to provide a negative tag. See 'Negative tags' below.
#' @param as For `list_tags`, the format in which to return results: a vector of tag names, a vector of tag IDs, a data frame of metadata, or a list of metadata.
#' @param iteration For `list_tags` and `get_tag`, the iteration ID (roughly, which model generation to use). Defaults to the latest iteration.
#' @param confirm For `remove_tags`, whether to ask for confirmation first.
#' @details
#' _Tags_ are the labels attached to images for use in classification projects. An image can have one or multiple tags associated with it; however, the latter only makes sense if the project is setup for multi-label classification.
#'
#' Tags form part of the metadata for a Custom Vision project, and have to be explicitly defined prior to use. Each tag has a corresponding ID which is used to manage it. In general, you can let AzureVision handle the details of managing tags and tag IDs.
#'
#' @section Negative tags:
#' A _negative tag_ is a special tag that represents the absence of any other tag. For example, if a project is classifying images into cats and dogs, an image that doesn't contain either a cat or dog should be given a negative tag. This can be distinguished from an _untagged_ image, where there is no information at all on what it contains.
#'
#' You can add a negative tag to a project with the `add_negative_tag` method. Once defined, a negative tag is treated like any other tag. A project can only have one negative tag defined.
#' @seealso
#' [`add_image_tags`], [`remove_image_tags`]
#' @rdname customvision_tags
#' @aliases customvision_tags
#' @export
add_tags <- function(project, tags)
{
    current_tags <- list_tags(project, as="names")
    tags <- unique_tags(tags)
    newtags <- setdiff(tags, current_tags)
    if(is_empty(newtags))
        return(NULL)

    res <- lapply(newtags, function(tag)
        do_training_op(project, "tags", options=list(name=tag), http_verb="POST"))

    do.call(rbind.data.frame, c(stringsAsFactors=FALSE,
        lapply(res, function(x) x[c("name", "id")]))
    )
}


#' @rdname customvision_tags
#' @export
add_negative_tag <- function(project, negative_name="_negative_")
{
    taglist <- list_tags(project, as="dataframe")

    if(any(taglist$type == "Negative"))
    {
        warning("Project already has a negative tag", call.=FALSE)
        return(NULL)
    }

    res <- if(negative_name %in% taglist$name)
    {
        tagid <- taglist$id[which(negative_name == taglist$name)]
        do_training_op(project, file.path("tags", tagid), body=list(type="Negative"), http_verb="PATCH")
    }
    else do_training_op(project, "tags", options=list(name=negative_name, type="Negative"), http_verb="POST")

    data.frame(res[c("name", "id")], stringsAsFactors=FALSE)
}


#' @rdname customvision_tags
#' @export
list_tags <- function(project, as=c("names", "ids", "dataframe", "list"), iteration=NULL)
{
    as <- match.arg(as)
    if(as == "list")
        return(do_training_op(project, "tags", options(iterationId=iteration)))

    tags <- do_training_op(project, "tags", options(iterationId=iteration), simplifyVector=TRUE)
    if(as == "names")
        tags$name
    else if(as == "ids")
        tags$id
    else as.data.frame(tags)
}


#' @rdname customvision_tags
#' @export
get_tag <- function(project, name=NULL, id=NULL, iteration=NULL)
{
    if(is.null(id))
    {
        taglist <- list_tags(project, iteration=iteration, as="list")
        tagnames <- sapply(taglist, `[[`, "name")
        i <- which(name == tagnames)
        if(is_empty(i))
            stop(sprintf("Image tag '%s' not found", name), call.=FALSE)

        taglist[[i]]
    }
    else do_training_op(project, file.path("tags", id), options(iterationid=iteration))
}


#' @rdname customvision_tags
#' @export
remove_tags <- function(project, tags, confirm=TRUE)
{
    tags <- unique_tags(tags)
    if(!confirm_delete("Are you sure you want to remove tags from the project?", confirm))
        return(invisible(project))

    lapply(get_tag_ids_from_names(tags, project), function(tag)
        do_training_op(project, file.path("tags", tag), http_verb="DELETE"))

    invisible(NULL)
}


#' Tag and untag images uploaded to a project
#'
#' @param project a Custom Vision project.
#' @param tags For `add_image_tags`, the tag labels to add to the images. For `remove_image_tags`, the tags (either text labels or IDs) to remove from images. The default for untagging is to remove all assigned tags.
#' @param image_ids The IDs of the images to tag or untag.
#' @details
#' `add_image_tags` is for tagging images that were uploaded previously, while `remove_image_tags` untags them. Adding tags does not remove previously assigned ones. Similarly, removing one tag from an image leaves any other tags intact.
#'
#' Tags can be specified in the following ways:
#' - As a single character string. In this case, the tag will be applied to all image IDs.
#' - As a vector of strings, with length equal to the length of `image_ids`. The tags will be applied to the images in order.
#' - As a list of vectors of strings, with the length of the list equal to the length of `image_ids`. Each vector in the list contains the tags to be assigned to the corresponding image.
#' @return
#' The vector of IDs for the images affected, invisibly.
#' @seealso
#' [`add_images`], [`list_tags`]
#' @rdname customvision_image_tags
#' @aliases customvision_image_tags
#' @export
add_image_tags <- function(project, image_ids=list_images(project, "untagged", as="ids"), tags)
{
    if(length(tags) != length(image_ids) && length(tags) != 1)
        stop("Must supply tags for each image", call.=FALSE)

    if(!all(sapply(image_ids, is_guid)))
        stop("Must provide GUIDs of images to be tagged", call.=FALSE)

    unique_tags <- unique_tags(tags)
    add_tags(project, unique_tags)
    tag_ids <- get_tag_ids_from_names(unique_tags, project)

    if(length(tags) == 1)
        tags <- rep(tags, length(image_ids))

    req_list <- lapply(seq_along(unique_tags), function(i)
    {
        this_set <- sapply(tags, function(tags_j) unique_tags[i] %in% tags_j)
        if(!is_empty(this_set))
            data.frame(imageId=image_ids[this_set], tagId=tag_ids[i], stringsAsFactors=FALSE)
        else NULL
    })

    # do by pages, to be safe
    while(!is_empty(req_list))
    {
        idx <- seq_len(min(length(req_list), 64))
        do_training_op(project, "images/tags", body=list(tags=do.call(rbind, req_list[idx])), http_verb="POST")
        req_list <- req_list[-idx]
    }
    invisible(image_ids)
}


#' @rdname customvision_image_tags
#' @export
remove_image_tags <- function(project, image_ids=list_images(project, "tagged", as="ids"),
                              tags=list_tags(project, as="ids"))
{
    if(!all(sapply(image_ids, is_guid)))
        stop("Must provide GUIDs of images to be untagged", call.=FALSE)

    if(!all(sapply(tags, is_guid)))
    {
        tagdf <- list_tags(project, as="dataframe")[c("name", "id")]
        tags <- tagdf$id[match(tags, tagdf$name)]
    }

    tmp_imgs <- image_ids
    while(!is_empty(tmp_imgs))
    {
        idx <- seq_len(min(length(tmp_imgs), 64))
        opts <- list(
            imageIds=paste0(tmp_imgs[idx], collapse=","),
            tagIds=paste0(tags, collapse=",")
        )
        do_training_op(project, "images/tags", options=opts, http_verb="DELETE")
        tmp_imgs <- tmp_imgs[-idx]
    }
    invisible(image_ids)
}

