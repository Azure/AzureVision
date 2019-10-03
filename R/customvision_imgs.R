list_tags <- function(project, iteration=NULL, as=c("names", "ids", "dataframe", "list"))
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


add_images <- function(project, images, tags=NULL, regions=NULL)
{
    all_files <- all(file.exists(images))
    all_urls <- all(sapply(images, is_any_uri))
    if(!all_files && !all_urls)
        stop("Must supply either a vector of all file names or all URLs", call.=FALSE)

    imglist <- if(all_files)
        add_image_files(project, images)
    else add_image_urls(project, images)

    if(!is_empty(tags))
    {
        add_tags(project, unique(as.character(tags)))
        tags <- get_tag_ids_from_names(tags, list_tags(project, as="dataframe"))
        tag_uploaded_images(project, tags, sapply(imglist, function(x) x$image$id))
    }
    invisible(project)
}


add_tags <- function(project, tags)
{
    current_tags <- list_tags(project, as="names")
    newtags <- setdiff(tags, current_tags)
    if(!is_empty(newtags))
    {
        lapply(newtags, function(tag)
            do_training_op(project, "tags", options=list(name=tag), http_verb="POST"))
    }
    invisible(project)
}


add_negative_tag <- function(project, negative_name="_negative_")
{
    taglist <- list_tags(project, as="dataframe")

    if(any(taglist$type == "Negative"))
    {
        warning("Project already has a negative tag", call.=FALSE)
        return(invisible(project))
    }

    if(negative_name %in% taglist$name)
    {
        tagid <- taglist$id[which(negative_name == taglist$name)]
        do_training_op(project, file.path("tags", tagid), body=list(type="Negative"), http_verb="PATCH")
    }
    else do_training_op(project, "tags", options=list(name=negative_name, type="Negative"), http_verb="POST")

    invisible(project)
}


list_images <- function(project, include=c("tagged", "untagged", "both"), iteration=NULL,
                       as=c("ids", "dataframe", "list"))
{
    include <- match.arg(include)
    as <- match.arg(as)
    simplify <- as != "list"

    tagged_imgs <- if(include != "untagged")
        do_training_op(project, "images/tagged", options=list(iterationId=iteration), simplifyVector=simplify)
    else NULL

    untagged_imgs <- if(include != "tagged")
        do_training_op(project, "images/untagged", options=list(iterationId=iteration), simplifyVector=simplify)
    else NULL

    if(as == "ids")
        rbind(tagged_imgs, untagged_imgs)$id
    else if(as == "dataframe")
        rbind(tagged_imgs, untagged_imgs)
    else c(tagged_imgs, untagged_imgs)
}


tag_uploaded_images <- function(project, tags, images=list_images(project, "untagged", as="ids"))
{
    tags <- data.frame(imageId=images, tagId=tags, stringsAsFactors=FALSE)
    do_training_op(project, "images/tags", body=list(tags=tags), http_verb="POST")
    invisible(project)
}


untag_uploaded_images <- function(project, images=list_images(project, "tagged", as="ids"),
                                  tags=list_tags(project, as="ids"))
{
    images <- paste0(images, collapse=",")
    tags <- paste0(tags, collapse=",")
    opts <- list(imageIds=images, tagIds=tags)

    do_training_op(project, "images/tags", options=opts, http_verb="DELETE")
    invisible(project)
}


remove_images <- function(project, images=list_images(project, "untagged", as="ids"), confirm=TRUE)
{
    if(!confirm_delete("Are you sure you want to remove images from the project?", confirm))
        return(invisible(project))

    images <- paste0(images, collapse=",")
    do_training_op(project, "images", options=list(imageIds=images), http_verb="DELETE")
    invisible(project)
}


remove_tags <- function(project, tag_names=NULL, tag_ids=NULL, confirm=TRUE)
{
    if(!confirm_delete("Are you sure you want to remove tags from the project?", confirm))
        return(invisible(project))

    if(is.null(tag_names) && is.null(tag_ids))
        stop("Must provide either tag names or IDs", call.=FALSE)
    if(!is.null(tag_names) && !is.null(tag_ids))
        stop("Provide either tag names or IDs, not both", call.=FALSE)

    if(is.null(tag_ids))
        tag_ids <- get_tag_ids_from_names(tag_names, list_tags(project, as="dataframe"))

    lapply(tag_ids, function(tag)
        do_training_op(project, file.path("tags", tag), http_verb="DELETE"))

    invisible(project)
}


add_image_files <- function(project, images)
{
    images <- lapply(images, function(img)
        list(name=img, contents=readBin(img, "raw", file.info(img)$size))
    )
    res <- do_training_op(project, "images/files", body=list(images=images), http_verb="POST")
    if(!res$isBatchSuccessful)
        warning("Not all images were successfully added", call.=FALSE)

    res$images
}


add_image_urls <- function(project, images)
{
    images <- lapply(images, function(img)
        list(url=img)
    )
    res <- do_training_op(project, "images/urls", body=list(images=images), http_verb="POST")
    if(!res$isBatchSuccessful)
        warning("Not all images were successfully added", call.=FALSE)

    res$images
}


get_tag_ids_from_names <- function(tagnames, tagdf)
{
    unname(structure(tagdf$id, names=tagdf$name)[tagnames])
}

