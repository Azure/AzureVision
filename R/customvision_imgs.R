list_tags <- function(project, iteration=NULL, as=c("names", "dataframe", "list"))
{
    as <- match.arg(as)
    if(as == "list")
        return(do_training_op(project, "tags"))

    tags <- do_training_op(project, "tags", simplifyVector=TRUE)
    if(as == "names")
        tags$name
    else as.data.frame(tags)
}


get_tag <- function(project, name=NULL, id=NULL, iteration=NULL)
{
    if(is.null(id))
    {
        tagdf <- list_tags(project, as="dataframe")
        tag <- tagdf[tagdf$name == name, ]
        if(nrow(tag) == 0)
            stop(sprintf("Image tag '%s' not found", name), call.=FALSE)

        tag <- as.list(tag)
        if(is.na(tag$description))
            tag["description"] <- list(NULL)
        tag
    }
    else do_training_op(project, file.path("tags", id))
}


add_images <- function(project, images, tags=NULL, regions=NULL)
{
    all_files <- all(sapply(images, file.exists))
    all_urls <- all(sapply(images, is_any_uri))
    if(!all_files && !all_urls)
        stop("Must supply either a vector of all file names or all URLs", call.=FALSE)

    if(all_files)
        add_image_files(project, images)
    else add_image_urls(project, images)

    if(!is_empty(tags))
    {
        add_tags(project, unique(as.character(tags)))
        tag_images(project, images, tags)
    }
    invisible(project)
}


add_tags <- function(project, tags)
{
    current_tags <- list_tags(project)
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
    taglist <- list_tags(project, as="list")

    if(any(sapply(taglist, `[[`, "type") == "Negative"))
    {
        warning("Project already has a negative tag", call.=FALSE)
        return(invisible(project))
    }

    tagnames <- sapply(taglist, `[[`, "name")
    if(negative_name %in% tagnames)
    {
        tagid <- taglist[[which(negative_name == tagnames)]]$id
        do_training_op(project, file.path("tags", tagid), body=list(type="Negative"), http_verb="PATCH")
    }
    else do_training_op(project, "tags", options=list(name=negative_name, type="Negative"), http_verb="POST")

    invisible(project)
}


tag_images <- function(project, images, tags)
{
}


add_image_files <- function(project, images)
{
    images <- lapply(images, function(img)
        list(name=img, contents=readBin(img, "raw", file.info(img)$size))
    )
    res <- do_training_op(project, "images/files", body=list(images=images), http_verb="POST")
    if(!res$isBatchSuccessful)
        warning("Not all images were successfully added", call.=FALSE)
}


add_image_urls <- function(project, images)
{
    images <- lapply(images, function(img)
        list(url=img)
    )
    res <- do_training_op(project, "images/urls", body=list(images=images), http_verb="POST")
    if(!res$isBatchSuccessful)
        warning("Not all images were successfully added", call.=FALSE)
}

