list_tags <- function(project, iteration=NULL, name_only=TRUE)
{
    tags <- do_training_op(project, "tags", simplifyVector=TRUE)
    if(name_only)
        tags$name
    else tags
}


get_tag <- function(project, name=NULL, id=NULL, iteration=NULL)
{
    if(is.null(id))
    {
        tagdf <- list_tags(project, name_only=FALSE)
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


add_images <- function(project, images)
{
    all_files <- all(sapply(images, file.exists))
    all_urls <- all(sapply(images, is_any_uri))
    if(!all_files && !all_urls)
        stop("Must supply either a vector of all file names or all URLs", call.=FALSE)

    if(all_files)
        add_image_files(project, images)
    else add_image_urls(project, images)
}


add_image_files <- function(project, images)
{
    images <- lapply(images, function(img)
        list(name=img, contents=readBin(img, "raw", file.info(img)$size))
    )
    do_training_op(project, "images/files", body=list(images=images), encode="json", http_verb="POST")
}


add_image_urls <- function(project, images)
{
    images <- lapply(images, function(img)
        list(url=img)
    )
    do_training_op(project, "images/urls", body=list(images=images), http_verb="POST")
}



get_tag_id_by_name <- function(project, name=NULL, iteration=NULL)
{
    if(is.null(name))
        stop("Either tag name or ID must be supplied", call.=FALSE)

    lst <- list_tags(project, iteration=iteration, name_only=FALSE)
    id <- lst$id[lst$name == name]
    if(is_empty(id))
        stop(sprintf("Image tag '%s' not found", name), call.=FALSE)

    id
}

