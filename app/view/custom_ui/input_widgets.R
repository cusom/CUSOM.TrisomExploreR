box::use(
    htmltools[tags, div, validateCssUnit],
    shinyWidgets[prettyRadioButtons],
    shiny[restoreInput],
    purrr[pmap]
)

box::use(
    app/view/custom_ui/feedback_tools[createTooltip]
)

#' @export
prettyRadioButtonsFieldSet <- function(
    input_id,
    field_set_data,
    label,
    selected = NULL,
    status = "primary",
    shape = c("round", "square", "curve"),
    outline = FALSE,
    fill = FALSE,
    thick = FALSE,
    animation = NULL,
    icon = NULL,
    plain = FALSE,
    bigger = FALSE,
    inline = FALSE,
    width = NULL
) {

    status <- match.arg(status, c("default", "primary", "success", "info", "danger", "warning"))

    shape <- match.arg(shape)

    selected <- shiny::restoreInput(id = input_id, default = selected)

    options <- htmltools::tags$div(
        sapply(
            as.list(unique(field_set_data$FieldSet)),
            create_field_set,
            field_set_data = field_set_data,
            input_id = input_id,
            label = label,
            selected = selected,
            status = status,
            shape = shape,
            outline = outline,
            fill = fill,
            thick = thick,
            animation = animation,
            icon = icon,
            plain = plain,
            bigger = bigger,
            inline = inline,
            width = width
        )
    )

    divClass <- "form-group shiny-input-radiogroup shiny-input-container"

    if (inline) {
        divClass <- paste(divClass, "shiny-input-container-inline")
    }

    radioTag <- htmltools::tags$div(
        id = input_id,
        style = if (!is.null(width)) paste0("width: ", htmltools::validateCssUnit(width), ";"),
        class = divClass,
        htmltools::tags$label(class = "control-label", `for` = input_id, class = if (is.null(label)) "shiny-label-null", label),
        options
    )

    shinyWidgets:::attachShinyWidgetsDep(radioTag, "pretty")
}


#' Create tag list of field sets
#'
#' @param fieldSetName string - name of grouped set of radio buttons
#' @param fieldSetData tibble - tibble of values to create radio button
#' @param ... dots to accomodate additiaon arguments passed when called from parent function
#' @importFrom shiny tagList
#' @importFrom dplyr filter
#' @importFrom dplyr select
create_field_set <- function(field_set_name, field_set_data, ...) {

    radioChoices <- field_set_data |>
        dplyr::filter(FieldSet == field_set_name) |>
        dplyr::select(-FieldSet)

    return(
        shiny::tagList(
            htmltools::tags$div(
                htmltools::tags$fieldset(
                    htmltools::tags$b(shiny::HTML(field_set_name)),
                    getRadioButton(radioChoices, ...)
                )
            )
        )
    )
}

#' get a pretty radio button with HTML tooltip choice names, values
#'
#' @param radioChoices tibble - containing required args to create HTML version of choice names list 
#' @param ... dots to accomodate additional arguments passed when called from parent function
#' @import  shinyWidgets
getRadioButton <- function(radioChoices, ...) {

    args <- list(...)

    return(
        shinyWidgets:::generatePretty(
            inputId = args$input_id,
            selected = args$selected,
            inline = args$inline,
            type = "radio",
            choiceNames = purrr::pmap(radioChoices, createTooltip),
            choiceValues = radioChoices$Values,
            status = args$status,
            shape = args$shape,
            outline = args$outline,
            fill = args$fill,
            thick = args$thick,
            animation = args$animation,
            icon = args$icon,
            plain = args$plain,
            bigger = args$bigger
        )
    )
}



#' @export
dfToTree <- function(
    df,
    hierarchy = colnames(df)){ 
    l <- df
    for(c in hierarchy){
        l <- dfrapply(
        list = l,
        f = function(x){
            split(x, x[[c]], drop = TRUE)
        }
        )
    }
    dfrapply(l, function(x){""})
}

#' @export
treeToDf <- function(tree, hierarchy){
    depth <- depth(tree) 
    
    if(depth > length(hierarchy)){
        stop("Not enough names specified in hierarchy.")
    }
    
    if(depth < length(hierarchy)){
        hierarchy <- utils::tail(hierarchy, depth)
        warning(sprintf("To many levels specified in hierarchy. Only using last %s: %s",
                        depth,
                        paste(hierarchy, collapse = ", ")
        )
        )
    }
    
    for(i in seq_len(length(hierarchy))){
        tree <- nodesToDf(tree, hierarchy)
        hierarchy <- hierarchy[-length(hierarchy)]
    }
    df <- tree
}

#' @export
treeToDf <- function(tree, hierarchy){
    depth <- depth(tree) 
    
    if(depth > length(hierarchy)){
        stop("Not enough names specified in hierarchy.")
    }
    
    if(depth < length(hierarchy)){
        hierarchy <- utils::tail(hierarchy, depth)
        warning(sprintf("To many levels specified in hierarchy. Only using last %s: %s",
                        depth,
                        paste(hierarchy, collapse = ", ")
        )
        )
    }
    
    for(i in seq_len(length(hierarchy))){
        tree <- nodesToDf(tree, hierarchy)
        hierarchy <- hierarchy[-length(hierarchy)]
    }
    df <- tree
}

stackList <- function(list, name){
    outputList <- lapply(names(list),
        function(x, name){
            if(inherits(list[[x]], "data.frame")){
            df <- list[[x]]
            df[,name] <- x
            } else {
            df <- data.frame(x, stringsAsFactors = FALSE)
            names(df) = name   
            }
            df
        },
        name = name)
    Reduce(function(x,y){merge(x,y,all=TRUE)}, outputList)
}

depth <- function(x){
    ifelse(is.list(x) && !is.data.frame(x), 1L + max(sapply(x, depth)), 0L)
}

dfrapply <- function(list, f, ...) {
    if (inherits(list, "data.frame")) {
        return(f(list, ...))
    }
    if (inherits(list, "list")) {
        return(lapply(list, function(x) dfrapply(x, f, ...)))
    }
    stop("List element must be either a data frame or another list")
}
