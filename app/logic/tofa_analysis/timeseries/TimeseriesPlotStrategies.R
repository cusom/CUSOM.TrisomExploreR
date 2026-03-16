box::use(
    R6[R6Class],
    glue[glue],
    tibble[tibble],
    dplyr[select, mutate, group_by, summarise, ungroup, rename_with, distinct, n,
        filter, pull, arrange, dense_rank, row_number, n_distinct, inner_join],
    purrr[pmap],
    stringr[str_split_1],
    rlang[sym],
    plotly[plot_ly, layout, config, highlight_key, add_boxplot, add_markers, add_paths, highlight, attrs_selected],
    htmlwidgets[onRender],
)


#' @export
BasePlotStrategy <- R6Class(
    "BasePlotStrategy",
    private = list(),
    active = list(
        participant_event_counts = function(value) {
            return(
                self$source_data |>
                    group_by(Event_Name) |>
                    summarise(n = n_distinct(Internal_ParticipantID)) 
            )
        },
        
        x_var_name = function(value) {
            return("Event_Name")
        },
        x_vals = function(value) {
            return(
                self$source_data |>
                    select(!!sym(self$x_var_name)) |>
                    inner_join(self$participant_event_counts, by = self$x_var_name) |>
                    mutate(label = glue(
                        "{Event_Name}
                        (n={n})"
                    )) |>
                    pull(label)

            )
        },
        x_axis_labels = function(value) {
            return(
                self$x_vals |>
                    unique()
            )
        },
        y_vals = function(value) {
            return(
                self$source_data |>
                    select(Value) |>
                    pull()
            )
        },
        y_axis_title = function(value) {
            return(self$feature)
        },
        plot_title = function(value) {
            return(glue("{self$feature}: Baseline vs. TOFA"))
        },
        event_colors = function(value) {
            return(c(
                "Baseline" = "#999999",
                "2 week" = "#c6dbef",
                "8 week" = "#9ecae1",
                "16 week" = "#6baed6",
                "40 week" = "#4292c6"
            ))
        },
        shapes = function(value) {
            return(NULL)
        }
    ),
    public = list(
        source_data = NULL,
        feature = NULL,
        highlight_group = NULL,
        initialize = function(type, dataset, cohort, feature, ...) {
            self$feature <- feature
            feature_tag <- gsub("[^A-Za-z0-9]+", "_", as.character(feature))
            self$highlight_group <- glue("participant_id_{type}_{feature_tag}")
        },
        set_source_data = function(data) {
            self$source_data <- data
            return(invisible(self$source_data))
        },
        render = function(prepared_data) {
            self$set_source_data(prepared_data)

            plot_data <- self$source_data |>
                mutate(
                    x_label = self$x_vals,
                    y_plot = self$y_vals,
                    x_index = as.numeric(factor(x_label, levels = self$x_axis_labels)),
                    participant_index = as.numeric(factor(Internal_ParticipantID)),
                    event_index = as.numeric(factor(Event_Name)),
                    jitter_offset = ((((participant_index * 1103515245) + (event_index * 12345)) %% 997) / 997 - 0.5) * 0.5,
                    x_jitter = x_index + jitter_offset
                )

            hk <- highlight_key(plot_data, ~Internal_ParticipantID, group = self$highlight_group)
            path_data <- plot_data |>
                arrange(Internal_ParticipantID, x_index)
            hk_paths <- highlight_key(path_data, ~Internal_ParticipantID, group = self$highlight_group)

            plot_ly() |>
                add_boxplot(
                    data = plot_data,
                    x = ~x_index,
                    y = ~y_plot,
                    type = "box",
                    color = ~Event_Name,
                    colors = self$event_colors,
                    boxpoints = FALSE
                ) |>
                add_markers(
                    data = hk,
                    x = ~x_jitter,
                    y = ~y_plot,
                    color = ~Event_Name,
                    colors = self$event_colors,
                    key = ~Internal_ParticipantID,
                    text = ~text,
                    hoverinfo = "text",
                    marker = list(size = 6, opacity = 0.8)
                ) |>
                add_paths(
                    data = hk_paths,
                    x = ~x_jitter,
                    y = ~y_plot,
                    split = ~Internal_ParticipantID,
                    key = ~Internal_ParticipantID,
                    hoverinfo = "skip",
                    showlegend = FALSE,
                    line = list(
                        color = "rgba(255,0,0,0)",
                        width = 1,
                        dash = "dot"
                    )
                ) |>
                layout(
                    showlegend = FALSE,
                    legend = list(
                        orientation = "h",
                        x = 0.5,
                        xanchor = "center"
                    ),
                    title = list(
                        text = self$plot_title,
                        x = 0.05,
                        font = list(
                            family = "Noto Serif', serif",
                            size = 18,
                            color = "rgb(58, 62, 65)"
                        )
                    ),
                    xaxis = list(
                        title = "",
                        titlefont =  list(
                            family = "Arial",
                            color = "rgb(58, 62, 65)",
                            size = 18
                        ),
                        showgrid = FALSE,
                        zeroline = FALSE,
                        showline = TRUE,
                        showticklabels = TRUE,
                        tickmode = "array",
                        tickvals = seq_along(self$x_axis_labels),
                        ticktext = self$x_axis_labels,
                        range = c(0.5, length(self$x_axis_labels) + 0.5)
                    ),
                    yaxis = list(
                        title = self$y_axis_title,
                        titlefont = list(
                            family = "Arial",
                            color = "rgb(58, 62, 65)",
                            size = 18
                        ),
                        showgrid = FALSE,
                        zeroline = FALSE,
                        showline = TRUE,
                        showticklabels = TRUE
                    ),
                    shapes = self$shapes,
                    margin = list(
                        autoexpand = TRUE,
                        l = 50,
                        r = 50,
                        t = 50,
                        b = 50
                    )
                ) |> 
                highlight(
                    on = "plotly_click",
                    off = "plotly_doubleclick", 
                    persistent = FALSE,
                    dynamic = FALSE,
                    selectize = list(
                        placeholder = "Select participant to highlight"
                    ),
                    selected = attrs_selected(
                        line = list(
                            color = "rgba(255,0,0,0.85)",
                            width = 2,
                            opacity = 0.9,
                            dash = "dot"
                        ),
                        marker = list(
                            color = "rgba(255,0,0,0.85)",
                            size = 14,
                            opacity = 1.0
                        )
                    ),
                    opacityDim = 0.4
                )
        }
    )
)


#' @export
DifferencePlotStrategy <- R6Class(
    "DifferencePlotStrategy",
    inherit = BasePlotStrategy,
    private = list(),
    active = list(
        y_axis_title = function(value) {
            return(glue("Difference from Baseline {self$feature}"))
        },
        plot_title = function(value) {
            return(glue("{self$feature}: Distribution of Differences from Baseline"))
        },
        shapes = function(value) {
            return(
                list(
                    type = "line",
                    xref = "paper",
                    x0 = 0,
                    x1 = 1,
                    y0 = 0,
                    y1 = 0,
                    line = list(
                        color = "rgb(58, 62, 65)",
                        width = 2,
                        dash = "dash"
                    )
                )
            )
        }
    ),
    public = list(
        initialize = function(type, dataset, cohort, feature, ...) {
            super$initialize(type, dataset, cohort, feature, ...)
        }
    )
)


