## Imports ----
library(tidyverse)
library(here)
library(ggrepel)
library(ggblend)
source(here("code", "mk_nytimes.R"))

## CONSTANTS ----
journal_col <- c(
    "General interest" = "#0072B2",
    "Demography"       = "#CC79A7",
    "Epidemiology"     = "#009E73",
    "Clinical"         = "#D55E00",
    "Specialty"        = "#E69F00",
    "Broad Scope"      = "#56B4E9"
)

anchor_journals <- c(
    "JAMA",
    "NEJM", 
    "Nature",
    "Science", 
    "PLOS ONE", 
    "BMJ Open", 
    "Demography",
    "AJPH",
    "Addiction" 
)

## Read raw data ----
journals <- read_csv(here("data", "journal_data.csv"))

journals <- journals |>
    mutate(rejection_rate_pct = 100 - acceptance_rate_pct) |>
    mutate(journal_cat = factor(
        journal_category,
        levels = c(
            "general_interest",
            "demography",
            "epi_ph",
            "clinical",
            "specialty",
            "broad_scope"
        ),
        labels = c(
            "General interest",
            "Demography",
            "Epidemiology",
            "Clinical",
            "Specialty",
            "Broad Scope"
        ),
        ordered = TRUE
    ))




## Add jitter once so points stay consistent across buildout
set.seed(94304)
journals_pos <- journals |>
    filter(!is.na(rejection_rate_pct)) |>
    mutate(
        x_jit = vipor::offsetX(rejection_rate_pct, width = 0.45),
        is_anchor = journal_name %in% anchor_journals
    ) |>
    arrange(is_anchor)

anchor_pos <- journals_pos |> filter(is_anchor)

## Subsets for the final 2D slide ----
both_metrics <- journals |>
    filter(!is.na(time_to_first_decision_days) & !is.na(rejection_rate_pct)) |>
    mutate(is_anchor = journal_name %in% anchor_journals) |>
    arrange(is_anchor)
acc_only <- journals |>
    filter(is.na(time_to_first_decision_days) & !is.na(rejection_rate_pct))
time_only <- journals |>
    filter(!is.na(time_to_first_decision_days) & is.na(rejection_rate_pct))
anchor_2d <- both_metrics |> filter(is_anchor)

## Shared scales / aesthetics ----
## Tweak things here
y_lim <- c(55, 100)
x_lim_1d <- c(-2, 2)
point_size <- 3
anchor_alpha <- 1.0
non_anchor_a <- 0.55 
legend_dot_sz <- 4

repel_opts <- list(
    size = 3.4,
    seed = 94304,
    min.segment.length = 0,
    box.padding = 0.6,
    point.padding = unit(1.6, "lines"), 
    max.overlaps = Inf,
    show.legend = FALSE
)

## 1D-specific repel 
repel_opts_1d <- c(repel_opts, list(force_pull = 3, xlim = x_lim_1d))

## Minor nudges for primary journals
nudges_1d <- tribble(
    ~journal_name, ~nx, ~ny,
    "JAMA", -0.3, 3,
    "NEJM", 0, 3,
    "Nature", -0.4, -2.5,
    "Science", 0.3, 0.5,
    "AJPH", -0.3, 0,
    "Demography", 0.4, -2.5,
    "PLOS ONE", 0, 0,
    "BMJ Open", 0, 0,
    "Addiction", 0.3, 0,

    "Population and Development Review", -0.4, -2,
    "Journal of Marriage and Family", 0.4, -2,
    "Biodemography and Social Biology", -0.4, -1,
    "Epidemiology", 0.4, 1,
    "Vaccine", 0.4, -3,
    "PLOS Medicine", 0, 2,
    "JAMA Internal Medicine", 0.4, -1.5,
    "Journal of General Internal Medicine", 0.3, -2,
    "Stroke", -0.4, -1,
    "JAMA Oncology", 0.3, 2,
    "JAMA Pediatrics", -0.3, 2,
    "Health Affairs", 0.4, -2,
    "Journal of Clinical Epidemiology", -0.3, -2,

    "Nature Communications", -0.3, -2,
    "Nature Human Behaviour", -0.7, 1.8,
    "Science Advances", 0.4, 1,
    "PNAS", 0.4, -1
)

## Highlight some journals in each category for buildout ----
category_highlights <- list(
    "General interest" = c(
        "Nature", "Science", "Nature Communications",
        "Nature Human Behaviour", "Science Advances", "PNAS"
    ),
    "Demography" = c(
        "Demography", "Population and Development Review",
        "Journal of Marriage and Family", "Biodemography and Social Biology"
    ),
    "Epidemiology" = c("AJPH", "Journal of Clinical Epidemiology"),
    "Clinical" = c(
        "JAMA", "NEJM", "PLOS Medicine",
        "Journal of General Internal Medicine", "Stroke"
    ),
    "Specialty" = c(
        "Addiction", "Vaccine",
        "JAMA Oncology", "JAMA Pediatrics", "Health Affairs"
    )
)
dim_alpha <- 0.15 
nudges_2d <- tribble(
    ~journal_name, ~nx, ~ny,
    "JAMA", 0, 3,
    "NEJM", 5, 3,
    "Nature", -3, 0,
    "Science", 1, -1,
    "AJPH", 2, 3,
    "Demography", -7, 3,
    "PLOS ONE", 0, 0,
    "BMJ Open", 0, 0,
    "Addiction", 4, 0
)
anchor_pos <- anchor_pos |> left_join(nudges_1d, by = "journal_name")
anchor_2d <- anchor_2d |> left_join(nudges_2d, by = "journal_name")

## Legend for all
in_plot_legend <- list(
    guides(
        color = guide_legend(override.aes = list(
            size     = legend_dot_sz,
            alpha    = 1,
            shape    = 16, # solid circle
            linetype = 0 # suppress any line glyph from geom_rug
        )),
        alpha = "none"
    ),
    theme(
        legend.position          = "inside",
        legend.position.inside   = c(0.985, 0.015),
        legend.justification     = c(1, 0),
        legend.background        = element_rect(fill = "white", color = NA)
    )
)

## x axis for all
hidden_x_axis <- list(
    scale_x_continuous(
        "Time to first decision (days)",
        breaks = 0,
        labels = " ",
        limits = x_lim_1d
    ),
    theme(
        axis.title.x = element_text(color = "transparent"),
        axis.text.x  = element_text(color = "transparent")
    )
)

## Last slide
p_final <- ggplot(
    both_metrics,
    aes(
        x = time_to_first_decision_days,
        y = rejection_rate_pct,
        color = journal_cat,
        alpha = is_anchor
    )
) +
    (geom_point(size = point_size) |> blend("multiply")) +
    geom_rug(
        data = acc_only,
        aes(y = rejection_rate_pct, color = journal_cat),
        sides = "l",
        inherit.aes = FALSE,
        linewidth = 0.7,
        length = unit(0.0125, "npc"),
        alpha = non_anchor_a
    ) +
    geom_rug(
        data = time_only,
        aes(x = time_to_first_decision_days, color = journal_cat),
        sides = "b",
        inherit.aes = FALSE,
        linewidth = 0.7,
        length = unit(0.025, "npc"),
        alpha = non_anchor_a
    ) +
    do.call(
        geom_text_repel,
        c(
            list(
                data = anchor_2d, mapping = aes(label = journal_name),
                nudge_x = anchor_2d$nx, nudge_y = anchor_2d$ny
            ),
            repel_opts
        )
    ) +
    scale_color_manual(NULL, values = journal_col, drop = FALSE) +
    scale_alpha_manual(values = c(`FALSE` = non_anchor_a, `TRUE` = anchor_alpha)) +
    scale_x_continuous("Time to first decision (days)") +
    scale_y_continuous("Rejection rate (%)", limits = y_lim) +
    mk_nytimes() +
    in_plot_legend

## Anchor journals only, neutral grey ----
p1 <- ggplot(anchor_pos, aes(x = x_jit, y = rejection_rate_pct)) +
    geom_point(color = "grey45", size = point_size, alpha = anchor_alpha) +
    do.call(
        geom_text_repel,
        c(
            list(
                mapping = aes(label = journal_name),
                nudge_x = anchor_pos$nx, nudge_y = anchor_pos$ny
            ),
            repel_opts_1d
        )
    ) +
    scale_y_continuous("Rejection rate (%)", limits = y_lim) +
    mk_nytimes(panel.grid.major.x = element_blank()) +
    hidden_x_axis

## Anchor journals colored by category ----
p2 <- ggplot(
    anchor_pos,
    aes(x = x_jit, y = rejection_rate_pct, color = journal_cat)
) +
    geom_point(size = point_size, alpha = anchor_alpha) +
    do.call(
        geom_text_repel,
        c(
            list(
                mapping = aes(label = journal_name),
                nudge_x = anchor_pos$nx, nudge_y = anchor_pos$ny
            ),
            repel_opts_1d
        )
    ) +
    scale_color_manual(NULL, values = journal_col, drop = FALSE) +
    scale_y_continuous("Rejection rate (%)", limits = y_lim) +
    mk_nytimes(panel.grid.major.x = element_blank()) +
    hidden_x_axis +
    in_plot_legend

## Helper function for progressive reveal 
make_p3_step <- function(highlight_cat, revealed_cats = NULL, show_all = FALSE) {
    spotlight_journals <- category_highlights[[highlight_cat]]
    plot_data <- journals_pos |>
        mutate(
            in_revealed = if (show_all) {
                TRUE
            } else {
                as.character(journal_cat) %in% revealed_cats | is_anchor
            },
            in_highlight = as.character(journal_cat) == highlight_cat,
            display_alpha = if_else(in_highlight, anchor_alpha, dim_alpha)
        ) |>
        filter(in_revealed) |>
        arrange(in_highlight) # highlight drawn on top
    label_data <- plot_data |>
        filter(in_highlight & journal_name %in% spotlight_journals) |>
        left_join(nudges_1d, by = "journal_name")
    ggplot(
        plot_data,
        aes(x = x_jit, y = rejection_rate_pct, color = journal_cat)
    ) +
        (geom_point(aes(alpha = display_alpha), size = point_size) |>
            blend("multiply")) +
        do.call(
            geom_text_repel,
            c(
                list(
                    data = label_data, mapping = aes(label = journal_name),
                    nudge_x = label_data$nx, nudge_y = label_data$ny
                ),
                repel_opts_1d
            )
        ) +
        scale_color_manual(NULL, values = journal_col, drop = FALSE) +
        scale_alpha_identity(guide = "none") +
        scale_y_continuous("Rejection rate (%)", limits = y_lim) +
        mk_nytimes(panel.grid.major.x = element_blank()) +
        hidden_x_axis +
        in_plot_legend
}

## Save them all (order matters here)
p3_steps <- list(
    make_p3_step("General interest",
        revealed_cats = c("General interest")),
    make_p3_step("Epidemiology",
        revealed_cats = c("General interest", "Epidemiology")),
    make_p3_step("Clinical",
        revealed_cats = c("General interest", "Epidemiology", "Clinical")),
    make_p3_step("Demography", show_all = TRUE),
    make_p3_step("Specialty", show_all = TRUE)
)

## All journals bright, no labels
p_all <- ggplot(
    journals_pos,
    aes(x = x_jit, y = rejection_rate_pct, color = journal_cat)
) +
    (geom_point(size = point_size, alpha = 0.9) |> blend("multiply")) +
    scale_color_manual(NULL, values = journal_col, drop = FALSE) +
    scale_y_continuous("Rejection rate (%)", limits = y_lim) +
    mk_nytimes(panel.grid.major.x = element_blank()) +
    hidden_x_axis +
    in_plot_legend

## Save ----
ggsave_quick <- function(plot_name, plot_object) {
    ggsave(
        here("plots", plot_name),
        plot_object,
        width = 16,
        height = 8,
        scale = .55,
        device = grDevices::quartz,
        type = "pdf"
    )
}
ggsave_quick("11_journals_buildout_1.pdf", p1)
ggsave_quick("12_journals_buildout_2.pdf", p2)
ggsave_quick("13_journals_buildout_3.pdf", p3_steps[[1]])
ggsave_quick("14_journals_buildout_4.pdf", p3_steps[[2]])
ggsave_quick("15_journals_buildout_5.pdf", p3_steps[[3]])
ggsave_quick("16_journals_buildout_6.pdf", p3_steps[[4]])
ggsave_quick("17_journals_buildout_7.pdf", p3_steps[[5]])
ggsave_quick("18_journals_buildout_8.pdf", p_all)
ggsave_quick("19_journals_buildout_final.pdf", p_final)
ggsave(
    here("plots", "19_journals_buildout_final.jpg"),
    p_final,
    width = 16,
    height = 8,
    scale = .55,
    dpi = 300
)
