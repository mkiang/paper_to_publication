## Imports ----
library(tidyverse)
library(readxl)
library(here)
source(here("code", "mk_nytimes.R"))

## Read raw data ----
pubs <- read_excel(here("data", "tracking_pubs_hashed.xlsx"))

## Make labels ----
pub_labs <- pubs %>%
    group_by(paper) %>%
    summarize(
        time = max(date_delta),
        rejects = sum(grepl("Reject", event)),
        desk_rejects = sum(grepl("Reject without", event))
    ) %>%
    ungroup() %>%
    mutate(paper_id = case_when(grepl("\\<9695", paper) ~ 1,
                                grepl("\\<61f5", paper) ~ 2,
                                grepl("\\<3f31", paper) ~ 3,
                                grepl("\\<7690", paper) ~ 4 
                                )) |> 
    mutate(
        paper_lab = sprintf(
            "Total time: %i days \n %i rejection%s (%i at the desk)",
            time,
            rejects,
            ifelse(rejects > 1, "s", ""),
            desk_rejects
        )
    ) %>%
    arrange(paper_id)

## Clean up ----
pubs <- pubs %>%
    left_join(pub_labs) %>%
    mutate(paper_cat = factor(paper_lab,
                              levels = pub_labs$paper_lab,
                              ordered = TRUE)) %>%
    ## Make a factor version (need ordering for color palette)
    mutate(event_cat = factor(
        event,
        levels = c(
            "Initial commit",
            "First draft",
            "Submitted",
            "Submitted after appeal",
            "Reject without review",
            "Reject after review",
            "Revise and resubmit",
            "Resubmitted",
            "Accepted",
            "Published"
        ),
        ordered = TRUE
    )) %>%
    ## Y-axis for keeping things mostly parallel
    mutate(y_axis = case_when(
        event %in% c("Submitted", "Submitted after appeal", "Resubmitted") ~ -1,
        event %in% c(
            "Reject without review",
            "Reject after review",
            "Revise and resubmit",
            "Initial commit",
            "First draft"
        ) ~ 0,
        event %in% c("Accepted", "Published") ~ 1
    )) %>%
    ## Y-axis for showing rejections more clearly
    mutate(y_axis_alt = case_when(
        event %in% c("Initial commit",
                     "First draft") ~ 0,
        event %in% c(
            "Submitted",
            "Submitted after appeal",
            "Reject without review",
            "Reject after review",
            "Revise and resubmit",
            "Resubmitted"
        ) ~ -1 * counter,
        event %in% c("Accepted", "Published") ~ 1
    )) |> 
    arrange(paper_id, date_delta)

## Make a color palette and shapes ----
event_col <-
    c(
        "black",
        "#313695",
        "#FDAE61",
        "#FDAE61",
        "#A50026",
        "#D73027",
        "#7FBC41",
        "#7FBC41",
        "#4D9221",
        "#276419"
    )

event_shapes <-
    c(5,
      18,
      19,
      1,
      4,
      8,
      10,
      19,
      17,
      15)

## Build out 1: Paper 1, step 1 ----
p1 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1) & date_delta < 1)
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step(data = pubs |> filter(date_delta < 1)) +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat, scales = "free") +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(0, .9), guide = "none")

## Build out 2: Paper 1, step 2 ----
p2 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1) & date_delta < 18)
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step(data = pubs |> filter(date_delta < 18)) +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat, scales = "free") +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(0, .9), guide = "none")

## Build out 3: Paper 1, step 3 ----
p3 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1) & date_delta < 37)
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step(data = pubs |> filter(date_delta < 37)) +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat, scales = "free") +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(0, .9), guide = "none")

## Build out 4: Paper 1, step 4 ----
p4 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1) & date_delta <= 49)
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step(data = pubs |> filter(date_delta < 50)) +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat, scales = "free") +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(0, .9), guide = "none")

## Build out 5: Paper 1, step 5 ----
p5 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1) & date_delta <= 51)
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step(data = pubs |> filter(date_delta < 52)) +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat, scales = "free") +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(0, .9), guide = "none")

## Build out 6: Paper 1, step 6 ----
p6 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1) & date_delta <= 79)
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step(data = pubs |> filter(date_delta < 80)) +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat, scales = "free") +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(0, .9), guide = "none")

## Build out 7: Paper 1, step 7 ----
p7 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1) & date_delta <= 95)
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step(data = pubs |> filter(date_delta < 96)) +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat, scales = "free") +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(0, .9), guide = "none")

## Build out 8: Paper 1, step 8 ----
p8 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1) & date_delta <= 110)
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step(data = pubs |> filter(date_delta < 111)) +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat, scales = "free") +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(0, .9), guide = "none")

## Build out 9: Paper 1, step 9 ----
p9 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1) & date_delta <= 133)
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step(data = pubs |> filter(date_delta < 134)) +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat, scales = "free") +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(0, .9), guide = "none")

## Final: all papers ----
p10 <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1:4))
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step() +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat, scales = "free") +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(.9, .9), guide = "none")

## Final: all papers fixed ----
p10fixed <- ggplot(
    pubs,
    aes(
        x = date_delta,
        y = y_axis_alt,
        color = event_cat,
        group = paper_cat,
        shape = event_cat,
        alpha = (paper_id %in% c(1:4))
    )
) +
    annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = seq(1.5, -7.5, -2),
        ymax = seq(0.5, -8.5, -2),
        alpha = .2
    ) +
    geom_step() +
    geom_point(size = 3.5) +
    facet_grid(~paper_cat) +
    mk_nytimes(
        legend.position = "bottom",
        legend.justification = .5,
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(
        "Where's the manuscript?",
        breaks = c(-8:1),
        labels = c(
            "With Journal\n(8th time)",
            rep("", 2),
            "With Journal\n(5th time)",
            rep("", 3),
            "With Journal\n(1st time)",
            "With Me",
            "Out"
        ),
        expand = c(0, .3)
    ) +
    scale_x_continuous("Days since first code commit") +
    scale_color_manual(NULL, values = event_col, drop = FALSE) +
    scale_shape_manual(NULL, values = event_shapes, drop = FALSE) +
    scale_alpha_manual(NULL, values = c(.9, .9), guide = "none")


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
ggsave_quick("01_papers_buildout_1.pdf", p1)
ggsave_quick("02_papers_buildout_2.pdf", p2)
ggsave_quick("03_papers_buildout_3.pdf", p3)
ggsave_quick("04_papers_buildout_4.pdf", p4)
ggsave_quick("05_papers_buildout_5.pdf", p5)
ggsave_quick("06_papers_buildout_6.pdf", p6)
ggsave_quick("07_papers_buildout_7.pdf", p7)
ggsave_quick("08_papers_buildout_8.pdf", p8)
ggsave_quick("09_papers_buildout_9.pdf", p9)
ggsave_quick("10_papers_buildout_10.pdf", p10)
ggsave_quick("10_papers_buildout_10fixed.pdf", p10fixed)
