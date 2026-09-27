#!/usr/bin/env Rscript

# Figure 1. Programme theory of change: SCF policy-instrument mix to verified protection.
# Author: Oleksandr Skytchenko
# Arrows are programme hypotheses, not established causal effects.

required_packages <- c("ggplot2", "svglite")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  stop(
    "Install the required package(s) first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages(library(ggplot2))

author_name <- "Oleksandr Skytchenko"

pal <- list(
  bg = "#111111",
  panel = "#1D1D1D",
  graphite = "#555B65",
  mid = "#8A9099",
  light = "#D9DDE3",
  orange = "#F28E2B",
  white = "#F7F5F0"
)

box <- function(xmin, xmax, ymin, ymax, label, group, fill, colour = NA,
                text_colour = pal$white, size = 3.25, fontface = "plain") {
  data.frame(xmin, xmax, ymin, ymax, label, group, fill, colour,
             text_colour, size, fontface, stringsAsFactors = FALSE)
}

boxes <- do.call(rbind, list(
  box(0.4, 4.0, 7.20, 8.00, "Financial support", "existing", pal$graphite),
  box(0.4, 4.0, 6.15, 6.95, "One-stop shops:\ninformation and navigation", "existing", pal$graphite),
  box(0.4, 4.0, 5.10, 5.90, "Supplier and contractual\narrangements", "existing", pal$graphite),
  box(0.4, 4.0, 4.05, 4.85, "MIS, reporting, controls\nand evaluation", "existing", pal$graphite),

  box(4.65, 8.65, 7.20, 8.00, "Decision-critical indicators", "proposed", pal$orange,
      text_colour = pal$bg, fontface = "bold"),
  box(4.65, 8.65, 6.15, 6.95, "Household and market safeguards", "proposed", "#6E4A1E"),
  box(4.65, 8.65, 5.10, 5.90, "Named owner, trigger, deadline,\nremedy and verified closure", "proposed", "#6E4A1E"),
  box(4.65, 8.65, 4.05, 4.85, "Independent evaluation boundary\nand four cost ledgers", "proposed", "#6E4A1E"),

  box(9.35, 13.6, 7.20, 8.00, "SHORT TERM\nAccess, control, commissioning, fault closure", "outcome", "#2B2B2B"),
  box(9.35, 13.6, 6.15, 6.95, "MEDIUM TERM\nAffordable use, supplier quality, responsive administration", "outcome", "#2B2B2B"),
  box(9.35, 13.6, 5.10, 5.90, "LONG TERM\nCredible indicators and maintained protection", "outcome", "#2B2B2B"),
  box(9.35, 13.6, 4.05, 4.85, "IMPACT\nAdditional and equitable protection\nper euro", "outcome", pal$white,
      text_colour = pal$bg, size = 2.95, fontface = "bold")
))

cycle <- data.frame(
  x = seq(1.0, 12.8, length.out = 6),
  y = rep(2.95, 6),
  label = c("SENSE", "VERIFY", "DIAGNOSE", "CORRECT", "EVALUATE", "ADAPT"),
  stringsAsFactors = FALSE
)

arrows_main <- data.frame(
  x = c(4.05, 8.70, 11.50),
  xend = c(4.55, 9.25, 11.50),
  y = c(6.03, 6.03, 4.00),
  yend = c(6.03, 6.03, 3.37)
)

cycle_arrows <- data.frame(
  x = head(cycle$x, -1) + 0.46,
  xend = tail(cycle$x, -1) - 0.46,
  y = 2.95,
  yend = 2.95
)

toolbox <- data.frame(
  xmin = c(0.4, 4.85, 9.35),
  xmax = c(4.25, 8.70, 13.6),
  ymin = c(0.50, 0.50, 0.50),
  ymax = c(1.35, 1.35, 1.35),
  label = c(
    "ALL-VALENCE LEARNING\nPositive · null · adverse\nRepair · NO-GO",
    "MODULAR TOOLBOX\nIndicators · safeguards\nWorkflows · decision rules",
    "CONDITIONAL ADAPTATION\nNew authority, law and market\nClimate and evidence test"
  ),
  fill = c("#242424", pal$orange, "#242424"),
  text_colour = c(pal$white, pal$bg, pal$white),
  stringsAsFactors = FALSE
)

p <- ggplot() +
  annotate("rect", xmin = 0.15, xmax = 13.85, ymin = 8.45, ymax = 8.52,
           fill = pal$orange, colour = NA) +
  annotate("text", x = 0.4, y = 8.78, label = "APPROVED SCF ARCHITECTURE",
           hjust = 0, colour = pal$light, fontface = "bold", size = 3.3) +
  annotate("text", x = 4.65, y = 8.78, label = "+ PROPOSED POLICY-INSTRUMENT MIX",
           hjust = 0, colour = pal$orange, fontface = "bold", size = 3.05) +
  annotate("text", x = 9.35, y = 8.78, label = "BEHAVIOUR AND OUTCOMES",
           hjust = 0, colour = pal$light, fontface = "bold", size = 3.3) +
  geom_rect(data = boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            colour = NA) +
  scale_fill_identity() +
  geom_text(data = boxes,
            aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2,
                label = label, colour = text_colour, size = size,
                fontface = fontface),
            lineheight = 0.92, show.legend = FALSE) +
  scale_colour_identity() +
  scale_size_identity() +
  geom_segment(data = arrows_main,
               aes(x = x, xend = xend, y = y, yend = yend),
               colour = pal$orange, linewidth = 1.05,
               arrow = arrow(type = "closed", length = grid::unit(0.16, "in"))) +
  annotate("rect", xmin = 0.45, xmax = 13.55, ymin = 2.30, ymax = 3.55,
           fill = "#191919", colour = pal$graphite, linewidth = 0.45) +
  annotate("text", x = 0.65, y = 3.36,
           label = "ADAPTIVE DELIVERY AND LEARNING CYCLE",
           hjust = 0, vjust = 1, colour = pal$orange,
           fontface = "bold", size = 3.2) +
  geom_segment(data = cycle_arrows,
               aes(x = x, xend = xend, y = y, yend = yend),
               colour = pal$mid, linewidth = 0.65,
               arrow = arrow(type = "closed", length = grid::unit(0.11, "in"))) +
  geom_point(data = cycle, aes(x = x, y = y), shape = 21, size = 8.2,
             stroke = 0.9, fill = pal$bg, colour = pal$orange) +
  geom_text(data = cycle, aes(x = x, y = y, label = label),
            colour = pal$white, fontface = "bold", size = 2.8) +
  annotate("text", x = 13.35, y = 3.37,
           label = "Stress-tested by heat, cold\nand service disruption",
           hjust = 1, vjust = 1, colour = pal$mid, size = 2.9,
           lineheight = 0.95) +
  geom_rect(data = toolbox,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            colour = NA) +
  geom_text(data = toolbox,
            aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2,
                label = label, colour = text_colour),
            fontface = "bold", size = 2.65, lineheight = 0.92,
            show.legend = FALSE) +
  geom_segment(aes(x = 4.28, xend = 4.75, y = 0.925, yend = 0.925),
               colour = pal$orange, linewidth = 0.9,
               arrow = arrow(type = "closed", length = grid::unit(0.13, "in"))) +
  geom_segment(aes(x = 8.73, xend = 9.25, y = 0.925, yend = 0.925),
               colour = pal$orange, linewidth = 0.9,
               arrow = arrow(type = "closed", length = grid::unit(0.13, "in"))) +
  annotate("text", x = 0.4, y = 1.70,
           label = "AFTER A DOCUMENTED GREEK LEARNING CYCLE",
           hjust = 0, colour = pal$light, fontface = "bold", size = 3.15) +
  coord_cartesian(xlim = c(0, 14), ylim = c(0.15, 9.15), clip = "off") +
  labs(
    title = "FROM SCF FINANCE TO VERIFIED PROTECTION",
    subtitle = "A proposed complementary policy-instrument mix operates through approved delivery channels",
    caption = paste0(
      "AUTHOR: ", toupper(author_name), " · AUTHOR'S DESIGN\n",
      "Programme theory of change.",
      "Mandatory SCF reporting duties remain unchanged; any later transfer requires a new legal, institutional, market, climate and evidence assessment."
    )
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = pal$bg, colour = NA),
    panel.background = element_rect(fill = pal$bg, colour = NA),
    plot.title = element_text(colour = pal$white, face = "bold", size = 19,
                              margin = margin(b = 7)),
    plot.subtitle = element_text(colour = pal$light, size = 11.5,
                                 margin = margin(b = 14)),
    plot.caption = element_text(colour = pal$mid, size = 8.4, hjust = 0,
                                margin = margin(t = 12)),
    plot.margin = margin(24, 28, 22, 28)
  )

# Optional command-line argument: the output directory.
# Example: Rscript scf_instrument_mix_toc.R "C:/PolicyPaper/figures"
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1 && nzchar(args[[1]])) {
  args[[1]]
} else {
  file.path(getwd(), "figures")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

file_stem <- "Figure_1_Protection_Conversion_Policy_Instrument_Mix"
pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf

ggsave(file.path(out_dir, paste0(file_stem, ".png")), p,
       width = 14, height = 9.2, units = "in", dpi = 300, bg = pal$bg)
ggsave(file.path(out_dir, paste0(file_stem, ".pdf")), p,
       width = 14, height = 9.2, units = "in", device = pdf_device, bg = pal$bg)
ggsave(file.path(out_dir, paste0(file_stem, ".svg")), p,
       width = 14, height = 9.2, units = "in", device = svglite::svglite, bg = pal$bg)

message(
  "Wrote Figure 1 as PNG, PDF and SVG to: ",
  normalizePath(out_dir, winslash = "/", mustWork = FALSE)
)
