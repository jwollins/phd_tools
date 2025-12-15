## HEADER ####
## who: J Collins
## what: Experimental design script (3 crops x 2 irrigation x 2 tillage; RCBD)
## when:

## 00 Setup ####

some_packages <- c("tidyverse", "agricolae", "desplot")
invisible(lapply(some_packages, library, character.only = TRUE))

## Experiment Parameters ####

# 3 crops
TrtCrop   <- paste0("C", 1:2)    # C1 - C3
n_TrtCrop <- n_distinct(TrtCrop) # 3

# 2 irrigation schedules
TrtIrrig   <- paste0("I", 1:2)    # I1 - I2
n_TrtIrrig <- n_distinct(TrtIrrig) # 2

# 2 tillage systems
TrtTill   <- paste0("T", 1:2)    # T1 - T2
n_TrtTill <- n_distinct(TrtTill) # 2

# number of replicates (blocks)
n_Reps <- 4


## RCBD Factorial (3 factors) ####
# agricolae::design.ab uses A, B, C as factor indices

fac3rcbd_out <- design.ab(
  trt    = c(n_TrtCrop, n_TrtIrrig, n_TrtTill),
  design = "rcbd",
  r      = n_Reps,
  seed   = 42
)

# Add Row and Col: each block = one row, 12 plots per block = columns
fac3rcbd_out$bookRowCol <- fac3rcbd_out$book %>%
  mutate(
    A = as.integer(as.character(A)),
    B = as.integer(as.character(B)),
    C = as.integer(as.character(C)),
    block = as.integer(as.character(block))
  ) %>%
  mutate(
    TrtCrop  = TrtCrop[A],
    TrtIrrig = TrtIrrig[B],
    TrtTill  = TrtTill[C],
    Label    = paste(TrtCrop, TrtIrrig, TrtTill, sep = "-")
  ) %>%
  group_by(block) %>%
  mutate(
    Row = block,
    Col = row_number()
  ) %>%
  ungroup()

# Plot field layout (block strips)
desplot(Label ~ Col + Row, flip = TRUE,
        text = Label, cex = 0.8, shorten = "no",
        out1 = block,
        data = fac3rcbd_out$bookRowCol,
        main = "RCBD: 2 crops (C) × 2 irrigation (I) × 2 tillage (T) (3 reps)",
        show.key = TRUE, key.cex = 0.6)


plot_w <- 2   # meters per column
plot_l <- 6   # meters per row

fac3rcbd_out$bookRowCol <- fac3rcbd_out$bookRowCol %>%
  mutate(
    x_m = Col * plot_w,
    y_m = Row * plot_l
  )









library(tidyverse)

plot_w <- 3   # meters per plot (x direction, within block)
plot_l <- 3   # meters per plot (y direction, block-to-block strip thickness)
alley_x <- 1  # meters between plots within a block (set e.g. 0.5 if you have alleys)
alley_y <- 6  # meters between blocks (set e.g. 1 if you have an alley between blocks)

layout_m <- fac3rcbd_out$bookRowCol %>%
  # Define block origin and plot origin in meters
  mutate(
    block_y0 = (Row - 1) * (plot_l + alley_y),
    x0 = (Col - 1) * (plot_w + alley_x),
    y0 = block_y0,
    xmin = x0,
    xmax = x0 + plot_w,
    ymin = y0,
    ymax = y0 + plot_l,
    x_center = (xmin + xmax) / 2,
    y_center = (ymin + ymax) / 2
  )

layout_m <- layout_m %>%
  mutate(
    TrtCombo = paste(TrtCrop, TrtIrrig, TrtTill, sep = " × ")
  )

p_field <- ggplot(layout_m) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = TrtCombo),
    color = "grey30", linewidth = 0.3
  ) +
  geom_text(aes(x = x_center, y = y_center, label = Label), size = 3) +
  coord_equal(expand = FALSE) +
  scale_x_continuous(
    name = "Width (m)",
    breaks = seq(0, ceiling(max(layout_m$xmax) / 3) * 3, by = 3)) +
  scale_y_continuous(
    name = "Length (m)",
    breaks = seq(0, ceiling(max(layout_m$xmax) / 3) * 3, by = 3)) +
  labs(
    title = "ERP 3 Hub experiment RCBD layout",
    fill = "Crop (C) × Irrigation (I) × Tillage (T)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_field





