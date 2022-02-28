# Define DO_colors

DO_colors <- c(
    default = "#379da7", # official teal-ish color
    mid = "#73bec6", # half-way between powderblue & official color
    light = "#b0e0e6", # powderblue
    website = "#45b4bf", # from website banner
    websafe = "#339999",
    orange = "#e06e3d", # use for disdriv?
    orange_mid = "#de9373",
    orange_light = "#ddb8ab",
    # standard DO colors +30 saturation
    sat = "#189dac",
    sat_mid = "#4fbfcc",
    sat_light = "#95e3ed"
)

usethis::use_data(DO_colors, overwrite = TRUE)
