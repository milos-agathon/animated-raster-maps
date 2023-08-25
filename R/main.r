#######################################################
#                 Animate rasters with R
#                 Milos Popovic
#                 2023/06/29
########################################################

remotes::install_github(
    "dieghernan/tidyterra"
)

libs <- c(
    "tidyverse", "terra", "tidyterra",
    "osmdata", "sf", "ggmap", "classInt",
    "gifski"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libraries == F)){
    install.packages(
        libs[!installed_libraries]
    )
}

invisible(lapply(
    libs, library, character.only = T
))

### 1. GET POPULATION DATA
### ----------------------
years <- seq(1975, 2030, by = 5)

urls <- paste0(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E",
    years,
    "_GLOBE_R2023A_4326_3ss/V1-0/tiles/GHS_POP_E",
    years, "_GLOBE_R2023A_4326_3ss_V1_0_R5_C21.zip"
)

for (url in urls) {
    download.file(
        url,
        destfile = basename(url),
        mode = "wb"
    )
}

fnames <- list.files(
    path = getwd(),
    pattern = ".zip",
    full.names = T
)

lapply(fnames, unzip)

raster_files <- list.files(
    path = getwd(),
    pattern = ".tif",
    full.names = T
)

pop_rasters <- lapply(
    raster_files, terra::rast
)

### 2. CROP ISTANBUL AREA
### ---------------------
crs_longlat <- "+proj=longlat +datum=WGS84 +no_defs"

city <- "Istanbul, Turkey"
istanbul_border <- osmdata::getbb(
    city
)

get_bbox <- function() {
    bb <- sf::st_sfc(
        sf::st_polygon(
            list(
                cbind(
                    c(
                        istanbul_border[[1]],
                        istanbul_border[[3]],
                        istanbul_border[[3]],
                        istanbul_border[[1]],
                        istanbul_border[[1]]
                    ),
                    c(
                        istanbul_border[[2]],
                        istanbul_border[[2]],
                        istanbul_border[[4]],
                        istanbul_border[[4]],
                        istanbul_border[[2]]
                    )
                )
            )
        ),
        crs = crs_longlat
    )

    return(bb)
}

bb <- get_bbox()

cropped_rasters <- lapply(
    pop_rasters,
    terra::crop,
    terra::vect(bb)
)

layer1 <- cropped_rasters[[1]]
ggplot() +
    tidyterra::geom_spatraster(
        data = layer1
    ) +
    geom_sf(
        data = bb,
        fill = "transparent",
        color = "red",
        size = .5
    ) +
    theme_void()

### 2. REMOVE 0S
### ------------

istanbul_rasters <- lapply(
    cropped_rasters,
    function(x) {
        terra::ifel(
            x == 0, NA, x
        )
    }
)

### 3. BREAKS
### ---------

istanbul_stacked <- terra::rast(
    istanbul_rasters
)

pop_values <- terra::values(
    istanbul_stacked,
    na.rm = T
)

min_pop <- min(pop_values)
max_pop <- max(pop_values)

breaks <- classInt::classIntervals(
    pop_values,
    n = 6,
    style = "fisher"
)$brks

### 4. STREET OVERLAY
### -----------------

istanbul_stamen <- ggmap::get_stamenmap(
    istanbul_border,
    zoom = 12,
    maptype = "toner-lines"
)

ggmap::ggmap(
    istanbul_stamen
) +
    tidyterra::geom_spatraster(
        data = layer1
    ) +
    scale_fill_gradientn(
        colours = hcl.colors(
            20, "viridis",
            alpha = .8
        ),
        na.value = NA
    ) +
    theme_void()

### 5. ANIMATE MAP
### --------------

names(istanbul_stacked) <- years
istanbul_layers <- names(istanbul_stacked)

for (
    i in seq_len(
        length(istanbul_layers)
    )
) {
    istanbul_layer <- istanbul_layers[i]
    istanbul_rast_year <- istanbul_stacked |>
        dplyr::select(
            dplyr::all_of(
                istanbul_layer
            )
        )
    Year <- as.numeric(
        istanbul_layer
    )

    p <- ggmap::ggmap(
        istanbul_stamen
    ) +
        tidyterra::geom_spatraster(
            data = istanbul_rast_year
        ) +
        scale_fill_gradientn(
            name = "",
            colours = hcl.colors(
                20, "viridis",
                alpha = .8
            ),
            breaks = breaks,
            labels = round(breaks, 0),
            limits = c(min_pop, max_pop),
            na.value = NA
        ) +
        guides(
            fill = guide_legend(
                direction = "horizontal",
                keyheight = unit(1.25, "mm"),
                keywidth = unit(15, "mm"),
                label.position = "bottom",
                label.hjust = .5,
                nrow = 1,
                byrow = T
            )
        ) +
        theme_void() +
        theme(
            legend.position = c(
                c(.5, .3)
            ),
            legend.text = element_text(
                size = 11, color = "grey10"
            ),
            plot.title = element_text(
                size = 20, color = "grey10",
                hjust = .5, vjust = 5
            ),
            plot.subtitle = element_text(
                size = 40, color = hcl.colors(
                    1, "viridis"
                ),
                hjust = .5, vjust = 3
            ),
            plot.caption = element_text(
                size = 10, color = "grey40",
                hjust = .5, vjust = 40
            ),
            plot.margin = unit(
                c(
                    t = 2, b = -5,
                    l = -5, r = -5
                ), "lines"
            )
        ) +
        labs(
            title = "Population in Istanbul (1975-2030)",
            subtitle = Year,
            caption = "Data: GHSL - Global Human Settlement Layer"
        )

    map_name <- file.path(
        getwd(), paste0(
            istanbul_layer,
            ".png"
        )
    )

    ggsave(
        map_name, p,
        width = 6, height = 8.5,
        units = "in", bg = "white"
    )
}

map_files <- file.path(
    getwd(), paste0(
        istanbul_layers,
        ".png"
    )
)

gifski::gifski(
    map_files,
    loop = T,
    delay = .5,
    width = 1200,
    height = 1200,
    gif_file = "istanbul-population.gif"
)

