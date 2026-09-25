## Colombia Earthquake Population Estimates
## Luis de la Rua ## June 2026 ##
# SETTINGS ----

# clean workspace
rm(list=ls())
gc()

# Libraries
library(readxl)
library(sf)
library(dplyr)
library(tidyr)
library(tidyterra)
library(terra)
library(stringr)
library(arrow)
library(readr)
library(exactextractr)
library(openxlsx)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(ggrepel)

# Paths
folder <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/COL_Earthquake_082026/"
layers <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/COL_Earthquake_082026/layers/"
# other
country <- "col"

# 1. PREPARING INPUT DATA -------
## 1.1 IMPACTED ZONES FROM SHAKEMAP ----

### 1.1.2 Download and process the ShakeMap GIS archive for the M 7.4 - 5 km S of San José del Palmar, Colombia ----
# 2026-08-10 12:34:28 (UTC)4.844°N 76.242°W110.3 km depth event ----
url <- "https://earthquake.usgs.gov/pdl/products/urn:usgs-product:us:shakemap:us6000tjl2:1786372856458/contents/download/shape.zip"
download.file(url, destfile = paste0(layers,country,"_shakemap.zip"), mode = "wb")

unzip(paste0(layers,country,"_shakemap.zip"), exdir = paste0(layers,"shakemap_data"))

# Load the MMI intensity contours for spatial operations
shakemap_mmi <- st_read(paste0(layers,"shakemap_data/mi.shp"))
head(shakemap_mmi)
crs(shakemap_mmi)

# Reclass and dissolve to generate the impact zones
# 1. Reclassify and add labels
shakemap_reclassed <- shakemap_mmi|>
  mutate(
    # Create an ordinal numeric class for grouping and sorting
    mmi_class = case_when(
      PARAMVALUE < 4 ~ 3,
      PARAMVALUE >= 4 & PARAMVALUE < 5 ~ 4,
      PARAMVALUE >= 5 & PARAMVALUE < 6 ~ 5,
      PARAMVALUE >= 6 & PARAMVALUE < 7 ~ 6,
      PARAMVALUE >= 7 & PARAMVALUE < 8 ~ 7,
      PARAMVALUE >= 8 & PARAMVALUE < 9 ~ 8,
      PARAMVALUE >= 9 ~ 9,
      TRUE ~ NA_real_
    ),
    
    # Add English labels matching the provided image
    label_en = case_when(
      mmi_class == 3 ~ "II - III (Weak)",
      mmi_class == 4 ~ "IV (Light)",
      mmi_class == 5 ~ "V (Moderate)",
      mmi_class == 6 ~ "VI (Strong)",
      mmi_class == 7 ~ "VII (Very Strong)",
      mmi_class == 8 ~ "VIII (Severe)",
      mmi_class == 9 ~ "IX (Violent)",
      TRUE ~ "Unknown"
    ),
    
    # Add Spanish labels (Standard USGS MMI translations)
    label_es = case_when(
      mmi_class == 3 ~ "II - III (Débil)",
      mmi_class == 4 ~ "IV (Ligero)",
      mmi_class == 5 ~ "V (Moderado)",
      mmi_class == 6 ~ "VI (Fuerte)",
      mmi_class == 7 ~ "VII (Muy Fuerte)",
      mmi_class == 8 ~ "VIII (Severo)",
      mmi_class == 9 ~ "IX (Violento)",
      TRUE ~ "Desconocido"
    )
  )


# Dissolve dealing with topological errors -
# 1. Turn off strict spherical geometry processing
sf_use_s2(FALSE)

# 2. Re-run the dissolve with the zero-buffer trick
shakemap_dissolved_74 <- shakemap_reclassed |>
  # Force polygon reconstruction to eliminate crossed loops
  st_buffer(dist = 0) |> 
  group_by(mmi_class, label_en, label_es) |>
  summarize(geometry = st_union(geometry), .groups = "drop") |>
  # Optional: Clean up any weird multi-geometries created during the union
  st_cast("MULTIPOLYGON")

# Turn s2 back on if you need it for other global calculations later
sf_use_s2(TRUE)

st_write(shakemap_dissolved_74, paste0(layers,"shakemap_data/mi_reclassM74.gpkg"), append= F)


# ### 1.1.3 Download and process the ShakeMap GIS archive for the M 5.0 - 16 km W of San José del Palmar, Colombia----
# ### 2026-08-10 13:18:10 (UTC)4.897°N 76.386°W98.5 km depth event----
# url <- "https://earthquake.usgs.gov/pdl/products/urn:usgs-product:us:shakemap:us6000t7zc:1782426800273/contents/download/shape.zip"
# download.file(url, destfile = paste0(layers,"Colombia_shakemap.zip"), mode = "wb")
# 
# unzip(paste0(layers,"Colombia_shakemap.zip"), exdir = paste0(layers,"shakemap_data"))
# 
# # Load the MMI intensity contours for spatial operations
# shakemap_mmi <- st_read(paste0(layers,"shakemap_data/mi.shp"))
# head(shakemap_mmi)
# crs(shakemap_mmi)
# 
# # Reclass and dissolve to generate the impact zones
# # 1. Reclassify and add labels
# shakemap_reclassed <- shakemap_mmi|>
#   mutate(
#     # Create an ordinal numeric class for grouping and sorting
#     mmi_class = case_when(
#       PARAMVALUE < 4 ~ 3,
#       PARAMVALUE >= 4 & PARAMVALUE < 5 ~ 4,
#       PARAMVALUE >= 5 & PARAMVALUE < 6 ~ 5,
#       PARAMVALUE >= 6 & PARAMVALUE < 7 ~ 6,
#       PARAMVALUE >= 7 & PARAMVALUE < 8 ~ 7,
#       PARAMVALUE >= 8 & PARAMVALUE < 9 ~ 8,
#       PARAMVALUE >= 9 ~ 9,
#       TRUE ~ NA_real_
#     ),
#     
#     # Add English labels matching the provided image
#     label_en = case_when(
#       mmi_class == 3 ~ "II - III (Weak)",
#       mmi_class == 4 ~ "IV (Light)",
#       mmi_class == 5 ~ "V (Moderate)",
#       mmi_class == 6 ~ "VI (Strong)",
#       mmi_class == 7 ~ "VII (Very Strong)",
#       mmi_class == 8 ~ "VIII (Severe)",
#       mmi_class == 9 ~ "IX (Violent)",
#       TRUE ~ "Unknown"
#     ),
#     
#     # Add Spanish labels (Standard USGS MMI translations)
#     label_es = case_when(
#       mmi_class == 3 ~ "II - III (Débil)",
#       mmi_class == 4 ~ "IV (Ligero)",
#       mmi_class == 5 ~ "V (Moderado)",
#       mmi_class == 6 ~ "VI (Fuerte)",
#       mmi_class == 7 ~ "VII (Muy Fuerte)",
#       mmi_class == 8 ~ "VIII (Severo)",
#       mmi_class == 9 ~ "IX (Violento)",
#       TRUE ~ "Desconocido"
#     )
#   )
# 
# 
# # Dissolve dealing with topological errors -
# # 1. Turn off strict spherical geometry processing
# sf_use_s2(FALSE)
# 
# # 2. Re-run the dissolve with the zero-buffer trick
# shakemap_dissolved_72 <- shakemap_reclassed |>
#   # Force polygon reconstruction to eliminate crossed loops
#   st_buffer(dist = 0) |> 
#   group_by(mmi_class, label_en, label_es) |>
#   summarize(geometry = st_union(geometry), .groups = "drop") |>
#   # Optional: Clean up any weird multi-geometries created during the union
#   st_cast("MULTIPOLYGON")
# 
# # Turn s2 back on if you need it for other global calculations later
# sf_use_s2(TRUE)
# 
# st_write(shakemap_dissolved_72, paste0(layers,"shakemap_data/mi_reclassM72.gpkg"), append= F)

## 1.2 ADMIN BOUNDARIES -----
# from Marco Geoestadistico Nacional de Colombia del geo portal del DANE
# https://geoportal.dane.gov.co/geovisores/territorio/mgn-marco-geoestadistico-nacional/
ab <- st_read(paste0(layers,"MGN_ADM_MPIO_GRAFICO_datPS2026.shp"))

original_crs <- st_crs(ab)

ab_fixed <- ab |>
  st_set_crs(NA) |>           # Blind R to the curvature of the earth
  st_buffer(dist = 0) |>      # Force the geometric rebuild
  st_set_crs(original_crs)

st_write(ab_fixed,paste0(layers,"col_ab_fixed_adm3.gpkg"), append = FALSE)
crs(ab)

## 1.3 WORLDPOP ------
# list all wpop files
# Example: col_f_00_2026_CN_100m_R2025A_v1.tif – this dataset represents constrained 
# estimates of total number of females of age group 0 to 12 months per grid square
# in Colombia for 2026 at 100m resolution, version R2025A v1.
# https://hub.worldpop.org/geodata/summary?id=80752
wp_files <- list.files(paste0(layers, "wpop"), pattern = "\\.tif$", full.names = TRUE)

# Load all rasters as a multi-layer SpatRaster
wp_stack <- rast(wp_files)

# Extract layer names to easily index them
layer_names <- names(wp_stack)

# Helper function using explicit underscores as delimiters
sum_cohorts <- function(raster_stack, sex_pattern, age_vectors) {
  # Build a pattern like: "_f_(00|01|05)_" or "_[fm]_(10|15|20)_"
  age_pattern <- paste0("_(", paste(age_vectors, collapse = "|"), ")_")
  full_pattern <- paste0("_", sex_pattern, age_pattern)
  
  idx <- grep(full_pattern, names(raster_stack))
  
  if(length(idx) == 0) {
    stop(paste("No layers matched pattern:", full_pattern))
  }
  
  app(raster_stack[[idx]], fun = sum, na.rm = TRUE)
}

### 1.3.1 Generate target population groups ----
# A. Total Female & Male (Matches any layer containing _f_ or _m_)
pop_female <- app(wp_stack[[grep("_f_", names(wp_stack))]], fun = sum, na.rm = TRUE)
pop_male   <- app(wp_stack[[grep("_m_", names(wp_stack))]], fun = sum, na.rm = TRUE)
pop_total  <- pop_female + pop_male

# B. Girls (female 10 - 14)
pop_fem_10_14 <- sum_cohorts(wp_stack, "[f]", "10")

# C. Teenagers (female Ages 15 - 19)
pop_teen_15_19 <- sum_cohorts(wp_stack, "[f]", "15")

# D. Young (female Ages 20 - 24)
pop_youth_20_24 <- sum_cohorts(wp_stack, "[f]", "20")

# E. Women of Reproductive Age (WRA: Female Ages 15 to 45)
pop_wra <- sum_cohorts(wp_stack, "f", c("15", "20", "25", "30", "35", "40", "45"))

# E. Older Persons (Ages 65, 70, 75, 80)
# (Assuming your stack also includes the male/female 65+ layers)
pop_older_65plus <- sum_cohorts(wp_stack, "[fm]", c("65", "70", "75", "80", "85", "90"))

demographics_stack <- c(
  pop_total, pop_female, pop_male, pop_fem_10_14, 
  pop_teen_15_19, pop_youth_20_24,pop_wra, pop_older_65plus
)

names(demographics_stack) <- c(
  "Total_Pop", "Female", "Male", "Girls_10_14", 
  "Teenagers_15_19","Youth_20_24", "WRA_15_49", "Older_65_Plus"
)

print("Demographic layers compiled successfully!")

layer_totals <- global(demographics_stack, "sum", na.rm = TRUE)

# Clean up the output dataframe for readability
layer_totals <- data.frame(
  Indicator = rownames(layer_totals),
  Total_Count = round(layer_totals$sum, 0)
) |>
  mutate(Total_Count_Formatted = format(Total_Count, big.mark = ","))

print("--- GLOBAL DEMOGRAPHIC TOTALS ---")
print(layer_totals[, c("Indicator", "Total_Count_Formatted")])


# # 2. SANITY CHECK SCRIPTS (AUTOMATED TESTING)
# 
# cat("\n--- RUNNING LOGICAL SANITY CHECKS ---\n")
# 
# # Extract counts as named numeric values for easy math
# counts <- setNames(layer_totals$Total_Pop , layer_totals$Indicator)
# 
# # Check 1: Do Male and Female perfectly equal the Total Population?
# sex_sum <- counts["Female"] + counts["Male"]
# diff_sex <- abs(counts["Total_Pop"] - sex_sum)
# 
# if (diff_sex == 0) {
#   cat("✅ PASS: Female + Male perfectly matches Total Population.\n")
# } else {
#   cat(sprintf("⚠️ WARNING: Mismatch between Total Pop and Sex Sum! Difference: %s people\n",
#               format(diff_sex, big.mark = ",")))
# }
# 
# # Check 2: Are sub-cohorts logically smaller than the Total Population?
# sub_cohort_sum <- counts["Children_0_9"] + counts["Youth_10_24"] + counts["Older_65_Plus"]
# cat(sprintf("Note: Selected age cohorts (0-9, 10-24, 65+) account for %s%% of the total population.\n",
#             round((sub_cohort_sum / counts["Total_Pop"]) * 100, 1)))
# 
# if (sub_cohort_sum < counts["Total_Pop"]) {
#   cat("✅ PASS: Extracted age sub-cohorts are within logical bounds (< Total Pop).\n")
# } else {
#   cat("❌ FAIL: Extracted age sub-cohorts exceed the Total Population!\n")
# }
# 
# # Check 3: Is Women of Reproductive Age (WRA) smaller than the Total Female Population?
# if (counts["WRA_15_49"] < counts["Female"]) {
#   cat("✅ PASS: Women of Reproductive Age is logically smaller than total females.\n")
# } else {
#   cat("❌ FAIL: WRA is greater than or equal to the total female population!\n")
# }

# 2. POPULATION ESTIMATES BY USGS SEISMIC INTENSITIES NATIONAL LEVEL ------


## 2.2 Run extraction for both events -----

# Calculate exposure for the M7.4 event
exposure_74 <- calculate_national_exposure(
  shakemap_sf = shakemap_dissolved_74, 
  demographics_stack = demographics_stack, 
  event_label =  "USGS-M 7.4 - 5 km S of San José del Palmar, Colombia"
)

## 2.3 Format and run ----
options(scipen = 999)

# Format tables with commas for reporting
# Format tables: Round to hundreds FIRST, then apply commas

format_exposure_table <- function(df) {
  df |>
    mutate(across(
      .cols = Total_Pop:Older_65_Plus, 
      # Change 0 to -2 to round to hundreds, and block scientific notation
      .fns = ~ format(round(.x, -2), big.mark = ",", scientific = FALSE, trim = TRUE)
    ))
}

exposure_74_formatted <- format_exposure_table(exposure_74)

View(exposure_74_formatted)


write_csv(exposure_74_formatted, paste0(folder, "tables/col_exposure_74_formatted.csv"))

## 2.4 Export into excel ------
### 2.4.1 Formatted 74 ----

# 1. Prepare your data
df_export <- exposure_74_formatted |>
  sf::st_drop_geometry()

# 2. Create a new workbook and add a worksheet
wb <- createWorkbook()
addWorksheet(wb, "National Exposure Colombia M7.4")

# 3. Write the main data table to the sheet
writeData(wb, sheet = 1, x = df_export, startRow = 1, startCol = 1)

# 4. Define our professional styles
headerStyle <- createStyle(fontColour = "#FFFFFF", fgFill = "#1F497D", halign = "center", valign = "center", textDecoration = "bold")
commaStyle <- createStyle(numFmt = "#,##0")
borderStyle <- createStyle(border = "TopBottomLeftRight", borderColour = "#D9D9D9")
zebraStyle <- createStyle(fgFill = "#F2F2F2")

# 5. Apply the styles to the main table
addStyle(wb, sheet = 1, style = headerStyle, rows = 1, cols = 1:ncol(df_export), gridExpand = TRUE)

data_rows <- 2:(nrow(df_export) + 1)
addStyle(wb, sheet = 1, style = borderStyle, rows = data_rows, cols = 1:ncol(df_export), gridExpand = TRUE)
addStyle(wb, sheet = 1, style = commaStyle, rows = data_rows, cols = 5:ncol(df_export), gridExpand = TRUE)

even_rows <- data_rows[data_rows %% 2 == 0]
if(length(even_rows) > 0) {
  addStyle(wb, sheet = 1, style = zebraStyle, rows = even_rows, cols = 1:ncol(df_export), gridExpand = TRUE)
}

# 6. Append the Metadata at the bottom using separate rows
# FIX: Both Key and Value now have exactly 8 items!
metadata <- data.frame(
  Key = c("METADATA & NOTES", "Event:", "Date Generated:", "Data Sources:", "", "", "Methodology:", "Prepared by:"),
  Value = c("", 
            "Earthquake M7.4 - 5 km S of San José del Palmar, Colombia - National Exposure", 
            format(Sys.Date(), "%B %Y"), 
            "Estimates of 2015-2030 total number of people per grid square broken down by gender and age groupings at a resolution of 3 arc (approximately 100m at the equator) R2025A version v1",
            "USGS-M 7.4 - 5 km S of San José del Palmar, Colombia", 
            "Colombia - Subnational Administrative Boundaries - Statistical Framework Colombia (DANE)", 
            "Fractional area extraction (exactextractr) on 100m demographic grids", 
            "UNFPA LACRO")
)

meta_start_row <- nrow(df_export) + 4
# FIX: Adjusted to cover all 8 rows of metadata (the title + 7 lines)
meta_rows <- (meta_start_row + 1):(meta_start_row + 7)

# Write the metadata table
writeData(wb, sheet = 1, x = metadata, startRow = meta_start_row, startCol = 1, colNames = FALSE)

# Style the metadata
metaTitleStyle <- createStyle(fontColour = "#1F497D", textDecoration = "bold")
metaKeyStyle <- createStyle(fontColour = "#595959", textDecoration = "bold")
metaValStyle <- createStyle(fontColour = "#595959", textDecoration = "italic")

addStyle(wb, sheet = 1, style = metaTitleStyle, rows = meta_start_row, cols = 1)
addStyle(wb, sheet = 1, style = metaKeyStyle, rows = meta_rows, cols = 1, gridExpand = TRUE)
addStyle(wb, sheet = 1, style = metaValStyle, rows = meta_rows, cols = 2, gridExpand = TRUE)

# 7. Auto-adjust column widths for readability
setColWidths(wb, sheet = 1, cols = 1:ncol(df_export), widths = "auto")
# Give the metadata value column a generous width
setColWidths(wb, sheet = 1, cols = 2, widths = 90) 

# 8. Save the styled workbook to your project folder
saveWorkbook(wb, paste0(folder, "tables/San_Jose_Palmar_Colombia_M74_National_Exposure_Clean.xlsx"), overwrite = TRUE)

print("Excel file for M7.4 successfully generated and formatted!")




# 3. POPULATION ESTIMATES BY USGS SEISMIC INTENSITIES AT SUBNATIONAL LEVEL ------

## 3.1. DEFINE THE ADMIN-LEVEL EXTRACTION FUNCTION -----

calculate_admin_exposure <- function(admin_sf, shakemap_sf, demographics_stack, admin_name_col) {
  cat("\nAligning CRS and intersecting geometries...\n")
  
  # 1. Ensure CRS matches between Admin boundaries and ShakeMap
  if (st_crs(admin_sf) != st_crs(shakemap_sf)) {
    admin_sf <- st_transform(admin_sf, st_crs(shakemap_sf))
  }
  
  # --- THE FIX: Force s2 off temporarily just for this operation ---
  current_s2_state <- sf_use_s2() # Remember what the user had it set to
  sf_use_s2(FALSE)                # Force it off
  
  # 2. Intersect: Slice the ShakeMap polygons by the Admin boundaries
  admin_shakemap_intersect <- suppressWarnings(st_intersection(admin_sf, shakemap_sf)) |>
    st_collection_extract("POLYGON") 
  
  sf_use_s2(current_s2_state)     # Turn it back on to whatever it was before
  
  
  # 3. Ensure the new intersected polygons match the raster stack CRS
  if (st_crs(admin_shakemap_intersect) != crs(demographics_stack, proj = TRUE)) {
    admin_shakemap_intersect <- st_transform(admin_shakemap_intersect, st_crs(demographics_stack))
  }
  
  cat("Running fast extraction by Admin Unit + MMI Zone...\n")
  
  # 4. Run the optimized C++ extraction
  exposure_raw <- exact_extract(
    x = demographics_stack, 
    y = admin_shakemap_intersect, 
    fun = "sum",
    progress = TRUE 
  )
  
  names(exposure_raw) <- gsub("^sum\\.", "", names(exposure_raw))
  
  # 5. Build the final reporting table
  exposure_table <- admin_shakemap_intersect |>
    st_drop_geometry() |>
    bind_cols(exposure_raw) |>
    select(all_of(admin_name_col), mmi_class, label_en, label_es, Total_Pop:Older_65_Plus) |>
    group_by(across(all_of(c(admin_name_col, "mmi_class", "label_en", "label_es")))) |>
    summarize(across(Total_Pop:Older_65_Plus, ~sum(.x, na.rm = TRUE)), .groups = "drop") |>
    arrange(!!sym(admin_name_col), desc(mmi_class))
  
  return(exposure_table)
}
ab_fixed <- st_read(paste0(layers,"col_ab_fixed_adm3.gpkg"))


## 3.2 Run the extraction for the M7.4 Mainshock by District -----
admin_exposure_74 <- calculate_admin_exposure(
  admin_sf = admin1,
  shakemap_sf = shakemap_dissolved_74,
  demographics_stack = demographics_stack,
  admin_name_col = "ADM1_ES" # Replace with your actual column name
)

# 2. Format for reporting
admin_exposure_74_formatted <- admin_exposure_74 |>
  mutate(across(
    .cols = Total_Pop:Older_65_Plus, 
    .fns = ~format(round(.x, -2), big.mark = ",")
  ))

## 3.3 Run extraction by Municipality ----
admin2_exposure_74 <- calculate_admin_exposure(
  admin_sf = ab_fixed,
  shakemap_sf = shakemap_dissolved_74,
  demographics_stack = demographics_stack,
  admin_name_col = "ADM2_ES" # Replace with your actual column name
)

# 2. Format for reporting
admin2_exposure_74_formatted <- admin2_exposure_74 |>
  mutate(across(
    .cols = Total_Pop:Older_65_Plus, 
    .fns = ~format(round(.x, -2), big.mark = ",")
  ))



# View the final cross-tabulated results
View(admin_exposure_74_formatted)
View(admin2_exposure_74_formatted)
## 3.3 Export basic outcome ----

write_csv(admin_exposure_74_formatted, paste0(folder, "tables/admin1_exposure_74_formatted.csv"))
write_csv(admin2_exposure_74_formatted, paste0(folder, "tables/admin2_exposure_74_formatted.csv"))

# Replace 'exposure_admin2_74' with the actual name of your table variable
municipality_counts <- admin2_exposure_74_formatted |>
  
  # Filter out spatial slivers with 0 population
  # (Checking for both number 0 and string "0" in case you already ran the formatting function)
  filter(Total_Pop != 0 & Total_Pop != "0") |>
  
  # Group by the Spanish MMI label
  group_by(label_es) |>
  
  # Count the unique municipalities in each zone
  summarise(Municipios_Afectados = n_distinct(ADM2_ES), .groups = "drop") |>
  
  # Sort from most severe to least severe
  arrange(desc(label_es))

print(municipality_counts)

# 4. STATIC MAP SHOWING RESULTS ----
library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)
library(gridExtra)
library(ggrepel)

## 4.1 National map EVENT 7.4 ------
# 1. SETUP & SPATIAL DATA

# Define standard system font to use Calibri across all elements
windowsFonts(Calibri = windowsFont("Calibri"))
base_font <- "Calibri"

epicenter <- data.frame(lon = -76.242, lat = 4.844) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cities <- data.frame(
  name = c("San José del Palmar", "Pereira", "Cali", "Manizales", "Quibdó", "Armenia", "Buenaventura", "Cartago"),
  lon = c(-76.23, -75.70, -76.53, -75.52, -76.66, -75.68, -77.03, -75.91),
  lat = c(4.97, 4.81, 3.45, 5.07, 5.69, 4.53, 3.88, 4.75)
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

col_map <- st_transform(ab_fixed, 4326) 
shake_map <- st_transform(shakemap_dissolved_74, 4326)

# 2. BUILD THE MAP (WITH LEGEND)

shake_colors <- c(
  "IV (Light)"       = "#FCE8B2", 
  "V (Moderate)"     = "#FCD15B", 
  "VI (Strong)"      = "#FABB21", 
  "VII (Very Strong)"= "#EA7E15", 
  "VIII (Severe)"    = "#C51F1C",
  "IX (Violent)"     = "#981E1C"  # Added a darker red so this row doesn't break!
)
# 1. Extract the exact bounding box of the ShakeMap
shake_bbox <- st_bbox(shake_map)
shake_map <- shake_map |> 
  filter(label_en != "II - III (Weak)")

# 2. Build the Map
p_map <- ggplot() +
  geom_sf(data = col_map, fill = "#F2F5F9", color = "#DCDCDC", size = 0.3) +
  geom_sf(data = shake_map, aes(fill = label_en), color = NA, alpha = 0.5) +
  geom_sf(data = epicenter, size = 6, shape = 21, color = "#C51F1C", fill = NA, stroke = 1.5) +
  geom_sf(data = epicenter, size = 2, color = "#C51F1C") +
  geom_text_repel(
    data = cities, aes(label = name, geometry = geometry), 
    stat = "sf_coordinates", size = 3.5, fontface = "bold", family = base_font,
    bg.color = "white", bg.r = 0.15 
  ) +
  geom_sf(data = cities, size = 1.5, color = "black") +
  scale_fill_manual(
    values = shake_colors, 
    name = "USGS MMI\nImpact Zones",
    breaks = c("IX (Violent)",  "VIII (Severe)", "VII (Very Strong)", "VI (Strong)", "V (Moderate)", "IV (Light)")
  ) +
  coord_sf(
    xlim = c(shake_bbox["xmin"], shake_bbox["xmax"]), 
    ylim = c(shake_bbox["ymin"], shake_bbox["ymax"]), 
    expand = TRUE 
  ) +
  theme_minimal(base_family = base_font) +
  theme(
    legend.position = "right", 
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.background = element_rect(fill = "#D6EAF8", color = NA), 
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = 0.2),
    
    # --- THE TITLE & SUBTITLE FIX ---
    # Title changed to black, margins adjusted to make room for subtitle
    plot.title = element_text(color = "black", face = "bold", size = 18, margin = margin(b = 5)),
    # Added subtitle formatting (dark grey, slightly smaller)
    plot.subtitle = element_text(color = "grey30", size = 14, margin = margin(b = 15)),
    
    axis.title = element_blank()
  ) +
  labs(
      title = "Earthquake Magnitude M 7.4 - 5 km S of San José del Palmar, Colombia | ShakeMap MMI Impact Zones",
      subtitle = "Estimated Population Exposure by Demographic Cohort" # Swap text here if you prefer another option!
  )

# 3. BUILD THE TABLE (WITH MMI ROW COLORS)

table_data <- exposure_74_formatted |>
  filter(mmi_class != 3) |> 
  select(label_en, Total_Pop, Girls_10_14, Teenagers_15_19, Youth_20_24, WRA_15_49, Older_65_Plus) |>
  rename(
    `MMI Intensity` = label_en,
    `Total Pop` = Total_Pop,
    `Girls (F10-14)` = Girls_10_14,
    `Teenagers (F15-19)` = Teenagers_15_19,
    `Youth (F20-24)` = Youth_20_24,
    `Women Reproductive Age (F15-49)` = WRA_15_49 ,
    `Elderly (T65+)` = Older_65_Plus
  )

# Extract the exact row colors by matching the MMI column to your palette
row_fills <- unname(shake_colors[table_data$`MMI Intensity`])


# Make text white for the dark red/orange rows so the numbers are readable
row_text_colors <- ifelse(table_data$`MMI Intensity` %in% c("IX (Violent)" , "VIII (Severe)", "VII (Very Strong)"), "white", "black")

# Apply Calibri and the dynamic colors to the gridExtra table
ttheme_custom <- ttheme_minimal(
  base_family = base_font,
  core = list(
    bg_params = list(fill = row_fills, col = "white"), # Painted rows with clean white borders
    fg_params = list(col = row_text_colors, fontsize = 11, hjust = 1, x = 0.95)       
  ),
  colhead = list(
    bg_params = list(fill = "#1F497D", col = "white"),                
    fg_params = list(col = "white", fontsize = 12, fontface = "bold")
  )
)

# Convert to graphical object and wrap it for patchwork
p_table_grob <- tableGrob(table_data, rows = NULL, theme = ttheme_custom)
p_table_wrapped <- wrap_elements(p_table_grob)



# 4. BUILD THE METADATA FOOTER (FIXED BLACK TEXT)

meta_data <- data.frame(
  x = c(0, 0, 0, 0, 0, 2.5, 2.5, 2.5, 2.5, 2.5),
  y = c(6, 5, 4, 3, 2, 5, 4, 3, 2, 1),
  label = c("METADATA & NOTES", "Event:", "Date Generated:", "Data Sources:", "Methodology:", 
                    "Earthquake M7.4 - 5 km S of San José del Palmar, Colombia - National Exposure", format(Sys.Date(), "%B %Y"), 
                    "USGS-M 7.4 - 5 km S of San José del Palmar, Colombia | Colombia - Subnational Administrative Boundaries - Statistical Framework Colombia (DANE)", 
                    "Fractional area extraction (exactextractr) on 100m demographic grids", "UNFPA LACRO"),
  fontface = c("bold", "bold", "bold", "bold", "bold", "italic", "italic", "italic", "italic", "italic"),
  color = "black"
)

p_footer <- ggplot(meta_data, aes(x = x, y = y, label = label, fontface = fontface, color = color)) + 
  geom_text(hjust = 0, size = 4, family = base_font) +
  scale_color_identity() + # <-- THE FIX: Forces ggplot to use actual black instead of default red
  theme_void() +
  coord_cartesian(xlim = c(0, 15), ylim = c(0.5, 6.5)) +
  theme(legend.position = "none")

# 5. ASSEMBLE AND EXPORT

# Ensure no stray characters at the end of the line
final_infographic <- p_map / p_table_wrapped / p_footer

# Apply the height ratios
final_infographic <- final_infographic + 
  plot_layout(heights = c(5, 1.5, 1))

# Export
ggsave(
  filename = paste0(folder, "maps/Colombia_M74_Exposure_Infographic_EN.png"),
  plot = final_infographic,
  width = 12, 
  height = 13, 
  dpi = 300,
  bg = "white"
)

## 4.2 Map spanish version -----
# Define standard system font to use Calibri across all elements
windowsFonts(Calibri = windowsFont("Calibri"))
base_font <- "Calibri"

epicenter <- data.frame(lon = -76.242, lat = 4.844) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cities <- data.frame(
  name = c("San José del Palmar", "Pereira", "Cali", "Manizales", "Quibdó", "Armenia", "Buenaventura", "Cartago"),
  lon = c(-76.23, -75.70, -76.53, -75.52, -76.66, -75.68, -77.03, -75.91),
  lat = c(4.97, 4.81, 3.45, 5.07, 5.69, 4.53, 3.88, 4.75)
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

col_map <- st_transform(ab_fixed, 4326) 
shake_map <- st_transform(shakemap_dissolved_74, 4326)

# 1. PREPARE DATA & TRANSLATE MMI CATEGORIES

shake_colors_es <- c(
  "IV (Ligero)"       = "#FCE8B2", 
  "V (Moderado)"      = "#FCD15B", 
  "VI (Fuerte)"       = "#FABB21", 
  "VII (Muy Fuerte)"  = "#EA7E15", 
  "VIII (Severo)"     = "#C51F1C",
  "IX (Violento)"     = "#981E1C"  
)

# Helper function to translate English MMI labels to Spanish
translate_mmi <- function(label) {
  dplyr::case_when(
    label == "IX (Violent)" ~ "IX (Violento)",
    label == "VIII (Severe)" ~ "VIII (Severo)",
    label == "VII (Very Strong)" ~ "VII (Muy Fuerte)",
    label == "VI (Strong)" ~ "VI (Fuerte)",
    label == "V (Moderate)" ~ "V (Moderado)",
    label == "IV (Light)" ~ "IV (Ligero)",
    label == "II - III (Weak)" ~ "II - III (Débil)",
    TRUE ~ label
  )
}

shake_bbox <- st_bbox(shake_map)

# Apply translation and filter out Weak
shake_map <- shake_map |> 
  mutate(label_es = translate_mmi(label_en)) |>
  filter(label_es != "II - III (Débil)")



# 2. BUILD THE MAP


p_map <- ggplot() +
  geom_sf(data = col_map, fill = "#F2F5F9", color = "#DCDCDC", size = 0.3) +
  geom_sf(data = shake_map, aes(fill = label_es), color = NA, alpha = 0.5) +
  geom_sf(data = epicenter, size = 6, shape = 21, color = "#C51F1C", fill = NA, stroke = 1.5) +
  geom_sf(data = epicenter, size = 2, color = "#C51F1C") +
  geom_text_repel(
    data = cities, aes(label = name, geometry = geometry), 
    stat = "sf_coordinates", size = 3.5, fontface = "bold", family = base_font,
    bg.color = "white", bg.r = 0.15 
  ) +
  geom_sf(data = cities, size = 1.5, color = "black") +
  scale_fill_manual(
    values = shake_colors_es, 
    name = "Zonas de Impacto\nMMI USGS",
    breaks = c("IX (Violento)",  "VIII (Severo)", "VII (Muy Fuerte)", "VI (Fuerte)", "V (Moderado)", "IV (Ligero)")
  ) +
  coord_sf(
    xlim = c(shake_bbox["xmin"], shake_bbox["xmax"]), 
    ylim = c(shake_bbox["ymin"], shake_bbox["ymax"]), 
    expand = TRUE 
  ) +
  theme_minimal(base_family = base_font) +
  theme(
    legend.position = "right", 
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.background = element_rect(fill = "#D6EAF8", color = NA), 
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = 0.2),
    plot.title = element_text(color = "black", face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "grey30", size = 14, margin = margin(b = 15)),
    axis.title = element_blank()
  ) +
  labs(
    title = "Terremoto Magnitud M 7.4 - 5 km S de San José del Palmar, Colombia | Zonas de Impacto MMI",
    subtitle = "Población Estimada Expuesta por Grupo Demográfico" 
  )



# 3. BUILD THE TABLE (SPANISH)


table_data <- exposure_74_formatted |>
  mutate(label_es = translate_mmi(label_en)) |>
  filter(mmi_class != 3) |> 
  select(label_es, Total_Pop, Girls_10_14, Teenagers_15_19, Youth_20_24, WRA_15_49, Older_65_Plus) |>
  rename(
    `Intensidad MMI` = label_es,
    `Población Total` = Total_Pop,
    `Niñas (F10-14)` = Girls_10_14,
    `Adolescentes (F15-19)` = Teenagers_15_19,
    `Jóvenes (F20-24)` = Youth_20_24,
    `Mujeres Edad Reproductiva (F15-49)` = WRA_15_49 ,
    `Adultos Mayores (T65+)` = Older_65_Plus
  )

# Extract row colors matching the Spanish MMI column
row_fills <- unname(shake_colors_es[table_data$`Intensidad MMI`])

# Make text white for the dark red/orange rows
row_text_colors <- ifelse(table_data$`Intensidad MMI` %in% c("IX (Violento)" , "VIII (Severo)", "VII (Muy Fuerte)"), "white", "black")

# Apply Calibri and the dynamic colors
ttheme_custom <- ttheme_minimal(
  base_family = base_font,
  core = list(
    bg_params = list(fill = row_fills, col = "white"), 
    fg_params = list(col = row_text_colors, fontsize = 11, hjust = 1, x = 0.95)       
  ),
  colhead = list(
    bg_params = list(fill = "#1F497D", col = "white"),                
    fg_params = list(col = "white", fontsize = 12, fontface = "bold")
  )
)

p_table_grob <- tableGrob(table_data, rows = NULL, theme = ttheme_custom)
p_table_wrapped <- wrap_elements(p_table_grob)


# 4. BUILD THE METADATA FOOTER (SPANISH)


# Retrieve system month in Spanish dynamically (or fallback to manual if locale is English)
sys_date_es <- format(Sys.Date(), "%B %Y")

meta_data <- data.frame(
  x = c(0, 0, 0, 0, 0, 2.5, 2.5, 2.5, 2.5, 2.5),
  y = c(6, 5, 4, 3, 2, 5, 4, 3, 2, 1),
  label = c("METADATOS Y NOTAS", "Evento:", "Fecha de Generación:", "Fuentes de Datos:", "Metodología:", 
            "Terremoto M7.4 - 5 km S de San José del Palmar, Colombia - Exposición Nacional", sys_date_es, 
            "USGS-M 7.4 - 5 km S de San José del Palmar | Límites Administrativos Subnacionales - Marco Geoestadístico (DANE)", 
            "Extracción de área fraccional (exactextractr) en grillas demográficas de 100m", "UNFPA LACRO"),
  fontface = c("bold", "bold", "bold", "bold", "bold", "italic", "italic", "italic", "italic", "italic"),
  color = "black"
)

p_footer <- ggplot(meta_data, aes(x = x, y = y, label = label, fontface = fontface, color = color)) + 
  geom_text(hjust = 0, size = 4, family = base_font) +
  scale_color_identity() + 
  theme_void() +
  coord_cartesian(xlim = c(0, 15), ylim = c(0.5, 6.5)) +
  theme(legend.position = "none")

# Convert to graphical object and wrap it for patchwork
p_table_grob <- tableGrob(table_data, rows = NULL, theme = ttheme_custom)

# Wrap it and add a left margin (padding) to push it to the right
p_table_wrapped <- wrap_elements(p_table_grob) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 70)) # 'l' is for left margin


final_infographic <- p_map / p_table_wrapped / p_footer
final_infographic <- final_infographic + plot_layout(heights = c(5, 1.5, 1))

# Export with _ES suffix
ggsave(
  filename = paste0(folder, "maps/Colombia_M74_Exposure_Infographic_ES.png"),
  plot = final_infographic,
  width = 12, 
  height = 13, 
  dpi = 300,
  bg = "white"
)

# 
# # 7. Update tabulation with Municipalities identified by UNGRD -----
# ## 7.1 Filter in the shapefile the departments that have been affected. -------
# # 1. Define the list of the 14 affected departments in UPPERCASE
# affected_deps_upper <- c(
#   "VALLE DEL CAUCA", "RISARALDA", "CAUCA", "CUNDINAMARCA", 
#   "BOLÍVAR", "QUINDÍO", "TOLIMA", "SUCRE", 
#   "PUTUMAYO", "NORTE DE SANTANDER", "HUILA", "CHOCÓ", 
#   "CALDAS", "ANTIOQUIA"
# )
# 
# # 2. Filter the shapefile
# # Note: Replace 'ADM1_ES' with your actual department column name
# ab_filt <- ab |> 
#   filter(ADM1_ES %in% affected_deps_upper)
# 
# 
# # 3. Validation Check
# cat("Departments filtered:", nrow(ab_filt), "\n")
# table(ab_filt$ADM1_ES)
# 
# mun_389 <- read_xlsx(paste0(folder,"389_municip_eq.xlsx"))
# 
# # match by name to see if it works
# mun_389_clean <- mun_389 |>
#   mutate(match_name = toupper(trimws(Municipio)))
# 
# ab_filt_clean <- ab_filt |>
#   mutate(match_name = toupper(trimws(ADM2_ES)))
# 
# ab_filt_excel <- st_drop_geometry(ab_filt_clean)
# 
# # 2. Export to Excel (with fixed .xlsx extension)
# ab_filt_excel_simpl <- ab_filt_excel |> 
#   select(c(match_name,ADM1_ES, ADM1_PCODE, COD_MUN, ADM2_ES, ADM2_PCODE ))
# write.xlsx(ab_filt_excel_simpl, paste0(folder, "ab_filt_clean.xlsx"))
# 
# # 3. See what matches perfectly
# matched_mun <- inner_join(mun_389_clean, ab_filt_clean, by = "match_name")
# cat("Successfully matched:", nrow(matched_mun), "out of", nrow(mun_389), "\n")
# 
# # 4. See what FAILED to match (The troubleshoot list)
# unmatched_mun <- anti_join(mun_389_clean, ab_filt_clean, by = "match_name")
# 
# if (nrow(unmatched_mun) > 0) {
#   cat("\nWARNING: The following municipalities did NOT match and need to be corrected:\n")
#   print(unmatched_mun$Municipio)
# } else {
#   cat("\nSUCCESS: All municipalities matched perfectly!\n")
# }
# 
# 
# library(stringi)
# # 1. Helper function to remove accents, apostrophes, punctuation, and extra whitespace
# clean_string <- function(x) {
#   x |>
#     # Convert accented characters to ASCII equivalent (e.g., Á -> A, ñ -> N, é -> E)
#     stri_trans_general("Latin-ASCII") |>
#     # Convert to uppercase
#     toupper() |>
#     # Remove apostrophes, hyphens, quotes, and punctuation (keep letters, numbers, and spaces)
#     gsub(pattern = "[^A-Z0-9 ]", replacement = "") |>
#     # Collapse multiple consecutive spaces into a single space and trim ends
#     gsub(pattern = "\\s+", replacement = " ") |>
#     trimws()
# }
# 
# # 2. Apply cleaning to both datasets
# mun_389_clean <- mun_389 |>
#   mutate(match_name = clean_string(Municipio))
# 
# ab_filt_clean <- ab_filt |>
#   mutate(match_name = clean_string(ADM2_ES))
# 
# # 3. Check perfect matches
# matched_mun <- inner_join(mun_389_clean, ab_filt_clean, by = "match_name")
# cat("Successfully matched:", nrow(matched_mun), "out of", nrow(mun_389), "\n")
# 
# # 4. Check remaining unmatched records
# unmatched_mun <- anti_join(mun_389_clean, ab_filt_clean, by = "match_name")
# 
# if (nrow(unmatched_mun) > 0) {
#   cat("\nRemaining unmatched municipalities (", nrow(unmatched_mun), "):\n", sep = "")
#   print(unmatched_mun$Municipio)
# } else {
#   cat("\nSUCCESS: All municipalities matched perfectly!\n")
# }

# loading matched municipalities finished manually 
mun_389_match <- read_xlsx(paste0(folder,"MATCH_389.xlsx"))
nrow(mun_389_match)

# remove artificial NAs
mun_389_match <- mun_389_match |> 
  filter(!is.na(codigo)) |> 
  select(ADM2_PCODE)

# Merge with ab
mun_389_geo <- ab_fixed |> 
  inner_join(mun_389_match,  by="ADM2_PCODE")


nrow(mun_389_geo)
# st_write(mun_389_geo, paste0(layers,"mun_389_geo.gpkg"))

## 7.2 run intersection by ab, impact zone and calculate population. -----

### 7.2.1 By Municipality ----

admin2_exposure_74_389 <- calculate_admin_exposure(
  admin_sf = mun_389_geo,
  shakemap_sf = shakemap_dissolved_74,
  demographics_stack = demographics_stack,
  admin_name_col = "ADM2_ES" # Replace with your actual column name
)

# 2. Format for reporting
admin2_exposure_74_formatted_389 <- admin2_exposure_74_389 |>
  mutate(across(
    .cols = Total_Pop:Older_65_Plus, 
    .fns = ~format(round(.x, -2), big.mark = ",")
  ))



# View the final cross-tabulated results
View(admin2_exposure_74_formatted_389)

## 3.3 Export basic outcome ----

write_csv(admin2_exposure_74_formatted_389, paste0(folder, "tables/admin2_exposure_74_formatted_389.csv"))


# Count municipalities

municipality_counts <- admin2_exposure_74_formatted_389 |>
  
  # Filter out spatial slivers with 0 population
  # (Checking for both number 0 and string "0" in case you already ran the formatting function)
  filter(Total_Pop != 0 & Total_Pop != "0") |>
  
  # Group by the Spanish MMI label
  group_by(ADM2_ES) |>
  
  # Count the unique municipalities in each zone
  summarise(Municipios_Afectados = n())

print(municipality_counts)


municipality_counts_long <- 

print(head(municipality_counts_long))

# 
# cov_data <- read_stars("C:/Users/luisr/Downloads/coverage_mmi_medium_res.covjson", Driver = "CovJSON")
# 
# 
# cov_data <- read_stars(file.choose("C:/Users/luisr/Downloads/"), Driver = "CovJSON")








########################################### HASTA AQUI #######
# 
# # 5. MORE TABLES --------
# ## 5.1 TABLES 5YEARS OLD AGE GROUPS AND BY SEX AND BY MMI ZONES ADMIN 1 ESTADOS ------
# 
# library(sf)
# library(dplyr)
# library(exactextractr)
# # Note: rlang is loaded automatically with dplyr, which handles the .data[[]] pronoun
# 
# extract_subnational_exposure <- function(admin_sf, shakemap_sf, raster_stack, admin_level_col) {
#   
#   cat("\nAligning CRS and intersecting geometries...\n")
#   
#   # 1. Ensure CRS matches between Admin boundaries and ShakeMap
#   if (st_crs(admin_sf) != st_crs(shakemap_sf)) {
#     admin_sf <- st_transform(admin_sf, st_crs(shakemap_sf))
#   }
#   
#   # --- THE FIX: Force s2 off temporarily just for this operation ---
#   current_s2_state <- sf_use_s2() # Remember what the user had it set to
#   suppressMessages(sf_use_s2(FALSE)) # Force it off to avoid "Edge crosses edge" errors
#   
#   # 2. Intersect: Slice the ShakeMap polygons by the Admin boundaries
#   admin_mmi_intersect <- suppressWarnings(st_intersection(admin_sf, shakemap_sf)) |>
#     st_collection_extract("POLYGON") |>
#     st_cast("MULTIPOLYGON")
#   
#   suppressMessages(sf_use_s2(current_s2_state)) # Turn it back on to whatever it was before
#   
#   # 3. Ensure the new intersected polygons match the raster stack CRS
#   if (st_crs(admin_mmi_intersect) != crs(raster_stack, proj = TRUE)) {
#     admin_mmi_intersect <- st_transform(admin_mmi_intersect, crs(raster_stack))
#   }
#   
#   cat("Running fast extraction by Admin Unit + MMI Zone...\n")
#   
#   # 4. Extract the population demographics
#   admin_mmi_raw <- exact_extract(
#     raster_stack, 
#     admin_mmi_intersect, 
#     fun = "sum",
#     append_cols = c(admin_level_col, "label_en"),
#     progress = TRUE 
#   )
#   
#   # 5. Clean, summarize, and sort dynamically
#   final_table <- admin_mmi_raw |>
#     rename_with(~ gsub("sum\\.", "", .x)) |>
#     
#     # Evaluate the dynamic column name
#     group_by(.data[[admin_level_col]], label_en) |>
#     summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop") |>
#     
#     # Drop the weak zone
#     filter(label_en != "II - III (Weak)") |>
#     
#     # Sort alphabetically by Admin Name, then by severity
#     arrange(.data[[admin_level_col]], desc(label_en))
#   
#   cat("Done!\n")
#   return(final_table)
# }
# 
# ### 5.1.1 For M 7.4 event -------
# # 1. State/Department Level (Admin 1)
# exposure_admin1_74 <- extract_subnational_exposure(
#   admin_sf = ab, 
#   shakemap_sf = shakemap_dissolved_74, 
#   raster_stack = wp_stack, 
#   admin_level_col = "ADM1_ES"
# )
# 
# # 2. Municipality (Admin 2)
# exposure_admin2_74 <- extract_subnational_exposure(
#   admin_sf = ab, 
#   shakemap_sf = shakemap_dissolved_74, 
#   raster_stack = wp_stack, 
#   admin_level_col = "ADM2_ES"
# )
# 
# 
# 
# 
# ## 5.2 Format the tables ------
# # Function to format these tables
# event_title<- "Earthquake M7.4 - 5 km S of San José del Palmar, Colombia - National Exposure"
# format_exposure_table <- function(df, event_title) {
#   
#   # 1. CLEAN COLUMN NAMES (Direct find-and-replace)
#   # This forces R to strip the strings instantly before dplyr even touches the table
#   names(df) <- gsub("_2026_CN_100m_R2025A_v1", "", names(df))
#   names(df) <- gsub("^ven_", "", names(df))
#   
#   df_formatted <- df |>
#     mutate(across(where(is.numeric), ~ format(round(.x, -1), big.mark = ",", scientific = FALSE, trim = TRUE))) |>
#     mutate(across(everything(), as.character))
#   
#   # 2. Dynamically grab the names of the first two columns (e.g., "adm1_name" and "label_en")
#   col1 <- names(df_formatted)[1]
#   col2 <- names(df_formatted)[2]
#   
#   # 3. Create the metadata block
#   meta_df <- data.frame(
#     col_a = c("", "METADATA & NOTES", "Event:", "Date Generated:", "Data Sources:", "Methodology:", ""),
#     col_b = c("", "", event_title, format(Sys.Date(), "%B %Y"), 
#               "USGS ShakeMap | DANE Colombia Admin Boundaries | WorldPop 2026_CN_100m_R2025A_v1 for Colombia", 
#               "Fractional area extraction (exactextractr) on 100m demographic grids", 
#               "UNFPA LACRO"),
#     stringsAsFactors = FALSE
#   )
#   
#   # Rename the metadata columns to perfectly match the main table
#   names(meta_df) <- c(col1, col2)
#   
#   # 4. Stack them together and replace any NA values with clean blank spaces
#   final_table <- bind_rows(df_formatted, meta_df)
#   final_table[is.na(final_table)] <- ""
#   
#   return(final_table)
# }
# 
# # Format the tables and add the specific event titles
# final_admin1_75 <- format_exposure_table(exposure_admin1_74, "Yumare M7.5 - Admin 1 (Region) Exposure")
# final_admin2_75 <- format_exposure_table(exposure_admin2_74, "Yumare M7.5 - Admin 2 (Municipio) Exposure")
# final_admin3_75 <- format_exposure_table(exposure_admin3_74, "Yumare M7.5 - Admin 3 (Parroquia) Exposure")
# 
# 
# 
# write.xlsx(final_admin1_75, paste0(folder,"tables/M75_Admin1.xlsx"), rowNames = FALSE)
# write.xlsx(final_admin2_75, paste0(folder,"tables/M75_Admin2.xlsx"), rowNames = FALSE)
# write.xlsx(final_admin3_75, paste0(folder,"tables/M75_Admin3.xlsx"), rowNames = FALSE)
# 
# write.xlsx(final_admin1_72, paste0(folder,"tables/M72_Admin1.xlsx"), rowNames = FALSE)
# write.xlsx(final_admin2_72, paste0(folder,"tables/M72_Admin2.xlsx"), rowNames = FALSE)
# write.xlsx(final_admin3_72, paste0(folder,"tables/M72_Admin3.xlsx"), rowNames = FALSE)
# 


##