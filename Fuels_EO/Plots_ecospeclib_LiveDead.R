# -------------------------------------------------------------------------
# This script extract live and dead vegetation samples from the 
# ECOSSTRESS spectral library
# r.vieiraleite@gmail.com
# https://github.com/leite-rvl/GEE-backup
# -------------------------------------------------------------------------





# Packages ----------------------------------------------------------------
library('dplyr')
library('tidyr')
library('ggplot2')
library('zoo')
library('RColorBrewer')
library('viridis')

# Directories -------------------------------------------------------------
# Directory where the ecospeclib data frame is stored
dir_tables <- ''
# Directory where the figures will be exported to
dir_figures <- ''


# Charts ------------------------------------------------------------------

data_all <- read.csv(file.path(dir_tables,
                               'ecospeclib_df.csv'))




# Filtrar e plotar --------------------------------------------------------
# Define theme to plot
my_theme <- theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 8),
        legend.position = 'none'
        ) 





# Plotting live and dead --------------------------------------------------


# Randomly select samples -------------------------------------------------
unique(data_all$Type)
# Filter only for VSWIR and live vegetation
data_filtered <- data_all %>% 
  mutate(Type = ifelse(Type == 'vegetation', 'Vegetation', Type)) %>% 
  filter(WavelengthRange == 'VSWIR',
         Class %in% c('Tree','needles','Shrub','leaves'),
         Type %in% c('Vegetation',
                     'non photosynthetic vegetation'))


# Get unique IDs for each Type
unique_vegetation_ids <- data_filtered %>%
  filter(Type == 'Vegetation') %>%
  distinct(Sample) %>%
  pull(Sample)

unique_non_photosynthetic_ids <- data_filtered %>%
  filter(Type == 'non photosynthetic vegetation') %>%
  distinct(Sample) %>%
  pull(Sample)

# Set the number of samples to randomly select
num_samples <- 10

# Randomly select 5 IDs for each Type
set.seed(81)  # Set seed for reproducibility
sample_size <- num_samples/2
selected_vegetation_ids <- sample(unique_vegetation_ids, sample_size)
selected_non_photosynthetic_ids <- sample(unique_non_photosynthetic_ids, sample_size)

# Combine the selected IDs for both Types
selected_ids <- c(selected_vegetation_ids, selected_non_photosynthetic_ids)

selected_samples <- data_filtered %>% filter(Sample %in% selected_ids)

unique(selected_samples$Sample)





##########################
# Plot
data2chart <- selected_samples %>% dplyr::filter(Wavelength > 0.3,Wavelength < 2.5 )



# Filter the data2chart dataframe for 'Vegetation' and 'non photosynthetic vegetation'
vegetation_data <- data2chart[data2chart$Type == 'Vegetation', ]
unique(vegetation_data$Sample)
vegetation_data <- vegetation_data %>% filter(Sample %in% unique(vegetation_data$Sample)[2:4])


non_photosynthetic_data <- data2chart[data2chart$Type == 'non photosynthetic vegetation', ]
unique(non_photosynthetic_data$Sample)

non_photosynthetic_data <- non_photosynthetic_data %>% 
  filter(Sample %in% unique(non_photosynthetic_data$Sample)[2:4])

# Create custom color palettes for each group
vegetation_palette <- viridis(num_samples)
vegetation_palette <- c( "#35B779FF", "#6DCD59FF", "#B4DE2CFF")

non_photosynthetic_palette <- magma(num_samples)
non_photosynthetic_palette <- c("#CD4071FF","#F1605DFF","#FD9567FF")


# Create the plot using the custom color palettes
ggplot() +
  geom_line(data = non_photosynthetic_data, size = 1,
            aes(x = Wavelength, y = Reflectance, color = Sample, group = Sample)) +
  geom_line(data = vegetation_data, size = 1,
            aes(x = Wavelength, y = Reflectance, color = Sample, group = Sample)) +
  scale_color_manual(values = c(vegetation_palette, non_photosynthetic_palette),
                     labels = c("", "",'','','','')) +
  labs(x = 'Wavelength (µm)',
       y = 'Reflectance (%)') +
  my_theme +
  theme(legend.position = c(0.8,0.72),
        legend.background = element_rect(fill = "transparent"),
        legend.spacing.y = unit(0.01, "cm"),
        legend.key = element_rect(fill = "transparent") 
        )


ggsave(file.path(dir_figures,'fig05_ecospeclib_LiveDead.tiff'),
       width = 4.5, height = 3,units = 'in',dpi = 300)



