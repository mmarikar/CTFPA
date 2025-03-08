# 3/5/2025
# Renamed LiverRel1f.png as CTFPA_Liver_Female.png
# Renamed LiverRel1f as CTFPA_Liver_Female
# 2/28/2025
# increase sizes of axis labels and plot title by modifying the theme's axis text and title, and centering the heading and increasing its font size

# Load required packages
library(ggplot2)
library(dplyr)
library(readxl)

CTFPACo <- read_excel("OrganWeightsPFAS.xlsx", sheet='CTFPA', na = c("", "NA")) 
#View(CTFPACo)
# Include only females (rows 61-120)
CTFPACof <- CTFPACo[c(61:120),]
#View(CTFPACof)

# Identify the top 3 highest relative liver weights
top_doses <- CTFPACof %>%
  group_by(Group) %>%
  summarise(max_LiverRel = max(LiverRel, na.rm = TRUE)) %>% #Finf max per group
  arrange(desc(max_LiverRel)) %>% # Sort by highest values
  slice(1:3) %>% # Select the three highest values
  mutate(y_position = max_LiverRel  + 0.05) # adjust y-position for asterisk placement

#Ensure Group is a factor to align correctly
CTFPACof$Group <- as.factor(CTFPACof$Group)
top_doses$Group <- as.factor(top_doses$Group)

# BOX PLOTS - Male Rat Relative Liver Weight Data PFNAC
#LiverRel1f 
CTFPA_Liver_Female<- ggplot(
  CTFPACof,
  aes(
    x = factor(Group),
    y = LiverRel,
    fill = factor(Group) # Fill for violin plot
  )
) +
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") + # Violin plot
  geom_boxplot(
    aes(fill = factor(Group)), # Separate fill for box plot
    width = 0.1,
    color = "black",
    alpha = 0.9
  ) +
  # Ensure the median is properly displayed
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "red") +
  # Add asterisks above the top dose
  geom_text(
    data = top_doses,
    aes(x = Group, y = y_position, label = "*"),
    size = 6, color = "black"
  ) +
  labs(
    title = "CTFPA - Female Rat Liver/Body Weight 90-Day Study",
    x = "Dose Group (mg/kg-day)",
    y = "Liver/BW Ratio(%)"
  ) +
  scale_fill_brewer(palette = "Set2", name = "Group") + # Colorful palette
  theme_bw() +
  theme(legend.position = "none")

# Save the plot as a PNG file, units in inches
ggsave(
  #filename = "LiverRel1f.png",
  filename = "CTFPA_Liver_Female.png",
 # plot = LiverRel1f,
  plot = CTFPA_Liver_Female,
  width = 8,
  height = 6,
  dpi = 300
)

# Display the plot
#LiverRel1f
CTFPA_Liver_Female

