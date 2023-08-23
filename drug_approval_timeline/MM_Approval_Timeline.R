library(tidyverse)
library(scales)
library(lubridate)
library(ggrepel)
library(googlesheets4)

# Set working directory to user desktop so outputs have an expected destination
setwd("~/Desktop")

# Read in table from lab website
drugs <- read_sheet("https://docs.google.com/spreadsheets/d/15r99MdYzkRRdhKcOSFV2w1EPEWTz6yjLnMGlJr2MDAc/edit?usp=sharing", sheet = "Initial_Approvals") %>% arrange(FDA_Approval_Date)

# Read in table if excel downloaded from website
#drugs <- readxl::read_xlsx("Multiple_Myeloma_Drug_and_Regimen_Approvals.xlsx", sheet = "Initial_Approvals") %>% arrange(FDA_Approval_Date)

# Add year and month columns
drugs <- drugs %>% mutate(Year = year(FDA_Approval_Date)) %>% 
  mutate(Month = month(FDA_Approval_Date, abbr = FALSE))

# Get the number of drugs
drug_count <- drugs %>% count() %>% pull()

# Add direction flags to alternate each drug on the top and bottom of the plot
drugs <- drugs %>% mutate(direction = rep(c(1,-1), length.out = drug_count))

# Determine how close each approval was in time and then set dynamic plotting locations
# Might require updates as table grows
drugs <- drugs %>% group_by(direction) %>% 
  mutate(LagDate = lag(FDA_Approval_Date), 
         Days_after = as.numeric(difftime(ymd(FDA_Approval_Date), ymd(LagDate), units = "days")),
         LeadDate = lead(FDA_Approval_Date), 
         Days_before = as.numeric(difftime(ymd(LeadDate), ymd(FDA_Approval_Date), units = "days"))) %>% 
  ungroup() %>% 
  mutate(y_start = case_when(direction == 1 & is.na(Days_after) ~ 0.03, 
                             direction == -1 & is.na(Days_after) ~ -0.03, 
                             direction == 1 & Days_after > 300 & Days_before > 300 ~ 0.03, 
                             direction == -1 & Days_after > 300 & Days_before > 300 ~ -0.03, 
                             direction == 1 & Days_after > 300 & Days_before < 300 ~ 0.03, 
                             direction == -1 & Days_after > 300 & Days_before < 300 ~ -0.03,
                             direction == 1 & Days_after < 300 & Days_before < 300 ~ 0.1, 
                             direction == -1 & Days_after < 300 & Days_before < 300 ~ -0.1, 
                             direction == 1 & Days_after < 300 & Days_before > 300 ~ 0.1, 
                             direction == -1 & Days_after < 300 & Days_before > 300 ~ -0.1, 
                             direction == 1 & is.na(Days_before) ~ 0.03, 
                             direction == -1 & is.na(Days_before) ~ -0.03, 
                             TRUE ~ 0.2), 
         position = case_when(direction == 1 & is.na(Days_after) ~ 0.055, 
                             direction == -1 & is.na(Days_after) ~ -0.055, 
                             direction == 1 & Days_after > 300 & Days_before > 300 ~ 0.055, 
                             direction == -1 & Days_after > 300 & Days_before > 300 ~ -0.055, 
                             direction == 1 & Days_after > 300 & Days_before < 300 ~ 0.055, 
                             direction == -1 & Days_after > 300 & Days_before < 300 ~ -0.055, 
                             direction == 1 & Days_after < 300 & Days_before < 300 ~ 0.125, 
                             direction == -1 & Days_after < 300 & Days_before < 300 ~ -0.125, 
                             direction == 1 & Days_after < 300 & Days_before > 300 ~ 0.125, 
                             direction == -1 & Days_after < 300 & Days_before > 300 ~ -0.125, 
                             direction == 1 & is.na(Days_before) ~ 0.055, 
                             direction == -1 & is.na(Days_before) ~ -0.055, 
                             TRUE ~ 0.2))

# Add text positions, dynamically positioned by the direction and dynamic position
drugs <- drugs %>% 
  mutate(text_position = 0.015 * direction + position) %>% 
  mutate(date = ymd(sprintf('%04d%02d%02d', Year, Month, 1)))

# Add Targetname so a plot version can include drug name followed by carriage return to put target gene directly below the drug name
drugs <- drugs %>% 
  mutate(TargetName = case_when(Target == "CD38" ~ str_c(CommonName, "\n", Target), 
                                Target == "BCMA" ~ str_c(CommonName, "\n", Target), 
                                Target == "GPRC5D" ~ str_c(CommonName, "\n", Target), 
                                TRUE ~ CommonName))

################################################
## Dynamic Plot Definitions for X-axis range
################################################

# create month dataframe
month_buffer <- 6
month_date_range <- seq(min(drugs$date) - months(month_buffer), max(drugs$date) + months(month_buffer), by = 'month')
# If we want the label in 3 character 
# month_format <- format(month_date_range, '%b')
# If we want the label in numbers
month_format <- as.numeric(format(month_date_range, '%m'))
month_df <- data.frame(month_date_range, month_format)


# Create year dataframe
year_date_range <- seq(min(drugs$date) - months(month_buffer), max(drugs$date) + months(month_buffer), by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)

# Add next year's label if month buffer includes additional year
year_date_range <- if(
  format(max(drugs$date) + months(month_buffer),'%Y') == format(max(year_date_range),"%Y")){
  year_date_range
} else {
  c(year_date_range, floor_date(max(drugs$date) + months(month_buffer), unit="year"))
}

year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)



# Specify status colors and levels, colors are color-blind safe
status_colors <- c('#d73027','#fc8d59','#fee090','#ffffbf','#e0f3f8','#91bfdb','#4575b4')
status_levels <- c("Antibody","Antibody Drug Conjugate",'CAR-T', 'IMiD','Other', 'Proteasome Inhibitor', 'T-cell Engager')


#############################
## Plot by Tradename
#############################
ggplot(drugs,aes(x=date,y=0, col=Type, label=Tradename)) +
  labs(col="Tradename") +
  scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE) +
  theme_classic() + 

  # Plot horizontal black line for timeline
  geom_hline(yintercept=0, color = "black", size=5.0) + 
  
  # Plot vertical segment lines for milestones
  geom_segment(data=drugs, aes(y=position,yend=y_start,xend=date), color='black', size=0.2) +
  
  # Plot scatter points at zero and date
  geom_point(data = drugs, aes(y=y_start), size=3) + 
  
  # Don't show axes, appropriately position legend
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(), 
        legend.position = "bottom", 
        legend.title=element_blank()) + 
  
  # Show year text
  geom_text(data = year_df, aes(x=year_date_range,y=0.0,label=year_format, fontface="bold"),size=3, color='white') + 
  
  # Show text for each milestone
  geom_text_repel(data = drugs, aes(y=text_position,label=Tradename),size=3.2, point.size=NA, color = "black")  +
  
  # Expand the graph so that all text are within the figure boundaries
  scale_x_continuous(
    expand = expansion(mult = 0.05))

ggsave("MM_Drug_Approvals_TradeName.png", width = 10, height = 3, dpi = 300, units = "in")


#############################
## Plot by CommonName
#############################
ggplot(drugs,aes(x=date,y=0, col=Type, label=CommonName)) +
  labs(col="CommonName") +
  scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE) +
  theme_classic() + 
  
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0, color = "black", size=5.0) + 
  
  # Plot vertical segment lines for milestones
  geom_segment(data=drugs, aes(y=position,yend=y_start,xend=date), color='black', size=0.2) +
  
  # Plot scatter points at zero and date
  geom_point(data = drugs, aes(y=y_start), size=3) + 
  
  # Don't show axes, appropriately position legend
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(), 
        legend.position = "bottom", 
        legend.title=element_blank()) + 
  
  # Show year text
  geom_text(data = year_df, aes(x=year_date_range,y=0.0,label=year_format, fontface="bold"),size=3, color='white') + 
  
  # Show text for each milestone
  geom_text_repel(data = drugs, aes(y=text_position,label=CommonName),size=3.2, point.size=NA, color = "black")  +
  
  # Expand the graph so that all text are within the figure boundaries
  scale_x_continuous(
    expand = expansion(mult = 0.05))

ggsave("MM_Drug_Approvals_CommonName.png", width = 10, height = 3, dpi = 300, units = "in")



#############################
## Plot by TargetName
#############################
ggplot(drugs,aes(x=date,y=0, col=Type, label=CommonName)) +
  labs(col="CommonName") +
  scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE) +
  theme_classic() + 
  
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0, color = "black", size=5.0) + 
  
  # Plot vertical segment lines for milestones
  geom_segment(data=drugs, aes(y=position,yend=y_start,xend=date), color='black', size=0.2) +
  
  # Plot scatter points at zero and date
  geom_point(data = drugs, aes(y=y_start, shape = Target), size=3) + 
  scale_shape_manual(values = c(15,16,17,18)) +
  
  # Don't show axes, appropriately position legend
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(), 
        legend.position = "bottom", 
        legend.title=element_blank()) + 
  
  # Show year text
  geom_text(data = year_df, aes(x=year_date_range,y=0.0,label=year_format, fontface="bold"),size=3, color='white') + 
  
  # Show text for each milestone
  geom_text_repel(data = drugs, aes(y=text_position,label=CommonName),size=3.2, point.size=NA, color = "black")  +
  
  # Expand the graph so that all text are within the figure boundaries
  scale_x_continuous(
    expand = expansion(mult = 0.05))

ggsave("MM_Drug_Approvals_TargetName.png", width = 10, height = 3, dpi = 300, units = "in")


#############################
## Acknowledgment
#############################

# Much of this timeline plot structure was adopted from (https://pharmacoecon.me/post/2021-04-18-timeline-graph/) by Chong H. Kim











