# Setup
source("Scripts/setup.R")


# Data exploration
# Trying to see if SLR has different total advisory level than other sites (higher is "better")

# For the purposes of this analysis, I am going to separate out advisories that are driven by contaminants other than mercury.

cons_data = cons_data %>%
  filter(adv_cause_desc != "PFAS")

# Grouping
cons_sum = cons_data %>%
  group_by(guide_locname_eng, population_type_id) %>% # Groups variables by waterbody and advisory population type (general or sensitive)
  mutate(population_type_id = as.factor(population_type_id)) %>%
  summarize(total_adv_level = sum(adv_level)/n()) # Summarizes waterbodies by the TOTAL advisory level (sum of all advisories) divided by number of columns to standardize sample size/# of fish species


sum_plot = ggplot(cons_sum, aes(x = guide_locname_eng, y = total_adv_level, col = population_type_id)) +
  geom_point() +
  geom_point(col = "red", size = 5, data = ~subset(., guide_locname_eng == "St. Lawrence River (Lake St. Francis)")) #To make our AOC stand out
sum_plot
# Ok but impossible to read. Lets try comparing group average to overall

sum_avg = cons_data %>%
 mutate(locname_2 = fct_other(guide_locname_eng, keep = c("St. Lawrence River (Lake St. Francis)", "St. Lawrence River (Lake Ontario to Cornwall)"))) %>% # Isolates area of interest and reference site
  group_by(locname_2, population_type_id) %>% # Groups variables by waterbody and advisory population type (general or sensitive)
  summarize(mean = mean(adv_level), sd = sd(adv_level), median = median(adv_level)) %>% # Summarizes waterbodies by the TOTAL advisory level (sum of all advisories) divided by number of columns to standardize sample size/# of fish species
  mutate(population_type_id = as.factor(population_type_id))

basic_plot = ggplot(sum_avg, aes(x = locname_2, y = mean, fill = population_type_id)) +
  geom_col(position=position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width = 0.2),position=position_dodge(0.8)) +
  labs(x = "Water Body", y = "Mean advisory level (meals per month)", fill = "Population Type") +
  scale_fill_discrete(labels = c("General", "Sensitive"))
  
  
basic_plot
# We do see that LSF is more restrictive, but there is a lot of variation.


# Ok let's zoom in a bit - by species.
lsf_spec = cons_data %>%
  filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)") %>% # Applies cases across the same species found in LSF
  distinct(specname)

lsf_spec_med = cons_data %>%
  filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)") %>% # Applies cases across the same species found in LSF
  group_by(specname) %>%
  summarize(median_size = median(length_category_id))

spec_sum = cons_data %>% 
  semi_join(lsf_spec, by = "specname") %>%
  semi_join(lsf_spec_med, by = "specname") %>%
  #mutate(locname_2 = fct_other(guide_locname_eng, keep = c("St. Lawrence River (Lake St. Francis)", "St. Lawrence River (Lake Ontario to Cornwall)"))) %>% # Isolates area of interest and reference site
  filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)" | guide_locname_eng =="St. Lawrence River (Lake Ontario to Cornwall)") %>%
  group_by(guide_locname_eng, specname, population_type_desc) %>% # Groups variables by waterbody and advisory population type (general or sensitive)
  summarize(mean = mean(adv_level), sd = sd(adv_level), med_adv = median(adv_level)) #%>%  # Summarizes by advisory level per species
  #mutate(locname_2 = factor(locname_2, levels = c("St. Lawrence River (Lake St. Francis)", "St. Lawrence River (Lake Ontario to Cornwall)", "Other")))



spec_plot = ggplot(spec_sum, aes(x = med_adv, y = specname, fill = guide_locname_eng)) +
  geom_bar(stat ="identity",position=position_dodge2(1, preserve = "single"), width = 0.8, color = "#191e28") +
  scale_fill_manual(values = c("#4276b1","goldenrod"), guide = guide_legend(reverse = T), labels = c("Reference", "AOC")) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd, width = 0.2),position=position_dodge(0.8)) +
  labs(x = "Median Advisory Level (meals per month)", y = "Fish Species", fill = "Water Body") +
  scale_x_continuous(limits = c(0,32), expand = c(0,0)) +
  theme_aaron() 

spec_plot_gen = spec_plot %+% subset(spec_sum, population_type_desc == "General")
spec_plot_sens = spec_plot %+% subset(spec_sum, population_type_desc == "Sensitive")
spec_plot
spec_plot_gen



spec_plot_combined= ggarrange(spec_plot_gen,spec_plot_sens,
                              common.legend = T,
                              legend = "bottom",
                              vjust = 1.25,
                              hjust = -1,
                              labels = c("General","Sensitive"))

spec_plot_combined

# These are fine as mean and SD, but don't quite capture information for all size of fish for each spceies. I will try to stack and group the chart to better represent the whole picture


spec_size_gen = cons_data %>%
  semi_join(lsf_spec, by = "specname") %>%
  filter(population_type_id == "1", guide_locname_eng == "St. Lawrence River (Lake St. Francis)" | guide_locname_eng =="St. Lawrence River (Lake Ontario to Cornwall)") %>%
  group_by(guide_locname_eng, specname, length_category_label) %>% # Groups variables by waterbody and advisory population type (general or sensitive)
  #summarize(mean = mean(adv_level), sd = sd(adv_level)) %>%  # Summarizes by advisory level per species
  mutate(guide_locname_eng = factor(guide_locname_eng, levels = c("St. Lawrence River (Lake St. Francis)", "St. Lawrence River (Lake Ontario to Cornwall)", "Other"))) %>%
  mutate(length_category_label = factor(length_category_label, levels = c("15-20cm","20-25cm","25-30cm","30-35cm","35-40cm","40-45cm","45-50cm","50-55cm","55-60cm","60-65cm","65-70cm","70-75cm",">75cm")))


spec_size_sens = cons_data %>%
  semi_join(lsf_spec, by = "specname") %>%
  filter(population_type_id == "2", guide_locname_eng == "St. Lawrence River (Lake St. Francis)" | guide_locname_eng =="St. Lawrence River (Lake Ontario to Cornwall)") %>%
  group_by(guide_locname_eng, specname, length_category_label) %>% # Groups variables by waterbody and advisory population type (general or sensitive)
  #summarize(mean = mean(adv_level), sd = sd(adv_level)) %>%  # Summarizes by advisory level per species
  mutate(guide_locname_eng = factor(guide_locname_eng, levels = c("St. Lawrence River (Lake St. Francis)", "St. Lawrence River (Lake Ontario to Cornwall)", "Other"))) %>%
  mutate(length_category_label = factor(length_category_label, levels = c("15-20cm","20-25cm","25-30cm","30-35cm","35-40cm","40-45cm","45-50cm","50-55cm","55-60cm","60-65cm","65-70cm","70-75cm",">75cm")))


spec_size_plot_gen = ggplot(spec_size_gen) +
  geom_bar(aes(x = adv_level, y = fct_rev(guide_locname_eng), fill = length_category_label),stat="identity",position="stack") +
  facet_grid(rows=vars(specname)) +
  scale_fill_manual(values = coul)+
  labs(x = "Advisory level (meals per month)", y = "Water Body", fill = "Length Category") +
  scale_x_continuous(limits = c(0,75), expand = c(0,0)) +
  theme_aaron() +
  theme(axis.text.y = element_text(size = 10),
        strip.text.y = element_blank())

spec_size_plot_gen


spec_size_plot_sens = ggplot(spec_size_sens) +
  geom_bar(aes(x = adv_level, y = fct_rev(guide_locname_eng), fill = length_category_label),stat="identity",position="stack") +
  facet_grid(rows=vars(specname)) +
  scale_fill_manual(values = coul)+
  labs(x = "Advisory level (meals per month)", y = "Water Body", fill = "Length Category") +
  scale_x_continuous(limits = c(0,75), expand = c(0,0)) +
  theme_aaron() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y = element_text(angle = 0))

spec_size_plot_sens

spec_size_plot_combined= ggarrange(spec_size_plot_gen,spec_size_plot_sens,
                              common.legend = T,
                              legend = "bottom",
                              vjust = 1.25,
                              hjust = -1,
                              widths = c(1,0.75),
                              labels = c("General","Sensitive"))

spec_size_plot_combined





# Vector for AOCs
aocs = c("St. Lawrence River (Lake St. Francis)","Thunder Bay (Lake Superior)","Nipigon Bay (Lake Superior)","Peninsula Harbour (Lake Superior)","St. Marys River","St. Clair River","Detroit River (Lower reach)","Detroit River (Upper reach)","Niagara River (Lower reach)","Niagara River (Upper reach)","Hamilton Harbour (Lake Ontario)","Toronto Waterfront Area (Lake Ontario)","	
Ganaraska River","Lake Ontario (Eastern Basin)")

spec_sum_aocs_gen = cons_data %>%
  semi_join(lsf_spec, by = "specname") %>%
  filter(population_type_id == "1", guide_locname_eng %in% aocs) %>%
  group_by(guide_locname_eng, specname) %>% # Groups variables by waterbody and advisory population type (general or sensitive)
  summarize(mean = mean(adv_level), sd = sd(adv_level))  # Summarizes by advisory level per species
  
spec_sum_aocs_gen$guide_locname_eng = factor(spec_sum_aocs_gen$guide_locname_eng,levels=aocs)

aoc_gen = ggplot(spec_sum_aocs_gen, aes(x = mean, y = fct_rev(guide_locname_eng))) +
  geom_bar(stat ="identity",position=position_dodge2(1, preserve = "single"), width = 0.8, color = "#191e28") +
  #scale_fill_manual(values = c("#44506b","#4276b1","#adcbe0"), guide = guide_legend(reverse = T)) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd, width = 0.2),position=position_dodge(0.8)) +
  labs(x = "Advisory level (meals per month)", y = "Fish Species", fill = "Water Body") +
  scale_x_continuous(limits = c(0,32), expand = c(0,0)) +
  facet_wrap(~specname) +
  theme_aaron() +
  theme(axis.text = element_text(size = 10))
aoc_gen



###############
# STATS

# Separated dataset
dat = cons_data %>%
  semi_join(lsf_spec, by = "specname") %>%
  filter(population_type_id == "1", guide_locname_eng == "St. Lawrence River (Lake St. Francis)" | guide_locname_eng =="St. Lawrence River (Lake Ontario to Cornwall)") %>%
  select(guide_locname_eng,adv_level,length_category_label,specname) %>%
  mutate(guide_locname_eng = factor(guide_locname_eng, levels = c("St. Lawrence River (Lake St. Francis)", "St. Lawrence River (Lake Ontario to Cornwall)")), adv_level = factor(adv_level))



### Below is essentially junk because I went the wrong direction. I was trying to use stats to show a significant difference in one way or another between sites
### But using models doesn't really make sense for this type of data. Really what I want is to summarize the differences between the two sites which I will do in the next section
### However, the ANOVA still stands, and shows at least that the two sites do statistically differ in general advisory level.
all_combinations = expand_grid(
  guide_locname_eng = unique(dat$guide_locname_eng),
  specname = unique(dat$specname),
  length_category_label = unique(dat$length_category_label)
)



missing_combinations = anti_join(all_combinations, dat, by = c("guide_locname_eng","specname","length_category_label"))

dat_full = full_join(dat, all_combinations, by = c("guide_locname_eng","specname","length_category_label"))
dat_full$adv_level[is.na(dat_full$adv_level)] = NA

#ANOVA, just to check
cons_aov = aov(adv_level~guide_locname_eng, na.rm = T,dat=dat_full)
summary(cons_aov)
#The two sites do differ in general advisory level.


#GLMM
cons_glmm = clmm(adv_level ~ guide_locname_eng * specname * length_category_label + (1|specname), data = dat_full)
summary(cons_glmm)


#Cumulative link
cons_cml = clm(adv_level ~ guide_locname_eng, data= dat)
summary(cons_cml)



################
# NOT QUITE STATS- SUMMARIES?

# Summarize differences
dat = cons_data %>%
  semi_join(lsf_spec, by = "specname") %>%
  filter(population_type_id == "2", guide_locname_eng == "St. Lawrence River (Lake St. Francis)" | guide_locname_eng =="St. Lawrence River (Lake Ontario to Cornwall)") %>%
  select(guide_locname_eng,adv_level,length_category_label,specname)# %>%
 # mutate(guide_locname_eng = factor(guide_locname_eng, levels = c("St. Lawrence River (Lake St. Francis)", "St. Lawrence River (Lake Ontario to Cornwall)")), adv_level = factor(adv_level))

# Creating a summary table that shows what species/sizes are more restrictive in the AOC vs reference
cons_summary_table = dat %>%
  group_by(specname, length_category_label) %>%
  summarize(
    aoc_mean = mean(adv_level[guide_locname_eng == "St. Lawrence River (Lake St. Francis)"]),
    reference_mean = mean(adv_level[guide_locname_eng == "St. Lawrence River (Lake Ontario to Cornwall)"]),
    difference = aoc_mean - reference_mean 
  ) %>%
  mutate(more_restrictive = difference < 0)

# SHOULD BE NOTED THAT NOT ALL ADVISORIES ARE MATCHED IN REFERENCE SITE -> MANY CASES WHERE THERE IS NO MATCHING DATA, AND NO CONCLUSION CAN BE MADE

# Summarize cases where advisories at the target site are more restrictive
summary_by_species <- cons_summary_table %>%
  group_by(specname) %>%
  summarize(
    stricter_count = sum(more_restrictive, na.rm = T),
    total = sum(!is.na(more_restrictive)),
    proportion_stricter = stricter_count / total
  )


ggplot(summary_by_species, aes(x = specname, y = proportion_stricter)) +
  geom_col(fill = "darkgrey",col="black") +
  labs(x = "Species", y = "Proportion More Restrictive", title = "Stricter Advisories by Species") +
  theme_aaron() +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  coord_flip()



## Using this for pie chart
grouped_adv_data = dat %>%
  mutate(grouped_adv_level = cut(adv_level, breaks = c(-Inf,0,4, 12, 16, 32), labels = c("0% of Max", "25", "25-50", "50", "Max")))

pie_data = grouped_adv_data %>%
  group_by(guide_locname_eng, grouped_adv_level) %>%
  summarize(
    n = n()
  )

