plot_moderators <- function(all_mod_df){
  
  axis_font_size = 6
  title_font_size = 8
  ## behavioral measure
  
  # shorten label 
  
  all_mod_df <- all_mod_df %>% 
    mutate(dataset_short = case_when(
      dataset == "Cross-situational word learning" ~ "X-situation",
      dataset == "Infant directed speech preference" ~ "IDS pref",
      dataset ==  "Label advantage in concept learning" ~ "Label adv", 
      dataset ==  "Language discrimination and preference" ~ "Lang disc & pref", 
      dataset ==  "Mutual exclusivity" ~ "Mutual exclusivity", 
      dataset ==  "Natural speech preference"  ~ "Natural speech preference", 
      dataset ==  "Sound symbolism"  ~ "Sound symbolism" , 
      dataset ==  "Syntactic bootstrapping"     ~ "Syntactic bootstrapping", 
      dataset ==  "Vowel discrimination (native)"  ~ "Vowel disc (native)" , 
      dataset ==  "Vowel discrimination (non-native)" ~ "Vowel disc (non-native)", 
      dataset ==  "Abstract rule learning" ~ "Abstract rule learning", 
      dataset ==  "Familiar word recognition" ~ "Fam word recog", 
      dataset ==  "Mispronunciation sensitivity" ~ "Mispronunciation sensitivity", 
      dataset ==  "Simple arithmetic competences" ~ "Arithmetic", 
      dataset ==  "Word Segmentation (combined)" ~ "Word seg", 
      dataset ==  "Categorization bias" ~ "Categorization bias", 
      dataset ==  "Prosocial agents" ~ "Prosocial agents", 
      dataset ==  "Statistical word segmentation" ~ "Stat word seg", 
      dataset ==  "Online word recognition" ~ "Online word recog", 
      dataset ==  "Switch task" ~ "Switch task", 
      dataset ==  "Symbolic play" ~ "Symbolic play", 
      dataset ==  "Statistical sound category learning (habituation)" ~ "Stat sound category",
      dataset ==  "Gaze following (combined)" ~ "Gaze following"
    ))
  
   bm_p <- 
    all_mod_df %>% 
    filter(grepl("Behavioral", type)) %>% 
    mutate(group = case_when(
      group == "Other" ~ "Manual", 
      TRUE ~ group
    )) %>% 
    ggplot(aes(x = reorder(dataset_short,estimate), y = estimate, color = group)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0, linetype = "dashed")+ 
    #coord_flip() +
    ylim(-2.5, 3)+ 
    xlab("") + 
    ylab("") + 
    labs(title = "Behavioral Measure (Baseline: looking)") + 
    theme_few() +
    theme(
      legend.position = "top",
      axis.text=element_text(size=axis_font_size,angle = 90, vjust = 0.5, hjust=1),

            legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0, "mm"),
      legend.spacing.y = unit(0, "mm"),
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      plot.title = element_text(hjust = 0.5, size = title_font_size)
    )  

  ## exposure phase
  
  ep_mod_p <- 
    all_mod_df %>% 
    filter(grepl("Exposure", type)) %>% 
    ggplot(aes(x = reorder(dataset_short,estimate), y = estimate, color = group)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0, linetype = "dashed")+ 
    #coord_flip() +
    ylim(-2.5, 3)+ 
    xlab("") + 
    ylab("") + 
    labs(title = "Exposure phase (Baseline: Conditioning)") + 
    theme_few() +
    theme(
      legend.position = "top",
      axis.text=element_text(size=axis_font_size,angle = 90, vjust = 0.5, hjust=1),
      #legend.position = c(0.7, 0.2),
      legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0, "mm"),
      legend.spacing.y = unit(0, "mm"),
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      plot.title = element_text(hjust = 0.5, size = title_font_size)
    )  

  ## visual 
  

  natural_mod_p <- 
    all_mod_df %>% 
    filter(grepl("Visual stimulus", type) | grepl("Auditory stimulus", type)) %>% 
    ggplot(aes(x = reorder(dataset_short,estimate), y = estimate, color = group)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0, linetype = "dashed")+ 
    #coord_flip() +
    ylim(-2.5, 3)+ 
    xlab("") + 
    ylab("") + 
    labs(title = "Stimuli Naturalness (Baseline: Artificial stimulus)") + 
    theme_few() +
    theme(
      axis.text=element_text(size=axis_font_size),
      legend.position = "none",
      legend.title = element_blank(),
      
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      plot.title = element_text(hjust = 0.5, size = title_font_size)
    )  



  (bm_p | ep_mod_p) / (natural_mod_p)
  
  
}



plot_major_author <- function(all_mod_df){
  axis_font_size = 6
  
  all_mod_df <- all_mod_df %>% 
    mutate(dataset_short = case_when(
      dataset == "Cross-situational word learning" ~ "X-situation",
      dataset == "Infant directed speech preference" ~ "IDS pref",
      dataset ==  "Label advantage in concept learning" ~ "Label adv", 
      dataset ==  "Language discrimination and preference" ~ "Lang disc & pref", 
      dataset ==  "Mutual exclusivity" ~ "Mutual exclusivity", 
      dataset ==  "Natural speech preference"  ~ "Natural speech preference", 
      dataset ==  "Sound symbolism"  ~ "Sound symbolism" , 
      dataset ==  "Syntactic bootstrapping"     ~ "Syntactic bootstrapping", 
      dataset ==  "Vowel discrimination (native)"  ~ "Vowel disc (native)" , 
      dataset ==  "Vowel discrimination (non-native)" ~ "Vowel disc (non-native)", 
      dataset ==  "Abstract rule learning" ~ "Abstract rule learning", 
      dataset ==  "Familiar word recognition" ~ "Fam word recog", 
      dataset ==  "Mispronunciation sensitivity" ~ "Mispronunciation sensitivity", 
      dataset ==  "Simple arithmetic competences" ~ "Arithmetic", 
      dataset ==  "Word Segmentation (combined)" ~ "Word seg", 
      dataset ==  "Categorization bias" ~ "Categorization bias", 
      dataset ==  "Prosocial agents" ~ "Prosocial agents", 
      dataset ==  "Statistical word segmentation" ~ "Stat word seg", 
      dataset ==  "Online word recognition" ~ "Online word recog", 
      dataset ==  "Switch task" ~ "Switch task", 
      dataset ==  "Symbolic play" ~ "Symbolic play", 
      dataset ==  "Statistical sound category learning (habituation)" ~ "Stat sound category",
      dataset ==  "Gaze following (combined)" ~ "Gaze following"
    ))
  
  
    all_mod_df %>% 
    filter(grepl("Author", type)) %>% 
    distinct(dataset, dataset_short, estimate, lb, ub) %>% 
    ggplot(aes(x = reorder(dataset_short,estimate), y = estimate)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0, linetype = "dashed")+ 
    coord_flip()+
    xlab("") + 
    ylab("") + 
  
    labs(title = "Major author \n (Baseline: Non-Major author)") + 
    theme_few() +
    theme(
      legend.position = c(0.8, 0.1),
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      axis.text=element_text(size=axis_font_size),
      plot.title = element_text(hjust = 0.5, size = 6)
    )  
  
  
}

