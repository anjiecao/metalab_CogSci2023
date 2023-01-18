plot_moderators <- function(all_mod_df){
  
  axis_font_size = 6
  title_font_size = 8
  ## behavioral measure
  
  bm_p <- 
    all_mod_df %>% 
    filter(grepl("Behavioral", type)) %>% 
    ggplot(aes(x = reorder(dataset,estimate), y = estimate, color = group)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0, linetype = "dashed")+ 
    coord_flip() +
    xlab("") + 
    ylab("") + 
    labs(title = "Behavioral Measure (Baseline: looking)") + 
    theme_few() +
    theme(
      legend.position = c(0.8, 0.15),
      axis.text=element_text(size=axis_font_size),
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      plot.title = element_text(hjust = 0.5, size = title_font_size)
    )  

  ## exposure phase
  
  ep_mod_p <- 
    all_mod_df %>% 
    filter(grepl("Exposure", type)) %>% 
    ggplot(aes(x = reorder(dataset,estimate), y = estimate, color = group)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0, linetype = "dashed")+ 
    coord_flip() +
    scale_x_discrete(position = "top")+
    xlab("") + 
    ylab("") + 
    labs(title = "Exposure phase (Baseline: Conditioning)") + 
    theme_few() +
    theme(
      axis.text=element_text(size=axis_font_size),
      legend.position = c(0.7, 0.2),
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      plot.title = element_text(hjust = 0.5, size = title_font_size)
    )  

  ## visual 
  

  vr_mod_p <- 
    all_mod_df %>% 
    filter(grepl("Visual stimulus", type)) %>% 
    ggplot(aes(x = reorder(dataset,estimate), y = estimate, color = group)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0, linetype = "dashed")+ 
    coord_flip() +
    scale_x_discrete(position = "top")+
    xlab("") + 
    ylab("") + 
    labs(title = "Visual stimulus type \n(Baseline: representation-type stimulus)") + 
    theme_few() +
    theme(
      axis.text=element_text(size=axis_font_size),
      legend.position = c(0.8, 0.1),
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      plot.title = element_text(hjust = 0.5, size = title_font_size)
    )  

  ## auditory 
  
  ar_mod_p <- all_mod_df %>% 
    filter(grepl("Auditory stimulus", type)) %>% 
    ggplot(aes(x = reorder(dataset,estimate), y = estimate, color = group)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0, linetype = "dashed")+ 
    coord_flip() +
    xlab("") + 
    ylab("") + 
    labs(title = "Auditory stimulus type \n(Baseline: artificial stimulus)") + 
    theme_few() +
    theme(
      axis.text=element_text(size=axis_font_size),
      legend.position = c(0.8, 0.1),
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      plot.title = element_text(hjust = 0.5, size = title_font_size)
    )  



  
  (bm_p | ep_mod_p) / (ar_mod_p  | vr_mod_p)
  
  
}