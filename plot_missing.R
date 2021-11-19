library(tidyverse)
library(patchwork)
library(ggnewscale)
plot_missing <- function(dataset, percent = FALSE) {
  
  #upper plot data
  upper_data = data.frame(colname = names(dataset), missing_number = colSums(is.na(dataset))) %>% 
    mutate(missing_percent = 100 * missing_number/sum(missing_number))
  #right plot data
  missing_patterns = data.frame(is.na(dataset)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  missing_patterns$Complete = apply(missing_patterns[, 1:ncol(dataset)], 1, function(x) any(x == TRUE))
  missing_patterns$idx = as.numeric(row.names(missing_patterns))
  missing_patterns = missing_patterns %>% mutate(percent=100*count/sum(count))
  #main plot data
  long = missing_patterns %>% 
    rownames_to_column() %>% 
    pivot_longer(!c(rowname, count, Complete, idx, percent),values_to = 'filling',values_transform =list(filling =as.logical))
  level_order = levels(reorder(upper_data$colname, -upper_data$missing_number))
  
  if (percent) {
    # upper graph
    upper_y = 'missing_percent'
    upper_title = '% rows missing'
    # right graph
    right_y = 'percent'
    right_title = '% rows'
  }
  else{
    # upper graph
    upper_y = 'missing_number'
    upper_title = 'num rows missing'
    # right graph
    right_y = 'count'
    right_title = 'row count'
  }
  x_angle=30
  annotate_y = which(missing_patterns$Complete == FALSE)
  annotate_x = missing_patterns %>% select(!c(count, Complete, idx, percent)) %>% ncol()/2 + 0.5
  upper = ggplot(data = upper_data, aes(x = reorder(colname, -get(upper_y)), y = get(upper_y))) + 
    geom_bar(stat = "identity", fill = "steelblue3", alpha = 0.5) + 
    ylab(upper_title) + 
    theme(axis.title.x = element_blank()) + 
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          panel.grid.major.y  = element_line(colour = "grey", size = rel(0.5)),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = x_angle)) + 
    ggtitle("Missing value patterns")
  
  right = ggplot(data = missing_patterns, aes(x = idx, y = get(right_y), fill = Complete)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = c(alpha("steelblue4", 0.7), alpha("steelblue3", 0.5))) +
    theme(axis.title.y = element_blank(), legend.position = "none") +
    ylab(right_title) +
    scale_x_reverse(labels = missing_patterns$idx, breaks = missing_patterns$idx) +
    theme(
      panel.background = element_rect(fill = "white", colour ="grey50"),
      panel.grid.major.x  = element_line(colour = "grey", size = rel(0.5)),
      panel.grid.minor = element_blank()
    )
  
  left = ggplot(long, aes(x = factor(name, level = level_order),
                          y = idx,
                          fill = filling,
                          alpha = !Complete)) +
    geom_tile(color = "white") +
    scale_y_reverse(labels = long$idx, breaks = long$idx) +
    scale_fill_manual(values = c("grey70", "mediumpurple2")) +
    scale_alpha_manual(values = c(0.6, 1)) +
    xlab("variable") +
    ylab("missing pattern") + # discrete scale
    theme(legend.position = "none") +
    annotate("text",y = annotate_y, x = annotate_x, label = "complete cases") +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),axis.text.x = element_text(angle = x_angle))
  
  layout <- "\nAAAA#\nBBBBD\nBBBBD\nBBBBD\n"
  
  #Final Plot
  upper + left + right +
    plot_layout(design = layout)
}