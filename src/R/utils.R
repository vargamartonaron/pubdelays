library(ggplot2)


#Create data

set.seed(123)
n <- 100  
group <- rep(c("covid", "non", "skib", "idi", "vbsdvs", "feds"), each = n)
var1 <- sample(1:1000, size = 2 * n, replace = TRUE)
var2 <- sample(10:100, size = 2 * n, replace = TRUE)
var3 <- sample(2016:2023, size = 2 * n, replace = TRUE)
var4 <- sample(1:12, size = 2 * n, replace = TRUE)
simulated_data <- data.frame(Group = group, delay = var1, citation = var2, year = var3, month = var4)

safe_colorblind_palette <- c("#88CCEE", "#332288", "#CC6677", "#AA4499", 
                            "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888", "#DDCC77",  "#117733")

#Test plot

simulated_data |>
  ggplot(aes(x = as.factor(Group), y = delay, fill = as.factor(Group))) +
  geom_violin(alpha = 1) + 
  labs(title = "Title", subtitle = "Subtitle", tag = "Tag") +
  theme_bw() +
  scale_fill_manual(values = safe_colorblind_palette) +
  theme(
    text = element_text(size = 12, family = "serif"))


#Visual function

plot_visuals <- function(title, subtitle, tag, n, with_legend = FALSE, legend_name = NULL, x_axis_name = NULL, y_axis_name = NULL, use_fill = TRUE) {
  
  # Check if the plot uses 'fill' or 'color'
  if (use_fill) {
    fill_layer <- scale_fill_manual(values = safe_colorblind_palette)
    color_layer <- NULL
  } else {
    fill_layer <- NULL
    color_layer <- scale_color_manual(values = safe_colorblind_palette)
  }
  
  layers <- list(
    labs(
      title = title,
      subtitle = subtitle,
      tag = tag,
      x = x_axis_name, 
      y = y_axis_name, 
      fill = if (with_legend & use_fill) legend_name else NULL,
      color = if (with_legend & !use_fill) legend_name else NULL
    ),
    theme_classic(),
    theme(
      plot.title = element_text(size = 12, family = "serif", face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 12, family = "serif", face = "italic", hjust = 0),
      plot.tag = element_text(size = 10, family = "serif"),
      text = element_text(size = 12, family = "serif"),
      legend.position = if (with_legend) "right" else "none",
      
      
      # Fine-tune title and subtitle positioning to be over the y-axis
      plot.title.position = "plot",  # Title will be placed relative to plot area, not outside it
      plot.subtitle.position = "plot"  # Same for subtitle
    ),
    fill_layer,    # Only applied if use_fill is TRUE
    color_layer    # Only applied if use_fill is FALSE
  )
  
  return(layers)
}



# plot_visuals <- function(title, subtitle, tag, n, with_legend = FALSE, legend_name = NULL, x_axis_name = NULL, y_axis_name = NULL, use_fill = TRUE) {
#   
#   # Check if the plot uses 'fill' or 'color'
#   if (use_fill) {
#     fill_layer <- scale_fill_manual(values = safe_colorblind_palette)
#     color_layer <- NULL
#   } else {
#     fill_layer <- NULL
#     color_layer <- scale_color_manual(values = safe_colorblind_palette)
#   }
#   
#   layers <- list(
#     labs(
#       title = title,
#       subtitle = subtitle,
#       tag = tag,
#       x = x_axis_name, 
#       y = y_axis_name, 
#       fill = if (with_legend & use_fill) legend_name else NULL,
#       color = if (with_legend & !use_fill) legend_name else NULL
#     ),
#     theme_bw(),
#     theme(text = element_text(size = 12, family = "serif")),
#     theme(legend.position = if (with_legend) "right" else "none"),
#     fill_layer,    # Only applied if use_fill is TRUE
#     color_layer    # Only applied if use_fill is FALSE
#   )
#   
#   return(layers)
# }

#Test plot with function

#Violin
simulated_data |>
  ggplot(aes(x = as.factor(Group), y = delay, fill = as.factor(Group))) +
  geom_violin(alpha = 1) + 
  plot_visuals("Title", "Bruh", "Tag", 4, T, "Skibidi", "sajt32", "sajt33")

