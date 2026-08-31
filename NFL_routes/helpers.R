plot_routes <- function(player, weeks, outcomes) {
  route_tbl <- paste0(player,"2018app")
  route_data <- get(route_tbl)
  select_routes <- route_data |> filter(week %in% weeks) |> arrange(uPlayId, frameId)
  outcomes_data <- get(paste0(route_tbl,"_results"))
  select_outcomes <- outcomes_data |> filter(week %in% weeks) |> arrange(uPlayId, frameId)
  if(outcomes) {
         ggplot() + 
         geom_path(data = select_routes, aes(y, abs_x, group = uPlayId)) + 
         coord_cartesian(ylim = c(-10,80), xlim = c(0, 53.3)) + 
         theme_minimal() + 
         theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) + 
         geom_point(data = select_outcomes, aes(y, abs_x, color = outcome), cex = 3) + 
         scale_color_manual(values = c("caught" = "#00BA38", "incomplete" = "#F8766D", "interception" = "red", "touchdown" = "purple", "touchdown catch" = "#619CFF"))
    } else {
         ggplot() + 
         geom_path(data = select_routes, aes(y, abs_x, group = uPlayId)) + 
         coord_cartesian(ylim = c(-10, 80), xlim = c(0, 53.3)) + 
         theme_minimal() + 
         theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) + 
         geom_point(data = select_outcomes, aes(y, abs_x, color = outcome), cex = -1) +  # cex = -1 means these dots are hidden in output
         scale_color_manual(values = c("#00BA38", "#F8766D", "red", "purple", "#619CFF"), guide = guide_legend(override.aes = list(alpha = 0))) + 
         theme(legend.title = element_text(color = "transparent"), legend.text = element_text(color = "transparent"))  # hide the legend, but keep it there for plot sizing purposes
        }
}

plot_density <- function(player) {
        plot_data <- get(paste0(player,"2018app"))
        full_name <- player_list |> filter(prefix == player) |> pull(fullName)
        breaks <- exp(seq(log(0.00002), log(0.007), length.out = 80))
        breaks <- c(0, breaks)
        ggplot(data = plot_data, aes(y, abs_x)) + 
          coord_cartesian(ylim = c(-10, 80), xlim = c(0, 53.3)) + 
          geom_density_2d_filled(breaks = breaks) + 
          theme_minimal() + 
          theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) + 
          ggtitle(paste(full_name, " route density map - 2018 all games"))
}