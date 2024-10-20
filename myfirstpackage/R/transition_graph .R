#' Transition Analysis Graph
#'
#' This function plot of a transition analysis, studying the transition from a certain source of income to other source of income. We consider in particular only five sources: unemployment, pension, sick benefit, minimum wage, and earned wage.
#'
#' @param title Title of the graph
#' @param png Path where we want to save the file. It is a string that should end in .png. If png = FALSE, the function will plot the graph in R without saving it.
#' @param data Data frame containing the information to construct the graph. It needs to have these three columns at least:
#'             "value" containing the numeric value of the main source of income express in euro per year
#'             "category" containing the name of the main source of income
#'             "year" containing the reference year
#' @import ggplot2
#' @examples
#'
#'  transition_graph (png = c("C:\Users\...\Desktop\"), data = mydata, title = c("Transition from receiving an unemployment benefit to other social protection"))
#'
#' @return This function return a plot, representing the percentage receiving a certain source of income as their main source of income over time.
#'
#' @export


transition_graph <- function (png, data, title) {

  if (png == FALSE) {

    ggplot(data, aes(x = as.numeric(year), y = value, color = category)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("net_pensions" = "#0073C2FF",
                                    "net_sick" = "red",
                                    "net_social_assistance" = "#1B9E77",
                                    "net_unemployment_benefit" = "#EFC000FF",
                                    "net_wage" = "#00BFFF"),
                         labels = c("net_pensions" = "Pensions",
                                    "net_sick" = "Sick benefit",
                                    "net_social_assistance" = "Minimum wage",
                                    "net_unemployment_benefit" = "Unemployment",
                                    "net_wage" = "Earned wage")) +
      labs(x = "Year", y = "Percentage", color = "") +
      scale_x_continuous(breaks = seq(2008, 2017, 1)) +
      ggtitle("title") +
      theme_bw() +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(color = "gray"),
            panel.grid.minor.y = element_line(color = "gray"),
            axis.text = element_text(size = 24),
            plot.title = element_text(hjust = 0.5, size = 24),
            axis.title = element_text(size = 24),
            legend.title = element_text(size = 24),
            legend.text = element_text(size = 24),
            legend.position = "bottom")
  }

  else {

    png('png', width = 1000, height = 800)
    ggplot(data, aes(x = as.numeric(year), y = value, color = category)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("net_pensions" = "#0073C2FF",
                                    "net_sick" = "red",
                                    "net_social_assistance" = "#1B9E77",
                                    "net_unemployment_benefit" = "#EFC000FF",
                                    "net_wage" = "#00BFFF"),
                         labels = c("net_pensions" = "Pensions",
                                    "net_sick" = "Sick benefit",
                                    "net_social_assistance" = "Minimum wage",
                                    "net_unemployment_benefit" = "Unemployment",
                                    "net_wage" = "Earned wage")) +
      labs(x = "Year", y = "Percentage", color = "") +
      scale_x_continuous(breaks = seq(2008, 2017, 1)) +
      ggtitle("title") +
      theme_bw() +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(color = "gray"),
            panel.grid.minor.y = element_line(color = "gray"),
            axis.text = element_text(size = 24),
            plot.title = element_text(hjust = 0.5, size = 24),
            axis.title = element_text(size = 24),
            legend.title = element_text(size = 24),
            legend.text = element_text(size = 24),
            legend.position = "bottom")
    dev.off()
  }

}





