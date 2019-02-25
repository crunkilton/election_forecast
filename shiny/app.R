library(shiny)
library(usmap)
library(tidyverse)
library(rsconnect)
library(DT)
library(directlabels)
library(civis)

### GETTING DATA
# setwd("C:/Users/user/Dropbox (PUSA)/priorities/src/midterms/forecasting/shiny")

o <- read.csv("simulations.csv")
#o <- read_civis("cody.forecast_10k_sims_1102")


p <- read.csv("predictions.csv") # pred 11-2
# p <- read_civis("cody.senate_predictions")

dat <- read.csv('rawpolldata.csv')
# dat <- read_civis("cody.polling_cleaned_1102")

state_plot <- function(prediction_data, raw_data, state_code) {
  
  state_plot_polls <- raw_data %>% filter(state == state_code & week_adj > 24)
  state_plot_mod <- prediction_data %>% filter(state == state_code & week_adj > 24)
  
  ggplot(data=state_plot_mod, aes(x=week, y=est)) + 
    geom_point(data=state_plot_polls, aes(x=week, y=twoway, size=sqrt(n_size), color = univ), alpha=0.4) + 
    geom_ribbon(aes(ymin=est-2*sd,ymax=est+2*sd), alpha = 0.5) +
    geom_line(color = "blue",size = 0.75) +
    theme_bw() +
    scale_x_reverse(name = "Weeks Before Election", limits= c(20, 0)) +
    coord_cartesian(ylim = c(min(0.46, min(state_plot_polls$twoway) - 0.01), max(.54, max(state_plot_polls$twoway) + 0.01))) + 
    scale_y_continuous(labels = scales::percent) +
    guides(size=F) + 
    geom_hline( aes(yintercept = 0.5), linetype="dashed") +
    labs(title = paste(state_plot_mod$state %>% unique %>% as.character, "Two-way Forecasted Vote Share"), y = "Democratic Two-way Vote Share",
         caption = "Note that this does not account for systematic polling error, which our simulations do account for.",
         color = "Pollster Universe")
}


# ui ----------------------------------------------------------------------

#ui <- pageWithSidebar(
ui <- fluidPage(
  # Application title
  headerPanel("Who Will Win the Senate?"),
  
  tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Outcome Summary", plotOutput("summary_plot")),
      tabPanel("Simulate the Election", actionButton("do", "Run a Simulation!"), plotOutput("distPlot")), 
      tabPanel("Beto!", actionButton("do_beto", "Beto!!!!"), plotOutput("beto_plot")), 
      tabPanel("Halloween Special", actionButton("do_gop", "Bring on the pain!"), plotOutput("gop_plot")), 
      tabPanel("State-by-State Predictions", dataTableOutput("state_table")),
      tabPanel("State-by-State Predictions--Chart", plotOutput("win_plot")),
      tabPanel("Change over Time", 
               textInput("state", label = h3("Choose State"), value = "FL"), 
               helpText("Input the two-letter state code. Use MN-Smith, MN-Klobuchar, MS-Wicker, or MS-Hyde-Smith for states with multiple races."),
           #    actionButton("Update Plot"), 
               plotOutput("time_plot"))
      
      
    )
  )
  )




server <- function(input, output) {
  
  ## overall averages:
  
  avg_results <- o %>% 
    group_by(simulation) %>% 
    summarise(seats = sum(win)+25) %>% 
    summarise(mean_seats = mean(seats) %>% round(1),
              sd_seats = sd(seats) %>% round(1))
  
  beto_wins <- o %>% 
    filter(state == "TX" & 
             win == 1)  
  
  beto_only <- o %>% 
    filter(simulation %in% beto_wins$simulation) %>% 
    select(simulation) %>% unique
  
  gop_polling_error <- o %>% 
    group_by(simulation) %>% 
    summarise(avg_error = mean(polling_error)) %>% 
    filter(avg_error < -.02)
  
  gop_error_only <- o %>% 
    filter(simulation %in% gop_polling_error$simulation) %>% 
    select(simulation) %>% unique
  
  # iteration ---------------------------------------------------------------
  
  
  iter_val <- eventReactive(input$do, {
    runif(1, 0, nrow(o %>% select(simulation) %>% unique)) %>% ceiling
  }
  )
  
  
  iter_val_beto <- eventReactive(input$do_beto, {
    sample(beto_only$simulation, 1)
  }
  )
  
  iter_val_gop <- eventReactive(input$do_gop, {
    sample(gop_error_only$simulation, 1)
  }
  )
  
  # plot 1 ------------------------------------------------------------------
  
  
  observeEvent(input$dimension,{
    output$distPlot <- renderPlot({
      
      validate(
        need(iter_val() != "", "Please select a data set")
      )
      
      ## filter
      
      once <- o %>%
        filter(simulation ==iter_val())
      
      ## how many seats do we have?
      
      total_seats <- once %>%
        summarise(dem_seats = 25 + sum(win),
                  net_gain = dem_seats - 49) %>% 
        mutate(message = case_when(net_gain < -8 ~ "Wow. Great work. You really fucked this up, didn't you?",
                                   net_gain >= -8 & net_gain <= -4 ~ "The model - and your colleagues - are disappointed in you. Do better.",
                                   net_gain >= 2 & net_gain < 5 ~ "You took back the senate! Congratulations.",
                                   net_gain >= 5 ~ "A blue wave of tidal proportions! You should basically take Patrick's job now.",
                                   TRUE ~ ""))
      
      
      ## what states do we pick up and lose?
      
      switches <- once %>%
        mutate(pickup = case_when(win == 1 & dem_incumbent == 0 ~ 1,
                                  TRUE ~ 0),
               loss = case_when(win == 0 & dem_incumbent == 1 ~ 1,
                                TRUE ~ 0)) %>%
        filter(pickup == 1 |
                 loss == 1) %>%
        mutate(pct_of_vote = paste0(state, " (", final_twoway %>% round(4) * 100, "%)")) %>%
        select(state, pct_of_vote, pickup, loss)
      
      pickups <- paste(switches$pct_of_vote[switches$pickup == 1], collapse = ", ")
      pickups <- ifelse(pickups == "", "nothing :(", pickups)
      
      losses <- paste(switches$pct_of_vote[switches$loss == 1], collapse = ", ")
      losses <- ifelse(losses == "", "nothing!", losses)
      
      gains <- paste("We picked up", pickups)
      
      lose <- paste("We lost", losses)
      
      ## a plot of the USA
      
      once_nodupes <- once %>%
        mutate(state = fct_recode(state, "MN" = "MN-Klobuchar", "MN" = "MN-Smith", "MS" = "MS-Hyde-Smith", "MS" = "MS-Wicker"),
               change = case_when((win == 1 & dem_incumbent == 0) | (win == 0 & dem_incumbent == 1) ~ "Flip",
                                  TRUE ~ "Hold")
        ) %>%
        group_by(state, change) %>%
        summarise(outcome = case_when(mean(win) == 1 ~ "Dem",
                                      mean(win) == 0 ~ "GOP",
                                      TRUE ~ "split") %>% as.factor(),
                  final_twoway = mean(final_twoway)) %>%
        mutate(result = paste(outcome, change, sep = "-"),
               result_color = case_when(result == "GOP-Hold" ~ "lightcoral",
                                        result == "Dem-Hold" ~ "lightblue",
                                        result == "Dem-Flip" ~ "blue",
                                        result == "GOP-Flip" ~ "red",
                                        TRUE ~ "purple"))
      
      data <- statepop %>%
        rename(state = abbr) %>%
        left_join(once_nodupes, by = "state")
      # mutate(outcome = ifelse(outcome %>% is.na() == T, "No Election", outcome %>% as.character))
      
      senate_map <- plot_usmap(data = data, values = "result") +
        scale_fill_manual(values = c("red", "blue", "grey")) +
        labs(title = sprintf("%s (D) - %s (R)", total_seats$dem_seats, 100 - total_seats$dem_seats),
             #   subtitle = sprintf("We expect dems to control %s seats on average, and there is a 2 in 3 chance the total seats are between %s and %s", avg_results$mean_seats %>% round(1), avg_results$mean_seats-avg_results$sd_seats, avg_results$mean_seats+avg_results$sd_seats),
             #  subtitle = sprintf("Between %s and %s seats are expected, with %s average", avg_results$mean_seats-avg_results$sd_seats, avg_results$mean_seats+avg_results$sd_seats, avg_results$mean_seats),
             subtitle = sprintf("%s seats are expected, but %s-%s would be reasonable", avg_results$mean_seats, (avg_results$mean_seats-avg_results$sd_seats) %>% round, (avg_results$mean_seats+avg_results$sd_seats) %>% round),
             caption = total_seats$message) +
        #  scale_fill_manual(values = color_scale) +
        scale_fill_manual(values = c("GOP-Flip" = "red", "GOP-Hold" = "lightcoral", "Dem-Flip" = "blue", "Dem-Hold" = "lightblue"), name = "Outcome")+
        theme(plot.title = element_text(size = 22, face = "bold", hjust = .5),
              plot.subtitle = element_text(size = 12, face = "italic", hjust = .5),
              plot.caption = element_text(size = 10))
      
      
      senate_map
      
    }, width = 800, height = 800, res = 72)
    
    # Outcome Summary ------------------------------------------------------------
    
    outcomes <- o %>% 
      group_by(simulation) %>% 
      summarise(seats = sum(win)+25) %>% 
      group_by(seats) %>% 
      summarise(count = n()) %>% 
      mutate(percent = count/sum(count)*100, 
             control = case_when(seats > 50 ~ "Democrats", 
                                 TRUE ~ "Republicans"))
    
    pct_win <- outcomes %>% 
      filter(control == "Democrats") %>%  
      summarise(win_pct = sum(percent))
    
    output$summary_plot <- renderPlot({
     outcomes %>% 
        ggplot(aes(x= seats, y = percent, fill = control)) + 
        scale_fill_manual(values = c("lightblue", "lightcoral")) +
        geom_col() +
        theme_bw() +
        labs(title = "Model Predictions",
             subtitle = paste0("We estimate Democrats have a ", pct_win$win_pct %>% round(1), "% chance of taking the senate"),
             x = "Democratic Seats",
             y = "Percent Likelihood"
             #  caption = "This is our best guess based on all publicly-available polling",
             
        ) +
        theme(legend.position = "none",
              plot.title = element_text(size = 22, face = "bold", hjust = .5),
              plot.subtitle = element_text(size = 12, face = "italic", hjust = .5),
              plot.caption = element_text(size = 10)
              )+
        scale_x_continuous(limits = c(37, 56))
      
    }, width = 800, height = 800, res = 72)
    
    # change over time --------------------------------------------------------
    
    
    
    output$time_plot <- renderPlot({
      state_plot(p, dat, input$state )
      
    }, width = 800, height = 800, res = 72)    
    
    # win plot ------------------------------------------------------------
    
    output$win_plot <- renderPlot({
      
      myplot <- o %>% 
        group_by(state, dem_incumbent) %>% 
        summarise(mean_twoway = mean(final_twoway),
                  sd_twoway = sd(final_twoway),
                  win_pct = mean(win) * 100) %>% 
        ungroup() %>% 
        mutate(state = factor(state, levels = state[order(win_pct)]),
               dem_incumbent = factor(dem_incumbent)) %>% 
        ggplot(aes(x = state, y = win_pct/100, color = dem_incumbent)) +
        scale_color_manual(values = c("red", "blue", "green")) + 
        #geom_point() + 
        geom_dl(aes(label = state), method = list(dl.combine( "last.bumpup"), cex = 1.2)) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())+
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        labs(title = "Win Probability by State",
             x = "",
             y = "Win Percent") +
        theme(plot.title = element_text(size = 22, face = "bold", hjust = .5),
              plot.subtitle = element_text(size = 12, face = "italic", hjust = .5),
              plot.caption = element_text(size = 10))
      
      myplot
      
      
    }, width = 800, height = 600, res = 72)
    
    # beto plot ---------------------------------------------------------------
    
    
    output$beto_plot <- renderPlot({
      validate(
        need(iter_val_beto() != "", "Please select a data set")
      )
      
      once <- o %>%
        filter(simulation ==iter_val_beto())
      
      ## how many seats do we have?
      
      total_seats <- once %>%
        summarise(dem_seats = 25 + sum(win),
                  net_gain = dem_seats - 49) %>% 
        mutate(message = case_when(net_gain < -3 ~ "Wow. Beto wins and THIS is the best you can do?",
                                   net_gain >= -3 & net_gain <= -1 ~ "Given the cushion you have you should be disappointed in yourself.",
                                   net_gain >= 2 & net_gain < 6 ~ "You took back the senate! Congratulations.",
                                   net_gain >= 6 ~ "Beto brings us to the promised land",
                                   TRUE ~ ""))
      
      
      ## what states do we pick up and lose?
      
      switches <- once %>%
        mutate(pickup = case_when(win == 1 & dem_incumbent == 0 ~ 1,
                                  TRUE ~ 0),
               loss = case_when(win == 0 & dem_incumbent == 1 ~ 1,
                                TRUE ~ 0)) %>%
        filter(pickup == 1 |
                 loss == 1) %>%
        mutate(pct_of_vote = paste0(state, " (", final_twoway %>% round(4) * 100, "%)")) %>%
        select(state, pct_of_vote, pickup, loss)
      
      pickups <- paste(switches$pct_of_vote[switches$pickup == 1], collapse = ", ")
      pickups <- ifelse(pickups == "", "nothing :(", pickups)
      
      losses <- paste(switches$pct_of_vote[switches$loss == 1], collapse = ", ")
      losses <- ifelse(losses == "", "nothing!", losses)
      
      gains <- paste("We picked up", pickups)
      
      lose <- paste("We lost", losses)
      
      ## a plot of the USA
      
      once_nodupes <- once %>%
        mutate(state = fct_recode(state, "MN" = "MN-Klobuchar", "MN" = "MN-Smith", "MS" = "MS-Hyde-Smith", "MS" = "MS-Wicker"),
               change = case_when((win == 1 & dem_incumbent == 0) | (win == 0 & dem_incumbent == 1) ~ "Flip",
                                  TRUE ~ "Hold")
        ) %>%
        group_by(state, change) %>%
        summarise(outcome = case_when(mean(win) == 1 ~ "Dem",
                                      mean(win) == 0 ~ "GOP",
                                      TRUE ~ "split") %>% as.factor(),
                  final_twoway = mean(final_twoway)) %>%
        mutate(result = paste(outcome, change, sep = "-"),
               result_color = case_when(result == "GOP-Hold" ~ "lightcoral",
                                        result == "Dem-Hold" ~ "lightblue",
                                        result == "Dem-Flip" ~ "blue",
                                        result == "GOP-Flip" ~ "red",
                                        TRUE ~ "purple"))
      
      data <- statepop %>%
        rename(state = abbr) %>%
        left_join(once_nodupes, by = "state")
      # mutate(outcome = ifelse(outcome %>% is.na() == T, "No Election", outcome %>% as.character))
      
      senate_map <- plot_usmap(data = data, values = "result") +
        scale_fill_manual(values = c("red", "blue", "grey")) +
        labs(title = sprintf("%s (D) - %s (R)", total_seats$dem_seats, 100 - total_seats$dem_seats),
             subtitle = sprintf("Given that Beto wins - what could the rest of the map look like?"),
             caption = total_seats$message) +
        #  scale_fill_manual(values = color_scale) +
        scale_fill_manual(values = c("GOP-Flip" = "red", "GOP-Hold" = "lightcoral", "Dem-Flip" = "blue", "Dem-Hold" = "lightblue"), name = "Outcome")+
        theme(plot.title = element_text(size = 22, face = "bold", hjust = .5),
              plot.subtitle = element_text(size = 12, face = "italic", hjust = .5),
              plot.caption = element_text(size = 10))
      
      
      senate_map
      
    }, width = 800, height = 800, res = 72)    
    
    # state tables ------------------------------------------------------------
    
    output$state_table <- DT::renderDataTable({
      
      tips <- o %>% 
        group_by(simulation) %>% 
        arrange(final_twoway %>% desc) %>% 
        slice(26) %>% 
        ungroup %>% 
        group_by(state) %>% 
        summarise(count = n(),
                  mean_twoway = mean(final_twoway),
                  mean_winpct = mean(win)) %>% 
        arrange(count %>% desc) %>% 
        mutate(pct_tip = count/sum(count),
               pct_tip_str = paste0(pct_tip, "%")) %>% 
        select(state, pct_tip)
      
      table2 <- o %>% 
        group_by(state) %>% 
        summarise(twoway = mean(final_twoway),
                  sd = sd(final_twoway),
                  win_pct = mean(win)) %>% 
        mutate(lower = twoway - sd,
               upper = twoway + sd,
               range = paste0(lower %>% round(1), "% - ", upper %>% round(1), "%"),
               #   win_pct = paste0(win_pct*100, "%"),
               twoway_str = paste0(twoway %>% round(1), "%")) %>% 
        arrange(abs(twoway - .50)) %>% 
        select(state, twoway, win_pct) %>% 
        left_join(tips, by = "state") %>% 
        rename(state = state, 
               `Two-Way Prediction` = twoway, 
               `Win Percent` = win_pct,
               `Odds of Determining Senate Control` = pct_tip)
      
      
      datatable(table2, options= list(pageLength = 50), rownames = FALSE) %>% 
        formatPercentage(c('Two-Way Prediction', 
                           'Win Percent',
                           'Odds of Determining Senate Control'), 1) 
      
      
      
      
      
    })
    
    # GOP plot ----------------------------------------------------------------
    
    output$gop_plot <- renderPlot({
      
      validate(
        need(iter_val_gop() != "", "Please select a data set")
      )
      
      once <- o %>%
        filter(simulation ==iter_val_gop())
      
      ## how many seats do we have?
      
      total_seats <- once %>%
        summarise(dem_seats = 25 + sum(win),
                  net_gain = dem_seats - 49) %>% 
        mutate(message = case_when(net_gain < -9 ~ "This looks awful. What did you do?",
                                   net_gain >= -9 & net_gain <= -6 ~ "How disappointing",
                                   net_gain >= -2 & net_gain < 2 ~ "Pretty good, considering",
                                   net_gain >= 2 ~ "Wow! You still took back the senate",
                                   TRUE ~ ""))
      
      
      ## what states do we pick up and lose?
      
      switches <- once %>%
        mutate(pickup = case_when(win == 1 & dem_incumbent == 0 ~ 1,
                                  TRUE ~ 0),
               loss = case_when(win == 0 & dem_incumbent == 1 ~ 1,
                                TRUE ~ 0)) %>%
        filter(pickup == 1 |
                 loss == 1) %>%
        mutate(pct_of_vote = paste0(state, " (", final_twoway %>% round(4) * 100, "%)")) %>%
        select(state, pct_of_vote, pickup, loss)
      
      pickups <- paste(switches$pct_of_vote[switches$pickup == 1], collapse = ", ")
      pickups <- ifelse(pickups == "", "nothing :(", pickups)
      
      losses <- paste(switches$pct_of_vote[switches$loss == 1], collapse = ", ")
      losses <- ifelse(losses == "", "nothing!", losses)
      
      gains <- paste("We picked up", pickups)
      
      lose <- paste("We lost", losses)
      
      ## a plot of the USA
      
      once_nodupes <- once %>%
        mutate(state = fct_recode(state, "MN" = "MN-Klobuchar", "MN" = "MN-Smith", "MS" = "MS-Hyde-Smith", "MS" = "MS-Wicker"),
               change = case_when((win == 1 & dem_incumbent == 0) | (win == 0 & dem_incumbent == 1) ~ "Flip",
                                  TRUE ~ "Hold")
        ) %>%
        group_by(state, change) %>%
        summarise(outcome = case_when(mean(win) == 1 ~ "Dem",
                                      mean(win) == 0 ~ "GOP",
                                      TRUE ~ "split") %>% as.factor(),
                  final_twoway = mean(final_twoway)) %>%
        mutate(result = paste(outcome, change, sep = "-"),
               result_color = case_when(result == "GOP-Hold" ~ "lightcoral",
                                        result == "Dem-Hold" ~ "lightblue",
                                        result == "Dem-Flip" ~ "blue",
                                        result == "GOP-Flip" ~ "red",
                                        TRUE ~ "purple"))
      
      data <- statepop %>%
        rename(state = abbr) %>%
        left_join(once_nodupes, by = "state")
      # mutate(outcome = ifelse(outcome %>% is.na() == T, "No Election", outcome %>% as.character))
      
      senate_map <- plot_usmap(data = data, values = "result") +
        scale_fill_manual(values = c("red", "blue", "grey")) +
        labs(title = sprintf("%s (D) - %s (R)", total_seats$dem_seats, 100 - total_seats$dem_seats),
             subtitle = sprintf("Given a 2+ point polling error in favor of the GOP, what might the senate look like?"),
             caption = total_seats$message) +
        #  scale_fill_manual(values = color_scale) +
        scale_fill_manual(values = c("GOP-Flip" = "red", "GOP-Hold" = "lightcoral", "Dem-Flip" = "blue", "Dem-Hold" = "lightblue"), name = "Outcome")+
        theme(plot.title = element_text(size = 22, face = "bold", hjust = .5),
              plot.subtitle = element_text(size = 12, face = "italic", hjust = .5),
              plot.caption = element_text(size = 10))
      
      
      senate_map
      
    }, width = 800, height = 800, res = 72)    
    
  })
  
  
  
  
  
}

shinyApp(server = server, ui = ui)

# deployApp('C:/Users/user/Desktop/shiny')
# set wd to desktop and copy/paste this there. 
