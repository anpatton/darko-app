############################
########## DARKO ###########
# Model: Kostya Medvedovsky
# Application: Andrew Patton
############################

#### Packages for App ####
library(knitr)
library(kableExtra)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(colorRamps)
library(grDevices)
library(tidyverse)
library(colorspace)
library(ggrepel)
library(lubridate)
library(DescTools)
library(kableExtra)
library(markdown)
library(htmlwidgets)
library(gt)
library(gtExtras)

## No clue how this works
drawCallback <- htmlwidgets::JS(
  "function (oSettings, json) {
        $('.dt-center').each(function(i) {
          var color = $(this).css('background-color');
          $(this).attr('style', 'background-color: '+color+' !important');
   })}"
)

source("utils.R")
source("load_data.R")

#### SETUP APPLICATION ####
ui <- fluidPage(
    tags$script(HTML('window.location.replace("https://www.darko.app");')),
  # tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {font-size:125% !important; background-color: unset !important;}')),
  navbarPage(
    "DARKO Exploration",
    tabPanel(
      "What is DARKO?",
      mainPanel(
        h2("Daily Adjusted and Regressed Kalman Optimized projections | DARKO"),
        img(src = "darko_clipart_logo.png", align = "right", width = 120, height = 100),
        includeMarkdown("explainer_new.md")
      )
    ),
    tabPanel(
      "Current Player Skill Projections",
      h4(paste0("Version: ", version_date[[1]]),
        style = "color: #1f2024"
      ),
      h4(a("$ Support DARKO $",
        style = "color: #0CCE6B",
        href = "https://www.buymeacoffee.com/darko",
        target = "_blank"
      )),
      h5(a("DARKO by: @kmedved",
        style = "color: #1f2024",
        href = "https://twitter.com/@kmedved",
        target = "_blank"
      )),
      h5(a("Application by @anpatt7",
        style = "color: #1f2024",
        href = "https://twitter.com/@anpatt7",
        target = "_blank"
      )),
      downloadButton("download_talent", "Download Data"),
      br(),
      verticalLayout(
        DT::dataTableOutput("table"),
        tableOutput("subtable"),
        radioButtons(
          inputId = "chart_options",
          label = "Talent Type",
          choices = c(
            "DPM",
            "O-DPM",
            "D-DPM",
            "FG3%",
            "FG3ARate%",
            "FT%",
            "FTARate%",
            "RimFGA/100"
          ),
          inline = TRUE
        ),
        splitLayout(
          plotOutput("trend") %>% withSpinner(color = "#44a8f3"),
          plotOutput("deriv") %>% withSpinner(color = "#44a8f3")
        )
      )
    ),
    tabPanel(
      "Lineup Projections",
      h4(paste0("Version: ", version_date[[1]]),
         style = "color: #1f2024"
      ),
      h4(a("$ Support DARKO $",
           style = "color: #0CCE6B",
           href = "https://www.buymeacoffee.com/darko",
           target = "_blank"
      )),
      h5(a("DARKO by: @kmedved",
           style = "color: #1f2024",
           href = "https://twitter.com/@kmedved",
           target = "_blank"
      )),
      h5(a("Application by @anpatt7",
           style = "color: #1f2024",
           href = "https://twitter.com/@anpatt7",
           target = "_blank"
      )),
      br(),
      h3("            Lineup Plus/Minus in Relation to League Average (+ Good, - Bad)"),
      h3("            Table Limited to Lineups with >100 Possessions"),
      br(),
      DT::dataTableOutput("lineup_table")
    ),
    tabPanel(
      "Player Profile",
      h4(paste0("Version: ", version_date[[1]]),
        style = "color: #1f2024"
      ),
      h4(a("$ Support DARKO $",
        style = "color: #0CCE6B",
        href = "https://www.buymeacoffee.com/darko",
        target = "_blank"
      )),
      h5(a("DARKO by: @kmedved",
        style = "color: #1f2024",
        href = "https://twitter.com/@kmedved",
        target = "_blank"
      )),
      h5(a("Application by @anpatt7",
        style = "color: #1f2024",
        href = "https://twitter.com/@anpatt7",
        target = "_blank"
      )),
      tags$head(
        tags$style(
          HTML(".shiny-split-layout>div {overflow: hidden;}"),
          ".tab-content {overflow: visible;}"
        )
      ),
      verticalLayout(
        inputPanel(
          selectInput("player_profile", "Player",
            choices = sort(unique(current_talent$codename)),
            selected = random_active_player,
            multiple = FALSE,
            selectize = TRUE
          ),
          selectizeInput("player_profile_trend", "Talent Trend",
            choices = all_talent_cols,
            selected = c("DPM"),
            multiple = FALSE
          ),
          selectizeInput("player_profile_comps", "Talent Percentiles",
            choices = all_talent_cols,
            selected = c("DPM", "O-DPM", "D-DPM", "FG3%", "FTARate%"),
            multiple = TRUE,
            options = list(maxItems = 5)
          ),
        ),
        shiny::verticalLayout(
          shiny::splitLayout(
            plotOutput("headshot"),
            shiny::verticalLayout(
              br(),
              br(),
              gt_output("table1"),
              br(),
              gt_output("table2"),
              br(),
              gt_output("table3"),
              br()
            ),
            plotOutput("percentile_plot")
          ),
          verticalLayout(
            splitLayout(
              plotOutput("trend_plot"),
              plotOutput("talent_dist")
            ),
            br(),
            splitLayout(
              gt_output("dpm_table") # ,
              # gt_output("dpm_table_projection")
            ),
            br(),
            splitLayout(
              gt_output("efficiency_table"),
              gt_output("style_table"),
              gt_output("box_table")
            ),
            br(),
            gt_output("per_100_table"),
            br()
          )
        )
      )
    ),
    tabPanel(
      "Daily Player Per-Game Projections",
      h4(paste0("Version: ", version_date[[1]]),
        style = "color: #1f2024"
      ),
      h4(a("$ Support DARKO $",
        style = "color: #0CCE6B",
        href = "https://www.buymeacoffee.com/darko",
        target = "_blank"
      )),
      h5(a("DARKO by: @kmedved",
        style = "color: #1f2024",
        href = "https://twitter.com/@kmedved",
        target = "_blank"
      )),
      h5(a("Application by @anpatt7",
        style = "color: #1f2024",
        href = "https://twitter.com/@anpatt7",
        target = "_blank"
      )),
      downloadButton("download_daily", "Download Data"),
      br(),
      verticalLayout(
        DT::dataTableOutput("table_daily")
      )
    ),
    tabPanel(
      "Historical Career Trajectory",
      h4(paste0("Version: ", version_date[[1]]),
        style = "color: #1f2024"
      ),
      h4(a("$ Support DARKO $",
        style = "color: #0CCE6B",
        href = "https://www.buymeacoffee.com/darko",
        target = "_blank"
      )),
      h5(a("DARKO by: @kmedved",
        style = "color: #1f2024",
        href = "https://twitter.com/@kmedved",
        target = "_blank"
      )),
      h5(a("Application by @anpatt7",
        style = "color: #1f2024",
        href = "https://twitter.com/@anpatt7",
        target = "_blank"
      )),
      h4("Retired player charts may look abnormal.",
        style = "color: #1f2024"
      ),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "time_type",
            label = "Time Scale",
            choices = c(
              "Games" = "game_num",
              "Age" = "age",
              "Seasons" = "total_game_idx"
            )
          ),
          selectizeInput(
            inputId = "comp_options",
            label = "Talent Type",
            choices = talent_label_link,
            selected = talent_label_link[11]
          ),
          selectizeInput(
            inputId = "comps",
            label = "Select Players to Compare (Max 5)",
            choices = sort(unique(historical_talent$codename)),
            selected = random_active_player,
            multiple = TRUE,
            options = list(
              maxOptions = length(unique(historical_talent$codename)),
              maxItems = 5
            )
          )
        ),
        mainPanel(
          plotOutput("comp") %>% withSpinner(color = "#44a8f3")
        )
      )
    ),
    tabPanel(
      "Current Season Snapshot",
      h4(paste0("Version: ", version_date[[1]]),
        style = "color: #1f2024"
      ),
      h4(a("$ Support DARKO $",
        style = "color: #0CCE6B",
        href = "https://www.buymeacoffee.com/darko",
        target = "_blank"
      )),
      h5(a("DARKO by: @kmedved",
        style = "color: #1f2024",
        href = "https://twitter.com/@kmedved",
        target = "_blank"
      )),
      h5(a("Application by @anpatt7",
        style = "color: #1f2024",
        href = "https://twitter.com/@anpatt7",
        target = "_blank"
      )),
      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            inputId = "snap_options",
            label = "Talent Type",
            choices = c(
              "DPM" = "dpm",
              "O-DPM" = "o_dpm",
              "D-DPM" = "d_dpm",
              "Box DPM" = "box_dpm",
              "Box O-DPM" = "box_odpm",
              "Box D-DPM" = "box_ddpm",
              "FG3%" = "fg3_pct",
              "FT%" = "ft_pct"
            )
          ),
          selectizeInput(
            inputId = "snaps",
            label = "Select Players to Compare (Max 5)",
            choices = sort(unique(current_talent_snapshot$codename)),
            selected = random_active_player,
            multiple = TRUE,
            options = list(maxItems = 5)
          ),
        ),
        mainPanel(
          plotOutput("snap") %>% withSpinner(color = "#44a8f3")
        )
      )
    ),
    tabPanel(
      "Longevity Projections (BETA)",
      h4(paste0("Version: ", version_date[[1]]),
        style = "color: #991D37"
      ),
      h4(a("$ Support DARKO $",
        style = "color: #0CCE6B",
        href = "https://www.buymeacoffee.com/darko",
        target = "_blank"
      )),
      h5(a("DARKO by: @kmedved",
        style = "color: #1f2024",
        href = "https://twitter.com/@kmedved",
        target = "_blank"
      )),
      h5(a("Application by @anpatt7",
        style = "color: #1f2024",
        href = "https://twitter.com/@anpatt7",
        target = "_blank"
      )),
      verticalLayout(
        splitLayout(
          plotOutput("surv_plot_roster") %>% withSpinner(color = "#44a8f3"),
          plotOutput("surv_plot_years") %>% withSpinner(color = "#44a8f3")
        ),
        DT::dataTableOutput("surv_table")
      )
    ),
    tabPanel(
      "Scatterplots",
      h4(paste0("Version: ", version_date[[1]]),
        style = "color: #991D37"
      ),
      h4(a("$ Support DARKO $",
        style = "color: #0CCE6B",
        href = "https://www.buymeacoffee.com/darko",
        target = "_blank"
      )),
      h5(a("DARKO by: @kmedved",
        style = "color: #1f2024",
        href = "https://twitter.com/@kmedved",
        target = "_blank"
      )),
      h5(a("Application by @anpatt7",
        style = "color: #1f2024",
        href = "https://twitter.com/@anpatt7",
        target = "_blank"
      )),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "statX",
            label = "X-Axis Talent Type",
            choices = talent_label_link,
            selected = "DPM"
          ),
          selectInput(
            inputId = "statY",
            label = "Y-Axis Talent Type",
            choices = talent_label_link,
            selected = "FG3%"
          ),
          selectInput(
            inputId = "scatterPlayer",
            label = "Player",
            choices = c("None", unique(scatter_data$player_name)),
            selected = "None"
          ),
          radioButtons(
            inputId = "scatterOptions",
            label = "Chart Options",
            choices = c("Trendline", "Quadrants")
          ),
          sliderInput(
            inputId = "sliderBPM",
            label = "Filter by DPM",
            min = min(DescTools::RoundTo(scatter_data$dpm, 1, floor)),
            max = max(DescTools::RoundTo(scatter_data$dpm, 1, ceiling)),
            value = c(
              min(DescTools::RoundTo(scatter_data$dpm, 1, floor)),
              max(DescTools::RoundTo(scatter_data$dpm, 1, ceiling))
            ),
            step = 0.5
          ),
          sliderInput(
            inputId = "sliderMin",
            label = "Filter by Minutes Per Game",
            min = min(DescTools::RoundTo(na.omit(scatter_data$minutes), 2, floor)),
            max = max(DescTools::RoundTo(na.omit(scatter_data$minutes), 2, ceiling)),
            value = c(
              min(DescTools::RoundTo(na.omit(scatter_data$minutes), 2, floor)),
              max(DescTools::RoundTo(na.omit(scatter_data$minutes), 2, ceiling))
            ),
            step = 2
          ),
          radioButtons(
            inputId = "scatterLabels",
            label = "Add Player Labels",
            choices = c("No", "Yes")
          )
        ),
        mainPanel(
          plotOutput("scatter") %>% withSpinner(color = "#44a8f3")
        )
      )
    )#,
    # tabPanel("Win and Playoff Projections",
    #          h4(paste0("Version: ", version_date[[1]]),
    #             style = "color: #991D37"
    #          ),
    #          h4(a("$ Support DARKO $",
    #               style = "color: #0CCE6B",
    #               href = "https://www.buymeacoffee.com/darko",
    #               target = "_blank"
    #          )),
    #          h5(a("DARKO by: @kmedved",
    #               style = "color: #1f2024",
    #               href = "https://twitter.com/@kmedved",
    #               target = "_blank"
    #          )),
    #          h5(a("Application by @anpatt7",
    #               style = "color: #1f2024",
    #               href = "https://twitter.com/@anpatt7",
    #               target = "_blank"
    #          )),
    #          h4("These are baseline projections based exclusively on DPM and minutes. No other adjustments.",
    #             style = "color: #1f2024"
    #          ),
    #          br(),
    #          br(),
    #          #plotOutput("win_projections")
    #          splitLayout(
    #            gt_output("west"),
    #            gt_output("east"), 
    #            cellWidths = 700
    #          )
    #          
    # )
  )
)


#### RENDER APPLICATION ####
server <- function(input, output, session) {

  #### App Wide Utility ####

  observeEvent(input$ref_player, {
    updateSliderInput(session, "game_slider",
      max = nrow(current_talent[current_talent$codename == input$ref_player, ]),
      value = c(1, nrow(current_talent[current_talent$codename == input$ref_player, ]))
    )
  })

  #### Talent Tables ####
  output$download_talent <- downloadHandler(
    filename <- function() {
      paste0("DARKO_player_talent_", version_date[[1]], ".csv")
    },
    content <- function(file) {
      write.csv(current_talent_clean, file, row.names = FALSE)
    }
  )

  output$table <- renderDataTable({
    datatable(select(current_talent_clean, -nba_id) %>%
      mutate(across(!tidyselect::ends_with("%") & where(is.numeric), function(x) round(x, 1))),
    filter = "top",
    rownames = FALSE,
    selection = list(mode = "single", target = "row", selected = c(1)),
    options = list(
      drawCallback = drawCallback,
      pageLength = 10,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    )
    ) %>%
      formatPercentage(c(
        "FG2%", "FG3%", "FG3ARate%", "FT%", "FTARate%", "USG%",
        "AST%", "BLK%", "STL%", "RimFG%"
      ), 1) %>%
      formatStyle("DPM",
        backgroundColor = styleInterval(brks_dpm, clrs_dpm),
        color = "black"
      ) %>%
      formatStyle("DPM Improvement",
        backgroundColor = styleInterval(brks_delta_dpm, clrs_delta_dpm),
        color = "black"
      ) %>%
      formatStyle("Box DPM",
        backgroundColor = styleInterval(brks_bdpm, clrs_bdpm),
        color = "black"
      )
  })

  selected_player_data <- eventReactive(input$table_rows_selected, {
    index <- input$table_rows_selected

    selected_player_data <- current_talent_clean[index, ]

    return(selected_player_data)
  })

  subcurrent_talent <- eventReactive(input$table_rows_selected, {
    if (is.null(input$table_rows_selected) == FALSE) {
      selectedPlayer <- current_talent_clean[input$table_rows_selected, ] %>%
        select(nba_id) %>%
        unique() %>%
        pull()

      subData <- current_talent_clean %>%
        mutate(nba_id = as.character(nba_id)) %>%
        mutate_if(is.numeric, function(x) rank(-1 * x)) %>%
        filter(nba_id == selectedPlayer)

      return(subData)
    } else {
      shiny::showNotification("Please select player row to show subtable", type = "error")

      NULL
    }
  })

  output$subtable <- eventReactive(input$table_rows_selected, {
    name_of_player <- subcurrent_talent()$Player

    select(subcurrent_talent(), -c(Player, Team, nba_id)) %>%
      kable(
        format = "html",
        align = c("c"),
        caption = paste0(name_of_player, " Talent Ranks")
      ) %>%
      kableExtra::kable_styling(position = "center", latex_options = "striped")
  })

  output$trend <- renderPlot(
    {
      shiny::validate(
        need(is.null(input$table_rows_selected) == FALSE, "Please Select a Player")
      )

      trend_data <- historical_talent %>%
        filter(seconds_played > 0) %>%
        filter(nba_id == selected_player_data()$nba_id) %>%
        mutate(game_num = 1:n()) %>% 
        rename_cols()

      season_data <- trend_data %>%
        group_by(season) %>%
        filter(career_game_num == min(career_game_num)) %>%
        select(game_num) %>%
        pull()

      name_of_player <- unique(trend_data$Player)
      
      col_name <- paste0(paste0("`", input$chart_options), "`")
      
      if (input$chart_options %in% c("FG3%", "FT%", "FG3ARate%", "FTARate%")) {
        ggplot() +
          geom_vline(xintercept = season_data, color = "#636166", linetype = 1, lwd = 0.25) +
          scale_x_continuous(
            breaks = c(season_data),
            labels = unique(na.omit(trend_data$season))
          ) +
          scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
          geom_point(
            data = trend_data, aes_string(x = "game_num", y = col_name),
            color = "#006BB6", size = 2, alpha = 0.65
          ) +
          geom_smooth(
            data = trend_data, aes_string(x = "game_num", y = col_name),
            color = NA, fill = "#ED174C",
            lwd = 1, method = "loess"
          ) +
          labs(
            x = "Season Start Points",
            y = paste0("DARKO ", input$chart_options),
            title = paste0(name_of_player),
            subtitle = paste0("Career DARKO ", input$chart_options, " Progression"),
            caption = basic_cite
          ) +
          theme_trend_single_player()
      } else if (input$chart_options %in% c("DPM", "O-DPM", "D-DPM")){
        ggplot() +
          geom_hline(yintercept = 0, color = "#636166", linetype = 1, lwd = 0.5) +
          geom_vline(xintercept = season_data, color = "#636166", linetype = 1, lwd = 0.25) +
          scale_x_continuous(
            breaks = c(season_data),
            labels = unique(unique(na.omit(trend_data$season)))
          ) +
          geom_point(
            data = trend_data, aes_string(x = "game_num", y = col_name),
            color = "#006BB6", size = 2, alpha = 0.65
          ) +
          geom_smooth(
            data = trend_data, aes_string(x = "game_num", y = col_name),
            color = NA, fill = "#ED174C",
            lwd = 1, method = "loess"
          ) +
          labs(
            x = "Season Start Points",
            y = paste0("DARKO ", input$chart_options),
            title = paste0(name_of_player),
            subtitle = paste0("Career DARKO ", input$chart_options, " Progression"),
            caption = basic_cite
          ) +
          theme_trend_single_player()
      } else {
        ggplot() +
          geom_vline(xintercept = season_data, color = "#636166", linetype = 1, lwd = 0.25) +
          scale_x_continuous(
            breaks = c(season_data),
            labels = unique(unique(na.omit(trend_data$season)))
          ) +
          geom_point(
            data = trend_data, aes_string(x = "game_num", y = col_name),
            color = "#006BB6", size = 2, alpha = 0.65
          ) +
          geom_smooth(
            data = trend_data, aes_string(x = "game_num", y = col_name),
            color = NA, fill = "#ED174C",
            lwd = 1, method = "loess"
          ) +
          labs(
            x = "Season Start Points",
            y = paste0("DARKO ", input$chart_options),
            title = paste0(name_of_player),
            subtitle = paste0("Career DARKO ", input$chart_options, " Progression"),
            caption = basic_cite
          ) +
          theme_trend_single_player()
      }
    },
    width = 600,
    height = 300
  )

  output$deriv <- renderPlot(
    {
      shiny::validate(
        need(is.null(input$table_rows_selected) == FALSE, "Please Select a Player")
      )

      #talent_lab <- all_talent_labels %>%
      #  filter(actual == input$chart_options) %>%
      #  select(display) %>%
      #  pull()

      trend_data <- historical_talent %>%
        filter(seconds_played > 0) %>%
        filter(nba_id == selected_player_data()$nba_id) %>%
        mutate(game_num = 1:n()) %>% 
        rename_cols()

      season_data <- trend_data %>%
        group_by(season) %>%
        filter(career_game_num == min(career_game_num)) %>%
        select(game_num) %>%
        pull()

      name_of_player <- unique(trend_data$Player)
      
      model <- loess(eval(sym(input$chart_options)) ~ game_num, data = trend_data)

      X <- data.frame(game_num = seq(1, nrow(trend_data), 0.1))
      Y <- predict(model, newdata = X)
      dY <- diff(Y) / diff(X$game_num)
      dX <- rowMeans(embed(X$game_num, 2))

      dat <- data.frame(
        y = dY,
        x = dX
      )

      ggplot() +
        geom_hline(yintercept = 0, color = "#636166", linetype = 1, lwd = 1) +
        geom_vline(xintercept = season_data, color = "#636166", linetype = 1, lwd = 0.25) +
        scale_x_continuous(
          breaks = c(season_data),
          labels = unique(unique(na.omit(trend_data$season)))
        ) +
        geom_line(
          data = dat, aes(
            x = x,
            y = y
          ),
          color = "#ED7D3A", lwd = 2
        ) +
        labs(
          x = "Season Start Points",
          y = paste0("DARKO ", input$chart_options, " Slope"),
          title = name_of_player,
          subtitle = paste0("Career DARKO ", input$chart_options, " Change"),
          caption = basic_cite
        ) +
        theme_trend_single_player()
    },
    width = 600,
    height = 300
  )


  #### Player Profile
  player_profile_data <- eventReactive(input$player_profile, {
    player_profile_data <- current_talent %>%
      rename_cols() %>%
      rename(`DPM Improvement` = dpm_delta) %>%
      filter(codename == input$player_profile)

    return(player_profile_data)
  })

  output$lineup_table <- DT::renderDataTable({
    
      DT::datatable(five_man,
                    filter = "top",
                    rownames = FALSE,
                    selection = list(mode = "single", target = "row", selected = c(1)),
                    options = list(
                      drawCallback = drawCallback,
                      pageLength = 20,
                      columnDefs = list(list(className = "dt-center", targets = "_all")))) |> 
        formatStyle("Net",
                    backgroundColor = styleInterval(brks_lineup_net, clrs_lineup_net),
                    color = "black"
        ) |> 
        formatStyle("Offense",
                    backgroundColor = styleInterval(brks_lineup_off, clrs_lineup_off),
                    color = "black"
        ) |> 
        formatStyle("Defense",
                    backgroundColor = styleInterval(brks_lineup_def, clrs_lineup_def),
                    color = "black"
        )
  })
  
  output$headshot <- shiny::renderPlot({
    player_name <- player_profile_data()$Player
    player_team <- player_profile_data()$Team
    player_id <- player_profile_data()$nba_id
    # player_age <- round(player_profile_data()$Age, 1)
    # player_games <- player_profile_data

    url <- paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", player_id, ".png")

    image_frame <- data.frame(x = 0, y = 0, image_url = url)

    ggplot() +
      ggimage::geom_image(
        data = image_frame,
        aes(x = x, y = y, image = image_url), size = 1.05
      ) +
      theme_void() +
      labs(
        title = paste0(player_name),
        subtitle = player_team
      ) +
      theme(
        aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
        plot.subtitle = element_text(
          hjust = 0.5, size = 18,
          margin = margin(0, 0, -30, 0)
        )
      )
  })

  output$percentile_plot <- shiny::renderPlot({
    player_name <- player_profile_data()$Player
    player_team <- player_profile_data()$Team
    player_codename <- player_profile_data()$codename
    player_pos <- player_profile_data()$Pos

    position_rank <- current_talent %>%
      rename_cols() %>%
      rename(`DPM Improvement` = dpm_delta) %>%
      filter(Pos == player_pos) %>%
      select(c("codename", input$player_profile_comps)) %>%
      mutate(across(where(is.numeric), ~ cume_dist(.x))) %>%
      filter(codename == player_codename) %>%
      select(-codename) %>%
      pivot_longer(cols = everything()) %>%
      mutate(name = factor(name, levels = input$player_profile_comps))

    ggplot(data = position_rank) +
      geom_col(aes(x = name, y = value, fill = name), color = "darkgrey") +
      scale_fill_brewer(palette = "Set1") +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = seq(0, 1, 0.1), limits = c(0, 1)
      ) +
      guides(fill = "none") +
      labs(
        x = "DARKO Talents", y = "Percentile",
        title = paste0(player_name),
        subtitle = paste0(player_pos, " Only (", today_date, ")"),
        caption = basic_cite
      ) +
      theme_trend_single_player()
  })


  output$trend_plot <- renderPlot({
    shiny::validate(
      need(!is.null(input$player_profile_trend) | !is.null(input$player_profile), "Please Select a Player & Trend")
    )

    player_name <- player_profile_data()$Player
    player_team <- player_profile_data()$Team
    player_codename <- player_profile_data()$codename
    player_id <- player_profile_data()$nba_id
    player_pos <- player_profile_data()$Pos

    trend_data <- historical_talent %>%
      filter(nba_id == player_id) %>%
      filter(seconds_played > 0) %>%
      mutate(game_num = 1:n()) %>%
      group_by(player_name, season) %>%
      mutate(first_game_of_season = ifelse(game_num == min(game_num), 1, 0)) %>%
      ungroup() %>%
      rename_cols()

    season_data <- trend_data %>%
      filter(first_game_of_season == 1) %>%
      select(game_num) %>%
      pull()

    col_name <- input$player_profile_trend
    col_name <- paste0(paste0("`", col_name), "`")

    if (input$player_profile_trend %in%
      c(
        "USG%", "ORB%", "DRB%", "AST%", "BLK%", "STL%", "TOV%",
        "FTARate%", "FG2%", "FG3%", "FT%", "FG3ARate%", "RimFG%"
      )) {
      ggplot() +
        geom_vline(xintercept = season_data, color = "#636166", linetype = 1, lwd = 0.25) +
        scale_x_continuous(
          breaks = c(season_data),
          labels = unique(na.omit(trend_data$season))
        ) +
        scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
        geom_point(
          data = trend_data, aes_string(x = "game_num", y = col_name),
          color = "#006BB6", size = 2, alpha = 0.65
        ) +
        geom_smooth(
          data = trend_data, aes_string(x = "game_num", y = col_name),
          color = NA, fill = "#ED174C",
          lwd = 1, method = "loess"
        ) +
        labs(
          x = "Season Start Points",
          y = paste0("DARKO ", input$player_profile_trend),
          title = paste0(player_name),
          subtitle = paste0("Career DARKO ", input$player_profile_trend, " Progression"),
          caption = basic_cite
        ) +
        theme_trend_single_player()
    } else if (input$player_profile_trend %in%
      c("DPM", "O-DPM", "D-DPM", "Box DPM", "Box O-DPM", "Box D-DPM")) {
      ggplot() +
        geom_hline(yintercept = 0, color = "#636166", linetype = 1, lwd = 0.5) +
        geom_vline(xintercept = season_data, color = "#636166", linetype = 1, lwd = 0.25) +
        scale_x_continuous(
          breaks = c(season_data),
          labels = unique(na.omit(trend_data$season))
        ) +
        geom_point(
          data = trend_data, aes_string(x = "game_num", y = col_name),
          color = "#006BB6", size = 2, alpha = 0.65
        ) +
        geom_smooth(
          data = trend_data, aes_string(x = "game_num", y = col_name),
          color = NA, fill = "#ED174C",
          lwd = 1, method = "loess"
        ) +
        labs(
          x = "Season Start Points",
          y = paste0("DARKO ", input$player_profile_trend),
          title = paste0(player_name),
          subtitle = paste0("Career DARKO ", input$player_profile_trend, " Progression"),
          caption = basic_cite
        ) +
        theme_trend_single_player()
    } else {
      ggplot() +
        geom_vline(xintercept = season_data, color = "#636166", linetype = 1, lwd = 0.25) +
        scale_x_continuous(
          breaks = c(season_data),
          labels = unique(na.omit(trend_data$season))
        ) +
        geom_point(
          data = trend_data, aes_string(x = "game_num", y = col_name),
          color = "#006BB6", size = 2, alpha = 0.65
        ) +
        geom_smooth(
          data = trend_data, aes_string(x = "game_num", y = col_name),
          color = NA, fill = "#ED174C",
          lwd = 1, method = "loess"
        ) +
        labs(
          x = "Season Start Points",
          y = paste0("DARKO ", input$player_profile_trend),
          title = paste0(player_name),
          subtitle = paste0("Career DARKO ", input$player_profile_trend, " Progression"),
          caption = basic_cite
        ) +
        theme_trend_single_player()
    }
  })

  output$talent_dist <- renderPlot({
    
    shiny::validate(
      need(!is.null(input$player_profile_trend) | !is.null(input$player_profile), "Please Select a Player & Trend")
    )
    
    player_name <- player_profile_data()$Player
    player_team <- player_profile_data()$Team
    player_codename <- player_profile_data()$codename
    player_id <- player_profile_data()$nba_id
    player_pos <- player_profile_data()$Pos
    
    col_name <- input$player_profile_trend
    col_name <- paste0(paste0("`", col_name), "`")
    
    plot_data <- current_talent %>% 
      rename_cols() %>% 
      mutate(Pos = factor(Pos, levels = c("PG", "SG", "SF", "PF", "C")))
    
    player_val <- current_talent %>% 
      rename_cols() %>% 
      filter(codename == player_codename) %>% 
      select(input$player_profile_trend) %>% 
      pull()
      
    if(str_detect(col_name, "%")) {
      
      ggplot(data = plot_data) +
        geom_density(aes_string(x = col_name, fill = "Pos", color = "Pos"), alpha = 0.25, lwd = 1.5) +
        scale_x_continuous(labels = scales::percent_format()) +
        geom_vline(xintercept = player_val, lwd = 2, color = "black") +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        scale_y_continuous(labels = NULL) +
        labs(x = paste0("DARKO ", input$player_profile_trend), 
             y = "", 
             title = paste0(player_name),
             subtitle = paste0("Positional Distribution"),
             caption = basic_cite) +
        theme_pos_dist() 
      
    } else {
      
      ggplot(data = plot_data) +
        geom_density(aes_string(x = col_name, fill = "Pos", color = "Pos"), alpha = 0.25, lwd = 1.5) +
        geom_vline(xintercept = player_val, lwd = 2, color = "black") +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        scale_y_continuous(labels = NULL) +
        labs(x = paste0("DARKO ", input$player_profile_trend), 
             y = "", 
             title = paste0(player_name),
             subtitle = "Positional Distribution",
             caption = basic_cite) +
        theme_pos_dist() 
      
    }
    
    
    
  })
  
  output$table1 <- render_gt({
    dpm_current_talent <- player_profile_data() %>%
      select(DPM, `O-DPM`, `D-DPM`)

    gt(dpm_current_talent) %>%
      cols_align(
        align = "center"
      ) %>%
      cols_label(
        `DPM` = md("**DPM**")
      ) %>%
      tab_header(
        title = md("**Overview**"),
      )
  })

  output$table2 <- render_gt({
    dpm_current_talent <- player_profile_data() %>%
      select(`Box DPM`, `Box O-DPM`, `Box D-DPM`)

    gt(dpm_current_talent) %>%
      cols_align(
        align = "center"
      )
  })

  output$table3 <- render_gt({
    demo_current_talent <- player_profile_data() %>%
      select(Age,
        `Total Games` = career_game_num,
        Pos, `Rookie Year` = rookie_season
      )


    gt(demo_current_talent) %>%
      cols_align(
        align = "center"
      )
  })

  output$dpm_table <- render_gt({
    gt(player_profile_data() %>%
      select(all_of(dpm_table_cols))) %>%
      cols_align(
        align = "center"
      ) %>%
      tab_header(
        title = md("**Plus-Minus**"),
      )
  })

  output$style_table <- render_gt({
    gt(player_profile_data() %>%
      select(all_of(style_table_cols)) %>%
      mutate(Pace = round(Pace, 1)) %>%
      mutate(across(tidyselect::ends_with("%") & where(is.numeric), function(x) round(100 * x, 1)))) %>%
      cols_align(
        align = "center"
      ) %>%
      fmt_number(
        columns = all_of(style_table_cols),
        decimals = 1,
        use_seps = FALSE
      ) %>%
      tab_header(
        title = md("**Play Style Talent**"),
      )
  })

  output$box_table <- render_gt({
    gt(player_profile_data() %>%
      select(all_of(box_table_cols)) * 100) %>%
      cols_align(
        align = "center"
      ) %>%
      fmt_number(
        columns = all_of(box_table_cols),
        decimals = 1,
        use_seps = FALSE
      ) %>%
      tab_header(
        title = md("**Box Score Talent**"),
      )
  })

  output$efficiency_table <- render_gt({
    gt(player_profile_data() %>%
      select(all_of(efficiency_table_cols)) * 100) %>%
      cols_align(
        align = "center"
      ) %>%
      fmt_number(
        columns = all_of(efficiency_table_cols),
        decimals = 1,
        use_seps = FALSE
      ) %>%
      tab_header(
        title = md("**Efficiency Talent**"),
      )
  })

  output$per_100_table <- render_gt({
    gt(player_profile_data() %>%
      select(all_of(per_100_table_cols))) %>%
      cols_align(
        align = "center"
      ) %>%
      fmt_number(
        columns = all_of(per_100_table_cols),
        decimals = 1,
        use_seps = FALSE
      ) %>%
      tab_header(
        title = md("**Per-100 Talent**"),
      )
  })

  #### Daily Per Game
  output$table_daily <- renderDataTable({
    datatable(per_game_table,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center; color:black; font-size:200% ;",
        "Daily Updating Per-Game Projections"
      ),
      filter = "top",
      rownames = FALSE,
      selection = list(mode = "single", target = "row", selected = c(1)),
      options = list(
        drawCallback = drawCallback,
        pageLength = 30,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    ) %>%
      formatStyle("Minutes",
        background = styleInterval(brks_min, clrs_min),
        color = "black"
      ) %>%
      formatStyle("Pace",
        background = styleInterval(brks_pace, clrs_pace),
        color = "black"
      ) %>%
      formatStyle("PTS",
        background = styleInterval(brks_pts, clrs_pts),
        color = "black"
      ) %>%
      formatStyle("AST",
        background = styleInterval(brks_ast, clrs_ast),
        color = "black"
      ) %>%
      formatStyle("DREB",
        background = styleInterval(brks_dreb, clrs_dreb),
        color = "black"
      ) %>%
      formatStyle("OREB",
        background = styleInterval(brks_oreb, clrs_oreb),
        color = "black"
      ) %>%
      formatStyle("BLK",
        background = styleInterval(brks_blk, clrs_blk),
        color = "black"
      ) %>%
      formatStyle("STL",
        background = styleInterval(brks_stl, clrs_stl),
        color = "black"
      ) %>%
      formatStyle("TOV",
        background = styleInterval(brks_tov, clrs_tov),
        color = "black"
      ) %>%
      formatStyle("FGA",
        background = styleInterval(brks_fga, clrs_fga),
        color = "black"
      ) %>%
      formatStyle("FTA",
        background = styleInterval(brks_fta, clrs_fta),
        color = "black"
      ) %>%
      formatStyle("FG3A",
        background = styleInterval(brks_fg3a, clrs_fg3a),
        color = "black"
      ) %>%
      formatStyle("RimFGA",
        background = styleInterval(brks_rfga, clrs_rfga),
        color = "black"
      ) %>%
      formatStyle("PF",
        background = styleInterval(brks_pf, clrs_pf),
        color = "black"
      )
  })


  #### Daily Projections ####
  output$download_daily <- downloadHandler(
    filename <- function() {
      paste0("DARKO_daily_projections_", version_date[[1]], ".csv")
    },
    content <- function(file) {
      write.csv(per_game_table %>%
        mutate(date_of_projection = version_date[[1]]), file, row.names = FALSE)
    }
  )

  #### Historical Trajectory ####
  output$comp <- renderPlot(
    {
      shiny::validate(
        need(is.null(input$comps) == FALSE, "Please Select at Least One Player.")
      )

      players_to_compare <- input$comps
      player_names <- gsub(":.*", "", players_to_compare)
      color_values <- c("#385BBB", "#EF2D56", "#0CCE6B", "#ED7D3A", "#DCED31")
      color_labels <- player_names
      player_levels <- players_to_compare

      talent_display_lab <- names(which(talent_label_link == input$comp_options))

      if (input$time_type == "game_num") {
        x_lab <- "Career Game Number"

        comparison_data <- historical_talent %>%
          filter(codename %in% players_to_compare) %>%
          group_by(player_name, codename) %>%
          mutate(game_num = 1:n()) %>%
          ungroup() %>%
          mutate(codename = factor(codename, levels = players_to_compare))
      } else if (input$time_type == "age") {
        x_lab <- "Player Age"

        comparison_data <- historical_talent %>%
          filter(codename %in% players_to_compare) %>%
          group_by(player_name, codename) %>%
          mutate(game_num = 1:n()) %>%
          ungroup() %>%
          mutate(codename = factor(codename, levels = players_to_compare))
      } else {
        x_lab <- "Season Start Points"

        comparison_data <- historical_talent %>%
          ungroup() %>%
          filter(codename %in% players_to_compare) %>%
          mutate(codename = factor(codename, levels = players_to_compare)) %>%
          mutate(total_game_num_dec = season_numeric + game_num / 1000) %>%
          mutate(total_game_idx = dense_rank(total_game_num_dec))

        season_starts <- comparison_data %>%
          group_by(season) %>% 
          filter(total_game_idx == min(total_game_idx)) %>%
          select(season, total_game_idx) %>%
          distinct()
      }

      if (input$comp_options %in% c("ft_pct", "fg3_pct", "rim_fg_pct", "drb_pct", "orb_pct",
                                    "fg2_pct", "ast_pct", "blk_pct", "stl_pct", "tov_pct")) {
        G <- ggplot() +
          labs(
            x = x_lab,
            y = paste0("DARKO ", talent_display_lab),
            title = paste0("DARKO Career ", talent_display_lab, " Progression"),
            caption = basic_cite
          ) +
          scale_y_continuous(labels = scales::label_percent(accuracy = 1))
      } else if(input$comp_options %in% c("dpm", "d_dpm", "o_dpm")) {
        G <- ggplot() +
          geom_hline(yintercept = 0, color = "black", linetype = 2, lwd = 1) +
          labs(
            x = x_lab,
            y = paste0("DARKO ", talent_display_lab),
            title = paste0("DARKO Career ", talent_display_lab, " Progression"),
            caption = basic_cite
          )
      } else {
        G <- ggplot() +
          labs(
            x = x_lab,
            y = paste0("DARKO ", talent_display_lab),
            title = paste0("DARKO Career ", talent_display_lab, " Progression"),
            caption = basic_cite
          )
      }

      if (input$time_type == "total_game_idx") {
        G <- G + geom_vline(
          xintercept = season_starts$total_game_idx,
          color = "#636166", linetype = 1, lwd = 0.25
        ) +
          scale_x_continuous(
            breaks = season_starts$total_game_idx,
            labels = paste0(season_starts$season)
          ) +
          theme_talent_comparison_seasons()
        # theme_trend_single_player() +
        # theme(legend.position = "top",
        #      plot.title = element_text(hjust = 0.5))
      } else {
        G <- G + theme_talent_comparison()
      }

      G + geom_point(
        data = comparison_data,
        aes_string(x = input$time_type, y = input$comp_options, color = "codename"),
        size = 3, alpha = 0.25, shape = 16
      ) +
        geom_smooth(
          data = comparison_data,
          aes_string(x = input$time_type, y = input$comp_options, color = "codename"),
          lwd = 2, method = "loess", se = FALSE, span = 0.5
        ) +
        scale_color_manual(
          values = color_values,
          name = "",
          labels = color_labels
        ) +
        guides(color = guide_legend(override.aes = list(size = 4)))
    },
    width = 900,
    height = 620
  )

  #### Snapshot ####
  output$snap <- renderPlot(
    {
      shiny::validate(
        need(is.null(input$snaps) == FALSE, "Please Select at Least One Player.")
      )

      players_to_compare <- input$snaps
      player_names <- gsub(":.*", "", players_to_compare)
      color_values <- c("#385BBB", "#EF2D56", "#0CCE6B", "#ED7D3A", "#DCED31")
      color_labels <- player_names
      player_levels <- players_to_compare

      talent_display_lab <- all_talent_labels %>%
        filter(actual == input$snap_options) %>%
        select(display) %>%
        pull()

      plot_data <- historical_talent %>%
        filter(season_numeric == max(season_numeric)) %>% 
        filter(codename %in% players_to_compare) %>%
        mutate(codename = factor(codename, levels = players_to_compare)) %>% 
        group_by(codename) %>% 
        mutate(season_game_num = n():1) %>% 
        ungroup() %>% 
        mutate(played_in_game = ifelse(seconds_played > 0, 1, 0)) %>% 
        mutate(played_in_game = factor(played_in_game, levels = c(0, 1)))

      ggplot(data = plot_data) +
        geom_vline(xintercept = 1, lwd = 1.5) +
        geom_path(aes_string(
          x = "season_game_num", y = input$snap_options,
          color = "codename"
        ),
        lwd = 1.5, method = "loess", se = FALSE, span = 0.5
        ) +
        geom_point(aes_string(
          x = "season_game_num", y = input$snap_options,
          color = "codename",
          shape = "played_in_game"
        ),
        size = 3, stroke = 2
        ) +
        scale_shape_manual(values = c(4, 16), name = "", labels = c("DNP", "Played")) +
        scale_x_reverse(breaks = c(82, 70, 60, 50, 40, 30, 20, 15, 10, 5, 1)) +
        scale_color_manual(
          values = color_values,
          name = "",
          labels = color_labels
        ) +
        labs(
          x = "# Games Ago",
          y = paste0("DARKO ", talent_display_lab),
          title = paste0("2024-25 DARKO ", talent_display_lab, " Snapshot"),
          caption = basic_cite
        ) +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        theme_talent_snap()
    },
    width = 800,
    height = 400
  )

  #### Scatterplots
  scatter_data_filtered <- reactive({
    return(scatter_data %>%
      filter(box_dpm >= min(input$sliderBPM) & box_dpm <= max(input$sliderBPM)) %>%
      filter(minutes >= min(input$sliderMin) & minutes <= max(input$sliderMin)) %>%
      mutate(highlight = ifelse(player_name == input$scatterPlayer, "Y", "N")))
  })

  scatter_labels_dpm <- reactive({
    return(data.frame(
      min = min(input$sliderBPM),
      max = max(input$sliderBPM)
    ))
  })

  scatter_labels_minutes <- reactive({
    return(data.frame(
      min = min(input$sliderMin),
      max = max(input$sliderMin)
    ))
  })

  xLab <- reactive({
    return(all_talent_labels %>%
      filter(actual == paste0(input$statX)) %>%
      select(display) %>%
      pull())
  })

  yLab <- reactive({
    return(all_talent_labels %>%
      filter(actual == paste0(input$statY)) %>%
      select(display) %>%
      pull())
  })

  output$scatter <- renderPlot(
    {
      if (input$scatterOptions == "Trendline") {
        G <- ggplot() +
          geom_smooth(
            data = scatter_data_filtered(),
            aes_string(x = input$statX, y = input$statY),
            method = "lm", lwd = 1.5, color = "#363537"
          )
      } else if (input$scatterOptions == "Quadrants") {
        G <- ggplot() +
          geom_hline(
            yintercept = mean(scatter_data_filtered()[[paste0(input$statY)]], na.rm = TRUE),
            lwd = 2, linetype = 2
          ) +
          geom_vline(
            xintercept = mean(scatter_data_filtered()[[paste0(input$statX)]], na.rm = TRUE),
            lwd = 2, linetype = 2
          )
      }

      if (input$scatterLabels == "No") {
        G + geom_point(
          data = scatter_data_filtered(),
          aes_string(
            x = input$statX,
            y = input$statY,
            color = "highlight"
          ),
          size = 3, alpha = 0.65
        ) +
          scale_color_manual(values = c("#0C39CE", "#EF2D56")) +
          guides(color = FALSE) +
          labs(
            x = paste0(xLab()),
            y = paste0(yLab()),
            title = paste0("DARKO 2020-21"),
            subtitle = paste0(
              "Box DPM: ", scatter_labels_dpm()$min, "-", scatter_labels_dpm()$max,
              " | Minutes: ", scatter_labels_minutes()$min, "-", scatter_labels_minutes()$max,
              " | Player: ", paste0(input$scatterPlayer)
            ),
            caption = basic_cite
          ) +
          theme_new()
      } else {
        G + geom_point(
          data = scatter_data_filtered(), aes_string(
            x = input$statX,
            y = input$statY,
            color = "highlight"
          ),
          size = 3, alpha = 0.65
        ) +
          geom_label_repel(
            data = scatter_data_filtered(),
            aes_string(
              x = input$statX,
              y = input$statY,
              label = "player_name"
            ), size = 3
          ) +
          scale_color_manual(values = c("#0C39CE", "#EF2D56")) +
          guides(color = FALSE) +
          labs(
            x = paste0(xLab()),
            y = paste0(yLab()),
            title = paste0("DARKO 2020-21"),
            subtitle = paste0(
              "Box DPM: ", scatter_labels_dpm()$min, "-", scatter_labels_dpm()$max,
              " | Minutes: ", scatter_labels_minutes()$min, "-", scatter_labels_minutes()$max,
              " | Player: ", paste0(input$scatterPlayer)
            ),
            caption = basic_cite
          ) +
          theme_new()
      }
    },
    width = 800,
    height = 800
  )


  #### Longevity

  selected_player_survival_data <- eventReactive(input$surv_table_rows_selected, {
    if (is.null(input$surv_table_rows_selected) == FALSE) {
      index <- input$surv_table_rows_selected

      selected_player_survival_data <- survival_current_talent[index, ] %>%
        ungroup()

      return(selected_player_survival_data)
    } else {
      shiny::showNotification("Please select player row to show subtable", type = "error")

      NULL
    }
  })

  output$surv_plot_roster <- renderPlot(
    {
      shiny::validate(
        need(is.null(input$surv_table_rows_selected) == FALSE, "Please Select a Player")
      )

      player_frame <- survival_data %>%
        filter(codename == unique(selected_player_survival_data()$codename)) %>%
        filter(career_game_num == max(career_game_num)) %>%
        select(as.character(seq(1:12))) %>%
        pivot_longer(
          cols = as.character(seq(1:12)),
          names_to = "yrs",
          values_to = "prob"
        ) %>%
        mutate(yrs = as.integer(yrs))

      player_name <- unique(selected_player_survival_data()$Player)

      vals <- player_frame$prob

      surv_colors <- c("#0CCE6B", "#DCED31", "#EF2D56")
      names(surv_colors) <- c(">75% on Roster", "75%-25% on Roster", "<25% on Roster")

      max_surv <- max(vals)
      min_surv <- min(vals)

      if (min_surv <= 0.25 & max_surv >= 0.75) { ## all three

        x75 <- max(which(vals >= 0.75))
        x25 <- min(which(vals <= 0.25)) - 1

        rect_frame <- data.frame(
          xmin = c(1, x75, x25),
          xmax = c(x75, x25, 12),
          ymin = c(0, 0, 0),
          ymax = c(1, 1, 1),
          lab = c(">75% on Roster", "75%-25% on Roster", "<25% on Roster")
        ) %>%
          mutate(lab = factor(lab, levels = c(">75% on Roster", "75%-25% on Roster", "<25% on Roster")))
      } else if (min_surv <= 0.25 & between(max_surv, 0.25, 0.75)) { ## just 50 and 25

        x25 <- min(which(vals <= 0.25)) - 1

        rect_frame <- data.frame(
          xmin = c(1, x25),
          xmax = c(x25, 12),
          ymin = c(0, 0),
          ymax = c(1, 1),
          lab = c("75%-25% on Roster", "<25% on Roster")
        ) %>%
          mutate(lab = factor(lab, levels = c("75%-25% on Roster", "<25% on Roster")))
      } else if (max_surv <= 0.25) { ## just 25

        rect_frame <- data.frame(
          xmin = c(1),
          xmax = c(12),
          ymin = c(0),
          ymax = c(1),
          lab = c("<25% on Roster")
        ) %>%
          mutate(lab = factor(lab, levels = c("<25% on Roster")))
      } else if (min_surv >= 0.75) { ## just 75

        rect_frame <- data.frame(
          xmin = c(1),
          xmax = c(12),
          ymin = c(0),
          ymax = c(1),
          lab = c(">75% on Roster")
        ) %>%
          mutate(lab = factor(lab, levels = c(">75% on Roster")))
      } else if (max_surv >= 0.25 & between(min_surv, 0.25, 0.75)) { ## just 75 and 50

        x75 <- max(which(vals >= 0.75))

        rect_frame <- data.frame(
          xmin = c(1, x75),
          xmax = c(x75, 12),
          ymin = c(0, 0),
          ymax = c(1, 1),
          lab = c(">75% on Roster", "75%-25% on Roster")
        ) %>%
          mutate(lab = factor(lab, levels = c(">75% on Roster", "75%-25% on Roster")))
      }

      # Get the current season from survival_data
      current_season <- survival_data %>%
        filter(current_season == 1) %>%
        select(season) %>%
        distinct() %>%
        pull()
      
      
      ggplot() +
        geom_rect(data = rect_frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = lab), alpha = 0.35) +
        scale_fill_manual(values = surv_colors, name = "") +
        geom_line(data = player_frame, aes(x = yrs, y = prob), lwd = 2) +
        geom_point(data = player_frame, aes(x = yrs, y = prob), size = 4) +
        scale_x_continuous(breaks = seq(1:12), limits = c(1, 12), labels = paste0("+", seq(1:12))) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(
          x = "NBA Seasons From Current",
          y = "Probability of Being on Roster",
          title = paste0(player_name),
          subtitle = paste0("Longevity Projections Onwards From ", current_season),
          caption = basic_cite
        ) +
        theme_surv()
    },
    width = 650,
    height = 300
  )

  output$surv_plot_years <- renderPlot(
    {
      shiny::validate(
        need(is.null(input$surv_table_rows_selected) == FALSE, "Please Select a Player")
      )

      player_frame <- survival_data %>%
        filter(codename == unique(selected_player_survival_data()$codename)) %>%
        mutate(game_num = 1:n()) %>%
        mutate(age_at_ret = projected_years_remaining + age)

      player_name <- unique(player_frame$player_name)

      season_data <- player_frame %>%
        filter(first_game_of_season == 1) %>%
        select(game_num) %>%
        pull()

      ggplot() +
        geom_vline(xintercept = season_data, color = "#636166", linetype = 1, lwd = 0.25) +
        scale_x_continuous(
          breaks = c(season_data),
          labels = unique(unique(na.omit(player_frame$season)))
        ) +
        scale_y_continuous(breaks = seq(0, 100, 2)) +
        geom_point(
          data = player_frame, aes(x = game_num, y = age_at_ret),
          color = "#006BB6", size = 2, alpha = 0.65
        ) +
        geom_smooth(
          data = player_frame, aes(x = game_num, y = age_at_ret),
          color = NA, fill = "#ED174C",
          lwd = 1, method = "loess"
        ) +
        labs(
          x = "Season Start Points",
          y = "Projected Age at Retirement\n",
          title = paste0(player_name),
          subtitle = paste0("Career Length Projections"),
          caption = "@kmedved | www.darko.app | @anpatt7"
        ) +
        theme_trend_single_player()
    },
    width = 650,
    height = 300
  )

  output$surv_table <- renderDataTable({
    DT::datatable(survival_current_talent %>%
      select(-codename),
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: center; color:black; font-size:200% ;",
      paste0("Career Longevity Projections")
    ),
    filter = "top",
    rownames = FALSE,
    selection = list(mode = "single", target = "row", selected = c(1)),
    options = list(
      drawCallback = drawCallback,
      pageLength = 20,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    )
    ) %>%
      formatPercentage(paste0("+", seq(1:12)), 1) %>%
      formatStyle("+1",
        backgroundColor = styleInterval(brks_1, clrs_1),
        color = "black"
      ) %>%
      formatStyle("+2",
        backgroundColor = styleInterval(brks_2, clrs_2),
        color = "black"
      ) %>%
      formatStyle("+3",
        backgroundColor = styleInterval(brks_3, clrs_3),
        color = "black"
      ) %>%
      formatStyle("+4",
        backgroundColor = styleInterval(brks_4, clrs_4),
        color = "black"
      ) %>%
      formatStyle("+5",
        backgroundColor = styleInterval(brks_5, clrs_5),
        color = "black"
      ) %>%
      formatStyle("+6",
        backgroundColor = styleInterval(brks_6, clrs_6),
        color = "black"
      ) %>%
      formatStyle("+7",
        backgroundColor = styleInterval(brks_7, clrs_7),
        color = "black"
      ) %>%
      formatStyle("+8",
        backgroundColor = styleInterval(brks_8, clrs_8),
        color = "black"
      ) %>%
      formatStyle("+9",
        backgroundColor = styleInterval(brks_9, clrs_9),
        color = "black"
      ) %>%
      formatStyle("+10",
        backgroundColor = styleInterval(brks_10, clrs_10),
        color = "black"
      ) %>%
      formatStyle("+11",
        backgroundColor = styleInterval(brks_11, clrs_11),
        color = "black"
      ) %>%
      formatStyle("+12",
        backgroundColor = styleInterval(brks_12, clrs_12),
        color = "black"
      )
  })
  
  output$west <- render_gt({
    
    projections %>% 
      filter(conference == "West") %>% 
      left_join(logos) %>% 
      mutate(wins = round(mean),
             losses = 82 - wins) %>% 
      arrange(-wins) %>% 
      mutate(Seed = 1:n()) %>% 
      select(logo_url, team_name, Seed, wins, losses, projected_1_7, projected_8_9) %>%
      gt() %>% 
      gt_img_rows(logo_url, img_source = 'web', height = 35) %>%
      cols_label("team_name" = "Team",
                 "logo_url" = "",
                 "wins" = "Wins",
                 "losses" = "Losses",
                 "projected_1_7" = "% Top Six",
                 "projected_8_9" = "% Play In") %>% 
      fmt_percent(columns = c("projected_1_7", "projected_8_9"),
                  decimals = 0) %>% 
      tab_header(
        title = md("**Baseline Western Conference DARKO Win Projections 2022-23 Season**")
      ) %>%
      tab_footnote(footnote = "Kostya Medvedovsky - @kmedved | www.darko.app | @anpatt7 - Andrew Patton") %>% 
      cols_align(
        align = c("center"),
        columns = Seed:projected_8_9
      ) %>% 
      gt_hulk_col_numeric(c("projected_1_7", "projected_8_9")) %>% 
      tab_style(
        style = list(
          cell_borders(
            sides = c("bottom"),
            color = "#363537",
            weight = px(4)
          )
        ),
        locations = list(
          cells_body(
            rows = c(6, 10)
          )
        )
      )
    
  })
  
  output$east <- render_gt({
    
    projections %>% 
      filter(conference == "East") %>% 
      left_join(logos) %>% 
      mutate(wins = round(mean),
             losses = 82 - wins) %>% 
      arrange(-wins) %>% 
      mutate(Seed = 1:n()) %>% 
      select(logo_url, team_name, Seed, wins, losses, projected_1_7, projected_8_9) %>%
      gt() %>% 
      gt_img_rows(logo_url, img_source = 'web', height = 35) %>%
      cols_label("team_name" = "Team",
                 "logo_url" = "",
                 "wins" = "Wins",
                 "losses" = "Losses",
                 "projected_1_7" = "% Top Six",
                 "projected_8_9" = "% Play In") %>% 
      fmt_percent(columns = c("projected_1_7", "projected_8_9"),
                  decimals = 0) %>% 
      tab_header(
        title = md("**Baseline Eastern Conference DARKO Win Projections 2022-23 Season**")
      ) %>%
      tab_footnote(footnote = "Kostya Medvedovsky - @kmedved | www.darko.app | @anpatt7 - Andrew Patton") %>% 
      cols_align(
        align = c("center"),
        columns = Seed:projected_8_9
      ) %>% 
      gt_hulk_col_numeric(c("projected_1_7", "projected_8_9")) %>% 
      tab_style(
        style = list(
          cell_borders(
            sides = c("bottom"),
            color = "#363537",
            weight = px(4)
          )
        ),
        locations = list(
          cells_body(
            rows = c(6, 10)
          )
        )
      )
    
  })


}

# Run the application
shinyApp(ui = ui, server = server)
