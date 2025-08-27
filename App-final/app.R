# Final App ----

# loading packages
library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(bslib)
library(here)

# loading data
load(file = ("data/PSPS.rda")) 

# data cleaning
PSPS <- PSPS |>
  filter(!nonagwork_pay_sch_1 %in% c(-666, -888, -999)) |>
  mutate(
    # Convert code to numeric
    nonagwork_pay_sch_1 = as.numeric(nonagwork_pay_sch_1),
    
    # multiplier based on pay schedule
    multiplier = case_when(
      nonagwork_pay_sch_1 == 1 ~ 26,       # Daily
      nonagwork_pay_sch_1 == 2 ~ 4.33,     # Weekly
      nonagwork_pay_sch_1 == 3 ~ 2.17,     # Biweekly
      nonagwork_pay_sch_1 == 4 ~ 1,        # Monthly
      nonagwork_pay_sch_1 == 5 ~ 0.5,      # Every other month
      nonagwork_pay_sch_1 == 6 ~ 0.33,     # Quarterly
      nonagwork_pay_sch_1 == 7 ~ 1,        # Irregular — treat as monthly
      nonagwork_pay_sch_1 == 8 ~ 1/12,     # Yearly
      nonagwork_pay_sch_1 == 9 ~ 1/12,     # One-time
      TRUE ~ NA_real_
    ),
    
    # Calculate monthly pay
    agwork_monthly_pay_1 = as.numeric(v_agwork_pay_amt_1) * multiplier,
    nonagwork_monthly_pay_1 = as.numeric(v_nonagwork_pay_amt_1) * multiplier,
    total_monthly_pay = rowSums(across(c(agwork_monthly_pay_1, nonagwork_monthly_pay_1)), na.rm = TRUE)
  ) |>
  mutate(
    gender = if_else(r_gender == 1, "Female", "Male")
  ) |>
  select(-r_gender) |>
  relocate(agwork_monthly_pay_1, nonagwork_monthly_pay_1) |>
  filter(!is.na(gender))

# UI

ui <- 
  
  page_sidebar(
  title = "Income and Labor Patterns in Western Visayas",
  
  h5("Dashboard Summary", style = "font-weight: bold;"),
  
  tags$p("This dashboard presents insights into income and labor patterns in rural Western Visayas, based on the Philippines Socioeconomic Panel Survey (PSPS). It focuses on the “Income and Labor” module, highlighting both earned and unearned income distributions. Users can explore how these distributions vary by gender and age, and toggle between different income types using the dropdown menus located on the left side of the dashboard."),
  tags$a(
    href = "https://www.kellogg.northwestern.edu/academics-research/global-poverty-research-lab/geographical-clusters/philippines/",
    "Learn more about the Global Poverty Research Lab’s work in the Philippines Here",
    target = "_blank"
  ),
  
  sidebar = sidebar(
    width = "3in",
    
    h5("Instructions", style = "font-weight: bold;"),
    
    tags$p("Use the dropdown menus below to customize the dashboard:"),
  
## Earned income sidebar    
    
    conditionalPanel(
      condition = "input.nav == 'Earned Income'",
      selectInput(
        "earned_xvar",
        label = "Occupation Type",
        choices = list(
          "Agricultural", 
          "Non-Agricultural"
        ),
        selected = "Agricultural"
      )
      
    ),
    
## Unearned income sidebar
    
    conditionalPanel(
      condition = "input.nav == 'Unearned Income'",
      selectInput(
        "unearned_xvar",
        label = "Remittance Type",
        choices = list(
          "Foreign", 
          "Domestic"
        ),
        selected = "Foreign"
      )
      
    ),
    
    selectInput(
      "variable",
      "Additional Variable",
      choices = c("Gender" = 1, "Age" = 2, "None" = 3),
      selected = 3
    ),
    
    h5("Data Source", style = "font-weight: bold;"),
    
    tags$p("Data used in this app comes from the Philippines Socioeconomic Panel Survey (PSPS), a longitudinal study of approximately 13,000 rural households in the Western Visayas region of the Philippines, which began in late 2023. 
    This survey was conducted by the Global Poverty Research Lab (GPRL) operating out of the Kellogg School of Management."),
    
    tags$p("The specific module used for this dashboard is Income and Labor, which pertains to all questions related to income and labor."),
    
  ),
  
## Earned income cards

  navset_tab(
    id = "nav",
    nav_panel("Earned Income",
              h3("Earned Income Tab", style = "margin-top: 20px; margin-bottom: 20px;"),
              
              # card 1
              layout_columns(
                card(
                  spacing = "lg",
                  conditionalPanel(
                    condition = "input.variable == 1 || input.variable == 3",
                    sliderInput(
                      inputId = "bw",
                      label = "Density Smoothing Bandwidth:",
                      min = 0.1,
                      max = 2,
                      value = 0.5,
                      step = 0.1
                    )
                  ),
                  plotOutput(outputId = "earningsplot", height = "500px")
                ),
                
                # card 2
                card(
                  spacing = "lg",
                  plotOutput(outputId = "agriplot_occupation", height = "500px")
                ),
                col_widths = c(7, 5)  # optional: adjusts relative widths
              )
    ),
   
## Unearned income cards
    
    nav_panel("Unearned Income",
              h3("Unearned Income Tab", style = "margin-top: 20px; margin-bottom: 20px;"),
              card(
                spacing = "lg",
                conditionalPanel(
                condition = "input.variable != 2",
                sliderInput(
                  inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30
                )
                ),
                plotOutput(outputId = "remittanceplot", height = "500px")
              )
    )
  )
)


# Server
server <- function(input, output) {
  
# Bar plots of occupation types

  output$agriplot_occupation <- renderPlot({
    if (input$earned_xvar == "Agricultural") {
      
      ## Agricultural plot
      PSPS |>
        filter(!is.na(agwork_prod_1)) |>
        separate_rows(agwork_prod_1, sep = " ") |>
        filter(agwork_prod_1 %in% c("1", "2", "3", "4", "5", "6", "7", "8")) |>
        count(agwork_prod_1) |>
        mutate(
          agwork_prod_1 = if_else(agwork_prod_1 %in% c("1", "6"), agwork_prod_1, "Other"),
          prop = n / sum(n),
          agwork_prod_1 = factor(agwork_prod_1, levels = c("1", "6", "Other"))
               ) |>
        ggplot(aes(x = agwork_prod_1, y = prop * 100)) +
        geom_col(fill = "#4682b4") +
        scale_x_discrete(labels = c(
          "1" = "Farming (Crops)",
          "6" = "Fishing or Aquaculture",
          "Other" = "Other"
        )) +
        labs(
          x = NULL,
          y = "Percentage of Respondents", 
          title = "Types of Agricultural\nWork Reported by Respondents"
             ) +
        theme_bw() +
        scale_y_continuous(
          labels = scales::label_number(suffix = "%")
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 15)),
          axis.title.y = element_text(size = 15, margin = margin(r = 20)),
          axis.text = element_text(size = 15)
          )
      
    } else {
      
      ## Non-Agricultural plot
      PSPS |>
        filter(!is.na(nonagwork_occ_1)) |>
        filter(nonagwork_occ_1 != -666) |> 
        count(nonagwork_occ_1) |>
        slice_max(n = 10, order_by = n) |>
        mutate(
          nonagwork_occ_1 = factor(if_else(!as.character(nonagwork_occ_1) %in% c("6", "8", "10"),
                                           as.character(nonagwork_occ_1), "Other")),
          prop = n / sum(n),
          nonagwork_occ_1 = factor(nonagwork_occ_1, levels = c("1", "2", "3", "4", "5", "14", "15", "Other"))
        ) |>
        
        # Creating bar graph
        ggplot(aes(x = nonagwork_occ_1, y = prop * 100)) +
        geom_col(fill = "#4682b4") +
        
        ## Labeling
        scale_x_discrete(labels = c(
          "1" = "Retail",
          "2" = "Construction",
          "3" = "Trades (e.g. welding)",
          "4" = "Transport",
          "5" = "Services (e.g. food)",
          "14" = "Domestic Work",
          "15" = "Government"
        )) +
        labs(
          x = NULL,
          y = "Percentage of Respondents", 
          title = "Types of Non-Agricultural\nWork Reported by Respondents"
          ) +
        scale_y_continuous(
          labels = scales::label_number(suffix = "%")
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15),
          plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 15)),
          axis.title.y = element_text(size = 15, margin = margin(r = 20)),
          axis.text = element_text(size = 15)
        )
    }
  })
  
  
# Density and scatterplots
  
  output$earningsplot <- renderPlot({
    
    if (input$variable == "2") {
      
      target_var <- case_when(
        input$earned_xvar == "Agricultural" ~ "agwork_monthly_pay_1",
        input$earned_xvar == "Non-Agricultural" ~ "nonagwork_monthly_pay_1"
      )
      
      # Age selected: show scatterplot
      PSPS |>
        ggplot(aes(x = log10(!!sym(target_var) + 1), y = r_age)) +
        geom_point(color = "#4D4D4D") +
        geom_smooth(method = "loess", color = "#0072B2", alpha = 0.2) +
        scale_x_continuous(
          name = "Monthly Earnings (₱, Log Base-10 Scale)",
          breaks = 1:6,
          labels = scales::label_dollar(prefix = "₱", scale = 1)(10 ^ (1:6)),
          limits = c(0, 5.5),
          expand = c(0, 0)
        ) +
        labs(y = "Age") +
        scale_y_continuous(
          expand = c(0, 0),
          limits = c(16, 90)
        ) +
        labs(
          title = paste0(
            "Distribution of Monthly Earnings by Age\n(Logarithmic Transformation on a Base-10 Scale)\n(", 
            input$earned_xvar, 
            " Jobs)"
          )
        ) +
        theme_bw() +
        theme(
          plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
          plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 10)),
          axis.title.x = element_text(size = 15, margin = margin(t = 15)),
          axis.title.y = element_text(size = 15, margin = margin(r = 15)),
          axis.text = element_text(size = 15)
        )
      
    } else {
      # Gender or None selected: show histogram
      target_var <- case_when(
        input$earned_xvar == "Agricultural" ~ "agwork_monthly_pay_1",
        input$earned_xvar == "Non-Agricultural" ~ "nonagwork_monthly_pay_1"
      )
      
      x    <- PSPS |> pull(!!sym(target_var))
      
      x_log <- log10(x + 1)
      
      bin_breaks <- seq(min(x_log, na.rm = TRUE), max(x_log, na.rm = TRUE), length.out = input$bins + 1)
      
      p <- ggplot(PSPS, aes(x = log10(!!sym(target_var) + 1)))
      
      if (input$variable == "1") {
        
        p <- p + stat_density(aes(fill = gender), alpha = 0.7, position = "identity", bw = input$bw) +
          
          labs(
            title = paste0(
              "Distribution of Monthly Earnings by Gender\n(Logarithmic Transformation on a Base-10 Scale)\n(", 
              input$earned_xvar, 
              " Jobs)"
            )
          )
          
      } else {
        
        p <- p + stat_density(fill = "lightblue", alpha = 0.7, bw = input$bw) +
          
          labs(
            title = paste0(
              "Distribution of Monthly Earnings\n(Logarithmic Transformation on a Base-10 Scale)\n(", 
              input$earned_xvar, 
              " Jobs)"
            )
          )
        
      }
      
      p +
        scale_x_continuous(
          name = "Monthly Earnings (₱, Log Base-10 Scale)",
          breaks = 0:6,
          labels = scales::label_dollar(prefix = "₱", scale = 1)(10 ^ (0:6)),
          limits = c(0, 6),
          expand = c(0, 0)
        ) +
        labs(
          y = NULL,
             ) +
        scale_y_continuous(
          expand = c(0, 0),
          limits = c(0, 0.65)
          ) +
        scale_fill_manual(
          values = c(
            "Female" = "#FFB6C1",    
            "Male" = "#ADD8E6"
          )
        ) +
        theme_bw() +
        theme(
          plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
          plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 10)),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          axis.title.x = element_text(size = 15, margin = margin(t = 15)),
          axis.text = element_text(size = 15)
          )
    }
  })
  
# histogram and scatterplots for remittances
  output$remittanceplot <- renderPlot({
    
        
    if (input$variable == "2") {
      
      target_var <- case_when(
        input$unearned_xvar == "Foreign" ~ "fremit_total",
        input$unearned_xvar == "Domestic" ~ "dremit_total"
      )
      
      # Age selected: show scatterplot
      PSPS |>
        ggplot(aes(x = log10(!!sym(target_var) + 1), y = r_age)) +
        geom_point(color = "#4D4D4D") +
        geom_smooth(method = "loess", color = "#0072B2", alpha = 0.2) +
        geom_jitter(width = 0.1, height = 0)  +
        scale_x_continuous(
          name = "Monthly Remittances (₱, Log Base-10 Scale)",
          breaks = 1:6,
          labels = scales::label_dollar(prefix = "₱", scale = 1)(10 ^ (1:6)),
          limits = c(0, 6),
          expand = c(0, 0)
        ) +
        labs(y = "Age") +
        scale_y_continuous(
          expand = c(0.1, 0),
          limits = c(16, 90)
        ) +
        labs(
          title = paste0(
            "Distribution of Monthly Earnings by Age\n(Logarithmic Transformation on a Base-10 Scale)\n(Recieved ", 
            input$unearned_xvar, 
            " Remittances)"
          )
        ) +
        theme_bw() +
        theme(
          plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
          plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 10)),
          axis.title.x = element_text(size = 15, margin = margin(t = 15)),
          axis.title.y = element_text(size = 15, margin = margin(r = 15)),,
          axis.text = element_text(size = 15)
        ) +
        annotate(
          "text", 
          x = 1.5, y = 81, 
          label = "*Remittances: Money that migrants\nsend home to support their families",
          size = 5,
          fontface = "italic"
        )
      
    } else {
    
    target_var <- case_when(
      input$unearned_xvar == "Foreign" ~ "fremit_total",
      input$unearned_xvar == "Domestic" ~ "dremit_total"
    )
    
    x    <- PSPS |> pull(!!sym(target_var))
    x_log <- log10(x + 1)
    bin_breaks <- seq(min(x_log, na.rm = TRUE), max(x_log, na.rm = TRUE), length.out = input$bins + 1)
    
    p <- ggplot(PSPS, aes(x = log10(!!sym(target_var) + 1)))
    
    if (input$variable == "1") {
      p <- p + geom_histogram(aes(fill = gender), breaks = bin_breaks, alpha = 0.5, position = "identity", bins = 40) +
        labs(
          title = paste0(
            "Distribution of Recieved Monthly Remittances by Gender\n(Logarithmic Transformation on a Base-10 Scale)\n(", 
            input$unearned_xvar, 
            " Remittances)"
          )
        ) 
      
    } else {
      p <- p + geom_histogram(fill = "lightblue", breaks = bin_breaks, alpha = 0.5, bins = 40) +
        labs(
          title = paste0(
            "Distribution of Recieved Monthly Remittances\n(Logarithmic Transformation on a Base-10 Scale)\n(", 
            input$unearned_xvar, 
            " Remittances)"
          )
        ) +
        annotate(
          "text", 
          x = 1, y = 750, 
          label = "*Remittances: Money that migrants\nsend home to support their families",
          size = 5,
          fontface = "italic"
          )
        
    }
    
    p +
      theme_bw() +
      scale_y_continuous(
        limits = c(0, 850),
        name = "Number of Respondents",
        expand = c(0,0)
      ) +
      scale_x_continuous(
        name = "Monthly Remittances (₱, Log Base-10 Scale)",
        breaks = 0:6,
        labels = scales::label_dollar(prefix = "₱", scale = 1)(10 ^ (0:6)),
        limits = c(0, 6),
        expand = c(0, 0)
      ) +
      scale_fill_manual(
        values = c(
          "Female" = "#FFB6C1",    
          "Male" = "#ADD8E6"
        )) +
      theme(
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 13)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15, margin = margin(t = 15)),
        axis.title.y = element_text(size = 15, margin = margin(r = 15)),
        axis.text = element_text(size = 15)
      ) +
      annotate(
        "text", 
        x = 1, y = 750, 
        label = "*Remittances: Money that migrants\nsend home to support their families",
        size = 5,
        fontface = "italic"
      )
    }
  })
  
  
}

# Run app
shinyApp(ui = ui, server = server)
