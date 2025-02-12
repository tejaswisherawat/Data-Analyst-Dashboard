library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(GGally)

# Load the dataset
data <- read.csv("Cleaned_Dataset.csv")

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Analysts in Canada"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Description", tabName = "data_description", icon = icon("info-circle")),
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Information Gains", tabName = "info_gains", icon = icon("lightbulb"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home Section
      tabItem(
        tabName = "home",
        # Centered Title
        fluidRow(
          column(12, 
                 h2("Welcome to the Data Analyst Dashboard", style = "text-align: center; font-size: 36px; font-weight: bold; color: #2c3e50; margin-top: 50px;")
          )
        ),
        # Canada Map Image below the title
        fluidRow(
          column(12, 
                 div(style = "text-align: center;",
                     img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/14/Political_map_of_Canada.png/1200px-Political_map_of_Canada.png", width = "50%", height = "auto", style = "border-radius: 10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); margin-top: 30px;")
                 )
          )
        ),
        h4("Hi there! I am Tejaswi Sherawat and welcome to my Dashboard", style = "font-weight: bold;"),
        p("Here, we will be exploring a very interesting dataset that analyses the data analyst jobs in Canada."),
        p("The Canadian job market is highly diverse, influenced by factors like industry type, work arrangement, and geographic location. This dataset, composed of job listings, presents an opportunity to explore trends in salary, type of work, and job availability across different provinces and industries. By analyzing these factors, job seekers and employers can make informed decisions, ensuring a better match between skills and opportunities. This dashboard uses exploratory data analysis (EDA) to examine key variables that affect salaries, including job titles, seniority, work type and location. The goal is to provide insights into the salary distribution and the relationship between these variables."),
      ),
        
        tabItem( tabName = "data_description",
        h2("Data Description"),
        p("The dataset contains 1,796 job listings across various Canadian cities, industries, and job types. It includes the following 13 variables:", br(),
          "Job Title: The title or position of the job listing.", br(),
          "Job Info: Additional details about the job.", br(),  
          "Position: Specific job positions (e.g., Software Engineer, Data Scientist).", br(),  
          "Employer: Name of the employer.", br(),  
          "City: The city where the job is located.", br(),  
          "Province: The Canadian province of the job listing.", br(),  
          "Skill: Key skills required for the job.", br(),  
          "Seniority: The level of experience required.", br(),                                                            
          "Work Type: Whether the job is remote, on-site, or hybrid.", br(),  
          "Industry Type: The sector or industry of the job (e.g., Technology, Finance).", br(),  
          "Min_Salary, Max_Salary: Minimum and maximum salary ranges for each listing.", br(),  
          "Avg_Salary: The average salary for the job listing.", br(),  
          "This dataset enables an in-depth exploration of salary variations based on geographic location, industry type, and required skills, offering critical insights into the Canadian labor market. The dataset can be accessed at -", a("Dataset", href = "https://www.kaggle.com/datasets/amanbhattarai695/data-analyst-job-roles-in-canada"))
        ),
      
      # EDA Section
      tabItem(
        tabName = "eda",
        h2("Exploratory Data Analysis"),
        selectInput("eda_choice", "Choose Analysis:", 
                    choices = c("Salary Analysis", "Work Analysis", "Location Analysis")),
        uiOutput("eda_output")
      ),
      
      # Information Gains Section
      tabItem(
        tabName = "info_gains",
        h2("Information Gains"),
        p("We observe key trends in the Canadian job market. A notable proportion of employees earn around $80,000, showing the developed nature of the country. Large number of outliers at senior levels show the increasing role of performance based rewards. Remote and hybrid roles match in-person salaries, indicating a shift in work dynamics. The highest pay is seen in Database and Business System Analysts, reflecting strong demand for these skills. Location is an important factor, both in job listings as well as in the average salary considerations. With remote work being important even in traditional sectors such as healthcare, this shows changing work dynamics and hence a need for adaptability, both by the employer and the job-seekers.")
        ,hr(),
        h3("Thankyou for visiting !" ,style = "text-align: center; font-size: 36px; font-weight: bold; color: #2c3e50; margin-top: 50px;")
        )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # EDA Output
  output$eda_output <- renderUI({
    if (input$eda_choice == "Salary Analysis") {
      tagList(
        p("1) First, we look at min, max and average salary and their pairwise correlation:-"),
        plotOutput("salary_plot1"),br(),
        p("Here, we can see that most job listings show an average salary of around $80,000 and there is a strong correlation among salary variables, particularly between average and maximum salaries."),
       hr(), p("2) Next, we look at the relationship between average salary and seniority level."),
        plotOutput("salary_plot2"), br(),
        p("Here, we see that median salaries rise with seniority, and the number of outliers increases at higher levels."),
       hr(), p("3) Next, we look at the relationship between average salary and work type:-"),
        plotOutput("salary_plot3"), br(),
        p("Remote and hybrid roles offer median salaries that are nearly on par with in-person work."),
        hr(), p("4) Finally, we look at Salary distribution against job roles:-"),
        plotOutput("salary_plot4"), br(),
        p("We observe that The highest salaries are found in roles like Database Analysts and Business Systems Analysts.")
      )
    } else if (input$eda_choice == "Work Analysis") {
      tagList(
        p("1) Here, we first look at the number of people in different job titles"),
        plotOutput("work_plot1"), br(),
        p("We see that Job titles with the highest number of people are :- Senior Supply chain data analysts and Senior Business Intelligence Analysts."),
        hr(),p("2) Next, we observe the distribution of work types:- "),
        plotOutput("work_plot2"), br(),
        p(" In person work dominates the job market by a huge margin but remote work followed by hybrid work are also becoming increasingly relevant."),
       hr(), p("3) Now, we see the Job count by Industry, Seniority and work type (Top 10, excluding 'Others'):- "),
        plotOutput("work_plot3"), br(),
        p("The Technology and Finance sectors provide the most jobs, with remote work increasing in Services, Technology, and Healthcare.")
      )
    } else if (input$eda_choice == "Location Analysis") {
      tagList(
        p("1) We sort the provinces in descending order based on the number of job listings"),
        plotOutput("location_plot1"), br(),
        p("Ontario, being an economic hub, leads in the number of job listings by a significant margin."),
       hr(), p("2) Next, we look at the relationship between provinces and average salary:-"),
        plotOutput("location_plot2"), br(),
        p("The highest salaries are offered in Newfoundland and Labrador, and British Columbia.")
      )
    }
  })
  
  # Salary Analysis Plot
  output$salary_plot1 <- renderPlot({
    max_count <- max(hist(data$Min_Salary, plot = FALSE)$counts,
                     hist(data$Max_Salary, plot = FALSE)$counts,
                     hist(data$Avg_Salary, plot = FALSE)$counts)
    
    # Custom function to create histograms with fixed y-axis limits
    custom_hist <- function(data, mapping, ...) {
      ggplot(data, mapping) + 
        geom_histogram(bins = 30, fill = "lightblue", color = "black") + 
        ylim(0, max_count) +  # Set fixed y-axis limit for consistency
        labs(x = NULL, y = NULL) +  # Clear x and y labels
        theme_minimal() +
        theme(
          axis.title.x = element_text(size = 10),  # Set x-axis title size
          axis.title.y = element_text(size = 10),  # Set y-axis title size
          axis.text.x = element_text(size = 8),  # Set x-axis text size
          axis.text.y = element_text(size = 8),  # Set y-axis text size
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),  # Add grid lines
          panel.grid.minor = element_blank()  # Remove minor grid lines
        )  
    }
    
    # Use ggpairs to create a pair plot with custom histograms for diagonal elements
    ggpairs(data[, c("Min_Salary", "Max_Salary", "Avg_Salary")],
            diag = list(continuous = custom_hist),  # Apply custom histograms
            lower = list(continuous = wrap("points", color = "salmon"))) +  # Color scatterplots blue
      labs(title = "Pairwise Relationships between Salary Variables")
  })
  
  output$salary_plot2 <- renderPlot({ggplot(data, aes(x = Seniority, y = Avg_Salary, fill = Seniority)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Average Salary by Seniority Level", x = "Seniority", y = "Average Salary")})
  
  output$salary_plot3 <- renderPlot({ggplot(data, aes(x = Work.Type, y = Avg_Salary, fill = Work.Type)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Average Salary by Work Type", x = "Work Type", y = "Average Salary")})
  
  output$salary_plot4 <- renderPlot({ggplot(data, aes(x = Job.Title, y = Avg_Salary, fill = Job.Title)) +
      geom_boxplot() +
      theme_minimal() +
      coord_flip()  + theme(legend.position = "none")+
      labs(title = "Salary Distribution by Job Roles", y = "Average Salary")})
  
  # Work Analysis Plot
  output$work_plot1 <- renderPlot({
    ggplot(data, aes(x = Job.Title)) +
      geom_bar(fill = "yellow1", width = 0.5) +
      theme_minimal() +
      coord_flip() +  # Flip for better readability
      labs(title = "Distribution of Job Titles", x = "Job Title", y = "Count")
  })
  
  output$work_plot2 <- renderPlot({ggplot(data, aes(x = Work.Type)) +
      geom_bar(fill = "skyblue") +
      labs(title = "Distribution of Work Types", 
           x = "Work.Type", 
           y = "Count") +
      theme_minimal()})
  
  output$work_plot3 <- renderPlot({industry_count <- data %>%
    filter(Industry.Type != "Others") %>%                          # Exclude "Other"
    group_by(Industry.Type) %>%
    summarize(Total_Count = n(), .groups = "drop") %>%            # Total count per industry
    arrange(desc(Total_Count)) %>%                                # Sort by total job count
    top_n(10, Total_Count)                                        # Select top 10 industries
  
  # Step 2: Filter the original dataset to only include the top 10 industries
  top_10_industries <- data %>%
    filter(Industry.Type %in% industry_count$Industry.Type) %>%   # Keep only the top 10 industries
    group_by(Industry.Type, Seniority, Work.Type) %>%
    summarize(Count = n(), .groups = "drop")                      # Count jobs by industry, seniority, and work type
  
  # Step 3: Create the plot
  ggplot(top_10_industries, aes(x = reorder(Industry.Type, -Count), y = Seniority, size = Count, color = Work.Type)) +
    geom_point(alpha = 0.7) +                                      # Scatter plot with adjusted transparency
    labs( title = "Job count by Industry, Seniority and work type (Top 10, excluding 'Others')",
      x = "Industry Type", y = "Seniority") +                   # Add labels and title
    theme_minimal() +                                              
    coord_flip() })
  
  
  
  
  # Location Analysis Plot
  output$location_plot1 <- renderPlot({
    job_count_by_province <- data %>%
      count(Province)
    
    ggplot(job_count_by_province, aes(x = reorder(Province, n), y = n)) +
      geom_bar(stat = "identity", fill = "red") +
      coord_flip() +  # Flip for better readability
      labs(title = "Number of Job Listings by Province",
           x = "Province", y = "Number of Job Listings")
  })
  output$location_plot2 <- renderPlot({ggplot(data, aes(x = Province, y = Avg_Salary, fill = Province)) +
      geom_boxplot() +
      theme_minimal() + theme(legend.position = "none")+
      coord_flip() +
      labs(title = "Average Salary by Province", x = "Province", y = "Average Salary")})
  
}

# Run the app
shinyApp(ui, server)
