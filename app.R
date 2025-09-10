# app.R

# Load necessary libraries
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  
  # Add custom CSS styling
  tags$head(
    tags$style(HTML("
      /* Body and background */
      body {
        background-color: #f0f2f5;
        font-family: 'Arial', sans-serif;
      }

      /* Main container styling for a card-like effect */
      .container-fluid {
        background-color: #fff;
        padding: 30px;
        border-radius: 15px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        margin-top: 20px;
        max-width: 1200px;
      }

      /* Sidebar styling */
      .well {
        background-color: #e9ecef;
        border-radius: 10px;
        padding: 20px;
        border: none;
      }

      /* Button styling */
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
        border-radius: 5px;
        font-weight: bold;
      }
      .btn-primary:hover {
        background-color: #0056b3;
        border-color: #004085;
      }

      /* Header styling */
      h3 {
        color: #343a40;
        border-bottom: 2px solid #007bff;
        padding-bottom: 5px;
        margin-bottom: 20px;
      }

      /* Data table styling */
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        padding: 0.2em 0.8em;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: #007bff !important;
        border-color: #007bff !important;
        color: #fff !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: #0056b3 !important;
        border-color: #0056b3 !important;
        color: #fff !important;
      }
    "))
  ),
  
  # Application title
  titlePanel("Personal Budget Tracker"),
  
  # Sidebar with input controls
  sidebarLayout(
    sidebarPanel(
      h3("Add New Transaction"),
      dateInput("date", "Date:", value = Sys.Date()),
      textInput("category", "Category:", placeholder = "e.g., Groceries, Salary"),
      numericInput("amount", "Amount:", value = 0, min = 0),
      selectInput("type", "Type:", choices = c("Expense", "Income")),
      actionButton("add_transaction", "Add Transaction", class = "btn-primary")
    ),
    
    # Main panel for displaying outputs with a new layout
    mainPanel(
      fluidRow(
        column(12,
               h3("Transaction Summary"),
               verbatimTextOutput("balance_summary")
        )
      ),
      fluidRow(
        column(12,
               h3("All Transactions"),
               DTOutput("transactions_table")
        )
      ),
      fluidRow(
        column(12,
               h3("Expenses by Category"),
               plotOutput("expenses_plot")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive value to store transactions
  transactions <- reactiveVal(data.frame(
    Date = as.Date(character()),
    Category = character(),
    Amount = numeric(),
    Type = character(),
    stringsAsFactors = FALSE
  ))
  
  # Observe event for adding a new transaction
  observeEvent(input$add_transaction, {
    # Corrected function name here:
    new_row <- data.frame(
      Date = input$date,
      Category = input$category,
      Amount = input$amount,
      Type = input$type,
      stringsAsFactors = FALSE
    )
    
    current_transactions <- transactions()
    updated_transactions <- rbind(current_transactions, new_row)
    transactions(updated_transactions)
    
    # Clear input fields after adding
    updateTextInput(getDefaultReactiveDomain(), "category", value = "")
    updateNumericInput(getDefaultReactiveDomain(), "amount", value = 0)
    updateDateInput(getDefaultReactiveDomain(), "date", value = Sys.Date())
  })
  
  # Render the transactions table
  output$transactions_table <- renderDT({
    datatable(
      transactions(),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  # Calculate and display balance summary
  output$balance_summary <- renderPrint({
    df <- transactions()
    if (nrow(df) == 0) {
      cat("No transactions yet.\n")
      cat("Current Balance: $0.00\n")
    } else {
      total_income <- df %>%
        filter(Type == "Income") %>%
        summarise(sum_amount = sum(Amount)) %>%
        pull(sum_amount)
      
      total_expenses <- df %>%
        filter(Type == "Expense") %>%
        summarise(sum_amount = sum(Amount)) %>%
        pull(sum_amount)
      
      current_balance <- total_income - total_expenses
      
      cat(sprintf("Total Income: $%.2f\n", total_income))
      cat(sprintf("Total Expenses: $%.2f\n", total_expenses))
      cat(sprintf("Current Balance: $%.2f\n", current_balance))
    }
  })
  
  # Render the expenses plot
  output$expenses_plot <- renderPlot({
    df <- transactions()
    if (nrow(df) > 0) {
      expenses_data <- df %>%
        filter(Type == "Expense") %>%
        group_by(Category) %>%
        summarise(TotalAmount = sum(Amount)) %>%
        arrange(desc(TotalAmount))
      
      if (nrow(expenses_data) > 0) {
        ggplot(expenses_data, aes(x = reorder(Category, -TotalAmount), y = TotalAmount, fill = Category)) +
          geom_bar(stat = "identity") +
          labs(title = "Expenses by Category", x = "Category", y = "Total Amount ($)") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No expenses to plot yet.", size = 6) +
          theme_void()
      }
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Add transactions to see the plot.", size = 6) +
        theme_void()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
