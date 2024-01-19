library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(e1071)
library(rpart)
library(psych)
library(corrplot)
library(caret)
library(pROC)
library(ROCR)
library(pROC)
library(pdp)
library(randomForest)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = "Exploration de données"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chargement de données", tabName = "data_upload", icon = icon("database")),
      menuItem("Analyses", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Modélisation", tabName = "modelling", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_upload",
              fluidRow(
                box(title = "Upload", status = "primary", solidHeader = TRUE,
                    fileInput("file", "Choisir un fichier CSV :"),
                    checkboxInput("header", "Le fichier a-t-il une ligne d'en-tête ?", TRUE),
                    actionButton("loadData", "Charger les données")
                ),
                box(title = "Variables", status = "primary", solidHeader = TRUE,
                    selectInput("x_variable", "Variable X :", choices = ""),
                    selectInput("y_variable", "Variable Y :", choices = "")
                ),
                box(title = "Prétraitement", status = "primary", solidHeader = TRUE,
                    selectInput("categorical_variables", "Sélectionnez les variables categorial et nominale :", multiple = TRUE, choices = NULL),
                    selectInput("column_to_convert", "Select Column to Convert", choices = NULL),
                    actionButton("convert_to_numeric", "Convert to Numeric"),
                    selectInput("training_variable", "Variable pour l'entraînement :", choices = ""),
                    selectInput("missing_columns", "Sélectionnez les variables avec valeurs manquantes :", multiple = TRUE, choices = NULL),
                    actionButton("removeMissingCols", "Supprimer"),
                    actionButton("removeMissingRows", "Supprimer NA"),
                    actionButton("refreshData", "Actualiser"),
                    numericInput("rows_to_delete", "Number of Rows to Delete", value = 0, min = 0),
                    numericInput("target_value", "Target Value to Delete", value = 4),
                    actionButton("delete_rows", "Supprimer les lignes")
                )
              )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Aperçu des données", status = "primary", solidHeader = TRUE,
                    style = "overflow-x: auto;", # Apply style here
                    DTOutput("table"),
                    width = 12
                ),
                box(title = "Statistiques descriptives unidimensionnelle", status = "primary", solidHeader = TRUE,
                    tabsetPanel(

                                 tabPanel("Histogramme", numericInput("binwidth_input", "Binwidth:", value = 0.2, min = 0.01, step = 0.01), plotlyOutput("histogram")),
                                 tabPanel("Box Plot", plotlyOutput("boxplot")),
                                 tabPanel("Density Plot", plotlyOutput("densityplot")),
                                 tabPanel("Extra", verbatimTextOutput("univariate_analysis")),
                                 tabPanel("Résume", verbatimTextOutput("summary"))

                    ),
                    width = 6 # Half width
                ),
                box(title = "Analyse bidimensionnelle", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("correlation plot",plotlyOutput("bivariate_analysis")),
                      tabPanel("Correlation Matrix", plotOutput("correlation_matrix_plot"))
                    ),
                    width = 6 # Half width
                ),
                box(title = "Label Analysis", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("Bar Plot", plotOutput(outputId = "label_analysis", height = 500, width = 600)),
                      tabPanel("Pie Chart", plotOutput("pie_chart",height = 500, width = 600))
                    ),
                    width = 6 # Half width
                )
              )
      ),
      
      
      # Modelling Tab
      tabItem(tabName = "modelling",
              fluidRow(
                box(title = "SVM", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                                 tabPanel("résume",verbatimTextOutput("model_results")),
                                 tabPanel("Evaluation", 
                                          plotOutput("confusion_matrix_plot"),
                                          plotOutput("roc_auc_curve_plot")
                                 ),
                                 tabPanel("PDPs", uiOutput("pdp_output"))
                               ),width = 6),
                box(title = "Random Forest", status = "primary", solidHeader = TRUE,
                      tabsetPanel(
                                 tabPanel("résume",verbatimTextOutput("model_results_RF")),
                                 tabPanel("Evaluation", 
                                          plotOutput("confusion_matrix_plot_RF"),
                                          plotOutput("roc_auc_curve_plot_RF")
                                 ),
                                 tabPanel("Feature Importances",
                                          plotOutput("feature_importance_plot")
                                 )
                               ), width = 6
                      )
                    ),
                    actionButton("train_model", "Entraîner le modèle")
                )
              )
      )
    )





server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)
  trained_model <- reactiveVal(NULL)
  test_set <- reactiveVal(NULL)
  target_variable <- reactive({input$training_variable})
  training_set <- reactiveVal()
  feature_names <- reactive(NULL)
  trained_model_RF <- reactiveVal(NULL)
  
  observe({
    if (!target_variable() %in% names(data())) {
      print(paste("The target variable", target_variable(), "is not in the dataset."))
    }
  })  
  
  observeEvent(input$file, {
    if (!is.null(input$file) && !identical(input$file, "")) {
      file_ext <- tools::file_ext(input$file$name)
      if (tolower(file_ext) == "csv") {
        dataInput <- read.csv(input$file$datapath, header = input$header)
        dataInput <- dataInput[,-1]
        last_column <- names(dataInput)[ncol(dataInput)]
        dataInput[[last_column]] <- as.factor(dataInput[[last_column]])
        data(dataInput)
        updateSelectInput(session, "x_variable", choices = names(dataInput))
        updateSelectInput(session, "y_variable", choices = names(dataInput))
        updateSelectInput(session, "training_variable", choices = names(dataInput),selected = last_column)
        updateSelectInput(session, "categorical_variables", choices = names(dataInput))
        updateSelectInput(session, "missing_columns", choices = names(dataInput)[colSums(is.na(dataInput)) > 0])
        showModal(modalDialog("Le fichier a été chargé avec succès.", easyClose = TRUE))
        default_training_variable <- "Class"
        updateSelectInput(session, "training_variable", selected = default_training_variable)
      } else {
        showModal(modalDialog("Veuillez charger un fichier CSV valide.", title = "Erreur de chargement", easyClose = TRUE))
      }
    }
  })
  
  observeEvent(input$train_model, {
    req(data())
    # Define the target variable
    #target_variable <- input$training_variable
    # Ensure the target variable is in the dataset
    if (!target_variable() %in% colnames(data())) {
      showModal(modalDialog("La variable cible sélectionnée n'est pas valide.", title = "Erreur", easyClose = TRUE))
      return()
    }
    
    set.seed(123) # for reproducibility
    partition <- createDataPartition(data()[[target_variable()]], p = 0.7, list = FALSE)
    training_set(data()[partition, ])
    test_set_temp <- data()[-partition, ]
    test_set(test_set_temp)
    updated_data <- test_set()
    updated_data[[target_variable()]] <- as.factor(updated_data[[target_variable()]])
    test_set(updated_data)
    
    if (!target_variable() %in% colnames(data())) {
      showModal(modalDialog("La variable cible sélectionnée n'est pas valide.", title = "Erreur", easyClose = TRUE))
      return()
    }
    svm_model <- svm(as.formula(paste(target_variable(), "~ .")), data = training_set(), probability = TRUE)
    test_predictions <- predict(svm_model, newdata = test_set(), probability = TRUE)
    confusion_matrix <- confusionMatrix(test_predictions, test_set()[[target_variable()]])
    precision <- confusion_matrix$byClass['Precision']
    recall <- confusion_matrix$byClass['Recall']
    f1_score <- confusion_matrix$byClass['F1']
    trained_model(list(model = svm_model, metrics = confusion_matrix, precision = precision, recall = recall, f1_score = f1_score))
    weights <- t(svm_model$coefs) %*% svm_model$SV
    
    rf_model <- randomForest(as.formula(paste(target_variable(), "~ .")), data = training_set(), ntree = 500)
    
    test_predictions_RF <- predict(rf_model, newdata = test_set())
    
    confusion_matrix_RF <- confusionMatrix(test_predictions_RF, test_set()[[target_variable()]])
    accuracy_RF <- confusion_matrix_RF$overall['Accuracy']
    precision_RF <- confusion_matrix_RF$byClass['Precision']
    recall_RF <- confusion_matrix_RF$byClass['Recall']
    F1_RF <- 2 * (precision_RF * recall_RF) / (precision_RF + recall_RF)
    
    # Calculate feature importances and plot them
    output$feature_importance_plot <- renderPlot({
      req(trained_model_RF())
      rf_model <- trained_model_RF()$model
      req(rf_model) # Ensure that rf_model is available
      importances <- importance(rf_model)
      if (is.matrix(importances) && !is.null(rownames(importances))) {
        importance_df <- data.frame(
          feature = rownames(importances),
          importance = importances[, 1], # Assuming we want the first importance metric
          stringsAsFactors = FALSE
        )
        importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ]
        ggplot(importance_df, aes(x = reorder(feature, importance), y = importance)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(title = "Feature Importances", x = "Features", y = "Importance") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        stop("importance() did not return a matrix with row names.")
      }
    })
    
    trained_model_RF(list(
      model = rf_model,
      metrics_RF = confusion_matrix_RF,
      accuracy_RF = accuracy_RF,
      precision_RF = precision_RF,
      recall_RF = recall_RF,
      F1_RF = F1_RF
    ))
    
  })
  
  output$model_results <- renderPrint({
    req(trained_model())
    metrics <- trained_model()$metrics
    precision <- trained_model()$precision
    recall <- trained_model()$recall
    f1_score <- trained_model()$f1_score
    cat("Confusion Matrix:\n")
    print(metrics$table)
    cat("\nAccuracy:", metrics$overall['Accuracy'], "\n")
    cat("Precision:", precision, "\n")
    cat("Recall:", recall, "\n")
    cat("F1 Score:", f1_score, "\n")
  })
  
  output$model_results_RF <- renderPrint({
    req(trained_model_RF())
    metrics_RF <- trained_model_RF()$metrics_RF
    accuracy_RF <- trained_model_RF()$accuracy_RF
    precision_RF <- trained_model_RF()$precision_RF
    recall_RF <- trained_model_RF()$recall_RF
    F1_RF <- trained_model_RF()$F1_RF
    cat("Confusion Matrix:\n")
    print(metrics_RF$table)
    cat("\nAccuracy:", accuracy_RF, "\n")
    cat("Precision:", precision_RF, "\n")
    cat("Recall:", recall_RF, "\n")
    cat("F1 Score:", F1_RF, "\n")
  })
  
  output$pdp_output <- renderUI({
    req(trained_model(), training_set())
    # Get the names of features for which to generate PDPs
    feature_names <- setdiff(names(training_set()), target_variable())
    # Create a list of plot outputs
    plot_output_list <- lapply(seq_along(feature_names), function(i) {
      plotlyOutput(outputId = paste0("pdp_plot_", i))
    })
    # Return a tagList of all plot outputs
    do.call(tagList, plot_output_list)
  })
  
  # Inside server function
  observe({
    req(trained_model(), training_set())
    feature_names <- setdiff(names(training_set()), target_variable())
    lapply(seq_along(feature_names), function(i) {
      local({
        feature_name <- feature_names[i]
        output[[paste0("pdp_plot_", i)]] <- renderPlotly({
          req(trained_model(), training_set())
          pdp_feature <- partial(trained_model()$model, pred.var = feature_name, train = training_set())
          plotly::ggplotly(ggplot2::autoplot(pdp_feature))
        })
      })
    })
  })
  
  output$confusion_matrix_plot <- renderPlot({
    req(trained_model())
    confusionMatrix <- trained_model()$metrics$table
    # Convert the confusion matrix to a tidy data frame
    confusion_df <- as.data.frame(confusionMatrix)
    colnames(confusion_df) <- c("Prediction", "Reference", "Freq")
    ggplot(confusion_df, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Freq/sum(Freq)*100)), vjust = 1) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  output$confusion_matrix_plot_RF <- renderPlot({
    req(trained_model_RF())
    # Get the confusion matrix from the trained random forest model
    confusionMatrix_RF <- trained_model_RF()$metrics_RF$table
    # Convert the confusion matrix to a tidy data frame
    confusion_df_RF <- as.data.frame(confusionMatrix_RF)
    colnames(confusion_df_RF) <- c("Prediction", "Reference", "Freq")
    # Create a fancy visualization of the confusion matrix using ggplot2
    ggplot(confusion_df_RF, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Freq/sum(Freq)*100)), vjust = 1) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Confusion Matrix for Random Forest Model", x = "Actual", y = "Predicted") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$roc_auc_curve_plot_RF <- renderPlot({
    req(trained_model_RF(), test_set())
    test_probabilities_RF <- predict(trained_model_RF()$model, newdata = test_set(), type = "prob")[,2]
    true_outcomes_RF <- as.numeric(test_set()[[target_variable()]]) - 1
    roc_obj_RF <- roc(response = true_outcomes_RF, predictor = test_probabilities_RF)
    roc_data_RF <- data.frame(
      specificity = 1 - roc_obj_RF$specificities,
      sensitivity = roc_obj_RF$sensitivities
    )
    roc_plot_RF <- ggroc(roc_obj_RF, colour = 'steelblue', size = 2) +
      geom_ribbon(aes(x = 1 - specificity, ymin = 0, ymax = sensitivity), 
                  fill = 'steelblue', alpha = 0.2) +
      annotate("text", x = 0.6, y = 0.2, label = paste0("AUC = ", round(auc(roc_obj_RF), 2)), 
               color = "red", size = 5) +
      ggtitle(paste0('ROC Curve for Random Forest Model (AUC = ', round(auc(roc_obj_RF), 2), ')')) +
      theme_minimal()
    print(roc_plot_RF)
  })
  
  output$roc_auc_curve_plot <- renderPlot({
    req(trained_model(), test_set())
    
    test_probabilities <- attr(predict(trained_model()$model, newdata = test_set(), probability = TRUE), "probabilities")[,2]
    true_outcomes <- as.numeric(test_set()[[target_variable()]]) - 1  # Assuming the positive class is coded as 2
    roc_obj <- roc(true_outcomes, test_probabilities)
    roc_df <- data.frame(
      sensitivity = roc_obj$sensitivities,
      specificity = roc_obj$specificities
    )
    roc_plot <- ggroc(roc_obj, colour = 'steelblue', size = 2) +
      geom_ribbon(data = roc_df, aes(x = 1 - specificity, ymin = 0, ymax = sensitivity), fill = 'steelblue', alpha = 0.2) +
      annotate("text", x = 0.6, y = 0.2, label = paste0("AUC = ", round(auc(roc_obj), 2)), color = "red", size = 5) +
      ggtitle(paste0('ROC Curve ', '(AUC = ', round(auc(roc_obj), 2), ')')) +
      theme_minimal()
    print(roc_plot)
  })
  0
  observeEvent(input$loadData, {
    if (!is.null(data())) {
      output$table <- renderDT({ datatable(data()) })
      output$summary <- renderPrint({ summary(data()) })
      # Update the choices for column_to_convert selectInput
      updateSelectInput(session, "column_to_convert", choices = names(data()))
    }
  })
  
  observeEvent(input$categorical_variables, {
    req(data())
    dataInput <- data()
    for (var in input$categorical_variables) {
      if (var %in% colnames(dataInput)) {
        dataInput[[var]] <- as.factor(dataInput[[var]])
      }
    }
    data(dataInput)
  })
  
  output$histogram <- renderPlotly({
    req(data(), input$x_variable)
    x_name <- input$x_variable
    binwidth <- input$binwidth_input
    ggplotly(
      ggplot(data(), aes(x = data()[[x_name]])) + 
        geom_histogram(binwidth = binwidth, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Histogram", x = x_name, y = "Frequency")
    )
  })
  
  output$boxplot <- renderPlotly({
    req(data(), input$x_variable)
    x_name <- input$x_variable
    ggplotly(
      ggplot(data(), aes(y = data()[[x_name]])) +
        geom_boxplot(fill = "green", color = "black", alpha = 0.7) +
        labs(title = "Box Plot", y = x_name)
    )
  })
  
  output$densityplot <- renderPlotly({
    req(data(), input$x_variable)
    x_name <- input$x_variable
    ggplotly(
      ggplot(data(), aes(x = data()[[x_name]])) + 
        geom_density(fill = "purple", color = "black", alpha = 0.7) +
        labs(title = "Density Plot", x = x_name, y = "Density")
    )
  })
  
  output$univariate_analysis <- renderPrint({
    req(data())
    var <- input$training_variable
    if (is.null(var) || var == "") {
      return("Veuillez sélectionner une variable pour l'analyse unidimensionnelle.")
    } else if (!var %in% colnames(data())) {
      return("La variable sélectionnée n'existe pas dans les données.")
    }
    result <- capture.output({
      cat("Analyse unidimensionnelle pour la variable :", var, "\n")
      cat("Nombre de valeurs manquantes :", sum(is.na(data()[[var]])), "\n")
      if (is.numeric(data()[[var]])) {
        cat("Moyenne :", mean(data()[[var]], na.rm = TRUE), "\n")
        cat("Écart-type :", sd(data()[[var]], na.rm = TRUE), "\n")
        cat("Valeurs uniques :", length(unique(na.omit(data()[[var]]))), "\n\n")
      } else if (is.factor(data()[[var]])) {
        cat("Nombre de catégories :", length(levels(data()[[var]])), "\n")
        cat("Fréquence des catégories :\n")
        print(table(data()[[var]]))
      } else {
        cat("Type de variable non pris en charge pour l'analyse unidimensionnelle.", "\n")
      }
    })
    
    result
  })
  output$bivariate_analysis <- renderPlotly({
    req(data(), input$x_variable, input$y_variable)
    x_variable_name <- input$x_variable
    y_variable_name <- input$y_variable
    # Create a scatter plot to visualize the correlation
    plot_ly(data(), x = ~data()[[x_variable_name]], y = ~data()[[y_variable_name]],
            type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = x_variable_name), yaxis = list(title = y_variable_name))
  })
  
  observeEvent(input$convert_to_numeric, {
    req(data(), input$column_to_convert)
    dataInput <- data()
    if (input$column_to_convert %in% colnames(dataInput)) {
      dataInput[[input$column_to_convert]] <- as.integer(dataInput[[input$column_to_convert]])
      data(dataInput)
      updateSelectInput(session, "column_to_convert", choices = names(dataInput))
      showModal(modalDialog("The column has been converted to numeric successfully.", easyClose = TRUE))
    } else {
      showModal(modalDialog("Please select a valid column to convert.", title = "Conversion Error", easyClose = TRUE))
    }
  })
  observeEvent(input$removeMissingCols, {
    req(data())
    if (!is.null(input$missing_columns) && length(input$missing_columns) > 0) {
      data(data()[, !names(data()) %in% input$missing_columns])
      updateSelectInput(session, "missing_columns", choices = names(data())[colSums(is.na(data())) > 0])
      output$table <- renderDT({ datatable(data()) })
      showModal(modalDialog("Les variables sélectionnées ont été supprimées.", easyClose = TRUE))
    } else {
      showModal(modalDialog("Veuillez sélectionner des variables à supprimer.", title = "Aucune sélection", easyClose = TRUE))
    }
  })
  # Add this observer to your server function
  observeEvent(input$removeMissingRows, {
    req(data())
    dataInput <- data()
    dataInput <- na.omit(dataInput)
    data(dataInput)
    
    # Update the select inputs to reflect the changes in the dataset
    updateSelectInput(session, "x_variable", choices = names(dataInput))
    updateSelectInput(session, "y_variable", choices = names(dataInput))
    updateSelectInput(session, "training_variable", choices = names(dataInput),selected = input$training_variable)
    updateSelectInput(session, "categorical_variables", choices = names(dataInput))
    updateSelectInput(session, "missing_columns", choices = names(dataInput)[colSums(is.na(dataInput)) > 0])
    updateSelectInput(session, "column_to_convert", choices = names(dataInput))
    
    showModal(modalDialog("Rows with missing values have been removed successfully.", easyClose = TRUE))
  })
  
  observeEvent(input$refreshData, {
    req(data())
    dataInput <- data()
    updateSelectInput(session, "x_variable", choices = names(dataInput))
    updateSelectInput(session, "y_variable", choices = names(dataInput))
    updateSelectInput(session, "training_variable", choices = names(dataInput),selected = input$training_variable)
    updateSelectInput(session, "missing_columns", choices = names(dataInput)[colSums(is.na(dataInput)) > 0])
    showModal(modalDialog("Les données ont été actualisées avec succès.", easyClose = TRUE))
  })
  output$correlation_matrix_plot <- renderPlot({
    req(data())
    # Exclude the first and last columns from the data
    data_excluded <- data()[-c(1, ncol(data()))]
    # Calculate the correlation matrix
    correlation_matrix <- cor(data_excluded, use = "pairwise.complete.obs")
    # Visualize the correlation matrix using a heatmap
    corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
  })
  
  output$label_analysis  <- renderPlot({
    ggplot(data(), aes(x=data()[,ncol(data())], fill=data()[,ncol(data())]))  + 
      geom_bar() +
      scale_y_continuous() +
      geom_text(aes(y = (after_stat(count)),label =  scales::percent((after_stat(count))/sum(after_stat(count)))),
                stat="count",vjust=-1) +       
      geom_text(stat='count', aes(label=after_stat(count)), vjust=3) +
      theme(legend.position="none") +
      labs(title = "Frequency of each category",
           x = 'Categorical variable',
           y = 'Frequency')
  })
  
  observeEvent(input$delete_rows, {
    req(data())
    dataInput <- data()
    rows_to_delete <- which(dataInput$Class == input$target_value)
    if (length(rows_to_delete) < input$rows_to_delete) {
      showModal(modalDialog("There are fewer rows available to delete than the number specified. All available rows will be deleted.", easyClose = TRUE))
      dataInput <- dataInput[-rows_to_delete, ]
    } else {
      dataInput <- dataInput[-rows_to_delete[1:input$rows_to_delete], ]
    }
    data(dataInput)
    updateSelectInput(session, "x_variable", choices = names(dataInput))
    updateSelectInput(session, "y_variable", choices = names(dataInput))
    updateSelectInput(session, "training_variable", choices = names(dataInput),selected = input$training_variable)
    updateSelectInput(session, "categorical_variables", choices = names(dataInput))
    updateSelectInput(session, "missing_columns", choices = names(dataInput)[colSums(is.na(dataInput)) > 0])
    updateSelectInput(session, "column_to_convert", choices = names(dataInput))
    showModal(modalDialog("Rows have been deleted successfully.", easyClose = TRUE))
  })
  
  output$pie_chart <- renderPlot({
    req(data())
    last_column <- names(data())[ncol(data())]
    ggplot(data(), aes(x = factor(1), fill = !!sym(last_column))) +
      geom_bar(width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Pie Chart for the label", fill = last_column)
  })
}
shinyApp(ui = ui, server = server)