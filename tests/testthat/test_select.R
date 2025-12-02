context("selectInput")

describe("updateSelectInput", {
  skip_on_cran()
  local_edition(3)

  .test_app <- function(initial_choices, initial_value, choices = NULL, value = NULL) {
    shiny::shinyApp(
      ui = semanticPage(
        shiny.semantic::selectInput("select", NULL, choices = initial_choices, selected = initial_value),
        shiny::actionButton("trigger", "Trigger")
      ),
      server = function(input, output, session) {
        shiny::observeEvent(input$trigger, {
          shiny.semantic::updateSelectInput(session, "select", choices = choices, selected = value)
        })
      }
    )
  }

  .get_label <- function(app) {
    app$get_js('document.querySelector(".dropdown > .text").textContent')
  }

  it("updates selection when choices are a named vector", {
    # Arrange
    value <- "b"
    choices <- c("my first item" = "a", "my second item" = "b")
    app <- init_driver(.test_app(
      initial_choices = choices,
      initial_value = NULL,
      value = value
    ))

    # Act
    app$click("trigger")
    label <- .get_label(app)

    # Assert
    expect_equal(value, app$get_value(input = "select"))
    expect_equal("my second item", label)
  })

  it("updates selection when choices are a named list", {
    # Arrange
    value <- "b"
    choices <- list("my first item" = "a", "my second item" = "b")
    app <- init_driver(.test_app(
      initial_choices = choices,
      initial_value = NULL,
      value = value
    ))

    # Act
    app$click("trigger")
    label <- .get_label(app)

    # Assert
    expect_equal(value, app$get_value(input = "select"))
    expect_equal("my second item", label)
  })

  it("updates choices and label when given a named vector", {
    # Arrange
    value <- "b"
    choices <- c("my first item" = "a", "my second item" = "b")
    app <- init_driver(.test_app(
      initial_choices = list(),
      initial_value = NULL,
      choices = choices,
      value = value
    ))

    # Act
    app$click("trigger")
    label <- .get_label(app)

    # Assert
    expect_equal(app$get_value(input = "select"), value)
    expect_equal(label, "my second item")
  })

  it("updates choices and label when given a named list", {
    # Arrange
    value <- "b"
    choices <- list("my first item" = "a", "my second item" = "b")
    app <- init_driver(.test_app(
      initial_choices = list(),
      initial_value = NULL,
      choices = choices,
      value = value
    ))

    # Act
    app$click("trigger")
    label <- .get_label(app)

    # Assert
    expect_equal(app$get_value(input = "select"), value)
    expect_equal(label, "my second item")
  })
})
