ui <- semanticPage(
  tags$head(
    tags$link(rel="stylesheet", href="style.css", type="text/css" )
  ),
  setBackgroundColor("ghostwhite"),
  grid(
    grid_template(
      default = list(
        areas = rbind(
          c("title", "title"),
          c("controls", "map"),
          c("stats", "map" )
        ),
        cols_width = c("40%", "60%"),
        rows_height = c("10%", "30%", "70%")),
      mobile = list(
        areas = rbind(
          "title",
          "controls",
          "stats",
          "map"
        ),
        rows_height = c("1fr", "2fr", "2fr", "4fr"),
        cols_width = c("100%"))
    ),
    title = div(
      h1(class = "main header", icon("ship"), "Marine data dashboard"),
      div(class = "ui divider")
    ),
    controls = div(class = "ui teal raised segment", 
                   div(class = "ui teal ribbon label", h3("Select vessel")),
                   h4("Type:"),
                   dropdown_input("vessel_type",
                                  sort(unique(marine_data$ship_type)), 
                                  value = "Cargo"),
                   h4("Name:"),
                   dropdown_input("vessel_name",
                                  sort(unique(marine_data[ship_type == 'Cargo']$SHIPNAME)), 
                                  value = ". PRINCE OF WAVES")
                   ),
    stats = div(class = "ui raised segment",
                div(class = "ui teal ribbon label", h3("General info")),
                h3(" "),
                div(class = "ui two column grid container",
                    div(class = "column",
                        div(class = "ui icon small grey message",
                            icon("flag outline"),
                            div(class = "content", div(class = "header", textOutput("flag")))
                            ),
                        div(class = "ui icon small grey message",
                            icon("expand"),
                            div(class = "content", div(class = "header", textOutput("size")))
                            )
                        ),
                    div(class = "column",
                        div(class = "ui icon small grey message",
                            icon("globe"), 
                            div(class = "content", div(class = "header", textOutput("gps_data")))
                            ),
                        div(class = "ui icon small grey message",
                            icon("thumbtack"), 
                            div(class = "content", div(class = "header", textOutput("latest_port")))
                            )
                        )
                    ),
                h3(" "),
                div(class = "ui teal ribbon label", h3("Longest distance info")),
                h3(" "),
                div(class = "ui two column grid container",
                    div(class = "column",
                        div(class = "ui icon small teal message",
                            icon("exchange"), 
                            div(class = "content", div(class = "header", textOutput("longest_distance")))
                        ),
                        div(class = "ui icon small teal message",
                            icon("calendar alternate outline"),
                            div(class = "content", div(class = "header", textOutput("longest_distance_date")))
                        )
                    ),
                    div(class = "column",
                        div(class = "ui icon small teal message",
                            icon("star outline"),
                            div(class = "content", div(class = "header", textOutput("longest_distance_speed")))
                        ),
                        div(class = "ui icon small teal message",
                            icon("clock outline"), 
                            div(class = "content", div(class = "header", textOutput('longest_distance_time')))
                        )
                    )
                )
    ),
    
    map = div(class = "ui teal raised segment", 
              div(class = "ui teal ribbon label", h3("Longest distance map")),
              leafletOutput("mymap")
              )
  ),
  
  shiny.info::display(
    shiny::span(
      "Created by Ewa Szczęsna",
      tags$a(href = "https://www.linkedin.com/in/ewa-szcz%C4%99sna/", icon("linkedin")),
      tags$a(href = "https://github.com/ewaszczesna", icon("github"))
    ),
    position = "bottom right"
  )

) 
