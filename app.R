# app.R
library(shiny)
library(DT)
library(readxl)
library(writexl)
library(lubridate) # Für die wochentag() Funktion
library(dplyr) # Für die Datenmanipulation


ui <- fluidPage(
  # Titel der Anwendung
  titlePanel("Dienstplaner für medizinisches Personal"),

  # Layout mit Seitenleiste
  sidebarLayout(
    # Seitenleiste für Datumsauswahl und Export-Button
    # Breite der Seitenleiste auf 3 von 12 Spalten gesetzt
    sidebarPanel(
      width = 2, # <--- HIER wurde die Breite der Seitenleiste festgelegt
      h4("Datum auswählen"),
      # Kalender für Datumseingabe
      dateInput("selected_date",
        label = "Datum:",
        value = Sys.Date() + 1, # Standardwert ist der nächste Tag
        language = "de", # Sprache auf Deutsch setzen
        weekstart = 1
      ), # Woche startet am Montag

      br(), # Kleiner Abstand
      h4("Dienstplan exportieren"),
      # Button zum Exportieren des Dienstplans als Excel-Datei
      downloadButton("download_plan", "Dienstplan exportieren")
    ),

    # Hauptbereich für die Dienstplan-Tabelle und Mitarbeiter-Tabelle
    mainPanel(
      fluidRow(
        column(
          width = 8, # ca. 70%      # Dynamischer Titel für den Dienstplan
          h4(textOutput("dienstplan_title")),
          helpText("Klicken Sie in einFeld der Spalte 'Name', um den ausgewählten Mitarbeiter einzufügen. Sie können mehrere Namen in ein Feld einfügen."),
          # Tabelle zur Anzeige und Bearbeitung des Dienstplans
          DTOutput("dienstplan_table")
        ),
        # br(), # Kleiner Abstand
        # hr(), # Horizontale Linie zur Trennung
        # br(), # Kleiner Abstand
        column(
          width = 4, # ca. 30%
          h4("Mitarbeiterliste"),
          helpText(HTML("Klicken Sie auf eine Zeile zur Auswahl für die Planung.<br>Der Name bleibt in der Liste, um ihn mehrfach zu verwenden.")),
          # Tabelle zur Anzeige der Mitarbeiterdaten
          DTOutput("mitarbeiter_table")
        )
      )
    )
  )
)
server <- function(input, output, session) {
  # Reactive Value zur Speicherung des aktuell ausgewählten Mitarbeiters für die Planung
  selected_employee_for_plan <- reactiveVal(NULL)

  # Reactive Value zur Speicherung der Dienstplan-Daten (veränderbar)
  rv_dienstplan <- reactiveVal(NULL)

  # Reactive Expression, die alle Nachnamen aus dem Dienstplan extrahiert.
  # Wird verwendet, um Mitarbeiter in der Mitarbeiter-Tabelle gelb zu markieren.
  employees_in_dienstplan <- reactive({
    if (is.null(rv_dienstplan())) {
      return(character(0))
    }
    # Extrahiert Namen, teilt sie bei Kommas, entfernt Leerzeichen und holt einzigartige Namen
    all_names_raw <- unlist(strsplit(as.character(rv_dienstplan()$Name), ","))
    unique(trimws(all_names_raw[all_names_raw != "" & !is.na(all_names_raw)]))
  })


  mitarbeiter_data <- reactive({
    data <- data.frame(
      Name = c(c("Müller", "Meier", "Schulze", "Schmitt", "Mustermann", "Young", "Frazey", "Eberhart", "Brauchler", "Farrar", "Korchuk", "Pippin", "Hahs", "Welsh", "Masar", "Alarcon", "Potter", "Mckim", "Hutchens", "Pokrant", "Hounshell", "Mckenney", "Rickman", "Knapp", "Cieloha")),
      Vorname = c(c("Paul", "Martha", "Manuela", "Emanuella", "Heinz", "Riley", "Elizabeth", "Kimber", "Jacob", "Nicholas", "Beau", "Rachel", "Brian", "Brian", "Krystal", "Aria", "Brandon", "Jonathan", "Rebecca", "Ethan", "Cortnee", "Ann", "Mary", "Tyler", "Kara")),
      Status = c(c("MA", "Praktikant", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA")),
      stringsAsFactors = FALSE # Wichtig, um Strings als Strings zu behalten
    )
    # Sortieren der Daten nach der Spalte 'Name'
    data[order(data$Name), ]
  })

  # Rendern der Tabelle mit DT
  output$mitarbeiter_table <- renderDT({
    datatable(mitarbeiter_data(), options = list(
      pageLength = 5, # Zeigt standardmäßig 5 Zeilen an
      lengthMenu = c(5, 10, 15, 20), # Optionen für die Anzahl der angezeigten Zeilen
      searching = FALSE, # Deaktiviert die Suchfunktion
      paging = TRUE, # Aktiviert die Paginierung
      info = FALSE # Deaktiviert die Anzeige von Informationen zur Tabelle (z.B. "Showing 1 to X of Y entries")
    ))
  })
  # Initialisiert den Dienstplan basierend auf dem ausgewählten Datum
  observeEvent(input$selected_date,
    {
      req(input$selected_date)

      # Grundlegende Stationsnamen
      stations <- c(
        "Intensivstation1", "Intensivstation2",
        "IntensivRB", "NEF (+ offene Konsile)",
        "Schmerztherapie", "RTH", "ab 11:30 ABS",
        "Dienst", "Spätdienst 11:30",
        "Saal1 (Notfallsaal)", "Saal 2 + MRT",
        "Saal 3", "Saal 4", "Saal 5", "Saal 6",
        "amb.OP", "Anästhesiekoordinator",
        "EPU", "Sprechstunde",
        "FZA nach Dienst",
        "Rettungsdienst ab 16:00",
        "krank",
        "Dienstreise",
        "abwesend/Hosp.",
        "Urlaub"
      )

      # Fügt "OP-Besprechung" hinzu, wenn der ausgewählte Tag ein Donnerstag ist
      # wday() gibt den Wochentag zurück, 1=Montag, ..., 7=Sonntag. Donnerstag ist 4.
      if (wday(input$selected_date, week_start = 1) == 4) {
        stations <- c(stations, "15:00 OP-Besprechung")
      }

      # Erstellt den initialen Dienstplan-Datensatz
      initial_plan <- data.frame(
        Station = c(stations, rep("", 5)), # 5 leere Zeilen für manuelle Eingaben
        Name = rep("", length(stations) + 5),
        `Zeit ab` = rep("", length(stations) + 5),
        `Zeit bis` = rep("", length(stations) + 5),
        Kommentar = rep("", length(stations) + 5),
        stringsAsFactors = FALSE,
        check.names = FALSE # Verhindert, dass R Spaltennamen ändert
      )
      rv_dienstplan(initial_plan)
    },
    ignoreNULL = FALSE
  ) # Führt die Funktion auch beim ersten Laden der App aus

  # Rendert die Mitarbeiter-Tabelle
  output$mitarbeiter_table <- renderDT({
    req(mitarbeiter_data()) # Stellt sicher, dass Daten vorhanden sind

    # Startet die Tabelle ohne Styling
    dt_table <- datatable(mitarbeiter_data(),
      selection = "single", # Erlaubt die Auswahl einer einzelnen Zeile
      width = 50,
      options = list(
        dom = "tp", # Zeigt nur Tabelle und Paginierung, versteckt Suche/Info
        pageLength = 25, # Standard-Seitenlänge
        ordering = FALSE, # Deaktiviert Sortierung
        columnDefs = list(list(
          targets = 0:1,
          style = "text-align:left; padding:5px;"
        )),
        rowStyle = list(
          targets = 0:1,
          style = paste0("height: ", 500, "px; line-height: ", input$zeilenhoehe, "px;")
        )
      ),
      rownames = FALSE # Versteckt Zeilennummern
    ) |>
      formatStyle(
        0:2,
        0:2,
        textAlign = "center",
        paddingTop = "1px",
        paddingBottom = "1px"
      )

    # Wendet das Styling nur an, wenn es Mitarbeiter im Dienstplan gibt, die markiert werden sollen
    if (length(employees_in_dienstplan()) > 0) {
      dt_table <- dt_table %>%
        formatStyle(
          "Name",
          target = "row",
          backgroundColor = styleEqual(
            employees_in_dienstplan(),
            rep("yellow", length(employees_in_dienstplan()))
          )
        )
    }

    dt_table # Gibt die (ggf. gestylte) Tabelle zurück
  })

  # Beobachtet die Auswahl in der Mitarbeiter-Tabelle
  observeEvent(input$mitarbeiter_table_rows_selected, {
    req(input$mitarbeiter_table_rows_selected)
    selected_row_index <- input$mitarbeiter_table_rows_selected

    # Holt den Nachnamen des ausgewählten Mitarbeiters
    employee_name <- mitarbeiter_data()[selected_row_index, "Name"]
    selected_employee_for_plan(employee_name)

    # Zeigt eine temporäre Benachrichtigung an
    showNotification(paste0("Mitarbeiter '", employee_name, "' ausgewählt. Klicken Sie auf ein 'Name'-Feld in der Dienstplan-Tabelle."),
      type = "message", duration = 3
    )
  })

  # Rendert die Dienstplan-Tabelle
  output$dienstplan_table <- renderDT({
    req(rv_dienstplan())

    # Ermittelt die Anzahl der fixen Stationszeilen
    num_fixed_stations <- length(unique(rv_dienstplan()$Station[rv_dienstplan()$Station != ""]))

    datatable(rv_dienstplan(),
      # Ermöglicht Zellbearbeitung, wobei spezifische Spalten und Zeilen deaktiviert werden
      editable = list(
        target = "cell",
        # Deaktiviert die Spalte "Station" (Index 0) für die ersten 'num_fixed_stations' Zeilen
        disable = list(columns = 0, rows = 1:num_fixed_stations)
      ),
      options = list(
        dom = "t", # Zeigt nur Tabelle, versteckt Suche/Info/Paginierung
        paging = FALSE, # Deaktiviert Paginierung
        ordering = FALSE, # Deaktiviert Sortierung
        autoWidth = FALSE, # Wichtig, wenn Breiten manuell gesetzt werden
        columnDefs = list( # Definition der Spaltenbreiten (0-basierter Index)
          list(width = "30%", targets = 0), # Station
          list(width = "30%", targets = 1), # Name
          list(width = "10%", targets = 2), # Zeit ab
          list(width = "10%", targets = 3), # Zeit bis
          list(width = "20%", targets = 4) # Kommentar
        )
      ),
      rownames = FALSE # Versteckt Zeilennummern
    ) |>
      formatStyle(
        "Station",
        "Station",
        textAlign = "right",
        paddingTop = "1px",
        paddingBottom = "1px"
      )
  })

  # Beobachtet manuelle Bearbeitungen in der Dienstplan-Tabelle
  observeEvent(input$dienstplan_table_cell_edit, {
    info <- input$dienstplan_table_cell_edit
    current_dienstplan <- rv_dienstplan()

    # WICHTIGER FIX: info$col ist 0-basiert, R-Indizes sind 1-basiert.
    # Daher addieren wir 1 zu info$col, um die korrekte Spalte im R-Datenrahmen anzusprechen.
    current_dienstplan[info$row, info$col + 1] <- info$value
    rv_dienstplan(current_dienstplan)
  })

  # Beobachtet Klicks auf Zellen in der Dienstplan-Tabelle, um Namen einzufügen
  observeEvent(input$dienstplan_table_cell_clicked, {
    info <- input$dienstplan_table_cell_clicked

    # Überprüft, ob eine Zelle geklickt wurde und ob ein Mitarbeiter ausgewählt ist
    if (length(info$row) > 0 && length(info$col) > 0 && !is.null(selected_employee_for_plan())) {
      # Die Spalte 'Name' hat den Index 1 (0-basiert in JavaScript, also info$col == 1)
      # Wir können hier direkt den Spaltennamen 'Name' verwenden, um Indexierungsfehler zu vermeiden.
      if (names(rv_dienstplan())[info$col + 1] == "Name") { # info$col + 1 für den R-Index
        current_dienstplan <- rv_dienstplan()

        # Holt den aktuellen Inhalt der Zelle
        current_names <- current_dienstplan[info$row, "Name"]

        # Holt den Namen des auszuwählenden Mitarbeiters
        employee_to_add <- selected_employee_for_plan()

        # Fügt den Namen hinzu (durch Komma getrennt), wenn die Zelle bereits belegt ist
        if (nchar(current_names) > 0) {
          # Überprüft, ob der Name bereits in dieser Zelle ist, um Duplikate zu vermeiden
          existing_names_list <- trimws(unlist(strsplit(current_names, ",")))
          if (!employee_to_add %in% existing_names_list) {
            new_names <- paste(current_names, employee_to_add, sep = ", ")
          } else {
            new_names <- current_names # Name existiert bereits, nicht erneut hinzufügen
            showNotification(paste0("Mitarbeiter '", employee_to_add, "' ist bereits in dieser Zelle."),
              type = "warning", duration = 2
            )
          }
        } else {
          new_names <- employee_to_add
        }

        # Aktualisiert die Zelle im Dienstplan
        current_dienstplan[info$row, "Name"] <- new_names
        rv_dienstplan(current_dienstplan)
      }
    }
  })

  # Rendert den dynamischen Dienstplan-Titel
  output$dienstplan_title <- renderText({
    req(input$selected_date)
    paste0("Dienstplan für den ", format(input$selected_date, "%d.%m.%Y"))
  })

  # Download-Handler für den Excel-Export
  output$download_plan <- downloadHandler(
    filename = function() {
      req(input$selected_date)
      # Dateiname: Dienstplanung_YYYYMMDD.xlsx
      paste0("Dienstplanung_", format(input$selected_date, "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(rv_dienstplan())
      # Exportiert den aktuellen Dienstplan als Excel-Datei
      writexl::write_xlsx(rv_dienstplan(), path = file)
    }
  )
}

# Startet die Shiny-App
shinyApp(ui, server)
