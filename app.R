library(bs4Dash)
library(gsheet)
library(shinyWidgets)
library(bslib)
library(collapse)
library(echarts4r)
library(waiter)

data_agenda_disposisi <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1jRKoTyefw0bMn2ob_Jwkr9Ha_wfws0zhTElhbxqecO8/edit")
data_agenda_disposisi <- as.data.frame(data_agenda_disposisi)
ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  dashboardHeader(title = "Agenda & Disposisi"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenu",
      menuItem(
        text = "Tim Kerja",
        tabName = "tab1",
        icon = icon("van-shuttle")
      ),
      menuItem(
        text = "Individu",
        tabName = "tab2"
      )
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
      tabItems(
        tabItem(
          tabName = "tab1",
          bs4Card(width = 3,
            title = "",
            pickerInput("input_timker","Pilih Tim Kerja", choices=unique(data_agenda_disposisi$`TIMKERJA PENERIMA DISPOSISI`), options = list(`actions-box` = TRUE),multiple = T),
            input_task_button(
              id = "task_cari",
              label = "Cari Data",
              icon = icon("search"),
              # Membuat tombol terlihat menonjol
              class = "btn-primary", 
              width = "100%"
            )
          ),
          bs4Card(width = 12,
            title = "Kehadiran Penerima Disposisi per Tim Kerja",
            echarts4rOutput("kehadiranPlot")
          )
        ),
        tabItem(
          tabName = "tab2",
          bs4Card(width = 3,
                  title = "",
                  pickerInput("input_pegawai","Pilih Pegawai", choices=unique(data_agenda_disposisi$`PENERIMA DISPOSISI`), options = list(`actions-box` = TRUE),multiple = T),
                  input_task_button(
                    id = "task_cari_pegawai",
                    label = "Cari Data",
                    icon = icon("search"),
                    # Membuat tombol terlihat menonjol
                    class = "btn-primary", 
                    width = "100%"
                  )
          ),
          bs4Card(width = 12,
                  title = "Kehadiran Penerima Disposisi per Pegawai",
                  echarts4rOutput("kehadiranPlotPegawai")
          )
        )
    )
  )
)

server <- function(input, output) {
  
  # 1. Event Reactive: Menangkap input HANYA saat tombol "Cari Data" ditekan
  data_filtered_input <- eventReactive(input$task_cari, {
    # Nilai dari pickerInput saat tombol ditekan
    input$input_timker
  })
  
  # 2. Filter data reaktif: Menggunakan input yang telah dikunci oleh eventReactive
  data_filtered <- reactive({
    req(data_filtered_input()) # Pastikan input dikunci ada
    
    # Menggunakan fsubset() dari collapse
    fsubset(data_agenda_disposisi, `TIMKERJA PENERIMA DISPOSISI` %in% data_filtered_input())
  })
  
  # --- GRAFIK KEHADIRAN (Interaktif & Warna Berbeda) ---
  output$kehadiranPlot <- renderEcharts4r({
    
    # Manipulasi Data dengan 'collapse'
    kehadiran_summary <- data_filtered() |>
      fgroup_by(`TIMKERJA PENERIMA DISPOSISI`, `KEHADIRAN PENERIMA DISPOSISI`) |>
      fsummarise(
        Jumlah = length(`KEHADIRAN PENERIMA DISPOSISI`)
      )
    
    kehadiran_summary |>
      group_by(`KEHADIRAN PENERIMA DISPOSISI`) |> # Grouping by Kehadiran to create the stack
      e_charts(`TIMKERJA PENERIMA DISPOSISI`) |>
      e_bar(Jumlah, stack = "stack", legend = TRUE) |>
     # e_title("Kehadiran Penerima Disposisi per Tim Kerja") |>
      e_tooltip(trigger = "axis") |>
      e_grid(left = '25%') |>
      e_flip_coords()
    
  })
  
  ##PEGAWAI
  # 1. Event Reactive: Menangkap input HANYA saat tombol "Cari Data" ditekan
  data_filtered_input_pegawai <- eventReactive(input$task_cari_pegawai, {
    # Nilai dari pickerInput saat tombol ditekan
    input$input_pegawai
  })
  
  # 2. Filter data reaktif: Menggunakan input yang telah dikunci oleh eventReactive
  data_filtered_pegawai <- reactive({
    req(data_filtered_input_pegawai()) # Pastikan input dikunci ada
    
    # Menggunakan fsubset() dari collapse
    fsubset(data_agenda_disposisi, `PENERIMA DISPOSISI` %in% data_filtered_input_pegawai())
  })
  
  # --- GRAFIK KEHADIRAN (Interaktif & Warna Berbeda) ---
  output$kehadiranPlotPegawai <- renderEcharts4r({
    
    # Manipulasi Data dengan 'collapse'
    kehadiran_summary <- data_filtered_pegawai() |>
      fgroup_by(`PENERIMA DISPOSISI`, `KEHADIRAN PENERIMA DISPOSISI`) |>
      fsummarise(
        Jumlah = length(`KEHADIRAN PENERIMA DISPOSISI`)
      )
    
    kehadiran_summary |>
      group_by(`KEHADIRAN PENERIMA DISPOSISI`) |> # Grouping by Kehadiran to create the stack
      e_charts(`PENERIMA DISPOSISI`) |>
      e_bar(Jumlah, stack = "stack", legend = TRUE) |>
     # e_title("Kehadiran Penerima Disposisi per Tim Kerja") |>
      e_tooltip(trigger = "axis") |>
      e_grid(left = '25%') |>
      e_flip_coords()
    
  })
}

shinyApp(ui, server)