library(bs4Dash)
library(gsheet)
library(shinyWidgets)
library(bslib)
library(collapse)
library(echarts4r)
library(waiter)
library(toastui)
library(dplyr)
library(anytime) # Digunakan untuk konversi tanggal/waktu
library(janitor) # Digunakan untuk membersihkan nama kolom

data_agenda_disposisi <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1jRKoTyefw0bMn2ob_Jwkr9Ha_wfws0zhTElhbxqecO8/edit")

colnames(data_agenda_disposisi) <- c("no","tanggal_naskah",  "nomor_naskah", "asal_naskah", "hal", 
                                     "isi_ringkas_surat", "tim_kerja_terkait", "link_zoom_meeting", 
                                     "tanggal_pelaksanaan", "jam", "keterangan_disposisi", "penerima_disposisi",
                                     "email_penerima_disposisi", "timkerja_penerima_disposisi", 
                                     "kehadiran_penerima_disposisi", "notulen_penerima_disposisi")

data_toastui_dplyr <- data_agenda_disposisi |>
  # Membersihkan nama kolom agar sesuai dengan standar R (huruf kecil, tanpa spasi)
  clean_names() |>
  
  # --- 2. Mengubah Tipe Data dan Memformat Kolom Waktu/Data Baru ---
  mutate(
    # Konversi kolom tanggal dan waktu menjadi tipe datetime
    # anytime() sangat fleksibel untuk menangani format yang berbeda
    tanggal_mulai = anytime(tanggal_pelaksanaan),
    tanggal_selesai = anytime(tanggal_pelaksanaan),
    
    # 3. Membuat Kolom Baru untuk Toast UI Calendar
    
    # id: Membuat ID unik berdasarkan nomor baris
    id = row_number() |> as.character(),
    
    # calendarId: Menggunakan 'timkerja_penerima_disposisi' untuk pengelompokan
    calendarId = timkerja_penerima_disposisi,
    
    # title: Menggunakan 'isi_ringkas_surat'
    title = isi_ringkas_surat,
    
    # start & end: Memformat waktu ke dalam string ISO 8601 (YYYY-MM-DDTHH:MM:SS)
    # Ini adalah format standar yang diterima oleh Toast UI Calendar
    start = format(tanggal_mulai, "%Y-%m-%dT%H:%M:%S"),
    end = format(tanggal_selesai, "%Y-%m-%dT%H:%M:%S"),
    
    # isAllDay: Atur ke FALSE (karena ada waktu spesifik)
    isAllDay = FALSE,
    
    # category: Atur ke 'time' (karena ada waktu spesifik)
    category = 'time',
    
    # location: Menggunakan 'link_zoom_meeting' (opsional)
    location = link_zoom_meeting
  ) |>
  
  # --- 4. Memilih dan Mengatur Ulang Urutan Kolom Akhir ---
  select(
    id,
    calendarId,
    title,
    start,
    end,
    isAllDay,
    category,
    location
    # Anda dapat menambahkan kolom lain yang dibutuhkan Toast UI di sini
  )

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
            pickerInput("input_timker","Pilih Tim Kerja", choices=unique(data_agenda_disposisi$timkerja_penerima_disposisi), options = list(`actions-box` = TRUE),multiple = T),
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
          ),
          h5("Jadwal Agenda Kegiatan", style="text-align: center;"),
          calendarOutput("kalender")
        ),
        tabItem(
          tabName = "tab2",
          bs4Card(width = 3,
                  title = "",
                  pickerInput("input_pegawai","Pilih Pegawai", choices=unique(data_agenda_disposisi$penerima_disposisi), options = list(`actions-box` = TRUE),multiple = T),
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
    fsubset(data_agenda_disposisi, timkerja_penerima_disposisi %in% data_filtered_input())
  })
  
  # --- GRAFIK KEHADIRAN (Interaktif & Warna Berbeda) ---
  output$kehadiranPlot <- renderEcharts4r({
    
    # Manipulasi Data dengan 'collapse'
    kehadiran_summary <- data_filtered() |>
      fgroup_by(timkerja_penerima_disposisi, kehadiran_penerima_disposisi) |>
      fsummarise(
        Jumlah = length(kehadiran_penerima_disposisi)
      )
    
    kehadiran_summary |>
      group_by(kehadiran_penerima_disposisi) |> # Grouping by Kehadiran to create the stack
      e_charts(timkerja_penerima_disposisi) |>
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
    fsubset(data_agenda_disposisi, penerima_disposisi %in% data_filtered_input_pegawai())
  })
  
  # --- GRAFIK KEHADIRAN (Interaktif & Warna Berbeda) ---
  output$kehadiranPlotPegawai <- renderEcharts4r({
    
    # Manipulasi Data dengan 'collapse'
    kehadiran_summary <- data_filtered_pegawai() |>
      fgroup_by(penerima_disposisi, kehadiran_penerima_disposisi) |>
      fsummarise(
        Jumlah = length(kehadiran_penerima_disposisi)
      )
    
    kehadiran_summary |>
      group_by(kehadiran_penerima_disposisi) |> # Grouping by Kehadiran to create the stack
      e_charts(penerima_disposisi) |>
      e_bar(Jumlah, stack = "stack", legend = TRUE) |>
     # e_title("Kehadiran Penerima Disposisi per Tim Kerja") |>
      e_tooltip(trigger = "axis") |>
      e_grid(left = '25%') |>
      e_flip_coords()
    
  })
  
  output$kalender <- renderCalendar({
    calendar(data_toastui_dplyr, navigation = TRUE, defaultDate = Sys.Date()) %>%
      cal_month_options(
        startDayOfWeek  = 1, 
        narrowWeekend = TRUE
      ) %>% 
      cal_props(cal_demo_props())
  })
}

shinyApp(ui, server)