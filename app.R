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
library(bslib)
library(reactable)

data_agenda_disposisi <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1jRKoTyefw0bMn2ob_Jwkr9Ha_wfws0zhTElhbxqecO8/edit")

colnames(data_agenda_disposisi) <- c("no","tanggal_naskah",  "nomor_naskah", "asal_naskah", "hal", 
                                     "isi_ringkas_surat", "tim_kerja_terkait", "link_zoom_meeting", 
                                     "tanggal_mulai", "tanggal_selesai", "keterangan_disposisi", "penerima_disposisi",
                                     "email_penerima_disposisi", "timkerja_penerima_disposisi", 
                                     "kehadiran_penerima_disposisi", "notulen_penerima_disposisi", "keterangan")

data_toastui_dplyr <- data_agenda_disposisi |>
  clean_names() |>
  
  # --- 1. Bersihkan dan standarkan format datetime ---
  mutate(
    # Ubah spasi menjadi T jika ada format campuran
    tanggal_mulai = gsub(" ", "T", tanggal_mulai),
    tanggal_selesai = gsub(" ", "T", tanggal_selesai),
    
    # Tambahkan leading zero untuk jam satu digit
    tanggal_mulai = gsub("T(\\d):", "T0\\1:", tanggal_mulai),
    tanggal_selesai = gsub("T(\\d):", "T0\\1:", tanggal_selesai),
    
    # Parse ke datetime
    tanggal_mulai = anytime(tanggal_mulai),
    tanggal_selesai = anytime(tanggal_selesai)
  ) |>
  
  # Lanjutkan dengan kode asli...
  arrange(tanggal_mulai) |>
  mutate(
    id = row_number() |> as.character(),
    calendarId = as.character(timkerja_penerima_disposisi),
    title = substr(isi_ringkas_surat, 1, 50),
    start = format(tanggal_mulai, "%Y-%m-%dT%H:%M:%S"),
    end = format(tanggal_selesai, "%Y-%m-%dT%H:%M:%S"),
    isAllDay = FALSE,
    category = 'time',
    location = as.character(link_zoom_meeting),
    body = paste0("Disposisi ke: ", timkerja_penerima_disposisi)
  ) |>
  select(id, calendarId, title, start, end, isAllDay, category, location, body)

csvDownloadButton <- function(id, filename = "data.csv", label = "Unduh Data (CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}


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
      )#,
      # menuItem(
      #   text = "Individu",
      #   tabName = "tab2"
      # )
    )
  ),
  dashboardBody(
    style = "background-color: #ffffff;",
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
          tabsetPanel(
            tabPanel(
              "Absolut",
              card(full_screen = T,
                echarts4rOutput("kehadiranPlot")
              )
            ),
            tabPanel(
              "Persentase", 
              card(
                echarts4rOutput("kehadiran_plot_persen"), full_screen = T
              )
            ),
            tabPanel(
              "Tabel",
              card(
                layout_column_wrap(
                  csvDownloadButton("kehadiran_tabel", filename = "kehadiran_timker.csv"),
                  "",
                  "",
                  "",
                  "",
                  ""
                ),
                br(),
                reactableOutput("kehadiran_tabel"), full_screen = T
              )
            )
          ),
          h5("Jadwal Agenda Kegiatan", style="text-align: center;"),
          tabsetPanel(
            tabPanel(
              "Kalender",
              calendarOutput("kalender")
            ),
            tabPanel(
              "Tabel Jadwal",
              card(
                reactableOutput("tabel_jadwal")
              )
            )
          )
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
  # --- GRAFIK KEHADIRAN (Interaktif, Warna Kustom & Urut Disposisi Terbanyak) ---
  output$kehadiranPlot <- renderEcharts4r({
    
    # Manipulasi Data dengan 'collapse'
    kehadiran_summary <- data_filtered() |>
      fgroup_by(timkerja_penerima_disposisi, kehadiran_penerima_disposisi) |>
      fsummarise(
        Jumlah = length(kehadiran_penerima_disposisi) # Hitung jumlah mutlak
      ) |>
      
      # 1. Hitung Total Disposisi per Tim Kerja (Basis untuk Pengurutan)
      fgroup_by(timkerja_penerima_disposisi) |> 
      fmutate(
        Total_Disposisi = fsum(Jumlah) 
      ) |>
      fungroup()
    
    # Definisi urutan kategori Kehadiran dan warna kustom (sesuai gambar)
    KEHADIRAN_LEVELS <- c("HADIR", "TIDAK HADIR", "TANPA KETERANGAN")
    CUSTOM_COLORS <- c(
      "HADIR" = "#008000",            # Biru Keunguan
      "TIDAK HADIR" = "#FFB300",      # Hijau Muda
      "TANPA KETERANGAN" = "#B22222"  # Oranye
    )
    # Pilih warna yang relevan berdasarkan data yang difilter
    plot_colors <- CUSTOM_COLORS[names(CUSTOM_COLORS) %in% unique(kehadiran_summary$kehadiran_penerima_disposisi)]
    
    kehadiran_summary |>
      # Pastikan faktor kehadiran diurutkan untuk legenda yang konsisten
      roworder(Total_Disposisi) |>
      group_by(kehadiran_penerima_disposisi) |> 
      e_charts(timkerja_penerima_disposisi) |>
      
      e_bar(Jumlah, stack = "stack", legend = TRUE) |>
      
      # Menerapkan Warna Kustom
      e_color(color = unname(plot_colors)) |>
      e_legend(
        bottom = 0, # Posisikan legend di bagian paling bawah
        orient = "horizontal", # Atur orientasi horizontal (default)
        padding = c(0, 0, 0, 0) # Atur padding jika perlu
      ) |>
      e_title("Status Tindak Lanjut Disposisi per Tim Kerja") |>
      e_tooltip(trigger = "axis") |>
      e_grid(left = '25%') |>
      e_flip_coords() |>
      e_theme("westeros")
  })
  
  # --- GRAFIK KEHADIRAN (Interaktif & Warna Berbeda) ---
  output$kehadiran_plot_persen <- renderEcharts4r({
    
    # Manipulasi Data dengan 'collapse':
    # 1. Hitung jumlah mutlak per kombinasi Tim Kerja dan Kehadiran
    kehadiran_summary <- data_filtered() |>
      fgroup_by(timkerja_penerima_disposisi, kehadiran_penerima_disposisi) |>
      fsummarise(
        Jumlah = length(kehadiran_penerima_disposisi)
      ) |>
      
      # 2. Hitung Total dan Persentase
      fgroup_by(timkerja_penerima_disposisi) |> # Kelompokkan hanya berdasarkan Tim Kerja
      fmutate(
        Total = fsum(Jumlah), # Hitung Total per Tim Kerja
        Persentase = (Jumlah / Total) * 100 # Hitung Persentase
      ) |>
      fungroup() # Hapus pengelompokan
    
    # Definisi urutan kategori dan warna kustom (sesuai gambar)
    # Catatan: Urutan ini penting! Pastikan urutan KEHADIRAN di data sesuai dengan urutan warna di e_color
    KEHADIRAN_LEVELS <- c("HADIR", "TIDAK HADIR", "TANPA KETERANGAN")
    CUSTOM_COLORS <- c(
      "HADIR" = "#008000",            # Biru Keunguan
      "TIDAK HADIR" = "#FFB300",      # Hijau Muda
      "TANPA KETERANGAN" = "#B22222"  # Oranye
    )
    
    # Pastikan kategori hadir di data, jika tidak, subset warna
    plot_colors <- CUSTOM_COLORS[names(CUSTOM_COLORS) %in% unique(kehadiran_summary$kehadiran_penerima_disposisi)]
    
    # Visualisasi menggunakan Persentase
    kehadiran_summary |>
      roworder(Persentase) |>
      group_by(kehadiran_penerima_disposisi) |> # Grouping by Kehadiran to create the stack
      e_charts(timkerja_penerima_disposisi) |>
      e_bar(Persentase, stack = "stack", legend = TRUE) |> # Menggunakan Persentase
      e_title("Persentase Tindak Lanjut Disposisi per Tim Kerja") |> # Judul Diperbarui
      # Menerapkan Warna Kustom
      e_color(
        color = unname(plot_colors)
      ) |>
      e_tooltip(trigger = "axis") |>
      e_grid(left = '25%') |>
      e_flip_coords() |>
      e_legend(
        bottom = 0, # Posisikan legend di bagian paling bawah
        orient = "horizontal", # Atur orientasi horizontal (default)
        padding = c(0, 0, 0, 0) # Atur padding jika perlu
      ) |>
      e_theme("westeros") # Tambahkan tema untuk konsistensi
  })
  
  output$kehadiran_tabel <- renderReactable({
    # Manipulasi Data dengan 'collapse'
    kehadiran_summary <- data_filtered() |>
      fgroup_by(timkerja_penerima_disposisi, kehadiran_penerima_disposisi) |>
      fsummarise(
        Jumlah = length(kehadiran_penerima_disposisi)
      )
    
   reactable(
     pivot(kehadiran_summary, ids = "timkerja_penerima_disposisi", values = "Jumlah", 
           names = "kehadiran_penerima_disposisi", how = "wider", fill = 0) |>
       frename(`TIM KERJA` = timkerja_penerima_disposisi) |>
       fgroup_by(`TIM KERJA`) |>
       fmutate(`TOTAL DISPOSISI` = sum(HADIR, `TANPA KETERANGAN`, `TIDAK HADIR`)) |>
       colorder(`TIM KERJA`,`TOTAL DISPOSISI`, HADIR, `TANPA KETERANGAN`, `TIDAK HADIR`)
   )
    
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
  
  output$tabel_jadwal <- renderReactable({
    data_agenda_disposisi |>
      fselect(tanggal_naskah, nomor_naskah, asal_naskah, hal, link_zoom_meeting, tanggal_mulai, tanggal_selesai, timkerja_penerima_disposisi, kehadiran_penerima_disposisi, notulen_penerima_disposisi)
    
    reactable(
      data_agenda_disposisi |>
        # --- 1. Bersihkan dan standarkan format datetime ---
        mutate(
          # Ubah spasi menjadi T jika ada format campuran
          tanggal_mulai = gsub(" ", "T", tanggal_mulai),
          tanggal_selesai = gsub(" ", "T", tanggal_selesai),
          
          # Tambahkan leading zero untuk jam satu digit
          tanggal_mulai = gsub("T(\\d):", "T0\\1:", tanggal_mulai),
          tanggal_selesai = gsub("T(\\d):", "T0\\1:", tanggal_selesai),
          
          # Parse ke datetime
          tanggal_mulai = anytime(tanggal_mulai),
          tanggal_selesai = anytime(tanggal_selesai)
        ) |>
        fmutate(
          tanggal_mulai = format(tanggal_mulai, "%Y-%m-%d %H:%M:%S"),
          tanggal_selesai = format(tanggal_selesai, "%Y-%m-%d %H:%M:%S")
        ) |>
        fselect(tanggal_naskah, nomor_naskah, asal_naskah, hal, link_zoom_meeting, tanggal_mulai, tanggal_selesai, timkerja_penerima_disposisi, kehadiran_penerima_disposisi, notulen_penerima_disposisi) |>
        frename(`Tanggal Naskah` = tanggal_naskah,
                `Nomor Naskah` = nomor_naskah,
                `Asal Naskah` = asal_naskah,
                 Hal = hal,
                `Link Zoom Meeting` = link_zoom_meeting,
                `Tanggal Mulai` = tanggal_mulai,
                `Tanggal Selesai` = tanggal_selesai,
                `Tim Kerja Penerima Disposisi` = timkerja_penerima_disposisi,
                `Status Kehadiran` = kehadiran_penerima_disposisi,
                Notulen = notulen_penerima_disposisi), 
      filterable = TRUE, 
      minRows = 10
      )
  })
}

shinyApp(ui, server)