all_events_for_all_days <- get_data_for_all_days_for_a_combo(accessTokenu,
                                    matching_name = "Michel_Michael_8473",
                                    project_name = "brpa-dev")


# all_events_for_all_days |>
#   tidyr::pivot_longer(cols = -c(Day, name, Event_Start, Event_End, Pilot1,
#                                 Pilot1_title, Pilot1_status))

all_events_for_all_days <- all_events_for_all_days |>
  dplyr::relocate(
    Day, SME, Instructor, Aircraft,
    Pilot1, Pilot1_title, Pilot1_status,
    Event = name, Event_Start, Event_End,
    everything()
  )


library(openxlsx)

full_workbook <- createWorkbook()

addWorksheet(full_workbook, sheetName = "All_Data", gridLines = TRUE)
writeDataTable(full_workbook, sheet = "All_Data",
               x = all_events_for_all_days,
               colNames = TRUE)

# saveWorkbook(full_workbook, "data.xlsx", overwrite = TRUE)
