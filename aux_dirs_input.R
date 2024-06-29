
# Directorios OneDrive ---------------------------------------------------------
### OneDrive ###################################################################
folder_onedrive <- paste0("C:/Users/",
                          tolower(Sys.info()['user']),
                          "/OneDrive - Instituto Nacional de Estadisticas")

### Carpeta con bases de datos #################################################
folder_data_gral <- paste0(folder_onedrive,"/2024/2406_Clases R Intermedio")

### Clases 1-2 #################################################################
folder_data1 <- paste0(folder_data_gral,"/data_clases1_2")
file_casen <- paste0(folder_data1,"/casen_2020_edit.feather")
file_demon <- paste0(folder_data1,"/demon_slayer.csv")
file_categorias <- paste0(folder_data1,"/categorias_imc.csv")

### Clases 3-4 #################################################################
folder_data2 <- paste0(folder_data_gral,"/data_clases3_4")

### Clases 5-6 #################################################################
folder_data3 <- paste0(folder_data_gral,"/data_clases5_6")

### Clases 7-8 #################################################################
folder_data4 <- paste0(folder_data_gral,"/data_clases7_8")
