
# Directorios OneDrive ---------------------------------------------------------
### OneDrive ###################################################################
folder_usuario <- paste0("C:/Users/",
                         tolower(Sys.info()['user']))

### OneDrive (INE) #############################################################
folder_onedrive_oficina <- paste0(folder_usuario,"/OneDrive - Instituto Nacional de Estadisticas")

### OneDrive (casa) ############################################################
folder_onedrive_casa <- paste0(folder_usuario,"/OneDrive")

### Carpeta con bases de datos (INE) ###########################################
folder_data_gral_oficina <- paste0(folder_onedrive_oficina,"/2024/2406_Clases R Intermedio")

### Carpeta con bases de datos (casa) ##########################################
folder_data_gral_casa <- paste0(folder_onedrive_casa,"/Documentos/INE/2406_Clases R Intermedio")

### Carpeta con bases de datos (seleccionar) ###################################
folder_data_gral <- folder_data_gral_casa # reemplazar si trabajo desde la oficina

### Clases 1-2 #################################################################
folder_data1 <- paste0(folder_data_gral,"/data_clases1_2")
file_casen <- paste0(folder_data1,"/casen_2020_edit.feather")
file_demon <- paste0(folder_data1,"/demon_slayer.csv")
file_categorias <- paste0(folder_data1,"/categorias_imc.csv")

### Clases 3-4 #################################################################
folder_data2 <- paste0(folder_data_gral,"/data_clases3_4")

### Clases 5-6 #################################################################
folder_data3 <- paste0(folder_data_gral,"/data_clases5_6")
file_telefonos <- paste0(folder_data3,"/telefonos.csv")

### Clases 7-8 #################################################################
folder_data4 <- paste0(folder_data_gral,"/data_clases7_8")
