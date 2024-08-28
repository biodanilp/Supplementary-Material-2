#PAQUETES#
install.packages("tiff")
library(tiff)
install.packages("cowplot")
library(cowplot)
install.packages("gridExtra")
library(gridExtra)
file.choose()
install.packages("grid")
library(grid)
file.choose()
#CARGA DE IMAGENES PNG#
imagen_p1 <- readTIFF("C:\\Users\\usuario\\Desktop\\Article_AEM\\Supplementary_Material_3\\GLUCOSE\\Brazil_WB1.tiff")
imagen_p2 <- readTIFF("C:\\Users\\usuario\\Desktop\\Article_AEM\\Supplementary_Material_3\\GLUCOSE\\Ghana_H1.tiff")
imagen_p3 <- readTIFF("C:\\Users\\usuario\\Desktop\\Article_AEM\\Supplementary_Material_3\\FRUCTOSE\\Ecuador_P1.tiff")
imagen_p4 <- readTIFF("C:\\Users\\usuario\\Desktop\\Article_AEM\\Supplementary_Material_3\\FRUCTOSE\\Malaysia_WB1.tiff")

#DATA TRANSFORMATION#
grob_p1 <- rasterGrob(imagen_p1)
grob_p2 <- rasterGrob(imagen_p2)
grob_p3 <- rasterGrob(imagen_p3)
grob_p4 <- rasterGrob(imagen_p4)

# Combina las imágenes en una cuadrícula
# Mostrar la gráfica combinada
plot_grid(grob_p1, grob_p2, grob_p3, grob_p4, ncol = 2)
