library(ggplot2)
library(tidyr)
ggplot(Database_TA_) +
  geom_point(aes(x = Strain, y = Stress, color =`Confining Pressure`))+
  labs(title = 'Data Triaxial CU',
       x = "Strain (%)", y = "Stress (kPa)")