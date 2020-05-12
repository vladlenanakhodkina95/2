Находкина Владлена Васильевна - для дневных данных за лето 2019 года, постройте регрессионную зависимость скорости сокотечения (переменная Flux) для деревьев с возрастом выше III

data  = read.csv ( " data.csv " )
сводка ( данные )

data_filtered_1  =  data [ data $ doy  >  243  &  data $ doy  <  335 ,]
data_filtered_2  =  data_filtered_1 [ data_filtered_1 $ hour  >  19  |  data_filtered_1 $ час  <  7 ,]
data_filtered_3  =  data_filtered_2 [ data_filtered_2 $ age_group_index  ==  " IV "  |  data_filtered_2 $ age_group_index  ==  " V " ,]
data_filtered_4  =  data_filtered_3 [ data_filtered_3 $ in_site_antrop_load  ==  " Высокий " ,]
data_filtered_4 = data_filtered_4 [, - c ( " id " , " Species " , " age_group_index " , " time " , " antrop_load " ,
                                           " in_site_antrop_load " )]
библиотека ( dplyr )

data_filtered_4 = select ( data_filtered_4 , - c ( " id " , " Species " , " age_group_index " , " time " , " antrop_load " ,
                                                   " in_site_antrop_load " ))


install.packages ( " ggcorrplot " )
корр  = кор ( data_filtered_4 , использование  =  " na.or.complete " ) ^ 2
# # corr = corr [corr> 0.49]
# install.packages ("ggcorrplot")
библиотека ( ggcorrplot )

ggcorrplot ( корр ,
            type  =  " lower " ,
            insig  =  " blank " ,
            lab  =  TRUE ,
            цифры  =  3
)
ggcorrplot ( корр ,
            tl.cex = 4
)
flux_corr = корр [, " поток " ]
flux_corr = flux_corr [ flux_corr > 0.1 ]
# cor.test (data $ flux, data $ t1)

формула8  =  поток  ~  u + rh
формула9  =  поток  ~  u + rh  +  u : rh

модель8  = лм ( данные = данные_фильтрованные_4 , формула8 )
модель9  = лм ( данные = данные_фильтрованные_4 , формула9 )

резюме ( модель8 )
резюме ( модель9 )