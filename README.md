# Análisis forense de las elecciones argentina de 2021

Cómo citar: 

*Miño, J.A. 2022. Análisis forense de las elecciones argentinas de 2021 . Repositorio de GitHub, URL: https://github.com/andresminio/forenseelectoral2021*

En este proyecto se utiliza el análisis forense electoral (election forensics) para identificar posibles irregularidades en las elecciones de Diputados Nacionales de Argentina en 2021, a partir de los datos del Escrutinio Provisorio. 
En el repositorio encontraras 3 códigos para preparar la información y replicar el análisis: 
1. **Electores**: carga datos de los electores registrados en 2021, reconstruye el ID de mesa utilizado por INDRA (la empresa encargada de la realización del escrutinio), calcula la cantidad de electores por mesa y hace un merge de las mesas de los 24 distritos electores. Al finalizar, se escribe el archivo "Electores por Mesa PASO 2021.csv", que contiene 101459 observaciones, correspondientes al total de mesas electorales en 2021.  
2.	**Resultados**: carga los resultados del escrutinio provisorio de las elecciones generales 2021, limpian y transforman las variables relevantes, ordena el data set a nivel de mesa e incorpora la información de los electores por mesa, obtenida en el paso anterior. Esto aporta dos datos relevantes, la cantidad de electores por mesa y el detalle de las mesas que no fueron cargadas en el escrutinio provisorio. Por último se escribe el archivo “generales21”, que contiene 101459 observaciones e información sobre el voto por cada agrupación política en el total de mesas para la categoría de Diputados Nacionales. 
3.	**Análisis**: después de la preparación del dataset, se evalúa la integridad del escrutinio provisorio. Se observa la cobertura del escrutinio provisorio y los patrones de participación electoral y voto por las principales agrupaciones políticas nacionales (Juntos por el Cambio, Frente de Todos, Liberales y FIT) y subnacionales (Hacemos por Córdoba-CBA, Frente Amplio Progresista-SFE, Movimiento Popular Neuquino-NQN, Frente Renovador de la Concordia Social-MIS, Juntos Somos Rio Negro-RN). En el notebook encontraras más detalles sobre este tipo de análisis, los indicadores utilizados y los resultados alcanzados. 
Este proyecto es realizado en el marco del curso de Metodología de los Estudios Políticos de la Maestría en Análisis Político de la Universidad Nacional de Tres de Febrero y tiene propósitos pedagógicos. Se utilizan datos provisorios del escrutinio y de electores por mesa, que pueden haber sufrido modificaciones. Para tener una idea más precisa del proceso electoral, revise el escrutinio definitivo en https://www.padron.gob.ar/publica/ 

Para ver una explicación más detallada del proyecto y sus resultados, ver https://rpubs.com/andresminio/forenseelectoral21

