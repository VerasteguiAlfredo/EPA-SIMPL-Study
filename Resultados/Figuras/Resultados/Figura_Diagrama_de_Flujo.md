# Diagrama de flujo del estudio

**Figura. Diagrama de flujo del estudio**

```mermaid
%%{init: {'theme': 'base', 'themeVariables': {'fontFamily': 'Arial, Helvetica, Liberation Sans, sans-serif', 'fontSize': '14px', 'primaryColor': '#ffffff', 'primaryBorderColor': '#000000', 'primaryTextColor': '#000000', 'lineColor': '#000000'}, 'flowchart': {'curve': 'linear', 'nodeSpacing': 45, 'rankSpacing': 50, 'padding': 12, 'wrappingWidth': 420, 'htmlLabels': true}}}%%
flowchart TB
    A["<b>Formularios de evaluación recibidos</b><br/>Evaluaciones de EPAs basadas en SIMPL, abril–agosto de 2026<br/>n = 293"]
    X1["<b>Excluidos</b><br/>Formularios incompletos, n = 41"]
    B["<b>Formularios de evaluación completos</b><br/>n = 252<br/>Evaluaciones del adscrito, n = 171<br/>Autoevaluaciones de residentes, n = 81<br/>159 casos quirúrgicos<br/>35 residentes (R1 a R5) · 3 cirujanos adscritos"]
    C1["<b>Desenlaces calificados por el adscrito</b><br/>n = 171 evaluaciones del adscrito<br/>35 residentes<br/><i>Tablas 3–4; Figuras 1–3, 5</i>"]
    C2["<b>Concordancia autoevaluación vs adscrito</b><br/>n = 80 casos pareados<br/>35 residentes<br/><i>Tabla 5; Figura 4</i>"]
    C3["<b>Desenlaces de implementación</b><br/>n = 252 formularios<br/>3 cirujanos adscritos · 35 residentes<br/><i>Tabla 6; Figura 6</i>"]
    X2["<b>Casos sin par válido</b><br/>n = 79<br/>Solo evaluación del adscrito, n = 65<br/>Solo autoevaluación, n = 1<br/>Dos evaluaciones del adscrito, n = 13<br/>"]
    S1["<b>Análisis de sensibilidad</b><br/>Excluyendo 17 casos marcados<br/>n = 141 evaluaciones del adscrito<br/><i>Tabla S5</i>"]
    S2["<b>Análisis de sensibilidad</b><br/>Excluyendo pares marcados, n = 4<br/>n = 76 casos pareados<br/><i>Tabla S6</i>"]

    A --> B
    A -.-> X1
    B --> C1
    B --> C2
    B --> C3
    C2 -.-> X2
    C1 -.-> S1
    C2 -.-> S2

    classDef main fill:#ffffff,stroke:#000000,stroke-width:1.2px,color:#000000,font-family:Arial\,Helvetica\,Liberation Sans\,sans-serif;
    classDef excl fill:#f4f4f4,stroke:#555555,stroke-width:1px,stroke-dasharray:4 3,color:#000000,font-family:Arial\,Helvetica\,Liberation Sans\,sans-serif;
    classDef sens fill:#ffffff,stroke:#555555,stroke-width:1px,stroke-dasharray:4 3,color:#000000,font-family:Arial\,Helvetica\,Liberation Sans\,sans-serif;
    class A,B,C1,C2,C3 main;
    class X1,X2 excl;
    class S1,S2 sens;
    linkStyle default stroke:#000000,stroke-width:1.2px;
```

Flujo de formularios de evaluación, casos quirúrgicos y residentes en el estudio (abril–agosto de 2026). Los recuadros continuos muestran las muestras analíticas y las tablas y figuras que las usan; los recuadros discontinuos muestran exclusiones y análisis de sensibilidad. Los casos marcados se identificaron durante la verificación de datos (dos formularios del adscrito en un caso, formularios de distintos residentes bajo un mismo número de caso, discrepancia de procedimiento, abordaje o fecha entre formularios pareados, o codificación inconsistente del evaluador). Abreviaturas: EPA, actividad profesional confiable; R1–R5, residente de primer a quinto año; SIMPL, System for Improving and Measuring Procedural Learning.

## Conteos usados en el diagrama

|Paso                                                            |   n|
|:---------------------------------------------------------------|---:|
|Formularios recibidos                                           | 293|
|Excluidos: formularios incompletos                              |  41|
|Formularios completos                                           | 252|
|Evaluaciones del adscrito                                       | 171|
|Autoevaluaciones de residentes                                  |  81|
|Casos quirúrgicos                                               | 159|
|Residentes                                                      |  35|
|Cirujanos adscritos                                             |   3|
|Casos pareados (1 adscrito + 1 autoevaluación, mismo residente) |  80|
|Casos con solo evaluación del adscrito                          |  65|
|Casos con solo autoevaluación                                   |   1|
|Casos con dos evaluaciones del adscrito                         |  13|
|Otros casos sin par                                             |   0|
|Casos marcados                                                  |  17|
|Evaluaciones del adscrito tras excluir casos marcados           | 141|
|Casos pareados tras excluir casos marcados                      |  76|

_Generado el 2026-10-17 19:24 a partir de EPA_SIMPL_Database.xlsx. Los formularios incompletos (n = 41) se capturan manualmente en EPA_Flowchart.R._
