# StrDiag: A library for manipulation and visualization of string diagrams

 
Estructura

Un diagrama es una representación de un morfismo en una 2-categoría.

Tiene 3 tipos básicos `Cat` , `Fc` y `Nt` para representar categorías (0-celdas), functores (1-celdas) y transformaciones naturales (2-celdas) respectivamente. 
También, tiene 3 tipos compuestos `Line`, `Band` y `Plane` para representar la composición de `Fc`, la composición horizontal de `Nt` y la composición vertical de `Band`. Cada composición se hace "concatenando" listas para no perder la información al componer. 
El tipo `Diagram` es el co-producto de los anteriores tipos.
Cada tipo es funtorial.

En *Cats/types.hs* se encuentra la implementación de estos tipos y las relaciones entre ellos. 

Los atributos gráficos de un diagrama  estan representados por el tipo `Atrib` (i.e. un diagrama listo para dibujar tendrá tipo `Diagram Atrib`). Cada tipo básico tiene atributos locales y cada tipo compuesto  atributos globales.

En *Cats/Atrib.hs* se encentra la descripción de los atributos (un nombre y un valor - en formato JSON- con todos los otros detalles) y los métodos para implementarlos `Det`modificarlos (`setCD`, `getCD` y `mapCD`)

En *Cats/Gen.hs* hay constructores genéricos para cada tipo y en *Lib.hs* se encuentra una pequeña "librería" con ejemplos.

Llamamos semántica a una función que transforma diagramas en valores. En *Cats/Semantic.hs* se encuentran las propiedades de una semántica y se implementan las etiquetas y mapas en Latex, `latexLabel` y `latexMap`.

Este proyecto plantea implementar tres semánticas.
 - La primera computa fórmulas en $\LaTex$.
 - La segunda computa grafos planos. 
 - la tercera diagramas de cuerdas. 