# StrDiag: A library for manipulation and visualization of string diagrams

 
Estructura

Cats: abstracción de 2-categoría.

Tiene 3 tipos básicos `Cat` , `Fc` y `Nt` para representar categorías (0-celdas), functores (1-celdas) y transformaciones naturales (2-celdas) respectivamente. 
También, tiene 3 tipos compuestos `Line`, `Band` y `Diag` para representar la composición de `Fc`, la composición horizontal de `Nt` y la composición vertical de `Band`.

Cada tipo básico tiene atributos (representados por el tipo `Atrib`) y la composición se hace "concatenando" listas para no perder la información al componer. El tipo `Diag` también tiene un espacio para guardar atributos globales.

En *types.hs* se encuentra la implementación de estos tipos y las relaciones entre ellos. 

En *Atrib.hs* se encentra la descripción de los atributos (un nombre y un valor - en formato JSON- con todos los otros detalles) y los métodos para implementarlos `Det`modificarlos (`setCD`, `getCD` y `mapCD`)

En *Gen.hs* hay constructores genéricos para cada tipo (y sus identidades) y en *Lib.hs* se encuentra una pequeña "librería" con ejemplos.

Una semántica consta de un método para obtener información a partir de los atributos `loadAtr` y varios métodos para computar información compuesta (uno por cada tipo).

Este proyecto plantea implementar tres semánticas.
 - La primera computa fórmulas en $\LaTex$.
 - La segunda computa grafos planos. 
 - la tercera diagramas de cuerdas. 