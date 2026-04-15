formal-puzzle
=============

La motivación de este proyecto es crear una app (web) donde
se alojen algunos puzzles de lógica similares a los que se
encuentran en los exámenes de la asignatura Lógica de 1er
semestre de Ciencias de la Computación de la UH.


Tabla de contenidos
===================

  * ¿Por qué?
  * Correrlo localmente
  * Mini tur del código
  * Licencia


¿Por qué?
=========

¿Por qué no?


Correrlo localmente
===================

Debes instalar `elm`, ya sea desde su web (https://elm-lang.org) o
desde tu administrador de paquetes favorito, asumiendo que esté
disponible.

Para generar el archivo `app.js` necesario para poder ver la web
corre lo siguiente en tu terminal:

   $ elm make src/Main.elm --output=app.js

Y listo, abre el archivo `index.html` y debería salirte una lista
de puzzles disponibles.


Mini tur del código
===================

El todo está implementado en el lenguaje Elm, que gracias a
su paradigma puramente funcional elimina un millar de errores
que ocurren meramente en tiempo de ejecución, con excepción
del archivo `index.html` que es lo que carga la app, y `app.css`
que le da estilo a la página.

Dentro del directorio `src/` se encuentran todos los archivos
de código útiles:

 * AllLevels.elm
    Todos los puzzles están aquí
 * AllRules.elm
    Las reglas de inferencia/equivalencias/implicaciones
 * Expr.elm
    Un pequeño analizador recursivo descendiente aka recursive
    descent parser para convertir las expresiones escritas en
    formato texto por el usuario a tipos mucho más faciles de
    manejar
 * Infer.elm
    Utilidad para reconocer cuándo una expresión es resultado
    de aplicar alguna transformación o regla de inferencia a
    partir de otras dadas
 * Level.elm
    La interfaz de un nivel, donde se resuelve cada puzzle
 * LevelTys.elm
    Tipos usados en `Level.elm`
 * Main.elm
    Punto de entrada de la app, delega las acciones a Level.elm
 * Match.elm
    Utilidad para, dadas dos expresiones A y B, en caso de B ser
    resultado de aplicar un reemplazo en A, reconocer si fue
    un reemplazo dado
 * MathML.elm
    Convierte expresiones y otras cosillas a MathML para que se
    vean bonito
 * TeaCommon.elm
    Cosas para evitar referencias circulares
 * Utils.elm
    Utilidades de carácter general


Licencia
========

Bajo los términos de la GNU General Public License (GPL)
versión 2

