# TrabajoPD

## Realización de un programa de aprendizaje en juegos

En este trabajo vamos a implementar un código en Haskell que nos permita jugar a los juegos del **TIC-TAC-TOE** y al **Juego del OSO** contra la máquina.

Para empezar, la máquina tendrá un conocimiento limitado del juego, conociendo simplemente las reglas de este y como poder jugarlo. A continuación modificaremos la máquina para que vaya teniendo un aprendizaje de mejora en el juego, y sea cada vez mas dificil ganarle.

También implementaremos el sistema de dificultades una vez la máquina ya haya sido previamente entrenada y sea capaz de ganar partidas con sencillez.

Este sistema de dificultad implementará tres dificultades iniciales:

- **Facil**: Siendo este el modo sin entrenamiento de la maquina y solo conociendo las reglas del juego.
    
- **Dificil**: Una maquina ya entrenada con conocimiento suficiente del juego para obligarnos a mejorar nuestras estrategias si queremos vencerla.

El programa incluira una interfaz externa con una interfaz agradable y cómoda para el usuario, en la cual podrá empezar una partida nueva, continuar la última partida guardada, cargar partidas guardadas, acceder a unos ajustes de la aplicación y finalmente una opción para salir de la aplicación.

En pleno juego tendremos un menú de pausa que nos permitirá continuar la partida que estamos jugando; guardar el estado de la partida actual, esto almacenara en memoria la distribución actual del tablero y el grado de dificultad que tenga la máquina en el momento actual; cargar otras partidas almacenadas en memoria, modificar los ajustes de la aplicación y salir de la aplicación.
