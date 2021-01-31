# TrabajoPD

## Realización de un programa de aprendizaje en juegos

En este trabajo vamos a implementar un código en Haskell que nos permita jugar a los juegos del **3 en raya** y al **juego del OSO** contra la máquina.

Para empezar, la máquina tendrá un conocimiento limitado del juego, conociendo simplemente las reglas de este y como poder jugarlo. A continuación modificaremos la máquina para que vaya teniendo un aprendizaje de mejora en el juego, y sea cada vez mas dificil ganarle.

También implementaremos el sistema de dificultades una vez la máquina ya haya sido previamente entrenada y sea capaz de ganar partidas con sencillez.

Este sistema de dificultad implementará tres dificultades iniciales:

- **Facil**: Siendo este el modo sin entrenamiento de la maquina y solo conociendo las reglas del juego.
    
- Normal: Un estado de la maquina en el que ya haya tenido conocimiento y aprendizaje del transcurso del juego y sea capaz de ganarnos alguna partida inicial.
    
- Dificil: Una maquina ya entrenada con conocimiento suficiente del juego para obligarnos a mejorar nuestras estrategias si queremos vencerla.