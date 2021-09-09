-- Módulos importados
-- ------------------------------------------------------------------------
import Utiles
import System.Directory
import Data.Array
import Data.Char
import System.IO
-- ------------------------------------------------------------------------

-- Inicializamos las variables que necesitaremos para este juego.
cj1 = 'X'
cj2 = 'O'
jugadorVsM = 'X'
maquina = 'O'
numeroDeFilas = 3
listaAleatoria = [(0,2),(2,2),(0,1),(1,2),(0,0),(1,1),(1,0),(2,1),(2,0)]

prueba :: Cuadricula
prueba = array ((0,0),(2,2)) [((0,0),'X'),((0,1),'O'),((0,2),'X'),((1,0),' '),((1,1),'O'),((1,2),' '),((2,0),'O'),((2,1),' '),((2,2),'X')]


-- Función main
main :: IO()
main = do
    modo <- leeDigito "Escoja. Modo 1 jugador o 2 jugadores. Para escoger simplemente ponga el número (1 ó 2): "
    if modo == 1
        then iniciaJuego2
        else iniciaJuego --Nótese la ironía en sus respectivos nombres.

-- También definimos el tipo Cuadricula que va a ser la referencia para el correcto desarrollo del juego. El tipo 'a' de la
-- Matriz lo vamos a sustituir por Char ya que vamos a usar 'X' y 'O' para representar los círculos y cruces del juego.
type Cuadricula = Matriz Char

-- Creamos la cuadrícula vacía para comenzar un nuevo juego.
inicial :: Cuadricula
inicial = matrizUnitaria 3 ' '

-- Vamos a crear una función para conocer si un juego se encuentra en el estado final o no.
-- ------------------------------------------------------------------------
finalizado :: Cuadricula -> Bool
finalizado c = (llena c) || (hay3EnRaya c)

-- Función para saber si la Cuadrícula del juego que nos pasan ya está llena.
llena :: Cuadricula -> Bool
llena c = (length es) == suma
    where es = elems c
          xs = [x | x<-es,x=='X']
          os = [o | o<-es,o=='O']
          suma = (length xs)+(length os)

-- Función para saber si hay un jugador con 3 en raya.
hay3EnRaya :: Cuadricula -> Bool
hay3EnRaya c = or [if x==3 then True else False | x<-lss]
    where fs = listaFilas c
          cs = listaColumnas c
          ds = diagonalesMatriz c
          fsx = [x | x<-fs,x=="XXX"]
          csx = [x | x<-cs,x=="XXX"]
          dsx = [x | x<-ds,x=="XXX"]
          fso = [x | x<-fs,x=="OOO"]
          cso = [x | x<-cs,x=="OOO"]
          dso = [x | x<-ds,x=="OOO"]
          ess = fsx++csx++dsx++fso++cso++dso
          lss = [length x | x<-ess]
-- ------------------------------------------------------------------------

-- Función que nos devuelve el carácter que usa cada jugador.
devuelveChar :: Int -> Char
devuelveChar j
    | j == 1 = cj1
    | otherwise = cj2

-- Función para conseguir los dígitos que vamos a necesitar en nuestra interacción.
leeDigito :: String -> IO Int
leeDigito c = do
    putStr c
    digito <- getLine
    if (length digito == 1) && isDigit (head digito)
        then return (read digito)
        else do 
            putStrLn "ERROR: No has introducido un dígito"
            d <- leeDigito c
            return d

-- Función para cambiar de jugador 1 a jugador 2.
siguiente :: Int -> Int
siguiente j = if j == 1 then 2 else 1

-- Función para gestionar que entren índices que estén dentro de la cuadrícula.
revisaIn :: (Int,Int) -> IO (Int,Int)
revisaIn (i,j) = do
    f <- leeDigito "-Primero indica la fila: "
    c <- leeDigito "-Ahora indica la columna: "
    if (f>=i && f<=j) && (c>=i && c<=j)
        then return (f, c)
        else do
            putStrLn "¡Fila o columna fuera de la cuadrícula. Vuelva a escoger!"
            revisaIn (i,j)

-- Función para realizar una jugada.
jugada :: Cuadricula -> (Int,Int) -> Char -> IO Cuadricula
jugada c (i,j) v
    | valido (i,j) c = do
        let cn = actualizaValor v c (i,j)
        return cn
    | otherwise = do
        putStrLn "Jugada no válida, vuelva a intentarlo."
        putStrLn "-Recuerde que no puede pintar casillas ya ocupadas."
        let rangosC = bounds c
        let par1 = fst rangosC
        let par2 = snd rangosC
        let menor = fst par1
        let mayor = snd par2
        putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
        putStrLn $ (show menor) ++ " y " ++ (show mayor) ++ "."
        (fil,col) <- revisaIn (menor,mayor)
        jugada c (fil,col) v

-- Función para representar el estado del juego en consola.
representaCuadricula :: Cuadricula -> IO()
representaCuadricula c = do
    let fs = listaFilas c
    let cuadricula = escribeCuadricula fs
    putStrLn $ "\n" ++ cuadricula

-- Función para continuar una partida. Recibe la cuadrícula estado del juego y el jugador que tiene turno.
juegoMedio :: Cuadricula -> Int -> IO()
juegoMedio c j = do
    putStrLn "Estado del juego:\n"
    representaCuadricula c
    putStrLn $ "-Le toca al jugador " ++ (show j)
    let rangosC = bounds c
    let par1 = fst rangosC
    let par2 = snd rangosC
    let menor = fst par1
    let mayor = snd par2
    putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
    putStrLn $ (show menor) ++ " y " ++ (show mayor) ++ "."
    (fil,col) <- revisaIn (menor,mayor)
    let v = devuelveChar j
    cn <- jugada c (fil,col) v
    gestionaTurno cn j

-- Función para continuar una partida con la máquina.
juegoMedio2 :: Cuadricula -> Int -> Int -> IO()
juegoMedio2 c j dif = do
    putStrLn "Estado del juego:\n"
    representaCuadricula c
    let rangosC = bounds c
    let par1 = fst rangosC
    let par2 = snd rangosC
    let menor = fst par1
    let mayor = snd par2
    if j == 1
        then do
            putStrLn "-Le toca al jugador"
            putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
            putStrLn $ (show menor) ++ " y " ++ (show mayor) ++ "."
            (fil,col) <- revisaIn (menor,mayor)
            cn <- jugada c (fil,col) jugadorVsM
            gestionaTurno2 cn j dif
        else do
            putStrLn "-Le toca a la máquina"
            if dif == 1
                then do
                    let cn = ponAleatorio c
                    gestionaTurno2 cn j dif
                else do
                    let cn = ponNoAleatorio c
                    gestionaTurno2 cn j dif
					

-- ********************************************************************************
-- Funciones necesarias para que la máquina funcione con aprendizaje automático

-- Valor por defecto para la poda
profBus :: Int
profBus = 6

-- Función para determinar las casillas libres
libres :: Cuadricula -> [(Int, Int)]
libres c = [p | p <- lista, valido p c]
	where lista = [(x,y) | x<-[0..2], y<-[0..2]]

-- Función de creacion de arbol
creaArbol :: Cuadricula -> Arbol Cuadricula
creaArbol c = creaArbol' c 0
creaArbol' c ac
            | llena c = (N c [])
            | otherwise = (N c as)
             where s = movimientos c ac
                   as = [creaArbol' a (ac+1) | a<-s]

-- Función de jugadas futuras segun su disponibilidad
movimientos :: Cuadricula -> Int -> [Cuadricula]
movimientos c pr
              | hay3EnRaya c = []
              | even pr = map (actualizaValor maquina c) (libres c)
              | otherwise = map (actualizaValor jugadorVsM c) (libres c)

-- Definimos tipo Puntuación para valorar las cuadrículas
type Puntuacion = Int

-- Función para puntuar las cuadrículas dentro del arbol de decisiones
puntuaciones :: [Arbol (Puntuacion,Cuadricula)] -> [Puntuacion]
puntuaciones arboles = [p | N (p,_) _ <- arboles]

-- Función para el nivel avanzado de la máquina
ponNoAleatorio :: Cuadricula -> Cuadricula
ponNoAleatorio c = mejorMov c

-- Funciones propias del algoritmo minimax con poda

-- Función para determinar el mejor movimiento posible para ganar.
mejorMov :: Cuadricula -> Cuadricula
mejorMov = seleccion . maximiza . poda profBus . creaArbol

-- Función que selecciona la cuadrícula que tenga asignada la puntuación mas favorable
seleccion :: Arbol (Puntuacion, Cuadricula) -> Cuadricula
seleccion (N (p,_) cs) = head [c | N (puntos, c) _ <- cs, puntos == p]

-- Función que actualiza el valor de una cuadrícula a mejor si resulta en un movimiento favorable
maximiza :: Arbol Cuadricula -> Arbol (Puntuacion, Cuadricula)
maximiza (N c []) | hay3EnRaya c = N (-1,c) [] 
				  | otherwise = N (0,c) []
maximiza (N c cs) = N (maximum (puntuaciones ps),c) ps
	where ps = map minimiza cs

-- Función que actualiza el valor de una cuadrícula a peor si resulta en un movimiento no favorable
minimiza :: Arbol Cuadricula -> Arbol (Puntuacion, Cuadricula)
minimiza (N c []) | hay3EnRaya c = N (1,c) [] 
				  | otherwise = N (0,c) []
minimiza (N c cs) = N (minimum (puntuaciones ps),c) ps
	where ps = map maximiza cs

-- Función que elimina caminos posibles en el arbol de decisiones según la profundidad de busqueda
poda :: Int -> Arbol a -> Arbol a
poda prof (N a as) | prof == 0 = N a [] | otherwise = N a (map (poda (prof-1)) as)


-- Función para manejar lo que le ocurre al juego entre turno y turno. Esta función se usará también para
-- gestionar el comienzo de un juego nuevo en el main.
gestionaTurno :: Cuadricula -> Int -> IO()
gestionaTurno c j = do
    if (finalizado c)
        then if (hay3EnRaya c)
            then do
                representaCuadricula c
                putStrLn $ "¡El jugador "++(show j)++" ha ganado!"
            else putStrLn "Empate..."
        else do
            let jn = siguiente j
            representaCuadricula c
            putStrLn "¿Desea guardar partida?"
            putStrLn "Si desea guardar partida escriba <<SI>> por favor"
            deseo <- getLine
            if deseo == "SI"
                then do
                    putStrLn "En ese caso escriba un nombre para el archivo de guardado"
                    nombre <- getLine
                    guardarPartida c jn nombre
                    putStrLn "¿Desea seguir jugando?"
                    putStrLn "Si desea seguir jugando escriba <<SI>> por favor. En caso contrario se entenderá como que no."
                    deseo2 <- getLine
                    if deseo2=="SI"
                        then juegoMedio c jn
                        else return ()
                else juegoMedio c jn

-- Función para manejar lo que le ocurre al juego entre turno y turno contra la máquina.
gestionaTurno2 :: Cuadricula -> Int -> Int -> IO()
gestionaTurno2 c j dif = do
    if (finalizado c)
        then if (hay3EnRaya c)
            then do
                representaCuadricula c
                if j == 1
                    then putStrLn "¡El jugador ha ganado!"
                    else putStrLn "La máquina gana..."
            else putStrLn "Empate..."
        else do
            let jn = siguiente j
            representaCuadricula c
            putStrLn "¿Desea guardar partida?"
            putStrLn "Si desea guardar partida escriba <<SI>> por favor"
            deseo <- getLine
            if deseo == "SI"
                then do
                    putStrLn "En ese caso escriba un nombre para el archivo de guardado"
                    nombre <- getLine
                    guardarPartida c jn nombre
                    putStrLn "¿Desea seguir jugando?"
                    putStrLn "Si desea seguir jugando escriba <<SI>> por favor. En caso contrario se entenderá como que no."
                    deseo2 <- getLine
                    if deseo2=="SI"
                        then juegoMedio2 c jn dif
                        else return ()
                else juegoMedio2 c jn dif

-- Función para generar una partida nueva.
partidaNueva :: IO ()
partidaNueva = do
    putStrLn "¿Quiere empezar el jugador con X o con O?"
    putStrLn "Escriba 'O' o 'X' por favor."
    eleccion <- getChar
    if ((eleccion=='X') || (eleccion=='O'))
        then if eleccion == 'X'
            then do
                let j = 1
                juegoMedio inicial j
            else do
                let j = 2
                juegoMedio inicial j
        else do
            putStrLn "Carácter inválido. Vuelva a intentarlo."
            partidaNueva

-- Función para generar una partida nueva con una máquina.
partidaNueva2 :: IO ()
partidaNueva2 = do
    dif <- leeDigito "Primero escoja una dificultad. Puede escoger entre simple(1) o complejo(2): "
    j <- leeDigito "Ahora escoja si quiere empezar usted(1) o la máquina(2) por favor: "
    juegoMedio2 inicial j dif

-- Función para cargar partida. Supondremos que los datos de los guardados son siempre correctos.
cargarPartida :: IO String
cargarPartida = do
    putStrLn "-Escriba el nombre del fichero que guarda la partida"
    fichero <- getLine
    existe <- doesFileExist fichero
    if existe
        then do
            contenido <- readFile fichero
            let lineas = [l | l<-lines contenido, length l > 0]
            return (concat lineas)
        else do
            putStrLn "Error, este fichero no existe en el directorio actual."
            contenido <- cargarPartida
            let lineas = [l | l<-lines contenido, length l > 0]
            return (concat lineas)
    

-- Función para crear un guardado del juego.
guardarPartida :: Cuadricula -> Int -> FilePath -> IO ()
guardarPartida c j nombre = do
    let estado = elems c
    let jugador = show j
    let texto = estado++"\n"++jugador
    writeFile nombre texto

-- Funciones para iniciar o cargar el juego para humanos.
-- ------------------------------------------------------------------------
iniciaJuego :: IO ()
iniciaJuego = do
    putStrLn "¿Quieres empezar un juego nuevo o cargar una partida?"
    putStrLn "Escribe 'nuevo' o 'cargar' por favor"
    respuesta <- getLine
    trataR respuesta

-- Función para tratar con la respuesta del usuario.
trataR :: String -> IO ()
trataR r
    | r == "nuevo" = partidaNueva
    | r == "cargar" = do
        datos <- cargarPartida
        let c = traduceCadena (init datos) 3
        let j = digitToInt (last datos)
        if (finalizado c)
            then if (llena c)
                then putStrLn "Empate..."
                else do
                    representaCuadricula c
                    let jn = siguiente j
                    putStrLn $ "¡El jugador "++(show jn)++" ha ganado!"
            else do
                juegoMedio c j
    | otherwise = do
        putStrLn "Entrada no válida. Inténtelo de nuevo"
        respuesta <- getLine
        trataR respuesta
-- ------------------------------------------------------------------------

-- Funciones para iniciar o cargar el juego para un jugador.
-- ------------------------------------------------------------------------
iniciaJuego2 :: IO ()
iniciaJuego2 = do
    putStrLn "¿Quieres empezar un juego nuevo o cargar una partida?"
    putStrLn "Escribe 'nuevo' o 'cargar' por favor"
    respuesta <- getLine
    trataR2 respuesta

-- Función para tratar con la respuesta del usuario.
trataR2 :: String -> IO ()
trataR2 r
    | r == "nuevo" = partidaNueva2
    | r == "cargar" = do
        datos <- cargarPartida
        let c = traduceCadena (init datos) 3
        let j = digitToInt (last datos)
        if (finalizado c)
            then if (llena c)
                then putStrLn "Empate..."
                else do
                    representaCuadricula c
                    let jn = siguiente j
                    if jn == 1
                        then do
                            putStrLn $ "¡El jugador ha ganado!"
                        else do
                            putStrLn "La máquina ha ganado..." 
            else do
                dif <- leeDigito "Escoja la dificultad con la que quiere reanudar la partida por favor"
                juegoMedio2 c j dif
    | otherwise = do
        putStrLn "Entrada no válida. Inténtelo de nuevo"
        respuesta <- getLine
        trataR2 respuesta
-- ------------------------------------------------------------------------

-- Función para el nivel más simple de la máquina.
ponAleatorio :: Cuadricula -> Cuadricula
ponAleatorio c = actualizaValor maquina c pos
    where validos = [x | x<-listaAleatoria, valido x c]
          pos = head validos


-- ------------------------------------------------------------------------

data Arbol a = N a [Arbol a]

muestraArbol (N x xs) = 
    show x ++ '\n' : (unlines . map ("  "++) . concatMap (lines . show)) xs
 
instance Show a => Show (Arbol a) where
  show = muestraArbol

-- Función de generación de arbol
generaArbol :: Cuadricula -> Arbol Cuadricula
generaArbol c | llena c = (N c [])
              | otherwise = (N c (map generaArbol (posiblesJugadas c)))

-- Función de jugadas futuras
posiblesJugadas :: Cuadricula -> [Cuadricula]
posiblesJugadas c | victoriaM c = []
                  | otherwise = (ponAleatorio c):posiblesJugadas c

-- Función para comprobar si la maquina ha ganado
victoriaM :: Cuadricula -> Bool
victoriaM c = or [if x==3 then True else False | x<-lss]
  where fs = listaFilas c
        cs = listaColumnas c
        ds = diagonalesMatriz c
        fso = [x | x<-fs,x=="OOO"]
        cso = [x | x<-cs,x=="OOO"]
        dso = [x | x<-ds,x=="OOO"]
        ess = fso++cso++dso
        lss = [length x | x <- ess]

-- Función para calcular la profundidad de cada rama
profundidad :: Arbol a -> [Int]
profundidad (N _ []) = [0]
profundidad (N _ [(N x [])]) = [1]
profundidad (N _ xs) = [1 + sum (profundidad x) | x <- xs]

-- Función para saber el número de jugadas óptimas para algún jugador
profundidadMin :: Arbol a -> Int
profundidadMin (N _ as)
      | null as = 0
      | otherwise = 1 + calc
            where calc = minimum (map profundidadMin as)

-- Función para saber el número de jugadas menos óptimas para algún jugador
profundidadMax :: Arbol a -> Int
profundidadMax (N _ as)
      | null as = 0
      | otherwise = 1 + calc
            where calc = maximum (map profundidadMax as)
