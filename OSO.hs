-- Este archivo contiene el main final del juego interactivo OSO

-- Módulos importados
-- ------------------------------------------------------------------------
import Utiles
import System.Directory
import Data.Array
import Data.Char
import Data.List
import System.IO
-- ------------------------------------------------------------------------

-- Función main
main :: IO()
main = iniciaJuego

-- Definimos el tipo Cuadricula que va a ser la referencia para el correcto desarrollo del juego. El tipo 'a' de la
-- Matriz lo vamos a sustituir por Char ya que vamos a usar 'O' y 'S' para formar la palabra OSO.
type Cuadricula = Matriz Char

-- Necesitaremos una función que nos cree cuadrículas con los valores que necesitemos más adelante.
creaCruadricula :: (Int,Int) -> [Char] -> Cuadricula
creaCruadricula is vs = matrizNueva is vs

-- Creamos la Cuadrícula vacía del tamaño escogido para comenzar un nuevo juego.
inicial :: Int -> Cuadricula
inicial tam = matrizUnitaria tam ' '

-- Vamos a crear una función para conocer si un juego se encuentra en el estado final o no.
-- ------------------------------------------------------------------------
finalizado :: Cuadricula -> Bool
finalizado c = (llena c)

-- Función auxiliar para saber si la Cuadrícula del juego que nos pasan ya está llena.
llena :: Cuadricula -> Bool
llena c = (length es) == suma
    where es = elems c
          os = [o | o<-es,o=='O']
          ss = [s | s<-es,s=='S']
          suma = (length ss)+(length os)

-- ------------------------------------------------------------------------

-- También vamos a necesitar puntuar todas las veces que se forme la palabra OSO en una Cuadrícula de 3x3.
puntuaOSO :: Cuadricula -> Int
puntuaOSO c = div (length es) 3
    where fs = listaFilas c
          cs = listaColumnas c
          ds = diagonalesMatriz c
          fso = [x | x<-fs,x=="OSO"]
          cso = [x | x<-cs,x=="OSO"]
          dso = [x | x<-ds,x=="OSO"]
          es = fso++cso++dso

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

-- Función para gestionar que entren índices que estén dentro de la Cuadrícula.
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
        let cn = actualizaValor (i,j) v c
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
    putStrLn cuadricula

-- Función para cambiar de jugador 1 a jugador 2.
siguiente :: Int -> Int
siguiente j = if j == 1 then 2 else 1

-- Función para conseguir el carácter pedido en la interacción.
devuelveChar :: String -> IO Char
devuelveChar c = do
    putStrLn c
    car <- getChar
    if (car=='O' || car=='S')
        then return car
        else do 
            putStrLn "ERROR: Sólo son admisibles la 'O' o la 'S'. Inténtelo de nuevo."
            d <- devuelveChar c
            return d

-- Funciones para ayudar con los cálculos de las puntuaciones.
-- ------------------------------------------------------------------------

-- Primero vamos a dividir en dos los cálculos para reducir su dificultad.
calculaPuntuacion :: Cuadricula -> (Int,Int) -> Char -> Int
calculaPuntuacion c (i,j) v
    | v=='O' = calculaPuntuacionO c (i,j)
    | otherwise = calculaPuntuacionS c (i,j)

-- De los dos cálculos posibles, el de 'S' es más sencillo ya que sólo necesitamos observar una posible Cuadrícula de 3x3.
calculaPuntuacionS :: Cuadricula -> (Int,Int) -> Int
calculaPuntuacionS c (i,j) = puntuaOSO acotada
    where lc = snd $ snd $ bounds c
          alrededor = [(m,n) | m<-[i-1..i+1],n<-[j-1..j+1], m>=0,n>=0,m<=lc,n<=lc]
          nfilas = length $ nub [fst ind | ind<-alrededor]
          nCols = length $ nub [snd ind | ind<-alrededor]
          es = [c ! p | p<-alrededor]
          acotada = creaCruadricula (nfilas,nCols) es

-- Con esta, vamos a aprovecharnos de la construida hace un momento para calcular la puntuación cuando se coloca una 'S'.
-- La idea es coger todas aquellas Cuadrículas de alrededor de nuestra 'O' y fijarnos si llevan 'S', en cuyo caso
-- sólo habría que calcular las puntuaciones de las 'S', por lo que volvemos a la función anterior.
calculaPuntuacionO :: Cuadricula -> (Int,Int) -> Int
calculaPuntuacionO c (i,j) = sum [calculaPuntuacionS c ind | ind<-contienenS]
    where lc = snd $ snd $ bounds c
          alrededor = [(m,n) | m<-[i-1..i+1],n<-[j-1..j+1], m>=0,n>=0,m<=lc,n<=lc]
          contienenS = [ind | ind<-alrededor,(c ! ind)=='S']

-- ------------------------------------------------------------------------
-- Función para continuar una partida. Recibe la Cuadrícula estado del juego y el jugador que tiene turno.
juegoMedio :: Cuadricula -> Int -> (Int,Int) -> IO()
juegoMedio c j puntuaciones = do
    putStrLn "Estado del juego:\n"
    representaCuadricula c
    putStrLn $ "Puntuación Jugador 1 = " ++ (show (fst puntuaciones)) ++ ", puntuación Jugador 2 = " ++ (show (snd puntuaciones))
    putStrLn $ "-Le toca al jugador " ++ (show j)
    let rangosC = bounds c
    let par1 = fst rangosC
    let par2 = snd rangosC
    let menor = fst par1
    let mayor = snd par2
    putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
    putStrLn $ (show menor) ++ " y " ++ (show mayor) ++ "."
    (fil,col) <- revisaIn (menor,mayor)
    v <- devuelveChar "-Indique si quiere colocar una 'O' o una 'S'"
    cn <- jugada c (fil,col) v
    let calculo = calculaPuntuacion cn (fil,col) v
    if j == 1
        then do
            let nuevasP = (calculo, snd puntuaciones)
            gestionaTurno cn j nuevasP
        else do
            let nuevasP = (fst puntuaciones, calculo)
            gestionaTurno cn j nuevasP

-- Función para manejar lo que le ocurre al juego entre turno y turno. Esta función se usará también para
-- gestionar el comienzo de un juego nuevo en el main.
gestionaTurno :: Cuadricula -> Int -> (Int,Int) -> IO()
gestionaTurno c j puntuaciones = do
    if (finalizado c)
        then if (fst puntuaciones)==(snd puntuaciones)
            then putStrLn "Empate..."
            else if (fst puntuaciones)>(snd puntuaciones)
                then do
                    representaCuadricula c
                    putStrLn "¡El jugador 1 ha ganado!"
                else do
                    representaCuadricula c
                    putStrLn "¡El jugador 2 ha ganado!"
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
                    guardarPartida c jn puntuaciones nombre
                    putStrLn "¿Desea seguir jugando?"
                    putStrLn "Si desea seguir jugando escriba <<SI>> por favor. En caso contrario se entenderá como que no."
                    deseo2 <- getLine
                    if deseo2=="SI"
                        then juegoMedio c jn puntuaciones
                        else return ()
                else juegoMedio c jn puntuaciones

-- Función para generar una partida nueva.
partidaNueva :: IO ()
partidaNueva = do juegoMedio (inicial 8) 1 (0,0)

-- Función para cargar partida. Supondremos que los datos de los guardados son siempre correctos.
cargarPartida :: IO [String]
cargarPartida = do
    putStrLn "-Escriba el nombre del fichero que guarda la partida"
    fichero <- getLine
    existe <- doesFileExist fichero
    if existe
        then do
            contenido <- readFile fichero
            let lineas = [l | l<-lines contenido, length l > 0]
            return lineas
        else do
            putStrLn "Error, este fichero no existe en el directorio actual."
            cargarPartida

-- Función para crear un guardado del juego.
guardarPartida :: Cuadricula -> Int -> (Int,Int) -> FilePath -> IO ()
guardarPartida c j (p1,p2) nombre = do
    let estado = elems c
    let jugador = show j
    let puntuaciones = (show p1) ++ "\n" ++ (show p2)
    let texto = estado ++ "\n"++ jugador ++ puntuaciones
    writeFile nombre texto

-- Funciones para iniciar o cargar el juego. A estas funciones son a las que acabará llamando el main.
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
        let c = traduceCadena (datos !! 0) 8
        let j = digitToInt $ head $ datos !! 1
        let p1 = digitToInt $ head $ datos !! 2
        let p2 = digitToInt $ head $ datos !! 3
        if (finalizado c)
        then if (p1)==(p2)
            then putStrLn "Empate..."
            else if (p1)>(p2)
                then do
                    representaCuadricula c
                    putStrLn "¡El jugador 1 ha ganado!"
                else do
                    representaCuadricula c
                    putStrLn "¡El jugador 2 ha ganado!"
            else do
                juegoMedio c j (p1,p2)
    | otherwise = do
        putStrLn "Entrada no válida. Inténtelo de nuevo"
        respuesta <- getLine
        trataR respuesta
-- ------------------------------------------------------------------------