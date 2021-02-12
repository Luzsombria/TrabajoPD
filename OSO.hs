-- Este archivo contiene el main final del juego interactivo OSO

-- Módulos importados
-- ------------------------------------------------------------------------
import Utiles
import System.Directory
import Data.Array
import Data.Char
import System.IO
-- ------------------------------------------------------------------------

-- Inicializamos las variables que necesitaremos para este juego.
numeroDeFilas4 = 4
numeroDeFilas5 = 5
numeroDeFilas8 = 8


-- Función main
main :: IO()
main = undefined

-- También definimos el tipo Cuadricula que va a ser la referencia para el correcto desarrollo del juego. El tipo 'a' de la
-- Matriz lo vamos a sustituir por Char ya que vamos a usar 'O' y 'S' para formar la palabra OSO.
type Cuadricula = Matriz Char

-- Creamos la cuadrícula vacía del tamaño escogido para comenzar un nuevo juego.
inicial :: Int -> Cuadricula
inicial tam = matrizUnitaria tam ' '

-- Vamos a crear una función para conocer si un juego se encuentra en el estado final o no.
-- ------------------------------------------------------------------------
finalizado :: Cuadricula -> Bool
finalizado c = (llena c)

-- Función para saber si la Cuadrícula del juego que nos pasan ya está llena.
llena :: Cuadricula -> Bool
llena c = (length es) == suma
    where es = elems c
          os = [o | o<-es,o=='O']
          ss = [s | s<-es,s=='S']
          suma = (length ss)+(length os)

-- ------------------------------------------------------------------------

-- También vamos a necesitar puntuar todas las veces que se forme la palabra OSO en una cuadrícula de 3x3.
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
    putStr c
    car <- getChar
    if (car=='O' || car=='S')
        then return car
        else do 
            putStrLn "ERROR: Sólo son admisibles la 'O' o la 'S'. Inténtelo de nuevo."
            d <- devuelveChar c
            return d

-- Función para continuar una partida. Recibe la cuadrícula estado del juego y el jugador que tiene turno.
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
    gestionaTurno cn j puntuaciones

-- Función para manejar lo que le ocurre al juego entre turno y turno. Esta función se usará también para
-- gestionar el comienzo de un juego nuevo en el main.
gestionaTurno :: Cuadricula -> Int -> (Int,Int) -> IO()
gestionaTurno c j puntuaciones = do
    if (finalizado c)
        then if (llena c)
            then putStrLn "Empate..."
            else do
                representaCuadricula c
                putStrLn $ "¡El jugador "++(show j)++" ha ganado!"
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