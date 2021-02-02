-- Este archivo contiene el main final del juego interactivo 3 en raya

-- Módulos importados
-- ------------------------------------------------------------------------
import Utiles
import System.Directory
import Data.Array
import Data.Char
-- ------------------------------------------------------------------------

-- Inicializamos las variables que necesitaremos para este juego.
cj1 = 'X'
cj2 = 'O'


-- Función main
main :: IO()
main = undefined

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

-- Función para realizar una jugada.
jugada :: Cuadricula -> (Int,Int) -> Char -> Cuadricula
jugada c (i,j) v
    | valido (i,j) c = actualizaValor (i,j) v c
    | otherwise = error "Jugada no válida."

-- Función para representar el estado del juego en consola.
representaCuadricula :: Cuadricula -> IO()
representaCuadricula c = do
    let fs = listaFilas c
    let cuadricula = escribeCuadricula fs
    putStrLn cuadricula

-- Función que nos devuelve el carácter que usa cada jugador.
devuelveChar :: Int -> Char
devuelveChar j
    | j == 1 = cj1
    | otherwise = cj2

-- Función para conseguir los dígitos que vamos a necesitar en nuestra interacción.
leeDigito :: String -> IO Int
leeDigito c = do
    putStr c
    digito <- getChar
    if isDigit digito
        then return (digitToInt digito)
        else do 
            putStrLn "ERROR: No has introducido un dígito"
            leeDigito c

-- Función para cambiar de jugador 1 a jugador 2.
siguiente :: Int -> Int
siguiente j = if j == 1 then 2 else 1

-- Función para gestionar que entren índices que estén dentro de la cuadrícula.
revisaIn :: (Int,Int) -> (Int,Int)
revisaIn (i,j) = do
    f <- leeDigito "Primero indica la fila: "
    c <- leeDigito "Ahora indica la columna: "
    if (f>=i && f<=j) && (c>=i && c<=j)
        then (f, c)
        else error "Fila o columna fuera de la cuadrícula. Vuelva a escoger."

-- Función para continuar una partida. Recibe la cuadrícula estado del juego y el jugador que tiene turno.
juegoMedio :: Cuadricula -> Int -> IO()
juegoMedio c j = do
    putStrLn "Estado del juego:"
    representaCuadricula c
    putStrLn $ "Le toca al jugador " ++ (show j)
    let rangosC = bounds c
    let par1 = fst rangosC
    let par2 = snd rangosC
    let menor = fst par1
    let mayor = snd par2
    putStr "Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
    putStrLn $ (show menor) ++ " y " ++ (show mayor) ++ "."
    let (fil,col) = revisaIn (menor,mayor)
    let v = devuelveChar j
    let cn = jugada c (fil,col) v
    gestionaTurno cn j

-- Función para manejar lo que le ocurre al juego entre turno y turno. Esta función se usará también para
-- gestionar el comienzo de un juego nuevo en el main.
gestionaTurno :: Cuadricula -> Int -> IO()
gestionaTurno c j = do
    if (finalizado c)
        then if (llena c)
            then putStrLn "Empate"
            else putStrLn $ "El jugador "++(show j)++" ha ganado."
        else do
            let jn = siguiente j
            juegoMedio c jn