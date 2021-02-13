-- Este archivo contiene el main final del juego interactivo 3 en raya

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
numeroDeFilas = 3


-- Función main
main :: IO()
main = iniciaJuego

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

-- Función para manejar lo que le ocurre al juego entre turno y turno. Esta función se usará también para
-- gestionar el comienzo de un juego nuevo en el main.
gestionaTurno :: Cuadricula -> Int -> IO()
gestionaTurno c j = do
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