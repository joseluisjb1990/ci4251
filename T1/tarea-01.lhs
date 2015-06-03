\documentclass[11pt,fleqn]{article}

\usepackage{tikz}
\usepackage{multicol}
\usepackage{latexsym}
\usepackage{array}
\usepackage[english,spanish]{babel}
\usepackage{lmodern}
\usepackage{listings}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}
\usepackage{xcolor}

\usepackage{algorithmic}
\usepackage{algorithm}

\usetikzlibrary{positioning,shapes,folding,positioning,shapes,trees}

\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue
}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}



\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey}
}

\long\def\ignore#1{}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 1}

\author{Ernesto Hernández-Novich\\
86-17791\\
\href{mailto:emhn@usb.ve}{<emhn@usb.ve>}}

\date{Abril 17, 2015}

\maketitle

\pagebreak

\section{Machine Learning}

En este ejercicio, investigaremos la técnica de Regresión Lineal
Multivariable con el método de \emph{Gradient Descent}, aprovechando
las ecuaciones normales. Implantaremos el algoritmo paso a paso en
Haskell, aprovechando \emph{folds} y \emph{unfolds}.

No es necesaria experiencia previa con el Algoritmo, pues todos los
detalles serán presentados a lo largo del problema. Sugiero que
construya las funciones en el mismo orden en que se van proponiendo,
pues van aumentando de complejidad.

\subsection{Definiciones Generales}

Para el desarrollo de la solución, serán necesarios los módulos
\footnote{Los dos últimos opcionales si quiere tener el
gráfico que muestra la convergencia.}

\begin{lstlisting}

> import Data.List
> import Data.Functor
> import Data.Monoid
> import Data.Foldable (foldMap)
> import Data.Tree

\end{lstlisting}

Las técnicas de \emph{Machine Learning} operan sobre conjuntos
de datos o muestras. En este caso, existe un conjunto de muestras
que serán usadas para ``aprender'', y así poder hacer proyecciones
sobre muestras fuera de ese conjunto. Para user el método de
regresión lineal multivariable, las muestras son \emph{vectores}
$(x_1,x_2,\ldots,x_n)$ acompañados del valor asociado $y$
correspondiente.

En este ejercicio, nos limitaremos a usar vectores de dos variables,
pero usaremos un tipo polimórfico basado en listas, para poder
utilizar \texttt{Float} o \texttt{Double} según nos convenga, y
modelar vectores de longitud arbitraria. Su programa no puede
hacer ninguna suposición sobre la longitud de los vectores, más
allá de que todos son del mismo tamaño.

Así, definiremos el tipo polimórfico

\begin{lstlisting}

> data Sample a = Sample { x :: [a], y :: a }
>      deriving (Show)

\end{lstlisting}

\newpage

Teniendo una colección de muestras como la anterior, el algoritmo
calcula una \emph{hipótesis}, que no es más que un \emph{vector}
de coeficientes $(\theta_0, \theta_1, \ldots, \theta_n)$ tal que
minimiza el error de predicción
$(\theta_0 + \theta_1 \times x_1 + \ldots + \theta_n x_n - y)$
para toda la colección de muestras.


\begin{lstlisting}

> data Hypothesis a = Hypothesis { c :: [a] }
>      deriving (Show)

\end{lstlisting}

En el caso general, asegurar la convergencia del algoritmo en un
tiempo razonable es hasta cierto punto ``artístico''. Sin entrar en
detalles, es necesario un coeficiente $\alpha$ que regule
cuán rápido se desciende por el gradiente

\begin{lstlisting}

> alpha :: Double
> alpha = 0.03

\end{lstlisting}

También hace falta determinar si el algoritmo dejó de progresar,
para lo cual definiremos un márgen de convergencia $\epsilon$
muy pequeño

\begin{lstlisting}

> epsilon :: Double
> epsilon = 0.0000001

\end{lstlisting}

Finalmente, el algoritmo necesita una hipótesis inicial, a partir
de la cual comenzar a calcular gradientes y descender hasta encontrar
el mínimo, con la esperanza que sea un mínimo global. Para nuestro
ejercicio, utilizaremos

\begin{lstlisting}

> guess :: Hypothesis Double
> guess = Hypothesis { c = [0.0, 0.0, 0.0] }

\end{lstlisting}

\subsection{Muestras de Entrenamiento}

En este archivo se incluye la definición

\begin{lstlisting}

> training :: [Sample Double]

\end{lstlisting}

\ignore{
\begin{code}
training = [
  Sample { x = [  0.1300098690745405, -0.2236751871685913 ], y = 399900 },
  Sample { x = [ -0.5041898382231769, -0.2236751871685913 ], y = 329900 },
  Sample { x = [  0.502476363836692, -0.2236751871685913 ], y = 369000 },
  Sample { x = [ -0.7357230646969468, -1.537766911784067 ], y = 232000 },
  Sample { x = [  1.257476015381594, 1.090416537446884 ], y = 539900 },
  Sample { x = [ -0.01973172848186497, 1.090416537446884 ], y = 299900 },
  Sample { x = [ -0.5872397998931161, -0.2236751871685913 ], y = 314900 },
  Sample { x = [ -0.7218814044186236, -0.2236751871685913 ], y = 198999 },
  Sample { x = [ -0.7810230437896409, -0.2236751871685913 ], y = 212000 },
  Sample { x = [ -0.6375731099961096, -0.2236751871685913 ], y = 242500 },
  Sample { x = [ -0.07635670234773261, 1.090416537446884 ], y = 239999 },
  Sample { x = [ -0.0008567371932424295, -0.2236751871685913 ], y = 347000 },
  Sample { x = [ -0.1392733399764744, -0.2236751871685913 ], y = 329999 },
  Sample { x = [  3.117291823687202,   2.40450826206236 ], y = 699900 },
  Sample { x = [ -0.9219563120780225, -0.2236751871685913 ], y = 259900 },
  Sample { x = [  0.3766430885792084,  1.090416537446884 ], y = 449900 },
  Sample { x = [ -0.856523008944131,  -1.537766911784067 ], y = 299900 },
  Sample { x = [ -0.9622229601604173, -0.2236751871685913 ], y = 199900 },
  Sample { x = [  0.7654679091248329,  1.090416537446884 ], y = 499998 },
  Sample { x = [  1.296484330711414,   1.090416537446884 ], y = 599000 },
  Sample { x = [ -0.2940482685431793, -0.2236751871685913 ], y = 252900 },
  Sample { x = [ -0.1417900054816241, -1.537766911784067 ], y = 255000 },
  Sample { x = [ -0.4991565072128776, -0.2236751871685913 ], y = 242900 },
  Sample { x = [ -0.04867338179108621, 1.090416537446884 ], y = 259900 },
  Sample { x = [  2.377392165173198,  -0.2236751871685913 ], y = 573900 },
  Sample { x = [ -1.133356214510595,  -0.2236751871685913 ], y = 249900 },
  Sample { x = [ -0.6828730890888036, -0.2236751871685913 ], y = 464500 },
  Sample { x = [  0.6610262906611214, -0.2236751871685913 ], y = 469000 },
  Sample { x = [  0.2508098133217248, -0.2236751871685913 ], y = 475000 },
  Sample { x = [  0.8007012261969283, -0.2236751871685913 ], y = 299900 },
  Sample { x = [ -0.2034483103577911, -1.537766911784067 ], y = 349900 },
  Sample { x = [ -1.259189489768079,  -2.851858636399542 ], y = 169900 },
  Sample { x = [  0.04947657290975102, 1.090416537446884 ], y = 314900 },
  Sample { x = [  1.429867602484346,  -0.2236751871685913 ], y = 579900 },
  Sample { x = [ -0.2386816274298865,  1.090416537446884 ], y = 285900 },
  Sample { x = [ -0.7092980768928753, -0.2236751871685913 ], y = 249900 },
  Sample { x = [ -0.9584479619026928, -0.2236751871685913 ], y = 229900 },
  Sample { x = [  0.1652431861466359,  1.090416537446884 ], y = 345000 },
  Sample { x = [  2.78635030976002,    1.090416537446884 ], y = 549000 },
  Sample { x = [  0.202993168723881,   1.090416537446884 ], y = 287000 },
  Sample { x = [ -0.4236565420583874, -1.537766911784067 ], y = 368500 },
  Sample { x = [  0.2986264579195686, -0.2236751871685913 ], y = 329900 },
  Sample { x = [  0.7126179335166897,  1.090416537446884 ], y = 314000 },
  Sample { x = [ -1.007522939253111,  -0.2236751871685913 ], y = 299000 },
  Sample { x = [ -1.445422737149154,  -1.537766911784067 ], y = 179900 },
  Sample { x = [ -0.1870899845743182,  1.090416537446884 ], y = 299900 },
  Sample { x = [ -1.003747940995387,  -0.2236751871685913 ], y = 239500 } ]
\end{code}

}

que cuenta con 47 muestras de entrenamiento listas para usar.
Tampoco debe preocuparle mucho qué representan -- al algoritmo no
le importan y Ud. tampoco tiene que preocuparse por eso.
\footnote{En la práctica, las muestras suelen estar en diferentes
escalas (precio en miles, unidades de Frobs, porcentajes, etc.) y
uno de los trabajos previos es normalizar las medidas. Ya eso fue
hecho por Ud. porque no es importante para esta materia.}

\subsection{Ahora le toca a Ud.}

\subsubsection{Comparar en punto flotante}

No se puede y Ud. lo sabe. Pero necesitamos una manera de determinar
si la diferencia entre dos números en punto flotante es
$\epsilon-$despreciable

\begin{lstlisting}

> veryClose :: Double -> Double -> Bool
> veryClose v0 v1 = undefined

\end{lstlisting}

Por favor \textbf{no} use un \texttt{if}\ldots

\subsubsection{Congruencia dimensional}

Seguramente notó que los vectores con muestras
tienen dimensión $n$, pero la hipótesis tiene dimensión $n+1$.
Eso es porque la hipótesis necesariamente debe agregar un coeficiente
constante para la interpolación lineal. En consecuencia \emph{todas}
las muestras necesitan incorporar $x_0 = 1$.

\begin{lstlisting}

> addOnes :: [Sample Double] -> [Sample Double]
> addOnes = undefined

\end{lstlisting}

Escriba la función \texttt{addOnes} usando exclusivamente funciones
de orden superior y en estilo \emph{point-free} (con argumento
implícito, como puede ver).

\subsubsection{Evaluando Hipótesis}

Si tanto una hipótesis $\theta$ como una muestra $X$ son vectores
de $n+1$ dimensiones, entonces se puede evaluar la hipótesis en
$h_\theta(X) = \theta^TX$ calculando el producto punto
de ambos vectores

\begin{lstlisting}

> theta h s :: Hypothesis Double -> Sample Double -> Double
> theta h s = undefined

\end{lstlisting}

Escriba la función \texttt{theta} usando exclusivamente funciones
de orden superior. Puede suponer que ambos vectores tienen las
dimensiones correctas.

Una vez que pueda evaluar hipótesis, es posible determinar cuán
buena es la hipótesis sobre el conjunto de entrenamiento. La calidad
de la hipótesis se mide según su \textbf{costo} $J(\theta)$ que no
es otra cosa sino determinar la suma de los cuadrados de los errores.
Para cada muestra $x^{(i)}$ en el conjunto de entrenamiento, se
evalúa la hipótesis en ese vector y se compara con el $y(i)$. La
fórmula concreta para $m$ muestras es
$$ J(\theta) = \frac{1}{2m} \sum_{i=1}^{m}{(h_\theta(x^{(i)}) - y^{(i)})^2} $$

\begin{lstlisting}

> cost :: Hypothesis Double -> [Sample Double] -> Double
> cost h ss = undefined

\end{lstlisting}

Su función debe ser escrita como un \emph{fold} que realice todos
los cómputos en \emph{una sola pasada} sobre el conjunto de muestras.
Debe escribirla de manera general, suponiendo que \texttt{ss} podría
tener una cantidad arbitraria de muestras disponibles.

\subsection{Bajando por el gradiente}

El algoritmo de descenso de gradiente por lotes es sorprendentemente
sencillo. Se trata de un algoritmo iterativo que parte de una
hipótesis $\theta$ que tiene un costo $c$, determina la dirección
en el espacio vectorial que maximiza el descenso, y produce una
nueva hipótesis $\theta'$ con un nuevo costo $c'$ tal que $c' \leq c$.
La ``velocidad'' con la cual se desciende por el gradiente viene
dada por el coeficiente ``de aprendizaje'' $\alpha$.

Dejando el álgebra vectorial de lado por un momento, porque no
importa para esta materia, es natural pensar que nuestro algoritmo
iterativo tendrá que detenerse cuando la diferencia entre $c$ y $c'$
sea $\epsilon-$despreciable.

La primera parte de este algoritmo, y sin duda la función más
complicada de este ejercicio, es aquella que dada una hipótesis
y un conjunto de entrenamiento, debe producir una nueva hipótesis
mejorada según el coeficiente de aprendizaje.

\begin{lstlisting}

> descend :: Double -> Hypothesis Double -> [Sample Double]
>         -> Hypothesis Double
> descend alpha h ss = undefined

\end{lstlisting}

Sea $\theta_j$ el $j-$ésimo componente del vector $\theta$
correspondiente a la hipótesis actual que pretendemos mejorar. La
función debe calcular, para todo $j$

$$\theta'_j \leftarrow \theta_j -
\frac{\alpha}{m}\sum_{i=1}^m{(h_\theta(x^{(i)})-y^{(i)})x_j^{(i)}}$$

donde $m$ es el número de muestras de entrenamiento.

Su función debe ser escrita exclusivamente empleando funciones
de orden superior. En particular, se trata de dos \emph{fold}
anidados, cada uno de ellos realizando sus cómputos en
\emph{una sola pasada}. Debe escribirla de manera general,
suponiendo que \texttt{ss} podría tener una cantidad arbitraria
de muestras disponibles y que $j$ es arbitrario.

La segunda parte de este algoritmo debe partir de una hipótesis
inicial y el conjunto de entrenamiento, para producir una lista
de elementos tales que permitan determinar, para cada iteración,
cuál es la hipótesis mejorada y el costo de la misma.

\begin{lstlisting}

> gd :: Double -> Hypothesis Double -> [Sample Double]
>    -> [(Integer,Hypothesis Double,Double)]
> gd alpha h ss = undefined

\end{lstlisting}

Su función debe ser escrita como un \emph{unfold}. Note que esta
será la función ``tope'', por lo tanto debe asegurarse de agregar
los coeficientes 1 antes de comenzar a iterar, y mantener la
iteración hasta que la diferencia entre los costos de dos
hipótesis consecutivas sean $\epsilon-$despreciables.

\subsection{¿Cómo sé si lo estoy haciendo bien?}

Probar las funciones \texttt{veryClose}, \texttt{addOnes}
y \texttt{theta} es trivial por inspección. Para probar la
función \texttt{cost} tendrá que hacer algunos cálculos a
mano con muestras pequeñas y comprobar los resultados que
arroja la función. Preste atención que estas funciones
\emph{asumen} que las muestras ya incluyen el coeficiente
constante 1.

Probar la función \texttt{descend} es algo más complicado,
pero la sugerencia general es probar paso a paso si se
produce una nueva hipótesis cuyo costo es, en efecto, menor.

Con las definiciones en este archivo, si su algoritmo está
implantado correctamente, hay convergencia. Para que tenga
una idea

\begin{lstlisting}
  ghci> take 3 (gd alpha guess training)
  [(0,Hypothesis {c = [0.0,0.0,0.0]},6.559154810645744e10),
   (1,Hypothesis {c = [10212.379787234042,3138.9880129854737,...
   (2,Hypothesis {c = [20118.388180851063,6159.113611965675,...]
\end{lstlisting}

y si se deja correr hasta terminar converge (el \emph{unfold}
\textbf{termina}) y los resultados numéricos en la última tripleta
deberían ser muy parecidos a (indentación mía)

\begin{lstlisting}
  (1072,
  Hypothesis {c = [340412.65957446716,
                   110631.04133702737,
                   -6649.4653290010865]},
  2.043280050602863e9)
\end{lstlisting}

Para su comodidad, he incluido la función \texttt{graph} de la
magnífica librería \texttt{chart} que permite hacer gráficos
sencillos (línea, torta, barra, etc.). Puede usarla para verificar
que su función está haciendo el trabajo

\begin{lstlisting}
  ghci> graph "works" (gd alpha guess training)
\end{lstlisting}

y en el archivo \texttt{works.png} debe obtener una imagen
similar a
\begin{center}
        \includegraphics[width=11cm]{works.png}
\end{center}

\subsection{¿Aprendió?}

Una vez que el algoritmo converge, obtenga la última hipótesis
y úsela para predecir el valor $y$ asociado al vector
$(-0.44127, -0.22368)$.

\begin{verbatim}
  ghci> let (_,h,_) = last (gd alpha guess training)
  ghci> let s = Sample ( x = [1.0, -0.44127,-0.22368], y = undefined }
  ghci> theta h s
  293081.85236
\end{verbatim}

\section{Monoids}

Durante la discusión en clase acerca de \texttt{Monoid} se dejó
claro que para algunos tipos de datos existe más de una instancia
posible. En concreto, para los números puede construirse una
instancia \texttt{Sum} usando \texttt{(+)} como operación y
\texttt{0} como elemento neutro, pero también puede construirse
una instancia \texttt{Product} usando \texttt{(*)} como operación
y \texttt{1} como elemento neutro. La solución al problema resultó
ser el uso de tipos equivalentes pero incompatibles aprovechando
\texttt{newtype}.

Siguiendo esa idea, construya una instancia \texttt{Monoid}
\emph{polimórfica} para \emph{cualquier} tipo comparable, tal que
al aplicarla sobre cualquier \texttt{Foldable} conteniendo 
elementos de un tipo concreto comparable, se retorne el máximo
valor almacenado, si existe. La aplicación se logra con la
función

\begin{verbatim}
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
\end{verbatim}

Note que en este caso \texttt{a} es el tipo comparable, y la
primera función debe levantar el valor libre al \texttt{Monoid}
calculador de máximos. Piense que el \texttt{Foldable} \texttt{t}
\emph{podría} estar vacío (lista, árbol, \ldots) así que el
\texttt{Monoid} debe operar con ``seguridad''

Oriéntese con los siguientes ejemplos

\begin{verbatim}
ghci> foldMap (Max . Just) []
Max {getMax = Nothing}
ghci> foldMap (Max . Just) ["foo","bar","baz"]
Max {getMax = Just "foo"}
ghci> foldMap (Max . Just) (Node 6 [Node 42 [], Node 7 [] ])
Max {getMax = Just 42}
ghci> foldMap (Max . Just) (Node [] [])
\end{verbatim}

\section{Zippers}

Considere el tipo de datos

\begin{lstlisting}

> data Filesystem a = File a | Directory a [Filesystem a]

\end{lstlisting}

Diseñe un zipper seguro para el tipo \texttt{Filesystem}
proveyendo todas las funciones de soporte que permitan trasladar
el foco dentro de la estructura de datos, así como la modificación
de cualquier posición dentro de la estructura.

\begin{lstlisting}

> data Breadcrumbs a = undefined
>
> type Zipper a = (Filesystem a, Breadcrumbs a)
>
> goDown   ::
> goRight  ::
> goLeft   ::
> goBack   ::
> tothetop :: 
> modify   ::
> focus    ::
> defocus  ::

\end{lstlisting}

\end{document}
