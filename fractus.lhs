\documentclass[a4paper]{scrbook}
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[english]{babel}
\usepackage{fancyhdr}

\usepackage{listings}
\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}

\begin{document}


\chapter{Introduction}
Looking at some of the acronyms and buzz words of the last two centuries 
like OOP\footnote{{\sc Object Oriented Programming}}, 
RMI\footnote{{\sc Remote Method Invocation}}, 
peer to peer networks, SOA\footnote{{\sc Service Oriented Architectures}} or
cloud computing in chronological order it shows a clear trend twords a 
highly interconnected world where data and code is distributed over a
huge {\it span} of different devices.

In the world of functional programming a function is a first class object.



\begin{code}
data Foo = Bar


\end{code}

\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
