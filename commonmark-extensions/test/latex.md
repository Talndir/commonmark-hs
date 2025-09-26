```````````````````````````````` example
Para

Para again
.
\noindent Para

Para again

````````````````````````````````

```````````````````````````````` example
Hello *darkness* my old _friend_
I've **come** to speak with __you__ again
.
\noindent Hello \textit{darkness} my old \textit{friend}
I've \textbf{come} to speak with \textbf{you} again

````````````````````````````````

```````````````````````````````` example
Let $x$ and $y$ be integers such that
$$x = y + 2$$
.
\noindent Let $x$ and $y$ be integers such that
\[
x = y + 2
\]

````````````````````````````````

```````````````````````````````` example
```lang
this is code
```
.
\begin{spec}
this is code
\end{spec}

````````````````````````````````

```````````````````````````````` example
`this is also code`
.
\noindent |this is also code|

````````````````````````````````

```````````````````````````````` example
`{lang}this should not say "lang"`
.
\noindent |this should not say "lang"|

````````````````````````````````

```````````````````````````````` example
Here is a list
* Item one
* Item two
* Item three
.
\noindent Here is a list

\begin{itemize}
\item{Item one}
\item{Item two}
\item{Item three}
\end{itemize}

````````````````````````````````

```````````````````````````````` example
# Section
## Subsection
### Subsubsection
#### Paragraph
##### Subparagraph
.
\section{Section}

\subsection{Subsection}

\subsubsection{Subsubsection}

\paragraph{Paragraph}

\subparagraph{Subparagraph}

````````````````````````````````

```````````````````````````````` example
[[PhD/Thesis/blah/file.md|Whatever]]
.
\noindent \Cref{blah/file.md}

````````````````````````````````

```````````````````````````````` example
[[Paper|paperref1920]]
.
\noindent \cite{paperref1920}

````````````````````````````````

```````````````````````````````` example
[[Anything|Whatever]]
.
\noindent Whatever

````````````````````````````````