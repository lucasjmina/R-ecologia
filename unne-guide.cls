\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{unne-guide}[2025 UNNE Course Guide]

\LoadClass[12pt,a4paper]{article}

% ---- Encoding / Language ----
\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage[spanish]{babel}

% ---- Margins & spacing ----
\RequirePackage{geometry}
\geometry{margin=2.5cm}
\RequirePackage{setspace}
\onehalfspacing

% ---- Fonts (Georgia) ----

% ---- Colours ----
\RequirePackage{xcolor}
\definecolor{unneblue}{HTML}{005D8A}
\definecolor{unnegold}{HTML}{C8A100}
\definecolor{headerbg}{HTML}{E6EEF3} % adjust if too pale
\definecolor{codebg}{gray}{0.98}

% ---- Header / Footer ----
\RequirePackage{fancyhdr}
\setlength{\headheight}{15pt}
\pagestyle{fancy}
\fancyhf{}
\fancyhead[L]{\textbf{RES. 2024-803-CD-EXA}}
\fancyhead[R]{\textbf{R aplicado a ecología - 2025}}
\fancyfoot[C]{\thepage}

% ---- Section styling (coloured backgrounds) ----
\RequirePackage{titlesec}
\newcommand{\sectionformat}[1]{%
    \colorbox{headerbg}{%
        \parbox{\dimexpr\linewidth-2\fboxsep}{\thesection\quad #1}
    }%
}
\titleformat{\section}
{\bfseries\Large}
{}
{0pt}
{\sectionformat}
[\vspace{0.5em}]

\titleformat{\subsection}
{\bfseries\large}
{}
{0pt}
{\color{unneblue}}

% ---- Better Title Page ----
\renewcommand{\maketitle}{
  \begin{center}
    {\Huge\bfseries \@title\par}
    \vspace{1cm}
    {\Large \@author\par}
    \vspace{0.2cm}
    {\large \@date\par}
    \vspace{2cm}
    \rule{0.8\textwidth}{1.5pt}\\[0.5cm]
    {\Large\bfseries Guía de práctica}\\[0.2cm]
    {\large Facultad de Ciencias Exactas y Naturales}\\[0.2cm]
    {\large Universidad Nacional del Nordeste}
    \\[1.5cm]
  \end{center}
  \newpage
}
