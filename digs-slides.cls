% Copyright 2013 Aaron Ecay
% Available under the Creative Commons AT-SA or GPL v2+ licenses: see
% the file LICENSE for more information

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{digs-slides}[2011/05/24]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{beamer}}

\ProcessOptions\relax
\LoadClass[xcolor=svgnames,compress,professionalfonts,ignorenonframetext]{beamer}

%%%%% Misc Packages

\RequirePackage{etex}

\RequirePackage{amssymb} % For \varnoting
\RequirePackage[english]{babel}
\RequirePackage{booktabs}
\RequirePackage{fixltx2e}
\RequirePackage{tikz}
\usetikzlibrary{shapes,arrows,decorations.markings}
\RequirePackage{tikz-qtree}
\RequirePackage{multicol}
\RequirePackage{csquotes} % biblatex gets whiny without it


\RequirePackage{expex}
\resetcountonoverlays{excnt}    % Play nicely with Beamer

\RequirePackage{graphicx}
\DeclareGraphicsExtensions{.pdf,.jpg,.png}

%%%%% References

\usepackage[hyperref=true,bibstyle=authoryear,%
citestyle=authoryear-comp,backend=biber,%
useprefix=true,backend=biber,dashed=false]{biblatex}

\renewcommand{\subtitlepunct}{:}
\renewcommand{\intitlepunct}{ }
\DeclareFieldFormat{url}{\href{#1}{#1}}
\DeclareFieldFormat[article]{pages}{#1}
\DeclareFieldFormat[inproceedings]{pages}{\lowercase{pp.}#1}
\DeclareFieldFormat[incollection]{pages}{\lowercase{pp.}#1}
\DeclareFieldFormat[article]{volume}{\textbf{#1}}
\DeclareFieldFormat[article]{number}{#1}
\DeclareFieldFormat[article]{title}{\MakeCapital{#1}}
\renewbibmacro{in:}{%
  \ifentrytype{article}{}{%
  \printtext{\bibstring{in}\intitlepunct}}}

%%%%%

\RequirePackage{hyperref}
\hypersetup{colorlinks,urlcolor=black,linkcolor=black}


%%%%% Fonts

\RequirePackage{fontspec}
\defaultfontfeatures{Mapping=tex-text,Numbers=OldStyle}
\setmainfont{Linux Libertine}
\setsansfont{Linux Biolinum}
\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\RequirePackage{microtype}

%%%%% Slides
\setbeamertemplate{navigation symbols}{}
\useoutertheme[subsection=false]{miniframes}
\useinnertheme{default}
\usecolortheme{seahorse}
\usecolortheme{rose}

\setbeamercolor{bibliography entry author}{use=normal text,fg=normal text.fg}
\setbeamercolor{bibliography entry location}{use=normal text,fg=normal text.fg}
\setbeamercolor{bibliography entry note}{use=normal text,fg=normal text.fg}

% from http://tex.stackexchange.com/questions/55806/mindmap-tikzpicture-in-beamer-reveal-step-by-step?lq=1
\tikzset{
    invisible/.style={opacity=0},
    visible on/.style={alt={#1{}{invisible}}},
    alt/.code args={<#1>#2#3}{%
        \alt<#1>{\pgfkeysalso{#2}}{\pgfkeysalso{#3}}
    },
}
