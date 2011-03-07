% ----------------------------------------------------------------------------
% FIT thesis reloaded
% ----------------------------------------------------------------------------
% Based on LaTeX class (C) 2007 Zdeněk Vašíček, 2008 Michal Bidlo
% Modifications: (C) 2009, 2011 Jan Pokorný
% ----------------------------------------------------------------------------
% 2007/04/13 Original version [ZV]
% 2008       [MB]
% 2009       Modification for nicer typesetting (golden ratios etc.) [JP]
% 2011/01/07 Modification to fit XeLaTeX and overall simplification [JP]
% 2011/03/05 Fix undesired spaces, remove quotes relates section [JP]
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{fitthesis}[2011/01/07 Sablona pro BP/DP]

% Builds upon `report' class
\LoadClass[a4paper,oneside,onecolumn,11pt]{report}
% Avoid clashes with some names defined in `report' class
\let\title=\undefined\let\abstract=\undefined
% Required for further class keyword arguments manipulation
\usepackage{keyval}
% For convenient construction of conditionals
\usepackage{ifthen}

%============================================================================
% CLASS OPTIONS

% Defaults for class options
\newif\ifAssignment  \let\ifAssignment \iftrue
\newif\ifCzech       \let\ifCzech      \iftrue
\newif\ifCover       \let\ifCover      \iffalse
\newif\ifDraft       \let\ifDraft      \iffalse
\newif\ifPrint       \let\ifPrint      \iffalse
\newif\ifWis         \let\ifWis        \iftrue

\DeclareOption{draft}{\let\ifDraft\iftrue}
\DeclareOption{english}{\let\ifCzech\iffalse}
\DeclareOption{cover}{\let\ifCover\iftrue}
\DeclareOption{print}{\let\ifAssignment\iffalse
                      \let\ifPrint\iftrue
                      \let\ifWis\iffalse}
\DeclareOption{keepassignment}{\let\ifAssignment\iftrue}

\DeclareOption*{}
\ProcessOptions\relax

%============================================================================
% META-CONSTRUCTS (TODO?)

%
%\newcommand{\thesis@newGetterSetter}[1]{%
%  \def\@tmpname{#1}
%  \newcommand{\@nameuse{{\@tmpname}@set}}[3]{%
%    \expandafter\def \csname {\@tmpname}@#1@#2\endcsname{#3}%
%  }
%  \newcommand{\@nameuse{{\@tmpname}@get}}[2]{\@nameuse{str@#1@#2}}
%}

%============================================================================
% PREFABRICATED STRINGS

% [private] Interface to underlaying storage of prefabricated strings
%           as variables
\newcommand{\str@set}[3]{%
  \expandafter\def \csname str@#1@#2\endcsname{#3}%
}
\newcommand{\str@get}[2]{\@nameuse{str@#1@#2}}


% Headers
\str@set{author}{CS}           {Autor práce}
\str@set{author}{EN}           {Author}
\str@set{department}{CS}       {Ústav}
\str@set{department}{EN}       {Department of}
\str@set{supervisor}{CS}       {Vedoucí práce}
\str@set{supervisor}{EN}       {Supervisor}


% Preface captions
\str@set{assignment}{CS}       {Zadání}
\str@set{assignment}{EN}       {Assignment}
\str@set{assignmentcue}{CS}    {toto nahradit zadáním}
\str@set{assignmentcue}{EN}    {assignment to be inserted here instead}
\str@set{abstract}{CS}         {Abstrakt}
\str@set{abstract}{EN}         {Abstract}
\str@set{keywords}{CS}         {Klíčová slova}
\str@set{keywords}{EN}         {Keywords}
\str@set{cite}{CS}             {Citace}
\str@set{cite}{EN}             {Bibliographic record}
\str@set{declaration}{CS}      {Prohlášení}
\str@set{declaration}{EN}      {Declaration}
\str@set{acknowledgement}{CS}  {Poděkování}
\str@set{acknowledgement}{EN}  {Acknowledgement}

% Preface pages as to be shown in PDF index
\str@set{prefacepages}{CS}     {Úvodní strany}
\str@set{prefacepages}{EN}     {Preface Pages}
\str@set{prefacecover}{CS}     {Desky}
\str@set{prefacecover}{EN}     {Cover}
\str@set{prefacetitle}{CS}     {Titulní strana}
\str@set{prefacetitle}{EN}     {Title Page}
\str@set{prefaceassignment}{CS}{\str@get{assignment}{CS}}
\str@set{prefaceassignment}{EN}{\str@get{assignment}{EN}}
\str@set{prefaceabstract}{CS}{%
    \str@get{abstract}{CS} + \str@get{keywords}{CS} + \str@get{cite}{CS}
}
\str@set{prefaceabstract}{EN}{%
    \str@get{abstract}{EN} + \str@get{keywords}{EN} + \str@get{cite}{CS}
}
\str@set{prefacedeclaration}{CS}{\str@get{declaration}{CS}}
\str@set{prefacedeclaration}{EN}{\str@get{declaration}{EN}}


% Contents captions
\str@set{contents}{CS}         {Obsah}
\str@set{contents}{EN}         {Contents}

% Contents pages as to be shown in PDF index
\str@set{contentspages}{CS}    {Obsah dokumentu}
\str@set{contentspages}{EN}    {Document Contents}
\str@set{contentscontents}{CS} {\str@get{contents}{CS}}
\str@set{contentscontents}{EN} {\str@get{contents}{EN}}


% Bibliography/appendices captions
\str@set{bibliography}{CS}     {Seznam použitých zdrojů}
\str@set{bibliography}{EN}     {Bibliography}
\str@set{listofappendices}{CS} {Seznam příloh}
\str@set{listofappendices}{EN} {List of Appendices}
\str@set{appendix}{CS}         {Příloha}
\str@set{appendix}{EN}         {Appendix}


% Project types enumeration
\str@set{BT}{CS}               {Bakalářská práce}
\str@set{BT}{EN}               {Bachelor's thesis}
\str@set{SP}{CS}               {Semestrální projekt}
\str@set{SP}{EN}               {Term project}
\str@set{MT}{CS}               {Diplomová práce}
\str@set{MT}{EN}               {Master's thesis}
\str@set{PT}{CS}               {Disertační práce}
\str@set{PT}{EN}               {PhD thesis}
%...

%
% University specific (BUT FIT), TODO: split?
%

% Departments
\str@set{UIFS}{CS}             {informačních systémů}
\str@set{UIFS}{EN}             {Information Systems}
\str@set{UPSY}{CS}             {počítačových systémů}
\str@set{UPSY}{EN}             {Computer Systems}
\str@set{UITS}{CS}             {inteligentních systémů}
\str@set{UITS}{EN}             {Intelligent Systems}
\str@set{UPGM}{CS}             {počítačové grafiky a multimédií}
\str@set{UPGM}{EN}             {Computer Graphics and Multimedia}
%...

%---------------------------------------------------
% [public]
\let\strGetIn\str@get

\newcommand{\strGet}[1]{%
  \ifCzech\strGetIn{#1}{CS}%
  \else\strGetIn{#1}{EN}\fi%
}

%============================================================================
% TEMPLATE PARAMETERIZATION

% [public] Interface for passing more document information items at once
\def\setDocInfo{\setkeys{docinfo}}

% [private] Interface to underlaying storage of document information items
%           as variables

% For items that are common for all the languages
\newcommand{\doc@setCommon}[2]{\expandafter\def \csname doc@#1\endcsname{#2}}
\newcommand{\doc@getCommon}[1]{\@ifundefined{doc@#1}{}{\@nameuse{doc@#1}}}
% For items that are language-specific
\newcommand{\doc@setLocalized}[3]{%
  \expandafter\def \csname doc@#1@#2\endcsname{#3}%
}
\newcommand{\doc@getLocalized}[2]{\@nameuse{doc@#1@#2}}

%---------------------------------------------------
% Special handling of enumerated choices

\newif\iffound
\newcommand{\doc@setLocalizedIfStrEq}[4]{
  \def\tmp@testa{#1}\def\tmp@testb{#2}
  \ifx\tmp@testa\tmp@testb \doc@setLocalized{#3}{#2}{#4} \fi
}
\newcommand{\doc@setPrefabricatedIfStrEq}[3]{
  \def\tmp@testa{#1}\def\tmp@testb{#2}
  \ifx\tmp@testa\tmp@testb
    \doc@setLocalized{#3}{CS}{\str@get{#2}{CS}}
    \doc@setLocalized{#3}{EN}{\str@get{#2}{EN}} \foundtrue
  \fi%
}
% Project (type) choice
\newcommand{\doc@setProject}[1]{
  \foundfalse
  \doc@setPrefabricatedIfStrEq{#1}{BT}{project}
  \doc@setPrefabricatedIfStrEq{#1}{SP}{project}
  \doc@setPrefabricatedIfStrEq{#1}{MT}{project}
  \doc@setPrefabricatedIfStrEq{#1}{PT}{project}
  \iffound\else
     \typeout{Bad project type! Choices: BT, SP, MP, PT}
     \doc@setProject{BT}
  \fi
}

%%% Specific for BUT FIT

% Department choice
\newcommand{\doc@setDepartment}[1]{
  \foundfalse
  \doc@setPrefabricatedIfStrEq{#1}{UIFS}{department@part}
  \doc@setPrefabricatedIfStrEq{#1}{UPSY}{department@part}
  \doc@setPrefabricatedIfStrEq{#1}{UITS}{department@part}
  \doc@setPrefabricatedIfStrEq{#1}{UPGM}{department@part}
  \iffound\else
     \typeout{Bad project type! Choices: UPSY, UIFS, UITS, UPGM}
     \doc@setDepartment{UIFS}
  \fi
}

%---------------------------------------------------

% Implementation of the interface above to store passed document information
% items into variables

\define@key{docinfo}{organization.cs}    {\doc@setLocalized{organization}{CS}{#1}}
\define@key{docinfo}{organization.en}    {\doc@setLocalized{organization}{EN}{#1}}
\define@key{docinfo}{organization.logo}  {\doc@setCommon{organization@logo}{#1}}
\define@key{docinfo}{organization.logo.w}{\doc@setCommon{organization@logo@w}{#1}}
\define@key{docinfo}{subsidiary.cs}      {\doc@setLocalized{subsidiary}{CS}{#1}}
\define@key{docinfo}{subsidiary.en}      {\doc@setLocalized{subsidiary}{EN}{#1}}
\define@key{docinfo}{subsidiary.logo}    {\doc@setCommon{subsidiary@logo}{#1}}
\define@key{docinfo}{subsidiary.logo.w}  {\doc@setCommon{subsidiary@logo@w}{#1}}

\define@key{docinfo}{title.cs}           {\doc@setlocalized{title}{CS}{#1}}
\define@key{docinfo}{title.en}           {\doc@setlocalized{title}{EN}{#1}}

\define@key{docinfo}{project}            {\doc@setProject{#1}}
\define@key{docinfo}{year}               {\doc@setCommon{year}{#1}}
\define@key{docinfo}{date}               {\doc@setCommon{date}{#1}}
\define@key{docinfo}{location}           {\doc@setCommon{location}{#1}}

\define@key{docinfo}{title.cs}           {\doc@setLocalized{title}{CS}{#1}}
\define@key{docinfo}{title.en}           {\doc@setLocalized{title}{EN}{#1}}

\define@key{docinfo}{author}             {\doc@setCommon{author@name}{#1}}
\define@key{docinfo}{author.title.a}     {\doc@setCommon{author@ta}{#1}}
\define@key{docinfo}{author.title.p}     {\doc@setCommon{author@tp}{#1}}
\define@key{docinfo}{author.email}       {\doc@setCommon{author@email}{#1}}

\define@key{docinfo}{department}         {\doc@setDepartment{#1}}

\define@key{docinfo}{supervisor}         {\doc@setCommon{supervisor@name}{#1}}
\define@key{docinfo}{supervisor.title.a} {\doc@setCommon{supervisor@ta}{#1}}
\define@key{docinfo}{supervisor.title.p} {\doc@setCommon{supervisor@tp}{#1}}

\define@key{docinfo}{abstract.cs}        {\doc@setLocalized{abstract}{CS}{#1}}
\define@key{docinfo}{abstract.en}        {\doc@setLocalized{abstract}{EN}{#1}}

\define@key{docinfo}{keywords.cs}        {\doc@setLocalized{keywords}{CS}{#1}}
\define@key{docinfo}{keywords.en}        {\doc@setLocalized{keywords}{EN}{#1}}

\define@key{docinfo}{copyright}          {\doc@setCommon{copyright}{#1}}
\define@key{docinfo}{declaration}        {\doc@setCommon{declaration}{#1}}
\define@key{docinfo}{acknowledgement}    {\doc@setCommob{acknowledgement}{#1}}

\define@key{docinfo}{assignment.img}     {\doc@setCommon{assignment@img}{#1}}

% Initially, default data is passed for later overriding from main document
\setDocInfo{
  date                = \today,
  organization.logo.w = 3.5cm,
  subsidiary.logo.w   = 3.5cm,
}

% Composite strings (as lazy evaluated macros)
\doc@setLocalized{department}{CS}{%
  \str@get{department}{CS}\ \doc@getLocalized{department@part}{CS}
}
\doc@setLocalized{department}{EN}{%
  \str@get{department}{EN}\ \doc@getLocalized{department@part}{EN}
}
\doc@setCommon{author}{%
  \ifthenelse{\equal{}{\doc@getCommon{author@tp}}}{}{\doc@getCommon{author@tp}~}%
  \MakeUppercase{\doc@getCommon{author@name}}%
  \ifthenelse{\equal{}{\doc@getCommon{author@ta}}}{}{,~\doc@getCommon{author@ta}}
}
\doc@setCommon{supervisor}{%
  \ifthenelse{\equal{}{\doc@getCommon{supervisor@tp}}}{}{\doc@getCommon{supervisor@tp}~}%
  \MakeUppercase{\doc@getCommon{supervisor@name}}%
  \ifthenelse{\equal{}{\doc@getCommon{supervisor@ta}}}{}{,~\doc@getCommon{supervisor@ta}}
}

%---------------------------------------------------

% [public] Extra setters of some document information items so they can be
%          passed separately
\newcommand{\setDeclaration}[1]{\doc@setCommon{declaration}{#1}}
\newcommand{\setAcknowledgement}[1]{\doc@setCommon{acknowledgement}{#1}}
\newcommand{\setAbstract}[2][CS]{\doc@setLocalized{abstract}{#1}{#2}}
\newcommand{\setKeywords}[2][CS]{\doc@setLocalized{keywords}{#1}{#2}}
\newcommand{\setCopyright}[1]{\doc@setCommon{copyright}{#1}}

% [public] Getters of previously passed information items about document
%          (first requires language spec. [CS, EN], the second makes
%          choice according to the current language used)
\newcommand\localizedDocInfoIn[2]{\doc@getLocalized{#2}{#1}}
\newcommand{\localizedDocInfo}[1]{%
  \ifCzech\localizedDocInfoIn{CS}{#1}%
  \else\localizedDocInfoIn{EN}{#1}\fi%
}
\newcommand{\fullDocAuthor}{%
  \ifx\@undefined\doc@getCommon{author@tp}\else\doc@getCommon{author@tp}~\fi%
  \doc@getCommon{author@name}%
  % unfortunately, `ifthenelse' does not work here (?)
  % FIXME: temporarily commented out to supress additional comma
  %\ifx\@undefined\doc@getCommon{author@ta}\else,~\doc@getCommon{author@ta}\fi%
  \ifx\@undefined\doc@getCommon{author@email}\else\ <\doc@getCommon{author@email}>\fi
}


%============================================================================
% COMMON MACROS

\newlength{\@mkspc}
\setlength{\@mkspc}{2pt}

\def\setfontsize#1{%
  \renewcommand{\baselinestretch}{1.5}%
  \fontsize{#1}{\f@baselineskip}\selectfont%
}
%text
\newcommand{\@mktxt}[2]{\vbox{\setfontsize{#1}\noindent#2}\vspace\@mkspc}
%bold
\newcommand{\@mktxt@b}[2]{\vbox{\setfontsize{#1}\noindent\bf#2}\vspace\@mkspc}
\newcommand{\@mktxt@uc}[2]{%
  \vbox{\setfontsize{#1}\noindent\MakeUppercase{#2}}\vspace\@mkspc%
}

% Use to count right margin for author and supervisor
\newsavebox{\@doc@supervisor}
\newlength{\@doc@supervisor@width}\newlength{\@doc@names@width}
\savebox{\@doc@supervisor}{%
  \MakeUppercase{\large{\textsf{\str@get{supervisor}{CS}}}}%
}
\settowidth{\@doc@supervisor@width}{\makebox{\usebox{\@doc@supervisor}}}
\setlength{\@doc@names@width}{\textwidth}
\addtolength{\@doc@names@width}{-\@doc@supervisor@width}


%============================================================================
% COVER PAGE, \prefacecover

\newcommand\prefacecover{\thispagestyle{empty}\sffamily%
%---------------------------------------------------
  \@mktxt@uc{24}{\doc@getLocalized{organization}{CS}}
  \vspace{0.4mm}
  \@mktxt@uc{11}{\doc@getLocalized{organization}{EN}}

  \vspace{13.6mm}

  \@mktxt@uc{14}{\doc@getLocalized{subsidiary}{CS}}
  \vspace{0.4mm}
  \@mktxt@uc{14}{\doc@getLocalized{department}{CS}}

  \vspace{3mm}

  \@mktxt@uc{11}{\doc@getLocalized{subsidiary}{EN}}
  \vspace{-0.72mm}
  \@mktxt@uc{11}{\doc@getLocalized{department}{EN}}

  \vspace{\stretch{0.382}}

  \@mktxt@uc{18}{\localizedDocInfo{title}} % In original language

  \vspace{\stretch{0.618}}

  \@mktxt@uc{14}{\doc@getLocalized{project}{CS}}
  \@mktxt@uc{10}{\doc@getLocalized{project}{EN}}

  \vspace{6mm}

  \@mktxt{14}{%
    \MakeUppercase{\str@get{author}{CS}}
    \hfill
    \doc@getCommon{author}\hspace*{0.145924\@doc@names@width}
  }
  \@mktxt@uc{10}{\str@author@EN}

  \vspace{16.32mm}

  \@mktxt{11}{\MakeUppercase{\doc@getCommon{location}} \doc@getCommon{year}}
%---------------------------------------------------
  \clearpage
}

%============================================================================
% TITLE PAGE, \prefacetitle

\newcommand\prefacetitle{\thispagestyle{empty}\sffamily%
%
\newsavebox{\@uni@logo}\newsavebox{\@uni@text}
\newlength{\@uni@logo@height}\newlength{\@uni@text@height}
\savebox{\@uni@logo}{%
  \ifthenelse{\equal{}{\doc@getCommon{organization@logo}}}{}{%
    \includegraphics[width=\doc@getCommon{organization@logo@w}cm
                    ,keepaspectratio
                    ]{\doc@getCommon{organization@logo}}
    \hspace{10pt}
  }%
}
\savebox{\@uni@text}{\vbox{%
  \@mktxt@uc{18}{\doc@getLocalized{organization}{CS}}
  \vspace{0.4mm}
  \@mktxt@uc{11}{\doc@getLocalized{organization}{EN}}
}}
\settoheight{\@uni@logo@height}{\makebox{\usebox{\@uni@logo}}}
\settoheight{\@uni@text@height}{\makebox{\usebox{\@uni@text}}}
\addtolength{\@uni@logo@height}{-\@uni@text@height}
\addtolength{\@uni@logo@height}{-0.1mm}
%
\newsavebox{\@fac@logo}\newsavebox{\@fac@text}
\newlength{\@fac@logo@height}\newlength{\@fac@text@height}
\savebox{\@fac@logo}{%
  \ifthenelse{\equal{}{\doc@getCommon{subsidiary@logo}}}{}{%
    \includegraphics[width=\doc@getCommon{organization@logo@w}cm
                    ,keepaspectratio
                    ]{\doc@getCommon{subsidiary@logo}}
    \hspace{10pt}
  }%
}
\savebox{\@fac@text}{\vbox{%
  \@mktxt@uc{18}{\doc@getLocalized{subsidiary}{CS}}
  \vspace{0.4mm}
  \@mktxt@uc{11}{\doc@getLocalized{subsidiary}{EN}}
}}
\settoheight{\@fac@logo@height}{\makebox{\usebox{\@fac@logo}}}
\settoheight{\@fac@text@height}{\makebox{\usebox{\@fac@text}}}
\addtolength{\@fac@logo@height}{-\@fac@text@height}
\addtolength{\@fac@logo@height}{-0.1mm}
%---------------------------------------------------
  \noindent
  \usebox{\@uni@logo}\raisebox{\@uni@logo@height}{\usebox{\@uni@text}}

  \vspace{6mm}

  \noindent
  \usebox{\@fac@logo}\raisebox{\@fac@logo@height}{\usebox{\@fac@text}}

  \vspace{\stretch{0.382}}

  \@mktxt@uc{18}{\doc@getLocalized{title}{CS}}
  \@mktxt@uc{10}{\doc@getLocalized{title}{EN}}

  \vspace{\stretch{0.618}}

  \@mktxt@uc{14}{\doc@getLocalized{project}{CS}}
  \@mktxt@uc{10}{\doc@getLocalized{project}{EN}}

  \vspace{6mm}

  \@mktxt{14}{%
    \MakeUppercase{\str@get{author}{CS}}
    \hfill
    \doc@getCommon{author}\hspace*{0.145924\@doc@names@width}
  }
  \@mktxt@uc{10}{\str@get{author}{EN}}

  \vspace{6mm}

  \@mktxt{14}{%
    \MakeUppercase{\str@get{supervisor}{CS}}
    \hfill
    \doc@getCommon{supervisor}\hspace*{0.145924\@doc@names@width}
  }
  \@mktxt@uc{10}{\str@get{supervisor}{EN}}

  \vspace{12.24mm}

  \@mktxt{11}{\MakeUppercase{\doc@getCommon{location}} \doc@getCommon{year}}
%---------------------------------------------------
  \clearpage
}

%============================================================================
% ASSIGNMENT, \prefaceassignment

\newcommand\prefaceassignment{\thispagestyle{empty}\rmfamily%
%---------------------------------------------------
  \ifWis
      \ifthenelse{\equal{}{\doc@getCommon{assignment@img}}}{
        \typeout{Assignment image not found!}
        \noindent\framebox[\textwidth]{\Huge{\textsl\textbf{Assignment image not found!}}}
      }{
        \noindent\hspace*{\fill}\\
        \vspace{\stretch{0.382}}
        \noindent
        \includegraphics[width=\textwidth, keepaspectratio]{\doc@getCommon{assignment@img}}\\
        \vspace{\stretch{0.618}}
        \hspace*{\fill}
      }
  \else
    \ifAssignment
      \ifthenelse{\equal{}{\doc@getCommon{assignment@img}}}{
        \typeout{Assignment image not found!}
        \noindent\framebox[\textwidth]{\Huge{\textsl\textbf{Assignment image not found!}}}
      }{
        \noindent\hspace*{\fill}\\
        \vspace{\stretch{0.382}}
        \noindent
        \includegraphics[width=\textwidth, keepaspectratio]{\doc@getCommon{assignment@img}}\\
        \vspace{\stretch{0.618}}
        \hspace*{\fill}
      }
    \else
      \noindent
      \framebox[\textwidth]{\Large{\slshape{}\str@get{assignmentcue}{CS}}}

      \vspace{\stretch{0.382}}

      \begin{center}
      \noindent\framebox[\textwidth]{\Huge{\slshape\bfseries{}\str@get{assignmentcue}{CS}}}
      \end{center}

      \vspace{\stretch{0.618}}

      \noindent\framebox[\textwidth]{\Large{\slshape{}\str@get{assignmentcue}{CS}}}
    \fi
  \fi
%---------------------------------------------------
  \clearpage
}

%============================================================================
% ABSTRACT+KEYWORDS PAGE, \prefaceabstract

\newcommand\prefaceabstract{\thispagestyle{empty}\rmfamily%
%---------------------------------------------------
  \@mktxt@b{16}{\str@get{abstract}{CS}}
  \vspace{0.25\headsep}
  \noindent
  \doc@getLocalized{abstract}{CS}

  \vfill

  \@mktxt@b{16}{\str@get{abstract}{EN}}
  \vspace{0.25\headsep}
  \noindent
  \doc@getLocalized{abstract}{EN}

  \vfill

  \@mktxt@b{16}{\str@get{keywords}{CS}}
  \vspace{0.25\headsep}
  \noindent
  \doc@getLocalized{keywords}{CS}

  \vfill

  \@mktxt@b{16}{\str@get{keywords}{EN}}
  \vspace{0.25\headsep}
  \noindent
  \doc@getLocalized{keywords}{EN}

  \vfill

  \@mktxt@b{16}{\str@get{cite}{CS}}
  \vspace{0.25\headsep}
  \noindent
  \doc@getCommon{author@name}: \localizedDocInfo{title},
  \MakeLowercase{\doc@getLocalized{project}{CS}}, Brno,
  FIT VUT v~Brně, \doc@getCommon{year}
%---------------------------------------------------
  \clearpage
}

%============================================================================
% DECLARATION+ACKNOWLEDGEMENT PAGE, \prefacedeclaration

\newcommand\prefacedeclaration{\thispagestyle{empty}\rmfamily%
%---------------------------------------------------
  \@mktxt@b{18}{\localizedDocInfo{title}}

  \vspace{1.5\headsep}
  \@mktxt@b{16}{\str@get{declaration}{CS}}
  \vspace{0.25\headsep}
  \noindent
  \doc@getCommon{declaration}
  \vspace{0.75\headsep}
  \begin{flushright}
    \shortstack[c]{%
      \makebox[4cm]{\dotfill}\\[0.25\headsep]
      \doc@getCommon{author@name}\\
      \doc@getCommon{date}}
  \end{flushright}

  \ifthenelse{\equal{}{\doc@getCommon{acknowledgement}}}{
    \vfill
  }{
    \vspace{\stretch{0.382}}

    \@mktxt@b{16}{\str@get{acknowledgement}{CS}}
    \vspace{0.25\headsep}
    \noindent\doc@getCommon{acknowledgement}

    \vspace{\stretch{0.618}}
  }

  \noindent
  \copyright\ \doc@getCommon{author@name}, \doc@getCommon{year}.
  \newline
  \emph{\doc@getCommon{copyright}}
%---------------------------------------------------
  \clearpage
}

\endinput
