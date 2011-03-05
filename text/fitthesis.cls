% ----------------------------------------------------------------------------
% FIT thesis
% ----------------------------------------------------------------------------
% Based on LaTeX class (C) 2007 Zdeněk Vašíček, 2008 Michal Bidlo
% Modifications: (C) 2009, 2011 Jan Pokorný
% ----------------------------------------------------------------------------
% 2007/04/13 Original version [ZV]
% 2008       [MB]
% 2009       Modification for nicer typesetting (golden ratios etc.) [JP]
% 2011/01/07 Modification to fit XeLaTeX and overall simplification [JP]
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{fitthesis}[2011/01/07 Sablona pro BP/DP]

% Builds upon `report' class
\LoadClass[a4paper,oneside,onecolumn,11pt]{report}
% Required for further class keyword arguments manipulation
\usepackage{keyval}
% For convenient construction of conditionals
\usepackage{ifthen}

% New file writer for `writeLocalizedDocInfo' and `writeLocalizedIn'
\newwrite\doc@tempfile

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
% META-CONSTRUCTS

%
%\newcommand{\fitthesis@newGetterSetter}[1]{%
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


\str@set{uni}{CS}              {Vysoké učení technické v~Brně}
\str@set{uni}{EN}              {Brno University of Technology}
\str@set{uni}{logo}            {fig/req/vut-zp2}

\str@set{fac}{CS}              {Fakulta informačních technologií}
\str@set{fac}{EN}              {Faculty of Information Technology}
\str@set{fac}{logo}            {fig/req/fit-zp2}

\str@set{author}{CS}           {Autor práce}
\str@set{author}{EN}           {Author}

\str@set{department}{CS}       {Ústav}
\str@set{department}{EN}       {Department of}

\str@set{supervisor}{CS}       {Vedoucí práce}
\str@set{supervisor}{EN}       {Supervisor}

\str@set{abstract}{CS}         {Abstrakt}
\str@set{abstract}{EN}         {Abstract}

\str@set{keywords}{CS}         {Klíčová slova}
\str@set{keywords}{EN}         {Keywords}

\str@set{cite}{CS}             {Citace}
\str@set{copyright}{CS}        {Tato práce vznikla jako školní dílo na Vysokém
                                učení technickém v Brně, Fakultě informačních
                                technologií. Práce je chráněna autorským
                                zákonem a její užití bez udělení oprávnění
                                autorem je nezákonné, s výjimkou zákonem
                                definovaných případů.
                               }
\str@set{declaration}{CS}      {Prohlášení}
\str@set{acknowledgement}{CS}  {Poděkování}

\str@set{assignmentpage}{CS}   {zde vložit zadání}

\str@set{BP}{CS}               {Bakalářská práce}
\str@set{BP}{EN}               {Bachelor's thesis}
\str@set{SP}{CS}               {Semestrální projekt}
\str@set{SP}{EN}               {Term project}
\str@set{DP}{CS}               {Diplomová práce}
\str@set{DP}{EN}               {Master's thesis}
\str@set{DR}{CS}               {Disertační práce}
\str@set{DR}{EN}               {PhD thesis}

%%% Specific for BUT FIT

\str@set{UIFS}{CS}             {informačních systémů}
\str@set{UIFS}{EN}             {Information Systems}
\str@set{UPSY}{CS}             {počítačových systémů}
\str@set{UPSY}{EN}             {Computer Systems}
\str@set{UITS}{CS}             {inteligentních systémů}
\str@set{UITS}{EN}             {Intelligent Systems}
\str@set{UPGM}{CS}             {počítačové grafiky a multimédií}
\str@set{UPGM}{EN}             {Computer Graphics and Multimedia}


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
\def\doc@setLocalizedIfStrEq#1#2#3#4{\def\tmp@testa{#1}\def\tmp@testb{#2}%
  \ifx\tmp@testa\tmp@testb \doc@setLocalized{#3}{#2}{#4} \fi%
}
\def\doc@setPrefabricatedIfStrEq#1#2#3{\def\tmp@testa{#1}\def\tmp@testb{#2}%
  \ifx\tmp@testa\tmp@testb
    \doc@setLocalized{#3}{CS}{\str@get{#2}{CS}}
    \doc@setLocalized{#3}{EN}{\str@get{#2}{EN}} \foundtrue
  \fi%
}
% Project (type) choice
\newcommand{\doc@setProject}[1]{
  \foundfalse
  \doc@setPrefabricatedIfStrEq{#1}{BP}{project}
  \doc@setPrefabricatedIfStrEq{#1}{SP}{project}
  \doc@setPrefabricatedIfStrEq{#1}{DP}{project}
  \doc@setPrefabricatedIfStrEq{#1}{DR}{project}
  \iffound\else
     \typeout{Bad project type! Choices: BP, SP, DP, DR}
     \doc@setProject{BP}
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

\define@key{docinfo}{declaration}        {\doc@setCommon{declaration}{#1}}
\define@key{docinfo}{acknowledgement}    {\doc@setCommob{acknowledgement}{#1}}

\define@key{docinfo}{assignment.img}     {\doc@setCommon{assignment@img}{#1}}

% Initially, default data is passed for later overriding from main document
\setDocInfo{
  project     = BP,
  date        = \today,
  location    = Brno,
  title.cs    = Název práce (česky),
  title.en    = Název práce (anglicky),
  author      = Jmeno Prijmeni,
  department  = UIFS,
  supervisor  = Jmeno Prijmeni,
  abstract.cs = ,
  abstract.en = ,
  keywords.cs = ,
  keywords.en = ,
  declaration = ,
}

% Composite strings
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

% Quotation marks

%\def\ifundefined#1{\expandafter\ifx\csname#1\endcsname\relax }%
%
%\global\chardef\f@@r=4
%
%\ifundefined{bq}%
%\gdef\bq{\kern-.25\fontdimen\f@@r\font,\kern-.8\fontdimen\f@@r\font,%
%                \kern-.35\fontdimen\f@@r\font}%
%\fi
%\ifundefined{eq}%
%\gdef\eq{\kern-.35\fontdimen\f@@r\font`\kern-.8\fontdimen\f@@r\font`%
%                \kern-.25\fontdimen\f@@r\font}
%\fi
%
%\def\uv#1{\ifCzech {„#1“} \else{“#1”} \fi}
%\catcode`\"=\active
%\def"{\bgroup \ifCzech {„\def"{“\egroup}} {“\def"{”\egroup}}\fi}


%============================================================================
% COVER PAGE, \makeCover

\ifCover
  \newcommand\makeCover{\thispagestyle{empty}\sffamily%
%---------------------------------------------------
  \@mktxt@uc{24}{\str@get{uni}{CS}}
  \vspace{0.4mm}
  \@mktxt@uc{11}{\str@get{uni}{EN}}
  
  \vspace{13.6mm}
  
  \@mktxt@uc{14}{\str@get{fac}{CS}}
  \vspace{0.4mm}
  \@mktxt@uc{14}{\doc@getLocalized{department}{CS}}
  
  \vspace{3mm}
  
  \@mktxt@uc{11}{\str@get{fac}{EN}}
  \vspace{-0.72mm}
  \@mktxt@uc{11}{\doc@getLocalized{department}{EN}}
  
  \vspace{\stretch{0.382}}
  
  \@mktxt@uc{18}{\localizedDocInfo{title}} % In original language
  
  \vspace{\stretch{0.618}}
  
  \@mktxt@uc{14}{\doc@getLocalized{project}{CS}}
  \@mktxt@uc{10}{\doc@getLocalized{project}{EN}}
  
  \vspace{6mm}
  
  \@mktxt{14}{
    \MakeUppercase{\str@get{author}{CS}}
    \hfill
    \doc@getCommon{author}\hspace*{0.145924\@doc@names@width}
  }
  \@mktxt@uc{10}{\str@author@EN}
  
  \vspace{16.32mm}
  
  \@mktxt{11}{\MakeUppercase{\doc@getCommon{location}} \doc@getCommon{year}}
%---------------------------------------------------
  \newpage}
\else
  \newcommand\makeCover{}
\fi

%============================================================================
% TITLE PAGE, \makeTitle

\ifDraft
  \newcommand\makeTitle{}
\else
  \newcommand\makeTitle{\thispagestyle{empty}\sffamily%
%
\newsavebox{\@uni@logo}\newsavebox{\@uni@text}
\newlength{\@uni@logo@height}\newlength{\@uni@text@height}
\savebox{\@uni@logo}{%
  \includegraphics[width=3.5cm,keepaspectratio]{\str@get{uni}{logo}}%
}
\savebox{\@uni@text}{\vbox{\@mktxt@uc{18}{%
  \str@get{uni}{CS}}\vspace{0.4mm}\@mktxt@uc{11}{\str@get{uni}{EN}}}%
}
\settoheight{\@uni@logo@height}{\makebox{\usebox{\@uni@logo}}}
\settoheight{\@uni@text@height}{\makebox{\usebox{\@uni@text}}}
\addtolength{\@uni@logo@height}{-\@uni@text@height}
\addtolength{\@uni@logo@height}{-0.1mm}
%
\newsavebox{\@fac@logo}\newsavebox{\@fac@text}
\newlength{\@fac@logo@height}\newlength{\@fac@text@height}
\savebox{\@fac@logo}{%
  \includegraphics[width=3.5cm,keepaspectratio]{\str@get{fac}{logo}}%
}
\savebox{\@fac@text}{\vbox{\@mktxt@uc{18}{%
  \str@get{fac}{CS}}\vspace{0.4mm}\@mktxt@uc{11}{\str@get{fac}{EN}}}%
}
\settoheight{\@fac@logo@height}{\makebox{\usebox{\@fac@logo}}}
\settoheight{\@fac@text@height}{\makebox{\usebox{\@fac@text}}}
\addtolength{\@fac@logo@height}{-\@fac@text@height}
\addtolength{\@fac@logo@height}{-0.1mm}
%---------------------------------------------------
  \noindent
  \usebox{\@uni@logo}\raisebox{\@uni@logo@height}{\hspace{10pt}
  \usebox{\@uni@text}}
  
  \vspace{6mm}
  
  \noindent
  \usebox{\@fac@logo}\raisebox{\@fac@logo@height}{\hspace{10pt}
  \usebox{\@fac@text}}
  
  \vspace{\stretch{0.382}}
  
  \@mktxt@uc{18}{\doc@getLocalized{title}{CS}}
  \@mktxt@uc{10}{\doc@getLocalized{title}{EN}}
  
  \vspace{\stretch{0.618}}
  
  \@mktxt@uc{14}{\doc@getLocalized{project}{CS}}
  \@mktxt@uc{10}{\doc@getLocalized{project}{EN}}
  
  \vspace{6mm}
  
  \@mktxt{14}{
    \MakeUppercase{\str@get{author}{CS}}
    \hfill
    \doc@getCommon{author}\hspace*{0.145924\@doc@names@width}
  }
  \@mktxt@uc{10}{\str@get{author}{EN}}
  
  \vspace{6mm}
  
  \@mktxt{14}{\MakeUppercase{
    \str@get{supervisor}{CS}}
    \hfill
    \doc@getCommon{supervisor}\hspace*{0.145924\@doc@names@width}
  }
  \@mktxt@uc{10}{\str@get{supervisor}{EN}}
  
  \vspace{12.24mm}
  
  \@mktxt{11}{\MakeUppercase{\doc@getCommon{location}} \doc@getCommon{year}}
%---------------------------------------------------
  \newpage}
\fi

%============================================================================
% ASSIGNMENT, \makeAssignment

\ifDraft
  \newcommand\makeAssignment{}
\else
  \newcommand\makeAssignment{\thispagestyle{empty}\rmfamily%
%---------------------------------------------------
  \ifWis
    \ifx\doc@getCommon{assignment@img}\undefined
    \else
      \noindent\hspace*{\fill}\\
      \vspace{\stretch{0.382}}
      \noindent
      \includegraphics[width=\textwidth, keepaspectratio]{\doc@getCommon{assignment@img}}\\
      \vspace{\stretch{0.618}}
      \hspace*{\fill}
    \fi
  \else
    \ifAssignment
      \ifx\doc@getCommon{assignment@img}\undefined
      \else
        \noindent\hspace*{\fill}\\
        \vspace{\stretch{0.382}}
        \noindent
        \includegraphics[width=\textwidth, keepaspectratio]{\doc@getCommon{assignment@img}}\\
        \vspace{\stretch{0.618}}
        \hspace*{\fill}
      \fi
    \else
      \noindent
      \framebox[\textwidth]{\Large{\slshape{}\str@get{assignmentpage}{CS}}}

      \vspace{\stretch{0.382}}

      \begin{center}
      \noindent\framebox[\textwidth]{\Huge{\slshape\bfseries{}\str@get{assignmentpage}{CS}}}
      \end{center}

      \vspace{\stretch{0.618}}

      \noindent\framebox[\textwidth]{\Large{\slshape{}\str@get{assignmentpage}{CS}}}
    \fi
  \fi
%---------------------------------------------------
  \newpage}
\fi

%============================================================================
% ABSTRACT+KEYWORDS PAGE, \makeAbstract

\ifDraft
  \newcommand\makeAbstract{}
\else
  \newcommand\makeAbstract{\thispagestyle{empty}\rmfamily%
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
  \newpage}
\fi

%============================================================================
% DECLARATION+ACKNOWLEDGEMENT PAGE, \makeDeclaration

\ifDraft
  \newcommand\makeDeclaration{}
\else
  \newcommand\makeDeclaration{\thispagestyle{empty}\rmfamily%
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
  \emph{\str@get{copyright}{CS}}
%---------------------------------------------------
  \newpage}
\fi

%============================================================================
% PRINT OUT COMMANDS

\newcommand\writeLocalizedDocInfo[2]{
  \immediate\openout\doc@tempfile=#2
  \immediate\write\doc@tempfile{\localizedDocInfo{#1}}
  \immediate\closeout\doc@tempfile
}

\newcommand\writeLocalizedDocInfoIn[3]{
  \immediate\openout\doc@tempfile=#3
  \immediate\write\doc@tempfile{\localizedDocInfoIn{#1}{#2}}
  \immediate\closeout\doc@tempfile
}

\endinput
