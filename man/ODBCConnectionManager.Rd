% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manager_DatabaseManagers.R
\name{ODBCConnectionManager}
\alias{ODBCConnectionManager}
\title{R6 Class to manage ODBC Database connections}
\description{
Manage ODBC Database connections
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{connection_open}}{- logical - whether the connection is currently is open or not}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ODBCConnectionManager-new}{\code{ODBCConnectionManager$new()}}
\item \href{#method-ODBCConnectionManager-connect}{\code{ODBCConnectionManager$connect()}}
\item \href{#method-ODBCConnectionManager-disconnect}{\code{ODBCConnectionManager$disconnect()}}
\item \href{#method-ODBCConnectionManager-clone}{\code{ODBCConnectionManager$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ODBCConnectionManager-new"></a>}}
\if{latex}{\out{\hypertarget{method-ODBCConnectionManager-new}{}}}
\subsection{Method \code{new()}}{
Create a new instance of ODBCConnectionManager object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ODBCConnectionManager$new(conn_args)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{conn_args}}{list - list of connection arguments to connect to database}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new `ODBCConnectionManager` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ODBCConnectionManager-connect"></a>}}
\if{latex}{\out{\hypertarget{method-ODBCConnectionManager-connect}{}}}
\subsection{Method \code{connect()}}{
Connect to target database
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ODBCConnectionManager$connect()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
none
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ODBCConnectionManager-disconnect"></a>}}
\if{latex}{\out{\hypertarget{method-ODBCConnectionManager-disconnect}{}}}
\subsection{Method \code{disconnect()}}{
Disconnect from target database
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ODBCConnectionManager$disconnect()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
none
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ODBCConnectionManager-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ODBCConnectionManager-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ODBCConnectionManager$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
