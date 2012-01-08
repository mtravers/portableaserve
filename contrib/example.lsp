<html>
<head>
<title>
</title>
</head>
<body>

<h1>LSP Example</h1>

<h2>User Agent</h2>
<%=
  (:princ-safe (or (net.aserve:header-slot-value com.lemonodor.lsp:request
						 :user-agent)
		   "None."))
%>

<h2>Referrer</h2>
<%=
  (:princ-safe (or (net.aserve:header-slot-value com.lemonodor.lsp:request
						 :referer)
		   "None."))
%>

<h2>Query Variables</h2>
<table>
<%
(let ((queries (net.aserve:request-query com.lemonodor.lsp:request)))
  (if (null queries)
    (net.html.generator:html (:tr (:td "None.")))
    (dolist (query queries)
      (net.html.generator:html
       (:tr (:td (:princ-safe (car query)))
	    (:td (:princ-safe (cdr query))))))))
%>
</table>

<h2>Loop Of Dynamism</h2>

<% (dotimes (i (+ (random 10) 1)) %>
  Hi!<br>
<% ) %>


</body>

</html>
