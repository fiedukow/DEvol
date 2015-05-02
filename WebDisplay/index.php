<html>
<head>
</head>
<body>
<p>
List of suites:
<ul>
<?php
$conn = mysql_connect('localhost', 'DEvol', 'devol');
mysql_select_db("DEvol");
$query = "SELECT `id`, `name` FROM `Suite`";
$res = mysql_query($query);
while ($row = mysql_fetch_row($res)) 
{
?>
<li><a href="suite.php?id=<?php echo $row[0]; ?>"><?php echo $row[1]; ?></a></li>
<?php
}
mysql_close($conn);
?>
</ul>
</p>
</body>
</html>
