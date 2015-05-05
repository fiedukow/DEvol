<?php
if (!isset($_GET['id']))
  die("404");
?>
<?php
if (!isset($_POST['potwierdz']))
{
?>
<h1>Usuwasz <a href="suite.php?id=<?php echo $_GET['id']; ?>">cały zestaw testów</a></h1>
<form method="post">
Jesteś pewien: <input type="text" name="potwierdz">
</form>
<?php
}
else if ($_POST['potwierdz']=="tak")
{
?>
<h1>Usuwam...</h1>
<pre>
<?php
$conn = mysql_connect('localhost', 'DEvol', 'devol');
mysql_select_db("DEvol");

$query = "SELECT `id` FROM `Experiment` WHERE `suite_id`=".$_GET["id"];
$res = mysql_query($query);
while($row = mysql_fetch_row($res))
{
  $query = "DROP TABLE `".$row[0]."_Series`;";
  echo $query."\n";
  mysql_query($query);
}


$query = "DELETE FROM `Suite` WHERE `id`=".$_GET["id"].";";
echo $query."\n";
mysql_query($query);
mysql_close($conn);
?>
</pre>
<h1>Już!</h1>
<?php
}
else
{
  die("NIE JESTEŚ PEWIEN!");
}
?>
