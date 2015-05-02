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
<?php
$conn = mysql_connect('localhost', 'DEvol', 'devol');
mysql_select_db("DEvol");
$query = "DELETE FROM `Suite` WHERE `id`=".$_GET["id"].";";
echo $query;
mysql_query($query);
mysql_close($conn);
?>
<h1>Już!</h1>
<?php
}
else
{
  die("NIE JESTEŚ PEWIEN!");
}
?>
