<?php
if (!isset($_GET['id']))
  die("404");

$conn = mysql_connect('localhost', 'DEvol', 'devol');
mysql_select_db("DEvol");

$query = "SELECT `id`, `name`, `description` FROM `Suite` WHERE `id`=".$_GET['id'];
$res = mysql_query($query);
$details = mysql_fetch_row($res);
?>

<html>
<head>
<style>
table {
    border-collapse: collapse;
}

table, th, td {
    border: 1px solid black;
}
</style>
</head>
<body>
<h1><?php echo $details[1]; ?></h1>
<p><a href="index.php">&lt;- back to suite selection</a></p>
<p>
<?php echo $details[2]; ?>
</p>
<?php
$query = "SELECT `name`, `value_numeric`, `value_text` FROM `SuiteParameter` WHERE `suite_id`=".$_GET['id'];
$res = mysql_query($query);
?>
<table>
<tr><th colspan=2>DETAILS</th></tr>
<?php
while ($params = mysql_fetch_row($res))
{
?>
<tr><td><?php echo $params[0]; ?></td><td><?php echo ($params[2]=="" ? $params[1] : $params[2]) ?></td></tr>
<?php
}
?>
</table>
<p>
List of experiments in suite:
<?php
$query = "SELECT `id` FROM `Experiment` WHERE `suite_id`=".$_GET['id'];
$res = mysql_query($query);
?>
<table>
<tr><th>id</th><th>time</th><th>fitness function</th><th>AVG BEST</th><th>SD BEST</th>
    <th>AVG MID</th><th>SD MID</th></tr></tr>
<?php
while ($exp = mysql_fetch_row($res))
{
// Ye, ye, I know. I just don't care.
$query = file_get_contents("./experiment_details.sql");
$query = str_replace("{EX_ID}", $exp[0], $query);
$rin = mysql_query($query);
$info = mysql_fetch_row($rin);
?>
<tr>
<td><a href="experiment.php?id=<?php echo $info[0]; ?>"><?php echo $info[0]; ?></a></td>
<td><cite><?php echo date('m/d/Y H:s', $info[3]); ?></cite></td>
<td><?php echo $info[2]; ?></td>
<td><?php echo $info[5]; ?></td>
<td><?php echo $info[6]; ?></td>
<td><?php echo $info[7]; ?></td>
<td><?php echo $info[8]; ?></td>
</li>
<?php
}
?>
</table>
</p>
<p style="margin-top: 200px;"><a style="color: red" href="del_suite.php?id=<?php echo $_GET['id']; ?>">USUŃ</a></p>
</body>
</html>

<?php
mysql_close($conn);
?>

