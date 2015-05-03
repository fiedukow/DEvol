<?php
if (!isset($_GET['id']))
  die("404");

$conn = mysql_connect('localhost', 'DEvol', 'devol');
mysql_select_db("DEvol");

$query = file_get_contents("./experiment_details.sql");
$query = str_replace("{EX_ID}", $_GET['id'], $query);
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

ul li { display: inline; }

ul {
margin-left: 0px;
padding-left: 0px;
}

iframe {
position: absolute;
float: right;
right: 0;
top: 0;
height: 100%;
width: 50%;
}

div{
position: absolute;
float: lef;
left: 0;
top: 0;
height: 100%;
width: 48%;
padding-left: 1%;
}

</style>
</head>
<body>
<div>
<h1><?php echo "Experiment on ".$details[2]; ?></h1>
<p>
<a href="suite.php?id=<?php echo $details[1]; ?>">&lt;- Back to suite</a>
</p>
<?php
$query =  "SELECT `name`, `value_numeric`, `value_text` FROM `ExperimentParameter` ";
$query .= "WHERE `experiment_id`=".$_GET['id'];
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
<tr><td>Start data</td><td><?php echo date('m/d/Y H:i:s', $details[4]); ?></td></tr>
<tr><td>End data</td><td><?php echo date('m/d/Y H:i:s', $details[5]); ?></td></tr>
<tr><td>Time taken (s.)</td><td><?php echo ($details[5]-$details[4]); ?></td></tr>
<tr><td>AVG BEST</td><td><strong><?php echo sprintf("%.3E", $details[6]); ?></strong></td></tr>
<tr><td>SD BEST</td><td><strong><?php echo sprintf("%.3E", $details[7]); ?></strong></td></tr>
<tr><td>AVG MID</td><td><strong><?php echo sprintf("%.3E", $details[8]); ?></strong></td></tr>
<tr><td>SD MID</td><td><strong><?php echo sprintf("%.3E", $details[9]); ?></strong></td></tr>
</table>
<?php
$query =  "SELECT `id` FROM `Run` WHERE `experiment_id`=".$_GET['id'];
$res = mysql_query($query);
?>
<p>Runs:
<ul>
<?php
$base_run;
while ($run = mysql_fetch_row($res))
{
?>
<li>
<a target="run" href="run.php?id=<?php echo $run[0]; ?>">#<?php echo $run[0]; ?></a>
</li>
<?php
$base_run = $run[0];
}
?>
</li>
</p>
</div>
<iframe src="run.php?id=<?php echo $base_run; ?>" name="run">
</iframe>
</body>
