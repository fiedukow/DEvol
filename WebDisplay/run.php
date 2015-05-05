<?php
if (!isset($_GET['id']))
  die("404");

$conn = mysql_connect('localhost', 'DEvol', 'devol');
mysql_select_db("DEvol");

$query = "SELECT `experiment_id` FROM `Run` WHERE `id`=".$_GET['id'];
$res = mysql_query($query);
$id_ex = mysql_fetch_row($res)[0];

$query = "SELECT `id`, `start_timestamp`, `end_timestamp` FROM `Run` WHERE `id`=".$_GET['id'];
$res = mysql_query($query);
$details = mysql_fetch_row($res);

$query =  "SELECT MIN(`value_numeric`) FROM `".$id_ex."_Series` WHERE `run_id`=".$_GET["id"]." AND `name`=\"best values\"";
$res = mysql_query($query);
$best_val = mysql_fetch_row($res);

$query =  "SELECT MIN(`value_numeric`) FROM `".$id_ex."_Series` WHERE `run_id`=".$_GET["id"]." AND `name`=\"mid values\"";
$res = mysql_query($query);
$best_mid_val = mysql_fetch_row($res);
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
<h1><?php echo "Run #".$details[0]; ?></h1>
<table>
<tr><th colspan=2>DETAILS</th></tr>
<tr><td>Start data</td><td><?php echo date('m/d/Y H:i:s', $details[1]); ?></td></tr>
<tr><td>End data</td><td><?php echo date('m/d/Y H:i:s', $details[2]); ?></td></tr>
<tr><td>Time taken (s.)</td><td><?php echo ($details[2]-$details[1]); ?></td></tr>
<tr><td>Best value</td><td><?php echo $best_val[0]; ?></td></tr>
<tr><td>Best mid value</td><td><?php echo $best_mid_val[0]; ?></td></tr>
</table>
</body>
