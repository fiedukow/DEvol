<a href="index.php">Select experiment</a>
<a href="display_i.php?name=<?php echo $_GET["name"]; ?>&i=<?php echo $_GET["i"] - 1;?>">&lt;&lt;</a>
<a href="display_i.php?name=<?php echo $_GET["name"]; ?>&i=<?php echo $_GET["i"] + 1;?>">&gt;&gt;</a>
<?php
$fc = file_get_contents($_GET["name"]."[".$_GET["i"]."].html");
$fc = preg_replace(":<p><img.*</p>:", "<p><img src=\"./".$_GET["name"]."[".$_GET["i"]."].png\" /></p>", $fc);
//$fc = preg_replace(":<p><img .* />:", "", $fc);
echo $fc;
//include($_GET["name"]."[".$_GET["i"]."].html");
?>
