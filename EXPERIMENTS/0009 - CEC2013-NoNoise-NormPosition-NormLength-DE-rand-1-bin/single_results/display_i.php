<a href="index.php">Select experiment</a>
<a href="display_i.php?name=<?php echo $_GET["name"]; ?>&i=<?php echo $_GET["i"] - 1;?>">&lt;&lt;</a>
<a href="display_i.php?name=<?php echo $_GET["name"]; ?>&i=<?php echo $_GET["i"] + 1;?>">&gt;&gt;</a>
<?php
include($_GET["name"]."[".$_GET["i"]."].html");
?>
