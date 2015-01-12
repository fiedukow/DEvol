<a href="../..">BACK</a>
<h1><?php echo basename(dirname(realpath("."))); ?></h1>
<pre>
<?php include("../description.txt"); ?>
</pre>
<h3>Experiments results.</h3>
<ul>
<?php
$files = glob('{,.}*.html',GLOB_BRACE);
usort($files, function($a, $b) {
    return filemtime($a) < filemtime($b);
});

$files = preg_replace(":\[[0-9]+\]:", "", $files);
$files = array_unique($files);

	foreach ($files as $filename) {
      $pfn = pathinfo($filename)["filename"];
	    echo "<li><a href=\"display_i.php?name=$pfn&i=1\">".$pfn."</a></li>\n";
	}
?>
<ul>
