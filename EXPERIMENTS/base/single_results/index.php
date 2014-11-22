<a href="../..">BACK</a>
<h3>Experiments results.</h3>
<ul>
<?php
$files = glob('{,.}*.html',GLOB_BRACE);
usort($files, function($a, $b) {
    return filemtime($a) < filemtime($b);
});

	foreach ($files as $filename) {
	    echo "<li><a href=\"$filename\">".pathinfo($filename)["filename"]."</a> - ".date ("F d Y H:i:s.", filemtime($filename))."</li>\n";
	}
?>
</ul>
