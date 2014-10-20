<h3>Experiments results.</h3>
<a href="http://github.com/fiedukow/DEvol/">More informations</a>
<ul>
<?php
	foreach (glob("*.html") as $filename) {
	    echo "<li><a href=\"$filename\">".pathinfo($filename)["filename"]."</a> - ".date ("F d Y H:i:s.", filemtime($filename))."</li>";
	}
?>
</ul>
