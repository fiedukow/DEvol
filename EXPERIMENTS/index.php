<a href="http://github.com/fiedukow/DEvol/">More informations</a>
<h3>Experiments list:</h3>
<ul>
<?php
$folders = scandir(".", 1);
foreach($folders as $folder) {
  if (!in_array($folder, array("..",".","base")) && is_dir($folder))
    echo "<li><a href=\"./$folder/single_results/\">$folder</a></li>";
}
?>
</ul>
