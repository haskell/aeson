<?php

$path = $argv[1];
// var_dump($filename);

$data = file_get_contents($path);

// http://php.net/manual/en/function.json-decode.php
// This function only works with UTF-8 encoded strings.
// PHP implements a superset of JSON as specified in the original » RFC 4627 - it will also encode and decode scalar types and NULL. RFC 4627 only supports these values when they are nested inside an array or an object.
// Although this superset is consistent with the expanded definition of "JSON text" in the newer » RFC 7159 (which aims to supersede RFC 4627) and » ECMA-404, this may cause interoperability issues with older JSON parsers that adhere strictly to RFC 4627 when encoding a single scalar value.
$result = json_decode($data);
//var_dump($result);

if ($result === null && json_last_error() !== JSON_ERROR_NONE) {
	exit(1);
}

exit(0);

?>
