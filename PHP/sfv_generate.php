#!/usr/bin/php
<?php

/*
   Copyright 2020-2022 Daniel Marschall, ViaThinkSoft

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

// This script generates SFV files
// If there is already an SFV file existing, only new files get appended to the existing SFV files.

function _rec($directory) {
	if (!is_dir($directory)) {
		fwrite(STDERR, "Invalid directory path '$directory'\n");
		return false;
	}

	if ((basename($directory) == '.') || (basename($directory) == '..')) {
		$sfv_file = rtrim($directory,DIRECTORY_SEPARATOR).DIRECTORY_SEPARATOR.
		            basename(realpath($directory)).'.sfv';

	} else {
		$sfv_file = rtrim($directory,DIRECTORY_SEPARATOR).DIRECTORY_SEPARATOR.
		            basename($directory).'.sfv';
	}

	if (file_exists($sfv_file)) {
		$existing_files = sfv_get_files($sfv_file);
	} else {
		$existing_files = array();
	}

	$sd = @scandir($directory);
	if ($sd === false) {
		fwrite(STDERR, "Error: Cannot scan directory $directory\n");
		return false;
	}

	foreach ($sd as $file) {
		if ($file === '.') continue;
		if ($file === '..') continue;
		if (substr($file,0,1) === '.') continue;
		if (strtolower($file) === 'thumbs.db') continue;
		if (strtolower(substr($file, -4)) === '.md5') continue;
		if (strtolower(substr($file, -4)) === '.sfv') continue;

		$fullpath = rtrim($directory,DIRECTORY_SEPARATOR).DIRECTORY_SEPARATOR.$file;
		if (is_dir($fullpath)) {
			global $do_recursive;
			if ($do_recursive) _rec($fullpath);
		} else if (is_file($fullpath)) {
			global $show_verbose;
			if ($show_verbose) echo "$fullpath\n";
			$dir = pathinfo($fullpath, PATHINFO_DIRNAME);

			if (!file_exists($sfv_file)) {
				file_put_contents($sfv_file, "; Generated by ViaThinkSoft\r\n"); // TODO: BOM
			}

			if (!in_array($file, $existing_files)) {
				$crc32 = strtoupper(crc32_file($fullpath));
				file_put_contents($sfv_file, "$file $crc32\r\n", FILE_APPEND);
			}
		} else {
			// For some reason, some files on a NTFS volume are "FIFO" pipe files?!
			fwrite(STDERR, "Warning: $fullpath is not a regular file!\n");
		}
	}

	return true;
}

function sfv_get_files($filename) {
	$out = array();
	$lines = file($filename);
	foreach ($lines as $line) {
		$line = rtrim($line);
		if ($line == '') continue;
		if (substr($line,0,1) == ';') continue;
		$out[] = substr($line, 0, strrpos($line, ' '));

	}
	return $out;
}

function swapEndianness($hex) {
	return implode('', array_reverse(str_split($hex, 2)));
}

function crc32_file($filename, $rawOutput = false) {
	$out = bin2hex(hash_file ('crc32b', $filename , true));
	if (hash('crc32b', 'TEST') == 'b893eaee') {
		// hash_file() in PHP 5.2 has the wrong Endianess!
		// https://bugs.php.net/bug.php?id=47467
		$out = swapEndianness($out);
	}
	return $out;
}

# ---

$show_verbose = false;
$do_recursive = false;
$dirs = array();

for ($i=1; $i<$argc; $i++) {
	if ($argv[$i] == '-v') {
		$show_verbose = true;
	} else if ($argv[$i] == '-r') {
		$do_recursive = true;
	} else {
		$dirs[] = $argv[$i];
	}
}

if (count($dirs) == 0) {
	echo "Syntax: $argv[0] [-v] [-r] <directory> [<directory> [...]]\n";
	exit(2);
}

$res = 0;
foreach ($dirs as $dir) {
	if (!_rec($dir)) $res = 1;
}
if ($show_verbose) echo "Done.\n";
exit($res);
