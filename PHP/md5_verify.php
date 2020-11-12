#!/usr/bin/php
<?php

/*
   Copyright 2020 Daniel Marschall, ViaThinkSoft

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

// TODO: make use of STDERR and return different exit codes

function testmd5($file) {
	// TODO: warn if an entry is multiple times (with different checksums) in a single file
	if (!file_exists($file)) {
		echo "ERROR: File $file does not exist.\n";
		return;
	}

	$lines = file($file);
	$is_first_line = true;
	$force_utf8 = false;
	foreach ($lines as $line) {
		if ($is_first_line) {
			$tmp = 0;
			$line = str_replace("\xEF\xBB\xBF",'',$line,$tmp);
			if ($tmp > 0) $force_utf8 = true;
			$is_first_line = false;
		}
		$is_ansi = strstr(utf8_decode($line),'?') !== false; // Attention: This assumes that '?' is not part of the line!
		if (!$force_utf8 && $is_ansi) $line = utf8_encode($line);

		$line = trim($line);
		if ($line == '') continue;
		$line = str_replace('*', ' ', $line);
		$line = str_replace("\t", ' ', $line);
		list($checksum, $origname) = explode(' ', $line, 2);
		$origname = dirname($file) . '/' . trim($origname);
		$checksum = trim($checksum);
		if (!file_exists($origname)) {
			echo "WARNING: File vanished : $origname\n";
		} else {
			$checksum2 = md5_file($origname);
			if (strtolower($checksum) != strtolower($checksum2)) {
				echo "CHECKSUM FAIL: $origname (expected $checksum, but is $checksum2)\n";
			} else {
				global $show_verbose;
				if ($show_verbose) echo "OK: $origname\n";
			}
		}
		// TODO: Also warn about extra files which are not indexed
	}
}

function _rec($directory) {
	$directory = rtrim($directory, '/\\');

	if (!is_dir($directory)) {
		exit("Invalid directory path $directory\n");
	}

	if ($dont_add_files = count(glob("$directory/*.md5")) == 0) {
		global $show_verbose;
		if ($show_verbose) echo "Directory $directory has no MD5 file. Skipping.\n";
	} else {
		$out = array();

		global $show_verbose;
		if ($show_verbose) echo "Check directory $directory\n";
		$md5files = glob($directory.'/*.md5');
		foreach ($md5files as $md5file) {
			testmd5($md5file);
		}
	}

	$sd = @scandir($directory);
	if ($sd === false) {
		echo "Error: Cannot scan directory $directory\n";
		return;
	}

	foreach ($sd as $file) {
		if ($file !== '.' && $file !== '..') {
			$file = $directory . '/' . $file;
			if (is_dir($file)) {
				_rec($file);
			}
		}
	}
}


# ---

$show_verbose = false;
$dir = '';

for ($i=1; $i<$argc; $i++) {
	if ($argv[$i] == '-v') {
		$show_verbose = true;
	} else {
		$dir = $argv[$i];
	}
}

if (empty($dir)) {
	echo "Syntax: $argv[0] [-v] <directory>\n";
	exit(2);
}

if (!is_dir($dir)) {
	echo "Directory not found\n";
	exit(1);
}

_rec($dir);

if ($show_verbose) echo "Done.\n";
