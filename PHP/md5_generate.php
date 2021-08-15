#!/usr/bin/php
<?php

/*
   Copyright 2020-2021 Daniel Marschall, ViaThinkSoft

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

// This script generates MD5 files
// If there is already an MD5 file existing, only new files get appended to the existing MD5 files.

// TODO: make use of STDERR and return different exit codes

function _rec($directory) {
	if (!is_dir($directory)) {
		die("Invalid directory path $directory\n");
	}

	if ((basename($directory) == '.') || (basename($directory) == '..')) {
		$md5_file = rtrim($directory,DIRECTORY_SEPARATOR).DIRECTORY_SEPARATOR.
		            basename(realpath($directory)).'.md5';

	} else {
		$md5_file = rtrim($directory,DIRECTORY_SEPARATOR).DIRECTORY_SEPARATOR.
		            basename($directory).'.md5';
	}

	if (file_exists($md5_file)) {
		$existing_files = md5_get_files($md5_file);
	} else {
		$existing_files = array();
	}

	$sd = @scandir($directory);
	if ($sd === false) {
		echo "Error: Cannot scan directory $directory\n";
		return;
	}

	foreach ($sd as $file) {
		if ($file === '.') continue;
		if ($file === '..') continue;
		if (strtolower($file) === 'thumbs.db') continue;
		if (strtolower(substr($file, -4)) === '.md5') continue;
		if (strtolower(substr($file, -4)) === '.sfv') continue;

		$fullpath = rtrim($directory,DIRECTORY_SEPARATOR).DIRECTORY_SEPARATOR.$file;
		if (is_dir($fullpath)) {
			_rec($fullpath);
		} else if (is_file($fullpath)) {
			global $show_verbose;
			if ($show_verbose) echo "$fullpath\n";
			$dir = pathinfo($fullpath, PATHINFO_DIRNAME);

			if (!file_exists($md5_file)) {
				file_put_contents($md5_file, "; Generated by ViaThinkSoft\r\n"); // TODO: BOM
			}

			if (!in_array($file, $existing_files)) {
				$md5 = strtolower(md5_file($fullpath));
				file_put_contents($md5_file, "$md5 *$file\r\n", FILE_APPEND);
			}
		} else {
			// For some reason, some files on a NTFS volume are "FIFO" pipe files?!
			echo "Warning: $fullpath is not a regular file!\n";
		}
	}
}

function md5_get_files($filename) {
	$out = array();
	$lines = file($filename);
	foreach ($lines as $line) {
		$line = str_replace("\xEF\xBB\xBF",'',$line);
		$line = trim($line);
		if ($line == '') continue;
		$line = str_replace('*', ' ', $line);
		$line = str_replace("\t", ' ', $line);
		list($checksum, $origname) = explode(' ', $line, 2);
		$origname = dirname($filename) . '/' . trim($origname);
		$checksum = trim($checksum);
		$out[] = $origname;
	}

	return $out;
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
