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

// This script generates MD5 files
// If there is already an MD5 file existing, only new files get appended to the existing MD5 files.

function _rec($directory) {
	if (!is_dir($directory)) {
		die("Invalid directory path $directory\n");
	}

	$md5_file = $directory.'/'.basename($directory).'.md5';

	if (file_exists($md5_file)) {
		$existing_files = md5_get_files($md5_file);
	} else {
		$existing_files = array();
	}

	foreach (scandir($directory) as $file) {
		if ($file === '.') continue;
		if ($file === '..') continue;
		if (strtolower($file) === 'thumbs.db') continue;
		if (substr($file, -4) === '.md5') continue;
		if (substr($file, -4) === '.sfv') continue;

		$fullpath = $directory . '/' . $file;
		if (is_dir($fullpath)) {
			_rec($fullpath);
		} else {
###			echo "$fullpath\n";
			$dir = pathinfo($fullpath, PATHINFO_DIRNAME);

			if (!file_exists($md5_file)) {
				file_put_contents($md5_file, "; Generated by ViaThinkSoft\r\n"); // TODO: BOM
			}

			if (!in_array($file, $existing_files)) {
				$md5 = strtolower(md5_file($fullpath));
				file_put_contents($md5_file, "$md5 *$file\r\n", FILE_APPEND);
			}
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

if ($argc != 2) {
	echo "Syntax: $argv[0] <directory>\n";
	exit(2);
}

if (!is_dir($argv[1])) {
	echo "Directory not found\n";
	exit(1);
}

_rec($argv[1]);

echo "Done.\n";