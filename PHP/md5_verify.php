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

function testmd5($file) {
	// TODO: warn if an entry is multiple times (with different checksums) in a single file
	if (!file_exists($file)) {
		echo "ERROR: File $file does not exist.\n";
		return;
	}

	$lines = file($file);
	foreach ($lines as $line) {
		$line = str_replace("\xEF\xBB\xBF",'',$line);
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
				//echo "OK: $origname\n";
			}
		}
		// TODO: Also warn about extra files which are not indexed
	}
}

function _rec($directory) {
	if (!is_dir($directory)) {
		exit("Invalid directory path $directory\n");
	}

	if ($dont_add_files = count(glob("$directory/*.md5")) == 0) {
		// echo "Directory $directory has no MD5 file. Skipping.\n";
	} else {
		$out = array();

		// echo "Check $directory\n";
		$md5files = glob('*.md5');
		foreach ($md5files as $md5file) {
			testmd5($md5file);
		}
	}

	foreach (scandir($directory) as $file) {
		if ($file !== '.' && $file !== '..') {
			$file = $directory . '/' . $file;
			if (is_dir($file)) {
				_rec($file);
			}
		}
	}
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