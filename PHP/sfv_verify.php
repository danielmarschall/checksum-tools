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

// TODO: On Windows file systems, accept file names case insensitively

function utf8_normalize($str) {
	// This helps to handle decomposite Unicode endpoints (E.g. German Umlauts have different representations)
	// Requires php-intl
	if (!class_exists('Normalizer')) return $str;
	return Normalizer::normalize($str);
}

function convertToUTF8($str) {
	$enc = mb_detect_encoding($str);
	if ($enc && $enc != 'UTF-8') {
		return iconv($enc, 'UTF-8', $str);
	} else {
		return $str;
	}
}

function testsfv($file) {
	// TODO: warn if an entry is multiple times (with different checksums) in a single file
	if (!file_exists($file)) {
		fwrite(STDERR, "ERROR: File $file does not exist.\n");
		return;
	}

	$files_checked = array();

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
		if (!$force_utf8) $line = convertToUTF8($line);

		if (substr(trim($line),0,1) == ';') continue;

		$line = rtrim($line);
		if ($line == '') continue;
		$checksum = substr($line,-8);
		$origname = rtrim(substr($line,0,strlen($line)-8));
		$origname = dirname($file) . '/' . rtrim($origname);
		if (!file_exists($origname)) {
			fwrite(STDERR, "WARNING: File vanished : $origname\n");
		} else {
			if (is_file($origname)) {
				$checksum2 = crc32_file($origname);
				if (strtolower($checksum) != strtolower($checksum2)) {
					fwrite(STDERR, "CHECKSUM FAIL: $origname (expected $checksum, but is $checksum2)\n");
				} else {
					global $show_verbose;
					if ($show_verbose) echo "OK: $origname\n";
				}
			} else {
				// For some reason, some files on a NTFS volume are "FIFO" pipe files?!
				fwrite(STDERR, "Warning: $origname is not a regular file!\n");
			}
		}

		$origname = utf8_normalize(basename($origname));
		$files_checked[] = dirname($file) . '/' . $origname;
	}

	// Now check if files have vanished!
	$directory = dirname($file);
	$sd = @scandir($directory);
	if ($sd === false) {
		fwrite(STDERR, "Error: Cannot scan directory $directory\n");
	} else {
		foreach ($sd as $file) {
			if ($file === '.') continue;
			if ($file === '..') continue;
			if (substr($file,0,1) === '.') continue;
			if (strtolower($file) === 'thumbs.db') continue;
			if (strtolower(substr($file, -4)) === '.md5') continue;
			if (strtolower(substr($file, -4)) === '.sfv') continue;
			$fullpath = $directory . '/' . $file;
			if (!is_dir($fullpath)) {
				$fullpath = utf8_normalize($fullpath);
				if (!in_array($fullpath,$files_checked)) {
					fwrite(STDERR, "Warning: File not in SFV checksum file: $fullpath\n");
				}
			}
		}
	}
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

function _rec($directory) {
	$directory = rtrim($directory, '/\\');

	if (!is_dir($directory)) {
		fwrite(STDERR, "Invalid directory path $directory\n");
		return false;
	}

	if ($dont_add_files = count(glob("$directory/*.sfv")) == 0) {
		global $show_verbose;
		if ($show_verbose) echo "Directory $directory has no SFV file. Skipping.\n";
	} else {
		$out = array();

		global $show_verbose;
		if ($show_verbose) echo "Check directory $directory\n";
		$sfvfiles = glob($directory.'/*.sfv');
		foreach ($sfvfiles as $sfvfile) {
			testsfv($sfvfile);
		}
	}

	$sd = @scandir($directory);
	if ($sd === false) {
		fwrite(STDERR, "Error: Cannot scan directory $directory\n");
		return false;
	}

	foreach ($sd as $file) {
		if ($file !== '.' && $file !== '..') {
			$file = $directory . '/' . $file;
			if (is_dir($file)) {
				_rec($file);
			}
		}
	}

	return true;
}


# ---

$show_verbose = false;
$dirs = array();

for ($i=1; $i<$argc; $i++) {
	if ($argv[$i] == '-v') {
		$show_verbose = true;
	} else {
		$dirs[] = $argv[$i];
	}
}

if (count($dirs) == 0) {
	echo "Syntax: $argv[0] [-v] <directory> [<directory> [...]]\n";
	exit(2);
}

$res = 0;
foreach ($dirs as $dir) {
	if (!_rec($dir)) $res = 1;
}
if ($show_verbose) echo "Done.\n";
exit($res);

