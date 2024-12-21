<?php

ini_set('memory_limit', '-1');

function nonEmpty($value)
{
	return strlen($value) > 0;
}

$content = file_get_contents('data/task.example');
// $content = file_get_contents('data/task.data');
$lines = explode("\n", $content);

$towels = explode(', ', $lines[0]);
$designs = array_filter(array_slice($lines, 2), 'nonEmpty');

// $trie = new \Tries\RadixTrie();
$hash = [];
foreach ($towels as $towel) {
	// $trie->add($towel, '$');
	$hash[$towel] = '$';
}

print_r($hash);

function canCreateDesignWithTrie(string $design, array $acc): array
{
	global $hash;

	if (empty($design)) {
		return [true, $acc];
	}

	for ($i = strlen($design); $i >= 1; $i--) {
		$suffix = substr($design, -$i);

		if (!isset($hash[$suffix])) {
			continue;
		}

		$acc[] = $suffix;
		list($canCreate, $newAcc) = canCreateDesignWithTrie(substr($design, 0, strlen($design) - $i), $acc);
		if ($canCreate) {
			return [true, $newAcc];
		}
	}

	return [false, []];
}

function canCreateDesignWithTrie2(string $design, array $acc): array
{
	global $hash;

	if (empty($design)) {
		return [[true, $acc]];
	}

	$results = [];
	for ($i = strlen($design); $i >= 1; $i--) {
		$suffix = substr($design, -$i);

		if (!isset($hash[$suffix])) {
			continue;
		}

		$newAcc = $acc;
		$newAcc[] = $suffix;
		$subResults = canCreateDesignWithTrie2(substr($design, 0, strlen($design) - $i), $newAcc);

		foreach ($subResults as $result) {
			if ($result[0]) {
				$results[] = $result;
			}
		}
	}

	return empty($results) ? [[false, []]] : $results;
}

$validDesigns = [];
foreach ($designs as $design) {
	list($canCreate, $acc) = canCreateDesignWithTrie($design, []);
	if ($canCreate) {
		$validDesigns[$design] = implode('|', $acc);
	}
}

print_r($validDesigns);

echo count($validDesigns) . PHP_EOL;

$count = 0;
foreach ($designs as $design) {
	$result = canCreateDesignWithTrie2($design, []);
	foreach ($result as $r) {
		if ($r[0]) {
			$count++;
			echo $design . ' -> ' . implode('|', $r[1]) . PHP_EOL;
		}
	}
	// echo $design . ' -> ' . print_r($acc, true) . PHP_EOL;
}
echo $count . PHP_EOL;
