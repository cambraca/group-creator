<?php

$peopleCount = 30;
$groupSize = 4;
$individualCount = 100;

function printIndividual($individual) {
  echo 'Grouping [';
  $first_outer = TRUE;
  foreach ($individual as $group) {
    if ($first_outer)
      $first_outer = FALSE;
    else
      echo ',';
    echo '[';

    echo implode(',', $group);
    echo ']';
  }
  echo ']'.PHP_EOL;
}

for ($i = 0; $i < $individualCount; $i++) {
  $leftToAdd = range(0, $peopleCount-1);
  shuffle($leftToAdd);
  $individual = array();

  do {
    $group = array();
    for ($j = 0; $j < rand($groupSize-1, $groupSize+1); $j++) {
      $group[] = array_shift($leftToAdd);
      if (!$leftToAdd)
        break;
    }

    $individual[] = $group;
  } while ($leftToAdd);

  printIndividual($individual);
}
