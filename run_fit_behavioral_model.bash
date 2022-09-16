#!/bin/bash

models=(rtage rtagepropot rtagepropotnull accage accprepotlin accprepot accprevcond)
chains=(1 2 3 4)

for model in ${models[@]}; do
  for chain in ${chains[@]}; do
    echo "Model: $model"
    echo "Chain: $chain"
    cmd="sbatch -J carit-$model-$chain -c 48 --mem=48G -t 7-00:00:00 /ncf/mclaughlin/users/jflournoy/data/containers/sbatch_R_command_som.bash verse-cmdstan-ggseg-libs.simg fit_behavioral_model.R --model $model --id $chain"
    echo "Command: $cmd"
    exec $cmd &
  done
done
