#!/bin/bash
#SBATCH -c 48
#SBATCH --mem=48G
#SBATCH -t 3-00:00
#SBATCH -n 1
#SBATCH -p fasse
#SBATCH --signal=USR2
#SBATCH --account=somerville_lab
# Outputs ----------------------------------
#SBATCH -o /n/home_fasse/jflournoy/data/containers/log/%A_%a-%x.out

#models=(rtage rtagepropot rtageprepotalt rtageprepotdist rtageprepotnull accage accprepot accprepotlin accprevcond accprevcondnull rtguessing rtguessingnull acctrial accprepottrial)
models=(rtage rtageprepot rtageprepotalt rtageprepotnull rtageprepotdist)
chains=(1 2 3 4)
#INDEX BY 0
Nmodels=${#models[@]}
Nchains=${#chains[@]}
Nrows=$(( $Nmodels * $Nchains - 1))
i=${SLURM_ARRAY_TASK_ID}

if [ -z ${i} ]; then
  echo "Index from 0-${Nrows}"
  rownums=($(seq 0 ${Nrows}))
  for i in ${rownums[@]}; do
    ichain=$(( $i % $Nchains))
    imodel=$(( $i / $Nchains))
    echo $i: ${models[$imodel]} ${chains[$ichain]}
  done
else
  ichain=$(( $i % $Nchains))
  imodel=$(( $i / $Nchains))
  model=${models[$imodel]}
  chain=${chains[$ichain]}
  echo "Model: $model"
  echo "Chain: $chain"
  cmd="bash /ncf/mclaughlin/users/jflournoy/data/containers/sbatch_R_command_som.bash"
  cmd+=" verse-cmdstan-ggseg-libs.simg"
  cmd+=" fit_behavioral_model.R"
  cmd+=" --model $model"
  cmd+=" --id $chain"
  cmd+=" --refit"
  echo "Command: $cmd"
  exec $cmd
fi
