#!/bin/bash
#SBATCH -c 48
#SBATCH --mem=48G
#SBATCH -t 7-00:00
#SBATCH -n 1
#SBATCH -p fasse
#SBATCH --signal=USR2
#SBATCH --account=somerville_lab
# Outputs ----------------------------------
#SBATCH -o /n/home_fasse/jflournoy/data/containers/log/%A_%a-%x.out

#models=(rtage rtagepropot rtagepropotdist rtagepropotnull accage accprepot accprepotlin accprevcond accprevcondnull rtguessing rtguessingnull acctrial accprepottrial)
chains=(1 2 3 4)
models=(rtagepropot)
#INDEX BY 0
Nmodels=${#models[@]}
Nchains=${#chains[@]}
Nrows=$(( $Nmodels * $Nchains - 1))
i=${SLURM_ARRAY_TASK_ID}

if [ -z ${i} ]; then
  echo "Index from 0-${Nrows}"
else
  ichain=$(( $i % $Nchains))
  imodel=$(( $i / $Nchains))
  model=${models[$imodel]}
  chain=${chains[$ichain]}
  echo "Model: $model"
  echo "Chain: $chain"
  cmd="bash /ncf/mclaughlin/users/jflournoy/data/containers/sbatch_R_command_som.bash verse-cmdstan-ggseg-libs.simg fit_behavioral_model.R --model $model --id $chain"
  cmd+=" --refit"
  echo "Command: $cmd"
  exec $cmd
fi
