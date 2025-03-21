#!/bin/bash
#
#  load the module
source /opt/asn/etc/asn-bash-profiles-special/modules.sh
module load raxml/8.0.24
#
#  put RAxML commands here
raxmlHPC-SSE3 -m GTRGAMMA -p 12345 -s core_gene_alignment.aln -n tre -f a -x 123 -N autoMRE -p 456
