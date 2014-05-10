#!/bin/bash

#python ~/scripts/Cascade/node_ext_info.py iheart_sorted_anonymized_seq iheart_preprocessed
#python ~/scripts/Cascade/raw_stat.py iheart_preprocessed_sorted.txt iheart_preprocessed_basic.txt ~/output_cascade/raw_stat_v2/ 86400

#python /home/rezaur/scripts/Cascade/gift_size_depth.py /home/rezaur/data/iheart_node_sorted.txt /home/rezaur/output_cascade/iheart_gift/ 4 3734781 0
#python /home/rezaur/scripts/Cascade/unique_top_node_info.py /home/rezaur/output_cascade/iheart_gift/children_of_parent.txt /home/rezaur/output_cascade/iheart_gift/top_size.csv 63072000 3734781 100

#python ~/scripts/Cascade/node_ext_info.py hugged_sorted_anonymized_seq hugged_preprocessed
#python ~/scripts/Cascade/raw_stat.py hugged_preprocessed_sorted.txt hugged_preprocessed_basic.txt ~/output_cascade/raw_stat_hugged/ 86400
#python /home/rezaur/scripts/Cascade/cascading_tree.py /home/rezaur/data/hugged_preprocessed_sorted.txt /home/rezaur/output_cascade/hugged_cascade/ 4 3734781 0 /home/rezaur/output_cascade/raw_stat_hugged/parent_proportion.csv
#python /home/rezaur/scripts/Cascade/unique_top_node_info.py /home/rezaur/output_cascade/hugged_cascade/children_of_parent.txt /home/rezaur/output_cascade/hugged_cascade/top_size.csv 63072000 3734781 100

python ~/scripts/Cascade/node_ext_info.py ismile_sorted_anonymized_seq ismile_preprocessed
python ~/scripts/Cascade/raw_stat.py ismile_preprocessed_sorted.txt ismile_preprocessed_basic.txt ~/output_cascade/raw_stat_ismile/ 86400
