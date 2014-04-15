#!/bin/bash

python2.7 /home/rezaur/scripts/Cascade/gift_size_depth.py /media/leaf/iheart_node_sorted.txt /media/leaf/output_cascade/iheart_gift/ 4 3734781 0
python2.7 /home/rezaur/scripts/Cascade/unique_top_node_info.py /media/leaf/output_cascade/iheart_gift/children_of_parent.txt /media/leaf/output_cascade/iheart_gift/top_size.csv 63072000 3734781 100
