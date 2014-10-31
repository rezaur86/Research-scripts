#python ../scripts/Cascade/users_features.py iheart_prep_all_gift_sorted.txt users_gender_locale.txt ../output_cascade/iheart_gift/ 3 1
#python ~/scripts/Cascade/adoption_features.py iheart_prep_all_gift_sorted.txt ../output_cascade/iheart_gift/user_features.csv ../output_cascade/iheart_gift/succ_ 1

#python ~/scripts/Cascade/node_gift_info.py hugged_sorted_anonymized_seq hugged_prep_all_gift
#python ~/scripts/Cascade/users_features.py hugged_prep_all_gift_sorted.txt users_gender_locale.txt ../output_cascade/hugged_gift/ 1 1
#python ~/scripts/Cascade/adoption_features.py hugged_prep_all_gift_sorted.txt ../output_cascade/hugged_gift/user_features.csv ../output_cascade/hugged_gift/succ_ 1

#python ~/scripts/Cascade/node_gift_info.py ismile_sorted_anonymized_seq ismile_prep_all_gift
python ~/scripts/Cascade/users_features.py ismile_prep_all_gift_sorted.txt users_gender_locale.txt ../output_cascade/ismile_gift/ 2 1
python ~/scripts/Cascade/adoption_features.py ismile_prep_all_gift_sorted.txt ../output_cascade/ismile_gift/user_features.csv ../output_cascade/ismile_gift/all_imp_ 1