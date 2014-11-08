#python ~/scripts/Cascade/node_gift_info.py hugged_sorted_anonymized_seq hugged_prep_all_gift
#python ~/scripts/Cascade/users_features.py hugged_prep_all_gift_sorted.txt users_gender_locale.txt ../output_cascade/hugged_gift/ 1 1
#python ~/scripts/Cascade/adoption_features.py hugged_prep_all_gift_sorted.txt ../output_cascade/hugged_gift/user_features.csv ../output_cascade/hugged_gift/all_imp_ 1

#python ~/scripts/Cascade/node_gift_info.py ismile_sorted_anonymized_seq ismile_prep_all_gift
#python ~/scripts/Cascade/users_features.py ismile_prep_all_gift_sorted.txt users_gender_locale.txt ../output_cascade/ismile_gift/ 2 1
#python ~/scripts/Cascade/adoption_features.py ismile_prep_all_gift_sorted.txt ../output_cascade/ismile_gift/user_features.csv ../output_cascade/ismile_gift/succ_ 1

#python ../scripts/Cascade/users_features.py iheart_prep_all_gift_sorted.txt users_gender_locale.txt ../output_cascade/iheart_gift/ 3 1
#python ~/scripts/Cascade/adoption_features.py iheart_prep_all_gift_sorted.txt ../output_cascade/iheart_gift/user_features.csv ../output_cascade/iheart_gift/succ_ 1

#python ../scripts/Cascade/users_features_timed.py iheart_prep_all_gift_sorted.txt 10660699 1247986799 ../output_cascade/iheart_gift/
#python ~/scripts/Cascade/adoption_features_timed.py iheart_prep_all_gift_sorted.txt ../output_cascade/iheart_gift/user_features_timed.csv 10660699 1247986799 ../output_cascade/iheart_gift/1mo_

python ../scripts/Cascade/users_features_timed.py iheart_prep_all_gift_sorted.txt ../output_cascade/iheart_gift/44w_burstiness.csv 84888343 1257667199 ../output_cascade/iheart_gift/44w_
python ~/scripts/Cascade/adoption_features_timed.py iheart_prep_all_gift_sorted.txt ../output_cascade/iheart_gift/44w_user_features_timed.csv 84888343 1257667199 ../output_cascade/iheart_gift/44w_
