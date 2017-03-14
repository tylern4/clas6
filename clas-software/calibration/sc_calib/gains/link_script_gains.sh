#/bin/sh
#
# shell script to create links for hscan and min_main
# 
#
# delete, then define new input and output data links
#
rm hscan_input
rm hscan_data
rm min_input
rm min_kumac.kumac

ln -s gmean_one_dim.hbook hscan_input
ln -s gmean_one_dim.dat   hscan_data
ln -s hscan_data min_input
ln -s min_kumac min_kumac.kumac

#
#
