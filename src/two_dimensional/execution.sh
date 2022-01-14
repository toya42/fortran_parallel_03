#!/bin/bash

compiler_type="gfortran"
build_type="fast"
number_of_loops_jacobi=1000
number_of_loops_riemann=500
number_of_trials=5

rm -r Build*
rm -r Run*
rm list.txt
rm -r output

mkdir output

# auto parallelize
num_of_openmp=1
for num_of_auto_parallelize in 0 1 2 4
do
    echo $num_of_auto_parallelize >input.txt
    echo $num_of_openmp >> input.txt
    echo $compiler_type >> input.txt
    echo $build_type >> input.txt
    echo $number_of_loops_jacobi >> input.txt
    echo $number_of_loops_riemann >> input.txt
    echo $number_of_trials >> input.txt
    echo $num_of_auto_parallelize >> input.txt
    echo $num_of_openmp >> input.txt
    bash -x compile_and_run.sh <input.txt
done

# OpenMP
num_of_auto_parallelize=0
#for num_of_openmp in 1 2 4
for num_of_openmp in 2 4
do
    echo $num_of_auto_parallelize >input.txt
    echo $num_of_openmp >> input.txt
    echo $compiler_type >> input.txt
    echo $build_type >> input.txt
    echo $number_of_loops_jacobi >> input.txt
    echo $number_of_loops_riemann >> input.txt
    echo $number_of_trials >> input.txt
    echo $num_of_auto_parallelize >> input.txt
    echo $num_of_openmp >> input.txt
    bash -x compile_and_run.sh <input.txt
done

cd output
output_file_name="output_${compiler_type}_${build_type}_${number_of_loops_jacobi}_${number_of_trials}"
cat *.csv | head -n 1 > $output_file_name && find . -name "*.csv" -exec sed -e '1d' {} \; >> $output_file_name
output_file_name_csv="${output_file_name}.csv"
mv $output_file_name $output_file_name_csv
