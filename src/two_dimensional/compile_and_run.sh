#!/bin/bash

echo "input num. of thread (auto parallelize)"
echo "  ('0' : auto parallelize OFF)"
read num_of_auto_parallelize

if [ $num_of_auto_parallelize -eq 0 ]; then
    echo -n '' > autopara.txt
else
    echo -n "-ftree-parallelize-loops="$num_of_auto_parallelize >autopara.txt
fi

echo "input num. of thread (OpenMP)"
read num_of_openmp
echo -n '' > openmp.txt



echo "input compiler"
read compiler_type
echo "input build type"
read build_type
directory_name="${compiler_type}_${build_type}_apara${num_of_auto_parallelize}_omp${num_of_openmp}"
build_directory_name="Build_${directory_name}"
execute_directory_name="Run_${directory_name}"

echo $execute_directory_name >> list.txt

mkdir $build_directory_name
cd $build_directory_name
cmake -D CMAKE_Fortran_COMPILER=$compiler_type -D CMAKE_BUILD_TYPE=$build_type ..
#VERBOSE=1 make
make

cd ..
#echo "$execute_directory_name"
mkdir $execute_directory_name
cd $execute_directory_name
ln -s ../$build_directory_name/2d_tests.exe ./

export OMP_NUM_THREADS=$num_of_openmp
./2d_tests.exe

output_file_name="output_${directory_name}"
cat *.csv | head -n 1 > $output_file_name && find . -name "*.csv" -exec sed -e '1d' {} \; >> $output_file_name
output_file_name_csv="${output_file_name}.csv"
mv $output_file_name $output_file_name_csv
cp  $output_file_name_csv ../output/
