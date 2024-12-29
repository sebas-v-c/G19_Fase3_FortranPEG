#!/bin/bash

# Compilar el módulo Generado.f90
gfortran -c parser_pepino.f90
if [ $? -ne 0 ]; then
    echo "Error al compilar parser.f90"
    exit 1
fi

# Compilar el programa principal parser.f90 y enlazarlo con el módulo Generado.o
gfortran -c test_pepino.f90
if [ $? -ne 0 ]; then
    echo "Error al compilar parser.f90"
    exit 1
fi

gfortran parser_pepino.o test_pepino.o -o test_pepino
if [ $? -ne 0 ]; then
    echo "Error al compilar parser.f90"
fi

# Ejecutar el ejecutable generado
echo "Ejecutando el programa..."
./test_pepino entrada.txt