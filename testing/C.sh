#!/bin/bash

# Compilar el módulo Generado.f90
gfortran -c parser.f90
if [ $? -ne 0 ]; then
    echo "Error al compilar parser.f90"
    exit 1
fi

# Compilar el programa principal parser.f90 y enlazarlo con el módulo Generado.o
gfortran -c test.f90
if [ $? -ne 0 ]; then
    echo "Error al compilar parser.f90"
    exit 1
fi

gfortran parser.o test.o -o test
if [ $? -ne 0 ]; then
    echo "Error al compilar parser.f90"
fi

# Ejecutar el ejecutable generado
echo "Ejecutando el programa..."
./test entrada.txt