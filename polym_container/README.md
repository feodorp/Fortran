Test program must be linked with time_utils. To compile the test file using gfortran:

```shell
foo@bar:~$ gfortan -c -O3 -flto poly_container8c.f90
foo@bar:~$ gfortan -c -O3 -flto ../time_utils/time_utils.f90
foo@bar:~$ gfortan -O3 -flto test.f90 poly_container8c.o time_utils.o
```
