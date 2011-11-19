#include <stdio.h>
#include <stdlib.h>
#include "ladder.h"

int main(void)
{
	printf("Starting up");
	fflush(stdout);
	SoftItem* psi = initialize("/home/providence/mindterm");
	printf("Directory is %s \n", psi->disk_location);
	fflush(stdout);
	exit(EXIT_SUCCESS);
}
