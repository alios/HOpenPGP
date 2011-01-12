
#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>


int main (int argc, char **argv)
{
    int sock = -1;

    if ((sock = socket(SOCK_STREAM, ,)) == -1)
	{
	    perror("socket");
	    exit(EXIT_FAILURE);
	}

    return 0;
}
