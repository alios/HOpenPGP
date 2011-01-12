#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <regex.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>


/* defines */
#define MAX_URI_LEN 256

/* typedefs */
typedef enum http_method { GET, HEAD, POST, PUT
                         , DELETE, TRACE, CONNECT } http_method_t;

typedef enum http_version { HTTP10, HTTP11 } http_version_t;

typedef struct http_request
{
    http_method_t method;
    char uri[MAX_URI_LEN + 1];
    http_version_t version;
} http_request_t;


/* consts */
static const in_port_t DEFAULT_PORT = 8888;
static const char* http_regex = "";

/* function prototypes */
int main (int argc, char **argv);
static void onexit();
static void sighandler(int signo);
int parseHTTP (const char* str, http_request_t* request);

/* globals */
static int running = -1;
static int sock = -1;
static int sock_client = -1;
static regex_t preg;


int main (int argc, char **argv)
{

    /* bind exit handler */
    printf("binding exit handler ...\n");
    if (atexit(onexit) != 0)
	{
	    perror("atexit");
	    exit(EXIT_FAILURE);
	}

    /* configure signal handling */
    printf("binding signal handlers ...\n");
    if (signal(SIGINT, sighandler) == SIG_ERR)
	{
	    perror("signal(SIGINT)");
	    exit(EXIT_FAILURE);
	}
    if (signal(SIGQUIT, sighandler) == SIG_ERR)
	{
	    perror("signal(SIGQUIT)");
	    exit(EXIT_FAILURE);
	}

    /* create listening socket */
    printf("creating listening socket ...\n");
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1)
	{
	    perror("socket");
	    exit(EXIT_FAILURE);
	}


    /* bind listening socket */
    printf("binding listening socket ... \n");
    {
	struct sockaddr_in address;

	memset(&address, 0, sizeof(address));

	address.sin_family = AF_INET;
	address.sin_addr.s_addr = htonl (INADDR_ANY);
	address.sin_port = htons(DEFAULT_PORT);

	if (bind(sock,(const struct  sockaddr*)&address, sizeof(address)) != 0)
	    {
		perror("bind");
		exit(EXIT_FAILURE);
	    }
    }

    /* set listening socket to listen state */
    printf("calling listen() on socket ... \n");
    if (listen(sock, 0) != 0)
	{
	    perror("listen");
	    exit(EXIT_FAILURE);
	}

    /* compile http regex */
    printf("compiling http regex '%s' ... \n", http_regex);
    {
	int retval = 0;

	if((retval = regcomp(&preg , http_regex, REG_EXTENDED)) != 0) 
	    {
		char errorbuffer[128];
		memset(errorbuffer, 0, sizeof(errorbuffer));
		regerror(retval, &preg, errorbuffer, sizeof(errorbuffer));
		fprintf(stderr, "regcomp: %s", errorbuffer);
	    }
    }

    /* wait for new client connection */
    while (running) {
	struct sockaddr_in address;
	socklen_t address_len = sizeof(address);

	memset(&address, 0, address_len);

	printf("waiting for new client connection ... \n");
	sock_client = accept(sock, (struct sockaddr *)&address, &address_len);
	if (sock_client == -1)
	    {
		perror("accept");
		exit(EXIT_FAILURE);
	    }
	
	/* new connection */
	{
	    char* addr_str = inet_ntoa(address.sin_addr);
	    printf("new connection from %s:%d ... \n", 
		   addr_str, ntohs(address.sin_port));    
	}
	
	/* handle the connection */
	{
	    char buffer[65535];
	    ssize_t readbytes = 0;
	    http_request_t http_req;

	    memset(buffer, 0, sizeof(buffer));
	    
	    if ((readbytes = read(sock_client, buffer, sizeof(buffer))) == -1)
		{
		    perror("read");
		    exit(EXIT_FAILURE);
		}
	    
	    printf("read %d bytes:\n%s\n", readbytes, buffer);
	    parseHTTP(buffer, &http_req); 
	}

	/* close client connection */
	if (sock_client != -1)
	    {
		printf("closing client connection (%d) ... \n", sock_client);
		
		if (close(sock_client) != 0) 
		    perror("close(sock_client)");

		sock_client = -1;
	    }
    }

    exit(EXIT_SUCCESS);
}

#define NMATCH 4

int parseHTTP (const char* str, http_request_t* request)
{
    regmatch_t pmatch[NMATCH];

    if (str == NULL || request == NULL)
	return -1;

    if(regexec(&preg, str, NMATCH, pmatch, 0) == REG_NOMATCH)
	{
	    return -1;
	}


    memset(request, 0, sizeof(http_request_t));

    return 0;
}



static void sighandler(int signo)
{
    switch (signo)
	{
	case SIGQUIT:
	case SIGINT:
	    running = 0;
	    exit(EXIT_SUCCESS);
	    break;
	}

}

static void onexit()
{
    if (sock_client != -1)
	{
	    printf("closing client connection (%d) ... \n", sock_client);

	    if (close(sock_client) != 0) 
		perror("close(sock_client)");
		    
	}

    if (sock != -1)
	{
	    printf("closing listening socket (%d) ... \n", sock);

	    if (close(sock) != 0) 
		perror("close(sock)");
	}

    
    printf("freeing HTTP regex ... \n");
    regfree(&preg);

    printf("finished ... \n");
}
