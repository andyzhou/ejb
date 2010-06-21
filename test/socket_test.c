#include <errno.h>   
#include <sys/types.h>   
#include <string.h>   
#include <sys/stat.h>   
#include <fcntl.h>   
#include <assert.h>   
#include <netdb.h>   
#include <sys/types.h>   
#include <netinet/in.h>   
#include <sys/socket.h>  
#include <stdio.h>
#include <stdlib.h>

#define SERV_HOST "127.0.0.1"
#define SERV_PORT  5555    
#define maxdatasize 100

#define MAX_CLIENTS 2

int client_test();
  
int main(int argc,char ** argv)
{
	int i = 0;
	for(i = 1; i <= MAX_CLIENTS; i++)
	{
		client_test(i);
		usleep(5000);
	}

	while(1)
	{
		printf("ok..............\n");
		sleep(20);
	}
}

int client_test(int no)
{
	int sockfd;   
    char buf[maxdatasize];   
    struct hostent *host;   
    struct sockaddr_in serv_addr;
  
    struct timeval timeout={10,0};   
  
    if((host=gethostbyname(SERV_HOST))==NULL){   
       
        perror("gethostbyname error");   
        exit(0);   
  
    }

    if((sockfd=socket(AF_INET,SOCK_STREAM,0))==-1){    
           
        perror("create socker error");   
        exit(0);   
    }      
       
    serv_addr.sin_family=AF_INET;   
    serv_addr.sin_port=htons(SERV_PORT);   
    serv_addr.sin_addr=*((struct in_addr *)host->h_addr);   
    bzero(&(serv_addr.sin_zero),8); 
	
    if(connect(sockfd,(struct sockaddr *)&serv_addr,sizeof(struct sockaddr))==-1){   
        perror("connect error");   
        exit(1);  
    }

	printf("client %d connect success!\n", no);

	//set test data
	char msg[256] = {0};

	snprintf(msg, sizeof(msg), "login,test%d,test%d\n\r", no, no);
	send(sockfd, msg, sizeof(msg), 0);
	printf("msg:%s\n", msg);
	//close(sockfd);
	
	return 1;
}
