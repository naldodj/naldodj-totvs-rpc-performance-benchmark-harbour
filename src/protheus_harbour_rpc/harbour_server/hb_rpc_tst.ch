// Constantes para melhor legibilidade
#define SOCKET_TIMEOUT_ACCEPT      100     // ms
#define SOCKET_TIMEOUT_CLIENT     30000    // ms
#define SOCKET_TIMEOUT_HANDSHAKE   5000    // ms
#define SOCKET_BUFFER_SIZE        65536
#define MAX_REQUEST_SIZE      100000000    // 100MB máximo

#define ACK_PREFIX "ACK_"
#define NAK_PREFIX "NAK_"
#define CMD_HEADER "CMD_HEADER"
#define CMD_DATA "CMD_DATA"
#define CMD_PROCESSING "CMD_PROCESSING"
#define CMD_RESPONSE "CMD_RESPONSE"