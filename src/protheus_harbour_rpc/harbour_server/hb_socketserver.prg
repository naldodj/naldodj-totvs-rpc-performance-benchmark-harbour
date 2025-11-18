// hb_socketserver.prg - Servidor RPC COM TRATAMENTO DE ERROS COMPLETO
#pragma -w3
#pragma -es2
#pragma -km+
#pragma -ko+

REQUEST HB_CODEPAGE_UTF8EX
REQUEST CALC_PRECO_OTIMIZADO
REQUEST PROCESSAR_LOTE_MASSIVO

#include "hbver.ch"
#include "inkey.ch"
#include "error.ch"
#include "fileio.ch"
#include "hbclass.ch"
#include "hbthread.ch"
#include "hbsocket.ch"
#include "hbzlib.ch"

// Constantes para melhor legibilidade
#define SOCKET_TIMEOUT_ACCEPT      100     // ms
#define SOCKET_TIMEOUT_CLIENT     30000    // ms
#define SOCKET_TIMEOUT_HANDSHAKE   5000    // ms
#define SOCKET_BUFFER_SIZE        65536
#define MAX_REQUEST_SIZE      100000000    // 100MB máximo

STATIC s_lRunning := .T.
STATIC s_aThreads := {}
STATIC s_hMtxThreads := hb_mutexCreate()
STATIC THREAD_COUNT_MAX := 50

PROCEDURE Main(cPort)
    LOCAL nPort, hServer, hClient, nTurn, aRead, aWrite, aError, nResult, nThreadID, aAddr

    nPort := iif(Empty(cPort), 123456, Val(cPort))
    nTurn := 0
    aRead := {}
    aWrite := {}
    aError := {}

    hb_cdpSelect("UTF8EX")
    SET DATE ANSI
    SET CENTURY ON

    CLS

    @ 01, 15 SAY "HARBOUR RPC SERVER - TRATAMENTO DE ERROS"
    @ 02, 05 SAY "Porta: " + hb_ntos(nPort) + " | [ESC] para sair"
    @ 03, 05 SAY "Inicializando servidor..."

    hServer := hb_socketOpen()
    IF hServer == NIL
        @ 04, 05 SAY "ERRO: Nao foi possivel criar socket"
        RETURN
    ENDIF

    // Configurações do socket servidor
    hb_socketSetReuseAddr(hServer, .T.)
    hb_socketSetKeepAlive(hServer, .T.)

    // Bind e Listen
    aAddr := hb_socketResolveInetAddr("0.0.0.0", nPort)
    IF Empty(aAddr)
        @ 04, 05 SAY "ERRO: Nao foi possivel resolver endereco"
        hb_socketClose(hServer)
        RETURN
    ENDIF

    IF !hb_socketBind(hServer, aAddr)
        @ 04, 05 SAY "ERRO: Bind falhou - Porta " + hb_ntos(nPort)
        hb_socketClose(hServer)
        RETURN
    ENDIF

    IF !hb_socketListen(hServer, 50)
        @ 04, 05 SAY "ERRO: Listen falhou"
        hb_socketClose(hServer)
        RETURN
    ENDIF

    @ 04, 05 SAY "Servidor ativo - Aguardando conexoes..."
    @ 05, 05 SAY "Clientes ativos: 0"

    // Loop principal
    DO WHILE s_lRunning
        aRead := { hServer }
        aWrite := {}
        aError := { hServer }

        nResult := hb_socketSelect(@aRead, .T., @aWrite, .T., @aError, .T., 500)

        IF nResult > 0
            IF hb_AScan(aRead, hServer, NIL, NIL, .T.) > 0
                hClient := hb_socketAccept(hServer)
                IF hClient != NIL
                    // Controle de threads
                    hb_mutexLock(s_hMtxThreads)
                    IF Len(s_aThreads) < THREAD_COUNT_MAX
                        nThreadID := hb_threadStart(@ProcessClient(), hClient)
                        AAdd(s_aThreads, nThreadID)
                        @ 05, 25 SAY hb_ntos(Len(s_aThreads)) + "  "
                    ELSE
                        hb_socketClose(hClient)
                        LogEvent("AVISO", "Limite de threads atingido - Conexao rejeitada")
                    ENDIF
                    hb_mutexUnlock(s_hMtxThreads)
                ENDIF
            ENDIF

            IF hb_AScan(aError, hServer, NIL, NIL, .T.) > 0
                @ 06, 05 SAY "ERRO: Socket servidor com problema"
                LogEvent("ERRO", "Socket servidor com problema")
                EXIT
            ENDIF
        ELSEIF nResult < 0
            @ 06, 05 SAY "ERRO: Select falhou - " + hb_ntos(hb_socketGetError())
        ENDIF

        Progress(@nTurn, 7, 40)
        
        IF Inkey() == 27
            s_lRunning := .F.
            @ 08, 05 SAY "Finalizando servidor..."
        ENDIF
    ENDDO

    hb_socketClose(hServer)
    @ 09, 05 SAY "Servidor finalizado."
RETURN

// ========================================
// PROCESSAMENTO DO CLIENTE (ROBUSTO)
// ========================================
STATIC PROCEDURE ProcessClient(hClient)
    LOCAL cRequest, cID, cFunction, nLen, cData, aItens, aResult
    LOCAL lSuccess, oError, nStartTime, cBuffer, nBytes, nPos,nError,cErrorMsg
    local cCHR10:=Chr(10)
    local cCHR13:=Chr(13)
    local cCHR1310:=cCHR13+cCHR10
    
    // Valores padrão
    cID := "UNKNOWN"
    cFunction := "UNKNOWN" 
    nLen := 0
    nStartTime := Seconds()

    // Configurações do socket
    hb_socketSetNoDelay(hClient, .T.)
    hb_socketSetKeepAlive(hClient, .T.)

    BEGIN SEQUENCE WITH {|oError| Break(oError)}
        LogEvent("DEBUG", "Nova conexao recebida")

        // Recebe dados brutos sem assumir formato
        cRequest := ""
        DO WHILE .T.
            cBuffer := Space(65535)
            nBytes := hb_socketRecv(hClient, @cBuffer,,, 5000)
            
            IF nBytes > 0
                cRequest += Left(cBuffer, nBytes)
                LogEvent("DEBUG", "Recebido " + hb_ntos(nBytes) + " bytes. Total: " + hb_ntos(Len(cRequest)))
                
                // Tenta encontrar fim do header por diferentes delimitadores
                nPos := hb_At(cCHR1310, cRequest)
                IF nPos == 0
                    nPos := hb_At(cCHR10, cRequest)
                ENDIF
                IF nPos == 0
                    nPos := hb_At(":", cRequest)
                ENDIF
                
                IF nPos > 0
                    EXIT
                ENDIF
                
                // Timeout protection
                IF (Seconds() - nStartTime) > 30
                    LogEvent("ERRO", "Timeout aguardando header")
                    BREAK
                ENDIF
            ELSEIF nBytes == 0
                LogEvent("ERRO", "Conexao fechada pelo cliente")
                BREAK
            ELSE
                nError := hb_socketGetError()
                IF nError != HB_SOCKET_ERR_TIMEOUT
                    LogEvent("ERRO", "Erro no socket: " + hb_ntos(nError))
                    BREAK
                ENDIF
            ENDIF
        ENDDO

        LogEvent("DEBUG", "Dados brutos recebidos: " + cRequest)

        // Parse flexível do handshake
        IF !ParseHandshake(cRequest, @cID, @cFunction, @nLen)
            LogEvent("ERRO", "Falha no parse do handshake: " + cRequest)
            SocketSendLine(hClient, "ERROR: Handshake invalido")
            BREAK
        ENDIF

        LogEvent("DEBUG", "Handshake: ID=" + cID + ", FUNC=" + cFunction + ", LEN=" + hb_ntos(nLen))

        // Validações básicas
        IF nLen <= 0 .OR. nLen > MAX_REQUEST_SIZE
            LogEvent("ERRO", "Tamanho invalido: " + hb_ntos(nLen))
            SocketSendLine(hClient, "ERROR: Tamanho invalido")
            BREAK
        ENDIF

        // Envia ACK
        IF !SocketSendLine(hClient, "OK:" + cID)
            LogEvent("ERRO", "Falha no ACK para: " + cID)
            BREAK
        ENDIF

        LogEvent("DEBUG", "ACK enviado, aguardando " + hb_ntos(nLen) + " bytes de dados")

        // Recebe dados restantes
        cData := SocketRecvExact(hClient, nLen, 30000)
        LogEvent("DEBUG", "Dados recebidos: " + hb_ntos(Len(cData)) + "/" + hb_ntos(nLen))
        
        IF Len(cData) != nLen
            LogEvent("ERRO", "Dados incompletos: " + hb_ntos(Len(cData)) + "/" + hb_ntos(nLen))
            SocketSendLine(hClient, "ERROR: Dados incompletos")
            BREAK
        ENDIF

        // Processa dados
        IF !ProcessData(@cData)
            SocketSendLine(hClient, "ERROR: Processamento dados falhou")
            BREAK
        ENDIF

        // Processa JSON
        aItens := hb_jsonDecode(cData)
        LogEvent("DEBUG", "JSON decode: " + iif(HB_ISARRAY(aItens), "ARRAY", iif(HB_ISHASH(aItens), "HASH", "INVALIDO")))
        
        IF !HB_ISARRAY(aItens) .AND. !HB_ISHASH(aItens)
            LogEvent("ERRO", "JSON invalido - Tamanho: " + hb_ntos(Len(cData)) + " Conteudo: " + Left(cData, 100))
            SocketSendLine(hClient, "ERROR: JSON invalido")
            BREAK
        ENDIF

        // Executa função
        aResult := ExecuteFunction(cFunction, aItens)
        
        IF Empty(aResult) .OR. (HB_ISHASH(aResult) .AND. hb_HHasKey(aResult, "ERROR"))
            cErrorMsg := "Resultado vazio"
            IF HB_ISHASH(aResult) .AND. hb_HHasKey(aResult, "ERROR")
                cErrorMsg := aResult["ERROR"]
            ENDIF
            LogEvent("ERRO", "Funcao falhou: " + cFunction + " - " + cErrorMsg)
            SocketSendLine(hClient, "ERROR: " + cErrorMsg)
            BREAK
        ENDIF

        // Envia resposta
        IF !SendResponseProtheus(hClient, cID, aResult)
            LogEvent("ERRO", "Falha ao enviar resposta para: " + cID)
            BREAK
        ENDIF

        lSuccess := .T.
        LogEvent("SUCESSO", "Processamento concluido: " + cID)

    RECOVER USING oError
        
        lSuccess:=.F.
        
        IF HB_ISOBJECT(oError)
            LogEvent("ERROR", "Excecao em ProcessClient: " + ErrorMessage(oError))
        ELSE
            LogEvent("ERROR", "Excecao desconhecida em ProcessClient")
        ENDIF
        
        // Tenta enviar erro para o cliente
        BEGIN SEQUENCE
            SocketSendLine(hClient, "ERROR: Excecao no servidor")
        RECOVER
        END SEQUENCE
    END SEQUENCE

    // Fecha socket
    BEGIN SEQUENCE
        hb_socketClose(hClient)
    RECOVER
    END SEQUENCE

    // Remove thread da lista
    hb_mutexLock(s_hMtxThreads)
    hb_ADel(s_aThreads, hb_AScan(s_aThreads, hb_threadSelf(), NIL, NIL, .T.), .T.)
    hb_mutexUnlock(s_hMtxThreads)

    // Log final
    IF lSuccess
        LogEvent("SUCESSO", cID + " - " + cFunction + " - " + hb_ntos(iif(HB_ISARRAY(aItens), Len(aItens), 1)) + " itens")
    ELSE
        LogEvent("FALHA", cID + " - " + cFunction + " - Handshake: " + cRequest)
    ENDIF
RETURN

// ========================================
// FUNÇÕES AUXILIARES ROBUSTAS
// ========================================

STATIC FUNCTION ParseHandshake(cRequest, cID, cFunction, nLen)
    LOCAL aLines, aParts, cLine

    // Estratégia 1: Split por linhas
    aLines := hb_ATokens(cRequest, Chr(10))
    FOR EACH cLine IN aLines
        cLine := AllTrim(cLine)
        IF !Empty(cLine)
            // Tenta split por ":"
            aParts := hb_ATokens(cLine, ":")
            IF Len(aParts) >= 3
                cID := AllTrim(aParts[1])
                cFunction := AllTrim(aParts[2])
                nLen := Val(AllTrim(aParts[3]))
                LogEvent("DEBUG", "Parse estrategia 1: " + cID + ":" + cFunction + ":" + hb_ntos(nLen))
                RETURN .T.
            ENDIF
            
            // Estratégia 2: Split por "|"
            aParts := hb_ATokens(cLine, "|")
            IF Len(aParts) >= 3
                cID := AllTrim(aParts[1])
                cFunction := AllTrim(aParts[2])
                nLen := Val(AllTrim(aParts[3]))
                LogEvent("DEBUG", "Parse estrategia 2: " + cID + ":" + cFunction + ":" + hb_ntos(nLen))
                RETURN .T.
            ENDIF
            
            // Estratégia 3: Split por ";"
            aParts := hb_ATokens(cLine, ";")
            IF Len(aParts) >= 3
                cID := AllTrim(aParts[1])
                cFunction := AllTrim(aParts[2])
                nLen := Val(AllTrim(aParts[3]))
                LogEvent("DEBUG", "Parse estrategia 3: " + cID + ":" + cFunction + ":" + hb_ntos(nLen))
                RETURN .T.
            ENDIF
        ENDIF
    NEXT
    
    // Estratégia 4: Procura padrões no texto completo
    IF ":" $ cRequest
        aParts := hb_ATokens(cRequest, ":")
        IF Len(aParts) >= 3
            cID := AllTrim(aParts[1])
            cFunction := AllTrim(aParts[2])
            nLen := Val(AllTrim(aParts[3]))
            LogEvent("DEBUG", "Parse estrategia 4: " + cID + ":" + cFunction + ":" + hb_ntos(nLen))
            RETURN .T.
        ENDIF
    ENDIF
    
    // Estratégia 5: Última tentativa - assume formato fixo
    IF Len(cRequest) >= 10
        cID := "CLIENT_" + hb_ntos(hb_Random(1000, 9999))
        cFunction := "CALC_PRECO_OTIMIZADO"
        nLen := Val(AllTrim(cRequest))
        IF nLen > 0
            LogEvent("DEBUG", "Parse estrategia 5: " + cID + ":" + cFunction + ":" + hb_ntos(nLen))
            RETURN .T.
        ENDIF
    ENDIF
    
    LogEvent("ERRO", "Nenhuma estrategia de parse funcionou para: " + cRequest)
RETURN .F.

STATIC FUNCTION SocketRecvExact(hSocket, nLen, nTimeout)
    LOCAL cData, nTotal, cBuffer, nBytes, nStartTime, nError

    cData := ""
    nTotal := 0
    nStartTime := Seconds()
    
    DO WHILE nTotal < nLen
        // Timeout check
        IF (Seconds() - nStartTime) * 1000 > nTimeout
            LogEvent("ERRO", "Timeout SocketRecvExact: " + hb_ntos(nTotal) + "/" + hb_ntos(nLen))
            EXIT
        ENDIF
        
        cBuffer := Space(Min(4096, nLen - nTotal))
        nBytes := hb_socketRecv(hSocket, @cBuffer,,, 1000)
        
        IF nBytes > 0
            cData += Left(cBuffer, nBytes)
            nTotal += nBytes
            LogEvent("DEBUG", "SocketRecvExact progresso: " + hb_ntos(nTotal) + "/" + hb_ntos(nLen))
        ELSEIF nBytes == 0
            LogEvent("DEBUG", "SocketRecvExact: conexao fechada")
            EXIT
        ELSE
            nError := hb_socketGetError()
            IF nError != HB_SOCKET_ERR_TIMEOUT
                LogEvent("ERRO", "SocketRecvExact erro: " + hb_ntos(nError))
                EXIT
            ENDIF
        ENDIF
    ENDDO
    
    LogEvent("DEBUG", "SocketRecvExact final: " + hb_ntos(Len(cData)) + "/" + hb_ntos(nLen))
RETURN cData

STATIC FUNCTION SocketSendLine(hSocket, cData)
    LOCAL nSent, cDataToSend
    
    BEGIN SEQUENCE WITH {|oError| Break(oError)}
        cDataToSend := cData + Chr(13) + Chr(10)
        nSent := hb_socketSend(hSocket, cDataToSend, Len(cDataToSend), 0, 5000)
    RECOVER
        nSent := 0
    END SEQUENCE
    
RETURN (nSent == Len(cDataToSend))

STATIC FUNCTION ProcessData(cData)
    LOCAL lSuccess, cUncompressed

    lSuccess := .T.
    
    BEGIN SEQUENCE WITH {|oError| Break(oError)}
        // Verifica se está compactado
        IF Left(cData, 2) == "GZ"
            cUncompressed := hb_ZUncompress(SubStr(cData, 3))
            IF !Empty(cUncompressed)
                cData := cUncompressed
                LogEvent("DEBUG", "Dados descomprimidos: " + hb_ntos(Len(cData)) + " bytes")
            ELSE
                LogEvent("ERRO", "Falha na descompressao GZ")
                lSuccess := .F.
            ENDIF
        ENDIF
    RECOVER
        LogEvent("ERRO", "Excecao no processamento de dados")
        lSuccess := .F.
    END SEQUENCE
    
RETURN lSuccess

STATIC FUNCTION ExecuteFunction(cFunction, aItens)
    LOCAL aResult, oError
    
    // Validações
    IF Empty(cFunction) .OR. !HB_ISSTRING(cFunction)
        RETURN {"ERROR" => "Funcao invalida: " + iif(HB_ISSTRING(cFunction), cFunction, "NÃO-STRING")}
    ENDIF
    
    IF aItens == NIL
        RETURN {"ERROR" => "Dados de entrada nulos"}
    ENDIF

    LogEvent("DEBUG", "Executando funcao: " + cFunction + " com " + ;
        iif(HB_ISARRAY(aItens), hb_ntos(Len(aItens)) + " itens", ;
        iif(HB_ISHASH(aItens), "HASH", "tipo " + ValType(aItens))))

    BEGIN SEQUENCE WITH {|oError| Break(oError)}
        DO CASE
        CASE Upper(cFunction) == "CALC_PRECO_OTIMIZADO"
            aResult := CALC_PRECO_OTIMIZADO(aItens)
            
        CASE Upper(cFunction) == "PROCESSAR_LOTE_MASSIVO"  
            aResult := PROCESSAR_LOTE_MASSIVO(aItens)
            
        OTHERWISE
            aResult := {"ERROR" => "Funcao desconhecida: " + cFunction}
        ENDCASE
    RECOVER USING oError
        LogEvent("ERROR", "Excecao em " + cFunction + ": " + ErrorMessage(oError))
        aResult := {"ERROR" => "Excecao: " + ErrorMessage(oError)}
    END SEQUENCE
    
    // Garante retorno válido
    IF aResult == NIL
        aResult := {"ERROR" => "Funcao retornou NIL"}
    ELSEIF !(HB_ISARRAY(aResult) .OR. HB_ISHASH(aResult))
        aResult := {"ERROR" => "Tipo de retorno invalido: " + ValType(aResult)}
    ENDIF
    
    LogEvent("DEBUG", "Resultado: " + iif(HB_ISARRAY(aResult), "ARRAY", "HASH") + " tamanho: " + ;
        iif(HB_ISARRAY(aResult), hb_ntos(Len(aResult)), "1"))
        
RETURN aResult

STATIC FUNCTION SendResponseProtheus(hSocket, cID, aResult)
    LOCAL lSuccess, cResponse, cHeader, nSent
    
    BEGIN SEQUENCE WITH {|oError| Break(oError)}
        // Codificação segura do JSON
        cResponse := hb_jsonEncode(aResult, .F.)
        IF Empty(cResponse)
            cResponse := '{"ERROR":"Falha na codificacao JSON"}'
        ENDIF
        
        cHeader := "RES:" + cID + ":" + hb_ntos(Len(cResponse)) + Chr(13) + Chr(10)
        
        // Envia header
        nSent := hb_socketSend(hSocket, cHeader, Len(cHeader), 0, 5000)
        IF nSent != Len(cHeader)
            LogEvent("ERRO", "Header resposta incompleto")
            BREAK
        ENDIF
        
        // Envia dados
        nSent := hb_socketSend(hSocket, cResponse, Len(cResponse), 0, 10000)
        IF nSent != Len(cResponse)
            LogEvent("ERRO", "Dados resposta incompletos")
            BREAK
        ENDIF
        
        lSuccess := .T.
        
    RECOVER
        lSuccess := .F.
    END SEQUENCE
    
RETURN lSuccess

PROCEDURE Progress(nProgress, nRow, nCol)
    LOCAL cChar, oError
    
    BEGIN SEQUENCE WITH {|oError| Break(oError)}
        cChar := SubStr("-\|/", (nProgress % 4) + 1, 1)
        @ nRow, nCol SAY "[" + cChar + "]"
        nProgress++
    RECOVER USING oError
        // Ignora erro no display
        LogEvent("ERRO", ErrorMessage(oError))
    END SEQUENCE
RETURN

STATIC PROCEDURE LogEvent(cType, cMessage)
    LOCAL hFile, cLogEntry
    
    BEGIN SEQUENCE WITH {|oError| Break(oError)}
        cLogEntry := DToC(Date()) + " " + Time() + " | " + cType + " | " + cMessage + hb_eol()
        
        hFile := FCreate("server_robusto.log", FC_NORMAL)
        IF hFile != -1
            FWrite(hFile, cLogEntry)
            FClose(hFile)
        ENDIF
    RECOVER
        // Fallback silencioso
    END SEQUENCE
RETURN

STATIC FUNCTION ErrorMessage(oError)
    LOCAL cMessage

    cMessage := ""
    
    IF HB_ISOBJECT(oError)
        IF HB_ISSTRING(oError:description)
            cMessage += oError:description
        ELSE
            cMessage += "Erro sem descricao"
        ENDIF
        
        IF !Empty(oError:operation) .AND. HB_ISSTRING(oError:operation)
            cMessage += " [" + oError:operation + "]"
        ENDIF
    ELSEIF HB_ISSTRING(oError)
        cMessage := oError
    ELSE
        cMessage := "Tipo de erro: " + ValType(oError)
    ENDIF

RETURN cMessage