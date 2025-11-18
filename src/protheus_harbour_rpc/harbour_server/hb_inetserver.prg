// hb_inetserver.prg - Servidor RPC Otimizado
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

// Variável global para controle de execução
STATIC lRunning := .F.

PROCEDURE Main(cPort)
    LOCAL nPort := iif(Empty(cPort), 123456, Val(cPort))
    LOCAL skServer, skClient
    LOCAL nTurn := 0

    hb_cdpSelect("UTF8EX")
    SET DATE ANSI
    SET CENTURY ON

    CLS
    hb_inetInit()

    @ 01, 15 SAY "HARBOUR RPC SERVER - Processamento Otimizado"
    @ 02, 05 SAY "Porta: " + hb_ntos(nPort) + " | [ESC] para sair"
    @ 03, 05 SAY "Aguardando conexoes..."

    skServer := hb_inetServer(nPort)
    
    IF skServer == NIL
        @ 04, 05 SAY "ERRO: Nao foi possivel criar o servidor na porta " + hb_ntos(nPort)
        Inkey(0)
        RETURN
    ENDIF
    
    hb_inetTimeout(skServer, 100) // Timeout baixo para resposta rápida
    lRunning := .T.

    DO WHILE lRunning
        skClient := hb_inetAccept(skServer)
        
        IF skClient != NIL
            hb_threadStart(@ProcessClient(), skClient, @nTurn)
        ENDIF

        Progress(@nTurn, 4, 40)
        
        IF Inkey(0.1) == 27
            lRunning := .F.
            EXIT
        ENDIF
    ENDDO

    @ 05, 05 SAY "Finalizando servidor..."
    hb_inetClose(skServer)
    hb_inetCleanup()
    @ 06, 05 SAY "Servidor finalizado."
RETURN

STATIC PROCEDURE ProcessClient(skClient)
    
    LOCAL aRequest
    LOCAL cRequest, cID, nLen, cData, aItens, aResult, cResponse
    LOCAL nRecv, nChunk, nToRecv, cChunk, nTurnLocal := 0
    LOCAL cFunction
    LOCAL lGZCompress
    LOCAL cClientAddr

    hb_inetTimeout(skClient, 30000)
    cClientAddr := hb_inetAddress(skClient)

    @ 05, 05 SAY "Cliente: " + cClientAddr + Space(20)
    @ 06, 05 SAY "Recebendo dados..." + Space(20)

    // Handshake:FUNCTION:LENGTH
    cRequest := Space(1024)
    nRecv := hb_inetRecv(skClient, @cRequest, 1024)

    IF nRecv <= 0
        hb_inetSendAll(skClient, "ERROR: Handshake:FUNCTION:LENGTH" + hb_eol())
        hb_inetClose(skClient)
        LogEvent("ERROR", "Handshake:FUNCTION:LENGTH - Cliente: " + cClientAddr)
        RETURN
    ENDIF
    
    // Remove caracteres nulos e espaços extras
    cRequest := AllTrim(StrTran(cRequest, Chr(0), ""))
    
    aRequest := hb_ATokens(cRequest, ":")
    
    IF Len(aRequest) < 3
        hb_inetSendAll(skClient, "ERROR: Formato handshake invalido" + hb_eol())
        hb_inetClose(skClient)
        LogEvent("ERROR", "Formato handshake invalido - Cliente: " + cClientAddr)
        RETURN
    ENDIF
    
    cID := AllTrim(aRequest[1])
    cFunction := AllTrim(aRequest[2])
    nLen := Val(AllTrim(aRequest[3]))

    IF nLen <= 0 .OR. nLen > 100000000 // Limite de 100MB
        hb_inetSendAll(skClient, "ERROR:" + cID + hb_eol())
        hb_inetClose(skClient)
        LogEvent("ERROR", cID + " - Tamanho invalido: " + hb_ntos(nLen))
        RETURN
    ENDIF

    // ACK
    IF hb_inetSendAll(skClient, "OK:" + cID + hb_eol()) <= 0
        hb_inetClose(skClient)
        LogEvent("ERROR", cID + " - Falha no ACK")
        RETURN
    ENDIF

    // Recebe dados
    cData := ""
    nChunk := 0
    
    @ 07, 05 SAY "Recebendo: 0/" + hb_ntos(nLen) + " bytes"
    
    DO WHILE Len(cData) < nLen .AND. lRunning
        nToRecv := Min(65536, nLen - Len(cData))
        cChunk := Space(nToRecv)
        nRecv := hb_inetRecv(skClient, @cChunk, nToRecv)
        
        IF nRecv <= 0
            EXIT
        ENDIF
        
        nChunk += nRecv
        cData += Left(cChunk, nRecv)
        
        @ 07, 05 SAY "Recebendo: " + hb_ntos(Len(cData)) + "/" + hb_ntos(nLen) + " bytes"
        Progress(@nTurnLocal, 8, 40)
        
        // Timeout de segurança
        IF nChunk = 0 .AND. Len(cData) = 0
            EXIT
        ENDIF
    ENDDO

    IF nChunk <= 0 .OR. Len(cData) != nLen
        hb_inetSendAll(skClient, "ERROR:" + cID + hb_eol())
        hb_inetClose(skClient)
        LogEvent("ERROR", cID + " - Dados incompletos: " + hb_ntos(Len(cData)) + "/" + hb_ntos(nLen))
        RETURN
    ENDIF
    
    @ 07, 05 SAY "Dados recebidos: " + hb_ntos(Len(cData)) + " bytes" + Space(10)
    
    // Descompacta se necessário
    lGZCompress := (Left(cData, 2) == "GZ")
    IF lGZCompress
        BEGIN SEQUENCE
            cData := hb_ZUncompress(SubStr(cData, 3))
        RECOVER
            hb_inetSendAll(skClient, "ERROR:" + cID + " - Falha descompressao" + hb_eol())
            hb_inetClose(skClient)
            LogEvent("ERROR", cID + " - Falha descompressao")
            RETURN
        END SEQUENCE
    ENDIF

    // Processa conforme função solicitada
    BEGIN SEQUENCE
        aItens := hb_jsonDecode(cData)
    RECOVER
        aItens := NIL
    END SEQUENCE
    
    IF HB_ISARRAY(aItens)
        @ 08, 05 SAY "Processando: " + hb_ntos(Len(aItens)) + " itens" + Space(10)
        
        BEGIN SEQUENCE
            DO CASE
                CASE cFunction == "CALC_PRECO_OTIMIZADO"
                    aResult := CALC_PRECO_OTIMIZADO(aItens)
                    
                CASE cFunction == "PROCESSAR_LOTE_MASSIVO"
                    aResult := PROCESSAR_LOTE_MASSIVO(aItens)
                    
                OTHERWISE
                    aResult := {"ERROR" => "Funcao desconhecida: " + cFunction}
            ENDCASE
        RECOVER
            aResult := {"ERROR" => "Excecao ao processar " + cFunction}
        END SEQUENCE
    ELSE
        aResult := {"ERROR" => "Dados JSON invalidos"}
        LogEvent("ERROR", cID + " - Dados JSON invalidos")
    ENDIF

    // Envia resposta
    BEGIN SEQUENCE
        cResponse := hb_jsonEncode(aResult, .F.)
        
        IF Empty(cResponse)
            cResponse := hb_jsonEncode({"ERROR" => "Resposta vazia"}, .F.)
        ENDIF
        
        // Compressão desabilitada por enquanto
        lGZCompress := .F.
        IF lGZCompress
            cResponse := "GZ" + hb_ZCompress(cResponse)
        ENDIF
        
        // Envia header de resposta
        IF hb_inetSendAll(skClient, "RES:" + cID + ":" + hb_NToC(Len(cResponse)) + hb_eol()) <= 0
            LogEvent("ERROR", cID + " - Falha envio header")
        ELSE
            // Envia dados
            IF hb_inetSendAll(skClient, cResponse) <= 0
                LogEvent("ERROR", cID + " - Falha envio dados")
            ELSE
                @ 09, 05 SAY "Processado: " + hb_ntos(IIF(HB_ISARRAY(aItens), Len(aItens), 0)) + " itens" + Space(10)
                LogEvent("SUCCESS", cID + " - " + cFunction + " - " + hb_ntos(IIF(HB_ISARRAY(aItens), Len(aItens), 0)) + " itens")
            ENDIF
        ENDIF
        
    RECOVER
        hb_inetSendAll(skClient, "ERROR:" + cID + " - Falha codificacao resposta" + hb_eol())
        LogEvent("ERROR", cID + " - Falha codificacao resposta")
    END SEQUENCE

    hb_inetClose(skClient)
RETURN

PROCEDURE Progress(nProgress, nRow, nCol)
    LOCAL cChar := SubStr("-\|/", (nProgress % 4) + 1, 1)
    @ nRow, nCol SAY "[" + cChar + "]"
    nProgress++
RETURN

STATIC PROCEDURE LogEvent(cType, cMessage)
    
    LOCAL hFile, cLogEntry, oError
    
    BEGIN SEQUENCE WITH {|oError| Break(oError)}
        cLogEntry := DToC(Date()) + " " + Time() + " | " + cType + " | " + cMessage + hb_eol()
        
        hFile := FOpen("server_robusto.log", FO_READWRITE)
        
        IF hFile == -1
            hFile := FCreate("server_robusto.log", FC_NORMAL)
        ELSE
            FSeek(hFile, 0, FS_END)
        ENDIF
        
        IF hFile != -1
            FWrite(hFile, cLogEntry)
            FClose(hFile)
        ENDIF
    RECOVER USING oError
        // Fallback silencioso - apenas mostra no console em debug
        ? "Erro log: " + ErrorMessage(oError)
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