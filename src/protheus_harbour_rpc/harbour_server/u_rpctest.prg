#include "hbclass.ch"
#include "hbhash.ch"
#include "hbsocket.ch"
#include "hb_rpc_tst.ch"

/*u_rpctest.prg - Cliente RPC para Harbour
Adaptado do código Protheus/ADVPL original*/

/*{Harbour.doc} TESTERPC()
Teste de cliente RPC para processamento otimizado com comparativo Harbour
@author SeuNome
@since 01/01/2024
@version 1.0
*/

REQUEST HB_CODEPAGE_UTF8EX
REQUEST CALC_PRECO_OTIMIZADO
REQUEST PROCESSAR_LOTE_MASSIVO

PROCEDURE MAIN()

    hb_cdpSelect("UTF8EX")
    SET DATE ANSI
    SET CENTURY ON

    CLS

    TESTERPC()
    //TESTELOTEMASSIVO()

RETURN

FUNCTION TESTERPC()

    LOCAL aItens := {}
    LOCAL nI, xResultRPC, aResultHarbour
    LOCAL nStart, nEnd, nTimeRPC, nTimeHarbour,nTimeHarbourC
    LOCAL nGanho
    LOCAL hDados := { => }
    LOCAL nJ
    LOCAL nFalhas := 0

    // Gera dados de teste - 50K itens para teste comparativo
    ? "Gerando dados de teste..."
    nStart := hb_MilliSeconds()
    FOR nI := 1 TO 50000
        AAdd(aItens, {nI, 100 + Mod(nI, 1000), 50.00 + Mod(nI, 200)})
    NEXT
    nEnd := hb_MilliSeconds()

    ? "==================================================="
    ? "TESTE DE PERFORMANCE: RPC vs HARBOUR vs HARBOUR + C"
    ? "==================================================="
    ? "Tempo geracao dados: " + Str(nEnd - nStart, 10, 2) + "ms"
    ? "Itens preparados: " + hb_NToC(Len(aItens))
    ? ""

    hDados["dados"] := aItens

    FOR nJ := 1 TO 3  // Reduzido para 3 iterações para testes

        // Processa via Harbour (implementacao pura)
        ?
        ? "PROCESSAMENTO HARBOUR:"
        ? "-------------------"
        nStart := hb_MilliSeconds()
        aResultHarbour := ProcessarHarbour(hDados)
        nEnd := hb_MilliSeconds()
        nTimeHarbour := nEnd - nStart

        IF aResultHarbour != NIL .AND. Len(aResultHarbour) > 0
            ? "Tempo processamento Harbour: " + Str(nTimeHarbour, 10, 2) + "ms"
            ? "Primeiro preco: R$ " + Transform(aResultHarbour[1][2], "@E 999,999.99")
            ? "Ultimo preco: R$ " + Transform(aResultHarbour[Len(aResultHarbour)][2], "@E 999,999.99")
        ELSE
            nFalhas++
            ? "Falha no processamento Harbour"
        ENDIF

        ?

        // Processa via Harbour (implementacao C)
        ?
        ? "PROCESSAMENTO HARBOUR + C:"
        ? "-------------------"
        nStart := hb_MilliSeconds()
        aResultHarbour := CALC_PRECO_OTIMIZADO(hDados["dados"])
        nEnd := hb_MilliSeconds()
        nTimeHarbourC := nEnd - nStart

        IF aResultHarbour != NIL .AND. Len(aResultHarbour) > 0
            ? "Tempo processamento Harbour + C: " + Str(nTimeHarbourC, 10, 2) + "s"
            ? "Primeiro preco: R$ " + Transform(aResultHarbour[1][2], "@E 999,999.99")
            ? "Ultimo preco: R$ " + Transform(aResultHarbour[Len(aResultHarbour)][2], "@E 999,999.99")
        ELSE
            nFalhas++
            ? "Falha no processamento Harbour"
        ENDIF

        ?

        // Processa via RPC
        ? "PROCESSAMENTO RPC + C:"
        ? "---------------------"
        nStart := hb_MilliSeconds()
        xResultRPC := RPCProcessar("CALC_PRECO_OTIMIZADO", hDados)
        nEnd := hb_MilliSeconds()
        nTimeRPC := nEnd - nStart

        IF ValType(xResultRPC) == "A" .AND. Len(xResultRPC) > 0
            ? "Tempo processamento RPC: " + Str(nTimeRPC, 10, 2) + "ms"
            ? "Primeiro preco: R$ " + Transform(xResultRPC[1][2], "@E 999,999.99")
            ? "Ultimo preco: R$ " + Transform(xResultRPC[Len(xResultRPC)][2], "@E 999,999.99")

            ?
            ? "=============================================="
            ? "RESUMO:"
            ? "HARBOUR: " + Str(nTimeHarbour, 8, 2) + " Milisegundos"
            ? "HARBOUR + C: " + Str(nTimeHarbourC, 8, 2) + " Milisegundos"
            ? "RPC+C: " + Str(nTimeRPC, 8, 2) + " Milisegundos"

            IF nTimeHarbour > 0 .AND. nTimeRPC > 0
                nGanho := ((nTimeHarbour - nTimeRPC) / nTimeHarbour) * 100
                ? "GANHO Harbour x RPC: " + Str(nGanho, 8, 1) + "% mais rapido"
                ? "RPC eh: " + Str(nTimeHarbour / nTimeRPC, 6, 1) + "x mais rapido"
                nGanho := ((nTimeHarbourC - nTimeRPC) / nTimeHarbourC) * 100
                ? "GANHO Harbour + C x RPC: " + Str(nGanho, 8, 1) + "% mais rapido"
                ? "RPC eh: " + Str(nTimeHarbour / nTimeRPC, 6, 1) + "x mais rapido"
                nGanho := ((nTimeHarbourC - nTimeHarbour) / nTimeHarbourC) * 100
                ? "GANHO Harbour + C x Harbour: " + Str(nGanho, 8, 1) + "% mais rapido"
                ? "Harbour C eh: " + Str(nTimeHarbourC / nTimeHarbour, 6, 1) + "x mais rapido"
            ENDIF
            ? "=============================================="

        ELSEIF ValType(xResultRPC) == "H" .AND. hb_HHasKey(xResultRPC, "ERROR")
            nFalhas++
            ? "ERRO no processamento RPC: " + hb_JsonEncode(xResultRPC, .F.)
        ELSE
            nFalhas++
            ? "Falha no processamento RPC - Tipo: " + ValType(xResultRPC)
        ENDIF

    NEXT nJ

    ?
    ? "=============================================="
    ? "Falhas no processamento RPC: " + hb_NToC(nFalhas)

RETURN xResultRPC

/*{Harbour.doc} ProcessarHarbour()
Processa dados puramente em Harbour (mesma logica do C)
@type function
@param hDados Hash - Dados para processamento
@return Array - Resultado do processamento
*/

FUNCTION ProcessarHarbour(hDados)
    LOCAL aResult := {}
    LOCAL nI, aItem, nQty, nPreco, nDesconto
    LOCAL nPrecoFinal
    LOCAL aDados

    IF hb_HHasKey(hDados, "dados")
        aDados := hDados["dados"]
    ELSE
        aDados := {}
    ENDIF

    // Mesma logica implementada em C
    FOR nI := 1 TO Len(aDados)
        aItem := aDados[nI]

        IF Len(aItem) >= 3
            nQty := aItem[2]
            nPreco := aItem[3]

            // EXATAMENTE a mesma logica de desconto do C
            IF nQty > 100
                nDesconto := 0.15
            ELSEIF nQty > 50
                nDesconto := 0.10
            ELSE
                nDesconto := 0.05
            ENDIF

            nPrecoFinal := nPreco * (1.0 - nDesconto)

            AAdd(aResult, {nI, nPrecoFinal})
        ENDIF
    NEXT nI

RETURN aResult

/*{Harbour.doc} ProcessarLoteHarbour()
Processamento mais complexo em Harbour (equivalente ao C)
@type function
@param hDados Hash - Dados para processamento
@return Array - Resultado do processamento
*/

FUNCTION ProcessarLoteHarbour(hDados)
    LOCAL aResult := {}
    LOCAL nI, aItem, nQty, nPreco
    LOCAL nMargem, nImposto, nPrecoFinal
    LOCAL aDados

    IF hb_HHasKey(hDados, "dados")
        aDados := hDados["dados"]
    ELSE
        aDados := {}
    ENDIF

    FOR nI := 1 TO Len(aDados)
        aItem := aDados[nI]

        IF Len(aItem) >= 3
            nQty := aItem[2]
            nPreco := aItem[3]

            // Mesma logica complexa do C
            nMargem := 0.30  // Margem padrão
            nImposto := 0.18 // ICMS + PIS + COFINS

            IF nQty > 1000
                nMargem := 0.25
                nImposto := 0.16
            ELSEIF nQty > 500
                nMargem := 0.28
                nImposto := 0.17
            ENDIF

            nPrecoFinal := nPreco * (1.0 + nMargem) * (1.0 - nImposto)

            AAdd(aResult, {nI, nPrecoFinal, nMargem * 100})
        ENDIF
    NEXT nI

RETURN aResult

/*{Harbour.doc} TESTELOTEMASSIVO()
Teste com processamento mais complexo
@type function
*/

PROCEDURE TESTELOTEMASSIVO()
    LOCAL aItens := {}
    LOCAL nI, xResultRPC, aResultHarbour
    LOCAL nStart, nEnd, nTimeRPC, nTimeHarbour, nGanho
    LOCAL hDados := { => }
    LOCAL nJ
    LOCAL nFalhas := 0

    // Gera dados de teste
    ? "Gerando dados para lote massivo..."
    nStart := hb_MilliSeconds()
    FOR nI := 1 TO 10000  // Reduzido para testes
        AAdd(aItens, {nI, 500 + Mod(nI, 1500), 100.00 + Mod(nI, 300)})
    NEXT
    nEnd := hb_MilliSeconds()

    hDados["dados"] := aItens

    ? "=============================================="
    ? "TESTE DE PERFORMANCE: RPC vs HARBOUR"
    ? "=============================================="
    ? "Tempo geracao dados: " + Str(nEnd - nStart, 10, 2) + "ms"
    ? "Itens preparados: " + hb_NToC(Len(aItens))
    ? ""

    FOR nJ := 1 TO 2  // Reduzido para 2 iterações para testes

        ?
        ? "TESTE PROCESSAMENTO COMPLEXO:"
        ? "============================="

        // Harbour
        nStart := hb_MilliSeconds()
        aResultHarbour := ProcessarLoteHarbour(hDados)
        nEnd := hb_MilliSeconds()
        nTimeHarbour := nEnd - nStart
        ? "HARBOUR: " + Str(nEnd - nStart, 6, 2) + "s - " + ;
            "Preco: R$ " + Transform(aResultHarbour[1][2], "@E 999,999.99")

        ? "HARBOUR: " + Str(nTimeHarbour, 8, 2) + " Milisegundos"

        // Harbour + C
        nStart := hb_MilliSeconds()
        aResultHarbour := PROCESSAR_LOTE_MASSIVO(hDados["dados"])
        nEnd := hb_MilliSeconds()
        nTimeHarbour := nEnd - nStart
        ? "HARBOUR + C: " + Str(nEnd - nStart, 6, 2) + "s - " + ;
            "Preco: R$ " + Transform(aResultHarbour[1][2], "@E 999,999.99")

        ? "HARBOUR + C: " + Str(nTimeHarbour, 8, 2) + " Milisegundos"

        // RPC
        nStart := hb_MilliSeconds()
        xResultRPC := RPCProcessar("PROCESSAR_LOTE_MASSIVO", hDados)
        nEnd := hb_MilliSeconds()
        nTimeRPC := nEnd - nStart

        IF ValType(xResultRPC) == "A" .AND. Len(xResultRPC) > 0
            ? "RPC+C: " + Str(nEnd - nStart, 6, 2) + "s - " + ;
                "Preco: R$ " + Transform(xResultRPC[1][2], "@E 999,999.99")
            ?
            ? "=============================================="
            ? "RESUMO:"
            ? "HARBOUR: " + Str(nTimeHarbour, 8, 2) + " Milisegundos"
            ? "RPC+C: " + Str(nTimeRPC, 8, 2) + " Milisegundos"

            IF nTimeHarbour > 0 .AND. nTimeRPC > 0
                nGanho := ((nTimeHarbour - nTimeRPC) / nTimeHarbour) * 100
                ? "GANHO: " + Str(nGanho, 8, 1) + "% mais rapido"

                IF nGanho > 0
                    ? "RPC eh: " + Str(nTimeHarbour / nTimeRPC, 6, 1) + "x mais rapido"
                ENDIF
            ENDIF
            ? "=============================================="

        ELSEIF ValType(xResultRPC) == "H" .AND. hb_HHasKey(xResultRPC, "ERROR")
            nFalhas++
            ? "ERRO no processamento RPC: " + hb_JsonEncode(xResultRPC, .T.)
        ELSE
            nFalhas++
            ? "Falha no processamento RPC - Tipo: " + ValType(xResultRPC)
        ENDIF

    NEXT nJ

    ?
    ? "=============================================="
    ? "Falhas no processamento RPC: " + hb_NToC(nFalhas)

RETURN

// ==================================================
// FUNÇÕES RPC (CORRIGIDAS)
// ==================================================

/*{Harbour.doc} RPCProcessar()
Processa dados via RPC
@type function
@param cFuncao Caracter - Funcao a executar
@param hDados Hash - Dados para processamento
@return Array/Hash - Resultado do processamento
*/

FUNCTION RPCProcessar(cFuncao, hDados)
    LOCAL oResult := NIL
    LOCAL lConectado := .F.
    LOCAL nTentativa
    LOCAL oSocket
    LOCAL aAddR

    // Cria socket
    oSocket := HB_SocketOpen()
    IF oSocket == NIL
        ? "ERRO: Nao foi possivel criar socket"
        oResult := { => }
        oResult["ERROR"] := "Nao foi possivel criar socket"
        RETURN oResult
    ENDIF

    aAddR := { HB_SOCKET_AF_INET, "127.0.0.1", 123456 }

    // Tenta conexao
    FOR nTentativa := 1 TO 3
        
        lConectado := HB_SocketConnect(oSocket, aAddR)
        
        IF lConectado
            EXIT
        ELSE
            ? "Tentativa " + hb_NToC(nTentativa) + " de conexao falhou"
            hb_IdleSleep(1)
        ENDIF
    NEXT nTentativa

    IF lConectado
        IF SendRPCDados(oSocket, cFuncao, hDados)
            oResult := ReceiveRPCResult(oSocket)
        ELSE
            oResult := { => }
            oResult["ERROR"] := "Falha ao enviar dados"
        ENDIF
        HB_SocketClose(oSocket)
    ELSE
        ? "ERRO: Nao foi possivel conectar ao servidor RPC"
        oResult := { => }
        oResult["ERROR"] := "Nao foi possivel conectar ao servidor"
    ENDIF

RETURN oResult

/*{Harbour.doc} SendRPCDados()
Envia dados para servidor RPC - VERSÃO CORRIGIDA
@type static function
*/

STATIC FUNCTION SendRPCDados(oSocket, cFuncao, hDados)
    LOCAL cDadosJSON
    LOCAL cID := GetUniqueID()
    LOCAL nTotal
    LOCAL cHeader
    LOCAL cResp := Space(100)
    LOCAL nBytes, lRet := .F.
    LOCAL nSent
    LOCAL cExpected

    // Converte dados para JSON
    cDadosJSON := hb_JsonEncode(hDados["dados"], .F.)
    nTotal := Len(cDadosJSON)

    ? "Enviando " + hb_NToC(nTotal) + " bytes de dados..."

    // CORREÇÃO: Mantém formato ID:FUNCAO:TAMANHO que o servidor espera
    cHeader := cID + ":" + cFuncao + ":" + StrZero(nTotal, 10)
    
    nBytes := HB_SocketSend(oSocket, cHeader)
    IF nBytes > 0
        ? "Header enviado: " + cHeader
        
        // CORREÇÃO: Aguarda confirmação OK:ID antes de enviar dados
        nBytes := HB_SocketRecv(oSocket, @cResp, Len(cResp), 0, 5000)
        IF nBytes > 0
            cResp := AllTrim(StrTran(cResp, Chr(10), ""))
            ? "Resposta header: " + cResp
            
            // CORREÇÃO: Verifica se recebeu OK:ID
            cExpected:="OK:" + cID
            IF cExpected == Left(cResp,Len(cExpected))
                ? "Handshake OK, enviando dados..."
                
                // Envia dados
                nSent := HB_SocketSend(oSocket, cDadosJSON)
                IF nSent == nTotal
                    ? "Dados enviados com sucesso: " + hb_NToC(nSent) + " bytes"
                    lRet := .T.
                ELSE
                    ? "ERRO: Dados parcialmente enviados: " + hb_NToC(nSent) + "/" + hb_NToC(nTotal)
                ENDIF
            ELSE
                ? "ERRO: Handshake falhou - Esperado: OK:" + cID + " - Recebido: " + cResp
            ENDIF
        ELSE
            ? "ERRO: Timeout ou erro no handshake"
        ENDIF
    ELSE
        ? "ERRO ao enviar header RPC"
    ENDIF

RETURN lRet

/*{Harbour.doc} ReceiveRPCResult()
Recebe resultado do RPC - VERSÃO OTIMIZADA
@type static function
*/

STATIC FUNCTION ReceiveRPCResult(oSocket)
    LOCAL cBuffer := "", nLen := 0, cData := "", nRecv := 0
    LOCAL oResult := NIL
    LOCAL nStartTime, nBytes
    LOCAL aResp
    LOCAL lHeaderProcessed := .F.
    LOCAL nChunkSize 
    LOCAL cChunk 
    LOCAL nHeaderEnd
    LOCAL cHeader
    LOCAL nProgress
    LOCAL nTotalTime
    LOCAL nJsonStart
    LOCAL nJsonTime
    LOCAL cType
    LOCAL nElements

    STATIC nLastTimeout := 0

    ? "Aguardando resposta RPC..."

    nStartTime := hb_MilliSeconds()

    // *** VERSÃO OTIMIZADA: Recebe dados grandes mais eficientemente ***
    WHILE (hb_MilliSeconds()- nStartTime) < 6000 .AND. (!lHeaderProcessed .OR. nRecv < nLen)
        
        nChunkSize := IIF(lHeaderProcessed, Min(65536, nLen - nRecv), 1024)
        cChunk := Space(nChunkSize)
        
        nBytes := HB_SocketRecv(oSocket, @cChunk, nChunkSize, 0, 5000)
        
        IF nBytes > 0
            cBuffer += Left(cChunk, nBytes)
            
            // Processa header se ainda não foi feito
            IF !lHeaderProcessed .AND. "RES:" $ cBuffer
                                
                // Procura por quebra de linha no header
                nHeaderEnd := At(Chr(10), cBuffer)
                IF nHeaderEnd == 0
                    nHeaderEnd := At(Chr(13), cBuffer)
                ENDIF
                IF nHeaderEnd == 0 .AND. Len(cBuffer) > 50
                    // Se não encontrou quebra, assume header nos primeiros 50 chars
                    nHeaderEnd := 50
                ENDIF
                
                IF nHeaderEnd > 0
                    cHeader := AllTrim(Left(cBuffer, nHeaderEnd - 1))
                    
                    // Verifica se é um header válido
                    IF "RES:" $ cHeader
                        aResp := hb_ATokens(cHeader, ":")
                        IF Len(aResp) >= 3
                            nLen := Val(aResp[3])
                            ? "Header: " + cHeader
                            ? "Tamanho dados: " + hb_NToC(nLen)
                            
                            // Remove header do buffer
                            cBuffer := SubStr(cBuffer, nHeaderEnd + 1)
                            // Remove LF adicional se existir (caso CR+LF)
                            IF Len(cBuffer) > 0 .AND. Left(cBuffer, 1) == Chr(10)
                                cBuffer := SubStr(cBuffer, 2)
                            ENDIF
                            
                            lHeaderProcessed := .T.
                            nRecv := Len(cBuffer)
                            cData := cBuffer
                            
                            ? "Dados iniciais: " + hb_NToC(nRecv) + " bytes"
                        ENDIF
                    ENDIF
                ENDIF
            ELSEIF lHeaderProcessed
                // Apenas acumula dados
                cData += Left(cChunk, nBytes)
                nRecv := Len(cData)
                
                // Feedback otimizado (menos verbose para dados grandes)
                nProgress := (nRecv / nLen) * 100
                IF nProgress % 10 == 0 .AND. nProgress != 0
                    ? "Progresso: " + hb_NToC(nRecv) + "/" + hb_NToC(nLen) + ;
                      " (" + hb_NToC(Round(nProgress, 0)) + "%)"
                ENDIF
            ENDIF
            
            // Sai do loop se recebeu todos os dados
            IF lHeaderProcessed .AND. nRecv >= nLen
                EXIT
            ENDIF
            
        ELSEIF nBytes == 0
            ? "Conexao fechada"
            EXIT
        ELSE
            // Timeout - feedback menos frequente
            IF hb_MilliSeconds() - nLastTimeout > 2
                ? "Aguardando dados..." + IIF(lHeaderProcessed, ;
                  " " + hb_NToC(nRecv)+ "/" + hb_NToC(nLen), "")
                nLastTimeout := hb_MilliSeconds()
            ENDIF
            hb_IdleSleep(0.05)
        ENDIF
    ENDDO

    nTotalTime := hb_MilliSeconds() - nStartTime
    ? "Tempo recebimento: " + AllTrim(Str(nTotalTime, 10, 2)) + "ms"
    ? "Dados recebidos: " + hb_NToC(nRecv) + "/" + hb_NToC(nLen) + ;
      " bytes | Header: " + IIF(lHeaderProcessed, "OK", "FALHA")

    // Processa resultado final
    IF lHeaderProcessed .AND. nRecv >= nLen
        // Ajusta para tamanho exato
        IF Len(cData) > nLen
            cData := Left(cData, nLen)
        ENDIF
        
        ? "Processando JSON (" + hb_NToC(Len(cData)) + " bytes)..."
        
        // Mede tempo de decodificação JSON
        nJsonStart := hb_MilliSeconds()
        BEGIN SEQUENCE WITH {|oErr| Break(oErr) }
            oResult := hb_JsonDecode(cData)
        RECOVER
            oResult := NIL
        END SEQUENCE
        nJsonTime := hb_MilliSeconds() - nJsonStart
        
        IF oResult == NIL
            ? "ERRO: Falha ao decodificar JSON (" + AllTrim(Str(nJsonTime, 10, 3)) + "s)"
            oResult := { => }
            oResult["ERROR"] := "JSON invalido"
        ELSE
            cType := ValType(oResult)
            nElements := 0
            IF cType == "A"
                nElements := Len(oResult)
            ELSEIF cType == "H"  
                nElements := Len(oResult)
            ENDIF
            
            ? "JSON OK: " + cType + " com " + hb_NToC(nElements) + ;
              " elementos (" + AllTrim(Str(nJsonTime, 10, 3)) + "s)"
        ENDIF
    ELSE
        ? "ERRO: Dados incompletos ou header invalido"
        oResult := { => }
        oResult["ERROR"] := IIF(!lHeaderProcessed, "Header invalido", "Dados incompletos: " + hb_NToC(nRecv)+ "/" + hb_NToC(nLen))
    ENDIF

RETURN oResult

/*{Harbour.doc} GetUniqueID()
Gera ID unico para transacao
@type static function
*/

STATIC FUNCTION GetUniqueID()
    LOCAL cUUID := ""
    LOCAL nI
    
    FOR nI := 1 TO 16
        cUUID += hb_StrToHex(Chr(hb_Random(0, 255)))
    NEXT
    
    // Formato simplificado para teste
    cUUID := SubStr(cUUID, 1, 8) + "-" + ;
             SubStr(cUUID, 9, 4) + "-" + ;
             SubStr(cUUID, 13, 4)
    
RETURN cUUID