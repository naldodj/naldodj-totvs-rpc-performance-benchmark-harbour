// calc_preco_otimizado.c - Versão final
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

// Função simples - mesma lógica do ADVPL
HB_FUNC( CALC_PRECO_OTIMIZADO )
{
    PHB_ITEM pArray = hb_param(1, HB_IT_ARRAY);
    
    if (!pArray) {
        hb_ret();
        return;
    }

    HB_SIZE nLen = hb_arrayLen(pArray);
    PHB_ITEM pResult = hb_itemArrayNew(nLen);

    for (HB_SIZE i = 0; i < nLen; i++) {
        PHB_ITEM pItem = hb_arrayGetItemPtr(pArray, i + 1);
        
        if (hb_arrayLen(pItem) >= 3) {
            double qty = hb_arrayGetND(pItem, 2);
            double preco = hb_arrayGetND(pItem, 3);
            double desconto;
            
            // EXATAMENTE a mesma lógica do ADVPL
            if (qty > 100) 
                desconto = 0.15;
            else if (qty > 50) 
                desconto = 0.10;
            else 
                desconto = 0.05;
            
            preco *= (1.0 - desconto);
            
            PHB_ITEM pResItem = hb_itemArrayNew(2);
            hb_arraySetNI(pResItem, 1, (int)(i + 1));
            hb_arraySetND(pResItem, 2, preco);
            hb_arraySet(pResult, i + 1, pResItem);
            hb_itemRelease(pResItem);
        }
    }

    hb_itemReturn(pResult);
}

// Função complexa - mesma lógica do ADVPL
HB_FUNC( PROCESSAR_LOTE_MASSIVO )
{
    PHB_ITEM pArray = hb_param(1, HB_IT_ARRAY);
    
    if (!pArray) {
        hb_ret();
        return;
    }

    HB_SIZE nLen = hb_arrayLen(pArray);
    PHB_ITEM pResult = hb_itemArrayNew(nLen);
    
    for (HB_SIZE i = 0; i < nLen; i++) {
        PHB_ITEM pItem = hb_arrayGetItemPtr(pArray, i + 1);
        
        if (hb_arrayLen(pItem) >= 3) {
            double qty = hb_arrayGetND(pItem, 2);
            double preco = hb_arrayGetND(pItem, 3);
            
            // MESMA lógica complexa do ADVPL
            double margem = 0.30;  // Margem padrão
            double imposto = 0.18; // ICMS + PIS + COFINS
            
            if (qty > 1000) {
                margem = 0.25;
                imposto = 0.16;
            } else if (qty > 500) {
                margem = 0.28;
                imposto = 0.17;
            }
            
            double preco_final = preco * (1.0 + margem) * (1.0 - imposto);
            
            PHB_ITEM pResItem = hb_itemArrayNew(3);
            hb_arraySetNI(pResItem, 1, (int)(i + 1));
            hb_arraySetND(pResItem, 2, preco_final);
            hb_arraySetND(pResItem, 3, margem * 100); // % margem
            
            hb_arraySet(pResult, i + 1, pResItem);
            hb_itemRelease(pResItem);
        }
    }
    
    hb_itemReturn(pResult);
}