# naldodj-totvs-rpc-performance-benchmark-harbour

<p align="center">
  <a href="https://youtu.be/Ryd_huMETMI">
    <img src="https://img.youtube.com/vi/Ryd_huMETMI/hqdefault.jpg" alt="Assista ao vídeo no YouTube">
  </a>
</p>

---

### 📌 Considerações Técnicas

Os exemplos apresentados neste repositório têm caráter exclusivamente demonstrativo e destinam-se a ilustrar conceitos fundamentais de comunicação RPC, manipulação de sockets, serialização e integração entre runtimes distintos. Embora funcionais, esses exemplos **não representam uma implementação adequada para ambientes de produção**.

Para uso real, recomenda-se desenvolver uma solução mais completa e resiliente, contemplando:

* **Tratamento robusto de erros e exceções**
* **Gerenciamento de conexões**, incluindo timeouts, reconexão e detecção de inatividade
* **Controle de concorrência e isolamento**, por meio de filas, pools de threads e limites de carga
* **Protocolos de serialização confiáveis**, evitando truncamento e garantindo integridade dos dados
* **Mecanismos de segurança**, como autenticação, autorização e transporte seguro
* **Monitoramento e observabilidade**, com métricas, logs estruturados e rastreamento de chamadas
* **Testes automatizados** cobrindo carga, resiliência e comportamento em cenários adversos

Embora os exemplos utilizem Harbour pela disponibilidade de recursos como threading, sockets e serialização nativa, **a tecnologia empregada no backend RPC não é restrita ao Harbour**.
A mesma arquitetura pode ser implementada em outras linguagens de baixo ou médio nível, dependendo das necessidades de desempenho, mantenibilidade e integração, incluindo:

* **C / C++**
* **Rust**
* **Zig**
* **Go**
* **Cython (Python otimizado)**

A função do motor RPC permanece a mesma independentemente da linguagem: **centralizar operações computacionais intensivas fora do ambiente interpretado**, oferecendo um ponto único de processamento otimizado e interoperável.

---
