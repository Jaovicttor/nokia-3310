# Nokia-3310
## Projeto apresentado a disciplina de  Paradigmas de Programação da Universidade Federal de Campina Grande.

### O objetivo do sistema é simular as funcionalidades presentes no nokia-3310, também conhecido como "Nokia Tijolão" ou celular indestrutivel.

### O sistema é composto por 6 funcionalidades básicas:
  1. Uma agenda telefônica
  2. Um calendários de eventos
  3. Um sistema de alarmes
  4. Um módulo de envio de mensagens
  5. Simuladores de ligação
  
### Stack
  - Haskell
  - Postgres

### Iniciar o sistema
  #### Certifique-se de ter o Haskell e o Postgress instalados na sua máquina
  
  #### Adicione a váriavel pg_config as suas variáveis de ambiente
  
  ##### Windowns :
  ###### \Program Files\PostgreSQL\versao\bin
  
  ##### Mac/OS : 
  ###### export LDFLAGS="-L/opt/homebrew/opt/postgresql@15/lib"
  ###### export CPFLAGS="-I/opt/homebrew/opt/postgresql@15/include"
  ###### 'export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"' >> ~/.zshrc
  
  #### cabal build
  #### cabal run

### Backlog do sistema
https://drive.google.com/file/d/1VXe5u9MV36_Sgve5Gl7MBd2S3SiFfNVL/view?usp=sharing

### Observações importante
  - Não clonar um projeto para um diretório que contenha caracteres especiais ou espaços em seu PATH
