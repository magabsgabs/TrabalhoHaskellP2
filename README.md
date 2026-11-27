# E-commerce API REST em Haskell (Yesod + SQLite)

## 1. Descrição do Projeto
API REST desenvolvida em **Haskell** utilizando o framework **Yesod** e banco de dados **SQLite** (via Persistent) para o gerenciamento completo de um sistema de e-commerce. O projeto conta com operações CRUD, validações de regras de negócio, integração com a API externa **ViaCEP**, rotas de filtros por relacionamento, suporte a CORS e tratamento padronizado de erros em JSON.

## 2. Integrantes do Grupo
* **Pessoa 1:** Maria Gabriella Giacomini  — Base do projeto, modelagem, CORS, Categoria e Produto
* **Pessoa 2:** Henrique Ferreira Mendes — CRUD de Cliente, integração ViaCEP e CRUD de Endereço
* **Pessoa 3:** Giovanna Campos Gomes Cardim — CRUD de Fornecedor, Pedido e ItemPedido (estoque e total)
* **Pessoa 4:** Guilherme de Oliveira Ferreira — Rotas de filtros, coleção do Postman e documentação (`README.md`)

## 3. Tecnologias Utilizadas
* **Linguagem:** Haskell (GHC via Stack)
* **Framework Web:** Yesod
* **ORM / Banco de Dados:** Persistent + SQLite
* **Integração Externa:** API ViaCEP (`https://viacep.com.br/ws/{cep}/json/`)
* **Testes de API:** Postman (`ecommerce-yesod.postman_collection.json`)

## 4. Modelagem do Banco de Dados
O banco possui 7 entidades principais configuradas em `config/models.persistentmodels`:
* **Categoria:** `nome`, `descricao`
* **Produto:** `nome`, `descricao`, `preco`, `estoque`, `categoriaId`
* **Cliente:** `nome`, `email`, `telefone`, `cpf`
* **Endereco:** `clienteId`, `cep`, `logradouro`, `numero`, `complemento`, `bairro`, `cidade`, `uf`
* **Fornecedor:** `nome`, `cnpj`, `email`, `telefone`
* **Pedido:** `clienteId`, `enderecoId`, `status`, `valorTotal`, `criadoEm`
* **ItemPedido:** `pedidoId`, `produtoId`, `quantidade`, `precoUnitario`

## 5. Como Executar o Projeto

### Pré-requisitos
* [Git](https://git-scm.com/)
* [Haskell Stack](https://docs.haskellstack.org/en/stable/) (no Windows: `winget install CommercialHaskell.Stack`)

### Passo a passo
Clone o repositório em um caminho **sem espaços** (ex: `C:\projetos`):

```bash
git clone [https://github.com/magabsgabs/TrabalhoHaskellP2.git](https://github.com/magabsgabs/TrabalhoHaskellP2.git)
cd TrabalhoHaskellP2