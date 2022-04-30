-- 1 Consulte os dados da tabela t_vendas, selecione todos os campos.

SELECT *
FROM t_vendas

--  2 Consulte os dados da tabela t_vendas, selecione os campos: codigo_cliente, loja, tamanho_pedido.

SELECT CODIGO_CLIENTE, LOJA, TAMANHO_PEDIDO
FROM t_vendas

-- 3 Consulte os dados da tabela t_vendas, selecione os campos: codigo_cliente, loja, tamanho_pedido, acrescente na condição WHERE loja = ‘Rio de Janeiro’.

SELECT CODIGO_CLIENTE, LOJA, TAMANHO_PEDIDO
FROM t_vendas
WHERE LOJA = 'Rio de janeiro'

-- 4 Consulte os dados da tabela t_vendas, selecione todos os campos , acrescente na condição WHERE loja in (‘Rio de Janeiro’, ‘Salvador’)

SELECT *
FROM t_vendas
WHERE LOJA IN ('Rio de janeiro','Salvador')

-- 5 Consulte os dados da tabela t_vendas, selecione todos os campos, acrescente na condição WHERE tamanho_pedido > 5 e codigo_cliente entre 100 e 200. 

-- Não existe dados com TAMANHO_PEDIDO > 5

SELECT *
FROM t_vendas
WHERE TAMANHO_PEDIDO > 5
	AND CODIGO_CLIENTE BETWEEN 100 AND 200

-- 6 
/*Consulte os dados da tabela t_vendas, crie o campo UF consultando o campo LOJA em que: LOJA = Rio de
Janeiro a UF será RJ; LOJA = São Paulo, a UF será SP; LOJA = Salvador a UF será BA; LOJA = Belo
Horizonte a UF será MG */

SELECT CASE LOJA WHEN 'Rio de Janeiro' THEN 'RJ' 
				 WHEN 'Nova Iguaçu' THEN 'RJ'
				 WHEN 'Niterói' THEN 'RJ'		
				 WHEN 'São Paulo' THEN 'SP'
				 WHEN 'Campinas' THEN 'SP'
				 WHEN 'Guarulhos' THEN 'SP'		
				 WHEN 'Salvador' THEN 'BA'
				 WHEN 'Belo Horizonte' THEN 'MG'
				 WHEN 'Porto Alegre' THEN 'RS'
				 WHEN 'Fortaleza' THEN 'CE'
				 WHEN 'Recife' THEN 'PE'
				 WHEN 'Curitiba' THEN 'PR'		
				 WHEN 'Goiânia' THEN 'GO'		
				 ELSE 'NI' END UF
FROM t_vendas

-- 7 Consulte os dados da tabela t_clientes, selecione todos os campos.

SELECT *
FROM t_clientes

-- 8 Consulte os dados da tabela t_clientes, selecione todos os cliente em que a UF = SP.
SELECT *
FROM t_clientes
WHERE UF = 'SP'

-- 9 Consulte os dados da tabela t_clientes, selecione todos os clientes em que o código do cliente esteja entre 10 e 20.
SELECT *
FROM t_clientes
WHERE CODIGO_CLIENTE BETWEEN 10 AND 20


-- 10  Consulte os dados da tabela t_clientes, selecione todos os clientes em que a UF = RJ e o código do cliente seja maior que 50.

SELECT *
FROM t_clientes
WHERE UF = 'RJ'
	AND CODIGO_CLIENTE > 50
