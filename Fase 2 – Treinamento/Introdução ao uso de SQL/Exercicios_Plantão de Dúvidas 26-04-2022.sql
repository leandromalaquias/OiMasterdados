SELECT * FROM cliente.dbo.t_vendas

--- GASTOS DE CLIENTES EM COMPRAS
SELECT v.codigo_cliente, v.marca, v.data_venda, c.telefone_celular, c.UF, c.DATA_NASCIMENTO,
	v.PRECO_UNITARIO*v.TAMANHO_PEDIDO AS gastos
	FROM cliente.dbo.t_vendas AS v 
	JOIN cliente.dbo.t_clientes AS c ON v.codigo_cliente = c.codigo_cliente
	WHERE v.codigo_cliente = 64 AND c.data_nascimento >= '1950-05-03'

--- GASTOS DE CLIENTES POR DATA DE VENDAS
SELECT v.codigo_cliente, v.marca, v.data_venda,
	SUM(v.preco_unitario*v.tamanho_pedido) AS gastos
	FROM cliente.dbo.t_vendas AS v 
	JOIN cliente.dbo.t_clientes AS c ON v.codigo_cliente = c.codigo_cliente
	WHERE v.codigo_cliente = 64 AND c.data_nascimento >= '1950-05-03'
	GROUP BY v.codigo_cliente, v.marca, v.data_venda

--- GASTOS DE CLIENTES EM COMPRAS PARA CADA MARCA, QUANTIDADE, DATA NASCIM. > 1950	
SELECT v.codigo_cliente, v.marca, MIN(v.data_venda) AS data_venda_min,
	MAX(v.data_venda) AS data_venda_max,
	COUNT(v.data_venda) AS qtd,
	SUM(v.preco_unitario*v.tamanho_pedido) AS gastos
	FROM cliente.dbo.t_vendas AS v 
	JOIN cliente.dbo.t_clientes AS c ON v.codigo_cliente = c.codigo_cliente
	WHERE v.codigo_cliente = 64 AND c.data_nascimento >= '1950-05-03'
	GROUP BY v.codigo_cliente, v.marca

	