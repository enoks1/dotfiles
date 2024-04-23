local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	{
		"sainnhe/everforest",
		init = function ()
			-- vim.cmd.colorscheme 'everforest'
		end
	},
	{
		"rebelot/kanagawa.nvim",
		init = function ()
			vim.cmd.colorscheme "kanagawa-dragon"
		end
	},
	{
		"williamboman/mason.nvim",
		init = function ()
			require("mason").setup()
		end,
	},
	{
		"williamboman/mason-lspconfig.nvim",
		init = function ()
			require("mason-lspconfig").setup {
				ensure_installed = { "clangd" },
			}
		end,
	},
	{
		"L3MON4D3/LuaSnip",
	},
	{
		"dundalek/lazy-lsp.nvim",
		dependencies = {
			"neovim/nvim-lspconfig",
			{"VonHeikemen/lsp-zero.nvim", branch = "v3.x"},
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/nvim-cmp",
		},
		init = function()
			local lsp_zero = require("lsp-zero")

			lsp_zero.on_attach(function(client, bufnr)
				-- see :help lsp-zero-keybindings to learn the available actions
				lsp_zero.default_keymaps({
					buffer = bufnr,
					preserve_mappings = false
				})
			end)

			require("lazy-lsp").setup {
				preferred_servers = { 
					cpp = { "clangd" },
				},
			}
		end,
	},
})
vim.g.have_nerd_font = false

vim.opt.mouse = 'a'
vim.opt.clipboard = 'unnamedplus'

-- Use tab-indent
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.smarttab = true

-- Use whitespace-mode style
vim.opt.list = true
vim.opt.listchars = { tab = "|  ", space = "·", eol = "↲"} -- [→]

-- Enable (relative) line-number-mode
vim.opt.number = true
vim.opt.relativenumber = true

-- Enable automatically changing working-dir when opening a file
vim.opt.autochdir = true
