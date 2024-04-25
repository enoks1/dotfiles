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
