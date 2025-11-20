vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

require("kelsin.lazy")
require("remember")

vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.smarttab = true
vim.opt.list = true
vim.opt.listchars = "eol:.,tab:>-,trail:~,extends:>,precedes:<"

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.signcolumn = "yes:1"
vim.opt.scrolloff = 8
vim.opt.showcmd = true

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.config/nvim/undodir"
vim.opt.undofile = true
vim.opt.clipboard = "unnamed"

vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.termguicolors = true

vim.opt.showmode = false
vim.opt.autoread = true
vim.opt.equalalways = true

-- No automatic comment insertion
vim.cmd([[autocmd FileType * set formatoptions-=ro]])

-- Autoread
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter" }, {
    command = "if mode() != 'c' | checktime | endif",
    pattern = "*",
})

-- Scrolling
vim.keymap.set({ "n", "v", "i" }, "<ScrollWheelUp>", "<C-Y>")
vim.keymap.set({ "n", "v", "i" }, "<ScrollWheelDown>", "<C-E>")

-- Buffer commands
vim.keymap.set("n", "<leader>bd", "<cmd>bd<cr>", { desc = "Delete Buffer" })
vim.keymap.set("n", "<leader>bw", "<cmd>bw<cr>", { desc = "Wipe Buffer" })

-- Editing common files
vim.keymap.set("n", "<leader>ev", ":e ~/.config/nvim/lua/kelsin/init.lua<CR>", { desc = "Edit neovim init.lua" })
vim.keymap.set("n", "<leader>ep", ":e ~/.config/nvim/lua/plugins<CR>", { desc = "Edit neovim plugins" })
vim.keymap.set("n", "<leader>ez", ":e ~/.zshrc<CR>", { desc = "Edit .zshrc" })
vim.keymap.set("n", "<leader>et", ":e ~/.tmux.conf<CR>", { desc = "Edit .tmux.conf" })

-- Diagnostics
vim.diagnostic.config({
    virtual_text = true,
})

-- SQLMesh LSP
vim.lsp.config["sqlmesh"] = {
    cmd = { "sqlmesh_lsp" },
    filetypes = { "sql" },
    root_markers = { { "config.py", "config.yaml" }, ".git" },
}
vim.lsp.enable("sqlmesh")
