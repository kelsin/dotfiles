vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.g.have_nerd_font = true

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
vim.opt.listchars = { eol = "", tab = "» ", trail = "·", nbsp = "␣" }

vim.opt.cursorline = true
vim.opt.inccommand = "split"
vim.opt.wrap = false
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 10
vim.opt.showcmd = true
vim.opt.signcolumn = "yes:1"

vim.opt.breakindent = true
vim.opt.mouse = "a"
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.config/nvim/undodir"
vim.opt.undofile = true

-- Sync clipboard between OS and Neovim.
--  Schedule the setting after `UiEnter` because it can increase startup-time.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.schedule(function()
    vim.opt.clipboard = "unnamedplus"
end)

vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.termguicolors = true

vim.opt.showmode = false
vim.opt.autoread = true
vim.opt.equalalways = true

-- Decrease update time
vim.opt.updatetime = 250

-- Decrease mapped sequence wait time
vim.opt.timeoutlen = 300

-- Configure how new splits should be opened
vim.opt.splitright = true
vim.opt.splitbelow = true

-- No automatic comment insertion
vim.cmd([[autocmd FileType * set formatoptions-=ro]])

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.hl.on_yank()`
vim.api.nvim_create_autocmd("TextYankPost", {
    desc = "Highlight when yanking (copying) text",
    group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
    callback = function()
        vim.hl.on_yank()
    end,
})

-- Autoread
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter" }, {
    command = "if mode() != 'c' | checktime | endif",
    pattern = "*",
})

-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Diagnostic keymaps
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Quickfix list" })

-- Scrolling
vim.keymap.set({ "n", "v", "i" }, "<ScrollWheelUp>", "<C-Y>")
vim.keymap.set({ "n", "v", "i" }, "<ScrollWheelDown>", "<C-E>")

-- Buffer commands
vim.keymap.set("n", "<leader>bd", "<cmd>bd<cr>", { desc = "Delete Buffer" })
vim.keymap.set("n", "<leader>bw", "<cmd>bw<cr>", { desc = "Wipe Buffer" })

-- Editing common files
vim.keymap.set("n", "<leader>eb", ":e ~/.dotfiles/Brewfile<CR>", { desc = "Edit Brewfile" })
vim.keymap.set("n", "<leader>eg", ":e ~/.config/ghostty/config<CR>", { desc = "Edit ghostty config" })
vim.keymap.set("n", "<leader>ep", ":Telescope lazy_plugins<CR>", { desc = "Edit neovim plugins" })
vim.keymap.set("n", "<leader>et", ":e ~/.tmux.conf<CR>", { desc = "Edit .tmux.conf" })
vim.keymap.set("n", "<leader>ev", ":e ~/.config/nvim/lua/kelsin/init.lua<CR>", { desc = "Edit neovim init.lua" })
vim.keymap.set("n", "<leader>ez", ":e ~/.zshrc<CR>", { desc = "Edit .zshrc" })

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
