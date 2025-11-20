return {
    "nvim-telescope/telescope.nvim",
    event = "VimEnter",
    dependencies = {
        "nvim-lua/plenary.nvim",
        { -- If encountering errors, see telescope-fzf-native README for installation instructions
            "nvim-telescope/telescope-fzf-native.nvim",

            -- `build` is used to run some command when the plugin is installed/updated.
            -- This is only run then, not every time Neovim starts up.
            build = "make",

            -- `cond` is a condition used to determine whether this plugin should be
            -- installed and loaded.
            cond = function()
                return vim.fn.executable("make") == 1
            end,
        },
        { "nvim-telescope/telescope-ui-select.nvim" },

        -- Useful for getting pretty icons, but requires a Nerd Font.
        { "nvim-tree/nvim-web-devicons", enabled = vim.g.have_nerd_font },

        -- Extensions

        {
            "dhruvmanila/browser-bookmarks.nvim",
            version = "*",
            -- Only required to override the default options
            opts = {
                -- Override default configuration values
                selected_browser = "chrome",
            },
        },
        { "nvim-telescope/telescope-dap.nvim" },
        { "nvim-telescope/telescope-file-browser.nvim" },
        { "nvim-telescope/telescope-project.nvim" },
        { "nvim-telescope/telescope-symbols.nvim" },
        { "polirritmico/telescope-lazy-plugins.nvim" },
        { "smartpde/telescope-recent-files" },
    },
    config = function()
        require("telescope").setup({
            extensions = {
                file_browser = {
                    theme = "ivy",
                    -- disables netrw and use telescope-file-browser in its place
                    hijack_netrw = true,
                },
                lazy_plugins = {
                    lazy_config = vim.fn.stdpath("config") .. "/lua/kelsin/lazy.lua", -- Must be a valid path to the file containing the lazy spec and setup() call.
                },
                project = {
                    base_dirs = {
                        { "~/src", max_depth = 2 },
                    },
                    ignore_missing_dirs = true, -- default: false
                    hidden_files = true, -- default: false
                    theme = "dropdown",
                    order_by = "asc",
                    search_by = "title",
                    sync_with_nvim_tree = true, -- default false
                },
                ["ui-select"] = {
                    require("telescope.themes").get_dropdown(),
                },
            },
        })
        pcall(require("telescope").load_extension, "bookmarks")
        pcall(require("telescope").load_extension, "dap")
        pcall(require("telescope").load_extension, "file_browser")
        pcall(require("telescope").load_extension, "fzf")
        pcall(require("telescope").load_extension, "lazy_plugins")
        pcall(require("telescope").load_extension, "project")
        pcall(require("telescope").load_extension, "recent_files")
        pcall(require("telescope").load_extension, "ui-select")

        local builtin = require("telescope.builtin")
        local ext = require("telescope").extensions
        vim.keymap.set(
            "n",
            "<space>d",
            ":Telescope file_browser path=%:p:h select_buffer=true<CR>",
            { desc = "Open buffer directory" }
        )
        vim.keymap.set("n", "<space>D", ":Telescope file_browser<CR>", { desc = "Open project directory" })
        vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "buffers" })
        vim.keymap.set("n", "<leader>fdb", ext.dap.list_breakpoints, { desc = "breakpoints" })
        vim.keymap.set("n", "<leader>fdc", ext.dap.commands, { desc = "commands" })
        vim.keymap.set("n", "<leader>fdg", ext.dap.configurations, { desc = "configurations" })
        vim.keymap.set("n", "<leader>fdf", ext.dap.frames, { desc = "frames" })
        vim.keymap.set("n", "<leader>fdv", ext.dap.variables, { desc = "variables" })
        vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "files" })
        vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "grep" })
        vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "help tags" })
        vim.keymap.set(
            "n",
            "<leader>fp",
            ":lua require('telescope').extensions.project.project({ display_type = 'full' })<CR>",
            { desc = "projects" }
        )
        vim.keymap.set("n", "<leader>fr", ext.recent_files.pick, { desc = "recent files" })
        vim.keymap.set("n", "<leader>gd", builtin.lsp_definitions, { desc = "definitions" })
        vim.keymap.set("n", "<leader>gi", builtin.lsp_implementations, { desc = "implementations" })
        vim.keymap.set("n", "<leader>gr", builtin.lsp_references, { desc = "references" })
        vim.keymap.set("n", "<leader>gt", builtin.treesitter, { desc = "treesitter" })
        vim.keymap.set("n", "<leader>is", builtin.symbols, { desc = "symbol" })
        vim.keymap.set("n", "<leader>ob", ext.bookmarks.bookmarks, { desc = "bookmark" })
    end,
}
