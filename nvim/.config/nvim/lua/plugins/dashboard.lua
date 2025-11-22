return {
    "nvimdev/dashboard-nvim",
    event = "VimEnter",
    config = function()
        require("dashboard").setup({
            config = {
                week_header = { enable = true },
                shortcut = {
                    { desc = "󰚰 Update", group = "@property", action = "Lazy sync", key = "u" },
                    {
                        desc = "󱁿 Project",
                        group = "@property",
                        action = "lua require'telescope'.extensions.project.project{ display_type = 'full' }",
                        key = "p",
                    },
                },
            },
        })
    end,
    dependencies = {
        { "nvim-tree/nvim-web-devicons", enabled = vim.g.have_nerd_font },
    },
}
