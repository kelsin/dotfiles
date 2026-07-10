// Use https://finicky-kickstart.now.sh to generate basic configuration
// Learn more about configuration options: https://github.com/johnste/finicky/wiki/Configuration

export default {
    defaultBrowser: "Google Chrome",
    options: {
        checkForUpdates: true,
    },
    rewrite: [],
    handlers: [
        {
            match: (url) => url.protocol === "slack:",
            browser: "Slack",
        },
        {
            match: finicky.matchDomains("open.spotify.com"),
            browser: "Spotify",
        },
        {
            match: [
                "https://discord.com/*",
                (url) => url.protocol === "discord:",
            ],
            browser: "Discord",
        },
        {
            // Open Apple Music links directly in Music.app
            match: [
                "music.apple.com*",
                "geo.music.apple.com*",
                (url) => url.protocol === "itmss:",
            ],
            browser: "Music",
        },
        {
            match: /globalprotect/i,
            browser: {
                name: "Google Chrome",
                profile: "Work",
            },
        },
        {
            match: /airbnb/i,
            browser: {
                name: "Google Chrome",
                profile: "Work",
            },
        },
        {
            match: /pagerduty\.com/i,
            browser: {
                name: "Google Chrome",
                profile: "Work",
            },
        },
        {
            match: /musta\.ch/i,
            browser: {
                name: "Google Chrome",
                profile: "Work",
            },
        },
        {
            match: /quack/i,
            browser: {
                name: "Google Chrome",
                profile: "Work",
            },
        },
        {
            match: /tobiko/i,
            browser: {
                name: "Google Chrome",
                profile: "Work",
            },
        },
        {
            match: /incident\.io/i,
            browser: {
                name: "Google Chrome",
                profile: "Work",
            },
        },
        {
            match: /rippling\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /paypal\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /ebay\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /kp\.org/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /kaiserpermanente\.org/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /itch\.io/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /reddit\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /18xx\.games/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /twitch\.tv/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /bga\.li/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /boardgamegeek\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /boardgamearena\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /rally-the-troops\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /notion\.so/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /steampowered/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /kelsin/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /kels\.in/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /hrf\.im/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /facebook\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /youtube\.com/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
        {
            match: /youtu\.be/,
            browser: {
                name: "Google Chrome",
                profile: "Home",
            },
        },
    ],
};
