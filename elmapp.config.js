module.exports = {
  configureWebpack: (config, env) => {
    // Manipulate the config object and return it.
    if (env == "development") {
      config.module.rules[2].use[0].options.plugins = [ "@babel/plugin-transform-class-properties" ];
    } else if (env == "production") {
      config.module.rules[1].use[0].options.plugins = [ "@babel/plugin-transform-class-properties" ];
    }

    return config;
  }
}
