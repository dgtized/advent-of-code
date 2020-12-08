defmodule Halting.MixProject do
  use Mix.Project

  def project do
    [
      app: :halting,
      version: "0.1.0",
      deps: [],
      escript: [main_module: Halting.CLI]
    ]
  end
end
