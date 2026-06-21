-- from https://quarto.org/docs/extensions/shortcodes.html
function git(command)
  local p = io.popen("git " .. command)
  local output = p:read('*all')
  p:close()
  return output
end

-- run an arbitrary shell command and return its stdout
function shell(command)
  local p = io.popen(command)
  local output = p:read('*all')
  p:close()
  return output
end

return {

  -- report the version of Julia used to render the site, e.g. "1.12.6".
  -- `julia` is always on PATH during a render because it is the notebook engine.
  ["julia-version"] = function(args, kwargs, meta)
    local output = shell("julia --version")
    local version = output:match("%d+%.%d+%.%d+[%w%.%-%+]*")
    return pandoc.Str(version or "unknown")
  end,

  ["git-rev"] = function(args, kwargs, meta)
    -- command line args
    local cmdArgs = ""
    local short = pandoc.utils.stringify(kwargs["short"])
    if short == "true" then
      cmdArgs = cmdArgs .. "--short "
    end
    
    -- run the command
    local cmd = "rev-parse " .. cmdArgs .. "HEAD"
    local rev = git(cmd)
    
    -- target repo
    local owner = pandoc.utils.stringify(meta["github.owner"])
    local repo = pandoc.utils.stringify(meta["github.repo"])
    local url = "https://github.com/" 
                .. owner .. "/" .. repo .. "/tree/" .. rev 
    
    -- return as link
    return pandoc.Link(pandoc.Str(rev), url)
  end
}
