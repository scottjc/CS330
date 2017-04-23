module Error

push!(LOAD_PATH, ".")
export LispError

type LispError <: Exception
    msg::AbstractString
end

end #module
