#using Pkg; Pkg.add(["CSV","DataFrames","Omniscape", "Rasters"])
using CSV, DataFrames, Omniscape, Rasters
#using Base.Threads  

array = parse(Int64,ENV["SLURM_ARRAY_TASK_ID"])
#print(parse(Int64,ENV["JULIA_NUM_THREADS"]))

path = "./00_rawdata/tables/df_sp_forSHI.csv"

# get species i
sp_list = CSV.read(path, DataFrame)
sp = sp_list[array,:sci_name]

# calc radius size
rad = sp_list[array,:rad]
rad = ifelse(rad>=3,rad,3) # set minimum to 3

#set block_size
block_size = sp_list[array,:block_size]

outputFolder0 = "./02_outdata/species/"#"/run/media/miap/C084AA1284AA0ACC/Hikaru/Doctorado/Documento/Capitulo1/outputs/"#
outputFolder = string(outputFolder0,replace(sp," "=>"_"))

t = 2010
#t_n = 2020 # should be larger than t_0 at least 2 years needed
#time_step = 10

#times = [t_0:time_step:t_n;]

println("Species: ",sp)
println("Species ID: ",array)

println("Search radius: ",rad)
println("Block size: ",block_size)

#Threads.@threads for i in times
#for i in times
    res, wkt, transform = Omniscape.read_raster(string(outputFolder,"/",sp,"_resist_layer_",t,".tif"), Float64)
    #sou, wkt2, transform = Omniscape.read_raster(string(outputFolder,"/",sp,"_source_layer_",t,".tif"), Float64)
    
    config = Dict{String, String}(
    "resistance_file" => string(outputFolder,"/",sp,"_resist_layer_",t,".tif"),
    "source_file" => string(outputFolder,"/",sp,"_source_layer_",t,".tif"),
    "source_from_resistance" => "false",
    "resistance_is_conductance" => "false",
    "radius" => string(rad), # associated to home range
    "block_size" => string(block_size),
    "project_name" => string(outputFolder,"/","Omniscape",t),
    "mask_nodata" => "true",
    "calc_normalized_current" => "false",
    "calc_flow_potential" => "false",
    "parallelize" => "true" 
    )
    currmap, flow_pot, norm_current = run_omniscape(config,
                                                    res,
                                                    wkt = wkt,
                                                    geotransform = transform,
                                                    write_outputs = true
                                                    )
#end

#using Pkg; Pkg.add(["Plots"])
#using Plots
#plot(Raster(string(outputFolder,"/",sp,"_resist_layer_","2000",".tif")))