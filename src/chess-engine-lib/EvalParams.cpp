#include "EvalParams.h"

#include "Piece.h"
#include "RangePatches.h"

#include <format>
#include <ranges>
#include <sstream>

#include <cstring>

namespace {

constexpr EvalParamArray kDefaultParams = {
        0.209377f,    1.000000f,    0.762991f,    1.679964f,    6.320265f,    0.000000f,
        0.209377f,    1.076343f,    0.292949f,    0.205006f,    0.440503f,    1.202317f,
        1.000000f,    -0.021696f,   0.000000f,    -0.037397f,   -0.002051f,   63.799564f,
        3.809391f,    158.074982f,  379.193848f,  386.743073f,  350.146515f,  418.825012f,
        576.717468f,  713.856812f,  1358.921997f, 1217.330078f, 0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    -32.366119f,  156.195099f,
        -32.549809f,  158.523895f,  -24.502291f,  163.023483f,  -41.332455f,  155.655807f,
        -25.576498f,  187.378204f,  26.444986f,   162.751511f,  25.719206f,   155.237930f,
        -12.835822f,  145.032776f,  -21.827496f,  152.252670f,  -17.161566f,  152.181686f,
        -9.073386f,   154.866348f,  -15.415519f,  165.417282f,  -9.548830f,   163.579880f,
        9.581491f,    159.000214f,  16.561657f,   149.315552f,  3.428589f,    146.394501f,
        -3.379439f,   164.384918f,  4.914340f,    163.405472f,  13.951690f,   160.872009f,
        20.634466f,   158.229034f,  19.748535f,   163.367569f,  33.019989f,   157.056747f,
        20.071520f,   161.265808f,  4.218941f,    158.546036f,  23.802505f,   177.731567f,
        25.935123f,   178.081741f,  25.187847f,   170.594696f,  42.389748f,   159.199265f,
        59.288208f,   163.063263f,  50.000252f,   155.049576f,  35.122215f,   169.643036f,
        34.989677f,   161.895477f,  59.452084f,   185.865250f,  37.262367f,   202.562317f,
        66.326889f,   187.892334f,  19.570206f,   188.714554f,  67.976128f,   143.976700f,
        129.503418f,  181.175888f,  122.246071f,  180.162842f,  83.572128f,   183.418976f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    319.476318f,  352.586884f,  329.828735f,  373.961975f,
        323.353760f,  379.462189f,  349.498810f,  371.124786f,  344.988342f,  364.435272f,
        349.933014f,  354.038269f,  322.492249f,  368.038391f,  301.475861f,  294.353241f,
        342.596863f,  344.368805f,  348.684784f,  370.208588f,  355.738953f,  386.726074f,
        368.018097f,  386.353485f,  359.759399f,  378.510559f,  370.060425f,  367.374451f,
        346.519165f,  361.165253f,  359.449585f,  327.516846f,  352.205200f,  373.083405f,
        367.889709f,  375.613617f,  373.454803f,  386.086945f,  392.150238f,  398.941162f,
        402.116760f,  393.329010f,  378.920746f,  371.015991f,  379.201691f,  355.844635f,
        364.654633f,  372.261597f,  373.686951f,  373.069550f,  425.457886f,  386.325226f,
        416.734222f,  402.950500f,  401.388672f,  406.652588f,  411.759705f,  418.364746f,
        407.234344f,  403.956848f,  407.071899f,  379.499451f,  367.288574f,  374.250031f,
        396.805695f,  390.788452f,  406.125763f,  393.607971f,  437.181427f,  405.912384f,
        463.450592f,  401.450165f,  426.596252f,  402.576752f,  451.637726f,  399.592896f,
        415.621796f,  400.046783f,  421.139099f,  375.989899f,  374.215759f,  394.331207f,
        436.878357f,  385.118134f,  483.377411f,  380.724182f,  470.343933f,  394.447113f,
        531.574585f,  375.440247f,  470.471832f,  396.181885f,  450.936462f,  371.639496f,
        400.709625f,  366.694641f,  432.105682f,  369.173126f,  380.937683f,  392.409515f,
        425.472443f,  382.543243f,  460.023560f,  403.899597f,  459.827057f,  377.575226f,
        424.245911f,  382.461975f,  409.368195f,  392.411835f,  366.883667f,  372.823456f,
        202.590759f,  399.840179f,  296.428284f,  429.652740f,  357.802582f,  398.159790f,
        408.470673f,  414.496368f,  330.876556f,  416.843140f,  369.830536f,  398.692688f,
        525.380676f,  317.727203f,  312.502747f,  293.406281f,  327.954620f,  382.255615f,
        340.982513f,  407.536957f,  322.139069f,  410.682831f,  320.328705f,  414.135010f,
        334.452148f,  403.057739f,  317.439850f,  417.162567f,  299.147949f,  406.862823f,
        300.717926f,  384.530334f,  356.701111f,  398.292480f,  341.960724f,  395.532501f,
        359.133057f,  408.384949f,  334.915771f,  419.043457f,  350.235229f,  405.899475f,
        349.172638f,  406.965485f,  366.749786f,  392.587738f,  340.728546f,  354.780975f,
        332.958984f,  417.615540f,  368.329559f,  415.101837f,  355.808472f,  431.191956f,
        360.735687f,  420.628448f,  358.018646f,  426.067963f,  359.282776f,  418.801361f,
        361.895416f,  408.709503f,  356.632172f,  405.244202f,  344.917480f,  428.437927f,
        349.802795f,  425.704834f,  359.444489f,  427.529755f,  368.420258f,  437.571930f,
        376.152710f,  424.840454f,  357.480865f,  429.102417f,  358.953156f,  424.109039f,
        353.984222f,  399.671509f,  338.265350f,  421.143066f,  357.898712f,  431.684296f,
        371.080200f,  438.486969f,  404.394989f,  424.176056f,  386.684479f,  426.087067f,
        379.234955f,  436.654663f,  361.103394f,  425.706970f,  356.079315f,  421.951019f,
        359.865875f,  421.076233f,  365.363220f,  436.120300f,  368.495453f,  432.258514f,
        403.042297f,  420.073517f,  399.312897f,  429.414246f,  433.589874f,  427.017303f,
        461.049011f,  418.700531f,  387.299805f,  414.394836f,  370.798340f,  416.655823f,
        355.078094f,  442.689392f,  389.194275f,  424.770294f,  358.621216f,  448.046906f,
        387.019714f,  415.174561f,  448.959686f,  411.635834f,  324.485870f,  435.799561f,
        325.210815f,  397.610535f,  351.528351f,  419.503876f,  362.260590f,  446.242126f,
        328.733887f,  434.668915f,  325.306030f,  442.244812f,  338.555298f,  438.774048f,
        376.429443f,  410.563782f,  324.059448f,  403.677551f,  439.731781f,  398.354309f,
        587.617188f,  687.746643f,  583.890625f,  688.674011f,  582.802795f,  691.845032f,
        577.780640f,  690.199951f,  578.379028f,  687.618774f,  567.070801f,  693.025391f,
        555.152588f,  691.960022f,  550.785706f,  679.797913f,  561.429016f,  704.345032f,
        579.346313f,  693.743958f,  575.007263f,  706.592163f,  564.593994f,  703.593201f,
        565.595459f,  710.703003f,  575.124695f,  688.187683f,  555.722778f,  701.585449f,
        518.400085f,  712.986511f,  561.221741f,  709.912781f,  562.268494f,  712.683716f,
        555.648193f,  714.824280f,  552.847595f,  720.130798f,  564.014038f,  710.864868f,
        546.082947f,  715.468750f,  558.982361f,  705.842163f,  535.450439f,  713.083130f,
        563.672119f,  728.459229f,  562.292603f,  731.880127f,  561.507507f,  732.098267f,
        547.790527f,  735.686951f,  557.445496f,  726.804626f,  559.460938f,  733.426880f,
        537.852173f,  742.589844f,  527.760315f,  731.506348f,  562.882507f,  745.912964f,
        587.815918f,  733.382812f,  603.065308f,  726.004211f,  596.283386f,  728.367981f,
        596.093262f,  721.856384f,  578.363831f,  730.356018f,  584.551514f,  738.390259f,
        559.175781f,  736.885071f,  573.889099f,  751.301025f,  608.065125f,  737.792664f,
        615.797791f,  732.964966f,  614.680969f,  725.622070f,  635.206482f,  727.458984f,
        653.589661f,  725.097168f,  648.751343f,  728.038635f,  612.093689f,  749.131958f,
        598.811462f,  753.704773f,  586.485046f,  759.841980f,  635.585876f,  742.949951f,
        632.438232f,  740.191284f,  610.413391f,  753.148010f,  662.206543f,  721.325745f,
        678.838074f,  725.194031f,  615.234924f,  743.595947f,  607.573608f,  739.040527f,
        635.557434f,  737.034302f,  604.468750f,  746.533813f,  588.804871f,  747.499390f,
        592.441833f,  752.951904f,  577.701782f,  756.264954f,  638.076782f,  746.515320f,
        641.974426f,  737.409790f,  1385.472778f, 1113.219360f, 1360.424438f, 1168.559082f,
        1357.149048f, 1169.677856f, 1349.103638f, 1170.441406f, 1343.786133f, 1162.245239f,
        1314.183228f, 1168.345215f, 1322.711548f, 1129.164917f, 1349.154663f, 1120.924683f,
        1365.358521f, 1177.871094f, 1377.855225f, 1143.296265f, 1374.854980f, 1156.880615f,
        1366.203979f, 1160.566528f, 1360.478638f, 1184.809814f, 1365.083374f, 1124.940186f,
        1382.710449f, 1063.218384f, 1326.096313f, 1150.587646f, 1365.992554f, 1162.029053f,
        1366.075439f, 1211.583496f, 1347.906006f, 1228.093140f, 1357.259155f, 1208.164185f,
        1353.455933f, 1219.744019f, 1353.119751f, 1234.307495f, 1365.630859f, 1184.716187f,
        1340.039673f, 1244.558960f, 1360.741211f, 1237.061768f, 1358.704468f, 1225.741943f,
        1354.752441f, 1238.604004f, 1350.571899f, 1266.749878f, 1347.270508f, 1286.069702f,
        1345.888306f, 1268.135742f, 1355.647949f, 1243.816650f, 1344.331177f, 1281.331787f,
        1350.071167f, 1267.485474f, 1343.629883f, 1279.822388f, 1352.124023f, 1276.873901f,
        1345.656616f, 1307.037109f, 1352.748047f, 1301.134033f, 1356.541260f, 1301.655151f,
        1365.789185f, 1272.447510f, 1378.844727f, 1263.448242f, 1373.882568f, 1248.748779f,
        1371.596558f, 1226.280762f, 1354.162231f, 1300.283569f, 1322.741455f, 1348.648560f,
        1363.020386f, 1324.925293f, 1424.668213f, 1272.144043f, 1447.848755f, 1184.935791f,
        1389.004272f, 1246.428955f, 1351.320435f, 1257.651489f, 1343.625122f, 1276.258789f,
        1365.578369f, 1262.548706f, 1358.540161f, 1307.936401f, 1348.261841f, 1337.341187f,
        1413.602661f, 1260.155884f, 1411.099121f, 1202.434814f, 1460.557617f, 1190.402588f,
        1393.398926f, 1224.454834f, 1394.255981f, 1213.249756f, 1391.164795f, 1247.543457f,
        1338.982056f, 1306.167480f, 1336.929810f, 1322.992310f, 1440.500610f, 1220.549805f,
        1421.870483f, 1228.958496f, 1357.651489f, 1279.272461f, 63.373493f,   -19.745916f,
        26.287491f,   -9.347361f,   -41.539635f,  5.940793f,    -55.978432f,  -5.077999f,
        -38.184418f,  -19.954098f,  -51.490433f,  9.909994f,    27.805532f,   4.744803f,
        60.140797f,   -14.136891f,  15.885465f,   -12.673320f,  -20.396608f,  -3.039553f,
        -67.437378f,  -1.172392f,   -138.479858f, 14.517755f,   -123.492432f, 5.588503f,
        -80.686577f,  4.604966f,    -6.900325f,   -7.333151f,   25.804022f,   -18.747707f,
        -25.483191f,  -21.223906f,  -66.170288f,  2.332238f,    -121.824844f, 3.272070f,
        -118.254211f, 1.935781f,    -142.663284f, 8.394234f,    -126.565948f, 4.439267f,
        -84.664108f,  -8.442426f,   -49.289257f,  -28.125433f,  -0.177257f,   -13.551477f,
        -5.559364f,   -19.273254f,  -53.321186f,  -11.122108f,  -76.716133f,  -7.822606f,
        -84.683029f,  -8.207245f,   -89.643272f,  -3.046535f,   -52.259350f,  -23.376120f,
        -119.118950f, -19.105904f,  24.894646f,   -4.720723f,   60.910980f,   -17.246078f,
        -21.452030f,  -3.167818f,   41.863590f,   -23.240536f,  -26.195675f,  -6.211488f,
        11.612698f,   -13.723832f,  17.189123f,   -21.479397f,  -101.538300f, -10.656960f,
        56.886196f,   0.381484f,    106.468895f,  -10.650331f,  68.469101f,   -4.217958f,
        58.946495f,   -5.317639f,   97.918015f,   -31.825190f,  173.702240f,  -20.128801f,
        87.130173f,   6.450596f,    140.919830f,  -15.980547f,  143.337784f,  -13.312281f,
        74.843803f,   11.286077f,   135.277313f,  -2.002573f,   135.893234f,  -30.751181f,
        5.897928f,    -14.766169f,  204.219833f,  -98.069389f,  127.738220f,  25.203848f,
        35.708694f,   39.829838f,   207.549622f,  -89.990768f,  203.026306f,  -50.819469f,
        86.717552f,   2.105858f,    0.573388f,    15.006980f,   91.051964f,   -54.553448f,
        15.636885f,   -10.155436f,  124.519432f,  -13.558252f,  606.548523f,  -255.921005f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    -1.063318f,   119.374023f,
        -16.815548f,  130.569000f,  19.812725f,   108.381973f,  3.245906f,    147.453125f,
        38.221233f,   121.311272f,  87.980217f,   114.389000f,  72.437904f,   105.531815f,
        52.953289f,   101.943047f,  9.493778f,    137.287125f,  2.840682f,    145.158020f,
        11.373116f,   134.462173f,  24.980288f,   143.455200f,  10.177155f,   157.475723f,
        50.478790f,   128.486420f,  60.217499f,   130.644775f,  19.289654f,   115.164848f,
        29.121346f,   197.440414f,  27.539650f,   170.803665f,  57.732925f,   168.948349f,
        65.005020f,   140.106705f,  93.318970f,   143.419373f,  74.079430f,   164.079163f,
        43.144558f,   152.422836f,  35.858059f,   144.308151f,  74.686920f,   255.847458f,
        52.593731f,   262.442963f,  87.178917f,   233.041046f,  102.213371f,  213.724380f,
        126.236710f,  193.534134f,  174.060898f,  145.455276f,  92.723366f,   185.992172f,
        145.368149f,  181.086456f,  79.171867f,   429.159515f,  86.900513f,   394.602478f,
        102.517479f,  288.097260f,  164.085327f,  245.772034f,  234.177689f,  193.524780f,
        211.866974f,  198.270508f,  137.207886f,  205.172440f,  210.962997f,  246.815384f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        2.802965f,    92.460190f,   12.107411f,   77.658943f,   -4.163481f,   81.409615f,
        32.368004f,   80.084610f,   25.992029f,   88.169601f,   68.936935f,   42.205841f,
        37.603008f,   54.175621f,   8.730464f,    52.531509f,   27.152430f,   88.939598f,
        64.069122f,   68.498367f,   9.665403f,    86.385429f,   14.197635f,   68.645699f,
        10.677752f,   75.234299f,   28.874117f,   73.113525f,   52.528275f,   77.655357f,
        45.140533f,   58.229248f,   99.458076f,   124.254654f,  122.145332f,  105.851913f,
        65.700592f,   103.770172f,  78.694023f,   91.343628f,   84.767967f,   89.294136f,
        103.905106f,  102.293129f,  55.012547f,   142.677887f,  65.259262f,   119.895576f,
        184.655380f,  188.970108f,  181.560852f,  187.104630f,  160.741180f,  164.438919f,
        160.694656f,  127.427612f,  176.500824f,  113.147713f,  165.418701f,  115.171165f,
        124.463326f,  168.043945f,  150.678070f,  153.189926f,  315.662231f,  253.781921f,
        301.722748f,  262.960724f,  273.933533f,  220.437027f,  270.194550f,  197.515884f,
        315.118073f,  153.639420f,  337.064117f,  138.248489f,  255.573517f,  193.676025f,
        216.833893f,  208.291809f,  531.949219f,  288.634674f,  569.138306f,  258.876312f,
        449.279480f,  279.986084f,  470.839508f,  219.526321f,  490.417572f,  184.142532f,
        451.419250f,  180.960175f,  156.086227f,  289.884094f,  205.463272f,  271.678467f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    15.936689f,   9.134013f,
        26.307905f,   44.011795f,   14.366298f,   23.326273f,   9.638988f,    16.834826f,
        -0.291491f,   -0.175336f,   -0.171992f,   -0.113921f,   -0.037822f,   0.079835f,
        25.684450f,   13.850655f,   38.392597f,   -3.358742f,   26.390135f,   -2.023636f,
        13.222403f,   -0.580612f,   0.000000f,    0.000000f,    -14.498485f,  1.457296f,
        -26.042086f,  -8.547302f,   -47.994884f,  29.047552f,   -59.553963f,  -218.869232f,
        31.384974f,   4.268120f,    35.812107f,   -13.469579f,  18.614061f,   -3.380007f,
        8.602697f,    6.323513f,    -0.000000f,   0.000000f,    -12.662477f,  7.120727f,
        -18.842522f,  10.159533f,   -39.994507f,  91.943733f,   -52.002495f,  -80.377594f,
        33.461281f,   78.484863f,   0.382468f,    -13.671442f,  -67.077995f,  -8.978979f,
        17.891241f,   10.934106f,   36.681686f,   -0.773518f,   -243.902954f, 81.561577f,
        -104.964302f, 30.202925f,   26.528992f,   -27.392998f,  6.655693f,    -9.290505f,
        0.000000f,    0.000000f,    1.321492f,    9.833986f,    1.466782f,    31.787151f,
        -7.221192f,   66.859970f,   -26.423630f,  125.665413f,  -463.750824f, 196.789505f,
        -140.269897f, 51.666443f,   23.666874f,   -27.430380f,  25.505350f,   -28.257706f,
        0.000000f,    0.000000f,    -9.553682f,   27.715714f,   -16.871315f,  55.107403f,
        -59.771816f,  113.961182f,  -181.999054f, 260.085052f,  141.988190f,  -73.176949f,
        60.326935f,   -77.222260f,  -81.341255f,  17.909163f,   -52.222683f,  -1.972996f,
        0.000000f,    0.000000f,    50.047356f,   -17.998957f,  102.357338f,  -40.669697f,
        246.893600f,  -189.743515f, 560.299622f,  -569.503662f, 2.295129f,    -0.186613f,
        0.000000f,    0.000000f,    -0.339190f,   2.328601f,    5.275782f,    3.475716f,
        3.372565f,    3.347925f,    2.317152f,    1.353858f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        -11.429132f,  -31.134434f,  -10.618642f,  -6.930160f,   -8.565966f,   1.506677f,
        -7.491817f,   3.209131f,    -4.879628f,   4.617555f,    0.000000f,    0.000000f,
        -0.545913f,   0.675977f,    -4.299822f,   -2.277601f,   72.305733f,   -21.068369f,
        13.329107f,   -38.994125f,  12.535216f,   -14.503390f,  10.459513f,   -8.737165f,
        7.813059f,    -3.467817f,   5.676799f,    -1.286207f,   0.000000f,    0.000000f,
        -9.045220f,   0.067274f,    -1.250105f,   -6.758497f,   28.694471f,   -18.042480f,
        15.652097f,   -38.197765f,  10.198857f,   -16.762962f,  8.170738f,    -11.661916f,
        5.955162f,    -6.995190f,   1.121286f,    -2.819638f,   0.000000f,    0.000000f,
        -1.802369f,   -0.239182f,   -0.365768f,   -1.613812f,   -0.490303f,   -5.677386f,
        0.910371f,    -115.691017f, 3.961308f,    -51.084263f,  4.644570f,    -34.579521f,
        4.772416f,    -27.651075f,  1.716380f,    -6.245218f,   0.000000f,    0.000000f,
        -1.672968f,   7.049455f,    -4.311030f,   18.928108f,   -2.262174f,   22.221542f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        6.951511f,    -0.612281f,   2.282013f,    1.298728f,    2.404505f,    -0.309168f,
        2.002366f,    0.876391f,    3.048295f,    -2.641889f,   6.398345f,    -0.867755f,
        4.494388f,    2.350417f,    7.097399f,    13.330534f,   -1.545748f,   7.461751f,
        -3.012421f,   -4.180311f,   0.788837f,    1.613233f,    -2.584391f,   -0.447860f,
        4.979761f,    -5.226386f,   4.363046f,    -5.608053f,   -2.211088f,   -2.302180f,
        -0.541197f,   -6.485151f,   -8.243846f,   -7.647994f,   1.634417f,    -8.997470f,
        20.000000f,   10.000000f,   -1.287361f,   -8.109919f,   -8.607478f,   -45.331173f,
        8.442314f,    -20.636879f,  8.224795f,    3.699188f,    -42.138081f,  -34.121651f,
        -3.572532f,   -11.015906f,  4.701866f,    10.484116f,   -43.644199f,  -34.355782f,
        -13.741261f,  -4.589169f,   4.017207f,    9.529076f,    -54.212276f,  -5.428097f,
        -6.022987f,   3.917392f,    3.205048f,    32.469173f,   -67.900490f,  -31.841967f,
        -49.246792f,  16.113632f,   0.000000f,    0.000000f,    31.843174f,   -46.761402f,
        31.940594f,   -45.038555f,  40.493767f,   -45.288059f,  70.172951f,   -58.468460f,
        89.653358f,   -66.925880f,  147.663956f,  -87.355934f,  264.099274f,  -114.874908f,
        148.474121f,  -59.143299f,  0.000000f,    0.000000f,    -54.637573f,  43.828636f,
        -35.239368f,  37.549507f,   -30.169565f,  38.307659f,   -36.975143f,  38.454865f,
        -38.724247f,  52.247452f,   0.000000f,    0.000000f,    0.000000f,    0.000000f,
        5.404550f,    1.097108f,    63.133335f,   -40.437096f,  131.579803f,  -80.602348f,
        220.761642f,  -124.170509f, 297.884186f,  -167.793365f, -11.545457f,  5.678489f,
        -18.126219f,  -50.558643f,  -23.134027f,  -46.712532f,  -52.767097f,  -14.496755f,
        -33.259243f,  -7.987786f,   -16.223324f,  14.072134f,   -9.904302f,   6.656082f,
        -18.885342f,  33.043377f};

std::string evalCalcTToString(const EvalCalcT term) {
    return std::format("{:>6.1f}", term);
}

std::string taperedTermToString(const TaperedTerm& term) {
    return std::format("{{{:>6.1f}, {:>6.1f}}}", term.early, term.late);
}

std::string getPstPieceTypeName(int idx) {
    if (idx == EvalParams::kCandidatePassedPawnPstIdx) {
        return "Candidate";
    } else if (idx == EvalParams::kPassedPawnPstIdx) {
        return "Passed pawn";
    } else {
        return pieceToString((Piece)idx);
    }
}

std::string getTropismPieceTypeName(int idx) {
    if (idx == EvalParams::kDoubledPawnTropismIdx) {
        return "Doubled pawn";
    } else if (idx == EvalParams::kIsolatedPawnTropismIdx) {
        return "Isolated pawn";
    } else if (idx == EvalParams::kPassedPawnTropismIdx) {
        return "Passed pawn";
    } else if (idx == EvalParams::kCandidatePassedPawnTropismIdx) {
        return "Candidate";
    } else {
        return pieceToString((Piece)idx);
    }
}

void writeSquareTable(const EvalParams::SquareTable& squareTable, std::ostream& out) {
    for (int rank = kRanks - 1; rank >= 0; --rank) {
        out << "\t\t";
        for (int file = 0; file < kFiles; ++file) {
            const auto& value = squareTable[rank * kFiles + file];
            out << std::format("{}, ", taperedTermToString(value));
        }
        out << "\n";
    }
}

void writePieceSquareTables(
        const EvalParams::PieceSquareTables& pieceSquareTables, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < (int)pieceSquareTables.size(); ++pieceIdx) {
        out << "\t" << getPstPieceTypeName(pieceIdx) << ": {\n";
        writeSquareTable(pieceSquareTables[pieceIdx], out);
        out << "\t}\n";
    }
}

void writeTropismTable(
        const std::array<TaperedTerm, EvalParams::kNumTropismPieceTypes>& tropism,
        std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < (int)EvalParams::kNumTropismPieceTypes; ++pieceIdx) {
        out << std::format(
                "\t{}: {}\n",
                getTropismPieceTypeName(pieceIdx),
                taperedTermToString(tropism[pieceIdx]));
    }
}

template <std::size_t N>
std::string arrayToString(const std::array<TaperedTerm, N>& valueArray) {
    std::string inner =
            valueArray | std::ranges::views::transform(taperedTermToString) | joinToString(", ");
    return "{" + inner + "}";
}

template <std::size_t N>
std::string arrayToString(const std::array<EvalCalcT, N>& valueArray) {
    std::string inner =
            valueArray | std::ranges::views::transform(evalCalcTToString) | joinToString(", ");
    return "{" + inner + "}";
}

void writeTaperedTermPerPiece(
        const std::array<TaperedTerm, kNumPieceTypes>& terms, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        out << std::format(
                "\t{}: {}\n", pieceToString((Piece)pieceIdx), taperedTermToString(terms[pieceIdx]));
    }
}

}  // namespace

EvalParams EvalParams::getEmptyParams() {
    return EvalParams{};
}

EvalParams EvalParams::getDefaultParams() {
    return evalParamsFromArray(kDefaultParams);
}

EvalParamArray evalParamsToArray(const EvalParams& params) {
    EvalParamArray array;
    std::memcpy(array.data(), &params, sizeof(EvalParams));
    return array;
}

EvalParams evalParamsFromArray(const EvalParamArray& array) {
    EvalParams params = EvalParams::getEmptyParams();
    std::memcpy(&params, array.data(), sizeof(EvalParams));
    return params;
}

std::string evalParamsToString(const EvalParams& params) {
    std::ostringstream oss;

    oss << "phaseMaterialValues: {\n";
    for (int pieceIdx = 0; pieceIdx < (int)EvalParams::kNumPstPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {:>6.1f}\n",
                getPstPieceTypeName(pieceIdx),
                params.phaseMaterialValues[pieceIdx]);
    }
    oss << "}\n";

    oss << std::format(
            "\noppositeColoredBishopFactor:\n\t{}",
            arrayToString(params.oppositeColoredBishopFactor));

    oss << std::format("\nsingleMinorFactor:\n\t{}", evalCalcTToString(params.singleMinorFactor));
    oss << std::format("\ntwoKnightsFactor:\n\t{}", evalCalcTToString(params.twoKnightsFactor));
    oss << std::format("\nrookVsMinorFactor:\n\t{}", evalCalcTToString(params.rookVsMinorFactor));
    oss << std::format(
            "\nrookAndMinorVsRookFactor:\n\t{}",
            evalCalcTToString(params.rookAndMinorVsRookFactor));

    oss << std::format("\nhasUnstoppablePawn:\n\t{}", evalCalcTToString(params.hasUnstoppablePawn));

    oss << "\npieceValues: {\n";
    writeTaperedTermPerPiece(params.pieceValues, oss);
    oss << "}\n";

    oss << "\npieceSquareTables: {\n";
    writePieceSquareTables(params.pieceSquareTables, oss);
    oss << "}\n";

    oss << std::format(
            "\nprotectedPawnBonus:\n\t{}", taperedTermToString(params.protectedPawnBonus));

    oss << std::format(
            "\nconnectedPassedPawnBonus:\n\t{}",
            taperedTermToString(params.connectedPassedPawnBonus));

    oss << std::format(
            "\ndoubledPawnPenalty:\n\t{}", taperedTermToString(params.doubledPawnPenalty));

    oss << std::format(
            "\nisolatedPawnPenalty:\n\t{}", taperedTermToString(params.isolatedPawnPenalty));

    oss << std::format(
            "\npasserMechanicalObstructionFactor:\n\t{}",
            taperedTermToString(params.passerMechanicalObstructionFactor));

    oss << std::format(
            "\npasserDynamicObstructionFactor:\n\t{}",
            taperedTermToString(params.passerDynamicObstructionFactor));

    oss << std::format(
            "\npasserSacrificialOstructionFactor:\n\t{}",
            taperedTermToString(params.passerSacrificialOstructionFactor));

    oss << std::format(
            "\nbishopPawnSameColorAdjustment:\n\t{}",
            arrayToString(params.bishopPawnSameColorAdjustment));

    oss << std::format(
            "\nbishopEnemyPawnSameColorAdjustment:\n\t{}",
            arrayToString(params.bishopEnemyPawnSameColorAdjustment));

    oss << std::format("\nbishopPairBonus:\n\t{}", taperedTermToString(params.bishopPairBonus));

    oss << std::format("\nknightPairBonus:\n\t{}", taperedTermToString(params.knightPairBonus));

    oss << std::format("\nrookPairBonus:\n\t{}", taperedTermToString(params.rookPairBonus));

    oss << std::format(
            "\nrookSemiOpenFileBonus:\n\t{}", taperedTermToString(params.rookSemiOpenFileBonus));

    oss << std::format("\nrookOpenFileBonus:\n\t{}", taperedTermToString(params.rookOpenFileBonus));

    oss << std::format("\nknightPawnAdjustment:\n\t{}", arrayToString(params.knightPawnAdjustment));

    oss << std::format("\nrookPawnAdjustment:\n\t{}", arrayToString(params.rookPawnAdjustment));

    oss << std::format("\nqueenPawnAdjustment:\n\t{}", arrayToString(params.queenPawnAdjustment));

    oss << std::format(
            "\nkingVirtualMobilityPenalty:\n\t{}",
            taperedTermToString(params.kingVirtualMobilityPenalty));

    oss << "\n\nmobilityBonus: {\n";
    writeTaperedTermPerPiece(params.mobilityBonus, oss);
    oss << "}\n";

    oss << "\n\nsafeMobilityAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                arrayToString(params.safeMobilityAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << "\nownKingTropism: {\n";
    writeTropismTable(params.ownKingTropism, oss);
    oss << "}\n";

    oss << "\nenemyKingTropism: {\n";
    writeTropismTable(params.enemyKingTropism, oss);
    oss << "}\n";

    oss << std::format("\ntempoBonus:\n\t{}", taperedTermToString(params.tempoBonus));

    oss << "\n\nattackDefendAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < (int)params.attackDefendAdjustment.size(); ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                arrayToString(params.attackDefendAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format("\ncontrolNearEnemyKing:\n\t{}", arrayToString(params.controlNearEnemyKing));

    oss << "\n\nkingAttackWeight: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.kingAttackWeight[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nnumKingAttackersAdjustment:\n\t{}",
            arrayToString(params.numKingAttackersAdjustment));

    oss << "\n\npiecePinnedAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.piecePinnedAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nkingOpenFileAdjustment:\n\t{}", taperedTermToString(params.kingOpenFileAdjustment));
    oss << std::format(
            "\nkingFlankOpenFileAdjustment:\n\t{}",
            taperedTermToString(params.kingFlankOpenFileAdjustment));
    oss << std::format(
            "\npassedPawnOutsideKingSquare:\n\t{}",
            taperedTermToString(params.passedPawnOutsideKingSquare));

    return oss.str();
}
