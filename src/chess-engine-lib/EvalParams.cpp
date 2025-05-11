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
        1.076343f,    0.295908f,    0.242347f,    0.451500f,    0.957807f,    1.000000f,
        -0.006380f,   -0.010422f,   -0.013014f,   -0.020863f,   135.394241f,  24.273703f,
        179.547974f,  437.268066f,  425.143677f,  383.700562f,  450.661530f,  607.792419f,
        793.690979f,  1429.692749f, 1347.436646f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -18.032896f,  177.397675f,  -18.578630f,
        180.637619f,  -7.869493f,   185.332733f,  -22.237183f,  179.092468f,  -4.818422f,
        206.669464f,  51.699165f,   184.137299f,  47.822842f,   172.518066f,  8.836363f,
        164.958572f,  -7.088316f,   174.776199f,  -1.870689f,   173.752869f,  7.517412f,
        180.340286f,  4.128037f,    187.138733f,  13.770085f,   188.233948f,  35.692329f,
        177.085785f,  38.048054f,   171.197754f,  24.725979f,   166.890427f,  12.189404f,
        186.571228f,  22.539551f,   187.238190f,  35.055218f,   180.339584f,  41.087509f,
        180.358047f,  42.875954f,   182.248581f,  58.366695f,   178.929092f,  44.426857f,
        185.727753f,  24.940617f,   177.580399f,  38.819798f,   200.607346f,  38.618137f,
        206.903412f,  52.396847f,   198.949707f,  66.788864f,   187.614548f,  88.349480f,
        187.473999f,  78.735580f,   178.442932f,  53.137325f,   198.969894f,  56.626148f,
        183.676559f,  75.466286f,   218.123398f,  40.291817f,   229.802658f,  84.821533f,
        213.936066f,  107.986259f,  184.896408f,  147.454803f,  201.672699f,  176.386765f,
        198.180847f,  141.036987f,  189.529694f,  130.408920f,  191.711441f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    375.721008f,  403.076843f,  380.158264f,  390.389252f,  370.261810f,
        413.395416f,  388.039124f,  426.951019f,  385.703430f,  411.952179f,  400.688049f,
        393.641510f,  373.150055f,  396.857208f,  327.471313f,  396.668915f,  387.365631f,
        400.625519f,  375.058929f,  431.356232f,  403.796814f,  426.355103f,  424.689880f,
        418.059631f,  417.186768f,  414.673859f,  423.074615f,  410.298523f,  397.699799f,
        401.618347f,  399.375702f,  405.110962f,  396.975952f,  402.849243f,  424.851562f,
        418.671539f,  433.720703f,  423.789337f,  448.810791f,  442.036407f,  460.270813f,
        435.987701f,  439.276581f,  407.475250f,  435.637177f,  400.296509f,  406.820740f,
        384.425354f,  431.899048f,  411.756165f,  489.224091f,  410.479187f,  478.998138f,
        436.692657f,  465.744415f,  446.952240f,  474.816620f,  454.457092f,  468.462006f,
        438.061829f,  464.911835f,  428.287842f,  421.625092f,  419.788086f,  452.461853f,
        418.147400f,  469.613464f,  429.693481f,  495.833069f,  432.694885f,  534.298218f,
        444.291931f,  492.624847f,  448.497498f,  512.331299f,  445.478119f,  480.272583f,
        443.034210f,  483.454529f,  416.043030f,  433.678467f,  418.165100f,  497.383942f,
        417.917908f,  525.816345f,  425.519409f,  544.903076f,  426.037384f,  591.992371f,
        421.914703f,  569.043640f,  429.074646f,  501.063263f,  415.083832f,  454.876312f,
        414.649017f,  471.398987f,  405.179047f,  422.874084f,  431.143524f,  528.501343f,
        411.093658f,  524.954224f,  440.582123f,  505.096680f,  443.003174f,  558.256104f,
        398.718018f,  477.760376f,  423.779816f,  414.467621f,  405.266571f,  276.409698f,
        440.556519f,  453.872528f,  428.937378f,  352.858826f,  445.948059f,  446.137970f,
        440.381104f,  468.696686f,  432.739319f,  392.523560f,  415.850220f,  498.415833f,
        344.600464f,  211.977127f,  363.770325f,  339.389282f,  431.413452f,  386.396362f,
        423.490326f,  353.442505f,  443.894684f,  361.056305f,  441.701660f,  358.706726f,
        443.672760f,  347.925049f,  450.589142f,  353.543121f,  438.204529f,  330.143250f,
        420.934875f,  395.224396f,  425.743866f,  375.366943f,  426.337646f,  400.887451f,
        431.897827f,  366.990845f,  454.709167f,  381.671936f,  445.002197f,  393.740784f,
        439.143036f,  404.400482f,  425.018005f,  363.687592f,  393.081909f,  365.265717f,
        438.386475f,  396.395050f,  445.479492f,  388.794556f,  460.069092f,  393.413116f,
        456.819885f,  387.923065f,  460.466064f,  396.416443f,  443.724976f,  392.986328f,
        446.350830f,  386.656830f,  446.646698f,  369.915955f,  459.330902f,  397.090363f,
        441.898315f,  392.771362f,  461.658051f,  402.632690f,  469.572266f,  416.397461f,
        454.994476f,  383.715057f,  464.833710f,  388.926971f,  460.259064f,  386.661255f,
        452.520996f,  373.289948f,  451.739777f,  390.339539f,  465.700836f,  411.409149f,
        458.227936f,  446.883911f,  454.963989f,  427.738525f,  458.554108f,  419.132050f,
        460.971710f,  394.439636f,  465.890839f,  396.336578f,  440.560974f,  402.221161f,
        453.742981f,  416.436584f,  461.950043f,  416.477844f,  467.363281f,  436.667969f,
        453.118805f,  431.445557f,  452.127930f,  462.388550f,  471.809113f,  465.539520f,
        458.134552f,  430.433197f,  444.488251f,  398.432343f,  458.531281f,  400.304565f,
        468.314911f,  409.182068f,  461.965271f,  401.049896f,  471.769897f,  420.502197f,
        459.759796f,  428.311432f,  453.267365f,  381.773468f,  461.670685f,  383.708160f,
        421.664398f,  458.546753f,  437.642395f,  386.378998f,  468.105255f,  397.357849f,
        457.077301f,  421.356537f,  452.974915f,  402.185822f,  460.548370f,  435.423462f,
        430.832886f,  429.185638f,  445.780701f,  406.446808f,  429.195282f,  618.545654f,
        777.814209f,  617.729797f,  771.391296f,  611.300110f,  782.313416f,  607.347168f,
        780.209656f,  605.745605f,  776.585083f,  596.494263f,  781.681213f,  578.180054f,
        783.165588f,  566.497375f,  776.515991f,  594.980652f,  786.876709f,  604.777832f,
        781.160156f,  604.782776f,  788.133179f,  600.276855f,  786.318787f,  599.876343f,
        787.805908f,  603.111694f,  771.522644f,  580.440308f,  788.455566f,  527.565796f,
        805.012268f,  601.026306f,  789.367859f,  613.839905f,  776.408142f,  594.387451f,
        791.666626f,  584.042419f,  798.796814f,  602.197388f,  785.429382f,  578.918457f,
        793.393433f,  605.947937f,  779.597290f,  560.666931f,  796.719299f,  602.703064f,
        810.330139f,  609.809814f,  801.419983f,  592.370728f,  811.472046f,  606.917175f,
        798.989136f,  601.374512f,  803.441650f,  579.228210f,  817.270752f,  594.671570f,
        815.623108f,  573.699219f,  809.372803f,  620.545471f,  814.105103f,  630.122864f,
        803.809021f,  647.494019f,  801.343140f,  644.344055f,  800.068115f,  621.731384f,
        807.163696f,  620.188416f,  803.699768f,  644.274719f,  802.609863f,  627.651001f,
        808.684692f,  645.134521f,  817.497253f,  657.216919f,  805.060425f,  651.811157f,
        806.109009f,  665.852966f,  794.738647f,  715.082764f,  786.539856f,  697.164612f,
        794.676941f,  708.312561f,  795.714478f,  685.316040f,  805.645203f,  659.003723f,
        820.703674f,  634.016296f,  829.470764f,  670.791931f,  820.133911f,  686.592346f,
        813.340332f,  679.168274f,  820.979858f,  701.982239f,  795.750854f,  706.187927f,
        802.430237f,  721.414978f,  800.497742f,  659.177917f,  807.180603f,  690.884705f,
        800.191284f,  681.378052f,  808.847107f,  659.462097f,  815.915466f,  638.888184f,
        830.054260f,  656.907349f,  824.707275f,  679.201660f,  822.722839f,  669.037354f,
        822.114014f,  1444.396484f, 1301.864380f, 1430.932495f, 1305.795410f, 1434.640503f,
        1256.333374f, 1420.874878f, 1292.112793f, 1422.695068f, 1266.473389f, 1394.706543f,
        1266.239990f, 1334.917847f, 1230.416504f, 1434.383179f, 1185.070312f, 1439.708252f,
        1296.635620f, 1451.150513f, 1263.993652f, 1445.235352f, 1287.799927f, 1437.576294f,
        1285.599609f, 1438.422119f, 1291.625000f, 1456.204590f, 1196.146362f, 1421.137207f,
        1173.943970f, 1346.250000f, 1377.544434f, 1426.636841f, 1359.566895f, 1428.036621f,
        1356.347778f, 1426.131104f, 1346.026611f, 1423.706909f, 1331.556519f, 1424.295776f,
        1340.261353f, 1426.428345f, 1355.333130f, 1433.289795f, 1320.492676f, 1399.104248f,
        1404.125244f, 1421.348999f, 1400.228394f, 1442.140625f, 1319.910645f, 1422.083252f,
        1366.358154f, 1414.182739f, 1401.660889f, 1423.718506f, 1394.619385f, 1415.354980f,
        1407.441895f, 1419.539917f, 1392.477173f, 1409.824463f, 1418.910522f, 1423.916626f,
        1385.505493f, 1413.766846f, 1397.681641f, 1421.541260f, 1414.165405f, 1402.198364f,
        1472.962158f, 1425.796387f, 1430.169312f, 1428.275391f, 1442.529297f, 1437.114990f,
        1411.744019f, 1438.389404f, 1433.999512f, 1417.492676f, 1422.856445f, 1415.525513f,
        1423.624878f, 1417.375244f, 1439.201660f, 1407.739136f, 1473.056885f, 1445.573486f,
        1426.880249f, 1499.648438f, 1408.886353f, 1490.031494f, 1380.044312f, 1474.310425f,
        1356.260620f, 1418.100220f, 1414.001587f, 1385.942627f, 1455.776611f, 1431.807495f,
        1411.450806f, 1413.446045f, 1476.730347f, 1396.910278f, 1490.847168f, 1479.604004f,
        1420.376709f, 1567.137817f, 1261.855103f, 1554.387207f, 1279.955444f, 1410.622803f,
        1430.158203f, 1463.619507f, 1396.118652f, 1434.229858f, 1409.588989f, 1404.407959f,
        1455.900269f, 1441.371704f, 1401.178101f, 1473.805908f, 1402.455200f, 1563.479248f,
        1331.379761f, 1450.587891f, 1399.903198f, 62.560726f,   -20.764105f,  31.368124f,
        -15.839226f,  -45.881351f,  4.902741f,    -41.239906f,  -13.965880f,  -30.006031f,
        -25.936424f,  -53.521812f,  10.718786f,   31.657862f,   -0.407420f,   71.544472f,
        -25.844257f,  15.459104f,   -25.837545f,  -10.510986f,  -8.171846f,   -77.794144f,
        -2.831783f,   -158.984268f, 17.406960f,   -142.375198f, 11.290405f,   -92.253181f,
        4.710523f,    -21.866425f,  -5.227015f,   25.293186f,   -28.900677f,  -41.999817f,
        -12.754401f,  -73.701118f,  -5.742857f,   -121.739388f, -0.626722f,   -145.334412f,
        8.852077f,    -153.347641f, 8.584655f,    -173.678375f, 13.746968f,   -98.225090f,
        -8.577398f,   -66.594780f,  -28.505062f,  -62.835339f,  -1.554740f,   -54.912113f,
        -8.761719f,   -80.266327f,  -4.280245f,   -82.205696f,  -6.533293f,   -119.333084f,
        0.133738f,    -140.673248f, 4.568828f,    -112.802383f, -15.734044f,  -117.836060f,
        -25.152294f,  41.697582f,   -25.753210f,  40.020325f,   -14.662676f,  -23.789351f,
        -0.280702f,   -15.055105f,  -10.508280f,  -50.304287f,  -5.543379f,   78.819092f,
        -40.519398f,  12.195759f,   -16.326403f,  -114.096283f, -15.405404f,  22.691252f,
        6.022946f,    65.806770f,   2.496607f,    92.274796f,   -5.926957f,   20.785803f,
        2.176717f,    76.507149f,   -23.115765f,  181.121872f,  -22.782242f,  165.859711f,
        -13.410737f,  187.011261f,  -44.828323f,  123.029144f,  -49.049175f,  102.009644f,
        5.655825f,    127.256042f,  8.738811f,    134.287704f,  -18.901871f,  140.604324f,
        -48.075436f,  99.952736f,   -27.210930f,  90.550514f,   18.933846f,   136.898895f,
        -15.789597f,  266.982544f,  -90.550835f,  132.843872f,  -10.079618f,  187.094772f,
        -61.200657f,  183.175110f,  -11.219452f,  155.208313f,  -59.223499f,  261.097595f,
        -53.732281f,  272.970337f,  -59.711735f,  -340.531433f, -13.650066f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    24.953693f,   85.758118f,   20.400852f,
        83.449326f,   25.190321f,   86.784996f,   -12.194148f,  86.600639f,   13.326161f,
        103.620735f,  72.458069f,   58.141827f,   50.518654f,   62.937786f,   47.202267f,
        46.033577f,   39.243923f,   106.830627f,  63.944424f,   88.703629f,   38.636780f,
        85.578529f,   41.928471f,   67.019852f,   35.605827f,   78.367050f,   44.085491f,
        84.813972f,   62.197498f,   78.867256f,   68.152840f,   64.162956f,   108.921265f,
        140.434799f,  110.227440f,  122.912384f,  86.390923f,   111.357269f,  93.854271f,
        100.097466f,  85.061234f,   102.278984f,  96.798027f,   117.535950f,  83.089882f,
        138.718460f,  82.637039f,   129.285019f,  186.293518f,  207.845535f,  182.351608f,
        203.192322f,  151.915970f,  182.314728f,  159.029053f,  145.288589f,  179.344360f,
        120.608330f,  216.132858f,  103.648827f,  139.294037f,  166.683807f,  139.918457f,
        174.351196f,  307.353638f,  280.693420f,  267.319489f,  293.805389f,  275.193970f,
        242.120392f,  246.995285f,  218.593307f,  282.700562f,  159.849976f,  365.238342f,
        130.344681f,  246.057739f,  204.304260f,  205.516296f,  231.790497f,  510.658783f,
        307.188751f,  473.735077f,  306.858490f,  418.381470f,  300.420959f,  470.293335f,
        224.223267f,  440.650482f,  196.505646f,  295.193024f,  246.758896f,  223.711304f,
        264.152161f,  223.083359f,  280.815308f,  0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    15.816571f,   8.940322f,    20.998161f,   58.924213f,   13.460522f,
        19.511887f,   11.436489f,   19.015022f,   5.166424f,    2.395800f,    34.851967f,
        19.671047f,   43.107090f,   0.927895f,    31.148842f,   0.661411f,    17.158716f,
        0.139012f,    0.000000f,    0.000000f,    -17.825615f,  3.195054f,    -36.979931f,
        6.827360f,    -42.585648f,  -2.637798f,   2.392241f,    -536.500305f, 36.371265f,
        1.258433f,    35.552113f,   -10.841220f,  22.111000f,   -4.157352f,   12.322925f,
        3.547384f,    -0.000000f,   0.000000f,    -13.623132f,  2.603981f,    -23.732849f,
        16.149475f,   -23.240017f,  0.074119f,    7.778013f,    -281.315582f, 34.648300f,
        83.689644f,   -3.314750f,   -12.470082f,  -46.629707f,  -38.933731f,  21.900824f,
        10.269050f,   40.746090f,   -2.277141f,   -363.878998f, 124.563591f,  -146.773911f,
        57.234798f,   29.621359f,   -20.837706f,  12.792532f,   -8.089973f,   0.000000f,
        0.000000f,    -0.123503f,   11.942530f,   -6.327760f,   33.696968f,   -17.409220f,
        63.807167f,   -40.514286f,  146.607635f,  -521.302307f, 229.168640f,  -147.886124f,
        59.790039f,   37.534767f,   -26.747803f,  34.191628f,   -27.877928f,  0.000000f,
        0.000000f,    -11.362300f,  22.785870f,   -28.262712f,  56.329769f,   -54.687023f,
        104.763840f,  -91.643723f,  154.172363f,  468.657135f,  -436.514191f, 57.331112f,
        -79.577003f,  -101.912651f, 26.388111f,   -75.033531f,  7.881800f,    0.000000f,
        0.000000f,    37.821857f,   5.724637f,    111.211243f,  -28.502729f,  216.988098f,
        -138.972641f, 363.172302f,  -407.044739f, 2.394019f,    0.173649f,    0.000000f,
        0.000000f,    -0.912566f,   1.746516f,    6.493638f,    3.427663f,    3.144647f,
        4.514406f,    2.103729f,    3.090362f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    -12.646853f,
        -35.735935f,  -9.143981f,   -12.694000f,  -6.622192f,   -5.251657f,   -6.248694f,
        0.460536f,    -4.651177f,   0.846006f,    0.000000f,    0.000000f,    1.942055f,
        -3.729869f,   10.786839f,   -7.508418f,   63.163853f,   -23.202585f,  16.828211f,
        -37.828133f,  15.044542f,   -15.822247f,  11.612217f,   -7.713845f,   7.989010f,
        -1.506197f,   7.304657f,    -0.933909f,   0.000000f,    0.000000f,    -1.468957f,
        0.406524f,    5.858419f,    -5.491107f,   25.510017f,   -19.840551f,  8.689874f,
        -34.144440f,  5.887367f,    -12.635252f,  5.920063f,    -5.911479f,   3.451228f,
        -3.236877f,   -1.067969f,   1.171874f,    0.000000f,    0.000000f,    -3.403093f,
        0.720017f,    -0.581272f,   -4.302737f,   4.373732f,    -11.726876f,  -2.500628f,
        -143.905441f, -0.233199f,   -55.051613f,  3.039714f,    -47.013229f,  1.447444f,
        -20.336876f,  -0.578131f,   3.333971f,    0.000000f,    0.000000f,    -2.358175f,
        13.908725f,   -1.688929f,   18.075993f,   -1.836615f,   24.331480f,   0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    7.055044f,
        -1.146424f,   2.494159f,    0.922937f,    2.112984f,    -0.475867f,   3.458759f,
        -1.015300f,   1.745175f,    1.151310f,    6.848824f,    -2.357395f,   5.033576f,
        1.660397f,    8.852196f,    13.603169f,   -0.855824f,   1.026495f,    -3.740489f,
        -4.820429f,   -0.729472f,   2.214984f,    -3.457563f,   -0.322012f,   3.959835f,
        -4.678849f,   3.891486f,    -5.196818f,   -3.727339f,   -2.444582f,   -1.804166f,
        -6.625666f,   -12.681057f,  -8.374393f,   5.967014f,    -4.527366f,   20.000000f,
        10.000000f,   -0.289348f,   -7.988224f,   -9.955474f,   -47.219498f,  10.643692f,
        -22.267801f,  7.548688f,    5.740634f,    -46.475597f,  -31.192644f,  -3.574153f,
        -11.555604f,  4.195525f,    14.365610f,   -44.182861f,  -35.595730f,  -16.094891f,
        -1.785258f,   5.636321f,    10.363617f,   -54.896679f,  -7.565694f,   -7.361600f,
        5.133762f,    4.765584f,    29.635271f,   -61.601509f,  -65.173355f,  -43.110817f,
        -11.250557f,  0.000000f,    0.000000f,    -25.688046f,  -21.532396f,  -25.614168f,
        -21.630348f,  -10.297934f,  -25.241535f,  12.763733f,   -38.661667f,  39.295261f,
        -48.542805f,  81.622124f,   -64.207825f,  181.018906f,  -91.774155f,  278.707062f,
        -101.174667f, 0.000000f,    0.000000f,    23.695627f,   22.052565f,   51.377983f,
        10.021887f,   53.475372f,   16.575893f,   46.736988f,   15.256134f,   46.669594f,
        32.161327f,   0.000000f,    0.000000f,    0.000000f,    0.000000f,    -24.873896f,
        0.350280f,    -55.593037f,  -15.076749f,  -64.298027f,  -34.031445f,  -43.507076f,
        -60.112804f,  -20.299862f,  -82.815788f,  -13.086043f,  6.570877f,    -18.628101f,
        -54.601624f,  -24.031013f,  -49.807983f,  -63.983105f,  -12.863715f,  -24.616329f,
        -34.814167f,  -15.621903f,  16.118164f,   -9.675009f,   7.430085f,    -22.064392f,
        33.164654f};

std::string evalCalcTToString(const EvalCalcT term) {
    return std::format("{:>6.1f}", term);
}

std::string taperedTermToString(const TaperedTerm& term) {
    return std::format("{{{:>6.1f}, {:>6.1f}}}", term.early, term.late);
}

std::string getPstPieceTypeName(int idx) {
    if (idx == EvalParams::kPassedPawnPstIdx) {
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
            "\ncandidatePassedPawnBonus:\n\t{}",
            taperedTermToString(params.candidatePassedPawnBonus));

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
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
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
