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
        1.076343f,    0.298128f,    0.240863f,    0.442923f,    1.024694f,    1.000000f,
        -0.005373f,   -0.020394f,   -0.009528f,   -0.001548f,   123.647041f,  25.185045f,
        182.987457f,  433.517487f,  413.517853f,  386.161926f,  439.976471f,  606.679199f,
        791.827332f,  1468.868408f, 1297.017456f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -23.249632f,  181.563644f,  -25.707708f,
        184.176682f,  -12.500653f,  190.339920f,  -23.644266f,  182.071716f,  -6.135588f,
        208.720474f,  51.208374f,   186.550995f,  45.173058f,   173.726089f,  4.183400f,
        167.883148f,  -5.387030f,   178.456726f,  2.162874f,    177.508698f,  11.451884f,
        184.656830f,  13.994034f,   188.751602f,  24.182915f,   190.937378f,  44.858639f,
        179.620956f,  45.506069f,   173.658112f,  30.941763f,   170.263306f,  10.590463f,
        190.623444f,  21.045265f,   191.331100f,  35.829269f,   186.920685f,  45.493641f,
        185.895996f,  45.702126f,   187.602127f,  62.778679f,   183.208206f,  44.449852f,
        190.486740f,  30.173937f,   180.649811f,  35.008865f,   204.169525f,  38.096401f,
        211.467209f,  54.312660f,   203.179749f,  72.360931f,   191.594864f,  90.063568f,
        190.790329f,  80.337601f,   184.309174f,  51.435326f,   203.856339f,  53.234829f,
        186.976425f,  72.088165f,   221.056061f,  37.063702f,   231.650391f,  81.402527f,
        216.933441f,  103.547585f,  189.270767f,  148.443008f,  203.591553f,  175.856796f,
        200.741440f,  137.136765f,  192.216721f,  132.312836f,  192.097900f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    370.970673f,  401.442169f,  379.272247f,  381.161469f,  368.702118f,
        405.328613f,  385.014587f,  419.835297f,  382.484528f,  406.337311f,  398.089294f,
        387.311096f,  371.868896f,  389.637848f,  328.293793f,  388.679138f,  389.768799f,
        391.209473f,  373.000183f,  423.438019f,  401.085815f,  415.442261f,  424.584839f,
        406.067535f,  415.275085f,  403.214325f,  420.456116f,  400.932678f,  394.109344f,
        394.351135f,  398.911987f,  398.344055f,  396.095184f,  393.759003f,  421.020630f,
        407.592133f,  428.424927f,  410.475586f,  445.443848f,  428.418579f,  457.706635f,
        422.740753f,  435.197388f,  392.624603f,  430.549652f,  389.814880f,  406.073273f,
        377.671021f,  429.004425f,  403.130707f,  483.183136f,  400.362244f,  472.099335f,
        423.868896f,  460.616302f,  434.166443f,  469.191864f,  442.168793f,  462.458557f,
        424.003723f,  461.465759f,  416.016113f,  418.709473f,  411.579956f,  446.399139f,
        412.651978f,  463.913879f,  419.712402f,  488.486389f,  419.754639f,  526.864563f,
        431.560913f,  483.755127f,  435.856720f,  504.288818f,  434.123108f,  473.566284f,
        431.706726f,  475.003601f,  414.318207f,  432.384918f,  410.721283f,  491.814056f,
        408.371552f,  521.735779f,  412.029541f,  539.185181f,  412.159760f,  587.056946f,
        407.927643f,  553.310791f,  417.733185f,  491.211700f,  404.894012f,  456.215424f,
        398.251068f,  464.632843f,  399.689728f,  418.215210f,  426.259674f,  522.528381f,
        401.206299f,  520.051147f,  429.536926f,  498.493469f,  434.107544f,  535.469727f,
        397.131317f,  492.009277f,  411.949646f,  419.226349f,  395.712128f,  252.381165f,
        426.435730f,  441.218781f,  424.269653f,  340.781708f,  440.806549f,  417.027252f,
        439.068542f,  482.552368f,  417.210724f,  447.567719f,  396.733551f,  480.616425f,
        345.771759f,  240.715195f,  349.348877f,  334.787537f,  426.154602f,  388.412903f,
        414.603912f,  354.101257f,  434.266479f,  361.310059f,  432.525818f,  361.129150f,
        433.429260f,  350.095337f,  441.215942f,  358.715820f,  428.475555f,  333.363098f,
        413.475708f,  399.965454f,  418.422638f,  376.323181f,  415.862427f,  404.112854f,
        421.251556f,  369.414032f,  443.731567f,  384.752289f,  433.965179f,  397.107788f,
        429.093323f,  408.548096f,  414.836456f,  369.422333f,  377.611633f,  369.174255f,
        430.671631f,  398.887146f,  433.842957f,  391.016052f,  448.407867f,  396.413696f,
        444.148407f,  390.062286f,  449.343170f,  398.692780f,  432.676025f,  396.387177f,
        435.981293f,  391.894836f,  438.314423f,  372.207458f,  449.969757f,  396.584076f,
        432.505798f,  394.096008f,  450.305969f,  404.158691f,  457.501831f,  418.664856f,
        443.316803f,  385.087189f,  453.093201f,  392.927887f,  448.586548f,  394.322693f,
        438.154572f,  375.884644f,  443.184845f,  391.791779f,  455.609283f,  414.715637f,
        446.314636f,  447.723694f,  443.298828f,  430.672852f,  445.402191f,  423.688354f,
        449.750122f,  397.818451f,  453.394409f,  402.881683f,  427.672668f,  405.492676f,
        445.854095f,  418.553650f,  451.984131f,  420.625244f,  455.461853f,  440.107239f,
        441.494507f,  431.518890f,  440.848755f,  468.790619f,  459.614532f,  468.839691f,
        446.725830f,  435.619446f,  435.164246f,  402.818207f,  449.232880f,  394.720642f,
        462.450745f,  411.559937f,  452.835419f,  410.399719f,  459.988190f,  421.294006f,
        449.826019f,  435.467377f,  443.709473f,  389.250336f,  449.938904f,  387.148102f,
        415.002380f,  448.307037f,  433.795380f,  382.056122f,  461.734863f,  382.623169f,
        452.201416f,  415.126160f,  446.040466f,  422.424408f,  445.992249f,  420.593109f,
        427.910614f,  435.341888f,  436.016632f,  410.811829f,  423.498962f,  617.930054f,
        776.559875f,  616.631470f,  770.297363f,  609.542603f,  780.904602f,  605.883423f,
        779.317139f,  604.379822f,  775.232483f,  595.627197f,  780.678711f,  576.920532f,
        781.721802f,  564.802979f,  775.138306f,  592.819580f,  784.690247f,  602.901550f,
        778.641418f,  601.645752f,  785.908142f,  598.256775f,  783.152588f,  597.240356f,
        785.112366f,  601.124146f,  769.101562f,  572.836365f,  788.277893f,  525.827332f,
        802.519531f,  598.540894f,  786.544312f,  610.337097f,  773.385132f,  592.420288f,
        787.773682f,  582.396118f,  794.271545f,  600.730164f,  780.525696f,  576.387939f,
        789.407349f,  602.507324f,  777.300232f,  555.965515f,  795.362061f,  602.281982f,
        806.947388f,  607.951782f,  798.361938f,  591.918152f,  807.399841f,  604.995544f,
        796.195190f,  600.663330f,  799.274658f,  579.720703f,  811.755005f,  591.718628f,
        812.742493f,  570.695801f,  806.895203f,  621.209534f,  811.659241f,  628.667725f,
        801.815552f,  647.489868f,  798.710205f,  642.736206f,  798.265137f,  619.543396f,
        804.756042f,  618.216187f,  800.869263f,  640.660583f,  801.153076f,  623.735474f,
        807.690430f,  645.620911f,  816.162109f,  659.862549f,  802.110718f,  650.675110f,
        804.619568f,  664.859741f,  792.766296f,  711.126770f,  785.436279f,  690.009277f,
        794.761475f,  692.464905f,  800.319641f,  690.853882f,  802.096436f,  660.662109f,
        819.472717f,  637.044983f,  826.823486f,  671.356506f,  818.752686f,  685.753296f,
        812.379333f,  679.952942f,  818.409912f,  694.335388f,  797.104736f,  711.627930f,
        799.822205f,  725.295776f,  798.770081f,  658.342285f,  806.389893f,  695.276550f,
        797.903809f,  682.854004f,  806.790894f,  664.106934f,  812.462341f,  640.183472f,
        827.751892f,  650.131897f,  825.252991f,  687.569153f,  818.696289f,  666.614380f,
        822.211365f,  1480.292358f, 1268.103271f, 1468.914185f, 1265.849731f, 1473.385864f,
        1208.111572f, 1460.926880f, 1243.503540f, 1464.360596f, 1211.408447f, 1435.473267f,
        1222.894409f, 1384.830078f, 1169.074463f, 1485.154419f, 1138.946899f, 1478.815063f,
        1246.794189f, 1489.207031f, 1216.499756f, 1482.601074f, 1238.168701f, 1476.119629f,
        1240.507935f, 1478.462646f, 1239.604858f, 1496.561523f, 1146.306152f, 1456.908813f,
        1135.586914f, 1396.189209f, 1313.279053f, 1463.997314f, 1319.804565f, 1464.413818f,
        1310.001465f, 1462.229980f, 1299.641724f, 1461.104248f, 1280.407471f, 1463.482666f,
        1287.247314f, 1466.979248f, 1303.055420f, 1472.283936f, 1272.626221f, 1443.161621f,
        1333.450195f, 1461.436768f, 1350.698364f, 1476.473999f, 1284.635498f, 1458.402222f,
        1317.235718f, 1453.129883f, 1347.114258f, 1462.581787f, 1341.687988f, 1454.433716f,
        1355.853149f, 1459.003418f, 1340.114624f, 1452.735718f, 1358.823242f, 1464.817139f,
        1336.739258f, 1451.787720f, 1346.477051f, 1457.311523f, 1370.387085f, 1444.201660f,
        1412.220459f, 1460.067017f, 1379.814087f, 1468.425049f, 1385.991821f, 1474.447754f,
        1362.488892f, 1479.291382f, 1376.532471f, 1460.759521f, 1366.229980f, 1460.903076f,
        1360.926758f, 1463.089722f, 1373.906128f, 1457.369873f, 1398.456055f, 1468.891724f,
        1397.625854f, 1543.883179f, 1344.679932f, 1527.696289f, 1332.025391f, 1513.948364f,
        1315.796265f, 1459.495483f, 1367.558716f, 1426.799438f, 1409.046631f, 1472.030884f,
        1359.590088f, 1457.455566f, 1417.710938f, 1453.240234f, 1415.941406f, 1495.925049f,
        1390.856812f, 1610.776978f, 1203.163330f, 1584.936401f, 1244.246582f, 1447.639160f,
        1383.800659f, 1503.407471f, 1345.425781f, 1474.865479f, 1362.080688f, 1448.207886f,
        1403.766479f, 1486.595703f, 1344.966187f, 1528.709961f, 1340.626343f, 1593.463501f,
        1283.337769f, 1505.969604f, 1330.196411f, 71.132736f,   -23.082735f,  34.680023f,
        -16.538002f,  -45.449772f,  4.724274f,    -38.721302f,  -14.104265f,  -28.462364f,
        -26.014618f,  -53.954487f,  10.488633f,   34.971779f,   0.116327f,    76.746208f,
        -25.569588f,  16.646875f,   -25.727985f,  -12.891630f,  -8.442950f,   -82.344307f,
        -2.302096f,   -161.408737f, 17.473221f,   -147.123505f, 11.417018f,   -96.488777f,
        4.197658f,    -26.263947f,  -4.504461f,   22.640362f,   -27.640453f,  -44.800613f,
        -11.629430f,  -76.380394f,  -7.220874f,   -129.993317f, -0.271139f,   -149.849152f,
        8.908557f,    -156.073181f, 7.779748f,    -177.550537f, 12.746346f,   -102.883789f,
        -10.211462f,  -72.416100f,  -27.734426f,  -42.959789f,  -7.309564f,   -54.301960f,
        -10.610374f,  -91.919357f,  -2.913182f,   -91.102089f,  -5.923110f,   -123.069206f,
        -0.940737f,   -152.817627f, 5.331670f,    -115.691620f, -17.505304f,  -116.259804f,
        -26.124716f,  -2.859581f,   -11.213725f,  36.113834f,   -14.275962f,  -31.270939f,
        0.581149f,    -24.702822f,  -9.130848f,   -77.363533f,  -1.734515f,   34.292278f,
        -32.533035f,  11.760565f,   -18.149729f,  -110.437645f, -15.613954f,  22.183073f,
        9.444998f,    69.715538f,   2.176358f,    52.582302f,   3.501335f,    18.295549f,
        1.523021f,    74.466301f,   -25.296303f,  120.772911f,  -11.476130f,  115.660927f,
        -2.577291f,   145.169006f,  -38.770821f,  125.177597f,  -40.342239f,  109.436493f,
        7.268095f,    118.478012f,  11.697366f,   115.750618f,  -17.296953f,  116.704315f,
        -43.467316f,  110.607361f,  -31.012571f,  59.847164f,   26.645102f,   149.447769f,
        -13.163100f,  423.658875f,  -110.203667f, 112.619743f,  -2.389807f,   273.473419f,
        -80.990730f,  186.516373f,  -12.337273f,  116.459724f,  -49.911400f,  203.961777f,
        -47.116432f,  262.119049f,  -62.438461f,  -707.782349f, 57.206551f,   0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    25.440840f,   89.726067f,   23.694654f,
        83.327621f,   23.525030f,   89.243660f,   -10.922949f,  90.451164f,   8.288363f,
        109.721519f,  76.518845f,   57.239399f,   56.745712f,   60.767609f,   43.143337f,
        49.764938f,   43.199245f,   108.580627f,  73.523460f,   87.876938f,   44.300694f,
        86.851921f,   48.419659f,   67.541687f,   44.212082f,   79.048203f,   54.109352f,
        85.397934f,   75.958122f,   77.331650f,   74.798813f,   65.730927f,   110.712227f,
        143.777206f,  111.962509f,  125.521843f,  89.826363f,   113.653442f,  100.041901f,
        102.211517f,  88.381996f,   105.964470f,  103.327667f,  119.201920f,  87.591248f,
        140.479614f,  87.833000f,   130.684296f,  189.200851f,  209.973465f,  182.702606f,
        206.918381f,  156.037827f,  183.884033f,  165.884293f,  148.120758f,  185.153320f,
        123.930923f,  208.729691f,  109.245560f,  154.412598f,  165.492676f,  140.715668f,
        177.944580f,  310.469391f,  282.793304f,  272.165131f,  295.624634f,  275.959717f,
        245.196884f,  250.021973f,  220.357620f,  286.894440f,  160.750580f,  359.621826f,
        133.943924f,  249.199280f,  206.969925f,  217.044632f,  230.232590f,  512.763611f,
        310.659241f,  463.285553f,  314.995850f,  418.940399f,  304.232239f,  467.333496f,
        229.332565f,  421.850922f,  206.151733f,  302.504181f,  246.749008f,  182.356583f,
        279.848938f,  228.039459f,  281.332855f,  0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    26.498795f,   56.192284f,   10.270651f,   20.869394f,   14.578134f,
        22.898979f,   41.426880f,   21.627712f,   45.889275f,   1.294770f,    31.469442f,
        -0.044942f,   16.593559f,   -0.741428f,   0.000000f,    0.000000f,    -17.557278f,
        4.127919f,    -36.808655f,  12.084945f,   -38.580883f,  -2.493207f,   13.240912f,
        -506.769928f, 30.037073f,   3.589001f,    35.330906f,   -8.258543f,   22.331154f,
        -2.382178f,   12.482063f,   4.857636f,    -0.000000f,   0.000000f,    -15.783271f,
        1.677598f,    -28.333622f,  16.418854f,   -30.332096f,  1.279591f,    4.519262f,
        -158.161072f, 34.583157f,   84.767830f,   -2.912424f,   -12.962224f,  -45.605728f,
        -38.207344f,  23.522156f,   11.453211f,   42.531826f,   -1.153425f,   -383.431152f,
        132.972549f,  -136.265503f, 55.580971f,   32.406754f,   -21.749237f,  12.991393f,
        -7.839388f,   0.000000f,    0.000000f,    -0.619204f,   12.760157f,   -6.670236f,
        34.251842f,   -18.974888f,  65.368607f,   -40.831371f,  153.388718f,  -533.891174f,
        233.541504f,  -148.508606f, 60.519024f,   37.892361f,   -27.419088f,  35.039146f,
        -28.925783f,  0.000000f,    0.000000f,    -11.677526f,  22.686228f,   -28.984797f,
        56.815315f,   -57.240776f,  107.293686f,  -89.537956f,  146.542862f,  559.382568f,
        -520.325256f, 59.389729f,   -74.787476f,  -97.204163f,  24.841402f,   -76.886826f,
        11.983528f,   0.000000f,    0.000000f,    37.254330f,   6.361644f,    109.292862f,
        -26.591856f,  219.616653f,  -139.903900f, 356.749512f,  -367.483551f, 2.027054f,
        0.196190f,    0.000000f,    0.000000f,    0.151981f,    3.238188f,    7.314908f,
        4.308872f,    3.232467f,    5.032517f,    2.543240f,    3.956392f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    -9.899837f,   -34.808601f,  -6.872657f,   -12.560859f,  -4.976655f,
        -5.264258f,   -5.024575f,   1.199828f,    -3.908625f,   1.296442f,    0.000000f,
        0.000000f,    2.814907f,    -3.267855f,   8.091547f,    -4.230698f,   60.927170f,
        -20.966341f,  18.079554f,   -37.920155f,  15.608200f,   -15.076222f,  11.743558f,
        -6.717040f,   8.137227f,    -0.998134f,   6.487773f,    0.014573f,    0.000000f,
        0.000000f,    -1.557891f,   0.375752f,    1.162821f,    -4.102153f,   16.423700f,
        -18.233364f,  10.794735f,   -32.631298f,  7.194030f,    -11.805713f,  6.658073f,
        -5.525007f,   3.565835f,    -2.473123f,   -1.440401f,   2.237964f,    0.000000f,
        0.000000f,    -4.090968f,   0.916110f,    -1.489074f,   -4.385657f,   3.526100f,
        -12.383390f,  -1.379825f,   -139.287704f, 1.204738f,    -52.111324f,  3.823279f,
        -43.335091f,  2.566075f,    -19.191452f,  0.703916f,    -0.575792f,   0.000000f,
        0.000000f,    -3.077710f,   16.262510f,   -2.346584f,   18.368366f,   -1.849497f,
        24.023350f,   0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    7.569688f,    -1.146352f,   2.657297f,    1.025105f,    2.322103f,
        -0.236607f,   4.014159f,    -0.765078f,   1.712286f,    1.696479f,    7.127638f,
        -2.268928f,   4.773058f,    1.886214f,    8.458288f,    14.021043f,   -3.720844f,
        -4.482083f,   -0.561585f,   2.346169f,    -4.026673f,   -0.445362f,   4.200006f,
        -4.744140f,   3.704446f,    -4.112027f,   -4.488333f,   -2.038433f,   -1.050955f,
        -6.628418f,   -12.368067f,  -8.383889f,   20.000000f,   10.000000f,   3.199139f,
        -4.582242f,   -10.368079f,  -48.058174f,  13.590766f,   -19.183290f,  7.639494f,
        6.279692f,    -48.037720f,  -31.646063f,  -3.274791f,   -11.608725f,  4.357546f,
        15.049479f,   -44.818554f,  -36.665798f,  -15.638129f,  -1.979968f,   6.349527f,
        10.665262f,   -55.142960f,  -7.907895f,   -6.947907f,   5.146645f,    5.171073f,
        29.948528f,   -66.088692f,  -46.151550f,  -43.615967f,  1.331270f,    0.000000f,
        0.000000f,    -9.349228f,   -29.772289f,  -9.876392f,   -30.638481f,  4.660227f,
        -33.631359f,  27.627924f,   -46.965412f,  51.148109f,   -56.785915f,  89.166107f,
        -70.736115f,  162.873779f,  -90.166161f,  301.028503f,  -112.856194f, 0.000000f,
        0.000000f,    1.555909f,    29.537653f,   27.078974f,   17.779037f,   28.857477f,
        25.367983f,   24.753071f,   22.756811f,   23.144918f,   39.611160f,   0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -18.032602f,  -0.038465f,   -23.956156f,
        -23.684460f,  -7.245345f,   -51.747536f,  43.227589f,   -87.542458f,  81.890244f,
        -116.387955f, -13.958578f,  6.788532f,    -17.901762f,  -56.438564f,  -24.253124f,
        -50.310043f,  -63.397900f,  -12.530915f,  -29.663237f,  -26.215139f,  -13.524894f,
        16.600191f,   -7.145056f,   8.548271f,    -21.775497f,  33.888783f};

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
            "\nconnectedPassedPawnBonus:\n\t{}",
            taperedTermToString(params.connectedPassedPawnBonus));

    oss << std::format(
            "\ndoubledPawnPenalty:\n\t{}", taperedTermToString(params.doubledPawnPenalty));

    oss << std::format(
            "\nisolatedPawnPenalty:\n\t{}", taperedTermToString(params.isolatedPawnPenalty));

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
