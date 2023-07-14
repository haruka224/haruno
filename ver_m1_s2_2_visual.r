# ver_m1_s2_1_tousu.r の可視化

# pre_visual
# block1 母豚数、分娩腹数
num_boton_H <- NULL
num_boton_H <- rbind(num_boton_H, boton_H, kouhai_H, bunben_H, per_bunben_H, one_bunben_H)
num_boton_H <- as.data.frame(num_boton_H)

# block2 産次別交配
kouhai_san_H <- NULL
kouhai_san_H <- cbind(kouhai_san_H, kouhai_san_hL$交配腹数, kouhai_san_hW$交配腹数, kouhai_san_h$交配腹数)

# block3 産次別分娩
bunben_san_H <- NULL
bunben_san_H <- cbind(bunben_san_H, bunben_san_hL$分娩腹数, bunben_san_hW$分娩腹数, bunben_san_h$分娩腹数)


# pre_visual
# block1 母豚数、分娩腹数
num_boton_T <- NULL
num_boton_T <- rbind(num_boton_T, boton_T, kouhai_T, bunben_T, per_bunben_T, one_bunben_T)
num_boton_T <- as.data.frame(num_boton_T)

# block2 産次別交配
kouhai_san_T <- NULL
kouhai_san_T <- cbind(kouhai_san_T, kouhai_san_hL$交配腹数, kouhai_san_hW$交配腹数, kouhai_san_h$交配腹数)

# block3 産次別分娩
bunben_san_T <- NULL
bunben_san_T <- cbind(bunben_san_T, bunben_san_hL$分娩腹数, bunben_san_hW$分娩腹数, bunben_san_h$分娩腹数)

# visual
num_boton_H
kouhai_san_H
bunben_san_H
var_H_list
one_H_list
rownames(var_H_list) <- c("分娩数", "死産", "哺乳頭数", "事故", "離乳数", "総分娩重", "総離乳重", "哺乳期間", "妊娠期間", "WMI3","増加体重")
colnames(var_H_list) <- c("L.mean","W.mean", "T.mean", "L.sd", "W.sd")

# visual
num_boton_T
kouhai_san_T
bunben_san_T
var_T_list
one_T_list
rownames(var_T_list) <- c("分娩数", "死産", "哺乳頭数", "事故", "離乳数", "総分娩重", "総離乳重", "哺乳期間", "妊娠期間", "WMI3","増加体重")
colnames(var_T_list) <- c("L.mean","W.mean", "T.mean", "L.sd", "W.sd")
