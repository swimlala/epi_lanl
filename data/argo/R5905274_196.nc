CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-21T15:30:44Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [h   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ѭ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � gx   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230521153044  20230521153044  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�*hwt�@�*hwt�11  @�*h����@�*h����@08q`�l@08q`�l�d.Ѣ��d.Ѣ�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  ?��?��H@=p�@�  @��R@��R@�G�A ��AG�A!G�A,(�A@  A`  A�Q�A�  A�  A�  A�Q�A�  A�\)A�  B   B  B(�B  B   B(  B0  B8(�B?�
BH  BP(�BX  B_�Bg�Bo�
Bx  B�  B�{B��B��B�  B�{B�{B��B�B��
B��
B��
B��B��B��B��B�{B�(�B�{B�{B�  B�{B�{B�(�B�  B��B�  B�  B�{B�  B�{B�(�C {C  C�C�HC  C	��C�
C  C��C��C  C
=C�C�C��C��C   C"  C${C&(�C({C*
=C,{C.{C0{C2�C4{C6  C8{C:33C<
=C>  C@  CB  CD  CF  CH
=CJ
=CL
=CM��CO��CR  CT
=CV{CX  CZ  C\  C]��C`  Ca��Cc�Ce�Cg�Ci��Cl  Cm��Co��Cr  Ct
=Cv  Cw�Cy��C|  C~
=C��C���C���C���C���C�  C�  C���C���C���C�C�  C�  C�  C���C�  C�  C�  C�C�C���C���C���C���C���C��C�  C�
=C�  C���C��C���C���C���C���C���C�C�  C�  C�C�  C�C�  C�  C�C�  C�C�C���C���C���C���C���C���C�C�  C�  C�C�
=C�  C���C�  C�  C�  C�C�
=C�  C�C�C�C�
=C�
=C�
=C�
=C�\C�
=C���C���C�  C�C�
=C�C�  C�C�C�C�C���C���C�C�C���C���C�C���C�  C�C�  C���C���C���C���C���C�C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C�
=C�C�C�  C�  C���C���C���C�C�C�C�C�C�  C���D ��D  D}qD�qD}qD�qD��D�qD� D�qDz�D��Dz�D  D��DD� D��D	� D
D
��D�D��DD�D�qDz�D��D}qD��DxRD�qD� DD��D  D��D�qD}qD�qD� D�D�D�D� D�qDz�D  D}qD��Dz�D��Dz�D�RDxRD�RDxRD�RD� D�qD}qD�qD}qD   D � D!�D!�D"�D"��D"�qD#}qD#�qD$� D%  D%� D&�D&��D&�qD'��D(�D(� D(�qD)��D*D*��D*�qD+z�D+�qD,��D-  D-}qD.D.�D/�D/}qD/�RD0z�D1�D1��D2  D2��D3�D3�D4�D4� D4�qD5� D6  D6z�D6�qD7� D7��D8}qD9�D9��D:  D:��D;  D;� D<�D<��D=  D=� D>�D>��D?�D?� D@  D@� DA�DA�DB�DB}qDB�RDCz�DD  DD��DE�DE�DF  DF� DF�qDG� DG�qDHz�DH��DI� DJ�DJ� DK�DK� DK�qDL� DM  DM� DN�DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR�qDSz�DS��DTz�DU  DU��DU�qDVz�DW�DW� DX  DX��DY  DY}qDZ  DZ�D[  D[��D[��D\}qD\��D]��D^�D^� D_  D_z�D_�qD`z�D`�RDa� Db  Db}qDb��Dc}qDd�Dd}qDe�De��Df  Df}qDg�Dg��Dg�qDh}qDi  Di��Dj  Dj� Dk  Dk}qDl  Dl�Dm�Dm��Dm�qDn}qDo  Do}qDp  Dp�Dq  Dq}qDr  Dr��DsDs��Dt  Dt}qDt��Duz�Du�qDv}qDwDw� Dx  Dx� Dx�qDy��Dy�qDz}qDz�qD{� D|�D|z�D}  D}z�D}�qD~}qD~�qD}qD�qD�>�D�}qD���D��D�AHD�� D�D�  D�@ D��HD��HD�  D�AHD��HD�� D���D�>�D�~�D�� D��qD�AHD��HD��qD�  D�@ D�~�D���D��qD�@ D�� D�� D��D�AHD�� D��HD���D�=qD�� D�� D�HD�AHD�� D���D�  D�>�D�� D��HD��qD�>�D�~�D��HD�HD�@ D���D�� D�HD�>�D�� D�� D�HD�<)D�~�D�� D�  D�C�D�~�D���D�  D�@ D�~�D��HD��D�@ D�~�D�� D��qD�AHD�� D���D�  D�>�D��HD�D��D�>�D�� D��HD�  D�>�D�}qD���D�  D�AHD�� D��qD���D�@ D�� D�� D�HD�>�D�~�D�� D�  D�@ D�~�D�� D�HD�B�D��HD�D�  D�=qD�~�D���D�  D�>�D�� D���D��qD�>�D�~�D���D���D�>�D�� D�� D�  D�@ D�~�D�D�  D�@ D��HD�� D���D�@ D��HD�� D�HD�@ D�� D��HD�HD�>�D�}qD�� D�  D�>�D�}qD���D�  D�=qD�� D���D���D�@ D��HD��HD�  D�>�D�� D��HD���D�=qD�� D��HD�HD�AHD�~�D�� D�  D�@ D�� D��HD��D�AHD�� D���D���D�@ D�� D�� D��D�@ D�~�D���D�  D�@ D�}qD��qD�HD�AHD�� D���D��qD�>�D�~�D���D�  D�@ D��HD��HD�HD�B�D���D�� D���D�>�D�� D��HD���D�@ D���D�� D���D�@ D���D�D�  D�@ D��HD�� D�HD�@ D�~�D��HD��D�@ D�� D��HD�HD�B�D�~�D��qD���D�>�D�~�D�� D�HD�@ D�~�D��qD���D�AHD�� D��HD�  D�@ D��HD�� D��D�AHD��HD���D�HD�@ D�~�D���D�HD�B�D��HD���D�  D�>�D D�� D�HD�@ D�~�D��HD�  D�=qD�~�D�� D�HD�AHDŁHDž�D�  D�AHDƁHD��HD�HD�@ Dǀ D�� D���D�>�DȀ DȽqD�  D�@ DɁHD�D��D�>�D�~�D��HD�HD�AHDˀ D˾�D��qD�>�D̀ D�� D�  D�>�D̀ D�� D�HD�AHD�~�D�� D�HD�@ D�~�D��HD�  D�>�DЁHD�D���D�@ D�~�DѾ�D�  D�@ DҁHD�� D���D�>�D�}qD�� D�  D�>�DԁHD�� D�  D�AHDՁHD�� D�  D�>�Dր D�� D�HD�>�D׀ D��HD���D�>�D؀ DؽqD�  D�@ Dـ D��HD�  D�@ D�~�D��HD�  D�AHDۀ D�� D�  D�>�D܀ Dܾ�D�HD�B�D݁HD��HD�  D�AHD�~�D޾�D���D�B�D߀ D߾�D�  D�@ D���D��HD�  D�AHD�HD���D���D�>�D�~�D�D���D�>�D�~�D�� D�  D�>�D�HD�D�  D�>�D�~�D�� D���D�>�D�}qD�� D��D�@ D�~�D羸D�  D�@ D�~�D��HD���D�AHD� D�� D��qD�@ D�~�D꾸D�  D�@ D�~�D��HD�HD�AHD�~�D��HD���D�>�D킏D�� D�  D�>�D�HD��HD�  D�@ D� D�qD�HD�@ D�~�D�D��D�@ D� D���D�HD�@ D�}qD�D�HD�B�D�~�D�qD�  D�AHD�D��HD���D�=qD��HD�� D�  D�>�D�� D�D��D�B�D��HD��HD�HD�=qD�~�D��HD�HD�AHD�~�D��qD�HD�=qD��HD���>��
>���?k�?���?��@   @��@5@L��@c�
@}p�@���@���@��@�@��
@��@�  @���@�p�AffA�RAA��A$z�A+�A2�\A:=qAAG�AHQ�AO\)AUA\��Ab�\Ah��An{As33Ax��A\)A�=qA�(�A�A�  A�=qA�z�A�ffA���A��\A���A��A�=qA�(�A�ffA�Q�A��\A���A��RA���A��A�(�A�{A�Q�A�=qA��
A�A�  A��\A���A��RA���A\A���AƸRA�  Aə�A˅A�p�A�
=A���Aҏ\A���A�ffAأ�A��HA���A�
=A���A�\A���A�RA�  A��A�(�A�ffA���A�=qA�z�A�ffA���A��HA���A�ffB Q�B�B=qB
=Bz�Bp�B�\B�Bz�B	��B
ffB�Bz�Bp�B�\B�B��B��BffB�BQ�B�B{B33B  B�B{B33B  B�B{B33B z�B!B"�RB#�B$��B&{B'33B(Q�B)p�B*ffB+�B,��B-B.�HB0  B1�B2ffB3\)B4z�B5p�B6ffB7\)B8z�B9��B:�RB;�
B<��B=�B?
=B@z�BA��BB�\BD  BEG�BF=qBG\)BHz�BI��BJ�RBK�BLz�BM��BN�\BO�BP��BR{BS�BT��BUBV�HBX(�BX��BZ{B[
=B\z�B]�B_
=B`Q�Bap�Bb�\Bc\)BdQ�Be��Bf�RBhQ�Bi��Bj�RBl  Bm�Bn{Bo\)Bp��BqBs33Bt��Bv{Bw\)Bx��ByB{33B|Q�B}��B~�\B�B�z�B��B��B�Q�B�
=B��B�Q�B��HB��B�  B�z�B���B���B�{B��RB��B�  B���B��B���B�{B��RB�33B��
B�z�B��B��B�Q�B���B�33B�B�z�B��B��B�Q�B���B�33B��
B�Q�B���B�B�Q�B��HB�p�B��
B�ffB���B��B�Q�B��HB�p�B�{B�ffB���B�\)B�(�B���B�p�B�  B��\B���B���B�{B���B��B�{B���B�
=B���B�(�B��RB�p�B�(�B���B�G�B��B�=qB��RB�\)B�(�B��RB�G�B��B�(�B��HB���B�=qB���B��B��B�=qB��HB���B�=qB��\B��B��B�(�B���B��B�=qB��\B��B��B�z�B��B�p�B�{B��\B�33B��B�z�B��B��B�  B\B��B��B�ffB���B�\)B��
Bƣ�B�33BǅB�{B���B�p�B��B�z�B�
=B�B�Q�Ḅ�B�G�B��BΏ\B��B�p�B�  B���B�\)B�B�Q�B��HBә�B��Bԏ\B�G�B��
B�=qB���B�\)B�{Bأ�B�
=BمB�Q�B��HB�\)B��
B�Q�B�
=BݮB�  B�z�B��B��
B�ffB�RB�G�B�{B�z�B���B㙚B�=qB�RB�33B�  B�\B��HB�p�B�=qB���B�33B��
B�\B�
=B�B�(�B���B�G�B��
B��B�33B�B�{B��HB�B�B�ffB�33B�B�{B�\B�\)B�B�=qB���B���B�  B�z�B�G�B��
B�=qB��RB��B�=qB��\B�
=B��
B�z�B���B�\)C {C Q�C �\C �C=qCffC�C  C\)C��C��C�Cz�C�C�CQ�C�\C��C�CffC�C�CG�C��CC{C�C��C
=C\)CC��C	G�C	�RC
  C
=qC
��C  C(�C�C�C�Cp�C�HC�Cz�C�C�C�C�C�C�C�C33C�\C
=CG�C�RC�C\)C��C{CffC�HC�C�\C  C=qC�RC
=CQ�C��C{Cp�C�C(�C��C
=CG�CC
=Cz�C�HC(�C��C�CG�C��C{C�\C�
C=qC�RC  Cz�C�RC 33C ��C �
C!Q�C!��C!��C"p�C"�RC#(�C#ffC#��C$=qC$p�C$�C%33C%��C%��C&G�C&C'  C'p�C'C(�C(��C(�HC)Q�C)��C*{C*z�C*�RC+=qC+�C+��C,\)C,�RC-=qC-�C.
=C.\)C.�
C/�C/�C0  C0=qC0�RC1  C1p�C1�RC233C2p�C2�C333C3��C3��C4Q�C4C5
=C5z�C5C633C6z�C6��C7(�C7��C7�
C8G�C8p�C8��C9  C9\)C9z�C9�
C9��C:=qC:Q�C:��C:�C:��C;  C;G�C;ffC;�C;��C;�HC<(�C<=qC<�C<��C<�
C={C=(�C=p�C=�C=��C=�HC>  C>Q�C>ffC>�C>C>��C?(�C?G�C?�C?�\C?�HC?�C@=qC@G�C@z�C@�RC@CA{CA�CAffCAp�CA�RCA��CA��CB=qCB=qCB�\CB��CB�HCB�CC33CC\)CCp�CCCC��CD{CD(�CDp�CD�CD�RCD�CE{CE=qCE\)CE��CE�CE��CF
=CFG�CFffCF��CF�CF��CG
=CGQ�CGffCG�\CG�
CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                               ?��?��H@=p�@�  @��R@��R@�G�A ��AG�A!G�A,(�A@  A`  A�Q�A�  A�  A�  A�Q�A�  A�\)A�  B   B  B(�B  B   B(  B0  B8(�B?�
BH  BP(�BX  B_�Bg�Bo�
Bx  B�  B�{B��B��B�  B�{B�{B��B�B��
B��
B��
B��B��B��B��B�{B�(�B�{B�{B�  B�{B�{B�(�B�  B��B�  B�  B�{B�  B�{B�(�C {C  C�C�HC  C	��C�
C  C��C��C  C
=C�C�C��C��C   C"  C${C&(�C({C*
=C,{C.{C0{C2�C4{C6  C8{C:33C<
=C>  C@  CB  CD  CF  CH
=CJ
=CL
=CM��CO��CR  CT
=CV{CX  CZ  C\  C]��C`  Ca��Cc�Ce�Cg�Ci��Cl  Cm��Co��Cr  Ct
=Cv  Cw�Cy��C|  C~
=C��C���C���C���C���C�  C�  C���C���C���C�C�  C�  C�  C���C�  C�  C�  C�C�C���C���C���C���C���C��C�  C�
=C�  C���C��C���C���C���C���C���C�C�  C�  C�C�  C�C�  C�  C�C�  C�C�C���C���C���C���C���C���C�C�  C�  C�C�
=C�  C���C�  C�  C�  C�C�
=C�  C�C�C�C�
=C�
=C�
=C�
=C�\C�
=C���C���C�  C�C�
=C�C�  C�C�C�C�C���C���C�C�C���C���C�C���C�  C�C�  C���C���C���C���C���C�C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C�
=C�C�C�  C�  C���C���C���C�C�C�C�C�C�  C���D ��D  D}qD�qD}qD�qD��D�qD� D�qDz�D��Dz�D  D��DD� D��D	� D
D
��D�D��DD�D�qDz�D��D}qD��DxRD�qD� DD��D  D��D�qD}qD�qD� D�D�D�D� D�qDz�D  D}qD��Dz�D��Dz�D�RDxRD�RDxRD�RD� D�qD}qD�qD}qD   D � D!�D!�D"�D"��D"�qD#}qD#�qD$� D%  D%� D&�D&��D&�qD'��D(�D(� D(�qD)��D*D*��D*�qD+z�D+�qD,��D-  D-}qD.D.�D/�D/}qD/�RD0z�D1�D1��D2  D2��D3�D3�D4�D4� D4�qD5� D6  D6z�D6�qD7� D7��D8}qD9�D9��D:  D:��D;  D;� D<�D<��D=  D=� D>�D>��D?�D?� D@  D@� DA�DA�DB�DB}qDB�RDCz�DD  DD��DE�DE�DF  DF� DF�qDG� DG�qDHz�DH��DI� DJ�DJ� DK�DK� DK�qDL� DM  DM� DN�DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR�qDSz�DS��DTz�DU  DU��DU�qDVz�DW�DW� DX  DX��DY  DY}qDZ  DZ�D[  D[��D[��D\}qD\��D]��D^�D^� D_  D_z�D_�qD`z�D`�RDa� Db  Db}qDb��Dc}qDd�Dd}qDe�De��Df  Df}qDg�Dg��Dg�qDh}qDi  Di��Dj  Dj� Dk  Dk}qDl  Dl�Dm�Dm��Dm�qDn}qDo  Do}qDp  Dp�Dq  Dq}qDr  Dr��DsDs��Dt  Dt}qDt��Duz�Du�qDv}qDwDw� Dx  Dx� Dx�qDy��Dy�qDz}qDz�qD{� D|�D|z�D}  D}z�D}�qD~}qD~�qD}qD�qD�>�D�}qD���D��D�AHD�� D�D�  D�@ D��HD��HD�  D�AHD��HD�� D���D�>�D�~�D�� D��qD�AHD��HD��qD�  D�@ D�~�D���D��qD�@ D�� D�� D��D�AHD�� D��HD���D�=qD�� D�� D�HD�AHD�� D���D�  D�>�D�� D��HD��qD�>�D�~�D��HD�HD�@ D���D�� D�HD�>�D�� D�� D�HD�<)D�~�D�� D�  D�C�D�~�D���D�  D�@ D�~�D��HD��D�@ D�~�D�� D��qD�AHD�� D���D�  D�>�D��HD�D��D�>�D�� D��HD�  D�>�D�}qD���D�  D�AHD�� D��qD���D�@ D�� D�� D�HD�>�D�~�D�� D�  D�@ D�~�D�� D�HD�B�D��HD�D�  D�=qD�~�D���D�  D�>�D�� D���D��qD�>�D�~�D���D���D�>�D�� D�� D�  D�@ D�~�D�D�  D�@ D��HD�� D���D�@ D��HD�� D�HD�@ D�� D��HD�HD�>�D�}qD�� D�  D�>�D�}qD���D�  D�=qD�� D���D���D�@ D��HD��HD�  D�>�D�� D��HD���D�=qD�� D��HD�HD�AHD�~�D�� D�  D�@ D�� D��HD��D�AHD�� D���D���D�@ D�� D�� D��D�@ D�~�D���D�  D�@ D�}qD��qD�HD�AHD�� D���D��qD�>�D�~�D���D�  D�@ D��HD��HD�HD�B�D���D�� D���D�>�D�� D��HD���D�@ D���D�� D���D�@ D���D�D�  D�@ D��HD�� D�HD�@ D�~�D��HD��D�@ D�� D��HD�HD�B�D�~�D��qD���D�>�D�~�D�� D�HD�@ D�~�D��qD���D�AHD�� D��HD�  D�@ D��HD�� D��D�AHD��HD���D�HD�@ D�~�D���D�HD�B�D��HD���D�  D�>�D D�� D�HD�@ D�~�D��HD�  D�=qD�~�D�� D�HD�AHDŁHDž�D�  D�AHDƁHD��HD�HD�@ Dǀ D�� D���D�>�DȀ DȽqD�  D�@ DɁHD�D��D�>�D�~�D��HD�HD�AHDˀ D˾�D��qD�>�D̀ D�� D�  D�>�D̀ D�� D�HD�AHD�~�D�� D�HD�@ D�~�D��HD�  D�>�DЁHD�D���D�@ D�~�DѾ�D�  D�@ DҁHD�� D���D�>�D�}qD�� D�  D�>�DԁHD�� D�  D�AHDՁHD�� D�  D�>�Dր D�� D�HD�>�D׀ D��HD���D�>�D؀ DؽqD�  D�@ Dـ D��HD�  D�@ D�~�D��HD�  D�AHDۀ D�� D�  D�>�D܀ Dܾ�D�HD�B�D݁HD��HD�  D�AHD�~�D޾�D���D�B�D߀ D߾�D�  D�@ D���D��HD�  D�AHD�HD���D���D�>�D�~�D�D���D�>�D�~�D�� D�  D�>�D�HD�D�  D�>�D�~�D�� D���D�>�D�}qD�� D��D�@ D�~�D羸D�  D�@ D�~�D��HD���D�AHD� D�� D��qD�@ D�~�D꾸D�  D�@ D�~�D��HD�HD�AHD�~�D��HD���D�>�D킏D�� D�  D�>�D�HD��HD�  D�@ D� D�qD�HD�@ D�~�D�D��D�@ D� D���D�HD�@ D�}qD�D�HD�B�D�~�D�qD�  D�AHD�D��HD���D�=qD��HD�� D�  D�>�D�� D�D��D�B�D��HD��HD�HD�=qD�~�D��HD�HD�AHD�~�D��qD�HD�=qD��HD���>��
>���?k�?���?��@   @��@5@L��@c�
@}p�@���@���@��@�@��
@��@�  @���@�p�AffA�RAA��A$z�A+�A2�\A:=qAAG�AHQ�AO\)AUA\��Ab�\Ah��An{As33Ax��A\)A�=qA�(�A�A�  A�=qA�z�A�ffA���A��\A���A��A�=qA�(�A�ffA�Q�A��\A���A��RA���A��A�(�A�{A�Q�A�=qA��
A�A�  A��\A���A��RA���A\A���AƸRA�  Aə�A˅A�p�A�
=A���Aҏ\A���A�ffAأ�A��HA���A�
=A���A�\A���A�RA�  A��A�(�A�ffA���A�=qA�z�A�ffA���A��HA���A�ffB Q�B�B=qB
=Bz�Bp�B�\B�Bz�B	��B
ffB�Bz�Bp�B�\B�B��B��BffB�BQ�B�B{B33B  B�B{B33B  B�B{B33B z�B!B"�RB#�B$��B&{B'33B(Q�B)p�B*ffB+�B,��B-B.�HB0  B1�B2ffB3\)B4z�B5p�B6ffB7\)B8z�B9��B:�RB;�
B<��B=�B?
=B@z�BA��BB�\BD  BEG�BF=qBG\)BHz�BI��BJ�RBK�BLz�BM��BN�\BO�BP��BR{BS�BT��BUBV�HBX(�BX��BZ{B[
=B\z�B]�B_
=B`Q�Bap�Bb�\Bc\)BdQ�Be��Bf�RBhQ�Bi��Bj�RBl  Bm�Bn{Bo\)Bp��BqBs33Bt��Bv{Bw\)Bx��ByB{33B|Q�B}��B~�\B�B�z�B��B��B�Q�B�
=B��B�Q�B��HB��B�  B�z�B���B���B�{B��RB��B�  B���B��B���B�{B��RB�33B��
B�z�B��B��B�Q�B���B�33B�B�z�B��B��B�Q�B���B�33B��
B�Q�B���B�B�Q�B��HB�p�B��
B�ffB���B��B�Q�B��HB�p�B�{B�ffB���B�\)B�(�B���B�p�B�  B��\B���B���B�{B���B��B�{B���B�
=B���B�(�B��RB�p�B�(�B���B�G�B��B�=qB��RB�\)B�(�B��RB�G�B��B�(�B��HB���B�=qB���B��B��B�=qB��HB���B�=qB��\B��B��B�(�B���B��B�=qB��\B��B��B�z�B��B�p�B�{B��\B�33B��B�z�B��B��B�  B\B��B��B�ffB���B�\)B��
Bƣ�B�33BǅB�{B���B�p�B��B�z�B�
=B�B�Q�Ḅ�B�G�B��BΏ\B��B�p�B�  B���B�\)B�B�Q�B��HBә�B��Bԏ\B�G�B��
B�=qB���B�\)B�{Bأ�B�
=BمB�Q�B��HB�\)B��
B�Q�B�
=BݮB�  B�z�B��B��
B�ffB�RB�G�B�{B�z�B���B㙚B�=qB�RB�33B�  B�\B��HB�p�B�=qB���B�33B��
B�\B�
=B�B�(�B���B�G�B��
B��B�33B�B�{B��HB�B�B�ffB�33B�B�{B�\B�\)B�B�=qB���B���B�  B�z�B�G�B��
B�=qB��RB��B�=qB��\B�
=B��
B�z�B���B�\)C {C Q�C �\C �C=qCffC�C  C\)C��C��C�Cz�C�C�CQ�C�\C��C�CffC�C�CG�C��CC{C�C��C
=C\)CC��C	G�C	�RC
  C
=qC
��C  C(�C�C�C�Cp�C�HC�Cz�C�C�C�C�C�C�C�C33C�\C
=CG�C�RC�C\)C��C{CffC�HC�C�\C  C=qC�RC
=CQ�C��C{Cp�C�C(�C��C
=CG�CC
=Cz�C�HC(�C��C�CG�C��C{C�\C�
C=qC�RC  Cz�C�RC 33C ��C �
C!Q�C!��C!��C"p�C"�RC#(�C#ffC#��C$=qC$p�C$�C%33C%��C%��C&G�C&C'  C'p�C'C(�C(��C(�HC)Q�C)��C*{C*z�C*�RC+=qC+�C+��C,\)C,�RC-=qC-�C.
=C.\)C.�
C/�C/�C0  C0=qC0�RC1  C1p�C1�RC233C2p�C2�C333C3��C3��C4Q�C4C5
=C5z�C5C633C6z�C6��C7(�C7��C7�
C8G�C8p�C8��C9  C9\)C9z�C9�
C9��C:=qC:Q�C:��C:�C:��C;  C;G�C;ffC;�C;��C;�HC<(�C<=qC<�C<��C<�
C={C=(�C=p�C=�C=��C=�HC>  C>Q�C>ffC>�C>C>��C?(�C?G�C?�C?�\C?�HC?�C@=qC@G�C@z�C@�RC@CA{CA�CAffCAp�CA�RCA��CA��CB=qCB=qCB�\CB��CB�HCB�CC33CC\)CCp�CCCC��CD{CD(�CDp�CD�CD�RCD�CE{CE=qCE\)CE��CE�CE��CF
=CFG�CFffCF��CF�CF��CG
=CGQ�CGffCG�\CG�
CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��;A��mA��`A��mA��`A��yA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�  A���A���A���AѼjAѴ9A�7LA��A�t�A�  A�~�A��A�~�A�7LA��`A�&�Aɴ9Aȧ�Aǩ�A�(�A�$�Aƣ�Aŕ�AċDA��;A�t�A�ȴA��A�XA��^A��9A�O�A�1'A��A���A�&�A���A��+A�ƨA��A�A�A��-A��A�/A�XA�7LA�&�A��A�E�A��A��A�&�A��#A�A��9A��A�x�A�ZA�ĜA��FA��yA�ZA��A�ȴA�S�A��A�hsA��^A~~�Ax��AuG�As?}An�RAk�-Ajz�Ah�yAghsAf�Ae7LAd{Ab�9A^��A\ȴA[O�AX-AP�9AM7LAL^5AI�TAHZAFA�AC��AB�jAB��ABZAA�7A@��A?dZA=S�A<��A;�A:  A8�A7�A5��A4-A3VA0�RA0�A/K�A-7LA+?}A(�A'��A&1'A&ffA%��A$�RA!��A!hsA�^At�A��A7LAȴA�A�;A��A�yAbA
=AK�A�9A`BA;dAVA�FA�AdZA�9A �A33A=qAA"�A	��AbNA��A\)A�jA=qA�A  A��A�^A ��A �A E�@��w@�@��T@�33@���@�b@�t�@�l�@�dZ@�S�@�ȴ@�Ĝ@��@�!@�~�@��@�x�@�G�@���@�z�@� �@@�+@���@���@�ȴ@���@���@�-@�X@�@�7@���@�I�@�M�@�G�@���@�K�@���@�ȴ@ް!@���@�G�@�(�@�ƨ@ۅ@ڰ!@�n�@��@ٙ�@أ�@׾w@�;d@և+@�5?@�$�@�@�?}@�&�@�j@���@���@�;d@��H@��@��H@��@��@��@���@ҸR@ҏ\@�M�@�$�@�7L@Ь@У�@Ь@У�@�Z@ϕ�@θR@�E�@��T@��@�r�@�I�@�b@�@�J@��@ɺ^@�x�@�O�@�Ĝ@�z�@�Q�@�I�@� �@ǅ@�o@���@�`B@�7L@�7L@�X@�9X@þw@Å@�+@���@�@�V@��@��^@�x�@�`B@�/@��@���@���@�z�@�9X@��@�\)@�o@���@�J@��^@��h@��@�hs@�G�@�Ĝ@�bN@� �@�  @���@���@��@�;d@�+@���@���@���@�^5@�E�@�5?@�{@�{@�{@��@���@�`B@��@���@�+@���@��@���@��\@��\@��\@��+@�~�@�^5@��@��h@�?}@�V@���@��j@��D@�1@���@���@�S�@���@�ff@�$�@��#@���@��@���@��@���@�t�@�33@��@��\@�^5@�E�@�5?@��@�{@�@��@���@��@�7L@��@��@���@�A�@�9X@�1@�;d@���@���@�^5@�M�@�=q@�@�`B@���@���@�(�@���@���@��R@�v�@�M�@��@�@�X@���@�z�@�Z@��w@�t�@�C�@�
=@���@�V@�{@��T@��7@�X@�&�@��@�r�@�Z@�A�@��@��;@�ƨ@��@�S�@��@��@�ȴ@�^5@��T@�x�@�%@�Z@��@�1@��m@��
@���@��P@�t�@�\)@�
=@���@�{@���@���@��@�hs@�O�@��@���@�Q�@��F@�l�@�33@��@��R@��@���@�hs@�X@�?}@�?}@�7L@�/@���@�z�@�I�@�b@��P@�C�@��@���@���@��T@���@��^@��h@�p�@�G�@��@���@���@���@�Z@�1@��@��P@�t�@�K�@�;d@�o@��H@�ȴ@��!@�^5@�-@�{@���@�X@�7L@��@���@��u@�I�@��;@�+@���@���@�n�@�{@�hs@�V@��`@�Ĝ@��u@�Z@�I�@�A�@�9X@�b@��@���@���@�C�@���@�ff@�E�@�$�@�@���@��@�?}@�?}@�%@�Ĝ@���@��@�z�@�bN@�Z@�I�@�9X@�1'@� �@��
@��
@�\)@�
=@��@���@���@�M�@�=q@��@��@���@�hs@�/@��@�%@��@�Ĝ@��@��@+@~�@~E�@}@}O�@}�@|��@|�D@|Z@z��@y�#@yX@y�@x�9@x �@w|�@w;d@w;d@w+@w�@v�R@u�@uO�@u�@tj@sƨ@s��@sC�@r�H@rJ@q�7@q&�@p��@p��@p�u@p  @o�@o;d@n��@n�@nȴ@n��@n5?@m�@l�/@l��@l�@l(�@kt�@k"�@j��@jJ@i&�@hĜ@hr�@hb@g�w@g��@g�P@g|�@gK�@g
=@f�@f��@fE�@e�@e@e��@eO�@dI�@cC�@b�@a�^@ahs@a&�@a%@`�`@`�u@`r�@`1'@_�w@_�w@_�w@_|�@_
=@^ff@]@]�@]V@\�/@\�@\�@[��@[t�@[S�@[33@Z�H@Z�\@Y�#@Y��@YX@Y%@X��@X1'@Xb@X  @W�;@W�@W
=@V��@Vȴ@V��@U�T@U`B@T�D@S�F@SC�@So@S@R�H@R�H@R��@R�!@RM�@R=q@R-@Q�@Q��@Q��@Q�^@Q��@QG�@PĜ@PQ�@P �@O�;@O�@O�@O�w@O
=@Nȴ@NV@M��@M�h@Mp�@L�@L�@L�D@K��@Ko@J�!@Jn�@J�@I��@I�7@IG�@H�u@H1'@Hb@H  @G�@G��@G�@G��@Gl�@G;d@F�y@F�+@E��@E�h@Ep�@E`B@E/@D��@D(�@D�@D1@Cƨ@C��@CS�@CC�@C"�@C"�@B�@B��@B��@B~�@B�@A��@A��@Ax�@A7L@A�@@��@@r�@@A�@@A�@@  @?��@?l�@>�@>��@>�+@>E�@>@=�-@<��@<j@;�m@;33@:��@:�\@:=q@9��@9��@9hs@9X@97L@9%@8��@8�u@8bN@7�@7|�@7\)@6�R@6��@6ff@6V@65?@6{@6@5�@5�T@5�h@5?}@4�/@4�@4�D@4j@4I�@4(�@41@3�
@3dZ@2�H@2�!@2��@2�\@1��@0�u@0A�@/�;@/|�@/\)@/K�@/+@/+@/�@/+@/�@/�@/�@/�@/�@/�@/
=@.��@.��@.ȴ@.5?@.@-�@-`B@-�@,j@+ƨ@+S�@+@*�@*��@)��@)�#@)��@)�^@)��@)�7@)&�@(�9@(�u@(r�@(Q�@(1'@( �@(b@(  @'��@'�w@'�@'��@'�P@'l�@'K�@';d@&��@&�R@&V@&{@&@%��@%�h@%`B@%?}@$�@$��@#ƨ@"�H@"n�@!��@!�@ �9@ �9@ ��@ �@ r�@ bN@ Q�@  �@ b@�;@�w@��@l�@;d@
=@��@ff@5?@��@�-@�@?}@/@V@�@��@�D@j@j@Z@I�@(�@�@�
@��@�@dZ@��@-@�#@�^@��@��@X@G�@&�@%@��@bN@b@�@|�@\)@+@�@ȴ@v�@�@�j@9X@��@�@@��@��@�\@^5@^5@=q@-@�#@x�@G�@7L@%@��@�`@�`@�`@�`@�`@�`@�`@�`@��@1'@�@�P@K�@+@�@
=@��@��@�@��@v�@5?@{@@�@�T@��@��@�@p�A�ƨA�A���A���A��#A��;A��TA��`A��`A��`A��TA��mA��yA��A��yA��mA��yA��A��A��A��A��yA��yA��yA��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A��A��A��A��A��A��A��A��A��A��A��A���A���A��A��A��A��A��A��A��A��A��A��yA��yA��yA��A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A�  A���A���A���A���A���A�  A�A�A�  A���A���A���A���A���A���A�A�A�A�  A�  A�  A�  A�  A�A�A�%A�%A�A�A�A�  A�  A���A���A���A���A���A���A�A�A�A�A�  A�  A�A��A��yA��TA���A�ƨA�ƨA���A���A���A���A���A���A���A���A���A���AѾwAѾwAѸRAѶFAѴ9AѼjA���A���A���AѴ9AѰ!AѴ9AѶFAѸRAэPA�x�A�O�A�-A� �A��A��A��A�oA�
=A�  A��A��AоwAС�AЍPAЃA�~�A�n�A�`BA�G�A�+A���A�Q�A���AΙ�A�dZA�?}A� �A���AͼjA�S�A�33A�1'A�&�A��A�JA�A��A��yA�ƨA�M�A�(�A˩�AˋDA�v�A�`BA�G�A�=qA�?}A�=qA�?}A�=qA�9XA�/A�&�A��A�VA�A���A��A���Aʉ7A�l�A�E�A�"�A��A�{A�oA�{A��A�JA��A�ĜAɛ�Aɉ7A�`BA�E�A�=qA��A�ƨA�jA�`BA�I�A�"�A�A��`A�ƨAǣ�AǋDA�^5A�1'A�+A�(�A�&�A�&�A�+A�(�A�$�A� �A� �A�"�A�&�A�(�A�&�A� �A�  A��A���AƬAƑhA�bNA��A�A��#AžwAŧ�A�|�A�ffA�7LA�A�ȴAğ�Aĕ�Aĉ7A�dZA�=qA�5?A�+A�A��A���A�AÙ�A�v�A�33A���A�/A�oA�{A�VA�A���A��HA���A�ƨA��^A���A��PA���A���A�|�A�=qA���A�|�A�v�A�ffA�XA�O�A�G�A�G�A�S�A�z�A��A���A���A��A��A�dZA�&�A���A���A��jA��A��A��hA�v�A�v�A�`BA�O�A�G�A�E�A�E�A�=qA�9XA�5?A�33A�-A�-A�1'A�-A�&�A� �A�oA���A���A��9A���A��PA�t�A�O�A�  A���A��/A�1A�t�A�G�A��#A�G�A�C�A�ƨA�$�A���A��#A��PA��A�ffA�Q�A�;dA�{A���A�x�A�ZA�{A��A��A�ZA��HA�XA���A�
=A��yA���A�r�A�hsA�C�A�bA�ƨA��hA�XA�{A���A��A��A��HA�v�A�`BA�5?A���A�ƨA��A�n�A�bNA�{A��/A�hsA�1A��/A��A���A��DA�~�A�A�A�
=A��wA��A�$�A��#A��-A�1A���A�;dA�VA���A���A���A�`BA��A��#A��#A�ȴA���A��A�?}A��/A��DA�bNA�K�A�VA��A�O�A��`A��hA�\)A�oA��;A�z�A�1'A���A�5?A��/A���A��A�t�A�-A���A�n�A�=qA�$�A�1'A�$�A��A��!A���A�ĜA��9A��A��!A��DA��A�~�A��A��A�|�A�z�A�x�A�x�A�r�A�x�A�v�A�x�A�E�A�?}A�I�A�"�A��A�E�A���A��A��wA�7LA��HA�\)A�$�A��A�n�A�G�A�{A��/A��jA�`BA�9XA��TA���A��A���A�M�A���A���A�ffA�C�A� �A���A��A��-A�r�A�/A�  A��wA���A�`BA�;dA�JA�ȴA��A�M�A��A��yA��PA�;dA��A�JA���A��`A��^A��A���A��uA�v�A�\)A�;dA��A�1A��A��A��FA���A��A�n�A�K�A�(�A�oA�A��A���A��wA���A��PA�\)A�%A��/A���A�t�A�  A�+A���A�/A�%A���A��A��mA��/A���A���A���A�ĜA��RA��RA���A��+A�XA� �A��RA�-At�AA~9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                               A���A��;A��mA��`A��mA��`A��yA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�  A���A���A���AѼjAѴ9A�7LA��A�t�A�  A�~�A��A�~�A�7LA��`A�&�Aɴ9Aȧ�Aǩ�A�(�A�$�Aƣ�Aŕ�AċDA��;A�t�A�ȴA��A�XA��^A��9A�O�A�1'A��A���A�&�A���A��+A�ƨA��A�A�A��-A��A�/A�XA�7LA�&�A��A�E�A��A��A�&�A��#A�A��9A��A�x�A�ZA�ĜA��FA��yA�ZA��A�ȴA�S�A��A�hsA��^A~~�Ax��AuG�As?}An�RAk�-Ajz�Ah�yAghsAf�Ae7LAd{Ab�9A^��A\ȴA[O�AX-AP�9AM7LAL^5AI�TAHZAFA�AC��AB�jAB��ABZAA�7A@��A?dZA=S�A<��A;�A:  A8�A7�A5��A4-A3VA0�RA0�A/K�A-7LA+?}A(�A'��A&1'A&ffA%��A$�RA!��A!hsA�^At�A��A7LAȴA�A�;A��A�yAbA
=AK�A�9A`BA;dAVA�FA�AdZA�9A �A33A=qAA"�A	��AbNA��A\)A�jA=qA�A  A��A�^A ��A �A E�@��w@�@��T@�33@���@�b@�t�@�l�@�dZ@�S�@�ȴ@�Ĝ@��@�!@�~�@��@�x�@�G�@���@�z�@� �@@�+@���@���@�ȴ@���@���@�-@�X@�@�7@���@�I�@�M�@�G�@���@�K�@���@�ȴ@ް!@���@�G�@�(�@�ƨ@ۅ@ڰ!@�n�@��@ٙ�@أ�@׾w@�;d@և+@�5?@�$�@�@�?}@�&�@�j@���@���@�;d@��H@��@��H@��@��@��@���@ҸR@ҏ\@�M�@�$�@�7L@Ь@У�@Ь@У�@�Z@ϕ�@θR@�E�@��T@��@�r�@�I�@�b@�@�J@��@ɺ^@�x�@�O�@�Ĝ@�z�@�Q�@�I�@� �@ǅ@�o@���@�`B@�7L@�7L@�X@�9X@þw@Å@�+@���@�@�V@��@��^@�x�@�`B@�/@��@���@���@�z�@�9X@��@�\)@�o@���@�J@��^@��h@��@�hs@�G�@�Ĝ@�bN@� �@�  @���@���@��@�;d@�+@���@���@���@�^5@�E�@�5?@�{@�{@�{@��@���@�`B@��@���@�+@���@��@���@��\@��\@��\@��+@�~�@�^5@��@��h@�?}@�V@���@��j@��D@�1@���@���@�S�@���@�ff@�$�@��#@���@��@���@��@���@�t�@�33@��@��\@�^5@�E�@�5?@��@�{@�@��@���@��@�7L@��@��@���@�A�@�9X@�1@�;d@���@���@�^5@�M�@�=q@�@�`B@���@���@�(�@���@���@��R@�v�@�M�@��@�@�X@���@�z�@�Z@��w@�t�@�C�@�
=@���@�V@�{@��T@��7@�X@�&�@��@�r�@�Z@�A�@��@��;@�ƨ@��@�S�@��@��@�ȴ@�^5@��T@�x�@�%@�Z@��@�1@��m@��
@���@��P@�t�@�\)@�
=@���@�{@���@���@��@�hs@�O�@��@���@�Q�@��F@�l�@�33@��@��R@��@���@�hs@�X@�?}@�?}@�7L@�/@���@�z�@�I�@�b@��P@�C�@��@���@���@��T@���@��^@��h@�p�@�G�@��@���@���@���@�Z@�1@��@��P@�t�@�K�@�;d@�o@��H@�ȴ@��!@�^5@�-@�{@���@�X@�7L@��@���@��u@�I�@��;@�+@���@���@�n�@�{@�hs@�V@��`@�Ĝ@��u@�Z@�I�@�A�@�9X@�b@��@���@���@�C�@���@�ff@�E�@�$�@�@���@��@�?}@�?}@�%@�Ĝ@���@��@�z�@�bN@�Z@�I�@�9X@�1'@� �@��
@��
@�\)@�
=@��@���@���@�M�@�=q@��@��@���@�hs@�/@��@�%@��@�Ĝ@��@��@+@~�@~E�@}@}O�@}�@|��@|�D@|Z@z��@y�#@yX@y�@x�9@x �@w|�@w;d@w;d@w+@w�@v�R@u�@uO�@u�@tj@sƨ@s��@sC�@r�H@rJ@q�7@q&�@p��@p��@p�u@p  @o�@o;d@n��@n�@nȴ@n��@n5?@m�@l�/@l��@l�@l(�@kt�@k"�@j��@jJ@i&�@hĜ@hr�@hb@g�w@g��@g�P@g|�@gK�@g
=@f�@f��@fE�@e�@e@e��@eO�@dI�@cC�@b�@a�^@ahs@a&�@a%@`�`@`�u@`r�@`1'@_�w@_�w@_�w@_|�@_
=@^ff@]@]�@]V@\�/@\�@\�@[��@[t�@[S�@[33@Z�H@Z�\@Y�#@Y��@YX@Y%@X��@X1'@Xb@X  @W�;@W�@W
=@V��@Vȴ@V��@U�T@U`B@T�D@S�F@SC�@So@S@R�H@R�H@R��@R�!@RM�@R=q@R-@Q�@Q��@Q��@Q�^@Q��@QG�@PĜ@PQ�@P �@O�;@O�@O�@O�w@O
=@Nȴ@NV@M��@M�h@Mp�@L�@L�@L�D@K��@Ko@J�!@Jn�@J�@I��@I�7@IG�@H�u@H1'@Hb@H  @G�@G��@G�@G��@Gl�@G;d@F�y@F�+@E��@E�h@Ep�@E`B@E/@D��@D(�@D�@D1@Cƨ@C��@CS�@CC�@C"�@C"�@B�@B��@B��@B~�@B�@A��@A��@Ax�@A7L@A�@@��@@r�@@A�@@A�@@  @?��@?l�@>�@>��@>�+@>E�@>@=�-@<��@<j@;�m@;33@:��@:�\@:=q@9��@9��@9hs@9X@97L@9%@8��@8�u@8bN@7�@7|�@7\)@6�R@6��@6ff@6V@65?@6{@6@5�@5�T@5�h@5?}@4�/@4�@4�D@4j@4I�@4(�@41@3�
@3dZ@2�H@2�!@2��@2�\@1��@0�u@0A�@/�;@/|�@/\)@/K�@/+@/+@/�@/+@/�@/�@/�@/�@/�@/�@/
=@.��@.��@.ȴ@.5?@.@-�@-`B@-�@,j@+ƨ@+S�@+@*�@*��@)��@)�#@)��@)�^@)��@)�7@)&�@(�9@(�u@(r�@(Q�@(1'@( �@(b@(  @'��@'�w@'�@'��@'�P@'l�@'K�@';d@&��@&�R@&V@&{@&@%��@%�h@%`B@%?}@$�@$��@#ƨ@"�H@"n�@!��@!�@ �9@ �9@ ��@ �@ r�@ bN@ Q�@  �@ b@�;@�w@��@l�@;d@
=@��@ff@5?@��@�-@�@?}@/@V@�@��@�D@j@j@Z@I�@(�@�@�
@��@�@dZ@��@-@�#@�^@��@��@X@G�@&�@%@��@bN@b@�@|�@\)@+@�@ȴ@v�@�@�j@9X@��@�@@��@��@�\@^5@^5@=q@-@�#@x�@G�@7L@%@��@�`@�`@�`@�`@�`@�`@�`@�`@��@1'@�@�P@K�@+@�@
=@��@��@�@��@v�@5?@{@@�@�T@��@��@�@p�A�ƨA�A���A���A��#A��;A��TA��`A��`A��`A��TA��mA��yA��A��yA��mA��yA��A��A��A��A��yA��yA��yA��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A��A��A��A��A��A��A��A��A��A��A��A���A���A��A��A��A��A��A��A��A��A��A��yA��yA��yA��A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A�  A���A���A���A���A���A�  A�A�A�  A���A���A���A���A���A���A�A�A�A�  A�  A�  A�  A�  A�A�A�%A�%A�A�A�A�  A�  A���A���A���A���A���A���A�A�A�A�A�  A�  A�A��A��yA��TA���A�ƨA�ƨA���A���A���A���A���A���A���A���A���A���AѾwAѾwAѸRAѶFAѴ9AѼjA���A���A���AѴ9AѰ!AѴ9AѶFAѸRAэPA�x�A�O�A�-A� �A��A��A��A�oA�
=A�  A��A��AоwAС�AЍPAЃA�~�A�n�A�`BA�G�A�+A���A�Q�A���AΙ�A�dZA�?}A� �A���AͼjA�S�A�33A�1'A�&�A��A�JA�A��A��yA�ƨA�M�A�(�A˩�AˋDA�v�A�`BA�G�A�=qA�?}A�=qA�?}A�=qA�9XA�/A�&�A��A�VA�A���A��A���Aʉ7A�l�A�E�A�"�A��A�{A�oA�{A��A�JA��A�ĜAɛ�Aɉ7A�`BA�E�A�=qA��A�ƨA�jA�`BA�I�A�"�A�A��`A�ƨAǣ�AǋDA�^5A�1'A�+A�(�A�&�A�&�A�+A�(�A�$�A� �A� �A�"�A�&�A�(�A�&�A� �A�  A��A���AƬAƑhA�bNA��A�A��#AžwAŧ�A�|�A�ffA�7LA�A�ȴAğ�Aĕ�Aĉ7A�dZA�=qA�5?A�+A�A��A���A�AÙ�A�v�A�33A���A�/A�oA�{A�VA�A���A��HA���A�ƨA��^A���A��PA���A���A�|�A�=qA���A�|�A�v�A�ffA�XA�O�A�G�A�G�A�S�A�z�A��A���A���A��A��A�dZA�&�A���A���A��jA��A��A��hA�v�A�v�A�`BA�O�A�G�A�E�A�E�A�=qA�9XA�5?A�33A�-A�-A�1'A�-A�&�A� �A�oA���A���A��9A���A��PA�t�A�O�A�  A���A��/A�1A�t�A�G�A��#A�G�A�C�A�ƨA�$�A���A��#A��PA��A�ffA�Q�A�;dA�{A���A�x�A�ZA�{A��A��A�ZA��HA�XA���A�
=A��yA���A�r�A�hsA�C�A�bA�ƨA��hA�XA�{A���A��A��A��HA�v�A�`BA�5?A���A�ƨA��A�n�A�bNA�{A��/A�hsA�1A��/A��A���A��DA�~�A�A�A�
=A��wA��A�$�A��#A��-A�1A���A�;dA�VA���A���A���A�`BA��A��#A��#A�ȴA���A��A�?}A��/A��DA�bNA�K�A�VA��A�O�A��`A��hA�\)A�oA��;A�z�A�1'A���A�5?A��/A���A��A�t�A�-A���A�n�A�=qA�$�A�1'A�$�A��A��!A���A�ĜA��9A��A��!A��DA��A�~�A��A��A�|�A�z�A�x�A�x�A�r�A�x�A�v�A�x�A�E�A�?}A�I�A�"�A��A�E�A���A��A��wA�7LA��HA�\)A�$�A��A�n�A�G�A�{A��/A��jA�`BA�9XA��TA���A��A���A�M�A���A���A�ffA�C�A� �A���A��A��-A�r�A�/A�  A��wA���A�`BA�;dA�JA�ȴA��A�M�A��A��yA��PA�;dA��A�JA���A��`A��^A��A���A��uA�v�A�\)A�;dA��A�1A��A��A��FA���A��A�n�A�K�A�(�A�oA�A��A���A��wA���A��PA�\)A�%A��/A���A�t�A�  A�+A���A�/A�%A���A��A��mA��/A���A���A���A�ĜA��RA��RA���A��+A�XA� �A��RA�-At�AA~9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�tB
�B
��B
��B
�B
�B
�B
��B
�tB
��B
��B
�B
�FB
��B
��B
��B
��B
��B
�FB
��B
�B
�B
�B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�LB
�B
��B
�FB
�B
�FB
��B
�zB
�tB
��B
�[B
�-B
�B
ޞB
��BSB
�B
��B
�)B
�B
�cB�B�B�B&BLdBm]B�=B�B��B�qB�B��B)�B($B*eB7�B9XB7�B:�B5?B=�B'�B�B  B�fB��B�
B�QB֡B��B�B��B��B��B~(B�iBsBlWBg�Bc�BZ�BOvB�B
�B
�HB
��B
��B
�B
o B
UgB
,B
�B	�B	�TB	��B	��B	�IB	�SB	��B	�1B	cB	}VB	y	B	t�B	jKB	Z�B	R�B	K�B	1�B	�B	eB	B	�B	B	B		7B	"hB	-wB	2�B	3hB	6B	H�B	K�B	O�B	PB	S&B	Z�B	OB	L�B	S�B	U2B	XyB	WsB	P�B	GB	J�B	MjB	F?B	IRB	L�B	F�B	3�B	/OB	)�B	$�B	&LB	$@B	(XB	*�B	+6B	/�B	4nB	5B	<�B	:�B	5?B	5�B	7LB	A B	8B	>B	@�B	DgB	EB	GB	EB	B�B	B'B	?}B	9$B	?HB	@�B	GzB	C�B	D3B	E�B	:*B	:�B	9$B	6zB	5B	5tB	2-B	.�B	)_B	%FB	&B	%zB	%FB	%B	$tB	%�B	+�B	-CB	,�B	-CB	/B	0UB	0UB	1�B	2�B	33B	49B	4�B	8RB	9�B	?�B	?B	?B	>wB	?HB	DgB	JXB	I�B	L0B	Q�B	S�B	YB	ZQB	Z�B	[�B	[WB	^5B	_pB	c�B	c�B	e,B	h�B	iDB	jB	lWB	o�B	qB	tTB	z�B	}VB	~(B	��B	��B	��B	�YB	�B	��B	�B	�~B	� B	�B	�B	�YB	��B	��B	��B	��B	��B	�=B	��B	�FB	��B	��B	��B	��B	�6B	��B	��B	��B	��B	�B	�tB	��B	��B	��B	��B	��B	��B	�dB	��B	�B	��B	��B	��B	�B	�UB	�-B	��B	��B	�B	�B	��B	�KB	��B	��B	�)B	̘B	�6B	�<B	бB	�B	уB	��B	҉B	҉B	ӏB	�aB	՛B	�gB	רB	רB	�KB	��B	�)B	ܒB	��B	��B	�dB	�BB	�HB	�B	��B	��B	�B	��B	��B	�&B	�B	�,B	��B	�fB	�fB	�B	�B	�sB	��B	�"B	�B	�/B	�5B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�JB	�B	�B	�B	��B	�PB	�]B	�(B	��B	��B	�cB	�cB	��B
 iB
�B
{B
GB
B
B
YB
%B
�B
+B
+B
+B
_B
_B
�B
_B
_B
�B
1B
�B
fB
1B
�B
	lB
�B
�B
1B
	B
	lB

	B
	�B
	�B

�B
�B
~B
B
�B
"B
�B
�B
�B
�B
bB
.B
4B
oB
B
�B
uB
uB
�B
�B
B
MB
�B
SB
�B
SB
�B
�B
�B
�B
YB
YB
$B
YB
�B
7B
�B
kB
�B
qB
=B
qB
B
�B
B
�B
�B
�B
�B
�B
�B
qB
�B
B
B
�B
�B
IB
�B
CB
B
�B
�B
B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
~B
B
�B
IB
IB
�B
B
B
!B
VB
VB
VB
VB
�B
�B
�B
 'B
 'B
 \B
 �B
 �B
!�B
"4B
!�B
"4B
"hB
"�B
#:B
"�B
#nB
#B
$@B
$B
#�B
%FB
%zB
%zB
%zB
%B
&LB
&LB
&�B
(�B
'�B
($B
(�B
)_B
)�B
*0B
*eB
*�B
*�B
+kB
+�B
+�B
+6B
+�B
,=B
+�B
,qB
,�B
.IB
.�B
.}B
.IB
/OB
/�B
0!B
0�B
/�B
0!B
0�B
1'B
1[B
0�B
0�B
0�B
1[B
1�B
1[B
1�B
1�B
1�B
1�B
2aB
2�B
2�B
2�B
2�B
2�B
2�B
3hB
2�B
4nB
4B
4nB
49B
4nB
4�B
4nB
6B
5tB
5�B
6FB
6B
5�B
5tB
6FB
5�B
5�B
7�B
7B
7�B
7LB
7�B
8RB
8B
8�B
8RB
8�B
8RB
8�B
9�B
:^B
9�B
:�B
<B
;0B
;�B
<�B
=�B
=B
=B
=�B
=�B
>B
?B
>�B
?B
?}B
?�B
?�B
@B
?�B
B'B
A�B
AUB
A�B
B�B
C-B
C�B
DgB
E9B
F?B
FB
F�B
F�B
GzB
GB
GB
GEB
G�B
G�B
HB
HKB
HKB
HB
G�B
G�B
H�B
IRB
JXB
K^B
J�B
J�B
K)B
K�B
K�B
K^B
K�B
L�B
L�B
L�B
LdB
L�B
MB
M�B
MjB
L�B
LdB
L�B
M6B
M6B
L0B
K�B
K�B
K�B
LdB
MB
MB
M6B
MjB
M�B
NpB
N�B
N�B
NpB
N<B
N�B
O�B
P�B
QNB
Q�B
S�B
S[B
T�B
T�B
TaB
TaB
T�B
T�B
TaB
T,B
T�B
T,B
TaB
S�B
T,B
TaB
T�B
T�B
U�B
V9B
VmB
V�B
W
B
W
B
VmB
VB
V�B
W�B
W�B
XB
W�B
W�B
XB
YKB
X�B
XB
Y�B
ZQB
ZQB
ZB
ZB
ZQB
Z�B
Z�B
[�B
\)B
\]B
\)B
[�B
[�B
\�B
[�B
[�B
[�B
[�B
\)B
[�B
\]B
\�B
\]B
\�B
]/B
]�B
]�B
]�B
^�B
_;B
_;B
_B
_�B
_;B
_pB
_�B
_;B
`BB
_�B
`BB
`�B
`�B
`vB
`�B
`�B
aHB
aB
`�B
a�B
bB
bNB
b�B
b�B
b�B
cTB
c B
b�B
c�B
c�B
c�B
d�B
e,B
e�B
e�B
f2B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g8B
g�B
h
B
h
B
g�B
h
B
h>B
h>B
h�B
iB
iB
h�B
h�B
iDB
i�B
iyB
iyB
jB
jKB
jB
jB
jB
j�B
j�B
j�B
j�B
kB
l"B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
n/B
n/B
m�B
m�B
m�B
n/B
m�B
n/B
m�B
n/B
n�B
m�B
n/B
n�B
n�B
oiB
n�B
oiB
pB
poB
p�B
qvB
qvB
qvB
rB
q�B
q�B
q�B
rB
q�B
r|B
r�B
r|B
sB
r�B
sMB
s�B
sB
r�B
sB
s�B
sB
s�B
sB
s�B
s�B
s�B
tB
s�B
t�B
t�B
tTB
t�B
t�B
t�B
t�B
t�B
t�B
v�B
u�B
v+B
v�B
w�B
x�B
xB
x8B
x�B
xB
xlB
xB
x�B
x8B
x�B
x8B
y	B
x�B
x�B
yrB
yrB
x�B
yrB
zDB
zB
zB
zDB
zB
y�B
zDB
zxB
z�B
{B
{B
z�B
zxB
{B
z�B
{JB
z�B
{JB
z�B
|PB
{�B
|�B
|PB
{�B
|B
|�B
|B
|�B
{�B
|�B
|�B
}"B
}VB
}�B
}�B
}�B
~(B
}�B
}�B
~�B
�4B
�B
� B
��B
�oB
�;B
�oB
�oB
�B
��B
��B
�B
�uB
��B
��B
��B
�GB
�{B
�GB
�{B
�{B
�GB
�GB
�GB
�GB
�B
��B
��B
��B
��B
��B
�B
�SB
�SB
�SB
�SB
��B
��B
��B
�%B
��B
�YB
��B
�YB
��B
�YB
��B
��B
��B
�nB
�?B
�nB
�zB
��B
��B
�B
�FB
��B
�?B
�tB
��B
��B
�tB
�B
�nB
�B
��B
�?B
��B
�B
��B
�FB
�FB
��B
��B
�B
��B
�B
�B
�B
��B
��B
��B
��B
�B
�FB
�LB
�FB
��B
�LB
�FB
��B
�B
�B
�B
�B
�B
�zB
�B
��B
�B
�LB
��B
�LB
�LB
��B
�zB
��B
�FB
�zB
��B
�zB
�B
��B
�zB
�B
��B
�B
��B
�LB
��B
�LB
��B
��B
�FB
�tB
�B
�?B
�zB
�B
�zB
�B
�B
�zB
��B
��B
��B
��B
��B
�zB
�B
��B
�B
�zB
��B
��B
�B
��B
��B
�zB
��B
�zB
��B
�FB
�LB
�LB
��B
�LB
�LB
�LB
��B
�LB
�B
��B
��B
��B
�B
��B
��B
�LB
�B
�B
�FB
�zB
�zB
��B
�B
��B
��B
��B
��B
�B
�zB
��B
�zB
�FB
�zB
��B
��B
�B
�LB
�B
�LB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�zB
��B
�FB
�B
�FB
�FB
�zB
�zB
�zB
�zB
�FB
�LB
��B
�LB
��B
�LB
�B
��B
�LB
�B
��B
��B
�?B
�zB
��B
��B
�B
��B
�LB
��B
�B
�FB
��B
��B
��B
��B
��B
��B
��B
�zB
�zB
�B
�LB
��B
��B
��B
��B
��B
�zB
�B
�B
�FB
�B
�FB
�zB
�LB
�LB
�B
�nB
�zB
�B
�?B
��B
��B
�tB
�B
��B
�FB
�UB
�B
�tB
�$B
�tB
�zB
�B
��B
�9B
��B
�LB
�B
��B
��B
�B
��B
�tB
�*B
��B
��B
�FB
��B
��B
�nB
�B
�?B
��B
��B
��B
�hB
�B
��B
�tB
��B
�[B
��B
�!B
��B
��B
��B
��B
��B
�nB
��B
��B
�IB
�B
�B
��B
�B
��B
��B
��B
��B
��B
�jB
�
B
�B
�B
�iB
��B
��B_B �B
��BABB�B�BfB�BuBYB
�B
ٴB
�
B
�WB
��B
�dB
�B
�HB
�B
�B
�B
�TB
�2B
�mB
�yB
��B
�yB
�B
�B
�;B
�fB
��B
�B
��B
�|B
��B
�B
�GB
�vB
��B
�B
��B
��B
��BxB�BB�BCB�B \B!�BBBIB�B�BB!�B!BB�BVB�B�BOB�B"4B!�B#:B$tB($B,=B1'BB�BDgBLdBJ#BIRBO�B_pBa|BiBk�Bm)Bn�Bo�Bu�Bv+B�AB�B��B��B�B��B�B��B��B��B�=B��B��B��B�nB��B��B��B��B�$B�^B��B�B�B�6B�wB��B��B��B�B�)B��B�sBخB�?B�QBیB�QB�B��B�B��BB!bB,qB7B;�B6�B9$B'�B)�B(�B'�B%�B&�B&LB%zB'�B&�B(XB)_B,�B1'B4nB6zB7B9$B9$B7�B8RB9XB8�B7B8�B<�B9�B9XB5?B6�B4�B1�B7LB6zBEB8B-BJ#B;0BN�B-�B=<BA B$�B$@B1�B?B;dBE9BEmBHB9�B5�B4�B0�B*eB)*B1�B&�B\B=qB!bBMB iB�"BBB�B��BB  B �BB�(B�B��B�/B�B�vB�B�B��B��B�B�yB��B�
B� B�TB��BٴB�BٴB��BخB�5B��B�9B�B�dB�B��B�BB��B�^B�B�^B��B��B��B�RB�zB��B��B��B��B�bB��B�IB�hB��B�CB��B�B�YB�4B��B��B��By�Bo�BwfBsMBtTB��B��B~�B�{BtTBv�BncBv�Be`BffBkQBk�Bk�BhsBh
BkQBhsBffBffBf�Bd�Bd�Bb�Bc�B_;B_B\�BiDB^5BF�BNpBB6zB&�B?HBeB*�B�B�B
�DBB
��B
�B
��B
��B
�,B
�yB
�B
��B
�BB
�B
�^B
��B
�B
��B
�gB
�B
��B
��B
��B
��B
��B
�nB
��B
��B
�FB
�B
�-B
�hB
�LB
��B
��B
�eB
��B
�qB
��B
��B
��B
��B
�+B
��B
��B
��B
cB
�uB
� B
� B
~(B
{JB
|B
{B
z�B
x8B
t�B
t�B
rGB
rB
n�B
m�B
j�B
jKB
gB
e�B
bNB
d�B
f�B
X�B
Z�B
UgB
W�B
iDB
a|B
A�B
1�B
33B
0�B
/�B
.�B
.B
+B
,qB
,B
*0B
'�B
+�B
&�B
*�B
)�B
,B
$�B
oB
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                               B
��B
��B
�B
��B
��B
�B
�B
�B
��B
��B
��B
��B
�B
�RB
��B
��B
��B
��B
��B
�RB
��B
�#B
�#B
�#B
��B
��B
��B
��B
��B
�#B
��B
��B
��B
�XB
�#B
��B
�RB
�B
�RB
��B
��B
��B
��B
�gB
�9B
�#B
֪B
��B
�_B
�B
��B
�5B
�+B
�oB�B�B�B$BDpBeiB�IB�*B��B�}BضB��B!�B 0B"qB/�B1dB/�B2�B-KB5�B�B�B�B�rB��B�B�]BέB��B�)B��B��B��Bv4BxuBk%BdcB_�B[�BR�BG�B�B
�B
�TB
��B
��B
w�B
gB
MsB
$B
	�B	�"B	�`B	��B	��B	�UB	�_B	��B	�=B	woB	ubB	qB	l�B	bWB	R�B	J�B	C�B	*B	�B	qB	B	�B	B�+B	CB	tB	%�B	*�B	+tB	.B	@�B	C�B	G�B	H B	K2B	R�B	GB	D�B	K�B	M>B	P�B	OB	H�B	?B	CB	EvB	>KB	A^B	D�B	>�B	+�B	'[B	!�B	�B	XB	LB	 dB	"�B	#BB	'�B	,zB	-B	4�B	2�B	-KB	-�B	/XB	9,B	0)B	6B	8�B	<sB	=B	?B	=B	:�B	:3B	7�B	10B	7TB	8�B	?�B	<
B	<?B	=�B	26B	2�B	10B	.�B	-B	-�B	*9B	&�B	!kB	RB	$B	�B	RB	B	�B	�B	#�B	%OB	$�B	%OB	''B	(aB	(aB	)�B	+B	+?B	,EB	,�B	0^B	1�B	7�B	7 B	7 B	6�B	7TB	<sB	BdB	A�B	D<B	I�B	K�B	Q#B	R]B	R�B	S�B	ScB	VAB	W|B	[�B	[�B	]8B	`�B	aPB	b"B	dcB	g�B	iB	l`B	r�B	ubB	v4B	z�B	{�B	|�B	~eB	{B	{�B	�B	��B	�B	�B	�*B	�eB	��B	��B	��B	��B	��B	�IB	��B	�RB	��B	��B	�B	�B	�BB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�pB	��B	�B	��B	��B	��B	� B	�aB	�9B	��B	��B	�B	�B	��B	�WB	��B	��B	�5B	ĤB	�BB	�HB	ȽB	�&B	ɏB	��B	ʕB	ʕB	˛B	�mB	ͧB	�sB	ϴB	ϴB	�WB	��B	�5B	ԞB	��B	�B	�pB	�NB	�TB	�B	��B	��B	�B	��B	��B	�2B	ܛB	�8B	��B	�rB	�rB	�B	߭B	�B	��B	�.B	�B	�;B	�AB	��B	�%B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�VB	�B	�"B	�B	��B	�\B	�iB	�4B	��B	��B	�oB	�oB	��B	�uB	��B	��B	�SB	�%B	�+B	�eB	�1B	��B	�7B	�7B	�7B	�kB	�kB	��B	�kB	�kB	��B
 =B
 �B
 rB
 =B
 �B
xB
 �B
 �B
 =B
B
xB
B
�B
�B
�B
�B
�B
'B
�B
.B
�B
�B
�B
�B
nB
:B
	@B

{B

B
	�B
�B
�B
�B
�B
B
YB
�B
_B
�B
_B
�B
�B
�B
�B
eB
eB
0B
eB
�B
CB
�B
wB
�B
}B
IB
}B
B
�B
B
�B
�B
�B
�B
�B
�B
}B
�B
B
!B
�B
�B
UB
�B
OB
!B
�B
�B
'B
�B
�B
�B
�B
�B
'B
'B
�B
�B
�B
�B
�B
'B
�B
UB
UB
�B
'B
'B
-B
bB
bB
bB
bB
�B
�B
�B
3B
3B
hB
�B
B
�B
@B
B
@B
tB
�B
FB
�B
zB
B
LB
B
�B
RB
�B
�B
�B
B
XB
XB
�B
 �B
�B
 0B
 �B
!kB
!�B
"<B
"qB
"�B
"�B
#wB
#�B
#�B
#BB
#�B
$IB
#�B
$}B
$�B
&UB
&�B
&�B
&UB
'[B
'�B
(-B
(�B
'�B
(-B
(�B
)3B
)gB
(�B
(�B
(�B
)gB
)�B
)gB
)�B
)�B
*B
)�B
*mB
*�B
+B
*�B
+B
*�B
*�B
+tB
*�B
,zB
,B
,zB
,EB
,zB
,�B
,zB
.B
-�B
-�B
.RB
.B
-�B
-�B
.RB
-�B
-�B
/�B
/#B
/�B
/XB
/�B
0^B
0)B
0�B
0^B
0�B
0^B
0�B
1�B
2jB
1�B
3B
4B
3<B
3�B
4�B
5�B
5B
5B
5�B
5�B
6B
7 B
6�B
7 B
7�B
7�B
7�B
8&B
7�B
:3B
9�B
9aB
9�B
:�B
;9B
<
B
<sB
=EB
>KB
>B
>�B
>�B
?�B
?B
?B
?QB
?�B
?�B
@#B
@WB
@WB
@#B
?�B
?�B
@�B
A^B
BdB
CjB
B�B
CB
C5B
C�B
C�B
CjB
C�B
D�B
D�B
D�B
DpB
D�B
EB
E�B
EvB
D�B
DpB
D�B
EBB
EBB
D<B
DB
DB
DB
DpB
EB
EB
EBB
EvB
E�B
F|B
F�B
F�B
F|B
FHB
F�B
G�B
H�B
IZB
I�B
K�B
KgB
L�B
L�B
LmB
LmB
M
B
L�B
LmB
L8B
L�B
L8B
LmB
LB
L8B
LmB
L�B
M
B
M�B
NEB
NyB
N�B
OB
OB
NyB
NB
N�B
O�B
O�B
PB
O�B
O�B
PB
QWB
P�B
PB
Q�B
R]B
R]B
R)B
R)B
R]B
R�B
R�B
TB
T5B
TiB
T5B
S�B
TB
T�B
TB
S�B
S�B
TB
T5B
TB
TiB
T�B
TiB
T�B
U;B
U�B
U�B
U�B
V�B
WGB
WGB
WB
W�B
WGB
W|B
W�B
WGB
XNB
W�B
XNB
X�B
X�B
X�B
X�B
X�B
YTB
YB
X�B
Y�B
Z%B
ZZB
Z�B
Z�B
Z�B
[`B
[,B
Z�B
[�B
[�B
[�B
]B
]8B
]�B
^
B
^>B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_DB
_�B
`B
`B
_�B
`B
`JB
`JB
`�B
aB
aB
`�B
`�B
aPB
a�B
a�B
a�B
b"B
bWB
b�B
b"B
b"B
b�B
b�B
b�B
b�B
c(B
d.B
e B
d�B
eiB
e�B
e�B
e�B
fB
f;B
f;B
e�B
fB
fB
f;B
fB
f;B
e�B
f;B
f�B
e�B
f;B
f�B
f�B
guB
f�B
guB
hB
h{B
h�B
i�B
i�B
i�B
jB
i�B
i�B
i�B
jB
i�B
j�B
j�B
j�B
k%B
j�B
kYB
k�B
k%B
j�B
k%B
k�B
k%B
k�B
k%B
k�B
k�B
k�B
l+B
k�B
l�B
l�B
l`B
l�B
l�B
l�B
l�B
l�B
l�B
n�B
nB
n7B
o	B
o�B
p�B
pB
pDB
p�B
pB
pxB
pB
p�B
pDB
p�B
pDB
qB
p�B
p�B
q~B
q~B
p�B
q~B
rPB
rB
rB
rPB
rB
q�B
rPB
r�B
r�B
s"B
s"B
r�B
r�B
s"B
r�B
sVB
r�B
sVB
r�B
t\B
s�B
t�B
t\B
s�B
t(B
t�B
t(B
t�B
s�B
t�B
t�B
u.B
ubB
u�B
u�B
u�B
v4B
u�B
u�B
v�B
x@B
w�B
xB
x�B
y{B
yGB
y{B
y{B
zB
y�B
y�B
zB
z�B
z�B
z�B
z�B
{SB
{�B
{SB
{�B
{�B
{SB
{SB
{SB
{SB
{B
{�B
{�B
|�B
|�B
|�B
}+B
}_B
}_B
}_B
}_B
}�B
}�B
}�B
~1B
~�B
~eB
~�B
~eB
B
~eB
�B
�B
��B
�zB
�KB
�zB
��B
��B
��B
�B
�RB
��B
�KB
��B
��B
��B
��B
�B
�zB
�B
��B
�KB
��B
�B
��B
�RB
�RB
��B
��B
�B
��B
�#B
�#B
�#B
��B
��B
��B
��B
�B
�RB
�XB
�RB
��B
�XB
�RB
��B
�B
�B
�B
�B
�B
��B
�#B
��B
�#B
�XB
��B
�XB
�XB
��B
��B
��B
�RB
��B
��B
��B
�B
��B
��B
�#B
��B
�#B
��B
�XB
��B
�XB
��B
��B
�RB
��B
�B
�KB
��B
�B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�#B
��B
��B
��B
�#B
��B
��B
��B
��B
��B
��B
�RB
�XB
�XB
��B
�XB
�XB
�XB
��B
�XB
�)B
��B
��B
��B
�)B
��B
��B
�XB
�#B
�#B
�RB
��B
��B
��B
�B
��B
��B
��B
��B
�B
��B
��B
��B
�RB
��B
��B
��B
�#B
�XB
�#B
�XB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�RB
�B
�RB
�RB
��B
��B
��B
��B
�RB
�XB
��B
�XB
��B
�XB
�)B
��B
�XB
�#B
��B
��B
�KB
��B
��B
��B
�)B
��B
�XB
��B
�B
�RB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�XB
��B
��B
��B
��B
��B
��B
�B
�B
�RB
�B
�RB
��B
�XB
�XB
�B
�zB
��B
�B
�KB
��B
��B
��B
�B
��B
�RB
�aB
�B
��B
�0B
��B
��B
�B
��B
�EB
��B
�XB
�#B
��B
��B
�B
��B
��B
�6B
��B
��B
�RB
��B
��B
�zB
�B
�KB
��B
��B
��B
�tB
�B
��B
��B
��B
�gB
��B
�-B
��B
��B
��B
��B
��B
�zB
�B
��B
�UB
�B
�B
��B
�B
�B
��B
��B
��B
��B
�vB
�B
�B
�B
�uB
��B
�B
�kB
��B
��B
�MB
�+B
��B
��B rB
��B
��BeB
��B
��B
�B
�cB
��B
�pB
ضB
�TB
ڎB
��B
��B
�`B
�>B
�yB
�B
��B
�B
�B
�B
�GB
�rB
��B
�B
�B
�B
��B
��B
�SB
�B
��B
�(B
��B
��B
��B�B �B
�+B�BOB�BhB�B'B!BUB�B�B*B�B-B'B�BbB�B�B[B�B@B�BFB�B 0B$IB)3B;B<sBDpBB/BA^BG�BW|BY�BaBc�Be5Bf�Bg�Bm�Bn7BzMB|%B~�B��B�$B��B�B��B��B��B�IB��B��B��B�zB�B��B��B��B�0B�jB��B�B�B�BB��B��B��B��B�B�5B��B�BкB�KB�]BӘB�]B�)B��B�B�
B!BnB$}B/#B3�B.�B10B�B"B �B�B�B�BXB�B�B�B dB!kB$�B)3B,zB.�B/#B10B10B/�B0^B1dB0�B/#B0�B4�B1�B1dB-KB.�B,�B)�B/XB.�B=B0)B%BB/B3<BF�B%�B5HB9,B�BLB*B7 B3pB=EB=yB@#B1�B-�B,�B(�B"qB!6B)�B�BhB5}BnB�YB�uB�.B�B�B��B��B�B�B��B�B�4B�B��B�;B�B�B�B�B��B�
B�B�B��B�B�,B�`B�B��B�)B��B��BкB�AB��B�EB�B�pB�B��B�NB��B�jB�#B�jB��B��B��B�^B��B��B��B��B��B�nB�B�UB�tB��B�OB|�B�B~eBx@B��B��B��Bq�Bg�BorBkYBl`B��B��Bv�B{�Bl`Bn�BfoBo	B]lB^rBc]Bc�Bc�B`B`Bc]B`B^rB^rB^�B\�B]BZ�B[�BWGBWBT�BaPBVAB>�BF|B�B.�B�B7TBqB"�B�B�B
�PB
�B
� B
��B
�B
��B
�8B
�B
��B
��B
�NB
ضB
�jB
��B
�)B
��B
�sB
�B
�B
��B
��B
��B
��B
�zB
��B
�B
�RB
�$B
�9B
�tB
�XB
��B
�B
�qB
��B
�}B
�B
��B
B
~�B
7B
�B
y�B
x�B
woB
z�B
xB
xB
v4B
sVB
t(B
s"B
r�B
pDB
l�B
l�B
jSB
jB
f�B
fB
b�B
bWB
_B
]�B
ZZB
]B
^�B
P�B
R�B
MsB
O�B
aPB
Y�B
9�B
)�B
+?B
(�B
'�B
&�B
& B
#B
$}B
$B
"<B
�B
#�B
�B
"�B
"B
$B
�B

{B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230521153044                            20230521153044AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023052115304420230521153044  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023052115304420230521153044QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023052115304420230521153044QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               