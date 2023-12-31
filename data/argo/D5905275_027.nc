CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-10-09T04:41:33Z creation; 2023-04-26T19:14:26Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181009044133  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_027                 7316_008644_027                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @؇�	���@؇�	���11  @؇�9Xb@؇�9Xb@+�py�,@+�py�,�d v�׈�d v�׈11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@E�@�  @�  @��R@޸R@��RA\)A   A+�A@��A`��A�Q�A�  A��A��A�Q�A�  A�  A�  B   B�
B�
B  B (�B(  B0  B8  B?�
BH  BP  BW�
B_�
Bg�Bo�
Bx  B�Q�B�  B�  B��B��B�  B�{B�{B�{B��B��B�  B�  B�  B��B��B�  B�{B��B��B�  B�  B�  B�  B�  B�  B�  B�{B�(�B�{B��B�  C   C  C  C
=C
=C

=C  C�C  C
=C��C��C  C  C
=C
=C   C"  C$  C&{C(
=C*  C,  C-��C0
=C2
=C4(�C5�C7�C9��C;��C>  C@
=CB  CD  CE��CG��CJ
=CL
=CN  CO��CQ��CT
=CV  CW��CY��C\  C^  C`  Cb  Cd
=Cf
=Ch  Cj  Cl  Cm��Co��Cq��Ct
=Cv  Cw��Cy��C|
=C~
=C�  C���C���C�  C���C�  C�  C���C�  C�  C�C�C�C���C���C�  C�  C���C���C���C�  C�  C���C���C���C�  C�C���C���C���C���C�C�C�  C���C�  C�  C�C�C���C���C�  C�  C�  C�  C�C�  C���C�  C�C�C�  C�  C�C�  C�C�C�C�C�  C���C���C�  C�C�C�  C���C�C�
=C�C�  C�  C�  C�  C�C�  C�  C�C�  C�  C���C�  C�C�C�  C�  C�C�C�C���C���C���C�  C���C���C���C���C�  C�C���C���C���C�  C�C�C�C�
=C�C�  C�  C�C���C�C�
=C�  C���C�  C�  C�  C�  C�  C���C���C���C���C�  C���C���C���D ��D  D}qD  D��D  D}qD�qD��D�D}qD�qD}qD�qD}qD  D� D	  D	� D
  D
��D�D��D  Dz�D�qD� D  D��D  D� D  D}qD  D��D  D}qD  D� D  D� D  D� D  D� D�D}qD  D��D  D� D�qD}qD�qDz�D��D}qD�qD}qD  D��D  D}qD �D ��D!  D!� D"  D"z�D"�qD#� D$  D$��D%D%�D&�D&�D'�D'}qD'�qD(��D(�qD)}qD*  D*� D+  D+� D,  D,� D-  D-� D-�qD.}qD/�D/}qD/��D0}qD1  D1� D1�qD2}qD2�qD3}qD4  D4� D5  D5� D6�D6}qD7  D7��D7�qD8��D9�D9� D:  D:� D;  D;}qD;��D<}qD<��D=z�D=�qD>��D?�D?��D@�D@��DA�DA��DB�DB}qDC  DC� DD  DD��DE�DE��DF�DF��DF�qDG� DH�DH}qDI  DI� DJ�DJ�DK  DK� DL  DL}qDL�qDM� DM��DNz�DN�qDO� DP  DP� DQ  DQ� DR  DR}qDR�qDS��DTDT��DU  DU� DV�DV� DW  DW}qDX  DX��DY�DY� DZ  DZ� D[�D[��D[�qD\� D]  D]� D^�D^��D^�qD_}qD_�qD`� Da  Da��Db  Db� Dc  Dc� Dd�Dd� De  De��Df�Df� Dg  Dg}qDg�qDh}qDi  Di��Dj�Dj��Dk�Dk� Dl  Dl� Dm  Dm� Dn  Dn��Do�Do��Dp�Dp�Dq�Dq��Dr�Dr��Dr�qDs� Dt  Dt}qDu  Du� Du�qDv}qDw  Dw� Dx�Dx� Dy  Dy� Dy�qDz}qDz�qD{� D|�D|��D}�D}��D~�D~� D  D��D�HD�AHD�� D���D�HD�AHD�� D�� D�HD�@ D�~�D�� D�  D�@ D��HD��HD�  D�AHD��HD�� D�  D�@ D��HD��HD�  D�@ D�~�D���D���D�>�D�� D���D���D�@ D�� D��HD���D�@ D�� D���D���D�AHD�� D�� D�  D�AHD�� D��HD��D�AHD��HD��HD�HD�>�D�}qD���D�  D�AHD�~�D�� D�  D�@ D��HD�D�  D�>�D�~�D�� D�  D�>�D�}qD��qD���D�@ D��HD�� D�  D�AHD��HD��HD�  D�@ D��HD�D�HD�>�D�}qD���D���D�@ D��HD�� D�  D�>�D�}qD���D�HD�AHD��HD��HD�HD�>�D�}qD��qD���D�@ D�� D�� D�  D�AHD��HD�� D���D�@ D��HD��HD�HD�>�D�� D��HD���D�=qD�~�D�� D���D�>�D��HD��HD�  D�@ D�~�D��HD�HD�AHD��HD���D���D�@ D�|)D��qD�HD�@ D��HD��HD�HD�@ D�~�D�� D���D�@ D��HD���D�  D�AHD�� D���D�HD�B�D��HD��HD�HD�AHD��HD�D�HD�@ D�� D�� D�  D�@ D�� D��HD��D�AHD�� D��HD�  D�>�D�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D���D�  D�@ D�� D�D�  D�@ D�� D���D���D�>�D�� D�� D�HD�AHD��HD��HD�HD�@ D�� D��HD�HD�@ D�� D���D���D�AHD�� D�� D�  D�@ D�� D�� D���D�>�D�� D�D�  D�@ D�� D�� D�  D�@ D�� D��HD��D�AHD�� D�� D���D�>�D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD��HD��D�@ D�� D��HD���D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D D¾�D���D�@ DÁHD�D�HD�@ DāHD��HD�  D�@ D�~�DŽqD���D�@ DƁHD�� D���D�@ DǁHD��HD��D�AHD�~�D�� D�  D�>�D�~�DɽqD���D�@ Dʂ�D�� D�  D�@ DˁHD��HD���D�=qD̀ D��HD�  D�@ D�~�D;�D�  D�AHD΀ D�� D�  D�AHDπ DϾ�D��qD�>�DЁHD��HD���D�@ Dр D�� D�  D�>�D�~�D�� D�HD�@ DӀ D��HD�HD�>�D�~�D��HD�HD�@ DՀ D�� D�  D�@ D�~�D־�D���D�@ D׀ D�� D�  D�@ D�~�D�� D���D�AHDق�D�D�  D�AHDځHD�� D���D�>�D�~�D�� D���D�@ D܁HD�� D���D�>�D�~�D�� D�  D�>�Dހ D��HD�HD�@ D�~�D߾�D�HD�AHD�� DྸD�  D�AHD�HD�� D�  D�@ D� D⾸D�  D�B�D�HD�D�  D�>�D� D�� D��D�B�D�~�D�� D�  D�>�D�~�D澸D���D�=qD� D羸D���D�@ D� D辸D�HD�AHD� D�� D�HD�@ D�~�D�� D�  D�>�D�~�D��HD�  D�>�D� D��HD���D�@ D�~�D�� D�  D�AHD� D��HD�  D�AHD� D�� D�  D�@ D�� D��HD�  D�@ D�HD�� D�  D�AHD�HD��HD�  D�>�D�~�D�D�  D�B�D� D���D���D�@ D�~�D���D�HD�@ D�� D�� D�  D�AHD��HD�D�  D�@ D�� D��HD���D�=qD�~�D��HD�  D�>�D�o\D��3?#�
?8Q�?u?�z�?�33?�
=?�@�@
=@�R@333@B�\@O\)@^�R@k�@z�H@�ff@�{@�@��R@��@���@�
=@�p�@�ff@У�@�
=@�  @�=q@��@���A�AA	��A�RA33AffA(�A   A#�
A(��A,��A0��A5A9��A>{AC33AFffAK�AO\)AS�
AX��A\(�A`��Ae�Ah��Amp�Aq�Au�Az�HA~{A���A��A�p�A�\)A���A��
A�p�A�  A��A��
A�ffA�Q�A��A�z�A�ffA�Q�A��HA���A��RA�G�A�33A��A�\)A���A�33A�p�A��A���A��A�{A��A��A�(�A�AǮA�=qA��
A�A�Q�A��A�(�A�ffA�  A�=qA�z�A�ffA�Q�A�\A���A�RA���A�33A���A�RA�G�A��HA�p�A��A���A�33A�p�A�\)B ��BB�HB�B��B�B�RB�
B��B	�B
�HB(�B��B�B33B  B�B=qB33B  BG�B=qB
=BQ�Bp�B=qB\)Bz�B�BffB\)B Q�B!p�B"�\B#33B$��B%��B&ffB'�B(��B)p�B*�RB+�B,z�B-��B.�HB/�B0��B1�B2�\B3�B4��B5B6�\B7�B8��B9B:�RB;�
B<��B=B>�HB@  BAG�BBffBC\)BD��BE�BF�HBH(�BIp�BJ�RBK�BL��BN=qBO�BP��BQBS
=BT(�BUG�BV�RBX  BX��BZffB[�B\��B]�B_\)B`z�Ba��Bb�RBdQ�Bep�BfffBg�Bi�Bj=qBk33Blz�BmBn�RBo�Bp��Br=qBs33Bt(�Bup�Bv�\Bw�Bxz�By��Bz�RB{�B|z�B}p�B~=qB~�RB\)B�
B�=qB�z�B��\B��RB���B��B�G�B�\)B��B��
B��B�{B�=qB��\B���B���B���B�G�B�\)B��B��B��B�(�B�=qB�ffB���B��HB���B��B�p�B���B�B��B�{B�Q�B�z�B���B���B�
=B�33B�p�B��B��B�  B�(�B�=qB�ffB���B��HB��HB�33B�p�B���B��B��B�(�B�Q�B�ffB���B��HB��B�33B�p�B��B��B�{B�=qB�ffB��RB��HB�
=B�33B��B��B��
B�{B�ffB���B��RB���B�33B��B�B��B�(�B�z�B���B���B��B��B��B�(�B�Q�B���B�
=B�\)B��B��
B�(�B��\B��HB��B�p�B��
B�(�B��\B���B��B�p�B��
B�(�B�ffB��RB��B��B��
B�{B�ffB���B�33B���B��
B�(�B��\B���B�\)B��B�  B�Q�B���B�33B���B��B�Q�B��RB��B���B�  B�Q�B��RB��B��B�{B��\B��HB�G�B��B�(�B���B��B���B�  B�ffB���B�33B��B�(�B��\B���B�G�B��B�(�B��RB��B�p�B��
B�=qB���B�33B��B�  B�ffB���B�33B�B�=qB���B���B�\)B�B�=qB��RB��B��B��
B�Q�B��RB�33B��B�(�B��\B�
=B��B�  B�z�B�
=B���B�(�B��\B�
=B���B�  B��\B��B��B�(�B���B��B���B�{B£�B��BîB�=qBĸRB�33Bř�B�{BƸRB�33B�B�=qBȸRB�33BɮB�(�Bʣ�B�33BˮB�=qB̸RB�33BͮB�{BΏ\B��BϮB�=qBиRB�33Bљ�B�{Bң�B��BӮB�=qB���B�\)B��B�ffB���B�p�B��B�z�B�
=BمB�  Bڣ�B�33B�B�Q�B��HB�p�B��B�ffB��HB�p�B��B�z�B�
=B�B�{B�RB�33B��
B�Q�B��HB�p�B��B�z�B���B�B�  B�z�B�
=B�B�(�B���B�\)B��
B�ffB��HB�\)B��B�ffB�
=BB�=qB�RB�G�B��
B�Q�B���B�G�B��
B�ffB���B��B�{B���B�33B�B�=qB���B�G�B��
B�Q�B���B�\)B��B�z�B���B��B�{B���B��B��C {C \)C ��C �
C{CQ�C�\C��C
=CG�C�\C��C
=CQ�C�\C�
C{C\)C��C�
C�C\)C��C�HC(�CffC��C�HC(�CffC��C�C(�Cp�C�C�C	33C	z�C	�RC	��C
=qC
z�C
C  CG�C�C��C
=CG�C�\C��C
=CQ�C�\C�
C{C\)C��C�HC�CffC��C�C(�Cp�C�C��C33Cz�CC
=CQ�C�\C�
C{C\)C��C�HC�CffC��C�C�CffC�C��C33Cz�C�RC  CG�C�\C�
C�CffC��C�C33Cz�C�RC  CQ�C��C�HC33Cz�CC
=CQ�C��C�C(�Cp�C�RC  C=qC�CC
=CG�C�\C�
C �C ffC �C!  C!G�C!�\C!�
C"(�C"p�C"�RC#  C#G�C#��C#�HC$=qC$�\C$�
C%�C%ffC%�C%��C&Q�C&��C&�C'33C'p�C'C(
=C(Q�C(��C(��C)G�C)�\C)�HC*(�C*p�C*�RC+
=C+ffC+�RC,
=C,Q�C,��C,�C-G�C-��C-�C.=qC.�\C.�
C/(�C/z�C/��C0(�C0p�C0�RC1  C1G�C1��C1�HC2=qC2�C2��C3{C3G�C3�C3�RC3�C4�C4Q�C4z�C4��C4C4�
C4�C5
=C533C5Q�C5p�C5��C5�C5�
C5�C6
=C6�C6G�C6ffC6�\C6�RC6�HC7  C7�C7=qC7\)C7z�C7��C7��C7��C8{C8=qC8\)C8z�C8��C8�RC8�C9{C9=qC9ffC9�C9��C9��C9��C:(�C:Q�C:z�C:��C:C:�C;{C;G�C;ffC;��C;C;�C<{C<33C<\)C<z�C<�C<�
C=  C=33C=Q�C=z�C=��C=C=�C>{C>=qC>p�C>�\C>�C>�
C>��C?(�C?Q�C?z�C?�C?��C?�C@{C@33C@\)C@�C@�C@�
CA  CA(�CAG�CAffCA�CA��CA�
CB  CB�CBG�CBp�CB�\CB�CB�
CB�CC{CC33CC\)CC�CC�CC�
CD
=CD�CDG�CDffCD�CD�CD��CD��CE(�CEQ�CEz�CE��CECE�HCF  CF(�CFG�CFz�CF�CF�
CG  CG(�CGG�CGffCG��CGCG��CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  ?��@�\@E�@�  @�  @��R@޸R@��RA\)A   A+�A@��A`��A�Q�A�  A��A��A�Q�A�  A�  A�  B   B�
B�
B  B (�B(  B0  B8  B?�
BH  BP  BW�
B_�
Bg�Bo�
Bx  B�Q�B�  B�  B��B��B�  B�{B�{B�{B��B��B�  B�  B�  B��B��B�  B�{B��B��B�  B�  B�  B�  B�  B�  B�  B�{B�(�B�{B��B�  C   C  C  C
=C
=C

=C  C�C  C
=C��C��C  C  C
=C
=C   C"  C$  C&{C(
=C*  C,  C-��C0
=C2
=C4(�C5�C7�C9��C;��C>  C@
=CB  CD  CE��CG��CJ
=CL
=CN  CO��CQ��CT
=CV  CW��CY��C\  C^  C`  Cb  Cd
=Cf
=Ch  Cj  Cl  Cm��Co��Cq��Ct
=Cv  Cw��Cy��C|
=C~
=C�  C���C���C�  C���C�  C�  C���C�  C�  C�C�C�C���C���C�  C�  C���C���C���C�  C�  C���C���C���C�  C�C���C���C���C���C�C�C�  C���C�  C�  C�C�C���C���C�  C�  C�  C�  C�C�  C���C�  C�C�C�  C�  C�C�  C�C�C�C�C�  C���C���C�  C�C�C�  C���C�C�
=C�C�  C�  C�  C�  C�C�  C�  C�C�  C�  C���C�  C�C�C�  C�  C�C�C�C���C���C���C�  C���C���C���C���C�  C�C���C���C���C�  C�C�C�C�
=C�C�  C�  C�C���C�C�
=C�  C���C�  C�  C�  C�  C�  C���C���C���C���C�  C���C���C���D ��D  D}qD  D��D  D}qD�qD��D�D}qD�qD}qD�qD}qD  D� D	  D	� D
  D
��D�D��D  Dz�D�qD� D  D��D  D� D  D}qD  D��D  D}qD  D� D  D� D  D� D  D� D�D}qD  D��D  D� D�qD}qD�qDz�D��D}qD�qD}qD  D��D  D}qD �D ��D!  D!� D"  D"z�D"�qD#� D$  D$��D%D%�D&�D&�D'�D'}qD'�qD(��D(�qD)}qD*  D*� D+  D+� D,  D,� D-  D-� D-�qD.}qD/�D/}qD/��D0}qD1  D1� D1�qD2}qD2�qD3}qD4  D4� D5  D5� D6�D6}qD7  D7��D7�qD8��D9�D9� D:  D:� D;  D;}qD;��D<}qD<��D=z�D=�qD>��D?�D?��D@�D@��DA�DA��DB�DB}qDC  DC� DD  DD��DE�DE��DF�DF��DF�qDG� DH�DH}qDI  DI� DJ�DJ�DK  DK� DL  DL}qDL�qDM� DM��DNz�DN�qDO� DP  DP� DQ  DQ� DR  DR}qDR�qDS��DTDT��DU  DU� DV�DV� DW  DW}qDX  DX��DY�DY� DZ  DZ� D[�D[��D[�qD\� D]  D]� D^�D^��D^�qD_}qD_�qD`� Da  Da��Db  Db� Dc  Dc� Dd�Dd� De  De��Df�Df� Dg  Dg}qDg�qDh}qDi  Di��Dj�Dj��Dk�Dk� Dl  Dl� Dm  Dm� Dn  Dn��Do�Do��Dp�Dp�Dq�Dq��Dr�Dr��Dr�qDs� Dt  Dt}qDu  Du� Du�qDv}qDw  Dw� Dx�Dx� Dy  Dy� Dy�qDz}qDz�qD{� D|�D|��D}�D}��D~�D~� D  D��D�HD�AHD�� D���D�HD�AHD�� D�� D�HD�@ D�~�D�� D�  D�@ D��HD��HD�  D�AHD��HD�� D�  D�@ D��HD��HD�  D�@ D�~�D���D���D�>�D�� D���D���D�@ D�� D��HD���D�@ D�� D���D���D�AHD�� D�� D�  D�AHD�� D��HD��D�AHD��HD��HD�HD�>�D�}qD���D�  D�AHD�~�D�� D�  D�@ D��HD�D�  D�>�D�~�D�� D�  D�>�D�}qD��qD���D�@ D��HD�� D�  D�AHD��HD��HD�  D�@ D��HD�D�HD�>�D�}qD���D���D�@ D��HD�� D�  D�>�D�}qD���D�HD�AHD��HD��HD�HD�>�D�}qD��qD���D�@ D�� D�� D�  D�AHD��HD�� D���D�@ D��HD��HD�HD�>�D�� D��HD���D�=qD�~�D�� D���D�>�D��HD��HD�  D�@ D�~�D��HD�HD�AHD��HD���D���D�@ D�|)D��qD�HD�@ D��HD��HD�HD�@ D�~�D�� D���D�@ D��HD���D�  D�AHD�� D���D�HD�B�D��HD��HD�HD�AHD��HD�D�HD�@ D�� D�� D�  D�@ D�� D��HD��D�AHD�� D��HD�  D�>�D�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D���D�  D�@ D�� D�D�  D�@ D�� D���D���D�>�D�� D�� D�HD�AHD��HD��HD�HD�@ D�� D��HD�HD�@ D�� D���D���D�AHD�� D�� D�  D�@ D�� D�� D���D�>�D�� D�D�  D�@ D�� D�� D�  D�@ D�� D��HD��D�AHD�� D�� D���D�>�D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD��HD��D�@ D�� D��HD���D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D D¾�D���D�@ DÁHD�D�HD�@ DāHD��HD�  D�@ D�~�DŽqD���D�@ DƁHD�� D���D�@ DǁHD��HD��D�AHD�~�D�� D�  D�>�D�~�DɽqD���D�@ Dʂ�D�� D�  D�@ DˁHD��HD���D�=qD̀ D��HD�  D�@ D�~�D;�D�  D�AHD΀ D�� D�  D�AHDπ DϾ�D��qD�>�DЁHD��HD���D�@ Dр D�� D�  D�>�D�~�D�� D�HD�@ DӀ D��HD�HD�>�D�~�D��HD�HD�@ DՀ D�� D�  D�@ D�~�D־�D���D�@ D׀ D�� D�  D�@ D�~�D�� D���D�AHDق�D�D�  D�AHDځHD�� D���D�>�D�~�D�� D���D�@ D܁HD�� D���D�>�D�~�D�� D�  D�>�Dހ D��HD�HD�@ D�~�D߾�D�HD�AHD�� DྸD�  D�AHD�HD�� D�  D�@ D� D⾸D�  D�B�D�HD�D�  D�>�D� D�� D��D�B�D�~�D�� D�  D�>�D�~�D澸D���D�=qD� D羸D���D�@ D� D辸D�HD�AHD� D�� D�HD�@ D�~�D�� D�  D�>�D�~�D��HD�  D�>�D� D��HD���D�@ D�~�D�� D�  D�AHD� D��HD�  D�AHD� D�� D�  D�@ D�� D��HD�  D�@ D�HD�� D�  D�AHD�HD��HD�  D�>�D�~�D�D�  D�B�D� D���D���D�@ D�~�D���D�HD�@ D�� D�� D�  D�AHD��HD�D�  D�@ D�� D��HD���D�=qD�~�D��HD�  D�>�D�o\G�O�?#�
?8Q�?u?�z�?�33?�
=?�@�@
=@�R@333@B�\@O\)@^�R@k�@z�H@�ff@�{@�@��R@��@���@�
=@�p�@�ff@У�@�
=@�  @�=q@��@���A�AA	��A�RA33AffA(�A   A#�
A(��A,��A0��A5A9��A>{AC33AFffAK�AO\)AS�
AX��A\(�A`��Ae�Ah��Amp�Aq�Au�Az�HA~{A���A��A�p�A�\)A���A��
A�p�A�  A��A��
A�ffA�Q�A��A�z�A�ffA�Q�A��HA���A��RA�G�A�33A��A�\)A���A�33A�p�A��A���A��A�{A��A��A�(�A�AǮA�=qA��
A�A�Q�A��A�(�A�ffA�  A�=qA�z�A�ffA�Q�A�\A���A�RA���A�33A���A�RA�G�A��HA�p�A��A���A�33A�p�A�\)B ��BB�HB�B��B�B�RB�
B��B	�B
�HB(�B��B�B33B  B�B=qB33B  BG�B=qB
=BQ�Bp�B=qB\)Bz�B�BffB\)B Q�B!p�B"�\B#33B$��B%��B&ffB'�B(��B)p�B*�RB+�B,z�B-��B.�HB/�B0��B1�B2�\B3�B4��B5B6�\B7�B8��B9B:�RB;�
B<��B=B>�HB@  BAG�BBffBC\)BD��BE�BF�HBH(�BIp�BJ�RBK�BL��BN=qBO�BP��BQBS
=BT(�BUG�BV�RBX  BX��BZffB[�B\��B]�B_\)B`z�Ba��Bb�RBdQ�Bep�BfffBg�Bi�Bj=qBk33Blz�BmBn�RBo�Bp��Br=qBs33Bt(�Bup�Bv�\Bw�Bxz�By��Bz�RB{�B|z�B}p�B~=qB~�RB\)B�
B�=qB�z�B��\B��RB���B��B�G�B�\)B��B��
B��B�{B�=qB��\B���B���B���B�G�B�\)B��B��B��B�(�B�=qB�ffB���B��HB���B��B�p�B���B�B��B�{B�Q�B�z�B���B���B�
=B�33B�p�B��B��B�  B�(�B�=qB�ffB���B��HB��HB�33B�p�B���B��B��B�(�B�Q�B�ffB���B��HB��B�33B�p�B��B��B�{B�=qB�ffB��RB��HB�
=B�33B��B��B��
B�{B�ffB���B��RB���B�33B��B�B��B�(�B�z�B���B���B��B��B��B�(�B�Q�B���B�
=B�\)B��B��
B�(�B��\B��HB��B�p�B��
B�(�B��\B���B��B�p�B��
B�(�B�ffB��RB��B��B��
B�{B�ffB���B�33B���B��
B�(�B��\B���B�\)B��B�  B�Q�B���B�33B���B��B�Q�B��RB��B���B�  B�Q�B��RB��B��B�{B��\B��HB�G�B��B�(�B���B��B���B�  B�ffB���B�33B��B�(�B��\B���B�G�B��B�(�B��RB��B�p�B��
B�=qB���B�33B��B�  B�ffB���B�33B�B�=qB���B���B�\)B�B�=qB��RB��B��B��
B�Q�B��RB�33B��B�(�B��\B�
=B��B�  B�z�B�
=B���B�(�B��\B�
=B���B�  B��\B��B��B�(�B���B��B���B�{B£�B��BîB�=qBĸRB�33Bř�B�{BƸRB�33B�B�=qBȸRB�33BɮB�(�Bʣ�B�33BˮB�=qB̸RB�33BͮB�{BΏ\B��BϮB�=qBиRB�33Bљ�B�{Bң�B��BӮB�=qB���B�\)B��B�ffB���B�p�B��B�z�B�
=BمB�  Bڣ�B�33B�B�Q�B��HB�p�B��B�ffB��HB�p�B��B�z�B�
=B�B�{B�RB�33B��
B�Q�B��HB�p�B��B�z�B���B�B�  B�z�B�
=B�B�(�B���B�\)B��
B�ffB��HB�\)B��B�ffB�
=BB�=qB�RB�G�B��
B�Q�B���B�G�B��
B�ffB���B��B�{B���B�33B�B�=qB���B�G�B��
B�Q�B���B�\)B��B�z�B���B��B�{B���B��B��C {C \)C ��C �
C{CQ�C�\C��C
=CG�C�\C��C
=CQ�C�\C�
C{C\)C��C�
C�C\)C��C�HC(�CffC��C�HC(�CffC��C�C(�Cp�C�C�C	33C	z�C	�RC	��C
=qC
z�C
C  CG�C�C��C
=CG�C�\C��C
=CQ�C�\C�
C{C\)C��C�HC�CffC��C�C(�Cp�C�C��C33Cz�CC
=CQ�C�\C�
C{C\)C��C�HC�CffC��C�C�CffC�C��C33Cz�C�RC  CG�C�\C�
C�CffC��C�C33Cz�C�RC  CQ�C��C�HC33Cz�CC
=CQ�C��C�C(�Cp�C�RC  C=qC�CC
=CG�C�\C�
C �C ffC �C!  C!G�C!�\C!�
C"(�C"p�C"�RC#  C#G�C#��C#�HC$=qC$�\C$�
C%�C%ffC%�C%��C&Q�C&��C&�C'33C'p�C'C(
=C(Q�C(��C(��C)G�C)�\C)�HC*(�C*p�C*�RC+
=C+ffC+�RC,
=C,Q�C,��C,�C-G�C-��C-�C.=qC.�\C.�
C/(�C/z�C/��C0(�C0p�C0�RC1  C1G�C1��C1�HC2=qC2�C2��C3{C3G�C3�C3�RC3�C4�C4Q�C4z�C4��C4C4�
C4�C5
=C533C5Q�C5p�C5��C5�C5�
C5�C6
=C6�C6G�C6ffC6�\C6�RC6�HC7  C7�C7=qC7\)C7z�C7��C7��C7��C8{C8=qC8\)C8z�C8��C8�RC8�C9{C9=qC9ffC9�C9��C9��C9��C:(�C:Q�C:z�C:��C:C:�C;{C;G�C;ffC;��C;C;�C<{C<33C<\)C<z�C<�C<�
C=  C=33C=Q�C=z�C=��C=C=�C>{C>=qC>p�C>�\C>�C>�
C>��C?(�C?Q�C?z�C?�C?��C?�C@{C@33C@\)C@�C@�C@�
CA  CA(�CAG�CAffCA�CA��CA�
CB  CB�CBG�CBp�CB�\CB�CB�
CB�CC{CC33CC\)CC�CC�CC�
CD
=CD�CDG�CDffCD�CD�CD��CD��CE(�CEQ�CEz�CE��CECE�HCF  CF(�CFG�CFz�CF�CF�
CG  CG(�CGG�CGffCG��CGCG��CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�4@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�jA�-A�l�A��#A�XA���A��A��A㕁A�hA�A�z�A�p�A�hsA�bNA�ZA�VA�Q�A�K�A�E�A�C�A�?}A�7LA�-A�bA╁A��;A�
=A�G�Aٟ�A�`BA��`Aו�A�O�A�5?A�?}AґhAиRA�\)A�ȴA˝�A�v�A�S�A�-A��/A�{A���A�l�A�9XA�
=A��A���A�r�A�\)A�M�A�9XA��^A�-A��+A�-A�C�A�XA��9A�%A�ĜA�M�A��#A�bA�A��9A��yA��DA���A� �A�I�A��A�XA��;A��A��TA�+A���A���A�`BA��A�l�A�?}A��mA���A}�As�hAp�!Ao�An�yAi�TAg/A`E�A\5?AWx�AV�AUATJARJAN�AKS�AIt�AHn�AG��AF{ABffA@{A>�+A<��A:�yA8��A7"�A6I�A5t�A49XA3�A3G�A2�RA1C�A/O�A.�\A-�-A-7LA,�`A,�/A,�A+�FA*�!A*��A*A�A)��A(�`A)oA(�yA'A'O�A&�jA%ƨA$�yA#��A"��A!��A!+A r�A�-AK�A�AK�A��A-A��AVA�\A�;A�FA�PA�yAVAA�;A�wA��AoAr�A�PAhsA/A��A�uA �A�mA��A+AȴAZAƨAK�A%A�/A�A�DAbNA1A��AK�AA�!Az�A$�AƨA�-A��AC�A��AĜAVA��A;dA
��A
��A
ĜA
�!A
jA
9XA
JA	A	|�A	C�A	7LA	�A�`An�AbA��A�A��A�7Ap�A&�A�yA�jA��A�uA^5A{A�A�A�TA�wA�hA�Ax�AXA?}A�RA9XA�
A��AdZA7LA�AjAZA�A1A�#AK�A �A ��A I�@�\)@��R@�{@��@��@��y@��@��j@���@��@��@���@�@�7L@�r�@�@�o@��H@�$�@�j@��@�ȴ@���@�Q�@�@�v�@���@�&�@��/@�@���@�l�@�ff@�h@���@�u@�Q�@�@�33@���@�ȴ@�\@�v�@�{@�@ߝ�@�l�@�l�@�dZ@�C�@���@�5?@�@ݑh@��@��`@�A�@�ƨ@�+@���@�$�@ٲ-@�V@أ�@�z�@�Z@� �@֟�@Ԭ@��;@ӝ�@�S�@��@��@҇+@�n�@��@щ7@��@�Ĝ@У�@�Z@��@���@��m@�ƨ@ύP@��@�hs@��/@̋D@�ƨ@���@�v�@�-@ɺ^@Ɂ@�X@�V@ȴ9@�Z@���@�dZ@Ɨ�@�{@ũ�@�O�@���@���@Ĵ9@Ĭ@ģ�@ģ�@ēu@��m@�dZ@�
=@��@�@�J@�x�@�`B@�G�@��/@��@�|�@�
=@�ȴ@��\@�^5@�-@��T@���@�hs@���@��`@��@��@�1'@��P@�+@�
=@���@�@�@�X@���@���@��@��
@�\)@�
=@���@��@���@���@�5?@���@�7L@�1'@��@��@��P@�dZ@�33@���@���@�=q@���@�hs@�%@���@�bN@��@���@�ƨ@��@�l�@�33@���@�{@��-@�?}@�&�@�%@��@��@�9X@��w@�;d@�o@�ȴ@�v�@�5?@���@�X@���@���@��@�ƨ@�t�@�\)@�;d@��y@��!@��+@�=q@��@��-@�?}@��9@�bN@�(�@��@�ƨ@��@���@�l�@�S�@���@�{@���@�j@���@�K�@��@�$�@��T@��h@�G�@���@��@�bN@��@��w@��@�l�@�"�@���@���@�M�@���@��T@�p�@���@��D@�I�@�  @��w@�dZ@��H@��R@��\@�v�@�V@�{@�@��@�7L@��@���@��9@�Q�@��@���@�;d@��@��@�@���@�x�@�7L@�%@��u@�9X@�1@��F@�C�@��H@��+@���@�O�@�V@�Ĝ@���@�dZ@���@��H@�ȴ@���@�~�@�$�@��@���@�X@��@��`@���@��9@�r�@�Z@�1'@���@�l�@�"�@��H@��!@�~�@�E�@��T@�x�@�O�@�&�@�%@��/@�Ĝ@��@��D@�9X@�1@���@��;@��F@�+@��!@���@�~�@�n�@�n�@�V@��T@�O�@���@�z�@�j@�Z@�b@�w@\)@~��@~ȴ@~��@~E�@}�@}`B@|j@{��@{33@z�\@y�7@yhs@y7L@x�`@x�9@xbN@x �@w�;@wK�@v�@v�+@u�@u?}@tI�@s�@r��@rn�@r-@q�^@q�@pQ�@pA�@pb@oK�@nȴ@nV@m��@m`B@l�/@lj@l�@k�m@k�@kS�@j~�@i�#@iX@i�@h��@h  @g�@f��@f�y@fȴ@f@ep�@d��@d�@d9X@c��@b�H@bM�@a��@a%@`��@`�`@`��@`�@`bN@`A�@`b@_��@_�@]�-@\��@\��@\��@\I�@[�
@[@Z�H@ZM�@Y��@Y�@Y%@X��@X�u@W�@W|�@V�@V��@VV@V{@U�-@U�@Up�@UO�@U/@T��@T�/@T1@SC�@S"�@So@R��@R��@R^5@Q�7@P��@P1'@Pb@P  @O��@O�w@O��@OK�@Nff@N{@M?}@L�j@Lj@LI�@L(�@K�m@Kt�@KdZ@KS�@J��@JM�@I�#@I%@H�`@H�9@G�;@G;d@Fȴ@Fv�@E@E?}@EV@D�@D�/@D��@Dz�@C�m@C��@C33@B�H@B�\@B�@A�#@AX@@��@@�9@@�@@ �@?��@?K�@?+@?�@>�R@>E�@=�@=@=�h@=/@<��@<�/@<��@<��@;�
@:��@:��@:��@:n�@:^5@:M�@9��@9�#@9�^@9x�@8��@8bN@7��@6�y@6�@6ȴ@6��@6E�@5�@5�-@5O�@4��@4j@3��@3�F@3��@3t�@3C�@3o@2�@2��@2-@1�@0�u@0Q�@/�w@/|�@/|�@/\)@/K�@/+@/
=@.��@.�@.ȴ@.�R@.��@.v�@.5?@-��@-@-�h@-`B@-/@,�@,�/@,I�@+��@+ƨ@+S�@*�@*��@*��@)�#@)G�@(�`@(�9@(�@(bN@(1'@(  @'�w@'l�@'+@&�@&��@&v�@&ff@&E�@&5?@&@%�@%��@%�h@%�@$�j@$�@$�@$�@$�@$�D@$j@$�@#��@#�m@#�@#C�@"�H@"M�@"J@!�7@ ��@ ��@ �@ bN@ Q�@ b@�;@�P@l�@;d@�@�+@ff@V@E�@E�@�@��@�h@`B@�@�j@��@�D@j@Z@j@Z@Z@I�@1@�F@��@o@��@n�@M�@J@�@�@�@�^@hs@7L@Ĝ@�@�@1'@�@�;@��@|�@l�@��@ȴ@��@��@v�@ff@E�@$�@{@�@�-@�-@��@��@�h@p�@?}@��@�@j@I�@I�@I�@(�@1@�
@�F@��@��@��@S�@"�@�H@�!@~�@n�@�@��@G�@7L@�@�@%@��@r�@b@�@��@�;@��@�w@�P@\)@
=@�@��@�+@ff@E�@@�@@�h@p�@O�@/@�@�@�D@j@Z@9X@��@�
@ƨ@��@�@t�@S�@C�@"�@"�@o@
��@
��@
~�@
n�@
M�@
-@
-A���A���A���A���A���A�FA�wA�ȴA�ĜA�RA�A��A�-A�RA�A�A�l�A���A�  A��TA�wA��A�A�K�A�9XA�  A��#A���A�jA�!A�A��A��A��A��A㝲A㛦A㙚A�hA㕁A�uA�\A�hA�PA�A�A�A�~�A�A�z�A�|�A�|�A�x�A�|�A�z�A�x�A�|�A�v�A�t�A�r�A�p�A�p�A�p�A�hsA�jA�l�A�ffA�jA�jA�dZA�dZA�hsA�bNA�dZA�ffA�^5A�bNA�dZA�^5A�^5A�`BA�ZA�ZA�^5A�ZA�XA�\)A�XA�S�A�XA�XA�S�A�ZA�VA�Q�A�VA�VA�O�A�S�A�Q�A�M�A�Q�A�O�A�K�A�O�A�M�A�I�A�M�A�M�A�I�A�G�A�I�A�G�A�E�A�G�A�E�A�A�A�G�A�C�A�A�A�E�A�E�A�?}A�A�A�E�A�A�A�A�A�E�A�?}A�?}A�C�A�?}A�9XA�=qA�;dA�7LA�;dA�9XA�33A�9XA�5?A�1'A�5?A�33A�-A�/A�1'A�-A�+A�+A�"�A��A��A�bA�oA�bA�A���A��mA�ȴA��A❲A�+A�t�A�hsA�S�A�A�A�/A�%A��A��`A�jA�z�A��A��yA�ƨA�\)A�&�A��HA߮A�"�A��HA�Q�A�oA�ĜA�-Aܥ�A�hsA���A�jA�z�A�ffA��A��/Aز-Aؕ�A�z�A�t�A�jA�`BA�Q�A�9XA�oA���A���A��yA��/A�ƨAײ-Aק�Aק�Aף�Aו�A׉7A�v�A�bNA�XA�S�A�Q�A�K�A�C�A�?}A�C�A�?}A�9XA�7LA�5?A�-A� �A��A�oA��AֶFA�hsA���A��Aԧ�A��AӲ-A�v�A�C�A��A��`AҰ!A�ffA�-A�oA�  A��A��A��`A��#A���A�AѮAї�A�|�A�l�A�A�A��A�VA���A��AмjAС�AЉ7A�|�A�p�A�ZA�I�A�9XA�(�A��A�1A��A��yA��;A���A�ȴAϺ^Aϲ-Aϩ�Aϥ�Aϙ�Aϕ�Aϝ�A�z�A�ZA�5?A�$�A�"�A��A�JA���A��/A�ȴAβ-Aδ9AΙ�A΍PA�~�A�jA�jA�dZA�7LA���A��yA���Aͥ�AͅA�|�A�hsA�A�A�7LA�"�A�VA��A���A�A̲-A̗�ÃA�t�A�XA�1'A��;A˸RAˬAˇ+A�G�A��A�?}A��mAɋDA�5?A��Aȴ9A�ffA�  A���AǗ�A�XA�oA���A�\)A��A���Aŝ�A�E�Aĺ^A�t�A�G�A�"�A�(�A��A�JA���A��yA���Aã�Aß�AÝ�AÕ�A�x�A�K�A�1'A� �A�%A��A�z�A�A���A�dZA�?}A�"�A�A��mA��9A�;dA�  A��A���A���A�l�A�VA��!A�|�A�XA�A�A�-A�{A�JA���A���A�ȴA�ĜA��^A���A���A��+A�t�A�n�A�n�A�p�A�dZA�ffA�ffA�dZA�dZA�dZA�XA�9XA�5?A�33A�33A�1'A� �A��A��A��A�oA�bA�oA�JA�1A�%A���A��A��A��`A�ĜA�VA�VA���A��A��A��A�ĜA��-A���A���A��PA��A��PA���A��A�p�A�n�A�l�A�hsA�dZA�bNA�ffA�ffA�dZA�ZA�ZA�XA�Q�A�Q�A�VA�Q�A�S�A�Q�A�M�A�K�A�G�A�E�A�G�A�C�A�A�A�;dA�7LA�5?A�-A�&�A�"�A�{A���A�ȴA���A��hA�|�A�`BA�?}A��A��A�C�A�
=A��-A�x�A�O�A� �A��/A���A��+A�jA�+A��yA��wA��RA�n�A�oA�
=A�A���A���A�~�A�hsA�XA�E�A�33A�"�A���A���A���A��A�v�A�XA�bA�A�  A���A���A���A��A��DA�S�A�K�A�E�A�5?A��A���A��A��yA��mA��mA��A��A��A��;A���A���A�p�A�bA��A�|�A�=qA��A���A��`A��
A���A���A���A��
A��TA��A��A�A�%A�JA��A��A�{A�bA�bA�JA�1A�1A�A�  A���A���A���A��A��A��yA���A���A�`BA�A�A��A�oA�1A�A��A��;A���A���A��+A�r�A�`BA�-A��A��mA��-A�bNA�(�A�ƨA�(�A���A���A�-A��A��A��FA���A�dZA�+A���A�A���A�~�A�O�A�%A���A��A�A��A�VA�%A�&�A��A���A��A���A�l�A�A�A�JA��/A���A�oA�/A�/A�A�v�A�K�A�33A�&�A�A�ȴA�~�A�E�A��A���A���A�XA��A��FA�bNA��A�E�A�bNA�\)A���A�VA�|�A�A�^5A��mA���A�XA���A���A�XA�C�A� �A��A�bA�A��mA���A��jA��!A���A���A��uA�~�A�~�A�z�A�z�A�l�A�bNA�bNA�dZA�VA�Q�A�I�A�A�A�"�A�M�A��-A�/A�;dA�S�A�G�A�I�A�t�A��
A�hsA���A��A��#A�r�A�$�A��A��jA���A��7A�jA�/A���A�A���A�?}A��7A� �A���A��DA�~�A�jA�9XA�A~��A}�FA|v�A{|�Az5?Ax��Av�jAs�;Ar9XAq�FAqS�Aq+Aq
=Ap��Ap�!Ap�\Apv�ApVAp$�ApJApAo�Ao��Ao�wAo�7AodZAo?}Ao�An��An��Anv�Al{Aj�Ajv�AjbAi�TAi��Ai�hAit�Ait�Ai\)Ai33Ai%Ah�jAh��Ah�uAh9XAh�Ah  Ag��Ag�TAg�FAgVAf�+Ae��Ae|�Ad��Ad�Ad  Ac�Aa�Aa
=A`=qA_��A_�A_�hA_�7A_|�A_�A_dZA_G�A_&�A_�A_�A^��A^�RA^�9A^bA]�A\E�AZ��AZbAY��AYl�AX��AX�\AXE�AX1AW�AW�wAW��AW�AWx�AWK�AW�AV�`AV�AV��AV�AV��AV��AV�DAVM�AV1'AV�AU��AU�TAU��AU�^AU�FAU��AU�AUp�AUhsAUXAUC�AU33AU�AUAT�yAT�ATĜAT�9ATn�ATffATVAT^5AT5?AT(�AT �AT1AT1AS��AS��AS�AS�mAS�TAS�
AS�
AS��AS�-AS�PAS\)AS33AR�/ARffAQ�#AQ|�AQAP��API�AO�AO��AOK�AO�AN�HAN�jAN��ANI�AN9XAM��AM�AM7LAL�HALjALA�ALJAK��AKAK�hAK|�AKdZAK\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  A���A�jA�-A�l�A��#A�XA���A��A��A㕁A�hA�A�z�A�p�A�hsA�bNA�ZA�VA�Q�A�K�A�E�A�C�A�?}A�7LA�-A�bA╁A��;A�
=A�G�Aٟ�A�`BA��`Aו�A�O�A�5?A�?}AґhAиRA�\)A�ȴA˝�A�v�A�S�A�-A��/A�{A���A�l�A�9XA�
=A��A���A�r�A�\)A�M�A�9XA��^A�-A��+A�-A�C�A�XA��9A�%A�ĜA�M�A��#A�bA�A��9A��yA��DA���A� �A�I�A��A�XA��;A��A��TA�+A���A���A�`BA��A�l�A�?}A��mA���A}�As�hAp�!Ao�An�yAi�TAg/A`E�A\5?AWx�AV�AUATJARJAN�AKS�AIt�AHn�AG��AF{ABffA@{A>�+A<��A:�yA8��A7"�A6I�A5t�A49XA3�A3G�A2�RA1C�A/O�A.�\A-�-A-7LA,�`A,�/A,�A+�FA*�!A*��A*A�A)��A(�`A)oA(�yA'A'O�A&�jA%ƨA$�yA#��A"��A!��A!+A r�A�-AK�A�AK�A��A-A��AVA�\A�;A�FA�PA�yAVAA�;A�wA��AoAr�A�PAhsA/A��A�uA �A�mA��A+AȴAZAƨAK�A%A�/A�A�DAbNA1A��AK�AA�!Az�A$�AƨA�-A��AC�A��AĜAVA��A;dA
��A
��A
ĜA
�!A
jA
9XA
JA	A	|�A	C�A	7LA	�A�`An�AbA��A�A��A�7Ap�A&�A�yA�jA��A�uA^5A{A�A�A�TA�wA�hA�Ax�AXA?}A�RA9XA�
A��AdZA7LA�AjAZA�A1A�#AK�A �A ��A I�@�\)@��R@�{@��@��@��y@��@��j@���@��@��@���@�@�7L@�r�@�@�o@��H@�$�@�j@��@�ȴ@���@�Q�@�@�v�@���@�&�@��/@�@���@�l�@�ff@�h@���@�u@�Q�@�@�33@���@�ȴ@�\@�v�@�{@�@ߝ�@�l�@�l�@�dZ@�C�@���@�5?@�@ݑh@��@��`@�A�@�ƨ@�+@���@�$�@ٲ-@�V@أ�@�z�@�Z@� �@֟�@Ԭ@��;@ӝ�@�S�@��@��@҇+@�n�@��@щ7@��@�Ĝ@У�@�Z@��@���@��m@�ƨ@ύP@��@�hs@��/@̋D@�ƨ@���@�v�@�-@ɺ^@Ɂ@�X@�V@ȴ9@�Z@���@�dZ@Ɨ�@�{@ũ�@�O�@���@���@Ĵ9@Ĭ@ģ�@ģ�@ēu@��m@�dZ@�
=@��@�@�J@�x�@�`B@�G�@��/@��@�|�@�
=@�ȴ@��\@�^5@�-@��T@���@�hs@���@��`@��@��@�1'@��P@�+@�
=@���@�@�@�X@���@���@��@��
@�\)@�
=@���@��@���@���@�5?@���@�7L@�1'@��@��@��P@�dZ@�33@���@���@�=q@���@�hs@�%@���@�bN@��@���@�ƨ@��@�l�@�33@���@�{@��-@�?}@�&�@�%@��@��@�9X@��w@�;d@�o@�ȴ@�v�@�5?@���@�X@���@���@��@�ƨ@�t�@�\)@�;d@��y@��!@��+@�=q@��@��-@�?}@��9@�bN@�(�@��@�ƨ@��@���@�l�@�S�@���@�{@���@�j@���@�K�@��@�$�@��T@��h@�G�@���@��@�bN@��@��w@��@�l�@�"�@���@���@�M�@���@��T@�p�@���@��D@�I�@�  @��w@�dZ@��H@��R@��\@�v�@�V@�{@�@��@�7L@��@���@��9@�Q�@��@���@�;d@��@��@�@���@�x�@�7L@�%@��u@�9X@�1@��F@�C�@��H@��+@���@�O�@�V@�Ĝ@���@�dZ@���@��H@�ȴ@���@�~�@�$�@��@���@�X@��@��`@���@��9@�r�@�Z@�1'@���@�l�@�"�@��H@��!@�~�@�E�@��T@�x�@�O�@�&�@�%@��/@�Ĝ@��@��D@�9X@�1@���@��;@��F@�+@��!@���@�~�@�n�@�n�@�V@��T@�O�@���@�z�@�j@�Z@�b@�w@\)@~��@~ȴ@~��@~E�@}�@}`B@|j@{��@{33@z�\@y�7@yhs@y7L@x�`@x�9@xbN@x �@w�;@wK�@v�@v�+@u�@u?}@tI�@s�@r��@rn�@r-@q�^@q�@pQ�@pA�@pb@oK�@nȴ@nV@m��@m`B@l�/@lj@l�@k�m@k�@kS�@j~�@i�#@iX@i�@h��@h  @g�@f��@f�y@fȴ@f@ep�@d��@d�@d9X@c��@b�H@bM�@a��@a%@`��@`�`@`��@`�@`bN@`A�@`b@_��@_�@]�-@\��@\��@\��@\I�@[�
@[@Z�H@ZM�@Y��@Y�@Y%@X��@X�u@W�@W|�@V�@V��@VV@V{@U�-@U�@Up�@UO�@U/@T��@T�/@T1@SC�@S"�@So@R��@R��@R^5@Q�7@P��@P1'@Pb@P  @O��@O�w@O��@OK�@Nff@N{@M?}@L�j@Lj@LI�@L(�@K�m@Kt�@KdZ@KS�@J��@JM�@I�#@I%@H�`@H�9@G�;@G;d@Fȴ@Fv�@E@E?}@EV@D�@D�/@D��@Dz�@C�m@C��@C33@B�H@B�\@B�@A�#@AX@@��@@�9@@�@@ �@?��@?K�@?+@?�@>�R@>E�@=�@=@=�h@=/@<��@<�/@<��@<��@;�
@:��@:��@:��@:n�@:^5@:M�@9��@9�#@9�^@9x�@8��@8bN@7��@6�y@6�@6ȴ@6��@6E�@5�@5�-@5O�@4��@4j@3��@3�F@3��@3t�@3C�@3o@2�@2��@2-@1�@0�u@0Q�@/�w@/|�@/|�@/\)@/K�@/+@/
=@.��@.�@.ȴ@.�R@.��@.v�@.5?@-��@-@-�h@-`B@-/@,�@,�/@,I�@+��@+ƨ@+S�@*�@*��@*��@)�#@)G�@(�`@(�9@(�@(bN@(1'@(  @'�w@'l�@'+@&�@&��@&v�@&ff@&E�@&5?@&@%�@%��@%�h@%�@$�j@$�@$�@$�@$�@$�D@$j@$�@#��@#�m@#�@#C�@"�H@"M�@"J@!�7@ ��@ ��@ �@ bN@ Q�@ b@�;@�P@l�@;d@�@�+@ff@V@E�@E�@�@��@�h@`B@�@�j@��@�D@j@Z@j@Z@Z@I�@1@�F@��@o@��@n�@M�@J@�@�@�@�^@hs@7L@Ĝ@�@�@1'@�@�;@��@|�@l�@��@ȴ@��@��@v�@ff@E�@$�@{@�@�-@�-@��@��@�h@p�@?}@��@�@j@I�@I�@I�@(�@1@�
@�F@��@��@��@S�@"�@�H@�!@~�@n�@�@��@G�@7L@�@�@%@��@r�@b@�@��@�;@��@�w@�P@\)@
=@�@��@�+@ff@E�@@�@@�h@p�@O�@/@�@�@�D@j@Z@9X@��@�
@ƨ@��@�@t�@S�@C�@"�@"�@o@
��@
��@
~�@
n�@
M�@
-G�O�A���A���A���A���A���A�FA�wA�ȴA�ĜA�RA�A��A�-A�RA�A�A�l�A���A�  A��TA�wA��A�A�K�A�9XA�  A��#A���A�jA�!A�A��A��A��A��A㝲A㛦A㙚A�hA㕁A�uA�\A�hA�PA�A�A�A�~�A�A�z�A�|�A�|�A�x�A�|�A�z�A�x�A�|�A�v�A�t�A�r�A�p�A�p�A�p�A�hsA�jA�l�A�ffA�jA�jA�dZA�dZA�hsA�bNA�dZA�ffA�^5A�bNA�dZA�^5A�^5A�`BA�ZA�ZA�^5A�ZA�XA�\)A�XA�S�A�XA�XA�S�A�ZA�VA�Q�A�VA�VA�O�A�S�A�Q�A�M�A�Q�A�O�A�K�A�O�A�M�A�I�A�M�A�M�A�I�A�G�A�I�A�G�A�E�A�G�A�E�A�A�A�G�A�C�A�A�A�E�A�E�A�?}A�A�A�E�A�A�A�A�A�E�A�?}A�?}A�C�A�?}A�9XA�=qA�;dA�7LA�;dA�9XA�33A�9XA�5?A�1'A�5?A�33A�-A�/A�1'A�-A�+A�+A�"�A��A��A�bA�oA�bA�A���A��mA�ȴA��A❲A�+A�t�A�hsA�S�A�A�A�/A�%A��A��`A�jA�z�A��A��yA�ƨA�\)A�&�A��HA߮A�"�A��HA�Q�A�oA�ĜA�-Aܥ�A�hsA���A�jA�z�A�ffA��A��/Aز-Aؕ�A�z�A�t�A�jA�`BA�Q�A�9XA�oA���A���A��yA��/A�ƨAײ-Aק�Aק�Aף�Aו�A׉7A�v�A�bNA�XA�S�A�Q�A�K�A�C�A�?}A�C�A�?}A�9XA�7LA�5?A�-A� �A��A�oA��AֶFA�hsA���A��Aԧ�A��AӲ-A�v�A�C�A��A��`AҰ!A�ffA�-A�oA�  A��A��A��`A��#A���A�AѮAї�A�|�A�l�A�A�A��A�VA���A��AмjAС�AЉ7A�|�A�p�A�ZA�I�A�9XA�(�A��A�1A��A��yA��;A���A�ȴAϺ^Aϲ-Aϩ�Aϥ�Aϙ�Aϕ�Aϝ�A�z�A�ZA�5?A�$�A�"�A��A�JA���A��/A�ȴAβ-Aδ9AΙ�A΍PA�~�A�jA�jA�dZA�7LA���A��yA���Aͥ�AͅA�|�A�hsA�A�A�7LA�"�A�VA��A���A�A̲-A̗�ÃA�t�A�XA�1'A��;A˸RAˬAˇ+A�G�A��A�?}A��mAɋDA�5?A��Aȴ9A�ffA�  A���AǗ�A�XA�oA���A�\)A��A���Aŝ�A�E�Aĺ^A�t�A�G�A�"�A�(�A��A�JA���A��yA���Aã�Aß�AÝ�AÕ�A�x�A�K�A�1'A� �A�%A��A�z�A�A���A�dZA�?}A�"�A�A��mA��9A�;dA�  A��A���A���A�l�A�VA��!A�|�A�XA�A�A�-A�{A�JA���A���A�ȴA�ĜA��^A���A���A��+A�t�A�n�A�n�A�p�A�dZA�ffA�ffA�dZA�dZA�dZA�XA�9XA�5?A�33A�33A�1'A� �A��A��A��A�oA�bA�oA�JA�1A�%A���A��A��A��`A�ĜA�VA�VA���A��A��A��A�ĜA��-A���A���A��PA��A��PA���A��A�p�A�n�A�l�A�hsA�dZA�bNA�ffA�ffA�dZA�ZA�ZA�XA�Q�A�Q�A�VA�Q�A�S�A�Q�A�M�A�K�A�G�A�E�A�G�A�C�A�A�A�;dA�7LA�5?A�-A�&�A�"�A�{A���A�ȴA���A��hA�|�A�`BA�?}A��A��A�C�A�
=A��-A�x�A�O�A� �A��/A���A��+A�jA�+A��yA��wA��RA�n�A�oA�
=A�A���A���A�~�A�hsA�XA�E�A�33A�"�A���A���A���A��A�v�A�XA�bA�A�  A���A���A���A��A��DA�S�A�K�A�E�A�5?A��A���A��A��yA��mA��mA��A��A��A��;A���A���A�p�A�bA��A�|�A�=qA��A���A��`A��
A���A���A���A��
A��TA��A��A�A�%A�JA��A��A�{A�bA�bA�JA�1A�1A�A�  A���A���A���A��A��A��yA���A���A�`BA�A�A��A�oA�1A�A��A��;A���A���A��+A�r�A�`BA�-A��A��mA��-A�bNA�(�A�ƨA�(�A���A���A�-A��A��A��FA���A�dZA�+A���A�A���A�~�A�O�A�%A���A��A�A��A�VA�%A�&�A��A���A��A���A�l�A�A�A�JA��/A���A�oA�/A�/A�A�v�A�K�A�33A�&�A�A�ȴA�~�A�E�A��A���A���A�XA��A��FA�bNA��A�E�A�bNA�\)A���A�VA�|�A�A�^5A��mA���A�XA���A���A�XA�C�A� �A��A�bA�A��mA���A��jA��!A���A���A��uA�~�A�~�A�z�A�z�A�l�A�bNA�bNA�dZA�VA�Q�A�I�A�A�A�"�A�M�A��-A�/A�;dA�S�A�G�A�I�A�t�A��
A�hsA���A��A��#A�r�A�$�A��A��jA���A��7A�jA�/A���A�A���A�?}A��7A� �A���A��DA�~�A�jA�9XA�A~��A}�FA|v�A{|�Az5?Ax��Av�jAs�;Ar9XAq�FAqS�Aq+Aq
=Ap��Ap�!Ap�\Apv�ApVAp$�ApJApAo�Ao��Ao�wAo�7AodZAo?}Ao�An��An��Anv�Al{Aj�Ajv�AjbAi�TAi��Ai�hAit�Ait�Ai\)Ai33Ai%Ah�jAh��Ah�uAh9XAh�Ah  Ag��Ag�TAg�FAgVAf�+Ae��Ae|�Ad��Ad�Ad  Ac�Aa�Aa
=A`=qA_��A_�A_�hA_�7A_|�A_�A_dZA_G�A_&�A_�A_�A^��A^�RA^�9A^bA]�A\E�AZ��AZbAY��AYl�AX��AX�\AXE�AX1AW�AW�wAW��AW�AWx�AWK�AW�AV�`AV�AV��AV�AV��AV��AV�DAVM�AV1'AV�AU��AU�TAU��AU�^AU�FAU��AU�AUp�AUhsAUXAUC�AU33AU�AUAT�yAT�ATĜAT�9ATn�ATffATVAT^5AT5?AT(�AT �AT1AT1AS��AS��AS�AS�mAS�TAS�
AS�
AS��AS�-AS�PAS\)AS33AR�/ARffAQ�#AQ|�AQAP��API�AO�AO��AOK�AO�AN�HAN�jAN��ANI�AN9XAM��AM�AM7LAL�HALjALA�ALJAK��AKAK�hAK|�AKdZAK\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�$B	��B	��B	�dB	��B	��B	�0B	�^B	��B	��B	�XB	��B	�XB	�XB	�XB	�XB	�XB	�$B	��B	��B	�RB	�B	�B	�zB	��B	��B	�qB	��B	�B	ںB	�VB	��B
�B
YB
�B
�B
+B
Q�B
qvB
�fB
�B
��B
��B4nBncB��B��B��B�-B�[B�wB��B��BɺB�)B�0B�6B�B��B��B��B�JB iB��B��B�rB�	B�2BBhBDB��B�5B�B�B��Bz�BZ�B�B
��B
�jB
��B
��B
��B
��B
�_B
I�B
�B	��B	��B	��B	g8B	C�B	>wB	9�B	33B	+�B	)�B	&�B	1B	�B	hB	VB	�B	�B	�B	 B	oB	�B	SB	 �B	,�B	3�B	B�B	YKB	r|B	�B	��B	��B	��B	�B	��B	��B	�B	��B	��B
�B
�B
	B
�B
�B
~B
�B
1�B
5B
;dB
:^B
=B
A�B
:�B
6FB
3hB
-CB
&�B
$�B
!B
�B
OB
!B
 'B
�B
"�B
(XB
*eB
*�B
.�B
0�B
2�B
6�B
7�B
7�B
;dB
;�B
<jB
<B
<B
;�B
=qB
>wB
=�B
>�B
>�B
>wB
>�B
>wB
=qB
>�B
=�B
=B
=qB
;0B
7�B
8�B
8B
<B
:�B
;�B
<jB
<B
9�B
;0B
;0B
;�B
=<B
>B
>BB
A�B
DgB
C�B
CaB
C�B
E9B
C�B
A�B
A B
B'B
C�B
D3B
B�B
B�B
DgB
D3B
C�B
CaB
C�B
CaB
B�B
@OB
?�B
?�B
?�B
?HB
?HB
?�B
?�B
?HB
?HB
?}B
?�B
?�B
?�B
?}B
?�B
@B
@�B
@�B
A�B
A�B
AUB
A�B
?�B
?}B
>�B
>wB
>�B
?�B
>wB
=�B
=�B
=B
<�B
=<B
:�B
:�B
9$B
3�B
1�B
.�B
-B
)�B
'�B
&LB
'B
%�B
$tB
#nB
#B
"4B
!bB
 \B
VB
~B
�B
xB
CB
7B
�B
�B
�B
�B
1B
�B
�B
_B
�B
_B
�B
�B
�B
B
B
{B
�B
B
4B
�B
�B
hB
B
�B
(B
�B
�B
�B
�B
�B
.B
 B
�B
�B
4B
hB
�B
bB
bB
�B
�B
\B
�B
"B
�B
PB
\B
VB
�B
JB
JB
B
B
B
�B
�B
~B
JB
�B
�B
B
B
�B
xB
B
B

	B
"B

�B
DB
xB
�B
DB
B
DB
B

�B
B
B
B

rB
�B
�B
xB
DB
�B
xB
B
B

�B
B

�B

�B
�B
JB
�B
�B
xB
B
JB
�B
�B
~B
�B
PB
~B
JB
�B
JB
JB
JB
JB
B
�B
JB
�B
�B
�B
�B
PB
�B
�B
�B
\B
.B
�B
�B
4B
�B
�B
 B
�B
 B
�B
�B
4B
 B
4B
�B
�B
�B
hB
4B
�B
�B
:B
oB
B
uB
�B
oB
B
:B
�B
�B
�B
uB
�B
{B
�B
{B
B
�B
B
B
MB
�B
�B
SB
B
�B
�B
�B
�B
YB
�B
+B
�B
�B
+B
�B
_B
_B
�B
�B
�B
�B
�B
1B
eB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
_B
7B
�B
B
�B
B
�B
�B
B
!B
�B
�B
OB
VB
VB
 'B
 �B
 \B
 'B
 �B
 �B
!-B
!�B
"4B
"�B
#:B
$B
$B
$�B
%FB
%FB
%zB
%zB
%FB
%�B
&B
&LB
&�B
'B
&�B
'B
'�B
($B
(�B
(�B
(XB
(�B
*0B
)�B
)�B
)�B
)�B
*�B
*0B
*�B
*�B
+B
+�B
+�B
,�B
,�B
+�B
+�B
,qB
,B
,=B
,B
,qB
,�B
,�B
-CB
-wB
-�B
-�B
.B
.B
-�B
.B
.B
-�B
.IB
.}B
.�B
.�B
/OB
/B
/OB
/�B
/�B
0!B
0UB
0UB
0�B
0�B
0�B
0�B
1'B
1�B
1�B
1�B
1�B
1�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
4�B
5tB
6�B
6zB
6zB
6zB
7B
6�B
7LB
7�B
7�B
7�B
7�B
7�B
8RB
8�B
9XB
9XB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:^B
:�B
:�B
:�B
:�B
:�B
;0B
:�B
;0B
;dB
;0B
<jB
<6B
<B
<6B
=B
=B
=�B
=qB
>B
>wB
>�B
>�B
?B
?HB
>�B
@OB
@OB
@�B
@�B
A B
A�B
A�B
B'B
A�B
A�B
B�B
B�B
CaB
C-B
CaB
D3B
DgB
EB
E�B
E�B
EmB
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FtB
HKB
H�B
H�B
HKB
H�B
H�B
IRB
IB
J#B
J�B
J�B
JXB
J�B
J�B
K�B
J�B
K^B
K)B
K^B
K�B
K�B
L0B
K�B
K�B
L0B
L0B
K�B
M6B
MjB
M�B
MjB
M�B
MjB
M�B
N�B
N�B
N�B
N�B
N�B
OB
OB
N�B
OBB
PB
O�B
QB
QB
QNB
QB
QNB
Q�B
Q�B
Q�B
QNB
Q�B
R B
RTB
R�B
R�B
R�B
S[B
S�B
S�B
S�B
T�B
T�B
U2B
U2B
U2B
T�B
UgB
U�B
U�B
VB
V9B
V�B
W
B
W
B
WsB
XEB
XB
XEB
X�B
YKB
YKB
YKB
YB
Y�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
[�B
\�B
]�B
]�B
]�B
^5B
^B
^B
^�B
^5B
^�B
^�B
^�B
_;B
`BB
`vB
`BB
`vB
`BB
`�B
aB
aB
a|B
bB
a�B
bNB
b�B
b�B
b�B
c B
cTB
c�B
c�B
dZB
d�B
dZB
d�B
e�B
f2B
e�B
ffB
gB
g8B
g8B
g�B
g�B
g�B
gmB
g�B
g�B
h
B
hsB
hsB
h�B
h�B
iyB
jB
i�B
j�B
jB
jKB
jB
jB
j�B
j�B
j�B
k�B
lWB
lWB
l�B
lWB
l�B
lWB
l�B
l�B
l�B
l�B
m]B
m)B
m]B
m�B
m�B
m�B
m�B
n�B
o B
oiB
o�B
pB
pB
o�B
pB
pB
pB
poB
p�B
qB
qvB
qAB
q�B
q�B
q�B
r|B
r�B
sB
r�B
sMB
r�B
s�B
r�B
s�B
s�B
s�B
s�B
tTB
t�B
t�B
t�B
u%B
v+B
v`B
v`B
v�B
w2B
w�B
v�B
wfB
v�B
wfB
v�B
w2B
w2B
w2B
w�B
wfB
w�B
x8B
x�B
x�B
x�B
y>B
yrB
yrB
y>B
y�B
zB
zB
z�B
{JB
z�B
{�B
{B
{�B
{�B
{B
{�B
|B
|PB
|�B
|�B
|�B
}"B
}"B
}"B
}�B
}�B
}�B
}�B
}�B
}�B
}VB
~(B
}�B
~�B
~]B
.B
�B
~�B
~�B
�B
.B
�B
�B
�B
�B
.B
cB
�iB
� B
�iB
��B
�4B
��B
��B
�B
��B
�AB
�oB
�AB
�AB
��B
��B
��B
�{B
��B
�GB
��B
�B
�GB
��B
��B
��B
��B
�MB
�MB
��B
�SB
��B
��B
��B
��B
�SB
��B
��B
�%B
��B
��B
�YB
��B
��B
��B
��B
��B
��B
�1B
�_B
�1B
��B
�1B
�1B
�B
�fB
��B
��B
�lB
��B	��B	��B	��B	��B	�XB	��B	��B	��B	��B	��B	��B	��B	�6B	��B	��B	��B	�B	��B	�zB	��B	�B	��B	��B	��B	�zB	��B	�6B	��B	�^B	�dB	��B	�XB	�^B	��B	��B	��B	��B	�$B	��B	�RB	��B	�^B	��B	�$B	��B	��B	�$B	��B	��B	�^B	��B	��B	�^B	��B	�$B	�*B	�B	��B	��B	��B	�*B	�XB	�RB	�*B	�XB	��B	�*B	��B	��B	��B	�XB	�RB	�^B	��B	��B	��B	��B	��B	�*B	�XB	�RB	�^B	��B	�B	�XB	�*B	�B	�$B	�*B	�B	��B	��B	��B	�$B	�*B	��B	�RB	��B	��B	�$B	��B	�RB	�$B	�*B	�B	��B	��B	�RB	��B	��B	�$B	��B	��B	�$B	��B	��B	�XB	�LB	�RB	�$B	�LB	��B	�XB	��B	��B	��B	�RB	�B	��B	��B	�B	��B	��B	��B	�FB	��B	��B	�zB	��B	�tB	�B	�zB	�?B	��B	�B	�B	�3B	��B	�nB	�3B	��B	��B	�[B	�'B	��B	�}B	�UB	��B	�IB	��B	�B	��B	��B	�eB	�qB	�}B	�IB	�qB	��B	��B	�qB	��B	�9B	��B	��B	��B	��B	�dB	�[B	��B	�B	�tB	�B	�B	�&B	�HB	�B	�B	�DB	�mB
�B
�B
uB	��B	�]B	�cB	��B	�VB	�(B	�.B
  B
�B
�B
�B
;B
�B
�B
�B
YB
%B
�B
�B
%B
�B
�B
%B
�B
�B
_B
�B
�B
fB
YB
�B
�B
�B
�B
�B
�B
�B
B
�B
VB
@B
%B
9$B
4nB
I�B
=�B
E�B
FtB
I�B
K^B
N�B
U2B
XyB
Y�B
[#B
YKB
YB
YKB
X�B
XyB
Z�B
^B
^�B
_pB
aHB
e,B
jKB
iB
jB
kB
v�B
v�B
u�B
xB
v�B
xB
y�B
|�B
}�B
�B
.B
��B
�B
�;B
��B
��B
�B
��B
�GB
��B
��B
��B
��B
��B
�1B
��B
�B
�	B
��B
�~B
��B
��B
��B
�uB
�VB
��B
��B
�bB
�@B
��B
�B
��B
�xB
�	B
��B
��B
�@B
�-B
�nB
��B
�hB
�B
�zB
��B
��B
��B
��B
�B
��B
�CB
�wB
��B
�$B
��B
�FB
��B
�^B
�B
�pB
ȀB
�B
�[B
�B
��B
�[B
�vB
��B
�vB
��B
�|B
�DB
�+B
�lB
�oB
�2B
��B.B�BkB�BA�BV�BV�BXB[#Bc�Bd�Bc Ba�Ba�Bg�Bl�Bm�BlWBo5Bt�B��B��B�+B��B��B�7B��B��B��B�B�4B��B��B�B��B�0B�[B�B�!B�!B��B�?B�OB�nB��B�3B�[B�!B��B��B��B�?B��B�'B��B�9B��B��B��B�aB��B�hB��B�UB�'B�[B�wB�UB��B��B�OB��B��B�B�IB�qB��B�qB��B�0B��B��B�BB�B�6B�;B��BĜB�6B�mB�EBǮB��B�tB�EB��B�6B�B�KBɺB�#B��B�)B�#B�RBʌB�6B��B�^B�jB�dB��B��B��B�^B��B�dB��B�0B�0B��B�0B�dB�6B��B�B�B�vB�B�[B�/BیBچB�dBޞB��B��B�oB�>B�B�|B��B�)B�5B�+B�lB��B�	B�]B�B�8B�+B�B�DB��B�8B�B�"B��B�rB��B�B�	B�PB��B �B iB��B��BB �B��B��B��B��B��BSB_B  B��B�	B�B��B��B��B��B�lB��B�fB��B��B�lB�DB�B�VB�B��B��B��B�ZB�B�|B�B�B��B��B�+B��B�VB�B��B
�B�BJB�BBuB@BBB�B BbB�B�B�BbBVB~B4B�B  B�BB�"B�JB�PB��B��B��B��B��B��B�;B��BuB��B�BܒB��B�BںB�0B˒BуB��BƨB��BŢB�B��B�aB��B��B�nB�B��B�B�!B�	B��B��B��B�Bs�Br�Bm]Bi�Bm)Bh�Be,B]�B^jBbBh�B\�B[WB%BB�B�B�B�B.B\B1B
��BYB
��B
�%B
� B
�B
��B
��B
��B
ݘB
�9B
��B
��B
��B
�aB
�4B
�MB
�B
��B
�:B
��B
�7B
��B
��B
�1B
�	B
�~B
�rB
��B
�7B
�rB
�xB
�	B
�~B
�7B
�7B
��B
��B
�B
�SB
�SB
�MB
�{B
�B
�B
��B
��B
�B
�GB
�:B
y�B
ncB
hsB
P}B
D�B
.IB
(�B
+�B
'B
�B
B
�B	�VB	�B	�B	��B	�%B	�8B	�B	�B	ݘB	�TB	�B	��B	�B	��B	��B	�-B	��B	�B	��B	�rB	��B	��B	{B	��B	��B	ZQB	O�B	H�B	E�B	E�B	GB	C�B	B�B	@�B	A B	B[B	?B	>wB	>BB	=B	:�B	<�B	:^B	9$B	8RB	6�B	0�B	9�B	c�B	33B	7�B	0�B	-�B	,=B	2�B	+�B	+6B	-B	*�B	3�B	+6B	'RB	0UB	,B	)*B	)�B	&�B	%B	(�B	.�B	+B	@�B	)�B	#nB	#�B	'B	.B	4�B	0UB	0�B	(�B	+B	&LB	&LB	(�B	$@B	%�B	"�B	$B	&B	VB	!�B	 'B	!B	(�B	!-B	=qB	+kB	/B	~B	%B	$�B	$�B	!B	7B	7B	�B	�B	YB	�B	�B	�B	�B	�B	_B	�B	�B	{B	�B	YB	�B	�B	YB	�B	�B	�B	�B	�B	B	�B	:B	@B	4B	4B	�B	�B	�B	�B	�B	B	 B	�B	�B	�B	�B	"B	VB	4B	"B	(B	"B	�B	PB	�B	PB	
rB	�B	�B	�B	�B	
=B	hB	�B	�B	(B	�B	B	"B	"B	�B	�B	�B	.B	�B	�B	�B	B		lB	7B		lB	�B	JB	�B	~B	B	VB	�B	�B	B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  B	�*B	�xB	��B	� B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�rB	�>B	�$B	�$B	�lB	�8B	�RB	��B	�tB	��B	�B	�B	��B	�wB
�B
�B
tB
�B
�B
6B
*�B
\xB
yXB
��B
�eB
ΥB
�MB;�B{�B�CB��B�?B�hB�3B�LB�B�7B�=B˒B��BбB�hB��B�}BAB �BMB�B��B�B��B�FBdBuBbBmB��B�B�dB�B�)Bn�B%zB�B
� B
��B
��B
��B
��B
��B
]dB
�B	��B	ȴB	�?B	p�B	F�B	B�B	H�B	?B	A�B	7�B	5ZB	B	_B	2B	�B	�B	�B	�B	�B	�B	�B	"�B	(>B	2GB	:�B	I7B	`�B	xB	��B	��B	��B	�8B	�DB	�FB	�&B	��B	��B
�B
	7B
	�B
	7B
B
�B
 �B
�B
2�B
7LB
=�B
9�B
=�B
E�B
<�B
8lB
6�B
0oB
+QB
($B
"4B
5B
 �B
!�B
!�B
"�B
&�B
*�B
,B
,�B
0�B
2�B
4�B
7�B
8RB
:DB
=qB
<�B
<�B
<�B
<�B
=�B
?�B
AoB
>wB
?}B
?�B
?�B
@iB
?cB
>�B
@iB
?HB
>�B
?}B
<�B
8�B
9	B
8lB
=B
;�B
<�B
=�B
=<B
:�B
<PB
<B
<�B
>wB
>]B
>�B
B�B
EmB
D�B
D�B
E�B
G+B
D�B
BAB
AoB
B�B
D�B
D�B
C�B
C�B
EmB
EB
C�B
C�B
D�B
EB
C�B
A;B
@iB
@OB
?�B
?�B
@OB
@�B
@OB
?�B
?�B
@OB
@�B
@B
?�B
?�B
@OB
@�B
@�B
A B
BB
BuB
C-B
C�B
A B
@iB
?}B
?HB
@4B
A B
>�B
>�B
>(B
=�B
>�B
>�B
;�B
<B
;JB
4�B
2�B
0oB
/ B
,=B
)�B
(sB
)B
&�B
$�B
#�B
$&B
#�B
"�B
!�B
 vB
B
jB
B
OB
�B
�B
�B
�B
�B
kB
�B
YB
B
�B
B
_B
mB
,B
�B
�B
�B
�B
�B
�B
 B
B
TB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
 B
oB
TB
�B
B
�B
�B
.B
B
BB
pB
VB
.B
�B
�B
jB
�B
�B
�B
�B
dB
~B
�B
PB
�B
�B
dB
�B
dB
B
�B
�B
B
6B
(B
�B
�B
B
�B
�B
�B
�B
^B
DB
�B
�B
�B
�B
PB
�B
JB
�B
JB
�B
DB
)B

�B
B

�B
B
�B
�B
JB
~B
~B
"B
~B
0B
�B
�B
�B
"B
B
�B
B
�B
�B
�B
B
�B
�B
�B
B
jB
"B
vB
�B
�B
�B
(B
.B
�B
�B
�B
�B
�B
oB
4B
B
4B
B
hB
B
:B
B
[B
TB
�B
�B
�B
 B
TB
�B
[B
�B
,B
B
@B
�B
�B
B
&B
�B
�B
�B
�B
{B
MB
MB
�B
�B
�B
�B
mB
�B
�B
�B
$B

B

B
�B
�B
1B
EB
B
�B
_B
EB
�B
�B
�B
1B
eB
B
�B
7B
B
KB
1B
B
1B
�B
eB
KB
�B
7B
B
�B
�B
WB
�B
�B
�B
xB
IB
B
pB
 'B
VB
B
�B
�B
 �B
!HB
 �B
 �B
!�B
!HB
"B
"�B
#B
#TB
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'mB
'RB
'8B
'�B
(�B
(�B
)DB
)DB
(�B
*B
*�B
)�B
*KB
*eB
*�B
+QB
*�B
+6B
+�B
+�B
,�B
,�B
-�B
-)B
,�B
-)B
-�B
,�B
,qB
,=B
,�B
-)B
-�B
-�B
./B
.cB
.cB
.}B
.IB
.B
.�B
.IB
.IB
/ B
/5B
/�B
/iB
/�B
/�B
/�B
0;B
0�B
0oB
0�B
0�B
0�B
0�B
0�B
1AB
1�B
2aB
2-B
2-B
2-B
2�B
4nB
3�B
3�B
3�B
3�B
3�B
4�B
5�B
6�B
6�B
6�B
6�B
6�B
7�B
7LB
7�B
7�B
7�B
8B
8B
8�B
9>B
9>B
:*B
:B
:�B
:*B
9�B
:DB
9�B
:B
:B
:DB
:xB
:�B
:�B
;0B
;JB
;�B
;�B
;�B
;0B
;B
;�B
;�B
="B
<PB
<6B
<�B
=�B
=�B
>(B
=�B
>�B
>�B
>�B
?B
?}B
?�B
?�B
@�B
@�B
@�B
AB
A�B
A�B
B[B
BAB
A�B
BuB
CaB
CGB
C�B
C�B
C�B
D�B
EB
E�B
F%B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FYB
G�B
IB
IB
H�B
H�B
IB
I�B
I�B
I�B
J�B
KDB
J�B
J�B
KB
K)B
LB
K�B
K�B
KxB
K�B
K�B
L0B
LJB
LB
L0B
LdB
LdB
L�B
M�B
M�B
M�B
M�B
M�B
M�B
NpB
OvB
OB
N�B
N�B
N�B
O(B
OBB
OBB
PB
P}B
P�B
Q�B
QhB
QhB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
R�B
SB
S&B
R�B
S[B
S�B
S�B
TFB
T�B
UMB
U2B
UMB
UMB
UMB
UgB
U�B
VB
VB
VSB
V�B
W
B
WYB
W�B
W�B
X_B
XEB
X�B
YeB
Y�B
YeB
YeB
YB
ZQB
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
[�B
\]B
]�B
]�B
]�B
^B
^OB
^B
^OB
^�B
^jB
^�B
_!B
_pB
`B
`�B
`�B
`\B
`�B
`�B
a-B
abB
a|B
bB
bNB
bNB
b�B
b�B
b�B
c B
cTB
c�B
dB
d@B
e`B
eFB
d�B
eFB
e�B
f2B
e�B
f�B
gB
gRB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
iB
i�B
j0B
jKB
kB
j�B
j�B
j�B
j�B
j�B
kkB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
mB
mCB
m)B
m�B
mCB
mwB
m�B
m�B
m�B
n/B
oB
oiB
o�B
o�B
pB
pB
o�B
p!B
p;B
pUB
p�B
p�B
qvB
q�B
q�B
raB
q�B
r-B
sB
sB
s3B
r�B
shB
r�B
tB
s3B
tB
s�B
tB
t9B
tnB
t�B
t�B
t�B
utB
vzB
vzB
v�B
v�B
w�B
w�B
wB
w�B
wB
wfB
wB
w2B
wLB
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
yXB
yrB
yrB
yrB
y�B
zDB
zxB
{B
{JB
z�B
{�B
{0B
{�B
{�B
{�B
|PB
|PB
|jB
|�B
|�B
}B
}<B
}<B
}<B
}�B
}�B
}�B
}�B
}�B
~B
}�B
~]B
~B
B
~�B
HB
�B
~�B
~�B
�B
cB
�B
�B
�B
�B
}B
�B
��B
�4B
��B
��B
��B
�UB
�'B
�'B
��B
�AB
��B
��B
�uB
�B
��B
��B
�{B
��B
�aB
��B
�MB
��B
�B
�B
��B
�B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�?B
�B
��B
��B
�B
��B
�B
��B
�B
�B
�KB
�zB
�1B
��B
��B
�fB
�B
��B
��B
�B
�lG�O�B	��B	��B	��B	��B	�XB	��B	��B	��B	��B	��B	��B	��B	�6B	��B	��B	��B	�B	��B	�zB	��B	�B	��B	��B	��B	�zB	��B	�6B	��B	�^B	�dB	��B	�XB	�^B	��B	��B	��B	��B	�$B	��B	�RB	��B	�^B	��B	�$B	��B	��B	�$B	��B	��B	�^B	��B	��B	�^B	��B	�$B	�*B	�B	��B	��B	��B	�*B	�XB	�RB	�*B	�XB	��B	�*B	��B	��B	��B	�XB	�RB	�^B	��B	��B	��B	��B	��B	�*B	�XB	�RB	�^B	��B	�B	�XB	�*B	�B	�$B	�*B	�B	��B	��B	��B	�$B	�*B	��B	�RB	��B	��B	�$B	��B	�RB	�$B	�*B	�B	��B	��B	�RB	��B	��B	�$B	��B	��B	�$B	��B	��B	�XB	�LB	�RB	�$B	�LB	��B	�XB	��B	��B	��B	�RB	�B	��B	��B	�B	��B	��B	��B	�FB	��B	��B	�zB	��B	�tB	�B	�zB	�?B	��B	�B	�B	�3B	��B	�nB	�3B	��B	��B	�[B	�'B	��B	�}B	�UB	��B	�IB	��B	�B	��B	��B	�eB	�qB	�}B	�IB	�qB	��B	��B	�qB	��B	�9B	��B	��B	��B	��B	�dB	�[B	��B	�B	�tB	�B	�B	�&B	�HB	�B	�B	�DB	�mB
�B
�B
uB	��B	�]B	�cB	��B	�VB	�(B	�.B
  B
�B
�B
�B
;B
�B
�B
�B
YB
%B
�B
�B
%B
�B
�B
%B
�B
�B
_B
�B
�B
fB
YB
�B
�B
�B
�B
�B
�B
�B
B
�B
VB
@B
%B
9$B
4nB
I�B
=�B
E�B
FtB
I�B
K^B
N�B
U2B
XyB
Y�B
[#B
YKB
YB
YKB
X�B
XyB
Z�B
^B
^�B
_pB
aHB
e,B
jKB
iB
jB
kB
v�B
v�B
u�B
xB
v�B
xB
y�B
|�B
}�B
�B
.B
��B
�B
�;B
��B
��B
�B
��B
�GB
��B
��B
��B
��B
��B
�1B
��B
�B
�	B
��B
�~B
��B
��B
��B
�uB
�VB
��B
��B
�bB
�@B
��B
�B
��B
�xB
�	B
��B
��B
�@B
�-B
�nB
��B
�hB
�B
�zB
��B
��B
��B
��B
�B
��B
�CB
�wB
��B
�$B
��B
�FB
��B
�^B
�B
�pB
ȀB
�B
�[B
�B
��B
�[B
�vB
��B
�vB
��B
�|B
�DB
�+B
�lB
�oB
�2B
��B.B�BkB�BA�BV�BV�BXB[#Bc�Bd�Bc Ba�Ba�Bg�Bl�Bm�BlWBo5Bt�B��B��B�+B��B��B�7B��B��B��B�B�4B��B��B�B��B�0B�[B�B�!B�!B��B�?B�OB�nB��B�3B�[B�!B��B��B��B�?B��B�'B��B�9B��B��B��B�aB��B�hB��B�UB�'B�[B�wB�UB��B��B�OB��B��B�B�IB�qB��B�qB��B�0B��B��B�BB�B�6B�;B��BĜB�6B�mB�EBǮB��B�tB�EB��B�6B�B�KBɺB�#B��B�)B�#B�RBʌB�6B��B�^B�jB�dB��B��B��B�^B��B�dB��B�0B�0B��B�0B�dB�6B��B�B�B�vB�B�[B�/BیBچB�dBޞB��B��B�oB�>B�B�|B��B�)B�5B�+B�lB��B�	B�]B�B�8B�+B�B�DB��B�8B�B�"B��B�rB��B�B�	B�PB��B �B iB��B��BB �B��B��B��B��B��BSB_B  B��B�	B�B��B��B��B��B�lB��B�fB��B��B�lB�DB�B�VB�B��B��B��B�ZB�B�|B�B�B��B��B�+B��B�VB�B��B
�B�BJB�BBuB@BBB�B BbB�B�B�BbBVB~B4B�B  B�BB�"B�JB�PB��B��B��B��B��B��B�;B��BuB��B�BܒB��B�BںB�0B˒BуB��BƨB��BŢB�B��B�aB��B��B�nB�B��B�B�!B�	B��B��B��B�Bs�Br�Bm]Bi�Bm)Bh�Be,B]�B^jBbBh�B\�B[WB%BB�B�B�B�B.B\B1B
��BYB
��B
�%B
� B
�B
��B
��B
��B
ݘB
�9B
��B
��B
��B
�aB
�4B
�MB
�B
��B
�:B
��B
�7B
��B
��B
�1B
�	B
�~B
�rB
��B
�7B
�rB
�xB
�	B
�~B
�7B
�7B
��B
��B
�B
�SB
�SB
�MB
�{B
�B
�B
��B
��B
�B
�GB
�:B
y�B
ncB
hsB
P}B
D�B
.IB
(�B
+�B
'B
�B
B
�B	�VB	�B	�B	��B	�%B	�8B	�B	�B	ݘB	�TB	�B	��B	�B	��B	��B	�-B	��B	�B	��B	�rB	��B	��B	{B	��B	��B	ZQB	O�B	H�B	E�B	E�B	GB	C�B	B�B	@�B	A B	B[B	?B	>wB	>BB	=B	:�B	<�B	:^B	9$B	8RB	6�B	0�B	9�B	c�B	33B	7�B	0�B	-�B	,=B	2�B	+�B	+6B	-B	*�B	3�B	+6B	'RB	0UB	,B	)*B	)�B	&�B	%B	(�B	.�B	+B	@�B	)�B	#nB	#�B	'B	.B	4�B	0UB	0�B	(�B	+B	&LB	&LB	(�B	$@B	%�B	"�B	$B	&B	VB	!�B	 'B	!B	(�B	!-B	=qB	+kB	/B	~B	%B	$�B	$�B	!B	7B	7B	�B	�B	YB	�B	�B	�B	�B	�B	_B	�B	�B	{B	�B	YB	�B	�B	YB	�B	�B	�B	�B	�B	B	�B	:B	@B	4B	4B	�B	�B	�B	�B	�B	B	 B	�B	�B	�B	�B	"B	VB	4B	"B	(B	"B	�B	PB	�B	PB	
rB	�B	�B	�B	�B	
=B	hB	�B	�B	(B	�B	B	"B	"B	�B	�B	�B	.B	�B	�B	�B	B		lB	7B		lB	�B	JB	�B	~B	B	VB	�B	�B	B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<3D�<�˔<�H�<#�
<#�
<#�
<#�
<#�
<#�
<�z�<9�1<#�
<#�
<]��<���<��K<#�
<_S�<�e#<#�
<#�
<#�
<#�
<(g�<0�<#�
<#�
<#�
<#�
<#�
<#�
<%P�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<9�1<O�~<��[<��<��\<gE<���<���<#�
<#�
<#�
<I2T=.?<�'n<)�<�'n<���<���<&�<#�
<#�
<w3�<G�l<�Q<f�x<u=�<#�
<#�
<#�
<#�
<[�L<#�
<#�
<#�
<#�
<#�
<K�"<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018100904413320181009044133IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018101907010620181019070106QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018101907010620181019070106QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551120190521075511IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                