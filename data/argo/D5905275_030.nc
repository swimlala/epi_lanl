CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-11-07T23:37:55Z creation; 2023-04-26T19:14:27Z DMQC;      
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
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20181107233755  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_030                 7316_008644_030                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @؎�� ѷ@؎�� ѷ11  @؎��)_@؎��)_@)�I�V��@)�I�V���d���a�d���a11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @G�@�  @�  @�  @޸R@�p�A\)A   A,��A@��A`  A�  A�Q�A�Q�A��A��A�  A߮A�A��B�
B�
B  B�
B(  B0(�B8(�B@(�BHQ�BP(�BXQ�B`  Bh  Bp  Bx  B�  B�{B�  B�  B�  B�  B�{B�{B�{B�  B�  B�  B��B�  B�  B�  B�{B�{B�  B��B�{B�  B��B�  B��B��B��B��B��B��B�  B�  B��C��C��C  C  C
  C��C  C{C  C  C  C  C  C  C��C 
=C"  C#��C%��C'�C)��C,  C-��C/�C2  C4
=C6
=C8
=C:  C;��C>
=C@  CA��CC��CE�CH  CJ
=CL
=CN  CP  CR  CT  CV
=CX
=CZ{C\  C]��C`  Cb
=Cd
=Cf  Ch
=Cj
=Cl{Cn  Co��Cr
=Ct{Cv  Cx  Cz{C|{C~  C�  C�  C���C�  C�C�  C�  C�C�C�C�C���C�  C�
=C�  C�  C�C�  C�  C�  C���C���C�  C�  C�  C�  C�C�C���C���C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�  C���C���C���C�  C�  C�  C�  C���C���C�  C�  C�C�C�  C���C�  C�C�C���C���C���C���C�  C�  C���C�  C���C���C���C���C���C�  C�C�
=C�C�  C�  C���C�  C�C�C���C���C�  C�  C���C�  C�  C���C���C�  C�
=C�C�
=C�  C�  C�
=C�C���C�  C�  C�C�  C�  C�  C�C�
=C�C�  C�  C�  C�C�C�  C�  C���C���C���C���C�  C���C���C�  C���C���C�  D   D � D �qD}qD�qD� D�D� D�D��D  D��D�D�D  Dz�D�qD��D	  D	� D
�D
� D  D}qD�qDz�D�qD��D  D� D�D� D  D}qD  D��D�D��D  D}qD  D��D�D��D�D�D�D}qD  D� D�qD}qD�D��D�D��D  D}qD��D}qD  D}qD�qD� D   D ��D!  D!}qD!�qD"� D"�qD#z�D$  D$}qD$�qD%� D&  D&}qD'  D'}qD'�qD(� D)  D)� D*�D*}qD+  D+�D,�D,}qD-  D-��D.  D.z�D.�qD/}qD/��D0� D1  D1� D2  D2}qD2�qD3}qD4  D4� D4��D5z�D5�qD6}qD6�qD7� D8  D8� D9  D9��D:D:��D:�qD;z�D;��D<}qD<�qD=}qD=�qD>� D>��D?}qD@�D@� D@�qDA� DB�DB}qDC  DC� DC�qDD� DE  DE� DF  DFz�DF�qDG� DH�DH�DI  DI��DJDJ� DK  DK��DK��DLz�DL��DM}qDM�qDN}qDN�qDO� DP�DP� DQ�DQ�DR�DR}qDS  DS��DT  DT� DU�DU}qDV  DV��DV�qDW� DX�DX}qDX��DY}qDZ  DZ� D[�D[z�D\  D\� D]�D]� D^  D^� D_  D_��D_�qD`� Da�Da� Db  Db��Dc�Dc��DdDd�Dd�qDe� Df  Df� Dg  Dg��DhDh� Di�Di��Dj�Dj��DkDk�Dl  Dl��Dm�Dm}qDn  Dn� Do  Do� Do��Dp}qDq�Dq��Dr�Dr��Ds�Ds� Dt  Dt��Du  Du� Dv  Dv}qDw  Dw��Dx  Dx� Dx�qDy}qDy�qDz}qD{  D{� D|�D|� D}  D}� D~�D~��D~�qD}qD�qD�>�D�}qD���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��HD��HD�HD�=qD�}qD�� D�HD�>�D�� D�� D���D�>�D��HD�� D���D�@ D�� D�� D���D�@ D�� D��HD�HD�>�D�~�D�� D���D�@ D��HD�� D�HD�C�D��HD�� D���D�>�D�� D��HD�HD�=qD�}qD�� D�HD�@ D�� D��HD�  D�>�D�� D�� D���D�AHD��HD��HD��D�@ D�~�D���D���D�@ D�~�D���D�HD�@ D�� D�� D�  D�@ D��HD��HD�  D�AHD��HD�� D���D�@ D��HD�� D�HD�AHD�� D��HD��D�@ D��HD��HD�  D�@ D�~�D���D���D�=qD�� D�D�  D�@ D��HD��HD�HD�B�D�� D��qD���D�>�D�� D�� D�HD�AHD�� D���D�HD�@ D��HD��HD���D�AHD�� D���D�  D�AHD�� D�� D���D�>�D�~�D��qD���D�AHD�� D�� D���D�>�D��HD�D��D�@ D��HD�� D�HD�B�D��HD���D�  D�>�D�� D�D�HD�>�D�� D�� D���D�@ D��HD��HD�HD�B�D�� D�� D��D�@ D�~�D���D��D�B�D�� D��HD�HD�AHD���D��HD�  D�@ D�� D��HD�HD�AHD��HD��HD�HD�AHD�� D�� D���D�>�D�� D�� D�HD�AHD��HD�D�HD�@ D���D�D�HD�>�D�� D��HD�HD�AHD��HD���D�  D�AHD�� D��qD���D�@ D��HD�D�  D�>�D�� D�� D�HD�AHD�� D�� D�HD�>�D�� D�� D���D�@ D��HD���D���D�@ D�~�D���D�  D�AHD��HD�� D�HD�AHD�� D�� D���D�>�D�� D���D�  D�AHD�� D�� D�  D�@ D��HD��HD���D�@ DHD�� D���D�@ DÁHD�� D���D�@ DāHD�� D�  D�@ Dŀ D�� D�  D�@ DƁHD��HD�  D�AHDǁHD�� D���D�>�D�~�D�� D�  D�>�Dɀ D��HD�  D�@ D�~�Dʾ�D�  D�AHDˀ D��HD�  D�AHD́HD̾�D��qD�>�D́HD��HD�  D�@ D�~�D�� D�  D�@ Dπ D�� D�  D�@ DЀ D��HD�  D�@ D�~�DѾ�D�  D�@ D�~�DҾ�D��qD�>�DӀ D�D��D�AHDԀ DԽqD�  D�AHDՁHD��HD�HD�@ D�~�DֽqD���D�@ D�~�D׽qD�  D�>�D�~�D�� D�  D�>�D�~�D��HD�HD�AHDڀ D�� D�  D�@ D�~�D۾�D���D�>�D܁HD��HD�  D�@ D݁HD�� D�HD�>�D�~�D��HD�HD�>�D�~�D�� D���D�>�D�� DྸD�  D�@ DႏD�� D���D�@ D� D�� D��qD�@ D�~�D�� D���D�@ D� D�D�HD�=qD�~�D��HD�  D�>�D� D�� D���D�@ D�~�D�qD�  D�@ D� D�� D�  D�AHD�HD龸D�  D�@ D�~�D꾸D�  D�@ D� D�� D�  D�AHD� D�D�HD�AHD킏D��HD�HD�AHD�HD�� D�HD�B�D�HD��HD�  D�@ D�� D�� D�HD�B�D�HD��HD�  D�@ D�~�D�� D�HD�B�D�HD�D�  D�@ D� D�� D�  D�@ D��HD��HD���D�@ D�� D��qD�  D�AHD�� D�� D�  D�>�D�� D��HD�  D�@ D�~�D�� D�  D�:�D��>�G�>�?W
=?�\)?\?��@
=q@�R@8Q�@Q�@k�@�  @��@�@��\@���@���@��
@�{@�Q�@��
@��@��HA33AQ�Ap�A33A��A\)A%�A)��A/\)A5�A:�HAAG�AFffAJ�HAP  ATz�AZ=qA^�RAc33Ag
=Aj=qAmp�AqG�Au�Ax��A{�A~{A���A��HA���A�ffA��A���A��HA���A�ffA��A�G�A��HA���A�ffA�  A�G�A�33A��A��RA�Q�A��A�33A��A�
=A���A�=qA�33A���A��RA���A��A�33A���A�ffA�Q�A���A��HA�z�A�ffA�  A�G�A\A�z�A�ffA�  A�G�A��HA�z�A�ffA�Q�A��A�33A��A�
=A���Aڏ\A�(�A�A߮A��A�A��A�ffA�Q�A�\A�z�A�{A�A�G�A��HA��A��RA�Q�A���A��A��A�
=B Q�BG�B�B�RB�B��Bp�B=qB�HB�
B��B	�B
�RB�Bz�Bp�B=qB33B(�BG�B=qB\)BQ�BG�B=qB\)B��B��B�RB�B��BB�HB   B!�B"{B"�HB$  B%G�B&ffB'�B(z�B)p�B*�\B+�
B,��B.=qB/\)B0z�B1p�B2�\B3�
B5�B6ffB7�B8��B9�B;
=B<Q�B=B?
=B@(�BAp�BB�RBD  BEG�BFffBG�
BIG�BJ�\BL  BMp�BN�RBP(�BQG�BR�\BS�BT��BUBV�RBW�BX��BY��BZffB[33B[�
B\z�B]�B]B^�\B_\)B`(�B`��BaBb�\Bc33Bc�
Bd��Bep�Bf=qBg
=Bg�
Bh��Bi��Bj�\Bk\)BlQ�Bm�Bm�Bn�RBo\)Bp(�Bp��BqBr�\Bs\)BtQ�Bu�Bu�Bv�RBw�BxQ�By�By�Bz�RB{�B|Q�B}�B}B~�\B\)B�(�B��\B���B�p�B��
B�Q�B��RB��B��B�  B�z�B��HB�\)B�B�(�B���B�
=B�p�B�  B�ffB��HB�G�B��B�(�B��\B�
=B�p�B��
B�Q�B��RB��B��B��B�ffB��RB�33B���B�  B�ffB��HB�G�B��B�(�B��\B���B�\)B��
B�Q�B��RB��B���B�  B�z�B��HB�G�B�B�=qB���B�
=B��B��B�ffB���B�G�B��B�{B��\B���B�\)B��
B�=qB���B�
=B��B��B�Q�B��RB��B��B��B�ffB���B�33B���B�{B�z�B��HB�G�B��B�{B��\B��HB�G�B��
B�=qB���B���B�\)B��
B�=qB���B�
=B��B�  B�ffB���B�33B��B�{B��\B���B�p�B��
B�Q�B���B�33B��B�{B��\B�
=B��B��B�ffB���B�G�B��B�(�B���B��B��B�  B�ffB���B�p�B��B�ffB��HB�\)B��B�Q�B���B�\)B��
B�ffB��HB�\)B��B�ffB��HB�p�B��B�z�B�
=BÅB�  Bď\B��BŮB�=qBƸRB�G�B�B�=qB���B�G�B��
B�ffB��HB�p�B��B�z�B�
=B͙�B�{BΣ�B��B�B�=qB���B�\)B��Bҏ\B��B�B�Q�B��HB�p�B�{B֣�B�33B׮B�Q�B��HBمB�(�B���B�\)B�  Bܣ�B�33B�B�Q�B���Bߙ�B�=qB��HB�B�(�B��HB�B�{B�RB�\)B�  B�RB�\)B�  B�RB�p�B�{B���B�B�(�B�RB�p�B�{B�RB�\)B�{B��HB�B�(�B���B�p�B�{B��RB�\)B�{B��RB�p�B�{B��RB�\)B�  B��\B�G�B��B���B�\)B�  B��RB�G�B��C =qC �\C �C=qC��C�HC33C�C�
C33C�\C�C=qC�C�
C33C�\C�C=qC�\C�CG�C��C��C=qC�\C�C	G�C	��C	�C
33C
��C
��C=qC�\C�HC=qC�C�
C�Cz�C��C(�C�C�HC33C�C��C�Cz�C��C(�C�C�
C33C�C�
C�Cp�C��C(�C�C��C�Cp�CC{Cp�CC{C\)C��C��CQ�C��C�C=qC�C�
C(�C�C��C�CffC�RC
=CffC�RC
=CQ�C��C��CQ�C��C�C=qC��C�HC (�C z�C �
C!(�C!p�C!C"{C"p�C"C#
=C#Q�C#��C$  C$Q�C$��C$�HC%33C%�C%��C&{C&Q�C&�C'  C'G�C'�\C'�
C((�C(z�C(C)
=C)Q�C)�C*  C*G�C*�\C*�C+=qC+�C+��C,(�C,�C,��C-{C-p�C-��C.{C.\)C.�C/  C/Q�C/�\C/�C0G�C0�\C0�
C1(�C1�C1�
C2(�C2p�C2��C3(�C3z�C3C4�C4z�C4��C5�C5p�C5�
C6(�C6p�C6C7(�C7z�C7��C8�C8z�C8�
C9(�C9z�C9��C:33C:�C:�
C;33C;�C;�C<=qC<�\C<�HC==qC=��C=��C>G�C>��C>��C?Q�C?�C@  C@Q�C@�CA{CAffCA�CB
=CBffCBCC{CCffCCCD(�CD�CD�
CE(�CE�\CE��CFQ�CF��CG  CGffCG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                     ?�  @   @G�@�  @�  @�  @޸R@�p�A\)A   A,��A@��A`  A�  A�Q�A�Q�A��A��A�  A߮A�A��B�
B�
B  B�
B(  B0(�B8(�B@(�BHQ�BP(�BXQ�B`  Bh  Bp  Bx  B�  B�{B�  B�  B�  B�  B�{B�{B�{B�  B�  B�  B��B�  B�  B�  B�{B�{B�  B��B�{B�  B��B�  B��B��B��B��B��B��B�  B�  B��C��C��C  C  C
  C��C  C{C  C  C  C  C  C  C��C 
=C"  C#��C%��C'�C)��C,  C-��C/�C2  C4
=C6
=C8
=C:  C;��C>
=C@  CA��CC��CE�CH  CJ
=CL
=CN  CP  CR  CT  CV
=CX
=CZ{C\  C]��C`  Cb
=Cd
=Cf  Ch
=Cj
=Cl{Cn  Co��Cr
=Ct{Cv  Cx  Cz{C|{C~  C�  C�  C���C�  C�C�  C�  C�C�C�C�C���C�  C�
=C�  C�  C�C�  C�  C�  C���C���C�  C�  C�  C�  C�C�C���C���C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�  C���C���C���C�  C�  C�  C�  C���C���C�  C�  C�C�C�  C���C�  C�C�C���C���C���C���C�  C�  C���C�  C���C���C���C���C���C�  C�C�
=C�C�  C�  C���C�  C�C�C���C���C�  C�  C���C�  C�  C���C���C�  C�
=C�C�
=C�  C�  C�
=C�C���C�  C�  C�C�  C�  C�  C�C�
=C�C�  C�  C�  C�C�C�  C�  C���C���C���C���C�  C���C���C�  C���C���C�  D   D � D �qD}qD�qD� D�D� D�D��D  D��D�D�D  Dz�D�qD��D	  D	� D
�D
� D  D}qD�qDz�D�qD��D  D� D�D� D  D}qD  D��D�D��D  D}qD  D��D�D��D�D�D�D}qD  D� D�qD}qD�D��D�D��D  D}qD��D}qD  D}qD�qD� D   D ��D!  D!}qD!�qD"� D"�qD#z�D$  D$}qD$�qD%� D&  D&}qD'  D'}qD'�qD(� D)  D)� D*�D*}qD+  D+�D,�D,}qD-  D-��D.  D.z�D.�qD/}qD/��D0� D1  D1� D2  D2}qD2�qD3}qD4  D4� D4��D5z�D5�qD6}qD6�qD7� D8  D8� D9  D9��D:D:��D:�qD;z�D;��D<}qD<�qD=}qD=�qD>� D>��D?}qD@�D@� D@�qDA� DB�DB}qDC  DC� DC�qDD� DE  DE� DF  DFz�DF�qDG� DH�DH�DI  DI��DJDJ� DK  DK��DK��DLz�DL��DM}qDM�qDN}qDN�qDO� DP�DP� DQ�DQ�DR�DR}qDS  DS��DT  DT� DU�DU}qDV  DV��DV�qDW� DX�DX}qDX��DY}qDZ  DZ� D[�D[z�D\  D\� D]�D]� D^  D^� D_  D_��D_�qD`� Da�Da� Db  Db��Dc�Dc��DdDd�Dd�qDe� Df  Df� Dg  Dg��DhDh� Di�Di��Dj�Dj��DkDk�Dl  Dl��Dm�Dm}qDn  Dn� Do  Do� Do��Dp}qDq�Dq��Dr�Dr��Ds�Ds� Dt  Dt��Du  Du� Dv  Dv}qDw  Dw��Dx  Dx� Dx�qDy}qDy�qDz}qD{  D{� D|�D|� D}  D}� D~�D~��D~�qD}qD�qD�>�D�}qD���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��HD��HD�HD�=qD�}qD�� D�HD�>�D�� D�� D���D�>�D��HD�� D���D�@ D�� D�� D���D�@ D�� D��HD�HD�>�D�~�D�� D���D�@ D��HD�� D�HD�C�D��HD�� D���D�>�D�� D��HD�HD�=qD�}qD�� D�HD�@ D�� D��HD�  D�>�D�� D�� D���D�AHD��HD��HD��D�@ D�~�D���D���D�@ D�~�D���D�HD�@ D�� D�� D�  D�@ D��HD��HD�  D�AHD��HD�� D���D�@ D��HD�� D�HD�AHD�� D��HD��D�@ D��HD��HD�  D�@ D�~�D���D���D�=qD�� D�D�  D�@ D��HD��HD�HD�B�D�� D��qD���D�>�D�� D�� D�HD�AHD�� D���D�HD�@ D��HD��HD���D�AHD�� D���D�  D�AHD�� D�� D���D�>�D�~�D��qD���D�AHD�� D�� D���D�>�D��HD�D��D�@ D��HD�� D�HD�B�D��HD���D�  D�>�D�� D�D�HD�>�D�� D�� D���D�@ D��HD��HD�HD�B�D�� D�� D��D�@ D�~�D���D��D�B�D�� D��HD�HD�AHD���D��HD�  D�@ D�� D��HD�HD�AHD��HD��HD�HD�AHD�� D�� D���D�>�D�� D�� D�HD�AHD��HD�D�HD�@ D���D�D�HD�>�D�� D��HD�HD�AHD��HD���D�  D�AHD�� D��qD���D�@ D��HD�D�  D�>�D�� D�� D�HD�AHD�� D�� D�HD�>�D�� D�� D���D�@ D��HD���D���D�@ D�~�D���D�  D�AHD��HD�� D�HD�AHD�� D�� D���D�>�D�� D���D�  D�AHD�� D�� D�  D�@ D��HD��HD���D�@ DHD�� D���D�@ DÁHD�� D���D�@ DāHD�� D�  D�@ Dŀ D�� D�  D�@ DƁHD��HD�  D�AHDǁHD�� D���D�>�D�~�D�� D�  D�>�Dɀ D��HD�  D�@ D�~�Dʾ�D�  D�AHDˀ D��HD�  D�AHD́HD̾�D��qD�>�D́HD��HD�  D�@ D�~�D�� D�  D�@ Dπ D�� D�  D�@ DЀ D��HD�  D�@ D�~�DѾ�D�  D�@ D�~�DҾ�D��qD�>�DӀ D�D��D�AHDԀ DԽqD�  D�AHDՁHD��HD�HD�@ D�~�DֽqD���D�@ D�~�D׽qD�  D�>�D�~�D�� D�  D�>�D�~�D��HD�HD�AHDڀ D�� D�  D�@ D�~�D۾�D���D�>�D܁HD��HD�  D�@ D݁HD�� D�HD�>�D�~�D��HD�HD�>�D�~�D�� D���D�>�D�� DྸD�  D�@ DႏD�� D���D�@ D� D�� D��qD�@ D�~�D�� D���D�@ D� D�D�HD�=qD�~�D��HD�  D�>�D� D�� D���D�@ D�~�D�qD�  D�@ D� D�� D�  D�AHD�HD龸D�  D�@ D�~�D꾸D�  D�@ D� D�� D�  D�AHD� D�D�HD�AHD킏D��HD�HD�AHD�HD�� D�HD�B�D�HD��HD�  D�@ D�� D�� D�HD�B�D�HD��HD�  D�@ D�~�D�� D�HD�B�D�HD�D�  D�@ D� D�� D�  D�@ D��HD��HD���D�@ D�� D��qD�  D�AHD�� D�� D�  D�>�D�� D��HD�  D�@ D�~�D�� D�  D�:�G�O�>�G�>�?W
=?�\)?\?��@
=q@�R@8Q�@Q�@k�@�  @��@�@��\@���@���@��
@�{@�Q�@��
@��@��HA33AQ�Ap�A33A��A\)A%�A)��A/\)A5�A:�HAAG�AFffAJ�HAP  ATz�AZ=qA^�RAc33Ag
=Aj=qAmp�AqG�Au�Ax��A{�A~{A���A��HA���A�ffA��A���A��HA���A�ffA��A�G�A��HA���A�ffA�  A�G�A�33A��A��RA�Q�A��A�33A��A�
=A���A�=qA�33A���A��RA���A��A�33A���A�ffA�Q�A���A��HA�z�A�ffA�  A�G�A\A�z�A�ffA�  A�G�A��HA�z�A�ffA�Q�A��A�33A��A�
=A���Aڏ\A�(�A�A߮A��A�A��A�ffA�Q�A�\A�z�A�{A�A�G�A��HA��A��RA�Q�A���A��A��A�
=B Q�BG�B�B�RB�B��Bp�B=qB�HB�
B��B	�B
�RB�Bz�Bp�B=qB33B(�BG�B=qB\)BQ�BG�B=qB\)B��B��B�RB�B��BB�HB   B!�B"{B"�HB$  B%G�B&ffB'�B(z�B)p�B*�\B+�
B,��B.=qB/\)B0z�B1p�B2�\B3�
B5�B6ffB7�B8��B9�B;
=B<Q�B=B?
=B@(�BAp�BB�RBD  BEG�BFffBG�
BIG�BJ�\BL  BMp�BN�RBP(�BQG�BR�\BS�BT��BUBV�RBW�BX��BY��BZffB[33B[�
B\z�B]�B]B^�\B_\)B`(�B`��BaBb�\Bc33Bc�
Bd��Bep�Bf=qBg
=Bg�
Bh��Bi��Bj�\Bk\)BlQ�Bm�Bm�Bn�RBo\)Bp(�Bp��BqBr�\Bs\)BtQ�Bu�Bu�Bv�RBw�BxQ�By�By�Bz�RB{�B|Q�B}�B}B~�\B\)B�(�B��\B���B�p�B��
B�Q�B��RB��B��B�  B�z�B��HB�\)B�B�(�B���B�
=B�p�B�  B�ffB��HB�G�B��B�(�B��\B�
=B�p�B��
B�Q�B��RB��B��B��B�ffB��RB�33B���B�  B�ffB��HB�G�B��B�(�B��\B���B�\)B��
B�Q�B��RB��B���B�  B�z�B��HB�G�B�B�=qB���B�
=B��B��B�ffB���B�G�B��B�{B��\B���B�\)B��
B�=qB���B�
=B��B��B�Q�B��RB��B��B��B�ffB���B�33B���B�{B�z�B��HB�G�B��B�{B��\B��HB�G�B��
B�=qB���B���B�\)B��
B�=qB���B�
=B��B�  B�ffB���B�33B��B�{B��\B���B�p�B��
B�Q�B���B�33B��B�{B��\B�
=B��B��B�ffB���B�G�B��B�(�B���B��B��B�  B�ffB���B�p�B��B�ffB��HB�\)B��B�Q�B���B�\)B��
B�ffB��HB�\)B��B�ffB��HB�p�B��B�z�B�
=BÅB�  Bď\B��BŮB�=qBƸRB�G�B�B�=qB���B�G�B��
B�ffB��HB�p�B��B�z�B�
=B͙�B�{BΣ�B��B�B�=qB���B�\)B��Bҏ\B��B�B�Q�B��HB�p�B�{B֣�B�33B׮B�Q�B��HBمB�(�B���B�\)B�  Bܣ�B�33B�B�Q�B���Bߙ�B�=qB��HB�B�(�B��HB�B�{B�RB�\)B�  B�RB�\)B�  B�RB�p�B�{B���B�B�(�B�RB�p�B�{B�RB�\)B�{B��HB�B�(�B���B�p�B�{B��RB�\)B�{B��RB�p�B�{B��RB�\)B�  B��\B�G�B��B���B�\)B�  B��RB�G�B��C =qC �\C �C=qC��C�HC33C�C�
C33C�\C�C=qC�C�
C33C�\C�C=qC�\C�CG�C��C��C=qC�\C�C	G�C	��C	�C
33C
��C
��C=qC�\C�HC=qC�C�
C�Cz�C��C(�C�C�HC33C�C��C�Cz�C��C(�C�C�
C33C�C�
C�Cp�C��C(�C�C��C�Cp�CC{Cp�CC{C\)C��C��CQ�C��C�C=qC�C�
C(�C�C��C�CffC�RC
=CffC�RC
=CQ�C��C��CQ�C��C�C=qC��C�HC (�C z�C �
C!(�C!p�C!C"{C"p�C"C#
=C#Q�C#��C$  C$Q�C$��C$�HC%33C%�C%��C&{C&Q�C&�C'  C'G�C'�\C'�
C((�C(z�C(C)
=C)Q�C)�C*  C*G�C*�\C*�C+=qC+�C+��C,(�C,�C,��C-{C-p�C-��C.{C.\)C.�C/  C/Q�C/�\C/�C0G�C0�\C0�
C1(�C1�C1�
C2(�C2p�C2��C3(�C3z�C3C4�C4z�C4��C5�C5p�C5�
C6(�C6p�C6C7(�C7z�C7��C8�C8z�C8�
C9(�C9z�C9��C:33C:�C:�
C;33C;�C;�C<=qC<�\C<�HC==qC=��C=��C>G�C>��C>��C?Q�C?�C@  C@Q�C@�CA{CAffCA�CB
=CBffCBCC{CCffCCCD(�CD�CD�
CE(�CE�\CE��CFQ�CF��CG  CGffCG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                     @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�FA�!A㝲A㗍A㟾A��A��A�uA�DA�A�A�A�v�A�hsA�dZA�bNA�dZA�|�A�{A�O�A�n�A��A�\)Aߣ�A��A���A�~�A�A�ZA��A��A�r�A���A��A�bNA��A�bNA�bNA�`BA�A�=qA��RA��/A�bA��A�G�A��DA���A���A�JA��hA���A�dZA�ĜA�-A���A�1A�dZA���A�?}A��A�x�A�A�E�A���A�z�A��-A�?}A��A��A�ƨA7LA}�Azv�Ax��Au�Ap(�AkƨAix�Ai�Ah�`AgƨAe��Ab��A]
=AX��AV9XAS�;ASAP$�AK��AFffA@�!A>^5A;A7��A5S�A3�7A2��A2�DA1�
A1;dA133A1�A1C�A1+A0�`A0�\A0Q�A09XA0  A/x�A/&�A.��A.~�A-�
A-oA,��A,VA+��A+;dA*�A*v�A*VA'��A&��A&I�A%S�A$��A#�
A#��A#p�A#K�A#�A"M�A!�PA ��A ��A r�A 1A�hA7LA��A��AI�AJAp�A�yA1'A-A  A`BAȴA=qA�;A��A|�AC�A�jA��AE�A��A?}A�A�\A=qA�#A�A?}AVA�9A{AK�AĜA�A9XA�^A��A�7AdZAoA��A�\AffA��A�A��AffA�A�AAĜAz�AI�A�A��A\)A33A
�A
��A
�A
�A
A	��A	�A	O�A	K�A	7LA	VA�+A �A��AS�A�/A��A�AA�A �AbA�FAt�AoA�AZA-A�TAl�A?}A
=A�\A�;A�-A�7A �yA �@�K�@��+@�M�@�-@��h@�hs@�7L@�%@��u@�  @�S�@�o@�ȴ@�ff@��@�X@��j@��@�A�@�|�@���@��@�X@�%@��@���@�\)@�-@���@���@@�R@�=q@��@��
@�|�@�S�@�@�R@�^5@�@�&�@���@�Z@�ƨ@�C�@�@�^@�@�D@䛦@��@�\)@�33@�@��@�V@��T@���@�@���@��D@��@���@��@ݲ-@��@�Q�@��
@��@��@ّh@�7L@���@���@؛�@ץ�@�ȴ@ְ!@�v�@պ^@�&�@��
@�+@�ff@Ѳ-@�`B@���@�A�@϶F@��@�~�@�-@�J@ͺ^@�hs@���@̃@�bN@� �@˾w@ʏ\@�@�%@Ȭ@�j@�(�@ǥ�@��y@Ƈ+@��#@��@��@ļj@�1'@þw@�l�@��y@°!@��@�G�@�&�@�Ĝ@�bN@�A�@�1@��@�;d@���@�@��@���@��@��@��@���@���@�;d@���@�n�@���@�x�@��`@��9@�bN@��w@�S�@���@�V@���@��h@�/@���@�r�@��;@�
=@���@�=q@��@��#@���@��@�j@�1'@��w@��P@�|�@�t�@��@�=q@�J@��#@�x�@�V@���@��9@��D@�1'@��
@�\)@���@��^@�`B@�O�@��@�z�@��@�ƨ@���@�|�@�S�@�
=@���@��+@�-@��h@��@�j@�1@��w@�\)@�@��@���@�v�@�{@�p�@���@�z�@���@�o@���@�n�@�-@��@��#@�@���@��7@��@��@��u@�z�@�(�@���@�l�@�K�@��@��R@��+@�$�@���@�/@���@��/@���@���@��@�r�@�j@�Z@�1@�ƨ@�|�@���@���@��@���@�`B@�%@�Ĝ@��D@�I�@��@�"�@�ȴ@��+@�{@���@�p�@�/@��/@��9@�z�@�9X@��@�  @��@��F@�C�@��\@��@���@�O�@�?}@��@��`@��u@�9X@��F@�+@��@��@��+@�^5@�{@��T@���@�@��h@�x�@�x�@�X@���@��@��@���@���@�t�@�S�@�C�@�@���@���@��+@�-@�@��T@���@��h@�x�@��@��`@��9@���@��@�r�@�Q�@�A�@�1'@��@�ƨ@�|�@�;d@��H@���@�E�@�@���@�p�@�`B@�&�@��@�j@�I�@�b@�@K�@
=@~�R@~ff@~E�@}�@}��@}`B@|�@|z�@{�
@{�@z�@zM�@zJ@yX@y7L@x��@xbN@xb@w�@vv�@u�T@u��@u�@t�@t�D@t�D@tZ@s�m@s�F@s��@s�@s33@r�!@r^5@q��@qG�@q%@p��@pb@o�;@o��@oK�@n�y@nv�@n5?@n@m��@l��@k��@kdZ@kC�@j��@j=q@jJ@i��@i��@hbN@h  @g�P@f��@e��@d�/@d1@c��@c33@co@b�H@b��@bn�@bM�@a�^@`�`@`bN@_�w@_�@_��@_|�@_\)@^��@^�R@^$�@]�-@]?}@]V@\�@\�@\z�@[�
@[��@[S�@["�@[o@Z�H@Z�\@ZM�@Y��@Y&�@X��@XĜ@X�@XQ�@W�;@W+@V�y@V��@Vv�@VV@U�@U�@U/@T��@TZ@S�F@R�@R�\@R^5@RM�@Q�^@P�u@P �@Ol�@N��@N$�@M�T@M@L�/@L1@K�F@K�@KC�@K@J~�@I��@Ix�@H�`@H��@H1'@H  @G�;@G��@G��@G+@F��@F5?@F{@E��@E�@D�@D��@DI�@D(�@Cƨ@C33@Co@C@B�@B��@B-@A�^@A&�@@�`@@Ĝ@@��@@r�@@1'@@  @?�@?;d@>��@>�y@>v�@>$�@>$�@>@>@>@=@=V@<�j@<j@<I�@<(�@<1@;�F@;"�@:^5@9�@9�^@9�7@9&�@8��@8��@8r�@81'@7�;@7�@7��@7�P@7|�@7\)@7\)@7;d@7+@6ȴ@6V@65?@65?@6$�@5@5�h@5/@4�@4z�@4j@49X@3�F@3��@3dZ@3C�@2��@2-@1�@1�^@1�7@1x�@1X@0Ĝ@0�u@0bN@0b@/�@/�;@/|�@.�@.��@.v�@.ff@.$�@-��@-p�@,��@,9X@+��@+�
@+dZ@+33@*�@*��@*��@*�!@*�\@*n�@*�@)�#@)hs@)&�@(��@(�u@(bN@(  @'�;@'��@'�@'�P@'l�@'+@&��@&v�@&5?@&$�@&{@&@%�@%�T@%��@%��@%O�@$�/@$9X@#ƨ@#�@#"�@#o@#@"~�@"=q@"-@!��@!�@!�#@!��@ �9@ �@ bN@ A�@ A�@ 1'@�;@�w@�@�P@�@ff@5?@E�@5?@5?@5?@{@�@�T@@��@�h@p�@�@V@��@�/@�/@�j@��@z�@9X@��@��@33@�@��@�\@n�@^5@M�@=q@J@��@�#@�7@�@Ĝ@�u@bN@A�@��@|�@\)@
=@E�@��@`B@O�@�@��@j@�@��@�m@ƨ@��@dZ@33@o@�@�H@�H@��@~�@^5@-@�@J@J@J@��@�#@x�@%@Ĝ@�9@�u@�u@r�@Q�@ �@�P@K�@�@�@��@��@V@5?@5?@$�@{@�@��@�-@�h@�h@�@?}@�@V@�@�j@��@z�@z�@I�@9X@�@�@(�@�@�m@�F@dZ@S�@S�@C�@33@
�@
��@
��@
�\@
~�@
=q@	��@	�@	�#@	��@	�^@	��A�!A�-A�RA�^A�9A�-A��A��A��A㗍A㙚A㕁A�uA��A��A��A��A㛦A��A��A��A��A㕁A�7A�hA�+A�DA�7A�A�A�A�A�A�A�A�A�|�A�z�A�~�A�A�v�A�n�A�l�A�l�A�p�A�p�A�p�A�jA�ffA�hsA�hsA�jA�hsA�dZA�bNA�ffA�hsA�jA�hsA�dZA�`BA�bNA�dZA�`BA�^5A�`BA�bNA�dZA�bNA�`BA�`BA�bNA�ffA�dZA�dZA�`BA�bNA�dZA�ffA�ffA�bNA�bNA�dZA�hsA�ffA�dZA�ffA�jA�p�A�v�A�A�PA㕁A㙚A��A�!A���A��yA���A��A�=qA�S�A�bNA�^5A�S�A�M�A�G�A�E�A�E�A�K�A�O�A�XA�bNA�x�A�hA�DA�A�r�A�hsA�`BA�VA�K�A�E�A�=qA�7LA�(�A��A�bA�%A���A��A��A��HA���A�A�~�A�bNA�;dA�
=A���A�DA��A��A��A�A���AޅA�K�A�x�A�~�A�G�A�S�A١�A�`BA�=qA��Aز-A�x�A�$�A�{A��Aן�A�?}A���A�bNA��A�bNA���A�`BAә�A�%A҃A���A�l�A�x�A�A�M�A�9XA�x�A�
=A�v�A���A�A��AɶFA�/A�bNA�O�A���A�dZA�ĜAĩ�A�A�A��\A�ffA�I�A���A�bA�1A��/A�|�A�bA�1A���A��A���A�A��A���A��hA�~�A�`BA�O�A�;dA��A��A���A���A��FA���A�z�A�I�A�/A�JA�A��jA��A��hA��A��A�9XA��!A���A���A���A���A��\A��+A�z�A�v�A�t�A�r�A�p�A�p�A�p�A�l�A�jA�hsA�dZA�`BA�XA�S�A�M�A�7LA�%A��
A���A�bNA�1'A��A�A�ȴA���A��+A��A��A�t�A�ffA�ZA�A�A�7LA�$�A��A�
=A��yA���A�ƨA��9A���A���A�jA�ffA�`BA�VA�K�A�C�A�;dA�1'A�&�A��A�%A��A��;A���A��wA��A���A���A��\A�~�A�l�A�bNA�ZA�33A���A���A���A�p�A�7LA��A���A�dZA�33A�oA���A��mA��
A��jA���A�r�A�ZA�A�A�-A�"�A�JA���A��HA��wA���A���A��DA�p�A�ZA�I�A�5?A�%A��mA���A���A���A���A���A���A��hA��A�hsA�`BA�S�A�M�A�9XA�%A��#A�t�A�1A�  A�A�VA��A�%A���A��yA��/A���A�A���A��+A�n�A�Q�A�1'A�VA�bA�%A��A��A���A�A��RA��A���A��uA��A��A�x�A�n�A�dZA�S�A�E�A�+A�JA��yA���A���A�\)A�VA���A�$�A�~�A��uA�;dA��A�bA�
=A�A���A��mA�ȴA���A�\)A�"�A��yA���A�`BA�/A���A��FA�hsA�;dA��A�A��TA���A�ZA�1A���A�~�A�bA�JA��A�?}A��uA�K�A��RA��RA�x�A�ZA�/A��A�%A�ȴA���A�5?A��-A�(�A�1A��A���A��^A��DA�G�A��A���A��!A�r�A�G�A�+A�VA���A��A��A��A�dZA�VA��hA���A���A�M�A���A�jA�%A�33A�ZA���A��#A��^A�z�A��#A�G�A�ĜA�x�A�+A���A�ZA��A�n�A�A���A�p�A�&�A��A�O�A��A��mA���A�ZA�+A�
=A��A�A�`BA��A��A�1A���A�dZA��TA�x�A�1'A�VA��-A�JA��HA��uA�K�A�oA��yA��HA���A�ȴA���A�t�A��A�7A/A~�/A~��A~bNA~�A}�#A}��A}O�A}%A|�jA| �A{%Az$�Ay�FAy�hAy|�AydZAyO�Ay7LAx�`AxjAx9XAw�#Aw&�Av1Au�At��As��As33Ar��Aq;dAo33Am��Al��Al�9Alz�Al�AkAkO�Aj�/Aj^5Ai��AiS�Ai&�Ai&�Ai"�Ai"�Ai�Ai�Ai�AioAi
=Ai%Ah��Ah�Ah�`Ah�AhĜAh�9Ah�+Ah5?Ag�wAg;dAg�Af�Af�!Af=qAeAe|�Ae\)Ae�Ad��Ad-AcG�Ab=qAaXA`�A`bA_|�A_VA]��A[7LAZ{AY��AYdZAYO�AY�AX�AX�!AXr�AX$�AW��AW�AVbAT��ATz�AT=qAT$�AT1AS�;ASAS��ASl�AS33AS
=AS
=AS
=AS%AR��AR$�AQ�AQXAP�/AO��ANbNAN�AM�AMƨAM�AK?}AJffAI�TAIK�AHz�AG��AF��AFA�AE33ADAB�AA�AA�A@1'A?��A?�A?\)A?;dA?
=A>�DA=�A=\)A=VA<��A;��A;�A;33A9��A8ĜA8v�A89XA8JA7�TA7��A7VA6M�A5�A5�-A5S�A4�A4bNA3��A3�^A3��A3|�A3`BA3O�A3C�A3�A3%A2�A2�A2�/A2��A2~�A2jA2�A2�+A2bNA2E�A2=qA1�A1�A1dZA1G�A1?}A1;dA1;dA1;dA17LA1;dA1?}A1;dA133A1/A1+A1&�A1�A1
=A1
=A1&�A17LA1?}A1G�A1O�A1G�A17LA1/A1;dA1?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                     A�FA�!A㝲A㗍A㟾A��A��A�uA�DA�A�A�A�v�A�hsA�dZA�bNA�dZA�|�A�{A�O�A�n�A��A�\)Aߣ�A��A���A�~�A�A�ZA��A��A�r�A���A��A�bNA��A�bNA�bNA�`BA�A�=qA��RA��/A�bA��A�G�A��DA���A���A�JA��hA���A�dZA�ĜA�-A���A�1A�dZA���A�?}A��A�x�A�A�E�A���A�z�A��-A�?}A��A��A�ƨA7LA}�Azv�Ax��Au�Ap(�AkƨAix�Ai�Ah�`AgƨAe��Ab��A]
=AX��AV9XAS�;ASAP$�AK��AFffA@�!A>^5A;A7��A5S�A3�7A2��A2�DA1�
A1;dA133A1�A1C�A1+A0�`A0�\A0Q�A09XA0  A/x�A/&�A.��A.~�A-�
A-oA,��A,VA+��A+;dA*�A*v�A*VA'��A&��A&I�A%S�A$��A#�
A#��A#p�A#K�A#�A"M�A!�PA ��A ��A r�A 1A�hA7LA��A��AI�AJAp�A�yA1'A-A  A`BAȴA=qA�;A��A|�AC�A�jA��AE�A��A?}A�A�\A=qA�#A�A?}AVA�9A{AK�AĜA�A9XA�^A��A�7AdZAoA��A�\AffA��A�A��AffA�A�AAĜAz�AI�A�A��A\)A33A
�A
��A
�A
�A
A	��A	�A	O�A	K�A	7LA	VA�+A �A��AS�A�/A��A�AA�A �AbA�FAt�AoA�AZA-A�TAl�A?}A
=A�\A�;A�-A�7A �yA �@�K�@��+@�M�@�-@��h@�hs@�7L@�%@��u@�  @�S�@�o@�ȴ@�ff@��@�X@��j@��@�A�@�|�@���@��@�X@�%@��@���@�\)@�-@���@���@@�R@�=q@��@��
@�|�@�S�@�@�R@�^5@�@�&�@���@�Z@�ƨ@�C�@�@�^@�@�D@䛦@��@�\)@�33@�@��@�V@��T@���@�@���@��D@��@���@��@ݲ-@��@�Q�@��
@��@��@ّh@�7L@���@���@؛�@ץ�@�ȴ@ְ!@�v�@պ^@�&�@��
@�+@�ff@Ѳ-@�`B@���@�A�@϶F@��@�~�@�-@�J@ͺ^@�hs@���@̃@�bN@� �@˾w@ʏ\@�@�%@Ȭ@�j@�(�@ǥ�@��y@Ƈ+@��#@��@��@ļj@�1'@þw@�l�@��y@°!@��@�G�@�&�@�Ĝ@�bN@�A�@�1@��@�;d@���@�@��@���@��@��@��@���@���@�;d@���@�n�@���@�x�@��`@��9@�bN@��w@�S�@���@�V@���@��h@�/@���@�r�@��;@�
=@���@�=q@��@��#@���@��@�j@�1'@��w@��P@�|�@�t�@��@�=q@�J@��#@�x�@�V@���@��9@��D@�1'@��
@�\)@���@��^@�`B@�O�@��@�z�@��@�ƨ@���@�|�@�S�@�
=@���@��+@�-@��h@��@�j@�1@��w@�\)@�@��@���@�v�@�{@�p�@���@�z�@���@�o@���@�n�@�-@��@��#@�@���@��7@��@��@��u@�z�@�(�@���@�l�@�K�@��@��R@��+@�$�@���@�/@���@��/@���@���@��@�r�@�j@�Z@�1@�ƨ@�|�@���@���@��@���@�`B@�%@�Ĝ@��D@�I�@��@�"�@�ȴ@��+@�{@���@�p�@�/@��/@��9@�z�@�9X@��@�  @��@��F@�C�@��\@��@���@�O�@�?}@��@��`@��u@�9X@��F@�+@��@��@��+@�^5@�{@��T@���@�@��h@�x�@�x�@�X@���@��@��@���@���@�t�@�S�@�C�@�@���@���@��+@�-@�@��T@���@��h@�x�@��@��`@��9@���@��@�r�@�Q�@�A�@�1'@��@�ƨ@�|�@�;d@��H@���@�E�@�@���@�p�@�`B@�&�@��@�j@�I�@�b@�@K�@
=@~�R@~ff@~E�@}�@}��@}`B@|�@|z�@{�
@{�@z�@zM�@zJ@yX@y7L@x��@xbN@xb@w�@vv�@u�T@u��@u�@t�@t�D@t�D@tZ@s�m@s�F@s��@s�@s33@r�!@r^5@q��@qG�@q%@p��@pb@o�;@o��@oK�@n�y@nv�@n5?@n@m��@l��@k��@kdZ@kC�@j��@j=q@jJ@i��@i��@hbN@h  @g�P@f��@e��@d�/@d1@c��@c33@co@b�H@b��@bn�@bM�@a�^@`�`@`bN@_�w@_�@_��@_|�@_\)@^��@^�R@^$�@]�-@]?}@]V@\�@\�@\z�@[�
@[��@[S�@["�@[o@Z�H@Z�\@ZM�@Y��@Y&�@X��@XĜ@X�@XQ�@W�;@W+@V�y@V��@Vv�@VV@U�@U�@U/@T��@TZ@S�F@R�@R�\@R^5@RM�@Q�^@P�u@P �@Ol�@N��@N$�@M�T@M@L�/@L1@K�F@K�@KC�@K@J~�@I��@Ix�@H�`@H��@H1'@H  @G�;@G��@G��@G+@F��@F5?@F{@E��@E�@D�@D��@DI�@D(�@Cƨ@C33@Co@C@B�@B��@B-@A�^@A&�@@�`@@Ĝ@@��@@r�@@1'@@  @?�@?;d@>��@>�y@>v�@>$�@>$�@>@>@>@=@=V@<�j@<j@<I�@<(�@<1@;�F@;"�@:^5@9�@9�^@9�7@9&�@8��@8��@8r�@81'@7�;@7�@7��@7�P@7|�@7\)@7\)@7;d@7+@6ȴ@6V@65?@65?@6$�@5@5�h@5/@4�@4z�@4j@49X@3�F@3��@3dZ@3C�@2��@2-@1�@1�^@1�7@1x�@1X@0Ĝ@0�u@0bN@0b@/�@/�;@/|�@.�@.��@.v�@.ff@.$�@-��@-p�@,��@,9X@+��@+�
@+dZ@+33@*�@*��@*��@*�!@*�\@*n�@*�@)�#@)hs@)&�@(��@(�u@(bN@(  @'�;@'��@'�@'�P@'l�@'+@&��@&v�@&5?@&$�@&{@&@%�@%�T@%��@%��@%O�@$�/@$9X@#ƨ@#�@#"�@#o@#@"~�@"=q@"-@!��@!�@!�#@!��@ �9@ �@ bN@ A�@ A�@ 1'@�;@�w@�@�P@�@ff@5?@E�@5?@5?@5?@{@�@�T@@��@�h@p�@�@V@��@�/@�/@�j@��@z�@9X@��@��@33@�@��@�\@n�@^5@M�@=q@J@��@�#@�7@�@Ĝ@�u@bN@A�@��@|�@\)@
=@E�@��@`B@O�@�@��@j@�@��@�m@ƨ@��@dZ@33@o@�@�H@�H@��@~�@^5@-@�@J@J@J@��@�#@x�@%@Ĝ@�9@�u@�u@r�@Q�@ �@�P@K�@�@�@��@��@V@5?@5?@$�@{@�@��@�-@�h@�h@�@?}@�@V@�@�j@��@z�@z�@I�@9X@�@�@(�@�@�m@�F@dZ@S�@S�@C�@33@
�@
��@
��@
�\@
~�@
=q@	��@	�@	�#@	��@	�^G�O�A�!A�-A�RA�^A�9A�-A��A��A��A㗍A㙚A㕁A�uA��A��A��A��A㛦A��A��A��A��A㕁A�7A�hA�+A�DA�7A�A�A�A�A�A�A�A�A�|�A�z�A�~�A�A�v�A�n�A�l�A�l�A�p�A�p�A�p�A�jA�ffA�hsA�hsA�jA�hsA�dZA�bNA�ffA�hsA�jA�hsA�dZA�`BA�bNA�dZA�`BA�^5A�`BA�bNA�dZA�bNA�`BA�`BA�bNA�ffA�dZA�dZA�`BA�bNA�dZA�ffA�ffA�bNA�bNA�dZA�hsA�ffA�dZA�ffA�jA�p�A�v�A�A�PA㕁A㙚A��A�!A���A��yA���A��A�=qA�S�A�bNA�^5A�S�A�M�A�G�A�E�A�E�A�K�A�O�A�XA�bNA�x�A�hA�DA�A�r�A�hsA�`BA�VA�K�A�E�A�=qA�7LA�(�A��A�bA�%A���A��A��A��HA���A�A�~�A�bNA�;dA�
=A���A�DA��A��A��A�A���AޅA�K�A�x�A�~�A�G�A�S�A١�A�`BA�=qA��Aز-A�x�A�$�A�{A��Aן�A�?}A���A�bNA��A�bNA���A�`BAә�A�%A҃A���A�l�A�x�A�A�M�A�9XA�x�A�
=A�v�A���A�A��AɶFA�/A�bNA�O�A���A�dZA�ĜAĩ�A�A�A��\A�ffA�I�A���A�bA�1A��/A�|�A�bA�1A���A��A���A�A��A���A��hA�~�A�`BA�O�A�;dA��A��A���A���A��FA���A�z�A�I�A�/A�JA�A��jA��A��hA��A��A�9XA��!A���A���A���A���A��\A��+A�z�A�v�A�t�A�r�A�p�A�p�A�p�A�l�A�jA�hsA�dZA�`BA�XA�S�A�M�A�7LA�%A��
A���A�bNA�1'A��A�A�ȴA���A��+A��A��A�t�A�ffA�ZA�A�A�7LA�$�A��A�
=A��yA���A�ƨA��9A���A���A�jA�ffA�`BA�VA�K�A�C�A�;dA�1'A�&�A��A�%A��A��;A���A��wA��A���A���A��\A�~�A�l�A�bNA�ZA�33A���A���A���A�p�A�7LA��A���A�dZA�33A�oA���A��mA��
A��jA���A�r�A�ZA�A�A�-A�"�A�JA���A��HA��wA���A���A��DA�p�A�ZA�I�A�5?A�%A��mA���A���A���A���A���A���A��hA��A�hsA�`BA�S�A�M�A�9XA�%A��#A�t�A�1A�  A�A�VA��A�%A���A��yA��/A���A�A���A��+A�n�A�Q�A�1'A�VA�bA�%A��A��A���A�A��RA��A���A��uA��A��A�x�A�n�A�dZA�S�A�E�A�+A�JA��yA���A���A�\)A�VA���A�$�A�~�A��uA�;dA��A�bA�
=A�A���A��mA�ȴA���A�\)A�"�A��yA���A�`BA�/A���A��FA�hsA�;dA��A�A��TA���A�ZA�1A���A�~�A�bA�JA��A�?}A��uA�K�A��RA��RA�x�A�ZA�/A��A�%A�ȴA���A�5?A��-A�(�A�1A��A���A��^A��DA�G�A��A���A��!A�r�A�G�A�+A�VA���A��A��A��A�dZA�VA��hA���A���A�M�A���A�jA�%A�33A�ZA���A��#A��^A�z�A��#A�G�A�ĜA�x�A�+A���A�ZA��A�n�A�A���A�p�A�&�A��A�O�A��A��mA���A�ZA�+A�
=A��A�A�`BA��A��A�1A���A�dZA��TA�x�A�1'A�VA��-A�JA��HA��uA�K�A�oA��yA��HA���A�ȴA���A�t�A��A�7A/A~�/A~��A~bNA~�A}�#A}��A}O�A}%A|�jA| �A{%Az$�Ay�FAy�hAy|�AydZAyO�Ay7LAx�`AxjAx9XAw�#Aw&�Av1Au�At��As��As33Ar��Aq;dAo33Am��Al��Al�9Alz�Al�AkAkO�Aj�/Aj^5Ai��AiS�Ai&�Ai&�Ai"�Ai"�Ai�Ai�Ai�AioAi
=Ai%Ah��Ah�Ah�`Ah�AhĜAh�9Ah�+Ah5?Ag�wAg;dAg�Af�Af�!Af=qAeAe|�Ae\)Ae�Ad��Ad-AcG�Ab=qAaXA`�A`bA_|�A_VA]��A[7LAZ{AY��AYdZAYO�AY�AX�AX�!AXr�AX$�AW��AW�AVbAT��ATz�AT=qAT$�AT1AS�;ASAS��ASl�AS33AS
=AS
=AS
=AS%AR��AR$�AQ�AQXAP�/AO��ANbNAN�AM�AMƨAM�AK?}AJffAI�TAIK�AHz�AG��AF��AFA�AE33ADAB�AA�AA�A@1'A?��A?�A?\)A?;dA?
=A>�DA=�A=\)A=VA<��A;��A;�A;33A9��A8ĜA8v�A89XA8JA7�TA7��A7VA6M�A5�A5�-A5S�A4�A4bNA3��A3�^A3��A3|�A3`BA3O�A3C�A3�A3%A2�A2�A2�/A2��A2~�A2jA2�A2�+A2bNA2E�A2=qA1�A1�A1dZA1G�A1?}A1;dA1;dA1;dA17LA1;dA1?}A1;dA133A1/A1+A1&�A1�A1
=A1
=A1&�A17LA1?}A1G�A1O�A1G�A17LA1/A1;dA1?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                     ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�yB�B�B�B�B�B�B�B�B��B�B��B�B�B�`B��B�B�FB	B[B	�-B	�!B	��B	�B
+6B
7�B
T�B
n�B
j�B
��B
��B�B<BZ�B�7B�wB�HB�B�.B+B�BBeBB�B�B�B�B4B"�B,B'RBeB��B��BɆB��B�B
�sB
�jB
�B
�eB
hsB
5tB
�B
�B	�DB	��B	��B	�BB	�!B	�*B	�0B	�B	�$B	��B	�oB	k�B	ZQB	PHB	L�B	H�B	<jB	0!B	$@B	 �B��B��B�vB�B�yB�B�6B� B�sB�,B��B�B��B	�B	B	�B	VB	0�B	T�B	g8B	sMB	��B	��B	��B	�$B	��B	��B	�9B	̘B	�mB	��B	�>B	�cB
�B
 B
�B
�B
(�B
!�B
B
�B
�B
=B
OB
B
�B
�B
�B
"hB
"�B
&�B
)�B
+�B
,qB
,�B
+B
*�B
+�B
-B
/OB
0�B
2�B
5B
5tB
9�B
:*B
9�B
:*B
;dB
<B
<�B
=B
?B
>wB
@B
B'B
C-B
C�B
D�B
FtB
FtB
GEB
GzB
HB
HB
GzB
J#B
I�B
I�B
I�B
I�B
I�B
IRB
I�B
I�B
J#B
J#B
I�B
JXB
HKB
G�B
I�B
G�B
H�B
GB
F�B
GB
G�B
H�B
GEB
FtB
F�B
G�B
F�B
GEB
FB
EmB
E�B
D�B
C�B
D3B
D3B
C�B
CaB
B'B
@�B
?�B
?HB
>�B
>�B
>�B
>B
=�B
=�B
=B
=B
=qB
;�B
:�B
:�B
8�B
8�B
8�B
8RB
6�B
5?B
4�B
5B
0�B
0�B
/�B
/OB
/B
/B
.�B
.}B
.B
.IB
-�B
-wB
,qB
,qB
,B
+�B
+�B
+B
*eB
)�B
*eB
(�B
($B
'B
&�B
%�B
%�B
%�B
$B
"�B
"�B
"hB
 �B
 �B
 �B
�B
B
�B
�B
�B
�B
B
�B
�B
=B
kB
_B
�B
_B
�B
�B
�B
�B
SB
�B
�B
�B
B
B
{B
�B
�B
{B
�B
SB
B
FB
�B
B
@B
�B
�B
B
�B
hB
 B
�B
oB
�B
.B
\B
�B
�B
�B
JB
�B
JB
�B
�B
JB
�B
JB
�B
�B
B
xB
�B
�B
DB
xB
xB

�B
JB
DB
JB
�B
xB
xB
B
JB
xB
�B
�B
�B
B
�B
B
JB
~B
�B
B
�B
�B
�B
�B
~B
~B
�B
JB
PB
B
�B
�B
(B
\B
�B
�B
�B
�B
�B
.B
�B
�B
�B
�B
.B
 B
�B
hB
 B
4B
hB
 B
�B
�B
�B
B
�B
:B
B
�B
B
B
�B
�B
@B
uB
@B
B
{B
�B
FB
{B
�B
MB
B
B
�B
�B
MB
�B
�B
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
_B
�B
�B
�B
1B
B
7B
�B
	B
�B
B
CB
B
�B
�B
CB
=B
�B
=B
�B
�B
	B
�B
�B
�B
�B
xB
�B
�B
�B
�B
~B
�B
�B
�B
�B
VB
�B
�B
!-B
!�B
"4B
!�B
!�B
!�B
"�B
"�B
#B
"�B
"�B
#nB
#nB
#�B
$�B
$�B
%�B
&LB
&�B
&�B
'B
'B
'RB
($B
(�B
(XB
(�B
(�B
(�B
)*B
)�B
)�B
)�B
*eB
*eB
*eB
*0B
)�B
)�B
*0B
*eB
+�B
,B
+�B
+�B
+�B
+�B
+6B
*eB
+kB
+kB
+6B
+�B
+kB
+kB
+�B
+�B
+�B
+�B
,=B
,qB
,�B
-CB
-�B
.IB
.}B
.}B
.}B
/OB
/�B
/�B
0UB
0!B
/�B
/�B
0UB
0!B
0!B
0�B
0!B
0UB
0�B
1'B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2aB
2-B
2�B
2�B
33B
3�B
3�B
4�B
4�B
4�B
5B
5tB
6B
6zB
6FB
6zB
6�B
6�B
7B
7B
7B
7LB
7B
7�B
7�B
8RB
8�B
8�B
9XB
9�B
9�B
:�B
:*B
:^B
:�B
:�B
;�B
<�B
<�B
<�B
<�B
=qB
=<B
=B
=�B
=�B
=�B
=qB
=qB
>B
>BB
>BB
>�B
>�B
>�B
?B
@B
?�B
@B
@OB
@OB
@�B
@�B
@�B
A B
A�B
B[B
A�B
A�B
C-B
B[B
B�B
B'B
C-B
CaB
B�B
C-B
D3B
EB
D�B
EmB
E9B
E�B
E�B
E�B
E�B
E�B
EmB
E�B
FtB
FtB
F�B
F�B
F�B
FtB
F�B
F�B
F�B
GEB
G�B
HB
HKB
H�B
H�B
HKB
IB
H�B
IB
H�B
H�B
IB
IRB
IRB
J#B
J#B
J�B
K�B
K�B
K�B
K�B
K�B
LdB
L0B
L0B
MjB
NB
N<B
N<B
MjB
N�B
O�B
O�B
PHB
O�B
O�B
PHB
O�B
OBB
OB
OvB
PB
P}B
Q�B
S�B
T,B
S�B
S�B
S�B
TaB
TaB
UgB
T�B
VB
U�B
V9B
VmB
VmB
VB
VmB
V�B
WsB
W�B
WsB
W�B
W�B
XEB
X�B
XyB
X�B
YKB
YB
YB
YKB
X�B
YB
YKB
YKB
YB
Y�B
Y�B
Y�B
Z�B
Z�B
[#B
[�B
[WB
[�B
[�B
\]B
\�B
\�B
\�B
\�B
\]B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
_B
^�B
_B
_B
_�B
_�B
`B
`vB
`�B
`�B
aB
`�B
aB
aB
aHB
aB
aB
a�B
a�B
a�B
a|B
a|B
a�B
a|B
bB
bNB
bNB
bB
bNB
b�B
b�B
b�B
b�B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
d�B
dZB
d�B
d�B
e,B
e,B
e�B
e�B
f2B
f2B
e�B
ffB
f�B
gmB
g�B
h�B
h�B
iDB
h�B
iB
iyB
iyB
i�B
i�B
iyB
i�B
jB
jKB
j�B
j�B
kB
k�B
k�B
k�B
l"B
lWB
lWB
l�B
l�B
l�B
m)B
m)B
m�B
m)B
m)B
m]B
m]B
m)B
m)B
m]B
m�B
ncB
o B
n�B
o B
o�B
o�B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
qvB
p�B
qAB
qAB
q�B
q�B
qvB
rB
rGB
rGB
r|B
s�B
t�B
t�B
tB
t�B
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
u�B
v�B
v`B
v+B
v�B
v`B
v�B
v�B
v�B
v�B
w2B
w�B
xB
x8B
x8B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y>B
y�B
zxB
y�B
zDB
y�B
zxB
zDB
zB
zDB
zB
zxB
z�B
zxB
z�B
zxB
{JB
{JB
{�B
{JB
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|PB
|PB
|PB
|�B
}�B
}VB
}VB
}VB
|�B
|�B
}"B
}�B
~�B
~�B
~(B
~�B
~(B
~�B
~�B
~�B
�B
.B
�B
�iB
�4B
�B
�iB
��B
�4B
��B
��B
��B
�B
�;B
�;B
�;B
�;B
��B
�oB
��B
��B
�B
�B
�AB
�AB
��B
�{B
��B
��B
�B
�MB
��B
��B
��B
��B
�SB
��B
��B
��B
�%B
�YB
�%B
�YB
��B
��B
�+B
�+B
��B
��B
�+B�DB�yB�
B��B��B��B��B�B�B��B�sB��B�B�B�B�fB�
B�B��B�mB�B�sB�B�B��B�QB�B��B�KB�B�QB��B�B�KB�KB�KB�WB��B�B�B�]B�cB��B�cB�]B�B��B�5B�oB�B�;B�B��B�AB�B�B�oB�;B�oB�B�%B�B��B��B�`B��B��B�B��B�fB��B��B�2B��B�>B�xB��B��B��B�B�VB�VB�"B�.B��BoB�B�B+BPBoB=B#nB"4B5tB=qB?B_�BjB� B��B��B�B	�B	�B	!�B	$@B	6zB	@�B	P�B	]�B	e�B	m�B	m�B	��B	��B	��B	�wB	�}B	��B	��B	��B	��B	�UB	�!B	��B	��B	��B	��B	��B	�OB	�B	�CB	�B	��B	�B	��B	�?B	��B	��B	�qB	�EB	�BB	��B	�
B	�B	��B	��B
oB
�B
 'B
;�B
+6B
$�B
(XB
.�B
.B
2�B
8�B
.�B
2aB
:*B
?�B
F�B
K�B
G�B
I�B
S�B
N�B
t�B
[#B
b�B
b�B
l�B
t�B
i�B
v�B
x�B
f�B
ffB
c B
g�B
iyB
|�B
e`B
ncB
|�B
��B
zxB
�=B
�B
�bB
��B
�_B
�B
�{B
�$B
�&B
�BGB&�B2�B0�B3�B8�B7�B8�B<6B8�B<6B>BB�BGBG�BJ�BS&BT�BR�BS&BX�B_�Bm�Bt�By	Bx�B��B��B�VB�B�B�OB��B��B��B�hB�LB��B��B��B�-B�mB��B�tB�BǮBȀB�#B˒B�B�}B��BچBߤB�,B�yB�B�B�`B��B�TB��B��B�.B��B��B��B��B��B�cB;B �B �B  BGB�B�BGB1B�BfB�B
�B�B\B�B�B(B\B.BbBbBhB�B@BB�B�B@B�BB�B�BYBIB�B�B�B_B�B_B�B�BSB@BuB�B�B�B:B B"B"BxB
rB
�B
=B
�B�B
=B	7B�BB_B�BPB.B
�B(BBDBxBDBB�B�B"B�B�BB�B�B.B+B
	B �B�B�B�BBoBhB�B�B�B B�B(BBMBeB�B,qB.IB0!B.B.�B,�B-B+�B+kB+6B)�B(�B(�B($B($B(�B(�B'B"�B%FB%B&B%B)�B"hB3hB~B�B��B��B��B�JB��B�VB��B�]B �B��B��B�B�B��B��B��B�BٴB�,B��B�dBʌB�B��B��B�B��B��B��Bl�Bm�B~�BW�B#�B
rB\B�B
�B  B
��B
�B
�B
�B
��B
�aB
�
B
�BB
�HB
уB
�,B
ǮB
��B
��B
�B
ΥB
�qB
�jB
�UB
��B
��B
��B
��B
�'B
�_B
�:B
��B
�B
�GB
[�B
R�B
e�B
Z�B
=qB
5B
/�B
5B
C�B
/OB
'B
�B
VB
�B
+B
�B
B
	B
SB	�B	��B
AB	��B	�B	�KB	��B	�sB	�|B	یB	�EB	�#B	�B	ݘB	�BB	��B	҉B	�NB	�sB	ɺB	ǮB	�wB	�B	B	��B	�dB	�B	�^B	�UB	��B	�nB	�UB	�B	��B	��B	��B	��B	�tB	�B	��B	�XB	��B	�^B	��B	�6B	��B	̘B	՛B	��B	��B	��B	�*B	��B	��B	�LB	�HB	��B	��B	��B	��B	��B	��B	�~B	�FB	��B	��B	�eB	�B	��B	v�B	n/B	n/B	q�B	k�B	i�B	hsB	g�B	_B	_�B	V�B	Q�B	P�B	PB	QB	QNB	OvB	OBB	N�B	MjB	M�B	M6B	L�B	MB	J�B	IB	H�B	MB	MB	M�B	B[B	@B	D3B	B[B	B'B	8RB	6�B	6FB	5?B	8�B	7�B	3�B	,�B	"�B	%�B	�B	@B	F?B	6FB	�B	B	�B	 iB	B��B��B��B��B��B	�B	%B��B�B�8B��B�B��B��B�8B��B��B�|B��B��B��B�`B��B��B��B�WB	
rB�B��B��B�WB�B�5B�DB�B�B�cB�,B�B��B�"B��B�ZB�vB�)BȀB�UB��B��B�LB��B��B�'B�^B�B��B�BBуBΥB	B�BϫB͟B�B�vBԕB�B��B�B�5B��B��B� B�B��B��B�B��B�B�B�8B�B��B�(B	�B	+B	B	�B	uB		�B	�B	
�B	
�B	!B	FB	IB	YB	�B	=B	kB	=B	�B	qB		B	�B	 'B	!�B	"�B	#�B	&�B	*�B	/B	>B	B�B	GzB	S�B	WsB	[WB	^�B	_B	a|B	d&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                     B�yB��B��B��B�B�*B�_B�B�KB��B�B�QB�B�B�B�FB�0BHB��B	@�B	�B	��B	�B
�B
88B
JrB
m�B
�RB
��B
�WB
��BkBA�BbNB��B��B�SB�B�B
XB:B�B#TB�B�B�B�B�BB%�B0oB6FB#:BtB��B��B�ABB
�[B
�B
��B
��B
z�B
E�B
*�B
(B	�3B	�kB	ڠB	�B	�B	� B	��B	�dB	��B	�wB	��B	s�B	[�B	QhB	P�B	O�B	G�B	B[B	1B	
	B	�B��B��B��B�JB�0B�mB�~B��BܬB��B�B��B		�B	$B	�B	�B	0�B	UB	h$B	t�B	�oB	�B	��B	�B	��B	��B	�tB	�\B	��B	�B	�*B
 �B
	lB
�B
kB
�B
0UB
&B
_B
	B
�B
jB
;B
�B
�B
 �B
"�B
%,B
%FB
'�B
*�B
-]B
.B
.B
+�B
+�B
-)B
.B
1[B
2�B
5ZB
5?B
6FB
;�B
<PB
;�B
;B
<B
<�B
=�B
>�B
?�B
?�B
BuB
C�B
DgB
ESB
E�B
G�B
G�B
H1B
HKB
IlB
JXB
J=B
K�B
J�B
J�B
KxB
JXB
J#B
I�B
KB
J�B
KB
J�B
K^B
MPB
I�B
IB
J�B
I�B
K)B
G�B
G�B
G�B
IB
I�B
HKB
GB
G�B
HfB
G�B
H�B
F�B
F?B
F�B
EmB
DB
D�B
D�B
E�B
D�B
C�B
A�B
A�B
@4B
?B
?�B
?cB
>wB
?B
>�B
>wB
>wB
>�B
<�B
<B
<PB
9�B
9rB
:�B
:�B
7�B
6B
7B
7�B
2GB
1�B
0!B
/�B
0!B
/iB
/B
.�B
/ B
/iB
.�B
-�B
-B
-)B
,�B
,�B
,�B
+kB
+B
+6B
,B
*KB
)*B
'�B
'8B
'8B
&�B
'�B
$�B
$tB
%B
$B
!�B
#:B
"�B
 'B
�B
�B
5B
jB
�B
;B
dB
�B
]B
WB
B
B
kB
B
�B
�B
�B
�B
�B
9B
SB
B
�B
B
�B
B
�B
�B
�B
�B
gB
B
B
B
{B
[B
�B
TB
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
6B
PB
PB
B
JB
�B
�B
B
�B
JB
�B
�B
JB
�B
jB
B
B
0B
�B
~B
jB
B
�B
B
PB
JB
B
�B
�B
6B
B
B
VB
B
�B
jB
B
B
6B
�B
�B
pB
"B
�B
vB
�B
�B
B
vB
HB
�B
�B
NB
}B
 B
�B
HB
hB
�B
�B
 B
�B
�B
 B
�B
�B
�B
&B
�B
�B
�B
:B
TB
B
aB
[B
uB
�B
�B
uB
�B
B
B
�B
MB
�B
�B
gB
�B
�B
9B
9B
�B
�B
yB
�B
?B
yB
yB
�B
EB
B
+B
_B
B
�B
�B
B
kB
	B
�B
qB
�B
xB
]B
�B
xB
xB
�B
/B
CB
]B
)B
�B
=B
�B
B
�B
�B
�B
�B
�B
B
B
B
5B
�B
VB
B
pB
�B
 'B
 vB
!�B
"�B
"�B
!�B
!�B
"B
# B
"�B
# B
"�B
#nB
#�B
$B
$�B
%�B
%�B
&LB
'B
'8B
'mB
'�B
'�B
(�B
)*B
)DB
(�B
)yB
)yB
)yB
)�B
*0B
*KB
*B
*�B
*�B
*�B
*eB
*B
*�B
+�B
+�B
,"B
,�B
+�B
+�B
,"B
,=B
+�B
+kB
,qB
+�B
+�B
,=B
+�B
+�B
,B
+�B
+�B
,=B
,qB
,�B
,�B
-�B
.�B
/iB
/B
.�B
.�B
/�B
0!B
0oB
0�B
0oB
0!B
0�B
0�B
0oB
0�B
0�B
0oB
1B
1[B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2-B
2�B
2�B
3�B
3�B
3�B
4B
4�B
5B
4�B
5B
5�B
6`B
6`B
6�B
6zB
7B
72B
72B
7fB
7LB
7fB
7�B
7�B
8B
8lB
8�B
9$B
9$B
9�B
9�B
:xB
:�B
:xB
:�B
;JB
;B
<�B
=qB
="B
<�B
="B
=�B
=<B
=<B
>B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?.B
?.B
?B
?�B
@OB
@4B
@iB
@�B
@�B
AB
@�B
@�B
BB
B[B
B�B
B'B
B[B
C�B
B�B
B�B
B�B
DMB
C�B
CGB
DB
EB
E�B
E�B
E�B
E�B
E�B
E�B
FB
FB
E�B
FB
FtB
F�B
GB
F�B
F�B
F�B
F�B
GB
G+B
GzB
G�B
HKB
HKB
H�B
I7B
H�B
H�B
IlB
I7B
IRB
IB
IB
IlB
I�B
I�B
J�B
JXB
J�B
K�B
K�B
LJB
L�B
K�B
L�B
LdB
LdB
M�B
NpB
N�B
NpB
NB
OvB
PbB
PHB
P}B
O�B
PHB
QNB
P.B
O�B
O�B
O�B
PbB
P�B
R�B
TaB
T{B
T,B
TFB
TFB
T�B
UB
U�B
U�B
VSB
V9B
VmB
V�B
V�B
V9B
V�B
WYB
W�B
W�B
W�B
X+B
XyB
X�B
X�B
X�B
YB
Y�B
Y�B
Y1B
YeB
YB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZB
Z�B
[#B
[qB
[�B
[�B
\B
\)B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
^B
]�B
]�B
]�B
]�B
^jB
^�B
_;B
_;B
_B
_pB
_VB
_�B
_�B
`\B
`�B
`�B
`�B
a-B
`�B
a-B
aB
abB
a-B
a|B
bB
a�B
a�B
a�B
a�B
a�B
a�B
b�B
b�B
bhB
bNB
b�B
b�B
c B
c B
c:B
c�B
c�B
c�B
c�B
c�B
c�B
dtB
d�B
d�B
eB
eB
eFB
e�B
ffB
fLB
fLB
fLB
fLB
f�B
g8B
h>B
h>B
iB
iB
i�B
iB
i_B
i�B
i�B
i�B
i�B
i�B
i�B
jeB
j�B
k6B
kB
k�B
k�B
k�B
lB
l=B
lqB
lqB
l�B
l�B
mwB
m]B
mwB
m�B
mCB
mCB
mwB
mwB
mCB
m]B
m�B
nB
o B
oiB
n�B
oiB
o�B
o�B
p�B
qB
p�B
qB
p�B
p�B
q'B
rGB
qB
q[B
q[B
q�B
q�B
q�B
r-B
raB
r|B
r�B
tTB
t�B
t�B
t9B
t�B
t�B
t�B
uB
u?B
utB
u�B
u�B
u�B
vB
v�B
vzB
vFB
v�B
vzB
v�B
wB
wLB
wLB
w�B
xB
xRB
xlB
xlB
x�B
x�B
x�B
x�B
y	B
x�B
x�B
y$B
y�B
y�B
z�B
zB
zxB
zDB
z�B
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{dB
{�B
{B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|jB
|�B
|�B
}"B
}�B
}qB
}VB
}VB
}B
|�B
}�B
~]B
~�B
~�B
~BB
~�B
~BB
~�B
~�B
HB
�B
cB
�B
��B
�OB
�B
��B
��B
�OB
��B
��B
��B
� B
�UB
�;B
�UB
��B
��B
��B
��B
�B
�'B
�'B
�AB
�uB
��B
��B
��B
��B
�3B
��B
�B
�9B
��B
��B
�mB
��B
��B
�%B
�?B
�tB
�?B
��B
��B
�B
�EB
�EB
��B
��G�O�B�DB�yB�
B��B��B��B��B�B�B��B�sB��B�B�B�B�fB�
B�B��B�mB�B�sB�B�B��B�QB�B��B�KB�B�QB��B�B�KB�KB�KB�WB��B�B�B�]B�cB��B�cB�]B�B��B�5B�oB�B�;B�B��B�AB�B�B�oB�;B�oB�B�%B�B��B��B�`B��B��B�B��B�fB��B��B�2B��B�>B�xB��B��B��B�B�VB�VB�"B�.B��BoB�B�B+BPBoB=B#nB"4B5tB=qB?B_�BjB� B��B��B�B	�B	�B	!�B	$@B	6zB	@�B	P�B	]�B	e�B	m�B	m�B	��B	��B	��B	�wB	�}B	��B	��B	��B	��B	�UB	�!B	��B	��B	��B	��B	��B	�OB	�B	�CB	�B	��B	�B	��B	�?B	��B	��B	�qB	�EB	�BB	��B	�
B	�B	��B	��B
oB
�B
 'B
;�B
+6B
$�B
(XB
.�B
.B
2�B
8�B
.�B
2aB
:*B
?�B
F�B
K�B
G�B
I�B
S�B
N�B
t�B
[#B
b�B
b�B
l�B
t�B
i�B
v�B
x�B
f�B
ffB
c B
g�B
iyB
|�B
e`B
ncB
|�B
��B
zxB
�=B
�B
�bB
��B
�_B
�B
�{B
�$B
�&B
�BGB&�B2�B0�B3�B8�B7�B8�B<6B8�B<6B>BB�BGBG�BJ�BS&BT�BR�BS&BX�B_�Bm�Bt�By	Bx�B��B��B�VB�B�B�OB��B��B��B�hB�LB��B��B��B�-B�mB��B�tB�BǮBȀB�#B˒B�B�}B��BچBߤB�,B�yB�B�B�`B��B�TB��B��B�.B��B��B��B��B��B�cB;B �B �B  BGB�B�BGB1B�BfB�B
�B�B\B�B�B(B\B.BbBbBhB�B@BB�B�B@B�BB�B�BYBIB�B�B�B_B�B_B�B�BSB@BuB�B�B�B:B B"B"BxB
rB
�B
=B
�B�B
=B	7B�BB_B�BPB.B
�B(BBDBxBDBB�B�B"B�B�BB�B�B.B+B
	B �B�B�B�BBoBhB�B�B�B B�B(BBMBeB�B,qB.IB0!B.B.�B,�B-B+�B+kB+6B)�B(�B(�B($B($B(�B(�B'B"�B%FB%B&B%B)�B"hB3hB~B�B��B��B��B�JB��B�VB��B�]B �B��B��B�B�B��B��B��B�BٴB�,B��B�dBʌB�B��B��B�B��B��B��Bl�Bm�B~�BW�B#�B
rB\B�B
�B  B
��B
�B
�B
�B
��B
�aB
�
B
�BB
�HB
уB
�,B
ǮB
��B
��B
�B
ΥB
�qB
�jB
�UB
��B
��B
��B
��B
�'B
�_B
�:B
��B
�B
�GB
[�B
R�B
e�B
Z�B
=qB
5B
/�B
5B
C�B
/OB
'B
�B
VB
�B
+B
�B
B
	B
SB	�B	��B
AB	��B	�B	�KB	��B	�sB	�|B	یB	�EB	�#B	�B	ݘB	�BB	��B	҉B	�NB	�sB	ɺB	ǮB	�wB	�B	B	��B	�dB	�B	�^B	�UB	��B	�nB	�UB	�B	��B	��B	��B	��B	�tB	�B	��B	�XB	��B	�^B	��B	�6B	��B	̘B	՛B	��B	��B	��B	�*B	��B	��B	�LB	�HB	��B	��B	��B	��B	��B	��B	�~B	�FB	��B	��B	�eB	�B	��B	v�B	n/B	n/B	q�B	k�B	i�B	hsB	g�B	_B	_�B	V�B	Q�B	P�B	PB	QB	QNB	OvB	OBB	N�B	MjB	M�B	M6B	L�B	MB	J�B	IB	H�B	MB	MB	M�B	B[B	@B	D3B	B[B	B'B	8RB	6�B	6FB	5?B	8�B	7�B	3�B	,�B	"�B	%�B	�B	@B	F?B	6FB	�B	B	�B	 iB	B��B��B��B��B��B	�B	%B��B�B�8B��B�B��B��B�8B��B��B�|B��B��B��B�`B��B��B��B�WB	
rB�B��B��B�WB�B�5B�DB�B�B�cB�,B�B��B�"B��B�ZB�vB�)BȀB�UB��B��B�LB��B��B�'B�^B�B��B�BBуBΥB	B�BϫB͟B�B�vBԕB�B��B�B�5B��B��B� B�B��B��B�B��B�B�B�8B�B��B�(B	�B	+B	B	�B	uB		�B	�B	
�B	
�B	!B	FB	IB	YB	�B	=B	kB	=B	�B	qB		B	�B	 'B	!�B	"�B	#�B	&�B	*�B	/B	>B	B�B	GzB	S�B	WsB	[WB	^�B	_B	a|B	d&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��<��g<X+c<�a�<���<֙a<�i�=a�<sH�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(� <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<v|<<'Y|<(� <8�<��c<�'v<@kk<#�
<#�
<Sg�<���<�'�<��S<sH�<@kk<*v�<q�Q<T��<#�
<#�
<#�
<$=�<#�
<5ib<���<` -<#�
<#�
<#�
<#�
<#�
<>fn<���<T�:<#�
<#�
<#�
<-�<z�<���<��<#�
<>fn<0;�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018110723375520181107233755IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010320190106200103QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010320190106200103QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551220190521075512IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                