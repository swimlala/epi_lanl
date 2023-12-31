CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:45Z creation      
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
_FillValue                 �  [`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɠ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  р   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � `   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230426223245  20230426223245  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�nj�{�@�nj�{�11  @�n�J�@�n�J�@+�%�ם
@+�%�ם
�c2ﲪ��c2ﲪ�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AB  AB  ?�  @�\@B�\@�G�@�G�@�G�@�  A ��A��A\)A+�A@��A`  A�  A�  A�  A�Q�A�  A�  A�  A�B   B(�B  B(�B (�B(  B/�
B7�
B?�
BG�
BO�BW�
B`  Bg�Bo�
Bx(�B�=qB�=qB�{B�{B�  B�  B�  B�  B�{B�{B�  B��B��B��B��B��B��B�  B��B�  B�{B�  B�  B�  B�{B�{B�  B�  B�  B�  B��B��C 
=C
=C  C  C  C
  C
=C  C��C
=C
=C  C��C  C
=C  C��C!��C#��C&  C(
=C)��C+��C.  C0  C2  C3��C5�C7��C:
=C<  C=��C?��CB  CD
=CE��CG�HCJ  CL  CN  CP
=CR{CT  CV  CX
=CZ  C\  C^
=C_��Cb  Cc��Ce��Ch
=Cj  Ck�Cm�Co�Cq�Cs�Cv  Cx
=Cz  C{��C}��C�C���C�  C�  C�C�  C�C�C�C�C�C�C�C�  C�  C�  C���C���C���C���C���C���C�  C�  C�C�C�
=C�
=C�  C�  C�
=C�C�
=C�
=C�C�  C�  C�C�C�  C�C�C���C�  C�  C�  C���C���C�  C���C���C�
=C�
=C�  C�  C�C���C�  C�C�  C�  C�  C�  C���C�  C���C�  C�
=C�
=C�  C�  C�
=C�
=C�
=C�C���C�  C�  C�  C�  C�  C�
=C�  C�C�
=C�C�  C���C���C��C���C�C�  C�
=C�C���C���C���C�C�C���C���C�  C�  C���C�  C�  C��C���C�
=C�
=C�
=C�
=C�C�C�  C���C��C�  C�
=C�
=C�
=C�C�  C���C�  C�C�  C���D � D�D� D  Dz�D  D��D  D}qD�D�D�D��D  D}qD��D� D	�D	� D
  D
}qD  D��D  D}qD�qD}qD��D}qD�D� D  D��D  D}qD��D� D�D��D  Dz�D�qD��D�qD� D  D� D  Dz�D�qD� D�qD� DD��D�D� D  D� D�qD� D  D� D   D }qD ��D!� D"  D"}qD"�qD#}qD$  D$� D%  D%� D&  D&��D'�D'� D(  D(� D)  D)� D*  D*� D+  D+}qD,  D,� D,�qD-}qD-�qD.��D/�D/� D/�qD0z�D0��D1}qD1�qD2}qD3�D3�D4�D4� D5  D5��D6�D6��D7  D7��D8�D8� D9  D9��D:  D:� D;  D;��D<  D<}qD=  D=}qD=��D>}qD?  D?��D@�D@� DA  DA� DA�qDB}qDC  DC� DD  DD� DD�qDE� DF�DF� DG�DG��DH�DH��DH�qDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN��DODO��DO�qDP� DQ  DQ��DR  DR� DS�DS}qDT  DT}qDT�qDU}qDU�qDV}qDW  DW� DX  DX}qDX�qDY}qDY�qDZ}qD[  D[��D\D\� D\�qD]� D^  D^}qD^�qD_� D`�D`� Da  Da��DbDb��Dc  Dc}qDd  Dd��Dd�qDe}qDf  Df}qDg  Dg��Dh  Dh� Di  Di}qDi�qDj� Dj�qDk��Dl  Dl}qDm  Dm}qDn  Dn� Dn�qDo� Dp  Dp}qDq  Dq�Dr�Dr��Ds  Ds� Ds�qDt}qDu  Du� Dv  Dv��Dv�qDw}qDx  Dx}qDy  Dy� Dy�qDz� D{�D{� D|  D|��D}  D}��D~  D~� D  D}qD�qD�>�D��HD��HD���D�>�D��HD��HD���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�@ D��HD��HD�  D�@ D�� D�� D���D�>�D�}qD�� D�HD�B�D���D��HD���D�@ D��HD�D�HD�@ D��HD�D�HD�@ D�~�D���D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�AHD��HD���D��qD�@ D�� D��HD�HD�AHD�� D���D�  D�>�D�~�D�� D���D�>�D�~�D���D�  D�AHD��HD�� D���D�>�D��HD�� D��qD�=qD�� D�� D��qD�>�D�~�D�� D�HD�AHD�� D�� D�  D�AHD�� D��HD�HD�@ D�~�D��qD���D�@ D�� D��HD�HD�B�D��HD�� D�HD�@ D�~�D�� D�  D�>�D�� D���D���D�AHD�� D�� D�HD�@ D��HD�� D�  D�AHD��HD���D���D�@ D�~�D�� D�HD�@ D�~�D��qD���D�@ D��HD���D��D�AHD�� D��HD�  D�@ D��HD���D���D�AHD��HD��HD�HD�@ D��HD��HD���D�>�D���D�D��D�AHD�~�D�� D�HD�>�D�� D�� D���D�@ D�}qD��qD���D�@ D�� D�� D�  D�AHD�~�D���D�  D�>�D�~�D���D�  D�@ D�� D��HD�HD�B�D�� D��qD���D�>�D�� D��HD�HD�>�D�}qD���D�  D�>�D�}qD���D�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD��HD�� D�HD�AHD�� D���D�  D�@ D�� D���D�HD�@ D�� D��HD�HD�@ D�� D�� D�HD�AHD�� D��HD�  D�@ D�� D�� D�HD�AHD�� D���D�  D�@ D��HD��HD�HD�AHD��HD�� D�  D�>�D�� D�D�  D�>�D D��HD�HD�>�DÀ D�� D�  D�@ DĀ Dľ�D���D�@ DŁHD�� D�  D�@ D�~�Dƾ�D�  D�AHDǀ DǾ�D�HD�@ D�}qDȾ�D�  D�@ D�~�D�� D�  D�AHDʀ D�� D�  D�@ Dˀ D�� D�HD�AHD̀ D�� D�  D�@ D̀ D;�D���D�AHD΁HD�� D��D�AHDπ D��HD�  D�@ DЁHD��HD�  D�@ D�~�DѾ�D�  D�@ DҁHD�D��D�@ DӀ D�� D�  D�>�D�~�DԾ�D�  D�@ DՀ D�� D�  D�@ D�}qD־�D�  D�AHD׀ D׽qD���D�@ D�~�Dؾ�D���D�@ Dـ Dپ�D��qD�>�Dڀ D�� D�  D�@ D�~�D۾�D���D�>�D�~�D�� D�HD�>�D݀ D��HD�  D�AHDށHD޽qD��qD�@ D߂�D�� D���D�@ D��HDྸD�  D�AHD�HD��HD���D�@ D�HD��HD���D�>�D� D��HD�HD�AHD� D�� D�HD�AHD� D�� D�  D�AHD�HD�� D�  D�@ D�HD��HD�  D�=qD�~�D�� D�  D�AHD� D龸D���D�@ D�HD�� D���D�>�D�~�D�qD�  D�@ D� D�� D�  D�@ D�}qD��qD�  D�AHD� D��HD���D�=qD�~�DﾸD���D�AHD�� D�D�HD�@ D� D�D�HD�AHD�HD�D���D�AHD�HD�D���D�>�D�~�D��HD�HD�>�D�~�D�� D�  D�AHD��HD��HD���D�@ D�� D��HD�HD�AHD���D��HD�HD�AHD��HD��HD�HD�9�D���>���?B�\?��?��R?Ǯ?�@��@(�@.{@E�@W
=@h��@}p�@��@��@�p�@�ff@�\)@���@��
@�{@ٙ�@��
@���@�A   A�A
�HA  A�
A��A{A#33A(Q�A,��A1G�A7
=A<(�AAG�AEAJ�HAO\)ATz�AZ=qA_\)Adz�Ah��An{As�
Ax��A}p�A�G�A��A�ffA�G�A��
A�{A�Q�A�33A�{A���A��HA��A�  A��HA�p�A��A�=qA��A�  A��\A���A�
=A��A�z�A�\)A�=qA���A�
=Aə�A�z�AϮA�=qA���A�
=Aٙ�A�(�A�
=A��A���A�\)A陚A�(�A�
=A��A�z�A�
=A�G�A��
A�ffB ��B�B\)Bz�BB
=Bz�B	�B33Bz�BB�HBQ�B�B33Bz�BB
=Bz�B�B33B��B�B33B z�B!�B#\)B$��B&{B'\)B(��B*{B+\)B,��B.=qB/�B0��B1�B3�B4��B6=qB7�B8��B:{B;�B<��B>ffB?�B@��BB{BC�BD��BFffBG�BH��BJ{BK\)BL��BN{BO�BP��BQBS
=BTz�BU�BW33BXz�BY�B[33B\z�B]B_33B`��Bb=qBc�Bd��Bf=qBg�Bi�Bj�\Bl(�BmBo33Bp��Br=qBs�Bt��Bv�\BxQ�ByB{33B|z�B}B
=B�(�B��RB�G�B��
B�=qB���B���B�G�B��B�  B�Q�B��\B��HB��B�G�B���B��B�=qB���B��HB��B�p�B��B��B�=qB��\B��HB�33B��B��B�  B�=qB��\B���B�33B�p�B�B�{B�ffB���B��HB��B�\)B��B�  B�Q�B��\B���B��B�\)B���B�  B�Q�B��\B��HB��B�\)B���B��
B�(�B�z�B���B��B�\)B��B��B�(�B�ffB���B���B�G�B���B��B�=qB�z�B���B���B�33B�p�B�B�{B�ffB��RB�
=B�\)B��B�B�{B�ffB���B���B�G�B���B��B�{B�ffB���B��HB�G�B��B��
B�(�B�z�B���B��B�\)B���B��B�=qB��\B��HB�G�B���B�  B�=qB���B��HB�33B�p�B�B�{B�z�B���B��B��B��
B�(�B��\B��HB�33B��B��
B�{B�ffB��RB�
=B�\)B��B�{B�ffB��RB�
=B�\)B��B��B�=qB�z�B���B��B�p�B�B�=qB��\B��HB�G�B��B�  B�Q�B���B���B�G�B��B�{B�z�B���B�33B���B��B�=qB���B���B�G�B��B�{B�z�B���B�\)B�B�(�B�z�B���B�\)B�B�(�B��\B���B�\)B�B�(�B���B��B���B�  B�z�B��HB�G�B�B�(�B���B�
=B��B��B�Q�B���B�G�BîB�(�Bģ�B��Bř�B�{BƏ\B�
=BǅB�  B�z�B��HB�\)B�B�=qBʸRB��B˙�B�{B̏\B���BͅB�  B�z�B���BυB��B�ffB��HB�p�B��B�ffB���B�G�B��
B�Q�B���B�G�B�B�=qBָRB�33B׮B�(�Bأ�B��Bٙ�B�{Bڏ\B���BۅB��B�z�B���B�p�B��B�Q�B��HB�\)B��
B�ffB��HB�p�B��B�z�B���B�p�B��
B�ffB���B�p�B�  B�z�B���B�B�{B�\B��B陚B�{B��B�33B�B�=qB�RB�G�B�B�Q�B��HB�\)B��B�z�B�
=B�B�{B��B�33B�B�=qB���B�G�B��
B�Q�B��HB�p�B�  B��\B��B��B�Q�B��HB�p�B�  B��\B��B���B�(�B��RB�G�B�C 33C p�C �RC  CG�C�\C�
C{CffC�C��C=qC�CC
=CQ�C��C�HC(�Cp�C�RC  CG�C�\C�
C�C\)C��C��C=qCz�CC	
=C	G�C	��C	��C
�C
\)C
��C
�
C{C\)C�\C�
C{CQ�C��C�
C�CffC��C�HC(�Cp�C�C�C(�CffC��C�HC�CffC�C�HC(�CffC�C�C(�CffC��C�C(�CffC��C�HC{CQ�C�\C�
C{C\)C��C�
C�C\)C��C�
C{CG�C�CC��C33CffC��C�HC�C\)C��C�HC�CffC�C�HC�C\)C��C��C{C\)C��C�HC(�CffC��C�HC�C\)C�\C�
C�C\)C��C�HC �C \)C ��C ��C!  C!G�C!�C!C"  C"G�C"�\C"��C#  C#=qC#p�C#�C#�C$33C$p�C$�RC$��C%=qC%p�C%�RC%�C&33C&p�C&�C&��C'=qC'�C'C(
=C(G�C(�C(��C){C)\)C)��C)�HC*(�C*z�C*C+
=C+=qC+�C+��C,{C,ffC,�RC-  C-=qC-z�C-C.
=C.Q�C.��C.��C/=qC/�C/��C0
=C0Q�C0��C0�HC133C1z�C1��C2{C2ffC2��C2�C333C3z�C3��C4{C4ffC4�RC5  C5Q�C5��C5�C6=qC6z�C6C7{C7p�C7C8{C8\)C8�C8�C9=qC9�\C9�HC:=qC:�C:��C;�C;p�C;�RC<
=C<\)C<�C=
=C=\)C=��C=��C>=qC>�\C>�
C?=qC?�\C?�HC@33C@�C@�
CA(�CAz�CA�
CB(�CBz�CB�
CC(�CCz�CC��CD{CDp�CDCE�CEz�CE��CF�CFp�CFCG�CGz�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                  ?�  @�\@B�\@�G�@�G�@�G�@�  A ��A��A\)A+�A@��A`  A�  A�  A�  A�Q�A�  A�  A�  A�B   B(�B  B(�B (�B(  B/�
B7�
B?�
BG�
BO�BW�
B`  Bg�Bo�
Bx(�B�=qB�=qB�{B�{B�  B�  B�  B�  B�{B�{B�  B��B��B��B��B��B��B�  B��B�  B�{B�  B�  B�  B�{B�{B�  B�  B�  B�  B��B��C 
=C
=C  C  C  C
  C
=C  C��C
=C
=C  C��C  C
=C  C��C!��C#��C&  C(
=C)��C+��C.  C0  C2  C3��C5�C7��C:
=C<  C=��C?��CB  CD
=CE��CG�HCJ  CL  CN  CP
=CR{CT  CV  CX
=CZ  C\  C^
=C_��Cb  Cc��Ce��Ch
=Cj  Ck�Cm�Co�Cq�Cs�Cv  Cx
=Cz  C{��C}��C�C���C�  C�  C�C�  C�C�C�C�C�C�C�C�  C�  C�  C���C���C���C���C���C���C�  C�  C�C�C�
=C�
=C�  C�  C�
=C�C�
=C�
=C�C�  C�  C�C�C�  C�C�C���C�  C�  C�  C���C���C�  C���C���C�
=C�
=C�  C�  C�C���C�  C�C�  C�  C�  C�  C���C�  C���C�  C�
=C�
=C�  C�  C�
=C�
=C�
=C�C���C�  C�  C�  C�  C�  C�
=C�  C�C�
=C�C�  C���C���C��C���C�C�  C�
=C�C���C���C���C�C�C���C���C�  C�  C���C�  C�  C��C���C�
=C�
=C�
=C�
=C�C�C�  C���C��C�  C�
=C�
=C�
=C�C�  C���C�  C�C�  C���D � D�D� D  Dz�D  D��D  D}qD�D�D�D��D  D}qD��D� D	�D	� D
  D
}qD  D��D  D}qD�qD}qD��D}qD�D� D  D��D  D}qD��D� D�D��D  Dz�D�qD��D�qD� D  D� D  Dz�D�qD� D�qD� DD��D�D� D  D� D�qD� D  D� D   D }qD ��D!� D"  D"}qD"�qD#}qD$  D$� D%  D%� D&  D&��D'�D'� D(  D(� D)  D)� D*  D*� D+  D+}qD,  D,� D,�qD-}qD-�qD.��D/�D/� D/�qD0z�D0��D1}qD1�qD2}qD3�D3�D4�D4� D5  D5��D6�D6��D7  D7��D8�D8� D9  D9��D:  D:� D;  D;��D<  D<}qD=  D=}qD=��D>}qD?  D?��D@�D@� DA  DA� DA�qDB}qDC  DC� DD  DD� DD�qDE� DF�DF� DG�DG��DH�DH��DH�qDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN��DODO��DO�qDP� DQ  DQ��DR  DR� DS�DS}qDT  DT}qDT�qDU}qDU�qDV}qDW  DW� DX  DX}qDX�qDY}qDY�qDZ}qD[  D[��D\D\� D\�qD]� D^  D^}qD^�qD_� D`�D`� Da  Da��DbDb��Dc  Dc}qDd  Dd��Dd�qDe}qDf  Df}qDg  Dg��Dh  Dh� Di  Di}qDi�qDj� Dj�qDk��Dl  Dl}qDm  Dm}qDn  Dn� Dn�qDo� Dp  Dp}qDq  Dq�Dr�Dr��Ds  Ds� Ds�qDt}qDu  Du� Dv  Dv��Dv�qDw}qDx  Dx}qDy  Dy� Dy�qDz� D{�D{� D|  D|��D}  D}��D~  D~� D  D}qD�qD�>�D��HD��HD���D�>�D��HD��HD���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�@ D��HD��HD�  D�@ D�� D�� D���D�>�D�}qD�� D�HD�B�D���D��HD���D�@ D��HD�D�HD�@ D��HD�D�HD�@ D�~�D���D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�AHD��HD���D��qD�@ D�� D��HD�HD�AHD�� D���D�  D�>�D�~�D�� D���D�>�D�~�D���D�  D�AHD��HD�� D���D�>�D��HD�� D��qD�=qD�� D�� D��qD�>�D�~�D�� D�HD�AHD�� D�� D�  D�AHD�� D��HD�HD�@ D�~�D��qD���D�@ D�� D��HD�HD�B�D��HD�� D�HD�@ D�~�D�� D�  D�>�D�� D���D���D�AHD�� D�� D�HD�@ D��HD�� D�  D�AHD��HD���D���D�@ D�~�D�� D�HD�@ D�~�D��qD���D�@ D��HD���D��D�AHD�� D��HD�  D�@ D��HD���D���D�AHD��HD��HD�HD�@ D��HD��HD���D�>�D���D�D��D�AHD�~�D�� D�HD�>�D�� D�� D���D�@ D�}qD��qD���D�@ D�� D�� D�  D�AHD�~�D���D�  D�>�D�~�D���D�  D�@ D�� D��HD�HD�B�D�� D��qD���D�>�D�� D��HD�HD�>�D�}qD���D�  D�>�D�}qD���D�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD��HD�� D�HD�AHD�� D���D�  D�@ D�� D���D�HD�@ D�� D��HD�HD�@ D�� D�� D�HD�AHD�� D��HD�  D�@ D�� D�� D�HD�AHD�� D���D�  D�@ D��HD��HD�HD�AHD��HD�� D�  D�>�D�� D�D�  D�>�D D��HD�HD�>�DÀ D�� D�  D�@ DĀ Dľ�D���D�@ DŁHD�� D�  D�@ D�~�Dƾ�D�  D�AHDǀ DǾ�D�HD�@ D�}qDȾ�D�  D�@ D�~�D�� D�  D�AHDʀ D�� D�  D�@ Dˀ D�� D�HD�AHD̀ D�� D�  D�@ D̀ D;�D���D�AHD΁HD�� D��D�AHDπ D��HD�  D�@ DЁHD��HD�  D�@ D�~�DѾ�D�  D�@ DҁHD�D��D�@ DӀ D�� D�  D�>�D�~�DԾ�D�  D�@ DՀ D�� D�  D�@ D�}qD־�D�  D�AHD׀ D׽qD���D�@ D�~�Dؾ�D���D�@ Dـ Dپ�D��qD�>�Dڀ D�� D�  D�@ D�~�D۾�D���D�>�D�~�D�� D�HD�>�D݀ D��HD�  D�AHDށHD޽qD��qD�@ D߂�D�� D���D�@ D��HDྸD�  D�AHD�HD��HD���D�@ D�HD��HD���D�>�D� D��HD�HD�AHD� D�� D�HD�AHD� D�� D�  D�AHD�HD�� D�  D�@ D�HD��HD�  D�=qD�~�D�� D�  D�AHD� D龸D���D�@ D�HD�� D���D�>�D�~�D�qD�  D�@ D� D�� D�  D�@ D�}qD��qD�  D�AHD� D��HD���D�=qD�~�DﾸD���D�AHD�� D�D�HD�@ D� D�D�HD�AHD�HD�D���D�AHD�HD�D���D�>�D�~�D��HD�HD�>�D�~�D�� D�  D�AHD��HD��HD���D�@ D�� D��HD�HD�AHD���D��HD�HD�AHD��HD��HD�HD�9�D���>���?B�\?��?��R?Ǯ?�@��@(�@.{@E�@W
=@h��@}p�@��@��@�p�@�ff@�\)@���@��
@�{@ٙ�@��
@���@�A   A�A
�HA  A�
A��A{A#33A(Q�A,��A1G�A7
=A<(�AAG�AEAJ�HAO\)ATz�AZ=qA_\)Adz�Ah��An{As�
Ax��A}p�A�G�A��A�ffA�G�A��
A�{A�Q�A�33A�{A���A��HA��A�  A��HA�p�A��A�=qA��A�  A��\A���A�
=A��A�z�A�\)A�=qA���A�
=Aə�A�z�AϮA�=qA���A�
=Aٙ�A�(�A�
=A��A���A�\)A陚A�(�A�
=A��A�z�A�
=A�G�A��
A�ffB ��B�B\)Bz�BB
=Bz�B	�B33Bz�BB�HBQ�B�B33Bz�BB
=Bz�B�B33B��B�B33B z�B!�B#\)B$��B&{B'\)B(��B*{B+\)B,��B.=qB/�B0��B1�B3�B4��B6=qB7�B8��B:{B;�B<��B>ffB?�B@��BB{BC�BD��BFffBG�BH��BJ{BK\)BL��BN{BO�BP��BQBS
=BTz�BU�BW33BXz�BY�B[33B\z�B]B_33B`��Bb=qBc�Bd��Bf=qBg�Bi�Bj�\Bl(�BmBo33Bp��Br=qBs�Bt��Bv�\BxQ�ByB{33B|z�B}B
=B�(�B��RB�G�B��
B�=qB���B���B�G�B��B�  B�Q�B��\B��HB��B�G�B���B��B�=qB���B��HB��B�p�B��B��B�=qB��\B��HB�33B��B��B�  B�=qB��\B���B�33B�p�B�B�{B�ffB���B��HB��B�\)B��B�  B�Q�B��\B���B��B�\)B���B�  B�Q�B��\B��HB��B�\)B���B��
B�(�B�z�B���B��B�\)B��B��B�(�B�ffB���B���B�G�B���B��B�=qB�z�B���B���B�33B�p�B�B�{B�ffB��RB�
=B�\)B��B�B�{B�ffB���B���B�G�B���B��B�{B�ffB���B��HB�G�B��B��
B�(�B�z�B���B��B�\)B���B��B�=qB��\B��HB�G�B���B�  B�=qB���B��HB�33B�p�B�B�{B�z�B���B��B��B��
B�(�B��\B��HB�33B��B��
B�{B�ffB��RB�
=B�\)B��B�{B�ffB��RB�
=B�\)B��B��B�=qB�z�B���B��B�p�B�B�=qB��\B��HB�G�B��B�  B�Q�B���B���B�G�B��B�{B�z�B���B�33B���B��B�=qB���B���B�G�B��B�{B�z�B���B�\)B�B�(�B�z�B���B�\)B�B�(�B��\B���B�\)B�B�(�B���B��B���B�  B�z�B��HB�G�B�B�(�B���B�
=B��B��B�Q�B���B�G�BîB�(�Bģ�B��Bř�B�{BƏ\B�
=BǅB�  B�z�B��HB�\)B�B�=qBʸRB��B˙�B�{B̏\B���BͅB�  B�z�B���BυB��B�ffB��HB�p�B��B�ffB���B�G�B��
B�Q�B���B�G�B�B�=qBָRB�33B׮B�(�Bأ�B��Bٙ�B�{Bڏ\B���BۅB��B�z�B���B�p�B��B�Q�B��HB�\)B��
B�ffB��HB�p�B��B�z�B���B�p�B��
B�ffB���B�p�B�  B�z�B���B�B�{B�\B��B陚B�{B��B�33B�B�=qB�RB�G�B�B�Q�B��HB�\)B��B�z�B�
=B�B�{B��B�33B�B�=qB���B�G�B��
B�Q�B��HB�p�B�  B��\B��B��B�Q�B��HB�p�B�  B��\B��B���B�(�B��RB�G�B�C 33C p�C �RC  CG�C�\C�
C{CffC�C��C=qC�CC
=CQ�C��C�HC(�Cp�C�RC  CG�C�\C�
C�C\)C��C��C=qCz�CC	
=C	G�C	��C	��C
�C
\)C
��C
�
C{C\)C�\C�
C{CQ�C��C�
C�CffC��C�HC(�Cp�C�C�C(�CffC��C�HC�CffC�C�HC(�CffC�C�C(�CffC��C�C(�CffC��C�HC{CQ�C�\C�
C{C\)C��C�
C�C\)C��C�
C{CG�C�CC��C33CffC��C�HC�C\)C��C�HC�CffC�C�HC�C\)C��C��C{C\)C��C�HC(�CffC��C�HC�C\)C�\C�
C�C\)C��C�HC �C \)C ��C ��C!  C!G�C!�C!C"  C"G�C"�\C"��C#  C#=qC#p�C#�C#�C$33C$p�C$�RC$��C%=qC%p�C%�RC%�C&33C&p�C&�C&��C'=qC'�C'C(
=C(G�C(�C(��C){C)\)C)��C)�HC*(�C*z�C*C+
=C+=qC+�C+��C,{C,ffC,�RC-  C-=qC-z�C-C.
=C.Q�C.��C.��C/=qC/�C/��C0
=C0Q�C0��C0�HC133C1z�C1��C2{C2ffC2��C2�C333C3z�C3��C4{C4ffC4�RC5  C5Q�C5��C5�C6=qC6z�C6C7{C7p�C7C8{C8\)C8�C8�C9=qC9�\C9�HC:=qC:�C:��C;�C;p�C;�RC<
=C<\)C<�C=
=C=\)C=��C=��C>=qC>�\C>�
C?=qC?�\C?�HC@33C@�C@�
CA(�CAz�CA�
CB(�CBz�CB�
CC(�CCz�CC��CD{CDp�CDCE�CEz�CE��CF�CFp�CFCG�CGz�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AмjAк^Aк^AоwAк^A���A���A���A�A���A���A��HA�r�AёhA�l�A�XA�C�A�/A��A��A�"�A�&�A�&�A�$�A� �A�bA�{A��A�{A�oA�VA��#A���AǾwA���A��7A��FA�I�A��/A��jA�%A�E�A�JA�dZA�dZA�A���A� �A�G�A���A�\)A���A�"�A�ƨA��`A��HA�A�5?A��PA�E�A�`BA�?}A��A���A��FA���A��7A�VA���A��A��A���A}O�Ay��AvĜAu33Ao�mAiAe"�A]��AYx�AXAU�AQ�
AM�TAM�7AK��AK|�AO�AQ%AJ��AI��AE?}AA��AA�wA@^5A;�A7/A7��A;�A@��AAG�AB�A@��A=�A=�A=��A=7LA;�;A:�HA;��A;�A9�PA7`BA6M�A4�`A2�HA1�A1"�A1%A1�hA1&�A0��A/�A/A.Q�A-�A-/A,-A+�-A+��A+hsA+&�A*�9A*E�A)�^A)t�A(��A(jA'\)A'�A&�`A&VA%��A%O�A$�A$~�A$��A%�A%dZA%l�A$��A$�A$n�A$^5A$$�A$1A#A#p�A#O�A#&�A#oA"�yA"VA!�#A!A!G�A �jA  �A�7A+A�yAI�A�TA;dA^5AA�TA�7A33A"�AS�AO�A�A  A7LA��A1A�wAS�A�+A1'A��A��A�AdZA;dA��AQ�A��A�FA`BA/A�AVA��AG�AVA��AQ�A��Ax�A��A9XA��A��A��AA��A?}AȴA�+AA�A�AG�A
ȴA
ffA
-A	�A	��A	�A	&�A�!A5?A�hA�A�HA�+A�mA|�A?}A33A��A�9A  A�7AK�AA��A�!A�uA=qA�FAC�A �A �A ��A ȴA ��A -@�33@��@��@���@��T@�hs@��@�b@���@�ff@��^@��/@�A�@�ƨ@�C�@�o@��@�~�@�@�7L@�w@�+@�33@��@�\@�v�@�+@�M�@��T@�@���@��-@�%@�@�Q�@�@�\@��@��@���@�9X@�F@��H@�^@�&�@�z�@��@�;d@�E�@� �@�33@���@�`B@�7L@���@�bN@�I�@��@ڸR@�5?@��T@�7L@�%@�z�@���@ו�@�|�@�S�@���@�~�@�{@���@Լj@�z�@Ӿw@ҟ�@���@ёh@�X@��`@�Z@ύP@�V@Ͳ-@�p�@�/@̓u@��@��m@�dZ@���@�^5@�$�@��#@�`B@��`@ȋD@�1@�l�@���@Ɵ�@�V@ŉ7@��@��@���@��@�Ĝ@ě�@�j@��@î@�K�@�o@§�@�M�@���@���@��@���@�?}@�?}@��`@�r�@�A�@���@�ƨ@��P@�S�@�33@���@���@�?}@��@��9@�Z@��@�33@��R@�V@�=q@�$�@�$�@��-@�?}@���@���@��u@�1'@��;@���@�t�@�S�@��@�$�@���@�O�@��@���@���@�z�@���@�|�@�33@�o@���@��\@�V@�-@�@�G�@���@�z�@�1@��F@�K�@�~�@��T@��-@�x�@�G�@�/@���@�Q�@�(�@�  @�ƨ@�|�@�S�@���@��@��@�7L@�/@��@���@���@�1'@�ƨ@��P@�
=@��!@�5?@�{@��^@��@�9X@��;@��@�|�@��y@�-@��#@��^@��h@�X@��@��`@�z�@�(�@�1@���@��P@�
=@�ȴ@�E�@���@�`B@���@��u@�b@��F@�S�@�@��@��\@�M�@���@�@��h@�O�@�%@��`@���@�I�@���@�K�@�"�@�
=@��y@��!@�^5@�=q@�{@��@��^@�x�@�`B@��@��@�Z@�1@��@��P@�l�@�C�@���@���@�V@��T@�X@��@��@���@�r�@�bN@�1@��m@���@��@�33@��R@���@���@�^5@�@���@�X@��@��`@��9@��D@�I�@���@��F@�\)@�33@���@���@��\@�ff@�E�@��T@��h@�p�@�X@��@���@���@�z�@�1'@��@��m@���@���@��P@�t�@�l�@�;d@�@��@���@��\@�M�@��@��-@�?}@��@���@�Ĝ@��@�1'@���@�;d@��@��\@��@�@��T@��^@��7@�x�@�p�@�O�@�&�@��@��j@�j@�A�@� �@�1@��@~$�@}�@}O�@}�@|��@|�@{��@{"�@z^5@z=q@y��@y�^@yhs@x�u@w�;@w�w@wl�@w�@v�R@vff@u�T@u�@u/@u�@t��@t��@tZ@s�F@sS�@r�H@r�\@rn�@r-@q%@p�9@p�@pQ�@o�w@n�y@n�+@nE�@n@m�T@m��@m��@mV@l��@lI�@l(�@l(�@k�m@k��@kdZ@kC�@j~�@i��@ihs@i�@h��@h��@h��@h�`@hr�@f�y@f$�@e�T@e@e�-@e�h@ep�@e?}@d�D@c�m@c33@b�H@b��@bn�@b^5@b-@b�@b�@a�@a�7@aG�@`��@`r�@_�@_\)@_+@_
=@^��@^��@^��@^�+@^V@]�@]?}@\�D@\Z@[�
@[�@[C�@[o@[@Z�@Z�H@Z~�@ZJ@YG�@X�@Xr�@XQ�@W�;@W|�@W�@V�@V�+@Vv�@Vff@U��@T�@Tz�@TI�@T(�@S�
@S��@S��@S��@S�@S�@St�@SdZ@S33@R��@RM�@R�@Q��@Qx�@P��@Pr�@P �@O��@Ol�@OK�@O+@N��@Nȴ@Nv�@NE�@N$�@N{@M�T@M�-@M�@M`B@M?}@L��@L�j@L�j@L��@Lz�@L�@Kƨ@K�@K33@J~�@I�@Ihs@I�@HĜ@G�@G��@G�w@Gl�@F��@F��@Fff@FE�@F{@E�T@E�-@E?}@D��@D9X@C��@B�@B^5@B-@A��@A�#@Ax�@AG�@@�`@@�@@bN@?�;@?|�@?;d@>��@>v�@>$�@=�@=��@=�-@=`B@=?}@=?}@<��@;ƨ@;dZ@;C�@;o@:�H@:��@:�\@:^5@:J@9�@9��@9G�@8��@8Q�@8Q�@8Q�@8  @7K�@6v�@65?@6$�@6@5�T@5��@5�@4�/@4��@4�j@49X@3dZ@2��@2=q@1�@17L@0�@0  @/l�@/
=@.5?@-@-V@,j@,(�@+�
@+@*�!@*�\@*^5@)�@)X@(�9@(bN@( �@'�w@'K�@'
=@&�@&�R@&��@&�+@&V@&{@%�T@%�-@%�h@%�@%�@%p�@%?}@$�/@$z�@$Z@$Z@$9X@$(�@$�@#ƨ@#�@#C�@#o@#@"�@"�H@"��@"��@"M�@"J@!�@!��@!�^@!��@!x�@!hs@!G�@!�@ �`@ Ĝ@ �9@ �u@ �u@ ��@ �@ A�@  �@   @�;@�w@�@��@�P@l�@�@��@ff@E�@E�@5?@5?@$�@{@@�T@�-@`B@�@�@�@��@��@9X@��@�F@��@t�@o@�H@^5@��@x�@%@��@�9@�9@��@�u@�@r�@A�@�@�@l�@K�@;d@�@��@��@ff@��@�h@p�@?}@�@V@�@Z@I�@9X@1@��@"�@�@�H@��@��@�!@n�@M�@=q@x�@��@��@��@b@��@��@�P@|�@|�AмjAмjAмjAк^AмjAмjAиRAк^AиRAиRAмjAоwAмjAоwAоwAмjAк^Aк^A���A���A�ĜA�AоwAоwAоwA���A�A���A���A���A�A�A���A�A���A��
A��A��
A��#A��/A��TA�JA�33A�ffA�v�AхAѝ�Aѧ�AѮAѧ�Aћ�Aщ7A�~�A�r�A�l�A�ffA�bNA�ffA�r�A�|�A�v�A�l�A�`BA�XA�M�A�E�A�?}A�A�A�C�A�E�A�E�A�A�A�=qA�9XA�7LA�33A�+A�"�A��A� �A�"�A�"�A��A��A��A��A��A��A��A��A��A��A� �A�$�A�&�A�&�A�"�A�"�A�$�A�(�A�(�A�(�A�&�A�$�A�&�A�(�A�(�A�(�A�&�A�$�A�"�A�$�A�&�A�&�A�$�A� �A��A� �A�"�A� �A� �A��A�{A�1A�%A�VA�{A�oA�oA�oA�oA�{A��A��A�{A�{A�{A��A��A��A�{A�oA�oA�{A�{A�{A�oA�bA�bA�bA�oA�oA�bA�bA�VA�JA�
=A�%A��yA��HA���AоwAа!A�p�A϶FAμjA�t�A�VA�VAɬA��A�
=A�+AƍPA�E�A���A�$�AľwA�I�A���A�dZA���A���A�
=A�S�A��TA�33A�ĜA�^5A�7LA���A�XA��!A��^A��TA��+A�ZA�K�A�?}A�1'A��A�JA���A��`A��#A���A�A��FA���A��7A�`BA��A��-A�A���A��A��hA�`BA�33A���A���A��!A��uA�z�A�G�A�(�A�  A��
A���A�;dA���A���A���A���A��\A�n�A�\)A�VA�VA�XA�bNA�l�A�r�A�hsA�dZA�x�A��A��A�p�A�7LA�  A��9A�7LA��/A�S�A��9A�O�A���A���A�ƨA��RA���A��hA��A�z�A�l�A�S�A�C�A�&�A���A��RA��DA�l�A�S�A�C�A�7LA�&�A��A���A���A���A�~�A�jA�S�A�;dA�/A��A��A�33A�x�A��A���A���A���A��A��A���A���A�A��A�1'A�I�A�33A�&�A�/A�=qA�9XA��A�A�A�1A�A��A�XA�G�A�E�A�G�A�l�A��DA��hA���A��7A�r�A�^5A�K�A�
=A��/A��
A��
A��A��HA���A�hsA�p�A�t�A�ffA�\)A�XA�M�A�O�A�K�A�E�A�1'A��A���A�~�A�$�A��uA� �A�~�A�;dA�A���A��HA��jA�x�A��HA�ĜA��jA��\A�~�A�t�A�^5A�/A�A��;A��uA�"�A���A�ZA��A��PA�ffA�;dA�/A�"�A��A�bA���A���A�M�A��A��yA�ƨA���A�\)A��`A�p�A��A��RA��A���A��A�ffA�\)A�Q�A�A�A�/A��A�A��A���A���A�\)A�5?A� �A�oA�  A��mA�A��-A���A��\A�~�A�Q�A�5?A���A��`A���A�ȴA��-A���A�|�A�dZA�I�A�33A��A�oA�A���A��mA���A���A���A��uA�v�A�Q�A�VA�ĜA���A��\A��PA�z�A���A���A�ĜA��RA��A���A���A�n�A�n�A�"�A��A���A�&�A���A��A�=qA��`A��^A��\A��DA��A�Q�A���A���A�hsA�E�A�VA�ƨA���A�ffA�$�A��/A�(�A���A�O�A��A�A��A�ȴA���A��uA�z�A�^5A�?}A� �A���A��RA��\A�1'A��A��^A�dZA�%A���A�Q�A�A�A�r�A�(�A���A��A�bA�~�A���A���A�$�A��A�C�A��A���A�7LA��A��mA�ffA�
=A���A�7LA��A��9A�S�A��+A��A�%A��A��FA�v�A�A�A��A��mA���A���A�~�A�Q�A� �A�JA�-A~�A}t�A{;dAz�`AzȴAz�!Az�\Az9XAy��Ay��Ay%Ax=qAw�FAw/Av�`Av��Av��Av��Av�AvI�Av  Au��AuhsAu�AuoAu
=AuAt�RAs��Ar�`Aq�Ap1AodZAn9XAm?}Al9XAj�HAjĜAi�;Ai�AidZAi/Ah��Ah�DAg+Af=qAex�Ae�Ad�jAd^5Ad(�Ac��Ab~�Aa%A_�7A^�9A]&�A[��A[?}AZ�AZ�DAZ5?AY�TAYAY`BAYVAX�jAX�AX�uAXn�AX �AXbAXAW�AW�mAW�-AWp�AV�DAV9XAV  AU�AU�AT�ATȴAT�RAT�+ATI�AShsAR��AQ��APn�AO�AO;dAN��ANA�AM�AMAM�FAM�AM��AM��AM��AM��AM��AM��AM��AM�AMx�AM?}AMoAL�RAL5?AK��AJ�AJ��AJ��AJ��AJ��AJ��AK�AK�AK�PAK�AK�AK�AK�AL��ANjANjAO+APz�AQ�AQ�7AQ�7AQ�hAQ��AR  AQ�;AQG�AOG�AO�hAM��AMVAL �AJ�`AJ1AIC�AH�AH�AI��AI��AI��AI�FAI�AI��AI|�AH1AG�AFȴAFbAC/AA�;AA�
AA��AA�;AA�
AA��AA�
AA��AA��AAƨAA��AA��AAƨAAƨAA�^AAx�AAoAA
=A@�A@��A?�PA?�A>VA<��A;t�A:�A:�DA9�A8A7XA733A7+A7&�A7"�A7�A7�A7oA7VA733A7+A7l�A:��A:�`A:��A:��A:�yA<I�A>v�A@v�A@�\A@��A@�jA@�RA@�!A@�A@�A@ȴAA&�AAC�AAAB��AB�/ACAC33AC7LAC"�ABv�AA�;AA��AAhsA@��A@-A?�A>�jA>A�A=�
A=��A=��A=�-A=��A=t�A=dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114414411111111144111111111111111111144111111111111111111111111111111111111111111114411111111111111111111111111111111111                                                                                                                                                                                                                                  AмjAк^Aк^AоwAк^A���A���A���A�A���A���A��HA�r�AёhA�l�A�XA�C�A�/A��A��A�"�A�&�A�&�A�$�A� �A�bA�{A��A�{A�oA�VA��#A���AǾwA���A��7A��FA�I�A��/A��jA�%A�E�A�JA�dZA�dZA�A���A� �A�G�A���A�\)A���A�"�A�ƨA��`A��HA�A�5?A��PA�E�A�`BA�?}A��A���A��FA���A��7A�VA���A��A��A���A}O�Ay��AvĜAu33Ao�mAiAe"�A]��AYx�AXAU�AQ�
AM�TAM�7AK��AK|�AO�AQ%AJ��AI��AE?}AA��AA�wA@^5A;�A7/A7��A;�A@��AAG�AB�A@��A=�A=�A=��A=7LA;�;A:�HA;��A;�A9�PA7`BA6M�A4�`A2�HA1�A1"�A1%A1�hA1&�A0��A/�A/A.Q�A-�A-/A,-A+�-A+��A+hsA+&�A*�9A*E�A)�^A)t�A(��A(jA'\)A'�A&�`A&VA%��A%O�A$�A$~�A$��A%�A%dZA%l�A$��A$�A$n�A$^5A$$�A$1A#A#p�A#O�A#&�A#oA"�yA"VA!�#A!A!G�A �jA  �A�7A+A�yAI�A�TA;dA^5AA�TA�7A33A"�AS�AO�A�A  A7LA��A1A�wAS�A�+A1'A��A��A�AdZA;dA��AQ�A��A�FA`BA/A�AVA��AG�AVA��AQ�A��Ax�A��A9XA��A��A��AA��A?}AȴA�+AA�A�AG�A
ȴA
ffA
-A	�A	��A	�A	&�A�!A5?A�hA�A�HA�+A�mA|�A?}A33A��A�9A  A�7AK�AA��A�!A�uA=qA�FAC�A �A �A ��A ȴA ��A -@�33@��@��@���@��T@�hs@��@�b@���@�ff@��^@��/@�A�@�ƨ@�C�@�o@��@�~�@�@�7L@�w@�+@�33@��@�\@�v�@�+@�M�@��T@�@���@��-@�%@�@�Q�@�@�\@��@��@���@�9X@�F@��H@�^@�&�@�z�@��@�;d@�E�@� �@�33@���@�`B@�7L@���@�bN@�I�@��@ڸR@�5?@��T@�7L@�%@�z�@���@ו�@�|�@�S�@���@�~�@�{@���@Լj@�z�@Ӿw@ҟ�@���@ёh@�X@��`@�Z@ύP@�V@Ͳ-@�p�@�/@̓u@��@��m@�dZ@���@�^5@�$�@��#@�`B@��`@ȋD@�1@�l�@���@Ɵ�@�V@ŉ7@��@��@���@��@�Ĝ@ě�@�j@��@î@�K�@�o@§�@�M�@���@���@��@���@�?}@�?}@��`@�r�@�A�@���@�ƨ@��P@�S�@�33@���@���@�?}@��@��9@�Z@��@�33@��R@�V@�=q@�$�@�$�@��-@�?}@���@���@��u@�1'@��;@���@�t�@�S�@��@�$�@���@�O�@��@���@���@�z�@���@�|�@�33@�o@���@��\@�V@�-@�@�G�@���@�z�@�1@��F@�K�@�~�@��T@��-@�x�@�G�@�/@���@�Q�@�(�@�  @�ƨ@�|�@�S�@���@��@��@�7L@�/@��@���@���@�1'@�ƨ@��P@�
=@��!@�5?@�{@��^@��@�9X@��;@��@�|�@��y@�-@��#@��^@��h@�X@��@��`@�z�@�(�@�1@���@��P@�
=@�ȴ@�E�@���@�`B@���@��u@�b@��F@�S�@�@��@��\@�M�@���@�@��h@�O�@�%@��`@���@�I�@���@�K�@�"�@�
=@��y@��!@�^5@�=q@�{@��@��^@�x�@�`B@��@��@�Z@�1@��@��P@�l�@�C�@���@���@�V@��T@�X@��@��@���@�r�@�bN@�1@��m@���@��@�33@��R@���@���@�^5@�@���@�X@��@��`@��9@��D@�I�@���@��F@�\)@�33@���@���@��\@�ff@�E�@��T@��h@�p�@�X@��@���@���@�z�@�1'@��@��m@���@���@��P@�t�@�l�@�;d@�@��@���@��\@�M�@��@��-@�?}@��@���@�Ĝ@��@�1'@���@�;d@��@��\@��@�@��T@��^@��7@�x�@�p�@�O�@�&�@��@��j@�j@�A�@� �@�1@��@~$�@}�@}O�@}�@|��@|�@{��@{"�@z^5@z=q@y��@y�^@yhs@x�u@w�;@w�w@wl�@w�@v�R@vff@u�T@u�@u/@u�@t��@t��@tZ@s�F@sS�@r�H@r�\@rn�@r-@q%@p�9@p�@pQ�@o�w@n�y@n�+@nE�@n@m�T@m��@m��@mV@l��@lI�@l(�@l(�@k�m@k��@kdZ@kC�@j~�@i��@ihs@i�@h��@h��@h��@h�`@hr�@f�y@f$�@e�T@e@e�-@e�h@ep�@e?}@d�D@c�m@c33@b�H@b��@bn�@b^5@b-@b�@b�@a�@a�7@aG�@`��@`r�@_�@_\)@_+@_
=@^��@^��@^��@^�+@^V@]�@]?}@\�D@\Z@[�
@[�@[C�@[o@[@Z�@Z�H@Z~�@ZJ@YG�@X�@Xr�@XQ�@W�;@W|�@W�@V�@V�+@Vv�@Vff@U��@T�@Tz�@TI�@T(�@S�
@S��@S��@S��@S�@S�@St�@SdZ@S33@R��@RM�@R�@Q��@Qx�@P��@Pr�@P �@O��@Ol�@OK�@O+@N��@Nȴ@Nv�@NE�@N$�@N{@M�T@M�-@M�@M`B@M?}@L��@L�j@L�j@L��@Lz�@L�@Kƨ@K�@K33@J~�@I�@Ihs@I�@HĜ@G�@G��@G�w@Gl�@F��@F��@Fff@FE�@F{@E�T@E�-@E?}@D��@D9X@C��@B�@B^5@B-@A��@A�#@Ax�@AG�@@�`@@�@@bN@?�;@?|�@?;d@>��@>v�@>$�@=�@=��@=�-@=`B@=?}@=?}@<��@;ƨ@;dZ@;C�@;o@:�H@:��@:�\@:^5@:J@9�@9��@9G�@8��@8Q�@8Q�@8Q�@8  @7K�@6v�@65?@6$�@6@5�T@5��@5�@4�/@4��@4�j@49X@3dZ@2��@2=q@1�@17L@0�@0  @/l�@/
=@.5?@-@-V@,j@,(�@+�
@+@*�!@*�\@*^5@)�@)X@(�9@(bN@( �@'�w@'K�@'
=@&�@&�R@&��@&�+@&V@&{@%�T@%�-@%�h@%�@%�@%p�@%?}@$�/@$z�@$Z@$Z@$9X@$(�@$�@#ƨ@#�@#C�@#o@#@"�@"�H@"��@"��@"M�@"J@!�@!��@!�^@!��@!x�@!hs@!G�@!�@ �`@ Ĝ@ �9@ �u@ �u@ ��@ �@ A�@  �@   @�;@�w@�@��@�P@l�@�@��@ff@E�@E�@5?@5?@$�@{@@�T@�-@`B@�@�@�@��@��@9X@��@�F@��@t�@o@�H@^5@��@x�@%@��@�9@�9@��@�u@�@r�@A�@�@�@l�@K�@;d@�@��@��@ff@��@�h@p�@?}@�@V@�@Z@I�@9X@1@��@"�@�@�H@��@��@�!@n�@M�@=q@x�@��@��@��@b@��@��@�P@|�@|�AмjAмjAмjAк^AмjAмjAиRAк^AиRAиRAмjAоwAмjAоwAоwAмjAк^Aк^A���A���A�ĜA�AоwAоwAоwA���A�A���A���A���A�A�A���A�A���A��
A��A��
A��#A��/A��TA�JA�33A�ffA�v�AхAѝ�Aѧ�AѮAѧ�Aћ�Aщ7A�~�A�r�A�l�A�ffA�bNA�ffA�r�A�|�A�v�A�l�A�`BA�XA�M�A�E�A�?}A�A�A�C�A�E�A�E�A�A�A�=qA�9XA�7LA�33A�+A�"�A��A� �A�"�A�"�A��A��A��A��A��A��A��A��A��A��A� �A�$�A�&�A�&�A�"�A�"�A�$�A�(�A�(�A�(�A�&�A�$�A�&�A�(�A�(�A�(�A�&�A�$�A�"�A�$�A�&�A�&�A�$�A� �A��A� �A�"�A� �A� �A��A�{A�1A�%A�VA�{A�oA�oA�oA�oA�{A��A��A�{A�{A�{A��A��A��A�{A�oA�oA�{A�{A�{A�oA�bA�bA�bA�oA�oA�bA�bA�VA�JA�
=A�%A��yA��HA���AоwAа!A�p�A϶FAμjA�t�A�VA�VAɬA��A�
=A�+AƍPA�E�A���A�$�AľwA�I�A���A�dZA���A���A�
=A�S�A��TA�33A�ĜA�^5A�7LA���A�XA��!A��^A��TA��+A�ZA�K�A�?}A�1'A��A�JA���A��`A��#A���A�A��FA���A��7A�`BA��A��-A�A���A��A��hA�`BA�33A���A���A��!A��uA�z�A�G�A�(�A�  A��
A���A�;dA���A���A���A���A��\A�n�A�\)A�VA�VA�XA�bNA�l�A�r�A�hsA�dZA�x�A��A��A�p�A�7LA�  A��9A�7LA��/A�S�A��9A�O�A���A���A�ƨA��RA���A��hA��A�z�A�l�A�S�A�C�A�&�A���A��RA��DA�l�A�S�A�C�A�7LA�&�A��A���A���A���A�~�A�jA�S�A�;dA�/A��A��A�33A�x�A��A���A���A���A��A��A���A���A�A��A�1'A�I�A�33A�&�A�/A�=qA�9XA��A�A�A�1A�A��A�XA�G�A�E�A�G�A�l�A��DA��hA���A��7A�r�A�^5A�K�A�
=A��/A��
A��
A��A��HA���A�hsA�p�A�t�A�ffA�\)A�XA�M�A�O�A�K�A�E�A�1'A��A���A�~�A�$�A��uA� �A�~�A�;dA�A���A��HA��jA�x�A��HA�ĜA��jA��\A�~�A�t�A�^5A�/A�A��;A��uA�"�A���A�ZA��A��PA�ffA�;dA�/A�"�A��A�bA���A���A�M�A��A��yA�ƨA���A�\)A��`A�p�A��A��RA��A���A��A�ffA�\)A�Q�A�A�A�/A��A�A��A���A���A�\)A�5?A� �A�oA�  A��mA�A��-A���A��\A�~�A�Q�A�5?A���A��`A���A�ȴA��-A���A�|�A�dZA�I�A�33A��A�oA�A���A��mA���A���A���A��uA�v�A�Q�A�VA�ĜA���A��\A��PA�z�A���A���A�ĜA��RA��A���A���A�n�A�n�A�"�A��A���A�&�A���A��A�=qA��`A��^A��\A��DA��A�Q�A���A���A�hsA�E�A�VA�ƨA���A�ffA�$�A��/A�(�A���A�O�A��A�A��A�ȴA���A��uA�z�A�^5A�?}A� �A���A��RA��\A�1'A��A��^A�dZA�%A���A�Q�A�A�A�r�A�(�A���A��A�bA�~�A���A���A�$�A��A�C�A��A���A�7LA��A��mA�ffA�
=A���A�7LA��A��9A�S�A��+A��A�%A��A��FA�v�A�A�A��A��mA���A���A�~�A�Q�A� �A�JA�-A~�A}t�A{;dAz�`AzȴAz�!Az�\Az9XAy��Ay��Ay%Ax=qAw�FAw/Av�`Av��Av��Av��Av�AvI�Av  Au��AuhsAu�AuoAu
=AuAt�RAs��Ar�`Aq�Ap1AodZAn9XAm?}Al9XAj�HAjĜAi�;Ai�AidZAi/Ah��Ah�DAg+Af=qAex�Ae�Ad�jAd^5Ad(�Ac��Ab~�Aa%A_�7A^�9A]&�A[��A[?}AZ�AZ�DAZ5?AY�TAYAY`BAYVAX�jAX�AX�uAXn�AX �AXbAXAW�AW�mAW�-AWp�AV�DAV9XAV  AU�AU�AT�ATȴAT�RAT�+ATI�AShsAR��AQ��APn�AO�AO;dAN��ANA�AM�AMAM�FAM�AM��AM��AM��AM��AM��AM��AM��AM�AMx�AM?}AMoAL�RAL5?AK��AJ�AJ��AJ��AJ��AJ��AJ��AK�AK�AK�PAK�AK�AK�AK�AL��ANjANjAO+APz�AQ�AQ�7AQ�7AQ�hAQ��AR  AQ�;AQG�AOG�AO�hAM��AMVAL �AJ�`AJ1AIC�AH�AH�AI��AI��AI��AI�FAI�AI��AI|�AH1AG�AFȴAFbAC/AA�;AA�
AA��AA�;AA�
AA��AA�
AA��AA��AAƨAA��AA��AAƨAAƨAA�^AAx�AAoAA
=A@�A@��A?�PA?�A>VA<��A;t�A:�A:�DA9�A8A7XA733A7+A7&�A7"�A7�A7�A7oA7VA733A7+A7l�A:��A:�`A:��A:��A:�yA<I�A>v�A@v�A@�\A@��A@�jA@�RA@�!A@�A@�A@ȴAA&�AAC�AAAB��AB�/ACAC33AC7LAC"�ABv�AA�;AA��AAhsA@��A@-A?�A>�jA>A�A=�
A=��A=��A=�-A=��A=t�A=dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114414411111111144111111111111111111144111111111111111111111111111111111111111111114411111111111111111111111111111111111                                                                                                                                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�zB	�zB	�zB	�B	��B	�B	�FB	�zB	��B	�LB	�jB	ɆB
fB
#B
'B
33B
5?B
;�B
<jB
>�B
A�B
C�B
D3B
D3B
EB
GEB
L0B
L�B
K�B
IRB
F�B
L0B
v�B
��B
�B
�_B
�B
�VB�B�B;�B��Bm�Bl�B��B�%B(�BK�B��BgmB?�B$�B\B�TB�B�dB�B��B�B�4B{B^5BGEB'�B
��B
�B
�5B
��B
��B
A�B
B
�B	�%B	��B	B	��B	�B	xlB	_�B	E�B	&B	7B	bB	B�"B��B	AB	YB	��B
!�B
!�B
B
�B	��B	�B	��B	�B	��B	��B
�B
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
�B
��B
�[B
��B
�!B
��B
�JB
�+B
��B
��B
�B
��B
��B
��B
�LB
�_B
�CB
�kB
��B
��B
�B
�4B
�'B
�XB
�XB
�FB
�zB
��B
�bB
�-B
�VB
��B
��B
�\B
�bB
�\B
��B
�B
��B
�nB
��B
��B
��B
�B
�B
�$B
�XB
�dB
��B
�XB
��B
��B
��B
�B
��B
�tB
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�hB
�uB
��B
��B
�!B
�~B
�+B
�bB
��B
��B
�B
}�B
xB
uZB
sMB
rGB
m]B
v`B
x�B
u�B
tB
s�B
r�B
poB
o B
n/B
l"B
k�B
h
B
g�B
f�B
dZB
c�B
b�B
c�B
^�B
\�B
[�B
Z�B
_B
`�B
_�B
]dB
]�B
\�B
\�B
Y�B
VmB
T�B
T�B
U2B
S�B
R�B
RTB
PB
N<B
K)B
HB
F?B
F�B
D�B
C�B
A�B
A�B
A B
A�B
CaB
@B
?HB
>�B
<jB
;�B
:^B
9�B
8RB
6�B
6�B
5�B
5tB
5tB
4�B
49B
2�B
/�B
0!B
-�B
)�B
)�B
)_B
(XB
'�B
%B
%FB
$tB
#B
"4B
!bB
 \B
�B
VB
�B
 �B
�B
B
�B
�B
�B
�B
�B
B
�B
IB
B
#:B
"�B
"4B
!bB
"�B
 �B
�B
!B
�B
�B
qB
�B
�B
�B
�B
B
�B
	B
1B
kB
�B
	B
�B
�B
B
�B
CB
IB
�B
B
CB
�B
�B
qB
�B
�B
�B
�B
7B
B
�B
YB
SB
�B
B
B
B
FB
�B
�B
FB
�B
�B
�B
�B
�B
xB
xB
�B
~B
B
JB
B

�B

rB
	�B
	�B
�B
	7B
	7B
	7B
B

�B
�B
�B
�B
�B
�B
�B
4B
 B
hB
 B
bB
�B
bB
4B
�B
�B
hB
�B
�B
�B
�B
uB
uB
�B
uB
�B
B
�B
{B
{B
FB
{B
�B
{B
�B
�B
B
�B
�B
�B
SB
SB
B
SB
SB
�B
�B
B
�B
�B
�B
�B
$B
�B
�B
YB
�B
+B
�B
�B
�B
�B
�B
+B
_B
�B
�B
�B
eB
�B
7B
kB
kB
1B
�B
eB
eB
kB
IB
~B
�B
B
�B
�B
�B
!-B
!�B
"�B
"�B
"hB
"hB
"hB
"�B
#nB
#:B
#�B
$B
%B
%FB
$�B
$�B
%�B
%�B
%�B
%FB
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'RB
&�B
'�B
'�B
'�B
($B
(�B
)*B
)_B
)�B
)_B
)�B
)�B
)�B
*eB
*0B
+6B
+B
*�B
+�B
+kB
,B
+�B
+�B
,B
,=B
,B
,�B
-B
.B
-wB
-�B
-�B
-�B
.}B
.�B
.�B
.�B
.�B
/B
/B
.�B
/OB
/�B
/�B
0!B
0�B
0UB
0!B
0�B
0�B
0�B
0�B
2-B
2-B
2-B
2aB
2�B
2�B
2�B
3�B
33B
33B
2�B
4nB
49B
3�B
3�B
3�B
3�B
4nB
4nB
4�B
5B
5?B
5B
5�B
5�B
6�B
6�B
7LB
7�B
8B
8RB
8RB
8�B
9$B
9XB
9$B
9$B
9�B
:*B
:^B
:^B
:�B
:�B
;0B
:�B
;dB
;�B
;�B
;�B
<6B
<jB
<jB
<�B
<�B
=qB
=�B
>BB
?B
>�B
>�B
?B
?}B
?HB
@B
?�B
?�B
@OB
@�B
@�B
@�B
A B
AUB
AUB
A B
A�B
A�B
A�B
B[B
B�B
B�B
B�B
B[B
B�B
D�B
D�B
DgB
EB
EB
EB
FB
FtB
F�B
FtB
F�B
F�B
GB
HKB
HKB
H�B
H�B
H�B
H�B
H�B
H�B
IB
H�B
H�B
H�B
H�B
IB
I�B
I�B
J#B
I�B
I�B
J#B
K�B
K)B
K)B
K)B
K�B
L0B
L0B
LdB
LdB
L�B
L0B
L�B
M6B
MjB
M�B
MjB
MjB
M�B
M�B
M�B
MjB
OB
N�B
OB
O�B
OBB
OB
OB
N�B
OBB
QNB
QB
QB
P�B
P�B
QB
P�B
P�B
R B
RTB
R�B
R�B
S&B
S&B
S&B
S[B
S&B
S&B
S[B
S�B
S�B
S�B
T,B
T�B
U2B
T�B
T�B
T�B
U2B
T�B
T�B
T�B
U2B
U�B
VB
V9B
V�B
V�B
W?B
W
B
V�B
V�B
V�B
W?B
WsB
YB
X�B
XyB
X�B
YB
YKB
YB
Y�B
Y�B
YB
YB
ZB
[WB
[WB
[#B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
^B
]�B
^B
]�B
^5B
^�B
^jB
^�B
^�B
^�B
_B
_;B
_;B
_;B
_�B
_�B
_�B
_�B
_�B
`B
`B
`B
`vB
`�B
a|B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
c B
c B
c B
cTB
cTB
cTB
c�B
c�B
d&B
d�B
d�B
e,B
e`B
e`B
e`B
e�B
e`B
e�B
f2B
f2B
f�B
f�B
g8B
g8B
gmB
g�B
g�B
g�B
g�B
h
B
h
B
g�B
hsB
iyB
iyB
iDB
iyB
i�B
i�B
i�B
jB
jB
jB
jB
jB
kQB
kQB
kB
kB
kQB
l"B
l�B
l�B
l�B
l�B
l�B
l�B
m]B
m]B
m)B
m)B
m�B
m�B
ncB
n�B
n�B
oiB
o�B
o�B
pB
poB
qB
qB
rB
rGB
rGB
r|B
s�B
tB
tB
tB
tTB
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v+B
v+B
v+B
v�B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
w�B
w�B
w�B
w�B
xB
xB
xB
xlB
x�B
y	B
y	B
x�B
y	B
y	B
y	B
y	B
y�B
y�B
y�B
y�B
zB
zB
zDB
zB
zxB
zxB
z�B
z�B
z�B
{B
z�B
z�B
{JB
{JB
{JB
{JB
{B
{�B
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}"B
}�B
}�B
~(B
~]B
~]B
~�B
~�B
~�B
.B
.B
cB
cB
�B
�B
�iB
�iB
�oB
��B
��B
�B
�B
�B
�B
�B
�AB
�uB
��B
��B
�GB
�B
�B
�GB
�GB
�GB
��B
��B
��B
��B
��B
��B
�B
�SB
��B
��B
��B
��B
��B
��B
��B
�+B
��B
��B
�+B
�_B
�+B
�+B
��B
�fB
��B
��B
��B
��B
�	B
�	B
�=B
�7B	�FB	�B	�B	�LB	��B	�FB	��B	��B	��B	��B	�B	�LB	��B	�B	��B	�?B	��B	�B	��B	�zB	�?B	�B	��B	�RB	�LB	��B	�tB	�FB	�B	��B	�LB	�zB	�B	��B	ĜB	ȴB	�'B	ȴB	ɆB	ȴB	�B	�dB	��B	�B
"B
�B
�B
�B
!�B
#:B
&LB
$�B
#�B
VB
!bB
"hB
%B
'RB
)�B
-�B
2�B
49B
2�B
1�B
2aB
3�B
3�B
2aB
2-B
3�B
6FB
8�B
9�B
:�B
;�B
:�B
;�B
<jB
<�B
;�B
:�B
;0B
<�B
=<B
>�B
>wB
=�B
=qB
=�B
=�B
AUB
@�B
?HB
B[B
A�B
B�B
C�B
D�B
D3B
CaB
B�B
B�B
C�B
EmB
D3B
C�B
C-B
C�B
D�B
EB
EB
D�B
CaB
C�B
C�B
E9B
E�B
D�B
C�B
D3B
E�B
GB
IRB
HKB
E9B
C�B
I�B
K^B
MjB
M6B
L�B
K�B
K^B
K�B
L�B
L�B
MjB
M6B
L0B
K^B
K�B
M6B
MB
J�B
JXB
I�B
I�B
I�B
I�B
HKB
G�B
FtB
F�B
F�B
F�B
GzB
E�B
FtB
R B
OvB
MjB
OB
G�B
JXB
X�B
t�B
~�B
��B
��B
�{B
�%B
��B
�FB
�B
��B
�MB
�4B
��B
�B
��B
�oB
��B
��B
�B
�.B
�B
�hB
��B
��B
��B
�^B
��B
��B
�2B
�>B�B�B	lB
�B	�B�BBB~B
�BBJBB~BB
rB�BDB�B�B�B�B�BBBBMB�B�B"4B�B�B �B(�B0!B/OB8�BB�BFBR BXBRTBQ�B[�BZQBiB~(B�B�hB�RB��B�B�B�)B�B�3B�BB�aB�B��B�oBn�BbNB[WBZQB\]B]/B`�B_B_B_pBd�Bb�Bg�Bh�Bm]Bk�Bh�Bi�BiyBkQBm]Bn�Bt�BwfBwfBuZBr|Bs�BsMBt�Bz�B|B|�B��B�IB��B��B�!B�$B�6B�OB҉B˒B��B�&B�(B�BPB�B7B~B�BqB�B�B#�B6�B&�B �BVB�B!BH�BCaBWsBYBS�BN�BR�BR�BP�BD3BC�BC�BA�B>wBsMB��B��B�B�=B��B�_B�MB��B�B�uB�B~�By>Bu�B��Bs�BsB]dB`BBUgBT�BT�BW�BR�B?�B@�B@B>B>wB>wBA�B9XB8RB@�B?}B5�B/�B*0B�B7BB�B�B�B�B�B&�B�BB	lB�BGBB��B�;B��B��BܒB�/B��B��B��B�QB�BٴB��B֡B�mB�
B�yB��B� B�B�jB͟BΥB�6BɆBƨBŢB�B�B�BÖB��B�$B�B�RB�zB�?B��B��B��B�OB�B�6B�0B�B�eB�XB�LB��B��B�hB��B��B�.B�(B��B�oB��B�_B��B�uB��B~]B}�B�Bu�Bs�B|�Bu�Bp;Bh�B`�B^5B`�BU�BRTBL�BOBBOBBV9BMjBC-B<�BA�B9�B/�B1�B)�B,�B-�BBPB
	B�B �B{B;B
��B
�xB
�ZB
��B
�5B
�B
�B
�cB
�|B
�B
��B
�B
�>B
��B
�dB
�2B
�B
ѷB
�9B
�mB
��B
��B
�^B
�nB
�B
��B
�OB
��B
��B
u�B
gmB
n�B
`BB
P�B
E�B
D�B
?B
/�B
.}B
5�B
<�B
#B
MB
�B
B
!�B

�B
�B

	B
SB
�B	�(B
B	�B	��B	�B	�xB
MB	��B	�B	��B	�#B	��B	ߤB	ݘB	�sB	�B	�?B	�vB	��B	��B	��B	�BB	��B	��B	��B	�qB	�0B	��B	��B	�'B	��B	�=B	��B	��B	�9B	�RB	��B	��B	�eB	��B	�7B	v`B	u%B	~�B	�GB	tB	i�B	o�B	uZB	��B	j�B	ncB	W
B	W�B	Q�B	RTB	OvB	`B	^�B	FB	?B	T�B	AUB	.B	.B	.}B	,�B	*�B	$�B	)�B	&�B	#�B	IB	B	CB	 �B		B	�B	$B	MB	�B	qB	<B	oB	oB	B	�B	�B	%B	�B	�B	+B	4B	@B	�B	�B	�B	_B	�B��B	�B��B�xB��B�DB��B��B�xB�	B�	B��B�>B�	B�xB�xB�.B	 �B	�B	�B�DB	�B	1B	�B		B	B	~B�GB	(XB	*eB	+kB	,�B	1B	�SB	��B	v�B	�!B	�B
B
�B
~B
B
hB
&LB
2�B
]�B
0�B
XyB
&B
0!B
*�B
�B
�B
GB
�B	��B
&�B
&LB
%�B
!bB
 \B
"�B
>B
�B
{B
#nB
S&B	�B	��B	�B	�`B	�|B	�|B	�B	��B	�MB	�B	��B	�B	��B	�B	�B	�2B	��B	�B	�B	�B	�%B	�NB	��B	�QB	ϫB	ȀB	�dB	�tB	ԕB	�6B	�4B	��B	�hB	�B	�nB	�nB	�LB	��B	�RB	��B	��B	�vB	��B
+B
B
�B	�B
�B
w2B
�B
��B
�B
��B
��B
�SB
��B
�B
~�B
�B
��B
y>B
�OB
�B
��B
��B
��B
�dB
�B
�B
��B
��B
��B
�zB
��B
�B
�hB
�IB
��B
�qB
�OB
�!B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114414411111111144111111111111111111144111111111111111111111111111111111111111111114411111111111111111111111111111111111                                                                                                                                                                                                                                  B	�,B	�,B	�,B	��B	��B	��B	��B	�,B	��B	��B	�B	�8B
B
�B
 �B
,�B
.�B
5KB
6B
8]B
;pB
=|B
=�B
=�B
>�B
@�B
E�B
FJB
EyB
CB
@�B
E�B
p{B
�6B
��B
�B
��B
�B^B�B5KB�3BgCBfrB��B��B"sBEyB:BaB9�B�B	B�B��B�B��B��B��B��Bu1BW�B@�B!9B
�=B
�B
��B
�KB
�FB
;�B
�B	�hB	��B	�rB	�AB	��B	��B	rB	Y�B	?�B	�B	�B	
B	�B��B�YB��B	B	�UB
HB
�B
�B
pB	�B	�B	ݣB	��B	�3B	�<B
	�B
|\B
~hB
��B
�8B
��B
�^B
�6B
��B
��B
�^B
��B
�QB
�B
�aB
��B
��B
��B
��B
~�B
��B
��B
�9B
�mB
�EB
��B
�B
��B
�B
�aB
�[B
��B
��B
��B
�
B
�
B
��B
�,B
�[B
�B
��B
�B
�HB
�NB
�B
�B
�B
�6B
��B
�dB
� B
�mB
�sB
�mB
��B
��B
��B
�
B
�B
��B
�
B
�mB
�2B
��B
��B
��B
�&B
�ZB
��B
�jB
�?B
��B
�NB
�dB
��B
��B
�qB
��B
�6B
�B
�'B
��B
�RB
��B
�0B
��B
�B
��B
}�B
z�B
w=B
q�B
oB
l�B
k�B
gB
pB
r�B
o�B
m�B
m�B
lbB
j!B
h�B
g�B
e�B
elB
a�B
aSB
`�B
^B
]�B
\4B
]�B
XPB
VDB
U>B
T�B
X�B
Z\B
Y�B
WB
W~B
V�B
VDB
S�B
PB
NGB
N�B
N�B
M�B
L�B
LB
I�B
G�B
D�B
A�B
?�B
@ZB
>NB
=|B
;pB
;;B
:�B
;;B
=B
9�B
8�B
8]B
6B
5KB
4B
3�B
2B
0�B
0`B
/�B
/&B
/&B
.�B
-�B
,|B
)5B
)�B
'^B
#EB
#�B
#B
"
B
!�B
�B
�B
&B
�B
�B
B
B
�B
B
�B
BB
�B
�B
�B
^B
�B
�B
�B
�B
�B
�B
�B
�B
NB
�B
B
�B
BB
<B
�B
�B
^B
#B
RB
�B
�B
�B
�B
�B
�B
�B
B
dB
�B
�B
^B
�B
XB
�B
�B
�B
�B
�B
�B
XB
#B
�B
�B
RB
RB
�B
�B
zB
B
B
gB
�B
�B
�B
�B
�B
[B
�B

}B
�B
6B
�B
�B
*B
*B
�B
0B
�B
�B
�B
�B
$B
RB
�B
�B
�B
�B
�B
�B
�B
^B
�B
�B
6B
	CB

}B

�B

�B
B

�B

B
	�B

B

�B
UB
UB
B
�B
�B
�B
�B
'B
'B
[B
'B
[B
�B
3B
-B
-B
�B
-B
�B
-B
�B
3B
�B
gB
�B
�B
B
B
�B
B
B
9B
9B
�B
�B
nB
�B
�B
�B
�B
�B
B
nB
�B
�B
tB
�B
�B
EB
�B
B
EB
EB
zB
B
LB
�B
B
B
�B
�B
B
B
B
�B
0B
�B
�B
6B
6B
6B
�B
}B
�B
NB
B
B
B
NB
 B
�B
�B
�B
�B
�B
�B
�B
�B
aB
aB
�B
aB
 gB
 3B
 gB
 3B
 gB
 gB
!B
 �B
!mB
!9B
!mB
!�B
"sB
"�B
#B
#yB
#B
#yB
#EB
#yB
$B
#�B
$�B
$�B
$�B
%QB
%B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
')B
'^B
'^B
'�B
(/B
(dB
(dB
(�B
(dB
(�B
(�B
(�B
)B
)�B
)5B
)�B
*<B
*B
)�B
*<B
*�B
*�B
*�B
+�B
+�B
+�B
,B
,�B
,|B
,|B
-NB
,�B
,�B
,�B
. B
-�B
-�B
-NB
-�B
-�B
. B
. B
.�B
.�B
.�B
.�B
/�B
/�B
0`B
0�B
0�B
1�B
1�B
2B
2B
28B
2�B
3
B
2�B
2�B
3>B
3�B
4B
4B
4yB
4yB
4�B
4�B
5B
5KB
5B
5B
5�B
6B
6B
6�B
6�B
7#B
7WB
7�B
8�B
8�B
8�B
8�B
9/B
8�B
9�B
9cB
9�B
:B
:jB
:jB
:�B
:�B
;B
;B
:�B
;;B
;;B
;�B
<B
<vB
<AB
<vB
<B
<vB
>NB
>NB
>B
>�B
>�B
>�B
?�B
@&B
@�B
@&B
@�B
@�B
@�B
A�B
A�B
B2B
BfB
BfB
B2B
B�B
B�B
B�B
B�B
B�B
BfB
B�B
B�B
ClB
C8B
C�B
ClB
C8B
C�B
EDB
D�B
D�B
D�B
EyB
E�B
E�B
FB
FB
FJB
E�B
FB
F�B
GB
GQB
GB
GB
GQB
G�B
GQB
GB
H�B
H�B
H�B
I]B
H�B
H�B
H�B
HWB
H�B
K B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
LB
LoB
L�B
L�B
L�B
L�B
MB
L�B
L�B
MB
MAB
MuB
MuB
M�B
N|B
N�B
N�B
N�B
N|B
N�B
N�B
N|B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q%B
R�B
R`B
R+B
R�B
R�B
R�B
S1B
SfB
S�B
S1B
S1B
S�B
U	B
U	B
T�B
U	B
U>B
UrB
UrB
U>B
U>B
U>B
UrB
U>B
UrB
VDB
VDB
VDB
VxB
VxB
V�B
WJB
WJB
WJB
W�B
W~B
W�B
W~B
W�B
XPB
XB
X�B
XPB
XPB
X�B
X�B
X�B
X�B
YVB
YVB
YVB
YVB
YVB
Y�B
Y�B
Y�B
Z(B
Z�B
[.B
[cB
[cB
[�B
\iB
\4B
\4B
\iB
\�B
\�B
\�B
\�B
]B
]B
]B
]oB
]�B
]�B
^AB
^�B
^�B
_B
_B
_B
_GB
_B
_�B
_�B
_�B
`MB
`�B
`�B
`�B
aB
aSB
aSB
aSB
aSB
a�B
a�B
a�B
b%B
c+B
c+B
b�B
c+B
c_B
c�B
c�B
c�B
c�B
c�B
d1B
d1B
eB
eB
d�B
d�B
eB
e�B
f=B
frB
f=B
frB
frB
frB
gB
gB
f�B
f�B
gCB
gxB
hB
hJB
hJB
iB
iPB
i�B
i�B
j!B
j�B
j�B
k�B
k�B
k�B
l.B
mhB
m�B
m�B
m�B
nB
m�B
n�B
n�B
n�B
o@B
ouB
o�B
o�B
o�B
o�B
o�B
pFB
pB
p�B
p{B
p�B
p�B
p�B
p{B
p�B
qLB
q�B
q�B
q�B
q�B
q�B
q�B
rB
rSB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sYB
sYB
s�B
s�B
s�B
s�B
s�B
s�B
t*B
t*B
t_B
t�B
t�B
t�B
t�B
t_B
t�B
t�B
t�B
t�B
u1B
ueB
ueB
ueB
ueB
u�B
vB
vkB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w=B
w�B
w�B
xB
xB
xCB
xwB
x�B
x�B
x�B
yB
yB
y~B
y~B
zB
zB
{!B
{UB
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|'B
|\B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}bB
~3B
~hB
~3B
~�B
~�B
~�B
B
nB
nB
nB
�B
�@B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�LB
�B
�LB
��B
��B
��B
��B
��B
��B
��B	��B	��B	��B	��B	�ZB	��B	�gB	�2B	��B	�`B	��B	��B	�2B	��B	�`B	��B	�`B	��B	��B	�,B	��B	��B	��B	�B	��B	�`B	�&B	��B	��B	�gB	��B	�,B	��B	��B	�NB	�fB	��B	�fB	�8B	�fB	��B	�B	�rB	�B
�B
	wB
�B
�B
}B
�B
�B
[B
�B
B
B
B
�B
!B
#yB
'^B
,|B
-�B
,|B
+vB
,B
-�B
-�B
,B
+�B
-�B
/�B
2mB
3sB
4�B
5KB
4EB
5KB
6B
6�B
5KB
4yB
4�B
6QB
6�B
8]B
8)B
7�B
7#B
7WB
7�B
;B
:�B
8�B
<B
;�B
<AB
=�B
>�B
=�B
=B
<�B
<�B
=�B
?B
=�B
=HB
<�B
=|B
>NB
>�B
>�B
>NB
=B
=HB
=�B
>�B
?TB
>NB
=|B
=�B
?TB
@�B
CB
A�B
>�B
=HB
ClB
EB
GB
F�B
FB
EDB
EB
EyB
FJB
FB
GB
F�B
E�B
EB
E�B
F�B
F�B
DsB
D
B
C8B
ClB
ClB
C�B
A�B
A`B
@&B
@ZB
@�B
@�B
A,B
?�B
@&B
K�B
I(B
GB
H�B
A`B
D
B
R`B
n:B
x�B
�tB
�aB
}-B
�B
�?B
��B
��B
��B
��B
��B
��B
��B
��B
�!B
��B
��B
��B
��B
��B
�B
�]B
��B
�TB
�B
ܝB
�uB
��B
��B
�nB
�hBBXB�B�B�B�B0B�B�B�B�B0B�B$B tB�B	wB�B�B�B�B�B�B�B�BLB?B�B<BpB�B"�B)�B)B2�B<vB?�BK�BQ�BLBKiBU>BTBb�Bw�B��B�B�B�KB��B��B��B��B��B��B�B��B�RB{!Bh~B\ BU	BTBVBV�BZ�BX�BX�BY"B^AB\�BaSBbYBgBe�Bb�Bc�Bc+BeBgBhJBnnBqBqBoBl.BmhBl�Bn�Bt_Bu�BvkB�9B��B��B��B��B��B��B�B�;B�DBًB��B��B�BB�B�B0B�B#B�B�BUB0�B 3BBBB<B�BB�B=BQ%BS1BM�BHWBLoBL�BJ�B=�B=�B=HB;�B8)Bl�B�LB��B��B��B��B�B}�B}�B}�B|'B��BxCBr�Bo@B�gBm4Bl�BWBY�BOBN�BNGBQZBL;B9�B:jB9�B7�B8)B8)B;;B3
B2B:5B9/B/ZB)jB#�BpB�B�B9BnBnB9B�B �B�B�BB�3B��B��B�_B��B�uB؅B�DB��BًBէBԠB�B��B�fBҔB�SB�BмB�+BҔB��B��B�B�QB�WB��B�8B�ZB�TB��B��B��B�HB��B��B��B�B�,B��B��B�|B�5B�B��B��B��B��B�B�
B��B��B��B�B��B��B��B��B�qB�!B�pB�B~3B|'B{�BxBwqBy~Bo�Bm4Bv�Bo@Bi�BbYBZ�BW�BZ\BO�BLBFBH�BH�BO�BGB<�B6�B;pB3sB)�B+�B#EB&�B'^B�BB�B
�\B
��B
�-B
��B
�@B
�*B
�B
�B
��B
�JB
��B
�B
�.B
�1B
�B
��B
��B
؅B
�B
��B
��B
�iB
��B
�B
��B
��B
�B
� B
��B
�9B
�B
�IB
~hB
o@B
aB
hJB
Y�B
JcB
?�B
>�B
8�B
)�B
(/B
/�B
6QB
�B
�B
jB
�B
HB
�B
eB
�B	�B	��B	��B	��B	��B	�@B	��B	�*B
�B	�B	��B	�~B	��B	�xB	�VB	�JB	�%B	�iB	��B	�(B	ħB	��B	�vB	��B	��B	�WB	�]B	�#B	��B	�2B	�|B	��B	�dB	��B	�<B	��B	��B	�B	��B	��B	�B	�IB	��B	pB	n�B	xwB	|�B	m�B	c�B	i�B	oB	~�B	deB	hB	P�B	QZB	K�B	LB	I(B	Y�B	XPB	?�B	8�B	NGB	;B	'�B	'�B	(/B	&WB	$�B	�B	#yB	 gB	UB	�B	�B	�B	wB	�B	?B	�B	�B	�B	#B	5�B	!B	!B	�B	�B	^B��B	 tB	�B	 �B	
�B	�B	�B	�B��B	B	 tB�=B�UB�B�*B�YB��B�_B��B�*B�B�B�SB��B�B�*B�*B��B��B�3B	�B��B	FB	�B	LB	�B��B	0B��B	"
B	$B	%B	&WB	�B	�B	��B	pFB	��B	��B
�B
�B
0B	��B
B
�B
,HB
WJB
*pB
R+B
�B
)�B
$�B
EB
nB	��B	��B	�rB
 gB
�B
aB
B
B
NB
7�B
�B
-B
 B
L�B	��B	�{B	�:B	�B	�.B	�.B	��B	�B	��B	�bB	�B	�B	�B	�CB	�B	��B	�{B	�AB	�iB	�lB	��B	� B	߰B	�B	�]B	�2B	�B	�&B	�GB	��B	��B	��B	�B	��B	� B	� B	��B	�sB	�B	��B	��B	�(B	��B
�B
�B
^B	ƳB
<B
p�B
z�B
z�B
{�B
�zB
~hB
B
:B
{�B
xCB
��B
�kB
r�B
�B
��B
�BB
��B
��B
�B
��B
��B
��B
�B
�TB
�,B
�>B
��B
�B
��B
�EB
�#B
�B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114414411111111144111111111111111111144111111111111111111111111111111111111111111114411111111111111111111111111111111111                                                                                                                                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223245                            20230426223245AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622324520230426223245  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622324520230426223245QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622324520230426223245QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               