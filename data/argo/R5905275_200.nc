CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-01T04:00:54Z creation      
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
resolution        =���   axis      Z        x  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [X   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  c8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɀ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x 0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230701040054  20230701040054  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�4��5~@�4��5~11  @�4�'ҁ�@�4�'ҁ�@-���$�@-���$��c��G���c��G��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�  ?��H@=p�@}p�@��R@�  @�  A   A��A!G�A,��A?\)A`  A�Q�A�Q�A�Q�A�  A��A�  A�Q�A�  A��B�
B�B�
B   B((�B0  B7�
B?�
BH  BP  BX  B`  Bg�
Bo�
Bw�
B�  B�{B�  B�  B�{B�{B�  B�  B�{B��B��B�  B��B�B��B�  B�  B�  B�{B�(�B�{B��B�{B�{B�  B��B�  B�  B�  B�  B�  B�  C 
=C
=C  C  C  C	�C��C��C
=C  C��C  C  C
=C{C
=C   C"  C#��C&  C(  C*
=C,  C-��C0  C1��C4  C6  C8  C:
=C<
=C>
=C@  CB  CD
=CF{CH
=CJ  CL
=CN{CP  CQ��CS��CV  CW��CY�C[��C^
=C`  Ca��Cc�Cf  Ch{Cj
=Cl
=Cn
=Cp
=Cr  Cs��Cu��Cx  Cz  C{��C}��C��C���C�C�C�C�  C���C�C�  C�  C�  C�  C���C���C���C���C�C�C���C�  C�
=C�C���C���C�  C�C�C�  C�  C���C�  C�  C�C�C�C�C�  C�  C�  C�C�C�  C���C���C�  C�C�C�C�
=C�  C���C�  C�\C�\C�
=C�
=C�C�C�C�
=C�  C���C���C�  C�
=C�
=C�
=C�  C���C�  C�  C���C���C���C�  C�  C�  C���C���C���C�  C�  C�C�C�
=C�  C���C�  C�  C�  C�C�C�C�
=C�C�  C�  C�  C�C���C���C���C���C���C���C���C���C���C���C���C���C���C�  C�C�C�C�C�  C���C�  C�C���C���C�  C�C�  C���C�D D �D  D� D�qD��D�qD}qD�D� D  D}qD  D� D�D��D  Dz�D�qD	��D
  D
��DD� D�D�D  D}qD  D��D  Dz�D  D��D  D}qD�qDz�D  D��D  D}qD�qDz�D��D}qD�qD}qD�qD}qD�qD� D  D� D�D}qD  D��D�D�D�D� D�qD}qD �D ��D!  D!}qD"  D"� D#  D#}qD$�D$� D%  D%� D&  D&� D'  D'� D'�qD(� D(�qD)}qD*  D*}qD+  D+� D+�qD,z�D,�qD-� D-�qD.}qD.�qD/� D0  D0� D0�qD1� D2�D2��D3  D3}qD4  D4� D4�qD5� D6�D6� D6�qD7��D8�D8� D9�D9�D:D:��D;�D;� D<  D<� D=  D=� D=�qD>� D?  D?}qD?�qD@}qD@�qDA� DB  DB� DC�DC�DD�DD� DE  DEz�DE�qDF� DG  DG}qDG�qDH}qDI  DI�DI�qDJz�DJ�qDK��DL�DL� DL��DM� DN�DN� DN��DO� DP�DP��DQDQ� DR  DR� DR��DS}qDT  DT��DU  DU}qDU�qDV}qDW  DW� DW�qDX� DY�DY� DY��DZ}qD[  D[� D\  D\� D]  D]� D]�qD^}qD^�qD_� D_�qD`}qD`�qDa}qDb�Db��Dc  Dc� Dd  Dd� Dd�qDe}qDf  Df� Dg  Dg� Dh  Dh}qDh�qDi� DjDj�Dj�qDk}qDl  Dl� Dm  Dm}qDn  Dn� Do�Do��Dp  Dp� Dq  Dq� Dr  Dr� Ds�Ds� Ds�qDt}qDu  Du��Dv  Dvz�Dv�qDw� Dx  Dx� Dy  Dy}qDz  Dz��D{�D{� D|  D|� D|�qD}��D~D~��D�D� D�  D�@ D�� D���D���D�=qD�~�D�� D�  D�@ D�~�D�� D�  D�>�D�}qD���D�  D�@ D�~�D���D�HD�AHD�~�D���D���D�>�D��HD�D�  D�>�D�� D�� D���D�@ D��HD�� D���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�>�D�~�D�� D�HD�B�D��HD�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D���D�@ D�� D��qD���D�@ D�~�D���D�  D�>�D�~�D��qD��qD�=qD�� D��HD�  D�@ D�~�D���D���D�@ D�� D�� D���D�>�D��HD�� D�  D�AHD��HD�� D���D�>�D�� D��HD�  D�>�D�~�D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�D�  D�@ D��HD�� D���D�@ D��HD��HD�HD�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD��HD��HD�HD�AHD���D�� D���D�@ D�� D��HD�  D�>�D�� D���D��qD�>�D��HD�D�  D�>�D�� D��HD�HD�AHD�� D���D���D�>�D��HD��HD��qD�>�D��HD�D���D�@ D��HD���D���D�AHD���D�� D��qD�>�D�� D�� D�HD�>�D�� D��HD�  D�=qD�~�D�� D���D�=qD�~�D��HD�  D�AHD���D�� D���D�=qD�~�D�� D�HD�@ D�~�D�� D�  D�>�D�}qD��qD��)D�@ D��HD��HD�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�>�D�� D��HD�  D�>�D�}qD��qD��qD�=qD�}qD��qD��qD�AHD���D�� D�  D�B�D��HD���D�  D�B�D���D�D��D�@ D�~�D���D���D�>�D�~�D���D�  D�@ D��HD�� D�  D�AHD�~�D��HD�HD�@ DHD�� D�  D�>�DÀ D�� D��qD�>�DĀ Dľ�D�  D�@ DŁHD�� D�  D�@ Dƀ Dƾ�D���D�@ Dǀ D��HD�  D�>�DȀ D�� D���D�@ Dɀ Dɾ�D�  D�AHDʀ D��HD�  D�@ DˁHD˾�D���D�@ D̀ D̾�D���D�@ D́HD�� D���D�>�D�~�D�� D�  D�=qD�~�D��HD�HD�@ DЁHD��HD���D�>�D�~�DѾ�D�  D�@ DҁHD�� D���D�AHDӀ D�� D�  D�@ DԁHD��HD�  D�@ D�~�DսqD���D�@ Dր D�D�  D�AHDׁHD�� D���D�>�D؀ D��HD��D�AHDـ D��HD�HD�AHDڀ D�� D��D�AHDۀ D��HD��D�@ D�~�D��HD�HD�B�D݁HDݾ�D�  D�>�Dހ D�� D�HD�B�D߁HD��HD�HD�@ D�� DྸD���D�>�D�~�DᾸD��qD�=qD� D��HD�HD�AHD� D㾸D���D�>�D� D�� D�  D�@ D� D�� D�  D�AHD�HD��HD�  D�=qD�~�D�� D�  D�AHD� D�� D��D�AHD�HD龸D�  D�B�D� D꾸D�  D�AHD�HD뾸D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�HD��HD�HD�B�D�HD�� D�  D�>�D�� D�� D���D�>�D�HD��HD�HD�@ D� D�D�  D�@ D�~�D�D�HD�>�D�~�D���D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D���D���D�AHD�� D�� D�HD�@ D�~�D���D�HD�<)>���?#�
?aG�?�\)?�{?�ff@�\@z�@(��@=p�@Tz�@c�
@u@��@��@��H@��
@���@�Q�@��
@���@�z�@�p�@���@�z�@��RA33A��A�RA�
A�A(�A!G�A&ffA*�HA/\)A3�
A8Q�A>{AC�
AH��AMp�AR�\AXQ�A]p�Ab�\Ag
=Al(�Aq�Aw�A|��A���A��A�ffA�G�A�33A�{A���A��
A�ffA���A��A�ffA�G�A��
A�{A���A�(�A��RA���A��
A��RA���A�z�A�
=A�G�A�z�A�\)A��A�z�A�
=A��A��A׮A��A���A߮A�\A��A�\)A�=qA��A�  A��HA�p�A�  A��HA�{B Q�B��B�HBQ�BB33Bz�B	B33B��B{B�Bz�B{B�B��B=qB�B��B=qB�B�B=qB�B ��B"ffB#�
B%�B&ffB'�B)�B*�\B,  B-G�B.�\B/�
B1p�B2�RB4  B5G�B6�RB8Q�B9��B:�HB<  B=p�B>�RB@Q�BABC
=BD(�BEp�BG
=BHz�BIBK
=BL(�BM��BN�HBPQ�BQBR�HBT(�BUp�BV�HBX(�BYp�BZ�\B[�
B]�B^�\B_�
Ba�Bb=qBc�Bd��Bf=qBg�Bh��Bj{Bk\)Bl��Bn{Bo�Bp��Br{Bs\)Bt��Bv{Bw�Bx��BzffB{�B|��B~=qB�B�z�B�G�B��B���B�G�B��
B�z�B�33B��B���B�G�B��
B�z�B�
=B�B�ffB��B�B�ffB�
=B���B�=qB��HB��B�=qB��HB���B�=qB���B�\)B�  B���B�\)B�  B��RB�G�B��B�z�B�
=B�B�ffB��B�B�ffB���B���B�(�B���B�p�B�(�B��HB���B�Q�B���B��B�Q�B���B���B�Q�B���B�B�z�B�33B��B��\B�33B�B�z�B�33B��
B��\B�G�B�  B��RB�\)B�  B��RB�p�B�{B���B��B�=qB��HB���B�Q�B��B��
B��\B�G�B�  B��RB�p�B�=qB���B��B�ffB��B��B£�B�\)B�{B���B�p�B�(�B��HBǮB�ffB��B��BʸRB�p�B�=qB���BͮB�z�B�33B��BУ�B�\)B�{B���BӅB�=qB���BծB�Q�B�
=Bי�B�(�BظRB�33BٮB�(�Bڏ\B���B�G�BۅB��B�(�B�ffBܸRB�
=B�\)BݮB�  B�Q�Bޣ�B�
=B�\)B�B�  B�ffB�RB�
=B�G�B�B�{B�ffB�RB��B�p�B�B�{B�ffB���B��B�B��
B�(�B�z�B��HB�G�B癚B�  B�Q�B�RB��B�p�B��
B�(�B�\B��HB�G�B뙚B�  B�Q�B�RB�
=B�p�B�B�(�B�z�B���B�33B�B��B�Q�B��B�
=B�p�B��B�Q�B�RB��B�B�  B�ffB���B�33B��B�{B�z�B��HB�G�B�B�(�B��\B�
=B�\)B��B�Q�B��RB�33B���B�{B�z�B��HB�\)B�B�(�B���B�
=B�p�B��C (�C \)C �\C C ��C33Cp�C��C�
C
=CG�Cp�C�C�
C{C=qCp�C��C�
C
=C=qCp�C��C��C
=C=qCp�C�C�HC�CG�C�C�RC��C(�C\)C��C��C  C=qCp�C��C�
C	{C	G�C	�C	C	��C
(�C
\)C
��C
��C  C=qCp�C��C�
C{CG�Cp�C�C�HC�CQ�C�\C��C  C=qCz�C�C�C33Cp�C�C��C(�CffC��C�C(�CffC��C�HC�C\)C��C��C
=CQ�C�\C��C{CQ�C�\C�
C{CQ�C�\C��C  C=qCp�C�C�C(�CffC�C�C(�CffC��C�HC{CQ�C�\C��C  C=qCz�CC  CG�C�\C��C
=C=qCz�C�RC��C33Cp�C�RC��C33Cz�C�RC��C33Cz�C�RC�C 33C p�C �C �C!=qC!z�C!�C!�C"(�C"ffC"��C"�C#(�C#p�C#�C#�C$(�C$\)C$��C$�
C%{C%Q�C%��C%�
C&�C&\)C&��C&�
C'{C'Q�C'�C'��C(
=C(Q�C(��C(�
C)�C)\)C)��C)��C*{C*Q�C*�\C*�HC+(�C+ffC+�C+�HC,�C,\)C,��C,�HC-(�C-ffC-�C-�C.(�C.p�C.��C.�HC/�C/\)C/��C/�C033C0z�C0�RC0��C133C1p�C1�C1��C2=qC2�C2C3
=C3Q�C3�\C3�
C4{C4\)C4�\C4��C5
=C5Q�C5��C5�HC6�C6Q�C6��C6��C7
=C7G�C7��C7�
C8{C8Q�C8�\C8��C9�C9ffC9��C9�C:�C:\)C:�C:��C;=qC;z�C;�RC<  C<G�C<��C<�HC=�C=ffC=�C>  C>G�C>�C>C?
=C?\)C?�C?��C@33C@z�C@CA{CAffCA�CA�CB33CB�CB�
CC�CCffCC�CC��CDG�CD�\CD�HCE(�CEp�CE�CF  CFG�CF��CF�HCG(�CGffCG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                               ?�  ?��H@=p�@}p�@��R@�  @�  A   A��A!G�A,��A?\)A`  A�Q�A�Q�A�Q�A�  A��A�  A�Q�A�  A��B�
B�B�
B   B((�B0  B7�
B?�
BH  BP  BX  B`  Bg�
Bo�
Bw�
B�  B�{B�  B�  B�{B�{B�  B�  B�{B��B��B�  B��B�B��B�  B�  B�  B�{B�(�B�{B��B�{B�{B�  B��B�  B�  B�  B�  B�  B�  C 
=C
=C  C  C  C	�C��C��C
=C  C��C  C  C
=C{C
=C   C"  C#��C&  C(  C*
=C,  C-��C0  C1��C4  C6  C8  C:
=C<
=C>
=C@  CB  CD
=CF{CH
=CJ  CL
=CN{CP  CQ��CS��CV  CW��CY�C[��C^
=C`  Ca��Cc�Cf  Ch{Cj
=Cl
=Cn
=Cp
=Cr  Cs��Cu��Cx  Cz  C{��C}��C��C���C�C�C�C�  C���C�C�  C�  C�  C�  C���C���C���C���C�C�C���C�  C�
=C�C���C���C�  C�C�C�  C�  C���C�  C�  C�C�C�C�C�  C�  C�  C�C�C�  C���C���C�  C�C�C�C�
=C�  C���C�  C�\C�\C�
=C�
=C�C�C�C�
=C�  C���C���C�  C�
=C�
=C�
=C�  C���C�  C�  C���C���C���C�  C�  C�  C���C���C���C�  C�  C�C�C�
=C�  C���C�  C�  C�  C�C�C�C�
=C�C�  C�  C�  C�C���C���C���C���C���C���C���C���C���C���C���C���C���C�  C�C�C�C�C�  C���C�  C�C���C���C�  C�C�  C���C�D D �D  D� D�qD��D�qD}qD�D� D  D}qD  D� D�D��D  Dz�D�qD	��D
  D
��DD� D�D�D  D}qD  D��D  Dz�D  D��D  D}qD�qDz�D  D��D  D}qD�qDz�D��D}qD�qD}qD�qD}qD�qD� D  D� D�D}qD  D��D�D�D�D� D�qD}qD �D ��D!  D!}qD"  D"� D#  D#}qD$�D$� D%  D%� D&  D&� D'  D'� D'�qD(� D(�qD)}qD*  D*}qD+  D+� D+�qD,z�D,�qD-� D-�qD.}qD.�qD/� D0  D0� D0�qD1� D2�D2��D3  D3}qD4  D4� D4�qD5� D6�D6� D6�qD7��D8�D8� D9�D9�D:D:��D;�D;� D<  D<� D=  D=� D=�qD>� D?  D?}qD?�qD@}qD@�qDA� DB  DB� DC�DC�DD�DD� DE  DEz�DE�qDF� DG  DG}qDG�qDH}qDI  DI�DI�qDJz�DJ�qDK��DL�DL� DL��DM� DN�DN� DN��DO� DP�DP��DQDQ� DR  DR� DR��DS}qDT  DT��DU  DU}qDU�qDV}qDW  DW� DW�qDX� DY�DY� DY��DZ}qD[  D[� D\  D\� D]  D]� D]�qD^}qD^�qD_� D_�qD`}qD`�qDa}qDb�Db��Dc  Dc� Dd  Dd� Dd�qDe}qDf  Df� Dg  Dg� Dh  Dh}qDh�qDi� DjDj�Dj�qDk}qDl  Dl� Dm  Dm}qDn  Dn� Do�Do��Dp  Dp� Dq  Dq� Dr  Dr� Ds�Ds� Ds�qDt}qDu  Du��Dv  Dvz�Dv�qDw� Dx  Dx� Dy  Dy}qDz  Dz��D{�D{� D|  D|� D|�qD}��D~D~��D�D� D�  D�@ D�� D���D���D�=qD�~�D�� D�  D�@ D�~�D�� D�  D�>�D�}qD���D�  D�@ D�~�D���D�HD�AHD�~�D���D���D�>�D��HD�D�  D�>�D�� D�� D���D�@ D��HD�� D���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�>�D�~�D�� D�HD�B�D��HD�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D���D�@ D�� D��qD���D�@ D�~�D���D�  D�>�D�~�D��qD��qD�=qD�� D��HD�  D�@ D�~�D���D���D�@ D�� D�� D���D�>�D��HD�� D�  D�AHD��HD�� D���D�>�D�� D��HD�  D�>�D�~�D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�D�  D�@ D��HD�� D���D�@ D��HD��HD�HD�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD��HD��HD�HD�AHD���D�� D���D�@ D�� D��HD�  D�>�D�� D���D��qD�>�D��HD�D�  D�>�D�� D��HD�HD�AHD�� D���D���D�>�D��HD��HD��qD�>�D��HD�D���D�@ D��HD���D���D�AHD���D�� D��qD�>�D�� D�� D�HD�>�D�� D��HD�  D�=qD�~�D�� D���D�=qD�~�D��HD�  D�AHD���D�� D���D�=qD�~�D�� D�HD�@ D�~�D�� D�  D�>�D�}qD��qD��)D�@ D��HD��HD�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�>�D�� D��HD�  D�>�D�}qD��qD��qD�=qD�}qD��qD��qD�AHD���D�� D�  D�B�D��HD���D�  D�B�D���D�D��D�@ D�~�D���D���D�>�D�~�D���D�  D�@ D��HD�� D�  D�AHD�~�D��HD�HD�@ DHD�� D�  D�>�DÀ D�� D��qD�>�DĀ Dľ�D�  D�@ DŁHD�� D�  D�@ Dƀ Dƾ�D���D�@ Dǀ D��HD�  D�>�DȀ D�� D���D�@ Dɀ Dɾ�D�  D�AHDʀ D��HD�  D�@ DˁHD˾�D���D�@ D̀ D̾�D���D�@ D́HD�� D���D�>�D�~�D�� D�  D�=qD�~�D��HD�HD�@ DЁHD��HD���D�>�D�~�DѾ�D�  D�@ DҁHD�� D���D�AHDӀ D�� D�  D�@ DԁHD��HD�  D�@ D�~�DսqD���D�@ Dր D�D�  D�AHDׁHD�� D���D�>�D؀ D��HD��D�AHDـ D��HD�HD�AHDڀ D�� D��D�AHDۀ D��HD��D�@ D�~�D��HD�HD�B�D݁HDݾ�D�  D�>�Dހ D�� D�HD�B�D߁HD��HD�HD�@ D�� DྸD���D�>�D�~�DᾸD��qD�=qD� D��HD�HD�AHD� D㾸D���D�>�D� D�� D�  D�@ D� D�� D�  D�AHD�HD��HD�  D�=qD�~�D�� D�  D�AHD� D�� D��D�AHD�HD龸D�  D�B�D� D꾸D�  D�AHD�HD뾸D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�HD��HD�HD�B�D�HD�� D�  D�>�D�� D�� D���D�>�D�HD��HD�HD�@ D� D�D�  D�@ D�~�D�D�HD�>�D�~�D���D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D���D���D�AHD�� D�� D�HD�@ D�~�D���D�HD�<)>���?#�
?aG�?�\)?�{?�ff@�\@z�@(��@=p�@Tz�@c�
@u@��@��@��H@��
@���@�Q�@��
@���@�z�@�p�@���@�z�@��RA33A��A�RA�
A�A(�A!G�A&ffA*�HA/\)A3�
A8Q�A>{AC�
AH��AMp�AR�\AXQ�A]p�Ab�\Ag
=Al(�Aq�Aw�A|��A���A��A�ffA�G�A�33A�{A���A��
A�ffA���A��A�ffA�G�A��
A�{A���A�(�A��RA���A��
A��RA���A�z�A�
=A�G�A�z�A�\)A��A�z�A�
=A��A��A׮A��A���A߮A�\A��A�\)A�=qA��A�  A��HA�p�A�  A��HA�{B Q�B��B�HBQ�BB33Bz�B	B33B��B{B�Bz�B{B�B��B=qB�B��B=qB�B�B=qB�B ��B"ffB#�
B%�B&ffB'�B)�B*�\B,  B-G�B.�\B/�
B1p�B2�RB4  B5G�B6�RB8Q�B9��B:�HB<  B=p�B>�RB@Q�BABC
=BD(�BEp�BG
=BHz�BIBK
=BL(�BM��BN�HBPQ�BQBR�HBT(�BUp�BV�HBX(�BYp�BZ�\B[�
B]�B^�\B_�
Ba�Bb=qBc�Bd��Bf=qBg�Bh��Bj{Bk\)Bl��Bn{Bo�Bp��Br{Bs\)Bt��Bv{Bw�Bx��BzffB{�B|��B~=qB�B�z�B�G�B��B���B�G�B��
B�z�B�33B��B���B�G�B��
B�z�B�
=B�B�ffB��B�B�ffB�
=B���B�=qB��HB��B�=qB��HB���B�=qB���B�\)B�  B���B�\)B�  B��RB�G�B��B�z�B�
=B�B�ffB��B�B�ffB���B���B�(�B���B�p�B�(�B��HB���B�Q�B���B��B�Q�B���B���B�Q�B���B�B�z�B�33B��B��\B�33B�B�z�B�33B��
B��\B�G�B�  B��RB�\)B�  B��RB�p�B�{B���B��B�=qB��HB���B�Q�B��B��
B��\B�G�B�  B��RB�p�B�=qB���B��B�ffB��B��B£�B�\)B�{B���B�p�B�(�B��HBǮB�ffB��B��BʸRB�p�B�=qB���BͮB�z�B�33B��BУ�B�\)B�{B���BӅB�=qB���BծB�Q�B�
=Bי�B�(�BظRB�33BٮB�(�Bڏ\B���B�G�BۅB��B�(�B�ffBܸRB�
=B�\)BݮB�  B�Q�Bޣ�B�
=B�\)B�B�  B�ffB�RB�
=B�G�B�B�{B�ffB�RB��B�p�B�B�{B�ffB���B��B�B��
B�(�B�z�B��HB�G�B癚B�  B�Q�B�RB��B�p�B��
B�(�B�\B��HB�G�B뙚B�  B�Q�B�RB�
=B�p�B�B�(�B�z�B���B�33B�B��B�Q�B��B�
=B�p�B��B�Q�B�RB��B�B�  B�ffB���B�33B��B�{B�z�B��HB�G�B�B�(�B��\B�
=B�\)B��B�Q�B��RB�33B���B�{B�z�B��HB�\)B�B�(�B���B�
=B�p�B��C (�C \)C �\C C ��C33Cp�C��C�
C
=CG�Cp�C�C�
C{C=qCp�C��C�
C
=C=qCp�C��C��C
=C=qCp�C�C�HC�CG�C�C�RC��C(�C\)C��C��C  C=qCp�C��C�
C	{C	G�C	�C	C	��C
(�C
\)C
��C
��C  C=qCp�C��C�
C{CG�Cp�C�C�HC�CQ�C�\C��C  C=qCz�C�C�C33Cp�C�C��C(�CffC��C�C(�CffC��C�HC�C\)C��C��C
=CQ�C�\C��C{CQ�C�\C�
C{CQ�C�\C��C  C=qCp�C�C�C(�CffC�C�C(�CffC��C�HC{CQ�C�\C��C  C=qCz�CC  CG�C�\C��C
=C=qCz�C�RC��C33Cp�C�RC��C33Cz�C�RC��C33Cz�C�RC�C 33C p�C �C �C!=qC!z�C!�C!�C"(�C"ffC"��C"�C#(�C#p�C#�C#�C$(�C$\)C$��C$�
C%{C%Q�C%��C%�
C&�C&\)C&��C&�
C'{C'Q�C'�C'��C(
=C(Q�C(��C(�
C)�C)\)C)��C)��C*{C*Q�C*�\C*�HC+(�C+ffC+�C+�HC,�C,\)C,��C,�HC-(�C-ffC-�C-�C.(�C.p�C.��C.�HC/�C/\)C/��C/�C033C0z�C0�RC0��C133C1p�C1�C1��C2=qC2�C2C3
=C3Q�C3�\C3�
C4{C4\)C4�\C4��C5
=C5Q�C5��C5�HC6�C6Q�C6��C6��C7
=C7G�C7��C7�
C8{C8Q�C8�\C8��C9�C9ffC9��C9�C:�C:\)C:�C:��C;=qC;z�C;�RC<  C<G�C<��C<�HC=�C=ffC=�C>  C>G�C>�C>C?
=C?\)C?�C?��C@33C@z�C@CA{CAffCA�CA�CB33CB�CB�
CC�CCffCC�CC��CDG�CD�\CD�HCE(�CEp�CE�CF  CFG�CF��CF�HCG(�CGffCG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA�+A�1'A��A�9XA�&�A�$�A��A��A��A�$�A�&�A��TA�\)A�A��TA��#A��A���A���A���A���A�ƨA���AμjAζFAΰ!AΧ�AΛ�A΍PA�|�A�hsA�\)A�{A��;A�M�A��;A�9XA�t�A²-A���A��A�5?A�I�A���A���A���A��DA��/A��9A��A���A�;dA��!A���A�  A�x�A��
A�G�A��A�O�A��9A��A��;A���A�7LA�9XA�7LA�;dA���A�;dA��#A��+A��RA�oA�?}A�p�A���A��A��A���A�bA� �A���A�~�A�1'A}/A{��Ax�HAr�jAoS�Ak��Af��Aa�A]�mA[��AZ��AX�yAP�RAI�-AHA�AG
=AE�hAD�yAD�DAD9XAB��A>�A=O�A<��A<bNA<A�A;�FA;�A;�PA:VA;&�A:��A:^5A9`BA7dZA6ĜA6bNA6E�A5G�A4ffA4^5A3�A3��A3&�A2��A2I�A1l�A0��A0^5A/`BA.~�A.A-��A-�A-
=A+��A+"�A*ffA)��A)VA(��A(E�A(A'A'7LA&�jA&I�A&�A&-A%�#A%�wA%�A$ZA$=qA#�TA#�A#`BA#?}A#G�A"�A"��A"�+A"v�A"bNA"�uA"bA!�A"bNA"�!A"�A#hsA#|�A#|�A#
=A!�A �A�AƨA��A��AdZA33AȴA��Al�A��A9XA�hAK�AoA��A=qA%A�AXA��AA��A�AG�A%A��A�yA�DA{A1A  A�mA��A?}A�A��A-A�FA�AK�A��A5?A�hA�A�AM�A�^AdZA;dA7LA
�HA
��A
ffA
1'A	�TA	t�A�A��A�A~�AZAJA��A`BA�AĜA�A�uAA�A��A+A��A��A{A��AoA��A9XAƨAA��A�-A�hA�7A?}AoA z�A J@�ƨ@�
=@�E�@���@�X@���@��@�t�@�;d@�n�@���@�hs@��9@�A�@���@�ƨ@�dZ@�ȴ@��@�@���@���@���@�9X@��
@�33@�\@���@�?}@�%@��D@�w@�C�@�^5@�hs@��@���@�9@�1@�;d@ꗍ@�{@�7L@�A�@��@�
=@�v�@�@�Ĝ@��@�"�@�!@�M�@��@��@�X@�A�@��
@ߕ�@�
=@އ+@��#@�&�@�Q�@۶F@�\)@��@�-@�O�@�Ĝ@�A�@�  @׮@�33@�^5@�@Ձ@�V@�I�@��@��@�V@�@Ѳ-@ёh@с@�X@��@�%@��/@�j@υ@�l�@�dZ@�+@��@�-@��#@�hs@�O�@�`B@���@̋D@��@��@�E�@���@ɑh@�/@���@�I�@��m@ǝ�@�o@�~�@��@�x�@���@�z�@�A�@��@î@�"�@��@°!@�v�@�5?@�/@�Q�@��
@���@�S�@��!@��@��h@�p�@��@�1'@��P@��@��H@�ȴ@��!@�{@���@��@�z�@�1@�t�@��@���@��y@�n�@�-@�@���@�X@���@��@��
@��@��@���@�~�@�@���@��`@�7L@��/@�(�@��
@��@�dZ@�"�@���@��@��@�O�@�?}@��@��@���@��@���@��9@�bN@���@��
@��F@���@��@�dZ@��@�n�@��@��@���@���@���@��@�|�@�
=@�n�@�E�@�5?@���@��h@��@��D@�Z@�1'@�ƨ@�;d@�ȴ@��+@�V@�$�@�@��#@��h@�/@��u@�9X@�1@��m@��w@�S�@���@��+@�E�@���@��-@���@��7@�/@���@�z�@��@���@���@�ȴ@��!@�V@�=q@��-@�?}@�&�@���@�z�@�  @��@�o@���@�ȴ@���@�-@��@���@���@�x�@�O�@�%@��9@�j@�1'@��w@�\)@��@��R@�~�@�V@�E�@��T@���@�G�@��@���@�9X@��m@��@�K�@�+@��@�o@��y@�E�@��@��#@��-@��h@�p�@���@���@��D@�bN@�  @��w@���@�\)@�"�@��H@���@�E�@�$�@�@���@�/@��@��j@��@�r�@�bN@�Q�@��@��;@��@�l�@�\)@�;d@���@��\@�~�@�n�@�M�@�=q@�$�@�@���@���@�hs@�?}@���@��`@��/@��@�z�@�Q�@�A�@�(�@�1@��m@��w@��@�C�@�o@��@��@��!@��+@�E�@�{@���@���@�p�@�X@�?}@�?}@��@���@��`@���@�I�@�b@��@\)@�@~�R@~V@~$�@}@}/@|�/@|9X@{ƨ@{t�@z�@z~�@y��@y�^@yG�@x��@xbN@x  @w�;@w�w@w|�@w
=@v�R@vff@u��@u?}@t�/@s��@s�F@s�@sC�@s33@r�!@r^5@q��@p�9@pb@ol�@n��@n�+@nff@nE�@n$�@m��@m�@mO�@l��@l�@l(�@k�F@k"�@j=q@i��@i��@i&�@h�@g��@gK�@f�R@e��@e�@e`B@e�@eV@d�/@d��@d�@c"�@b=q@ahs@aX@aX@a%@`r�@` �@` �@_��@_\)@^��@^$�@]�@]?}@\��@\��@\�j@\�j@\j@[�F@[33@[@Z��@Z-@Y��@Y��@Yhs@X��@W�;@W�@W��@W|�@V��@Vv�@U�T@Up�@U?}@U�@T��@T�j@TZ@T9X@S�
@S33@R�\@RM�@R�@Q��@Q�#@Q7L@PQ�@O�@O|�@O;d@N��@N{@M�@M@M`B@M/@L�j@L�@K��@Kƨ@K��@KS�@K@Jn�@J�@Ihs@H�`@H1'@H  @G�@F�R@F{@EO�@E/@D�@D9X@D�@C�
@C�F@B�@B��@B�!@B��@A��@A�@@�9@@�@@A�@@b@@  @?��@?;d@>�@>{@=�h@=`B@=V@<�D@<(�@;��@;�
@;��@;t�@;dZ@:�@9�@9�7@9��@9X@8��@8r�@8 �@7l�@7+@6��@6�R@65?@5�T@5p�@5O�@5?}@5/@5/@5�@4��@4��@4�D@4z�@4I�@3��@3S�@3C�@333@2�@2��@2��@2��@2��@2-@1x�@0�u@0bN@0b@/�@/��@/\)@/
=@.ȴ@.�R@.�R@.��@.v�@.5?@.$�@.ff@.E�@.@-�@-�@-V@,�@,��@,j@,Z@,�D@,�D@+��@+�@+S�@+@*-@)hs@)&�@(�9@(1'@(  @'�@'+@&�y@&v�@&5?@%�T@%p�@%O�@%�@$��@$��@$Z@#��@#�
@#��@#dZ@#C�@#33@#@"��@"��@"~�@"M�@!��@!�#@!��@!x�@!7L@ Ĝ@ ��@ ��@ �@ Q�@   @��@l�@��@ȴ@v�@$�@@�-@��@�h@�@?}@��@�@�@I�@1@��@C�@�H@��@�!@~�@M�@-@�@��@�^@��@�7@G�@%@�`@��@Ĝ@�9@�u@r�@ �@�w@\)@;d@
=@�@�R@ff@�@�@O�@�@�@��@��@Z@�m@ƨ@�@dZ@dZ@33@�@�\@=q@�^@��@hs@G�@7L@�@�`@�9@�u@A�@�@�;@�w@�@|�@\)@+@
=@�@��@��@v�@V@V@E�@5?@5?A�?}A�?}A�?}A�(�A�-A�33A�/A��A�7LA�7LA�33A��A��A��A�+A�9XA�;dA�;dA�&�A�(�A� �A��A�"�A�(�A�"�A�bA��A��A��A��A�bA�{A��A��A�"�A�$�A�/A�/A�-A�1'A�&�A���A��yA��mA��
A���A��HA�  A���A�|�A�Q�A�E�A�E�A�=qA��A��A�{A��A��A��yA��mA��yA��mA��TA��;A��/A��/A��;A��/A��A��A��A��#A��A��
A��
A��A��A��
A���A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A���A���A���A�ȴA�ȴA���A���A�ȴA�ĜA�ĜA�A�ĜA�ĜA���AξwAξwA���AξwAκ^AθRAκ^AμjAμjAθRAδ9AζFAζFAδ9Aβ-Aΰ!Aΰ!Aβ-Aβ-AήAΩ�AΧ�AΧ�AΧ�AΧ�AΡ�AΝ�AΝ�AΟ�AΝ�AΙ�AΓuAΏ\AΑhAΏ\A΍PA΋DAΉ7A΃A΅A΁A�~�A�v�A�p�A�l�A�jA�jA�hsA�dZA�bNA�bNA�dZA�dZA�`BA�\)A�S�A�G�A�?}A�9XA�$�A�A��TA���AͰ!A͇+A��Ḁ�A�(�A�z�Aʙ�A�\)A�E�A�5?A��A��mA�jA�?}A�A��A���A�33A�l�A�A�A�oA�$�A�5?A�+A���Aú^A�z�A�33A��A��A��#A���A¶FA¬A�A�jA�C�A�/A���A��9A��;A�ȴA�\)A�9XA� �A�VA�%A��A���A�ffA�I�A�5?A�bA���A��A��`A���A��A��A��DA�E�A���A���A���A��hA��A�`BA�33A�  A�ƨA�v�A�E�A�/A��A���A���A���A�v�A���A��uA�bNA��A��yA�z�A�bNA�I�A�"�A���A��RA�x�A�G�A�&�A�A��`A�r�A�oA���A��/A��A�
=A���A��A���A��RA���A���A��DA�Q�A�  A��^A�+A��^A��A�VA��A��yA��A�-A�  A��/A��jA���A�x�A�^5A�I�A�A�A�33A��mA��9A���A��\A��7A��A�r�A�\)A�?}A�oA��#A��A��A�~�A��7A�~�A�VA���A�ĜA���A�A�ĜA�ȴA�ȴA���A�`BA�5?A�bA���A��;A��A�x�A�E�A��A���A��/A��jA��+A�=qA���A���A�ffA�-A��A���A�^5A��A��A���A��-A��7A�\)A��yA�x�A��A�ĜA��9A���A���A��\A��+A�v�A�jA�S�A�33A�1A��mA�ȴA��!A���A��uA��A�z�A�^5A�=qA�+A��A���A��A���A��hA�G�A��#A�\)A�1'A�%A��^A��PA��+A�5?A���A���A�33A��;A�^5A��A�VA���A�~�A�?}A�=qA�=qA�33A�"�A�A��/A��A�bNA�{A���A��A��A�ȴA���A�z�A�n�A�jA�Q�A�;dA�+A��A�VA�  A��`A��A��
A��
A���A��
A��
A���A���A�ȴA��FA���A��A�\)A�33A�1A��A��A��mA��HA��HA��HA��;A��;A��/A��#A��A���A���A�ȴA���A��#A�\)A��mA��PA��A���A��DA�bNA�=qA�{A��A�A���A�|�A�l�A�^5A�S�A�A�A�7LA�(�A� �A��A�
=A�  A��mA���A���A��-A���A�|�A�^5A�5?A�
=A��yA�A��A�VA�7LA��A��yA���A��uA���A�A���A�A�jA�"�A��yA��A��A�33A���A�ffA�A�A�$�A��A�VA�1A�  A���A��A��A��yA��yA��mA��`A��TA��HA��;A��#A��A��
A���A���A���A�ĜA��jA��-A���A��hA�x�A�"�A��A�dZA�JA�ĜA��DA�Q�A�C�A�1'A��A��
A�A��hA�^5A� �A���A���A���A��A�\)A�33A�A��HA�ĜA��FA���A�M�A���A�VA��HA�K�A��A�&�A;dA~bNA~A}ƨA}��A}�A}l�A}XA}7LA|��A|��A|n�A|A�A|9XA|-A{�A{�A{t�A{XA{?}A{�AzȴAzVAy��Ax�9Aw��Av�!Av1'Au`BAt��At9XAs�7Ar(�AqXAqApȴAp�ApA�ApJAo��Ao��AoAnZAm�#Am�-Am��Am\)Al�`Ak�FAj�+Ai|�Ah�HAhZAg�#Agp�Af�Af�DAe�AeG�Ad�!AdjAc��Ac"�Ab1A`v�A_��A_dZA^�yA^�\A^ZA^5?A^A]�
A]`BA]VA\��A\�A\=qA\  A[�^A[dZA[;dA["�A[AZ�AZ�AZ�AZ�yAZ��AZ�9AZ��AZv�AZbNAZ-AZ  AY`BAX�AW|�AVJAT��AS��AS\)ARn�AP��AN(�AL~�AK��AJ�/AJbAI��AIp�AIO�AI;dAH��AH��AH�9AHjAHM�AH$�AG�AG�wAG��AG��AG�7AGp�AGoAFQ�AFbAE�#AE��AE��AE��AE�hAE|�AEp�AEK�AE�AD��AD�/AD��AD��AD�jAD�AD��AD�uAD�+AD�AD~�ADr�ADn�ADffAD=qAD1'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                               A�7LA�+A�1'A��A�9XA�&�A�$�A��A��A��A�$�A�&�A��TA�\)A�A��TA��#A��A���A���A���A���A�ƨA���AμjAζFAΰ!AΧ�AΛ�A΍PA�|�A�hsA�\)A�{A��;A�M�A��;A�9XA�t�A²-A���A��A�5?A�I�A���A���A���A��DA��/A��9A��A���A�;dA��!A���A�  A�x�A��
A�G�A��A�O�A��9A��A��;A���A�7LA�9XA�7LA�;dA���A�;dA��#A��+A��RA�oA�?}A�p�A���A��A��A���A�bA� �A���A�~�A�1'A}/A{��Ax�HAr�jAoS�Ak��Af��Aa�A]�mA[��AZ��AX�yAP�RAI�-AHA�AG
=AE�hAD�yAD�DAD9XAB��A>�A=O�A<��A<bNA<A�A;�FA;�A;�PA:VA;&�A:��A:^5A9`BA7dZA6ĜA6bNA6E�A5G�A4ffA4^5A3�A3��A3&�A2��A2I�A1l�A0��A0^5A/`BA.~�A.A-��A-�A-
=A+��A+"�A*ffA)��A)VA(��A(E�A(A'A'7LA&�jA&I�A&�A&-A%�#A%�wA%�A$ZA$=qA#�TA#�A#`BA#?}A#G�A"�A"��A"�+A"v�A"bNA"�uA"bA!�A"bNA"�!A"�A#hsA#|�A#|�A#
=A!�A �A�AƨA��A��AdZA33AȴA��Al�A��A9XA�hAK�AoA��A=qA%A�AXA��AA��A�AG�A%A��A�yA�DA{A1A  A�mA��A?}A�A��A-A�FA�AK�A��A5?A�hA�A�AM�A�^AdZA;dA7LA
�HA
��A
ffA
1'A	�TA	t�A�A��A�A~�AZAJA��A`BA�AĜA�A�uAA�A��A+A��A��A{A��AoA��A9XAƨAA��A�-A�hA�7A?}AoA z�A J@�ƨ@�
=@�E�@���@�X@���@��@�t�@�;d@�n�@���@�hs@��9@�A�@���@�ƨ@�dZ@�ȴ@��@�@���@���@���@�9X@��
@�33@�\@���@�?}@�%@��D@�w@�C�@�^5@�hs@��@���@�9@�1@�;d@ꗍ@�{@�7L@�A�@��@�
=@�v�@�@�Ĝ@��@�"�@�!@�M�@��@��@�X@�A�@��
@ߕ�@�
=@އ+@��#@�&�@�Q�@۶F@�\)@��@�-@�O�@�Ĝ@�A�@�  @׮@�33@�^5@�@Ձ@�V@�I�@��@��@�V@�@Ѳ-@ёh@с@�X@��@�%@��/@�j@υ@�l�@�dZ@�+@��@�-@��#@�hs@�O�@�`B@���@̋D@��@��@�E�@���@ɑh@�/@���@�I�@��m@ǝ�@�o@�~�@��@�x�@���@�z�@�A�@��@î@�"�@��@°!@�v�@�5?@�/@�Q�@��
@���@�S�@��!@��@��h@�p�@��@�1'@��P@��@��H@�ȴ@��!@�{@���@��@�z�@�1@�t�@��@���@��y@�n�@�-@�@���@�X@���@��@��
@��@��@���@�~�@�@���@��`@�7L@��/@�(�@��
@��@�dZ@�"�@���@��@��@�O�@�?}@��@��@���@��@���@��9@�bN@���@��
@��F@���@��@�dZ@��@�n�@��@��@���@���@���@��@�|�@�
=@�n�@�E�@�5?@���@��h@��@��D@�Z@�1'@�ƨ@�;d@�ȴ@��+@�V@�$�@�@��#@��h@�/@��u@�9X@�1@��m@��w@�S�@���@��+@�E�@���@��-@���@��7@�/@���@�z�@��@���@���@�ȴ@��!@�V@�=q@��-@�?}@�&�@���@�z�@�  @��@�o@���@�ȴ@���@�-@��@���@���@�x�@�O�@�%@��9@�j@�1'@��w@�\)@��@��R@�~�@�V@�E�@��T@���@�G�@��@���@�9X@��m@��@�K�@�+@��@�o@��y@�E�@��@��#@��-@��h@�p�@���@���@��D@�bN@�  @��w@���@�\)@�"�@��H@���@�E�@�$�@�@���@�/@��@��j@��@�r�@�bN@�Q�@��@��;@��@�l�@�\)@�;d@���@��\@�~�@�n�@�M�@�=q@�$�@�@���@���@�hs@�?}@���@��`@��/@��@�z�@�Q�@�A�@�(�@�1@��m@��w@��@�C�@�o@��@��@��!@��+@�E�@�{@���@���@�p�@�X@�?}@�?}@��@���@��`@���@�I�@�b@��@\)@�@~�R@~V@~$�@}@}/@|�/@|9X@{ƨ@{t�@z�@z~�@y��@y�^@yG�@x��@xbN@x  @w�;@w�w@w|�@w
=@v�R@vff@u��@u?}@t�/@s��@s�F@s�@sC�@s33@r�!@r^5@q��@p�9@pb@ol�@n��@n�+@nff@nE�@n$�@m��@m�@mO�@l��@l�@l(�@k�F@k"�@j=q@i��@i��@i&�@h�@g��@gK�@f�R@e��@e�@e`B@e�@eV@d�/@d��@d�@c"�@b=q@ahs@aX@aX@a%@`r�@` �@` �@_��@_\)@^��@^$�@]�@]?}@\��@\��@\�j@\�j@\j@[�F@[33@[@Z��@Z-@Y��@Y��@Yhs@X��@W�;@W�@W��@W|�@V��@Vv�@U�T@Up�@U?}@U�@T��@T�j@TZ@T9X@S�
@S33@R�\@RM�@R�@Q��@Q�#@Q7L@PQ�@O�@O|�@O;d@N��@N{@M�@M@M`B@M/@L�j@L�@K��@Kƨ@K��@KS�@K@Jn�@J�@Ihs@H�`@H1'@H  @G�@F�R@F{@EO�@E/@D�@D9X@D�@C�
@C�F@B�@B��@B�!@B��@A��@A�@@�9@@�@@A�@@b@@  @?��@?;d@>�@>{@=�h@=`B@=V@<�D@<(�@;��@;�
@;��@;t�@;dZ@:�@9�@9�7@9��@9X@8��@8r�@8 �@7l�@7+@6��@6�R@65?@5�T@5p�@5O�@5?}@5/@5/@5�@4��@4��@4�D@4z�@4I�@3��@3S�@3C�@333@2�@2��@2��@2��@2��@2-@1x�@0�u@0bN@0b@/�@/��@/\)@/
=@.ȴ@.�R@.�R@.��@.v�@.5?@.$�@.ff@.E�@.@-�@-�@-V@,�@,��@,j@,Z@,�D@,�D@+��@+�@+S�@+@*-@)hs@)&�@(�9@(1'@(  @'�@'+@&�y@&v�@&5?@%�T@%p�@%O�@%�@$��@$��@$Z@#��@#�
@#��@#dZ@#C�@#33@#@"��@"��@"~�@"M�@!��@!�#@!��@!x�@!7L@ Ĝ@ ��@ ��@ �@ Q�@   @��@l�@��@ȴ@v�@$�@@�-@��@�h@�@?}@��@�@�@I�@1@��@C�@�H@��@�!@~�@M�@-@�@��@�^@��@�7@G�@%@�`@��@Ĝ@�9@�u@r�@ �@�w@\)@;d@
=@�@�R@ff@�@�@O�@�@�@��@��@Z@�m@ƨ@�@dZ@dZ@33@�@�\@=q@�^@��@hs@G�@7L@�@�`@�9@�u@A�@�@�;@�w@�@|�@\)@+@
=@�@��@��@v�@V@V@E�@5?@5?A�?}A�?}A�?}A�(�A�-A�33A�/A��A�7LA�7LA�33A��A��A��A�+A�9XA�;dA�;dA�&�A�(�A� �A��A�"�A�(�A�"�A�bA��A��A��A��A�bA�{A��A��A�"�A�$�A�/A�/A�-A�1'A�&�A���A��yA��mA��
A���A��HA�  A���A�|�A�Q�A�E�A�E�A�=qA��A��A�{A��A��A��yA��mA��yA��mA��TA��;A��/A��/A��;A��/A��A��A��A��#A��A��
A��
A��A��A��
A���A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A���A���A���A�ȴA�ȴA���A���A�ȴA�ĜA�ĜA�A�ĜA�ĜA���AξwAξwA���AξwAκ^AθRAκ^AμjAμjAθRAδ9AζFAζFAδ9Aβ-Aΰ!Aΰ!Aβ-Aβ-AήAΩ�AΧ�AΧ�AΧ�AΧ�AΡ�AΝ�AΝ�AΟ�AΝ�AΙ�AΓuAΏ\AΑhAΏ\A΍PA΋DAΉ7A΃A΅A΁A�~�A�v�A�p�A�l�A�jA�jA�hsA�dZA�bNA�bNA�dZA�dZA�`BA�\)A�S�A�G�A�?}A�9XA�$�A�A��TA���AͰ!A͇+A��Ḁ�A�(�A�z�Aʙ�A�\)A�E�A�5?A��A��mA�jA�?}A�A��A���A�33A�l�A�A�A�oA�$�A�5?A�+A���Aú^A�z�A�33A��A��A��#A���A¶FA¬A�A�jA�C�A�/A���A��9A��;A�ȴA�\)A�9XA� �A�VA�%A��A���A�ffA�I�A�5?A�bA���A��A��`A���A��A��A��DA�E�A���A���A���A��hA��A�`BA�33A�  A�ƨA�v�A�E�A�/A��A���A���A���A�v�A���A��uA�bNA��A��yA�z�A�bNA�I�A�"�A���A��RA�x�A�G�A�&�A�A��`A�r�A�oA���A��/A��A�
=A���A��A���A��RA���A���A��DA�Q�A�  A��^A�+A��^A��A�VA��A��yA��A�-A�  A��/A��jA���A�x�A�^5A�I�A�A�A�33A��mA��9A���A��\A��7A��A�r�A�\)A�?}A�oA��#A��A��A�~�A��7A�~�A�VA���A�ĜA���A�A�ĜA�ȴA�ȴA���A�`BA�5?A�bA���A��;A��A�x�A�E�A��A���A��/A��jA��+A�=qA���A���A�ffA�-A��A���A�^5A��A��A���A��-A��7A�\)A��yA�x�A��A�ĜA��9A���A���A��\A��+A�v�A�jA�S�A�33A�1A��mA�ȴA��!A���A��uA��A�z�A�^5A�=qA�+A��A���A��A���A��hA�G�A��#A�\)A�1'A�%A��^A��PA��+A�5?A���A���A�33A��;A�^5A��A�VA���A�~�A�?}A�=qA�=qA�33A�"�A�A��/A��A�bNA�{A���A��A��A�ȴA���A�z�A�n�A�jA�Q�A�;dA�+A��A�VA�  A��`A��A��
A��
A���A��
A��
A���A���A�ȴA��FA���A��A�\)A�33A�1A��A��A��mA��HA��HA��HA��;A��;A��/A��#A��A���A���A�ȴA���A��#A�\)A��mA��PA��A���A��DA�bNA�=qA�{A��A�A���A�|�A�l�A�^5A�S�A�A�A�7LA�(�A� �A��A�
=A�  A��mA���A���A��-A���A�|�A�^5A�5?A�
=A��yA�A��A�VA�7LA��A��yA���A��uA���A�A���A�A�jA�"�A��yA��A��A�33A���A�ffA�A�A�$�A��A�VA�1A�  A���A��A��A��yA��yA��mA��`A��TA��HA��;A��#A��A��
A���A���A���A�ĜA��jA��-A���A��hA�x�A�"�A��A�dZA�JA�ĜA��DA�Q�A�C�A�1'A��A��
A�A��hA�^5A� �A���A���A���A��A�\)A�33A�A��HA�ĜA��FA���A�M�A���A�VA��HA�K�A��A�&�A;dA~bNA~A}ƨA}��A}�A}l�A}XA}7LA|��A|��A|n�A|A�A|9XA|-A{�A{�A{t�A{XA{?}A{�AzȴAzVAy��Ax�9Aw��Av�!Av1'Au`BAt��At9XAs�7Ar(�AqXAqApȴAp�ApA�ApJAo��Ao��AoAnZAm�#Am�-Am��Am\)Al�`Ak�FAj�+Ai|�Ah�HAhZAg�#Agp�Af�Af�DAe�AeG�Ad�!AdjAc��Ac"�Ab1A`v�A_��A_dZA^�yA^�\A^ZA^5?A^A]�
A]`BA]VA\��A\�A\=qA\  A[�^A[dZA[;dA["�A[AZ�AZ�AZ�AZ�yAZ��AZ�9AZ��AZv�AZbNAZ-AZ  AY`BAX�AW|�AVJAT��AS��AS\)ARn�AP��AN(�AL~�AK��AJ�/AJbAI��AIp�AIO�AI;dAH��AH��AH�9AHjAHM�AH$�AG�AG�wAG��AG��AG�7AGp�AGoAFQ�AFbAE�#AE��AE��AE��AE�hAE|�AEp�AEK�AE�AD��AD�/AD��AD��AD�jAD�AD��AD�uAD�+AD�AD~�ADr�ADn�ADffAD=qAD1'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
6B
6�B
5B
5�B
5B
5�B
5B
5?B
5?B
3�B
4�B
5B
2�B
49B
2-B
0�B
0�B
1'B
1[B
1[B
1[B
1�B
1�B
1�B
1�B
1�B
1[B
0�B
0UB
/OB
.}B
,�B
+6B
'B
*�B
"B	��B
�B
{B	�`B	��B
 iB
+�B
<�B
F�B
j�B
�1B
�tB
��B
��B
��B:�BQ�B[WBo B��B��B��B�'B��B��B�\B[#B7�B!-B
�fB
��B
iB
cTB
g�B
y�B
��B
��B
�iB
u%B
k�B
\�B
?B
#�B
�B
	B
�B	��B	�/B	��B	�XB	��B	��B	�-B	�B	~(B	�B	�AB	�lB	��B	�SB	cB	�B	�~B	�:B	�B	��B	�dB	�iB	��B
 �B
1B	�iB	�`B	�QB
	B
�B
A B
J�B
k�B
h�B
�4B
�MB
�OB
�eB
��B
�4B
�\B
�VB
��B
��B
�$B
��B
�1B
��B
��B
�$B
��B
��B
��B
��B
�hB
��B
�rB
�FB
�B
�B
��B
�B
�B
|B
~]B
� B
{�B
z�B
z�B
}"B
|�B
��B
��B
�AB
�AB
�B
|PB
�B
�iB
��B
�B
��B
�B
��B
�1B
��B
�	B
�~B
�\B
��B
�\B
��B
�4B
�B
�aB
��B
��B
��B
��B
�zB
�IB
��B
�$B
��B
�kB
�6B
��B
�B
��B
��B
��B
��B
��B
�YB
�iB
p�B
iyB
_pB
[WB
Y�B
QNB
QNB
P�B
Q�B
U�B
YB
X�B
[#B
\)B
\�B
]�B
^B
_pB
aB
a�B
a�B
`B
]dB
^jB
^B
_;B
\]B
Z�B
YB
W?B
V�B
U�B
S�B
Q�B
V9B
UgB
T�B
TaB
RTB
RTB
PB
O�B
M�B
L�B
L�B
MB
L�B
K)B
JXB
HKB
FtB
E�B
EB
C�B
B�B
?�B
=�B
<�B
<�B
:^B
:^B
7LB
8B
6B
5tB
6zB
7�B
7B
6�B
6zB
6B
6B
3�B
3�B
3hB
3�B
2�B
2aB
2�B
1�B
1'B
0�B
1'B
.}B
.IB
-CB
,B
+�B
*�B
+B
+�B
*0B
)�B
+6B
)_B
'�B
'B
'B
(XB
&�B
'�B
&LB
%FB
&B
%FB
%�B
$B
$B
!�B
!bB
!�B
"4B
!�B
 'B
�B
�B
B
IB
�B
�B
B
kB
�B
$B
�B
�B
YB
�B
B
B
B
�B
uB
oB
hB
�B
�B
VB
�B
�B
�B
VB
�B
�B
JB
JB
B
JB
�B
DB
�B
xB

�B
JB
xB
xB
�B
JB
�B
�B
�B
\B
\B
�B
"B
�B
B
�B
�B
B
�B
4B
�B
hB
�B
oB
B
uB
:B
oB
oB
oB
�B
:B
�B
4B
�B
4B
hB
hB
hB
4B
4B
 B
�B
 B
�B
bB
�B
�B
�B
�B
�B
.B
�B
B
{B
B
uB
{B
FB
�B
MB
B
�B
FB
MB
B
�B
�B
B
�B
FB
B
B
�B
B
MB
B
�B
{B
�B
�B
�B
$B
$B
$B
�B
�B
_B
CB
B
B
�B
OB
OB
~B
�B
�B
B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
"hB
"4B
"hB
"4B
"4B
"4B
"hB
"�B
#:B
#nB
#:B
#B
"�B
#�B
$�B
$@B
%B
%B
$�B
$�B
%B
%B
&B
&�B
&�B
'RB
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)*B
(�B
)_B
*�B
*eB
*0B
*0B
*0B
*�B
+6B
+B
+kB
,qB
,B
+�B
,B
,qB
,=B
,�B
,�B
-�B
-wB
-CB
-�B
.}B
-�B
/OB
.IB
.IB
.}B
.}B
/OB
/OB
/�B
/�B
/�B
/�B
0�B
0�B
0�B
1'B
0�B
0�B
1'B
0�B
0�B
1'B
1�B
1�B
2-B
2�B
2�B
2aB
2-B
33B
2�B
3�B
4B
4nB
4nB
4�B
5B
5?B
5?B
5?B
5B
5�B
6FB
6�B
6zB
6�B
6�B
6�B
8B
7�B
8B
8RB
8�B
9$B
8�B
9�B
9�B
9�B
:*B
;0B
:^B
:�B
;dB
;�B
<6B
<B
<B
<�B
<6B
<jB
<�B
<�B
=<B
=<B
=qB
=<B
>wB
>BB
>B
>BB
>wB
>BB
>wB
>�B
>�B
?}B
?}B
?}B
@OB
?�B
?�B
@�B
@�B
A B
A B
AUB
A�B
A�B
A�B
B[B
B�B
B�B
B�B
B[B
B�B
B[B
B�B
B�B
CaB
D3B
D3B
D3B
DgB
DgB
E9B
EB
EmB
E�B
F�B
F?B
F�B
F�B
G�B
HB
HKB
HKB
H�B
H�B
H�B
IRB
H�B
IB
IRB
I�B
I�B
I�B
JXB
J�B
K^B
K)B
K)B
K)B
K^B
K^B
K^B
K�B
LdB
L0B
L�B
M�B
M6B
M6B
M6B
L�B
MjB
M�B
N�B
OB
OBB
O�B
PB
P}B
PHB
PHB
P}B
P�B
QB
QB
QNB
QB
Q�B
QNB
Q�B
RTB
Q�B
R B
R�B
R�B
S&B
S�B
S�B
T�B
TaB
T�B
T�B
T�B
T�B
TaB
T,B
V�B
VmB
VmB
V9B
V9B
V�B
V9B
UgB
UgB
V9B
W
B
W
B
W?B
W?B
V�B
W�B
W?B
W�B
XEB
YKB
ZQB
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[WB
[#B
Z�B
[#B
Z�B
[�B
\]B
\)B
\)B
\)B
\�B
\]B
\]B
\�B
]�B
]�B
]�B
]�B
]dB
]dB
^jB
^�B
_;B
^�B
_pB
_�B
`BB
_�B
`B
`vB
`BB
aB
a|B
aB
a�B
a|B
a|B
a�B
a�B
bNB
b�B
b�B
c�B
c�B
d�B
d�B
e�B
e`B
e`B
e�B
e�B
ffB
f�B
f�B
gmB
g8B
gB
g8B
gmB
h>B
h
B
h>B
hsB
hsB
hsB
h�B
i�B
i�B
jB
jB
j�B
j�B
kB
kQB
kB
k�B
l"B
l"B
k�B
k�B
k�B
k�B
l"B
l�B
k�B
l"B
m)B
m�B
m]B
m]B
m�B
n�B
o5B
oiB
oiB
oiB
oiB
oiB
o5B
o5B
oiB
o5B
o B
n�B
ncB
n�B
n�B
n�B
n�B
o5B
o�B
oiB
oiB
oiB
o�B
o B
n�B
o5B
o5B
o5B
o5B
oiB
o�B
p;B
p;B
p;B
p�B
qAB
q�B
sB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
v`B
w2B
v�B
v�B
v�B
v`B
v+B
u�B
v+B
v+B
u�B
v`B
v�B
v�B
w2B
w2B
xB
xlB
x�B
y>B
y	B
y	B
y>B
y�B
zDB
z�B
z�B
zxB
zDB
z�B
z�B
z�B
{B
{JB
{B
{B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}"B
}�B
}�B
~(B
~]B
~(B
~�B
~�B
.B
.B
.B
.B
.B
�B
�B
�B
�4B
�iB
��B
�;B
�oB
�B
��B
�B
�uB
��B
��B
��B
��B
�{B
�GB
��B
��B
�B
�SB
�B
�SB
�B
�SB
�SB
��B
��B
�%B
�%B
�YB
��B
��B
��B
��B
��B
��B
�1B
��B
��B
�B
�7B
��B
��B
�7B
�lB
�7B
��B
��B
�=B
�rB
�B
��B
�B
�DB
�xB
�xB
��B
��B
��B
�~B
�~B
��B
��B
��B
��B
��B
�PB
��B
��B
��B
��B
�"B
�"B
�"B
�VB
��B
�VB
7�B
7LB
8RB
5�B
6zB
33B
6�B
6FB
.B
6�B
2�B
6FB
5B
5�B
3�B
5�B
4�B
9XB
4B
5tB
5�B
6�B
6B
3�B
9�B
:*B
6�B
5�B
2�B
2aB
7�B
5tB
5�B
33B
5�B
4�B
6FB
4�B
2aB
3hB
8RB
5�B
3�B
4�B
1�B
/�B
0�B
0UB
5tB
5tB
2�B
2aB
.�B
0�B
6�B
1[B
2-B
1�B
0�B
1�B
1[B
0UB
0UB
0�B
2-B
1[B
0�B
/�B
0�B
1[B
1[B
1[B
/�B
0�B
1�B
1�B
0UB
0�B
1�B
2-B
1[B
0UB
1'B
1�B
2-B
0�B
0UB
0�B
2-B
2aB
1�B
0�B
0�B
2-B
2�B
0�B
0�B
0�B
1�B
2aB
2-B
0�B
0�B
1[B
2�B
2aB
1�B
0�B
0�B
2-B
2aB
1�B
0�B
1'B
2aB
2�B
1�B
0�B
0�B
1�B
2�B
2-B
1�B
0�B
1'B
1�B
2aB
0�B
0�B
0�B
1�B
1�B
1�B
0�B
/�B
0�B
1�B
1'B
/�B
/�B
/�B
0�B
0�B
/�B
.�B
.}B
.}B
/B
0UB
.�B
-�B
.B
/B
/B
.�B
-B
+�B
+�B
,qB
-B
,�B
+�B
*0B
*eB
*�B
,�B
*0B
(�B
%�B
($B
($B
%�B
$tB
'�B
"4B
0!B
0�B
)�B
,qB
.}B
�B
�B
B
�B
{B
+B
Q�B
!�B	��B	��B	�nB
�B
�B	�B	�"B
YB
�B
B

rB
�B	�2B	��B	��B	��B	�|B	�B	�B	�]B
 4B	��B	��B	��B	��B
$B	�vB	�fB	�+B	��B	�B	��B
�B
0�B
(�B
*�B
+B
.B
-wB
-B
.�B
.�B
8�B
O�B
J�B
>�B
FB
GzB
DgB
C�B
EmB
LdB
Q�B
Y�B
jKB
t�B
sB
u%B
�uB
��B
�1B
�B
�DB
��B
�CB
�=B
�RB
�B
�VB
��B
��B
��B
�@B
��B
��B
��B
�qB
�eB
��B
�RB
�B
��B
�HB
�<B
�sBYB"�B1�B9�B<�B:�B=<BG�BMjBM�B[�BS&BN�BQ�BT�BVBS�BX�Bd�BiDBl"BrBr�Bt�Bv�Bw�B�B�B�B�GB��B��B��B�xB�B�kB��B��B�RB��B��B�B��B�0B�aBÖB��B�'B�UB��B�6BҽB՛B�dB��BŢB�zBĜB�HB��B�qB�B�RB��B�}B��B��B�bB��B�7B�oB��B�B��B}�By�Bw2Bs�BtTBq�Bo5BjBTaBOBBN�BK�BJ#BJXBGBC-BE9BFB<6B8�B8RB2�B,�B+B,=B,=B/OB.IB$@B#�B$BIB&�B 'BxB1BB  B
��B
��B
�AB
��B
�QB�B
�QB
��B
�*B
��B
��B
�?B
��B
��B
n�B
gmB
iDB
iyB
gB
g�B
iB
m)B
k�B
t�B
bB
b�B
c�B
jB
k�B
bNB
_�B
`BB
`B
b�B
`�B
b�B
c�B
d�B
iB
gB
g8B
gB
gB
f2B
f�B
g8B
gB
h�B
k�B
l�B
n�B
qB
v�B
��B
��B
�B
��B
�B
�B
�B
�;B
��B
��B
� B
~�B
~�B
}VB
}VB
�B
�B
��B
��B
��B
��B
�kB
��B
�7B
��B
��B
��B
.B
�;B
� B
z�B
z�B
zDB
x8B
w�B
w2B
v�B
sMB
v+B
p;B
qB
m�B
o B
pB
l�B
kQB
o�B
oiB
d�B
h
B
f�B
g�B
a�B
]/B
\�B
Y�B
V�B
]dB
o B
XyB
PHB
R�B
QB
;dB
:^B
49B
2�B
8B
6�B
)*B
*0B
$tB
!bB
!-B
 �B
 'B
 �B
 \B
 'B
�B
!B
�B
�B
OB
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
_B
PB
�B
�B
�B
GB	�xB	��B	��B	�DB	��B	�B	�rB	�B	�B	�B	�KB	��B	�2B	�B	�B	�B	�pB	רB	�yB	�B	�B	�#B	�B	�sB	רB	�RB	ɺB	�gB	��B	��B	�aB	��B	��B	�wB	�}B	�[B	�}B	��B	��B	��B	�:B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�XB	��B	�MB	�=B	��B	�VB	�:B	�B	�fB	�B	�4B	.B	}VB	z�B	zB	{JB	��B	��B	{JB	xlB	v+B	w�B	}"B	� B	�1B	��B	cB	�GB	�4B	��B	~�B	� B	��B	��B	�B	{B	��B	�(B	��B	��B	�=B	��B	��B	�%B	��B	��B	��B	�GB	��B	��B	�SB	��B	�B	��B	�SB	��B	�GB	�GB	��B	��B	cB	~�B	~�B	��B	|�B	~�B	{B	{�B	|�B	x�B	��B	|�B	�4B	��B	��B	�%B	��B	� B	��B	�,B	��B	��B	��B	��B	�~B	��B	�B	��B	��B	�B	��B	�<B	��B	�}B	�BB	��B	�BB	��B	�HB	��B	�XB	�WB	҉B	��B	چB	��B	چB	�]B	ߤB	�vB	��B	�DB	�B	�B	�B	��B	��B	�B	�|B	��B	��B	��B	��B	�B	�JB	��B
B
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                               B
/�B
0`B
.�B
/�B
.�B
/ZB
.�B
.�B
.�B
-NB
.TB
.�B
,|B
-�B
+�B
*�B
*pB
*�B
+B
+B
+B
+BB
+BB
+BB
+BB
+BB
+B
*�B
*B
)B
(/B
&�B
$�B
 �B
$�B
�B	�B	��B	�-B	�B	�wB	�B
%�B
6QB
@ZB
deB
��B
�&B
��B
�jB
؅B4EBK�BU	Bh�BzOB��B�UB��BáB�WB�BT�B1gB�B
�B
nB
b�B
]B
a�B
sYB
zOB
��B
zB
n�B
e�B
V�B
8�B
UB
6B
�B
�B	�wB	��B	�~B	�
B	�5B	�[B	��B	��B	w�B	z�B	{�B	�B	~�B	B	yB	|�B	�0B	��B	��B	��B	�B	�B	�LB	��B
�B	�B	�B	�B
�B
�B
:�B
D>B
elB
b�B
��B
��B
�B
�B
�RB
��B
�B
�B
�BB
�EB
��B
��B
��B
�LB
��B
��B
�<B
�zB
�LB
�?B
�B
�kB
�$B
��B
��B
��B
�FB
|�B
y~B
u�B
xB
y�B
ueB
t_B
t_B
v�B
vkB
{UB
~hB
{�B
{�B
z�B
vB
y~B
zB
|�B
|�B
{�B
��B
��B
��B
�zB
��B
�0B
�B
�OB
�B
�XB
��B
��B
�B
�mB
�2B
�`B
�jB
�,B
��B
�XB
��B
�KB
�B
��B
��B
��B
�6B
�EB
�UB
�CB
�RB
�B
zB
jVB
c+B
Y"B
U	B
SfB
K B
K B
J�B
K�B
OMB
R�B
R�B
T�B
U�B
VDB
W~B
W�B
Y"B
Z�B
[cB
[cB
Y�B
WB
XB
W�B
X�B
VB
TlB
R�B
P�B
PSB
OMB
MAB
K�B
O�B
OB
N|B
NB
LB
LB
I�B
I�B
GQB
FJB
FJB
F�B
FJB
D�B
D
B
A�B
@&B
?TB
>�B
=�B
<vB
9cB
7�B
6�B
6QB
4B
4B
0�B
1�B
/�B
/&B
0,B
1gB
0�B
0`B
0,B
/�B
/�B
-�B
-�B
-B
-NB
,|B
,B
,�B
+�B
*�B
*<B
*�B
(/B
'�B
&�B
%�B
%QB
$�B
$�B
%QB
#�B
#EB
$�B
#B
!�B
 �B
 �B
"
B
 �B
!�B
�B
�B
�B
�B
aB
�B
�B
�B
B
HB
�B
}B
�B
jB
<B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
B
9B
�B
�B
�B
�B
'B
!B
B
	wB
�B
B
�B
�B
�B
B
eB
�B
�B
�B
�B
�B
�B
�B
�B
*B
�B
�B
*B
*B
^B
�B
�B
6B
<B
	B
	B
	wB
�B

IB
�B
�B
UB
�B
OB

�B

}B
B
�B
!B
�B
'B
�B
!B
!B
!B
�B
�B
OB

�B
OB

�B
B
B
B

�B

�B

�B

}B

�B

IB

B

IB
	CB

IB
	wB
	wB
	�B

}B
�B
-B
�B
'B
-B
�B
gB
�B
�B
aB
�B
�B
�B
�B
UB
�B
�B
�B
�B
�B
3B
�B
�B
�B
�B
-B
3B
nB
?B
�B
�B
�B
�B
�B
B
�B
�B
�B
^B
B
B
0B
dB
6B
�B
6B
6B
�B
�B
wB
}B
�B
}B
B
�B
B
�B
�B
�B
B
�B
�B
 B
�B
�B
�B
�B
[B
�B
�B
�B
�B
[B
�B
�B
�B
 gB
 gB
!B
!�B
"sB
"?B
"�B
"�B
"�B
"�B
"�B
"�B
#B
$KB
$B
#�B
#�B
#�B
$�B
$�B
$�B
%B
&#B
%�B
%�B
%�B
&#B
%�B
&�B
&�B
'^B
')B
&�B
'�B
(/B
'^B
)B
'�B
'�B
(/B
(/B
)B
)B
)�B
)5B
)5B
)jB
*<B
*�B
*�B
*�B
*�B
*pB
*�B
*�B
*�B
*�B
+�B
+BB
+�B
,HB
,|B
,B
+�B
,�B
,|B
-�B
-�B
. B
. B
.TB
.�B
.�B
.�B
.�B
.�B
/ZB
/�B
0`B
0,B
0�B
0`B
0`B
1�B
1gB
1�B
2B
2mB
2�B
2mB
3>B
3sB
3sB
3�B
4�B
4B
4EB
5B
5KB
5�B
5�B
5�B
6QB
5�B
6B
6QB
6�B
6�B
6�B
7#B
6�B
8)B
7�B
7�B
7�B
8)B
7�B
8)B
8]B
8�B
9/B
9/B
9/B
:B
9�B
9cB
:5B
:jB
:�B
:�B
;B
;pB
;;B
;�B
<B
<vB
<AB
<AB
<B
<AB
<B
<vB
<�B
=B
=�B
=�B
=�B
>B
>B
>�B
>�B
?B
?TB
@ZB
?�B
@ZB
@�B
A`B
A�B
A�B
A�B
B�B
B�B
B�B
CB
B�B
B�B
CB
ClB
ClB
ClB
D
B
DsB
EB
D�B
D�B
D�B
EB
EB
EB
EyB
FB
E�B
FJB
GQB
F�B
F�B
F�B
FB
GB
GQB
HWB
H�B
H�B
I�B
I�B
J/B
I�B
I�B
J/B
JcB
J�B
J�B
K B
J�B
K5B
K B
KiB
LB
K�B
K�B
L;B
L�B
L�B
MAB
M�B
N|B
NB
NGB
NGB
NGB
NGB
NB
M�B
P�B
PB
PB
O�B
O�B
PSB
O�B
OB
OB
O�B
P�B
P�B
P�B
P�B
P�B
QZB
P�B
QZB
Q�B
R�B
TB
SfB
S�B
T8B
TlB
TlB
TlB
T�B
U�B
U�B
U	B
T�B
T�B
T�B
T�B
U>B
VB
U�B
U�B
U�B
VDB
VB
VB
VDB
WJB
WJB
WJB
W~B
WB
WB
XB
X�B
X�B
X�B
Y"B
YVB
Y�B
YVB
Y�B
Z(B
Y�B
Z�B
[.B
Z�B
[cB
[.B
[.B
[�B
[�B
\ B
\iB
\�B
]oB
]oB
^uB
^AB
_GB
_B
_B
_{B
_�B
`B
`MB
`MB
aB
`�B
`�B
`�B
aB
a�B
a�B
a�B
b%B
b%B
b%B
bYB
c_B
c_B
d1B
d1B
deB
d�B
d�B
eB
d�B
elB
e�B
e�B
elB
e�B
e7B
e�B
e�B
frB
e�B
e�B
f�B
gCB
gB
gB
g�B
hJB
h�B
iB
iB
iB
iB
iB
h�B
h�B
iB
h�B
h�B
h~B
hB
h~B
hJB
h~B
h~B
h�B
i�B
iB
iB
iB
iPB
h�B
h~B
h�B
h�B
h�B
h�B
iB
iPB
i�B
i�B
i�B
jVB
j�B
k�B
l�B
m�B
mhB
m�B
mhB
m4B
m4B
m�B
m�B
n:B
o�B
pB
p�B
p�B
p�B
pFB
pB
o�B
ouB
o�B
o�B
o�B
pB
pFB
pFB
p�B
p�B
q�B
rB
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t_B
t_B
t*B
s�B
t_B
t�B
t�B
t�B
t�B
u1B
u1B
u�B
u�B
v7B
v�B
v�B
vkB
v7B
v�B
wqB
wqB
w�B
xB
w�B
xCB
xwB
x�B
x�B
x�B
x�B
x�B
yIB
y~B
yIB
y�B
zB
zOB
z�B
{!B
{�B
{�B
{�B
|'B
|\B
|\B
|\B
|�B
}-B
|�B
~hB
~�B
~�B
B
~�B
B
~�B
B
B
nB
�B
�B
�B
�B
�@B
�@B
��B
�FB
�zB
�zB
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�RB
��B
��B
�$B
��B
��B
��B
��B
�*B
�*B
�^B
�^B
�^B
�0B
�0B
�eB
�eB
�eB
��B
��B
�B
�6B
�kB
�kB
��B
��B
��B
��B
�B
�<B
�B
12B
0�B
2B
/�B
0,B
,�B
0`B
/�B
'�B
0`B
,HB
/�B
.�B
/ZB
-�B
/ZB
.TB
3
B
-�B
/&B
/ZB
0�B
/�B
-�B
3�B
3�B
0`B
/ZB
,|B
,B
1�B
/&B
/�B
,�B
/ZB
.TB
/�B
.TB
,B
-B
2B
/ZB
-�B
.TB
+�B
)jB
*�B
*B
/&B
/&B
,|B
,B
(�B
*pB
0�B
+B
+�B
+�B
*<B
+BB
+B
*B
*B
*�B
+�B
+B
*pB
)jB
*<B
+B
+B
+B
)�B
*<B
+vB
+vB
*B
*<B
+BB
+�B
+B
*B
*�B
+vB
+�B
*pB
*B
*�B
+�B
,B
+BB
*�B
*<B
+�B
,HB
*pB
*pB
*pB
+vB
,B
+�B
*�B
*<B
+B
,HB
,B
+vB
*pB
*pB
+�B
,B
+vB
*<B
*�B
,B
,HB
+�B
*pB
*<B
+vB
,HB
+�B
+BB
*pB
*�B
+vB
,B
*�B
*pB
*<B
+�B
+vB
+BB
*pB
)jB
*�B
+BB
*�B
)�B
)5B
)�B
*�B
*pB
)�B
(�B
(/B
(/B
(�B
*B
(dB
'^B
'�B
(�B
(�B
(dB
&�B
%�B
%QB
&#B
&�B
&�B
%QB
#�B
$B
$KB
&WB
#�B
"sB
aB
!�B
!�B
aB
&B
!mB
�B
)�B
*pB
#�B
&#B
(/B
zB	�hB	��B	�\B	�-B
 �B
KiB
}B	ςB	��B	� B	��B
	wB	��B	��B
 B
qB
�B
$B
6B	��B	�@B	�nB	�xB	�.B	�4B	��B	�B	��B	��B	�_B	�=B	�CB
�B	�(B	�B	��B	�YB	��B	�IB
OB
*pB
"?B
$KB
$�B
'�B
')B
&�B
(�B
(�B
2�B
I]B
D>B
8]B
?�B
A,B
>B
=�B
?B
FB
K�B
S�B
c�B
nnB
l�B
n�B
|'B
|�B
��B
~�B
��B
�[B
��B
��B
�B
��B
�B
�^B
�wB
��B
��B
�aB
�aB
��B
�#B
�B
�tB
�B
��B
�B
��B
��B
�%B B�B+BB3�B6�B4yB6�BA�BGBGQBUrBL�BHWBKiBN�BO�BMuBR`B^�Bb�Be�Bk�Bl�Bn�Bp{BqLB{�B��Bz�B|�B:B�tB��B�*B��B�B�}B�UB�B�WB�9B��B��B��B�B�HB�;B��B�B��B��B�oB�MB�BŭB�TB�,B�NB��B��B�#B��B�B��B�/B��B��B�B�3B��B�!B�kB��B�tBwqBsYBp�BmhBnBk\Bh�Bc�BNBH�BH�BEyBC�BD
B@�B<�B>�B?�B5�B2�B2B,HB&WB$�B%�B%�B)B'�B�BUB�B�B 3B�B*B�B�B
��B
�eB
�B
��B
�rB
�B3B
�B
�sB
��B
��B
��B
��B
�8B
��B
hJB
aB
b�B
c+B
`�B
a�B
b�B
f�B
e7B
nnB
[�B
\iB
]:B
c�B
elB
\ B
Y�B
Y�B
Y�B
\iB
Z\B
\�B
]:B
^AB
b�B
`�B
`�B
`�B
`�B
_�B
`MB
`�B
`�B
bYB
elB
f�B
h~B
j�B
p�B
~3B
}�B
{�B
|\B
{�B
{�B
{�B
z�B
zOB
zOB
y�B
x�B
xCB
wB
wB
��B
��B
��B
��B
��B
�;B
�B
��B
��B
��B
��B
nB
x�B
z�B
y�B
t�B
t_B
s�B
q�B
qLB
p�B
p{B
l�B
o�B
i�B
j�B
g�B
h�B
i�B
frB
eB
iPB
iB
^AB
a�B
`�B
a�B
[cB
V�B
VDB
SfB
PSB
WB
h�B
R+B
I�B
L;B
J�B
5B
4B
-�B
,|B
1�B
0�B
"�B
#�B
&B
B
�B
wB
�B
BB
B
�B
pB
�B
�B
6B
B
dB
�B
^B
�B
�B
RB
�B
�B
LB
zB
�B
zB
nB
9B
�B
B
B
�B
�B	�3B	��B	�*B	��B	�CB	��B	�FB	��B	�$B	��B	�B	�=B	��B	�B	��B	�MB	�SB	׳B	�"B	�ZB	�+B	ٿB	�cB	��B	϶B	�%B	�ZB	�B	�lB	�B	��B	�ZB	�B	��B	�dB	�)B	�/B	�B	�/B	��B	�WB	�mB	��B	�yB	��B	��B	��B	��B	�wB	�pB	��B	�pB	��B	�
B	�RB	��B	��B	��B	�B	��B	��B	�B	z�B	y�B	x�B	wB	t_B	s�B	t�B	{UB	�FB	t�B	rB	o�B	q�B	v�B	��B	��B	��B	yB	|�B	y�B	zOB	x�B	y�B	:B	|\B	}�B	u1B	�B	��B	�@B	�?B	��B	~hB	�LB	�B	|�B	}�B	|�B	|�B	��B	:B	B	�B	~�B	~hB	B	��B	|�B	|�B	|�B	{UB	yB	xCB	xCB	zOB	vkB	xCB	t�B	u�B	v7B	rSB	:B	v�B	��B	}�B	�?B	�B	�tB	��B	�KB	��B	�UB	�EB	��B	��B	�0B	��B	��B	��B	�^B	��B	�2B	��B	��B	�/B	��B	�cB	��B	�cB	��B	�5B	�
B	�	B	�;B	�rB	�8B	ԠB	�8B	�B	�VB	�(B	��B	��B	�PB	�4B	�\B	�B	�B	�B	�.B	�uB	�B	�YB	��B	��B	��B	�7B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230701040054                            20230701040054AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023070104005420230701040054  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023070104005420230701040054QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023070104005420230701040054QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               