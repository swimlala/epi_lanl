CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-11T02:00:48Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230711020048  20230711020048  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�7����@�7����11  @�7�>��@�7�>��@-���T��@-���T���c�Xy=��c�Xy=�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AB  AB  ?u?��H@@  @�  @�  @�G�@�G�A ��A  A\)A+�A@  A`��A�Q�A�  A�  A�  A�  A�  A߮A�  B   B(�B(�BQ�B (�B'�
B/�
B8(�B@(�BG�BO�BW�
B`(�Bh  Bp  Bx(�B�(�B�{B�{B�(�B�  B��B�  B��B��B�{B�  B�  B�  B��
B�  B�{B�{B�{B�  B�  B�(�B�(�B��B��B��B�  B�  B�  B��B��B�  B��C   C
=C  C��C  C

=C
=C  C  C
=C  C  C  C
=C
=C  C��C!�C$  C&
=C'��C*  C,  C-��C/��C1��C4  C6
=C8
=C:  C;��C=��C@  CA��CC�CF  CH  CJ  CL
=CN
=CP  CR  CS��CU��CW��CZ  C\  C^  C`  Ca��Cc��Cf  Ch  Ci��Ck�Cm��Cp{Cr  Cs�Cu�Cw��Cz  C|  C~  C�C���C�  C�  C���C���C���C�  C�
=C�C�C�  C���C���C���C���C���C�  C�
=C�C���C���C���C�  C�  C�C�  C���C���C�C�
=C�C�  C���C�  C�C�C�C�
=C�  C���C�  C�  C���C���C���C���C���C���C���C���C���C���C�C�
=C�  C�C�C�
=C�  C���C�C�  C�  C���C���C�  C�
=C�C�C�  C�  C�  C�  C�
=C�C�  C�C���C�  C�C�  C�  C�C�  C�C�
=C�C���C�  C�  C���C�  C�C�  C���C�  C�C�  C���C�  C�
=C�C�  C�  C�C�  C��C���C�
=C�C�C�  C���C���C���C�  C�
=C�C�  C�  C���C���C�  C�C�  C�  C���C���D � D�D��D�D� D  D� D  D� D�qD��D  D}qD�D� D  D}qD	�D	�D
�D
�D�D}qD  D�D  D}qD�qD��D�D}qD�D��D�D��D  D}qD��D� D  D}qD  D��D�D}qD�qD}qD�qD� D  D��DD� D��D��D�D� D�D� D  D� D  D� D   D � D!  D!� D"  D"��D#�D#� D$  D$� D$�qD%� D&  D&}qD&��D'� D(�D(� D)  D)� D*�D*��D*�qD+}qD,  D,� D-  D-}qD-�qD.� D.�qD/� D0  D0� D1  D1��D2  D2}qD3  D3��D4�D4� D5  D5� D6�D6��D7D7��D8  D8� D9  D9}qD:  D:� D;  D;� D<  D<� D<�qD=� D>  D>}qD>�qD?}qD?�qD@� DA  DA� DA�qDBz�DB�qDC� DD  DD}qDD��DE� DF�DF� DG�DG��DH  DH��DIDI��DJ  DJ}qDK  DK� DK�qDL� DMDM�DNDN��DO  DO� DP�DP� DQ  DQ��DR�DR� DS  DS� DT  DT}qDU�DU� DV  DV��DW  DW� DX�DX��DY�DY��DZ  DZ� DZ�qD[� D\�D\� D]  D]��D^  D^}qD^�qD_� D`  D`��Da�Da��Db�Db}qDb�qDc}qDd  Dd� De  De��DfDf�Dg  Dg� Dh  Dh}qDi�Di��Dj  Dj� Dj�qDk� Dl�Dl��Dm�Dm� Dn  Dn� Dn�qDo� Dp�Dp� Dq  Dq� Dr  Dr}qDs  Ds� Dt�Dt��DuDu��Dv�Dv}qDv��Dwz�Dw�qDx� Dy  Dy� Dz  Dz��D{�D{��D|�D|}qD|�qD}� D}��D~}qD  D}qD�qD�=qD�� D�D��D�AHD��HD�D�  D�=qD�~�D�� D���D�=qD�~�D��HD�  D�AHD��HD�� D�  D�>�D�� D�� D���D�AHD��HD���D�  D�@ D�� D��HD�  D�@ D��HD�� D���D�@ D��HD�� D�HD�B�D�� D���D��qD�=qD�� D��HD�  D�>�D��HD��HD��qD�=qD�� D�� D�  D�@ D�� D��HD�  D�>�D�~�D���D��qD�@ D��HD��HD�HD�@ D�� D�� D�  D�>�D�~�D���D���D�AHD��HD�� D�HD�AHD��HD��HD�HD�AHD��HD�D�  D�>�D�~�D�� D�  D�AHD��HD�� D���D�>�D�~�D���D�  D�@ D��HD��HD�HD�>�D�}qD���D�  D�B�D��HD�� D���D�>�D��HD�� D�  D�B�D���D���D��qD�>�D��HD�D�  D�@ D�� D���D�  D�@ D�~�D���D�  D�AHD���D�� D��D�B�D��HD��HD�  D�AHD���D��HD�HD�AHD�� D�� D�HD�B�D��HD��HD�  D�AHD���D���D��D�@ D�� D�� D�  D�>�D�~�D���D���D�=qD�� D��HD�HD�AHD��HD��HD�HD�AHD�� D�� D�  D�>�D�}qD��qD��qD�=qD�~�D�� D��D�AHD�~�D���D�HD�@ D�� D�� D�  D�>�D�}qD�� D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D���D���D�>�D�~�D�� D�  D�@ D��HD��HD�HD�AHD��HD���D���D�B�D���D�� D���D�@ D��HD��HD��D�@ D�}qD�� D�HD�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D�� D�� D���D�>�D�� D�� D���D�>�D�� D�� D���D�>�D�� D�� D���D�@ D��HD�� D���D�@ D D�� D�  D�>�D�~�D�� D�HD�AHDāHD�� D���D�@ DŁHD�� D�  D�@ Dƀ D�� D���D�>�Dǀ DǾ�D���D�@ DȁHD�� D�  D�@ D�~�Dɾ�D�HD�@ Dʀ D�� D���D�>�DˁHD��HD�  D�AHD̂�D��HD��D�B�D͂�D�� D���D�@ D΀ D�� D�  D�@ D�~�DϾ�D�HD�AHDЀ D�� D�  D�>�D�~�D�� D�HD�AHDҀ DҾ�D�  D�>�D�~�DӾ�D���D�>�D�~�D�� D�  D�AHDՁHD�� D�  D�@ Dր D�� D���D�AHDׁHD�� D�  D�>�D�}qDؽqD�  D�@ Dـ D��HD�  D�AHDځHDھ�D��qD�>�Dۀ D��HD�  D�@ D܁HD��HD�  D�@ D݀ D�� D�HD�AHDށHD�D��D�AHD߀ D߾�D��qD�>�D�� DྸD�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��HD���D�=qD�~�D�� D�  D�@ D� D�� D���D�>�D� D�D��D�>�D� D羸D���D�>�D� D�� D�  D�@ D�HD��HD���D�>�D� D꾸D�  D�AHD� D�� D�HD�>�D�~�D쾸D�HD�AHD�~�D�� D�HD�@ D�~�DD�  D�>�D�HD�D�  D�@ D�� D��HD���D�=qD�~�D�� D���D�@ D� D�� D�HD�>�D�}qD�D�  D�>�D� D��HD�  D�AHD���D��HD�  D�AHD��HD�� D�  D�@ D�� D�� D���D�>�D�~�D�� D��D�AHD�� D���D�  D�,�D�l�>�?.{?k�?�z�?���?��?�@�@��@+�@=p�@J=q@\(�@s33@��\@���@�z�@��R@�=q@�@��R@Ǯ@�z�@�G�@�=q@��@�p�A�A�A  Az�A=qA!G�A'
=A*�HA0��A7
=A<��AAG�AFffAK�AP��AVffAZ=qA^{Ac�
AhQ�Al(�Ap  As�
Ay��A}p�A�  A��\A�z�A�ffA�  A��A�(�A�ffA�  A��A�z�A�ffA�  A��A�z�A�
=A���A�33A�A���A��A�A�  A��\A��A��A�=qA�z�A�\)A�=qA���A�
=A��A���A�\)A��A�(�A�\)A�=qA���A�\)A�\A�A���A�A�{A�G�A�z�A�\)A��A���B   BB
=Bz�B{B�
B	p�B
�RBQ�B�B�B�BffB�
Bp�B
=Bz�BB
=B��B=qB�B ��B"=qB#�
B%�B&ffB'�
B)G�B*�RB,(�B-G�B.�\B0(�B1B3
=B4(�B5B733B8z�B9B;33B<��B>=qB?\)B@��BB=qBC�
BE�BFffBG�BI�BJ�\BL(�BMG�BN�\BP  BQp�BR�HBT(�BU��BV�RBX(�BY��B[
=B\Q�B]��B_
=B`z�Ba�Bc\)Bdz�BeBg\)Bh��Bj=qBk\)Bl��BnffBo�Bp��Br=qBs�Bu�BvffBw�By�Bz�\B|  B}�B~ffB�  B��RB�\)B��B���B�p�B�(�B���B�\)B�  B��RB�p�B�{B���B�G�B�  B��RB�G�B��
B�ffB��B�B�Q�B��HB�p�B�(�B��HB�p�B�{B���B�\)B�{B��HB��B�(�B��HB���B�ffB��B��
B�z�B�33B��B��RB�p�B�{B���B��B�Q�B��B�B�z�B�33B�  B���B���B�Q�B���B�B�z�B�\)B�{B��RB�\)B�(�B���B��B�ffB�
=B�B�z�B�33B�  B���B��B�(�B��HB��B��\B�G�B�  B��RB�p�B�(�B���B��B�z�B�G�B�  B���B�p�B�Q�B��B�B\B�G�B�  B���Bř�B�ffB��B��
Bȏ\B�G�B�{B���B˅B�(�B���BͅB�(�B��HB�p�B�{BЏ\B���B�p�B��
B�Q�BҸRB��B�\)Bә�B��B�(�Bԏ\B��HB�33B�p�BծB�  B�=qB֏\B��HB�33B�p�B׮B�  B�=qBأ�B���B�33B�p�BٮB�  B�Q�BڸRB�
=B�G�Bۙ�B��
B�{B�ffBܸRB�
=B�\)Bݙ�B��
B�{B�ffB޸RB��B�\)Bߙ�B��B�(�B�ffB���B��B�p�B�B�{B�Q�B��B��HB�33B�B��
B�(�B�z�B���B���B�G�B�B��
B�(�B�\B��HB��B�\)B�B�  B�ffB��B���B�G�B�B��
B�=qB��B���B�\)B�B�  B�Q�B��B���B�G�B��B�{B�z�B��HB�G�B�B��B�=qB��\B���B�p�B��
B�(�B�\B�
=B�\)B�B�(�B�\B���B�\)B�B�{B�z�B���B�G�B��B�(�B��\B���B�\)B��B�Q�B��RB�33B���B�  B�ffB���B�33B��B�{B�ffB���B�33B���C 
=C =qC p�C ��C �HC{CG�C�C�RC��C(�C\)C��C��C��C33CffC��C�
C
=CG�Cz�C�RC�C(�C\)C��C�HC{CG�C�\CC  C=qCz�C�RC��C33Cp�C�C�C	(�C	ffC	��C	�HC
{C
\)C
��C
�
C{CQ�C�\C��C
=CQ�C�\C��C{CQ�C��C�
C{C\)C��C�
C�C\)C��C�
C{CQ�C��C�
C{C\)C��C�
C{CQ�C��C��C{C\)C��C�
C�C\)C��C�C(�Cp�C�RC��C=qC�CC
=CQ�C��C�HC�C\)C��C�C(�Cp�C��C�C33Cp�C�RC  C=qCz�CC
=CQ�C�\C�
C{C\)C��C�C=qCz�CC
=CQ�C��C�C (�C z�C C!  C!G�C!�\C!��C"{C"\)C"��C"�C#(�C#p�C#�C#��C$=qC$z�C$C%
=C%Q�C%��C%�
C&(�C&ffC&�RC'  C'G�C'�\C'�
C(�C(p�C(�RC)  C)G�C)��C)�
C*�C*\)C*�C*��C+=qC+�\C+�
C,�C,p�C,�RC-  C-G�C-�\C-��C.{C.Q�C.��C.�C/33C/�C/��C0{C0\)C0��C0�
C1�C1ffC1�C1��C2=qC2�\C2�
C3�C3ffC3�C3�C433C4p�C4C5{C5\)C5��C5�HC6(�C6ffC6�RC6��C7=qC7�\C7�
C8�C8ffC8�C8�C933C9p�C9�RC:  C:G�C:�\C:�
C;{C;Q�C;�\C;�
C<{C<\)C<��C<�HC=33C=z�C=�RC>  C>G�C>�\C>��C?
=C?G�C?�\C?��C@{C@\)C@��C@�CA=qCAz�CACB
=CBG�CB�CB��CC(�CCp�CCCD
=CDQ�CD�\CD�
CE�CEp�CECF{CFffCF�CF��CG33CG�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              ?u?��H@@  @�  @�  @�G�@�G�A ��A  A\)A+�A@  A`��A�Q�A�  A�  A�  A�  A�  A߮A�  B   B(�B(�BQ�B (�B'�
B/�
B8(�B@(�BG�BO�BW�
B`(�Bh  Bp  Bx(�B�(�B�{B�{B�(�B�  B��B�  B��B��B�{B�  B�  B�  B��
B�  B�{B�{B�{B�  B�  B�(�B�(�B��B��B��B�  B�  B�  B��B��B�  B��C   C
=C  C��C  C

=C
=C  C  C
=C  C  C  C
=C
=C  C��C!�C$  C&
=C'��C*  C,  C-��C/��C1��C4  C6
=C8
=C:  C;��C=��C@  CA��CC�CF  CH  CJ  CL
=CN
=CP  CR  CS��CU��CW��CZ  C\  C^  C`  Ca��Cc��Cf  Ch  Ci��Ck�Cm��Cp{Cr  Cs�Cu�Cw��Cz  C|  C~  C�C���C�  C�  C���C���C���C�  C�
=C�C�C�  C���C���C���C���C���C�  C�
=C�C���C���C���C�  C�  C�C�  C���C���C�C�
=C�C�  C���C�  C�C�C�C�
=C�  C���C�  C�  C���C���C���C���C���C���C���C���C���C���C�C�
=C�  C�C�C�
=C�  C���C�C�  C�  C���C���C�  C�
=C�C�C�  C�  C�  C�  C�
=C�C�  C�C���C�  C�C�  C�  C�C�  C�C�
=C�C���C�  C�  C���C�  C�C�  C���C�  C�C�  C���C�  C�
=C�C�  C�  C�C�  C��C���C�
=C�C�C�  C���C���C���C�  C�
=C�C�  C�  C���C���C�  C�C�  C�  C���C���D � D�D��D�D� D  D� D  D� D�qD��D  D}qD�D� D  D}qD	�D	�D
�D
�D�D}qD  D�D  D}qD�qD��D�D}qD�D��D�D��D  D}qD��D� D  D}qD  D��D�D}qD�qD}qD�qD� D  D��DD� D��D��D�D� D�D� D  D� D  D� D   D � D!  D!� D"  D"��D#�D#� D$  D$� D$�qD%� D&  D&}qD&��D'� D(�D(� D)  D)� D*�D*��D*�qD+}qD,  D,� D-  D-}qD-�qD.� D.�qD/� D0  D0� D1  D1��D2  D2}qD3  D3��D4�D4� D5  D5� D6�D6��D7D7��D8  D8� D9  D9}qD:  D:� D;  D;� D<  D<� D<�qD=� D>  D>}qD>�qD?}qD?�qD@� DA  DA� DA�qDBz�DB�qDC� DD  DD}qDD��DE� DF�DF� DG�DG��DH  DH��DIDI��DJ  DJ}qDK  DK� DK�qDL� DMDM�DNDN��DO  DO� DP�DP� DQ  DQ��DR�DR� DS  DS� DT  DT}qDU�DU� DV  DV��DW  DW� DX�DX��DY�DY��DZ  DZ� DZ�qD[� D\�D\� D]  D]��D^  D^}qD^�qD_� D`  D`��Da�Da��Db�Db}qDb�qDc}qDd  Dd� De  De��DfDf�Dg  Dg� Dh  Dh}qDi�Di��Dj  Dj� Dj�qDk� Dl�Dl��Dm�Dm� Dn  Dn� Dn�qDo� Dp�Dp� Dq  Dq� Dr  Dr}qDs  Ds� Dt�Dt��DuDu��Dv�Dv}qDv��Dwz�Dw�qDx� Dy  Dy� Dz  Dz��D{�D{��D|�D|}qD|�qD}� D}��D~}qD  D}qD�qD�=qD�� D�D��D�AHD��HD�D�  D�=qD�~�D�� D���D�=qD�~�D��HD�  D�AHD��HD�� D�  D�>�D�� D�� D���D�AHD��HD���D�  D�@ D�� D��HD�  D�@ D��HD�� D���D�@ D��HD�� D�HD�B�D�� D���D��qD�=qD�� D��HD�  D�>�D��HD��HD��qD�=qD�� D�� D�  D�@ D�� D��HD�  D�>�D�~�D���D��qD�@ D��HD��HD�HD�@ D�� D�� D�  D�>�D�~�D���D���D�AHD��HD�� D�HD�AHD��HD��HD�HD�AHD��HD�D�  D�>�D�~�D�� D�  D�AHD��HD�� D���D�>�D�~�D���D�  D�@ D��HD��HD�HD�>�D�}qD���D�  D�B�D��HD�� D���D�>�D��HD�� D�  D�B�D���D���D��qD�>�D��HD�D�  D�@ D�� D���D�  D�@ D�~�D���D�  D�AHD���D�� D��D�B�D��HD��HD�  D�AHD���D��HD�HD�AHD�� D�� D�HD�B�D��HD��HD�  D�AHD���D���D��D�@ D�� D�� D�  D�>�D�~�D���D���D�=qD�� D��HD�HD�AHD��HD��HD�HD�AHD�� D�� D�  D�>�D�}qD��qD��qD�=qD�~�D�� D��D�AHD�~�D���D�HD�@ D�� D�� D�  D�>�D�}qD�� D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D���D���D�>�D�~�D�� D�  D�@ D��HD��HD�HD�AHD��HD���D���D�B�D���D�� D���D�@ D��HD��HD��D�@ D�}qD�� D�HD�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D�� D�� D���D�>�D�� D�� D���D�>�D�� D�� D���D�>�D�� D�� D���D�@ D��HD�� D���D�@ D D�� D�  D�>�D�~�D�� D�HD�AHDāHD�� D���D�@ DŁHD�� D�  D�@ Dƀ D�� D���D�>�Dǀ DǾ�D���D�@ DȁHD�� D�  D�@ D�~�Dɾ�D�HD�@ Dʀ D�� D���D�>�DˁHD��HD�  D�AHD̂�D��HD��D�B�D͂�D�� D���D�@ D΀ D�� D�  D�@ D�~�DϾ�D�HD�AHDЀ D�� D�  D�>�D�~�D�� D�HD�AHDҀ DҾ�D�  D�>�D�~�DӾ�D���D�>�D�~�D�� D�  D�AHDՁHD�� D�  D�@ Dր D�� D���D�AHDׁHD�� D�  D�>�D�}qDؽqD�  D�@ Dـ D��HD�  D�AHDځHDھ�D��qD�>�Dۀ D��HD�  D�@ D܁HD��HD�  D�@ D݀ D�� D�HD�AHDށHD�D��D�AHD߀ D߾�D��qD�>�D�� DྸD�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��HD���D�=qD�~�D�� D�  D�@ D� D�� D���D�>�D� D�D��D�>�D� D羸D���D�>�D� D�� D�  D�@ D�HD��HD���D�>�D� D꾸D�  D�AHD� D�� D�HD�>�D�~�D쾸D�HD�AHD�~�D�� D�HD�@ D�~�DD�  D�>�D�HD�D�  D�@ D�� D��HD���D�=qD�~�D�� D���D�@ D� D�� D�HD�>�D�}qD�D�  D�>�D� D��HD�  D�AHD���D��HD�  D�AHD��HD�� D�  D�@ D�� D�� D���D�>�D�~�D�� D��D�AHD�� D���D�  D�,�D�l�>�?.{?k�?�z�?���?��?�@�@��@+�@=p�@J=q@\(�@s33@��\@���@�z�@��R@�=q@�@��R@Ǯ@�z�@�G�@�=q@��@�p�A�A�A  Az�A=qA!G�A'
=A*�HA0��A7
=A<��AAG�AFffAK�AP��AVffAZ=qA^{Ac�
AhQ�Al(�Ap  As�
Ay��A}p�A�  A��\A�z�A�ffA�  A��A�(�A�ffA�  A��A�z�A�ffA�  A��A�z�A�
=A���A�33A�A���A��A�A�  A��\A��A��A�=qA�z�A�\)A�=qA���A�
=A��A���A�\)A��A�(�A�\)A�=qA���A�\)A�\A�A���A�A�{A�G�A�z�A�\)A��A���B   BB
=Bz�B{B�
B	p�B
�RBQ�B�B�B�BffB�
Bp�B
=Bz�BB
=B��B=qB�B ��B"=qB#�
B%�B&ffB'�
B)G�B*�RB,(�B-G�B.�\B0(�B1B3
=B4(�B5B733B8z�B9B;33B<��B>=qB?\)B@��BB=qBC�
BE�BFffBG�BI�BJ�\BL(�BMG�BN�\BP  BQp�BR�HBT(�BU��BV�RBX(�BY��B[
=B\Q�B]��B_
=B`z�Ba�Bc\)Bdz�BeBg\)Bh��Bj=qBk\)Bl��BnffBo�Bp��Br=qBs�Bu�BvffBw�By�Bz�\B|  B}�B~ffB�  B��RB�\)B��B���B�p�B�(�B���B�\)B�  B��RB�p�B�{B���B�G�B�  B��RB�G�B��
B�ffB��B�B�Q�B��HB�p�B�(�B��HB�p�B�{B���B�\)B�{B��HB��B�(�B��HB���B�ffB��B��
B�z�B�33B��B��RB�p�B�{B���B��B�Q�B��B�B�z�B�33B�  B���B���B�Q�B���B�B�z�B�\)B�{B��RB�\)B�(�B���B��B�ffB�
=B�B�z�B�33B�  B���B��B�(�B��HB��B��\B�G�B�  B��RB�p�B�(�B���B��B�z�B�G�B�  B���B�p�B�Q�B��B�B\B�G�B�  B���Bř�B�ffB��B��
Bȏ\B�G�B�{B���B˅B�(�B���BͅB�(�B��HB�p�B�{BЏ\B���B�p�B��
B�Q�BҸRB��B�\)Bә�B��B�(�Bԏ\B��HB�33B�p�BծB�  B�=qB֏\B��HB�33B�p�B׮B�  B�=qBأ�B���B�33B�p�BٮB�  B�Q�BڸRB�
=B�G�Bۙ�B��
B�{B�ffBܸRB�
=B�\)Bݙ�B��
B�{B�ffB޸RB��B�\)Bߙ�B��B�(�B�ffB���B��B�p�B�B�{B�Q�B��B��HB�33B�B��
B�(�B�z�B���B���B�G�B�B��
B�(�B�\B��HB��B�\)B�B�  B�ffB��B���B�G�B�B��
B�=qB��B���B�\)B�B�  B�Q�B��B���B�G�B��B�{B�z�B��HB�G�B�B��B�=qB��\B���B�p�B��
B�(�B�\B�
=B�\)B�B�(�B�\B���B�\)B�B�{B�z�B���B�G�B��B�(�B��\B���B�\)B��B�Q�B��RB�33B���B�  B�ffB���B�33B��B�{B�ffB���B�33B���C 
=C =qC p�C ��C �HC{CG�C�C�RC��C(�C\)C��C��C��C33CffC��C�
C
=CG�Cz�C�RC�C(�C\)C��C�HC{CG�C�\CC  C=qCz�C�RC��C33Cp�C�C�C	(�C	ffC	��C	�HC
{C
\)C
��C
�
C{CQ�C�\C��C
=CQ�C�\C��C{CQ�C��C�
C{C\)C��C�
C�C\)C��C�
C{CQ�C��C�
C{C\)C��C�
C{CQ�C��C��C{C\)C��C�
C�C\)C��C�C(�Cp�C�RC��C=qC�CC
=CQ�C��C�HC�C\)C��C�C(�Cp�C��C�C33Cp�C�RC  C=qCz�CC
=CQ�C�\C�
C{C\)C��C�C=qCz�CC
=CQ�C��C�C (�C z�C C!  C!G�C!�\C!��C"{C"\)C"��C"�C#(�C#p�C#�C#��C$=qC$z�C$C%
=C%Q�C%��C%�
C&(�C&ffC&�RC'  C'G�C'�\C'�
C(�C(p�C(�RC)  C)G�C)��C)�
C*�C*\)C*�C*��C+=qC+�\C+�
C,�C,p�C,�RC-  C-G�C-�\C-��C.{C.Q�C.��C.�C/33C/�C/��C0{C0\)C0��C0�
C1�C1ffC1�C1��C2=qC2�\C2�
C3�C3ffC3�C3�C433C4p�C4C5{C5\)C5��C5�HC6(�C6ffC6�RC6��C7=qC7�\C7�
C8�C8ffC8�C8�C933C9p�C9�RC:  C:G�C:�\C:�
C;{C;Q�C;�\C;�
C<{C<\)C<��C<�HC=33C=z�C=�RC>  C>G�C>�\C>��C?
=C?G�C?�\C?��C@{C@\)C@��C@�CA=qCAz�CACB
=CBG�CB�CB��CC(�CCp�CCCD
=CDQ�CD�\CD�
CE�CEp�CECF{CFffCF�CF��CG33CG�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�r�A�x�A�v�A�x�A�x�A�v�A�v�A�r�A�t�A�x�A�x�A�v�A�x�A�z�A�|�A�z�A�ffA�G�A�5?A�$�A��A�bA� �A��A���A��A��A��A��yA��yA��yA��yA��yA��`A��/A��A��
A���A���A�ƨAѮA��A���A� �AÕ�A�ƨA���A���A��#A�I�A�A�C�A�ȴA�9XA���A��A�G�A�VA�r�A�5?A��A�VA�5?A�^5A��A�jA��!A��`A���A��A�33A��yA��A���A��A�33A�ȴA�=qA���A�I�A��`A�jA���A�9XA��A��FA���A��A�{A~��Az��AxQ�Au�TAsx�An�/Af�jAb�Aa��A_�A\�RAY�#AV��AQ��AOC�ALĜAH�HAH(�AH(�AG�^AG�hAG|�AG&�AEK�AE�mAF�\AGt�AGVAF1AD�+AC�hAA?}A@v�A@jA@bNA@^5A@ȴA@��A@�jA@jA@9XA?�wA>�uA=/A<��A<ZA;�FA;C�A;�A:ZA9|�A9p�A8��A7��A7�^A7��A7p�A6�yA6r�A6Q�A6�A5��A5l�A5+A4�DA4 �A3�FA2�yA2�uA2$�A1�FA1+A0��A0A�A0(�A/�#A/`BA.��A.�A.^5A-�hA-�A,��A,(�A+K�A*�/A*�DA*1A)��A)S�A(��A(  A'�-A'O�A&�jA&~�A&JA%K�A$�A$��A$~�A#��A#�FA#�A#A"v�A!�FA!�7A!G�A �A z�A A�A�TA~�A�-A��AAl�A��A��A=qA��AbNA�mA��A�#AhsA1'A^5AZA=qA�A�/AZA�A|�A%AĜA-A�An�A��AQ�A��A/A%A��AA�AƨAȴA��A�A1'A��A��A��A7LA
�A
^5A	�TA	XA	%A�`A��AZA$�AJA�A��A�A7LA��A�A~�AZA9XA�A�FA`BA��A5?A�;A�A�Al�A?}A��A��A�A�;A�-AdZA �jA �\A ff@�|�@��@��@�?}@��u@��@�C�@��R@�ff@�G�@���@�9X@���@�$�@�@�p�@�r�@�|�@���@���@�C�@�
=@�~�@��@�x�@��@� �@땁@���@�ff@�5?@�hs@�D@�l�@��@��@�I�@�dZ@�+@�~�@��T@�&�@�j@ߍP@�\)@�;d@���@��T@��@�t�@ڏ\@��@�X@ج@�1'@��
@�t�@֗�@ՙ�@ԓu@�  @Ӿw@ӝ�@�t�@��@�n�@�@��@�Ĝ@�r�@�1'@�z�@�j@�9X@��@�ƨ@ύP@�;d@ΰ!@�$�@���@�p�@�%@�l�@�~�@�E�@ə�@��@ȼj@�I�@��
@�K�@���@ư!@�{@Ų-@�?}@�Ĝ@�b@�l�@+@��7@��/@�j@�1'@���@�"�@���@�$�@�{@���@��`@��@��@���@�\)@�
=@�n�@��@�@�`B@��@���@�I�@�1'@��@���@�K�@�o@��y@���@��@��@��`@�Z@��@�K�@�;d@��@���@�~�@�5?@���@��^@�x�@��@���@�1@��@�t�@�l�@��@��@���@�=q@�J@�@��@�r�@�9X@�1@��w@��@�"�@���@���@���@��+@�{@�@��7@�x�@�O�@�%@�j@�(�@�1@��F@�K�@���@��\@�5?@��#@��h@�X@��u@��@�1@��@��
@���@�C�@��R@�M�@�J@�@��h@�?}@��@��@���@���@���@��@��@�1'@���@�S�@��+@���@���@�x�@�&�@��`@���@��@�bN@� �@�ƨ@���@��@��@�|�@�"�@��H@�^5@���@���@�V@�r�@�Z@�b@���@�"�@�ȴ@���@�n�@��@���@��@�/@��9@�r�@�A�@��@��m@��w@��P@�33@��@��y@��!@���@��+@�ff@�-@��T@�x�@�`B@��@���@��u@�Z@�9X@�b@�b@��@��w@���@�S�@��@��\@�V@�-@��@���@�p�@��@�r�@�  @�ƨ@��F@��@�o@���@�~�@�=q@��@���@��^@�X@���@���@�z�@�bN@�A�@�b@�  @���@�|�@�o@��@��R@��\@�ff@�M�@�J@�@���@�X@���@���@�z�@�A�@���@��w@���@��@�t�@�\)@�+@��y@���@���@��+@�M�@��@��^@��h@�O�@�%@��/@��@�Z@�1'@��@�  @�@|�@l�@+@~��@~ff@}��@}O�@|��@|(�@{�F@{dZ@z�H@z^5@y�@yx�@y&�@x��@xr�@xQ�@x �@x  @wl�@v�R@vV@v@u�@u�h@u�@t�/@t�j@tj@t9X@s��@st�@so@r��@r�\@r~�@r^5@rJ@q�^@q��@qx�@q%@p�u@p1'@o��@o��@oK�@n��@m�-@m/@mV@l�@l�@l�@kS�@j��@j�!@j~�@j^5@j=q@i��@hĜ@h�@hA�@g;d@f�y@f��@f$�@e@eV@dI�@c�
@c��@cdZ@b��@bJ@a�^@ax�@aG�@a&�@`��@`��@`A�@` �@`  @_��@_�P@^V@]@\�/@\z�@\�@[�
@[@Zn�@Z=q@Z�@Y�@Y��@Y��@YX@Y%@XbN@W��@W\)@V�@U�@Up�@U`B@U`B@UO�@U�@T�@T��@T�@S�
@St�@R�@R�@R�!@Q�@QG�@P��@P��@PĜ@Pr�@P  @O�w@Ol�@N�@N5?@N{@M�@M��@M�-@M�@M�@MV@L�@L�@Lj@L(�@K�
@K��@Kt�@J�H@J�\@J~�@JM�@I�#@IG�@HĜ@Hr�@H1'@Hb@G�;@Gl�@Fȴ@Fff@FE�@F{@E��@Ep�@E`B@D�@D�D@D�@C��@Co@B�\@BJ@A�7@A&�@@��@@��@?�P@?
=@>��@>ff@>$�@=��@<��@<��@<(�@;t�@;o@:��@:�!@:�\@:n�@:n�@:M�@:-@9�#@9X@8�9@8Q�@8b@7�@7K�@6�@6E�@5�-@5p�@5V@4��@4Z@3��@3��@3�@3S�@333@3"�@3"�@3@2��@2~�@2J@1��@1��@1X@0A�@0b@/+@.v�@.E�@.@-�@-�h@-/@,�j@,I�@,�D@+�
@+ƨ@+o@*�\@*J@)��@)x�@)�@(�u@(1'@'��@'��@'��@'K�@'
=@&�@&��@&ff@&$�@&@%�@%�-@%?}@$�@$�@$�D@$�@#�F@#t�@#S�@#C�@#"�@"��@"^5@!�#@!x�@!�@ �`@ r�@ A�@ 1'@  �@  �@  �@ b@ b@   @�w@��@|�@�@
=@�@ȴ@ȴ@��@E�@{@��@?}@/@�@V@�/@�j@z�@1@�F@t�@S�@33@�@��@=q@�@�#@��@�7@x�@hs@X@7L@&�@��@��@�9@�u@bN@ �@�;@�w@��@|�@|�@K�@
=@�y@��@v�@$�@�@�-@p�@��@��@�D@I�@9X@9X@1@ƨ@�F@�F@��@��@dZ@o@�H@�!@��@�\@n�@-@�@�#@��@��@X@%@��@��@Ĝ@�9@Q�@ �@b@��@�@|�@l�@�@�y@�y@�@�R@�+@V@E�@E�@E�@E�A�p�A�r�A�r�A�p�A�r�A�x�A�z�A�v�A�z�A�z�A�v�A�t�A�t�A�z�A�z�A�v�A�v�A�z�A�z�A�x�A�t�A�t�A�v�A�x�A�t�A�n�A�t�A�x�A�t�A�p�A�r�A�x�A�z�A�x�A�v�A�v�A�z�A�v�A�v�A�t�A�x�A�|�A�x�A�v�A�v�A�z�A�~�A�x�A�t�A�|�A�~�A�x�A�z�A�~�A�|�A�v�A�v�A�|�A�~�A�x�A�v�AҁAҁA�~�A�|�A�|�AҁA�|�A�v�A�|�A�l�A�x�A�p�A�hsA�hsA�`BA�\)A�O�A�K�A�K�A�O�A�C�A�?}A�G�A�I�A�A�A�(�A�&�A�1'A�33A�1'A�(�A� �A��A�"�A�-A�"�A��A��A��A��A��A��A�{A�bA���A��A�$�A�&�A�"�A��A� �A� �A�&�A��A�A���A���A���A���A��A���A���A���A��A��A���A���A��A��A��A��A��A��A��A��A��A��A��mA��yA��A��A��mA��mA��A��A��yA��mA��yA��A��A��mA��mA��yA��A��A��yA��mA��`A��yA��A��A��yA��mA��mA��A��A��yA��`A��mA��`A��mA��`A��TA��HA��;A��;A��/A��A��A��#A��#A��A��
A��
A��A��
A���A���A��A��
A���A���A��
A��A��
A���A���A���A��
A���A���A���A���A���A�ȴA�A���A�A�AѾwAѸRAѸRAѺ^AѴ9AёhA�^5A�I�A�-A��A��TA�-Aϛ�A�r�A�z�A�(�A�33A��A�ĜAŶFA�{A�ȴA�|�A�(�A��TAøRAã�A�l�A�9XA�"�A�1A��A���A�~�A�=qA�9XA�&�A��uA��A��mA���A���A��A��DA�ZA�1'A�A��/A��A�A���A��A�hsA�Q�A�;dA��A��`A�v�A�JA���A�p�A�ƨA��A�I�A�E�A�I�A�jA���A��yA��`A��#A���A�ffA�O�A�E�A�=qA�-A�VA��TA��7A��A���A��RA��9A��!A��A��A���A���A���A��+A�C�A���A��mA��FA�ZA��A��A���A�XA�$�A�JA���A��yA��TA��
A��wA���A��A�O�A�&�A�1A��mA���A���A���A��A�p�A�S�A�K�A�?}A�5?A�(�A� �A��A��A��jA��RA���A�ĜA�ƨA���A��HA���A�VA�{A�{A�oA�VA�
=A�A�  A���A���A��A��/A��!A�dZA�9XA�&�A�
=A��A���A��-A��A���A���A�r�A�l�A�G�A�33A�/A�&�A��A��A���A��A��A��`A��A�ƨA��^A��A��A���A���A�|�A�?}A��A�ĜA���A��+A�bNA�9XA�  A�hsA�
=A���A�v�A�;dA��A���A��RA���A��hA�M�A�{A���A��A��A�ƨA��A��+A�hsA�\)A�7LA�"�A�bA�1A���A���A��A��A�M�A��;A��DA�bNA�1'A�A��yA��HA��A���A�dZA�(�A�
=A��A��TA��A��9A�|�A�ffA�O�A�7LA�-A� �A��A�VA��A��/A��jA���A��+A�`BA�;dA�VA���A�A�A��HA���A�l�A�C�A� �A���A���A��uA�M�A�VA��^A�9XA�r�A�K�A�A�A�7LA�+A�{A��A��
A���A�l�A�XA�33A�{A���A��mA���A���A�t�A�=qA��;A�v�A��wA�A���A�&�A��A���A�r�A�M�A�7LA�1'A�-A�(�A�"�A�{A��A��TA��A���A�ƨA��jA��FA��A���A��PA�z�A�hsA�O�A�&�A���A��9A��7A��A��FA���A��;A��\A��DA��7A��A�~�A�t�A�bNA�VA�7LA���A���A��hA��FA�$�A��PA�9XA�  A��A��9A���A��A�jA�\)A�O�A�?}A� �A�bA���A��`A���A��A�G�A�JA��/A��hA�ffA�bNA�M�A�+A�1A��9A�$�A�^5A��uA���A���A�~�A���A��`A��7A�;dA��wA�jA�oA��^A�I�A�{A��`A�ƨA��A�XA��yA���A�~�A�?}A���A���A�p�A�E�A���A��A��uA�XA�&�A��A��^A���A�dZA�5?A�A~��A~�A~ �A}O�A|��A|�+A|�Az��Az9XAy�
AyK�Ay%AxĜAx�AxbNAxQ�Ax�Aw�FAw��AwXAv�\Au�wAuXAu
=At�HAt��AtĜAt�9AtVAs��Ar�RAq�Aq?}Apv�Ao��Ao�#Ao��An�9AmhsAlI�Ak�Ak/Ai��Ae�wAc��Ac�hAc|�Acp�AcO�Ac+AcVAb�Ab��Ab�\Abr�AbA�Ab{Aa�Aa��AaO�A`��A`~�A_A_t�A_&�A_oA_
=A^��A^�A]�TA]�PA]\)A]XA]&�A\$�A[hsAZ��AZn�AZE�AZbAY��AYƨAY|�AY�AXĜAX�\AX5?AW�
AW&�AUx�ATr�AS��AR��AR�AQ��AQp�AQK�AP��AP��AP9XAO�-AO`BAN��AN��AN�jAN�DAN �AM��AMt�AL��ALAKt�AJM�AI\)AI�AH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              A�r�A�x�A�v�A�x�A�x�A�v�A�v�A�r�A�t�A�x�A�x�A�v�A�x�A�z�A�|�A�z�A�ffA�G�A�5?A�$�A��A�bA� �A��A���A��A��A��A��yA��yA��yA��yA��yA��`A��/A��A��
A���A���A�ƨAѮA��A���A� �AÕ�A�ƨA���A���A��#A�I�A�A�C�A�ȴA�9XA���A��A�G�A�VA�r�A�5?A��A�VA�5?A�^5A��A�jA��!A��`A���A��A�33A��yA��A���A��A�33A�ȴA�=qA���A�I�A��`A�jA���A�9XA��A��FA���A��A�{A~��Az��AxQ�Au�TAsx�An�/Af�jAb�Aa��A_�A\�RAY�#AV��AQ��AOC�ALĜAH�HAH(�AH(�AG�^AG�hAG|�AG&�AEK�AE�mAF�\AGt�AGVAF1AD�+AC�hAA?}A@v�A@jA@bNA@^5A@ȴA@��A@�jA@jA@9XA?�wA>�uA=/A<��A<ZA;�FA;C�A;�A:ZA9|�A9p�A8��A7��A7�^A7��A7p�A6�yA6r�A6Q�A6�A5��A5l�A5+A4�DA4 �A3�FA2�yA2�uA2$�A1�FA1+A0��A0A�A0(�A/�#A/`BA.��A.�A.^5A-�hA-�A,��A,(�A+K�A*�/A*�DA*1A)��A)S�A(��A(  A'�-A'O�A&�jA&~�A&JA%K�A$�A$��A$~�A#��A#�FA#�A#A"v�A!�FA!�7A!G�A �A z�A A�A�TA~�A�-A��AAl�A��A��A=qA��AbNA�mA��A�#AhsA1'A^5AZA=qA�A�/AZA�A|�A%AĜA-A�An�A��AQ�A��A/A%A��AA�AƨAȴA��A�A1'A��A��A��A7LA
�A
^5A	�TA	XA	%A�`A��AZA$�AJA�A��A�A7LA��A�A~�AZA9XA�A�FA`BA��A5?A�;A�A�Al�A?}A��A��A�A�;A�-AdZA �jA �\A ff@�|�@��@��@�?}@��u@��@�C�@��R@�ff@�G�@���@�9X@���@�$�@�@�p�@�r�@�|�@���@���@�C�@�
=@�~�@��@�x�@��@� �@땁@���@�ff@�5?@�hs@�D@�l�@��@��@�I�@�dZ@�+@�~�@��T@�&�@�j@ߍP@�\)@�;d@���@��T@��@�t�@ڏ\@��@�X@ج@�1'@��
@�t�@֗�@ՙ�@ԓu@�  @Ӿw@ӝ�@�t�@��@�n�@�@��@�Ĝ@�r�@�1'@�z�@�j@�9X@��@�ƨ@ύP@�;d@ΰ!@�$�@���@�p�@�%@�l�@�~�@�E�@ə�@��@ȼj@�I�@��
@�K�@���@ư!@�{@Ų-@�?}@�Ĝ@�b@�l�@+@��7@��/@�j@�1'@���@�"�@���@�$�@�{@���@��`@��@��@���@�\)@�
=@�n�@��@�@�`B@��@���@�I�@�1'@��@���@�K�@�o@��y@���@��@��@��`@�Z@��@�K�@�;d@��@���@�~�@�5?@���@��^@�x�@��@���@�1@��@�t�@�l�@��@��@���@�=q@�J@�@��@�r�@�9X@�1@��w@��@�"�@���@���@���@��+@�{@�@��7@�x�@�O�@�%@�j@�(�@�1@��F@�K�@���@��\@�5?@��#@��h@�X@��u@��@�1@��@��
@���@�C�@��R@�M�@�J@�@��h@�?}@��@��@���@���@���@��@��@�1'@���@�S�@��+@���@���@�x�@�&�@��`@���@��@�bN@� �@�ƨ@���@��@��@�|�@�"�@��H@�^5@���@���@�V@�r�@�Z@�b@���@�"�@�ȴ@���@�n�@��@���@��@�/@��9@�r�@�A�@��@��m@��w@��P@�33@��@��y@��!@���@��+@�ff@�-@��T@�x�@�`B@��@���@��u@�Z@�9X@�b@�b@��@��w@���@�S�@��@��\@�V@�-@��@���@�p�@��@�r�@�  @�ƨ@��F@��@�o@���@�~�@�=q@��@���@��^@�X@���@���@�z�@�bN@�A�@�b@�  @���@�|�@�o@��@��R@��\@�ff@�M�@�J@�@���@�X@���@���@�z�@�A�@���@��w@���@��@�t�@�\)@�+@��y@���@���@��+@�M�@��@��^@��h@�O�@�%@��/@��@�Z@�1'@��@�  @�@|�@l�@+@~��@~ff@}��@}O�@|��@|(�@{�F@{dZ@z�H@z^5@y�@yx�@y&�@x��@xr�@xQ�@x �@x  @wl�@v�R@vV@v@u�@u�h@u�@t�/@t�j@tj@t9X@s��@st�@so@r��@r�\@r~�@r^5@rJ@q�^@q��@qx�@q%@p�u@p1'@o��@o��@oK�@n��@m�-@m/@mV@l�@l�@l�@kS�@j��@j�!@j~�@j^5@j=q@i��@hĜ@h�@hA�@g;d@f�y@f��@f$�@e@eV@dI�@c�
@c��@cdZ@b��@bJ@a�^@ax�@aG�@a&�@`��@`��@`A�@` �@`  @_��@_�P@^V@]@\�/@\z�@\�@[�
@[@Zn�@Z=q@Z�@Y�@Y��@Y��@YX@Y%@XbN@W��@W\)@V�@U�@Up�@U`B@U`B@UO�@U�@T�@T��@T�@S�
@St�@R�@R�@R�!@Q�@QG�@P��@P��@PĜ@Pr�@P  @O�w@Ol�@N�@N5?@N{@M�@M��@M�-@M�@M�@MV@L�@L�@Lj@L(�@K�
@K��@Kt�@J�H@J�\@J~�@JM�@I�#@IG�@HĜ@Hr�@H1'@Hb@G�;@Gl�@Fȴ@Fff@FE�@F{@E��@Ep�@E`B@D�@D�D@D�@C��@Co@B�\@BJ@A�7@A&�@@��@@��@?�P@?
=@>��@>ff@>$�@=��@<��@<��@<(�@;t�@;o@:��@:�!@:�\@:n�@:n�@:M�@:-@9�#@9X@8�9@8Q�@8b@7�@7K�@6�@6E�@5�-@5p�@5V@4��@4Z@3��@3��@3�@3S�@333@3"�@3"�@3@2��@2~�@2J@1��@1��@1X@0A�@0b@/+@.v�@.E�@.@-�@-�h@-/@,�j@,I�@,�D@+�
@+ƨ@+o@*�\@*J@)��@)x�@)�@(�u@(1'@'��@'��@'��@'K�@'
=@&�@&��@&ff@&$�@&@%�@%�-@%?}@$�@$�@$�D@$�@#�F@#t�@#S�@#C�@#"�@"��@"^5@!�#@!x�@!�@ �`@ r�@ A�@ 1'@  �@  �@  �@ b@ b@   @�w@��@|�@�@
=@�@ȴ@ȴ@��@E�@{@��@?}@/@�@V@�/@�j@z�@1@�F@t�@S�@33@�@��@=q@�@�#@��@�7@x�@hs@X@7L@&�@��@��@�9@�u@bN@ �@�;@�w@��@|�@|�@K�@
=@�y@��@v�@$�@�@�-@p�@��@��@�D@I�@9X@9X@1@ƨ@�F@�F@��@��@dZ@o@�H@�!@��@�\@n�@-@�@�#@��@��@X@%@��@��@Ĝ@�9@Q�@ �@b@��@�@|�@l�@�@�y@�y@�@�R@�+@V@E�@E�@E�@E�A�p�A�r�A�r�A�p�A�r�A�x�A�z�A�v�A�z�A�z�A�v�A�t�A�t�A�z�A�z�A�v�A�v�A�z�A�z�A�x�A�t�A�t�A�v�A�x�A�t�A�n�A�t�A�x�A�t�A�p�A�r�A�x�A�z�A�x�A�v�A�v�A�z�A�v�A�v�A�t�A�x�A�|�A�x�A�v�A�v�A�z�A�~�A�x�A�t�A�|�A�~�A�x�A�z�A�~�A�|�A�v�A�v�A�|�A�~�A�x�A�v�AҁAҁA�~�A�|�A�|�AҁA�|�A�v�A�|�A�l�A�x�A�p�A�hsA�hsA�`BA�\)A�O�A�K�A�K�A�O�A�C�A�?}A�G�A�I�A�A�A�(�A�&�A�1'A�33A�1'A�(�A� �A��A�"�A�-A�"�A��A��A��A��A��A��A�{A�bA���A��A�$�A�&�A�"�A��A� �A� �A�&�A��A�A���A���A���A���A��A���A���A���A��A��A���A���A��A��A��A��A��A��A��A��A��A��A��mA��yA��A��A��mA��mA��A��A��yA��mA��yA��A��A��mA��mA��yA��A��A��yA��mA��`A��yA��A��A��yA��mA��mA��A��A��yA��`A��mA��`A��mA��`A��TA��HA��;A��;A��/A��A��A��#A��#A��A��
A��
A��A��
A���A���A��A��
A���A���A��
A��A��
A���A���A���A��
A���A���A���A���A���A�ȴA�A���A�A�AѾwAѸRAѸRAѺ^AѴ9AёhA�^5A�I�A�-A��A��TA�-Aϛ�A�r�A�z�A�(�A�33A��A�ĜAŶFA�{A�ȴA�|�A�(�A��TAøRAã�A�l�A�9XA�"�A�1A��A���A�~�A�=qA�9XA�&�A��uA��A��mA���A���A��A��DA�ZA�1'A�A��/A��A�A���A��A�hsA�Q�A�;dA��A��`A�v�A�JA���A�p�A�ƨA��A�I�A�E�A�I�A�jA���A��yA��`A��#A���A�ffA�O�A�E�A�=qA�-A�VA��TA��7A��A���A��RA��9A��!A��A��A���A���A���A��+A�C�A���A��mA��FA�ZA��A��A���A�XA�$�A�JA���A��yA��TA��
A��wA���A��A�O�A�&�A�1A��mA���A���A���A��A�p�A�S�A�K�A�?}A�5?A�(�A� �A��A��A��jA��RA���A�ĜA�ƨA���A��HA���A�VA�{A�{A�oA�VA�
=A�A�  A���A���A��A��/A��!A�dZA�9XA�&�A�
=A��A���A��-A��A���A���A�r�A�l�A�G�A�33A�/A�&�A��A��A���A��A��A��`A��A�ƨA��^A��A��A���A���A�|�A�?}A��A�ĜA���A��+A�bNA�9XA�  A�hsA�
=A���A�v�A�;dA��A���A��RA���A��hA�M�A�{A���A��A��A�ƨA��A��+A�hsA�\)A�7LA�"�A�bA�1A���A���A��A��A�M�A��;A��DA�bNA�1'A�A��yA��HA��A���A�dZA�(�A�
=A��A��TA��A��9A�|�A�ffA�O�A�7LA�-A� �A��A�VA��A��/A��jA���A��+A�`BA�;dA�VA���A�A�A��HA���A�l�A�C�A� �A���A���A��uA�M�A�VA��^A�9XA�r�A�K�A�A�A�7LA�+A�{A��A��
A���A�l�A�XA�33A�{A���A��mA���A���A�t�A�=qA��;A�v�A��wA�A���A�&�A��A���A�r�A�M�A�7LA�1'A�-A�(�A�"�A�{A��A��TA��A���A�ƨA��jA��FA��A���A��PA�z�A�hsA�O�A�&�A���A��9A��7A��A��FA���A��;A��\A��DA��7A��A�~�A�t�A�bNA�VA�7LA���A���A��hA��FA�$�A��PA�9XA�  A��A��9A���A��A�jA�\)A�O�A�?}A� �A�bA���A��`A���A��A�G�A�JA��/A��hA�ffA�bNA�M�A�+A�1A��9A�$�A�^5A��uA���A���A�~�A���A��`A��7A�;dA��wA�jA�oA��^A�I�A�{A��`A�ƨA��A�XA��yA���A�~�A�?}A���A���A�p�A�E�A���A��A��uA�XA�&�A��A��^A���A�dZA�5?A�A~��A~�A~ �A}O�A|��A|�+A|�Az��Az9XAy�
AyK�Ay%AxĜAx�AxbNAxQ�Ax�Aw�FAw��AwXAv�\Au�wAuXAu
=At�HAt��AtĜAt�9AtVAs��Ar�RAq�Aq?}Apv�Ao��Ao�#Ao��An�9AmhsAlI�Ak�Ak/Ai��Ae�wAc��Ac�hAc|�Acp�AcO�Ac+AcVAb�Ab��Ab�\Abr�AbA�Ab{Aa�Aa��AaO�A`��A`~�A_A_t�A_&�A_oA_
=A^��A^�A]�TA]�PA]\)A]XA]&�A\$�A[hsAZ��AZn�AZE�AZbAY��AYƨAY|�AY�AXĜAX�\AX5?AW�
AW&�AUx�ATr�AS��AR��AR�AQ��AQp�AQK�AP��AP��AP9XAO�-AO`BAN��AN��AN�jAN�DAN �AM��AMt�AL��ALAKt�AJM�AI\)AI�AH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
4�B
49B
49B
49B
49B
49B
4B
4B
3�B
49B
4B
49B
49B
4B
4B
3�B
4�B
3�B
3�B
33B
3�B
3hB
2-B
4B
33B
2�B
2�B
2�B
2�B
2�B
2�B
2aB
2-B
1�B
1�B
0�B
0UB
/OB
-�B
+6B
'B
�B
%FB	�`B	��B	��B	�rB	��B
{B
B
7LB
�EBC�B�{B��B�B�CB��B�-B�9BBLdBEBAUBM6B_Bb�BJ�B)�BB�>B��B�B��BrGBK�BB�B:�B(XBB
�`B
��B
�-B
��B
}�B
&�B	��B	�B	�B	�?B	�RB	��B	�eB	�uB	�rB	�DB	tB	s�B	o�B	ncB	l�B	y�B	�SB	�7B	��B	��B	�$B	�RB	�?B	�WB	��B	��B
B
49B
K�B
v�B
�XB
�CB
�B
��B
��B
�?B
�0B
�B
��B
�B
�B;B
�"B iBB�BAB
�B
�B
��B
�2B
�B
��B
�B
�|B
��B
�oB
��B
��B
��B
�B
�fB
��B
��B
�	B
��B
��B
��B
�fB
��B
��B
��B
��B
�MB
�MB
�B
�;B
� B
�B
�/B
��B
�B
�B
��B
�,B
��B
�B
ܒB
�B
��B
��B
֡B
�[B
ӏB
бB
�jB
͟B
ɺB
��B
�XB
�B
�EB
�gB
ŢB
B
�UB
��B
�B
�jB
��B
�zB
��B
�3B
��B
�qB
��B
�\B
�_B
�bB
��B
��B
��B
��B
��B
��B
|PB
x8B
{�B
{B
{JB
qAB
x�B
y	B
x�B
|�B
x�B
wfB
u�B
t�B
p�B
oiB
n�B
ffB
^jB
f�B
iyB
c�B
_B
aHB
_;B
Z�B
Z�B
V�B
VB
W
B
YB
XB
XB
W
B
V�B
TaB
T,B
R B
PB
M6B
L�B
L�B
NB
L�B
MB
L�B
K�B
M6B
L0B
L�B
K�B
J�B
JXB
I�B
I�B
HB
H�B
GzB
F?B
C�B
C-B
B�B
A�B
A�B
A�B
A�B
@OB
>BB
>BB
>BB
<jB
9�B
:�B
8�B
9�B
6B
1[B
1�B
.}B
.IB
-wB
-wB
-CB
*�B
+B
,qB
*0B
(XB
(�B
(�B
%B
'�B
"4B
!bB
�B
 'B
!B
�B
�B
!bB
 'B
 'B
�B
�B
B
B
�B
�B
�B
�B
 B
B
B
�B
�B
B
�B
B
�B
FB
oB
bB
�B
DB
�B
xB
B

�B

�B

�B
B
B
�B
~B
B
�B
xB
B

�B

=B
	lB
	lB
	lB
	�B
\B
:B
B
�B
�B
�B
uB
B
�B
uB
uB
@B
@B
�B
:B
B
hB
hB
�B
hB
�B
hB
�B
�B
4B
hB
 B
hB
�B
�B
�B
hB
4B
 B
�B
hB
oB
B
B
�B
�B
@B
@B
�B
@B
uB
�B
�B
@B
oB
�B
@B
�B
�B
B
uB
�B
uB
�B
�B
B
�B
B
�B
�B
B
�B
�B
�B
�B
YB
YB
$B
�B
�B
+B
1B
1B
1B
eB
qB
�B
7B
�B
B
�B
�B
�B
B
CB
xB
OB
 \B
 'B
 \B
 �B
 �B
 �B
!-B
!-B
!bB
!�B
"�B
#:B
#B
#B
#nB
$tB
$�B
$B
$�B
$�B
%FB
%B
&B
%�B
%�B
%�B
%�B
&�B
'B
(XB
'�B
(XB
(XB
(�B
)�B
)�B
)�B
)�B
)�B
*eB
*0B
)�B
*�B
*�B
*eB
,qB
+�B
,B
,B
,�B
,qB
,qB
,�B
,�B
-wB
-wB
-�B
-wB
-wB
-wB
-wB
-CB
.}B
.B
-�B
/�B
.�B
.}B
/B
/OB
/�B
0UB
/�B
/�B
0�B
0!B
0UB
0�B
0�B
1'B
1�B
1�B
1�B
1�B
1�B
2-B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
33B
3�B
3�B
4nB
49B
4nB
4nB
49B
4�B
4�B
4�B
5?B
5�B
5�B
6B
5�B
6B
6zB
6FB
7B
8�B
8�B
8�B
8�B
8�B
9�B
:^B
:^B
:�B
:�B
:�B
:�B
<B
<B
<6B
<jB
<jB
<�B
<�B
<jB
<�B
=qB
>BB
>BB
>B
>�B
>BB
>BB
>�B
?HB
?HB
?�B
@�B
@OB
@�B
@�B
AUB
AUB
A�B
A�B
A�B
A�B
B'B
B�B
B�B
C-B
CaB
C�B
D�B
D3B
D�B
EB
EB
EmB
EmB
EmB
E�B
E�B
FB
F�B
F�B
F�B
GB
F�B
G�B
HB
H�B
IB
IB
IRB
IRB
I�B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L0B
L0B
LdB
L0B
L�B
L�B
M6B
MB
MjB
M6B
M6B
M�B
NB
N<B
NpB
N<B
N<B
N�B
NpB
NpB
NpB
N�B
N�B
OBB
OvB
OBB
OBB
P}B
P�B
P�B
P�B
P�B
P�B
Q�B
R B
RTB
R B
R B
R B
Q�B
R�B
S[B
R�B
S&B
T,B
S�B
S�B
T,B
TaB
T�B
U�B
U�B
U�B
UgB
VmB
VmB
V�B
W
B
W
B
W
B
W
B
W�B
W�B
WsB
W�B
WsB
WsB
YKB
X�B
Y�B
YB
Y�B
YB
Z�B
Z�B
ZB
Z�B
ZQB
ZQB
Z�B
[#B
[WB
[�B
[�B
[�B
\�B
\�B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]dB
]�B
]dB
^5B
_B
^�B
_B
_B
_�B
_pB
_;B
_;B
`B
`BB
`�B
`vB
`vB
`vB
`vB
`�B
`�B
`�B
aB
aB
aHB
aHB
a|B
a�B
a�B
bNB
bB
bB
bB
bNB
b�B
b�B
bB
b�B
cTB
c�B
d&B
d�B
d�B
d�B
d�B
e`B
e`B
e`B
e�B
f2B
ffB
f�B
f�B
g8B
g8B
gmB
gmB
gmB
gmB
h�B
iB
iB
iyB
iDB
i�B
jB
j�B
j�B
kB
k�B
l"B
l"B
lWB
lWB
lWB
lWB
l�B
l�B
m]B
m�B
m�B
m�B
n�B
n�B
ncB
n/B
n/B
n�B
o�B
poB
qB
qB
qAB
qAB
qAB
qB
qAB
qAB
qB
qAB
qvB
q�B
qvB
qAB
qB
qvB
qvB
qB
p�B
q�B
rGB
s�B
tTB
s�B
s�B
s�B
u%B
t�B
t�B
u�B
t�B
t�B
tTB
tB
tTB
t�B
t�B
t�B
u�B
u�B
u�B
v`B
v�B
w2B
wfB
w�B
w�B
w�B
xB
xlB
y	B
y>B
yrB
zDB
zxB
z�B
z�B
z�B
z�B
{B
{�B
{�B
{�B
|B
|PB
|�B
|�B
|�B
}"B
}"B
|�B
}"B
|�B
}"B
}VB
}VB
}�B
~(B
}�B
~(B
~(B
~(B
~�B
~�B
~�B
�B
�4B
�4B
�4B
�iB
�iB
��B
�B
��B
�AB
�AB
�uB
�uB
��B
�B
��B
��B
�MB
�MB
�MB
��B
��B
��B
��B
��B
��B
�B
�B
�SB
��B
��B
�%B
�%B
�%B
�YB
�YB
��B
��B
��B
�+B
�_B
��B
��B
��B
�fB
�7B
�lB
��B
��B
��B
��B
�	B
�=B
�=B
�=B
�=B
�=B
��B
��B
�DB
�DB
�DB
�B
�xB
�xB
��B
��B
��B
��B
�B
�~B
��B
�~B
�~B
�~B
�PB
�PB
��B
�"B
�VB
�VB
��B
��B
��B
��B
�(B
�\B
��B
��B
��B
��B
��B
��B
5�B
4nB
49B
5B
6FB
2�B
49B
5B
4nB
3�B
49B
4�B
4�B
2�B
3hB
49B
5tB
3�B
3hB
33B
4�B
5B
33B
3�B
4�B
5�B
3hB
2aB
1�B
4�B
4�B
3�B
3hB
4B
4�B
4�B
2�B
33B
4B
6B
3hB
33B
4B
4�B
4�B
3hB
3�B
5�B
5B
2-B
3hB
5tB
4nB
3�B
33B
4B
5B
2�B
33B
3�B
4nB
3�B
3�B
4�B
5B
3�B
2�B
4�B
4B
4�B
9XB
5B
49B
6FB
5�B
3�B
2�B
5tB
3�B
4B
1[B
49B
2�B
0�B
33B
6FB
3�B
3�B
1'B
3�B
2-B
6B
5?B
1[B
/�B
2�B
4nB
8�B
2aB
1[B
2�B
33B
5B
2�B
6B
1�B
(�B
5B
0�B
33B
49B
1[B
4�B
2-B
1�B
6zB
3�B
2�B
2-B
3�B
4nB
2�B
1�B
2�B
3�B
3�B
1�B
2�B
3�B
3�B
2�B
1�B
2aB
3hB
3�B
3hB
2-B
2-B
4B
2�B
1�B
2aB
3�B
3�B
1�B
1�B
2�B
3�B
2�B
1�B
1�B
3hB
3�B
2�B
1[B
1�B
2-B
33B
3�B
2aB
1�B
1�B
2aB
3hB
3hB
1�B
1[B
1�B
33B
2aB
2�B
0UB
1�B
2-B
2�B
3�B
0�B
0�B
1�B
1�B
0�B
0UB
1[B
1�B
0�B
/�B
0�B
1[B
0�B
/�B
/�B
0�B
0�B
/B
.IB
.�B
0UB
/B
-�B
-wB
-�B
.B
-�B
,qB
,qB
+�B
+�B
+kB
)�B
($B
'�B
'�B
'B
$tB
$�B
&LB
$tB
!�B
B
7B
 �B
:B
(�B
7LB
!�B
�B
M6B
B
1B	��B	��B	ںB	֡B	�#B	רB	��B	бB	�?B	ԕB	��B	�[B	�&B	�}B	��B	��B	�KB
�B
�B
 iB	�rB	�8B	�B	��B
�B
�B
B
B
AB
B
B
�B
�B
�B
�B
eB
eB
�B
*�B
9�B
>wB
?B
QB
��B
�BB
��B
��B
��B�B'�B6FB;0Bi�B��B~]B�B�GB�YB�B��B��B��B��B�{B�FB��B��B�B��B��B�B��B��B�wB�RB�0B�9B�B�XB��B��B�CB�CB�CB��B�9B�$B��B��BʌB�,B�[B��B�dB�dB�BԕB��B�pB�B�<BбB�B�BB��B��B�yB�sB�QB�B�%B��B�>BB�B2aB5?B4�B7�B9�B<B=�B?B@�BB[BJ#BIRBUgBW�BN�BP}BQ�BQNBP�BIRBIRBK)BJ�BGEBFBM�BA�BFBA�BA�B@�BF�B?�BA�BB'B=qB@B>BB?B=�B=<B<�BC�BB'BB[BF�BCaB@�BIRBF�BMjB\�BL�BL0BOBBQ�BN�BE�BIRBN�BT�BbB^BncBh�B`�B]�B]�Ba|B_�B`BaHBaHB`B_�B]�BdZBhsBt�BXEB\]BN<BK�BI�BFtB@�B>�BN�BW?B>wB3hB1[B(�B(�B'B+B(XB"�B�BB=BSB�B�B B\BPBB�B�B  B��B �B�B�B�B��B�jBɺBȀB�B�?B� B��B��B�mB��B��B�'B��B�IB��B�!B�CB�IB�qB�@B�FB��B�JB��B��B��B��B��B�GB��B��B}�BqABj�B_B`BR�BR�BM�BJXBH�BI�BG�BH�BM�BEBB�BB�BAUB@�B?HB?B>�B=�B=�B:�B:�B:�B:^B6zB2�B8�B1�BJ#B4�B�B:B�B\BPBVB	�B�B
=B�B
��B
��BB 4B
��B
��B
�B
�pB
�5B
��B
��B
��B
уB
ѷB
�B
ΥB
�)B
ƨB
�9B
ŢB
ÖB
� B
��B
�hB
�B
��B
�:B
�tB
�B
�xB
��B
�hB
�	B
��B
{JB
��B
��B
2�B
/�B
=<B
7�B
!�B
�B
	B
1B
B
;B
MB	�>B	��B	��B	�B	�B	�QB	�`B	�8B	�B	רB	�gB	�B	�B	��B	��B	� B	ĜB	� B	�wB	�0B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�@B	��B	�\B	�VB	�B	�SB	��B	�	B	�-B	��B	�kB	��B	�VB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�SB	�B	�B	��B	�B	~]B	��B	�B	��B	cB	w�B	��B	��B	�JB	yrB	v�B	t�B	tTB	tTB	s�B	u�B	rGB	u%B	p�B	t�B	p�B	q�B	s�B	sMB	r�B	{�B	w�B	q�B	p�B	l"B	jB	m]B	j�B	q�B	k�B	g�B	c�B	kB	�B	l"B	�AB	j�B	iDB	i�B	kB	jKB	p;B	lWB	p;B	j�B	m�B	ncB	}�B	��B	cB	�SB	��B	��B	�MB	��B	�4B	�YB	�%B	�B	�JB	�JB	�DB	��B	�YB	�fB	� B	��B	��B	�MB	�	B	�eB	�=B	��B	�~B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              B
.�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-NB
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.TB
-NB
-�B
,�B
-�B
-B
+�B
-�B
,�B
,�B
,�B
,�B
,|B
,HB
,|B
,B
+�B
+�B
+vB
*�B
*B
)B
'�B
$�B
 �B
pB
�B	�B	�|B	ΰB	�$B	��B	�-B
�B
0�B
��B=|B}-B��B��B��B�mB��B��B�BFB>�B;BF�BX�B\4BD>B#�B�B��B�QB��B�XBk�BEDB<�B4�B"
B�B
�B
ͪB
��B
�aB
w�B
 �B	�qB	�oB	��B	��B	�B	��B	�B	�'B	�$B	��B	m�B	m4B	i�B	hB	f=B	sYB	B	��B	�3B	��B	��B	�B	��B	�	B	ۗB	�B	��B
-�B
E�B
p{B
�
B
��B
��B
�^B
��B
��B
��B
��B
�|B
�_B
��B
��B
��B
�B
��B
�UB
��B
��B
�B
�uB
��B
�:B
�nB
��B
�.B
�B
�!B
�nB
�LB
�SB
��B
�B
�B
�SB
�B
�B
�B
�B
�B
�B
�{B
�@B
�B
��B
��B
��B
��B
�B
�PB
��B
�rB
�7B
�eB
�B
��B
ݣB
�iB
�DB
��B
էB
ҔB
�SB
�B
�AB
�cB
�B
�QB
�lB
ħB
�
B
��B
��B
�B
�TB
�AB
�B
��B
��B
�B
�2B
�,B
�ZB
��B
�dB
�#B
��B
�B
�B
�B
��B
|�B
��B
~hB
�tB
~�B
vB
q�B
ueB
u1B
t�B
j�B
rSB
r�B
r�B
v7B
r�B
qB
o�B
n�B
jVB
iB
h~B
`B
XB
`�B
c+B
]:B
X�B
Z�B
X�B
TlB
TlB
P�B
O�B
P�B
R�B
Q�B
Q�B
P�B
P�B
NB
M�B
K�B
I�B
F�B
FJB
FB
G�B
FB
F�B
FJB
E�B
F�B
E�B
FJB
EyB
DsB
D
B
C�B
C�B
A�B
B2B
A,B
?�B
=�B
<�B
<AB
;pB
;�B
;pB
;pB
:B
7�B
7�B
7�B
6B
3�B
4EB
28B
3>B
/�B
+B
+vB
(/B
'�B
')B
')B
&�B
$KB
$�B
&#B
#�B
"
B
"�B
"?B
�B
!mB
�B
B
pB
�B
�B
6B
<B
B
�B
�B
<B
6B
�B
�B
�B
3B
aB
�B

�B
�B
�B
OB
OB
�B
UB
�B
aB
�B
!B

B
�B
�B
^B
*B
�B
�B
XB
�B
�B
�B
eB
0B
�B
�B
*B
�B
�B
�B
B
B
B
RB
	B
�B
�B
�B
�B
[B
'B
�B
[B
'B
'B
�B
�B
OB
�B
�B
B
B
OB
B
OB
B
�B
�B

�B
B

�B
B
OB
�B
OB
B

�B

�B
�B
B
!B
�B
�B
UB
[B
�B
�B
�B
�B
'B
[B
�B
�B
!B
UB
�B
�B
�B
�B
'B
[B
'B
[B
�B
�B
�B
�B
3B
nB
�B
�B
9B
nB
nB
B
B
�B
?B
?B
�B
�B
�B
�B
B
#B
RB
�B
�B
�B
LB
XB
�B
�B
�B
*B
B
B
�B
B
�B
BB
�B
�B
�B
B
�B
�B
�B
�B
�B
 B
&B
[B
�B
[B
[B
�B
�B
�B
aB
aB
aB
aB
 3B
 �B
"
B
!�B
"
B
"
B
"�B
#EB
#yB
#yB
#�B
#�B
$B
#�B
#yB
$�B
$�B
$B
&#B
%�B
%�B
%�B
&WB
&#B
&#B
&WB
&�B
')B
')B
'^B
')B
')B
')B
')B
&�B
(/B
'�B
'�B
)5B
(�B
(/B
(�B
)B
)jB
*B
)jB
)�B
*<B
)�B
*B
*<B
*�B
*�B
+BB
+BB
+vB
+BB
+�B
+�B
+�B
,HB
,HB
,HB
,HB
,HB
,HB
,�B
-B
,�B
-�B
-�B
. B
-�B
. B
. B
-�B
.TB
.TB
.TB
.�B
/ZB
/ZB
/�B
/�B
/�B
0,B
/�B
0�B
2�B
2mB
2mB
28B
2�B
3�B
4B
4B
4yB
4yB
4yB
4EB
5�B
5�B
5�B
6B
6B
6�B
6QB
6B
6�B
7#B
7�B
7�B
7�B
8]B
7�B
7�B
8]B
8�B
8�B
9cB
:jB
:B
:5B
:jB
;B
;B
;;B
;;B
;pB
;pB
;�B
<�B
<�B
<�B
=B
=|B
>NB
=�B
>NB
>�B
>�B
?B
?B
?B
?TB
?TB
?�B
@ZB
@�B
@ZB
@�B
@�B
A�B
A�B
B2B
B�B
B�B
CB
CB
ClB
D
B
D>B
D>B
DsB
D�B
D�B
DsB
D�B
DsB
EyB
E�B
E�B
FB
E�B
FB
FB
F�B
F�B
GB
F�B
F�B
G�B
G�B
G�B
H"B
G�B
G�B
HWB
H"B
H"B
H"B
H�B
H�B
H�B
I(B
H�B
H�B
J/B
J�B
J�B
JcB
J�B
J�B
KiB
K�B
LB
K�B
K�B
K�B
K�B
LoB
MB
LoB
L�B
M�B
MuB
MuB
M�B
NB
N�B
OMB
OMB
OMB
OB
PB
PB
PSB
P�B
P�B
P�B
P�B
QZB
QZB
Q%B
QZB
Q%B
Q%B
R�B
R`B
S�B
S1B
S�B
S1B
T8B
T8B
S�B
T8B
TB
TB
T8B
T�B
U	B
UrB
UrB
U�B
VDB
VxB
U�B
U�B
U�B
U�B
U�B
U�B
VDB
VDB
V�B
WB
W~B
WB
W�B
X�B
X�B
X�B
X�B
YVB
Y"B
X�B
X�B
Y�B
Y�B
Z�B
Z(B
Z(B
Z(B
Z(B
Z\B
Z�B
Z\B
Z�B
Z�B
Z�B
Z�B
[.B
[�B
[cB
\ B
[�B
[�B
[�B
\ B
\iB
\4B
[�B
\4B
]B
]:B
]�B
^AB
^uB
^uB
^�B
_B
_B
_B
_�B
_�B
`B
`�B
`�B
`�B
`�B
aB
aB
aB
aB
b�B
b�B
b�B
c+B
b�B
c�B
d1B
deB
d�B
d�B
e7B
e�B
e�B
f	B
f	B
f	B
f	B
f=B
f�B
gB
gxB
gCB
g�B
hJB
hJB
hB
g�B
g�B
h~B
i�B
j!B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k(B
k\B
k(B
j�B
j�B
k(B
k(B
j�B
j�B
k\B
k�B
m4B
nB
m�B
m4B
mhB
n�B
nnB
nnB
o�B
n�B
nnB
nB
m�B
nB
n:B
n:B
n�B
o@B
ouB
o�B
pB
p{B
p�B
qB
qLB
qLB
q�B
q�B
rB
r�B
r�B
s$B
s�B
t*B
t_B
t_B
t_B
t�B
t�B
ueB
ueB
u�B
u�B
vB
vkB
vkB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wB
wqB
w�B
w�B
w�B
w�B
w�B
xCB
xwB
xwB
yIB
y�B
y�B
y�B
zB
zB
zOB
z�B
{UB
{�B
{�B
|'B
|'B
|�B
|�B
}bB
}bB
}�B
}�B
}�B
~3B
~3B
~3B
~hB
~hB
~�B
~�B
~�B
B
:B
�B
�B
�B
�B
�B
�B
�tB
�tB
��B
��B
�B
�zB
��B
��B
�B
��B
�B
�RB
�RB
�RB
�RB
��B
��B
��B
��B
��B
��B
�XB
��B
��B
��B
��B
��B
�*B
�*B
��B
�^B
�^B
��B
��B
�0B
�eB
�0B
�0B
�0B
�B
�B
�kB
��B
�B
�B
�<B
��B
��B
��B
��B
�B
�wB
�wB
��B
�wB
��B
��B
/�B
. B
-�B
.�B
/�B
,HB
-�B
.�B
. B
-NB
-�B
.TB
.�B
,|B
-B
-�B
/&B
-�B
-B
,�B
.TB
.�B
,�B
-NB
.�B
/ZB
-B
,B
+BB
.�B
.TB
-NB
-B
-�B
.�B
.TB
,�B
,�B
-�B
/�B
-B
,�B
-�B
.�B
.TB
-B
-NB
/ZB
.�B
+�B
-B
/&B
. B
-NB
,�B
-�B
.�B
,HB
,�B
-�B
. B
-NB
-NB
.TB
.�B
-�B
,HB
.�B
-�B
.�B
3
B
.�B
-�B
/�B
/ZB
-NB
,HB
/&B
-�B
-�B
+B
-�B
,HB
*<B
,�B
/�B
-�B
-NB
*�B
-NB
+�B
/�B
.�B
+B
)5B
,�B
. B
28B
,B
+B
,|B
,�B
.�B
,HB
/�B
+BB
"?B
.�B
*pB
,�B
-�B
+B
.TB
+�B
+vB
0,B
-NB
,HB
+�B
-�B
. B
,|B
+�B
,�B
-�B
-�B
+�B
,HB
-�B
-�B
,|B
+vB
,B
-B
-NB
-B
+�B
+�B
-�B
,�B
+�B
,B
-NB
-NB
+�B
+�B
,|B
-NB
,HB
+BB
+�B
-B
-�B
,|B
+B
+�B
+�B
,�B
-NB
,B
+vB
+BB
,B
-B
-B
+BB
+B
+�B
,�B
,B
,HB
*B
+BB
+�B
,|B
-NB
*�B
*<B
+vB
+�B
*pB
*B
+B
+�B
*<B
)�B
*<B
+B
*pB
)5B
)5B
*<B
*<B
(�B
'�B
(�B
*B
(�B
'�B
')B
'�B
'�B
'^B
&#B
&#B
%QB
%�B
%B
#�B
!�B
!mB
!�B
 �B
&B
[B
�B
&B
HB
�B
�B
BB
�B
"�B
0�B
�B
dB
F�B
�B
�B	�wB	؅B	�lB	�SB	��B	�ZB	�|B	�cB	��B	�GB	ʗB	�B	��B	�/B	֭B	ӚB	��B	�\B
OB	�B	�$B	��B	��B	�CB	��B	��B	��B	��B	��B	��B	��B
 @B	�3B
	�B
9B
B
B
�B
$�B
3>B
8)B
8�B
J�B
��B
��B
ͪB
�uB
ԠB
��B!�B/�B4�Bc�B��BxBz�B|�B�B��B�qB�LB�wB��B�-B��B��B�3B��B�?B��B��B�}B��B�)B�B��B��BǹB�
B��B�5B��B��B��B��B��B��B�WB��B�>B��B�B�sB�B�B��B�GBǅB�"BǹB��B�cB��B��B�uB�~B�+B�%B�B�\B��B�B��B��BLB,B.�B.TB1�B3sB5�B7WB8�B:5B<BC�BCBOBQ�BH�BJ/BKiBK BJ�BCBCBD�BDsB@�B?�BGQB;pB?�B;�B;�B:5B@�B9cB;�B;�B7#B9�B7�B8�B7�B6�B6QB=�B;�B<B@�B=B:5BCB@ZBGBVxBFJBE�BH�BK5BHWB?�BCBH�BNGB[�BW�BhBbYBZ\BWJBWJB[.BYVBY�BZ�BZ�BY�BY�BW~B^Bb%Bn:BQ�BVBG�BEyBC�B@&B:�B8�BHWBP�B8)B-B+B"?B"�B �B$�B"
BNBpB�B�BB3B[B
�B	BB�B�:B�hB��B�B��B�VB׳B��B̤B�B�lB�2B��B��B��B��B��B�B�TB�HB��B�jB��B��B��B��B��B�#B��B��B��B��B�LB��B��B~hB|\B|�Bz�B�RBw�Bj�Bd�BX�BY�BL�BLoBGQBD
BB�BC�BA�BB2BGQB>�B<vB<AB;B:�B8�B8�B8�B7WB7WB4EB4yB4�B4B0,B,|B2�B+�BC�B.�B�B�B	�B	BBB�B6B�B<B
��B
�wB�B
��B
�LB
�B
�4B
�"B
��B
юB
ЈB
ΰB
�5B
�iB
��B
�WB
��B
�ZB
��B
�TB
�HB
��B
�EB
�B
��B
�sB
��B
�&B
��B
�*B
��B
�B
��B
�XB
t�B
��B
��B
,�B
)5B
6�B
1�B
�B
�B
�B
�B
�B	��B	��B	��B	�uB	��B	��B	�1B	�B	�B	��B	�MB	�ZB	�B	�oB	�=B	ŭB	��B	��B	�NB	��B	�)B	��B	��B	��B	��B	�mB	�TB	��B	��B	�9B	��B	�KB	��B	��B	�jB	�B	�B	��B	�B	��B	��B	��B	�?B	�B	�pB	�B	�tB	��B	��B	�CB	�<B	�6B	�3B	�[B	��B	�B	��B	��B	��B	y~B	xB	�LB	��B	��B	yB	qLB	��B	էB	��B	s$B	p{B	n�B	nB	nB	m4B	o�B	k�B	n�B	jVB	n:B	j�B	k\B	m4B	l�B	l�B	u�B	q�B	k�B	j�B	e�B	c�B	gB	deB	k\B	elB	a�B	]oB	d�B	z�B	e�B	{�B	d�B	b�B	c_B	d�B	c�B	i�B	f	B	i�B	deB	g�B	hB	w�B	��B	yB	B	��B	��B	}�B	z�B	y�B	�B	�B	��B	��B	��B	��B	�tB	�B	�B	��B	�qB	�qB	��B	��B	�B	��B	�UB	�0B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230711020048                            20230711020048AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023071102004820230711020048  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023071102004820230711020048QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023071102004820230711020048QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               