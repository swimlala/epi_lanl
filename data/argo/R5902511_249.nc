CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:49:50Z creation      
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
resolution        =���   axis      Z        8  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  b�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8 p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 5�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8 =x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8 d�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  20230721224950  20230721224950  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�-���@�-���11  @�`��@�`��@2c�V��@2c�V���d�c�	�d�c�	11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?�  @�@E�@�  @��R@��R@�  A   A  A   A,��AAG�A`  A~�RA��A�  A�Q�A���A�Q�A�  A�\)A�
=B�B�
B(�B (�B(  B0  B8  B@  BG�BO�BW�
B`  Bh(�Bp  Bx(�B�(�B�  B�{B�{B��B��
B�  B�{B��B��
B��B�{B�  B�{B�{B�  B�  B�  B��B�  B�=qB�{B��B��B�{B�  B�  B�  B��B��B�  B�{B��C��C��C�C  C

=C  C��C  C  C��C  C��C  C
=C  C�C!��C$  C&
=C(  C)��C,  C.  C0  C2  C4  C6  C8  C9��C;�C>  C@{CB
=CD  CF  CH  CI��CL  CN  CP  CR  CT  CU��CW��CY��C\  C^
=C`{Cb{Cd
=Cf
=Cg��Ci��Cl
=Cm��Cp  Cq��Cs��Cv  Cx  Cz  C{��C~  C��C�  C���C���C�  C�  C�C���C���C�C���C���C���C�  C�
=C�
=C�C�  C�C�  C�  C�
=C�  C�
=C�  C���C�C�  C�
=C�C�  C�
=C�  C���C�  C�  C�  C�  C���C���C���C���C���C�C�  C�  C�C�  C�  C�
=C�  C�  C�  C�
=C�
=C�  C���C���C�  C���C���C�C�C���C���C���C���C�C�C�  C���C�  C���C���C�  C�  C���C�  C�C�  C�C�  C�  C���C���C�  C�C�  C���C���C���C���C�
=C�C�C�  C�  C���C�  C�
=C�  C�  C�C�  C�  C���C���C�  C�C�  C�  C�  C�  C���C���C�  C�C�  C�  C�C�C�  C�  C���C���C���C���C���C���D � D  D��D�D��DD� D�qD}qD�qD}qD  D��DD}qD��D� D�qD	z�D	�qD
��D  D� D�D}qD  D� D�qD}qD  D� D  D��D�D��D�D}qD�D� D�qD� D  Dz�D�qD�D�D� D�qD��DD��D�D� D  D� D  D� D�qDz�D�qD}qD  D�D�qD xRD!  D!�D"  D"}qD#  D#}qD$  D$� D$�qD%}qD%��D&}qD'  D'� D(  D(}qD(�qD)��D*�D*��D+�D+}qD,  D,� D-  D-� D-�qD.}qD.�qD/� D0�D0�D1�D1� D2  D2� D3�D3��D3�qD4}qD4�qD5}qD6�D6� D6�qD7}qD8  D8� D8�qD9z�D9�RD:� D;D;� D;�qD<� D<�qD=� D>�D>��D?�D?��D@  D@}qD@�qDAz�DA�qDB��DC�DC� DD  DD��DE�DE� DF�DF��DG  DG}qDG�qDHz�DI  DI�DJ  DJ� DK�DK� DL  DL� DM  DM� DM�qDN� DODO�DP�DP��DQDQ� DQ�qDR��DS  DS� DT�DT��DT�qDU}qDV  DV� DW  DW� DX  DX� DX��DY� DZ�DZ� DZ�qD[� D\�D\�D]D]��D^�D^��D_  D_z�D_�qD`� D`�qDaxRDa��Db}qDb�qDcz�Dc��Dd}qDe  De� De�qDfz�Dg  Dg��Dh�Dh� Dh�qDi� Dj�Dj��Dk  Dk}qDl�Dl��Dm  Dm��Dn�Dn}qDn�qDo� Dp�Dp��DqDq� Dr  Dr� Ds  Ds� Ds�qDt� DuDu��Du�qDv� Dw�Dw�Dw�qDx}qDx�qDy� Dz�Dz��D{  D{� D|  D|� D}  D}� D~�D~� D~��Dz�D�qD�=qD�|)D��qD�  D�@ D�~�D���D�  D�>�D�� D��HD��qD�@ D�� D��qD�  D�>�D�~�D��HD�HD�>�D�� D��HD�  D�AHD�� D�� D�  D�@ D��HD�D�HD�AHD���D���D�  D�>�D���D��HD���D�AHD�� D���D���D�>�D�� D��HD��D�@ D�~�D���D�HD�C�D��HD���D���D�>�D�~�D�� D���D�>�D�~�D��qD�  D�@ D�� D�� D���D�@ D�}qD��qD�HD�@ D�}qD��qD���D�AHD���D�� D���D�@ D�� D�� D�  D�AHD�� D���D�  D�AHD��HD��HD�  D�AHD�� D�� D�HD�B�D�� D���D���D�@ D�� D��qD���D�@ D�~�D���D���D�>�D�~�D�� D���D�@ D�� D�� D�  D�>�D�~�D�� D�  D�>�D�� D��HD�HD�AHD�� D�� D���D�@ D���D�� D�  D�@ D��HD���D���D�@ D�� D��HD�  D�>�D�~�D�� D���D�@ D�� D�� D��D�@ D�~�D���D�  D�>�D�� D�� D�  D�AHD�� D�� D���D�@ D�� D��HD�HD�@ D�~�D��qD���D�>�D�� D��HD�HD�@ D�~�D��HD�  D�>�D�~�D��qD�  D�B�D��HD���D���D�=qD�~�D�� D�  D�@ D��HD��HD�  D�@ D��HD��HD�  D�AHD��HD���D���D�@ D��HD��HD�HD�@ D�� D���D���D�=qD�}qD�� D�  D�>�D�}qD���D���D�=qD�}qD���D�  D�>�D�~�D���D���D�AHD�� D���D���D�>�D�� D��HD�  D�AHD�� D��HD�HD�@ D�}qD���D�  D�>�D�� D��HD���D�@ D��HD��HD�HD�B�D��HD�D�HD�>�D��HD�� D�HD�AHD�~�D���D���D�@ D�~�D���D���D�>�D D��HD�HD�=qD�}qD�� D�HD�>�D�~�D�� D���D�AHDŃ�D�� D�  D�AHDƁHDƾ�D���D�AHDǀ DǾ�D�  D�>�DȁHD��HD���D�>�Dɀ D��HD��D�AHDʀ Dʾ�D���D�>�Dˀ D˾�D���D�AHD́HD��HD�  D�AHD́HD�� D���D�>�D�~�DνqD���D�@ D�~�DϾ�D���D�@ DЁHD��HD�  D�@ Dр DѾ�D�HD�AHDҀ D�� D�  D�@ DӀ D�� D�  D�>�D�~�DԾ�D���D�@ DՀ D�� D�  D�AHDցHD�� D�  D�B�Dׂ�D��HD�HD�@ D؁HD�D�  D�=qD�}qDٽqD�  D�@ Dڀ D�D�  D�>�D�~�D�� D���D�AHD܂�D��HD�  D�>�D�}qD�� D�  D�@ DށHD��HD�  D�>�D�~�D�� D��D�B�D��HD��HD�  D�>�D� D�D�  D�=qD�~�D⾸D�  D�@ D�HD��HD�  D�=qD� D侸D���D�AHD�~�D�qD���D�AHD悏D�� D��qD�>�D�HD��HD���D�@ D�HD辸D���D�@ D� D�� D�HD�AHD�~�D꾸D���D�AHD�HD��HD�  D�@ D�~�D�� D��D�B�D킏D��HD���D�@ D� DD��qD�>�D�HD�� D�HD�B�D�~�D�D���D�@ D�~�D�� D�HD�@ D�~�D�� D���D�>�D�HD�D�HD�@ D� D��HD�  D�>�D�~�D�� D�HD�@ D��HD��HD�  D�>�D�� D���D�  D�:�?\)?��?8Q�?�=q?�33?Ǯ@   @�@�R@8Q�@J=q@\(�@s33@�  @���@�@�G�@�=q@�@��R@���@�33@�(�@���@��@��HAz�A��Ap�A�AQ�A��A#33A'�A,(�A333A7�A<(�AB�\AHQ�AL(�AQ�AX��A\��Ac33AhQ�Al��As�
Aw�A}p�A��A��
A�
=A���A��
A�\)A���A�z�A�  A��A�p�A�  A��\A�{A�  A��
A�{A���A�(�A�{A��A�z�A�
=A��HA��A�  A˅A�{AУ�A�z�A�
=A�=qA�p�A�\)A�\A�{A�Q�A��HA�{A���A�33A�ffA�G�A��
A�\)B ��BffB(�BG�B
=BQ�B	��B\)B��B�B�B��B{B�
B�B=qB  B��B�HB�
BG�B�RB�
B!G�B"�RB#�B%G�B&�\B'�B)G�B*{B+�B,��B-�B/�B0z�B1B3\)B4z�B5��B733B8  B9�B;
=B<  B=B>�RB@  BA��BBffBC�
BE�BE�BG�BH��BI��BK\)BLz�BMp�BO
=BPQ�BQG�BR�HBT(�BU�BV�RBW�
BX��BZ�\B[�
B\��B^=qB_�B`z�Ba�Bc\)Bd(�Be��Bg
=Bh  Bip�Bj�RBk�Bm�BnffBo\)Bp��Br�\Bs�Bu�Bv�\Bw�ByG�Bz�\B{�
B}p�B~�RB�B��\B�\)B��B�ffB�33B�B�ffB�G�B�B�z�B�\)B��
B��\B�\)B��B���B�p�B�  B���B���B�{B��HB��B�(�B�
=B��
B�ffB��B�  B��\B�33B�{B��HB�\)B�{B���B���B�(�B�
=B��B�Q�B�33B��
B�ffB�G�B��B��\B�p�B�{B���B���B�(�B���B��B�Q�B���B�B��\B��B��
B���B�G�B��B���B��B�{B���B�B�Q�B��B�  B���B�33B�(�B���B�p�B�Q�B��B��B�ffB�G�B�B��\B�p�B��B��\B�\)B�(�B���B��B�(�B��RB��B�=qB���BîB�ffB��HB�B�ffB�
=B��
B�z�B���BɮB�Q�B���B˅B�(�B�z�B�33BͮB�(�B��HB�G�Bϙ�B�=qBЏ\B��HBљ�B�B�=qBҸRB���B�G�B�B�{B�Q�B���B�G�BՅB�B�=qBָRB��HB�33B�B�  B�ffB���B�
=B�\)B�B�=qB�ffB���B�\)BۅB��
B�Q�B���B���B�\)B��
B�{B�ffB��HB��B�\)B��
B�Q�B��\B�RB��BᙚB�B�{B�\B��HB�
=B�\)B��
B�(�B�Q�B���B�33B�\)B�B�=qB�z�B�RB�G�B�p�B��
B�Q�B��B���B�\)B陚B��
B�Q�B�RB��HB�\)B��
B�  B�ffB��HB��B�\)B��B�(�B�ffB��HB�G�B�\)B��
B�Q�B��\B���B�G�B�p�B�B�=qB�ffB��B��B�p�B�B�{B�z�B�\B���B�\)B��B�B�Q�B��\B���B�G�B��B��B�=qB��\B���B�33B�p�B��B�(�B�z�B���B�33B�G�B��B�(�B�Q�B���B��B�\)B��B�  B�ffB��\B��HB�\)B�p�B��
C (�C =qC ffC ��C �C �C�C33CffC��C�C�HC�C33C\)C��C�RC�HC�C=qC\)C��C�RC�HC(�C33Cp�C��C�C�HC�C33C\)C��C�RC�C�C33Cp�C��C�RC  C33CG�C�\C�RC�HC�CQ�Cp�C��C�HC��C	(�C	p�C	�\C	�C	�C
�C
Q�C
p�C
��C
�HC
��C{CQ�C�\C��C��C{C�C\)C�\C�C��C
=C=qC\)C�CC��C�C=qCz�C�RC�HC{C=qCp�C�RC�
C  C=qC�C�C��C�CG�Cp�C�C��C(�CG�Cz�CC��C�CQ�C�\C�
C��C(�Cp�C��CC
=CG�Cp�C�\C�HC
=C(�Cz�C�C��C
=CQ�Cz�C��C�HC(�Cp�C�\CC  C=qCp�C��C��C(�C=qC�\C��C��C(�Cp�C�C��C
=CQ�Cz�C��C�
C�C\)C�C�C��C33C\)Cz�C�RC
=CG�CffC��C�HC �C G�C z�C �RC!  C!33C!\)C!��C!�HC"{C"33C"z�C"�RC#  C#�C#G�C#�C#��C$
=C$(�C$\)C$��C$�
C%{C%G�C%ffC%�C%�C&�C&=qC&z�C&C&�HC'
=C'G�C'�C'��C'��C({C(G�C(ffC(��C(�
C)
=C)(�C)Q�C)��C)��C)�C*{C*Q�C*z�C*��C*��C+
=C+=qC+Q�C+�C+C+��C,{C,G�C,�C,�C,��C,��C-33C-p�C-��C-�RC-��C.33C.Q�C.z�C.C.��C/{C/=qC/�C/�RC/�HC0  C0=qC0z�C0�C0�HC1  C133C1p�C1�C1��C1��C2=qC2z�C2�\C2��C3{C3G�C3ffC3��C3�HC4{C4=qC4\)C4��C4�HC5{C5G�C5ffC5��C5�
C6{C6G�C6ffC6��C6�
C7{C7G�C7p�C7�\C7�
C8
=C8G�C8p�C8�\C8�
C9{C9=qC9ffC9�\C9��C:{C:=qC:ffC:�\C:�RC;  C;33C;Q�C;z�C;�RC;��C<�C<G�C<p�C<��C<�C=�C==qC=p�C=��C=�C>{C>33C>ffC>�C>�HC?  C?(�C?p�C?��C?��C?�C@�C@\)C@��C@C@�HCA{CAQ�CA�\CA�RCA�
CB
=CBQ�CBz�CB��CB��CC
=CCG�CCp�CC�\CCCD  CD33CDp�CD��CDCD�CE�CEQ�CE�\CE��CF  CF33CF\)CF�\CF�CF�CG(�CGp�CG��CG�
CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                    ?�  @�@E�@�  @��R@��R@�  A   A  A   A,��AAG�A`  A~�RA��A�  A�Q�A���A�Q�A�  A�\)A�
=B�B�
B(�B (�B(  B0  B8  B@  BG�BO�BW�
B`  Bh(�Bp  Bx(�B�(�B�  B�{B�{B��B��
B�  B�{B��B��
B��B�{B�  B�{B�{B�  B�  B�  B��B�  B�=qB�{B��B��B�{B�  B�  B�  B��B��B�  B�{B��C��C��C�C  C

=C  C��C  C  C��C  C��C  C
=C  C�C!��C$  C&
=C(  C)��C,  C.  C0  C2  C4  C6  C8  C9��C;�C>  C@{CB
=CD  CF  CH  CI��CL  CN  CP  CR  CT  CU��CW��CY��C\  C^
=C`{Cb{Cd
=Cf
=Cg��Ci��Cl
=Cm��Cp  Cq��Cs��Cv  Cx  Cz  C{��C~  C��C�  C���C���C�  C�  C�C���C���C�C���C���C���C�  C�
=C�
=C�C�  C�C�  C�  C�
=C�  C�
=C�  C���C�C�  C�
=C�C�  C�
=C�  C���C�  C�  C�  C�  C���C���C���C���C���C�C�  C�  C�C�  C�  C�
=C�  C�  C�  C�
=C�
=C�  C���C���C�  C���C���C�C�C���C���C���C���C�C�C�  C���C�  C���C���C�  C�  C���C�  C�C�  C�C�  C�  C���C���C�  C�C�  C���C���C���C���C�
=C�C�C�  C�  C���C�  C�
=C�  C�  C�C�  C�  C���C���C�  C�C�  C�  C�  C�  C���C���C�  C�C�  C�  C�C�C�  C�  C���C���C���C���C���C���D � D  D��D�D��DD� D�qD}qD�qD}qD  D��DD}qD��D� D�qD	z�D	�qD
��D  D� D�D}qD  D� D�qD}qD  D� D  D��D�D��D�D}qD�D� D�qD� D  Dz�D�qD�D�D� D�qD��DD��D�D� D  D� D  D� D�qDz�D�qD}qD  D�D�qD xRD!  D!�D"  D"}qD#  D#}qD$  D$� D$�qD%}qD%��D&}qD'  D'� D(  D(}qD(�qD)��D*�D*��D+�D+}qD,  D,� D-  D-� D-�qD.}qD.�qD/� D0�D0�D1�D1� D2  D2� D3�D3��D3�qD4}qD4�qD5}qD6�D6� D6�qD7}qD8  D8� D8�qD9z�D9�RD:� D;D;� D;�qD<� D<�qD=� D>�D>��D?�D?��D@  D@}qD@�qDAz�DA�qDB��DC�DC� DD  DD��DE�DE� DF�DF��DG  DG}qDG�qDHz�DI  DI�DJ  DJ� DK�DK� DL  DL� DM  DM� DM�qDN� DODO�DP�DP��DQDQ� DQ�qDR��DS  DS� DT�DT��DT�qDU}qDV  DV� DW  DW� DX  DX� DX��DY� DZ�DZ� DZ�qD[� D\�D\�D]D]��D^�D^��D_  D_z�D_�qD`� D`�qDaxRDa��Db}qDb�qDcz�Dc��Dd}qDe  De� De�qDfz�Dg  Dg��Dh�Dh� Dh�qDi� Dj�Dj��Dk  Dk}qDl�Dl��Dm  Dm��Dn�Dn}qDn�qDo� Dp�Dp��DqDq� Dr  Dr� Ds  Ds� Ds�qDt� DuDu��Du�qDv� Dw�Dw�Dw�qDx}qDx�qDy� Dz�Dz��D{  D{� D|  D|� D}  D}� D~�D~� D~��Dz�D�qD�=qD�|)D��qD�  D�@ D�~�D���D�  D�>�D�� D��HD��qD�@ D�� D��qD�  D�>�D�~�D��HD�HD�>�D�� D��HD�  D�AHD�� D�� D�  D�@ D��HD�D�HD�AHD���D���D�  D�>�D���D��HD���D�AHD�� D���D���D�>�D�� D��HD��D�@ D�~�D���D�HD�C�D��HD���D���D�>�D�~�D�� D���D�>�D�~�D��qD�  D�@ D�� D�� D���D�@ D�}qD��qD�HD�@ D�}qD��qD���D�AHD���D�� D���D�@ D�� D�� D�  D�AHD�� D���D�  D�AHD��HD��HD�  D�AHD�� D�� D�HD�B�D�� D���D���D�@ D�� D��qD���D�@ D�~�D���D���D�>�D�~�D�� D���D�@ D�� D�� D�  D�>�D�~�D�� D�  D�>�D�� D��HD�HD�AHD�� D�� D���D�@ D���D�� D�  D�@ D��HD���D���D�@ D�� D��HD�  D�>�D�~�D�� D���D�@ D�� D�� D��D�@ D�~�D���D�  D�>�D�� D�� D�  D�AHD�� D�� D���D�@ D�� D��HD�HD�@ D�~�D��qD���D�>�D�� D��HD�HD�@ D�~�D��HD�  D�>�D�~�D��qD�  D�B�D��HD���D���D�=qD�~�D�� D�  D�@ D��HD��HD�  D�@ D��HD��HD�  D�AHD��HD���D���D�@ D��HD��HD�HD�@ D�� D���D���D�=qD�}qD�� D�  D�>�D�}qD���D���D�=qD�}qD���D�  D�>�D�~�D���D���D�AHD�� D���D���D�>�D�� D��HD�  D�AHD�� D��HD�HD�@ D�}qD���D�  D�>�D�� D��HD���D�@ D��HD��HD�HD�B�D��HD�D�HD�>�D��HD�� D�HD�AHD�~�D���D���D�@ D�~�D���D���D�>�D D��HD�HD�=qD�}qD�� D�HD�>�D�~�D�� D���D�AHDŃ�D�� D�  D�AHDƁHDƾ�D���D�AHDǀ DǾ�D�  D�>�DȁHD��HD���D�>�Dɀ D��HD��D�AHDʀ Dʾ�D���D�>�Dˀ D˾�D���D�AHD́HD��HD�  D�AHD́HD�� D���D�>�D�~�DνqD���D�@ D�~�DϾ�D���D�@ DЁHD��HD�  D�@ Dр DѾ�D�HD�AHDҀ D�� D�  D�@ DӀ D�� D�  D�>�D�~�DԾ�D���D�@ DՀ D�� D�  D�AHDցHD�� D�  D�B�Dׂ�D��HD�HD�@ D؁HD�D�  D�=qD�}qDٽqD�  D�@ Dڀ D�D�  D�>�D�~�D�� D���D�AHD܂�D��HD�  D�>�D�}qD�� D�  D�@ DށHD��HD�  D�>�D�~�D�� D��D�B�D��HD��HD�  D�>�D� D�D�  D�=qD�~�D⾸D�  D�@ D�HD��HD�  D�=qD� D侸D���D�AHD�~�D�qD���D�AHD悏D�� D��qD�>�D�HD��HD���D�@ D�HD辸D���D�@ D� D�� D�HD�AHD�~�D꾸D���D�AHD�HD��HD�  D�@ D�~�D�� D��D�B�D킏D��HD���D�@ D� DD��qD�>�D�HD�� D�HD�B�D�~�D�D���D�@ D�~�D�� D�HD�@ D�~�D�� D���D�>�D�HD�D�HD�@ D� D��HD�  D�>�D�~�D�� D�HD�@ D��HD��HD�  D�>�D�� D���D�  D�:�?\)?��?8Q�?�=q?�33?Ǯ@   @�@�R@8Q�@J=q@\(�@s33@�  @���@�@�G�@�=q@�@��R@���@�33@�(�@���@��@��HAz�A��Ap�A�AQ�A��A#33A'�A,(�A333A7�A<(�AB�\AHQ�AL(�AQ�AX��A\��Ac33AhQ�Al��As�
Aw�A}p�A��A��
A�
=A���A��
A�\)A���A�z�A�  A��A�p�A�  A��\A�{A�  A��
A�{A���A�(�A�{A��A�z�A�
=A��HA��A�  A˅A�{AУ�A�z�A�
=A�=qA�p�A�\)A�\A�{A�Q�A��HA�{A���A�33A�ffA�G�A��
A�\)B ��BffB(�BG�B
=BQ�B	��B\)B��B�B�B��B{B�
B�B=qB  B��B�HB�
BG�B�RB�
B!G�B"�RB#�B%G�B&�\B'�B)G�B*{B+�B,��B-�B/�B0z�B1B3\)B4z�B5��B733B8  B9�B;
=B<  B=B>�RB@  BA��BBffBC�
BE�BE�BG�BH��BI��BK\)BLz�BMp�BO
=BPQ�BQG�BR�HBT(�BU�BV�RBW�
BX��BZ�\B[�
B\��B^=qB_�B`z�Ba�Bc\)Bd(�Be��Bg
=Bh  Bip�Bj�RBk�Bm�BnffBo\)Bp��Br�\Bs�Bu�Bv�\Bw�ByG�Bz�\B{�
B}p�B~�RB�B��\B�\)B��B�ffB�33B�B�ffB�G�B�B�z�B�\)B��
B��\B�\)B��B���B�p�B�  B���B���B�{B��HB��B�(�B�
=B��
B�ffB��B�  B��\B�33B�{B��HB�\)B�{B���B���B�(�B�
=B��B�Q�B�33B��
B�ffB�G�B��B��\B�p�B�{B���B���B�(�B���B��B�Q�B���B�B��\B��B��
B���B�G�B��B���B��B�{B���B�B�Q�B��B�  B���B�33B�(�B���B�p�B�Q�B��B��B�ffB�G�B�B��\B�p�B��B��\B�\)B�(�B���B��B�(�B��RB��B�=qB���BîB�ffB��HB�B�ffB�
=B��
B�z�B���BɮB�Q�B���B˅B�(�B�z�B�33BͮB�(�B��HB�G�Bϙ�B�=qBЏ\B��HBљ�B�B�=qBҸRB���B�G�B�B�{B�Q�B���B�G�BՅB�B�=qBָRB��HB�33B�B�  B�ffB���B�
=B�\)B�B�=qB�ffB���B�\)BۅB��
B�Q�B���B���B�\)B��
B�{B�ffB��HB��B�\)B��
B�Q�B��\B�RB��BᙚB�B�{B�\B��HB�
=B�\)B��
B�(�B�Q�B���B�33B�\)B�B�=qB�z�B�RB�G�B�p�B��
B�Q�B��B���B�\)B陚B��
B�Q�B�RB��HB�\)B��
B�  B�ffB��HB��B�\)B��B�(�B�ffB��HB�G�B�\)B��
B�Q�B��\B���B�G�B�p�B�B�=qB�ffB��B��B�p�B�B�{B�z�B�\B���B�\)B��B�B�Q�B��\B���B�G�B��B��B�=qB��\B���B�33B�p�B��B�(�B�z�B���B�33B�G�B��B�(�B�Q�B���B��B�\)B��B�  B�ffB��\B��HB�\)B�p�B��
C (�C =qC ffC ��C �C �C�C33CffC��C�C�HC�C33C\)C��C�RC�HC�C=qC\)C��C�RC�HC(�C33Cp�C��C�C�HC�C33C\)C��C�RC�C�C33Cp�C��C�RC  C33CG�C�\C�RC�HC�CQ�Cp�C��C�HC��C	(�C	p�C	�\C	�C	�C
�C
Q�C
p�C
��C
�HC
��C{CQ�C�\C��C��C{C�C\)C�\C�C��C
=C=qC\)C�CC��C�C=qCz�C�RC�HC{C=qCp�C�RC�
C  C=qC�C�C��C�CG�Cp�C�C��C(�CG�Cz�CC��C�CQ�C�\C�
C��C(�Cp�C��CC
=CG�Cp�C�\C�HC
=C(�Cz�C�C��C
=CQ�Cz�C��C�HC(�Cp�C�\CC  C=qCp�C��C��C(�C=qC�\C��C��C(�Cp�C�C��C
=CQ�Cz�C��C�
C�C\)C�C�C��C33C\)Cz�C�RC
=CG�CffC��C�HC �C G�C z�C �RC!  C!33C!\)C!��C!�HC"{C"33C"z�C"�RC#  C#�C#G�C#�C#��C$
=C$(�C$\)C$��C$�
C%{C%G�C%ffC%�C%�C&�C&=qC&z�C&C&�HC'
=C'G�C'�C'��C'��C({C(G�C(ffC(��C(�
C)
=C)(�C)Q�C)��C)��C)�C*{C*Q�C*z�C*��C*��C+
=C+=qC+Q�C+�C+C+��C,{C,G�C,�C,�C,��C,��C-33C-p�C-��C-�RC-��C.33C.Q�C.z�C.C.��C/{C/=qC/�C/�RC/�HC0  C0=qC0z�C0�C0�HC1  C133C1p�C1�C1��C1��C2=qC2z�C2�\C2��C3{C3G�C3ffC3��C3�HC4{C4=qC4\)C4��C4�HC5{C5G�C5ffC5��C5�
C6{C6G�C6ffC6��C6�
C7{C7G�C7p�C7�\C7�
C8
=C8G�C8p�C8�\C8�
C9{C9=qC9ffC9�\C9��C:{C:=qC:ffC:�\C:�RC;  C;33C;Q�C;z�C;�RC;��C<�C<G�C<p�C<��C<�C=�C==qC=p�C=��C=�C>{C>33C>ffC>�C>�HC?  C?(�C?p�C?��C?��C?�C@�C@\)C@��C@C@�HCA{CAQ�CA�\CA�RCA�
CB
=CBQ�CBz�CB��CB��CC
=CCG�CCp�CC�\CCCD  CD33CDp�CD��CDCD�CE�CEQ�CE�\CE��CF  CF33CF\)CF�\CF�CF�CG(�CGp�CG��CG�
CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��HA��TA��`A��TA��mA��`A��`A��TA��HA��HA��mA��yA��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A�ĜA���A�VAʗ�A��A�XA�VAɝ�A�VA���A�AȾwA���A�AȬAȕ�AȃA�~�A�z�A�l�A�jA�bNA�Q�A�G�A�+A��Aǟ�A���A�\)A�I�A�"�A��A��wA�v�A��A�`BA�l�A���A��A�\)A��`A�JA�Q�A��A��9A�5?A��A�JA�=qA��yA�G�A��!A�ĜA��A�=qA���A���A�l�A�E�A���A�$�A���A��\A���A�1'A�;dA�1'A�Q�A�Q�A�bA��^A�VA�G�A�`BA�`BA�dZA|�A|��Az^5AxJAw?}Avz�At�\Aq�
Ao��An��Am�mAlZAh��Ae��Aa��A\�AX�AV�9AU`BAP��AMƨALI�AGl�AD�AA��AAC�AA"�A@ĜA?�A<�A;?}A9x�A7C�A5hsA4�RA4JA3VA133A/��A.(�A,-A+��A*ĜA)��A(�uA'�wA'x�A&�yA$�A$n�A$ �A#��A"M�A!A E�A �A�A|�A��A`BAffA�A�A;dAE�A  A�TA��At�At�A�A7LA~�A�\A�A��A�TAA��A�Ar�A�7AĜA�TA$�A�A��A`BA
��A
ȴA
�AO�At�A?}A�A
�HA
~�A	�AE�A�-A33A��Av�A�9A"�An�AƨA\)A ~�A �DA {@��@�M�@�@��@�ƨ@��H@���@�l�@�F@�?}@�F@�"�@@�~�@��@�=q@��@�bN@�(�@�l�@�ff@���@��`@��@�ƨ@�-@�!@�;d@�K�@陚@��
@���@��@��@�33@�ȴ@��@���@�9X@ޟ�@�hs@�7L@ܬ@�b@�E�@�(�@׶F@��@Չ7@���@ҏ\@�`B@�%@���@�A�@��
@�l�@�"�@�V@��T@́@�A�@˾w@�l�@��@�v�@�7L@�Z@�1@ǝ�@�\)@��@ƸR@��@ř�@��@�(�@å�@�ȴ@�J@�@���@��u@�(�@��@�+@�ȴ@�n�@�$�@��@���@�/@��D@�1@�K�@���@��#@��@�G�@��j@�r�@� �@���@�\)@�33@��y@�^5@�E�@�{@�G�@��j@�j@�b@�t�@��@��@�M�@��@��T@��^@��^@��^@�p�@�G�@��`@���@�Q�@�ƨ@�;d@���@�^5@��-@�?}@��@���@��j@�r�@�1'@���@���@��@���@���@��@�l�@�dZ@�\)@�ȴ@��\@�ff@��@�V@���@���@���@��`@��/@���@�b@���@���@���@���@�hs@�p�@�/@���@�Ĝ@��@�r�@�I�@� �@���@���@���@�S�@��@���@��+@�^5@�^5@��@�@���@�O�@�V@���@��D@�Q�@�A�@�1@��F@��@�K�@��@�5?@��7@�/@��@��@��@�V@���@���@�Z@�1@��@�|�@�K�@�33@��@���@��\@��@��^@���@��7@��@�%@���@�j@�1'@��@��F@�K�@��@��@��!@��@�`B@�%@���@��@��`@��9@�bN@��@��@��;@���@�dZ@�+@���@�~�@�^5@�-@��T@�@���@��7@�`B@�/@��@���@���@��@�9X@�1'@��@�\)@��@��H@���@�v�@�^5@�5?@�-@�-@�$�@���@�x�@�/@��/@�bN@�9X@��m@��P@�t�@�K�@�33@�o@��y@���@�ff@�-@��@��#@��^@���@�hs@�V@���@�  @��w@��F@���@�33@���@���@��R@�n�@�-@�-@��@���@��-@�&�@�V@��/@�I�@�1'@� �@��;@���@��P@�|�@�l�@�K�@�+@��@���@��+@�v�@�ff@�ff@�V@�J@��T@���@�O�@�&�@���@���@�r�@�Q�@�b@�@�P@|�@�P@K�@~ȴ@~V@~5?@~5?@~$�@}@}/@|�D@{ƨ@z�H@z-@y��@yx�@y�@x��@x�@xbN@x  @wl�@w�@vȴ@vv�@vV@v$�@u�-@t�@t�@t(�@s�F@s�@sS�@s@r�\@r^5@q�7@q%@p��@p�9@pb@ol�@o�@n�R@n{@m�-@m�h@m`B@m`B@m?}@m/@l�@lz�@l(�@kƨ@k�@kS�@k"�@j�!@ix�@i&�@h��@h�@hA�@hA�@h �@hb@g�@g\)@f�@f$�@e/@d�/@d�@d��@dj@d1@ct�@c@a��@`��@`r�@_�;@_��@_�P@_\)@_;d@_�@^ȴ@^V@^@]��@\��@\��@[�m@[�F@[��@[�@[t�@[o@Z��@ZM�@Y�#@Y��@Y&�@X�`@X�9@X�@XQ�@X �@W��@V��@V��@Vff@VV@V{@U��@Up�@UV@T�j@TI�@S��@SC�@R��@R^5@Q��@Q�7@QG�@Q&�@P��@P��@PQ�@Pb@O�@O�w@O��@O\)@O�@N��@N$�@M��@M�-@M�@MV@L�/@LI�@K�@J�@J�\@J~�@J^5@J-@JJ@I��@I��@Ix�@I%@HĜ@H��@Hr�@Hb@GK�@Fȴ@F��@FV@F$�@F{@F@F@E�@E��@E`B@D��@D��@Dz�@Dj@D(�@C�
@Ct�@B�H@B�!@B=q@B-@A�#@AX@@��@@�@@bN@@A�@@1'@@ �@@  @?�w@?l�@?+@?�@?
=@>�y@>�@>ff@>{@=@=O�@=V@<j@<Z@<1@;�m@;��@;dZ@;33@:�@:�!@:-@9��@9�^@9�7@9G�@9%@8�`@8�`@8��@8�9@8r�@8A�@7�;@7�w@7��@7�P@7|�@7l�@7;d@6��@6�y@6��@6��@6v�@6E�@5O�@4��@3�m@3C�@3C�@3C�@333@3"�@2��@2�\@2�\@2~�@1�@1hs@0��@0Ĝ@/��@/�P@/l�@/;d@.�@.v�@.$�@.@-�T@-��@-�@-p�@-p�@-`B@-O�@-?}@-/@-�@,�/@,z�@,I�@,1@+�
@+�F@+dZ@+"�@+o@*��@*�!@*=q@)�@)�@)�#@)�^@)��@)�7@)hs@)X@)G�@)&�@)%@(��@(r�@(�u@(�u@(bN@( �@'�;@'�@'��@'K�@'
=@&��@&E�@&{@%�@%��@%`B@$�/@$��@$�D@$Z@$1@#�
@#�@#dZ@#"�@"�H@"�!@"�\@"^5@"=q@"�@!��@!hs@!7L@ �`@ �9@ �u@ Q�@  �@   @�w@�P@�P@�@�y@�y@�R@V@5?@�T@@�h@p�@�@��@��@Z@��@ƨ@ƨ@��@t�@C�@�@��@~�@=q@-@J@��@�^@��@�7@X@%@��@��@Ĝ@��@�u@r�@Q�@ �@�@�w@;d@�@ȴ@��@�+@ff@{@@��@`B@/@��@�@��@�D@1@ƨ@�@S�@@��@��@��@�\@n�@�@��@��@x�@G�@7L@�@%@��@�`@�`@��@1'@  @��@l�@;d@+@�@��@�@�@��@ff@{@�@�T@@��@p�@?}@�/@�@��@j@I�A���A���A���A��
A���A���A��`A��mA��`A��mA��TA��TA��`A��HA��mA��TA��;A��`A��`A��`A��yA��yA��TA��`A��TA��`A��mA��TA��HA��`A��HA��HA��TA��HA��HA��mA��`A��TA��mA��yA��`A��mA��yA��mA��A��yA��mA��A��mA��A��A��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A���A��A��A���A���A��A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�A���A���A�  A���A�  A�  A���A���A���A��A��A��HA��
A���A˼jA˰!A˥�AˁA�;dA���Aʧ�Aʉ7AʅA�r�A�S�A�S�A�S�A�Q�A�XA�XA�Q�A�O�A�I�A�\)A���A��A�(�A� �A�9XA�1'A� �A��A���AʍPA�bNA�C�A�G�A�C�A�=qA�?}A�1'A��A���A��`A���Aɺ^Aɲ-Aɣ�Aə�AɍPA�`BA�1'A��A�1A���A���A���A��A��#A���A�ĜA�ƨA���A�A�AȾwA�A�AȾwAȾwA�AȾwAȾwA���A�AȾwAȾwA�A���AȾwA�ĜA���A�A�ĜA���AȼjAȸRAȬAȟ�Aȝ�Aș�Aȕ�Aț�Aȗ�AȑhAȑhAȍPAȃA�~�AȁA�~�AȁAȅA�~�A�|�AȁA�|�A�z�A�~�A�|�A�v�A�v�A�x�A�p�A�l�A�n�A�jA�jA�n�A�l�A�jA�jA�jA�dZA�ffA�hsA�bNA�^5A�^5A�\)A�S�A�S�A�O�A�I�A�M�A�K�A�G�A�I�A�G�A�?}A�?}A�;dA�1'A�-A�$�A��A��A�VA�A��A��mA��#A��#A���A�ĜA�ƨAǺ^AǮAǮAǛ�AǓuA�~�A�jA�`BA�ZA�G�A�9XA�"�A��A���A�ȴAƺ^Aƥ�AƏ\AƏ\AƍPAƅA�~�A�|�A�jA�^5A�M�A�$�A�%A��`AŸRAş�Ař�AŋDA�n�A�S�A�33A�
=A��A��A��Aġ�A�\)A�9XA�"�A��A�{A�oA�VA�1A�
=A�VA�
=A�%A�%A�A��Aú^A�%A¬A�~�A�ZA��A���A���A�hsA��yA�;dA�E�A�VA��wA�O�A��HA���A���A�A�A��#A��jA��FA���A���A���A���A��7A�=qA�{A���A��yA��A�ȴA�A��hA�v�A�r�A�jA�dZA�^5A�^5A�Q�A�7LA�JA���A��;A�ĜA���A�dZA�Q�A�K�A�C�A�1'A��A��A�1'A���A���A�K�A���A�C�A�
=A�A��A���A��wA��9A���A���A��uA��A�z�A�^5A�=qA�-A���A���A�"�A��A�x�A�hsA�`BA�Q�A�M�A�G�A�=qA�7LA�&�A���A���A��yA���A��-A���A��DA�~�A�t�A�bNA�O�A�Q�A�I�A�E�A�?}A�+A�JA���A��A��yA��A��`A��HA��TA��;A��TA��HA��A��
A���A���A��RA��PA�dZA�K�A��A��PA�n�A�A�A��TA���A�;dA��HA�~�A�Q�A�=qA�+A�1A��A��A��!A��A�ffA�^5A�G�A�1'A��A�bA���A��mA��/A��TA���A�ƨA�ƨA���A�A��wA��^A��A���A���A���A��\A�r�A�\)A���A���A��DA��A��
A��A��A�n�A�A�A�9XA���A���A�z�A�S�A�A��wA�Q�A�1A��A��`A�r�A�=qA�"�A�  A��A��/A�ƨA��FA���A���A��A�x�A�l�A�Q�A� �A�A��A���A���A�r�A�\)A�=qA�A���A���A���A�x�A�O�A��A��jA���A��+A�hsA��A���A�ĜA��wA���A��jA��FA��RA��FA��9A��!A��-A���A�~�A�XA�5?A�"�A��A��PA��A�p�A�dZA�^5A�\)A�Q�A�C�A�5?A��A��A�A�t�A�bNA��`A��A�r�A�G�A�-A�9XA��A��A��TA��#A��A��
A���A��
A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�ƨA�ĜA��RA��A���A��uA��uA��7A�x�A�M�A�VA��A��RA���A��PA�\)A�O�A�A�A�33A�oA���A��yA���A�n�A�  A���A��A��-A��A�S�A�&�A���A���A��hA�O�A��A�O�A��A��DA���A�bA���A�Q�A�?}A�9XA�+A�$�A��A�
=A�A���A��mA�ƨA��RA���A��A�E�A�/A�
=A��#A�|�A�7LA�JA��yA��!A�\)A�  A�x�A�;dA��jA�1'A�(�A�VA���A���A���A���A��uA��hA��A�9XA��mA�ȴA��!A���A���A��A�l�A�VA�C�A�1'A�"�A�bA�%A���A��yA���A��-A��7A�bNA�=qA��A��jA���A�p�A�VA��A��mA��+A�/A�{A�  A���A���A��FA���A���A��DA�x�A�dZA�M�A�7LA�/A� �A�
=A�  A��A��-A�;dA��A�A��yA���A���A��jA���A�|�A�r�A�bNA�A�A�1A��HA��^A��A���A�I�A�9XA��yA��wA��uA�n�A�^5A�Q�A�G�A� �A���A��A��A���A���A���A�t�A�VA�5?A�%A��`A��9A��7A�hsA��A��hA�M�A�9XA�{A�%A��A��/A��
A���A���A��^A���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                    A���A��HA��TA��`A��TA��mA��`A��`A��TA��HA��HA��mA��yA��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A�ĜA���A�VAʗ�A��A�XA�VAɝ�A�VA���A�AȾwA���A�AȬAȕ�AȃA�~�A�z�A�l�A�jA�bNA�Q�A�G�A�+A��Aǟ�A���A�\)A�I�A�"�A��A��wA�v�A��A�`BA�l�A���A��A�\)A��`A�JA�Q�A��A��9A�5?A��A�JA�=qA��yA�G�A��!A�ĜA��A�=qA���A���A�l�A�E�A���A�$�A���A��\A���A�1'A�;dA�1'A�Q�A�Q�A�bA��^A�VA�G�A�`BA�`BA�dZA|�A|��Az^5AxJAw?}Avz�At�\Aq�
Ao��An��Am�mAlZAh��Ae��Aa��A\�AX�AV�9AU`BAP��AMƨALI�AGl�AD�AA��AAC�AA"�A@ĜA?�A<�A;?}A9x�A7C�A5hsA4�RA4JA3VA133A/��A.(�A,-A+��A*ĜA)��A(�uA'�wA'x�A&�yA$�A$n�A$ �A#��A"M�A!A E�A �A�A|�A��A`BAffA�A�A;dAE�A  A�TA��At�At�A�A7LA~�A�\A�A��A�TAA��A�Ar�A�7AĜA�TA$�A�A��A`BA
��A
ȴA
�AO�At�A?}A�A
�HA
~�A	�AE�A�-A33A��Av�A�9A"�An�AƨA\)A ~�A �DA {@��@�M�@�@��@�ƨ@��H@���@�l�@�F@�?}@�F@�"�@@�~�@��@�=q@��@�bN@�(�@�l�@�ff@���@��`@��@�ƨ@�-@�!@�;d@�K�@陚@��
@���@��@��@�33@�ȴ@��@���@�9X@ޟ�@�hs@�7L@ܬ@�b@�E�@�(�@׶F@��@Չ7@���@ҏ\@�`B@�%@���@�A�@��
@�l�@�"�@�V@��T@́@�A�@˾w@�l�@��@�v�@�7L@�Z@�1@ǝ�@�\)@��@ƸR@��@ř�@��@�(�@å�@�ȴ@�J@�@���@��u@�(�@��@�+@�ȴ@�n�@�$�@��@���@�/@��D@�1@�K�@���@��#@��@�G�@��j@�r�@� �@���@�\)@�33@��y@�^5@�E�@�{@�G�@��j@�j@�b@�t�@��@��@�M�@��@��T@��^@��^@��^@�p�@�G�@��`@���@�Q�@�ƨ@�;d@���@�^5@��-@�?}@��@���@��j@�r�@�1'@���@���@��@���@���@��@�l�@�dZ@�\)@�ȴ@��\@�ff@��@�V@���@���@���@��`@��/@���@�b@���@���@���@���@�hs@�p�@�/@���@�Ĝ@��@�r�@�I�@� �@���@���@���@�S�@��@���@��+@�^5@�^5@��@�@���@�O�@�V@���@��D@�Q�@�A�@�1@��F@��@�K�@��@�5?@��7@�/@��@��@��@�V@���@���@�Z@�1@��@�|�@�K�@�33@��@���@��\@��@��^@���@��7@��@�%@���@�j@�1'@��@��F@�K�@��@��@��!@��@�`B@�%@���@��@��`@��9@�bN@��@��@��;@���@�dZ@�+@���@�~�@�^5@�-@��T@�@���@��7@�`B@�/@��@���@���@��@�9X@�1'@��@�\)@��@��H@���@�v�@�^5@�5?@�-@�-@�$�@���@�x�@�/@��/@�bN@�9X@��m@��P@�t�@�K�@�33@�o@��y@���@�ff@�-@��@��#@��^@���@�hs@�V@���@�  @��w@��F@���@�33@���@���@��R@�n�@�-@�-@��@���@��-@�&�@�V@��/@�I�@�1'@� �@��;@���@��P@�|�@�l�@�K�@�+@��@���@��+@�v�@�ff@�ff@�V@�J@��T@���@�O�@�&�@���@���@�r�@�Q�@�b@�@�P@|�@�P@K�@~ȴ@~V@~5?@~5?@~$�@}@}/@|�D@{ƨ@z�H@z-@y��@yx�@y�@x��@x�@xbN@x  @wl�@w�@vȴ@vv�@vV@v$�@u�-@t�@t�@t(�@s�F@s�@sS�@s@r�\@r^5@q�7@q%@p��@p�9@pb@ol�@o�@n�R@n{@m�-@m�h@m`B@m`B@m?}@m/@l�@lz�@l(�@kƨ@k�@kS�@k"�@j�!@ix�@i&�@h��@h�@hA�@hA�@h �@hb@g�@g\)@f�@f$�@e/@d�/@d�@d��@dj@d1@ct�@c@a��@`��@`r�@_�;@_��@_�P@_\)@_;d@_�@^ȴ@^V@^@]��@\��@\��@[�m@[�F@[��@[�@[t�@[o@Z��@ZM�@Y�#@Y��@Y&�@X�`@X�9@X�@XQ�@X �@W��@V��@V��@Vff@VV@V{@U��@Up�@UV@T�j@TI�@S��@SC�@R��@R^5@Q��@Q�7@QG�@Q&�@P��@P��@PQ�@Pb@O�@O�w@O��@O\)@O�@N��@N$�@M��@M�-@M�@MV@L�/@LI�@K�@J�@J�\@J~�@J^5@J-@JJ@I��@I��@Ix�@I%@HĜ@H��@Hr�@Hb@GK�@Fȴ@F��@FV@F$�@F{@F@F@E�@E��@E`B@D��@D��@Dz�@Dj@D(�@C�
@Ct�@B�H@B�!@B=q@B-@A�#@AX@@��@@�@@bN@@A�@@1'@@ �@@  @?�w@?l�@?+@?�@?
=@>�y@>�@>ff@>{@=@=O�@=V@<j@<Z@<1@;�m@;��@;dZ@;33@:�@:�!@:-@9��@9�^@9�7@9G�@9%@8�`@8�`@8��@8�9@8r�@8A�@7�;@7�w@7��@7�P@7|�@7l�@7;d@6��@6�y@6��@6��@6v�@6E�@5O�@4��@3�m@3C�@3C�@3C�@333@3"�@2��@2�\@2�\@2~�@1�@1hs@0��@0Ĝ@/��@/�P@/l�@/;d@.�@.v�@.$�@.@-�T@-��@-�@-p�@-p�@-`B@-O�@-?}@-/@-�@,�/@,z�@,I�@,1@+�
@+�F@+dZ@+"�@+o@*��@*�!@*=q@)�@)�@)�#@)�^@)��@)�7@)hs@)X@)G�@)&�@)%@(��@(r�@(�u@(�u@(bN@( �@'�;@'�@'��@'K�@'
=@&��@&E�@&{@%�@%��@%`B@$�/@$��@$�D@$Z@$1@#�
@#�@#dZ@#"�@"�H@"�!@"�\@"^5@"=q@"�@!��@!hs@!7L@ �`@ �9@ �u@ Q�@  �@   @�w@�P@�P@�@�y@�y@�R@V@5?@�T@@�h@p�@�@��@��@Z@��@ƨ@ƨ@��@t�@C�@�@��@~�@=q@-@J@��@�^@��@�7@X@%@��@��@Ĝ@��@�u@r�@Q�@ �@�@�w@;d@�@ȴ@��@�+@ff@{@@��@`B@/@��@�@��@�D@1@ƨ@�@S�@@��@��@��@�\@n�@�@��@��@x�@G�@7L@�@%@��@�`@�`@��@1'@  @��@l�@;d@+@�@��@�@�@��@ff@{@�@�T@@��@p�@?}@�/@�@��@j@I�A���A���A���A��
A���A���A��`A��mA��`A��mA��TA��TA��`A��HA��mA��TA��;A��`A��`A��`A��yA��yA��TA��`A��TA��`A��mA��TA��HA��`A��HA��HA��TA��HA��HA��mA��`A��TA��mA��yA��`A��mA��yA��mA��A��yA��mA��A��mA��A��A��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A���A��A��A���A���A��A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�A���A���A�  A���A�  A�  A���A���A���A��A��A��HA��
A���A˼jA˰!A˥�AˁA�;dA���Aʧ�Aʉ7AʅA�r�A�S�A�S�A�S�A�Q�A�XA�XA�Q�A�O�A�I�A�\)A���A��A�(�A� �A�9XA�1'A� �A��A���AʍPA�bNA�C�A�G�A�C�A�=qA�?}A�1'A��A���A��`A���Aɺ^Aɲ-Aɣ�Aə�AɍPA�`BA�1'A��A�1A���A���A���A��A��#A���A�ĜA�ƨA���A�A�AȾwA�A�AȾwAȾwA�AȾwAȾwA���A�AȾwAȾwA�A���AȾwA�ĜA���A�A�ĜA���AȼjAȸRAȬAȟ�Aȝ�Aș�Aȕ�Aț�Aȗ�AȑhAȑhAȍPAȃA�~�AȁA�~�AȁAȅA�~�A�|�AȁA�|�A�z�A�~�A�|�A�v�A�v�A�x�A�p�A�l�A�n�A�jA�jA�n�A�l�A�jA�jA�jA�dZA�ffA�hsA�bNA�^5A�^5A�\)A�S�A�S�A�O�A�I�A�M�A�K�A�G�A�I�A�G�A�?}A�?}A�;dA�1'A�-A�$�A��A��A�VA�A��A��mA��#A��#A���A�ĜA�ƨAǺ^AǮAǮAǛ�AǓuA�~�A�jA�`BA�ZA�G�A�9XA�"�A��A���A�ȴAƺ^Aƥ�AƏ\AƏ\AƍPAƅA�~�A�|�A�jA�^5A�M�A�$�A�%A��`AŸRAş�Ař�AŋDA�n�A�S�A�33A�
=A��A��A��Aġ�A�\)A�9XA�"�A��A�{A�oA�VA�1A�
=A�VA�
=A�%A�%A�A��Aú^A�%A¬A�~�A�ZA��A���A���A�hsA��yA�;dA�E�A�VA��wA�O�A��HA���A���A�A�A��#A��jA��FA���A���A���A���A��7A�=qA�{A���A��yA��A�ȴA�A��hA�v�A�r�A�jA�dZA�^5A�^5A�Q�A�7LA�JA���A��;A�ĜA���A�dZA�Q�A�K�A�C�A�1'A��A��A�1'A���A���A�K�A���A�C�A�
=A�A��A���A��wA��9A���A���A��uA��A�z�A�^5A�=qA�-A���A���A�"�A��A�x�A�hsA�`BA�Q�A�M�A�G�A�=qA�7LA�&�A���A���A��yA���A��-A���A��DA�~�A�t�A�bNA�O�A�Q�A�I�A�E�A�?}A�+A�JA���A��A��yA��A��`A��HA��TA��;A��TA��HA��A��
A���A���A��RA��PA�dZA�K�A��A��PA�n�A�A�A��TA���A�;dA��HA�~�A�Q�A�=qA�+A�1A��A��A��!A��A�ffA�^5A�G�A�1'A��A�bA���A��mA��/A��TA���A�ƨA�ƨA���A�A��wA��^A��A���A���A���A��\A�r�A�\)A���A���A��DA��A��
A��A��A�n�A�A�A�9XA���A���A�z�A�S�A�A��wA�Q�A�1A��A��`A�r�A�=qA�"�A�  A��A��/A�ƨA��FA���A���A��A�x�A�l�A�Q�A� �A�A��A���A���A�r�A�\)A�=qA�A���A���A���A�x�A�O�A��A��jA���A��+A�hsA��A���A�ĜA��wA���A��jA��FA��RA��FA��9A��!A��-A���A�~�A�XA�5?A�"�A��A��PA��A�p�A�dZA�^5A�\)A�Q�A�C�A�5?A��A��A�A�t�A�bNA��`A��A�r�A�G�A�-A�9XA��A��A��TA��#A��A��
A���A��
A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�ƨA�ĜA��RA��A���A��uA��uA��7A�x�A�M�A�VA��A��RA���A��PA�\)A�O�A�A�A�33A�oA���A��yA���A�n�A�  A���A��A��-A��A�S�A�&�A���A���A��hA�O�A��A�O�A��A��DA���A�bA���A�Q�A�?}A�9XA�+A�$�A��A�
=A�A���A��mA�ƨA��RA���A��A�E�A�/A�
=A��#A�|�A�7LA�JA��yA��!A�\)A�  A�x�A�;dA��jA�1'A�(�A�VA���A���A���A���A��uA��hA��A�9XA��mA�ȴA��!A���A���A��A�l�A�VA�C�A�1'A�"�A�bA�%A���A��yA���A��-A��7A�bNA�=qA��A��jA���A�p�A�VA��A��mA��+A�/A�{A�  A���A���A��FA���A���A��DA�x�A�dZA�M�A�7LA�/A� �A�
=A�  A��A��-A�;dA��A�A��yA���A���A��jA���A�|�A�r�A�bNA�A�A�1A��HA��^A��A���A�I�A�9XA��yA��wA��uA�n�A�^5A�Q�A�G�A� �A���A��A��A���A���A���A�t�A�VA�5?A�%A��`A��9A��7A�hsA��A��hA�M�A�9XA�{A�%A��A��/A��
A���A���A��^A���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B iBB �B;B �B;B �B;BoBoBB �B �B iB �B �B �B �BBoB�BuB�BBB�BBBSB
�B>BB�@B�^B��BPB��B��B��B�&B�B�B��B��B��B�cB�VB��B��B �B�BB �B;B;BuB�B�B�B�B\B	�B�PB�B�tB��B�^B��B�B�'B��B�_B�B�+B�SB{�Bd�BJ�B-�B'BYB�B��B�`B�BیB�NB�HB�RB��B�bBzDBW�BH�B+6B�BMB
�>B
�B
�&B
ŢB
�LB
��B
��B
t�B
g�B
W�B
R�B
B[B
49B
&LB
�B
�B
\B
{B	�B	��B	�B	�yB	��B	��B	��B	��B	t�B	f2B	_B	TaB	=qB	8B	*0B	B	\B		7B	�B	�B	 iB�%B��B��B�TB�B�gB�TB�BB�dB�OB�B��B�zB��B�'B��B��B�LB�$B��B��B��B�OB�VB�xB�B��B�B��B��B�SB�FB��B�4B�@B�SB��B��B��B��B�=B�wB��B��B�B��B�6B��B��B��B��B��B��B�hB�aB��B��B�<B��B�0B��B��B�
B� B�B�>B�B��B��B��B��B�>B�2B�B�fB�;B��B��B��B�fB�WB�B�B�vB�AB�xB��B�B	AB	GB�VB��B�B��B��B�B��B�lB�B�B�]B	�B	fB	�B		�B	�B	'�B	EmB	K�B	UgB	[�B	bNB	aB	`�B	]�B	]�B	]/B	\]B	]dB	[�B	[�B	\�B	XEB	XEB	VB	T�B	UgB	QB	OBB	P�B	OB	T�B	TaB	T�B	U�B	VmB	W�B	WsB	X�B	Y�B	Z�B	[�B	c�B	a�B	b�B	d�B	e�B	iDB	p�B	s�B	u�B	v+B	w2B	xlB	x�B	|�B	}�B	�B	��B	�_B	��B	��B	��B	�:B	��B	��B	�_B	�7B	�OB	��B	��B	�B	�B	��B	�0B	��B	�wB	��B	��B	�aB	�9B	�B	�B	��B	�RB	��B	��B	�*B	��B	�B	��B	��B	��B	� B	�[B	��B	�-B	�3B	�9B	�KB	ʌB	�^B	��B	��B	�jB	�B	�pB	��B	ϫB	бB	�}B	�vB	�}B	бB	ѷB	҉B	ҽB	�[B	��B	�,B	�gB	��B	�mB	�mB	�mB	�mB	�
B	�sB	רB	�B	�yB	�WB	ݘB	�vB	�BB	�B	�B	�B	�B	��B	�fB	�B	�sB	�B	�B	��B	�WB	��B	�B	��B	��B	� B	�B	�B	�B	��B	��B	��B	�TB	��B	�%B	��B	�ZB	�`B	�+B	�2B	�2B	��B	�lB	��B	�rB	�rB	��B	�PB	�(B	��B
 4B
 �B
;B
oB
oB
uB
B
GB
GB
{B
B
uB
�B
�B
B
GB
�B
�B
�B
_B
_B
�B
+B
�B
	7B
�B
	7B

	B
DB
JB
PB
�B
�B
�B
�B
�B
(B
(B
\B
�B
�B
bB
bB
4B
4B
:B
B
oB
�B
B
�B
�B
B
FB
MB
B
�B
�B
�B
YB
�B
�B
�B
_B
�B
�B
7B
�B
B
�B
CB
B
B
�B
�B
�B
�B
�B
CB
~B
�B
�B
 \B
 �B
 �B
 �B
!-B
!-B
!�B
!�B
"�B
"�B
#:B
#B
#:B
#�B
#�B
#�B
%zB
%FB
%FB
%FB
&�B
&�B
&�B
&�B
'�B
'B
&�B
(�B
(�B
)�B
(�B
(�B
)*B
(�B
($B
(XB
(�B
(�B
(�B
(�B
(�B
)*B
)_B
)�B
*eB
)�B
)�B
)�B
)�B
)�B
*eB
*�B
*eB
+�B
,B
,qB
,�B
-�B
-�B
.B
.�B
/B
/B
.�B
/�B
/�B
0UB
0!B
0!B
/�B
/�B
0UB
0�B
1[B
2�B
2-B
2aB
2�B
33B
33B
33B
2�B
3hB
3hB
33B
3hB
2�B
3hB
4B
4B
5B
4�B
5�B
6B
6�B
7LB
7�B
8B
7�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
:^B
;dB
;�B
<6B
<B
<�B
=B
=<B
=<B
=<B
=<B
=�B
=qB
=�B
=�B
>wB
=�B
=<B
=�B
>wB
?B
?B
?HB
?B
?HB
?}B
?}B
@OB
@�B
A B
A B
A B
A B
A�B
A�B
B[B
D3B
D�B
EB
E�B
E�B
E�B
F?B
F?B
FtB
F�B
GEB
GEB
G�B
HKB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IRB
I�B
JXB
JXB
J�B
J�B
J�B
J�B
K)B
K^B
J�B
J�B
J�B
J�B
K)B
K)B
K�B
K�B
LdB
MB
M6B
M�B
M�B
NB
N�B
N�B
N�B
N�B
OBB
OvB
O�B
O�B
O�B
PB
PB
P}B
P�B
QNB
QNB
Q�B
Q�B
R B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
U2B
UgB
U�B
U�B
VB
VB
VmB
WsB
W�B
W�B
W�B
W�B
WsB
WsB
WsB
WsB
W?B
WsB
W�B
W�B
XB
XB
XEB
XyB
X�B
X�B
YB
Z�B
Z�B
[#B
Z�B
Z�B
ZQB
Z�B
Z�B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
[WB
\)B
\)B
[�B
\�B
]dB
^5B
^jB
^�B
^�B
^�B
_�B
`B
`vB
`�B
`�B
aB
aB
a�B
a�B
a�B
bB
bNB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
e�B
e�B
f2B
f�B
ffB
f�B
ffB
g8B
g8B
gmB
g�B
hsB
h�B
h�B
h>B
h>B
h>B
hsB
h�B
i�B
i�B
iyB
iyB
iyB
i�B
jB
j�B
j�B
jB
jKB
jB
j�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
kB
kQB
k�B
k�B
k�B
k�B
l"B
lWB
l�B
m]B
m�B
m�B
ncB
n�B
n�B
n�B
n�B
n�B
n/B
oiB
o�B
o�B
o�B
pB
p�B
p�B
p�B
qAB
qvB
q�B
q�B
rGB
r|B
r�B
r�B
sB
sMB
sMB
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u%B
uZB
uZB
u�B
v`B
v�B
v�B
w2B
w�B
wfB
w�B
xlB
xlB
x8B
x8B
xB
w�B
x8B
x8B
x�B
x�B
y	B
y	B
yrB
y>B
y>B
y>B
y�B
zxB
z�B
z�B
z�B
z�B
{B
{B
{B
{B
{�B
{B
|PB
|�B
}"B
}VB
}VB
}�B
}�B
~(B
~(B
~�B
~�B
~�B
~�B
~�B
.B
.B
�B
� B
� B
�4B
��B
��B
��B
��B
�B
�oB
�oB
��B
�AB
�uB
�uB
��B
�uB
��B
��B
�uB
��B
�GB
�B
�GB
�{B
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
��B
�%B
��B
�YB
��B
��B
��BMB�BB�B;BB  B �BoB 4B�BoB 4BB  B �BAB  B �B�B  B �BBB;BoB 4B�B�B 4B�BAB iB�B�B  B �B�B �B iB�BB 4BoB 4BB;B
�cBB 4B
��BB iB
��B �B
�.B �BB
��BAB �B;BoB
��B�B
�.B;B �B
��BB 4B �BB
��BoB�B 4B �BoB
��BABB �BB �B 4BBB �B
��BB;B�B�B �BBAB�BBoBB{B�BAB�BoB�B{B�BuB�BB�BuBMBGBuB�B�B�B�BB�B�BB�B�BBBuB�BGB�BMB�BB�BGB�B%B�B�BSBMB�B�B�B�B�B	7B�BuB#:B*�B2�BD3BN<B]dB|�B�VB��B��B�7B�xB�tB��B��B�^B�B�<B��B��B�aBȴB�tB��BB9$BK)BJ#BOBR BUgBh�B|�B� B��B�B��B��B��B�B��B�!B�B��B��B�CB��B��B��B��BȀB��B�9B�B��B��B�B�>B��B��B��B�B�|B�B��B��B�B��B��B�B�`B��B��B�%B��B��B��B�2B�rB�>B�B�.B�]B��B  B��B 4B 4B��B��B�(B�PB��B��B��B�PB��B��B��B��B��B��B �BB��B  B;B��B  BoBB 4B�BGB �B�BuB �B �B�BB iB�B �B��BBoB �B��B�BoB �BAB 4B iBBB �B�B �BB�B �BuBMB�B�B+B	�BfB
=B�B
�B�B	�BxB�BJB�B(B4B4BoB�BBFB�B�B_BFB�B�B{B4B�BoB:BbBB.BoB�BB�B�B�B�B�BVBSB�B�B�B�B�B�B�B�B	�B
	B1B�B�B+B�BMB�B�B�B �B�B�B�B��B�	B�B��B�B��B�B�2B��B�BB�6B՛B�2B�RB��B�gB�9B��B�gBĜB��B��B�gB�B�B�?B�B�9B�3B��B�3B��B�B��B��B��B��B�OB�<B�B�3B�B�qB�qB��B��B��B�9B�hB��B�B�OB��B�0B�9BB��B��B�B��B��B�IB��B��B��B�qB��B��B�eB�B�=B��B��B�IB��B�B��B�RB��B��B��B�OB�OB�bB��B��B�RB��B�4B��B��B��B��B��B��B�~B�OB�B�qB�	B�kB��B��B��B��B�kB��B��B�_B��B�_B��B�MB��B��B�{B��B��B�YB�B��B�'B�$B�VB��B��B�.B�SB��B�B�SB��B�AB�B~(B}�B�AB��BcB��B��B��B�B�+B��B�+B�MB��B�;B�iB��B{�B�iB}VB{B}�B{Bv`Bu�B{�Bu�BrGBkBl"Bi�BZ�B^BaBY�BU2BN<BK�BU�BQ�BGBH�BF�BJ�BC�BC-BG�B>BB2�B.�B-�B-B($B*0B+�B(�B(�B*0B(�B(�B&�B(�B+kB$B!�B"�B#nBCB�BIB�BSB{B�B"B BCB�B�BfB	B BB�B��B�B��B��B�JB�PB�DB�B�	B��B��B �B�ZB�lB��B��B�;B�;B��B�5B��B��B�)B�B��B�B�iB��BߤB��BݘB��B��B�vB��B�5B��B�2B� BуBҽB��B�B�BҽB��B�}B��B��B��B�HB�B�6B��B�HB�vB�vB� B��B�B��B�^B�zB�B�^B�BĜB��B�qB�$B�6B�B�FB��B�B�!B��B�6B�6B��B��B�3B�kB�SB�{B��B�bB� B�DB��B�.B�fB�B{�B��Bv`BoiB`BB[WB[�B]�BU�BYB_BU�BWsBXyB\)BVBT�BRTBK�BM�BHBO�BTaBHB>B<�B?}B@B@OB7�B/�B;�B&�BkB!�B�BYBuB�BBB�B"4B�B�B
�B�B�B+B�B1BSBB�B�BoB
��B
��B
��B iB
�(B
�B
��B
��B
�B
�B
�B
�B
��B
�B
�B
�2B
�B
�B
ٴB
�;B
�QB
��B
ԕB
�mB
�?B
՛B
��B
՛B
�B
уB
͟B
�0B
�6B
��B
�vB
��B
��B
ÖB
�}B
�B
�BB
�wB
��B
��B
�$B
�XB
�XB
�B
�nB
�kB
�6B
��B
��B
��B
��B
��B
��B
��B
�B
�@B
�YB
��B
��B
��B
��B
�B
�xB
�B
�=B
��B
�SB
��B
��B
}�B
~�B
�;B
�AB
x8B
n�B
l�B
l"B
oiB
k�B
iB
gmB
ffB
h�B
e�B
j�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                    B
��B
��B
�VB
�"B
��B
��B
��B
�"B
��B
��B
��B
�VB
�"B
��B
��B
�"B
�"B
�"B
�"B
�VB
��B
�(B
��B
��B
�bB
�bB
�.B
�bB iB�B�B:�B��B��B�NBLdB�B��B�B�vB��B�B�GB�B��B��B��B�	B�B��B�(B�VB��B��B��B��B�B
=B�B�B�B�B��B�`B��B��B��B��B�RB�wB��B��B�nB�{B��Bx7B`�BGB*0B#nB�BMB�B�B��B��B͞B̘BŢB�9B��Bv�BS�BEB'�B!B �B
��B
�B
�vB
��B
��B
��B
��B
qB
c�B
T,B
OB
>�B
0�B
"�B
=B
�B
�B	��B	�B	�B	��B	��B	�3B	��B	�B	�B	p�B	b�B	[WB	P�B	9�B	4mB	&�B	_B	�B	�B	B	 �B��B�uB�JB�2BߤB�mBѷBΤB˒BȴB��B�jB�B��B��B�wB��B�B��B�tB��B�CB�B��B��B��B�_B��B�RB�B��B��B��B��B��B��B��B�B�OB�B�B��B��B��B�!B�RB��B��B��B�)B�B�-B�IB�BB��B��B�KB�B��B�B��B�KB�B�ZB�pB��B�B��B�8B�B�%B�,B�B�B��B�BۋB�B�B�%B�B�B��B�lB��B�B��B�1B�`B��B��B��B�+B�SB��B�B�fB��B��B�SB�`B��B	GB	�B	B	�B	
�B	$@B	A�B	G�B	Q�B	XEB	^�B	]cB	\�B	Y�B	Y�B	YB	X�B	Y�B	XB	XEB	YB	T�B	T�B	RTB	QNB	Q�B	MjB	K�B	M5B	K^B	QNB	P�B	QB	Q�B	R�B	T,B	S�B	T�B	VB	W>B	W�B	`AB	^5B	_;B	`�B	bB	e�B	l�B	p;B	q�B	r{B	s�B	t�B	u%B	y	B	zB	|B	�B	��B	��B	��B	�B	��B	�B	�:B	��B	��B	��B	�!B	�-B	�bB	�hB	�@B	��B	�$B	��B	�<B	�B	��B	��B	�[B	�[B	��B	��B	�9B	�?B	�zB	��B	�RB	�#B	��B	�6B	�pB	��B	�B	�}B	��B	��B	ěB	��B	ǮB	�B	�KB	ɺB	�WB	��B	�)B	��B	�B	��B	��B	��B	�B	�B	��B	�B	ϫB	�BB	�|B	ѷB	� B	ҽB	ҽB	ҽB	ҽB	�ZB	��B	��B	��B	��B	קB	��B	��B	ܒB	�]B	�]B	�cB	��B	�B	�B	��B	��B	��B	�
B	�>B	�B	�B	��B	�JB	�B	�PB	�WB	��B	��B	�5B	�5B	�;B	�B	�AB	�uB	�AB	�B	�B	�{B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�xB	�B	��B	��B	��B	��B	��B	��B	�bB	��B	��B	��B
 iB	��B	��B
 4B	�bB	��B
 4B
 �B
B
�B
�B
GB
{B
B
�B
�B
�B
YB
�B
�B
	�B
	7B
	�B

	B
B
B
xB
xB
�B
CB
IB
�B
�B
�B
�B
�B
VB
�B
'B
\B
�B
'B
bB
�B
�B
nB
B
@B
�B
�B
B
�B
B
�B
�B
B
�B
�B
_B
�B
�B
_B
_B
*B
0B
0B
0B
7B
�B
�B
7B
B
�B
�B
IB
IB
}B
}B
�B
B
�B
!B
�B
UB
�B
�B
�B
 'B
!�B
!�B
!�B
!�B
#B
"�B
#9B
#9B
$B
#nB
#9B
%B
%B
%�B
%FB
$�B
%zB
$�B
$tB
$�B
%B
%FB
%B
%FB
%FB
%zB
%�B
&B
&�B
&B
&B
&B
%�B
&B
&�B
&�B
&�B
'�B
(XB
(�B
)*B
*0B
)�B
*dB
+B
+kB
+kB
+6B
+�B
,<B
,�B
,qB
,qB
,B
,<B
,�B
-BB
-�B
/B
.}B
.�B
/B
/�B
/�B
/�B
/OB
/�B
/�B
/�B
/�B
/OB
/�B
0UB
0UB
1[B
1'B
1�B
2aB
33B
3�B
49B
4mB
49B
5?B
5?B
5B
4�B
5�B
6B
6B
6�B
7�B
8B
8�B
8RB
8�B
9XB
9�B
9�B
9�B
9�B
9�B
9�B
:)B
9�B
:�B
9�B
9�B
:)B
:�B
;dB
;dB
;�B
;dB
;�B
;�B
;�B
<�B
=B
=pB
=pB
=pB
=pB
=�B
>BB
>�B
@�B
@�B
AUB
A�B
B&B
B&B
B�B
B�B
B�B
C,B
C�B
C�B
D3B
D�B
D�B
E9B
E9B
EB
E9B
EB
EB
D�B
E9B
E�B
F
B
F�B
F�B
F�B
F�B
GB
GB
GyB
G�B
GB
GEB
GB
GEB
GyB
GyB
HB
HKB
H�B
IQB
I�B
J#B
J#B
JWB
K)B
K)B
K)B
K)B
K�B
K�B
L/B
L/B
L/B
LdB
LdB
L�B
M5B
M�B
M�B
M�B
NB
NpB
N�B
PB
PHB
QB
QB
QB
QNB
QNB
QNB
QNB
Q�B
Q�B
R B
R B
RTB
RTB
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T,B
T`B
T`B
T�B
T�B
T�B
T�B
U�B
W>B
W
B
WsB
W>B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
W>B
W>B
W>B
W�B
XyB
XyB
XEB
YKB
Y�B
Z�B
Z�B
[#B
Z�B
[#B
[�B
\]B
\�B
\�B
]/B
]cB
]cB
^B
^B
^5B
^iB
^�B
_B
^�B
^�B
^�B
^�B
_;B
_;B
_�B
_�B
`B
`B
`B
_�B
`AB
`AB
`AB
`AB
`B
`AB
`B
aB
aB
aGB
aB
`�B
`�B
`�B
`�B
a|B
bB
bB
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
d%B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
gB
gB
f�B
f�B
f�B
gB
f�B
gB
g8B
gB
g8B
gB
gB
g8B
glB
g�B
g�B
h
B
h
B
h>B
hrB
h�B
iB
i�B
i�B
jB
j�B
j�B
kB
kB
j�B
kB
jB
k�B
l"B
l"B
l"B
lWB
m(B
m(B
m(B
m�B
m�B
m�B
n.B
n�B
n�B
o B
o5B
oiB
o�B
o�B
o�B
o�B
pB
pB
p;B
pB
pB
pB
p;B
poB
p�B
qB
p�B
p�B
qAB
qAB
qB
quB
q�B
q�B
q�B
r�B
r�B
sB
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
tSB
tB
t�B
t�B
u%B
u%B
uYB
uYB
u�B
u�B
u�B
u�B
v+B
v�B
v�B
v�B
w1B
w1B
wfB
wfB
w�B
w�B
xB
w�B
x�B
x�B
yrB
y�B
y�B
y�B
zDB
zxB
zxB
z�B
z�B
{B
{B
{B
{~B
{~B
|B
|PB
|PB
|�B
|�B
|�B
|�B
}"B
}VB
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
�B
bB
�B
�B
� B
� B
� B
� B
�4B
�B
�:B
�:B
�:B
�B
�:B
�:B
��B
�B
�uB
��B
��B
��B
�B
�GB �B �B
�\B
��B
��B
�\B
�PB
�"B
��B
��B
�(B
��B
��B
�\B
�PB
�"B
��B
�PB
�"B
�(B
�PB
�"B
�VB
�VB
��B
��B
��B
��B
�(B
��B
��B
��B
��B
�(B
��B
�PB
�"B
�(B
��B
��B
�(B
�VB
��B
��B
��B
�VB
��B
��B
�\B
��B
�B
�\B
��B
�B
�"B
�~B
�"B
�VB
��B
��B
��B
��B
��B
��B
�(B
�~B
��B
��B
�B
�\B
��B
�"B
�\B
��B
��B
�(B
��B
��B
��B
�B
��B
�VB
��B
�\B
��B
��B
�VB
�\B
��B
�B
�bB
��B
��B
�.B
�"B
�\B
��B
��B
�bB
��B
�\B
��B
�(B
��B
�.B
��B
�.B
��B
��B
��B
�.B
�\B �B
��B �B
��B
��B  B
��B
��B 4B
�\B
��B 4B
�\B  B
�(B
�bB iB
��B �B
��B
��B �B 4B
�bB:B
��B:BuB 4B@B�B �BGB:BBBB�B�B�B�B&�B.�B@�BJ�BY�By	B��B�B��B��B��B��B��B�EB��B�XB��B�)B�<B��B�B��B�>B\B5tBGyBFsBK^BNpBQ�Bd�By	B�PB�4B�\B�.B�B��B�nB�B�qB�bB�B��B��B��B��B� B�B��B�B҉B�gB�8B�B�QB�B�JB�(B�(B��B��B�iB�AB�;B�oB�GB��B�oB�B�B��B�uB��B��B�GB�B��B��B�lB�~B��B�JB�PB��B��B��B�>B�	B�xB��B��B�DB�	B��B�B�7B�1B�	B�B��B��B�VB��B�PB��B��B�PB��B�VB��B�(B��B��B�(B��B��B��B�(B�VB��B�(B��B�B�VB��B��B�B�(B��B�"B��B��B��B�\B�VB��B��B��B�VB�.B�"B��B �B 4BB{B%B�B�B�B+B1B�B�B	7B�B
�BxB�B�B�B�BhB�BLBB�B�B�BB�B�BB�B�B�B\B~B�BBVB!BB
�B
	B
=B
�B�B!B�B1B
	B	B:B:B	7B�BYB�BGB�B{BB �B�B:B��B�"B 4B�B	B��B�YB�B�GB�fB�8B��B�B�BܒBɆB��BтBŢB�9B��B҉B�#B��B��B�&B� B��B�UB�gBӏB�mB��B��B�HB��B��B�gB�B��B�6B�6B��B��B�dB��B�^B��B��B�B��B�B��B��B� B�gB��B�B��B��B��B�EB�<B�jB�B�*B��B��B��B�B��B�FB�B��B�hB��B�B�9B��B�OB�UB�B��B�CB��B�B��B��B��B�CB��B��B�=B��B�OB�B�0B�=B�7B�*B��B��B�RB��B�YB��B�7B��B��B�$B��B�B�LB��B�@B��B��B��B�FB�B��B��B�FB��B�bB��B�wB�tB��B�'B��B�~B��B��B�hB��B� B~�B}VBzxBy�B~�B}�B{�B|�B�B��B�SB�{B�B�{B��B.B}�B|�B.Bx7B|�By�BwfBzBw�Br�BrGBx7BrBn�BglBhrBf2BW>BZQB]cBVBQ�BJ�BHKBQ�BM�BCaBD�BC,BF�B?�B?}BC�B:�B.�B+6B)�B)^B$tB&�B'�B%B%FB&�B%B%B"�B%FB'�B [BOB!B�B�BB�B�B�B�BCB
rBPB�BMB�B�BSBPB iB  B�B��B�B��B��B��B��B��B�YB�	B�7B�"B�B��B�B�GB�B�B�.B�B�JB�JB�yB��B�B��B�B�B��B�B��B�EB�B��B�BBڅB� BтB�pB��B�B�HB�dB�jB�B�<B��B�/B�5B�<B̘B�dBɆB�BB̘B��B��B�pB�?B�WB�?BǮB��B�QBǮB�[B��B�&B��B�tB��B�gB��B�B�gB�qB�6B��B��B��B��B��B��B��B��B��B��B�PB��B�1B�~B��BbBxB�Br�Bk�B\�BW�BXEBZBR BUgB[WBQ�BS�BT�BXyBRTBP�BN�BHBI�BDgBL/BP�BDgB:^B9#B;�B<jB<�B49B,<B7�B#9B�B�B�B�B�B'B\BVBB�B�B
�B+BMBMB{BB�B�B iB
�.B
�.B
��B
��B
��B
�	B
��B
�xB
��B
�B
�MB
�B
� B
��B
�B
�B
�B
� B
�B
�QB
�mB
�B
ۋB
֡B
�&B
��B
ҽB
ӏB
��B
�BB
��B
�dB
��B
��B
ȀB
ɆB
�,B
��B
� B
�B
��B
��B
�^B
��B
��B
��B
�EB
�tB
��B
��B
�aB
��B
��B
��B
�B
�B
��B
��B
��B
�B
�:B
�hB
��B
��B
��B
��B
�B
��B
�_B
��B
�eB
��B
�B
��B
~(B
�:B
zDB
{JB
}�B
~�B
t�B
j�B
h�B
hrB
k�B
h>B
e`B
c�B
b�B
e,B
bB
g8G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721224950                            20230721224950AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122495020230721224950  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495020230721224950QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495020230721224950QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               