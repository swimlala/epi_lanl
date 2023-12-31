CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:34Z creation      
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
_FillValue                 �  [p   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cT   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223234  20230426223234  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�r(9�@�r(9�11  @�����@�����@/'�{�@/'�{��d\���d\��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?�  @�\@E�@}p�@�G�@��@�G�@��RA  A!G�A+�A?\)A_\)A�  A�  A��A�\)A��AϮA�  A�B   B  BQ�BQ�B (�B((�B/�
B7�B?�
BG�
BO�
BX  B_�
Bg�
Bp  Bx  B�{B�  B�  B�{B�  B��
B��B�{B��B�  B�{B��B��B�  B��B��
B�  B�  B�  B�  B�  B�{B�(�B�(�B�  B�{B�{B��
B�  B�  B��
B�C   C=qC  C  C�C	��C  C  C  C��C��C  C
=C��C  C
=C 
=C"{C$
=C&  C(
=C*
=C,  C.  C0
=C2
=C4  C5��C7�C9��C<
=C>  C?��CB  CC��CE��CG��CJ  CL  CN
=CP
=CR
=CT  CU�HCW�CZ  C\
=C^  C_�Ca�Cc�Ce��Cg��Ci�Ck��Cn
=Cp
=Cr
=Ct
=Cu��Cw�Cz
=C|  C~
=C��C���C�C�  C�C�  C���C���C�  C�  C���C�  C�
=C�  C�  C�C�\C�
=C�C�  C���C���C��C��C���C�C�C�
=C�\C�
=C�  C�C���C�  C�
=C�C���C�  C�\C�C���C���C�  C�
=C�C�  C�  C���C���C���C�C�
=C�C�\C�
=C���C���C���C���C���C���C�  C���C���C�C���C���C�  C�C�  C�
=C�  C�  C���C���C�  C�C���C�  C�  C���C��C�  C�
=C�  C�  C�\C�C�  C�C�  C���C�  C�C�C�  C�  C�C�
=C���C���C�C�  C���C���C�C�C�
=C�  C���C�  C�  C�C�C���C���C���C�  C�C���C���C�  C�
=C�C�  C�C�
=C�
=D   D }qD ��Dz�D�qD� DD��D�D� D�D�D�D� D��D}qD  D��D	�D	�D
�D
�D�D}qD  D�D�qD}qD�D� D  D}qD�qD�D  D��D  DxRD�D� D�qD�D  D� D�qD� D  Dz�D  D}qD�Dz�D�D}qD  D�D�qD�D�Dz�D�D��D  Dz�D�qD � D ��D!}qD"  D"��D#�D#� D$�D$��D%�D%� D&  D&� D'�D'�D'�qD(}qD(��D)��D)�RD*}qD+  D+� D,  D,��D-�D-��D.D.��D/D/��D/�qD0xRD0�qD1��D2  D2� D3  D3�D4  D4}qD5  D5z�D5�qD6��D7�D7� D8  D8��D9�D9z�D9�qD:��D;�D;xRD<  D<}qD<�qD=��D>D>�D?�D?� D@  D@� DA�DA� DB  DBz�DC  DC� DC�qDD� DEDE}qDE�qDF� DF�qDG}qDH  DH� DI�DI� DJ�DJ��DK�DK}qDK�qDL� DM  DM� DN  DN� DO  DO}qDO�qDP}qDP�qDQz�DQ�qDR}qDR�qDS� DT  DT}qDT�qDU��DV�DV��DW�DW� DX  DX��DX�qDY� DZ�DZ��DZ�qD[}qD\  D\� D]�D]��D^  D^� D_  D_��D`  D`��Da  Da� Db�Db� Db�qDc� Dd  Dd��Dd�qDe}qDfDf��Df�qDg}qDh  Dh��Di�Di� Di�qDj��Dk�Dk� Dl  Dl}qDm  Dm� Dn  Dn� Do  Do� Do�qDp}qDq�Dq� Dr�Dr�Ds  Ds��Dt�Dt� Dt�qDu}qDu�qDv� Dw  Dw� Dw�qDx}qDy  Dy� Dy��Dz� D{  D{z�D{�qD|� D}�D}}qD}�qD~� D  D� D�HD�@ D��HD�� D�  D�@ D�� D���D�  D�@ D��HD�� D�  D�@ D�� D��HD���D�>�D�~�D���D�  D�@ D��HD��HD���D�>�D��HD�� D�  D�>�D�~�D��HD���D�>�D�� D���D�  D�@ D��HD��HD�  D�=qD�~�D��HD���D�AHD�� D��qD�HD�@ D�� D���D���D�>�D�� D��HD�  D�>�D�� D���D���D�@ D�� D�� D�HD�AHD���D��HD�HD�AHD�~�D�D�  D�>�D�~�D�� D��D�B�D��HD��HD�  D�@ D��HD��HD�HD�B�D�~�D�� D�  D�B�D���D�D�HD�B�D��HD��HD�HD�>�D�|)D�� D�  D�>�D�~�D��HD�  D�@ D�� D��HD�HD�>�D���D���D�HD�@ D�� D��qD���D�=qD�� D��HD�HD�@ D�}qD�� D��D�B�D���D��HD�  D�>�D�~�D���D�  D�B�D��HD��HD��D�AHD��HD���D���D�=qD�~�D�� D���D�>�D��HD��HD�HD�B�D�� D��qD���D�@ D�}qD���D�HD�=qD�� D��HD��qD�AHD��HD�� D�HD�B�D��HD�� D��D�AHD��HD��HD�HD�B�D�}qD���D�  D�<)D�}qD��qD���D�B�D���D���D�  D�AHD�}qD�� D���D�@ D�� D��HD�HD�>�D�� D��HD��D�AHD�� D���D�HD�<)D�}qD��HD�HD�AHD���D�� D�HD�C�D���D��HD�  D�AHD���D�D�HD�@ D�~�D���D�  D�AHD��HD�D���D�@ D�� D�� D���D�@ D��HD�D��D�AHD�~�D���D���D�>�D�~�D���D�HD�AHD�}qD���D�  D�AHD���D��qD���D�@ D�~�D���D�HD�AHD���D��HD�  D�>�D�� D��HD�  D�=qD�}qD���D��qD�@ DHD¾�D��D�>�DÁHDþ�D��D�=qDĀ D��HD�  D�>�DŁHDž�D�  D�@ D�}qDƾ�D���D�AHDǀ D��HD�  D�>�DȁHD��HD�  D�=qD�~�D��HD�  D�AHD�|)D��HD��D�@ D˂�D��HD���D�B�D�~�D�� D���D�@ D�~�D�� D���D�@ D΀ D�� D�  D�@ Dπ D��HD�  D�AHDЀ D�� D�  D�@ Dр D��HD��qD�@ DҀ DҾ�D�HD�@ DӀ D��HD�  D�>�DԀ D�� D�  D�AHD�~�D�D�  D�=qDւ�D־�D�HD�@ D׀ D׾�D�HD�@ D؁HD�� D�HD�AHDق�D�� D��D�>�Dڀ D�� D���D�AHD�~�D��HD�  D�@ D�~�D�� D�  D�@ D݀ D�� D�  D�@ Dނ�D��HD�  D�>�D߀ D�D�HD�AHD�� D��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�AHD�HD�� D��D�AHD�~�D侸D�  D�=qD�HD�D�  D�AHD�HD�� D�HD�@ D�~�D�D���D�@ D�~�D�� D���D�=qD�HD龸D���D�AHD�HD꾸D�HD�>�D�~�D�� D���D�>�D�~�D쾸D���D�@ D�~�D���D��D�B�D� D�qD�  D�AHD� D�qD�  D�AHD�~�D��HD��D�AHD� D�D��D�AHD� D�D�HD�>�D�HD�� D�HD�>�D��D��HD�  D�>�D��HD���D���D�@ D�� D���D�  D�AHD�� D��HD�HD�@ D���D��HD���D�>�D�� D��HD�HD�@ D�~�D�D��=>�G�?L��?���?���@�\@
=@333@O\)@n{@z�H@��@�@��\@�\)@�p�@˅@ٙ�@�ff@��A   A�A
=qA  A
=Ap�A$z�A*�HA0��A7�A=p�AC33AG
=AL��AQ�AW�A]p�Ab�\Ag
=Ak�Ap  Atz�AxQ�A|(�A~�RA���A�=qA��
A�A�\)A�G�A�33A���A�ffA�Q�A��A��A��A��RA�G�A�33A��A��RA�Q�A��A��
A�p�A�\)A�Q�A�=qA���A�ffA���A��HA�p�A��A��A��
A�A�\)A�G�A��HA���AƸRAȣ�A�=qA�(�A�ffA���A�33A��A�\)A�G�A�33A��A�\)AᙚA�(�A�ffA��A��HA�p�A�A�\A�z�A��RA���A�33A�A��B�B{B33B  B��B{B33B(�B	G�B
�\B�
B�B=qB\)BQ�BG�B�B
=B�
B�B{B\)Bz�Bp�B�RB�BQ�BG�B=qB33B (�B!p�B"�RB#�
B$��B%B&�RB'�B(Q�B)p�B*ffB+�B,z�B-��B.�RB0  B1�B2{B3
=B4(�B5G�B5B6�RB7�B8��B9B;33B<(�B=G�B=�B>�RB?�
B@��BA�BC33BDQ�BEp�BF�\BG�BHQ�BIG�BJffBK�BLQ�BM�BO
=BP(�BQG�BQ�BR�HBT  BUG�BV{BW�BX��BYBZ�HB[�
B\z�B]p�B^�\B_�B`��Bb{Bc
=Bd(�BeG�Be�Bf�HBg�
Bh��Bi�Bk33BlQ�BmG�BnffBo�Bp  Bq�Br{Bs
=Bt  Bup�Bv�RBw�Bx��Byp�BzffB{�B|z�B}�B33B�{B���B�33B���B�  B��\B�
=B��B�Q�B��HB��B�  B�Q�B���B�\)B�{B���B�\)B��B�ffB���B�G�B��
B�Q�B���B���B�=qB���B�\)B��B�=qB��HB�p�B�(�B���B��B���B�(�B��RB�G�B�{B��\B��B��B�{B���B��B��B��\B�
=B��B��B��\B��B�B�ffB��HB�\)B��
B���B�33B��B�{B��\B�\)B�  B�z�B��HB�p�B��B��RB�G�B�B�(�B��RB��B�  B��\B��HB�p�B�{B��RB�G�B��B�{B��\B�G�B��B�ffB���B�33B��
B�z�B���B��B��
B�Q�B���B��B�  B�z�B���B�33B��B�=qB���B�\)B��B�{B�z�B��B��B�(�B���B���B�p�B�  B��\B�
=B�\)B��B��\B��B�\)B��
B�ffB��BÙ�B��
B�Q�B��BŮB�  B�z�B�
=BǮB�=qB�z�B�
=B�B�=qBʸRB��BˮB�ffB�
=BͅB�  BΣ�B�\)B��
B�Q�B��HBѮB�=qBң�B�\)B�(�B�ffB�
=B��
B�ffB���B�\)B�(�BظRB��B�B�z�B���BۅB�=qB��HB�G�B��
Bޣ�B�G�Bߙ�B�=qB�
=B�B�{B�\B�p�B�{B�ffB�
=B��
B�ffB���B癚B�Q�B���B�\)B�(�B���B��B�B�\B�33B홚B�(�B�
=BB�  B���B�B��B�\B�\)B�  B�z�B�\)B�{B��\B�G�B�{B�z�B�33B�{B�z�B��B�{B��\B��B�  B�z�B��B��
C �C \)C C ��C(�Cz�C�C�RC  C(�C�C=qCp�CffCz�C��C��C�C�HC��C�C  C(�C(�C=qC\)Cz�Cp�C��C�RC�C�
C��C�C  C(�C(�C33CffCffCp�C��C�RC�C�
C��C�C{C
=C(�CQ�CG�C\)C�\C�C��C��CC�C{C  C(�CG�C=qCp�C�C�C�C��CC��C
=C  C33C33C=qCp�Cp�C�CCC�
C
=C
=C�C\)CffCp�C�CCC	
=C	
=C	�C	\)C	ffC	z�C	�RC	C	��C
{C
�C
\)C
ffC
�\C
��C
��C
=C(�C=qCz�C�C�C�HC�C33C=qCz�C��C�C��C  C=qCQ�C�CCC
=C{C=qC�C��C�HC�C{CffCp�C�RCC  C33CG�C��C��C�C  C�Cp�C�C��C�
C�C33C\)C��C�C��C  CG�C\)C��C�C��C{C=qCz�C�C��C�HC(�C=qC\)C�CC  C{C\)Cp�C�C�
C  C=qCG�C��C��C��C  CQ�CffC��CC
=C�Cp�C�C�
C�C=qCQ�C��C�RC  C�CffC�C��C�HC(�CQ�C�\CC�HC33C=qC��C�C��C
=CffCz�C��C�HC33CQ�C��C�RC   C �C p�C �C �
C ��C!=qC!\)C!�C!C"{C"33C"p�C"��C"�HC#  C#G�C#ffC#�RC#�
C$(�C$=qC$�\C$��C$��C%{C%ffC%z�C%��C%�C&=qC&\)C&�C&�
C&�C'33C'Q�C'��C'�RC(
=C((�C(p�C(�\C(�
C(��C)=qC)ffC)�C)�RC*{C*=qC*ffC*�RC*C+�C+33C+z�C+�C+�
C,�C,G�C,�C,�C,��C-{C-p�C-�\C-C-��C.=qC.z�C.��C.�C/  C/Q�C/z�C/��C/�HC033C0\)C0�\C0�HC1  C1G�C1ffC1�RC1�C2{C2ffC2z�C2��C2�C3=qC3\)C3��C3��C4�C433C4�C4��C4��C5�C5Q�C5��C5�RC6
=C633C6z�C6��C6��C7
=C7Q�C7�\C7�RC8{C833C8�C8�C9
=C9�C9p�C9�C9�HC:(�C:Q�C:��C:C;(�C;=qC;�\C;�
C;��C<Q�C<p�C<��C<�C==qC=�C=��C>  C>(�C>�C>��C>�HC?33C?\)C?�RC?�
C@(�C@ffC@�\C@�CA{CAp�CA��CA��CB(�CBG�CB��CB��CC(�CCQ�CC�\CC�HCD
=CDffCD�CD�CE{CEffCE��CE��CF(�CF\)CF�RCF�CG�CGz�CG�CH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114114114111411411141141141141141114114141141141141141141141141141411411411411111141411111111111114111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                ?�  @�\@E�@}p�@�G�@��@�G�@��RA  A!G�A+�A?\)A_\)A�  A�  A��A�\)A��AϮA�  A�B   B  BQ�BQ�B (�B((�B/�
B7�B?�
BG�
BO�
BX  B_�
Bg�
Bp  Bx  B�{B�  B�  B�{B�  B��
B��B�{B��B�  B�{B��B��B�  B��B��
B�  B�  B�  B�  B�  B�{B�(�B�(�B�  B�{B�{B��
B�  B�  B��
B�C   C=qC  C  C�C	��C  C  C  C��C��C  C
=C��C  C
=C 
=C"{C$
=C&  C(
=C*
=C,  C.  C0
=C2
=C4  C5��C7�C9��C<
=C>  C?��CB  CC��CE��CG��CJ  CL  CN
=CP
=CR
=CT  CU�HCW�CZ  C\
=C^  C_�Ca�Cc�Ce��Cg��Ci�Ck��Cn
=Cp
=Cr
=Ct
=Cu��Cw�Cz
=C|  C~
=C��C���C�C�  C�C�  C���C���C�  C�  C���C�  C�
=C�  C�  C�C�\C�
=C�C�  C���C���C��C��C���C�C�C�
=C�\C�
=C�  C�C���C�  C�
=C�C���C�  C�\C�C���C���C�  C�
=C�C�  C�  C���C���C���C�C�
=C�C�\C�
=C���C���C���C���C���C���C�  C���C���C�C���C���C�  C�C�  C�
=C�  C�  C���C���C�  C�C���C�  C�  C���C��C�  C�
=C�  C�  C�\C�C�  C�C�  C���C�  C�C�C�  C�  C�C�
=C���C���C�C�  C���C���C�C�C�
=C�  C���C�  C�  C�C�C���C���C���C�  C�C���C���C�  C�
=C�C�  C�C�
=C�
=D   D }qD ��Dz�D�qD� DD��D�D� D�D�D�D� D��D}qD  D��D	�D	�D
�D
�D�D}qD  D�D�qD}qD�D� D  D}qD�qD�D  D��D  DxRD�D� D�qD�D  D� D�qD� D  Dz�D  D}qD�Dz�D�D}qD  D�D�qD�D�Dz�D�D��D  Dz�D�qD � D ��D!}qD"  D"��D#�D#� D$�D$��D%�D%� D&  D&� D'�D'�D'�qD(}qD(��D)��D)�RD*}qD+  D+� D,  D,��D-�D-��D.D.��D/D/��D/�qD0xRD0�qD1��D2  D2� D3  D3�D4  D4}qD5  D5z�D5�qD6��D7�D7� D8  D8��D9�D9z�D9�qD:��D;�D;xRD<  D<}qD<�qD=��D>D>�D?�D?� D@  D@� DA�DA� DB  DBz�DC  DC� DC�qDD� DEDE}qDE�qDF� DF�qDG}qDH  DH� DI�DI� DJ�DJ��DK�DK}qDK�qDL� DM  DM� DN  DN� DO  DO}qDO�qDP}qDP�qDQz�DQ�qDR}qDR�qDS� DT  DT}qDT�qDU��DV�DV��DW�DW� DX  DX��DX�qDY� DZ�DZ��DZ�qD[}qD\  D\� D]�D]��D^  D^� D_  D_��D`  D`��Da  Da� Db�Db� Db�qDc� Dd  Dd��Dd�qDe}qDfDf��Df�qDg}qDh  Dh��Di�Di� Di�qDj��Dk�Dk� Dl  Dl}qDm  Dm� Dn  Dn� Do  Do� Do�qDp}qDq�Dq� Dr�Dr�Ds  Ds��Dt�Dt� Dt�qDu}qDu�qDv� Dw  Dw� Dw�qDx}qDy  Dy� Dy��Dz� D{  D{z�D{�qD|� D}�D}}qD}�qD~� D  D� D�HD�@ D��HD�� D�  D�@ D�� D���D�  D�@ D��HD�� D�  D�@ D�� D��HD���D�>�D�~�D���D�  D�@ D��HD��HD���D�>�D��HD�� D�  D�>�D�~�D��HD���D�>�D�� D���D�  D�@ D��HD��HD�  D�=qD�~�D��HD���D�AHD�� D��qD�HD�@ D�� D���D���D�>�D�� D��HD�  D�>�D�� D���D���D�@ D�� D�� D�HD�AHD���D��HD�HD�AHD�~�D�D�  D�>�D�~�D�� D��D�B�D��HD��HD�  D�@ D��HD��HD�HD�B�D�~�D�� D�  D�B�D���D�D�HD�B�D��HD��HD�HD�>�D�|)D�� D�  D�>�D�~�D��HD�  D�@ D�� D��HD�HD�>�D���D���D�HD�@ D�� D��qD���D�=qD�� D��HD�HD�@ D�}qD�� D��D�B�D���D��HD�  D�>�D�~�D���D�  D�B�D��HD��HD��D�AHD��HD���D���D�=qD�~�D�� D���D�>�D��HD��HD�HD�B�D�� D��qD���D�@ D�}qD���D�HD�=qD�� D��HD��qD�AHD��HD�� D�HD�B�D��HD�� D��D�AHD��HD��HD�HD�B�D�}qD���D�  D�<)D�}qD��qD���D�B�D���D���D�  D�AHD�}qD�� D���D�@ D�� D��HD�HD�>�D�� D��HD��D�AHD�� D���D�HD�<)D�}qD��HD�HD�AHD���D�� D�HD�C�D���D��HD�  D�AHD���D�D�HD�@ D�~�D���D�  D�AHD��HD�D���D�@ D�� D�� D���D�@ D��HD�D��D�AHD�~�D���D���D�>�D�~�D���D�HD�AHD�}qD���D�  D�AHD���D��qD���D�@ D�~�D���D�HD�AHD���D��HD�  D�>�D�� D��HD�  D�=qD�}qD���D��qD�@ DHD¾�D��D�>�DÁHDþ�D��D�=qDĀ D��HD�  D�>�DŁHDž�D�  D�@ D�}qDƾ�D���D�AHDǀ D��HD�  D�>�DȁHD��HD�  D�=qD�~�D��HD�  D�AHD�|)D��HD��D�@ D˂�D��HD���D�B�D�~�D�� D���D�@ D�~�D�� D���D�@ D΀ D�� D�  D�@ Dπ D��HD�  D�AHDЀ D�� D�  D�@ Dр D��HD��qD�@ DҀ DҾ�D�HD�@ DӀ D��HD�  D�>�DԀ D�� D�  D�AHD�~�D�D�  D�=qDւ�D־�D�HD�@ D׀ D׾�D�HD�@ D؁HD�� D�HD�AHDق�D�� D��D�>�Dڀ D�� D���D�AHD�~�D��HD�  D�@ D�~�D�� D�  D�@ D݀ D�� D�  D�@ Dނ�D��HD�  D�>�D߀ D�D�HD�AHD�� D��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�AHD�HD�� D��D�AHD�~�D侸D�  D�=qD�HD�D�  D�AHD�HD�� D�HD�@ D�~�D�D���D�@ D�~�D�� D���D�=qD�HD龸D���D�AHD�HD꾸D�HD�>�D�~�D�� D���D�>�D�~�D쾸D���D�@ D�~�D���D��D�B�D� D�qD�  D�AHD� D�qD�  D�AHD�~�D��HD��D�AHD� D�D��D�AHD� D�D�HD�>�D�HD�� D�HD�>�D��D��HD�  D�>�D��HD���D���D�@ D�� D���D�  D�AHD�� D��HD�HD�@ D���D��HD���D�>�D�� D��HD�HD�@ D�~�D�D��=>�G�?L��?���?���@�\@
=@333@O\)@n{@z�H@��@�@��\@�\)@�p�@˅@ٙ�@�ff@��A   A�A
=qA  A
=Ap�A$z�A*�HA0��A7�A=p�AC33AG
=AL��AQ�AW�A]p�Ab�\Ag
=Ak�Ap  Atz�AxQ�A|(�A~�RA���A�=qA��
A�A�\)A�G�A�33A���A�ffA�Q�A��A��A��A��RA�G�A�33A��A��RA�Q�A��A��
A�p�A�\)A�Q�A�=qA���A�ffA���A��HA�p�A��A��A��
A�A�\)A�G�A��HA���AƸRAȣ�A�=qA�(�A�ffA���A�33A��A�\)A�G�A�33A��A�\)AᙚA�(�A�ffA��A��HA�p�A�A�\A�z�A��RA���A�33A�A��B�B{B33B  B��B{B33B(�B	G�B
�\B�
B�B=qB\)BQ�BG�B�B
=B�
B�B{B\)Bz�Bp�B�RB�BQ�BG�B=qB33B (�B!p�B"�RB#�
B$��B%B&�RB'�B(Q�B)p�B*ffB+�B,z�B-��B.�RB0  B1�B2{B3
=B4(�B5G�B5B6�RB7�B8��B9B;33B<(�B=G�B=�B>�RB?�
B@��BA�BC33BDQ�BEp�BF�\BG�BHQ�BIG�BJffBK�BLQ�BM�BO
=BP(�BQG�BQ�BR�HBT  BUG�BV{BW�BX��BYBZ�HB[�
B\z�B]p�B^�\B_�B`��Bb{Bc
=Bd(�BeG�Be�Bf�HBg�
Bh��Bi�Bk33BlQ�BmG�BnffBo�Bp  Bq�Br{Bs
=Bt  Bup�Bv�RBw�Bx��Byp�BzffB{�B|z�B}�B33B�{B���B�33B���B�  B��\B�
=B��B�Q�B��HB��B�  B�Q�B���B�\)B�{B���B�\)B��B�ffB���B�G�B��
B�Q�B���B���B�=qB���B�\)B��B�=qB��HB�p�B�(�B���B��B���B�(�B��RB�G�B�{B��\B��B��B�{B���B��B��B��\B�
=B��B��B��\B��B�B�ffB��HB�\)B��
B���B�33B��B�{B��\B�\)B�  B�z�B��HB�p�B��B��RB�G�B�B�(�B��RB��B�  B��\B��HB�p�B�{B��RB�G�B��B�{B��\B�G�B��B�ffB���B�33B��
B�z�B���B��B��
B�Q�B���B��B�  B�z�B���B�33B��B�=qB���B�\)B��B�{B�z�B��B��B�(�B���B���B�p�B�  B��\B�
=B�\)B��B��\B��B�\)B��
B�ffB��BÙ�B��
B�Q�B��BŮB�  B�z�B�
=BǮB�=qB�z�B�
=B�B�=qBʸRB��BˮB�ffB�
=BͅB�  BΣ�B�\)B��
B�Q�B��HBѮB�=qBң�B�\)B�(�B�ffB�
=B��
B�ffB���B�\)B�(�BظRB��B�B�z�B���BۅB�=qB��HB�G�B��
Bޣ�B�G�Bߙ�B�=qB�
=B�B�{B�\B�p�B�{B�ffB�
=B��
B�ffB���B癚B�Q�B���B�\)B�(�B���B��B�B�\B�33B홚B�(�B�
=BB�  B���B�B��B�\B�\)B�  B�z�B�\)B�{B��\B�G�B�{B�z�B�33B�{B�z�B��B�{B��\B��B�  B�z�B��B��
C �C \)C C ��C(�Cz�C�C�RC  C(�C�C=qCp�CffCz�C��C��C�C�HC��C�C  C(�C(�C=qC\)Cz�Cp�C��C�RC�C�
C��C�C  C(�C(�C33CffCffCp�C��C�RC�C�
C��C�C{C
=C(�CQ�CG�C\)C�\C�C��C��CC�C{C  C(�CG�C=qCp�C�C�C�C��CC��C
=C  C33C33C=qCp�Cp�C�CCC�
C
=C
=C�C\)CffCp�C�CCC	
=C	
=C	�C	\)C	ffC	z�C	�RC	C	��C
{C
�C
\)C
ffC
�\C
��C
��C
=C(�C=qCz�C�C�C�HC�C33C=qCz�C��C�C��C  C=qCQ�C�CCC
=C{C=qC�C��C�HC�C{CffCp�C�RCC  C33CG�C��C��C�C  C�Cp�C�C��C�
C�C33C\)C��C�C��C  CG�C\)C��C�C��C{C=qCz�C�C��C�HC(�C=qC\)C�CC  C{C\)Cp�C�C�
C  C=qCG�C��C��C��C  CQ�CffC��CC
=C�Cp�C�C�
C�C=qCQ�C��C�RC  C�CffC�C��C�HC(�CQ�C�\CC�HC33C=qC��C�C��C
=CffCz�C��C�HC33CQ�C��C�RC   C �C p�C �C �
C ��C!=qC!\)C!�C!C"{C"33C"p�C"��C"�HC#  C#G�C#ffC#�RC#�
C$(�C$=qC$�\C$��C$��C%{C%ffC%z�C%��C%�C&=qC&\)C&�C&�
C&�C'33C'Q�C'��C'�RC(
=C((�C(p�C(�\C(�
C(��C)=qC)ffC)�C)�RC*{C*=qC*ffC*�RC*C+�C+33C+z�C+�C+�
C,�C,G�C,�C,�C,��C-{C-p�C-�\C-C-��C.=qC.z�C.��C.�C/  C/Q�C/z�C/��C/�HC033C0\)C0�\C0�HC1  C1G�C1ffC1�RC1�C2{C2ffC2z�C2��C2�C3=qC3\)C3��C3��C4�C433C4�C4��C4��C5�C5Q�C5��C5�RC6
=C633C6z�C6��C6��C7
=C7Q�C7�\C7�RC8{C833C8�C8�C9
=C9�C9p�C9�C9�HC:(�C:Q�C:��C:C;(�C;=qC;�\C;�
C;��C<Q�C<p�C<��C<�C==qC=�C=��C>  C>(�C>�C>��C>�HC?33C?\)C?�RC?�
C@(�C@ffC@�\C@�CA{CAp�CA��CA��CB(�CBG�CB��CB��CC(�CCQ�CC�\CC�HCD
=CDffCD�CD�CE{CEffCE��CE��CF(�CF\)CF�RCF�CG�CGz�CG�CH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114114114111411411141141141141141114114141141141141141141141141141411411411411111141411111111111114111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA��mA��A��A��A��A��A��A��A��A��A��A��A���A���A��A��A��mA��yA��A��
A��A��A��/A��HA��
A̾wA̸RA̲-Ạ�A̟�A̧�A̧�A̧�A̧�A̰!A̲-A̮A̬A̩�A̩�A̬A̩�A̧�Ḁ�A̧�A̧�Ḁ�Ạ�A̟�A̴9A���A̮Aˉ7A�9XA�r�A�v�A��yA��hA���A���A�ƨA�oA��+A�ƨA��9A�+A��\A���A�;dA�Q�A��`A�1A�S�A�A�$�A��RA�ƨA���A�-A|AsO�Al�Ai?}Afz�Ab�!A_�TA\��AV-AS�hAQ
=AN�AHr�AEO�AC��AB�+AAO�A?t�A;�mA:^5A:�DA:v�A:bA9��A8z�A5�FA3�mA3��A2z�A1O�A0^5A/"�A-�
A,JA*��A(�A%ƨA%��A%dZA$A�A#�PA"��A �/AC�A�A�-AA��AA�
A\)A��A7LA�
A��AoAS�A�A��A-AJA�A �A�wA�AVA��AS�Ar�A�wAS�A�A
��A
�uA
�\A
�\A
�A
��A
��A
�`A
�A
��A
ĜA
�jA
��A	��A	�FA	�A�!A��A�AffAVA(�A�A�A��A�7Ax�Ap�Ap�A`BAG�A�A�A�A��AAffA�;A?}AoA%A�`AZA�-AbNA7LAZA;dA �`A $�@��-@�t�@��u@���@�{@�hs@��m@�"�@�+@�S�@�\)@�\)@�S�@�R@��@�j@� �@���@��T@�9X@땁@��@��@�X@�V@��@��@�w@�t�@�V@��@�v�@�@��@���@���@��@�r�@�n�@��@�x�@���@�A�@���@ۅ@ڧ�@�^5@�-@�J@٩�@�V@أ�@�A�@ם�@�t�@�"�@�-@�J@�@Ցh@���@ԛ�@��@�t�@Ұ!@ҏ\@�ff@��@�&�@���@мj@Ϯ@�S�@�+@��y@�ȴ@�v�@�`B@̓u@�bN@�A�@�1'@�1@�S�@ʗ�@��@Ɂ@��`@ț�@� �@��m@�l�@Ɵ�@��@�&�@Ĵ9@�Q�@��@�b@���@Õ�@�;d@�ȴ@�v�@�v�@�v�@�v�@�-@���@�Ĝ@�A�@���@��@�;d@�ȴ@��^@���@���@�l�@���@��@��@���@�M�@��#@���@�p�@�%@�j@��@���@�+@��@���@���@�p�@�?}@��@��`@�z�@��;@�|�@�C�@�"�@��R@�J@�hs@��@��9@���@�Z@��;@���@�K�@��@�ff@���@���@��-@�x�@��@�Q�@��m@��w@�\)@�o@���@�$�@���@��^@�`B@��@��/@�j@���@�t�@�33@��@��R@�^5@���@�X@�?}@��@�Ĝ@��@��@�9X@�1'@� �@��;@�S�@���@��\@��#@�p�@�%@��@��u@�bN@��@�  @��@��w@���@�\)@�33@��@�o@��H@��!@���@��+@�ff@�@���@�hs@�&�@���@�bN@�9X@��@���@���@�ƨ@���@��@���@�M�@�{@���@��#@���@���@�X@���@�Q�@�b@��
@�dZ@�@�ff@�J@���@�`B@��@���@��D@�I�@�1'@�  @��;@��F@��@�\)@�+@���@��R@�~�@�=q@��@�@��h@�G�@�%@���@��@�I�@�(�@��@��P@�\)@�;d@���@���@��\@�ff@�M�@�=q@�{@�@�x�@�X@��u@�z�@�j@�bN@�Q�@�I�@� �@���@�
=@�ȴ@���@��\@�v�@�V@���@�X@��/@���@��@�bN@��@��m@��@�K�@���@���@�~�@�E�@�J@�x�@�?}@�V@��@�A�@��@��F@��P@�l�@�K�@��@��H@��R@�ff@�M�@�=q@�$�@��@���@��-@���@��7@��@��@��u@�Q�@�A�@� �@��@|�@K�@;d@+@~��@~v�@~$�@}@}p�@}V@|j@{C�@z=q@y�@y�^@y�7@yhs@x��@x��@x  @w��@w�@w\)@v��@v{@u�T@u�h@uV@t��@t�D@t�@sdZ@r�@r~�@rJ@q�^@q��@q7L@pĜ@pĜ@p�9@pbN@p1'@o�;@o�P@o\)@n�y@nv�@n5?@m��@l��@lZ@l(�@l�@k�m@k��@k�@kS�@j~�@i�#@i%@hĜ@h�9@g�w@f��@fȴ@fV@f@e�T@e?}@d�@c@b��@b-@a�^@a7L@`��@`bN@`b@_�;@_�w@_�w@_��@_;d@_
=@^��@^�@^ȴ@^�R@^��@^V@^@]O�@\9X@[�
@[��@[�@[S�@[o@Z��@Z~�@Z=q@Y�^@Yx�@Yx�@YX@YG�@Y7L@Y%@X�@X  @W|�@V��@V$�@U�@U�-@U��@U�@U�@U/@T��@T�D@Tj@TI�@T�@S�
@S�@R�H@RJ@Qx�@P�9@PQ�@O��@O;d@N�@NV@M�@M�T@M�T@M��@M��@M/@L�j@L��@Lj@K33@J�H@J��@I�^@H�9@Hr�@HQ�@HA�@H1'@Hb@H  @H  @G��@G��@G\)@GK�@G;d@F��@F{@E�T@Ep�@Dz�@Ct�@A��@A�^@A�7@A7L@@�u@@  @?�@?l�@?�@>ȴ@>V@=p�@=/@=?}@=/@=�@<�/@<��@<�@<z�@<I�@;��@;��@;dZ@;"�@:�!@:~�@:�@9�@9G�@8r�@7�;@7�@7�@7�;@7��@7��@7��@7��@7��@7��@7��@7�P@6��@5�@5�h@5/@4�/@4��@3ƨ@3��@3S�@2��@2M�@1��@1��@17L@0�u@0bN@0bN@0bN@01'@0  @/��@/�@/l�@/\)@/
=@.��@.E�@-��@-O�@-V@,I�@,1@+�m@+�
@+�
@+��@+�@*�H@*n�@*J@)��@)��@)�7@)X@)G�@)G�@)7L@)%@(�@(Q�@(A�@( �@'�@'��@'��@'�@'l�@'+@&�+@%��@%�-@%?}@$�j@$��@$z�@$I�@$�@$1@#�m@#�m@#�
@#�m@#�
@#�
@#ƨ@#�F@#��@#�@#t�@#C�@#"�@"��@"�!@"^5@!�^@!G�@!&�@!�@ ��@ �`@ Ĝ@ bN@   @�@�;@�w@\)@�y@��@ff@@@�h@�@`B@O�@/@�@V@�@�/@�j@�@z�@9X@ƨ@��@t�@33@�@��@�\@~�@^5@��@��@��@�7@x�@hs@7L@��@��@�u@bN@bN@Q�@Q�@1'@b@�@�@�;@�;@��@�P@K�@�@��@V@�@@p�@O�@?}@/@�@V@��@Z@9X@(�@��@ƨ@�F@��@��@C�@"�@�@�!@�@�#@��@��@��@X@Ĝ@��@��@A�@ �@�@�w@|�@+@+@
=@��@��@��@�R@v�@ff@V@5?@$�@{@�@�h@`B@O�@/@��@�/@�@�D@(�@�@�m@�F@�@t�@t�@S�@C�@C�@C�@33@"�@"�@@
�@
��@
��@
�\@
^5@
-@	��@	��@	��@	�@	�#@	�7@	�7@	x�@	X@	7L@��@�9@�9@�u@bN@1'@ �@  @  @�;@��@�w@��@|�@|�@l�@\)@\)A��TA��HA��mA��TA��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A���A���A���A���A���A���A���A���A��mA��mA��mA��mA��mA��`A��`A��`A��`A��mA��A��yA��mA��TA��A��A��A��yA��mA��A���A���A���A��HA��A��
A��
A��
A���A���A���A��
A���A���A���A��
A��A��A��#A��;A��;A��;A��
A��
A��
A��#A��A��#A��A��A��A��;A��HA��TA��TA��HA��TA��TA��TA��;A��;A��;A��HA��;A��#A��#A��/A���A���A���A���A̺^A̺^A̺^A̾wA̼jA̼jA̼jA̼jA̸RA̶FA̸RA̴9A̶FA̲-A̲-A̲-A̶FA̶FA̰!A̴9A̬A̩�A̧�Ḁ�A̡�Ạ�Ḁ�A̡�Ạ�A̟�A̝�A̛�A̝�A̟�A̡�Ạ�Ạ�A̧�A̧�Ḁ�Ḁ�Ḁ�A̧�A̧�A̩�A̬A̩�A̧�A̧�Ḁ�Ḁ�A̧�A̩�A̩�A̩�A̩�A̩�Ḁ�Ḁ�Ḁ�Ḁ�A̧�A̩�A̩�A̬A̩�A̩�A̩�A̩�A̩�A̬A̰!A̴9A̴9A̶FA̴9A̴9A̮A̰!A̰!A̲-A̲-A̴9A̲-A̰!A̮A̬A̬A̩�A̬A̮A̮A̮A̮A̬A̩�A̧�A̩�A̬A̬A̮A̬A̩�A̧�A̧�A̧�A̩�A̩�A̬A̬A̬A̬A̩�A̩�A̩�A̩�A̬A̮A̮A̬A̩�A̩�A̬A̬A̩�A̩�A̧�Ḁ�Ḁ�Ḁ�A̧�A̧�A̩�A̧�Ạ�Ạ�Ạ�A̧�A̧�A̧�Ḁ�Ạ�Ạ�A̧�A̧�A̧�Ḁ�A̩�A̬A̬A̩�A̧�Ḁ�A̧�A̩�A̩�A̧�Ạ�Ḁ�Ḁ�A̧�A̧�A̡�A̡�Ạ�Ḁ�Ạ�A̟�A̟�A̡�Ạ�Ạ�A̡�A̟�A̛�A̝�A̟�A̡�A̡�Ḁ�A̧�A̴9A���A���A�A�A�A�ƨA�ȴA���A���A���A���A���A���A���A���A���A̺^A̬Ả7A�O�A�/A���A���A���A˟�A� �A��A��A�"�A�"�A���A��mA�hsAɰ!A�n�A�%A���Aȕ�A��HA�n�A�5?A��/Aư!AƝ�A�l�A�
=A�ZAľwAĉ7AÓuA��TA�&�A��A�JA��mA��/A���A��A�dZA�A���A�x�A�p�A�bA�
=A��A���A�1'A���A���A���A��uA�z�A�bNA��A��HA�`BA� �A��`A�dZA�(�A��#A���A�ffA��A��#A��A�7LA��A���A��jA��!A���A��DA�O�A�9XA��A�l�A�/A���A�=qA��A�O�A��;A��DA�/A���A�VA�5?A�;dA���A�`BA�K�A��A��+A�A���A�33A��#A��9A��A�G�A� �A��A�ȴA��!A���A��A�p�A�`BA�Q�A�M�A�=qA�=qA�7LA�"�A�%A���A��`A��!A�|�A�?}A���A��^A��+A�O�A�A��jA�z�A�E�A��A���A���A�bNA�A��-A�S�A��A�bA��A��/A��uA���A���A�n�A�5?A��jA��TA���A�ȴA��wA��!A��hA�v�A�S�A�/A��A��PA�`BA�;dA�oA��A��!A��+A�^5A�;dA�{A���A��TA��^A���A�`BA�A���A�l�A� �A��A���A��FA��!A���A���A���A���A��+A�I�A�
=A��`A�ȴA��A�bNA�S�A�A��wA��A�hsA���A��/A���A�x�A�{A��hA��A���A��A�Q�A��#A�ĜA��FA���A��7A�dZA��A��jA�~�A���A��^A��!A�x�A�K�A��A�ƨA�ƨA�jA���A�?}A��A���A��A��A�`BA�VA�I�A�(�A�
=A�ĜA�p�A�1'A�{A��/A���A��A�S�A�&�A�
=A��/A�n�A�
=A�$�A��/A�S�A��RA�K�A���A��!A�z�A��
A��A��uA�t�A�VA�K�A��A��A���A���A���A���A���A�n�A�33A��HA��!A�v�A�`BA�C�A�$�A�1A�ȴA��\A�1'A��A`BA~�jA~9XA}l�A|��A{��A{x�A{�Az��Ay��AxĜAx(�Av�AvQ�Au�As�AsAr�RArI�Aq�wAq�Aq�ApI�AodZAn��Am�AmG�AlVAlAk��Ak7LAj�/Aj��Aj$�Ai�TAi��Ai�7Ail�Ai�Ah�yAh��AhVAh9XAg�
Ag�7Ag+Ag�Afv�Af{Ae�Ae��Ae�hAet�Ae"�AdAc��Ac|�Ab�\AbE�Aa�AaVA`�A`ȴA`��A`�DA`A�A` �A`1A_�A_x�A_C�A_+A^��A^�!A^v�A]��A\�yA[��A[��A[?}AZ��AZ��AZI�AYS�AW��AV~�AU;dAT~�ATffAT$�AT�AS�#ASAS�^AS�FAS�^AS��AS��AS�hASdZASO�AR�AR5?AQ�^AQC�AQVAP�AP�RAP��APr�API�AO��AO�mAO��AOANv�AN  AMO�AL�ALI�AK7LAJn�AI�wAIhsAH�yAH�AG��AGhsAG?}AG
=AF��AF �AE�FAE|�AE;dAE�AD��AD��AD��ADz�ADZADE�AD-AC�AC��AC�7ACl�ACS�AC33AC
=AB��AB��AB�DABz�ABr�ABQ�AB-ABJAA��AA��AA�AAt�AAO�AA+A@��A@�A@�jA@�DA@$�A?�mA?��A?dZA?�A>��A>E�A=�A<^5A<(�A<$�A<{A;ƨA;t�A;K�A;G�A:��A:�A:�A:bNA:9XA:�A: �A:-A:E�A:VA:ffA:�\A:�\A:��A:��A:��A:�RA:��A:ĜA:ĜA:��A:=qA: �A:�A:bA:�A:bA:bA:bA:1A:JA:A:JA:A9��A9��A9�A9��A9�A9�A9�A9�A9��A9G�A8^5A7�TA7��A7;dA6�A6��A6jA61'A5��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                A��HA��mA��A��A��A��A��A��A��A��A��A��A��A���A���A��A��A��mA��yA��A��
A��A��A��/A��HA��
A̾wA̸RA̲-Ạ�A̟�A̧�A̧�A̧�A̧�A̰!A̲-A̮A̬A̩�A̩�A̬A̩�A̧�Ḁ�A̧�A̧�Ḁ�Ạ�A̟�A̴9A���A̮Aˉ7A�9XA�r�A�v�A��yA��hA���A���A�ƨA�oA��+A�ƨA��9A�+A��\A���A�;dA�Q�A��`A�1A�S�A�A�$�A��RA�ƨA���A�-A|AsO�Al�Ai?}Afz�Ab�!A_�TA\��AV-AS�hAQ
=AN�AHr�AEO�AC��AB�+AAO�A?t�A;�mA:^5A:�DA:v�A:bA9��A8z�A5�FA3�mA3��A2z�A1O�A0^5A/"�A-�
A,JA*��A(�A%ƨA%��A%dZA$A�A#�PA"��A �/AC�A�A�-AA��AA�
A\)A��A7LA�
A��AoAS�A�A��A-AJA�A �A�wA�AVA��AS�Ar�A�wAS�A�A
��A
�uA
�\A
�\A
�A
��A
��A
�`A
�A
��A
ĜA
�jA
��A	��A	�FA	�A�!A��A�AffAVA(�A�A�A��A�7Ax�Ap�Ap�A`BAG�A�A�A�A��AAffA�;A?}AoA%A�`AZA�-AbNA7LAZA;dA �`A $�@��-@�t�@��u@���@�{@�hs@��m@�"�@�+@�S�@�\)@�\)@�S�@�R@��@�j@� �@���@��T@�9X@땁@��@��@�X@�V@��@��@�w@�t�@�V@��@�v�@�@��@���@���@��@�r�@�n�@��@�x�@���@�A�@���@ۅ@ڧ�@�^5@�-@�J@٩�@�V@أ�@�A�@ם�@�t�@�"�@�-@�J@�@Ցh@���@ԛ�@��@�t�@Ұ!@ҏ\@�ff@��@�&�@���@мj@Ϯ@�S�@�+@��y@�ȴ@�v�@�`B@̓u@�bN@�A�@�1'@�1@�S�@ʗ�@��@Ɂ@��`@ț�@� �@��m@�l�@Ɵ�@��@�&�@Ĵ9@�Q�@��@�b@���@Õ�@�;d@�ȴ@�v�@�v�@�v�@�v�@�-@���@�Ĝ@�A�@���@��@�;d@�ȴ@��^@���@���@�l�@���@��@��@���@�M�@��#@���@�p�@�%@�j@��@���@�+@��@���@���@�p�@�?}@��@��`@�z�@��;@�|�@�C�@�"�@��R@�J@�hs@��@��9@���@�Z@��;@���@�K�@��@�ff@���@���@��-@�x�@��@�Q�@��m@��w@�\)@�o@���@�$�@���@��^@�`B@��@��/@�j@���@�t�@�33@��@��R@�^5@���@�X@�?}@��@�Ĝ@��@��@�9X@�1'@� �@��;@�S�@���@��\@��#@�p�@�%@��@��u@�bN@��@�  @��@��w@���@�\)@�33@��@�o@��H@��!@���@��+@�ff@�@���@�hs@�&�@���@�bN@�9X@��@���@���@�ƨ@���@��@���@�M�@�{@���@��#@���@���@�X@���@�Q�@�b@��
@�dZ@�@�ff@�J@���@�`B@��@���@��D@�I�@�1'@�  @��;@��F@��@�\)@�+@���@��R@�~�@�=q@��@�@��h@�G�@�%@���@��@�I�@�(�@��@��P@�\)@�;d@���@���@��\@�ff@�M�@�=q@�{@�@�x�@�X@��u@�z�@�j@�bN@�Q�@�I�@� �@���@�
=@�ȴ@���@��\@�v�@�V@���@�X@��/@���@��@�bN@��@��m@��@�K�@���@���@�~�@�E�@�J@�x�@�?}@�V@��@�A�@��@��F@��P@�l�@�K�@��@��H@��R@�ff@�M�@�=q@�$�@��@���@��-@���@��7@��@��@��u@�Q�@�A�@� �@��@|�@K�@;d@+@~��@~v�@~$�@}@}p�@}V@|j@{C�@z=q@y�@y�^@y�7@yhs@x��@x��@x  @w��@w�@w\)@v��@v{@u�T@u�h@uV@t��@t�D@t�@sdZ@r�@r~�@rJ@q�^@q��@q7L@pĜ@pĜ@p�9@pbN@p1'@o�;@o�P@o\)@n�y@nv�@n5?@m��@l��@lZ@l(�@l�@k�m@k��@k�@kS�@j~�@i�#@i%@hĜ@h�9@g�w@f��@fȴ@fV@f@e�T@e?}@d�@c@b��@b-@a�^@a7L@`��@`bN@`b@_�;@_�w@_�w@_��@_;d@_
=@^��@^�@^ȴ@^�R@^��@^V@^@]O�@\9X@[�
@[��@[�@[S�@[o@Z��@Z~�@Z=q@Y�^@Yx�@Yx�@YX@YG�@Y7L@Y%@X�@X  @W|�@V��@V$�@U�@U�-@U��@U�@U�@U/@T��@T�D@Tj@TI�@T�@S�
@S�@R�H@RJ@Qx�@P�9@PQ�@O��@O;d@N�@NV@M�@M�T@M�T@M��@M��@M/@L�j@L��@Lj@K33@J�H@J��@I�^@H�9@Hr�@HQ�@HA�@H1'@Hb@H  @H  @G��@G��@G\)@GK�@G;d@F��@F{@E�T@Ep�@Dz�@Ct�@A��@A�^@A�7@A7L@@�u@@  @?�@?l�@?�@>ȴ@>V@=p�@=/@=?}@=/@=�@<�/@<��@<�@<z�@<I�@;��@;��@;dZ@;"�@:�!@:~�@:�@9�@9G�@8r�@7�;@7�@7�@7�;@7��@7��@7��@7��@7��@7��@7��@7�P@6��@5�@5�h@5/@4�/@4��@3ƨ@3��@3S�@2��@2M�@1��@1��@17L@0�u@0bN@0bN@0bN@01'@0  @/��@/�@/l�@/\)@/
=@.��@.E�@-��@-O�@-V@,I�@,1@+�m@+�
@+�
@+��@+�@*�H@*n�@*J@)��@)��@)�7@)X@)G�@)G�@)7L@)%@(�@(Q�@(A�@( �@'�@'��@'��@'�@'l�@'+@&�+@%��@%�-@%?}@$�j@$��@$z�@$I�@$�@$1@#�m@#�m@#�
@#�m@#�
@#�
@#ƨ@#�F@#��@#�@#t�@#C�@#"�@"��@"�!@"^5@!�^@!G�@!&�@!�@ ��@ �`@ Ĝ@ bN@   @�@�;@�w@\)@�y@��@ff@@@�h@�@`B@O�@/@�@V@�@�/@�j@�@z�@9X@ƨ@��@t�@33@�@��@�\@~�@^5@��@��@��@�7@x�@hs@7L@��@��@�u@bN@bN@Q�@Q�@1'@b@�@�@�;@�;@��@�P@K�@�@��@V@�@@p�@O�@?}@/@�@V@��@Z@9X@(�@��@ƨ@�F@��@��@C�@"�@�@�!@�@�#@��@��@��@X@Ĝ@��@��@A�@ �@�@�w@|�@+@+@
=@��@��@��@�R@v�@ff@V@5?@$�@{@�@�h@`B@O�@/@��@�/@�@�D@(�@�@�m@�F@�@t�@t�@S�@C�@C�@C�@33@"�@"�@@
�@
��@
��@
�\@
^5@
-@	��@	��@	��@	�@	�#@	�7@	�7@	x�@	X@	7L@��@�9@�9@�u@bN@1'@ �@  @  @�;@��@�w@��@|�@|�@l�@\)@\)A��TA��HA��mA��TA��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A���A���A���A���A���A���A���A���A��mA��mA��mA��mA��mA��`A��`A��`A��`A��mA��A��yA��mA��TA��A��A��A��yA��mA��A���A���A���A��HA��A��
A��
A��
A���A���A���A��
A���A���A���A��
A��A��A��#A��;A��;A��;A��
A��
A��
A��#A��A��#A��A��A��A��;A��HA��TA��TA��HA��TA��TA��TA��;A��;A��;A��HA��;A��#A��#A��/A���A���A���A���A̺^A̺^A̺^A̾wA̼jA̼jA̼jA̼jA̸RA̶FA̸RA̴9A̶FA̲-A̲-A̲-A̶FA̶FA̰!A̴9A̬A̩�A̧�Ḁ�A̡�Ạ�Ḁ�A̡�Ạ�A̟�A̝�A̛�A̝�A̟�A̡�Ạ�Ạ�A̧�A̧�Ḁ�Ḁ�Ḁ�A̧�A̧�A̩�A̬A̩�A̧�A̧�Ḁ�Ḁ�A̧�A̩�A̩�A̩�A̩�A̩�Ḁ�Ḁ�Ḁ�Ḁ�A̧�A̩�A̩�A̬A̩�A̩�A̩�A̩�A̩�A̬A̰!A̴9A̴9A̶FA̴9A̴9A̮A̰!A̰!A̲-A̲-A̴9A̲-A̰!A̮A̬A̬A̩�A̬A̮A̮A̮A̮A̬A̩�A̧�A̩�A̬A̬A̮A̬A̩�A̧�A̧�A̧�A̩�A̩�A̬A̬A̬A̬A̩�A̩�A̩�A̩�A̬A̮A̮A̬A̩�A̩�A̬A̬A̩�A̩�A̧�Ḁ�Ḁ�Ḁ�A̧�A̧�A̩�A̧�Ạ�Ạ�Ạ�A̧�A̧�A̧�Ḁ�Ạ�Ạ�A̧�A̧�A̧�Ḁ�A̩�A̬A̬A̩�A̧�Ḁ�A̧�A̩�A̩�A̧�Ạ�Ḁ�Ḁ�A̧�A̧�A̡�A̡�Ạ�Ḁ�Ạ�A̟�A̟�A̡�Ạ�Ạ�A̡�A̟�A̛�A̝�A̟�A̡�A̡�Ḁ�A̧�A̴9A���A���A�A�A�A�ƨA�ȴA���A���A���A���A���A���A���A���A���A̺^A̬Ả7A�O�A�/A���A���A���A˟�A� �A��A��A�"�A�"�A���A��mA�hsAɰ!A�n�A�%A���Aȕ�A��HA�n�A�5?A��/Aư!AƝ�A�l�A�
=A�ZAľwAĉ7AÓuA��TA�&�A��A�JA��mA��/A���A��A�dZA�A���A�x�A�p�A�bA�
=A��A���A�1'A���A���A���A��uA�z�A�bNA��A��HA�`BA� �A��`A�dZA�(�A��#A���A�ffA��A��#A��A�7LA��A���A��jA��!A���A��DA�O�A�9XA��A�l�A�/A���A�=qA��A�O�A��;A��DA�/A���A�VA�5?A�;dA���A�`BA�K�A��A��+A�A���A�33A��#A��9A��A�G�A� �A��A�ȴA��!A���A��A�p�A�`BA�Q�A�M�A�=qA�=qA�7LA�"�A�%A���A��`A��!A�|�A�?}A���A��^A��+A�O�A�A��jA�z�A�E�A��A���A���A�bNA�A��-A�S�A��A�bA��A��/A��uA���A���A�n�A�5?A��jA��TA���A�ȴA��wA��!A��hA�v�A�S�A�/A��A��PA�`BA�;dA�oA��A��!A��+A�^5A�;dA�{A���A��TA��^A���A�`BA�A���A�l�A� �A��A���A��FA��!A���A���A���A���A��+A�I�A�
=A��`A�ȴA��A�bNA�S�A�A��wA��A�hsA���A��/A���A�x�A�{A��hA��A���A��A�Q�A��#A�ĜA��FA���A��7A�dZA��A��jA�~�A���A��^A��!A�x�A�K�A��A�ƨA�ƨA�jA���A�?}A��A���A��A��A�`BA�VA�I�A�(�A�
=A�ĜA�p�A�1'A�{A��/A���A��A�S�A�&�A�
=A��/A�n�A�
=A�$�A��/A�S�A��RA�K�A���A��!A�z�A��
A��A��uA�t�A�VA�K�A��A��A���A���A���A���A���A�n�A�33A��HA��!A�v�A�`BA�C�A�$�A�1A�ȴA��\A�1'A��A`BA~�jA~9XA}l�A|��A{��A{x�A{�Az��Ay��AxĜAx(�Av�AvQ�Au�As�AsAr�RArI�Aq�wAq�Aq�ApI�AodZAn��Am�AmG�AlVAlAk��Ak7LAj�/Aj��Aj$�Ai�TAi��Ai�7Ail�Ai�Ah�yAh��AhVAh9XAg�
Ag�7Ag+Ag�Afv�Af{Ae�Ae��Ae�hAet�Ae"�AdAc��Ac|�Ab�\AbE�Aa�AaVA`�A`ȴA`��A`�DA`A�A` �A`1A_�A_x�A_C�A_+A^��A^�!A^v�A]��A\�yA[��A[��A[?}AZ��AZ��AZI�AYS�AW��AV~�AU;dAT~�ATffAT$�AT�AS�#ASAS�^AS�FAS�^AS��AS��AS�hASdZASO�AR�AR5?AQ�^AQC�AQVAP�AP�RAP��APr�API�AO��AO�mAO��AOANv�AN  AMO�AL�ALI�AK7LAJn�AI�wAIhsAH�yAH�AG��AGhsAG?}AG
=AF��AF �AE�FAE|�AE;dAE�AD��AD��AD��ADz�ADZADE�AD-AC�AC��AC�7ACl�ACS�AC33AC
=AB��AB��AB�DABz�ABr�ABQ�AB-ABJAA��AA��AA�AAt�AAO�AA+A@��A@�A@�jA@�DA@$�A?�mA?��A?dZA?�A>��A>E�A=�A<^5A<(�A<$�A<{A;ƨA;t�A;K�A;G�A:��A:�A:�A:bNA:9XA:�A: �A:-A:E�A:VA:ffA:�\A:�\A:��A:��A:��A:�RA:��A:ĜA:ĜA:��A:=qA: �A:�A:bA:�A:bA:bA:bA:1A:JA:A:JA:A9��A9��A9�A9��A9�A9�A9�A9�A9��A9G�A8^5A7�TA7��A7;dA6�A6��A6jA61'A5��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�oB	��B	��B	�;B	�B	�;B	�;B	�oB	�B	�oB	�oB	�oB	�oB	�;B	�oB	��B	�oB	�B	�cB	�5B	�]B	�]B	�)B	�)B	�B	�)B	�B	�B	��B	�B	�B	�;B	�oB	�B	� B	��B	�lB	�>B	�xB	��B
 iB
;B
uB
{B
�B
�B
1B
fB
	B

	B
eB
/�B
S[B
��B
��B
�6B
�|B
�2B
�aB
ȴB
� B
��B
�wB
��B
�B
��B
d&B
C�B
9�B
1[B
&B
B
 iB	��B	�NB	�+B	��B
�B	�fB	��B	� B	��B	�(B	{JB	p;B	e�B	T�B	JXB	<�B	(XB	B	�B	�B	�B	GB	 �B��B��B�B��B��B	(B	B	B	 �B	($B	*�B	<B	7�B	-CB	!�B	!�B	#nB	CB	
�B	DB	  B�cB��B��B�rB��B�]B	B	B	7B	"hB	#:B	&�B	)�B	1'B	7B	4�B	*�B	�B	
=B	 B	+B	�B	eB	VB	!�B	#�B	#:B	$@B	(�B	2�B	K�B	S�B	S[B	R�B	R B	V9B	a�B	e�B	h>B	kB	l�B	ncB	t�B	{B	}�B	�B	�B	�{B	��B	�B	�~B	�B	��B	��B	�!B	�nB	�FB	�zB	��B	�B	�B	��B	��B	��B	�*B	��B	�*B	�qB	��B	��B	��B
�B
1B
MB
�B
�B
 iB	�(B	��B	��B	��B	ݘB	�}B	�^B	ƨB	��B	��B	��B	�LB	�B	��B	�FB	��B	��B	�eB	�B	��B	�wB	�=B	��B	��B	��B	��B	�}B	�IB	��B	��B	�'B	�UB	�B	��B	��B	��B	��B	�kB	��B	�nB	�-B	�hB	�tB	��B	�[B	��B	��B	�}B	�wB	�B	�IB	�}B	��B	��B	�hB	��B	��B	��B	��B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	�B	��B	ĜB	ÖB	��B	�B	��B	ȀB	�B	��B	��B	�RB	�B	�B	��B	�aB	�aB	�,B	��B	��B	��B	��B	֡B	�mB	רB	�B	�B	��B	ٴB	�#B	��B	�]B	�B	��B	�B	�5B	�jB	�jB	�pB	�B	�vB	��B	�pB	��B	�B	�BB	�HB	�NB	��B	�B	�B	�NB	�B	�&B	�B	��B	�B	�fB	�2B	��B	�fB	�8B	�sB	�8B	�>B	��B	�B	�B	�B	�WB	��B	��B	��B	��B	�cB	��B	�cB	� B	�B	��B	��B	�B	�vB	�B	�B	�B	�B	�MB	�B	�MB	�B	�B	�B	��B	�+B	�`B	�`B	��B	�fB	��B	�>B	�rB	�B	�B	�B	��B	��B	�VB	��B	��B	�]B
 iB
 �B
;B
B
B
AB
�B
SB
�B
SB
SB
%B
�B
�B
�B
�B
�B
YB
�B
�B
	�B

�B

�B
�B
�B
B
~B
�B
�B
PB
PB
PB
"B
"B
"B
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
B
�B
�B
B
oB
�B
:B
�B
uB
{B
B
B
B
�B
MB
MB
SB
�B
$B
YB
�B
1B
7B
kB
7B
	B
�B
CB
�B
�B
B
IB
�B
B
�B
�B
B
B
�B
�B
OB
�B
VB
�B
OB
!B
�B
 'B
 'B
 'B
!-B
!bB
!�B
!�B
"hB
"�B
"hB
"�B
#B
#:B
"�B
$B
$@B
$�B
%�B
%zB
%�B
%FB
%�B
%�B
&B
&�B
(XB
'�B
($B
'�B
'�B
'RB
(�B
)_B
(�B
)�B
)�B
)�B
*�B
*�B
+�B
,B
,�B
-CB
,�B
-B
-CB
.}B
.�B
.}B
/�B
/OB
/�B
0UB
0UB
0�B
0�B
0�B
1�B
1[B
2aB
2�B
2�B
2�B
3hB
3hB
3hB
3hB
3�B
3hB
5?B
5?B
5�B
5tB
5?B
6B
6zB
6B
6zB
6�B
6�B
7�B
7�B
8B
8�B
8�B
9�B
:^B
;0B
;0B
;�B
<B
<B
<jB
<�B
=B
=B
<jB
=�B
=<B
=qB
<�B
=qB
>BB
>�B
>�B
?}B
@B
@B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
AUB
A�B
B'B
B[B
A�B
B[B
B[B
B�B
C-B
C�B
C�B
C�B
D3B
DgB
D�B
DgB
D�B
D�B
E�B
EB
E9B
EmB
D�B
F�B
FtB
FtB
GB
GzB
GB
G�B
IB
I�B
IRB
J#B
JXB
JXB
K)B
K^B
J�B
K^B
K�B
K�B
K�B
K�B
L0B
K�B
L0B
K�B
L0B
K�B
LdB
K�B
LdB
NpB
NB
NB
NB
NpB
N�B
N�B
N�B
OB
P}B
PHB
O�B
O�B
O�B
O�B
OBB
O�B
P}B
QNB
RTB
Q�B
RTB
Q�B
R B
RTB
Q�B
R�B
S&B
R�B
R�B
R�B
S&B
S&B
R�B
T,B
T�B
T�B
U�B
U2B
VmB
W
B
V�B
XB
W�B
W�B
WsB
W�B
W�B
XB
XB
WsB
W�B
YKB
X�B
YB
[#B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\)B
[�B
[�B
[�B
]/B
]dB
\�B
\�B
^�B
_pB
`BB
_�B
_pB
`BB
a�B
a|B
a�B
a|B
bB
bB
b�B
d&B
c�B
c�B
c�B
c�B
d&B
c�B
c�B
d&B
d&B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
dZB
e`B
f�B
hsB
g�B
g�B
g�B
h
B
h>B
h
B
h
B
h
B
g�B
g�B
h
B
h�B
iDB
iB
iDB
iyB
iDB
i�B
i�B
i�B
jKB
jKB
j�B
j�B
kQB
k�B
l"B
k�B
k�B
l"B
l�B
l�B
l�B
m)B
l�B
m]B
m)B
m�B
m�B
m�B
n/B
n�B
o5B
n�B
o5B
n�B
oiB
o B
o�B
poB
poB
p�B
qB
p�B
qAB
qAB
p�B
qB
qB
rB
qvB
rB
q�B
rB
rGB
rGB
rGB
rGB
r�B
sMB
s�B
s�B
tB
t�B
t�B
t�B
u%B
uZB
t�B
uZB
u%B
uZB
t�B
u�B
u%B
u�B
uZB
u�B
u%B
u%B
u�B
v+B
v+B
u�B
v+B
wfB
wfB
wfB
xB
w�B
wfB
xB
xB
xlB
w�B
x8B
x�B
yrB
y	B
y>B
y�B
zDB
z�B
zxB
zDB
{B
zxB
z�B
z�B
z�B
{B
z�B
{B
{B
{B
{B
|�B
{�B
{�B
|�B
|�B
|�B
}"B
|�B
}VB
}�B
~(B
}�B
}�B
~(B
}�B
~]B
~(B
~�B
~�B
.B
~�B
~�B
~�B
~�B
cB
�B
�B
�B
�B
� B
� B
� B
�B
�4B
�iB
�;B
�;B
��B
��B
��B
�AB
��B
��B
�uB
�B
�GB
�GB
�B
��B
�GB
��B
�{B
��B
��B
��B
�MB
��B
�B
��B
�SB
��B
��B
��B
�YB
��B
��B
�%B
��B
�+B
�+B
��B
�_B
��B
��B
��B
�_B
��B
�1B
��B
�fB
�1B
�fB
�fB
��B
�B
�lB
�B
�lB
�lB
��B
��B
��B
��B
��B
��B
��B
�DB
�xB
�xB
��B
��B
��B
��B
��B
��B
�xB
�JB
��B
�JB
��B
��B
�~B
��B
��B
�B
��B
�B
��B
��B
�B
�B
��B
��B
��B
�VB
�"B
��B
��B
�(B
�\B
�\B
�(B
�(B
�(B
��B
�(B
�\B
��B
�.B
��B
��B	�cB	��B	� B	�;B	�/B	�;B	�oB	�B	�B	�oB	�;B	�cB	��B	�iB	�iB	�oB	�B	��B	�AB	�B	�B	��B	�5B	�iB	�B	�;B	�AB	�AB	�AB	�vB	�AB	��B	�iB	�B	�oB	�iB	�B	�B	�B	�oB	�oB	�B	�AB	�B	�;B	�;B	�B	�;B	��B	�oB	�B	�;B	�B	�B	�B	�iB	�B	�;B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�oB	�B	�iB	�B	�iB	�iB	�B	�B	�B	�B	�iB	��B	�B	��B	�cB	�B	�)B	�)B	��B	��B	�B	�B	�B	�B	��B	�B	��B	��B	�/B	��B	�TB	�)B	��B	��B	�)B	�B	�]B	��B	��B	��B	��B	�]B	��B	�/B	�/B	��B	��B	�"B	�"B	�"B	�)B	��B	��B	�)B	�B	��B	�)B	�)B	�B	�)B	�B	�B	��B	�)B	�)B	�/B	� B	� B	�]B	��B	�]B	�B	��B	�B	�cB	�mB	��B	�B	�B	��B	�B	�yB	�yB	�B	�yB	�B	�B	�QB	�]B	�WB	�WB	�B	�B	�B	�B	�B	�KB	�B	�QB	�B	�B	��B	�B	�KB	�KB	�B	�B	�"B	�B	�"B	�"B	�B	�B	�B	�B	�QB	�AB	�B	�B	�B	�B	�oB	�5B	��B	�B	�oB	�B	�B	�vB	��B	�B	�B	�cB	�5B	�iB	� B	�B	��B	��B	� B	��B	��B	�cB	�cB	��B	��B	�B	�|B	��B	��B	��B	�`B	�+B	��B	�lB	��B	�rB	�>B	��B	�B	�fB	�B	��B	�rB	�DB	�B	��B	��B	�rB	�>B	�>B	�>B	�DB	�B	��B	��B	�B	��B	��B	�JB	�VB
 iB
 �B
 iB
  B
 �B	��B	�cB	��B
 4B
oB
;B
�B
�B
B
 �B
B
�B
�B
�B
B
uB
�B
AB
B
�B
MB
MB
B
�B
�B
�B
SB
SB
�B
�B
�B
�B
�B
%B
�B
B
MB
YB
1B
+B
YB
+B
fB
	B
	B
1B
_B
�B
�B
	B
�B
�B
�B
_B
�B
	�B
	B
1B
	7B

	B

	B
�B
fB
fB
�B
	B
	lB
B
B
�B
B
�B
�B
�B
�B
"hB
'B
)�B
+kB
,=B
+�B
+kB
0�B
2�B
6zB
;�B
@�B
@�B
A B
F?B
QNB
Z�B
gmB
xlB
��B
�"B
��B
�4B
��B
��B
��B
�VB
�SB
�SB
��B
��B
��B
��B
��B
��B
��B
�3B
ѷB
��B
�}B
уB
ѷB
��B
�?B
�2B
��B
�<B
�KB
�B�B
�2B
уB
�mB
�B
� B
��B
��B
�B
��B
�9B
��B
ΥB
ҽB
ʌB
��B
�HB
ԕB
�dB
�OB
�B
�B
�OB
�HB
�zB
�wB
��B
�*B
�BB
�aB
�RB
��B
�UB
��B
�^B
��B
�UB
��B
��B
�'B
��B
�$B
��B
�@B
�B
�B
��B
��B
��B
�3B
�	B
�	B
��B
��B
��B
�B
z�B
�B
�DB
�aB
m�B
��B
yrB
�fB
b�B
\]B
U�B
M�B
GzB
B�B
A�B
C�B
?�B
?HB
;�B
8RB
9XB
8B
6B
4�B
1�B
2aB
2aB
.B
0�B
1[B
1'B
*�B
)�B
0UB
/�B
,=B
/�B
%�B
&�B
 �B
"�B
!bB
"hB
~B
�B
�B
"4B
�B
"4B
qB
33B
-wB
!B
�B
DB
�B
�B
�B
�B
	�B
�B
:B
�B

rB
B

�B
�B

�B
JB
	�B
PB
�B
	7B
�B
YB
+B
YB
�B
�B
B
�B
oB
{B
�B
�B
�B
�B
�B
 4B
AB	��B	��B	��B	�2B	��B	�fB	�TB	��B	�+B	�xB	�lB	�MB	��B	��B	�/B	�"B	��B	�cB	�TB	�/B	�KB	�B	�B	��B	�DB	��B	�B	�;B	��B	�B	��B	��B	�B	��B	��B	� B	��B	ޞB	�B	�B	ܒB	ޞB	�B	��B	��B	�"B	�HB
 iB
�B	�;B	��B	�%B	�;B	�;B	��B	��B	�cB
YB
%B
DB
B
GB
{B
 iB	�]B	��B	�B	�iB	�sB	�mB	�|B	�vB
B
$@B
(�B
�B
1B
1B
'�B
%zB
�B	��B	��B	��B	�DB	��B	��B	�.B	�B	��B	�B	�TB	��B	�ZB	�B	� B	��B	�B	��B	��B	��B	֡B	�yB	��B	��B	�?B	ƨB	�EB	��B	�pB	�mB	ŢB	�6B	�B	�<B	�B	�XB	�!B	��B	�3B	� B	�B	�B	��B	��B	�	B	�YB	��B	�	B	��B	�:B	�rB	�uB	�B	��B	�rB	�B	�4B	�4B	��B	}"B	|B	{B	yrB	|�B	xB	y>B	s�B	t�B	y�B	v+B	t�B	l�B	��B	kQB	m)B	g�B	lWB	hsB	m�B	�4B	_�B	cTB	y�B	^�B	k�B	]/B	YKB	Y�B	V9B	XB	YB	T�B	S�B	T�B	XEB	QB	N�B	S�B	MB	HB	WsB	V9B	OBB	F�B	D3B	?�B	=<B	B�B	K�B	NB	F�B	MjB	0�B	/�B	33B	*eB	-�B	*0B	)�B	'RB	%�B	($B	&B	'B	*�B	$@B	0!B	+�B	$�B	!bB	B	�B	CB	$B	�B	�B	MB	�B	B	�B	�B	B	�B	B	�B	�B	uB		�B	(B	�B	:B	�B	�B	_B	_B	B	�B	�B	fB	�B	�B	{B	�B	MB	�B	B	oB	{B	SB	YB	uB	B	�B	;B	�B	�B	�B�(B��B��B	 �B	 4B��B��B��B�B��B�B�	B��B��B��B��B�B�rB��B�fB��B�B�B	:B�B�B�B�B��B�AB�"B�B�B��B��B�B��B�/B�B�;B��B�AB�;B�cB�rB�lB�rB�B�xB	�B		�B	
	B	 B	B	�B	{B	B	oB	�B	�B	B	�B	oB	�B	:B	uB	uB	@B	uB	oB	@B	oB	@B	�B	�B	,�B	%zB	&�B	VB	#�B	&B	#�B	"4B	#:B	(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                B	�{B	��B	��B	�GB	�B	�GB	�GB	�{B	�B	�{B	�{B	�{B	�{B	�GB	�{B	��B	�{B	�B	�oB	�AB	�iB	�iB	�5B	�5B	�B	�5B	�B	�(B	��B	�(B	��B	�GB	�{B	�B	�B	��B	�xB	�JB	�B	��B	�uB	�GB	��B	��B	��B	��B
 =B
 rB
B
B
qB
'�B
KgB
��B
��B
�BB
وB
�>B
�mB
��B
�,B
��B
��B
�B
�B
��B
\2B
;�B
1�B
)gB
$B

B	�uB	��B	�ZB	�7B	�B

�B	�rB	�B	�,B	��B	�4B	sVB	hGB	^
B	M
B	BdB	4�B	 dB	'B	�B	�B��B�SB��B�B��B�B��B��B	4B	B	B	B	 0B	"�B	4B	/�B	%OB	B	B	zB	OB	�B	PB�B�oB�B��B�~B��B�iB	B	B	CB	tB	FB	�B	"B	)3B	/#B	,�B	"�B	�B	IB		B	7B	�B	qB	bB	�B	�B	FB	LB	 �B	*�B	C�B	LB	KgB	J�B	J,B	NEB	Y�B	^
B	`JB	c(B	d�B	foB	l�B	s�B	u�B	w�B	yB	{�B	�B	�B	��B	�!B	��B	��B	�-B	�zB	�RB	��B	��B	�*B	�*B	��B	��B	�B	�6B	�B	�6B	�}B	��B	��B	��B	��B
 =B	�YB	��B	��B	�uB	�4B	��B	��B	��B	դB	ȉB	�jB	��B	��B	��B	��B	�XB	�B	�B	�RB	��B	��B	�qB	�B	��B	��B	�IB	��B	��B	�B	��B	��B	�UB	��B	��B	�3B	�aB	�'B	��B	��B	��B	��B	�wB	��B	�zB	�9B	�tB	��B	��B	�gB	��B	��B	��B	��B	�B	�UB	��B	��B	��B	�tB	�B	��B	��B	��B	�B	� B	��B	� B	��B	�&B	��B	��B	��B	��B	�
B	�B	��B	��B	��B	�
B	�B	��B	��B	�#B	��B	��B	�^B	�B	�B	��B	�mB	�mB	�8B	�B	��B	��B	��B	έB	�yB	ϴB	�#B	�#B	��B	��B	�/B	��B	�iB	�B	�B	�B	�AB	�vB	�vB	�|B	�B	؂B	��B	�|B	��B	�B	�NB	�TB	�ZB	��B	��B	�%B	�ZB	��B	�2B	ܛB	�B	ݡB	�rB	�>B	�
B	�rB	�DB	�B	�DB	�JB	��B	�B	�B	�B	�cB	��B	� B	��B	�B	�oB	��B	�oB	�B	�B	��B	��B	�B	�B	�%B	�B	�B	��B	�YB	�+B	�YB	��B	�+B	�B	�B	�7B	�lB	�lB	�B	�rB	��B	�JB	�~B	�B	�"B	�(B	��B	��B	�bB	� B	��B	�iB	�uB	��B	�GB	�B	�B	�MB	��B	�_B	��B	�_B	�_B	�1B	��B	��B	��B	��B	��B	�eB	��B
 	B
�B
�B
�B
�B
�B
!B
�B
�B
�B
\B
\B
\B
.B
.B
.B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
	tB

B
	�B
	�B

B

{B
	�B

FB

�B
�B
�B
$B
$B
$B
�B
YB
YB
_B
�B
0B
eB
�B
=B
CB
wB
CB
B
�B
OB
�B
�B
!B
UB
�B
!B
�B
�B
'B
'B
�B
�B
[B
�B
bB
�B
[B
-B
�B
3B
3B
3B
9B
nB
B
�B
tB
�B
tB
�B
B
FB
�B
B
LB
�B
�B
�B
�B
RB
�B
�B
$B
�B
 dB
�B
 0B
�B
�B
^B
 �B
!kB
!B
!�B
"B
!�B
"�B
"�B
#�B
$B
$�B
%OB
$�B
%B
%OB
&�B
&�B
&�B
'�B
'[B
'�B
(aB
(aB
(�B
(�B
(�B
)�B
)gB
*mB
*�B
*�B
*�B
+tB
+tB
+tB
+tB
+�B
+tB
-KB
-KB
-�B
-�B
-KB
.B
.�B
.B
.�B
.�B
.�B
/�B
/�B
0)B
0�B
0�B
2B
2jB
3<B
3<B
3�B
4B
4B
4vB
4�B
5B
5B
4vB
5�B
5HB
5}B
4�B
5}B
6NB
6�B
6�B
7�B
8&B
8&B
8[B
8�B
8�B
8�B
8�B
8�B
8�B
9aB
9�B
:3B
:gB
9�B
:gB
:gB
:�B
;9B
;�B
;�B
<
B
<?B
<sB
<�B
<sB
<�B
<�B
=�B
=B
=EB
=yB
<�B
>�B
>�B
>�B
?B
?�B
?B
?�B
A)B
A�B
A^B
B/B
BdB
BdB
C5B
CjB
CB
CjB
DB
C�B
DB
C�B
D<B
C�B
D<B
C�B
D<B
C�B
DpB
DB
DpB
F|B
FB
FB
FB
F|B
F�B
F�B
F�B
GB
H�B
HTB
G�B
G�B
G�B
G�B
GNB
G�B
H�B
IZB
J`B
I�B
J`B
I�B
J,B
J`B
I�B
J�B
K2B
J�B
J�B
J�B
K2B
K2B
J�B
L8B
M
B
M
B
M�B
M>B
NyB
OB
N�B
PB
O�B
O�B
OB
O�B
O�B
PB
PB
OB
O�B
QWB
P�B
Q#B
S/B
TB
S�B
S�B
TB
S�B
S�B
TB
S�B
TB
T5B
TB
S�B
TB
U;B
UpB
T�B
UB
V�B
W|B
XNB
W�B
W|B
XNB
Y�B
Y�B
Y�B
Y�B
Z%B
Z%B
Z�B
\2B
[�B
[�B
[�B
[�B
\2B
[�B
[�B
\2B
\2B
\�B
]B
]B
]B
]B
]B
]8B
\fB
]lB
^�B
`B
_�B
_�B
_�B
`B
`JB
`B
`B
`B
_�B
_�B
`B
`�B
aPB
aB
aPB
a�B
aPB
a�B
a�B
a�B
bWB
bWB
b�B
b�B
c]B
c�B
d.B
c�B
c�B
d.B
d�B
e B
e B
e5B
e B
eiB
e5B
e�B
e�B
e�B
f;B
f�B
gAB
f�B
gAB
f�B
guB
gB
g�B
h{B
h{B
h�B
iB
h�B
iMB
iMB
h�B
iB
iB
jB
i�B
jB
i�B
jB
jSB
jSB
jSB
jSB
j�B
kYB
k�B
k�B
l+B
l�B
l�B
l�B
m1B
mfB
l�B
mfB
m1B
mfB
l�B
m�B
m1B
m�B
mfB
m�B
m1B
m1B
m�B
n7B
n7B
m�B
n7B
orB
orB
orB
pB
o�B
orB
pB
pB
pxB
o�B
pDB
p�B
q~B
qB
qJB
q�B
rPB
r�B
r�B
rPB
s"B
r�B
r�B
r�B
r�B
s"B
r�B
s�B
s"B
s�B
s�B
t�B
s�B
s�B
t�B
t�B
t�B
u.B
t�B
ubB
u�B
v4B
v B
u�B
v4B
u�B
viB
v4B
wB
v�B
w:B
v�B
v�B
v�B
wB
woB
w�B
w�B
w�B
w�B
xB
xB
xB
w�B
x@B
xuB
yGB
yGB
y�B
y�B
y�B
zMB
y�B
y�B
z�B
{B
{SB
{SB
{B
z�B
{SB
{�B
{�B
{�B
{�B
{�B
|YB
|�B
}+B
}�B
}_B
|�B
}�B
}�B
~eB
}�B
~�B
~1B
B
7B
7B
�B
kB
�B
�B
�B
kB
�	B
�=B
�	B
�rB
�=B
�rB
�rB
��B
�B
�xB
�B
�xB
�xB
��B
��B
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
��B
��B
��B
��B
��B
�VB
��B
�VB
��B
��B
��B
��B
��B
�'B
��B
�'B
��B
��B
�'B
�'B
��B
��B
��B
�bB
�.B
��B
��B
�4B
�hB
�hB
�4B
�4B
�4B
��B
�4B
�hB
��B
�:B
�B
��B	�oB	��B	�B	�GB	�;B	�GB	�{B	�B	�B	�{B	�GB	�oB	��B	�uB	�uB	�{B	�B	��B	�MB	�B	�B	��B	�AB	�uB	�B	�GB	�MB	�MB	�MB	�B	�MB	��B	�uB	�B	�{B	�uB	�B	�B	�B	�{B	�{B	�B	�MB	�B	�GB	�GB	�B	�GB	��B	�{B	�B	�GB	�B	�B	�B	�uB	�B	�GB	��B	�B	�B	�B	��B	��B	��B	��B	��B	�{B	�B	�uB	�B	�uB	�uB	�B	�B	�B	�B	�uB	��B	�B	��B	�oB	�B	�5B	�5B	��B	��B	�B	�B	�B	�B	��B	�B	��B	��B	�;B	�B	�`B	�5B	� B	��B	�5B	�B	�iB	��B	��B	��B	�B	�iB	�B	�;B	�;B	��B	��B	�.B	�.B	�.B	�5B	��B	��B	�5B	�B	��B	�5B	�5B	�B	�5B	�B	�B	� B	�5B	�5B	�;B	�B	�B	�iB	��B	�iB	��B	��B	�B	�oB	�yB	� B	�B	��B	��B	�"B	�B	�B	�B	�B	�(B	�B	�]B	�iB	�cB	�cB	�B	�B	�(B	�B	�B	�WB	�B	�]B	�"B	��B	��B	�B	�WB	�WB	�B	�B	�.B	�B	�.B	�.B	�B	�(B	�(B	��B	�]B	�MB	�B	�B	�B	�B	�{B	�AB	��B	�B	�{B	�B	�B	�B	��B	�B	�B	�oB	�AB	�uB	�B	�B	��B	��B	�B	�B	��B	�oB	�oB	��B	��B	�B	�B	��B	��B	�B	�lB	�7B	�B	�xB	�B	�~B	�JB	�B	�B	�rB	�B	��B	�~B	�PB	�B	�B	��B	�~B	�JB	�JB	�JB	�PB	�B	��B	��B	�"B	��B	�B	�VB	�bB	�uB	��B	�uB	�B	��B	��B	�oB	��B	�@B	�{B	�GB	��B	��B	�B	��B	�B	��B	��B	��B	�B	��B	��B	�MB	�B	��B	�YB	�YB	�B	��B	��B	��B	�_B	�_B	��B	��B	��B	��B	��B	�1B	��B	�%B	�YB	�eB
 =B	�7B	�eB	�7B
 rB
B
B
 =B	�kB	��B
 	B
B
 �B
 �B	��B	�kB
 �B
�B
B
 =B
CB
B
B
 �B
 rB
 rB
 �B
B
xB
B
!B
�B
!B
�B
	�B
	�B
�B
tB
*B
!�B
#wB
$IB
#�B
#wB
(�B
*�B
.�B
3�B
8�B
8�B
9,B
>KB
IZB
R�B
_yB
pxB
}�B
�.B
��B
�@B
��B
�B
��B
�bB
�_B
�_B
��B
�B
��B
��B
��B
��B
��B
�?B
��B
��B
ȉB
ɏB
��B
�B
�KB
�>B
��B
�HB
�WB
�B
��B
�>B
ɏB
�yB
�#B
�,B
�B
��B
�B
��B
�EB
��B
ƱB
��B
B
�B
�TB
̡B
�pB
�[B
� B
�B
�[B
�TB
��B
��B
��B
�6B
�NB
�mB
�^B
��B
�aB
��B
�jB
��B
�aB
��B
��B
�3B
��B
�0B
��B
�LB
�$B
�B
��B
��B
��B
�?B
�B
�B
��B
��B
{�B
yB
r�B
w�B
�PB
�mB
fB
��B
q~B
�rB
Z�B
TiB
M�B
E�B
?�B
;B
9�B
;�B
7�B
7TB
3�B
0^B
1dB
0)B
.B
,�B
*B
*mB
*mB
& B
(�B
)gB
)3B
"�B
!�B
(aB
'�B
$IB
'�B
�B
�B
�B
�B
nB
tB
�B
�B
�B
@B
�B
@B
}B
+?B
%�B
-B
�B
PB
�B
	�B
�B
�B
�B
�B

FB
�B
~B
B
�B
�B
�B
VB
�B
\B
�B
CB	��B	�eB	�7B	�eB
 	B	��B	�%B	��B	�{B	��B	��B	��B	��B
�B	��B	�@B	�MB	��B	��B	�B	�>B	��B	�rB	�`B	��B	�7B	�B	�xB	�YB	��B	�B	�;B	�.B	��B	�oB	�`B	�;B	�WB	ާB	ާB	��B	�PB	��B	ާB	�GB	��B	�"B	��B	��B	�B	��B	��B	�,B	��B	֪B	�B	�B	ԞB	֪B	۔B	��B	�B	�.B	�TB	�uB	��B	�GB	��B	�1B	�GB	�GB	��B	�B	�oB	�eB	�1B
PB	�B	�SB	��B	�uB	�iB	��B	��B	�uB	�B	�yB	�B	؂B
'B
LB
 �B
�B
=B
=B
�B
�B
 �B	��B	��B	��B	�PB	��B	��B	�:B	�B	��B	�B	�`B	�B	�fB	�%B	�B	��B	�B	��B	��B	��B	έB	ЅB	�B	�B	�KB	��B	�QB	�
B	�|B	�yB	��B	�BB	�B	�HB	�#B	�dB	�-B	��B	�?B	�,B	�#B	�$B	��B	��B	�B	�eB	��B	�B	��B	�FB	�~B	��B	�B	��B	�~B	}+B	x@B	x@B	~�B	u.B	t(B	s"B	q~B	t�B	pB	qJB	k�B	l�B	q�B	n7B	l�B	d�B	x�B	c]B	e5B	_�B	dcB	`B	fB	x@B	W�B	[`B	q�B	V�B	c�B	U;B	QWB	Q�B	NEB	PB	Q#B	M
B	K�B	L�B	PQB	I&B	F�B	K�B	EB	@#B	OB	NEB	GNB	>�B	<?B	7�B	5HB	:�B	C�B	FB	>�B	EvB	(�B	'�B	+?B	"qB	%�B	"<B	!�B	^B	�B	 0B	$B	*B	"�B	LB	(-B	#�B	�B	nB	B	�B	OB	0B	�B	�B	YB	�B	B	�B	�B	*B	
�B	'B	�B	B	�B	�B	4B	�B	
FB	�B	 	B�kB�kB	'B	�B��B	 rB��B��B��B��B�YB��B�%B�{B��B�_B�eB��B�B��B�GB��B��B��B�4B��B�B��B�@B��B��B��B�"B�B�B�B�B�	B�	B�B�"B�~B��B�rB��B�B�+B	
FB�B�(B�B�(B��B�MB�.B�B�B��B��B�B��B�;B�B�GB��B�MB�GB�oB�~B�xB�~B�B�B��B	�B	B		B	$B	�B	�B	B	
{B	�B	�B	B	�B	
{B	�B	
FB	�B	�B	LB	�B	
{B	LB	
{B	LB		�B	�B	$�B	�B	�B	bB	�B	$B	�B	@B	FB	 �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223234                            20230426223234AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622323420230426223234  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323420230426223234QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323420230426223234QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               