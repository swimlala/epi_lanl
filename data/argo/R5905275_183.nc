CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:49Z creation      
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
_FillValue                 �  [�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ch   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � hP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223249  20230426223249  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�
dL���@�
dL���11  @�
d����@�
d����@*��`�V@*��`�V�c^6z���c^6z��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?u@   @=p�@z�H@�G�@\@�G�@��RA  A ��A+�A>�RA`  A�  A�  A�  A��A��A�  A�Q�A�Q�B Q�B  B�
B  B (�B'�
B0  B8(�B?�
BH  BP  BW�
B_�
Bg�
Bp  Bx(�B�{B�{B�  B��B�  B��B��B�  B�{B�  B��B��B��B�{B�{B�  B��B��B�  B��B��
B�  B�{B�  B�{B�(�B�(�B�  B�  B��B��B��B��C  C  C  C
=C
  C  C
=C
=C
=C
=C  C
=C
=C��C  C   C"  C#��C&  C(  C)��C+��C-��C0  C1��C3��C5��C7��C:
=C<
=C>  C@  CB
=CD  CF  CG��CI��CL  CN{CP
=CR  CT
=CV  CX  CZ{C\  C]��C`  Cb
=Cd  Ce��Ch  Cj  Cl
=Cn{Cp
=Cr  Cs��Cv  Cx
=Cy��C|  C~  C�C�
=C�  C�  C�  C�  C�C�  C�C�
=C�C�  C�  C���C�  C���C�  C�C���C���C�  C���C���C�  C�  C�  C���C�  C�  C���C�  C�C�  C�  C�  C���C���C�  C�  C�  C���C���C�  C�
=C�C���C�C�C�C���C�  C�  C���C���C�C�C�  C���C�  C�
=C�C�  C���C���C���C���C�C�  C���C�C�C�C�C���C�  C�C�  C���C�  C�C�
=C�  C�  C�C�
=C�  C���C���C�  C���C�  C�
=C�C�C�C�C�C�C�C�C�C�
=C�\C�  C���C���C���C���C���C���C��C���C�  C�C�
=C�
=C�C�  C�C�C�  C�
=C�C�  C�  C���C�C�D   D �D�D��D  D� D  D}qD�qD}qD�D��D  D� D�qD�DD�D	�D	� D
D
�D�D��D�D��D�D��D�D�D�qD}qD  D� D�D�DD� D  D� D�D��DD�D  D� D�D��DD� D  D� D�qD}qD  D� D  D��D  Dz�D�D� D��Dz�D�qD � D!  D!��D"�D"��D#�D#� D$  D$��D%D%��D&  D&� D&�qD'}qD(  D(� D(�qD)� D*�D*� D+  D+� D+�qD,}qD-  D-��D.  D.}qD/  D/� D0�D0��D1�D1��D2  D2��D3  D3}qD3�qD4� D5�D5��D6�D6� D7  D7� D8  D8� D9  D9� D:�D:� D;  D;}qD;�qD<��D=�D=��D>�D>��D?�D?��D@�D@��DA  DA��DB  DB}qDB�qDC��DD�DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK��DL�DL}qDM  DM��DN  DN� DO�DO� DO�qDP}qDP�qDQ� DR�DR� DS  DS� DT�DT�DUDU��DV�DV� DW  DW}qDW��DXz�DX�qDY� DY�qDZ}qDZ�qD[� D\  D\� D]�D]� D^�D^��D^�qD_z�D_��D`}qDa�Da��Db�Db�Dc  Dc}qDc�qDd}qDe  De}qDe�qDf� Dg  Dg� DhDh��Di�Di��Dj  Djz�Dj�qDk� Dl�Dl� Dl�qDm��DnDn��Do  Do� Do�qDp}qDq  Dq��Dr  Dr� Dr�qDs}qDt  Dt��Du  Du� Du�qDv}qDv�qDw}qDx  Dx� Dy�Dy}qDz  Dz� Dz�qD{}qD{�qD|� D}�D}� D}�qD~z�D~��D}qD�  D�>�D�~�D���D��qD�@ D�~�D���D���D�>�D�� D�� D���D�AHD�� D���D�HD�AHD�� D�� D���D�>�D�� D�� D�  D�@ D�~�D�� D�  D�>�D�� D��HD��D�AHD�� D��HD�  D�@ D��HD���D�  D�AHD�� D���D�HD�@ D�~�D��HD�HD�@ D��HD��HD�  D�>�D�� D�� D���D�@ D��HD���D���D�@ D��HD��HD�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D���D���D�>�D�~�D���D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D��HD�D�  D�@ D�� D���D�  D�@ D��HD��HD�  D�@ D���D��HD���D�@ D��HD�� D���D�>�D�� D���D���D�@ D��HD��HD�  D�>�D��HD��HD�  D�B�D���D��HD�  D�=qD�~�D���D�  D�@ D�~�D���D�HD�B�D��HD�� D���D�>�D�~�D��qD�  D�AHD��HD��HD�HD�B�D�� D�� D�  D�@ D���D��HD�  D�>�D�� D��HD�HD�@ D�~�D���D��qD�@ D�� D��HD�HD�>�D�~�D���D�  D�@ D�� D�� D�HD�AHD���D�� D���D�@ D��HD��HD�  D�>�D�� D���D��qD�@ D�� D���D�  D�@ D�~�D��HD�HD�=qD�~�D���D�  D�B�D���D��HD��D�@ D�}qD���D���D�>�D�� D�� D�HD�>�D�~�D�� D�HD�C�D��HD�� D���D�AHD���D��HD�  D�@ D�~�D��HD��D�B�D��HD�� D���D�@ D���D�D�  D�=qD�}qD��qD��qD�<)D�� D�D��D�B�D���D�D�  D�=qD�}qD�� D��D�AHD�� D�� D�  D�@ D��HD��HD�  D�>�D�� D��HD��D�AHD D�� D�  D�>�D�}qDþ�D�HD�AHDĀ D�� D���D�>�DŁHD��HD�  D�@ DƁHD�� D���D�=qD�~�D�� D���D�@ DȀ DȾ�D�HD�AHDɁHD��HD��D�AHDʀ Dʾ�D��qD�>�DˁHD�� D�  D�@ D́HD�� D���D�@ D́HD��HD�  D�=qD�~�D��HD��D�@ DρHD�D�HD�AHDЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�AHDӀ DӾ�D�  D�AHDԁHD��HD�HD�AHDՀ Dվ�D���D�@ Dր D־�D�  D�@ D׀ D׾�D��qD�AHD؀ Dؾ�D���D�@ Dـ D�� D�HD�AHDځHDھ�D���D�AHDہHD۾�D�  D�AHD܁HD��HD�  D�>�D݀ D�� D���D�>�D�~�D�� D�  D�@ D߀ D��HD�  D�>�D�~�D�� D�  D�AHD�HD�� D�HD�AHD� D�� D�HD�AHD� D�qD���D�AHD�HD�� D�  D�@ D� D��HD�HD�@ D�}qD�� D�HD�AHD� D羸D�  D�AHD� D��HD�HD�AHD� D龸D���D�@ D� D�� D�HD�@ D�}qD�� D�HD�>�D� D�� D�  D�@ D� D�� D�  D�>�D� D�� D�  D�@ D�HD��HD�  D�>�D�~�D�� D�  D�@ D� D�� D�  D�@ D�~�D�� D�  D�@ D� D�� D�HD�AHD� D�� D�HD�AHD�� D�� D���D�@ D���D�� D���D�>�D�~�D���D���D�@ D��HD�� D�  D�>�D�~�D���D���D�AHD�� D�� D��D�4{D�u�>���>�G�?B�\?�=q?�33?��?�@z�@+�@=p�@Q�@fff@z�H@���@�z�@�p�@��@�33@�  @���@�@�  @�@�A ��A
=A(�A��A
=A��A#33A'�A,��A1�A7�A>{AC�
AH��AN{AS33AX��A^�RAdz�Ai��An�RAs�
Ax��A~�RA�=qA���A��A��A�z�A�
=A��A���A��A�=qA�z�A�
=A���A���A�\)A���A�(�A�ffA���A��A�ffA���A�33A�p�A�Q�A��HA�Aȣ�A˅A�{A�Q�A�33A�A���A��
A޸RA���A�A�ffA���A�(�A�ffA���A��
A��RA���A�(�A�ffB z�B�B\)B��B{B\)Bz�B
{B�B��B=qB�B��B{B�B��BffB�
B�BffB�B�B�\B   B!G�B"�\B#�
B%�B&ffB'�
B)G�B*�RB,  B-G�B.ffB/�
B1�B2�\B4  B5G�B6�\B7�B8��B:=qB;�B<��B>ffB?�B@��BB{BC33BD��BE�BG33BHz�BI��BJ�RBL  BL��BN=qBO�BP��BQ�BS33BT(�BU�BV=qBW33BXQ�BY��BZ�\B[�B\��B]p�B^ffB_\)B`z�BaG�Bb=qBc33Bd  Be�Bf{Bg33Bh(�Bi�Bi�Bj�HBl(�BmG�BnffBo�Bp��Br=qBs\)Bt��BuBv�HBx  ByG�Bz�RB|  B}p�B~�RB�
B���B�G�B��B��\B�33B��
B��\B�G�B��B���B�\)B�  B���B�\)B�{B��RB�p�B�{B��RB�p�B�(�B��HB���B�Q�B�
=B��
B��\B�G�B�  B��RB�\)B�{B���B��B�=qB���B�B�z�B�33B�  B���B�p�B�=qB���B�B�z�B�G�B�  B��RB�p�B�=qB���B��B�ffB�33B��
B���B�\)B�(�B���B�B��\B�\)B�(�B��HB���B�=qB���B���B�=qB���B�\)B��B�z�B�
=B��B�  B�z�B��HB�\)B�B�=qB���B�
=B�p�B��
B�Q�B��RB��B���B�  B�z�B���B�p�B��B�Q�B���B�G�B��B�=qB���B��B���B�{B\B���B�p�B��B�ffB��HB�\)B��
B�Q�B���B�\)B��
B�Q�B��HB�p�B�  B�z�B���B˅B�  B̏\B�
=B͙�B�{BΣ�B��BϮB�(�BУ�B�33BѮB�=qBҸRB�G�B�B�Q�B��HB�p�B�  B֏\B��Bי�B�=qB���B�G�B��
B�ffB���BۅB�{Bܣ�B�33B�B�Q�B��HB�p�B�  B��B��B�B�Q�B��HB�B�{B��B�33B��
B�z�B�
=B癚B�(�B�RB�\)B��B�z�B�
=B�B�ffB��HB�B�{B��B�33B��
B�ffB���B�B�(�B�RB�G�B��
B�ffB�
=B���B�(�B���B�\)B�  B��\B��B�B�ffB���B���B�=qB��HB�p�B�  B���B�33B��
C 33C �C ��C{CffC�C��C=qCz�CC
=CQ�C��C�HC(�Cp�C�RC  CG�C�\C�
C�C\)C�C�C33Cz�CC
=CQ�C��C�HC	�C	ffC	��C	�HC
�C
\)C
��C
�
C�CQ�C��C�
C�C\)C��C�HC�C\)C��C�
C�C\)C��C�HC�CffC��C�HC�C\)C�\C�
C
=CG�C�CC  C=qCz�C�C�C�CQ�C�\CC��C(�C\)C�\CC��C(�C\)C�\CC  C=qCp�C�C�HC�CQ�C�C�RC��C�C\)C�\CC  C33CffC��C�HC{CQ�C�CC�C�CQ�C�C�C�HC�CQ�C�\CC�C�CG�Cz�C��C�HC
=C=qCp�C��C��C  C(�CQ�Cz�C�C�HC {C G�C �C �RC �C!�C!Q�C!�C!�RC!�C"33C"p�C"��C"�
C#
=C#=qC#p�C#�C#�HC$�C$\)C$��C$�
C%
=C%=qC%z�C%�C%�C&�C&\)C&��C&�
C'{C'Q�C'�\C'�
C({C(Q�C(�C(C)  C)=qC)�C)��C*{C*Q�C*�\C*��C+
=C+G�C+�\C+��C,{C,\)C,��C,�C-33C-ffC-�C-�C.=qC.�C.��C/�C/\)C/��C/�HC0(�C0p�C0C1{C1ffC1�RC1��C2=qC2�C2��C3�C3p�C3��C4{C4\)C4��C4�C5=qC5�\C5�C6=qC6�\C6�
C7�C7p�C7��C8(�C8z�C8�
C9�C9p�C9C:{C:p�C:��C;(�C;z�C;C<{C<p�C<��C=(�C=z�C=��C>�C>z�C>�
C?=qC?��C?�C@G�C@��C@�CAG�CA�CB
=CBp�CB��CC�CCp�CC��CD33CD��CD�CEG�CE��CE��CF\)CFCG{CGffCGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                ?u@   @=p�@z�H@�G�@\@�G�@��RA  A ��A+�A>�RA`  A�  A�  A�  A��A��A�  A�Q�A�Q�B Q�B  B�
B  B (�B'�
B0  B8(�B?�
BH  BP  BW�
B_�
Bg�
Bp  Bx(�B�{B�{B�  B��B�  B��B��B�  B�{B�  B��B��B��B�{B�{B�  B��B��B�  B��B��
B�  B�{B�  B�{B�(�B�(�B�  B�  B��B��B��B��C  C  C  C
=C
  C  C
=C
=C
=C
=C  C
=C
=C��C  C   C"  C#��C&  C(  C)��C+��C-��C0  C1��C3��C5��C7��C:
=C<
=C>  C@  CB
=CD  CF  CG��CI��CL  CN{CP
=CR  CT
=CV  CX  CZ{C\  C]��C`  Cb
=Cd  Ce��Ch  Cj  Cl
=Cn{Cp
=Cr  Cs��Cv  Cx
=Cy��C|  C~  C�C�
=C�  C�  C�  C�  C�C�  C�C�
=C�C�  C�  C���C�  C���C�  C�C���C���C�  C���C���C�  C�  C�  C���C�  C�  C���C�  C�C�  C�  C�  C���C���C�  C�  C�  C���C���C�  C�
=C�C���C�C�C�C���C�  C�  C���C���C�C�C�  C���C�  C�
=C�C�  C���C���C���C���C�C�  C���C�C�C�C�C���C�  C�C�  C���C�  C�C�
=C�  C�  C�C�
=C�  C���C���C�  C���C�  C�
=C�C�C�C�C�C�C�C�C�C�
=C�\C�  C���C���C���C���C���C���C��C���C�  C�C�
=C�
=C�C�  C�C�C�  C�
=C�C�  C�  C���C�C�D   D �D�D��D  D� D  D}qD�qD}qD�D��D  D� D�qD�DD�D	�D	� D
D
�D�D��D�D��D�D��D�D�D�qD}qD  D� D�D�DD� D  D� D�D��DD�D  D� D�D��DD� D  D� D�qD}qD  D� D  D��D  Dz�D�D� D��Dz�D�qD � D!  D!��D"�D"��D#�D#� D$  D$��D%D%��D&  D&� D&�qD'}qD(  D(� D(�qD)� D*�D*� D+  D+� D+�qD,}qD-  D-��D.  D.}qD/  D/� D0�D0��D1�D1��D2  D2��D3  D3}qD3�qD4� D5�D5��D6�D6� D7  D7� D8  D8� D9  D9� D:�D:� D;  D;}qD;�qD<��D=�D=��D>�D>��D?�D?��D@�D@��DA  DA��DB  DB}qDB�qDC��DD�DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK��DL�DL}qDM  DM��DN  DN� DO�DO� DO�qDP}qDP�qDQ� DR�DR� DS  DS� DT�DT�DUDU��DV�DV� DW  DW}qDW��DXz�DX�qDY� DY�qDZ}qDZ�qD[� D\  D\� D]�D]� D^�D^��D^�qD_z�D_��D`}qDa�Da��Db�Db�Dc  Dc}qDc�qDd}qDe  De}qDe�qDf� Dg  Dg� DhDh��Di�Di��Dj  Djz�Dj�qDk� Dl�Dl� Dl�qDm��DnDn��Do  Do� Do�qDp}qDq  Dq��Dr  Dr� Dr�qDs}qDt  Dt��Du  Du� Du�qDv}qDv�qDw}qDx  Dx� Dy�Dy}qDz  Dz� Dz�qD{}qD{�qD|� D}�D}� D}�qD~z�D~��D}qD�  D�>�D�~�D���D��qD�@ D�~�D���D���D�>�D�� D�� D���D�AHD�� D���D�HD�AHD�� D�� D���D�>�D�� D�� D�  D�@ D�~�D�� D�  D�>�D�� D��HD��D�AHD�� D��HD�  D�@ D��HD���D�  D�AHD�� D���D�HD�@ D�~�D��HD�HD�@ D��HD��HD�  D�>�D�� D�� D���D�@ D��HD���D���D�@ D��HD��HD�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D���D���D�>�D�~�D���D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D��HD�D�  D�@ D�� D���D�  D�@ D��HD��HD�  D�@ D���D��HD���D�@ D��HD�� D���D�>�D�� D���D���D�@ D��HD��HD�  D�>�D��HD��HD�  D�B�D���D��HD�  D�=qD�~�D���D�  D�@ D�~�D���D�HD�B�D��HD�� D���D�>�D�~�D��qD�  D�AHD��HD��HD�HD�B�D�� D�� D�  D�@ D���D��HD�  D�>�D�� D��HD�HD�@ D�~�D���D��qD�@ D�� D��HD�HD�>�D�~�D���D�  D�@ D�� D�� D�HD�AHD���D�� D���D�@ D��HD��HD�  D�>�D�� D���D��qD�@ D�� D���D�  D�@ D�~�D��HD�HD�=qD�~�D���D�  D�B�D���D��HD��D�@ D�}qD���D���D�>�D�� D�� D�HD�>�D�~�D�� D�HD�C�D��HD�� D���D�AHD���D��HD�  D�@ D�~�D��HD��D�B�D��HD�� D���D�@ D���D�D�  D�=qD�}qD��qD��qD�<)D�� D�D��D�B�D���D�D�  D�=qD�}qD�� D��D�AHD�� D�� D�  D�@ D��HD��HD�  D�>�D�� D��HD��D�AHD D�� D�  D�>�D�}qDþ�D�HD�AHDĀ D�� D���D�>�DŁHD��HD�  D�@ DƁHD�� D���D�=qD�~�D�� D���D�@ DȀ DȾ�D�HD�AHDɁHD��HD��D�AHDʀ Dʾ�D��qD�>�DˁHD�� D�  D�@ D́HD�� D���D�@ D́HD��HD�  D�=qD�~�D��HD��D�@ DρHD�D�HD�AHDЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�AHDӀ DӾ�D�  D�AHDԁHD��HD�HD�AHDՀ Dվ�D���D�@ Dր D־�D�  D�@ D׀ D׾�D��qD�AHD؀ Dؾ�D���D�@ Dـ D�� D�HD�AHDځHDھ�D���D�AHDہHD۾�D�  D�AHD܁HD��HD�  D�>�D݀ D�� D���D�>�D�~�D�� D�  D�@ D߀ D��HD�  D�>�D�~�D�� D�  D�AHD�HD�� D�HD�AHD� D�� D�HD�AHD� D�qD���D�AHD�HD�� D�  D�@ D� D��HD�HD�@ D�}qD�� D�HD�AHD� D羸D�  D�AHD� D��HD�HD�AHD� D龸D���D�@ D� D�� D�HD�@ D�}qD�� D�HD�>�D� D�� D�  D�@ D� D�� D�  D�>�D� D�� D�  D�@ D�HD��HD�  D�>�D�~�D�� D�  D�@ D� D�� D�  D�@ D�~�D�� D�  D�@ D� D�� D�HD�AHD� D�� D�HD�AHD�� D�� D���D�@ D���D�� D���D�>�D�~�D���D���D�@ D��HD�� D�  D�>�D�~�D���D���D�AHD�� D�� D��D�4{D�u�>���>�G�?B�\?�=q?�33?��?�@z�@+�@=p�@Q�@fff@z�H@���@�z�@�p�@��@�33@�  @���@�@�  @�@�A ��A
=A(�A��A
=A��A#33A'�A,��A1�A7�A>{AC�
AH��AN{AS33AX��A^�RAdz�Ai��An�RAs�
Ax��A~�RA�=qA���A��A��A�z�A�
=A��A���A��A�=qA�z�A�
=A���A���A�\)A���A�(�A�ffA���A��A�ffA���A�33A�p�A�Q�A��HA�Aȣ�A˅A�{A�Q�A�33A�A���A��
A޸RA���A�A�ffA���A�(�A�ffA���A��
A��RA���A�(�A�ffB z�B�B\)B��B{B\)Bz�B
{B�B��B=qB�B��B{B�B��BffB�
B�BffB�B�B�\B   B!G�B"�\B#�
B%�B&ffB'�
B)G�B*�RB,  B-G�B.ffB/�
B1�B2�\B4  B5G�B6�\B7�B8��B:=qB;�B<��B>ffB?�B@��BB{BC33BD��BE�BG33BHz�BI��BJ�RBL  BL��BN=qBO�BP��BQ�BS33BT(�BU�BV=qBW33BXQ�BY��BZ�\B[�B\��B]p�B^ffB_\)B`z�BaG�Bb=qBc33Bd  Be�Bf{Bg33Bh(�Bi�Bi�Bj�HBl(�BmG�BnffBo�Bp��Br=qBs\)Bt��BuBv�HBx  ByG�Bz�RB|  B}p�B~�RB�
B���B�G�B��B��\B�33B��
B��\B�G�B��B���B�\)B�  B���B�\)B�{B��RB�p�B�{B��RB�p�B�(�B��HB���B�Q�B�
=B��
B��\B�G�B�  B��RB�\)B�{B���B��B�=qB���B�B�z�B�33B�  B���B�p�B�=qB���B�B�z�B�G�B�  B��RB�p�B�=qB���B��B�ffB�33B��
B���B�\)B�(�B���B�B��\B�\)B�(�B��HB���B�=qB���B���B�=qB���B�\)B��B�z�B�
=B��B�  B�z�B��HB�\)B�B�=qB���B�
=B�p�B��
B�Q�B��RB��B���B�  B�z�B���B�p�B��B�Q�B���B�G�B��B�=qB���B��B���B�{B\B���B�p�B��B�ffB��HB�\)B��
B�Q�B���B�\)B��
B�Q�B��HB�p�B�  B�z�B���B˅B�  B̏\B�
=B͙�B�{BΣ�B��BϮB�(�BУ�B�33BѮB�=qBҸRB�G�B�B�Q�B��HB�p�B�  B֏\B��Bי�B�=qB���B�G�B��
B�ffB���BۅB�{Bܣ�B�33B�B�Q�B��HB�p�B�  B��B��B�B�Q�B��HB�B�{B��B�33B��
B�z�B�
=B癚B�(�B�RB�\)B��B�z�B�
=B�B�ffB��HB�B�{B��B�33B��
B�ffB���B�B�(�B�RB�G�B��
B�ffB�
=B���B�(�B���B�\)B�  B��\B��B�B�ffB���B���B�=qB��HB�p�B�  B���B�33B��
C 33C �C ��C{CffC�C��C=qCz�CC
=CQ�C��C�HC(�Cp�C�RC  CG�C�\C�
C�C\)C�C�C33Cz�CC
=CQ�C��C�HC	�C	ffC	��C	�HC
�C
\)C
��C
�
C�CQ�C��C�
C�C\)C��C�HC�C\)C��C�
C�C\)C��C�HC�CffC��C�HC�C\)C�\C�
C
=CG�C�CC  C=qCz�C�C�C�CQ�C�\CC��C(�C\)C�\CC��C(�C\)C�\CC  C=qCp�C�C�HC�CQ�C�C�RC��C�C\)C�\CC  C33CffC��C�HC{CQ�C�CC�C�CQ�C�C�C�HC�CQ�C�\CC�C�CG�Cz�C��C�HC
=C=qCp�C��C��C  C(�CQ�Cz�C�C�HC {C G�C �C �RC �C!�C!Q�C!�C!�RC!�C"33C"p�C"��C"�
C#
=C#=qC#p�C#�C#�HC$�C$\)C$��C$�
C%
=C%=qC%z�C%�C%�C&�C&\)C&��C&�
C'{C'Q�C'�\C'�
C({C(Q�C(�C(C)  C)=qC)�C)��C*{C*Q�C*�\C*��C+
=C+G�C+�\C+��C,{C,\)C,��C,�C-33C-ffC-�C-�C.=qC.�C.��C/�C/\)C/��C/�HC0(�C0p�C0C1{C1ffC1�RC1��C2=qC2�C2��C3�C3p�C3��C4{C4\)C4��C4�C5=qC5�\C5�C6=qC6�\C6�
C7�C7p�C7��C8(�C8z�C8�
C9�C9p�C9C:{C:p�C:��C;(�C;z�C;C<{C<p�C<��C=(�C=z�C=��C>�C>z�C>�
C?=qC?��C?�C@G�C@��C@�CAG�CA�CB
=CBp�CB��CC�CCp�CC��CD33CD��CD�CEG�CE��CE��CF\)CFCG{CGffCGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AϬAϩ�Aϩ�Aϩ�AϮAϰ!Aϲ-Aϲ-Aϰ!Aϰ!Aϴ9Aϲ-Aϲ-Aϲ-Aϴ9A϶FAϼjA���A��A��A��mA��`A��HA��;A��
A���A�ĜA�ĜA�A���AϾwAϼjA϶FAϰ!A���A��A��jA�;dA���A�r�A�oA���A�ƨA�G�A���A�x�A�VA��#A��/A��`A���A��uA�7LA�ĜA��FA��-A�x�A���A��#A��A�?}A��A�n�A��^A���A�I�A��A�v�A�\)A��A�(�A�/A���A��FA��`A�S�A�7LA|1'Az��Aw�mAs��Ak&�Ad�A`VA[�AUl�AKƨAH$�AG�AG
=AE��AC��AD �AE33AF(�AF��AF�AE+AD��AD��AEdZAE%AE�-AEC�AD��AC��AB��AB  AA��AAt�A@�A>^5A>��A?S�A>$�A=
=A<�A;l�A;`BA9�wA8jA7�A6v�A6(�A5hsA4�A3��A2�+A2�A1��A0��A0bNA0�DA/��A.�9A.ZA-�7A-?}A-p�A-XA-K�A-�A,^5A+A+O�A*�/A)�^A(�RA(1'A'�hA'K�A&z�A%��A%��A&1'A&��A&��A&�RA&�uA&�uA&��A%�A%dZA%G�A$v�A$JA#hsA#�A"ĜA"A�A!�A!��A ��A �!A M�A�Ap�A/AȴA(�A��A�AdZA�A��A�uAz�AjA-A�^A|�AoA��A�!A�uAE�AdZA�RA�AZA{A��A|�AoA�`A�`A�jA�TA33A�A �A  A�#A`BA�A��AbA�TA�wAl�A`BAK�AVA��A�9AM�A��AK�A%A��A�A��A��A?}A�`A�;A�7A
��A
��A
5?A	��A	\)A��A5?A��AG�A"�A��A��An�A=qAJA�#A�hA`BA;dA�A��Ar�AQ�A�-A��A=qA�A��Ax�AG�A"�A �A ȴA ĜA j@�l�@�^5@���@�  @�S�@���@��R@�V@��T@�`B@��9@�ƨ@�o@�E�@��j@�o@�M�@���@�&�@��
@�l�@�+@���@��H@���@�!@�+@���@�b@�{@�X@�G�@�?}@�?}@�?}@�?}@���@�Z@�C�@��@���@�h@��@�j@���@㝲@�"�@⟾@��@��@�hs@�Z@�1@���@�|�@�+@��@���@އ+@�E�@�{@��@��T@��`@��@�dZ@�=q@�x�@�z�@ְ!@��#@�%@�Q�@���@���@�ff@�V@�=q@��@�V@��@ϝ�@���@�G�@�r�@���@�K�@ʇ+@�^5@���@�?}@�Ĝ@�r�@�1@�33@Ɨ�@��T@�7L@Ĭ@�z�@�r�@�Z@� �@�b@�  @öF@�;d@���@�@�v�@�-@��T@���@�&�@�r�@��
@�dZ@�@��R@�n�@�M�@�$�@��T@��h@���@� �@�K�@���@�7L@��`@���@��9@���@�r�@�I�@�(�@��m@���@��@�K�@���@�M�@�?}@��D@�1@��;@�\)@�E�@���@�hs@�7L@�Ĝ@��F@��H@���@��h@�7L@��@�%@��@��/@�j@�1@��F@�"�@���@���@�ff@�V@�=q@��T@�hs@��@�r�@�Z@��F@�;d@�ȴ@�^5@�$�@�J@�@��-@�x�@�X@�&�@��@���@�z�@�9X@���@�|�@��@���@�V@�$�@��@�{@��T@��-@��h@�p�@���@�A�@���@��F@�|�@�33@���@�=q@���@�`B@���@�z�@�1'@��@��@�l�@��@�n�@�^5@�M�@�$�@��@��T@��h@�G�@�7L@�/@��@�%@��@�(�@���@���@�|�@�\)@�
=@��R@�v�@�@��h@��@��j@�z�@�  @���@�dZ@�o@���@�~�@�5?@��@�@��h@�?}@�V@���@�(�@� �@��@��@���@���@�C�@��+@�=q@��#@�hs@��@��`@���@�j@�A�@� �@�1@���@�33@��!@�n�@�V@�5?@�@��T@�@�x�@�/@��`@��9@��D@�Q�@� �@���@��F@�dZ@�33@�o@��@���@��\@�V@�5?@�-@�{@���@�O�@���@��9@���@��@��@���@�\)@��@��\@�@��-@���@�x�@�O�@�?}@��@���@�Ĝ@�z�@�1@l�@;d@
=@~�@~v�@~{@}��@}�@}O�@|��@|�@{S�@{33@z�H@z^5@y��@y�7@x��@x�9@xbN@xQ�@xA�@x �@x  @w�;@wl�@wK�@w+@v�R@vff@v$�@u?}@tz�@t9X@t(�@s�m@s�F@st�@s@rn�@q�#@q��@q�^@qx�@qG�@pĜ@p1'@o\)@o;d@o+@o
=@n�+@m@m?}@l��@l��@lZ@l1@kƨ@k�F@k��@k��@k�@kdZ@j��@j^5@jJ@i��@h��@hbN@hA�@g�w@gl�@g+@fȴ@fE�@e�h@e/@e�@eV@d�@d�j@d��@dj@d�@c�F@c"�@bn�@b=q@a��@a�@a�#@a��@ahs@`��@`1'@_�P@_+@_�@^�y@^V@]�T@]�h@]p�@]?}@]�@\�D@[��@[33@Z��@Z�!@Z^5@Y��@Y��@Yhs@X�`@X �@W�P@WK�@V��@U��@U�@Up�@U/@T�@S�m@S33@R�H@R^5@QX@P�u@Pb@O��@Ol�@O\)@OK�@O
=@N��@NE�@N@M�h@L��@LI�@K�
@K�F@K��@K@J��@Jn�@J^5@JJ@I��@IG�@HQ�@H �@Hb@G��@GK�@F��@F�+@F5?@E��@E�@EO�@D�j@Dj@D9X@D1@C�F@C"�@B�!@BM�@A�7@A%@@��@@Ĝ@@Q�@@  @?�@?�w@?�P@?+@>��@>��@>v�@>5?@=�T@=��@=�h@=�@=p�@=?}@=V@<��@;33@:�@9�^@9�7@97L@8��@8�9@8Q�@8b@7�;@7�w@7K�@6��@6�@6v�@6$�@6@5�@5��@5�h@5/@4��@4I�@41@3�m@3�m@3ƨ@3t�@3C�@2�@2��@2�\@1��@17L@1&�@1%@0�`@0Ĝ@0��@0�u@0Q�@/�@/��@/��@/��@/|�@/;d@/
=@.E�@-��@-`B@-/@,��@,��@,�@,�D@,z�@,Z@,9X@,1@+��@+@*�H@*��@*�H@*��@*��@*��@*�!@*n�@)�@)��@)hs@)7L@)%@(��@(��@( �@'�P@'�@&��@&�@&��@&�+@%�T@%`B@$�j@$�D@$I�@#��@#ƨ@#��@#S�@"~�@"n�@"n�@"~�@"^5@"=q@!��@!�#@!��@!��@!hs@!&�@ �9@ b@   @   @�@�w@l�@�@��@V@{@�@�@�T@@`B@V@�j@j@1@��@o@�H@�!@~�@^5@-@��@�#@�7@&�@��@�`@��@��@�9@�u@�@r�@bN@Q�@b@��@�@|�@l�@;d@
=@�@��@ff@{@�@@�@O�@�@�@�j@z�@Z@(�@��@ƨ@�F@t�@o@��@�\@~�@^5@-@��@�@�^@x�@�@�`@�@A�@ �@b@b@b@  @�@��@��@5?@@�T@�-@�@p�@/@�@�@��@Z@9X@1@��@dZ@C�@"�@
�!@
n�@
n�@
n�@
M�@
-@
J@	��@	�#@	�AϮAϮAϮAϬAϧ�Aϥ�AϬAϮAϩ�Aϧ�Aϧ�Aϧ�AϬAϮAϬAϬAϩ�Aϰ!Aϲ-A϶FAϰ!Aϰ!Aϰ!Aϴ9Aϲ-Aϲ-Aϰ!AϮAϰ!Aϲ-Aϲ-Aϰ!Aϲ-Aϴ9Aϲ-Aϴ9Aϲ-Aϰ!Aϰ!Aϲ-Aϴ9Aϴ9Aϲ-Aϰ!Aϰ!Aϲ-Aϴ9A϶FAϴ9Aϴ9Aϰ!Aϰ!Aϲ-A϶FA϶FAϸRAϴ9Aϲ-Aϲ-A϶FAϺ^AϸRA϶FAϴ9Aϲ-Aϲ-AϼjA���A���A�ĜA�ĜA�ȴA���A���A���A���A���A���A���A��;A��yA��A��A��A��yA��yA��A��A��yA��`A��`A��mA��yA��mA��`A��`A��`A��mA��`A��`A��HA��;A��HA��TA��TA��TA��;A��;A��/A��/A��;A��HA��#A��
A���A���A���A���A���A���A���A�ƨA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ĜA�A�A�A�ĜA�ƨA�ƨA�ĜA���A���A���A�A�ĜA�ĜA�A���AϾwAϾwA���A���A���A���AϾwAϼjAϼjAϼjAϺ^AϼjAϾwAϾwAϾwAϺ^A϶FA϶FA϶FA϶FAϸRA϶FA϶FAϴ9Aϰ!Aϰ!Aϲ-Aϴ9Aϲ-AϮAϮAϩ�Aϩ�Aϡ�A�Q�AάA��/Aʕ�A�{AŃA��#A���A�A�A��A��A�;dA�~�A�G�A�(�A��uA��A��A�?}A���A�ZA���A�5?A�ȴA�n�A���A�33A�7LA��+A�/A��7A��A���A��uA�|�A�`BA�9XA��A��DA�&�A��/A���A���A���A���A��FA���A��A��DA�1A��A���A���A�n�A�1'A���A��+A�{A���A�7LA��yA���A�~�A�x�A�t�A�r�A�p�A�p�A�x�A��A��hA���A�/A���A���A���A��7A�+A��TA��uA��A���A�5?A��^A��uA�l�A�C�A�5?A��A���A��\A�Q�A��A��wA��wA��
A���A�~�A�/A�A��;A��RA���A���A���A��A�l�A�Q�A�9XA��A��yA���A�ZA��A��`A��A�r�A�XA�G�A�33A���A��wA��FA���A�|�A�K�A�-A�  A��;A��wA���A���A��A�p�A�dZA�G�A�A�ĜA���A���A��PA�dZA��A��A��A��A�r�A�?}A�7LA�7LA�(�A�A���A���A��DA�hsA�VA�7LA��A�
=A�A��A���A��9A���A�v�A�\)A�?}A�oA��/A�A���A�~�A�;dA�
=A���A�{A��A���A�O�A�A��
A���A�1A��A�+A���A��;A���A��FA�p�A�K�A�+A�A��;A���A�A��RA��A���A���A���A���A���A���A���A��\A��A�l�A�O�A�?}A�;dA�1'A��A��A�JA���A��;A�ĜA���A�|�A�\)A�"�A���A�`BA��A���A�Q�A�JA�ƨA��uA�\)A�JA�z�A���A�Q�A�A��RA�"�A��jA��7A�p�A�^5A�C�A�$�A��A�JA���A��
A��RA��\A�VA���A���A�z�A�A�A�  A��A��mA���A���A��A�bNA�;dA��FA�A�XA���A���A��-A��PA�ffA�A�A�oA���A���A�bNA�oA���A�33A���A�|�A�$�A�ȴA�jA�{A���A�x�A�"�A��A��A��FA�O�A���A�JA}A|��A|�A{XA{+A{
=Az��Az��Az�Az�yAz�/Az��Az�DAzjAzQ�Az{Ay�Ay�Ayt�Ax�HAx(�Aw�Av��Av�+AvVAvA�Au�;Au�Au��Au7LAt�9At^5ArbAq%Ap  Ao"�Am�;AlZAkx�Aj�Ajv�AjAi��AiO�Ah�jAh1Af�Ae��Ad�Ac�-Ac;dAbĜAbbNAb �Aax�Aa�A`�HA`�RA`�+A`I�A`A_�hA_&�A^�RA^^5A]�#A]
=A\E�A[hsAZ�`AZ��AZz�AZZAZ �AY�TAYAYO�AX^5AWƨAV�AV-AUC�AR�+AO�mAN��AN  AMC�AL��ALffAL{AK�7AJ�/AJ�DAI�AI`BAI�AH��AHr�AH$�AGAGt�AG\)AGS�AGK�AGC�AG;dAG+AG�AGoAG%AG%AF��AF�AF�AGAG�AG;dAGK�AG"�AF�RAFz�AFVAFI�AF9XAF1AE�wAEC�AD�9AC�AC�;AC�
AC��AC|�AC��ADADM�ADn�ADM�AD�AC��AC��AC�AD1'ADI�AD�AD��AEt�AE�AE�AE�TAF5?AGVAF��AE�AE�wAE��AE�AE��AFI�AFȴAG�AGAG�7AGhsAG+AG�AF�AF�9AF(�AE�-AE��AE�AE;dAD��AD��AD�/AE�AE;dAEl�AE|�AD�RADE�ADA�ADz�AE�AE�AEG�AEG�AEl�AEx�AEx�AEdZAEK�AE;dAE�AD�AD�AD�AD�AEXAE�#AF�AE��AE�7AE�AEx�AE`BAES�AE;dAE�AEoAEAD�AD�/AD�RAD��ADz�ADM�AD�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                AϬAϩ�Aϩ�Aϩ�AϮAϰ!Aϲ-Aϲ-Aϰ!Aϰ!Aϴ9Aϲ-Aϲ-Aϲ-Aϴ9A϶FAϼjA���A��A��A��mA��`A��HA��;A��
A���A�ĜA�ĜA�A���AϾwAϼjA϶FAϰ!A���A��A��jA�;dA���A�r�A�oA���A�ƨA�G�A���A�x�A�VA��#A��/A��`A���A��uA�7LA�ĜA��FA��-A�x�A���A��#A��A�?}A��A�n�A��^A���A�I�A��A�v�A�\)A��A�(�A�/A���A��FA��`A�S�A�7LA|1'Az��Aw�mAs��Ak&�Ad�A`VA[�AUl�AKƨAH$�AG�AG
=AE��AC��AD �AE33AF(�AF��AF�AE+AD��AD��AEdZAE%AE�-AEC�AD��AC��AB��AB  AA��AAt�A@�A>^5A>��A?S�A>$�A=
=A<�A;l�A;`BA9�wA8jA7�A6v�A6(�A5hsA4�A3��A2�+A2�A1��A0��A0bNA0�DA/��A.�9A.ZA-�7A-?}A-p�A-XA-K�A-�A,^5A+A+O�A*�/A)�^A(�RA(1'A'�hA'K�A&z�A%��A%��A&1'A&��A&��A&�RA&�uA&�uA&��A%�A%dZA%G�A$v�A$JA#hsA#�A"ĜA"A�A!�A!��A ��A �!A M�A�Ap�A/AȴA(�A��A�AdZA�A��A�uAz�AjA-A�^A|�AoA��A�!A�uAE�AdZA�RA�AZA{A��A|�AoA�`A�`A�jA�TA33A�A �A  A�#A`BA�A��AbA�TA�wAl�A`BAK�AVA��A�9AM�A��AK�A%A��A�A��A��A?}A�`A�;A�7A
��A
��A
5?A	��A	\)A��A5?A��AG�A"�A��A��An�A=qAJA�#A�hA`BA;dA�A��Ar�AQ�A�-A��A=qA�A��Ax�AG�A"�A �A ȴA ĜA j@�l�@�^5@���@�  @�S�@���@��R@�V@��T@�`B@��9@�ƨ@�o@�E�@��j@�o@�M�@���@�&�@��
@�l�@�+@���@��H@���@�!@�+@���@�b@�{@�X@�G�@�?}@�?}@�?}@�?}@���@�Z@�C�@��@���@�h@��@�j@���@㝲@�"�@⟾@��@��@�hs@�Z@�1@���@�|�@�+@��@���@އ+@�E�@�{@��@��T@��`@��@�dZ@�=q@�x�@�z�@ְ!@��#@�%@�Q�@���@���@�ff@�V@�=q@��@�V@��@ϝ�@���@�G�@�r�@���@�K�@ʇ+@�^5@���@�?}@�Ĝ@�r�@�1@�33@Ɨ�@��T@�7L@Ĭ@�z�@�r�@�Z@� �@�b@�  @öF@�;d@���@�@�v�@�-@��T@���@�&�@�r�@��
@�dZ@�@��R@�n�@�M�@�$�@��T@��h@���@� �@�K�@���@�7L@��`@���@��9@���@�r�@�I�@�(�@��m@���@��@�K�@���@�M�@�?}@��D@�1@��;@�\)@�E�@���@�hs@�7L@�Ĝ@��F@��H@���@��h@�7L@��@�%@��@��/@�j@�1@��F@�"�@���@���@�ff@�V@�=q@��T@�hs@��@�r�@�Z@��F@�;d@�ȴ@�^5@�$�@�J@�@��-@�x�@�X@�&�@��@���@�z�@�9X@���@�|�@��@���@�V@�$�@��@�{@��T@��-@��h@�p�@���@�A�@���@��F@�|�@�33@���@�=q@���@�`B@���@�z�@�1'@��@��@�l�@��@�n�@�^5@�M�@�$�@��@��T@��h@�G�@�7L@�/@��@�%@��@�(�@���@���@�|�@�\)@�
=@��R@�v�@�@��h@��@��j@�z�@�  @���@�dZ@�o@���@�~�@�5?@��@�@��h@�?}@�V@���@�(�@� �@��@��@���@���@�C�@��+@�=q@��#@�hs@��@��`@���@�j@�A�@� �@�1@���@�33@��!@�n�@�V@�5?@�@��T@�@�x�@�/@��`@��9@��D@�Q�@� �@���@��F@�dZ@�33@�o@��@���@��\@�V@�5?@�-@�{@���@�O�@���@��9@���@��@��@���@�\)@��@��\@�@��-@���@�x�@�O�@�?}@��@���@�Ĝ@�z�@�1@l�@;d@
=@~�@~v�@~{@}��@}�@}O�@|��@|�@{S�@{33@z�H@z^5@y��@y�7@x��@x�9@xbN@xQ�@xA�@x �@x  @w�;@wl�@wK�@w+@v�R@vff@v$�@u?}@tz�@t9X@t(�@s�m@s�F@st�@s@rn�@q�#@q��@q�^@qx�@qG�@pĜ@p1'@o\)@o;d@o+@o
=@n�+@m@m?}@l��@l��@lZ@l1@kƨ@k�F@k��@k��@k�@kdZ@j��@j^5@jJ@i��@h��@hbN@hA�@g�w@gl�@g+@fȴ@fE�@e�h@e/@e�@eV@d�@d�j@d��@dj@d�@c�F@c"�@bn�@b=q@a��@a�@a�#@a��@ahs@`��@`1'@_�P@_+@_�@^�y@^V@]�T@]�h@]p�@]?}@]�@\�D@[��@[33@Z��@Z�!@Z^5@Y��@Y��@Yhs@X�`@X �@W�P@WK�@V��@U��@U�@Up�@U/@T�@S�m@S33@R�H@R^5@QX@P�u@Pb@O��@Ol�@O\)@OK�@O
=@N��@NE�@N@M�h@L��@LI�@K�
@K�F@K��@K@J��@Jn�@J^5@JJ@I��@IG�@HQ�@H �@Hb@G��@GK�@F��@F�+@F5?@E��@E�@EO�@D�j@Dj@D9X@D1@C�F@C"�@B�!@BM�@A�7@A%@@��@@Ĝ@@Q�@@  @?�@?�w@?�P@?+@>��@>��@>v�@>5?@=�T@=��@=�h@=�@=p�@=?}@=V@<��@;33@:�@9�^@9�7@97L@8��@8�9@8Q�@8b@7�;@7�w@7K�@6��@6�@6v�@6$�@6@5�@5��@5�h@5/@4��@4I�@41@3�m@3�m@3ƨ@3t�@3C�@2�@2��@2�\@1��@17L@1&�@1%@0�`@0Ĝ@0��@0�u@0Q�@/�@/��@/��@/��@/|�@/;d@/
=@.E�@-��@-`B@-/@,��@,��@,�@,�D@,z�@,Z@,9X@,1@+��@+@*�H@*��@*�H@*��@*��@*��@*�!@*n�@)�@)��@)hs@)7L@)%@(��@(��@( �@'�P@'�@&��@&�@&��@&�+@%�T@%`B@$�j@$�D@$I�@#��@#ƨ@#��@#S�@"~�@"n�@"n�@"~�@"^5@"=q@!��@!�#@!��@!��@!hs@!&�@ �9@ b@   @   @�@�w@l�@�@��@V@{@�@�@�T@@`B@V@�j@j@1@��@o@�H@�!@~�@^5@-@��@�#@�7@&�@��@�`@��@��@�9@�u@�@r�@bN@Q�@b@��@�@|�@l�@;d@
=@�@��@ff@{@�@@�@O�@�@�@�j@z�@Z@(�@��@ƨ@�F@t�@o@��@�\@~�@^5@-@��@�@�^@x�@�@�`@�@A�@ �@b@b@b@  @�@��@��@5?@@�T@�-@�@p�@/@�@�@��@Z@9X@1@��@dZ@C�@"�@
�!@
n�@
n�@
n�@
M�@
-@
J@	��@	�#@	�AϮAϮAϮAϬAϧ�Aϥ�AϬAϮAϩ�Aϧ�Aϧ�Aϧ�AϬAϮAϬAϬAϩ�Aϰ!Aϲ-A϶FAϰ!Aϰ!Aϰ!Aϴ9Aϲ-Aϲ-Aϰ!AϮAϰ!Aϲ-Aϲ-Aϰ!Aϲ-Aϴ9Aϲ-Aϴ9Aϲ-Aϰ!Aϰ!Aϲ-Aϴ9Aϴ9Aϲ-Aϰ!Aϰ!Aϲ-Aϴ9A϶FAϴ9Aϴ9Aϰ!Aϰ!Aϲ-A϶FA϶FAϸRAϴ9Aϲ-Aϲ-A϶FAϺ^AϸRA϶FAϴ9Aϲ-Aϲ-AϼjA���A���A�ĜA�ĜA�ȴA���A���A���A���A���A���A���A��;A��yA��A��A��A��yA��yA��A��A��yA��`A��`A��mA��yA��mA��`A��`A��`A��mA��`A��`A��HA��;A��HA��TA��TA��TA��;A��;A��/A��/A��;A��HA��#A��
A���A���A���A���A���A���A���A�ƨA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ĜA�A�A�A�ĜA�ƨA�ƨA�ĜA���A���A���A�A�ĜA�ĜA�A���AϾwAϾwA���A���A���A���AϾwAϼjAϼjAϼjAϺ^AϼjAϾwAϾwAϾwAϺ^A϶FA϶FA϶FA϶FAϸRA϶FA϶FAϴ9Aϰ!Aϰ!Aϲ-Aϴ9Aϲ-AϮAϮAϩ�Aϩ�Aϡ�A�Q�AάA��/Aʕ�A�{AŃA��#A���A�A�A��A��A�;dA�~�A�G�A�(�A��uA��A��A�?}A���A�ZA���A�5?A�ȴA�n�A���A�33A�7LA��+A�/A��7A��A���A��uA�|�A�`BA�9XA��A��DA�&�A��/A���A���A���A���A��FA���A��A��DA�1A��A���A���A�n�A�1'A���A��+A�{A���A�7LA��yA���A�~�A�x�A�t�A�r�A�p�A�p�A�x�A��A��hA���A�/A���A���A���A��7A�+A��TA��uA��A���A�5?A��^A��uA�l�A�C�A�5?A��A���A��\A�Q�A��A��wA��wA��
A���A�~�A�/A�A��;A��RA���A���A���A��A�l�A�Q�A�9XA��A��yA���A�ZA��A��`A��A�r�A�XA�G�A�33A���A��wA��FA���A�|�A�K�A�-A�  A��;A��wA���A���A��A�p�A�dZA�G�A�A�ĜA���A���A��PA�dZA��A��A��A��A�r�A�?}A�7LA�7LA�(�A�A���A���A��DA�hsA�VA�7LA��A�
=A�A��A���A��9A���A�v�A�\)A�?}A�oA��/A�A���A�~�A�;dA�
=A���A�{A��A���A�O�A�A��
A���A�1A��A�+A���A��;A���A��FA�p�A�K�A�+A�A��;A���A�A��RA��A���A���A���A���A���A���A���A��\A��A�l�A�O�A�?}A�;dA�1'A��A��A�JA���A��;A�ĜA���A�|�A�\)A�"�A���A�`BA��A���A�Q�A�JA�ƨA��uA�\)A�JA�z�A���A�Q�A�A��RA�"�A��jA��7A�p�A�^5A�C�A�$�A��A�JA���A��
A��RA��\A�VA���A���A�z�A�A�A�  A��A��mA���A���A��A�bNA�;dA��FA�A�XA���A���A��-A��PA�ffA�A�A�oA���A���A�bNA�oA���A�33A���A�|�A�$�A�ȴA�jA�{A���A�x�A�"�A��A��A��FA�O�A���A�JA}A|��A|�A{XA{+A{
=Az��Az��Az�Az�yAz�/Az��Az�DAzjAzQ�Az{Ay�Ay�Ayt�Ax�HAx(�Aw�Av��Av�+AvVAvA�Au�;Au�Au��Au7LAt�9At^5ArbAq%Ap  Ao"�Am�;AlZAkx�Aj�Ajv�AjAi��AiO�Ah�jAh1Af�Ae��Ad�Ac�-Ac;dAbĜAbbNAb �Aax�Aa�A`�HA`�RA`�+A`I�A`A_�hA_&�A^�RA^^5A]�#A]
=A\E�A[hsAZ�`AZ��AZz�AZZAZ �AY�TAYAYO�AX^5AWƨAV�AV-AUC�AR�+AO�mAN��AN  AMC�AL��ALffAL{AK�7AJ�/AJ�DAI�AI`BAI�AH��AHr�AH$�AGAGt�AG\)AGS�AGK�AGC�AG;dAG+AG�AGoAG%AG%AF��AF�AF�AGAG�AG;dAGK�AG"�AF�RAFz�AFVAFI�AF9XAF1AE�wAEC�AD�9AC�AC�;AC�
AC��AC|�AC��ADADM�ADn�ADM�AD�AC��AC��AC�AD1'ADI�AD�AD��AEt�AE�AE�AE�TAF5?AGVAF��AE�AE�wAE��AE�AE��AFI�AFȴAG�AGAG�7AGhsAG+AG�AF�AF�9AF(�AE�-AE��AE�AE;dAD��AD��AD�/AE�AE;dAEl�AE|�AD�RADE�ADA�ADz�AE�AE�AEG�AEG�AEl�AEx�AEx�AEdZAEK�AE;dAE�AD�AD�AD�AD�AEXAE�#AF�AE��AE�7AE�AEx�AE`BAES�AE;dAE�AEoAEAD�AD�/AD�RAD��ADz�ADM�AD�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
CaB
B�B
B�B
B�B
B�B
B�B
B�B
C-B
C-B
B�B
B�B
C-B
C�B
C�B
C�B
DgB
F�B
OB
W
B
jB
o5B
poB
qAB
q�B
r|B
sMB
s�B
sMB
sMB
r�B
q�B
oiB
k�B
a|B
��B
�B�	B�&BB�B�"B�BB�BB�"B!�BEB6�B(�B2�B1�B"4B
�B��B�B�fB�yB��B��B�B�FB|�Bo5BjKBe�B\)BK�B-BB
�B
�TB
��B
�MB
r�B
Q�B
*�B	��B	֡B	�mB	�CB	�SB	g�B	I�B	4nB	)*B	4B		lB	_B	!�B	H�B	?�B	_�B	��B	��B	��B
!�B
>B
m)B
�B
�MB
�uB
�3B
�HB
�HB
�B
��B
�gB
�RB
�^B
�B
��B
�}B
�QB
�`B
�5B
�sB
�&B
��B
՛B
�-B
�'B
��B
�B
��B
�KB
��B
��B
�9B
ɆB
�B
�B
ŢB
�B
�0B
�RB
��B
�aB
�6B
��B
� B
�3B
�HB
�0B
�*B
��B
��B
��B
��B
��B
��B
�LB
��B
�_B
�B
�RB
��B
�'B
�UB
��B
�-B
��B
�wB
�BB
�BB
�$B
�zB
�hB
�aB
��B
��B
�eB
�XB
��B
��B
��B
�zB
�FB
��B
�B
�hB
��B
�-B
��B
�-B
��B
��B
�OB
�B
�CB
�=B
�	B
�1B
��B
�YB
��B
�B
��B
��B
��B
��B
�1B
��B
�YB
�B
�GB
�AB
~�B
zB
y>B
t�B
s�B
s�B
r�B
pB
o�B
l�B
l"B
l"B
l�B
l"B
l�B
k�B
kB
j�B
m�B
hsB
f2B
d&B
cTB
aHB
^�B
^B
[�B
[#B
V9B
Q�B
O�B
M�B
N�B
M6B
L�B
M�B
IRB
I�B
I�B
I�B
J#B
IB
K^B
IRB
IB
H�B
GB
E�B
E9B
DgB
C-B
A�B
AUB
C-B
CaB
@�B
A B
?HB
>wB
>wB
=qB
=qB
;�B
:�B
;�B
:�B
7�B
6�B
6zB
4B
2aB
2-B
1�B
0�B
/�B
/�B
/B
,qB
,�B
,qB
(�B
(�B
&�B
(�B
&�B
%FB
$�B
$@B
#�B
#nB
"�B
!�B
#nB
"�B
!�B
~B
~B
�B
B
�B
�B
OB
�B
!B
	B
kB
B
�B
7B
�B
�B
1B
�B
�B
1B
�B
eB
�B
+B
�B
�B
�B
$B
$B
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
(B
�B
"B
�B
.B
�B
�B
�B
�B
�B
�B
hB
oB
hB
hB
�B
B
�B
{B
B
�B
B
�B
{B
�B
�B
SB
�B
�B
�B
�B
�B
�B
SB
SB
SB
�B
�B
�B
�B
SB
B
�B
B
SB
�B
�B
�B
SB
�B
B
�B
B
�B
SB
B
YB
�B
�B
�B
�B
�B
1B
1B
�B
�B
B
�B
�B
�B
eB
7B
�B
qB
	B
�B
�B
B
YB
�B
�B
�B
�B
_B
�B
�B
�B
+B
+B
+B
�B
�B
B
�B
B
B
CB
B
�B
qB
=B
qB
�B
�B
!�B
!�B
"hB
"�B
#:B
#�B
#�B
#�B
$�B
$tB
$tB
$�B
$�B
$�B
$�B
%B
%B
%zB
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&LB
%�B
'B
'�B
'RB
'�B
'�B
($B
(�B
)_B
)�B
)�B
)�B
*�B
+B
+6B
+6B
+kB
+�B
+�B
+kB
+�B
+�B
,B
+kB
,�B
-B
-B
,�B
,�B
,�B
-�B
-�B
-wB
.B
-wB
-�B
-�B
-wB
-CB
-�B
-�B
-�B
-B
,�B
-�B
-�B
-wB
-�B
.B
.B
/B
.}B
.�B
.�B
/OB
/OB
1'B
1�B
1�B
1�B
2-B
1�B
1�B
2�B
4B
4B
4nB
5tB
5?B
5�B
6zB
6�B
6�B
6zB
6zB
6�B
8B
8�B
8�B
8�B
8�B
8�B
8�B
9$B
9$B
9�B
9�B
9�B
:^B
:�B
:�B
:�B
;0B
;�B
;�B
;�B
<B
<6B
<�B
<�B
<�B
<jB
<�B
=<B
=�B
>BB
>B
=�B
>B
>�B
?B
?}B
?�B
@B
A B
A B
A B
AUB
A�B
AUB
A�B
A�B
B'B
B[B
C�B
CaB
CaB
CaB
CaB
C�B
D3B
D3B
D3B
DgB
EB
EmB
FtB
FB
F�B
F�B
GEB
G�B
HB
HKB
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
LdB
M6B
MB
M6B
MjB
MjB
M�B
NB
N�B
OB
N�B
N�B
OB
OB
OvB
O�B
PHB
O�B
O�B
PB
O�B
P�B
PHB
PHB
QNB
Q�B
R B
Q�B
R B
Q�B
Q�B
Q�B
Q�B
RTB
Q�B
Q�B
Q�B
R�B
RTB
RTB
R�B
R�B
R�B
S�B
S�B
UgB
U2B
T�B
U2B
UgB
UgB
UgB
U�B
U�B
VB
V�B
W
B
V�B
W?B
W
B
W
B
W?B
W�B
WsB
WsB
WsB
WsB
W?B
WsB
W�B
XB
W�B
W�B
XEB
X�B
YB
YB
YKB
YB
YB
YB
Y�B
ZQB
ZB
Z�B
Z�B
[#B
Z�B
[�B
[�B
[#B
Z�B
Z�B
Z�B
[WB
[#B
Z�B
[�B
\)B
\�B
\�B
]�B
]�B
]�B
]dB
]�B
^B
^5B
^jB
^�B
`B
`B
`vB
_�B
_�B
`�B
`�B
`vB
`vB
`�B
`�B
aHB
bB
a�B
a|B
bNB
bB
bNB
b�B
b�B
c B
c B
b�B
c�B
c�B
cTB
cTB
c�B
dZB
d�B
d�B
e`B
e`B
e,B
e`B
e�B
e�B
e�B
e�B
f2B
f�B
ffB
f�B
f�B
gB
g8B
gmB
gmB
gmB
g8B
gmB
g8B
gB
iDB
iyB
i�B
i�B
jB
jKB
jB
jB
j�B
j�B
j�B
kQB
kQB
kB
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
l�B
l�B
m)B
l�B
m)B
m]B
m]B
m�B
m�B
m�B
o B
o B
o B
o B
o B
o5B
o5B
o5B
oiB
o�B
o�B
o�B
o�B
p;B
p;B
pB
qAB
qvB
q�B
q�B
rB
rB
r|B
r|B
r|B
r|B
r|B
r|B
sB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
tTB
u�B
u�B
u�B
v+B
v`B
v�B
v�B
w2B
w�B
w�B
w�B
w�B
w�B
w�B
xlB
x�B
x�B
x�B
x�B
x�B
y	B
x�B
y�B
zDB
zDB
z�B
{B
{�B
{�B
|PB
|PB
|PB
|PB
|�B
|�B
}VB
}�B
}�B
}�B
}VB
}�B
}�B
~(B
~(B
~(B
~]B
~(B
~(B
~(B
~]B
~�B
~�B
.B
�B
�B
�4B
��B
�B
�oB
�oB
��B
��B
�B
�B
��B
��B
�GB
�GB
�GB
�GB
�{B
�{B
��B
��B
��B
��B
�B
�MB
�MB
��B
��B
��B
��B
�B
�SB
��B
��B
��B
�%B
�YB
��B
��B
��B
�+B
�_B
�+B
��B
��B
��B
��B
�fB
��B
�B
�lB
�lB
�lB
��B
��B
��B
�	B
�rB
��B
��B
�xB
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
�"B
�"B
�VB
�VB
��B
��B
��B
�(B
�(B
�\B
�\B
��B
�.B
��B
�.B
�4B
�4B
�4B
�hB
�hB
��B
��B
�B
�B
� B
A�B
A�B
B�B
B�B
C�B
C�B
B�B
A�B
B�B
D3B
C�B
B[B
AUB
B'B
C-B
C�B
C�B
B�B
A�B
@�B
CaB
C�B
CaB
A�B
B�B
B'B
C�B
DgB
B�B
A�B
B'B
B�B
C�B
B�B
C-B
A�B
B[B
D3B
E9B
D�B
C-B
D�B
CaB
DgB
D�B
D�B
C�B
C-B
B�B
B�B
D�B
C�B
D�B
D3B
C�B
FtB
B[B
C�B
C�B
C�B
G�B
C�B
C�B
C-B
C�B
D�B
B�B
?HB
P�B
I�B
P�B
O�B
N�B
U�B
LdB
PHB
T,B
R�B
QB
]�B
e�B
e,B
d�B
l�B
k�B
m�B
l�B
l�B
ncB
o�B
p�B
o�B
n�B
oiB
p�B
qAB
qB
poB
p�B
p�B
q�B
rGB
rB
p�B
p�B
p�B
q�B
r|B
r|B
rGB
q�B
qAB
q�B
rGB
sB
s�B
sB
rGB
rB
r�B
sB
tB
t�B
tB
s�B
r�B
r�B
r�B
s�B
t�B
tTB
s�B
sMB
r�B
rGB
sB
tB
tTB
s�B
sB
rB
rB
rB
r�B
s�B
s�B
q�B
qvB
p�B
qAB
qvB
q�B
rB
qvB
qAB
poB
o5B
ncB
m�B
ncB
o5B
ncB
m�B
l"B
jB
i�B
iDB
h>B
h�B
g8B
d�B
bNB
`�B
`B
\�B
Z�B
U�B
R�B
V�B
l�B
�4B
�hB
�B
�B
�?B
��B
�)B
֡B
�0B
�BH�BkBXEBuZB�uB��B�RBɺB�B��B�B��B�B��B;BPB�B��BPB�B�B��B��B��BYB�B�B�B�B�rB��B�]B��B�BGB�B0�B�B�B�B4B�B�B�B�B�B�B{BB�B�VB�rB�B�JB��B��B�VB��B 4B�>B�B,qBC�BC-BO�BPBHB?}B8B8RBP�B-wB/B2-B*eB+kB+�B.IB#�B!B,�B(�B�B6FBB�B?HBA B8�B7LB5?B0�B0!B1�B2-B0!B.B,qB,=B,qB+�B&�B!�B�BOB�B�BDB�B�B�BMB+BBSBBGBuBMB�B��B��B��B��B�fB��B�B� B�]B��B�B�rB�B�B�2B�B�sBܒB�/B��B��B�2B��B�mB��B�TB�NB�NB�6B�^B͟B�RB��B��B��B��B�UB�3B�wB�RB��B��B��B��B�[B�RB�nB�B��B��B��B��B�hB�@B��B�MB�4B}"B�B�oBzxBy�Bv�Bt�Bp�BpBo�Bn/Bm]Bl�Bk�Bk�Bk�BjBjBkBjBj�Bk�Bf2BcTBd&Bd�B_;B`BB^�B\�B[WBX�BXyBR�BS[BXEB8�B<jBCaBC�B9$B1�B+B(�B$tB2-B!B
	B+B�BB
�fB
�]B
�B
��B
��B
�TB
ݘB
�dB
�)B
��B
�QB
��B
՛B
�aB
�XB
�gB
��B
�wB
��B
�OB
��B
�B
��B
��B
�nB
�-B
��B
�nB
��B
�4B
|B
{�B
z�B
tTB
q�B
t�B
o5B
j�B
m)B
f�B
`�B
\�B
VB
P�B
FB
B'B
<6B
5�B
2�B
1[B
:�B
B
B
�B
(�B
#:B

�B	�B	��B	�B	��B	ٴB	�/B	�B	��B	�B	�yB	�B	�vB	� B	�9B	ϫB	ϫB	��B	̘B	�mB	ǮB	��B	�FB	�BB	�LB	��B	�hB	��B	�hB	��B	�B	�B	��B	��B	�eB	��B	��B	��B	��B	�AB	|B	y>B	u�B	sB	x�B	u�B	{B	~(B	j�B	`BB	`�B	ZQB	R�B	S�B	VmB	O�B	JXB	IRB	J�B	F�B	H�B	EB	EB	@�B	<B	A�B	A B	<B	@B	5�B	,=B	(�B	($B	(�B	#�B	 'B	+kB	&�B	"hB	 \B	�B	&�B	N<B	.B	 �B	�B	�B	�B	B	~B	uB	�B	DB	oB	�B		lB	~B	_B	�B	~B	fB	_B	%B	YB	�B	%B	�B	�B	1B	�B	1B	1B		B		B	
�B	�B	B	.IB	=qB	H�B	G�B	H�B	H�B	I�B	D3B	MB	J�B	GzB	B[B	0�B	>wB	C�B	@B	>�B	.�B	T�B	R�B	\)B	_pB	a�B	a�B	dZB	ZB	|�B	��B	��B	�VB	��B	��B	�7B	��B	��B	�B	چB	��B	��B	ޞB	�)B	�pB	� B	�
B
�B
�B
�B
�B
=B
IB
�B
,B
-B
33B
?�B
FB
AUB
?}B
=<B
Q�B
[�B
c�B
rB
�%B
u�B
rB
g8B
��B
�(B
�hB
� B
��B
��B
��B
��B
��B
�SB
��B
��B
�@B
�uB
��B
��B
�!B
�zB
��B
бB
��B
�HB
�HB
�}B
��B
�HB
�6B
��B
��B
��B
��B
�vB
�B
�HB
�6G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                B
=B
<�B
<�B
<AB
<vB
<vB
<�B
<�B
<�B
<vB
<�B
<�B
=�B
=�B
=|B
>B
@ZB
H�B
P�B
d1B
h�B
j!B
j�B
k�B
l.B
l�B
m4B
l�B
l�B
lbB
k\B
iB
elB
[.B
��B
�VB��B��B��B��B��B��B�B�B��B��B}B>�B0�B"sB,|B+�B�B�B�qB��B�B�+BħB�]B��B��BvkBh�Bc�B_GBU�BEyB&�B
��B
�4B
�B
�jB
��B
lbB
K�B
$KB	�B	�SB	�B	��B	B	a�B	ClB	. B	"�B	
�B	B	B	HB	B2B	9cB	Y�B	�?B	�uB	�B
HB
7�B
f�B
|�B
��B
�'B
��B
��B
��B
ƳB
��B
�B
�B
�B
��B
�8B
�/B
�B
�B
��B
�%B
��B
̤B
�MB
��B
��B
�vB
��B
B
��B
ŭB
�pB
��B
�8B
��B
��B
�TB
��B
��B
�B
�TB
�B
��B
��B
��B
��B
��B
��B
��B
��B
�8B
��B
�KB
�3B
��B
��B
��B
�B
��B
�B
�jB
��B
�B
��B
��B
�|B
�)B
��B
��B
��B
�,B
�B
�B
�5B
�WB
�B
�
B
��B
�aB
�UB
�,B
��B
�3B
��B
�B
��B
��B
�}B
��B
�pB
��B
�B
��B
��B
��B
��B
��B
��B
�B
�9B
��B
�kB
�^B
�XB
��B
��B
��B
�B
}�B
|�B
{�B
xwB
s�B
r�B
n�B
m4B
m�B
l�B
i�B
iPB
f�B
e�B
e�B
f=B
e�B
frB
e�B
d�B
deB
gCB
b%B
_�B
]�B
]B
Z�B
XPB
W�B
U>B
T�B
O�B
KiB
I]B
G�B
HWB
F�B
FJB
GQB
CB
C�B
C8B
ClB
C�B
B�B
EB
CB
B�B
B2B
@�B
?�B
>�B
>B
<�B
;�B
;B
<�B
=B
:�B
:�B
8�B
8)B
8)B
7#B
7#B
5KB
4yB
5B
4yB
1gB
0�B
0,B
-�B
,B
+�B
+BB
*pB
)�B
)5B
(�B
&#B
&WB
&#B
"�B
"sB
 gB
"�B
 �B
�B
�B
�B
�B
 B
�B
�B
 B
NB
HB
0B
0B
�B
�B
^B
^B
B
jB
�B
�B
B
�B
LB
�B
LB
EB
�B
LB
�B
�B
�B
B
EB
�B
EB
�B
?B
�B
�B
9B
9B
9B
�B
�B
gB
aB
�B
�B
�B

}B
�B
<B
�B
<B
	�B
	�B
	wB
	CB
	CB

IB

}B
B
!B
B
B
�B
�B
aB
-B
�B
aB
�B
�B
-B
gB
gB
B
�B
�B
nB
9B
nB
nB
B
B
B
�B
nB
9B
9B
B
�B
�B
�B
B
9B
nB
9B
B
9B
�B
�B
�B
gB
B
�B
B
�B
LB
zB
�B
zB
�B
�B
LB
LB
�B
�B
�B
LB
B
�B
�B
#B
�B
�B
RB
�B
B
�B
�B
tB
�B
B
�B
EB
�B
�B
�B
�B
�B
LB
�B
�B
�B
�B
�B
�B
�B
#B
�B
#B
�B
dB
}B
�B
B
NB
�B
UB
�B
�B
[B
&B
&B
[B
[B
�B
�B
�B
�B
,B
aB
�B
 3B
 3B
 3B
 gB
 �B
 gB
�B
�B
 �B
!mB
!B
!�B
!mB
!�B
"sB
#B
#yB
#�B
#�B
$�B
$�B
$�B
$�B
%B
%�B
%�B
%B
%QB
%�B
%�B
%B
&�B
&�B
&�B
&�B
&�B
&WB
'�B
'^B
')B
'�B
')B
'^B
'^B
')B
&�B
'^B
'^B
'^B
&�B
&�B
'�B
'^B
')B
'�B
'�B
'�B
(�B
(/B
(dB
(�B
)B
)B
*�B
+�B
+vB
+BB
+�B
+vB
+�B
,|B
-�B
-�B
. B
/&B
.�B
/�B
0,B
0`B
0`B
0,B
0,B
0�B
1�B
28B
28B
28B
28B
2�B
2�B
2�B
2�B
3>B
3sB
3>B
4B
4EB
4yB
4EB
4�B
5KB
5KB
5B
5�B
5�B
6QB
6QB
6�B
6B
6QB
6�B
7�B
7�B
7�B
7�B
7�B
8�B
8�B
9/B
9�B
9�B
:�B
:�B
:�B
;B
;;B
;B
;pB
;pB
;�B
<B
=HB
=B
=B
=B
=B
=|B
=�B
=�B
=�B
>B
>�B
?B
@&B
?�B
@ZB
@ZB
@�B
A`B
A�B
A�B
B2B
B2B
B2B
BfB
B�B
B�B
C�B
ClB
ClB
D>B
D>B
DsB
FB
F�B
F�B
F�B
GB
GB
GQB
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I(B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
K B
KiB
K�B
K�B
K�B
K�B
K�B
KiB
KiB
LB
KiB
K�B
K�B
L;B
LB
LB
L�B
L�B
L�B
MAB
M�B
OB
N�B
N�B
N�B
OB
OB
OB
OMB
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
QZB
Q%B
Q%B
Q%B
Q%B
P�B
Q%B
Q�B
Q�B
Q�B
QZB
Q�B
R`B
R�B
R�B
R�B
R�B
R�B
S1B
S�B
TB
S�B
T8B
T�B
T�B
TlB
U�B
U>B
T�B
T�B
TlB
TlB
U	B
T�B
TlB
UrB
U�B
V�B
VxB
WJB
WJB
WJB
WB
WJB
W�B
W�B
XB
XPB
Y�B
Y�B
Z(B
Y�B
Y�B
Z�B
Z\B
Z(B
Z(B
Z�B
Z\B
Z�B
[�B
[cB
[.B
\ B
[�B
\ B
\4B
\4B
\�B
\�B
\iB
]oB
]oB
]B
]B
]oB
^B
^AB
^AB
_B
_B
^�B
_B
_{B
_�B
_{B
_�B
_�B
`MB
`B
`�B
`�B
`�B
`�B
aB
aB
aB
`�B
aB
`�B
`�B
b�B
c+B
c_B
c_B
c�B
c�B
d1B
d1B
deB
deB
d�B
eB
eB
d�B
elB
elB
elB
elB
elB
e�B
e�B
frB
frB
f�B
f�B
f�B
f�B
gB
gB
gxB
gxB
gxB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iB
i�B
i�B
i�B
iPB
i�B
i�B
i�B
j�B
k(B
k\B
k�B
k�B
k�B
l.B
l.B
l.B
l.B
l.B
l.B
l�B
m4B
m4B
m�B
m�B
m�B
m�B
m�B
m�B
nB
o@B
o@B
o�B
o�B
pB
pFB
pFB
p�B
qLB
q�B
qLB
q�B
q�B
q�B
rB
r�B
r�B
rSB
rSB
r�B
r�B
r�B
s�B
s�B
s�B
t�B
u1B
u�B
u�B
vB
vB
vB
vB
v7B
v7B
wB
w=B
wqB
w=B
wB
wqB
w�B
w�B
w�B
w�B
xB
w�B
w�B
w�B
xB
x�B
x�B
x�B
yIB
y~B
y�B
z�B
z�B
{!B
{!B
{UB
{�B
{�B
{�B
|\B
|�B
|�B
|�B
|�B
|�B
}-B
}-B
}bB
}bB
}bB
}�B
}�B
}�B
}�B
~hB
~hB
~hB
~�B
~�B
B
:B
nB
nB
�B
�B
�@B
�tB
��B
��B
�B
��B
�FB
�zB
��B
��B
�B
��B
��B
�B
�B
�B
��B
��B
��B
��B
�$B
�XB
��B
�*B
�^B
��B
�^B
��B
�^B
�^B
��B
��B
�kB
�kB
��B
��B
��B
�B
�B
�<B
�<B
�qB
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�OB
��B
��B
��B
��B
;pB
;pB
<AB
<vB
=�B
=HB
<AB
;;B
<AB
=�B
=|B
<B
;B
;�B
<�B
=HB
=HB
<AB
;;B
:�B
=B
=HB
=B
;�B
<AB
;�B
=|B
>B
<�B
;�B
;�B
<�B
=�B
<�B
<�B
;;B
<B
=�B
>�B
>NB
<�B
>NB
=B
>B
>NB
>NB
=�B
<�B
<�B
<�B
>NB
=|B
>NB
=�B
=HB
@&B
<B
=�B
=�B
=HB
A`B
=|B
=|B
<�B
=|B
>�B
<�B
8�B
J�B
C�B
J�B
I�B
H�B
OMB
FB
I�B
M�B
LoB
J�B
WJB
_�B
^�B
^uB
frB
elB
gxB
frB
frB
hB
i�B
j�B
i�B
h~B
iB
jVB
j�B
j�B
j!B
jVB
jVB
k�B
k�B
k�B
j�B
jVB
j�B
k�B
l.B
l.B
k�B
k\B
j�B
k�B
k�B
l�B
mhB
l�B
k�B
k�B
l�B
l�B
m�B
n:B
m�B
mhB
l�B
lbB
l�B
m4B
n:B
nB
m�B
l�B
lbB
k�B
l�B
m�B
nB
mhB
l�B
k�B
k�B
k�B
lbB
m4B
m4B
k�B
k(B
j�B
j�B
k(B
k�B
k�B
k(B
j�B
j!B
h�B
hB
g�B
hB
h�B
hB
gxB
e�B
d1B
c�B
b�B
a�B
bYB
`�B
^�B
\ B
Z�B
Y�B
VxB
T8B
OMB
L;B
PSB
f=B
��B
�B
�4B
�iB
��B
ۗB
��B
�SB
��B
�CBBfBd�BQ�BoB|'B�<B�B�lB�_BۗB�SBԠB��B�B��BB��B�=BB
}B�UB��B��B�CB B�bB6B�B��B�$B�B�B�B��B��B<B*pBLBqB�B
�BqBqB
IB�BzB?B-B�B�\B�B�$B��B��B��B��B�B�CB��B��B��B&#B=HB<�BI�BI�BA�B9/B1�B2BJcB')B(�B+�B$B%B%QB'�B�B�B&WB"�BnB/�B<�B8�B:�B28B0�B.�B*pB)�B+BB+�B)�B'�B&#B%�B&#B%QB gB�B^BB3B	�B�B�BUB�hB��B �B��B�B��B��B�'B��B�1B�_B�=B�{B�LB�B�SB�PB�B�B�B�:B�$B�B��B��B�AB�%B�DB��B؅BէB��BΰB�BΰB�B� B� B��B�B�QB�BħB��B��B��B�B��B�)B�B�mB�mB�>B�BB�B�B� B��B�3B��B�nB�tB�B��B��B}�By�Bv�ByIB{!Bt*Bs�BpFBn:BjVBi�BiPBg�BgBf=BelBe�BelBc�Bc�Bd�Bd1BdeBe7B_�B]B]�B^ABX�BY�BXPBVxBU	BR`BR+BLoBMBQ�B28B6B=B=|B2�B+BB$�B"?B&B+�B�B�B �B
�nB�B
�B
�B
��B
��B
ݣB
�B
�JB
�B
��B
ԠB
�B
ΰB
�MB
�B
�
B
�B
�QB
�)B
�<B
�B
��B
��B
�yB
��B
� B
��B
��B
� B
��B
y�B
u�B
ueB
t_B
nB
k\B
n:B
h�B
d�B
f�B
`�B
Z�B
VDB
O�B
JcB
?�B
;�B
5�B
/�B
,|B
+B
4EB
�B
�B
�B
"�B
�B
�B	�oB	�B	عB	ҔB	�fB	��B	��B	ҔB	��B	�+B	��B	�(B	��B	��B	�]B	�]B	ǅB	�JB	�B	�`B	ȋB	��B	��B	��B	�|B	�B	��B	�B	�vB	��B	��B	�TB	��B	�B	�EB	��B	�BB	~�B	{�B	u�B	r�B	o�B	l�B	rSB	ouB	t�B	w�B	d�B	Y�B	Z\B	TB	LoB	MuB	PB	I]B	D
B	CB	D>B	@ZB	BfB	>�B	>�B	:�B	5�B	;;B	:�B	5�B	9�B	/ZB	%�B	"�B	!�B	"?B	�B	�B	%B	 3B	B	B	jB	 �B	G�B	'�B	wB	�B	�B		�B	�B	0B	'B	[B	�B	!B	6B	B	0B	B	�B	0B	B	B��B	 B��B��B��B	zB	�B	�B	�B	�B	�B	�B	XB	�B	�B	'�B	7#B	B2B	A�B	BfB	B2B	ClB	=�B	F�B	D�B	A,B	<B	*�B	8)B	=�B	9�B	8�B	(dB	N|B	LoB	U�B	Y"B	[cB	[cB	^B	S�B	vkB	}�B	�<B	�B	�3B	�?B	��B	�^B	ǅB	�1B	�8B	ًB	�rB	�PB	��B	�"B	�B	�B
�B
�B
�B
EB
�B
�B
�B
%�B
&�B
,�B
9�B
?�B
;B
9/B
6�B
K�B
U>B
]oB
k�B
�B
ouB
k�B
`�B
�wB
��B
�B
��B
�OB
��B
�[B
�?B
�aB
�B
�^B
�?B
��B
�'B
��B
|�B
��B
�,B
�~B
�cB
ɑB
��B
��B
�/B
˞B
��B
��B
ȋB
ɑB
̤B
ʗB
�(B
ƳB
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223249                            20230426223249AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622324920230426223249  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622324920230426223249QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622324920230426223249QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               