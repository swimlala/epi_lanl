CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:50Z creation      
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
resolution        =���   axis      Z        p  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [P   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  c,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p ?<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ^�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �X   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223250  20230426223250  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�ޔJd�@�ޔJd�11  @�����@�����@*N�1���@*N�1����cp@$�=��cp@$�=�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�  @�@B�\@�G�@�G�@�G�@�G�AG�A  A\)A,(�A?\)A`  A�  A�Q�A�Q�A�  A�  A�  A�Q�A�Q�B   B  B�
B�
B   B((�B0  B8(�B@  BH  BP(�BW�
B_�Bg�
Bp(�BxQ�B�  B��B��B�  B�{B�{B�Q�B��B��B�  B��B�  B�  B�  B��B�  B�{B�  B�  B�{B�  B�  B�  B�{B�  B��B�  B�{B�{B�  B��B��B��C  C  C��C  C

=C
=C  C
=C
=C
=C  C��C  C{C{C {C"
=C$
=C&
=C(
=C)��C+��C-��C/��C1��C4  C6
=C8{C:  C;��C=��C?��CA�CD  CF
=CH  CJ  CL  CN  CP  CR  CT  CU��CW��CZ
=C\
=C^  C`
=Cb  Cc��Ce��Cg��Ci��Ck��Cm��Cp  Cr
=Ct  Cv  Cx  Cz  C|  C~  C�  C�C�
=C�C���C���C�  C�C�C���C�  C�
=C�  C�C�C�C�C�  C���C���C�C���C���C���C���C�  C�C�C�  C���C���C�C�C�  C�C�  C�C�  C���C�C�C�C�C�C�C�C�  C�  C���C���C���C�C�
=C�
=C�C�  C���C���C���C���C���C���C�  C�C�  C�  C�  C���C���C���C���C�C�
=C���C���C�C�  C�  C���C���C���C�C�C���C���C���C���C�  C�  C�  C�  C�C�C�C�C�C�C���C���C���C�  C���C��C���C���C�  C�C�
=C�C���C�  C�C�C�
=C�C���C�  C�C�C�C�C�  C���C�  C�  C�C�C���C���D }qD  D��D�D� D�D�D  D}qD  D� D�qD� DD��D  D��D�qD	� D
�D
}qD  D� D�qDz�D�qD��D  D}qD�D��D  D� DD�DD��D�qDz�D  D� D��D}qD�D� D�qD��D  D� D  D� D�qD}qD�qDz�D�D��D�qD� D�qD� D�D�D D ��D!�D!�D"�D"��D"�qD#z�D#�RD$}qD%�D%� D&D&��D'D'��D(  D(}qD(��D)� D*�D*� D*�qD+}qD+��D,� D-D-�D.D.��D/  D/� D/�qD0z�D0�qD1��D2  D2� D3D3�D4�D4��D4�qD5}qD6  D6}qD6�qD7� D8�D8}qD8�qD9� D9�qD:}qD:�qD;� D;�qD<� D=D=��D>  D>� D?�D?� D@  D@��DA  DA� DA�qDB� DC�DC� DC�qDD}qDD�qDE� DF  DF}qDG�DG��DH  DH� DI  DI� DJ  DJ� DK  DK}qDK�qDL}qDM  DM��DN  DN}qDN�qDO}qDP�DP��DQ  DQ}qDQ�qDR� DS�DS��DS�qDT}qDT�qDU}qDV  DV}qDW  DW��DX  DX� DY�DY��DZ  DZ� D[  D[� D\  D\� D]  D]��D]�qD^}qD_  D_� D`  D`� Da  Da� Db�Db� Db�qDc� Dd  Dd}qDe  De�Df  Df� Dg�Dg� Dh  Dh}qDh��Di}qDj  Dj� Dk  Dk}qDl  Dl� Dm�Dm� Dn  Dn� Do  Do��Dp  Dp� Dq�Dq��Dq�qDr}qDs  Ds� Dt  Dt}qDt�qDu� Dv�Dv� Dw  Dw}qDw�qDx� Dy  Dy� Dy�qDz��D{�D{}qD{�qD|� D}  D}� D}�qD~}qD�D��D�  D�@ D�� D�� D�  D�@ D�~�D���D�HD�AHD��HD��HD�  D�@ D�� D��HD��D�AHD�}qD��qD�  D�>�D�~�D�� D�HD�AHD�~�D��qD�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D��HD�� D�  D�@ D�~�D�� D�  D�>�D��HD��HD�HD�B�D�� D���D�  D�@ D�� D��qD���D�@ D�~�D��qD��qD�>�D��HD��HD�HD�B�D���D�D�HD�@ D�~�D��qD�  D�@ D�~�D��HD�  D�>�D�� D�� D�  D�>�D�}qD���D�  D�>�D�~�D��qD��qD�=qD�� D���D���D�AHD��HD��HD��D�AHD�~�D�� D��D�AHD��HD�� D���D�AHD�� D���D���D�@ D�� D���D���D�@ D�� D���D�  D�AHD�� D���D��qD�@ D���D�� D�  D�AHD�~�D���D���D�>�D�~�D���D���D�@ D��HD��HD��D�@ D�~�D�� D���D�<)D�~�D�� D�HD�@ D�}qD�� D��D�AHD�~�D�� D�HD�@ D�~�D���D�  D�>�D�~�D�� D�  D�>�D��HD�D�HD�@ D���D��HD�  D�AHD���D��HD�  D�B�D��HD��HD�  D�@ D��HD��HD�  D�AHD��HD�� D���D�>�D�~�D��qD�  D�@ D�~�D��HD�  D�>�D��HD��HD���D�>�D��HD��HD�  D�AHD��HD��HD���D�=qD�� D��HD��qD�=qD��HD��HD���D�@ D��HD�� D�  D�AHD��HD�D���D�@ D���D�� D�  D�@ D�� D�� D��qD�>�D�~�D���D�  D�AHD���D��HD�  D�AHD���D�� D���D�AHD�� D���D�HD�AHD��HD�� D�  D�>�D�~�D�� D�HD�@ D�~�D��qD���D�AHD�� D���D�  D�AHD�� D��qD�  D�AHD D¾�D��qD�@ DÁHD�� D���D�@ DāHD�� D���D�@ DŁHD�� D���D�@ DƁHD�� D�  D�AHDǀ DǽqD���D�@ D�~�D��HD��D�AHD�~�D�� D�HD�>�Dʀ D��HD�  D�AHDˁHD˾�D���D�AHD�~�D�� D�HD�>�D�}qD;�D�  D�=qD΀ D��HD�  D�@ Dπ D�� D�HD�AHDЁHD�D�HD�>�D�~�DѾ�D�  D�AHDҁHD��HD�  D�AHDӀ DӾ�D�  D�@ D�~�D�� D��D�AHDՁHD��HD�HD�AHDր D��HD�HD�@ DׁHD�D�  D�=qD�~�D�� D�  D�@ DفHD�D�HD�@ Dڀ D�� D�HD�AHD�~�D۾�D�  D�AHD܁HD��HD�HD�@ D݀ D��HD�HD�AHDހ D�� D�  D�>�D�}qD߾�D�  D�@ D�� DྸD���D�AHD�HDᾸD��qD�@ D�HD�� D�  D�>�D�~�D㾸D���D�@ D�HD�� D���D�@ D�HD��HD�HD�@ D� D澸D���D�@ D�HD��HD���D�>�D�HD��HD��D�B�D�HD�� D�  D�AHD� D�� D���D�>�D�}qD�qD�  D�AHD� D�� D���D�>�D�~�D��HD��D�@ D�~�DD���D�@ D� D��HD�HD�AHD�� D�� D�  D�>�D� D��HD�  D�@ D�~�D�qD���D�>�D�~�D�� D�  D�@ D� D�� D�HD�>�D�� D�� D���D�>�D�� D��HD�HD�@ D�~�D��qD���D�@ D�~�D���D���D�@ D��HD��HD���?�?L��?�  ?�33?�
=?��H@�@(��@=p�@Q�@c�
@�  @�=q@��@�p�@���@�z�@�  @Ǯ@�33@޸R@�=q@�z�A   A�A
�HA  A�A=qA!G�A'
=A,(�A0��A6ffA<��AB�\AG�AL��AS33AY��A_\)Ac�
Ah��Ao\)Au�Az�HA�  A��\A�A���A��A�{A���A��
A��RA���A�(�A��RA���A�z�A�\)A��A�z�A�
=A�=qA��A��A�=qA�p�A�Q�A\A��A�Q�A˅A�{AУ�A�33A�{A���A�33A�A�Q�A�33A�A�  A��HA�p�A��A�33A�p�A�Q�A�33A�{B Q�B��B
=B��B=qB�B��B
�\B(�BB33Bz�B{B�
BG�B�RB(�B��B\)B��B�\B   B!p�B#
=B$��B&=qB'�B)�B*�RB,Q�B-B/33B0z�B2{B3�
B5G�B6�RB8  B9��B;33B<��B>=qB?�B@��BB�\BD(�BE��BF�HBHQ�BI�BK�BL��BN=qBO�BQG�BR�HBTQ�BUBW
=BXz�BZ{B[�B\��B^{B_�Ba�Bb�RBd(�BeG�Bf�\Bh(�Bi��Bk33Blz�BmBo
=Bpz�Bq�Bs�Bt��Bv{Bw\)Bx��Bz�\B|  B}G�B~�\B�  B��RB�p�B�(�B��RB�G�B�  B���B�\)B�{B���B�G�B��
B��\B�33B��
B�ffB���B���B�Q�B���B���B�(�B���B�p�B�{B���B�p�B�  B��\B��B�B�ffB���B���B�{B��\B���B�\)B��
B�=qB���B�
=B�33B�p�B���B��
B�  B�=qB�z�B��RB���B��B�G�B�p�B���B�B��B�(�B�ffB��\B��HB���B��B�G�B��B�B�  B�=qB�ffB��\B���B�
=B�\)B���B�B��B�(�B�z�B���B�
=B�G�B��B��B�  B�=qB���B��HB��B�G�B���B��
B�=qB�z�B��HB��B�\)B���B��
B�(�B��\B���B�G�B��B��
B�(�B�z�B���B�33B���B��B�=qB��\B��HB�33B��B��
B�(�B��\B���B�G�B���B��B�=qB��\B��HB�33B���B�  B�Q�B���B�
=B�\)B��B��B�Q�B���B���B�\)B�B�(�B�z�B��HB�33B��B��B�=qB���B�
=B�p�B��
B�=qB���B��B��B��
B�(�B��\B���B�\)B�B�=qB���B��B�p�B��B�Q�B��RB�
=B�\)B�B�(�B���B�
=B�p�B��B�Q�B¸RB��BÙ�B��B�ffB���B�33Bř�B�  B�ffB���B�33BǙ�B�  B�ffB���B�G�BɮB�(�Bʏ\B���B�p�B��B�Q�B̸RB�33B͙�B�  B�Q�BθRB��Bϙ�B�  B�ffB��HB�G�BѮB�{B�z�B���B�\)B�B�(�Bԏ\B��HB�G�Bՙ�B�  B�Q�BָRB��BׅB��
B�Q�Bأ�B��B�p�B��
B�=qBڣ�B�
=B�p�B�B�(�B܏\B��HB�33Bݙ�B��B�=qBޏ\B���B�G�B߮B�  B�ffB�RB�
=B�p�B��B�=qB��B���B�\)B�B�{B�\B��HB�\)B�B�{B�z�B���B��B癚B��B�ffB���B�33B�B��B�Q�B��B��B�B��B�Q�B�RB��B�B��B�Q�B�RB��B�B��B�Q�B�RB��B�B�  B�ffB���B�33B�B�{B�z�B��HB�G�B�B�(�B��\B�
=B�p�B��
B�=qB��RB��B���B�{B�z�B���B�p�B��
B�Q�B��RB�G�B��B�(�B��RB�33B��C �C \)C �\C ��C
=CG�C�\CC  C=qC�CC  C33Cz�C�C��C(�Cp�C��C�HC�C\)C��C�HC�CffC�C�C(�Cp�C�C��C33Cz�CC	
=C	Q�C	�\C	�
C
�C
\)C
��C
�C33Cp�C�C��C=qC�CC
=CQ�C��C�
C�CffC�C  CG�C�\C�
C�Cp�C�RC��CG�C�\C��C{CffC�C��CG�C��C�
C33C�C��C{CffC�RC  CQ�C��C�HC(�Cp�CC{C\)C�C  CQ�C��C��CG�C�\C�HC(�Cp�CC
=C\)C��C��CQ�C��C�C=qC��C�HC33Cz�C��C {C \)C �C ��C!G�C!��C!�HC"=qC"�C"�
C#�C#ffC#�RC$  C$Q�C$��C$��C%=qC%�\C%�
C&{C&\)C&�C&�C'=qC'�C'��C(�C(p�C(�RC(��C)=qC)�C)��C*{C*ffC*�RC+  C+Q�C+��C+�
C,(�C,p�C,C-{C-ffC-�RC.  C.G�C.�C.�
C/�C/z�C/C0
=C0Q�C0��C0�
C1�C1p�C1C2{C2\)C2��C2�HC3(�C3z�C3�RC4
=C4\)C4��C4�C533C5p�C5�RC6  C6G�C6��C6�C733C7p�C7�RC8  C8=qC8�C8��C9{C9\)C9��C9�C:=qC:�C:C;
=C;Q�C;��C;�HC<�C<p�C<C={C=\)C=��C=�HC>(�C>p�C>C?{C?ffC?�C?��C@=qC@�C@��CA�CAp�CA�RCB
=CBQ�CB��CB�
CC�CCp�CCCD{CDffCD�CD��CE33CEz�CE��CF�CFp�CF�RCF��CG=qCG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                    ?�  @�@B�\@�G�@�G�@�G�@�G�AG�A  A\)A,(�A?\)A`  A�  A�Q�A�Q�A�  A�  A�  A�Q�A�Q�B   B  B�
B�
B   B((�B0  B8(�B@  BH  BP(�BW�
B_�Bg�
Bp(�BxQ�B�  B��B��B�  B�{B�{B�Q�B��B��B�  B��B�  B�  B�  B��B�  B�{B�  B�  B�{B�  B�  B�  B�{B�  B��B�  B�{B�{B�  B��B��B��C  C  C��C  C

=C
=C  C
=C
=C
=C  C��C  C{C{C {C"
=C$
=C&
=C(
=C)��C+��C-��C/��C1��C4  C6
=C8{C:  C;��C=��C?��CA�CD  CF
=CH  CJ  CL  CN  CP  CR  CT  CU��CW��CZ
=C\
=C^  C`
=Cb  Cc��Ce��Cg��Ci��Ck��Cm��Cp  Cr
=Ct  Cv  Cx  Cz  C|  C~  C�  C�C�
=C�C���C���C�  C�C�C���C�  C�
=C�  C�C�C�C�C�  C���C���C�C���C���C���C���C�  C�C�C�  C���C���C�C�C�  C�C�  C�C�  C���C�C�C�C�C�C�C�C�  C�  C���C���C���C�C�
=C�
=C�C�  C���C���C���C���C���C���C�  C�C�  C�  C�  C���C���C���C���C�C�
=C���C���C�C�  C�  C���C���C���C�C�C���C���C���C���C�  C�  C�  C�  C�C�C�C�C�C�C���C���C���C�  C���C��C���C���C�  C�C�
=C�C���C�  C�C�C�
=C�C���C�  C�C�C�C�C�  C���C�  C�  C�C�C���C���D }qD  D��D�D� D�D�D  D}qD  D� D�qD� DD��D  D��D�qD	� D
�D
}qD  D� D�qDz�D�qD��D  D}qD�D��D  D� DD�DD��D�qDz�D  D� D��D}qD�D� D�qD��D  D� D  D� D�qD}qD�qDz�D�D��D�qD� D�qD� D�D�D D ��D!�D!�D"�D"��D"�qD#z�D#�RD$}qD%�D%� D&D&��D'D'��D(  D(}qD(��D)� D*�D*� D*�qD+}qD+��D,� D-D-�D.D.��D/  D/� D/�qD0z�D0�qD1��D2  D2� D3D3�D4�D4��D4�qD5}qD6  D6}qD6�qD7� D8�D8}qD8�qD9� D9�qD:}qD:�qD;� D;�qD<� D=D=��D>  D>� D?�D?� D@  D@��DA  DA� DA�qDB� DC�DC� DC�qDD}qDD�qDE� DF  DF}qDG�DG��DH  DH� DI  DI� DJ  DJ� DK  DK}qDK�qDL}qDM  DM��DN  DN}qDN�qDO}qDP�DP��DQ  DQ}qDQ�qDR� DS�DS��DS�qDT}qDT�qDU}qDV  DV}qDW  DW��DX  DX� DY�DY��DZ  DZ� D[  D[� D\  D\� D]  D]��D]�qD^}qD_  D_� D`  D`� Da  Da� Db�Db� Db�qDc� Dd  Dd}qDe  De�Df  Df� Dg�Dg� Dh  Dh}qDh��Di}qDj  Dj� Dk  Dk}qDl  Dl� Dm�Dm� Dn  Dn� Do  Do��Dp  Dp� Dq�Dq��Dq�qDr}qDs  Ds� Dt  Dt}qDt�qDu� Dv�Dv� Dw  Dw}qDw�qDx� Dy  Dy� Dy�qDz��D{�D{}qD{�qD|� D}  D}� D}�qD~}qD�D��D�  D�@ D�� D�� D�  D�@ D�~�D���D�HD�AHD��HD��HD�  D�@ D�� D��HD��D�AHD�}qD��qD�  D�>�D�~�D�� D�HD�AHD�~�D��qD�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D��HD�� D�  D�@ D�~�D�� D�  D�>�D��HD��HD�HD�B�D�� D���D�  D�@ D�� D��qD���D�@ D�~�D��qD��qD�>�D��HD��HD�HD�B�D���D�D�HD�@ D�~�D��qD�  D�@ D�~�D��HD�  D�>�D�� D�� D�  D�>�D�}qD���D�  D�>�D�~�D��qD��qD�=qD�� D���D���D�AHD��HD��HD��D�AHD�~�D�� D��D�AHD��HD�� D���D�AHD�� D���D���D�@ D�� D���D���D�@ D�� D���D�  D�AHD�� D���D��qD�@ D���D�� D�  D�AHD�~�D���D���D�>�D�~�D���D���D�@ D��HD��HD��D�@ D�~�D�� D���D�<)D�~�D�� D�HD�@ D�}qD�� D��D�AHD�~�D�� D�HD�@ D�~�D���D�  D�>�D�~�D�� D�  D�>�D��HD�D�HD�@ D���D��HD�  D�AHD���D��HD�  D�B�D��HD��HD�  D�@ D��HD��HD�  D�AHD��HD�� D���D�>�D�~�D��qD�  D�@ D�~�D��HD�  D�>�D��HD��HD���D�>�D��HD��HD�  D�AHD��HD��HD���D�=qD�� D��HD��qD�=qD��HD��HD���D�@ D��HD�� D�  D�AHD��HD�D���D�@ D���D�� D�  D�@ D�� D�� D��qD�>�D�~�D���D�  D�AHD���D��HD�  D�AHD���D�� D���D�AHD�� D���D�HD�AHD��HD�� D�  D�>�D�~�D�� D�HD�@ D�~�D��qD���D�AHD�� D���D�  D�AHD�� D��qD�  D�AHD D¾�D��qD�@ DÁHD�� D���D�@ DāHD�� D���D�@ DŁHD�� D���D�@ DƁHD�� D�  D�AHDǀ DǽqD���D�@ D�~�D��HD��D�AHD�~�D�� D�HD�>�Dʀ D��HD�  D�AHDˁHD˾�D���D�AHD�~�D�� D�HD�>�D�}qD;�D�  D�=qD΀ D��HD�  D�@ Dπ D�� D�HD�AHDЁHD�D�HD�>�D�~�DѾ�D�  D�AHDҁHD��HD�  D�AHDӀ DӾ�D�  D�@ D�~�D�� D��D�AHDՁHD��HD�HD�AHDր D��HD�HD�@ DׁHD�D�  D�=qD�~�D�� D�  D�@ DفHD�D�HD�@ Dڀ D�� D�HD�AHD�~�D۾�D�  D�AHD܁HD��HD�HD�@ D݀ D��HD�HD�AHDހ D�� D�  D�>�D�}qD߾�D�  D�@ D�� DྸD���D�AHD�HDᾸD��qD�@ D�HD�� D�  D�>�D�~�D㾸D���D�@ D�HD�� D���D�@ D�HD��HD�HD�@ D� D澸D���D�@ D�HD��HD���D�>�D�HD��HD��D�B�D�HD�� D�  D�AHD� D�� D���D�>�D�}qD�qD�  D�AHD� D�� D���D�>�D�~�D��HD��D�@ D�~�DD���D�@ D� D��HD�HD�AHD�� D�� D�  D�>�D� D��HD�  D�@ D�~�D�qD���D�>�D�~�D�� D�  D�@ D� D�� D�HD�>�D�� D�� D���D�>�D�� D��HD�HD�@ D�~�D��qD���D�@ D�~�D���D���D�@ D��HD��HD���?�?L��?�  ?�33?�
=?��H@�@(��@=p�@Q�@c�
@�  @�=q@��@�p�@���@�z�@�  @Ǯ@�33@޸R@�=q@�z�A   A�A
�HA  A�A=qA!G�A'
=A,(�A0��A6ffA<��AB�\AG�AL��AS33AY��A_\)Ac�
Ah��Ao\)Au�Az�HA�  A��\A�A���A��A�{A���A��
A��RA���A�(�A��RA���A�z�A�\)A��A�z�A�
=A�=qA��A��A�=qA�p�A�Q�A\A��A�Q�A˅A�{AУ�A�33A�{A���A�33A�A�Q�A�33A�A�  A��HA�p�A��A�33A�p�A�Q�A�33A�{B Q�B��B
=B��B=qB�B��B
�\B(�BB33Bz�B{B�
BG�B�RB(�B��B\)B��B�\B   B!p�B#
=B$��B&=qB'�B)�B*�RB,Q�B-B/33B0z�B2{B3�
B5G�B6�RB8  B9��B;33B<��B>=qB?�B@��BB�\BD(�BE��BF�HBHQ�BI�BK�BL��BN=qBO�BQG�BR�HBTQ�BUBW
=BXz�BZ{B[�B\��B^{B_�Ba�Bb�RBd(�BeG�Bf�\Bh(�Bi��Bk33Blz�BmBo
=Bpz�Bq�Bs�Bt��Bv{Bw\)Bx��Bz�\B|  B}G�B~�\B�  B��RB�p�B�(�B��RB�G�B�  B���B�\)B�{B���B�G�B��
B��\B�33B��
B�ffB���B���B�Q�B���B���B�(�B���B�p�B�{B���B�p�B�  B��\B��B�B�ffB���B���B�{B��\B���B�\)B��
B�=qB���B�
=B�33B�p�B���B��
B�  B�=qB�z�B��RB���B��B�G�B�p�B���B�B��B�(�B�ffB��\B��HB���B��B�G�B��B�B�  B�=qB�ffB��\B���B�
=B�\)B���B�B��B�(�B�z�B���B�
=B�G�B��B��B�  B�=qB���B��HB��B�G�B���B��
B�=qB�z�B��HB��B�\)B���B��
B�(�B��\B���B�G�B��B��
B�(�B�z�B���B�33B���B��B�=qB��\B��HB�33B��B��
B�(�B��\B���B�G�B���B��B�=qB��\B��HB�33B���B�  B�Q�B���B�
=B�\)B��B��B�Q�B���B���B�\)B�B�(�B�z�B��HB�33B��B��B�=qB���B�
=B�p�B��
B�=qB���B��B��B��
B�(�B��\B���B�\)B�B�=qB���B��B�p�B��B�Q�B��RB�
=B�\)B�B�(�B���B�
=B�p�B��B�Q�B¸RB��BÙ�B��B�ffB���B�33Bř�B�  B�ffB���B�33BǙ�B�  B�ffB���B�G�BɮB�(�Bʏ\B���B�p�B��B�Q�B̸RB�33B͙�B�  B�Q�BθRB��Bϙ�B�  B�ffB��HB�G�BѮB�{B�z�B���B�\)B�B�(�Bԏ\B��HB�G�Bՙ�B�  B�Q�BָRB��BׅB��
B�Q�Bأ�B��B�p�B��
B�=qBڣ�B�
=B�p�B�B�(�B܏\B��HB�33Bݙ�B��B�=qBޏ\B���B�G�B߮B�  B�ffB�RB�
=B�p�B��B�=qB��B���B�\)B�B�{B�\B��HB�\)B�B�{B�z�B���B��B癚B��B�ffB���B�33B�B��B�Q�B��B��B�B��B�Q�B�RB��B�B��B�Q�B�RB��B�B��B�Q�B�RB��B�B�  B�ffB���B�33B�B�{B�z�B��HB�G�B�B�(�B��\B�
=B�p�B��
B�=qB��RB��B���B�{B�z�B���B�p�B��
B�Q�B��RB�G�B��B�(�B��RB�33B��C �C \)C �\C ��C
=CG�C�\CC  C=qC�CC  C33Cz�C�C��C(�Cp�C��C�HC�C\)C��C�HC�CffC�C�C(�Cp�C�C��C33Cz�CC	
=C	Q�C	�\C	�
C
�C
\)C
��C
�C33Cp�C�C��C=qC�CC
=CQ�C��C�
C�CffC�C  CG�C�\C�
C�Cp�C�RC��CG�C�\C��C{CffC�C��CG�C��C�
C33C�C��C{CffC�RC  CQ�C��C�HC(�Cp�CC{C\)C�C  CQ�C��C��CG�C�\C�HC(�Cp�CC
=C\)C��C��CQ�C��C�C=qC��C�HC33Cz�C��C {C \)C �C ��C!G�C!��C!�HC"=qC"�C"�
C#�C#ffC#�RC$  C$Q�C$��C$��C%=qC%�\C%�
C&{C&\)C&�C&�C'=qC'�C'��C(�C(p�C(�RC(��C)=qC)�C)��C*{C*ffC*�RC+  C+Q�C+��C+�
C,(�C,p�C,C-{C-ffC-�RC.  C.G�C.�C.�
C/�C/z�C/C0
=C0Q�C0��C0�
C1�C1p�C1C2{C2\)C2��C2�HC3(�C3z�C3�RC4
=C4\)C4��C4�C533C5p�C5�RC6  C6G�C6��C6�C733C7p�C7�RC8  C8=qC8�C8��C9{C9\)C9��C9�C:=qC:�C:C;
=C;Q�C;��C;�HC<�C<p�C<C={C=\)C=��C=�HC>(�C>p�C>C?{C?ffC?�C?��C@=qC@�C@��CA�CAp�CA�RCB
=CBQ�CB��CB�
CC�CCp�CCCD{CDffCD�CD��CE33CEz�CE��CF�CFp�CF�RCF��CG=qCG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��#A��HA��;A��;A��;A��;A��/A��;A��HA��;A��;A��HA��;A��;A��;A��;A��HA��HA��;A��;A��HA��;A��;A��;A��;A��HA��HA��TA��TA��/A��A��#A���A���A���A�ȴA̴9A̩�A̗�A�VA�(�A��TA��-A���A���A��A�O�A�JA�E�A��DA���A��
A�G�A�O�A�-A�C�A��uA�5?A��!A��A�JA�p�A�%A���A�~�A�\)A�1'A�M�A��A���A���A}�Au�Ao��Ak�Aep�Ab^5A\ȴAY��AW�AV��AV1AU�7AU%AT��AS�-AP�AL��AJ�RAH�AFQ�AC&�A?��A>~�A>{A>��A>��A?
=A>�A>ZA=VA<�DA;VA9�;A9|�A9?}A8�A8ffA7;dA6��A6�A5�#A5�#A5��A5%A4bA3��A3&�A2=qA133A0�DA0v�A0{A/��A/33A.ZA-l�A-A,�yA, �A+;dA*�A)�FA(��A(9XA'�TA'p�A';dA'&�A'G�A'��A'�;A'�TA'�hA'`BA'/A&��A&ffA%�wA#��A#��A$ �A#�
A#�A"E�A!��A ��A jA  �A�mA�TA (�AƨA�A~�A��A��A1A��A��A{A�A�A�!A�\AbA�-AdZA�A�HA�\AI�AVAQ�A��A-AK�A�jAn�A1A�AoA�AffAJAt�A"�A�HAZA(�A�-AXA�`A��AM�A�Al�AVA�A��A�A�\AI�A��A�^AO�A��A�A�DAI�A�A�wA�7AdZA33A
��A
ȴA
A�A
A	�mA	�A	+A�\A��A�wA�PA;dA��Az�A{A��A33AA��A��A�+AM�A�A��AXA��A��AVA$�A�AhsA�A v�@��@�33@��y@�@�%@��j@���@�Z@��@���@��
@��w@���@�J@��-@�j@�|�@��@���@�5?@���@�Ĝ@���@�@�K�@��H@�\@��@�A�@�!@�=q@���@웦@��m@�P@��@��@��@�1'@���@��m@���@�F@�S�@�R@�@�V@�r�@�@�ȴ@�-@���@��@�9X@��
@�dZ@��@��H@ޏ\@�-@ݩ�@�V@ܴ9@�A�@�  @ۍP@�$�@�G�@ج@�9X@׮@�t�@�+@�~�@�@�`B@�?}@�7L@�&�@�Q�@�o@�M�@���@У�@� �@��@��m@�|�@ΰ!@�^5@�$�@́@���@�  @˥�@�
=@ʰ!@��T@�G�@ț�@�ƨ@�33@��H@Ə\@�J@�hs@��/@�1'@���@Å@��y@�=q@���@�X@���@�b@�K�@��!@�E�@�@���@��7@�O�@�&�@� �@�l�@��@�v�@�E�@�$�@��@�`B@��@��D@��m@��P@�;d@���@��!@�V@�-@���@��@��@�9X@�|�@�"�@��!@��@��h@�hs@�%@�I�@�1@�  @��@��P@�
=@�v�@���@�X@�/@��`@�b@���@�dZ@�33@�@���@���@�ff@���@��T@���@��@��@���@���@�j@�A�@�(�@��@�  @���@��@���@�@��@�&�@�Z@��@���@�\)@��H@���@�^5@���@��h@�p�@�?}@�&�@��@�z�@�A�@��@��;@��w@�t�@�"�@��y@�v�@�=q@���@��-@�G�@��@��/@�z�@�1@��F@��@�\)@�
=@���@�{@���@�O�@��@���@��@���@���@�t�@�C�@��H@�ȴ@��!@�V@���@�X@��@��D@�r�@�b@�ƨ@���@�l�@�"�@��@��@���@���@��!@�=q@�O�@��/@��9@��D@�j@�9X@��m@�ƨ@�K�@�33@�"�@���@��+@�M�@�=q@��@��@���@�`B@��@���@�z�@�I�@�(�@��
@�t�@�K�@�"�@���@��@��+@�@���@��-@���@�x�@�hs@�G�@�7L@�&�@��@���@� �@��w@���@�"�@�M�@��@��^@�x�@�/@��@�V@�Ĝ@��u@�z�@�j@� �@��
@��@��@�C�@�+@�
=@���@��+@��@��@�`B@�&�@��j@���@��D@�Q�@��@�1@�  @��@+@~�R@~ff@~E�@~5?@}�@}`B@|��@|��@|Z@{dZ@{33@{@z�!@z-@y��@x��@xQ�@w�@w\)@w�@v��@vV@u�@u�@tz�@t9X@t9X@t9X@t�@t�@s��@s"�@r��@r^5@r�@q�#@qX@p�`@pr�@o�;@o��@o|�@n��@nE�@mp�@l��@lI�@kt�@ko@jn�@jM�@jJ@i�#@h�`@hĜ@h��@hr�@g��@f��@f�+@e�T@eV@d�/@dz�@d9X@c��@c��@cC�@co@b�H@b^5@a��@a��@a��@a�7@a�7@ax�@aG�@`�`@`r�@`  @_��@_�P@_|�@_\)@^��@]�T@]p�@]`B@]�@\�@\�@\��@\j@\I�@\(�@\�@[t�@Z=q@Y�^@Y��@Y�^@Y��@Yx�@Y&�@XĜ@Xr�@X �@W�@W��@W+@V�+@V5?@V@V@V@U�@U��@U`B@U/@T�@T�D@Tz�@T9X@St�@R�H@R�\@Q�#@Qhs@QG�@Q&�@PĜ@PbN@Pb@O��@O�@Ol�@O�@N�R@Nff@N@N@N{@N$�@N{@M�h@MV@Lj@LZ@LI�@L9X@K�F@Ko@J�@J��@J��@JM�@I�@I�#@I��@I��@Ihs@I�@H�u@Hb@G�@G��@G�@Gl�@G
=@FV@F{@E@E/@D�@Dz�@DZ@C�m@C��@CS�@B�@B�@B�H@B�@B~�@A��@A��@AX@A%@@��@@�@@b@?�;@?�@?K�@?
=@>ȴ@>V@>{@=��@=��@=�h@=p�@=?}@<�/@<j@;��@;�F@;�@;S�@:�@:~�@:J@9��@9�7@8��@8b@7�;@7��@7�w@7|�@7
=@6�y@6ȴ@6v�@5�T@5�-@5`B@5V@4�D@49X@3��@3�F@3dZ@3C�@2�H@2��@2�!@2n�@2=q@2J@1��@1�#@1G�@0Ĝ@0r�@0Q�@0b@/�w@.�y@.��@.E�@-�@-�h@-O�@,�@,�j@,z�@+�
@+@*�!@*n�@)�@)�7@)X@)7L@(��@(��@(�u@(Q�@( �@'�@&��@&��@%�-@%`B@%/@$��@$�@$��@$�j@$Z@#�F@#33@"��@"n�@!�#@!��@!G�@!�@!%@ �`@ ��@ �u@�;@��@\)@K�@�@��@v�@E�@{@�T@�-@`B@�@�@V@�/@z�@9X@�@��@��@dZ@dZ@C�@"�@@�H@��@~�@�@�@J@��@�#@��@hs@G�@�`@�u@�u@�u@bN@ �@  @�w@l�@K�@�@
=@�@�+@ff@V@$�@�T@�-@p�@�@�/@��@��@j@Z@I�@�
@��@S�@�@��@�!@�\@^5@=q@�@��@�^@�7@X@%@��@�9@r�@Q�@A�@1'@ �@b@b@  @�;@�w@�@|�@;d@V@$�@�@��@�-@�@O�@O�@/@/@V@�@�j@z�@Z@(�@1@��@�
@��@�@t�@C�@"�@"�@A���A���A���A���A���A��A��HA��TA��HA��;A��;A��HA��;A��/A��;A��TA��HA��;A��#A��;A��HA��;A��#A��;A��HA��HA��/A��/A��HA��TA��;A��/A��/A��HA��TA��HA��;A��;A��TA��HA��;A��/A��;A��TA��TA��/A��#A��/A��;A��HA��;A��#A��/A��HA��HA��/A��#A��;A��TA��TA��HA��;A��/A��HA��HA��;A��/A��HA��TA��HA��;A��HA��TA��`A��HA��HA��TA��TA��;A��/A��/A��HA��HA��/A��/A��/A��TA��HA��;A��;A��TA��`A��TA��HA��;A��;A��HA��;A��#A��/A��HA��TA��HA��#A��/A��HA��HA��;A��/A��;A��HA��HA��HA��;A��/A��HA��HA��HA��;A��/A��;A��TA��TA��HA��/A��HA��TA��TA��HA��;A��HA��mA��mA��TA��HA��HA��TA��`A��`A��HA��#A��;A��
A��
A���A���A��A��;A��;A��#A��A��#A��;A��;A��
A���A���A���A���A���A���A�ȴA���A���A���A���A���A���A�ȴA���A���A���A�ƨA�ƨA�ȴA�ȴA���A�ĜA̼jA̺^A̰!A̲-A̮Ḁ�A̬A̮A̰!A̬Ḁ�A̡�ȂhA̙�Ḁ�A̩�Ȧ+ÁA�v�A�ZA�A��
A�ffA�E�A�&�A�XAǛ�A�oA�n�A�;dAĉ7A� �A¾wA��
A��A��#A��wA��A�A�A��hA�A�`BA�{A��`A�;dA�+A��A��A�
=A���A��`A��/A���A��RA��-A��!A���A���A��\A��\A��7A��7A��+A��A�|�A�~�A�|�A�z�A�t�A�p�A�n�A�hsA�ZA�M�A�5?A�VA�
=A��A��jA�jA�9XA��A���A���A��^A���A�|�A�O�A�E�A�1'A��A��A��!A� �A���A�`BA�A�A�$�A�
=A��A���A���A�ƨA���A��+A�1'A��FA��#A�v�A�
=A��TA��uA�\)A�C�A�7LA�(�A��A��A�{A�
=A���A��yA��`A��;A���A��-A���A��uA��A�\)A�1'A�bA�%A�  A���A���A�  A�%A�%A���A��/A��RA���A�t�A�E�A�JA��A��FA�~�A�XA�/A��A��!A�XA�&�A�oA��mA��RA��A�=qA�1A��;A�ƨA��FA��!A���A��A�\)A��A��A��9A���A�jA�/A���A�ĜA���A�z�A�I�A�1A��A��#A��jA�|�A�
=A��A��A���A��DA�v�A�\)A�=qA�(�A�1A���A��yA��#A�ĜA��!A��PA�ffA�M�A�;dA��A���A��HA���A�ȴA��9A���A���A��hA��+A��A�r�A�ffA�dZA�ZA�O�A�G�A�A�A�9XA�/A�$�A��A���A��jA��A�S�A�bA��FA�r�A�dZA�`BA�VA� �A���A��TA���A��A�v�A�M�A�"�A��A���A���A��A�p�A�`BA�K�A�C�A�-A��A�
=A�A��A���A��RA�~�A�I�A�9XA�$�A��TA���A�`BA��A���A���A�n�A�;dA�A� �A�E�A�dZA���A�=qA�~�A��mA�|�A�ZA�ZA�A�A�VA�ȴA���A��A�S�A��A��`A��FA��uA�\)A�VA���A�M�A��A�  A��mA���A���A�E�A��A���A���A�x�A�oA���A�;dA���A���A��\A�ffA�9XA��A�JA���A��;A��^A���A�?}A���A��;A�{A�t�A��A���A�A�A�
=A���A�r�A�"�A�ĜA��hA��+A�p�A�33A���A�r�A��A���A���A�r�A�ZA�;dA�A�jA��A��RA�n�A�+A�#AG�A~��A~jA~�A}��A}�7A|�A|�jAz�\Ay+Av�jAv-Au|�AtbNAsAr��AqhsAp�ApjAo�TAo��Aol�Ao7LAnn�An$�An  Am��Al9XAi�wAh�DAg`BAf�Afr�Ae�Ae��Ae?}Ad�HAdn�Ac�Ac�Ac/Ab��Ab�9Ab$�AaVA_��A^ĜA]��A]VA\~�A\�A[p�AZ��AZ-AY�mAYAY�hAYhsAY/AX�uAW�TAW��AW|�AWS�AV��AV�AVȴAV�9AV��AV�uAV�AVffAVQ�AV=qAV �AV{AU��AU�#AU��AUƨAU�AU��AU�AUl�AUp�AUdZAUG�AUVAT��AT��AT�HAT��AT��ATĜAT�!AT��AT��AT�+ATbNAT9XAT�AS�AS��ASdZAR��AR��AR��ARA�AQ�AP(�AN�\AM�TAM/AL�HAL�`AL��ALZAL1AK��AK\)AK�AJ��AJ�!AJjAJA�AI�#AI��AI�AH��AHM�AG��AGhsAGhsAG
=AFĜAFr�AF-AE�#AE7LAD�HADz�AD1AC7LAB^5AAƨAAO�A@��A?��A?��A??}A?33A?"�A?VA>�A>��A>�A>n�A>bNA>VA>JA=�A=�A=�A=��A>5?A>I�A>VA>ZA>�A>��A>��A>�RA>�!A>��A>�yA>�A>�A>��A>��A>��A>��A>��A?A?%A?VA?oA?oA?oA?VA?%A>��A?A>��A>��A>ȴA>�RA>�RA>��A>z�A>ffA>�A=�FA=l�A=O�A=&�A=
=A<�A<�/A<ȴA<��A<��A<�uA<~�A<z�A<v�A<r�A<E�A;�7A:�HA:��A:��A:n�A:Q�A: �A:bA9�A9��A9�-A9��A9��A9�hA9�A9x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                    A���A��#A��HA��;A��;A��;A��;A��/A��;A��HA��;A��;A��HA��;A��;A��;A��;A��HA��HA��;A��;A��HA��;A��;A��;A��;A��HA��HA��TA��TA��/A��A��#A���A���A���A�ȴA̴9A̩�A̗�A�VA�(�A��TA��-A���A���A��A�O�A�JA�E�A��DA���A��
A�G�A�O�A�-A�C�A��uA�5?A��!A��A�JA�p�A�%A���A�~�A�\)A�1'A�M�A��A���A���A}�Au�Ao��Ak�Aep�Ab^5A\ȴAY��AW�AV��AV1AU�7AU%AT��AS�-AP�AL��AJ�RAH�AFQ�AC&�A?��A>~�A>{A>��A>��A?
=A>�A>ZA=VA<�DA;VA9�;A9|�A9?}A8�A8ffA7;dA6��A6�A5�#A5�#A5��A5%A4bA3��A3&�A2=qA133A0�DA0v�A0{A/��A/33A.ZA-l�A-A,�yA, �A+;dA*�A)�FA(��A(9XA'�TA'p�A';dA'&�A'G�A'��A'�;A'�TA'�hA'`BA'/A&��A&ffA%�wA#��A#��A$ �A#�
A#�A"E�A!��A ��A jA  �A�mA�TA (�AƨA�A~�A��A��A1A��A��A{A�A�A�!A�\AbA�-AdZA�A�HA�\AI�AVAQ�A��A-AK�A�jAn�A1A�AoA�AffAJAt�A"�A�HAZA(�A�-AXA�`A��AM�A�Al�AVA�A��A�A�\AI�A��A�^AO�A��A�A�DAI�A�A�wA�7AdZA33A
��A
ȴA
A�A
A	�mA	�A	+A�\A��A�wA�PA;dA��Az�A{A��A33AA��A��A�+AM�A�A��AXA��A��AVA$�A�AhsA�A v�@��@�33@��y@�@�%@��j@���@�Z@��@���@��
@��w@���@�J@��-@�j@�|�@��@���@�5?@���@�Ĝ@���@�@�K�@��H@�\@��@�A�@�!@�=q@���@웦@��m@�P@��@��@��@�1'@���@��m@���@�F@�S�@�R@�@�V@�r�@�@�ȴ@�-@���@��@�9X@��
@�dZ@��@��H@ޏ\@�-@ݩ�@�V@ܴ9@�A�@�  @ۍP@�$�@�G�@ج@�9X@׮@�t�@�+@�~�@�@�`B@�?}@�7L@�&�@�Q�@�o@�M�@���@У�@� �@��@��m@�|�@ΰ!@�^5@�$�@́@���@�  @˥�@�
=@ʰ!@��T@�G�@ț�@�ƨ@�33@��H@Ə\@�J@�hs@��/@�1'@���@Å@��y@�=q@���@�X@���@�b@�K�@��!@�E�@�@���@��7@�O�@�&�@� �@�l�@��@�v�@�E�@�$�@��@�`B@��@��D@��m@��P@�;d@���@��!@�V@�-@���@��@��@�9X@�|�@�"�@��!@��@��h@�hs@�%@�I�@�1@�  @��@��P@�
=@�v�@���@�X@�/@��`@�b@���@�dZ@�33@�@���@���@�ff@���@��T@���@��@��@���@���@�j@�A�@�(�@��@�  @���@��@���@�@��@�&�@�Z@��@���@�\)@��H@���@�^5@���@��h@�p�@�?}@�&�@��@�z�@�A�@��@��;@��w@�t�@�"�@��y@�v�@�=q@���@��-@�G�@��@��/@�z�@�1@��F@��@�\)@�
=@���@�{@���@�O�@��@���@��@���@���@�t�@�C�@��H@�ȴ@��!@�V@���@�X@��@��D@�r�@�b@�ƨ@���@�l�@�"�@��@��@���@���@��!@�=q@�O�@��/@��9@��D@�j@�9X@��m@�ƨ@�K�@�33@�"�@���@��+@�M�@�=q@��@��@���@�`B@��@���@�z�@�I�@�(�@��
@�t�@�K�@�"�@���@��@��+@�@���@��-@���@�x�@�hs@�G�@�7L@�&�@��@���@� �@��w@���@�"�@�M�@��@��^@�x�@�/@��@�V@�Ĝ@��u@�z�@�j@� �@��
@��@��@�C�@�+@�
=@���@��+@��@��@�`B@�&�@��j@���@��D@�Q�@��@�1@�  @��@+@~�R@~ff@~E�@~5?@}�@}`B@|��@|��@|Z@{dZ@{33@{@z�!@z-@y��@x��@xQ�@w�@w\)@w�@v��@vV@u�@u�@tz�@t9X@t9X@t9X@t�@t�@s��@s"�@r��@r^5@r�@q�#@qX@p�`@pr�@o�;@o��@o|�@n��@nE�@mp�@l��@lI�@kt�@ko@jn�@jM�@jJ@i�#@h�`@hĜ@h��@hr�@g��@f��@f�+@e�T@eV@d�/@dz�@d9X@c��@c��@cC�@co@b�H@b^5@a��@a��@a��@a�7@a�7@ax�@aG�@`�`@`r�@`  @_��@_�P@_|�@_\)@^��@]�T@]p�@]`B@]�@\�@\�@\��@\j@\I�@\(�@\�@[t�@Z=q@Y�^@Y��@Y�^@Y��@Yx�@Y&�@XĜ@Xr�@X �@W�@W��@W+@V�+@V5?@V@V@V@U�@U��@U`B@U/@T�@T�D@Tz�@T9X@St�@R�H@R�\@Q�#@Qhs@QG�@Q&�@PĜ@PbN@Pb@O��@O�@Ol�@O�@N�R@Nff@N@N@N{@N$�@N{@M�h@MV@Lj@LZ@LI�@L9X@K�F@Ko@J�@J��@J��@JM�@I�@I�#@I��@I��@Ihs@I�@H�u@Hb@G�@G��@G�@Gl�@G
=@FV@F{@E@E/@D�@Dz�@DZ@C�m@C��@CS�@B�@B�@B�H@B�@B~�@A��@A��@AX@A%@@��@@�@@b@?�;@?�@?K�@?
=@>ȴ@>V@>{@=��@=��@=�h@=p�@=?}@<�/@<j@;��@;�F@;�@;S�@:�@:~�@:J@9��@9�7@8��@8b@7�;@7��@7�w@7|�@7
=@6�y@6ȴ@6v�@5�T@5�-@5`B@5V@4�D@49X@3��@3�F@3dZ@3C�@2�H@2��@2�!@2n�@2=q@2J@1��@1�#@1G�@0Ĝ@0r�@0Q�@0b@/�w@.�y@.��@.E�@-�@-�h@-O�@,�@,�j@,z�@+�
@+@*�!@*n�@)�@)�7@)X@)7L@(��@(��@(�u@(Q�@( �@'�@&��@&��@%�-@%`B@%/@$��@$�@$��@$�j@$Z@#�F@#33@"��@"n�@!�#@!��@!G�@!�@!%@ �`@ ��@ �u@�;@��@\)@K�@�@��@v�@E�@{@�T@�-@`B@�@�@V@�/@z�@9X@�@��@��@dZ@dZ@C�@"�@@�H@��@~�@�@�@J@��@�#@��@hs@G�@�`@�u@�u@�u@bN@ �@  @�w@l�@K�@�@
=@�@�+@ff@V@$�@�T@�-@p�@�@�/@��@��@j@Z@I�@�
@��@S�@�@��@�!@�\@^5@=q@�@��@�^@�7@X@%@��@�9@r�@Q�@A�@1'@ �@b@b@  @�;@�w@�@|�@;d@V@$�@�@��@�-@�@O�@O�@/@/@V@�@�j@z�@Z@(�@1@��@�
@��@�@t�@C�@"�@"�@A���A���A���A���A���A��A��HA��TA��HA��;A��;A��HA��;A��/A��;A��TA��HA��;A��#A��;A��HA��;A��#A��;A��HA��HA��/A��/A��HA��TA��;A��/A��/A��HA��TA��HA��;A��;A��TA��HA��;A��/A��;A��TA��TA��/A��#A��/A��;A��HA��;A��#A��/A��HA��HA��/A��#A��;A��TA��TA��HA��;A��/A��HA��HA��;A��/A��HA��TA��HA��;A��HA��TA��`A��HA��HA��TA��TA��;A��/A��/A��HA��HA��/A��/A��/A��TA��HA��;A��;A��TA��`A��TA��HA��;A��;A��HA��;A��#A��/A��HA��TA��HA��#A��/A��HA��HA��;A��/A��;A��HA��HA��HA��;A��/A��HA��HA��HA��;A��/A��;A��TA��TA��HA��/A��HA��TA��TA��HA��;A��HA��mA��mA��TA��HA��HA��TA��`A��`A��HA��#A��;A��
A��
A���A���A��A��;A��;A��#A��A��#A��;A��;A��
A���A���A���A���A���A���A�ȴA���A���A���A���A���A���A�ȴA���A���A���A�ƨA�ƨA�ȴA�ȴA���A�ĜA̼jA̺^A̰!A̲-A̮Ḁ�A̬A̮A̰!A̬Ḁ�A̡�ȂhA̙�Ḁ�A̩�Ȧ+ÁA�v�A�ZA�A��
A�ffA�E�A�&�A�XAǛ�A�oA�n�A�;dAĉ7A� �A¾wA��
A��A��#A��wA��A�A�A��hA�A�`BA�{A��`A�;dA�+A��A��A�
=A���A��`A��/A���A��RA��-A��!A���A���A��\A��\A��7A��7A��+A��A�|�A�~�A�|�A�z�A�t�A�p�A�n�A�hsA�ZA�M�A�5?A�VA�
=A��A��jA�jA�9XA��A���A���A��^A���A�|�A�O�A�E�A�1'A��A��A��!A� �A���A�`BA�A�A�$�A�
=A��A���A���A�ƨA���A��+A�1'A��FA��#A�v�A�
=A��TA��uA�\)A�C�A�7LA�(�A��A��A�{A�
=A���A��yA��`A��;A���A��-A���A��uA��A�\)A�1'A�bA�%A�  A���A���A�  A�%A�%A���A��/A��RA���A�t�A�E�A�JA��A��FA�~�A�XA�/A��A��!A�XA�&�A�oA��mA��RA��A�=qA�1A��;A�ƨA��FA��!A���A��A�\)A��A��A��9A���A�jA�/A���A�ĜA���A�z�A�I�A�1A��A��#A��jA�|�A�
=A��A��A���A��DA�v�A�\)A�=qA�(�A�1A���A��yA��#A�ĜA��!A��PA�ffA�M�A�;dA��A���A��HA���A�ȴA��9A���A���A��hA��+A��A�r�A�ffA�dZA�ZA�O�A�G�A�A�A�9XA�/A�$�A��A���A��jA��A�S�A�bA��FA�r�A�dZA�`BA�VA� �A���A��TA���A��A�v�A�M�A�"�A��A���A���A��A�p�A�`BA�K�A�C�A�-A��A�
=A�A��A���A��RA�~�A�I�A�9XA�$�A��TA���A�`BA��A���A���A�n�A�;dA�A� �A�E�A�dZA���A�=qA�~�A��mA�|�A�ZA�ZA�A�A�VA�ȴA���A��A�S�A��A��`A��FA��uA�\)A�VA���A�M�A��A�  A��mA���A���A�E�A��A���A���A�x�A�oA���A�;dA���A���A��\A�ffA�9XA��A�JA���A��;A��^A���A�?}A���A��;A�{A�t�A��A���A�A�A�
=A���A�r�A�"�A�ĜA��hA��+A�p�A�33A���A�r�A��A���A���A�r�A�ZA�;dA�A�jA��A��RA�n�A�+A�#AG�A~��A~jA~�A}��A}�7A|�A|�jAz�\Ay+Av�jAv-Au|�AtbNAsAr��AqhsAp�ApjAo�TAo��Aol�Ao7LAnn�An$�An  Am��Al9XAi�wAh�DAg`BAf�Afr�Ae�Ae��Ae?}Ad�HAdn�Ac�Ac�Ac/Ab��Ab�9Ab$�AaVA_��A^ĜA]��A]VA\~�A\�A[p�AZ��AZ-AY�mAYAY�hAYhsAY/AX�uAW�TAW��AW|�AWS�AV��AV�AVȴAV�9AV��AV�uAV�AVffAVQ�AV=qAV �AV{AU��AU�#AU��AUƨAU�AU��AU�AUl�AUp�AUdZAUG�AUVAT��AT��AT�HAT��AT��ATĜAT�!AT��AT��AT�+ATbNAT9XAT�AS�AS��ASdZAR��AR��AR��ARA�AQ�AP(�AN�\AM�TAM/AL�HAL�`AL��ALZAL1AK��AK\)AK�AJ��AJ�!AJjAJA�AI�#AI��AI�AH��AHM�AG��AGhsAGhsAG
=AFĜAFr�AF-AE�#AE7LAD�HADz�AD1AC7LAB^5AAƨAAO�A@��A?��A?��A??}A?33A?"�A?VA>�A>��A>�A>n�A>bNA>VA>JA=�A=�A=�A=��A>5?A>I�A>VA>ZA>�A>��A>��A>�RA>�!A>��A>�yA>�A>�A>��A>��A>��A>��A>��A?A?%A?VA?oA?oA?oA?VA?%A>��A?A>��A>��A>ȴA>�RA>�RA>��A>z�A>ffA>�A=�FA=l�A=O�A=&�A=
=A<�A<�/A<ȴA<��A<��A<�uA<~�A<z�A<v�A<r�A<E�A;�7A:�HA:��A:��A:n�A:Q�A: �A:bA9�A9��A9�-A9��A9��A9�hA9�A9x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
lWB
k�B
k�B
lWB
lWB
`vB
l"B
l�B
lWB
lWB
k�B
lWB
l"B
l"B
l"B
l"B
l"B
l"B
l"B
l"B
k�B
l"B
k�B
k�B
k�B
k�B
k�B
k�B
kQB
kQB
l"B
l"B
k�B
l�B
l�B
lWB
k�B
lWB
iyB
e�B
r|B
��B
��B�B"�B#BGzBf�Bp�B�iB��B�,B�>B��BÖB�zB��B�_B�B�CB�	Bv�B^B-CB
��B
�B
��B
�SB
zDB
H�B
)�B
B	�B	ĜB	�hB	�uB	gB	[�B	ZQB	j�B	��B	��B	�FB	��B	�aB	ںB	�B	�;B	��B	��B	��B	��B	�dB	�gB	ܒB
1B
	B
%�B
0�B
=B
GEB
MB
LdB
U�B
[�B
[WB
`BB
i�B
iyB
ncB
w�B
|PB
zxB
zB
y�B
{B
u�B
xlB
y>B
y	B
z�B
{B
}�B
�B
��B
�uB
��B
|�B
�7B
��B
�$B
�oB
��B
�MB
��B
�_B
��B
�.B
��B
��B
�VB
��B
��B
��B
��B
��B
�UB
��B
�$B
��B
��B
�B
�_B
��B
�'B
�FB
�PB
��B
{B
y	B
x�B
y	B
��B
�MB
�B
{JB
r�B
s�B
}�B
{�B
v�B
s�B
r|B
pB
m�B
pB
l�B
lWB
j�B
jB
i�B
h�B
f�B
hsB
h�B
o5B
oiB
l�B
f�B
g�B
dZB
a�B
`�B
]/B
\�B
[#B
^B
Z�B
[�B
YB
XEB
XyB
V�B
WsB
VB
V9B
T�B
T�B
S�B
TaB
T,B
Z�B
\]B
[�B
ZQB
YB
XEB
W�B
U�B
VB
UgB
T�B
S�B
T�B
TaB
T�B
S&B
R�B
T,B
Q�B
Q�B
R�B
R�B
R�B
O�B
NpB
N<B
M�B
LdB
L�B
JXB
J#B
GEB
F?B
EmB
D�B
D3B
DgB
C�B
C-B
A�B
A�B
@B
?B
=�B
>BB
<�B
<�B
<�B
;0B
:�B
9XB
:*B
8�B
7LB
7B
6�B
6zB
5�B
5tB
4�B
6B
33B
2-B
33B
/�B
/�B
.}B
-�B
-�B
-CB
+�B
*�B
*0B
)�B
(�B
&�B
+6B
%FB
%B
$tB
$�B
"�B
"4B
$�B
 �B
"4B
!bB
!�B
 �B
 �B
 'B
 'B
�B
 �B
�B
�B
�B
IB
~B
�B
IB
IB
~B
�B
B
�B
xB
�B
�B
CB
�B
B
�B
B
qB
eB
�B
�B
�B
�B
�B
+B
�B
�B
�B
�B
$B
�B
�B
SB
�B
�B
�B
FB
�B
MB
�B
{B
{B
FB
�B
�B
�B
B
�B
SB
FB
�B
�B
�B
MB
MB
MB
�B
�B
�B
�B
B
�B
B
�B
�B
SB
$B
�B
�B
�B
�B
�B
�B
YB
�B
+B
YB
_B
+B
+B
�B
+B
�B
_B
eB
eB
�B
kB
�B
�B
�B
�B
CB
B
�B
�B
B
	B
	B
�B
kB
�B
eB
B
�B
xB
xB
�B
�B
kB
7B
qB
�B
IB
OB
OB
�B
OB
�B
B
�B
OB
�B
OB
OB
�B
!B
�B
 'B
 'B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
�B
VB
VB
�B
�B
�B
 �B
 \B
 �B
"4B
!�B
"hB
#�B
'�B
)_B
(�B
)_B
)_B
)*B
(�B
(�B
($B
(�B
)*B
)*B
)�B
)�B
*�B
)�B
*�B
*�B
+�B
+kB
+kB
+6B
+�B
,qB
,�B
-B
-B
-CB
-B
-wB
.B
.B
-�B
.IB
.}B
.IB
.B
.�B
.�B
/B
.�B
.IB
.IB
.�B
.IB
.IB
.}B
.�B
.�B
.�B
.}B
.}B
.IB
.�B
0�B
0�B
0�B
1'B
1'B
1�B
1�B
2-B
3�B
3hB
33B
4nB
4�B
4�B
4�B
4�B
5B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
7�B
7�B
7�B
7�B
8B
7�B
8�B
9�B
9XB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:*B
:�B
:�B
:�B
<6B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
>�B
>wB
>�B
>wB
?HB
?�B
?}B
?}B
?�B
?�B
?�B
@B
@OB
A B
A�B
A�B
A�B
B�B
B�B
B�B
C-B
C�B
CaB
CaB
C�B
D3B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
FB
G�B
GEB
GzB
G�B
HKB
HKB
I�B
I�B
I�B
I�B
J�B
J�B
K)B
J�B
K�B
LdB
K�B
K�B
K�B
K�B
K�B
K�B
L�B
LdB
MB
MB
MB
M�B
NB
NB
NpB
N<B
NB
N�B
OB
OvB
OvB
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
S&B
RTB
R B
S&B
S[B
S�B
S�B
T�B
T,B
S�B
T�B
T,B
T�B
T�B
T�B
T�B
U2B
U�B
W
B
W?B
W?B
W?B
W?B
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
W�B
WsB
WsB
W�B
XEB
W�B
W�B
XB
X�B
XyB
XyB
X�B
X�B
X�B
XyB
YKB
Y�B
Y�B
YB
YB
Y�B
ZQB
Z�B
[�B
[#B
Z�B
Z�B
[WB
[�B
\)B
\�B
\�B
\]B
\]B
\�B
]dB
^B
^�B
_B
_B
^�B
_B
`BB
`B
`B
`vB
aHB
aB
aB
a�B
a�B
a�B
a�B
a�B
a�B
bB
bB
b�B
b�B
b�B
b�B
c B
c B
c�B
cTB
c�B
c�B
c�B
c�B
dZB
dZB
c�B
c�B
d&B
dZB
d�B
dZB
c�B
d&B
d&B
d&B
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
f2B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
gmB
h�B
h�B
h�B
h�B
iB
iDB
iB
i�B
i�B
i�B
jB
jKB
jKB
k�B
kQB
k�B
l"B
k�B
k�B
k�B
l"B
l�B
l�B
l�B
l�B
l�B
m�B
ncB
m�B
n/B
ncB
o�B
oiB
o5B
o5B
o5B
o�B
poB
pB
pB
p;B
qB
p�B
p�B
qB
qvB
qB
qAB
qvB
qvB
q�B
q�B
q�B
q�B
rGB
rGB
r�B
rGB
rB
sB
sMB
sB
sMB
s�B
s�B
tB
tTB
t�B
u%B
u%B
u%B
u�B
uZB
u%B
uZB
v`B
v+B
v`B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
x8B
y	B
x�B
zDB
zB
zDB
zxB
zDB
zxB
zB
zxB
{B
{B
{B
{�B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
}"B
~(B
~(B
~]B
~]B
~�B
.B
.B
cB
cB
�B
� B
�iB
�iB
�iB
�4B
�iB
��B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
�AB
�uB
�uB
�uB
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
�YB
��B
�_B
�+B
�+B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�	B
�=B
�rB
�rB
��B
��B
�DB
�DB
�xB
�B
�B
�JB
��B
��B
��B
�B
�B
��B
��B
�B
�PB
�PB
�PB
�PB
��B
�(B
��B
�\B
�(B
�\B
��B
��B
��B
��B
��B
�.B
�.B
�bB
��B
��B
� B
� B
�4B
�4B
��B
��B
��B
�B
�oB
�:B
�oB
l"B
n/B
kQB
j�B
lWB
jKB
k�B
j�B
k�B
l�B
l�B
k�B
lWB
l�B
l�B
kQB
k�B
l�B
m�B
k�B
kB
l"B
m]B
l"B
k�B
k�B
l�B
m)B
k�B
k�B
lWB
m)B
l�B
k�B
j�B
k�B
m]B
l�B
k�B
k�B
l"B
l�B
m)B
l"B
k�B
k�B
l�B
l�B
k�B
kB
k�B
m)B
l�B
k�B
k�B
l�B
m]B
lWB
kB
kB
k�B
l�B
m)B
k�B
kQB
l�B
l�B
k�B
j�B
l�B
l�B
l"B
kQB
j�B
l�B
l�B
k�B
kQB
lWB
m)B
k�B
k�B
kB
l�B
lWB
lWB
jKB
j�B
l"B
l�B
k�B
jB
l"B
l�B
l�B
lWB
j�B
k�B
lWB
k�B
kQB
jB
kB
l�B
l�B
k�B
j�B
lWB
l�B
l"B
kQB
kB
j�B
l"B
l�B
k�B
j�B
j�B
l�B
l�B
l"B
j�B
kQB
l�B
l�B
k�B
j�B
j�B
kB
l"B
k�B
jKB
j�B
kB
l"B
l"B
k�B
jKB
j�B
k�B
m]B
k�B
k�B
l�B
m)B
m]B
l�B
jB
kB
l�B
l�B
lWB
j�B
j�B
k�B
m]B
m�B
l"B
k�B
l"B
m)B
m]B
l�B
lWB
k�B
k�B
l�B
m]B
m�B
l"B
kB
k�B
m)B
l�B
lWB
k�B
j�B
k�B
m�B
lWB
m�B
kB
k�B
k�B
jB
i�B
hsB
h>B
hsB
gmB
kQB
f�B
d�B
cTB
b�B
b�B
cTB
a|B
��B
~�B
xlB
l"B
s�B
��B
��B
�qB
�@B
��B
��B
�<B
��B
خBB�B�B
�B
��B
�lB �BPB9�B:^B$tB �B#:B!�B%FB#�B"�B"�B!�B(XB!�B"�B%�B%FB$�B"�B!-B!-B �B �B"4B!�B!bB �B!�B�B!B �B �B�B�B!�BkB"�B \B)_B#nB"�B&B,=B'RB)_B.IB1�B.IB/�B4�B6�BD�B_;BXEBR BOvBQNBT�BU�BY�BT,BT�BR�BS�B^�Bc�Bz�BsBs�BncBx8BrGBsBpBp�Bo Bn�Bo Bo�Bo�BqBp�Br�BxBtTBw�Bz�By>BzDB{B�uB�YB�B��B�B��B��B�MB��B�*B�B��B�'B�LB�HB��B�RBĜB� BɆB͟B�aB�?BרB�)B�mB��B�B�B�]B�DB��B�B�|B�B��B�NB��B�pB�]B�KB�yBٴB� B�}B��B��B�B��B�0B�<B�wBǮBȀBΥB��B�*B�tB��B�^B��B��B�FB�?B�nB��B�[B��B��B��B��B�!B��B��B�wB�CB�CB��B��B��B�*B��B��B��B��B�@B�B�B�@B��B�:B�nB��B��B��B�_B��B�B�4B�RB�CB�{B�@B��B��B��B��B��B��B��B�VB�PB��B��B�+B~]B�B|�B|�BzBwfBx�BwfBq�Bq�Br�Bt�Br�Bj�Bc�Be,Bh>Ba�B^jBW�BXyBMjBL�BB�BC-B`BF�B.IB.�B&�B!bBxB�B
�(B
��B
��B
��B
�JB
�vB
�B
�B
�B
�DB
�B
��B
��B
��B
�TB
�KB
��B
�0B
��B
�B
�<B
�?B
�}B
�dB
��B
� B
�$B
��B
��B
�$B
��B
��B
��B
��B
�B
�bB
��B
�JB
��B
��B
�"B
�B
��B
�B
pB
g8B
n/B
W?B
W?B
R�B
S&B
M�B
M6B
>B
8�B
:�B
@�B
=�B
5�B
33B
*�B
!�B
�B
_B
B
�B
�B
B
.B
�B	��B
AB	��B	��B	� B	�B	�mB	�B	� B	֡B
%B	��B	��B	�nB	ÖB	��B	�qB	��B	�IB	��B	��B	�MB	��B	�lB	�xB	�JB	|�B	{B	.B	�1B	��B	��B	p;B	m)B	l�B	k�B	ffB	e`B	d�B	c�B	b�B	^jB	YKB	W
B	XB	dZB	]dB	Z�B	gB	T,B	TaB	W?B	T,B	Z�B	c�B	a�B	a�B	gB	kQB	qAB	y>B	�B	�rB	�=B	�bB	��B	��B	�bB	�:B	��B	��B	�_B	�=B	��B	��B	��B	��B	��B	��B	�*B	�dB	�jB	��B	�OB	��B	��B	��B	ʌB	�BB	�2B	��B	�gB	�?B	��B	�B	�yB	��B	�yB	֡B	��B	�vB	�B	��B	�B	�)B	��B	�B	�
B	�B	�B	�;B
MB	�B	��B	�B	�#B	�#B	�B	רB	�B	�B	�
B	��B	��B	�QB	��B	خB	֡B	��B	ܒB	�BB	רB	�BB	��B	��B	�B	�2B	�
B	�gB	��B	��B	��B	�B	�]B	�B	خB	��B	�#B	��B	ޞB	��B	ӏB	�}B	�[B	��B	ںB	�]B	�B	ٴB	�B	��B	��B	�.B
YB
�B
�B
(B
hB
B
�B
B
�B
�B
%zB
�B
#:B
%zB
$�B
#nB
%�B
&�B
&�B
'�B
(�B
-B
.B
.�B
2�B
4B
6�B
6�B
9�B
<6B
:�B
=B
@�B
A�B
B�B
@�B
C�B
C�B
EB
N�B
M�B
OvB
K�B
K�B
MB
MB
LdB
MjB
OB
L�B
MjB
L�B
K�B
JXB
J#B
PHB
a�B
\]B
RTB
R�B
S�B
T,B
VmB
Y�B
]dB
`BB
[�B
\)B
[#B
[#B
[�B
\�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                    B
f	B
e7B
e�B
f	B
f	B
Z(B
e�B
f=B
f	B
f	B
e�B
f	B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
elB
elB
elB
e�B
elB
elB
eB
eB
e�B
e�B
elB
frB
f=B
f	B
e�B
f	B
c+B
_GB
l.B
�3B
�B�BNB�BA,B`MBj�BzB��B��B��BςB�HB�,B��B�B��B��B��Bp{BW�B&�B
�B
϶B
�EB
�B
s�B
B�B
#�B
�B	�lB	�NB	�B	|'B	`�B	U�B	TB	deB	�UB	��B	��B	�vB	�B	�lB	��B	��B	�uB	ӚB	�rB	ҔB	�B	�B	�DB
�B
�B
aB
*<B
6�B
@�B
F�B
FB
OMB
U>B
U	B
Y�B
c_B
c+B
hB
qLB
vB
t*B
s�B
s�B
t�B
o@B
rB
r�B
r�B
t_B
t�B
w�B
{�B
|\B
|'B
z�B
vkB
��B
�LB
��B
�!B
��B
��B
�RB
�B
�eB
��B
��B
�RB
�B
��B
�5B
�HB
�|B
�|B
�B
�WB
��B
�gB
��B
��B
�B
��B
��B
��B
�B
}bB
u1B
r�B
rSB
r�B
nB
}�B
z�B
t�B
l�B
m�B
w=B
u�B
p{B
m�B
l.B
i�B
gxB
i�B
f�B
f	B
d�B
c�B
c�B
bYB
`�B
b%B
bYB
h�B
iB
f=B
`MB
a�B
^B
[cB
Z�B
V�B
V�B
T�B
W�B
T�B
U�B
S1B
Q�B
R+B
P�B
Q%B
O�B
O�B
N|B
N�B
MAB
NB
M�B
T8B
VB
U�B
TB
R�B
Q�B
Q�B
O�B
O�B
OB
N|B
M�B
N|B
NB
NGB
L�B
L;B
M�B
K�B
K�B
LoB
L�B
LoB
I�B
H"B
G�B
GQB
FB
FJB
D
B
C�B
@�B
?�B
?B
>NB
=�B
>B
=|B
<�B
;pB
;pB
9�B
8�B
7WB
7�B
6QB
6QB
6�B
4�B
4yB
3
B
3�B
2�B
0�B
0�B
0�B
0,B
/�B
/&B
.�B
/�B
,�B
+�B
,�B
)jB
)5B
(/B
'�B
'�B
&�B
%�B
$KB
#�B
#EB
"�B
 �B
$�B
�B
�B
&B
�B
NB
�B
[B
�B
�B
B
HB
wB
wB
�B
�B
pB
�B
jB
�B
6B
�B
0B
6B
�B
�B
0B
dB
�B
^B
*B
�B
XB
�B
�B
�B
RB
�B
#B
B
EB
?B
9B
�B
nB
�B
�B
�B
EB
�B
�B
�B
zB
B
3B
nB
aB
�B
�B
�B
3B
-B
-B
�B
gB
�B
�B
�B
�B
B
�B
9B
�B
�B
�B
�B
�B
�B
�B
9B
gB
�B
gB
�B
�B
�B
B
�B
?B
?B
tB
tB
tB
�B
B
nB
�B
B
B
�B
�B
tB
�B
EB
B
B
B
�B
B
RB
RB
RB
XB
�B
�B
�B
�B
�B
�B
�B
RB
B
�B
B
�B
XB
*B
*B
^B
�B
B
�B
#B
^B
�B
B
B
�B
B
dB
�B
�B
B
6B
B
B
jB
�B
pB
�B
�B
�B
�B
�B
pB
BB
wB
wB
BB
�B
B
B
<B
pB
�B
BB
B
wB
�B
}B
B
�B
!9B
#B
"�B
#B
#B
"�B
"sB
"sB
!�B
"?B
"�B
"�B
#EB
#yB
$KB
#�B
$KB
$�B
%QB
%B
%B
$�B
%�B
&#B
&�B
&�B
&�B
&�B
&�B
')B
'�B
'�B
'�B
'�B
(/B
'�B
'�B
(dB
(�B
(�B
(�B
'�B
'�B
(dB
'�B
'�B
(/B
(dB
(�B
(dB
(/B
(/B
'�B
(�B
*pB
*<B
*pB
*�B
*�B
+vB
+�B
+�B
-NB
-B
,�B
. B
.TB
.TB
.TB
.�B
.�B
/ZB
/�B
0�B
0`B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
2mB
3>B
3
B
3>B
3sB
3sB
3sB
3�B
3sB
3sB
3�B
3�B
4�B
4�B
4yB
5�B
6�B
6�B
7#B
7WB
7�B
7�B
7�B
8]B
8)B
8]B
8)B
8�B
9cB
9/B
9/B
9�B
9cB
9�B
9�B
:B
:�B
;pB
;;B
;�B
<vB
<AB
<vB
<�B
=HB
=B
=B
=|B
=�B
>NB
>NB
>�B
>NB
>�B
?TB
?TB
?TB
?�B
A`B
@�B
A,B
A`B
A�B
A�B
ClB
C8B
C�B
C�B
D>B
DsB
D�B
D�B
EyB
FB
E�B
E�B
E�B
EyB
EDB
EyB
FB
FB
F�B
F�B
F�B
GQB
G�B
G�B
H"B
G�B
G�B
H�B
H�B
I(B
I(B
J�B
K5B
K5B
KiB
KiB
K�B
K�B
L�B
LB
K�B
L�B
MB
MAB
MAB
N|B
M�B
M�B
NGB
M�B
NGB
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
QZB
Q%B
Q%B
Q�B
Q�B
Q�B
QZB
Q�B
R`B
R+B
R+B
R`B
R`B
R`B
R+B
R�B
SfB
S�B
S1B
S1B
SfB
TB
T�B
U>B
T�B
T�B
TlB
U	B
UrB
U�B
VDB
VDB
VB
VB
VxB
WB
W�B
XPB
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Z(B
Z�B
Z�B
Z�B
[cB
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\iB
\�B
\�B
\�B
\�B
\�B
]�B
]B
]oB
]oB
]oB
]oB
^B
^B
]�B
]oB
]�B
^B
^AB
^B
]�B
]�B
]�B
]�B
^uB
^uB
^uB
^uB
^AB
^�B
_GB
_GB
_{B
_GB
_�B
`MB
`MB
`MB
aB
aSB
a�B
a�B
aSB
a�B
aB
bYB
bYB
b�B
b�B
b�B
b�B
b�B
c_B
c_B
c�B
c�B
c�B
c�B
elB
eB
e�B
e�B
elB
elB
e�B
e�B
f=B
f�B
frB
frB
f�B
gxB
hB
g�B
g�B
hB
iPB
iB
h�B
h�B
h�B
iPB
j!B
i�B
i�B
i�B
j�B
jVB
j�B
j�B
k(B
j�B
j�B
k(B
k(B
k\B
k�B
k�B
k�B
k�B
k�B
lbB
k�B
k�B
l�B
l�B
l�B
l�B
m4B
m4B
m�B
nB
nnB
n�B
n�B
n�B
o@B
oB
n�B
oB
pB
o�B
pB
p{B
p�B
p�B
p�B
qB
qLB
qLB
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
t*B
s�B
t*B
s�B
t*B
t�B
u1B
u1B
u�B
vB
v7B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
xB
xCB
x�B
x�B
yB
yB
yIB
y�B
zB
zB
zB
y�B
zB
z�B
z�B
z�B
z�B
{UB
{UB
{UB
{UB
{�B
{�B
{�B
{�B
{�B
|'B
|'B
|'B
|\B
|�B
|�B
|�B
|�B
}bB
}bB
}bB
}bB
}�B
}�B
}�B
~hB
~�B
~�B
~�B
~�B
:B
nB
nB
�B
�B
�@B
�B
��B
�B
��B
��B
�zB
�zB
�zB
�zB
�LB
��B
��B
�RB
��B
��B
��B
�$B
�$B
�XB
��B
��B
��B
�*B
��B
��B
��B
�eB
�eB
�eB
��B
��B
��B
��B
��B
�B
�B
�B
�B
�kB
��B
��B
�B
��B
�B
�wB
�wB
�wB
��B
��B
��B
��B
�B
�IB
�}B
��B
��B
��B
��B
�OB
�OB
�OB
��B
�!B
��B
�!B
e�B
g�B
eB
deB
f	B
c�B
e7B
d�B
e�B
f�B
f=B
e7B
f	B
f�B
f�B
eB
elB
f=B
gCB
e�B
d�B
e�B
gB
e�B
e7B
e7B
f=B
f�B
e�B
e7B
f	B
f�B
frB
e�B
d�B
e�B
gB
frB
e�B
elB
e�B
frB
f�B
e�B
elB
e7B
frB
frB
e7B
d�B
e�B
f�B
frB
e7B
elB
f�B
gB
f	B
d�B
d�B
e7B
f=B
f�B
e7B
eB
f=B
f�B
e�B
d�B
f=B
f�B
e�B
eB
d�B
frB
f�B
elB
eB
f	B
f�B
e�B
e7B
d�B
frB
f	B
f	B
c�B
d�B
e�B
f=B
e7B
d1B
e�B
f=B
frB
f	B
d�B
elB
f	B
elB
eB
d1B
d�B
f�B
frB
e7B
deB
f	B
frB
e�B
eB
d�B
d�B
e�B
f=B
e7B
d�B
d�B
f=B
f=B
e�B
deB
eB
f=B
frB
elB
d�B
d�B
d�B
e�B
e�B
c�B
deB
d�B
e�B
e�B
e7B
c�B
deB
elB
gB
elB
e�B
f�B
f�B
gB
frB
c�B
d�B
f=B
frB
f	B
deB
deB
e7B
gB
gxB
e�B
elB
e�B
f�B
gB
f�B
f	B
elB
elB
f�B
gB
g�B
e�B
d�B
e�B
f�B
f=B
f	B
e7B
deB
e�B
gxB
f	B
gCB
d�B
e7B
e7B
d1B
c_B
b%B
a�B
b%B
aB
eB
`�B
^AB
]B
\4B
\4B
]B
[.B
��B
xwB
rB
e�B
mhB
�<B
��B
�#B
��B
��B
�^B
��B
߰B
�`B�B^B
�UB
��B
�B
�BBBB3>B4B&B�B�B�B�BUBNBNB�B"
BHB�BaB�B[BNB�B�B�B�B�B}BB�B}B6B�BBBwBjB�BHBBNBB#B BNB�B%�B!B#B'�B+�B'�B)5B.TB0�B>NBX�BQ�BK�BI(BK BN�BO�BS�BM�BN|BLoBM�BXPB]:Bt_Bl�BmhBhBq�Bk�Bl�Bi�BjVBh�Bh~Bh�Bi�BiPBj�BjVBl�Bq�BnBqLBt_Br�Bs�Bt�B|'B�B��B��B��B�6B��B��B��B��B��B�|B��B��B��B�5B�B�NB��B�8B�QB�B��B�ZB��B�B�B�VB��B�B��B�uB�:B�.B�iBީB� BݣB�"B�B��B�+B�fB��B�/BħB��B��B�|B��B��B�)B�`B�2B�WB��B��B�&B��B�B�mB�8B��B��B� B��B�B�|B�`B��B��B��B�NB�BB�)B��B��B��B�QB�yB��B�mB��B�9B��B��B��B��B��B�NB��B� B��B��B�aB�B�aB��B��B�B��B�-B��B�[B��B��B�OB�6B�RB��B�B�B��BnB��BxBz�Bv7BvkBs�BqBrSBqBk\Bk\Bl�Bn�Bl�BdeB]oB^�Ba�B[cBXBQZBR+BGBFJB<AB<�BY�B@�B'�B(dB 3BB*B�B
��B
�_B
�qB
��B
��B
�(B
�B
�VB
�7B
��B
��B
ًB
ۗB
�{B
�B
��B
ʗB
��B
B
��B
��B
��B
�/B
�B
�QB
��B
��B
�sB
��B
��B
��B
�BB
�dB
��B
��B
�B
�wB
��B
�XB
}bB
��B
~�B
��B
{�B
i�B
`�B
g�B
P�B
P�B
L�B
L�B
GQB
F�B
7�B
2�B
4yB
:�B
7WB
/�B
,�B
$KB
�B
�B
B
�B
�B
pB
�B
	�B	�:B	��B	��B	�YB	�{B	�B	�1B	�B	��B	�B	�SB	��B	�rB	˞B	� B	�HB	��B	�#B	�<B	��B	��B	�LB	��B	��B	�B	�*B	��B	vkB	t�B	x�B	��B	�eB	z�B	i�B	f�B	frB	e�B	`B	_B	^�B	]:B	\iB	XB	R�B	P�B	Q�B	^B	WB	TlB	`�B	M�B	NB	P�B	M�B	T8B	]:B	[�B	[�B	`�B	eB	j�B	r�B	{�B	�$B	��B	�B	�gB	��B	�B	��B	��B	�?B	�B	��B	�jB	�pB	�HB	�|B	��B	�gB	��B	�B	�B	�]B	�B	�;B	��B	��B	�>B	��B	��B	ͪB	�B	��B	ҔB	��B	�+B	ԠB	�+B	�SB	؅B	�(B	�4B	߰B	�eB	��B	�B	�:B	�B	�\B	�_B	��B	��B	�=B	�rB	��B	��B	��B	��B	�ZB	��B	�1B	мB	ЈB	ӚB	�B	�~B	�`B	�SB	ҔB	�DB	��B	�ZB	��B	؅B	˞B	�:B	��B	мB	�B	̤B	�~B	ΰB	عB	�B	�_B	�`B	�xB	��B	ԠB	�PB	˞B	�AB	�/B	�B	ΰB	�lB	�B	��B	�fB	��B	�rB	�FB	��B
 B
�B
 @B
�B
B
�B
�B
�B
XB
^B
,B
jB
�B
,B
�B
 B
�B
 gB
 �B
!9B
"sB
&�B
'�B
(�B
,�B
-�B
0�B
0`B
3>B
5�B
4yB
6�B
:�B
;pB
<AB
:jB
=|B
=|B
>�B
HWB
GQB
I(B
EDB
E�B
F�B
F�B
FB
GB
H�B
FB
GB
FB
EDB
D
B
C�B
I�B
[cB
VB
LB
L�B
MAB
M�B
PB
S�B
WB
Y�B
UrB
U�B
T�B
T�B
UrB
V�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223250                            20230426223250AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622325020230426223250  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325020230426223250QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325020230426223250QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               