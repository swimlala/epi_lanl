CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:52Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230426223252  20230426223252  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�YͿ�\@�YͿ�\11  @�Z`��@�Z`��@)�j�ܱF@)�j�ܱF�c}�fQm��c}�fQm�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�  @   @@  @�G�@�G�@�G�@�  A   A\)A\)A,(�A@��Aa�A�Q�A��A��A��A��A�  A��A�Q�A��B�B�
B�
B�
B'�
B0  B7�
B?�BH  BP(�BW�
B`  Bh  Bo�
Bw�
B�  B�{B�  B�{B��B�B��B�=qB�{B�{B�  B��B�  B�{B�{B�  B��B��B�  B��B��B�  B�{B�{B�{B�  B�  B��B��B�  B�  B��C   C  C��C  C  C
  C  C  C  C��C��C
=C
=C  C��C��C   C"
=C$
=C&
=C(  C*  C,  C.
=C0
=C2  C4
=C6  C7��C9�C;��C>  C@  CB  CD  CF
=CH
=CJ
=CL
=CM��CO��CQ��CS��CU��CW��CY��C\
=C^  C`  Cb  Cd  Cf  Ch  Cj
=Cl
=Cn  Cp  Cr  Ct  Cv
=Cx  Cz  C{��C}��C�C�C�C�C�  C�  C�C�  C���C���C�  C�C�C�  C�  C�C�C�  C���C�  C�C�  C���C�C�C���C���C���C���C�  C���C���C�  C���C�  C�C�  C���C�  C�  C���C�  C�
=C�C���C���C���C���C�  C���C���C�  C�  C�  C�  C���C�C�
=C�C�  C�C�
=C�C�  C�C�
=C�  C�  C�C���C�  C�C�  C���C�  C�C�  C���C�  C�
=C�C�C�  C���C���C�  C�
=C�  C���C�  C�C�
=C�C�  C�  C�  C�  C�  C�C�  C���C�  C�
=C�C�  C���C�C�C�  C�
=C�
=C�C�C�  C���C���C���C���C���C���C���C���C���C�  C�  C�C�C�  C���D � D�D�D  Dz�D�qD� D�D� D�qDz�D��D}qD�D� D�qD��D	  D	}qD
  D
� D
�qD}qD  D� D  D� D�D��D�D��DD�D  D}qD  Dz�D�qD}qD�qD� D  D��D�qDxRD��D}qD  D��D�qD� D�D}qD  D��D�qD� D�D}qD  D� D�D��D �D ��D!�D!��D!�qD"}qD#  D#� D$  D$z�D$�qD%}qD&  D&��D'  D'z�D(  D(��D(�qD)� D*�D*� D+�D+��D+�qD,� D-�D-}qD-�RD.}qD/  D/��D0D0��D1  D1}qD2  D2�D3  D3� D4�D4}qD5  D5�D6�D6� D6�qD7� D8�D8� D8�qD9z�D9�qD:��D;  D;� D<�D<�D=�D=��D>  D>}qD>�qD?}qD@  D@}qD@�qDA}qDB  DB� DC  DC}qDC��DD� DE  DE}qDF  DF� DG  DG}qDG�qDH� DI�DI��DJ  DJ� DJ�qDK��DLDL��DM  DM� DN  DN��DO�DO��DP  DP}qDP�qDQ� DR  DR� DS  DS��DT�DT� DU  DU}qDU�qDV}qDV�qDW}qDW��DX}qDY  DY� DZ�DZ� D[  D[� D\  D\��D]�D]��D^  D^� D_  D_� D`  D`}qD`�qDa� Db  Db��Dc�Dc�DdDd��De�De��Df  Dfz�Df�qDg}qDh  Dh��Di�Di� Dj�Dj� Dj��Dk� DlDl��Dm  Dm}qDm�qDn� Do�Do� Dp  Dp}qDq�Dq��Dq�qDr}qDr�qDs� Dt  Dt� Du  Du� Dv  Dv� Dv�qDw}qDw�qDx}qDy  Dy��Dz  Dzz�Dz��D{� D|  D|}qD|�qD}}qD~  D~� D�D� D�qD�AHD��HD�� D�HD�@ D�~�D���D���D�@ D�� D��HD��D�B�D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D��HD�� D���D�>�D�� D�D�HD�>�D�� D���D�  D�B�D���D��HD�  D�AHD��HD��HD���D�>�D��HD�� D�  D�@ D�� D�� D���D�>�D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D��qD���D�AHD��HD�� D���D�=qD�~�D��HD�  D�@ D�~�D���D�  D�>�D�~�D���D���D�AHD�� D�� D�  D�@ D�� D��qD���D�@ D�~�D���D���D�AHD���D�D�  D�=qD�~�D���D���D�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D���D�  D�AHD�� D��HD��D�AHD��HD�� D���D�>�D�}qD�� D��D�B�D���D�� D���D�@ D�~�D��qD���D�AHD�� D���D�HD�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD�~�D���D�  D�AHD��HD���D�  D�AHD�� D���D��qD�>�D��HD���D�  D�AHD�� D���D��qD�>�D��HD�� D���D�=qD�}qD�� D�HD�@ D�~�D��qD�  D�AHD�� D���D�HD�C�D���D�D��D�@ D�}qD��qD���D�>�D�~�D�� D�HD�B�D�� D��qD���D�@ D�� D��HD��D�B�D�� D��qD�  D�AHD�}qD���D���D�@ D�� D�� D���D�AHD���D�� D��qD�@ D���D�� D��qD�<)D�~�D�D��D�AHD�� D�� D���D�<)D�~�D��HD���D�=qD�}qD���D�HD�@ D��HD�D�  D�>�D�� D��HD���D�@ D��HD��HD�HD�>�D�~�D�D��D�>�D�� D�� D�  D�AHDHD�� D�  D�@ DÀ D�� D�  D�@ DāHD�� D���D�>�D�~�D��HD�HD�>�D�~�D�D�HD�AHDǀ D�� D��D�B�DȀ D�� D�  D�>�D�~�D�� D���D�>�D�~�Dʾ�D���D�>�DˁHD��HD��qD�=qD̀ D�� D���D�AHD́HD�� D�  D�AHD΂�D�� D���D�@ DρHD�D���D�AHDЁHDнqD���D�@ DсHD��HD�  D�AHDҁHDҾ�D��qD�@ DӁHD��HD�HD�AHDԀ D�� D�  D�AHDՁHDսqD���D�@ Dր D�� D���D�@ DׁHD�� D���D�>�D�~�DؽqD�  D�AHDفHD�� D�  D�@ DځHD��HD���D�>�D�~�D۾�D�  D�@ D܀ Dܾ�D�  D�AHD݀ D�� D�  D�>�Dހ D�� D�HD�AHD߀ D߾�D���D�@ D���D�D��D�@ D�~�DᾸD���D�>�D� D�� D�  D�AHD� D�� D�HD�AHD� D侸D���D�@ D� D��HD�  D�AHD�~�D�� D�HD�@ D� D�� D���D�@ D肏D��HD�  D�@ D�HD龸D��qD�>�D� D��HD�HD�AHD� D�� D�  D�>�D� D�� D�  D�AHD� D�� D�HD�@ D�~�D�qD���D�@ D�~�DﾸD�  D�B�D�� D�� D�HD�>�D�~�D�� D�HD�@ D�}qD�D�  D�>�D�~�D��HD�HD�>�D�~�D�� D�HD�AHD�~�D���D���D�=qD�~�D�� D���D�@ D���D�D�HD�AHD��HD�� D�  D�AHD���D��HD��RD�AH?�?\)?L��?��?��
?�33?���?��@�@z�@!G�@0��@B�\@Q�@\(�@h��@z�H@��\@�=q@�\)@�33@�Q�@�  @��@��@�\)@�z�@��H@�  @��@�=q@�\)@�z�@��H@�  @��
@�@�\)@�@��HA ��A33AA��A(�A\)A�\AA��A{A"�\A%A(��A.{A2�\A6ffA:�HA>{AC33AI��AMp�AR�\AW
=A[�A`  AeAj=qAn�RAr�\AvffAz=qA~�RA��A�z�A�ffA���A��HA��A�  A�=qA���A��RA���A��HA�A�  A��A��
A�{A���A��HA�p�A�\)A�G�A��A�A�Q�A��\A���A��RA�G�A��
A�ffA�Q�Aʏ\A���A�
=A�=qA�z�A�
=A�G�A�33A�A��A�33A�A�  A�\A��A�A�=qA���A�
=A�G�A��
A�{B z�BB�HB  BG�B�\B�
B	�B
=qB33Bz�BB
=BQ�BG�BffB\)B��B{B\)Bz�B��B�HB  BG�B�RB�
B!�B"=qB#\)B$��B&{B'\)B(z�B)��B*�HB,(�B-��B.�RB0  B1G�B2=qB3�B5�B6ffB7�B8��B:{B;\)B<z�B=�B?33B@z�BA��BB�HBD(�BEp�BF�RBH  BIp�BJ�RBK�
BM�BN=qBO�
BQ�BRffBS�
BT��BV=qBW�BX��BZ=qB[�B\��B]�B_33B`z�BaBc33Bdz�Be�Bg
=BhQ�Bi��Bj�HBlQ�BmBo33Bp��Br=qBs�Bt��Bv=qBw�
ByG�Bz�HB|Q�B}�B\)B�ffB��B��
B��\B�G�B�  B��RB��B�=qB���B��B�ffB��B��
B��\B�G�B��B��RB��B�=qB�
=B�B��\B�G�B�  B��RB�p�B�(�B���B��B�(�B��HB���B�(�B���B�\)B��B�(�B��\B��HB�33B���B��B�=qB�z�B��\B���B��B�G�B���B�B��B�(�B�ffB���B���B�33B��B�B�  B�Q�B��\B��RB��HB�33B�p�B��B�  B�Q�B��\B��HB��B�p�B��B�  B�Q�B��\B���B��B�\)B���B��B�=qB��\B��HB�33B��B��
B�(�B�ffB��RB���B�33B��B��
B�(�B�z�B���B��B�p�B�B�  B�Q�B���B��HB�33B��B��
B�(�B�z�B���B��B�p�B�B�{B�z�B���B�33B��B��
B�(�B��\B��HB�G�B��B��B�=qB��\B��HB�G�B���B��
B�=qB�z�B��HB�33B��B��
B�=qB��\B���B�G�B��B�  B�ffB��RB�
=B�p�B�B�(�B��\B���B�G�B��B�{B�z�B���B�33B���B��B�Q�B���B��B��B��B�=qB£�B�
=B�\)B�B�(�Bď\B���B�\)B��
B�=qBƣ�B��BǅB�  B�Q�B���B�33BɮB�(�Bʏ\B�
=B�p�B��
B�Q�B̸RB��BͮB�{B�z�B���B�\)B�B�(�BЏ\B�
=B�p�B��B�Q�B���B�33Bә�B�{Bԏ\B���B�\)B��
B�Q�BָRB�33Bי�B�{B�z�B���B�\)B��
B�=qBڣ�B��BۅB�  B�z�B��HB�\)B�B�=qB޸RB��Bߙ�B�  B�ffB��HB�\)B�B�=qB��B�
=B�B��B�Q�B���B�G�B�B�{B�\B���B�p�B��
B�=qB�RB�33B陚B�{B�z�B���B�p�B��B�Q�B���B�33B��B�(�B��B��B�B�  B�z�B���B�p�B��
B�Q�B���B�G�B�B�Q�B��RB�33B��B�(�B��\B�
=B��B��B�ffB��HB�\)B��
B�ffB��HB�\)B��
B�ffB��HB�G�B��
B�=qB��RB�33B��C {C Q�C �\C �
C{CffC��C�HC�C\)C��C�
C�CQ�C��C�HC(�CffC�C�C33Cp�C�C��C(�CffC�C�C(�CffC�C��C=qCp�C�C�C	33C	p�C	�C	��C
=qC
�C
��C
=C\)C��C�HC(�CffC��C�C(�CffC�C��C=qC�C��C
=CQ�C�\C��C
=CG�C�C��C{C\)C��C�
C�CQ�C��C�
C�C\)C��C�C(�Cp�C�C�C(�Cz�C�RC  C=qCz�C�C��C=qC�C�RC  C33Cp�C�RC��C=qCz�C�C��C(�C\)C��C�
C{C\)C��C�
C{CQ�C�\C��C  C=qCz�C�C�C=qCp�C�RC��C(�CffC��C�HC (�C ffC �C �C!�C!\)C!��C!�
C"{C"\)C"��C"�HC#{C#Q�C#�\C#�
C$�C$\)C$��C$�
C%{C%G�C%�C%C&
=C&Q�C&�\C&C&��C'33C'p�C'�C'��C(=qC(z�C(�C(�HC)(�C)ffC)�C)�C*33C*p�C*�C*�C+�C+ffC+��C+�C,33C,z�C,�RC,��C-(�C-p�C-�RC-��C.G�C.z�C.�RC.��C/=qC/�C/��C0
=C0Q�C0�\C0��C1{C1\)C1��C1�C2�C2ffC2��C2��C3=qC3�C3C4
=C4G�C4�\C4�HC533C5ffC5��C5�C633C6z�C6��C7{C7\)C7��C7�HC8�C8p�C8C9
=C9G�C9�C9��C:�C:p�C:�RC;  C;=qC;�C;��C<{C<ffC<�C<��C==qC=z�C=C>{C>ffC>�C>��C?33C?z�C?C@
=C@\)C@��C@��CA33CAp�CA�RCB
=CB\)CB�CB�CC(�CCz�CCCD{CDffCD�CD��CE=qCEz�CE��CF�CFp�CF�CF�CG33CG�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                          ?�  @   @@  @�G�@�G�@�G�@�  A   A\)A\)A,(�A@��Aa�A�Q�A��A��A��A��A�  A��A�Q�A��B�B�
B�
B�
B'�
B0  B7�
B?�BH  BP(�BW�
B`  Bh  Bo�
Bw�
B�  B�{B�  B�{B��B�B��B�=qB�{B�{B�  B��B�  B�{B�{B�  B��B��B�  B��B��B�  B�{B�{B�{B�  B�  B��B��B�  B�  B��C   C  C��C  C  C
  C  C  C  C��C��C
=C
=C  C��C��C   C"
=C$
=C&
=C(  C*  C,  C.
=C0
=C2  C4
=C6  C7��C9�C;��C>  C@  CB  CD  CF
=CH
=CJ
=CL
=CM��CO��CQ��CS��CU��CW��CY��C\
=C^  C`  Cb  Cd  Cf  Ch  Cj
=Cl
=Cn  Cp  Cr  Ct  Cv
=Cx  Cz  C{��C}��C�C�C�C�C�  C�  C�C�  C���C���C�  C�C�C�  C�  C�C�C�  C���C�  C�C�  C���C�C�C���C���C���C���C�  C���C���C�  C���C�  C�C�  C���C�  C�  C���C�  C�
=C�C���C���C���C���C�  C���C���C�  C�  C�  C�  C���C�C�
=C�C�  C�C�
=C�C�  C�C�
=C�  C�  C�C���C�  C�C�  C���C�  C�C�  C���C�  C�
=C�C�C�  C���C���C�  C�
=C�  C���C�  C�C�
=C�C�  C�  C�  C�  C�  C�C�  C���C�  C�
=C�C�  C���C�C�C�  C�
=C�
=C�C�C�  C���C���C���C���C���C���C���C���C���C�  C�  C�C�C�  C���D � D�D�D  Dz�D�qD� D�D� D�qDz�D��D}qD�D� D�qD��D	  D	}qD
  D
� D
�qD}qD  D� D  D� D�D��D�D��DD�D  D}qD  Dz�D�qD}qD�qD� D  D��D�qDxRD��D}qD  D��D�qD� D�D}qD  D��D�qD� D�D}qD  D� D�D��D �D ��D!�D!��D!�qD"}qD#  D#� D$  D$z�D$�qD%}qD&  D&��D'  D'z�D(  D(��D(�qD)� D*�D*� D+�D+��D+�qD,� D-�D-}qD-�RD.}qD/  D/��D0D0��D1  D1}qD2  D2�D3  D3� D4�D4}qD5  D5�D6�D6� D6�qD7� D8�D8� D8�qD9z�D9�qD:��D;  D;� D<�D<�D=�D=��D>  D>}qD>�qD?}qD@  D@}qD@�qDA}qDB  DB� DC  DC}qDC��DD� DE  DE}qDF  DF� DG  DG}qDG�qDH� DI�DI��DJ  DJ� DJ�qDK��DLDL��DM  DM� DN  DN��DO�DO��DP  DP}qDP�qDQ� DR  DR� DS  DS��DT�DT� DU  DU}qDU�qDV}qDV�qDW}qDW��DX}qDY  DY� DZ�DZ� D[  D[� D\  D\��D]�D]��D^  D^� D_  D_� D`  D`}qD`�qDa� Db  Db��Dc�Dc�DdDd��De�De��Df  Dfz�Df�qDg}qDh  Dh��Di�Di� Dj�Dj� Dj��Dk� DlDl��Dm  Dm}qDm�qDn� Do�Do� Dp  Dp}qDq�Dq��Dq�qDr}qDr�qDs� Dt  Dt� Du  Du� Dv  Dv� Dv�qDw}qDw�qDx}qDy  Dy��Dz  Dzz�Dz��D{� D|  D|}qD|�qD}}qD~  D~� D�D� D�qD�AHD��HD�� D�HD�@ D�~�D���D���D�@ D�� D��HD��D�B�D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D��HD�� D���D�>�D�� D�D�HD�>�D�� D���D�  D�B�D���D��HD�  D�AHD��HD��HD���D�>�D��HD�� D�  D�@ D�� D�� D���D�>�D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D��qD���D�AHD��HD�� D���D�=qD�~�D��HD�  D�@ D�~�D���D�  D�>�D�~�D���D���D�AHD�� D�� D�  D�@ D�� D��qD���D�@ D�~�D���D���D�AHD���D�D�  D�=qD�~�D���D���D�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D���D�  D�AHD�� D��HD��D�AHD��HD�� D���D�>�D�}qD�� D��D�B�D���D�� D���D�@ D�~�D��qD���D�AHD�� D���D�HD�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD�~�D���D�  D�AHD��HD���D�  D�AHD�� D���D��qD�>�D��HD���D�  D�AHD�� D���D��qD�>�D��HD�� D���D�=qD�}qD�� D�HD�@ D�~�D��qD�  D�AHD�� D���D�HD�C�D���D�D��D�@ D�}qD��qD���D�>�D�~�D�� D�HD�B�D�� D��qD���D�@ D�� D��HD��D�B�D�� D��qD�  D�AHD�}qD���D���D�@ D�� D�� D���D�AHD���D�� D��qD�@ D���D�� D��qD�<)D�~�D�D��D�AHD�� D�� D���D�<)D�~�D��HD���D�=qD�}qD���D�HD�@ D��HD�D�  D�>�D�� D��HD���D�@ D��HD��HD�HD�>�D�~�D�D��D�>�D�� D�� D�  D�AHDHD�� D�  D�@ DÀ D�� D�  D�@ DāHD�� D���D�>�D�~�D��HD�HD�>�D�~�D�D�HD�AHDǀ D�� D��D�B�DȀ D�� D�  D�>�D�~�D�� D���D�>�D�~�Dʾ�D���D�>�DˁHD��HD��qD�=qD̀ D�� D���D�AHD́HD�� D�  D�AHD΂�D�� D���D�@ DρHD�D���D�AHDЁHDнqD���D�@ DсHD��HD�  D�AHDҁHDҾ�D��qD�@ DӁHD��HD�HD�AHDԀ D�� D�  D�AHDՁHDսqD���D�@ Dր D�� D���D�@ DׁHD�� D���D�>�D�~�DؽqD�  D�AHDفHD�� D�  D�@ DځHD��HD���D�>�D�~�D۾�D�  D�@ D܀ Dܾ�D�  D�AHD݀ D�� D�  D�>�Dހ D�� D�HD�AHD߀ D߾�D���D�@ D���D�D��D�@ D�~�DᾸD���D�>�D� D�� D�  D�AHD� D�� D�HD�AHD� D侸D���D�@ D� D��HD�  D�AHD�~�D�� D�HD�@ D� D�� D���D�@ D肏D��HD�  D�@ D�HD龸D��qD�>�D� D��HD�HD�AHD� D�� D�  D�>�D� D�� D�  D�AHD� D�� D�HD�@ D�~�D�qD���D�@ D�~�DﾸD�  D�B�D�� D�� D�HD�>�D�~�D�� D�HD�@ D�}qD�D�  D�>�D�~�D��HD�HD�>�D�~�D�� D�HD�AHD�~�D���D���D�=qD�~�D�� D���D�@ D���D�D�HD�AHD��HD�� D�  D�AHD���D��HD��RD�AH?�?\)?L��?��?��
?�33?���?��@�@z�@!G�@0��@B�\@Q�@\(�@h��@z�H@��\@�=q@�\)@�33@�Q�@�  @��@��@�\)@�z�@��H@�  @��@�=q@�\)@�z�@��H@�  @��
@�@�\)@�@��HA ��A33AA��A(�A\)A�\AA��A{A"�\A%A(��A.{A2�\A6ffA:�HA>{AC33AI��AMp�AR�\AW
=A[�A`  AeAj=qAn�RAr�\AvffAz=qA~�RA��A�z�A�ffA���A��HA��A�  A�=qA���A��RA���A��HA�A�  A��A��
A�{A���A��HA�p�A�\)A�G�A��A�A�Q�A��\A���A��RA�G�A��
A�ffA�Q�Aʏ\A���A�
=A�=qA�z�A�
=A�G�A�33A�A��A�33A�A�  A�\A��A�A�=qA���A�
=A�G�A��
A�{B z�BB�HB  BG�B�\B�
B	�B
=qB33Bz�BB
=BQ�BG�BffB\)B��B{B\)Bz�B��B�HB  BG�B�RB�
B!�B"=qB#\)B$��B&{B'\)B(z�B)��B*�HB,(�B-��B.�RB0  B1G�B2=qB3�B5�B6ffB7�B8��B:{B;\)B<z�B=�B?33B@z�BA��BB�HBD(�BEp�BF�RBH  BIp�BJ�RBK�
BM�BN=qBO�
BQ�BRffBS�
BT��BV=qBW�BX��BZ=qB[�B\��B]�B_33B`z�BaBc33Bdz�Be�Bg
=BhQ�Bi��Bj�HBlQ�BmBo33Bp��Br=qBs�Bt��Bv=qBw�
ByG�Bz�HB|Q�B}�B\)B�ffB��B��
B��\B�G�B�  B��RB��B�=qB���B��B�ffB��B��
B��\B�G�B��B��RB��B�=qB�
=B�B��\B�G�B�  B��RB�p�B�(�B���B��B�(�B��HB���B�(�B���B�\)B��B�(�B��\B��HB�33B���B��B�=qB�z�B��\B���B��B�G�B���B�B��B�(�B�ffB���B���B�33B��B�B�  B�Q�B��\B��RB��HB�33B�p�B��B�  B�Q�B��\B��HB��B�p�B��B�  B�Q�B��\B���B��B�\)B���B��B�=qB��\B��HB�33B��B��
B�(�B�ffB��RB���B�33B��B��
B�(�B�z�B���B��B�p�B�B�  B�Q�B���B��HB�33B��B��
B�(�B�z�B���B��B�p�B�B�{B�z�B���B�33B��B��
B�(�B��\B��HB�G�B��B��B�=qB��\B��HB�G�B���B��
B�=qB�z�B��HB�33B��B��
B�=qB��\B���B�G�B��B�  B�ffB��RB�
=B�p�B�B�(�B��\B���B�G�B��B�{B�z�B���B�33B���B��B�Q�B���B��B��B��B�=qB£�B�
=B�\)B�B�(�Bď\B���B�\)B��
B�=qBƣ�B��BǅB�  B�Q�B���B�33BɮB�(�Bʏ\B�
=B�p�B��
B�Q�B̸RB��BͮB�{B�z�B���B�\)B�B�(�BЏ\B�
=B�p�B��B�Q�B���B�33Bә�B�{Bԏ\B���B�\)B��
B�Q�BָRB�33Bי�B�{B�z�B���B�\)B��
B�=qBڣ�B��BۅB�  B�z�B��HB�\)B�B�=qB޸RB��Bߙ�B�  B�ffB��HB�\)B�B�=qB��B�
=B�B��B�Q�B���B�G�B�B�{B�\B���B�p�B��
B�=qB�RB�33B陚B�{B�z�B���B�p�B��B�Q�B���B�33B��B�(�B��B��B�B�  B�z�B���B�p�B��
B�Q�B���B�G�B�B�Q�B��RB�33B��B�(�B��\B�
=B��B��B�ffB��HB�\)B��
B�ffB��HB�\)B��
B�ffB��HB�G�B��
B�=qB��RB�33B��C {C Q�C �\C �
C{CffC��C�HC�C\)C��C�
C�CQ�C��C�HC(�CffC�C�C33Cp�C�C��C(�CffC�C�C(�CffC�C��C=qCp�C�C�C	33C	p�C	�C	��C
=qC
�C
��C
=C\)C��C�HC(�CffC��C�C(�CffC�C��C=qC�C��C
=CQ�C�\C��C
=CG�C�C��C{C\)C��C�
C�CQ�C��C�
C�C\)C��C�C(�Cp�C�C�C(�Cz�C�RC  C=qCz�C�C��C=qC�C�RC  C33Cp�C�RC��C=qCz�C�C��C(�C\)C��C�
C{C\)C��C�
C{CQ�C�\C��C  C=qCz�C�C�C=qCp�C�RC��C(�CffC��C�HC (�C ffC �C �C!�C!\)C!��C!�
C"{C"\)C"��C"�HC#{C#Q�C#�\C#�
C$�C$\)C$��C$�
C%{C%G�C%�C%C&
=C&Q�C&�\C&C&��C'33C'p�C'�C'��C(=qC(z�C(�C(�HC)(�C)ffC)�C)�C*33C*p�C*�C*�C+�C+ffC+��C+�C,33C,z�C,�RC,��C-(�C-p�C-�RC-��C.G�C.z�C.�RC.��C/=qC/�C/��C0
=C0Q�C0�\C0��C1{C1\)C1��C1�C2�C2ffC2��C2��C3=qC3�C3C4
=C4G�C4�\C4�HC533C5ffC5��C5�C633C6z�C6��C7{C7\)C7��C7�HC8�C8p�C8C9
=C9G�C9�C9��C:�C:p�C:�RC;  C;=qC;�C;��C<{C<ffC<�C<��C==qC=z�C=C>{C>ffC>�C>��C?33C?z�C?C@
=C@\)C@��C@��CA33CAp�CA�RCB
=CB\)CB�CB�CC(�CCz�CCCD{CDffCD�CD��CE=qCEz�CE��CF�CFp�CF�CF�CG33CG�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�ȴA���A���A�ȴA�ȴA�ȴA���A�ȴA�ȴA���A���A���A���A���A��
A��
A��
A���A�ȴA�ȴA�ȴAиRAд9AП�AЁA�dZA��A�VAΡ�A�$�A��A�ƨA�ZA�dZA�^5A�x�A��A�~�A���A���A���A���A��A�\)A���A��A���A���A��A���A���A��A��A��A�Q�A���A��A�-A�ƨA��A��A�VA��A�dZA�I�A�A}VAwS�Aq�PAkt�Ah1Ac/A]�PAZ�9AY��AYdZAU��AN��AJ�yAI7LAH�AD~�AB�/A@�A=�A<�+A<Q�A<�A9G�A9C�A8�`A8�9A933A9��A9%A6��A6Q�A5�A5S�A533A5�A4�RA4��A4�DA4jA4�A3%A21A17LA0=qA/�hA/;dA/A/�TA/�
A0A0bA/�hA/A.z�A.$�A-hsA,�DA+��A+?}A+��A+|�A+�A+�-A+dZA+G�A+%A*�!A)|�A(�A(z�A'�FA'?}A'
=A&jA%K�A#�#A#p�A#&�A"r�A!33A!�wA!p�A (�Al�A�7A"�A/A�DAƨA��A�AȴAM�A��AK�A�DAA�A��A;dA�uAAƨA7LAdZA33A�yA5?A{AAG�A�`A^5A�A�-A��A��At�A\)A/A��A��AA�A�#Ax�AG�A�A�yAĜAĜAz�A��A��AhsAC�A�A��A��A��AbNA{A��A/AĜAbNA�hA&�AVA
�HA
��A
z�A
1'A	x�A�\A�TAO�A��A��A�A1AO�A+A%A�HA�9A�A�+AZA��A�FA�PA33A�HA��Av�AVA�A{AbA��A��A�A �A {@���@�-@��@�7L@��@�1'@���@��P@�ȴ@�hs@��9@��@�(�@���@��m@��;@���@�+@�ȴ@�{@��u@�j@�(�@�@�
=@��@�hs@��@��/@�9@�@�1'@�dZ@�ff@�@��@�Q�@�R@��@�G�@��
@��y@�$�@��`@�D@�@�@��@◍@�J@���@��@��/@߶F@�K�@��@�V@���@�x�@�%@�j@�9X@���@۾w@�S�@���@��@ٙ�@ف@�G�@ؓu@�(�@�ƨ@��@�^5@�E�@�=q@�5?@�$�@ղ-@Դ9@�I�@�1'@�ƨ@�;d@ҸR@�M�@�5?@�{@�p�@��@мj@��;@���@���@�7L@��`@���@���@��@Ɂ@�O�@ȓu@�9X@��;@�"�@Ɵ�@�$�@�7L@ċD@�A�@��;@å�@Ý�@�|�@�C�@�@��@�J@���@��@��@�I�@��@�1@���@��w@�+@��@��!@�5?@��T@�&�@�I�@�  @��;@��
@��@�+@�@���@��\@��@���@�X@�V@���@�Q�@�9X@�  @��@�;d@�n�@��T@�G�@���@��9@��u@���@��P@�;d@��\@�E�@���@���@�A�@�1'@�  @��P@��H@�5?@�O�@��`@�A�@�ƨ@�S�@��@�v�@�E�@���@��@��9@���@�t�@�
=@���@���@�^5@��@�O�@�7L@���@�j@��@��@�;d@�@��R@�@�@��h@�p�@�7L@��@�b@���@�l�@�C�@�+@�o@���@���@�V@��T@�`B@�G�@��@���@�I�@��
@��@��@�K�@��!@�E�@�@�G�@�%@���@��`@���@��j@���@��u@��u@�I�@�1@�;d@��y@�n�@���@���@�x�@�7L@���@�Ĝ@���@�r�@�  @��m@���@��H@�v�@��@���@��h@�p�@�%@���@�(�@�1@��m@��w@��@��P@�;d@�@���@�$�@�@��@��T@���@�&�@���@��@��@��w@���@�l�@�;d@�@���@��+@��@���@��h@��@��@��u@�Z@�A�@�9X@�1'@� �@�b@���@�dZ@��H@�v�@�=q@���@�X@��@���@�z�@�  @�l�@���@��@��@��@�ȴ@��R@��!@���@�^5@���@��@�`B@�`B@�`B@�X@�7L@�&�@��@�Ĝ@��D@�A�@�1'@�@|�@~�R@~ff@}�T@}�-@}��@}�h@}O�@|��@|�@|��@|1@{@z=q@y�7@y�@x��@w�;@w�P@w�@v��@u@t�@t��@t9X@s�F@s�@s"�@r�H@r�!@r�\@r=q@qx�@q�@pĜ@p�@pA�@p  @o��@o�P@oK�@o�@nȴ@n$�@m/@lj@l�@k��@k�F@kdZ@j�@j��@j~�@j�@i�#@ihs@h�`@h�9@h��@hr�@hA�@hb@g�w@g
=@fff@e/@d�@d�j@d�@d�@dz�@d�@c�@cC�@c@b�!@b�@a�#@a�#@a��@a�^@a��@ax�@ahs@a%@`r�@`b@_�@_�w@_�P@_;d@^��@^�@^��@^5?@^@]@]`B@]�@\��@\�@\�/@\�@\z�@\I�@[��@[o@Z��@Z=q@Z�@Y�#@Y��@Y��@Yx�@Y7L@X�u@XA�@W�;@W|�@V�y@V{@U?}@T��@T�/@Tz�@T1@S��@SC�@S@R��@Rn�@R-@Q�#@Q��@Qx�@QX@Q7L@PbN@O+@N��@N�@Nȴ@N�R@N��@NE�@M�T@M`B@M?}@M/@M�@L�/@Lj@L1@L�@L1@K�@J�H@J�!@J�!@J�!@J^5@I��@IX@I7L@H�`@Hr�@H1'@Hb@G�@G�w@G��@G�P@Gl�@G+@F�@E�-@E/@E�@D�j@DI�@C��@C��@Ct�@Co@B�H@B��@Bn�@B-@A��@A�^@A��@A��@Ahs@A%@@�9@@ �@?�@?K�@>V@>@=��@=`B@=?}@=/@=�@<��@<��@<�@<Z@<(�@;��@;��@;o@:�H@:��@:��@:M�@9��@9�7@9hs@9X@9�@8�`@8�@81'@8  @8  @7��@6V@5�@4�@41@3�F@3o@2��@2n�@1�#@1�@0 �@/|�@.�y@.�R@.��@.ff@-�@-�-@-?}@,��@,��@,I�@+��@+�m@+�@+o@+@*�H@*��@*M�@*�@)��@)�^@)��@)��@)x�@)�@(��@'�w@'K�@'+@&�@&E�@&{@%�T@%��@%@%�-@%�h@%�@%O�@$��@#ƨ@#"�@"�@"��@"n�@!��@!��@!hs@!%@ ��@ �9@ ��@ �u@ �@ Q�@ A�@ 1'@ 1'@   @l�@+@�@�R@5?@��@`B@/@��@��@z�@��@ƨ@�F@��@�@t�@@��@~�@�#@�7@�@�`@�`@��@�u@b@��@;d@
=@�@�+@ff@V@V@V@5?@{@@��@��@��@�@/@�j@z�@9X@(�@1@�
@�F@dZ@33@33@o@�!@J@�^@��@x�@hs@X@X@X@G�@G�@7L@&�@%@�9@�u@�@A�@ �@b@�;@K�@;d@�@�@
=@�y@ȴ@�R@ff@5?@@�h@�@`B@/@V@�@�j@z�@j@Z@9X@1@��@��@�m@�m@�m@�
@ƨ@��@S�@33@@
�@
�H@
�H@
�H@
��@
��@
�!@
�!@
�!@
��@
^5@
M�@
M�@
M�@
M�A���A���A���A���A���A���A���A���A���A�ƨA�ĜA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�ȴA���A�ȴA�ĜA�ĜA�ȴA���A���A�ȴA�A�ĜA�ȴA���A���A�ƨA�ƨA�ȴA���A���A���A�ȴA���A�ȴA�ȴA�ƨA�ƨA�ƨA���A���A�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A���A���A���A���A��
A��A���A���A��
A��
A��A��
A���A���A��
A��#A��#A��A���A���A��
A��A��#A��A��
A�A�ĜA�ȴA���A���A���A�ȴA�ƨA���A���A�ȴA���A���A�ƨA�ȴA���A���A�ȴA�ƨA�ƨA���A���A���A�ȴA�ƨA�ƨA�ȴA�ƨAд9Aв-Aа!Aа!Aв-AжFAд9Aа!Aа!Aд9AжFAв-AУ�AЩ�AГuAБhAЙ�AЗ�AЗ�AЉ7AЉ7AЏ\A�z�A�`BA�XA�XA�\)A�hsA�z�A�ffA�ffA�Q�A�?}A�$�A��A��A��yAϛ�A�z�A�`BA�5?A�&�A�oA�JA�A���AΏ\A�hsA�`BA�?}A�1'A�1'A�/A�&�A�{A�1A���A���A��A��A��yA��;A���A���A�ȴA�ƨAͼjA͸RAͮA͉7A�dZA�O�A�33A��A�/A��ÁA�C�A��`AˋDA�hsA�  A���A�z�AȬA��#A��A�z�A¡�A��A�p�A��+A�(�A���A���A���A�|�A�+A�A�ZA��A�E�A���A�
=A�/A��A�dZA��A��hA�oA��\A���A��RA�A�E�A���A��;A�t�A�33A�%A��/A��jA���A�|�A�bNA�K�A�A�A�=qA�9XA�1'A��A�
=A��A��HA��wA���A��\A�|�A�n�A�dZA�O�A�(�A�1A��A���A�ȴA��A���A�`BA�A��A���A��
A��9A�I�A���A���A�t�A�G�A�"�A�%A��/A���A��A�|�A�r�A�jA�hsA�jA�ffA�`BA�"�A�VA��A�ƨA�A�^5A��A�1A���A���A���A��A��A��A��A��`A��#A���A���A��9A��\A�^5A�33A���A���A��wA��A�v�A�?}A��A�bA��mA���A�ZA�-A��A��DA�v�A��A��\A��hA��/A��`A��mA��A���A���A��FA�XA�bA�A��PA�\)A�"�A��`A��A�jA�&�A��A��A��
A�v�A�C�A�%A��A�ƨA��+A�E�A�%A���A���A���A��RA��PA�C�A�-A�  A��jA��A�Q�A�$�A��A�A��
A��FA���A�hsA��#A���A�hsA�=qA�A��
A��+A�5?A�(�A�  A���A�  A���A�^5A� �A��`A��RA�~�A�O�A� �A��A���A���A�ƨA�A��A���A��A��A��-A��-A��hA�r�A�I�A�(�A��A�p�A���A���A�S�A� �A���A��/A��jA�p�A�Q�A�A�A�+A�A��jA�z�A�S�A�1'A�bA��A��wA���A��+A�p�A�ZA�9XA�A��wA�ffA��A���A�~�A���A��^A���A��7A�v�A�dZA�A�A��A��`A���A�x�A���A�VA��TA�|�A�  A��A�I�A�"�A�ƨA��A�I�A��A�(�A��-A��A�I�A��jA��+A�jA�VA��9A�n�A�;dA���A��yA��jA��A�A�A��yA���A�bNA�&�A��mA���A�hsA�{A���A�1'A��hA���A��+A�A�=qA�^A~�`A~ �A}x�A|�A|z�A|1'A|  A{+Az  Ay�7Ax��Aw�Av�yAu�Au�Atn�At1As�Ar�Arz�Aq�wAp�!An�An$�AmhsAl�AlVAk��Aj�AjI�Ai��Ai/Ah�`Ah�9Ah�AhI�Ag��Ag��Ag+AfjAf�Aep�Ad-Ac`BAb=qAaXA`�`A`Q�A_�A_�A^ffA]��A\^5A[��A[��A[�7A[�A[x�A[AZ~�AZ1AY��AY�wAY��AY��AY��AY��AY�hAY��AY�AY�-AY��AY�
AY��AYXAY?}AY
=AX��AX��AX��AW�wAV�jAU�FAS�^AR=qAQoAP-AO�hAOK�AOVAN��AN�\ANJAMl�AL��AK�AK;dAJ�9AJE�AJ1AI��AI��AIx�AIG�AI&�AI33AI33AI&�AI�AIAH�AH��AH�uAHVAHAG��AG7LAF��AFI�AE��AEK�ADz�AC��AC�ACdZACK�AC;dAC+AC"�AC�AC%AB�RAB��ABffAB-AB(�AA��AA��AAl�A@�yA@jA?�A?`BA?
=A>��A>�!A>�+A>(�A=O�A<�9A<�!A<�RA<�jA<�A<�uA<�+A<n�A<Q�A<VA<^5A<E�A<�A< �A<(�A<=qA<jA<��A<�A<��A=%A=oA<�jA<bNA;��A:�+A9�FA9l�A9\)A9S�A9O�A9?}A9/A9&�A9&�A9&�A9"�A9&�A933A9dZA9hsA9hsA9\)A9?}A9%A8�A8��A8��A8�HA8�`A8�\A8E�A8^5A8��A8�`A8�A8�yA8�`A8�A8�A8��A8��A9�A9��A9��A9�;A9�A:  A:A9�mA9�FA9�hA9�7A9�PA9�7A9t�A9?}A8�yA8��A8A�A7��A7�-A7\)A6�A6r�A6jA6�DA6�uA6�9A6�RA6�\A6(�A5��A5��A5�^A5��A5�PA5�A5x�A5t�A5x�A5`BA5S�A5S�A5O�A5O�A5O�A5S�A5\)A5S�A5G�A5;dA5+A5�A5oA5�A5+A533A5�A5%A5VA5
=A4��A4�HA4ĜA4�A4��A4�A4�!A4��A4��A4��A4��A4��A4��A4��A4��A4��A4�uA4�uG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                          A���A���A�ȴA���A���A�ȴA�ȴA�ȴA���A�ȴA�ȴA���A���A���A���A���A��
A��
A��
A���A�ȴA�ȴA�ȴAиRAд9AП�AЁA�dZA��A�VAΡ�A�$�A��A�ƨA�ZA�dZA�^5A�x�A��A�~�A���A���A���A���A��A�\)A���A��A���A���A��A���A���A��A��A��A�Q�A���A��A�-A�ƨA��A��A�VA��A�dZA�I�A�A}VAwS�Aq�PAkt�Ah1Ac/A]�PAZ�9AY��AYdZAU��AN��AJ�yAI7LAH�AD~�AB�/A@�A=�A<�+A<Q�A<�A9G�A9C�A8�`A8�9A933A9��A9%A6��A6Q�A5�A5S�A533A5�A4�RA4��A4�DA4jA4�A3%A21A17LA0=qA/�hA/;dA/A/�TA/�
A0A0bA/�hA/A.z�A.$�A-hsA,�DA+��A+?}A+��A+|�A+�A+�-A+dZA+G�A+%A*�!A)|�A(�A(z�A'�FA'?}A'
=A&jA%K�A#�#A#p�A#&�A"r�A!33A!�wA!p�A (�Al�A�7A"�A/A�DAƨA��A�AȴAM�A��AK�A�DAA�A��A;dA�uAAƨA7LAdZA33A�yA5?A{AAG�A�`A^5A�A�-A��A��At�A\)A/A��A��AA�A�#Ax�AG�A�A�yAĜAĜAz�A��A��AhsAC�A�A��A��A��AbNA{A��A/AĜAbNA�hA&�AVA
�HA
��A
z�A
1'A	x�A�\A�TAO�A��A��A�A1AO�A+A%A�HA�9A�A�+AZA��A�FA�PA33A�HA��Av�AVA�A{AbA��A��A�A �A {@���@�-@��@�7L@��@�1'@���@��P@�ȴ@�hs@��9@��@�(�@���@��m@��;@���@�+@�ȴ@�{@��u@�j@�(�@�@�
=@��@�hs@��@��/@�9@�@�1'@�dZ@�ff@�@��@�Q�@�R@��@�G�@��
@��y@�$�@��`@�D@�@�@��@◍@�J@���@��@��/@߶F@�K�@��@�V@���@�x�@�%@�j@�9X@���@۾w@�S�@���@��@ٙ�@ف@�G�@ؓu@�(�@�ƨ@��@�^5@�E�@�=q@�5?@�$�@ղ-@Դ9@�I�@�1'@�ƨ@�;d@ҸR@�M�@�5?@�{@�p�@��@мj@��;@���@���@�7L@��`@���@���@��@Ɂ@�O�@ȓu@�9X@��;@�"�@Ɵ�@�$�@�7L@ċD@�A�@��;@å�@Ý�@�|�@�C�@�@��@�J@���@��@��@�I�@��@�1@���@��w@�+@��@��!@�5?@��T@�&�@�I�@�  @��;@��
@��@�+@�@���@��\@��@���@�X@�V@���@�Q�@�9X@�  @��@�;d@�n�@��T@�G�@���@��9@��u@���@��P@�;d@��\@�E�@���@���@�A�@�1'@�  @��P@��H@�5?@�O�@��`@�A�@�ƨ@�S�@��@�v�@�E�@���@��@��9@���@�t�@�
=@���@���@�^5@��@�O�@�7L@���@�j@��@��@�;d@�@��R@�@�@��h@�p�@�7L@��@�b@���@�l�@�C�@�+@�o@���@���@�V@��T@�`B@�G�@��@���@�I�@��
@��@��@�K�@��!@�E�@�@�G�@�%@���@��`@���@��j@���@��u@��u@�I�@�1@�;d@��y@�n�@���@���@�x�@�7L@���@�Ĝ@���@�r�@�  @��m@���@��H@�v�@��@���@��h@�p�@�%@���@�(�@�1@��m@��w@��@��P@�;d@�@���@�$�@�@��@��T@���@�&�@���@��@��@��w@���@�l�@�;d@�@���@��+@��@���@��h@��@��@��u@�Z@�A�@�9X@�1'@� �@�b@���@�dZ@��H@�v�@�=q@���@�X@��@���@�z�@�  @�l�@���@��@��@��@�ȴ@��R@��!@���@�^5@���@��@�`B@�`B@�`B@�X@�7L@�&�@��@�Ĝ@��D@�A�@�1'@�@|�@~�R@~ff@}�T@}�-@}��@}�h@}O�@|��@|�@|��@|1@{@z=q@y�7@y�@x��@w�;@w�P@w�@v��@u@t�@t��@t9X@s�F@s�@s"�@r�H@r�!@r�\@r=q@qx�@q�@pĜ@p�@pA�@p  @o��@o�P@oK�@o�@nȴ@n$�@m/@lj@l�@k��@k�F@kdZ@j�@j��@j~�@j�@i�#@ihs@h�`@h�9@h��@hr�@hA�@hb@g�w@g
=@fff@e/@d�@d�j@d�@d�@dz�@d�@c�@cC�@c@b�!@b�@a�#@a�#@a��@a�^@a��@ax�@ahs@a%@`r�@`b@_�@_�w@_�P@_;d@^��@^�@^��@^5?@^@]@]`B@]�@\��@\�@\�/@\�@\z�@\I�@[��@[o@Z��@Z=q@Z�@Y�#@Y��@Y��@Yx�@Y7L@X�u@XA�@W�;@W|�@V�y@V{@U?}@T��@T�/@Tz�@T1@S��@SC�@S@R��@Rn�@R-@Q�#@Q��@Qx�@QX@Q7L@PbN@O+@N��@N�@Nȴ@N�R@N��@NE�@M�T@M`B@M?}@M/@M�@L�/@Lj@L1@L�@L1@K�@J�H@J�!@J�!@J�!@J^5@I��@IX@I7L@H�`@Hr�@H1'@Hb@G�@G�w@G��@G�P@Gl�@G+@F�@E�-@E/@E�@D�j@DI�@C��@C��@Ct�@Co@B�H@B��@Bn�@B-@A��@A�^@A��@A��@Ahs@A%@@�9@@ �@?�@?K�@>V@>@=��@=`B@=?}@=/@=�@<��@<��@<�@<Z@<(�@;��@;��@;o@:�H@:��@:��@:M�@9��@9�7@9hs@9X@9�@8�`@8�@81'@8  @8  @7��@6V@5�@4�@41@3�F@3o@2��@2n�@1�#@1�@0 �@/|�@.�y@.�R@.��@.ff@-�@-�-@-?}@,��@,��@,I�@+��@+�m@+�@+o@+@*�H@*��@*M�@*�@)��@)�^@)��@)��@)x�@)�@(��@'�w@'K�@'+@&�@&E�@&{@%�T@%��@%@%�-@%�h@%�@%O�@$��@#ƨ@#"�@"�@"��@"n�@!��@!��@!hs@!%@ ��@ �9@ ��@ �u@ �@ Q�@ A�@ 1'@ 1'@   @l�@+@�@�R@5?@��@`B@/@��@��@z�@��@ƨ@�F@��@�@t�@@��@~�@�#@�7@�@�`@�`@��@�u@b@��@;d@
=@�@�+@ff@V@V@V@5?@{@@��@��@��@�@/@�j@z�@9X@(�@1@�
@�F@dZ@33@33@o@�!@J@�^@��@x�@hs@X@X@X@G�@G�@7L@&�@%@�9@�u@�@A�@ �@b@�;@K�@;d@�@�@
=@�y@ȴ@�R@ff@5?@@�h@�@`B@/@V@�@�j@z�@j@Z@9X@1@��@��@�m@�m@�m@�
@ƨ@��@S�@33@@
�@
�H@
�H@
�H@
��@
��@
�!@
�!@
�!@
��@
^5@
M�@
M�@
M�@
M�A���A���A���A���A���A���A���A���A���A�ƨA�ĜA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�ȴA���A�ȴA�ĜA�ĜA�ȴA���A���A�ȴA�A�ĜA�ȴA���A���A�ƨA�ƨA�ȴA���A���A���A�ȴA���A�ȴA�ȴA�ƨA�ƨA�ƨA���A���A�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A���A���A���A���A��
A��A���A���A��
A��
A��A��
A���A���A��
A��#A��#A��A���A���A��
A��A��#A��A��
A�A�ĜA�ȴA���A���A���A�ȴA�ƨA���A���A�ȴA���A���A�ƨA�ȴA���A���A�ȴA�ƨA�ƨA���A���A���A�ȴA�ƨA�ƨA�ȴA�ƨAд9Aв-Aа!Aа!Aв-AжFAд9Aа!Aа!Aд9AжFAв-AУ�AЩ�AГuAБhAЙ�AЗ�AЗ�AЉ7AЉ7AЏ\A�z�A�`BA�XA�XA�\)A�hsA�z�A�ffA�ffA�Q�A�?}A�$�A��A��A��yAϛ�A�z�A�`BA�5?A�&�A�oA�JA�A���AΏ\A�hsA�`BA�?}A�1'A�1'A�/A�&�A�{A�1A���A���A��A��A��yA��;A���A���A�ȴA�ƨAͼjA͸RAͮA͉7A�dZA�O�A�33A��A�/A��ÁA�C�A��`AˋDA�hsA�  A���A�z�AȬA��#A��A�z�A¡�A��A�p�A��+A�(�A���A���A���A�|�A�+A�A�ZA��A�E�A���A�
=A�/A��A�dZA��A��hA�oA��\A���A��RA�A�E�A���A��;A�t�A�33A�%A��/A��jA���A�|�A�bNA�K�A�A�A�=qA�9XA�1'A��A�
=A��A��HA��wA���A��\A�|�A�n�A�dZA�O�A�(�A�1A��A���A�ȴA��A���A�`BA�A��A���A��
A��9A�I�A���A���A�t�A�G�A�"�A�%A��/A���A��A�|�A�r�A�jA�hsA�jA�ffA�`BA�"�A�VA��A�ƨA�A�^5A��A�1A���A���A���A��A��A��A��A��`A��#A���A���A��9A��\A�^5A�33A���A���A��wA��A�v�A�?}A��A�bA��mA���A�ZA�-A��A��DA�v�A��A��\A��hA��/A��`A��mA��A���A���A��FA�XA�bA�A��PA�\)A�"�A��`A��A�jA�&�A��A��A��
A�v�A�C�A�%A��A�ƨA��+A�E�A�%A���A���A���A��RA��PA�C�A�-A�  A��jA��A�Q�A�$�A��A�A��
A��FA���A�hsA��#A���A�hsA�=qA�A��
A��+A�5?A�(�A�  A���A�  A���A�^5A� �A��`A��RA�~�A�O�A� �A��A���A���A�ƨA�A��A���A��A��A��-A��-A��hA�r�A�I�A�(�A��A�p�A���A���A�S�A� �A���A��/A��jA�p�A�Q�A�A�A�+A�A��jA�z�A�S�A�1'A�bA��A��wA���A��+A�p�A�ZA�9XA�A��wA�ffA��A���A�~�A���A��^A���A��7A�v�A�dZA�A�A��A��`A���A�x�A���A�VA��TA�|�A�  A��A�I�A�"�A�ƨA��A�I�A��A�(�A��-A��A�I�A��jA��+A�jA�VA��9A�n�A�;dA���A��yA��jA��A�A�A��yA���A�bNA�&�A��mA���A�hsA�{A���A�1'A��hA���A��+A�A�=qA�^A~�`A~ �A}x�A|�A|z�A|1'A|  A{+Az  Ay�7Ax��Aw�Av�yAu�Au�Atn�At1As�Ar�Arz�Aq�wAp�!An�An$�AmhsAl�AlVAk��Aj�AjI�Ai��Ai/Ah�`Ah�9Ah�AhI�Ag��Ag��Ag+AfjAf�Aep�Ad-Ac`BAb=qAaXA`�`A`Q�A_�A_�A^ffA]��A\^5A[��A[��A[�7A[�A[x�A[AZ~�AZ1AY��AY�wAY��AY��AY��AY��AY�hAY��AY�AY�-AY��AY�
AY��AYXAY?}AY
=AX��AX��AX��AW�wAV�jAU�FAS�^AR=qAQoAP-AO�hAOK�AOVAN��AN�\ANJAMl�AL��AK�AK;dAJ�9AJE�AJ1AI��AI��AIx�AIG�AI&�AI33AI33AI&�AI�AIAH�AH��AH�uAHVAHAG��AG7LAF��AFI�AE��AEK�ADz�AC��AC�ACdZACK�AC;dAC+AC"�AC�AC%AB�RAB��ABffAB-AB(�AA��AA��AAl�A@�yA@jA?�A?`BA?
=A>��A>�!A>�+A>(�A=O�A<�9A<�!A<�RA<�jA<�A<�uA<�+A<n�A<Q�A<VA<^5A<E�A<�A< �A<(�A<=qA<jA<��A<�A<��A=%A=oA<�jA<bNA;��A:�+A9�FA9l�A9\)A9S�A9O�A9?}A9/A9&�A9&�A9&�A9"�A9&�A933A9dZA9hsA9hsA9\)A9?}A9%A8�A8��A8��A8�HA8�`A8�\A8E�A8^5A8��A8�`A8�A8�yA8�`A8�A8�A8��A8��A9�A9��A9��A9�;A9�A:  A:A9�mA9�FA9�hA9�7A9�PA9�7A9t�A9?}A8�yA8��A8A�A7��A7�-A7\)A6�A6r�A6jA6�DA6�uA6�9A6�RA6�\A6(�A5��A5��A5�^A5��A5�PA5�A5x�A5t�A5x�A5`BA5S�A5S�A5O�A5O�A5O�A5S�A5\)A5S�A5G�A5;dA5+A5�A5oA5�A5+A533A5�A5%A5VA5
=A4��A4�HA4ĜA4�A4��A4�A4�!A4��A4��A4��A4��A4��A4��A4��A4��A4��A4�uA4�uG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	jB	j�B	j�B	j�B	j�B	j�B	j�B	j�B	jB	kB	kB	j�B	jB	jB	j�B	jB	jKB	jB	jKB	kB	kB	kB	j�B	j�B	iyB	iB	g�B	c�B	a�B	X�B	ZQB	d&B	|�B	��B	�>B
C-B
�B
רB
�EB
��B
��B
� B�B�B�B�BPB�B�B�B8�BC�B1�BZBGEB�BB�B
�>B
�B
�aB
��B
�hB
z�B
VmB
:^B
$tB

rB	��B	��B	��B	�FB	�B	zxB	`�B	S�B	V9B	^jB	Y�B	�B	.B	�B	kB	�B	!�B	S�B	OB	K�B	Y�B	�B	��B	�	B	��B	��B	�MB
B
1[B
-B
B�B
H�B
HKB
OB
W�B
oiB
qB
r|B
qvB
��B
�B
{B
tB
o5B
l"B
m�B
�B
�+B
��B
��B
��B
�:B
�oB
��B
��B
��B
�_B
��B
�4B
��B
�{B
�IB
��B
��B
�nB
��B
�B
��B
��B
��B
�IB
�YB
�qB
��B
��B
�1B
��B
��B
�B
w�B
�rB
��B
��B
v+B
��B
�iB
�{B
��B
�B
z�B
xB
|PB
z�B
zB
u�B
o B
lWB
o5B
pB
l�B
kQB
l�B
n�B
sB
sMB
r�B
o B
l�B
lWB
l�B
h�B
h>B
gB
e�B
ffB
iB
iB
j�B
lWB
m]B
l"B
kB
jB
j�B
m�B
l�B
m�B
m)B
m�B
o�B
k�B
j�B
jKB
iyB
h�B
h
B
f�B
e`B
d�B
cTB
bB
`�B
`�B
_B
`vB
[�B
Z�B
Y�B
XB
V�B
T�B
T�B
P�B
PHB
LdB
K)B
J�B
H�B
I�B
EmB
C�B
D�B
CaB
C�B
A�B
B�B
A�B
A�B
?�B
?�B
?�B
>�B
>B
=B
=B
<�B
;�B
;�B
:�B
:�B
9�B
:�B
9$B
6�B
5�B
5tB
5B
4�B
5?B
4B
3�B
4nB
5�B
4B
4B
5B
5?B
5?B
5?B
5tB
5B
3hB
6zB
1�B
1'B
0�B
0UB
0!B
/�B
-�B
-wB
,�B
,=B
+�B
+kB
+6B
*�B
)�B
(XB
'RB
)�B
&LB
%�B
$tB
�B
VB
�B
1B
�B
�B
�B
qB
�B
�B
�B
kB
YB
YB
$B
�B
�B
B
{B
�B
B
�B
�B
�B
@B
�B
�B
�B
4B
B
 B
hB
�B
bB
�B
�B
\B
�B
�B
bB
�B
\B
�B
�B
(B
(B
VB
�B
"B
B
JB
VB
(B
�B
VB
PB
�B
.B
bB
�B
�B
�B
�B
�B
�B
B
FB
FB
FB
B
�B
{B
{B
�B
�B
�B
YB
SB
$B
YB
�B
�B
�B
�B
�B
+B
_B
�B
+B
�B
�B
_B
�B
�B
�B
�B
1B
�B
�B
_B
�B
�B
+B
�B
�B
�B
1B
�B
�B
7B
�B
�B
�B
~B
~B
OB
�B
�B
�B
�B
B
IB
�B
IB
B
�B
OB
!B
�B
�B
OB
B
�B
�B
IB
B
B
xB
�B
!B
!�B
#�B
%FB
%FB
$�B
$B
$@B
#�B
#�B
#�B
$tB
#nB
"�B
"hB
#B
"�B
#:B
#nB
#B
#:B
"�B
#B
$B
%zB
&�B
'�B
($B
(�B
(�B
(�B
*eB
+kB
,qB
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,=B
-B
-B
-CB
.}B
-�B
.B
.B
.B
.IB
.IB
.B
-�B
.B
.IB
/OB
/B
/�B
/�B
/�B
/�B
/�B
/�B
/OB
/B
/B
/�B
/OB
/�B
/�B
0!B
0�B
0�B
0�B
0�B
1�B
1�B
2aB
2aB
2�B
2�B
2�B
2�B
3�B
3�B
4nB
5tB
5?B
5B
4�B
5�B
6B
7B
7�B
7�B
7�B
7�B
7�B
8B
8RB
8�B
8�B
9�B
9$B
9�B
:^B
:*B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
<B
<�B
<jB
=qB
=�B
>B
=�B
>wB
?B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
C-B
C-B
B�B
C-B
B�B
C-B
C-B
C-B
D3B
D3B
D�B
DgB
EB
EmB
FB
FtB
F�B
F�B
F�B
F�B
F�B
GB
F�B
F�B
GzB
H�B
H�B
IB
IRB
I�B
J#B
JXB
K)B
K^B
K�B
LdB
L0B
MjB
M�B
M�B
N<B
NpB
NpB
NpB
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
PB
PB
PB
P�B
Q�B
Q�B
RTB
R B
R�B
R�B
S&B
S&B
S&B
S�B
S�B
T,B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
V�B
W�B
WsB
W�B
W�B
WsB
W�B
XB
XyB
X�B
YB
YB
Y�B
YB
YKB
YKB
YB
YB
YB
YKB
ZQB
ZQB
Z�B
ZQB
ZQB
ZQB
Z�B
Z�B
Z�B
[#B
[WB
Z�B
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
]/B
\�B
]/B
]/B
]dB
]�B
]�B
]�B
^jB
_;B
_B
_pB
_pB
_�B
`BB
`�B
`vB
`�B
`�B
`�B
aHB
a�B
a|B
bB
a�B
bNB
bB
bNB
bNB
bNB
a�B
dZB
c�B
c�B
d&B
d&B
c�B
d&B
d&B
d�B
e,B
d�B
d�B
d�B
e,B
e�B
e`B
e,B
e,B
e�B
f�B
f2B
e�B
e�B
f2B
gB
gB
gB
gmB
g�B
g�B
g�B
g�B
h
B
h>B
hsB
h�B
iB
h�B
kB
i�B
i�B
jB
jB
jB
j�B
j�B
kB
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
k�B
kB
j�B
k�B
kQB
k�B
k�B
k�B
k�B
k�B
l"B
l"B
l"B
lWB
l"B
lWB
l�B
l�B
m)B
l�B
m)B
m]B
m�B
m�B
m�B
m�B
m�B
n/B
ncB
ncB
ncB
m�B
n/B
p;B
p;B
p;B
p�B
p�B
qvB
qAB
q�B
rGB
r�B
s�B
s�B
t�B
tB
tB
t�B
t�B
t�B
uZB
u�B
u�B
v`B
v`B
v`B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
w�B
x8B
xB
xB
xB
x8B
xlB
y�B
yrB
y�B
zB
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
|PB
|PB
|�B
|�B
|�B
}VB
}�B
}VB
}�B
~(B
~(B
~]B
~]B
~]B
~�B
~�B
~�B
~]B
~�B
.B
.B
~�B
�B
� B
��B
��B
�B
�;B
�oB
��B
�AB
�uB
�uB
��B
�uB
�uB
�GB
�GB
�{B
�B
�MB
��B
��B
��B
��B
��B
�B
��B
��B
��B
�%B
��B
��B
��B
��B
��B
��B
��B
��B
�+B
��B
�_B
�_B
��B
��B
��B
�B
�B
�7B
�7B
�lB
��B
�	B
��B
��B
�rB
��B
�B
�B
�JB
�~B
�~B
�~B
�~B
�~B
�~B
�~B
��B
��B
�PB
�PB
��B
��B
��B
��B
�VB
��B
��B
��B
��B
��B
�(B
�(B
�(B
��B
��B
�.B
�.B
�bB
��B
��B
� B
�4B
��B
�:B
�B
�:B
�oB
��B
��B
��B
��B
��B
��B
��B
�B
�@B
��B
��B
�B
�B
�B
��B
��B
�B
�B
�FB
�FB
�{B
��B
�B
�B
�B
�MB
�FB	jB	jKB	iyB	jB	j�B	kQB	j�B	iDB	jB	j�B	k�B	j�B	iyB	j�B	jKB	k�B	jB	i�B	jB	kB	k�B	j�B	j�B	iyB	kB	k�B	j�B	i�B	jB	kB	k�B	jKB	i�B	jKB	k�B	lWB	j�B	j�B	i�B	jB	lWB	k�B	kB	iDB	i�B	jB	k�B	kB	j�B	jB	kQB	k�B	k�B	i�B	jB	kB	j�B	k�B	jB	j�B	i�B	j�B	kQB	k�B	j�B	iDB	jB	j�B	k�B	k�B	j�B	iDB	iyB	j�B	j�B	k�B	jB	jB	i�B	i�B	kB	k�B	j�B	jB	jB	jB	kQB	kQB	jB	i�B	iDB	jKB	j�B	j�B	jB	iB	i�B	j�B	k�B	jKB	iB	iDB	jKB	kQB	kB	j�B	i�B	iB	iyB	jB	n�B	k�B	kB	hsB	iyB	kB	k�B	kQB	jB	jB	kQB	jKB	l�B	k�B	kQB	jKB	i�B	kB	l"B	k�B	j�B	jKB	i�B	jB	k�B	k�B	l�B	j�B	l�B	hsB	kB	j�B	j�B	h�B	j�B	h�B	iyB	jB	i�B	i�B	gmB	g�B	ncB	lWB	jB	iyB	gB	h
B	ffB	bNB	iB	c�B	d&B	a�B	dZB	`B	d&B	e,B	`vB	e�B	aHB	f2B	_;B	\]B	_�B	h>B	XyB	Y�B	XyB	U�B	VmB	S�B	TaB	]�B	YKB	\)B	\�B	_�B	aHB	`�B	_pB	`B	g�B	pB	r�B	xB	|�B	}VB	�uB	��B	�FB	�qB	��B	��B	�B	��B	��B	��B	��B	�vB	��B
	�B
�B
�B
B[B
L�B
j�B
�B
��B
�B
��B
��B
�GB
�B
�ZB
�jB
��B
ǮB
�mB
�tB
��B
��B
�B
�B
��B
�-B
�EB
˒B
˒B
��B
�HBFB
�KB
�B
��B
��B
��B
�>B
�B
rB+B
	B=B 'B%�BSB\B	7B
�BuBMBSB�BB{B�B
��BoB�B�B�BB�B�BMBMBoB;B�B�B
��B�BB
��BB  B�BVB�B
=BBfB�B�B�B	7B�B
rB�BB
=B�B�BhB�B�B"B�B�B�BYBVBB:*B)�B �B 'B~B�BCB�B�BB�B�B+BeBB�BSB�B:B�B�B	B	lB�B�B �B
��B�B+B%BB�B�B
��B
��BuBB�BL�BR BR BO�BTaBX�Bb�BZQBTaBJ�BK�BIBDgBA B<�B7�B.�B9�B?}BC�B+B2aB'RB(XB-�B/�B0�B-CB2�B;0BGzBc�B`�B\�B_�BbNB\�BY�B^jBW
BT�BT,BR BM6BR�BW?B<�B5B49B-CB($B+6B�B�B�B#B!�BhB�B�B�B�B�B�BMB�B
�(B
��B
�]B  B�B�BABGBB1BDB	B�B�B�B
�B
��B
�MB
�oB
�>B
��B
�|B
��B
�NB
�yB
��B
�[B
��B
��B
�dB
�^B
�)B
�zB
��B
ĜB
��B
��B
��B
�jB
�B
��B
�0B
��B
�mB
�wB
��B
�B
��B
��B
�B
��B
��B
��B
��B
�@B
�B
�lB
��B
�PB
� B
� B
�uB
t�B
l�B
g�B
lWB
_�B
_B
a�B
p;B
NpB
L0B
Q�B
GEB
C-B
@�B
N�B
D�B
:�B
<6B
33B
+kB
8�B
/OB
/�B
-B
(�B
!�B
#nB
!-B
�B
1B
�B
�B
�B
 'B
JB	�DB	�]B
�B	��B	�B	�B	��B	�[B	��B	�B	��B	҉B	͟B	�$B	�B	�[B	�HB	��B	�B	��B	��B	��B	�FB	�bB	��B	��B	��B	��B	��B	�MB	�1B	��B	�SB	�hB	�.B	�	B	��B	�uB	�4B	�B	}�B	��B	{JB	��B	y�B	�AB	�B	�B	��B	qAB	lWB	poB	e�B	g�B	t�B	X�B	d�B	XB	V�B	O�B	K�B	L�B	VB	[�B	W�B	VB	T�B	V�B	U�B	V9B	U�B	VB	V9B	XEB	\]B	[#B	aB	c�B	`BB	^�B	^jB	ZB	VmB	X�B	sMB	iyB	YB	h�B	F?B	5B	*�B	~B	kB	+B	FB	�B	+B	�B	CB	�B	MB	�B	PB		B	�B	�B	�B	+B	�B	�B	�B	:B	�B	�B	�B	�B	xB	�B	B	�B	7B	�B	�B	�B	�B	%B	�B	�B	JB	xB	DB	�B	xB	4B	B	'B	IB	VB	K�B	N�B	P�B	W�B	P}B	Q�B	\)B	Q�B	R�B	O�B	K�B	K�B	L�B	RTB	XyB	L�B	GEB	H�B	JXB	M�B	L�B	L0B	NB	M�B	J�B	J#B	MB	Q�B	OB	S&B	TaB	aHB	t�B	x�B	x�B	|�B	��B	��B	��B	�kB	��B	��B	�rB	��B	��B	��B	�1B	�1B	��B	�lB	�xB	�"B	�4B	�MB	��B	�6B	�CB	�UB	�nB	�?B	�-B	��B	��B	��B	�^B	��B	��B	��B	�dB	��B	��B	��B	��B	�B	�sB	�KB	�
B	��B
;B
+B
"B
.B
4B
uB
_B
�B
!-B
&�B
*�B
.�B
1'B
5tB
4nB
33B
/�B
.}B
+6B
/B
0�B
(�B
"hB
6zB
6�B
8B
?�B
FtB
K^B
E�B
IRB
GEB
M6B
I�B
H�B
G�B
H�B
G�B
G�B
E�B
GEB
H�B
I�B
IRB
H�B
HB
NpB
M6B
PHB
PB
QNB
P}B
MjB
P�B
S�B
XyB
XB
XEB
]�B
aB
jB
q�B
p�B
p�B
qAB
qB
poB
p;B
p;B
qB
q�B
p�B
p�B
qAB
r|B
s�B
rGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                          B	d1B	d�B	deB	deB	d�B	deB	d�B	d�B	d1B	d�B	d�B	deB	d1B	d1B	deB	d1B	c�B	c�B	c�B	d�B	d�B	d�B	d�B	d�B	c+B	b�B	aSB	]:B	[cB	R�B	TB	]�B	v7B	�pB	��B
<�B
��B
�ZB
��B
B
ݣB
�B�B�B
�\B
��BB�B9B
��B28B=|B+BBS�B@�B�B
��B
��B
��B
��B
�B
�<B
�B
t_B
PB
4B
&B
$B	ΰB	�KB	�3B	��B	{�B	t*B	Z\B	M�B	O�B	XB	SfB	�B		�B	�B	B	gB	}B	MAB	H�B	EyB	S�B	��B	�zB	��B	��B	ԠB	��B
�B
+B
&�B
<AB
B2B
A�B
H�B
Q�B
iB
j�B
l.B
k(B
�zB
}�B
t�B
m�B
h�B
e�B
gxB
y~B
��B
�tB
��B
�aB
��B
�!B
�qB
��B
��B
�B
|�B
y�B
�CB
�-B
��B
�pB
�}B
� B
�[B
��B
�aB
��B
�jB
��B
�B
�#B
�zB
�3B
��B
~�B
|\B
y~B
q�B
�$B
��B
zOB
o�B
}�B
zB
}-B
~3B
yIB
t�B
q�B
vB
t_B
s�B
o@B
h�B
f	B
h�B
i�B
f�B
eB
f=B
h~B
l�B
l�B
lbB
h�B
frB
f	B
f=B
b�B
a�B
`�B
_�B
`B
b�B
b�B
deB
f	B
gB
e�B
d�B
c�B
d�B
g�B
frB
gCB
f�B
gxB
iPB
e�B
deB
c�B
c+B
bYB
a�B
`MB
_B
^AB
]B
[�B
Z\B
Z\B
X�B
Z(B
U>B
TlB
S�B
Q�B
PSB
N�B
NGB
JcB
I�B
FB
D�B
DsB
B�B
ClB
?B
=�B
>NB
=B
=HB
;pB
<vB
;;B
;pB
9�B
9cB
9�B
8�B
7�B
6�B
6�B
6QB
5B
5KB
4yB
4EB
3>B
4yB
2�B
0`B
/ZB
/&B
.�B
.�B
.�B
-�B
-�B
. B
/�B
-�B
-�B
.�B
.�B
.�B
.�B
/&B
.�B
-B
0,B
+BB
*�B
*<B
*B
)�B
)5B
'�B
')B
&WB
%�B
%QB
%B
$�B
$KB
#yB
"
B
!B
#�B
�B
�B
&B
pB
B
�B
�B
zB
�B
XB
#B
LB
EB
tB
B
B
B
�B
3B
gB
�B
-B
�B
�B
�B
�B
OB
�B
�B
OB

}B

�B
�B

�B
B
OB

B
	�B
	CB
	B
�B

IB

B
	�B
	B

IB
	�B
�B
�B
B
<B
�B
�B
�B
B
�B
<B
B
B

}B
	�B

B
6B
�B
	�B
	CB

}B
gB
�B
�B
�B
�B
�B
aB
-B
-B
aB
gB
�B
B
B
�B
B
?B
�B
�B
�B
�B
�B
B
�B
�B
EB
tB
B
EB
�B
zB
zB
�B
zB
zB
B
�B
zB
�B
?B
�B
tB
�B
LB
�B
�B
RB
XB
^B
0B
0B
B
�B
�B
6B
6B
�B
�B
^B
�B
�B
dB
B
�B
pB
<B
B
�B
XB
^B
�B
�B
�B
*B
�B
�B
�B
�B
�B
�B
[B
�B
�B
�B
UB
�B
&B
 B
NB
B
�B
�B
�B
 B
�B
�B
�B
�B
�B
,B
 �B
!�B
!�B
"?B
"?B
"�B
$B
%B
&#B
%�B
%QB
%QB
%QB
%QB
%QB
%QB
%QB
%�B
&�B
&�B
&�B
(/B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
)B
(�B
)�B
)jB
)�B
)�B
)jB
)5B
)B
(�B
(�B
)jB
)B
)jB
)�B
)�B
*�B
*�B
*pB
*<B
+BB
+vB
,B
,B
,HB
,�B
,|B
,�B
-�B
-�B
. B
/&B
.�B
.�B
.�B
/ZB
/�B
0�B
12B
12B
1gB
1gB
1�B
1�B
2B
28B
2mB
3�B
2�B
3sB
4B
3�B
4�B
4yB
4EB
4EB
4EB
4�B
4�B
4�B
5B
5�B
6QB
6B
7#B
7�B
7�B
7�B
8)B
8�B
:B
:5B
:5B
:5B
:jB
:jB
:jB
:jB
:5B
;;B
<AB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>NB
>B
>�B
?B
?�B
@&B
@ZB
@ZB
@ZB
@ZB
@�B
@�B
@ZB
@�B
A,B
B2B
B�B
B�B
CB
C8B
C�B
D
B
D�B
EB
EDB
FB
E�B
GB
GQB
G�B
G�B
H"B
H"B
H"B
HWB
I�B
I�B
I]B
I�B
I]B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
LB
K�B
L;B
L;B
L�B
L�B
L�B
MAB
MuB
M�B
NGB
NGB
NGB
NGB
N|B
N|B
N|B
O�B
PSB
QZB
Q%B
QZB
QZB
Q%B
QZB
Q�B
R+B
R`B
R�B
R�B
SfB
S1B
R�B
R�B
S1B
S1B
S1B
R�B
TB
TB
T8B
TB
TB
TB
T�B
T�B
T�B
T�B
U	B
T�B
U	B
U>B
U>B
U>B
U>B
UrB
UrB
UrB
UrB
U�B
V�B
VxB
V�B
V�B
WB
WJB
WJB
WJB
XB
X�B
X�B
Y"B
Y"B
Y�B
Y�B
Z�B
Z(B
Z\B
Z�B
Z�B
Z�B
[cB
[.B
[�B
[�B
\ B
[�B
\ B
\ B
\ B
[�B
^B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^uB
^�B
^uB
^�B
^uB
^�B
_{B
_B
^�B
^�B
_�B
`MB
_�B
_�B
_�B
_�B
`�B
`�B
`�B
aB
aSB
a�B
a�B
a�B
a�B
a�B
b%B
b�B
b�B
b�B
d�B
c�B
c�B
d1B
d1B
d1B
deB
d�B
d�B
eB
e7B
e7B
elB
elB
e�B
elB
e7B
elB
e�B
e�B
elB
d�B
d�B
elB
eB
e7B
e7B
elB
e�B
e�B
e�B
e�B
e�B
f	B
e�B
f	B
f�B
f�B
f�B
frB
f�B
gB
g�B
g�B
g�B
g�B
g�B
g�B
hB
hB
hB
g�B
g�B
i�B
i�B
i�B
j�B
jVB
k(B
j�B
k\B
k�B
lbB
m4B
mhB
n:B
m�B
m�B
n:B
n�B
n�B
oB
o@B
o�B
pB
pB
pB
p�B
p�B
p�B
p�B
qB
qLB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
sYB
s$B
sYB
s�B
t*B
t*B
t_B
t_B
t�B
t_B
t�B
t_B
t_B
t�B
vB
vB
v7B
v7B
v�B
wB
w=B
wB
w�B
w�B
w�B
xB
xB
xB
xCB
xCB
xCB
xB
xCB
x�B
x�B
x�B
yIB
y�B
zOB
z�B
z�B
z�B
{!B
{�B
{�B
|'B
|'B
|\B
|'B
|'B
|�B
|�B
}-B
}�B
}�B
~hB
~hB
~3B
~3B
~�B
~�B
nB
nB
nB
�B
�@B
�@B
�@B
�tB
�@B
�tB
��B
��B
��B
��B
�B
�B
��B
�LB
�LB
��B
��B
��B
��B
�B
��B
��B
��B
�RB
�$B
��B
��B
��B
��B
�0B
�0B
�0B
�0B
�0B
�0B
�0B
�eB
��B
�B
�B
�6B
�kB
�kB
�kB
�B
�qB
�<B
�qB
�qB
��B
��B
��B
��B
�CB
�CB
��B
��B
�B
�IB
�}B
��B
��B
��B
��B
��B
��B
�!B
��B
�UB
�UB
��B
��B
��B
��B
��B
��B
�[B
�[B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�-B
�aB
��B
��B
��B
��B
��B	c�B	c�B	c+B	c�B	deB	eB	d�B	b�B	c�B	d�B	e7B	deB	c+B	deB	c�B	elB	d1B	c_B	c�B	d�B	e7B	d�B	deB	c+B	d�B	elB	d�B	c�B	d1B	d�B	e�B	c�B	c�B	c�B	e7B	f	B	d�B	d�B	c�B	c�B	f	B	e�B	d�B	b�B	c�B	c�B	elB	d�B	d�B	d1B	eB	elB	e�B	c�B	c�B	d�B	d�B	elB	d1B	deB	c_B	deB	eB	elB	deB	b�B	c�B	d�B	e7B	elB	d�B	b�B	c+B	deB	d�B	e�B	d1B	c�B	c_B	c�B	d�B	e7B	deB	c�B	c�B	d1B	eB	eB	d1B	c_B	b�B	c�B	d�B	d�B	d1B	b�B	c_B	d�B	e7B	c�B	b�B	b�B	c�B	eB	d�B	deB	c_B	b�B	c+B	d1B	hJB	e�B	d�B	b%B	c+B	d�B	elB	eB	c�B	d1B	eB	c�B	f=B	e�B	eB	c�B	c�B	d�B	e�B	elB	deB	c�B	c�B	d1B	e7B	e7B	f=B	d�B	f=B	b%B	d�B	d�B	deB	b�B	d�B	bYB	c+B	c�B	c_B	c_B	aB	aSB	hB	f	B	c�B	c+B	`�B	a�B	`B	\ B	b�B	]�B	]�B	[cB	^B	Y�B	]�B	^�B	Z(B	_GB	Z�B	_�B	X�B	VB	YVB	a�B	R+B	S�B	R+B	O�B	PB	M�B	NB	WJB	R�B	U�B	VDB	YVB	Z�B	Z\B	Y"B	Y�B	a�B	i�B	lbB	q�B	vkB	wB	|'B	��B	��B	�#B	�^B	�jB	��B	��B	�BB	ǅB	�B	�(B	�qB
�B	��B
�B
<B
FB
deB
z�B
�zB
��B
�yB
�`B
��B
��B
�B
�B
ȋB
�`B
�B
�&B
��B
�EB
��B
ƳB
��B
��B
��B
�DB
�DB
˞B
��B�B
��B
��B
ܝB
�xB
֭B
��B
�lB$B �B�B�B�B�BB	B�BXB
�'B
��B
�B
�hB
��B
�-B
��B
�CB
�!B
��B
��B
�:B
��B
�bB
�bB
��B
��B
�!B
��B
�\B
��B
�IB
��B
��B
�IB
��B
��B
�nBB
��B�B
��BB[B	�BzB�BLB$BFB�B�B	�BqBB	�B<B�B�B�BRBBB�B3�B#�BBB�B0BjB�BXB�B�B�B�B�BB�BzBB?B�BaB�B�BB�B
�3B
��B
��B
��B �B
��B
��BeB �B
�7B
�=B'B�B�BFBK�BK�BI]BNBR`B\�BTBNBD�BE�BB�B>B:�B6�B1gB(dB3�B9/B=HB$�B,B!B"
B'�B)5B*<B&�B,�B4�BA,B]�BZ�BVDBY�B\ BVDBSfBXBP�BN|BM�BK�BF�BL;BP�B6�B.�B-�B&�B!�B$�B�BEBpB�B�BB�B�BzB @B�B
�nB
��B
�bB
��B
��B
�B
��B
�bB
�UB
��B
��B
��B�B�B�BzB
��B
��B�B
�B
��B
�!B
��B
��B
�.B
�uB
� B
�+B
�|B
�B
ςB
ӚB
�B
�B
��B
�,B
��B
�NB
��B
��B
�WB
�B
��B
�EB
��B
��B
�B
�)B
�pB
��B
��B
�[B
��B
�qB
�CB
�[B
��B
��B
��B
�B
��B
�B
y�B
y�B
|'B
n�B
frB
aSB
f	B
Y�B
X�B
[cB
i�B
H"B
E�B
K�B
@�B
<�B
:�B
HWB
>�B
4yB
5�B
,�B
%B
28B
)B
)5B
&�B
"?B
}B
 B
�B
6B
�B
�B
�B
EB
�B
�B	��B	�B	�hB	�B	�B	��B	ҔB	�B	ɑB	��B	��B	�;B	�QB	��B	��B	�B	��B	�KB	��B	�WB	�3B	��B	��B	�B	�gB	��B	��B	�EB	�RB	��B	��B	�9B	�B	�B	��B	��B	~hB	|'B	y�B	z�B	wqB	|�B	t�B	�RB	sYB	{�B	~�B	yIB	|\B	j�B	f	B	j!B	_{B	a�B	n:B	R�B	^uB	Q�B	P�B	I�B	EDB	FJB	O�B	UrB	QZB	O�B	NGB	P�B	OMB	O�B	O�B	O�B	O�B	Q�B	VB	T�B	Z�B	]�B	Y�B	XPB	XB	S�B	PB	R�B	l�B	c+B	R�B	b�B	?�B	.�B	$�B	0B	B	�B	�B	3B	�B	�B	�B	�B	�B	
}B	B	�B	 @B	 tB	 tB	 �B��B�:B	�B	�B	�B	�B	�B	�B	*B	LB	�B	zB	�B	RB	dB	zB	�B	�B	3B		wB	�B	*B	�B	�B	*B	
�B	�B	 �B	�B	O�B	E�B	H�B	J�B	QZB	J/B	K�B	U�B	K5B	L;B	I]B	EyB	EDB	FJB	LB	R+B	FB	@�B	B�B	D
B	GQB	FJB	E�B	G�B	G�B	D>B	C�B	F�B	K5B	H�B	L�B	NB	Z�B	n:B	rSB	r�B	v7B	�nB	��B	��B	�B	��B	��B	�$B	~�B	nB	nB	��B	��B	��B	�B	�*B	��B	��B	��B	�gB	��B	��B	�B	� B	��B	��B	�TB	�EB	��B	�B	ͪB	ɑB	�B	�B	؅B	ݣB	�uB	��B	�YB	�%B	��B	�B	�B	��B
 �B
�B
	�B

�B
'B
B
�B
�B
 3B
$KB
(dB
*�B
/&B
. B
,�B
)�B
(/B
$�B
(�B
*<B
"�B
B
0,B
0`B
1�B
9�B
@&B
EB
?TB
CB
@�B
F�B
ClB
B2B
A`B
B2B
A�B
A`B
?�B
@�B
BfB
ClB
CB
B�B
A�B
H"B
F�B
I�B
I�B
K B
J/B
GB
J�B
MuB
R+B
Q�B
Q�B
WJB
Z�B
c�B
k\B
jVB
j�B
j�B
j�B
j!B
i�B
i�B
j�B
k\B
j�B
jVB
j�B
l.B
m4B
k�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223252                            20230426223252AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622325220230426223252  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325220230426223252QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325220230426223252QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               