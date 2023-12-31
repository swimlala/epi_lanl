CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:55Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230426223255  20230426223255  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�O_bA�@�O_bA�11  @�O��Q�@�O��Q�@)�{5*�8@)�{5*�8�c{w�r�(�c{w�r�(11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  ?��@   @=p�@}p�@�  @\@�  A   A��A\)A+�A>�RA^�RA�  A�Q�A�  A��A�  AϮA�  A�B   B(�B(�B(�B�
B'�B/�
B7�
B?�
BH  BP  BX(�B`  Bh  Bp  Bx(�B�(�B�{B�  B�{B�{B�{B�{B�{B�  B�  B�{B�  B�  B�{B�{B�  B�{B�(�B��B�  B��B��B�  B�{B�  B�  B�  B�  B�{B�{B�  B�  B��C��C  C  C  C	��C�C��C  C
=C  C  C  C
=C  C�C   C"
=C$  C&  C(
=C*{C,
=C-��C0  C2
=C4  C6  C8  C9��C;�C=��C?�CA��CD  CF  CG��CI��CL  CN
=CP
=CR{CT
=CV
=CX{CZ  C[��C^  C`{Cb
=Cd  Cf  Ch
=Cj  Ck��Cn  Cp  Cr
=Ct  Cv  Cx
=Cz
=C|  C~  C��C�  C���C���C�  C���C��C���C�C�  C�  C���C���C���C���C���C���C�  C�  C�C���C���C�  C���C���C�  C���C���C���C���C�  C�C�  C���C�  C�  C���C�  C�C�
=C�  C���C���C���C���C���C���C�  C�  C�C�
=C�  C���C���C���C�  C�C�C�C�C�
=C�
=C�
=C�C�C�C�  C���C���C�  C�C�C�C�  C�  C���C���C���C�  C�C�  C�  C���C�C�\C�\C�
=C�
=C�  C�  C�
=C�C�  C�  C�  C���C���C���C���C���C���C���C���C�  C�  C�  C�  C�C�C�C�C�
=C�\C�C���C�  C�C�  C���C���C�  C���C���C�  C���C���C�  C���C���D ��D�D}qD��D� D  D� D�qD� D  D}qD  D��D  D}qD��DxRD�qD	��D	�qD
}qD
�qD}qD��Dz�D  D�D�D��D�D��D�D��D  D� D�qD� D�D�D  D}qDD��DD�D�D� D  D}qD��D}qD  D� D  D� D  D��D�D}qD  D� D  D��D   D }qD �qD!��D"�D"��D#  D#� D$  D$}qD%�D%� D%�qD&� D'  D'}qD'�qD(� D)  D)� D*  D*}qD*��D+}qD+�qD,}qD,��D-z�D-��D.z�D.��D/}qD/�qD0� D1�D1��D1�qD2}qD3  D3� D4D4�D5  D5��D6  D6� D7  D7}qD7�qD8� D9�D9� D9�qD:� D;�D;� D<  D<��D=  D=� D>�D>��D?  D?� D@�D@��DA  DA}qDB  DB� DC  DC� DD�DD��DD�qDE}qDE�qDF� DG�DG}qDH  DH��DH�qDI� DJ  DJ}qDK�DK� DL  DL� DM�DM��DN  DN}qDN�qDO� DO�qDPz�DP��DQz�DQ�qDR� DR��DSz�DS�qDT}qDU  DU� DV  DV� DW  DW}qDW�qDX��DY�DY��DZ�DZ}qD[  D[� D\�D\��D]  D]}qD]��D^}qD_  D_}qD_�qD`� Da�Da��Db  Db}qDc  Dc��Dd�Dd��De  De� De�qDf� Dg  Dg}qDg�qDh��Di�Di��Dj�Dj� Dj�qDk}qDl  Dl� Dl�qDm� DnDn��Do�Do��Dp�Dp��DqDq�Dr�Dr�Ds  Ds� Dt  Dt}qDt�qDu}qDu�qDv� Dw�Dw��Dx  Dx}qDy  Dy� Dz�Dz��D{�D{��D|  D|� D}  D}}qD}�qD~� D  D� D�HD�@ D�� D�� D�  D�@ D�}qD���D�  D�@ D�~�D�� D���D�=qD�~�D�� D�HD�@ D��HD��HD�  D�@ D�� D�� D�HD�>�D�� D��HD�HD�AHD���D�D�HD�AHD��HD��HD�HD�AHD�� D��qD���D�@ D��HD���D�  D�@ D��HD���D���D�>�D�~�D�� D�  D�AHD��HD�D�  D�AHD��HD�� D�  D�B�D���D�� D�HD�B�D��HD�� D���D�>�D�� D��HD�  D�>�D�}qD�� D�HD�@ D�� D��HD���D�=qD�� D��HD�  D�>�D�}qD��qD�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�>�D�~�D�� D�HD�AHD�~�D�� D�  D�AHD�� D���D�  D�AHD�~�D�� D�HD�@ D��HD�� D���D�>�D�}qD�� D�HD�@ D�~�D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D��D�AHD��HD�D�HD�AHD�� D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D�� D�HD�>�D�~�D�D��D�@ D�|)D�� D�HD�>�D�~�D�� D�HD�@ D�~�D��HD�HD�@ D�� D���D��qD�=qD�� D�D��D�B�D��HD��HD�  D�@ D�~�D���D���D�>�D�� D�� D�  D�B�D���D�� D���D�>�D�}qD�� D�  D�@ D�� D���D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�@ D�� D�� D���D�>�D�~�D���D���D�>�D�~�D��qD��qD�>�D�}qD���D�  D�AHD��HD�D��D�AHD�� D��HD�HD�@ D�~�D�� D�HD�AHD��HD���D�  D�AHD�� D�� D�  D�AHD��HD�� D�HD�@ D�� D�� D�HD�AHD��HD�D�HD�@ D D�� D�  D�AHDÀ D�� D�  D�@ DĀ D��HD�HD�@ Dŀ D�D�  D�=qDƀ D�� D���D�@ Dǀ D�� D�  D�@ DȁHD��HD�  D�@ Dɀ D��HD�  D�=qD�~�D�� D�  D�@ DˁHD�� D���D�@ D̀ D��HD�  D�AHD́HD�� D�  D�@ D΀ D��HD�  D�>�D�~�D�� D�HD�B�DЁHD�� D�  D�AHDр D�� D��D�AHDҀ D��HD�  D�>�DӀ DӾ�D���D�>�D�~�DԾ�D���D�AHDՁHD�� D�HD�AHDր DֽqD���D�>�D�~�D�� D���D�@ D؁HD��HD�  D�>�Dـ D��HD�  D�>�DځHD�D�  D�>�Dۀ D�� D���D�>�D�~�Dܾ�D�  D�AHD݀ DݽqD�  D�AHDށHD޾�D���D�@ D߀ D߾�D�  D�AHD�� DྸD�  D�AHD�~�D�� D�HD�@ D�~�D⾸D���D�@ D� D��HD�HD�@ D�~�D�� D��D�AHD�~�D徸D�  D�@ D�~�D澸D�  D�>�D� D�� D�  D�>�D� D�D��D�AHD�HD�� D�  D�>�D�~�D�qD�  D�AHD�HD��HD�HD�AHD� D�� D�  D�@ D�~�D���D���D�@ D� D�� D�HD�AHD�HD�D�HD�@ D��HD�D��qD�@ D� D�D���D�AHD� D�� D���D�>�D�HD�� D��qD�=qD� D�� D��qD�>�D��HD�D�  D�=qD�~�D��HD��D�@ D�~�D��qD��qD�@ D�~�D�� D�HD�@ D�~�D���D�HD�.D�o\>�Q�?#�
?W
=?�z�?�p�?�@�\@z�@(��@@  @Tz�@c�
@xQ�@�ff@��@��H@��
@�{@��H@��@�{@�Q�@�ff@��@��HA�A
=Ap�A�
A��Ap�A"�\A)��A0��A5A:=qA?\)AE�AJ�HAQG�AVffAZ�HA`��Ag�An{As33AxQ�A~�RA�=qA��A�\)A�=qA�p�A�Q�A�33A�{A���A��
A��RA�G�A�(�A�\)A�=qA��A�  A��\A��A�Q�A�33A�{A���A�33A�{A���A��
A�ffA���A��
AָRA�=qA���A߮A�\A�ffA���A��
A�ffA�A��A�  A��HA�p�B (�BB\)B��B{B\)B��B
=qB�B�BffB�
BG�B�RB  B�B�\B  BG�BffB�B��BffB�B ��B"{B#33B$z�B%�B'33B((�B)G�B*�\B,  B-G�B.�\B/�B0��B2=qB3�B4��B6ffB7�B8��B9�B;\)B<��B>{B?33B@Q�BA��BB�HBD(�BE��BF�RBH  BI�BJffBK�BL��BN=qBO\)BP��BQBS
=BTz�BUBW
=BX(�BYG�BZ�\B[�
B]G�B^�\B_�
Ba�Bb{Bc\)Bd��Bf{Bg�Bh��BiBk
=BlQ�BmBo
=Bpz�BqBr�HBt(�Bup�Bv�HBxQ�By��Bz�RB|  B}G�B~�RB�{B��RB�G�B�  B�z�B��B�B�ffB��B��B�Q�B��HB�p�B�{B���B�p�B�{B��RB�33B��
B�z�B��B��
B�ffB�
=B���B�(�B���B�p�B�{B��RB�\)B��B�z�B�
=B�B�ffB�
=B���B�(�B���B�p�B�{B��RB�G�B��
B�ffB��B��
B�ffB���B��B�(�B��RB�\)B�  B��RB�G�B��
B�ffB���B���B�Q�B��HB��B�  B���B�33B��B��\B��B��B�=qB���B�p�B�  B��RB�G�B��B�ffB���B��B�{B��RB�\)B�  B��\B��B���B�(�B��RB�\)B��B�z�B�
=B��B�=qB���B�\)B�  B��\B�33B��
B�z�B���B���B�{B��\B���B�p�B��
B�Q�B£�B��HB�33B�p�BÙ�BÙ�B�B�B�B�B�B�B�B��
B��B�  B�  B�{B�{B�{B�(�B�=qB�Q�B�z�Bď\BĸRB���B���B�
=B�33B�G�B�p�Bř�B�B�  B�(�B�ffBƏ\B���B���B��B�G�BǅBǮB��
B�{B�=qB�z�BȸRB�
=B�33BɅBɮB��B�(�B�ffBʣ�B��HB��B�\)B˅B�B�  B�=qB̏\B���B�
=B�\)B͙�B��
B�(�B�z�B���B�
=B�p�B��
B�(�B�z�B��HB�33Bљ�B��B�Q�Bҏ\B�
=B�\)B�B�{B�z�B��HB�G�BծB�{B�z�B���B�G�Bי�B�  B�ffB���B�33Bٙ�B�  B�ffB���B�33Bۙ�B�  B�Q�B���B��B݅B��B�Q�B���B�33B߅B��B�Q�B�RB��B�B��B�ffB���B�33B㙚B�  B�ffB���B�33B�B�{B�z�B���B�p�B��
B�Q�B���B�33B�B�(�B�\B�
=B�B�  B�z�B���B�p�B��B�ffB��HB�\)B��
B�ffB��HB�\)B��
B�ffB���B�G�B�B�=qB���B�33B��B�=qB���B�33B���B�(�B��\B��B���B�{B��\B�
=B���B�{B��\B�
=B���B�{B��\B��B���C {C Q�C �\C ��C
=C=qCz�C�RC��C(�CffC��C�HC�CffC��C�HC(�CffC��C�HC�C\)C�\C��C
=CG�C�CC  C=qCz�C�RC  CG�Cz�CC	  C	=qC	p�C	�C	�HC
�C
\)C
��C
�
C{CQ�C��C�HC�C\)C��C�HC�CffC��C�C33C�C��C
=CG�C�\CC
=C\)C��C�HC(�Cp�C�RC��C=qCz�C��C
=CQ�C��C��C=qC�C��C{CQ�C��C�C=qC�C�
C{C\)C��C��CG�C��C�HC33C�C��C{C\)C�C  CQ�C��C��CG�C�\C��C�Cp�C��C�Cp�C�RC  CG�C�\C�HC 33C �\C �
C!(�C!p�C!�RC"
=C"Q�C"��C#  C#Q�C#��C#��C$=qC$�\C$�HC%(�C%z�C%�
C&33C&�C&��C'{C'ffC'�RC({C(ffC(�C)  C)G�C)�\C)�C*=qC*��C*�C+33C+z�C+��C,�C,ffC,�RC-{C-ffC-�RC-��C.=qC.�\C.�HC/33C/�C/�
C0�C0ffC0�C1  C1G�C1��C1��C2G�C2��C2�HC333C3z�C3��C4(�C4z�C4��C5(�C5z�C5��C6{C6ffC6�C7
=C7ffC7�C8  C8G�C8��C8�C9=qC9��C9�C:33C:z�C:��C;�C;z�C;��C<�C<ffC<�C=  C=Q�C=�C>  C>Q�C>��C>�HC?33C?�C?�HC@33C@z�C@CA�CAp�CA��CB�CBp�CB�RCC
=CCffCCCD{CDffCD�RCE
=CEffCECF�CFp�CFCG{CGp�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141444444111414411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                 ?��@   @=p�@}p�@�  @\@�  A   A��A\)A+�A>�RA^�RA�  A�Q�A�  A��A�  AϮA�  A�B   B(�B(�B(�B�
B'�B/�
B7�
B?�
BH  BP  BX(�B`  Bh  Bp  Bx(�B�(�B�{B�  B�{B�{B�{B�{B�{B�  B�  B�{B�  B�  B�{B�{B�  B�{B�(�B��B�  B��B��B�  B�{B�  B�  B�  B�  B�{B�{B�  B�  B��C��C  C  C  C	��C�C��C  C
=C  C  C  C
=C  C�C   C"
=C$  C&  C(
=C*{C,
=C-��C0  C2
=C4  C6  C8  C9��C;�C=��C?�CA��CD  CF  CG��CI��CL  CN
=CP
=CR{CT
=CV
=CX{CZ  C[��C^  C`{Cb
=Cd  Cf  Ch
=Cj  Ck��Cn  Cp  Cr
=Ct  Cv  Cx
=Cz
=C|  C~  C��C�  C���C���C�  C���C��C���C�C�  C�  C���C���C���C���C���C���C�  C�  C�C���C���C�  C���C���C�  C���C���C���C���C�  C�C�  C���C�  C�  C���C�  C�C�
=C�  C���C���C���C���C���C���C�  C�  C�C�
=C�  C���C���C���C�  C�C�C�C�C�
=C�
=C�
=C�C�C�C�  C���C���C�  C�C�C�C�  C�  C���C���C���C�  C�C�  C�  C���C�C�\C�\C�
=C�
=C�  C�  C�
=C�C�  C�  C�  C���C���C���C���C���C���C���C���C�  C�  C�  C�  C�C�C�C�C�
=C�\C�C���C�  C�C�  C���C���C�  C���C���C�  C���C���C�  C���C���D ��D�D}qD��D� D  D� D�qD� D  D}qD  D��D  D}qD��DxRD�qD	��D	�qD
}qD
�qD}qD��Dz�D  D�D�D��D�D��D�D��D  D� D�qD� D�D�D  D}qDD��DD�D�D� D  D}qD��D}qD  D� D  D� D  D��D�D}qD  D� D  D��D   D }qD �qD!��D"�D"��D#  D#� D$  D$}qD%�D%� D%�qD&� D'  D'}qD'�qD(� D)  D)� D*  D*}qD*��D+}qD+�qD,}qD,��D-z�D-��D.z�D.��D/}qD/�qD0� D1�D1��D1�qD2}qD3  D3� D4D4�D5  D5��D6  D6� D7  D7}qD7�qD8� D9�D9� D9�qD:� D;�D;� D<  D<��D=  D=� D>�D>��D?  D?� D@�D@��DA  DA}qDB  DB� DC  DC� DD�DD��DD�qDE}qDE�qDF� DG�DG}qDH  DH��DH�qDI� DJ  DJ}qDK�DK� DL  DL� DM�DM��DN  DN}qDN�qDO� DO�qDPz�DP��DQz�DQ�qDR� DR��DSz�DS�qDT}qDU  DU� DV  DV� DW  DW}qDW�qDX��DY�DY��DZ�DZ}qD[  D[� D\�D\��D]  D]}qD]��D^}qD_  D_}qD_�qD`� Da�Da��Db  Db}qDc  Dc��Dd�Dd��De  De� De�qDf� Dg  Dg}qDg�qDh��Di�Di��Dj�Dj� Dj�qDk}qDl  Dl� Dl�qDm� DnDn��Do�Do��Dp�Dp��DqDq�Dr�Dr�Ds  Ds� Dt  Dt}qDt�qDu}qDu�qDv� Dw�Dw��Dx  Dx}qDy  Dy� Dz�Dz��D{�D{��D|  D|� D}  D}}qD}�qD~� D  D� D�HD�@ D�� D�� D�  D�@ D�}qD���D�  D�@ D�~�D�� D���D�=qD�~�D�� D�HD�@ D��HD��HD�  D�@ D�� D�� D�HD�>�D�� D��HD�HD�AHD���D�D�HD�AHD��HD��HD�HD�AHD�� D��qD���D�@ D��HD���D�  D�@ D��HD���D���D�>�D�~�D�� D�  D�AHD��HD�D�  D�AHD��HD�� D�  D�B�D���D�� D�HD�B�D��HD�� D���D�>�D�� D��HD�  D�>�D�}qD�� D�HD�@ D�� D��HD���D�=qD�� D��HD�  D�>�D�}qD��qD�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�>�D�~�D�� D�HD�AHD�~�D�� D�  D�AHD�� D���D�  D�AHD�~�D�� D�HD�@ D��HD�� D���D�>�D�}qD�� D�HD�@ D�~�D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D��D�AHD��HD�D�HD�AHD�� D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D�� D�HD�>�D�~�D�D��D�@ D�|)D�� D�HD�>�D�~�D�� D�HD�@ D�~�D��HD�HD�@ D�� D���D��qD�=qD�� D�D��D�B�D��HD��HD�  D�@ D�~�D���D���D�>�D�� D�� D�  D�B�D���D�� D���D�>�D�}qD�� D�  D�@ D�� D���D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�@ D�� D�� D���D�>�D�~�D���D���D�>�D�~�D��qD��qD�>�D�}qD���D�  D�AHD��HD�D��D�AHD�� D��HD�HD�@ D�~�D�� D�HD�AHD��HD���D�  D�AHD�� D�� D�  D�AHD��HD�� D�HD�@ D�� D�� D�HD�AHD��HD�D�HD�@ D D�� D�  D�AHDÀ D�� D�  D�@ DĀ D��HD�HD�@ Dŀ D�D�  D�=qDƀ D�� D���D�@ Dǀ D�� D�  D�@ DȁHD��HD�  D�@ Dɀ D��HD�  D�=qD�~�D�� D�  D�@ DˁHD�� D���D�@ D̀ D��HD�  D�AHD́HD�� D�  D�@ D΀ D��HD�  D�>�D�~�D�� D�HD�B�DЁHD�� D�  D�AHDр D�� D��D�AHDҀ D��HD�  D�>�DӀ DӾ�D���D�>�D�~�DԾ�D���D�AHDՁHD�� D�HD�AHDր DֽqD���D�>�D�~�D�� D���D�@ D؁HD��HD�  D�>�Dـ D��HD�  D�>�DځHD�D�  D�>�Dۀ D�� D���D�>�D�~�Dܾ�D�  D�AHD݀ DݽqD�  D�AHDށHD޾�D���D�@ D߀ D߾�D�  D�AHD�� DྸD�  D�AHD�~�D�� D�HD�@ D�~�D⾸D���D�@ D� D��HD�HD�@ D�~�D�� D��D�AHD�~�D徸D�  D�@ D�~�D澸D�  D�>�D� D�� D�  D�>�D� D�D��D�AHD�HD�� D�  D�>�D�~�D�qD�  D�AHD�HD��HD�HD�AHD� D�� D�  D�@ D�~�D���D���D�@ D� D�� D�HD�AHD�HD�D�HD�@ D��HD�D��qD�@ D� D�D���D�AHD� D�� D���D�>�D�HD�� D��qD�=qD� D�� D��qD�>�D��HD�D�  D�=qD�~�D��HD��D�@ D�~�D��qD��qD�@ D�~�D�� D�HD�@ D�~�D���D�HD�.D�o\>�Q�?#�
?W
=?�z�?�p�?�@�\@z�@(��@@  @Tz�@c�
@xQ�@�ff@��@��H@��
@�{@��H@��@�{@�Q�@�ff@��@��HA�A
=Ap�A�
A��Ap�A"�\A)��A0��A5A:=qA?\)AE�AJ�HAQG�AVffAZ�HA`��Ag�An{As33AxQ�A~�RA�=qA��A�\)A�=qA�p�A�Q�A�33A�{A���A��
A��RA�G�A�(�A�\)A�=qA��A�  A��\A��A�Q�A�33A�{A���A�33A�{A���A��
A�ffA���A��
AָRA�=qA���A߮A�\A�ffA���A��
A�ffA�A��A�  A��HA�p�B (�BB\)B��B{B\)B��B
=qB�B�BffB�
BG�B�RB  B�B�\B  BG�BffB�B��BffB�B ��B"{B#33B$z�B%�B'33B((�B)G�B*�\B,  B-G�B.�\B/�B0��B2=qB3�B4��B6ffB7�B8��B9�B;\)B<��B>{B?33B@Q�BA��BB�HBD(�BE��BF�RBH  BI�BJffBK�BL��BN=qBO\)BP��BQBS
=BTz�BUBW
=BX(�BYG�BZ�\B[�
B]G�B^�\B_�
Ba�Bb{Bc\)Bd��Bf{Bg�Bh��BiBk
=BlQ�BmBo
=Bpz�BqBr�HBt(�Bup�Bv�HBxQ�By��Bz�RB|  B}G�B~�RB�{B��RB�G�B�  B�z�B��B�B�ffB��B��B�Q�B��HB�p�B�{B���B�p�B�{B��RB�33B��
B�z�B��B��
B�ffB�
=B���B�(�B���B�p�B�{B��RB�\)B��B�z�B�
=B�B�ffB�
=B���B�(�B���B�p�B�{B��RB�G�B��
B�ffB��B��
B�ffB���B��B�(�B��RB�\)B�  B��RB�G�B��
B�ffB���B���B�Q�B��HB��B�  B���B�33B��B��\B��B��B�=qB���B�p�B�  B��RB�G�B��B�ffB���B��B�{B��RB�\)B�  B��\B��B���B�(�B��RB�\)B��B�z�B�
=B��B�=qB���B�\)B�  B��\B�33B��
B�z�B���B���B�{B��\B���B�p�B��
B�Q�B£�B��HB�33B�p�BÙ�BÙ�B�B�B�B�B�B�B�B��
B��B�  B�  B�{B�{B�{B�(�B�=qB�Q�B�z�Bď\BĸRB���B���B�
=B�33B�G�B�p�Bř�B�B�  B�(�B�ffBƏ\B���B���B��B�G�BǅBǮB��
B�{B�=qB�z�BȸRB�
=B�33BɅBɮB��B�(�B�ffBʣ�B��HB��B�\)B˅B�B�  B�=qB̏\B���B�
=B�\)B͙�B��
B�(�B�z�B���B�
=B�p�B��
B�(�B�z�B��HB�33Bљ�B��B�Q�Bҏ\B�
=B�\)B�B�{B�z�B��HB�G�BծB�{B�z�B���B�G�Bי�B�  B�ffB���B�33Bٙ�B�  B�ffB���B�33Bۙ�B�  B�Q�B���B��B݅B��B�Q�B���B�33B߅B��B�Q�B�RB��B�B��B�ffB���B�33B㙚B�  B�ffB���B�33B�B�{B�z�B���B�p�B��
B�Q�B���B�33B�B�(�B�\B�
=B�B�  B�z�B���B�p�B��B�ffB��HB�\)B��
B�ffB��HB�\)B��
B�ffB���B�G�B�B�=qB���B�33B��B�=qB���B�33B���B�(�B��\B��B���B�{B��\B�
=B���B�{B��\B�
=B���B�{B��\B��B���C {C Q�C �\C ��C
=C=qCz�C�RC��C(�CffC��C�HC�CffC��C�HC(�CffC��C�HC�C\)C�\C��C
=CG�C�CC  C=qCz�C�RC  CG�Cz�CC	  C	=qC	p�C	�C	�HC
�C
\)C
��C
�
C{CQ�C��C�HC�C\)C��C�HC�CffC��C�C33C�C��C
=CG�C�\CC
=C\)C��C�HC(�Cp�C�RC��C=qCz�C��C
=CQ�C��C��C=qC�C��C{CQ�C��C�C=qC�C�
C{C\)C��C��CG�C��C�HC33C�C��C{C\)C�C  CQ�C��C��CG�C�\C��C�Cp�C��C�Cp�C�RC  CG�C�\C�HC 33C �\C �
C!(�C!p�C!�RC"
=C"Q�C"��C#  C#Q�C#��C#��C$=qC$�\C$�HC%(�C%z�C%�
C&33C&�C&��C'{C'ffC'�RC({C(ffC(�C)  C)G�C)�\C)�C*=qC*��C*�C+33C+z�C+��C,�C,ffC,�RC-{C-ffC-�RC-��C.=qC.�\C.�HC/33C/�C/�
C0�C0ffC0�C1  C1G�C1��C1��C2G�C2��C2�HC333C3z�C3��C4(�C4z�C4��C5(�C5z�C5��C6{C6ffC6�C7
=C7ffC7�C8  C8G�C8��C8�C9=qC9��C9�C:33C:z�C:��C;�C;z�C;��C<�C<ffC<�C=  C=Q�C=�C>  C>Q�C>��C>�HC?33C?�C?�HC@33C@z�C@CA�CAp�CA��CB�CBp�CB�RCC
=CCffCCCD{CDffCD�RCE
=CEffCECF�CFp�CFCG{CGp�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141444444111414411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aϕ�Aϗ�AϏ\AϓuAϑhAϑhAϑhAϕ�Aϙ�Aϝ�Aϛ�Aϟ�Aϥ�Aϥ�Aϣ�Aϡ�AϬAϮAϰ!Aϰ!Aϰ!Aϰ!AϬAϧ�Aϧ�Aϡ�AϑhAυAϋDAχ+A�~�A�v�A�x�A�v�A�v�A�v�A�v�A�v�A�z�A�z�AσAυA�~�A�|�AρAυAσAσAχ+AρA�r�A�ffA�1AǁA�ƨA���A�7LA���A��A��9A��#A�`BA�jA�"�A�9XA�33A��-A���A���A��A~M�A|Q�Ay�#Ax=qAu�ArM�Ag��Ae�Aa�mA_K�A]��AZ��AW�wAUhsAR��AOdZAL�AI��AG�AG�AF��AC�mABȴAB  A@�/A?�A> �A=�;A=x�A=7LA<�/A<�RA=%A=�A<ĜA<r�A;��A;A:�!A:9XA9��A9�hA9;dA7�A7hsA6�A6 �A5
=A5�A4��A4^5A4{A3XA2�RA2-A1l�A0�\A/��A.n�A.5?A.(�A.=qA.9XA.VA.-A-��A,��A,=qA+�TA*��A*ffA*5?A)\)A)�A(��A(A(  A'p�A&�9A&n�A%��A$�`A$n�A#�mA#��A#��A#�PA#?}A"��A!t�A ��A ��A �uA��An�AAA-A$�A�^A"�A�DA5?AA�A1AdZA��A�DA��A��A%A��A`BA�PAO�A�jA�\AZA�mA�AhsA;dA+A�A/AoA�A�A�AdZAK�A��Av�A��A&�A^5A�#A
=A�`AQ�A�A��Ap�AC�A%A�HA�+A5?A�A�#A�-A�7A&�A
�uA
 �A	�mA	��A	x�A	C�A�AȴA�jA�+Ar�AffA��AC�A�DAM�A�At�A&�A��A��A9XA�-A�A"�A�A�A��A`BA �A z�A 1'@��
@�l�@�+@�M�@�7L@���@��@���@�ff@�G�@��@�b@��!@�/@�1'@�P@�
=@�5?@�j@�P@�dZ@�+@�ff@�%@�z�@���@�dZ@��@�5?@陚@��/@�Z@�(�@畁@�@�\@��#@��@�9X@�@��H@�v�@��T@���@�Z@��@��
@��@ޏ\@�G�@��/@܃@۝�@ڇ+@�$�@�hs@���@ش9@ج@� �@�@�`B@���@�1'@Ӯ@�V@��@Ь@ύP@��@�J@͙�@�/@���@���@̛�@�1'@ˍP@�@ɡ�@���@�z�@�I�@�  @Ǿw@�|�@Ə\@��@ŉ7@���@�1'@��;@öF@�"�@��H@�ȴ@§�@+@�=q@�{@���@���@�&�@��/@���@���@�"�@�^5@�=q@�$�@�@���@��@� �@��P@�+@���@�$�@��^@�hs@�?}@���@��m@�K�@���@�E�@���@�p�@�`B@�?}@�V@���@��@�|�@�dZ@�+@���@�M�@�/@��@��D@��@�S�@���@��!@�ff@��@��@��h@�X@�?}@�&�@��j@�Q�@��m@�|�@��H@���@�M�@�@���@�7L@���@��D@�1@�\)@�@���@��!@���@�ff@���@��^@��@��@��`@���@��D@�1@���@�l�@�"�@��y@���@�ȴ@�n�@��@���@�p�@�V@��u@���@���@�@���@��R@���@�V@�5?@���@�G�@�&�@��`@�j@�1'@�b@��
@��F@���@�C�@��@�o@��@�@�v�@��#@�p�@�?}@���@��@�z�@�(�@�1@��m@���@�l�@�33@���@��@��@�`B@�O�@�/@��@��D@�bN@� �@��F@�C�@��@�
=@��H@���@���@�n�@�=q@�$�@��@�hs@�&�@�V@�%@���@���@��/@��u@�A�@�  @��m@��@�+@�
=@��@�n�@�^5@�{@��^@�?}@���@��D@�I�@��m@���@��P@��P@�|�@�+@�@��H@��R@���@���@�^5@�$�@��@�hs@�z�@�9X@�(�@��@�1@��;@��@�l�@�K�@�;d@�"�@��@�ff@�-@��^@�7L@���@�r�@�Q�@�9X@� �@�  @��m@���@��w@��F@���@�33@�~�@�{@��T@�@���@�p�@�%@���@��j@�z�@�1'@�P@~ȴ@~V@}��@}�@|1@{dZ@z=q@y�7@x�@xA�@x  @w��@w�@w
=@u�h@u/@t�j@s�F@sdZ@r�@r�!@rn�@rM�@r�@q��@q��@q�7@qx�@pĜ@pbN@o�@n�y@m��@m/@l��@lI�@l1@kƨ@kt�@ko@j��@j��@j~�@i��@h��@hĜ@h��@hr�@hA�@g��@gK�@f�@f��@fV@e@e�@d�@d9X@c�@ct�@b�!@b=q@a�#@a�^@a��@`��@_�;@_K�@_
=@_
=@_
=@^ff@]��@]`B@]�@\�@\��@\�D@\9X@[t�@[o@Z=q@Z=q@Z�@Yhs@X�`@X�9@X1'@W�@W\)@W�@V��@U�T@U��@U�-@U�h@U?}@T��@T�@T��@T�@T��@T�D@TI�@S�m@SdZ@S@R�\@Q��@Q��@Q�^@Q��@Q��@QG�@P�u@PA�@P �@Pb@P  @O�;@O�w@O��@O��@O|�@O+@Nff@N{@M�T@M�-@M�@M/@MV@L��@Lz�@LI�@KS�@J^5@I�@I�#@Ihs@IX@H�`@G��@G�@F�@F��@Fff@FV@F$�@F@E�-@EV@Dj@D�@C��@C��@C�m@CS�@B��@BM�@A��@Ahs@@�`@@��@@Ĝ@@�@@A�@@ �@@  @?�w@?\)@?
=@>�@>��@>�+@>E�@=��@=�-@=p�@<�j@<j@<I�@<9X@<(�@;��@;ƨ@;�F@;��@;��@;�@;dZ@;33@;@:�!@:�@9��@97L@8��@8Ĝ@8��@8��@8�u@8bN@8A�@81'@8 �@8  @7�;@7��@7+@6�R@6v�@6@5p�@4�@4j@4�@3�F@3dZ@333@2��@2-@1��@1�#@1��@1�^@1��@1X@1%@0Q�@/�@.ȴ@.v�@.{@-@-��@,��@,j@+�@*�H@*�\@*�@)��@)x�@)&�@(��@(��@(Ĝ@(�u@(Q�@'�@'�w@'�@'|�@'K�@&�@&v�@&$�@%��@%@%�@$��@$��@$�@#��@#t�@#o@"�@"�!@"~�@"n�@"n�@"^5@"�@!��@!�^@!�^@!hs@!7L@ �`@ �@ r�@ Q�@ b@�@�P@;d@�@�@�+@$�@@��@�@�@�@z�@I�@�@1@�m@�
@�@S�@"�@��@^5@=q@-@�@�#@��@�^@�^@�^@��@��@x�@%@Ĝ@�@ �@  @��@K�@�@
=@��@�@�R@��@�+@V@5?@$�@�@��@?}@V@�/@��@��@�@��@��@z�@j@9X@�F@"�@��@�\@~�@n�@M�@��@��@�7@7L@&�@&�@&�@&�@�@�@��@�9@�@bN@Q�@A�@1'@  @��@�w@�P@+@�@�R@E�@�@��@p�@?}@�@��@z�@Z@�@�F@��@��@��@t�@S�@dZ@S�@S�@C�@C�@
��@
�\@
�\@
n�@
=q@
-@
�@	��@	��@	x�@	&�@	%@	%@��@�u@�@Q�@ �@�;@�w@�w@�P@;d@��@�@�@�AϏ\AϑhAϕ�Aϗ�Aϙ�Aϗ�Aϕ�Aϕ�AϓuAω7AϑhAϕ�AϑhAϓuAϓuAϏ\AύPAϏ\AϓuAϑhAϏ\AϑhAϓuAϓuAϑhAϓuAϙ�Aϙ�Aϙ�Aϙ�Aϙ�Aϝ�Aϝ�Aϛ�Aϛ�AϓuAϙ�Aϩ�AϬAϩ�Aϥ�Aϣ�Aϧ�Aϣ�Aϟ�Aϟ�Aϣ�Aϧ�Aϩ�Aϥ�Aϩ�AϬAϧ�Aϡ�Aϡ�Aϟ�Aϡ�Aϥ�Aϝ�Aϝ�Aϡ�AϬAϬAϩ�Aϧ�Aϩ�AϮAϰ!Aϰ!AϬAϬAϬAϰ!Aϰ!Aϰ!AϮAϬAϰ!Aϲ-Aϲ-AϮAϮAϮAϲ-Aϴ9Aϰ!AϬAϮAϲ-Aϰ!AϬAϬAϲ-Aϴ9Aϲ-Aϩ�Aϩ�Aϰ!AϮAϬAϧ�Aϧ�Aϥ�Aϩ�Aϩ�Aϥ�Aϣ�Aϥ�Aϩ�Aϩ�Aϧ�Aϣ�Aϣ�AϮAϮAϡ�Aϙ�Aϛ�Aϕ�Aϝ�Aϡ�A�x�A�z�AϑhAϗ�AύPA�~�A�z�A�z�AυAχ+AϏ\AύPAω7Aω7Aχ+AϋDAύPAϋDAω7Aχ+AρAυAω7AυAϋDA�|�A�x�A�x�A�z�A�z�A�z�A�x�A�v�A�t�A�v�A�x�A�z�A�x�A�v�A�v�A�v�A�v�A�z�A�z�A�x�A�t�A�t�A�r�A�t�A�x�A�x�A�v�A�t�A�r�A�t�A�t�A�x�A�x�A�x�A�t�A�t�A�t�A�x�A�x�A�x�A�v�A�v�A�t�A�v�A�x�A�x�A�v�A�v�A�v�A�x�A�z�A�z�A�|�A�z�A�x�A�x�A�x�A�x�A�|�A�|�A�|�A�|�A�x�A�x�AσAχ+Aω7Aω7AυAσAσAσAυAσAσA�~�A�|�A�|�A�|�A�~�AρAρA�~�A�z�A�|�A�~�A�~�AρA�~�A�|�A�|�AσAχ+Aχ+Aχ+AσAυAυAυAυAυAσAρA�~�AρAρAυAυAσA�~�AρAρAχ+AϋDAϏ\AϋDAυAσAυAσAυAυAυAρA�~�A�|�A�z�A�z�A�z�A�x�A�r�A�n�A�jA�jA�jA�l�A�n�A�n�A�l�A�hsA�M�A�oA���A�M�A�I�AʶFA�%A�7LAȩ�A��HA�S�A�7LA�/A�-A�"�A���A�;dA���A�Q�AăA���AÕ�A�hsA�^5A�O�A�XA�S�A�XA�XA�XA�hsA�n�A�jA�dZA�`BA�XA�S�A�?}A�7LA�oA²-A�1'A��HA���A�
=A��^A���A���A���A��DA�x�A�M�A�A��RA���A�%A�|�A�1A�|�A���A�p�A��A�ȴA�p�A�oA���A�1A��PA�A�A���A��
A���A�ȴA�ƨA���A�M�A�/A��;A�+A�ZA�C�A�(�A���A���A��HA���A��^A��A��A�jA��A��-A��A���A�S�A��A��A�JA�hsA�bA���A��HA��RA�x�A�9XA��
A�JA�t�A��A�^5A���A�XA�/A��A�ZA��HA�C�A�ZA��A�M�A��A���A��A��/A���A���A�ȴA���A��!A��uA�~�A�x�A�x�A�v�A�hsA�XA�G�A�E�A�=qA�-A���A���A�1A��jA��\A��A�VA�oA���A���A�7LA��A��A�;dA���A��A�  A��!A�v�A�
=A��RA��hA�O�A���A��uA��DA��A�33A�A�A��PA�JA���A�dZA�33A�1A��9A��A�=qA���A���A�ffA���A��-A�t�A�XA�M�A�/A��FA�I�A��mA��\A�XA��A���A��A��PA�x�A�`BA�-A�A��HA���A��+A�t�A�jA�VA�{A��A"�A~ĜA~�+A~ffA~I�A}�#A}S�A|�yA|�9A|��A|~�A|ffA|^5A|(�A{��A{��A{|�Az�yAzM�Az1Ay�AydZAx��Ax��Ax��Ax�+Axv�AxM�Ax5?Ax$�Ax{Aw�Aw�^Awp�Av�yAv5?Au�^AuXAu�AuoAu
=At�`At��AtJAr�ArJAp��AoO�Am�FAkhsAhbNAgl�Af��Af��Afr�AfI�AfJAe�^Ae��Ae�7Ae�Ae�Aep�Ae&�Ad1'Ac|�Ab�HAb1AaoA`z�A`A�A`-A`$�A`A_�-A^��A^r�A^M�A^ �A^A]�A]��A]�-A]K�A]
=A\z�A[��A[��AZ��AZAY��AY
=AX��AXr�AX�AW�AWK�AV�`AV~�AV(�AU��AU`BAU�AT�AT�\AT=qAS��AS&�ARM�AQdZAP�HAP^5AO�#AOp�AO;dAO"�AO�AN��AN��AN�+AM�-AKoAJbNAJ1'AJ-AJ(�AJ �AJ  AI�FAH��AH^5AH{AG�AG�AG33AGoAGoAGVAGoAGoAG�AG�AG�AGVAG
=AGAF�AF��AF1'AE�AE��AE��ACƨAB�AB�`AB�/AB�AB�AB��ABȴAB��AB�RAB��AB��AB~�AB=qAB1AA��AAK�AA"�A@��A@�`A@�/A@��A@��A@ĜA@�!A@v�A?��A?hsA>�A>ȴA>�uA>Q�A>5?A=��A=�A=�A=�mA=�A=�A=�A=�TA=�
A=�-A=�hA=|�A=x�A=p�A=l�A=\)A=S�A=G�A=?}A=33A=+A=�A=VA<��A<�`A<�A<��A<ȴA<��A<�!A<��A<�!A<�RA<��A<��A<�yA<��A<��A=VA=�A=�A=�A="�A="�A=+A=&�A=
=A<�`A<ĜA<�yA<�yA<�!A<�+A<�+A<�DA<�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                 Aϕ�Aϗ�AϏ\AϓuAϑhAϑhAϑhAϕ�Aϙ�Aϝ�Aϛ�Aϟ�Aϥ�Aϥ�Aϣ�Aϡ�AϬAϮAϰ!Aϰ!Aϰ!Aϰ!AϬAϧ�Aϧ�Aϡ�AϑhAυAϋDAχ+A�~�A�v�A�x�A�v�A�v�A�v�A�v�A�v�A�z�A�z�AσAυA�~�A�|�AρAυAσAσAχ+AρA�r�A�ffA�1AǁA�ƨA���A�7LA���A��A��9A��#A�`BA�jA�"�A�9XA�33A��-A���A���A��A~M�A|Q�Ay�#Ax=qAu�ArM�Ag��Ae�Aa�mA_K�A]��AZ��AW�wAUhsAR��AOdZAL�AI��AG�AG�AF��AC�mABȴAB  A@�/A?�A> �A=�;A=x�A=7LA<�/A<�RA=%A=�A<ĜA<r�A;��A;A:�!A:9XA9��A9�hA9;dA7�A7hsA6�A6 �A5
=A5�A4��A4^5A4{A3XA2�RA2-A1l�A0�\A/��A.n�A.5?A.(�A.=qA.9XA.VA.-A-��A,��A,=qA+�TA*��A*ffA*5?A)\)A)�A(��A(A(  A'p�A&�9A&n�A%��A$�`A$n�A#�mA#��A#��A#�PA#?}A"��A!t�A ��A ��A �uA��An�AAA-A$�A�^A"�A�DA5?AA�A1AdZA��A�DA��A��A%A��A`BA�PAO�A�jA�\AZA�mA�AhsA;dA+A�A/AoA�A�A�AdZAK�A��Av�A��A&�A^5A�#A
=A�`AQ�A�A��Ap�AC�A%A�HA�+A5?A�A�#A�-A�7A&�A
�uA
 �A	�mA	��A	x�A	C�A�AȴA�jA�+Ar�AffA��AC�A�DAM�A�At�A&�A��A��A9XA�-A�A"�A�A�A��A`BA �A z�A 1'@��
@�l�@�+@�M�@�7L@���@��@���@�ff@�G�@��@�b@��!@�/@�1'@�P@�
=@�5?@�j@�P@�dZ@�+@�ff@�%@�z�@���@�dZ@��@�5?@陚@��/@�Z@�(�@畁@�@�\@��#@��@�9X@�@��H@�v�@��T@���@�Z@��@��
@��@ޏ\@�G�@��/@܃@۝�@ڇ+@�$�@�hs@���@ش9@ج@� �@�@�`B@���@�1'@Ӯ@�V@��@Ь@ύP@��@�J@͙�@�/@���@���@̛�@�1'@ˍP@�@ɡ�@���@�z�@�I�@�  @Ǿw@�|�@Ə\@��@ŉ7@���@�1'@��;@öF@�"�@��H@�ȴ@§�@+@�=q@�{@���@���@�&�@��/@���@���@�"�@�^5@�=q@�$�@�@���@��@� �@��P@�+@���@�$�@��^@�hs@�?}@���@��m@�K�@���@�E�@���@�p�@�`B@�?}@�V@���@��@�|�@�dZ@�+@���@�M�@�/@��@��D@��@�S�@���@��!@�ff@��@��@��h@�X@�?}@�&�@��j@�Q�@��m@�|�@��H@���@�M�@�@���@�7L@���@��D@�1@�\)@�@���@��!@���@�ff@���@��^@��@��@��`@���@��D@�1@���@�l�@�"�@��y@���@�ȴ@�n�@��@���@�p�@�V@��u@���@���@�@���@��R@���@�V@�5?@���@�G�@�&�@��`@�j@�1'@�b@��
@��F@���@�C�@��@�o@��@�@�v�@��#@�p�@�?}@���@��@�z�@�(�@�1@��m@���@�l�@�33@���@��@��@�`B@�O�@�/@��@��D@�bN@� �@��F@�C�@��@�
=@��H@���@���@�n�@�=q@�$�@��@�hs@�&�@�V@�%@���@���@��/@��u@�A�@�  @��m@��@�+@�
=@��@�n�@�^5@�{@��^@�?}@���@��D@�I�@��m@���@��P@��P@�|�@�+@�@��H@��R@���@���@�^5@�$�@��@�hs@�z�@�9X@�(�@��@�1@��;@��@�l�@�K�@�;d@�"�@��@�ff@�-@��^@�7L@���@�r�@�Q�@�9X@� �@�  @��m@���@��w@��F@���@�33@�~�@�{@��T@�@���@�p�@�%@���@��j@�z�@�1'@�P@~ȴ@~V@}��@}�@|1@{dZ@z=q@y�7@x�@xA�@x  @w��@w�@w
=@u�h@u/@t�j@s�F@sdZ@r�@r�!@rn�@rM�@r�@q��@q��@q�7@qx�@pĜ@pbN@o�@n�y@m��@m/@l��@lI�@l1@kƨ@kt�@ko@j��@j��@j~�@i��@h��@hĜ@h��@hr�@hA�@g��@gK�@f�@f��@fV@e@e�@d�@d9X@c�@ct�@b�!@b=q@a�#@a�^@a��@`��@_�;@_K�@_
=@_
=@_
=@^ff@]��@]`B@]�@\�@\��@\�D@\9X@[t�@[o@Z=q@Z=q@Z�@Yhs@X�`@X�9@X1'@W�@W\)@W�@V��@U�T@U��@U�-@U�h@U?}@T��@T�@T��@T�@T��@T�D@TI�@S�m@SdZ@S@R�\@Q��@Q��@Q�^@Q��@Q��@QG�@P�u@PA�@P �@Pb@P  @O�;@O�w@O��@O��@O|�@O+@Nff@N{@M�T@M�-@M�@M/@MV@L��@Lz�@LI�@KS�@J^5@I�@I�#@Ihs@IX@H�`@G��@G�@F�@F��@Fff@FV@F$�@F@E�-@EV@Dj@D�@C��@C��@C�m@CS�@B��@BM�@A��@Ahs@@�`@@��@@Ĝ@@�@@A�@@ �@@  @?�w@?\)@?
=@>�@>��@>�+@>E�@=��@=�-@=p�@<�j@<j@<I�@<9X@<(�@;��@;ƨ@;�F@;��@;��@;�@;dZ@;33@;@:�!@:�@9��@97L@8��@8Ĝ@8��@8��@8�u@8bN@8A�@81'@8 �@8  @7�;@7��@7+@6�R@6v�@6@5p�@4�@4j@4�@3�F@3dZ@333@2��@2-@1��@1�#@1��@1�^@1��@1X@1%@0Q�@/�@.ȴ@.v�@.{@-@-��@,��@,j@+�@*�H@*�\@*�@)��@)x�@)&�@(��@(��@(Ĝ@(�u@(Q�@'�@'�w@'�@'|�@'K�@&�@&v�@&$�@%��@%@%�@$��@$��@$�@#��@#t�@#o@"�@"�!@"~�@"n�@"n�@"^5@"�@!��@!�^@!�^@!hs@!7L@ �`@ �@ r�@ Q�@ b@�@�P@;d@�@�@�+@$�@@��@�@�@�@z�@I�@�@1@�m@�
@�@S�@"�@��@^5@=q@-@�@�#@��@�^@�^@�^@��@��@x�@%@Ĝ@�@ �@  @��@K�@�@
=@��@�@�R@��@�+@V@5?@$�@�@��@?}@V@�/@��@��@�@��@��@z�@j@9X@�F@"�@��@�\@~�@n�@M�@��@��@�7@7L@&�@&�@&�@&�@�@�@��@�9@�@bN@Q�@A�@1'@  @��@�w@�P@+@�@�R@E�@�@��@p�@?}@�@��@z�@Z@�@�F@��@��@��@t�@S�@dZ@S�@S�@C�@C�@
��@
�\@
�\@
n�@
=q@
-@
�@	��@	��@	x�@	&�@	%@	%@��@�u@�@Q�@ �@�;@�w@�w@�P@;d@��@�@�@�AϏ\AϑhAϕ�Aϗ�Aϙ�Aϗ�Aϕ�Aϕ�AϓuAω7AϑhAϕ�AϑhAϓuAϓuAϏ\AύPAϏ\AϓuAϑhAϏ\AϑhAϓuAϓuAϑhAϓuAϙ�Aϙ�Aϙ�Aϙ�Aϙ�Aϝ�Aϝ�Aϛ�Aϛ�AϓuAϙ�Aϩ�AϬAϩ�Aϥ�Aϣ�Aϧ�Aϣ�Aϟ�Aϟ�Aϣ�Aϧ�Aϩ�Aϥ�Aϩ�AϬAϧ�Aϡ�Aϡ�Aϟ�Aϡ�Aϥ�Aϝ�Aϝ�Aϡ�AϬAϬAϩ�Aϧ�Aϩ�AϮAϰ!Aϰ!AϬAϬAϬAϰ!Aϰ!Aϰ!AϮAϬAϰ!Aϲ-Aϲ-AϮAϮAϮAϲ-Aϴ9Aϰ!AϬAϮAϲ-Aϰ!AϬAϬAϲ-Aϴ9Aϲ-Aϩ�Aϩ�Aϰ!AϮAϬAϧ�Aϧ�Aϥ�Aϩ�Aϩ�Aϥ�Aϣ�Aϥ�Aϩ�Aϩ�Aϧ�Aϣ�Aϣ�AϮAϮAϡ�Aϙ�Aϛ�Aϕ�Aϝ�Aϡ�A�x�A�z�AϑhAϗ�AύPA�~�A�z�A�z�AυAχ+AϏ\AύPAω7Aω7Aχ+AϋDAύPAϋDAω7Aχ+AρAυAω7AυAϋDA�|�A�x�A�x�A�z�A�z�A�z�A�x�A�v�A�t�A�v�A�x�A�z�A�x�A�v�A�v�A�v�A�v�A�z�A�z�A�x�A�t�A�t�A�r�A�t�A�x�A�x�A�v�A�t�A�r�A�t�A�t�A�x�A�x�A�x�A�t�A�t�A�t�A�x�A�x�A�x�A�v�A�v�A�t�A�v�A�x�A�x�A�v�A�v�A�v�A�x�A�z�A�z�A�|�A�z�A�x�A�x�A�x�A�x�A�|�A�|�A�|�A�|�A�x�A�x�AσAχ+Aω7Aω7AυAσAσAσAυAσAσA�~�A�|�A�|�A�|�A�~�AρAρA�~�A�z�A�|�A�~�A�~�AρA�~�A�|�A�|�AσAχ+Aχ+Aχ+AσAυAυAυAυAυAσAρA�~�AρAρAυAυAσA�~�AρAρAχ+AϋDAϏ\AϋDAυAσAυAσAυAυAυAρA�~�A�|�A�z�A�z�A�z�A�x�A�r�A�n�A�jA�jA�jA�l�A�n�A�n�A�l�A�hsA�M�A�oA���A�M�A�I�AʶFA�%A�7LAȩ�A��HA�S�A�7LA�/A�-A�"�A���A�;dA���A�Q�AăA���AÕ�A�hsA�^5A�O�A�XA�S�A�XA�XA�XA�hsA�n�A�jA�dZA�`BA�XA�S�A�?}A�7LA�oA²-A�1'A��HA���A�
=A��^A���A���A���A��DA�x�A�M�A�A��RA���A�%A�|�A�1A�|�A���A�p�A��A�ȴA�p�A�oA���A�1A��PA�A�A���A��
A���A�ȴA�ƨA���A�M�A�/A��;A�+A�ZA�C�A�(�A���A���A��HA���A��^A��A��A�jA��A��-A��A���A�S�A��A��A�JA�hsA�bA���A��HA��RA�x�A�9XA��
A�JA�t�A��A�^5A���A�XA�/A��A�ZA��HA�C�A�ZA��A�M�A��A���A��A��/A���A���A�ȴA���A��!A��uA�~�A�x�A�x�A�v�A�hsA�XA�G�A�E�A�=qA�-A���A���A�1A��jA��\A��A�VA�oA���A���A�7LA��A��A�;dA���A��A�  A��!A�v�A�
=A��RA��hA�O�A���A��uA��DA��A�33A�A�A��PA�JA���A�dZA�33A�1A��9A��A�=qA���A���A�ffA���A��-A�t�A�XA�M�A�/A��FA�I�A��mA��\A�XA��A���A��A��PA�x�A�`BA�-A�A��HA���A��+A�t�A�jA�VA�{A��A"�A~ĜA~�+A~ffA~I�A}�#A}S�A|�yA|�9A|��A|~�A|ffA|^5A|(�A{��A{��A{|�Az�yAzM�Az1Ay�AydZAx��Ax��Ax��Ax�+Axv�AxM�Ax5?Ax$�Ax{Aw�Aw�^Awp�Av�yAv5?Au�^AuXAu�AuoAu
=At�`At��AtJAr�ArJAp��AoO�Am�FAkhsAhbNAgl�Af��Af��Afr�AfI�AfJAe�^Ae��Ae�7Ae�Ae�Aep�Ae&�Ad1'Ac|�Ab�HAb1AaoA`z�A`A�A`-A`$�A`A_�-A^��A^r�A^M�A^ �A^A]�A]��A]�-A]K�A]
=A\z�A[��A[��AZ��AZAY��AY
=AX��AXr�AX�AW�AWK�AV�`AV~�AV(�AU��AU`BAU�AT�AT�\AT=qAS��AS&�ARM�AQdZAP�HAP^5AO�#AOp�AO;dAO"�AO�AN��AN��AN�+AM�-AKoAJbNAJ1'AJ-AJ(�AJ �AJ  AI�FAH��AH^5AH{AG�AG�AG33AGoAGoAGVAGoAGoAG�AG�AG�AGVAG
=AGAF�AF��AF1'AE�AE��AE��ACƨAB�AB�`AB�/AB�AB�AB��ABȴAB��AB�RAB��AB��AB~�AB=qAB1AA��AAK�AA"�A@��A@�`A@�/A@��A@��A@ĜA@�!A@v�A?��A?hsA>�A>ȴA>�uA>Q�A>5?A=��A=�A=�A=�mA=�A=�A=�A=�TA=�
A=�-A=�hA=|�A=x�A=p�A=l�A=\)A=S�A=G�A=?}A=33A=+A=�A=VA<��A<�`A<�A<��A<ȴA<��A<�!A<��A<�!A<�RA<��A<��A<�yA<��A<��A=VA=�A=�A=�A="�A="�A=+A=&�A=
=A<�`A<ĜA<�yA<�yA<�!A<�+A<�+A<�DA<�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	�6B	�B	�jB	�6B	�jB	�jB	�B	�jB	�6B	��B	��B	��B	͟B	�<B	�jB	�jB	��B	͟B	�B	�<B	�pB	�pB	�B	��B	�<B	�pB	�B	�B	͟B	�B	��B	��B	��B	͟B	��B	�B	�pB	ϫB	��B	�TB	��B	�9B	��B	خB	ںB	چB	�QB	�B	�jB	�B	��B
�B
B
#�B
`vB
��B
��B
q�B
� B
j�B
_�B
[�B
+B
#�B
0UB
B
 B	��B	�B	��B	�QB	ٴB	�jB	�B	�0B	��B	}"B	{B	j�B	bNB	dZB	]/B	^B	^�B	U�B	d&B	gB	y�B	~]B	��B	�B	�B	��B	�!B	��B	�NB	�fB	�B	��B
AB
�B
GzB
^5B
��B
�9B
��B
�UB
�B
�wB
�B
�UB
�'B
��B
��B
��B
�B
��B
�hB
�9B
�3B
��B
�3B
�9B
�B
��B
��B
�B
��B
��B
��B
��B
�tB
�UB
��B
��B
�B
��B
�}B
�'B
�qB
�=B
��B
�LB
�LB
�\B
��B
��B
��B
��B
��B
�kB
�SB
��B
��B
��B
�SB
�B
�FB
�(B
��B
�1B
�_B
��B
~�B
z�B
{�B
�B
�YB
��B
�;B
|�B
yrB
x�B
|�B
zDB
tB
r�B
lWB
iyB
a�B
a|B
n/B
q�B
u�B
p;B
p;B
o5B
m�B
i�B
kQB
j�B
kB
l�B
poB
o�B
r|B
w2B
wfB
poB
r�B
u%B
s�B
m]B
kQB
c B
`�B
a�B
cTB
_�B
\�B
[WB
Z�B
Z�B
YKB
Y�B
\�B
[#B
[#B
[�B
Z�B
[#B
\�B
W�B
U�B
R�B
S�B
S�B
R�B
RTB
QB
P�B
QB
R B
S�B
S�B
RTB
M�B
M�B
N<B
L�B
J�B
J#B
H�B
H�B
F?B
EB
D�B
C�B
A�B
?}B
=�B
=B
;�B
9�B
9�B
8�B
8B
7�B
6�B
6FB
7LB
6FB
5B
1[B
0!B
.�B
/B
.IB
)�B
(�B
&�B
&�B
&B
#:B
!�B
!�B
"�B
 �B
�B
�B
B
OB
�B
�B
�B
OB
�B
�B
=B
CB
�B
B
B
	B
eB
1B
eB
7B
�B
�B
�B
YB
$B
SB
�B
@B
�B
�B
B
�B
�B
bB
.B
�B
bB
(B
"B
�B
�B
�B
�B
�B
PB
PB
PB
�B
�B
�B
PB
�B
�B
PB
B
VB
"B
"B
�B
"B
VB
"B
4B
hB
4B
�B
bB
�B
�B
hB
B
oB
@B
B
�B
YB
�B
�B
+B
�B
+B
�B
�B
�B
$B
�B
�B
�B
�B
SB
�B
�B
$B
�B
YB
YB
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
	B
	B
B
�B
=B
�B
=B
qB
kB
_B
+B
�B
�B
�B
eB
B
�B
B
�B
!B
!B
VB
 'B
�B
�B
�B
 'B
�B
�B
 'B
 \B
 �B
 \B
 �B
!�B
"4B
#B
#:B
$tB
$tB
$tB
%FB
%B
%FB
%�B
%�B
%zB
%�B
%�B
%�B
%FB
%FB
%FB
%B
$�B
%FB
%�B
%�B
%�B
%�B
&�B
&B
&B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
*�B
+�B
,=B
,�B
-�B
-�B
-�B
-�B
-wB
.B
.B
.�B
/�B
0!B
/�B
/�B
/OB
/OB
/B
/�B
/B
/B
/�B
/B
/OB
0!B
0�B
0�B
0!B
/�B
/�B
/�B
/�B
/OB
/OB
/�B
/�B
0!B
0UB
0�B
0�B
0�B
0�B
1'B
1'B
1�B
2�B
2�B
2�B
2�B
2aB
2aB
2�B
2�B
3hB
3hB
33B
4B
4�B
49B
4�B
5tB
5B
5�B
6B
7B
7B
7LB
7�B
8B
8RB
8B
7�B
8B
8RB
8B
8RB
8�B
8B
8B
8�B
8B
8B
9�B
;0B
:�B
:�B
:�B
:�B
;0B
;dB
;�B
;�B
;dB
;dB
<6B
<�B
<jB
=�B
>B
?HB
?B
?HB
?HB
?}B
?�B
?�B
?�B
?�B
?�B
?�B
AUB
B�B
B�B
B�B
C-B
B�B
C�B
C�B
C�B
C�B
DgB
D�B
E�B
E�B
F?B
FB
FB
H�B
HB
IRB
J#B
J�B
J�B
K)B
J�B
J�B
K�B
M�B
M6B
MjB
N�B
NB
N�B
N�B
N�B
N�B
OBB
OvB
OBB
OB
OBB
PB
O�B
PHB
QB
Q�B
Q�B
Q�B
R B
RTB
Q�B
RTB
R�B
R B
Q�B
RTB
R�B
R�B
S&B
S[B
S�B
TaB
U�B
VB
V9B
VmB
W
B
W
B
V�B
W
B
VB
V9B
WsB
XB
W�B
W�B
WsB
W
B
YB
YKB
YKB
X�B
X�B
XyB
ZQB
[WB
[�B
[�B
[�B
[�B
[�B
\)B
\)B
\�B
\�B
\)B
\)B
]dB
\�B
\�B
]�B
]�B
_pB
_;B
`vB
`�B
`�B
`�B
`�B
aB
aB
aB
aHB
aHB
aHB
aB
aB
a�B
a�B
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
d�B
e,B
e,B
e,B
e,B
e`B
e�B
e`B
e�B
e�B
e`B
gB
f�B
f�B
f�B
gB
f�B
g8B
g8B
f�B
gB
gmB
gmB
g�B
g�B
g�B
g�B
hsB
h>B
h
B
h
B
h
B
h
B
iyB
i�B
iDB
iDB
jB
jB
jKB
jB
j�B
j�B
kB
j�B
kQB
k�B
k�B
k�B
k�B
k�B
lWB
lWB
lWB
l�B
m]B
l�B
m]B
m)B
m)B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
n�B
o B
o5B
oiB
o5B
o5B
o5B
oiB
oiB
o�B
o�B
o�B
pB
pB
pB
poB
p;B
p�B
poB
qAB
q�B
rB
q�B
rB
r|B
sB
r�B
r�B
sB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u%B
u�B
u�B
v`B
u�B
w2B
w2B
w�B
w�B
x8B
xlB
x�B
x�B
y	B
y	B
x�B
y	B
x�B
y�B
zB
zDB
zDB
zB
zDB
z�B
z�B
{B
{JB
{B
{B
|PB
|PB
|�B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~]B
~]B
~�B
�B
�B
� B
�B
�B
�B
�4B
�iB
�B
�;B
�oB
��B
�oB
��B
��B
��B
��B
�AB
�uB
��B
��B
�B
�GB
�GB
�{B
��B
��B
�B
��B
��B
��B
��B
�B
�B
�SB
�SB
�SB
�SB
�SB
�SB
��B
�YB
��B
�+B
�+B
�+B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�lB
�lB
�lB
��B
��B
�	B
�	B
�rB
��B
��B
��B
�B
��B
��B
��B
�DB
�B
��B
�B
��B
�B
��B
��B
��B
��B
�"B
�VB
�VB
�VB
�"B
�VB
�VB
�VB
�VB
��B
��B
��B
��B
��B
��B
�(B
�(B
�(B
�\B
��B
��B
��B
�bB
��B
� B
� B
� B
�hB
�B
��B
�B
�:B
��B
��B
��B
��B
��B
�@B
�B
�B
�B
�B
��B
��B
��B
��B
�B
�B
�B
�B
�FB
�FB
��B
�B
�B
�B
�MB
�MB
�MB
��B
�B
��B
��B
��B
�YB
��B
��B
�+B
�+B
�$B	ɺB	�B	�jB	��B	�0B	�B	͟B	��B	�<B	�B	˒B	�0B	�<B	��B	�dB	͟B	�<B	�jB	̘B	͟B	�B	�pB	̘B	�dB	�B	�pB	�dB	�jB	��B	��B	͟B	�6B	�jB	�dB	�6B	͟B	��B	�dB	��B	�jB	��B	ΥB	��B	̘B	�B	�BB	�B	�B	�B	�pB	�<B	�jB	͟B	ΥB	�B	�vB	��B	��B	�B	�<B	��B	̘B	��B	̘B	�<B	�pB	�<B	��B	�jB	�pB	�B	��B	͟B	̘B	�6B	�<B	ΥB	͟B	��B	�6B	�pB	�B	�pB	�jB	�dB	ΥB	�BB	��B	�6B	͟B	ΥB	�vB	�B	̘B	�jB	ϫB	�B	�B	�vB	�6B	�<B	ϫB	ϫB	�6B	�6B	�<B	ΥB	�B	�0B	�}B	͟B	��B	��B	�6B	��B	� B	уB	�#B	��B	��B	̘B	�mB	҉B	�tB	� B	��B	�B	�BB	�B	��B	ΥB	�^B	̘B	͟B	��B	�<B	��B	˒B	�0B	�6B	͟B	�vB	��B	�dB	�6B	˒B	��B	ΥB	�B	͟B	�6B	�jB	��B	��B	ΥB	�B	�B	�B	�B	��B	�<B	��B	�pB	͟B	��B	͟B	��B	�pB	��B	��B	̘B	�dB	�B	��B	ΥB	�pB	�B	�B	̘B	�jB	�pB	��B	��B	͟B	�6B	�jB	�<B	��B	�B	��B	�jB	�jB	�B	��B	�B	��B	��B	�vB	�B	�BB	��B	�HB	бB	��B	�vB	��B	�BB	ϫB	�}B	бB	�jB	�gB	�aB	��B	��B	��B	֡B	�9B	՛B	�gB	��B	�gB	՛B	֡B	�sB	��B	�B	�B	�
B	�B	�sB	֡B	�B	�B	�
B	خB	��B	�KB	�KB	�KB	ںB	��B	��B	��B	�QB	�B	�B	�B	�#B	یB	��B	چB	�B	�B	�B	�#B	��B	��B	�B	�B	�)B	�pB	�B	�BB	ߤB	�pB	ޞB	��B	�B	ޞB	�B	�B	�B	�B	�/B	��B	ޞB	�pB	�5B	�5B	ܒB	��B	�B	��B	�
B	�B	ٴB	یB	�B
+6B
\B
oB
 B
�B
+B
�B
$�B
B
B
	7B
	7B
�B
!�B
�B
 'B
;�B
)�B
*eB
%�B
!�B
OB
!B
�B
�B
�B
!�B
CB
=B
xB
B
�B
�B
�B
7B

�B	��B
�B
kB
B
VB
DgB
D3B
,qB
K�B
9�B
6�B
5?B
8�B
4�B
*�B
^B
k�B
T�B
S[B
W�B
\)B
^B
a�B
b�B
b�B
h
B
l"B
u%B
uZB
u�B
u�B
q�B
�B
rGB
sMB
rB
u%B
k�B
d�B
��B
�jB
�IB
�uB
�uB
�.B
��B
��B
�B
��B
�fB
��B
��B
��B
�GB
��B
j�B
{B
kB
��B
�_B
h�B
h
B
kQB
o5B
m�B
h
B
zB
�;B
y�B
l�B
��B
s�B
��B
�SB
��B
{�B
x8B
�oB
��B
��B
~(B
t�B
n�B
jKB
m�B
i�B
i�B
gB
g�B
h�B
iDB
f�B
b�B
aB
`�B
`�B
^�B
_�B
Y�B
WsB
^jB
hsB
x�B
��B
pB
MjB
H�B
=qB
6zB
>B
@OB
1[B
(�B
&�B
'�B
%B
5?B
)_B
�B
!-B
OB
�B
B
�B
%B
�B
:�B
R�B
=qB
Q�B
/�B
1�B
,B
!B
�B
"4B
$B
 �B
�B
IB
�B
$B
B
�B
eB
DB

�B
�B
�B
B
B
�B
�B
	�B
 �B	��B	��B	��B	��B	�B	�rB	�%B	��B	�fB	�B	��B	�]B	�;B	��B	�sB	�mB	��B	�;B	�pB	�sB	�ZB	�pB	��B	��B	چB	�QB	�B	یB	��B	��B	�WB	�fB	�B	�2B	�EB	�9B	�,B	�gB	��B	��B	��B	�}B	ϫB	˒B	�XB	�^B	�XB	��B	�B	�pB	ȀB	�'B	��B	��B	��B	�B	�$B	��B	��B	��B	ÖB	��B	�B	�sB	��B	�YB	�B	��B	��B	�B	�B	�B	{B	z�B	y�B	x�B	z�B	�B	�fB	|�B	}"B	�AB	�B	t�B	l�B	jB	h>B	jB	r�B	l"B	iB	c�B	d�B	`�B	a|B	`�B	b�B	b�B	bB	m]B	[�B	a�B	j�B	iyB	Z�B	jB	\�B	XB	\�B	\)B	^�B	`B	`�B	^jB	a|B	^jB	XyB	\)B	]�B	\�B	]dB	b�B	bB	aHB	[�B	Z�B	\�B	YKB	U2B	R�B	P�B	P�B	O�B	S&B	d�B	�CB	^�B	`�B	^�B	^�B	^�B	b�B	e�B	zB	v�B	u%B	u%B	y�B	�B	{B	{B	{B	|B	}�B	}�B	cB	��B	��B	�B	��B	��B	�SB	��B	�%B	��B	�hB	��B	�eB	�B	��B	�FB	�B	�FB	�B	�@B	��B	�LB	�B	��B	�zB	��B	��B	��B	�IB	��B	��B	��B	��B	��B	��B	��B	�B	� B	��B	��B	�}B	��B	ݘB	�BB	�
B	��B	�fB	�fB	�`B	��B	�,B	�`B	�mB	�B	�B	�/B	��B	�/B	� B	�vB	�B	�B	��B	��B	��B	�lB	��B	��B
�B
uB
B
�B
�B
�B
�B
VB
"4B
%�B
0�B
=�B
B�B
EmB
H�B
J�B
S�B
XEB
Z�B
[�B
\)B
^�B
f�B
r�B
�SB
��B
�\B
��B
��B
��B
��B
�9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                 B	�B	��B	ǹB	�B	��B	�B	�B	ƳB	�B	��B	ǅB	�B	ǅB	�QB	��B	�B	�B	ǅB	�QB	ǹB	��B	�"B	�"B	ǹB	ǅB	��B	�"B	ǹB	ƳB	�QB	ǹB	ǅB	ǅB	ǅB	�QB	ǅB	ǹB	�"B	�]B	ɑB	�B	ςB	��B	ЈB	�`B	�lB	�8B	�B	عB	�B	׳B	ҔB
 @B
�B
UB
Z(B
�zB
�^B
k�B
y�B
deB
YVB
U�B
$�B
�B
*B
�B

�B	�qB	�\B	߰B	�B	�fB	�B	��B	��B	��B	v�B	t�B	d�B	\ B	^B	V�B	W�B	X�B	O�B	]�B	`�B	sYB	xB	~�B	��B	��B	��B	��B	�;B	� B	�B	�JB	�@B	��B
�B
A,B
W�B
��B
��B
�EB
�B
��B
�)B
��B
�B
��B
�gB
�HB
�mB
��B
�EB
�B
��B
��B
�pB
��B
��B
��B
��B
��B
��B
�}B
�<B
�BB
��B
�&B
�B
�ZB
�HB
��B
�^B
�/B
��B
�#B
��B
��B
��B
��B
�B
�BB
��B
�6B
�<B
��B
�B
�B
��B
�OB
��B
�B
��B
��B
��B
�XB
��B
�B
�RB
x�B
t�B
ueB
yIB
�B
}bB
z�B
v�B
s$B
r�B
v7B
s�B
m�B
lbB
f	B
c+B
[cB
[.B
g�B
k�B
ouB
i�B
i�B
h�B
g�B
c_B
eB
d�B
d�B
frB
j!B
iPB
l.B
p�B
qB
j!B
lbB
n�B
m�B
gB
eB
\�B
Z\B
[cB
]B
YVB
VxB
U	B
TlB
T8B
R�B
S�B
VDB
T�B
T�B
UrB
T8B
T�B
VDB
Q�B
O�B
LoB
MuB
M�B
L�B
LB
J�B
J�B
J�B
K�B
MAB
MAB
LB
G�B
GQB
G�B
FJB
D>B
C�B
BfB
B2B
?�B
>�B
>�B
=|B
;;B
9/B
7�B
6�B
5B
3�B
3sB
28B
1�B
1gB
0�B
/�B
0�B
/�B
.�B
+B
)�B
(�B
(�B
'�B
#�B
"?B
 �B
 gB
�B
�B
HB
HB
NB
wB
<B
�B
�B
B
dB
dB
dB
B
�B
�B
�B
�B
XB
�B
�B
�B
B
�B
B
�B
?B
9B
nB
B
�B
B
[B
�B
[B
�B
�B
UB

}B

B
	�B
	�B

B
�B
�B
kB
eB
	wB
�B
kB
B
B
B
�B
�B
kB
B
kB
�B
B
�B
B
�B
�B
�B
�B
B
�B

�B
B

�B

}B

B
OB
�B
B
�B
!B
�B
�B
nB
B
?B
�B
�B
�B
�B
EB
zB
?B
�B
tB
zB
zB
�B
B
9B
�B
�B
nB
B
B
tB
tB
?B
tB
�B
tB
LB
�B
�B
�B
�B
�B
�B
XB
�B
XB
�B
#B
B
B
�B
tB
tB
�B
B
�B
�B
�B
�B
�B
�B
B
�B
pB
�B
<B
�B
<B
�B
�B
B
BB
B
BB
}B
�B
�B
�B
&B
&B
&B
�B
�B
�B
�B
aB
,B
�B
aB
aB
�B
�B
�B
�B
[B
�B
�B
aB
aB
aB
 gB
�B
�B
!�B
!mB
!9B
!mB
!mB
!9B
"?B
#yB
#yB
$�B
%�B
%�B
&WB
'�B
'�B
'^B
'^B
')B
'�B
'�B
(dB
)5B
)�B
)jB
)jB
)B
)B
(�B
)jB
(�B
(�B
)5B
(�B
)B
)�B
*<B
*<B
)�B
)�B
)�B
)�B
)jB
)B
)B
)5B
)�B
)�B
*B
*<B
*pB
*pB
*�B
*�B
*�B
+vB
,HB
,HB
,HB
,HB
,B
,B
,HB
,�B
-B
-B
,�B
-�B
.TB
-�B
.TB
/&B
.�B
/ZB
/�B
0�B
0�B
0�B
1gB
1�B
2B
1�B
1�B
1�B
2B
1�B
2B
28B
1�B
1�B
28B
1�B
1�B
3�B
4�B
4�B
4yB
4yB
4�B
4�B
5B
5KB
5KB
5B
5B
5�B
6QB
6B
7WB
7�B
8�B
8�B
8�B
8�B
9/B
9cB
9�B
9�B
9�B
9cB
9cB
;B
<AB
<�B
<vB
<�B
<�B
=HB
=�B
=�B
=|B
>B
>NB
?TB
?�B
?�B
?�B
?�B
B2B
A�B
CB
C�B
D�B
D�B
D�B
D�B
DsB
E�B
G�B
F�B
GB
HWB
G�B
H�B
H�B
H�B
H�B
H�B
I(B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
K5B
KiB
K�B
K�B
LB
K�B
LB
L;B
K�B
K�B
LB
L;B
L�B
L�B
MB
M�B
NB
OMB
O�B
O�B
PB
P�B
P�B
PSB
P�B
O�B
O�B
Q%B
Q�B
Q�B
QZB
Q%B
P�B
S1B
R�B
R�B
R�B
R�B
R+B
TB
U	B
UrB
UrB
U�B
U�B
UrB
U�B
U�B
V�B
VxB
U�B
U�B
WB
VxB
V�B
W~B
W~B
Y"B
X�B
Z(B
Z�B
Z\B
Z\B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[cB
[�B
\ B
\4B
\�B
\iB
\iB
\4B
\iB
\�B
]�B
]oB
]oB
]oB
]oB
]�B
]�B
]�B
]�B
]oB
^AB
^�B
^�B
^�B
^�B
_B
_GB
_B
_GB
_GB
_B
`�B
`MB
`�B
`MB
`�B
`MB
`�B
`�B
`�B
`�B
aB
aB
a�B
a�B
aSB
a�B
b%B
a�B
a�B
a�B
a�B
a�B
c+B
c_B
b�B
b�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
eB
e7B
e7B
elB
e�B
e7B
f	B
f	B
f	B
f=B
gB
f�B
gB
f�B
f�B
gB
gCB
gxB
g�B
g�B
g�B
gxB
g�B
g�B
h~B
h�B
h�B
iB
h�B
h�B
h�B
iB
iB
iPB
i�B
i�B
i�B
i�B
i�B
j!B
i�B
jVB
j!B
j�B
k\B
k�B
k\B
k�B
l.B
l�B
l�B
l�B
l�B
l�B
m4B
mhB
mhB
mhB
mhB
mhB
n:B
n:B
n�B
o@B
o�B
pB
o�B
p�B
p�B
q�B
q�B
q�B
rB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sYB
s�B
s�B
s�B
s�B
s�B
t_B
t_B
t�B
t�B
t�B
u1B
vB
vB
v�B
v�B
wB
w=B
w=B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xwB
yIB
y~B
y�B
y~B
y~B
y~B
y�B
zB
z�B
z�B
{!B
{UB
{!B
{�B
{UB
{�B
{�B
{�B
|'B
|\B
|�B
|�B
|�B
|�B
}-B
}�B
}�B
}�B
~3B
~3B
~hB
~hB
~�B
~�B
B
B
B
B
B
B
nB
�B
�tB
��B
��B
��B
�FB
�LB
�LB
�LB
�LB
��B
��B
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
�XB
��B
��B
��B
��B
��B
��B
��B
�eB
��B
��B
��B
�6B
�kB
�kB
��B
��B
�B
�B
�B
��B
�B
�B
�B
�B
�<B
�<B
�<B
�qB
�qB
��B
��B
��B
��B
�B
�wB
�wB
��B
�B
�IB
��B
��B
��B
�B
��B
�OB
��B
��B
�UB
�UB
��B
�UB
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
��B
��B
�aB
��B
��B
��B
��B
��B
��B
��B
��B
�9B
�nB
�nB
�B
�tB
��B
��B
��B
��B	�lB	ƳB	�B	�B	��B	ƳB	�QB	ǅB	��B	��B	�DB	��B	��B	�B	�B	�QB	��B	�B	�JB	�QB	ǹB	�"B	�JB	�B	ǹB	�"B	�B	�B	�yB	ǅB	�QB	��B	�B	�B	��B	�QB	ħB	�B	�B	�B	ȋB	�WB	ŭB	�JB	��B	��B	ƳB	ƳB	ƳB	�"B	��B	�B	�QB	�WB	��B	�(B	�B	ȋB	ǹB	��B	ŭB	�JB	ŭB	�JB	��B	�"B	��B	ǅB	�B	�"B	��B	ȋB	�QB	�JB	��B	��B	�WB	�QB	�B	��B	�"B	��B	�"B	�B	�B	�WB	��B	ǅB	��B	�QB	�WB	�(B	ǹB	�JB	�B	�]B	ǹB	ǹB	�(B	��B	��B	�]B	�]B	��B	��B	��B	�WB	ǹB	��B	�/B	�QB	ȋB	ǅB	��B	�B	��B	�5B	��B	ɑB	ŭB	�JB	�B	�;B	�&B	��B	˞B	ǹB	��B	��B	�yB	�WB	�B	�JB	�QB	ǅB	��B	�B	�DB	��B	��B	�QB	�(B	ǅB	�B	��B	�DB	ʗB	�WB	��B	�QB	��B	�B	ǅB	ǅB	�WB	ǹB	ƳB	ƳB	ƳB	ǅB	��B	ȋB	�"B	�QB	�B	�QB	ǅB	�"B	ȋB	ǅB	�JB	�B	ƳB	ǅB	�WB	�"B	ǹB	ƳB	�JB	�B	�"B	ȋB	ȋB	�QB	��B	�B	��B	ȋB	��B	ȋB	�B	�B	ǹB	ɑB	��B	ɑB	ɑB	�(B	��B	��B	ɑB	��B	�cB	ʗB	�(B	ȋB	��B	�]B	�/B	�cB	�B	�B	�B	ΰB	ςB	ЈB	�SB	��B	�MB	�B	ΰB	�B	�MB	�SB	�%B	ЈB	϶B	϶B	мB	��B	�%B	�SB	϶B	϶B	мB	�`B	ҔB	��B	��B	��B	�lB	�rB	�rB	�rB	�B	�1B	��B	��B	��B	�>B	ԠB	�8B	�1B	�1B	��B	��B	ԠB	�rB	��B	��B	��B	�"B	�\B	��B	�VB	�"B	�PB	�~B	׳B	�PB	عB	عB	׳B	׳B	��B	�xB	�PB	�"B	��B	��B	�DB	�rB	��B	юB	мB	϶B	�fB	�>B	�iB
$�B
	B	�!B

�B
OB
�B
pB
[B
�B
�B
�B
�B
zB
}B
EB
�B
5KB
#EB
$B
�B
HB
B
�B
6B
�B
<B
HB
�B
�B
*B
�B
�B
�B
tB
�B
�B	�IB
kB
B
�B
B
>B
=�B
&#B
EDB
3�B
0�B
.�B
28B
.TB
$�B
W�B
elB
N|B
MB
QZB
U�B
W�B
[cB
\�B
\�B
a�B
e�B
n�B
oB
ouB
ouB
k\B
y~B
k�B
l�B
k�B
n�B
elB
^�B
��B
�B
��B
�'B
�'B
��B
��B
�6B
��B
�OB
�B
�dB
~hB
�EB
|�B
{UB
deB
t�B
d�B
�ZB
�B
b�B
a�B
eB
h�B
gCB
a�B
s�B
z�B
s�B
f=B
z�B
m4B
~3B
�B
{�B
u�B
q�B
{!B
��B
~�B
w�B
nnB
h~B
c�B
g�B
c�B
c_B
`�B
aSB
bYB
b�B
`MB
\iB
Z�B
Z\B
Z\B
X�B
YVB
S�B
Q%B
XB
b%B
r�B
��B
i�B
GB
B�B
7#B
0,B
7�B
:B
+B
"sB
 �B
!mB
�B
.�B
#B
�B
�B
B
dB
�B
�B
�B
�B
4EB
L�B
7#B
K�B
)jB
+�B
%�B
�B
�B
�B
�B
BB
LB
�B
�B
�B
�B
�B
B
�B
XB
<B
�B
�B
�B	�nB	�bB
�B	�OB	��B	�B	�YB	�B	�1B	�$B	��B	�FB	�B	�=B	�B	�B	��B	�~B	�%B	�B	ܝB	��B	�"B	�%B	�B	�"B	֭B	ԠB	�8B	�B	�1B	�>B	ԠB	էB	�	B	�B	�4B	��B	��B	��B	��B	�B	˞B	ɑB	ȋB	�/B	�]B	�DB	�
B	�B	�
B	�B	��B	�"B	�2B	��B	��B	��B	�sB	��B	��B	�QB	ǅB	��B	�HB	�|B	��B	�%B	�BB	�B	��B	�tB	}�B	{�B	|�B	z�B	u1B	t_B	s�B	rSB	t�B	y~B	�B	vkB	v�B	{�B	y~B	n�B	f�B	c�B	a�B	c�B	l�B	e�B	b�B	]oB	^AB	Z\B	[.B	Z�B	\4B	\iB	[�B	gB	UrB	[cB	deB	c+B	TlB	d1B	V�B	Q�B	V�B	U�B	X�B	Y�B	Z�B	XB	[.B	XB	R+B	U�B	W~B	VxB	WB	\iB	[�B	Z�B	U>B	TlB	VDB	R�B	N�B	LoB	J�B	J�B	I]B	L�B	^AB	��B	X�B	Z\B	XPB	X�B	XPB	\iB	_{B	s�B	p{B	n�B	n�B	s�B	}�B	t�B	t�B	u1B	u�B	w�B	wqB	yB	zOB	z�B	z�B	{UB	nB	B	�eB	�B	~�B	�B	��B	�B	��B	�aB	��B	��B	��B	��B	��B	��B	��B	��B	�aB	�,B	��B	�9B	�BB	��B	��B	��B	�<B	�<B	�jB	�5B	�5B	��B	��B	��B	ȋB	�/B	юB	�JB	��B	�B	�{B	�B	�B	�B	ީB	��B	�B	�B	�lB	�CB	��B	�xB	��B	�B	�(B	�bB	�4B	�uB	�B	�{B	�B	��B	�~B	��B	�'B	��B	��B
 �B
�B
3B
B
�B
aB
*<B
7�B
<�B
?B
BfB
DsB
M�B
Q�B
TlB
U>B
U�B
XPB
`MB
l�B
B
�wB
�B
��B
�5B
�<B
�NB
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223255                            20230426223255AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622325520230426223255  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325520230426223255QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622325520230426223255QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               