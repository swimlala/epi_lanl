CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-30T02:01:03Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230730020103  20230730020103  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�>=����@�>=����11  @�>>8�@@�>>8�@@/�xB0�@/�xB0��d-�*\}�d-�*\}11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?k�@�\@E�@z�H@�  @��R@�  A   A�RA\)A,(�A?\)AaG�A���A�Q�A�  A�Q�A���AУ�A�  A�  B (�B(�B(�B  B�
B'�
B/�
B8  B?�
BG�
BP  BW�B_�Bh  Bo�Bw�B�B�B��B�  B��B��
B�  B�  B��B��B�  B�{B�{B�{B�{B�  B�{B�(�B��
B��
B��
B��B�  B��B��
B��
B��B�(�B�{B�{B�  B��B��C
=C  C  C
=C
  C��C  C
=C  C�C�C  C
=C  C��C 
=C"{C$
=C&
=C({C*
=C,  C.  C/�C1��C4
=C5��C8  C9��C;��C=�C?�CB  CD
=CE��CG��CI�CL  CN{CP
=CQ��CS��CV
=CX{CZ  C\  C^
=C`
=Cb  Cd  Cf  Ch{Cj  Ck�Cn  Cp{Cr{Ct
=Cv  Cx
=Cz  C|  C~
=C�C���C���C���C���C�  C�
=C�
=C���C�  C�C�C�  C�  C�  C���C���C���C�  C�  C���C���C�C�  C���C�  C�  C�  C�C�C�
=C�
=C�C�C�C�C���C���C�C�C�
=C�C�  C�  C�  C�C�C�C�
=C���C���C�
=C�
=C�  C�C���C���C�C�C���C���C�C�  C�  C�C�  C���C�  C�  C���C���C�  C�  C���C�C�  C���C�C�\C�\C�
=C�  C���C���C�C�  C���C���C���C���C���C�  C�  C���C���C���C���C�C�C�  C�C�C���C�  C�C���C���C���C�  C�  C���C���C�  C�C�C�C�  C�
=C�\C�
=C�  C�C�  C�C�
=C�C�C���C���D z�D ��D}qD�qD� DD��D  Dz�D�qDxRD�qD� D��D� D�D}qD��D	� D	��D
z�D�D� D��D�D�D}qD�qD�D�DxRD��D}qD  D��D�qDz�D�RDxRD�D�DD}qD�Dz�D�qD}qD�D� D�qD�D�qD��D�D��D  D}qD�qD� D�D� D  D}qD �D ��D ��D!��D"�D"}qD#  D#� D$�D$� D%  D%}qD%�qD&��D'�D'� D'�qD(� D(��D)}qD*�D*� D+�D+� D+��D,��D-D-��D-�qD.��D/D/}qD0�D0}qD0�qD1� D2�D2� D2�qD3��D4�D4�D4�qD5}qD6�D6��D6��D7z�D8�D8�D8�qD9}qD9�qD:� D;D;� D;��D<xRD<��D=�D>�D>�D>�qD?��D@D@��DA  DA� DBDB��DC  DC� DC�qDDxRDD�qDE� DF�DF��DG�DG}qDH  DH�DI  DI}qDI�qDJ}qDK�DK� DK�qDL��DM�DM}qDM�qDN�DODO��DP  DP}qDP�qDQ��DR�DR�DS  DSxRDS�qDT}qDU�DU��DV  DV}qDW  DW��DW�qDX}qDY  DY� DZ  DZ}qD[  D[}qD\  D\� D\�qD]}qD^�D^��D_  D_}qD`�D`��D`�qDa}qDb�Db� Dc  Dc� Dd  Dd��De�De�Df  Df��Df�qDg}qDh  Dh��Dh�qDi� Dj  Dj� Dk  Dk}qDk�qDl��Dm�Dm��Dn�Dn� Do  Do��Dp  Dp��Dq�Dq� Dr  Dr}qDs  Ds� Ds�qDt� Dt�qDu��Dv  Dv� Dw  Dwz�Dx  Dx��Dy  Dy��Dz�Dz� D{  D{� D|  D|� D}  D}}qD~  D~� D�D� D�  D�>�D��HD���D�HD�>�D��HD��HD���D�>�D�~�D���D���D�>�D�� D��HD�  D�@ D�~�D���D���D�@ D��HD�� D�  D�>�D�}qD�� D�  D�@ D�~�D�� D�HD�>�D�~�D�� D�HD�AHD��HD��HD�  D�@ D�}qD���D�HD�@ D�~�D���D�HD�@ D�}qD���D���D�=qD�� D��qD�  D�@ D�|)D��qD�HD�AHD�~�D���D��D�AHD�� D��HD�  D�@ D�}qD�� D�HD�B�D�� D�D���D�>�D��HD��HD�HD�@ D���D��HD��D�@ D��HD��HD�  D�AHD��HD�� D�  D�>�D�� D��HD�HD�AHD��HD��HD��D�AHD��HD�� D�HD�@ D�|)D�� D�HD�@ D�� D��HD�  D�AHD�~�D�D��qD�>�D�}qD��HD��D�=qD�� D�� D��)D�=qD�~�D���D���D�@ D�~�D�� D�HD�>�D�~�D���D��D�>�D���D���D�  D�AHD�� D��HD�  D�@ D���D�� D���D�@ D��HD��HD��)D�<)D�|)D���D�  D�AHD�~�D���D��qD�@ D��HD��HD���D�=qD�}qD���D���D�>�D�~�D�� D�  D�@ D��HD���D�  D�C�D��HD�� D�  D�AHD��HD�� D�  D�>�D�~�D��HD�  D�>�D�~�D��)D��qD�@ D��HD�D�HD�@ D�}qD��)D��qD�@ D�~�D��)D���D�@ D���D�D�HD�AHD�� D�� D�  D�B�D��HD�� D���D�@ D���D���D�HD�B�D��HD���D��qD�>�D��HD�D��D�AHD�~�D��qD�  D�AHD�~�D�� D�HD�B�D��HD��qD��qD�>�D�� D��HD�HD�B�D���D�D��D�AHD�~�D���D���D�@ D�� D��qD�HD�@ D�� D��HD�HD�AHD��HD��HD�  D�AHDHD�� D���D�AHDÁHD�D��D�@ DĀ D�D��D�AHDŁHDŽqD�  D�@ DƁHD�� D�  D�B�DǁHD�� D�  D�>�D�~�D��HD��qD�>�D�~�D�� D�  D�@ Dʀ D��HD�HD�AHDˁHD�� D���D�AHD̂�D��HD�  D�>�D̀ D;�D���D�=qD�|)Dξ�D�  D�AHDπ DϽqD��qD�>�D�}qDнqD�HD�AHDсHD��HD�  D�AHDҁHD���D�HD�@ Dӂ�D�� D�  D�@ DԀ DԽqD�  D�>�D�}qD�� D��qD�@ DցHD��HD�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�B�Dق�D��HD��D�AHDڀ DڽqD��qD�AHDہHD�� D��qD�>�D܀ Dܾ�D��qD�@ D݁HD�� D�  D�B�DށHD�� D�HD�AHD߁HD�� D�  D�@ D�� D�� D�HD�B�DႏD�� D�HD�AHD�~�D�D���D�>�D� D�� D�HD�B�D�HD侸D���D�AHD�}qD�qD���D�@ D�HD��HD���D�B�D� D�)D��qD�@ D肏D��HD��qD�>�D�~�D��HD���D�@ D� D��HD�  D�>�D�~�D�� D��qD�AHD�}qD��HD���D�>�D�~�D���D�  D�>�D� D�D��D�>�D�~�D��HD��D�AHD��HD���D�HD�B�D� D�D���D�@ D�~�D�� D���D�>�D�~�D��HD�  D�B�D� D��qD�HD�AHD�� D�� D�  D�B�D�� D�� D���D�@ D��HD�D�  D�@ D�� D�� D�HD�@ D�� D��HD�HD�=qD�aH>�?#�
?u?���?�(�@�@�R@:�H@Y��@p��@��@�z�@��\@�{@���@Ǯ@�z�@�  @���@���A�
A
=qAG�AQ�A{A%A-p�A3�
A:=qAAG�AG�AN�RAU�AZ�HAb�\Ah��An{As�
Aw�A}p�A�G�A��
A��RA���A��HA��A�
=A���A�33A��A�\)A���A��HA���A�
=A���A��HA���A�
=A���A��HA���A�ffA�  A��A��A�A��A���A��
A�A��A���AÅA�AǮA��A���A�
=A���A�33A�p�A�\)A���Aڏ\A�z�A�ffA��A��HA��A�
=A���A�33A�p�A�  A�\A�z�A��RA���A��HA���A�\)B ��BB�HB�
B��B�B
=B(�B	G�B
=qB33B(�Bp�BffB�Bz�BB�RB�
B��BB�HB(�B�B{B33B(�BG�BffB\)B Q�B!p�B"�\B#�B$��B%��B&�HB'�
B(��B)�B*�HB,  B-G�B.=qB/\)B0Q�B1p�B2�\B3�B4��B5�B6�HB8  B9�B:=qB;\)B<z�B=��B>�RB?�
B@��BA�BC
=BD(�BEG�BFffBG�BH��BIBJ�RBK�
BL��BMBN�HBP  BQ�BR=qBS\)BTz�BU��BV�RBW�
BX��BY�B[
=B\(�B]G�B^ffB_�B`��BaBc
=BdQ�BeG�Bf�\Bg�Bh��BiBj�RBk�Blz�BmBn�RBo�
Bp��Br{Br�HBt(�BuG�Bv�\Bw�Bx��By�B{
=B|  B}�B~=qB33B�{B��\B�
=B���B�{B���B�33B��B�=qB��RB�\)B�  B��\B�
=B���B�(�B���B�G�B��B�{B���B��B���B�=qB��RB�33B�B�Q�B���B�\)B��
B�ffB��B���B�(�B���B�33B��B�{B�z�B�
=B���B�{B��\B��B��
B�Q�B��HB�\)B��
B�ffB���B�p�B��B�Q�B���B�33B�B�=qB���B�33B��
B�z�B�
=B��B�{B���B�
=B�p�B��B�ffB���B��B�=qB��RB�G�B�B�ffB��HB�p�B��
B�Q�B��HB�p�B��B��\B�33B�B�Q�B��HB�p�B��
B�=qB���B�\)B��
B�z�B�33B�B�Q�B���B�G�B��
B�ffB�33B�B�Q�B���B��B��B�ffB�
=B���B�=qB�
=B���B�=qB���B�G�B��
B�ffB�33B��B\B�33B�B�=qB���B�p�B�  Bƣ�B�33B�  B���B�p�B�(�Bʣ�B�33B�B�ffB�
=B͙�B�z�B�33B��
B�z�B��Bљ�B�=qB��HB�p�B�Q�B�
=BծB�Q�B���B�\)B�{Bأ�BمB�=qB��HB�\)B�  B܏\B�\)B�(�B���B�\)B��B��\B�33B�{B�RB�p�B��B�\B��B�  B�RB�p�B��B�z�B�33B��B�\B�G�B�  B�RB��B��
B�z�B�\)B�{B�z�B��B�B��B�\)B�{B�\B��B�B�z�B�\)B��B���B�
=B��B�Q�B��B��
B�z�B�
=B��B�=qB�33B��C 33C �C �
CG�C��C  C=qC�\C��C\)C�C��CG�C�RC�Cp�C��C
=CffC�
C33C�C��C33C��C	  C	=qC	�\C
{C
p�C
�RC
=CffC��C33Cp�C��C�C��C��C33C�\C  CQ�C�RC��CffC��C
=CffCC=qC��C�
C33C�C
=C\)C�C{C�C�
C(�C�C��CG�C��C{Cp�C�C
=C�C�C(�Cz�C  C\)C��C��Cp�C�RC
=C\)C�HC=qCz�C�
CG�C��C�C =qC �C ��C!G�C!C"�C"ffC"�RC#33C#�\C#��C$(�C$��C$��C%G�C%��C&{C&Q�C&�C'(�C'z�C'��C(Q�C(�C(��C)ffC)��C*{C*z�C*�C+33C+��C,
=C,Q�C,�RC-�C-ffC-�
C.=qC.z�C.�C/Q�C/��C0  C0z�C0�RC1{C1�\C1�
C2=qC2�C2��C3ffC3��C4
=C4�C4�C5(�C5�\C6
=C6G�C6��C7�C7p�C7C8=qC8�C8�HC9\)C9��C:
=C:z�C:C;=qC;��C;�HC<\)C<�RC=
=C=z�C=�HC>(�C>��C?  C?G�C?C@{C@p�C@�CA33CA��CB�CB\)CB�RCC=qCCz�CC��CDffCD��CE(�CEz�CE��CFG�CF��CG  CGz�CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                     ?k�@�\@E�@z�H@�  @��R@�  A   A�RA\)A,(�A?\)AaG�A���A�Q�A�  A�Q�A���AУ�A�  A�  B (�B(�B(�B  B�
B'�
B/�
B8  B?�
BG�
BP  BW�B_�Bh  Bo�Bw�B�B�B��B�  B��B��
B�  B�  B��B��B�  B�{B�{B�{B�{B�  B�{B�(�B��
B��
B��
B��B�  B��B��
B��
B��B�(�B�{B�{B�  B��B��C
=C  C  C
=C
  C��C  C
=C  C�C�C  C
=C  C��C 
=C"{C$
=C&
=C({C*
=C,  C.  C/�C1��C4
=C5��C8  C9��C;��C=�C?�CB  CD
=CE��CG��CI�CL  CN{CP
=CQ��CS��CV
=CX{CZ  C\  C^
=C`
=Cb  Cd  Cf  Ch{Cj  Ck�Cn  Cp{Cr{Ct
=Cv  Cx
=Cz  C|  C~
=C�C���C���C���C���C�  C�
=C�
=C���C�  C�C�C�  C�  C�  C���C���C���C�  C�  C���C���C�C�  C���C�  C�  C�  C�C�C�
=C�
=C�C�C�C�C���C���C�C�C�
=C�C�  C�  C�  C�C�C�C�
=C���C���C�
=C�
=C�  C�C���C���C�C�C���C���C�C�  C�  C�C�  C���C�  C�  C���C���C�  C�  C���C�C�  C���C�C�\C�\C�
=C�  C���C���C�C�  C���C���C���C���C���C�  C�  C���C���C���C���C�C�C�  C�C�C���C�  C�C���C���C���C�  C�  C���C���C�  C�C�C�C�  C�
=C�\C�
=C�  C�C�  C�C�
=C�C�C���C���D z�D ��D}qD�qD� DD��D  Dz�D�qDxRD�qD� D��D� D�D}qD��D	� D	��D
z�D�D� D��D�D�D}qD�qD�D�DxRD��D}qD  D��D�qDz�D�RDxRD�D�DD}qD�Dz�D�qD}qD�D� D�qD�D�qD��D�D��D  D}qD�qD� D�D� D  D}qD �D ��D ��D!��D"�D"}qD#  D#� D$�D$� D%  D%}qD%�qD&��D'�D'� D'�qD(� D(��D)}qD*�D*� D+�D+� D+��D,��D-D-��D-�qD.��D/D/}qD0�D0}qD0�qD1� D2�D2� D2�qD3��D4�D4�D4�qD5}qD6�D6��D6��D7z�D8�D8�D8�qD9}qD9�qD:� D;D;� D;��D<xRD<��D=�D>�D>�D>�qD?��D@D@��DA  DA� DBDB��DC  DC� DC�qDDxRDD�qDE� DF�DF��DG�DG}qDH  DH�DI  DI}qDI�qDJ}qDK�DK� DK�qDL��DM�DM}qDM�qDN�DODO��DP  DP}qDP�qDQ��DR�DR�DS  DSxRDS�qDT}qDU�DU��DV  DV}qDW  DW��DW�qDX}qDY  DY� DZ  DZ}qD[  D[}qD\  D\� D\�qD]}qD^�D^��D_  D_}qD`�D`��D`�qDa}qDb�Db� Dc  Dc� Dd  Dd��De�De�Df  Df��Df�qDg}qDh  Dh��Dh�qDi� Dj  Dj� Dk  Dk}qDk�qDl��Dm�Dm��Dn�Dn� Do  Do��Dp  Dp��Dq�Dq� Dr  Dr}qDs  Ds� Ds�qDt� Dt�qDu��Dv  Dv� Dw  Dwz�Dx  Dx��Dy  Dy��Dz�Dz� D{  D{� D|  D|� D}  D}}qD~  D~� D�D� D�  D�>�D��HD���D�HD�>�D��HD��HD���D�>�D�~�D���D���D�>�D�� D��HD�  D�@ D�~�D���D���D�@ D��HD�� D�  D�>�D�}qD�� D�  D�@ D�~�D�� D�HD�>�D�~�D�� D�HD�AHD��HD��HD�  D�@ D�}qD���D�HD�@ D�~�D���D�HD�@ D�}qD���D���D�=qD�� D��qD�  D�@ D�|)D��qD�HD�AHD�~�D���D��D�AHD�� D��HD�  D�@ D�}qD�� D�HD�B�D�� D�D���D�>�D��HD��HD�HD�@ D���D��HD��D�@ D��HD��HD�  D�AHD��HD�� D�  D�>�D�� D��HD�HD�AHD��HD��HD��D�AHD��HD�� D�HD�@ D�|)D�� D�HD�@ D�� D��HD�  D�AHD�~�D�D��qD�>�D�}qD��HD��D�=qD�� D�� D��)D�=qD�~�D���D���D�@ D�~�D�� D�HD�>�D�~�D���D��D�>�D���D���D�  D�AHD�� D��HD�  D�@ D���D�� D���D�@ D��HD��HD��)D�<)D�|)D���D�  D�AHD�~�D���D��qD�@ D��HD��HD���D�=qD�}qD���D���D�>�D�~�D�� D�  D�@ D��HD���D�  D�C�D��HD�� D�  D�AHD��HD�� D�  D�>�D�~�D��HD�  D�>�D�~�D��)D��qD�@ D��HD�D�HD�@ D�}qD��)D��qD�@ D�~�D��)D���D�@ D���D�D�HD�AHD�� D�� D�  D�B�D��HD�� D���D�@ D���D���D�HD�B�D��HD���D��qD�>�D��HD�D��D�AHD�~�D��qD�  D�AHD�~�D�� D�HD�B�D��HD��qD��qD�>�D�� D��HD�HD�B�D���D�D��D�AHD�~�D���D���D�@ D�� D��qD�HD�@ D�� D��HD�HD�AHD��HD��HD�  D�AHDHD�� D���D�AHDÁHD�D��D�@ DĀ D�D��D�AHDŁHDŽqD�  D�@ DƁHD�� D�  D�B�DǁHD�� D�  D�>�D�~�D��HD��qD�>�D�~�D�� D�  D�@ Dʀ D��HD�HD�AHDˁHD�� D���D�AHD̂�D��HD�  D�>�D̀ D;�D���D�=qD�|)Dξ�D�  D�AHDπ DϽqD��qD�>�D�}qDнqD�HD�AHDсHD��HD�  D�AHDҁHD���D�HD�@ Dӂ�D�� D�  D�@ DԀ DԽqD�  D�>�D�}qD�� D��qD�@ DցHD��HD�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�B�Dق�D��HD��D�AHDڀ DڽqD��qD�AHDہHD�� D��qD�>�D܀ Dܾ�D��qD�@ D݁HD�� D�  D�B�DށHD�� D�HD�AHD߁HD�� D�  D�@ D�� D�� D�HD�B�DႏD�� D�HD�AHD�~�D�D���D�>�D� D�� D�HD�B�D�HD侸D���D�AHD�}qD�qD���D�@ D�HD��HD���D�B�D� D�)D��qD�@ D肏D��HD��qD�>�D�~�D��HD���D�@ D� D��HD�  D�>�D�~�D�� D��qD�AHD�}qD��HD���D�>�D�~�D���D�  D�>�D� D�D��D�>�D�~�D��HD��D�AHD��HD���D�HD�B�D� D�D���D�@ D�~�D�� D���D�>�D�~�D��HD�  D�B�D� D��qD�HD�AHD�� D�� D�  D�B�D�� D�� D���D�@ D��HD�D�  D�@ D�� D�� D�HD�@ D�� D��HD�HD�=qD�aH>�?#�
?u?���?�(�@�@�R@:�H@Y��@p��@��@�z�@��\@�{@���@Ǯ@�z�@�  @���@���A�
A
=qAG�AQ�A{A%A-p�A3�
A:=qAAG�AG�AN�RAU�AZ�HAb�\Ah��An{As�
Aw�A}p�A�G�A��
A��RA���A��HA��A�
=A���A�33A��A�\)A���A��HA���A�
=A���A��HA���A�
=A���A��HA���A�ffA�  A��A��A�A��A���A��
A�A��A���AÅA�AǮA��A���A�
=A���A�33A�p�A�\)A���Aڏ\A�z�A�ffA��A��HA��A�
=A���A�33A�p�A�  A�\A�z�A��RA���A��HA���A�\)B ��BB�HB�
B��B�B
=B(�B	G�B
=qB33B(�Bp�BffB�Bz�BB�RB�
B��BB�HB(�B�B{B33B(�BG�BffB\)B Q�B!p�B"�\B#�B$��B%��B&�HB'�
B(��B)�B*�HB,  B-G�B.=qB/\)B0Q�B1p�B2�\B3�B4��B5�B6�HB8  B9�B:=qB;\)B<z�B=��B>�RB?�
B@��BA�BC
=BD(�BEG�BFffBG�BH��BIBJ�RBK�
BL��BMBN�HBP  BQ�BR=qBS\)BTz�BU��BV�RBW�
BX��BY�B[
=B\(�B]G�B^ffB_�B`��BaBc
=BdQ�BeG�Bf�\Bg�Bh��BiBj�RBk�Blz�BmBn�RBo�
Bp��Br{Br�HBt(�BuG�Bv�\Bw�Bx��By�B{
=B|  B}�B~=qB33B�{B��\B�
=B���B�{B���B�33B��B�=qB��RB�\)B�  B��\B�
=B���B�(�B���B�G�B��B�{B���B��B���B�=qB��RB�33B�B�Q�B���B�\)B��
B�ffB��B���B�(�B���B�33B��B�{B�z�B�
=B���B�{B��\B��B��
B�Q�B��HB�\)B��
B�ffB���B�p�B��B�Q�B���B�33B�B�=qB���B�33B��
B�z�B�
=B��B�{B���B�
=B�p�B��B�ffB���B��B�=qB��RB�G�B�B�ffB��HB�p�B��
B�Q�B��HB�p�B��B��\B�33B�B�Q�B��HB�p�B��
B�=qB���B�\)B��
B�z�B�33B�B�Q�B���B�G�B��
B�ffB�33B�B�Q�B���B��B��B�ffB�
=B���B�=qB�
=B���B�=qB���B�G�B��
B�ffB�33B��B\B�33B�B�=qB���B�p�B�  Bƣ�B�33B�  B���B�p�B�(�Bʣ�B�33B�B�ffB�
=B͙�B�z�B�33B��
B�z�B��Bљ�B�=qB��HB�p�B�Q�B�
=BծB�Q�B���B�\)B�{Bأ�BمB�=qB��HB�\)B�  B܏\B�\)B�(�B���B�\)B��B��\B�33B�{B�RB�p�B��B�\B��B�  B�RB�p�B��B�z�B�33B��B�\B�G�B�  B�RB��B��
B�z�B�\)B�{B�z�B��B�B��B�\)B�{B�\B��B�B�z�B�\)B��B���B�
=B��B�Q�B��B��
B�z�B�
=B��B�=qB�33B��C 33C �C �
CG�C��C  C=qC�\C��C\)C�C��CG�C�RC�Cp�C��C
=CffC�
C33C�C��C33C��C	  C	=qC	�\C
{C
p�C
�RC
=CffC��C33Cp�C��C�C��C��C33C�\C  CQ�C�RC��CffC��C
=CffCC=qC��C�
C33C�C
=C\)C�C{C�C�
C(�C�C��CG�C��C{Cp�C�C
=C�C�C(�Cz�C  C\)C��C��Cp�C�RC
=C\)C�HC=qCz�C�
CG�C��C�C =qC �C ��C!G�C!C"�C"ffC"�RC#33C#�\C#��C$(�C$��C$��C%G�C%��C&{C&Q�C&�C'(�C'z�C'��C(Q�C(�C(��C)ffC)��C*{C*z�C*�C+33C+��C,
=C,Q�C,�RC-�C-ffC-�
C.=qC.z�C.�C/Q�C/��C0  C0z�C0�RC1{C1�\C1�
C2=qC2�C2��C3ffC3��C4
=C4�C4�C5(�C5�\C6
=C6G�C6��C7�C7p�C7C8=qC8�C8�HC9\)C9��C:
=C:z�C:C;=qC;��C;�HC<\)C<�RC=
=C=z�C=�HC>(�C>��C?  C?G�C?C@{C@p�C@�CA33CA��CB�CB\)CB�RCC=qCCz�CC��CDffCD��CE(�CEz�CE��CFG�CF��CG  CGz�CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��/A�ƨA���A�A�AּjA��
A��#A־wA֣�A֮A֩�A֗�A֏\A�l�A�ffA�`BA�`BA�`BA�`BA�ZA�XA�XA�XA�XA�XA�Q�A�K�A�M�A�M�A�K�A�K�A�M�A�M�A�E�A�E�A�=qA�5?A�7LA�9XA�9XA�"�A�JA��mA��/AӃA�A�AϑhA��A���A�z�A�VA��#A���A��A��A��AɋDA�A�5?A�ffA��yA���A�7LAŁAć+A��A�I�A�n�A�`BA�bA���A���A�ffA���A��TA��A�G�A���A���A�r�A���A�ƨA�hsA�XA�A�A���A�  A���A���A�VA�bNA���A���A��;A�&�A�&�A�n�A�?}A�z�A�A�^5A���A��A���A���A��jA��A�jA�l�A�(�A��A�bNA�7LA��A�7LA~�A{ƨAx1Au"�Ap�An��Aj��Af1A`ZA]"�AYdZAT�jAQ7LAN �AK�FAIAF��ADQ�AC�-AA�TA?�A=��A<5?A9"�A6��A5x�A3C�A1p�A/t�A.�A,��A+�FA)A'�FA&��A&z�A%�#A#�A"bNA!dZA v�A��A��AVA�AVA�jA�DA�/A�#A�At�A�TA;dAĜA5?A��AC�Ax�AO�AA�RA�TA7LA�\A5?A�;A
��A	�TA	S�A	A��A��A��AZA5?A{A�mA�A33A=qA�A�-A�AAXA ��A j@��y@�E�@���@�G�@�Z@�"�@�^5@��-@��/@�;d@�M�@��`@�A�@��
@��y@�ff@���@�G�@��@�u@���@�I�@@�V@�Ĝ@�j@��m@�\)@��@�v�@�x�@�u@�1@�dZ@��@�+@�M�@�J@��@�h@�X@�7L@��@�9@�A�@���@㕁@�t�@�t�@�@�Z@��@�h@�K�@���@��@�7L@�?}@�`B@�X@��@�l�@�V@���@�p�@��/@�b@�+@�n�@���@�@�&�@ؓu@�  @׮@׍P@�t�@ׅ@ו�@�\)@�o@���@�V@��@�33@���@�ȴ@�^5@Ѻ^@��`@�b@�K�@��@ΰ!@Η�@�E�@�p�@�V@��`@̬@�b@�1@�C�@��y@ʗ�@ʇ+@�ff@�M�@�$�@�O�@�z�@��m@��
@ǅ@��y@Ɨ�@��@�?}@���@�1'@�  @�
=@���@�v�@�5?@�J@��@��T@��-@�hs@�O�@�O�@���@���@���@���@��@���@�@�~�@�V@�J@��^@�hs@�?}@��@�I�@�  @�|�@��@���@�=q@�{@��T@��7@��@�bN@�dZ@�"�@�
=@��R@�{@��@��@��@���@���@�Q�@���@�\)@��+@�@�?}@�A�@��w@�S�@��y@��!@��\@�=q@��@��T@���@�O�@�Q�@�|�@��@�ȴ@�v�@�n�@�ff@�J@�p�@�j@�  @��m@���@��@�;d@�@���@��+@�@���@��@�A�@�1'@�(�@���@�K�@�n�@�-@���@�`B@��@��j@��D@��@�Z@� �@��
@�l�@�"�@�o@��H@��!@��\@�V@�V@�$�@���@�x�@�hs@�`B@�hs@�p�@�hs@�V@�9X@���@�
=@��+@�J@�@�?}@��@��`@�Ĝ@���@�r�@�Q�@�b@��@�|�@�dZ@�dZ@�;d@��@��@�@���@��R@���@�5?@�@��h@�7L@���@���@�Z@�1'@�(�@� �@��@��F@���@�l�@�@���@��\@�ff@��@��@���@���@�x�@�`B@�&�@��@�r�@�1@�ƨ@���@��@�\)@�C�@�@���@�n�@��@���@�hs@�7L@�Ĝ@�Ĝ@���@��9@�1@�ƨ@���@�\)@�o@��R@�v�@��@�@���@��^@��@�`B@�X@�7L@���@��`@��9@�9X@��@�  @��w@��@�dZ@�\)@���@�~�@�5?@��T@��h@�p�@�X@�?}@�?}@�/@��@���@��/@���@��9@�bN@�(�@��m@���@�l�@�C�@��y@���@��!@��+@�M�@�$�@���@�p�@�/@��@��`@��u@�Z@� �@�b@�  @��@�P@+@~�+@~5?@}�T@}�@|�D@{�m@{��@{ƨ@{C�@{@z~�@y�^@y&�@x��@xbN@w�w@v�@vff@v5?@u@u��@u?}@t�/@tz�@t9X@t1@sdZ@r��@rn�@q��@q%@p��@o�@o�w@o�@n��@n��@n�@n��@nV@m@m?}@l�j@l�D@lz�@l9X@k��@k��@kdZ@ko@j��@j��@j~�@jn�@jn�@j-@i��@i��@ihs@hr�@hb@h  @g�@g\)@g\)@gK�@f��@fv�@f5?@e�T@eO�@e?}@e/@d�D@d�@c�m@c��@c�@cdZ@cdZ@b��@b�\@b-@a�#@a�7@aX@`Ĝ@`A�@`b@_�@_�@^��@^v�@^E�@]�T@]��@]p�@]/@]�@\�@\�j@\j@\I�@\Z@\9X@[ƨ@[��@[S�@Z��@Y�@Y�7@Y&�@Y%@X�`@Xr�@W�@W�w@W|�@W\)@V�y@VE�@V{@U�-@U/@Tj@T(�@St�@R�@R�\@Q��@Q�#@Q�7@Q�@P�u@Pr�@Pr�@PbN@PbN@PQ�@O�@O�@O��@O��@O|�@O;d@Nȴ@N�+@N@M��@M@M��@M��@Mp�@M�@L�j@LI�@K�@K@J�@J�@J�H@J�H@J�!@I��@I7L@I�@H�9@HQ�@G�;@G��@G|�@G|�@Gl�@F��@F@E�@D��@Dj@D�@C��@C�m@C��@B�@Bn�@A��@A��@AG�@@��@@�9@@�u@?�@?�P@?\)@?
=@>ȴ@>��@>$�@=�h@<j@;33@:��@:��@:�!@:��@:^5@9�7@8��@8�u@8�@8r�@8bN@8Q�@81'@7�@7;d@6�@6ff@6@5`B@4j@4j@4z�@4Z@3�
@3�@3S�@3@2�!@2��@2M�@2n�@2�@1�@0�`@0�`@0�@/��@/l�@/l�@/+@.��@.V@-�-@-`B@-�@,��@,�D@+�
@+33@*~�@*=q@*�@*�@*J@*J@)��@)��@)��@)�@)�@)�#@)�#@)�#@)�@)�#@)��@)�^@)��@)hs@)G�@(��@(  @'�@'��@'l�@&��@&ȴ@&v�@&5?@%��@%/@$��@$I�@#�m@#�F@#�@#dZ@#S�@#C�@#"�@"�@"�@"��@"�!@"��@"n�@"�@!��@ Ĝ@ 1'@ b@�@�w@l�@+@��@�y@�@�@��@��@ff@V@5?@{@�@�T@��@@��@�h@�@p�@p�@`B@O�@?}@?}@/@/@��@�j@9X@ƨ@dZ@��@�\@^5@M�@M�@M�@=q@J@X@&�@�@%@��@Ĝ@Q�@�;@�w@+@v�@{@�T@@��@�h@�h@p�@?}@O�@O�@`B@O�@O�@?}@/@�@V@�@�@�/@�/@��@��@�j@�@j@�m@�F@33@��@n�@M�@=q@��@�^@x�@x�@hs@7L@��@�`@��@�9@�u@�@r�@Q�@A�@b@  @  @�@�@�@�;@�w@|�@K�@�@
=@�y@�@�@ȴ@ȴ@ȴ@�R@��@�+@v�@V@5?@@p�@�@V@��A���A��A��#A��A��`A��HA��
A���A�A־wA־wA־wA�ƨAּjA���A���Aֺ^AּjAֺ^A��A��`A��TA��
A�ȴA�ĜAֲ-A֣�A֬Aֲ-A֮Aֲ-Aְ!Aְ!A֮A֩�A֥�A֥�A֣�A֣�A֙�A֋DA֓uA֗�A֕�A֕�A֓uA֓uA֏\A֑hA֕�A�x�A�t�A�n�A�l�A�l�A�jA�hsA�dZA�bNA�bNA�dZA�bNA�hsA�hsA�hsA�ffA�dZA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�`BA�\)A�\)A�\)A�\)A�^5A�^5A�`BA�bNA�bNA�`BA�bNA�bNA�bNA�bNA�bNA�`BA�`BA�`BA�`BA�`BA�^5A�ZA�XA�ZA�XA�XA�XA�XA�XA�XA�XA�XA�XA�ZA�ZA�XA�ZA�XA�XA�VA�XA�VA�XA�ZA�XA�XA�XA�VA�VA�VA�VA�XA�VA�VA�S�A�VA�S�A�S�A�S�A�VA�S�A�VA�S�A�S�A�VA�S�A�VA�Q�A�O�A�K�A�M�A�M�A�K�A�I�A�I�A�I�A�I�A�I�A�K�A�I�A�K�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�K�A�K�A�M�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�K�A�I�A�K�A�K�A�I�A�G�A�G�A�G�A�E�A�G�A�E�A�G�A�G�A�E�A�E�A�E�A�C�A�C�A�A�A�;dA�=qA�?}A�?}A�=qA�7LA�9XA�7LA�5?A�7LA�7LA�7LA�9XA�9XA�7LA�5?A�5?A�5?A�33A�5?A�5?A�5?A�5?A�9XA�7LA�7LA�9XA�9XA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�9XA�5?A�1'A�/A�-A�+A��A��A�bA�oA�bA�bA�VA�JA�
=A�
=A�1A�1A�%A�A���A���A��yA�ĜA�AվwAՍPA�I�A��A��AԃA�bNA�XA�-A���AӰ!Aӕ�A�l�A�G�A�9XA�1'A�+A�JA��`AҶFAҋDA�
=A���A�\)AϼjAϥ�Aϙ�AϏ\AρA�t�A�p�A�hsA�K�A�7LA�33A�"�A�JA���A���A���AΟ�A�r�A�AͅA��A���A̬ÁA�t�A�n�A�hsA�hsA�ffA�ffA�`BA�\)A�VA�M�A�E�A�7LA�+A�oA��A�ƨA˟�A�r�A�5?A���A��mA���Aʰ!A�|�A�O�A��A���A�  A�JA�+A�S�A�M�A�"�A�{A�JA���A��A��A���A��A��A��`A��#A���A�Aɺ^A�l�A�C�A�9XA�1'A�$�A�bA���A��A���A�A�-A�5?A�C�A�M�A�E�A�
=Aȧ�A�v�A�A�A�&�A��A�oA�%A�  A��A��
AǼjA�|�A�XA��;Aƣ�AƋDAƃAƁA�|�A�hsA�1'A��A�A��AŴ9AœuA�z�A�t�A�jA�5?A�%A�A�r�A�Q�A�5?A�&�A�A���A��A��A��TA��Aã�A�`BA�\)A�C�A��A®A�M�A�JA���A��A���A�Q�A� �A���A�`BA�VA���A���A�p�A�ffA�K�A�
=A�dZA��DA�VA���A��PA�C�A���A��
A���A���A��\A���A��PA��+A�|�A�dZA�Q�A�E�A�33A�$�A�JA���A��;A��FA� �A�I�A��FA�S�A�1'A�A��A��
A�~�A�/A��A�VA���A�dZA�33A���A�p�A�;dA�A��RA��7A�(�A���A��A���A�A�A��9A���A���A�~�A�O�A�;dA�33A�"�A���A��A��A�K�A�$�A��TA���A��A���A��!A���A�~�A�bNA�9XA�%A��^A��DA�bNA�=qA���A��A��#A���A�O�A��TA��FA�hsA�A��TA�VA�1'A��A��`A�`BA�%A���A��A�v�A�A�A��TA���A�v�A�O�A�+A��A���A���A���A�\)A�&�A�JA��A�ĜA��A�hsA�`BA�ZA�\)A�VA�-A��A��A�$�A�oA�1A�  A���A��A��HA��HA��HA��HA��`A��TA�ȴA��uA�K�A��A�A��-A��PA�n�A�K�A�"�A���A�\)A�+A�p�A�C�A�bA�r�A��^A�\)A�&�A���A��TA�ƨA��!A��DA�^5A�VA��jA�|�A��A���A��-A�$�A��hA�G�A�-A�%A���A���A���A��A��-A��!A�9XA���A��A�^5A�33A���A��A�I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                     A���A��/A�ƨA���A�A�AּjA��
A��#A־wA֣�A֮A֩�A֗�A֏\A�l�A�ffA�`BA�`BA�`BA�`BA�ZA�XA�XA�XA�XA�XA�Q�A�K�A�M�A�M�A�K�A�K�A�M�A�M�A�E�A�E�A�=qA�5?A�7LA�9XA�9XA�"�A�JA��mA��/AӃA�A�AϑhA��A���A�z�A�VA��#A���A��A��A��AɋDA�A�5?A�ffA��yA���A�7LAŁAć+A��A�I�A�n�A�`BA�bA���A���A�ffA���A��TA��A�G�A���A���A�r�A���A�ƨA�hsA�XA�A�A���A�  A���A���A�VA�bNA���A���A��;A�&�A�&�A�n�A�?}A�z�A�A�^5A���A��A���A���A��jA��A�jA�l�A�(�A��A�bNA�7LA��A�7LA~�A{ƨAx1Au"�Ap�An��Aj��Af1A`ZA]"�AYdZAT�jAQ7LAN �AK�FAIAF��ADQ�AC�-AA�TA?�A=��A<5?A9"�A6��A5x�A3C�A1p�A/t�A.�A,��A+�FA)A'�FA&��A&z�A%�#A#�A"bNA!dZA v�A��A��AVA�AVA�jA�DA�/A�#A�At�A�TA;dAĜA5?A��AC�Ax�AO�AA�RA�TA7LA�\A5?A�;A
��A	�TA	S�A	A��A��A��AZA5?A{A�mA�A33A=qA�A�-A�AAXA ��A j@��y@�E�@���@�G�@�Z@�"�@�^5@��-@��/@�;d@�M�@��`@�A�@��
@��y@�ff@���@�G�@��@�u@���@�I�@@�V@�Ĝ@�j@��m@�\)@��@�v�@�x�@�u@�1@�dZ@��@�+@�M�@�J@��@�h@�X@�7L@��@�9@�A�@���@㕁@�t�@�t�@�@�Z@��@�h@�K�@���@��@�7L@�?}@�`B@�X@��@�l�@�V@���@�p�@��/@�b@�+@�n�@���@�@�&�@ؓu@�  @׮@׍P@�t�@ׅ@ו�@�\)@�o@���@�V@��@�33@���@�ȴ@�^5@Ѻ^@��`@�b@�K�@��@ΰ!@Η�@�E�@�p�@�V@��`@̬@�b@�1@�C�@��y@ʗ�@ʇ+@�ff@�M�@�$�@�O�@�z�@��m@��
@ǅ@��y@Ɨ�@��@�?}@���@�1'@�  @�
=@���@�v�@�5?@�J@��@��T@��-@�hs@�O�@�O�@���@���@���@���@��@���@�@�~�@�V@�J@��^@�hs@�?}@��@�I�@�  @�|�@��@���@�=q@�{@��T@��7@��@�bN@�dZ@�"�@�
=@��R@�{@��@��@��@���@���@�Q�@���@�\)@��+@�@�?}@�A�@��w@�S�@��y@��!@��\@�=q@��@��T@���@�O�@�Q�@�|�@��@�ȴ@�v�@�n�@�ff@�J@�p�@�j@�  @��m@���@��@�;d@�@���@��+@�@���@��@�A�@�1'@�(�@���@�K�@�n�@�-@���@�`B@��@��j@��D@��@�Z@� �@��
@�l�@�"�@�o@��H@��!@��\@�V@�V@�$�@���@�x�@�hs@�`B@�hs@�p�@�hs@�V@�9X@���@�
=@��+@�J@�@�?}@��@��`@�Ĝ@���@�r�@�Q�@�b@��@�|�@�dZ@�dZ@�;d@��@��@�@���@��R@���@�5?@�@��h@�7L@���@���@�Z@�1'@�(�@� �@��@��F@���@�l�@�@���@��\@�ff@��@��@���@���@�x�@�`B@�&�@��@�r�@�1@�ƨ@���@��@�\)@�C�@�@���@�n�@��@���@�hs@�7L@�Ĝ@�Ĝ@���@��9@�1@�ƨ@���@�\)@�o@��R@�v�@��@�@���@��^@��@�`B@�X@�7L@���@��`@��9@�9X@��@�  @��w@��@�dZ@�\)@���@�~�@�5?@��T@��h@�p�@�X@�?}@�?}@�/@��@���@��/@���@��9@�bN@�(�@��m@���@�l�@�C�@��y@���@��!@��+@�M�@�$�@���@�p�@�/@��@��`@��u@�Z@� �@�b@�  @��@�P@+@~�+@~5?@}�T@}�@|�D@{�m@{��@{ƨ@{C�@{@z~�@y�^@y&�@x��@xbN@w�w@v�@vff@v5?@u@u��@u?}@t�/@tz�@t9X@t1@sdZ@r��@rn�@q��@q%@p��@o�@o�w@o�@n��@n��@n�@n��@nV@m@m?}@l�j@l�D@lz�@l9X@k��@k��@kdZ@ko@j��@j��@j~�@jn�@jn�@j-@i��@i��@ihs@hr�@hb@h  @g�@g\)@g\)@gK�@f��@fv�@f5?@e�T@eO�@e?}@e/@d�D@d�@c�m@c��@c�@cdZ@cdZ@b��@b�\@b-@a�#@a�7@aX@`Ĝ@`A�@`b@_�@_�@^��@^v�@^E�@]�T@]��@]p�@]/@]�@\�@\�j@\j@\I�@\Z@\9X@[ƨ@[��@[S�@Z��@Y�@Y�7@Y&�@Y%@X�`@Xr�@W�@W�w@W|�@W\)@V�y@VE�@V{@U�-@U/@Tj@T(�@St�@R�@R�\@Q��@Q�#@Q�7@Q�@P�u@Pr�@Pr�@PbN@PbN@PQ�@O�@O�@O��@O��@O|�@O;d@Nȴ@N�+@N@M��@M@M��@M��@Mp�@M�@L�j@LI�@K�@K@J�@J�@J�H@J�H@J�!@I��@I7L@I�@H�9@HQ�@G�;@G��@G|�@G|�@Gl�@F��@F@E�@D��@Dj@D�@C��@C�m@C��@B�@Bn�@A��@A��@AG�@@��@@�9@@�u@?�@?�P@?\)@?
=@>ȴ@>��@>$�@=�h@<j@;33@:��@:��@:�!@:��@:^5@9�7@8��@8�u@8�@8r�@8bN@8Q�@81'@7�@7;d@6�@6ff@6@5`B@4j@4j@4z�@4Z@3�
@3�@3S�@3@2�!@2��@2M�@2n�@2�@1�@0�`@0�`@0�@/��@/l�@/l�@/+@.��@.V@-�-@-`B@-�@,��@,�D@+�
@+33@*~�@*=q@*�@*�@*J@*J@)��@)��@)��@)�@)�@)�#@)�#@)�#@)�@)�#@)��@)�^@)��@)hs@)G�@(��@(  @'�@'��@'l�@&��@&ȴ@&v�@&5?@%��@%/@$��@$I�@#�m@#�F@#�@#dZ@#S�@#C�@#"�@"�@"�@"��@"�!@"��@"n�@"�@!��@ Ĝ@ 1'@ b@�@�w@l�@+@��@�y@�@�@��@��@ff@V@5?@{@�@�T@��@@��@�h@�@p�@p�@`B@O�@?}@?}@/@/@��@�j@9X@ƨ@dZ@��@�\@^5@M�@M�@M�@=q@J@X@&�@�@%@��@Ĝ@Q�@�;@�w@+@v�@{@�T@@��@�h@�h@p�@?}@O�@O�@`B@O�@O�@?}@/@�@V@�@�@�/@�/@��@��@�j@�@j@�m@�F@33@��@n�@M�@=q@��@�^@x�@x�@hs@7L@��@�`@��@�9@�u@�@r�@Q�@A�@b@  @  @�@�@�@�;@�w@|�@K�@�@
=@�y@�@�@ȴ@ȴ@ȴ@�R@��@�+@v�@V@5?@@p�@�@V@��A���A��A��#A��A��`A��HA��
A���A�A־wA־wA־wA�ƨAּjA���A���Aֺ^AּjAֺ^A��A��`A��TA��
A�ȴA�ĜAֲ-A֣�A֬Aֲ-A֮Aֲ-Aְ!Aְ!A֮A֩�A֥�A֥�A֣�A֣�A֙�A֋DA֓uA֗�A֕�A֕�A֓uA֓uA֏\A֑hA֕�A�x�A�t�A�n�A�l�A�l�A�jA�hsA�dZA�bNA�bNA�dZA�bNA�hsA�hsA�hsA�ffA�dZA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�`BA�\)A�\)A�\)A�\)A�^5A�^5A�`BA�bNA�bNA�`BA�bNA�bNA�bNA�bNA�bNA�`BA�`BA�`BA�`BA�`BA�^5A�ZA�XA�ZA�XA�XA�XA�XA�XA�XA�XA�XA�XA�ZA�ZA�XA�ZA�XA�XA�VA�XA�VA�XA�ZA�XA�XA�XA�VA�VA�VA�VA�XA�VA�VA�S�A�VA�S�A�S�A�S�A�VA�S�A�VA�S�A�S�A�VA�S�A�VA�Q�A�O�A�K�A�M�A�M�A�K�A�I�A�I�A�I�A�I�A�I�A�K�A�I�A�K�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�K�A�K�A�M�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�K�A�I�A�K�A�K�A�I�A�G�A�G�A�G�A�E�A�G�A�E�A�G�A�G�A�E�A�E�A�E�A�C�A�C�A�A�A�;dA�=qA�?}A�?}A�=qA�7LA�9XA�7LA�5?A�7LA�7LA�7LA�9XA�9XA�7LA�5?A�5?A�5?A�33A�5?A�5?A�5?A�5?A�9XA�7LA�7LA�9XA�9XA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�9XA�5?A�1'A�/A�-A�+A��A��A�bA�oA�bA�bA�VA�JA�
=A�
=A�1A�1A�%A�A���A���A��yA�ĜA�AվwAՍPA�I�A��A��AԃA�bNA�XA�-A���AӰ!Aӕ�A�l�A�G�A�9XA�1'A�+A�JA��`AҶFAҋDA�
=A���A�\)AϼjAϥ�Aϙ�AϏ\AρA�t�A�p�A�hsA�K�A�7LA�33A�"�A�JA���A���A���AΟ�A�r�A�AͅA��A���A̬ÁA�t�A�n�A�hsA�hsA�ffA�ffA�`BA�\)A�VA�M�A�E�A�7LA�+A�oA��A�ƨA˟�A�r�A�5?A���A��mA���Aʰ!A�|�A�O�A��A���A�  A�JA�+A�S�A�M�A�"�A�{A�JA���A��A��A���A��A��A��`A��#A���A�Aɺ^A�l�A�C�A�9XA�1'A�$�A�bA���A��A���A�A�-A�5?A�C�A�M�A�E�A�
=Aȧ�A�v�A�A�A�&�A��A�oA�%A�  A��A��
AǼjA�|�A�XA��;Aƣ�AƋDAƃAƁA�|�A�hsA�1'A��A�A��AŴ9AœuA�z�A�t�A�jA�5?A�%A�A�r�A�Q�A�5?A�&�A�A���A��A��A��TA��Aã�A�`BA�\)A�C�A��A®A�M�A�JA���A��A���A�Q�A� �A���A�`BA�VA���A���A�p�A�ffA�K�A�
=A�dZA��DA�VA���A��PA�C�A���A��
A���A���A��\A���A��PA��+A�|�A�dZA�Q�A�E�A�33A�$�A�JA���A��;A��FA� �A�I�A��FA�S�A�1'A�A��A��
A�~�A�/A��A�VA���A�dZA�33A���A�p�A�;dA�A��RA��7A�(�A���A��A���A�A�A��9A���A���A�~�A�O�A�;dA�33A�"�A���A��A��A�K�A�$�A��TA���A��A���A��!A���A�~�A�bNA�9XA�%A��^A��DA�bNA�=qA���A��A��#A���A�O�A��TA��FA�hsA�A��TA�VA�1'A��A��`A�`BA�%A���A��A�v�A�A�A��TA���A�v�A�O�A�+A��A���A���A���A�\)A�&�A�JA��A�ĜA��A�hsA�`BA�ZA�\)A�VA�-A��A��A�$�A�oA�1A�  A���A��A��HA��HA��HA��HA��`A��TA�ȴA��uA�K�A��A�A��-A��PA�n�A�K�A�"�A���A�\)A�+A�p�A�C�A�bA�r�A��^A�\)A�&�A���A��TA�ƨA��!A��DA�^5A�VA��jA�|�A��A���A��-A�$�A��hA�G�A�-A�%A���A���A���A��A��-A��!A�9XA���A��A�^5A�33A���A��A�I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�"B
��B
��B
�"B
��B
��B
�B
�]B
��B
��B
�PB
��B
��B
�PB
�B
��B
�B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�JB
�JB
�JB
�B
�JB
�JB
�B
��B
��B
��B
�B
��B
��B
�rB
��B
�`B
�B
�AB
�B
�AB
��B
��B�B�BuB@�BW�BR�B�~B��B��B��B��BخB�5B��B�B
	B�B~B�B)_BB�BY�BT,BL0BV9BYKB]dBZ�B`vBY�BS�BK�BK^BF?BH�B9XB2-B'�B�B�.B�|B�B�;B�5B�}B͟B˒B�B��B�BncBYB9$B�B�B
��B
�)B
��B
�0B
��B
�$B
�~B
zB
lWB
^B
,�B
$@B
PB	�>B	�B	��B	�B	��B	��B	�oB	^jB	IB	8�B	"�B	"B	{B�B�"B�;B��B�B�)B�aB�B�nB�aB�}B��B��B�'B�B�CB��B��B�B��B�B��B��B�eB�:B��B�7B��B��B��B�!B��B�hB�OB�MB�B�oB��B�$B��B��B��B�9BB��BٴBܒB��B�]B�;B��B��B�;B�5B� B�B�B�)B��B�rB��B�.B	 iB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"hB	'RB	&�B	&�B	'�B	+�B	.}B	-wB	/OB	0�B	49B	49B	7�B	7B	7LB	<6B	<�B	<�B	=�B	@B	E�B	P}B	XB	W�B	WsB	XB	Z�B	]/B	_B	_�B	aB	a|B	`�B	c�B	e�B	h>B	i�B	jB	lWB	qB	rB	rGB	rGB	rB	r�B	r�B	u�B	v+B	v�B	w2B	xB	|B	�B	��B	��B	�PB	�$B	�eB	�xB	�'B	�B	�0B	��B	��B	��B	��B	�eB	�B	��B	��B	�zB	��B	��B	�LB	�FB	�nB	��B	��B	��B	��B	�BB	��B	�OB	��B	�qB	��B	��B	�3B	�'B	� B	�BB	�B	��B	B	��B	��B	�mB	��B	�XB	��B	̘B	��B	�HB	��B	��B	��B	ޞB	�;B	�B	��B	�ZB	�&B	��B	�TB	�&B	��B	�B	�NB	�HB	�B	�|B	�ZB	�B	��B	��B	��B	�DB	��B	�B	�B	��B	�/B	�/B	�cB	��B	�cB	�B	�B	�B	�B	��B	�B	�MB	��B	�B	�B	�TB	��B	�+B	�fB	�B	�8B	��B	��B	��B	�	B	�rB	�rB	�rB	�rB	�>B	�xB	�rB	�>B	�	B	��B	�B	��B	�>B	�lB	�B	�8B	�ZB	�%B	��B	�B	��B	�B	�B	�TB	�`B	��B	�%B	�B	�B	�MB	�oB	�iB	�B	�5B	�B	�B	�TB	�B	��B	��B	�B	��B	��B	�8B	�>B	��B	�xB	�	B	�>B	�B	�DB	��B	�B	��B	�B	��B	�B	�"B	��B	�"B	�(B	��B	��B	��B	�(B	��B	�cB
 4B	��B
  B
 4B
 �B
oB
 �B
B
�B
�B
�B
B
MB
�B
�B
fB
	7B
�B
�B
fB
�B
�B
	7B
	B
	7B
	�B

rB

	B

	B

�B
�B
PB
�B
�B
.B
bB
 B
B
FB
�B
�B
B
�B
�B
�B
�B
7B
eB
�B
1B
�B
B
eB
�B
�B
7B
B
	B
�B
qB
CB
�B
�B
B
�B
B
�B
VB
VB
�B
�B
 'B
!-B
!�B
!�B
!�B
"4B
$B
$@B
%B
%B
&B
&�B
'RB
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
(�B
)�B
)_B
*eB
*�B
)�B
*�B
+6B
*�B
+B
,=B
,qB
,qB
.B
.�B
.B
-wB
.�B
/�B
/OB
0UB
0�B
0UB
0�B
2-B
1�B
1�B
1�B
1�B
2-B
1�B
1'B
2�B
3hB
4B
4B
49B
4nB
5?B
5tB
5tB
5�B
5B
4�B
6zB
7B
7LB
6�B
7�B
7�B
7�B
7�B
7�B
7�B
7LB
7B
7�B
8�B
8�B
8�B
9$B
;�B
;0B
;0B
;�B
<B
<6B
<jB
=qB
=<B
=<B
<�B
=�B
>B
>B
>BB
>wB
>BB
>wB
?B
>�B
?B
>wB
@B
?B
?}B
@�B
@B
@�B
AUB
@�B
B[B
A�B
AUB
A�B
A�B
A�B
B�B
B�B
D3B
D3B
C�B
D�B
DgB
EmB
E9B
EB
E�B
FB
EB
E�B
EB
F?B
E9B
F?B
E�B
GzB
GzB
F�B
G�B
HB
GzB
G�B
HKB
H�B
H�B
I�B
I�B
IRB
I�B
J#B
JXB
J�B
K)B
J�B
K)B
J�B
L0B
K�B
K�B
L0B
L0B
LdB
M6B
L�B
L�B
MB
L�B
M�B
MB
M6B
NB
NB
N<B
NpB
N�B
P}B
R B
RTB
RTB
RTB
R�B
S&B
S�B
S�B
S�B
S�B
T�B
UgB
U2B
UgB
V9B
VmB
V�B
VmB
V�B
V�B
W?B
W
B
V�B
W?B
WsB
W
B
W�B
W�B
XB
YB
XyB
X�B
YKB
YKB
YB
YB
YB
YB
YB
ZQB
ZQB
Z�B
ZQB
ZQB
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
[�B
[WB
[#B
[#B
[WB
\)B
[�B
[�B
\)B
]/B
^B
^B
^�B
^�B
^5B
^�B
^5B
^5B
^5B
]�B
^5B
_�B
_pB
`�B
aHB
a�B
a�B
a�B
a|B
a|B
a�B
bNB
bNB
b�B
cTB
c�B
d&B
d&B
c�B
d�B
e,B
e,B
e�B
e�B
e�B
e�B
e�B
e�B
d�B
d�B
e`B
gB
g�B
h>B
iDB
i�B
iyB
i�B
i�B
i�B
i�B
iyB
jKB
jKB
jB
j�B
j�B
k�B
kB
kB
kQB
k�B
k�B
k�B
k�B
k�B
kB
kB
k�B
k�B
l�B
kQB
k�B
l�B
m)B
m]B
n/B
n�B
o B
o B
o5B
o B
n�B
m�B
n/B
n�B
n�B
p�B
p�B
qB
qAB
qAB
qAB
qB
qAB
qAB
qvB
qAB
qvB
qAB
qAB
qvB
qAB
qvB
qAB
qvB
q�B
q�B
q�B
rGB
r|B
rGB
r|B
sB
r�B
sMB
sB
s�B
s�B
s�B
tB
t�B
uZB
u%B
u�B
u�B
uZB
uZB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v+B
wfB
xB
x�B
x�B
x�B
x�B
y>B
yrB
y�B
yrB
y�B
y�B
y�B
y�B
zB
zB
zDB
zxB
zDB
z�B
z�B
zxB
z�B
z�B
z�B
z�B
{B
zxB
{B
{B
z�B
{B
z�B
{B
{B
{�B
{�B
|�B
}�B
}"B
}�B
}�B
}�B
}VB
}�B
}�B
~�B
~]B
~�B
~]B
~�B
~�B
.B
cB
cB
�4B
�;B
�B
�;B
�oB
�oB
��B
�oB
��B
��B
��B
�B
�oB
��B
��B
��B
�AB
��B
��B
�AB
��B
�AB
��B
�uB
�B
�AB
�AB
�uB
�GB
�uB
�{B
��B
��B
�B
�MB
��B
�B
�B
�B
��B
�SB
��B
��B
��B
��B
��B
�%B
��B
��B
��B
�YB
�+B
��B
��B
��B
��B
��B
�_B
�1B
��B
�fB
�B
�B
�B
��B
�lB
��B
�B
��B
�lB
�B
��B
�B
�lB
�	B
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
�VB
��B
�(B
��B
��B
�"B
��B
��B
��B
�]B
�VB
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
�B
��B
��B
�JB
��B
�B
��B
�VB
��B
��B
��B
�B
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
�xB
�JB
��B
��B
�PB
��B
��B
��B
�(B
��B
�PB
��B
�VB
�JB
��B
�JB
��B
�JB
��B
��B
�xB
�xB
��B
�B
��B
��B
��B
�B
�JB
�JB
�PB
�"B
��B
��B
��B
��B
��B
��B
��B
�JB
�xB
�B
�DB
��B
��B
�JB
��B
��B
�JB
�B
�B
��B
��B
�B
�PB
��B
��B
�PB
��B
�PB
�PB
��B
��B
�JB
�B
��B
�JB
��B
�B
�B
�B
�PB
��B
�B
�JB
��B
��B
��B
��B
��B
��B
��B
�B
�PB
�PB
�B
�PB
�PB
��B
�PB
��B
��B
�PB
�PB
��B
��B
�B
�VB
�B
�B
�B
�B
��B
�PB
�B
��B
��B
�B
�B
��B
�B
��B
�B
�PB
��B
�PB
�PB
�PB
�B
��B
�PB
�PB
��B
�B
��B
��B
��B
��B
�B
��B
��B
�xB
�xB
�DB
�xB
��B
�xB
�DB
�B
��B
�DB
�xB
��B
��B
��B
�JB
��B
�B
�B
�PB
��B
�JB
��B
�B
�DB
�B
��B
��B
�DB
�rB
�B
�DB
��B
��B
�JB
�B
��B
��B
��B
�JB
�xB
��B
�B
��B
�DB
�xB
�rB
�	B
�>B
��B
�	B
�rB
�B
�xB
��B
��B
�xB
��B
��B
��B
�xB
�B
�rB
��B
��B
�lB
�rB
�	B
�	B
��B
��B
��B
�lB
�rB
��B
�xB
��B
��B
��B
��B
�>B
��B
��B
�+B
��B
��B
��B
��B
��B
��B
�`B
�`B
��B
�ZB
��B
�`B
��B
�oB
��B
��B
�B
�B
�B
�B
��B
�`B
�B
�B
�B
�B
�B
�B
�fB
��B
��B
��B
�B
�B
�B
�B{BxB
�xB
��B
�|B
�B
�B
�MB
�B
��B
�>B
��B
��B
�fB
�+B
�xB
��B
�%B
��B
�"B.BoB�B(B�B{BhBoB:B:B�BhB�B�BBFBB�B �B)�B4nBYBa|Bb�Be,B[#BXEBV�BVBP}BU�BMBH�BK)BOvBbB��B�uB��B�B�4B�B�bB��B�B��B�B�6B�B�B��B�kB�XB�~B�7B��B��B��B��B�_B��B��B��B�'B�B�tB�#B��B�B�sB�HB��B�sB�B�QBںBݘB�|B��B�B�TBB	lB�BB;B�B�BDB	�B�B	7B~BB
	B+B�B
�BYB B�B�B�BB�BPBBB~B
rBFBhBhBB�B*0B �B"hB*�B.B0�B$B+B?}BLdBL0BI�BMjBM�BL�BR B\]Bh�Bk�B\�BV�BOBB[�BD�BHBMBH�BJ�BNBWsBQNBW�BV�BW?BXyBW�BXyB]/BXyBY�BU�Bk�Bg�BZ�BT�BS&BV�BU�BX�Bi�B]�BWsBXBc�B^B^5Be�B]/BU�B\]BXB[�BX�Bd�Bb�BK)BIRBIBNBK)BJ�BL0BMjBH�BGEBHKBQNBI�BI�BK)BI�BMjBH�B?}BF�BI�BQNBI�BGBDgB?}BD3B:�B7B5�B<�B1'B3�B7LB7LB1[B$B$�B49BNpB	B�BB�B%zBJB�B&�B
�B�B�]B��B��B�ZB�|B�B�|B�B�B�B�B�B�B��B�"B��BޞB�/BیBںB�B��B�B�)B�TB��B�NB�B�HB�B�jB�6B�<B˒B�0B��B˒B��BƨBɆBбB��B�^B�jB�B��B��BϫB�0B�B�SB�B��B� B}�Bx�Bt�BrGBo5BkBbBdZBl"BaHB^BR�BM6BNpBYKB;�BMB-CB	�BBBB�B�B"�B(B 4B
��B
��B
�MB
�vB
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                     B
��B
�.B
��B
��B
�.B
��B
��B
�B
�iB
� B
� B
�\B
��B
��B
�\B
�(B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�VB
�VB
�VB
�"B
�VB
�VB
�"B
��B
�B
�B
�B
��B
��B
�~B
��B
�lB
�+B
�MB
�B
�MB
�B
��B
��B	�B�B8�BO�BJ�B��B��B��B��B��BкB�AB��B��BB�B�BB!kB:�BQ�BL8BD<BNEBQWBUpBR�BX�BQ�BK�BC�BCjB>KB@�B1dB*9B�B�B�:B�B�B�GB�ABȉBūBÞB�B��BzBfoBQ#B10B
��B
��B
�B
�5B
�B
�<B
��B
�0B
��B
rB
dcB
VB
$�B
LB
\B	�JB	�B	��B	� B	��B	��B	y{B	VvB	A)B	0�B	�B	.B��B�B�.B�GB��B�B�5B�mB�#B�zB�mB��B��B��B�3B�'B�OB��B�B�B��B�'B��B��B�qB�FB��B�CB��B��B��B�-B��B�tB�[B�YB�B�{B��B�0B��B��B�B�EB��B��B��BԞB�B�iB�GB��B��B�GB�AB�,B�B�B�5B��B�~B�B�:B�uB��B��B	�B	�B	�B		�B	�B	�B	�B	�B	tB	^B	�B	�B	�B	#�B	&�B	%�B	'[B	(�B	,EB	,EB	/�B	/#B	/XB	4BB	4�B	4�B	5�B	8&B	=�B	H�B	PB	O�B	OB	PB	R�B	U;B	WB	W�B	YB	Y�B	X�B	[�B	^
B	`JB	a�B	b�B	dcB	iB	jB	jSB	jSB	jB	j�B	j�B	m�B	n7B	n�B	o>B	pB	t(B	}+B	��B	��B	�\B	�0B	�qB	��B	�3B	�B	�<B	�B	��B	��B	��B	�qB	�B	��B	��B	��B	��B	��B	�XB	�RB	�zB	��B	��B	��B	��B	�NB	��B	�[B	��B	�}B	��B	��B	�?B	�3B	�,B	�NB	�B	��B	��B	�B	��B	�yB	��B	�dB	��B	ĤB	��B	�TB	��B	�
B	��B	֪B	�GB	�B	��B	�fB	�2B	��B	�`B	�2B	��B	ڎB	�ZB	�TB	�B	وB	�fB	ݡB	�
B	��B	��B	�PB	��B	�B	�B	��B	�;B	�;B	�oB	��B	�oB	�B	�B	�B	��B	��B	��B	�YB	��B	�+B	��B	�`B	�B	�7B	�rB	�B	�DB	�B	�B	��B	�B	�~B	�~B	�~B	�~B	�JB	�B	�~B	�JB	�B	��B	�B	��B	�JB	�xB	�B	�DB	�fB	�1B	��B	�B	�B	�+B	�+B	�`B	�lB	�B	�1B	�+B	�B	�YB	�{B	�uB	�B	�AB	�B	�%B	�`B	�%B	��B	��B	��B	�B	�	B	�DB	�JB	��B	�B	�B	�JB	�"B	�PB	�B	�(B	�B	�(B	��B	�(B	�.B	��B	�.B	�4B	��B	� B	��B	�4B	��B	�oB	�@B	��B	�B	�@B	��B	�{B	��B	�B	��B	��B	��B	�%B	�YB	��B	��B
 rB
CB
 �B
 �B
 rB	��B	��B
CB
B
CB
�B
~B
B
B
�B
�B
\B
�B
�B
:B
nB
	B
B
RB
�B
�B
*B
�B
�B
B
�B
CB
qB
�B
=B
B
B
qB
�B
�B
CB
B
B
�B
}B
OB
�B
�B
!B
�B
'B
�B
bB
bB
�B
�B
3B
9B
�B
�B
�B
@B
B
LB
B
B
$B
�B
^B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
!�B
!kB
"qB
"�B
"B
"�B
#BB
"�B
#B
$IB
$}B
$}B
& B
&�B
& B
%�B
&�B
'�B
'[B
(aB
(�B
(aB
(�B
*9B
)�B
*B
*B
*B
*9B
)�B
)3B
*�B
+tB
,B
,B
,EB
,zB
-KB
-�B
-�B
-�B
-B
,�B
.�B
/#B
/XB
.�B
/�B
/�B
/�B
/�B
/�B
/�B
/XB
/#B
/�B
0�B
0�B
0�B
10B
3�B
3<B
3<B
3�B
4B
4BB
4vB
5}B
5HB
5HB
4�B
5�B
6B
6B
6NB
6�B
6NB
6�B
7 B
6�B
7 B
6�B
8&B
7 B
7�B
8�B
8&B
8�B
9aB
8�B
:gB
9�B
9aB
9�B
9�B
9�B
;B
;B
<?B
<?B
;�B
<�B
<sB
=yB
=EB
=B
=�B
>B
=B
=�B
=B
>KB
=EB
>KB
=�B
?�B
?�B
>�B
?�B
@#B
?�B
?�B
@WB
@�B
@�B
A�B
A�B
A^B
A�B
B/B
BdB
B�B
C5B
B�B
C5B
B�B
D<B
C�B
DB
D<B
D<B
DpB
EBB
D�B
D�B
EB
D�B
E�B
EB
EBB
FB
FB
FHB
F|B
F�B
H�B
J,B
J`B
J`B
J`B
J�B
K2B
K�B
K�B
K�B
LB
L�B
MsB
M>B
MsB
NEB
NyB
N�B
NyB
N�B
N�B
OKB
OB
N�B
OKB
OB
OB
O�B
O�B
PB
Q#B
P�B
P�B
QWB
QWB
Q#B
Q#B
Q#B
Q#B
Q#B
R]B
R]B
R�B
R]B
R]B
R�B
R�B
S/B
S�B
S�B
S�B
S�B
S�B
ScB
S/B
S/B
ScB
T5B
TB
TB
T5B
U;B
VB
VB
V�B
V�B
VAB
V�B
VAB
VAB
VAB
U�B
VAB
W�B
W|B
X�B
YTB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZZB
ZZB
Z�B
[`B
[�B
\2B
\2B
[�B
]B
]8B
]8B
]�B
]�B
]�B
^
B
^
B
]�B
\�B
\�B
]lB
_B
_�B
`JB
aPB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bWB
bWB
b�B
b�B
b�B
c�B
c(B
c(B
c]B
c�B
c�B
c�B
c�B
c�B
c(B
c(B
c�B
c�B
d�B
c]B
c�B
e B
e5B
eiB
f;B
f�B
gB
gB
gAB
gB
f�B
fB
f;B
f�B
f�B
h�B
h�B
iB
iMB
iMB
iMB
iB
iMB
iMB
i�B
iMB
i�B
iMB
iMB
i�B
iMB
i�B
iMB
i�B
i�B
i�B
i�B
jSB
j�B
jSB
j�B
k%B
j�B
kYB
k%B
k�B
k�B
k�B
l+B
l�B
mfB
m1B
m�B
m�B
mfB
mfB
m�B
nB
m�B
m�B
m�B
nB
n7B
n7B
orB
pB
p�B
p�B
p�B
p�B
qJB
q~B
q�B
q~B
q�B
q�B
q�B
q�B
rB
rB
rPB
r�B
rPB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s"B
r�B
s"B
s�B
r�B
s"B
r�B
s"B
s"B
s�B
s�B
t�B
u�B
u.B
u�B
u�B
u�B
ubB
u�B
u�B
v�B
viB
v�B
viB
v�B
v�B
w:B
woB
woB
x@B
yGB
yB
yGB
y{B
y{B
y�B
y{B
y�B
y�B
y�B
zB
y{B
y�B
y�B
y�B
zMB
y�B
y�B
zMB
y�B
zMB
y�B
z�B
zB
zMB
zMB
z�B
{SB
z�B
{�B
{�B
|�B
|%B
|YB
|�B
}+B
}+B
}+B
}�B
}_B
}�B
}�B
}�B
}�B
~�B
~1B
}�B
~�B
~�B
~eB
7B
B
B
B
B
�B
kB
�=B
��B
�rB
�B
�B
�B
��B
�xB
��B
�B
��B
�xB
�B
��B
�B
�xB
�B
��B
��B
��B
��B
� B
��B
�B
��B
�B
��B
�bB
��B
�4B
��B
��B
�.B
��B
��B
��B
�iB
�bB
��B
� B
��B
��B
�\B
�B
��B
�B
��B
��B
�B
��B
��B
�VB
�B
�(B
�B
�bB
��B
��B
�B
�(B
�B
��B
�B
�B
�B
�\B
��B
��B
��B
��B
�B
�VB
��B
��B
�\B
��B
��B
��B
�4B
��B
�\B
�B
�bB
�VB
��B
�VB
��B
�VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�"B
�VB
�VB
�\B
�.B
��B
��B
��B
��B
��B
��B
�B
�VB
�B
�"B
�PB
��B
��B
�VB
�B
�B
�VB
�B
�"B
��B
��B
�B
�\B
�B
��B
�\B
��B
�\B
�\B
�B
��B
�VB
�B
�B
�VB
��B
�(B
�(B
�(B
�\B
��B
�"B
�VB
��B
��B
�B
�B
�B
��B
�B
�B
�\B
�\B
�(B
�\B
�\B
��B
�\B
��B
��B
�\B
�\B
��B
��B
�(B
�bB
�B
�(B
�B
�(B
��B
�\B
�(B
��B
��B
�(B
�(B
��B
�(B
��B
�(B
�\B
��B
�\B
�\B
�\B
�(B
��B
�\B
�\B
�B
�B
��B
�B
��B
��B
�"B
��B
�B
�B
�B
�PB
�B
�B
�B
�PB
�B
�B
�PB
�B
�B
��B
��B
�VB
��B
�"B
�B
�\B
�B
�VB
��B
�"B
�PB
�B
�B
��B
�PB
�~B
�B
�PB
��B
�B
�VB
�B
�B
��B
�B
�VB
�B
��B
�(B
��B
�PB
�B
�~B
�B
�JB
��B
�B
�~B
�B
�B
�B
��B
�B
��B
�B
�B
�B
�B
�~B
�B
��B
�xB
�~B
�B
�B
�B
�B
��B
�xB
�~B
�B
�B
�B
�B
��B
��B
�JB
�	B
�B
�7B
��B
�B
�B
��B
��B
�B
�lB
�lB
��B
�fB
��B
�lB
�B
�{B
��B
��B
�B
�B
�B
ާB
��B
�lB
�"B
�B
�B
�B
�B
�B
�rB
��B
��B
��B
ާB
�B
ާB
�B
��B�B
�B
��B
�B
�B
�B
�YB
�+B
��B
�JB
��B
��B
�rB
�7B
�B
��B
�1B
� B
�.B:B
{B
�B4BB�B	tB
{B
FB
FB	�B	tB
�B	�B
BRB*B�B�B!�B,zBQ�BY�BZ�B]8BS/BPQBN�BNBH�BM�BEB@�BC5BG�BZ%B|�B��B��B�B�@B�B�nB��B�B��B�*B�BB�B� B��B�wB�dB��B�CB��B��B��B��B�kB��B��B��B�3B�B��B�/B�
B�B�B�TB��B�B�B�]B��BդBوB��B�B�`B�+BxB��B�B�GB��B�BPB�B��BCB�B!BB�7B��B�B�eB	B�B�B�B!B�B\B!B'B�B~BRB	tB	tB*B�B"<B�BtB"�B& B(�BB#B7�BDpBD<BA�BEvBE�BD�BJ,BTiB`�Bc�BT�BN�BGNBS�B<�B@#BEB@�BB�BFBOBIZBO�BN�BOKBP�BO�BP�BU;BP�BQ�BM�Bc�B_�BR�BM
BK2BN�BM�BP�Ba�BU�BOBPB[�BVBVAB^
BU;BM�BTiBPBS�BP�B\�BZ�BC5BA^BA)BFBC5BB�BD<BEvB@�B?QB@WBIZBA�BA�BC5BA�BEvB@�B7�B>�BA�BIZBA�B?B<sB7�B<?B2�B/#B-�B4�B)3B+�B/XB/XB)gBB�B,EBF|BB�B
B�B�BVB�B�B�B��B�iB��B�B�fB�B�B�B�B�+B�B�"B�B�B��B�.B�B֪B�;BӘB��B��B��BڎB�5B�`B��B�ZB�&B�TB�&B�vB�BB�HBÞB�<B��BÞB��B��B��BȽB��B�jB�vB�#B��B��BǷB�<B�$B�_B� B��BxBu�Bp�Bl�BjSBgABc(BZ%B\fBd.BYTBVBJ�BEBBF|BQWB3�BEB%OB�B
�B
�B
�%B
��B
��B�B4B
�@B
�B
��B
�YB
�B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230730020103                            20230730020103AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023073002010320230730020103  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023073002010320230730020103QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023073002010320230730020103QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               