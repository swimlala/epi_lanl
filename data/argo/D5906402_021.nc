CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-06-10T03:24:51Z creation; 2022-04-26T16:06:58Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     (  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  Z0   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     (  a|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  ~�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     (  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  ǌ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( 	(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L &P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( -�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L J�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( R   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` o8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   o�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   u�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   {�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210610032451  20220426232406  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_021                 8138_008904_021                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @�{B��G�@�{B��G�11  @�{B�s�@�{B�s�@,nߤ?��@,nߤ?���d���n/�d���n/11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@@  @�  @�  @�  @�  A ��A  A ��A,��A@  A_\)A~�RA�\)A�  A�  A�  A�\)A�  A�  A��B�B  B(�B (�B((�B0  B8  B@(�BH  BO�
BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�{B�{B�{B�{B�  B�  B��B�{B�(�B��B��B��
B��B��B��B�  B�  B�  B��B�  B�{B�{B�{B�{B�  B��B�  B�  B��C  C��C  C  C	��C  C  C  C��C��C  C  C  C  C��C��C"  C$  C&  C(  C*  C+��C.  C0  C2  C3��C5��C7��C9��C;��C=��C?��CB
=CD  CF  CH
=CJ  CL  CM��CO��CR  CT{CV  CW��CZ  C\  C^  C`
=Cb
=Cc��Cf  Ch{Cj
=Cl  Cm�Cp  Cr
=Ct  Cu�Cw�Cz  C|  C~
=C�
=C�C�  C�  C�C�
=C�
=C�C�  C���C�C�  C���C�  C�  C���C�  C�  C���C�  C�  C�  C�  C�  C���C�  C���C�  C�  C�  C�  C�  C�C�  C�  C�  C�  C���C�  C�C�C�  C���C�  C�C���C�  C�C�  C�  C�  C�  C�  C�  C�  C���C���C�  C���C���C�C�C���C���C�  C�  C���C���C���C�  C���C�  C�  C�C�
=C�C�  C�C�C�C�
=C�
=C�C�C�
=C�C�  C���C�  C�C�C�C�C���C�  C�  C���C�  C�
=C�C�  C���C�  C�  C���C�C�C�  C�  C�  C���C���C�  C�  C�C�C���C�  C�C�  C�  C�C���C�  C�C�C�C�D   D }qD ��Dz�D��D� DD��D�D� D  D}qD�qD}qD�qD� D  D}qD�qD	� D	�qD
z�D
�qD� D�D� D�qD� D  D}qD�qD}qDD�D  D� D�D��D�D��D�D��D�D� D�qD� D�qD}qD�D��D  D� D  D��D�qD� D�D��D  D}qD�D�D�qD� D D �D!  D!z�D!��D"z�D"�qD#� D$  D$��D%D%��D&  D&}qD'  D'� D'�qD(��D)�D)��D*  D*� D+D+��D,�D,��D-  D-��D-�qD.� D/�D/z�D/�qD0� D0��D1� D2�D2� D3  D3��D3�qD4z�D4��D5��D6D6� D7�D7�D8D8��D9�D9� D:�D:� D:��D;}qD<  D<� D<�qD=}qD=�qD>� D?  D?}qD@  D@� DA  DA� DB�DB��DC  DC� DC�qDD� DE�DE� DF  DF� DG�DG� DH  DH}qDH�qDI� DJ  DJ��DK  DK��DL  DL� DM�DM� DN  DN� DO  DO� DP  DP� DQ�DQ� DR  DR}qDS�DS� DT  DT� DU  DU}qDV  DV}qDV�qDW� DX  DX��DY  DY��DZ�DZ��D[�D[��D\  D\��D]  D]}qD^  D^}qD^�qD_� D_�qD`}qDa  Da� Db  Db� Dc�Dc��Dd�Dd��Dd�qDe��Df�Df� Dg  Dg� Dh�Dh� Dh�qDi� Dj  Dj� Dk  Dk� Dl  Dl� Dl�qDm}qDm�qDn}qDn�qDo��Dp  Dp� Dq  Dq}qDr�Dr� Ds�Ds�Dt�Dt� Dt�qDu� Dv�Dv� Dv�qDwz�Dx  Dx��Dy  Dy� DzDz��D{D{�D|�D|}qD}  D}}qD}�qD~� D  D}qD�HD�AHD�� D��HD�HD�AHD�� D���D�  D�B�D��HD��HD�  D�>�D�� D�D�HD�@ D��HD�D�  D�AHD��HD�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�AHD�� D��HD�HD�AHD��HD�� D���D�>�D�}qD�� D��D�@ D�� D��HD�HD�@ D�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�>�D�� D��HD�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD��HD�� D�HD�AHD�~�D���D���D�=qD�~�D�� D�HD�B�D��HD��HD�  D�>�D�� D�D�  D�@ D���D�� D��qD�@ D���D��HD�HD�@ D�� D���D���D�>�D�� D�� D���D�@ D�� D���D���D�@ D�� D���D���D�>�D�~�D���D���D�>�D�� D���D�  D�AHD�� D��qD���D�AHD���D�� D���D�>�D�~�D�� D�  D�@ D�~�D���D���D�@ D�� D�� D���D�>�D�� D�� D�HD�AHD�� D���D�  D�@ D�� D��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��HD�D�HD�AHD�� D���D���D�>�D�� D�� D�  D�>�D�� D�� D�  D�AHD��HD��HD�  D�>�D�� D�� D���D�=qD�~�D�� D�HD�@ D�� D��HD��D�@ D�~�D��HD�HD�AHD�� D�� D�HD�@ D�� D��HD���D�>�D�~�D�� D�HD�@ D�� D�� D���D�@ D�� D��HD�HD�B�D�� D���D�  D�@ D��HD��HD�HD�@ D��HD�� D���D�@ D�� D��HD�HD�B�D��HD�� D�HD�AHD��HD�� D�HD�AHD��HD�� D�  D�>�D�~�D¾�D�  D�>�DÀ Dþ�D�  D�@ D�~�Dľ�D���D�@ Dŀ D�� D���D�>�D�~�D�� D�HD�@ D�~�DǾ�D���D�=qDȀ D��HD�HD�>�D�~�D�� D�  D�>�D�~�Dʾ�D�  D�>�Dˀ D�� D�HD�@ D�~�D��HD�HD�@ D�~�D�� D�  D�@ D΁HD�� D�  D�AHD�~�D�� D�  D�AHDЁHD��HD�  D�AHDсHD��HD�  D�>�DҀ D�� D�  D�@ D�~�DӾ�D���D�@ DԂ�D��HD�HD�AHDՀ DսqD���D�@ DցHD��HD�  D�>�D׀ D�� D�  D�>�D؀ D��HD�HD�AHD�~�D�� D�HD�>�D�~�Dھ�D���D�>�Dۀ D�� D�  D�AHD܂�D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��HD���D�@ D߁HD�D�HD�@ D�� D�� D���D�@ D�HD�D�HD�AHD�~�D⾸D�  D�>�D�~�D�qD�  D�@ D� D��HD�  D�=qD� D��HD�HD�AHD�HD�� D�  D�@ D�HD��H?��?8Q�?k�?���?�Q�?�
=?�@�@��@(��@5@E�@Y��@fff@u@��@�{@�z�@��R@�ff@���@�Q�@�  @Ǯ@У�@��H@�G�@�=q@�z�@�(�A�\A
=A�A  Az�AQ�Ap�A!G�A%�A*�HA/\)A333A7�A<��A@��AEAJ=qAN{AS33AW�A[�A`��Ae�Ai��An{As33Aw
=A{�A�Q�A�=qA���A��RA���A�33A�p�A�\)A��A��
A�A�Q�A�=qA�z�A�
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?�  ?��H@@  @�  @�  @�  @�  A ��A  A ��A,��A@  A_\)A~�RA�\)A�  A�  A�  A�\)A�  A�  A��B�B  B(�B (�B((�B0  B8  B@(�BH  BO�
BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�{B�{B�{B�{B�  B�  B��B�{B�(�B��B��B��
B��B��B��B�  B�  B�  B��B�  B�{B�{B�{B�{B�  B��B�  B�  B��C  C��C  C  C	��C  C  C  C��C��C  C  C  C  C��C��C"  C$  C&  C(  C*  C+��C.  C0  C2  C3��C5��C7��C9��C;��C=��C?��CB
=CD  CF  CH
=CJ  CL  CM��CO��CR  CT{CV  CW��CZ  C\  C^  C`
=Cb
=Cc��Cf  Ch{Cj
=Cl  Cm�Cp  Cr
=Ct  Cu�Cw�Cz  C|  C~
=C�
=C�C�  C�  C�C�
=C�
=C�C�  C���C�C�  C���C�  C�  C���C�  C�  C���C�  C�  C�  C�  C�  C���C�  C���C�  C�  C�  C�  C�  C�C�  C�  C�  C�  C���C�  C�C�C�  C���C�  C�C���C�  C�C�  C�  C�  C�  C�  C�  C�  C���C���C�  C���C���C�C�C���C���C�  C�  C���C���C���C�  C���C�  C�  C�C�
=C�C�  C�C�C�C�
=C�
=C�C�C�
=C�C�  C���C�  C�C�C�C�C���C�  C�  C���C�  C�
=C�C�  C���C�  C�  C���C�C�C�  C�  C�  C���C���C�  C�  C�C�C���C�  C�C�  C�  C�C���C�  C�C�C�C�D   D }qD ��Dz�D��D� DD��D�D� D  D}qD�qD}qD�qD� D  D}qD�qD	� D	�qD
z�D
�qD� D�D� D�qD� D  D}qD�qD}qDD�D  D� D�D��D�D��D�D��D�D� D�qD� D�qD}qD�D��D  D� D  D��D�qD� D�D��D  D}qD�D�D�qD� D D �D!  D!z�D!��D"z�D"�qD#� D$  D$��D%D%��D&  D&}qD'  D'� D'�qD(��D)�D)��D*  D*� D+D+��D,�D,��D-  D-��D-�qD.� D/�D/z�D/�qD0� D0��D1� D2�D2� D3  D3��D3�qD4z�D4��D5��D6D6� D7�D7�D8D8��D9�D9� D:�D:� D:��D;}qD<  D<� D<�qD=}qD=�qD>� D?  D?}qD@  D@� DA  DA� DB�DB��DC  DC� DC�qDD� DE�DE� DF  DF� DG�DG� DH  DH}qDH�qDI� DJ  DJ��DK  DK��DL  DL� DM�DM� DN  DN� DO  DO� DP  DP� DQ�DQ� DR  DR}qDS�DS� DT  DT� DU  DU}qDV  DV}qDV�qDW� DX  DX��DY  DY��DZ�DZ��D[�D[��D\  D\��D]  D]}qD^  D^}qD^�qD_� D_�qD`}qDa  Da� Db  Db� Dc�Dc��Dd�Dd��Dd�qDe��Df�Df� Dg  Dg� Dh�Dh� Dh�qDi� Dj  Dj� Dk  Dk� Dl  Dl� Dl�qDm}qDm�qDn}qDn�qDo��Dp  Dp� Dq  Dq}qDr�Dr� Ds�Ds�Dt�Dt� Dt�qDu� Dv�Dv� Dv�qDwz�Dx  Dx��Dy  Dy� DzDz��D{D{�D|�D|}qD}  D}}qD}�qD~� D  D}qD�HD�AHD�� D��HD�HD�AHD�� D���D�  D�B�D��HD��HD�  D�>�D�� D�D�HD�@ D��HD�D�  D�AHD��HD�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�AHD�� D��HD�HD�AHD��HD�� D���D�>�D�}qD�� D��D�@ D�� D��HD�HD�@ D�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�>�D�� D��HD�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD��HD�� D�HD�AHD�~�D���D���D�=qD�~�D�� D�HD�B�D��HD��HD�  D�>�D�� D�D�  D�@ D���D�� D��qD�@ D���D��HD�HD�@ D�� D���D���D�>�D�� D�� D���D�@ D�� D���D���D�@ D�� D���D���D�>�D�~�D���D���D�>�D�� D���D�  D�AHD�� D��qD���D�AHD���D�� D���D�>�D�~�D�� D�  D�@ D�~�D���D���D�@ D�� D�� D���D�>�D�� D�� D�HD�AHD�� D���D�  D�@ D�� D��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��HD�D�HD�AHD�� D���D���D�>�D�� D�� D�  D�>�D�� D�� D�  D�AHD��HD��HD�  D�>�D�� D�� D���D�=qD�~�D�� D�HD�@ D�� D��HD��D�@ D�~�D��HD�HD�AHD�� D�� D�HD�@ D�� D��HD���D�>�D�~�D�� D�HD�@ D�� D�� D���D�@ D�� D��HD�HD�B�D�� D���D�  D�@ D��HD��HD�HD�@ D��HD�� D���D�@ D�� D��HD�HD�B�D��HD�� D�HD�AHD��HD�� D�HD�AHD��HD�� D�  D�>�D�~�D¾�D�  D�>�DÀ Dþ�D�  D�@ D�~�Dľ�D���D�@ Dŀ D�� D���D�>�D�~�D�� D�HD�@ D�~�DǾ�D���D�=qDȀ D��HD�HD�>�D�~�D�� D�  D�>�D�~�Dʾ�D�  D�>�Dˀ D�� D�HD�@ D�~�D��HD�HD�@ D�~�D�� D�  D�@ D΁HD�� D�  D�AHD�~�D�� D�  D�AHDЁHD��HD�  D�AHDсHD��HD�  D�>�DҀ D�� D�  D�@ D�~�DӾ�D���D�@ DԂ�D��HD�HD�AHDՀ DսqD���D�@ DցHD��HD�  D�>�D׀ D�� D�  D�>�D؀ D��HD�HD�AHD�~�D�� D�HD�>�D�~�Dھ�D���D�>�Dۀ D�� D�  D�AHD܂�D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��HD���D�@ D߁HD�D�HD�@ D�� D�� D���D�@ D�HD�D�HD�AHD�~�D⾸D�  D�>�D�~�D�qD�  D�@ D� D��HD�  D�=qD� D��HD�HD�AHD�HD�� D�  D�@ D�HG�O�?��?8Q�?k�?���?�Q�?�
=?�@�@��@(��@5@E�@Y��@fff@u@��@�{@�z�@��R@�ff@���@�Q�@�  @Ǯ@У�@��H@�G�@�=q@�z�@�(�A�\A
=A�A  Az�AQ�Ap�A!G�A%�A*�HA/\)A333A7�A<��A@��AEAJ=qAN{AS33AW�A[�A`��Ae�Ai��An{As33Aw
=A{�A�Q�A�=qA���A��RA���A�33A�p�A�\)A��A��
A�A�Q�A�=qA�z�A�
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�ȴA���A�ƨAӸRAө�AӲ-AӲ-A�ĜA�Aӗ�A�|�A�z�A�\)A�G�A�5?A�/A�(�A�+A�-A�1'A�7LA�;dA�;dA�?}A�A�A�E�A�G�A�I�A�M�A�Q�A�\)A�jA�r�A�~�A�n�A�O�A�A�r�A�%AѴ9AЧ�A���A͓uA�XA��Aʣ�A�\)A���A�dZA��;A�v�A�A��TA�|�AƓuA�`BA�-A� �A���A��Aš�A�Q�Aĥ�Aĉ7A�l�A�"�Aá�A��A���A�^5A�bAhA�O�A��wA�1'A��A�l�A��A��-A�(�A�v�A���A��DA��^A�VA�A�n�A�r�A�A�A�A��FA�t�A��A�+A�M�A��hA���A���A���A�I�A��\A�O�A�A�Q�A���A�"�A�=qA�O�A�33A�JA��A�mAu��As33Ao�TAmS�Aj9XAcC�A_�wA^ffA\VA[%AZVAXI�ATJAP��ANZAM�AM/ALAJffAGK�AE+AD  AC33AA��A@�A?C�A>5?A=��A=�A<��A<�A:��A9ƨA9��A9�A7��A6ȴA5/A3��A1\)A.bNA,ȴA,^5A+�FA+XA+VA)33A(�A'dZA'/A&I�A&1'A$��A#?}A"��A"��A#
=A$�A&9XA&(�A%�;A%ƨA%�PA%l�A%&�A$��A#�mA"jA!��A �jA�A�mAQ�AQ�A�
A��A�9A��A9XA��AVA��A��At�A��A�PAx�A33A�`A�FA�AC�A�!AbA��A��A?}Ar�AS�Ar�Al�A�yA�;AoA �A��AO�A
��A
^5A	7LA �A�A�AĜAz�A��AG�AjA�A�#A�7A%A�jA�!A�\A��Az�AffAE�A(�A�A/AS�A;dA�A ��A ��A Q�@���@���@�^5@��@���@��D@�1@��@���@�o@�E�@��h@��j@�Z@��@��P@�;d@�ff@�V@���@�bN@�F@���@���@�M�@�@�O�@��`@���@�+@�/@��@�Ĝ@�@�A�@�!@��@��#@�x�@�bN@��@�@�\)@�
=@�!@�{@�^@噚@�7L@���@�D@�Q�@���@��;@㝲@�\)@�!@�^@��/@�p�@�p�@��;@���@݁@�O�@�X@�X@�`B@�p�@�7L@�Ĝ@ۮ@�{@��`@���@�b@�@��@��@���@ԓu@��@�S�@��H@�^5@�5?@�-@Ѳ-@�X@��@У�@ϕ�@�K�@�+@θR@Ώ\@Ο�@���@��T@�X@���@� �@�1'@� �@�1@�;d@�@��@�5?@ʏ\@��H@�ȴ@�%@�A�@��@���@�V@�%@ț�@�I�@Ǿw@���@�E�@�-@���@�hs@�1'@�\)@¸R@�ff@��@��@��7@�7L@��j@��@�bN@�9X@��@�ƨ@��@�|�@�dZ@�S�@�;d@�@��!@�E�@��@�`B@���@���@�j@�A�@�ƨ@�l�@�
=@��@���@���@�n�@�E�@���@��7@�G�@�&�@��u@�  @�33@�~�@�J@��@�/@�Ĝ@�Q�@� �@�\)@�"�@�ȴ@�E�@��^@���@��@���@�t�@�ȴ@�=q@��@��#@��^@���@�O�@��@��D@��;@���@�;d@��!@�n�@�{@��7@�7L@���@��@�Q�@���@�l�@�
=@��R@���@�G�@��/@���@�(�@�|�@�
=@��\@�V@�{@��#@���@�@���@�p�@�hs@�`B@��@��`@���@�z�@�Q�@�1'@�  @��@��@��@��@���@���@�C�@��\@�V@���@�%@�Ĝ@���@�Q�@�A�@�(�@�1@�l�@�C�@���@�v�@�$�@��@�@��#@��7@��@�%@��@��u@�I�@�(�@��;@�t�@��@��\@�E�@�=q@���@��h@�x�@�X@�7L@�Ĝ@�Q�@�1'@�1@���@��m@���@�;d@��@���@��H@���@��+@�E�@�J@�@�O�@�&�@���@��/@��9@��D@�j@�Z@�A�@�b@��
@���@�|�@�C�@�@��H@�^5@���@��@���@��^@���@�X@��9@�bN@�A�@�1'@�b@�ƨ@�+@���@�=q@���@�x�@��/@�r�@�bN@�A�@�  @���@�|�@�\)@�S�@�C�@�o@��@�V@��@��#@�@���@�x�@�x�@�`B@��@�z�@�I�@��;@�\)@�C�@�"�@���@��@��R@�n�@�=q@��T@��h@�`B@��@�I�@�  @�@\)@;d@~��@~ȴ@~�+@~E�@}��@}O�@}�@|�/@|�j@|Z@{�m@{dZ@{o@z^5@z-@y�^@yhs@yG�@x��@xĜ@x�@w��@w
=@v�+@v@up�@t�@s��@sS�@r�!@q��@pĜ@o��@o��@ol�@n��@n�@nȴ@n�R@n�+@nv�@n5?@m@mO�@l1@kC�@k@j�\@j-@jJ@i��@iX@i�@h��@h�@hA�@h �@g�w@g\)@f�+@e�h@d�@d�j@d�@dj@c��@c��@c33@b~�@b�@ahs@`��@`�u@`�@`bN@`1'@`b@_�;@_l�@_K�@_;d@^��@^�R@^5?@\�@\�@\�D@\(�@[�@[C�@[@Z��@Z�!@Zn�@Z=q@ZJ@Y�@Y��@Y�^@Y�^@Y��@Y�7@YG�@XĜ@XbN@XQ�@W�@W�@W;d@V�y@V��@V$�@U?}@T�@S�@SdZ@S@R�!@R=q@Q��@Q�#@Q�^@Qx�@P��@P�`@PĜ@PQ�@O�@O�@O�@O��@N��@N5?@N@M�-@M�@Mp�@M?}@MV@Lj@Kƨ@K��@KdZ@K33@J�@J��@J�!@J�\@J^5@J�@I��@H�`@HQ�@G�@G�@F�@FV@Ep�@D�@DZ@D1@C�@CC�@B�\@A��@Ahs@A7L@A%@@��@@�9@@ �@?�@?l�@?+@>��@>V@>5?@>$�@>@>@=�@=@=p�@<��@<��@;�
@;S�@;o@:�H@:��@:��@:�\@:~�@:=q@:�@9��@9�^@9hs@8��@8�@8r�@8Q�@81'@8  @7�w@7\)@6�@6ff@6V@6E�@65?@5�T@5��@5�h@5�@4��@4�@3��@3t�@333@2��@2~�@2�@1�@1��@1�@0�@/�@/�;@/�@/l�@.ȴ@.��@.5?@-@,�@,��@,z�@,j@,Z@,(�@+��@+ƨ@+��@+dZ@+dZ@+C�@+33@+@*��@*�\@*~�@*J@)��@)G�@(��@(�u@(bN@(Q�@(A�@(1'@'�@'|�@'\)@'K�@'+@&��@&$�@%�@%@%�@%`B@%?}@%?}@%�@$j@$1@#ƨ@#�F@#��@#t�@#t�@#C�@"�H@"�!@"n�@"�@"J@!��@!�@!�^@!��@!x�@!�@ ��@ A�@��@l�@;d@+@�@�@��@ȴ@��@v�@V@5?@{@�T@@@@�hA���A���A���A���A�ȴA���A���A���A���A�ȴA�ĜA���A���A�ȴA���A���A�ȴA���A���A�A���A���AӰ!AӺ^AӺ^Aӥ�Aӡ�Aӣ�Aӥ�AӶFAӲ-AӲ-AӬAӸRAӲ-A�ƨA�ƨA�ƨA�ĜA���AӺ^Aӟ�Aӧ�Aӡ�Aӗ�AӋDAӋDAӇ+AӉ7A�z�A�v�A�|�A�|�A�v�A�v�A�x�A�v�A�|�A�~�A�x�A�|�A�t�A�jA�`BA�^5A�\)A�^5A�VA�O�A�M�A�K�A�K�A�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A���A���A�ȴA���A�ƨAӸRAө�AӲ-AӲ-A�ĜA�Aӗ�A�|�A�z�A�\)A�G�A�5?A�/A�(�A�+A�-A�1'A�7LA�;dA�;dA�?}A�A�A�E�A�G�A�I�A�M�A�Q�A�\)A�jA�r�A�~�A�n�A�O�A�A�r�A�%AѴ9AЧ�A���A͓uA�XA��Aʣ�A�\)A���A�dZA��;A�v�A�A��TA�|�AƓuA�`BA�-A� �A���A��Aš�A�Q�Aĥ�Aĉ7A�l�A�"�Aá�A��A���A�^5A�bAhA�O�A��wA�1'A��A�l�A��A��-A�(�A�v�A���A��DA��^A�VA�A�n�A�r�A�A�A�A��FA�t�A��A�+A�M�A��hA���A���A���A�I�A��\A�O�A�A�Q�A���A�"�A�=qA�O�A�33A�JA��A�mAu��As33Ao�TAmS�Aj9XAcC�A_�wA^ffA\VA[%AZVAXI�ATJAP��ANZAM�AM/ALAJffAGK�AE+AD  AC33AA��A@�A?C�A>5?A=��A=�A<��A<�A:��A9ƨA9��A9�A7��A6ȴA5/A3��A1\)A.bNA,ȴA,^5A+�FA+XA+VA)33A(�A'dZA'/A&I�A&1'A$��A#?}A"��A"��A#
=A$�A&9XA&(�A%�;A%ƨA%�PA%l�A%&�A$��A#�mA"jA!��A �jA�A�mAQ�AQ�A�
A��A�9A��A9XA��AVA��A��At�A��A�PAx�A33A�`A�FA�AC�A�!AbA��A��A?}Ar�AS�Ar�Al�A�yA�;AoA �A��AO�A
��A
^5A	7LA �A�A�AĜAz�A��AG�AjA�A�#A�7A%A�jA�!A�\A��Az�AffAE�A(�A�A/AS�A;dA�A ��A ��A Q�@���@���@�^5@��@���@��D@�1@��@���@�o@�E�@��h@��j@�Z@��@��P@�;d@�ff@�V@���@�bN@�F@���@���@�M�@�@�O�@��`@���@�+@�/@��@�Ĝ@�@�A�@�!@��@��#@�x�@�bN@��@�@�\)@�
=@�!@�{@�^@噚@�7L@���@�D@�Q�@���@��;@㝲@�\)@�!@�^@��/@�p�@�p�@��;@���@݁@�O�@�X@�X@�`B@�p�@�7L@�Ĝ@ۮ@�{@��`@���@�b@�@��@��@���@ԓu@��@�S�@��H@�^5@�5?@�-@Ѳ-@�X@��@У�@ϕ�@�K�@�+@θR@Ώ\@Ο�@���@��T@�X@���@� �@�1'@� �@�1@�;d@�@��@�5?@ʏ\@��H@�ȴ@�%@�A�@��@���@�V@�%@ț�@�I�@Ǿw@���@�E�@�-@���@�hs@�1'@�\)@¸R@�ff@��@��@��7@�7L@��j@��@�bN@�9X@��@�ƨ@��@�|�@�dZ@�S�@�;d@�@��!@�E�@��@�`B@���@���@�j@�A�@�ƨ@�l�@�
=@��@���@���@�n�@�E�@���@��7@�G�@�&�@��u@�  @�33@�~�@�J@��@�/@�Ĝ@�Q�@� �@�\)@�"�@�ȴ@�E�@��^@���@��@���@�t�@�ȴ@�=q@��@��#@��^@���@�O�@��@��D@��;@���@�;d@��!@�n�@�{@��7@�7L@���@��@�Q�@���@�l�@�
=@��R@���@�G�@��/@���@�(�@�|�@�
=@��\@�V@�{@��#@���@�@���@�p�@�hs@�`B@��@��`@���@�z�@�Q�@�1'@�  @��@��@��@��@���@���@�C�@��\@�V@���@�%@�Ĝ@���@�Q�@�A�@�(�@�1@�l�@�C�@���@�v�@�$�@��@�@��#@��7@��@�%@��@��u@�I�@�(�@��;@�t�@��@��\@�E�@�=q@���@��h@�x�@�X@�7L@�Ĝ@�Q�@�1'@�1@���@��m@���@�;d@��@���@��H@���@��+@�E�@�J@�@�O�@�&�@���@��/@��9@��D@�j@�Z@�A�@�b@��
@���@�|�@�C�@�@��H@�^5@���@��@���@��^@���@�X@��9@�bN@�A�@�1'@�b@�ƨ@�+@���@�=q@���@�x�@��/@�r�@�bN@�A�@�  @���@�|�@�\)@�S�@�C�@�o@��@�V@��@��#@�@���@�x�@�x�@�`B@��@�z�@�I�@��;@�\)@�C�@�"�@���@��@��R@�n�@�=q@��T@��h@�`B@��@�I�@�  @�@\)@;d@~��@~ȴ@~�+@~E�@}��@}O�@}�@|�/@|�j@|Z@{�m@{dZ@{o@z^5@z-@y�^@yhs@yG�@x��@xĜ@x�@w��@w
=@v�+@v@up�@t�@s��@sS�@r�!@q��@pĜ@o��@o��@ol�@n��@n�@nȴ@n�R@n�+@nv�@n5?@m@mO�@l1@kC�@k@j�\@j-@jJ@i��@iX@i�@h��@h�@hA�@h �@g�w@g\)@f�+@e�h@d�@d�j@d�@dj@c��@c��@c33@b~�@b�@ahs@`��@`�u@`�@`bN@`1'@`b@_�;@_l�@_K�@_;d@^��@^�R@^5?@\�@\�@\�D@\(�@[�@[C�@[@Z��@Z�!@Zn�@Z=q@ZJ@Y�@Y��@Y�^@Y�^@Y��@Y�7@YG�@XĜ@XbN@XQ�@W�@W�@W;d@V�y@V��@V$�@U?}@T�@S�@SdZ@S@R�!@R=q@Q��@Q�#@Q�^@Qx�@P��@P�`@PĜ@PQ�@O�@O�@O�@O��@N��@N5?@N@M�-@M�@Mp�@M?}@MV@Lj@Kƨ@K��@KdZ@K33@J�@J��@J�!@J�\@J^5@J�@I��@H�`@HQ�@G�@G�@F�@FV@Ep�@D�@DZ@D1@C�@CC�@B�\@A��@Ahs@A7L@A%@@��@@�9@@ �@?�@?l�@?+@>��@>V@>5?@>$�@>@>@=�@=@=p�@<��@<��@;�
@;S�@;o@:�H@:��@:��@:�\@:~�@:=q@:�@9��@9�^@9hs@8��@8�@8r�@8Q�@81'@8  @7�w@7\)@6�@6ff@6V@6E�@65?@5�T@5��@5�h@5�@4��@4�@3��@3t�@333@2��@2~�@2�@1�@1��@1�@0�@/�@/�;@/�@/l�@.ȴ@.��@.5?@-@,�@,��@,z�@,j@,Z@,(�@+��@+ƨ@+��@+dZ@+dZ@+C�@+33@+@*��@*�\@*~�@*J@)��@)G�@(��@(�u@(bN@(Q�@(A�@(1'@'�@'|�@'\)@'K�@'+@&��@&$�@%�@%@%�@%`B@%?}@%?}@%�@$j@$1@#ƨ@#�F@#��@#t�@#t�@#C�@"�H@"�!@"n�@"�@"J@!��@!�@!�^@!��@!x�@!�@ ��@ A�@��@l�@;d@+@�@�@��@ȴ@��@v�@V@5?@{@�T@@@G�O�A���A���A���A���A�ȴA���A���A���A���A�ȴA�ĜA���A���A�ȴA���A���A�ȴA���A���A�A���A���AӰ!AӺ^AӺ^Aӥ�Aӡ�Aӣ�Aӥ�AӶFAӲ-AӲ-AӬAӸRAӲ-A�ƨA�ƨA�ƨA�ĜA���AӺ^Aӟ�Aӧ�Aӡ�Aӗ�AӋDAӋDAӇ+AӉ7A�z�A�v�A�|�A�|�A�v�A�v�A�x�A�v�A�|�A�~�A�x�A�|�A�t�A�jA�`BA�^5A�\)A�^5A�VA�O�A�M�A�K�A�K�A�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
B

�B

�B

�B

�B
xB
B
xB
xB

rB
DB
�B
�B
�B
(B
�B
�B
B
B
�B
�B
�B
�B
B
�B
�B
!bB
"�B
$B
%B
&�B
(�B
-B
3�B
8�B
A�B
C�B
A�B
@�B
;�B
7�B
2�B
0UB
$B
%FB
'B
#:B
&�B
&B
%FB
$�B
'�B
#�B
*eB
*�B
 �B
5�B
4�B
4�B
6�B
7�B
8RB
?�B
B�B
8�B
8RB
9XB
=qB
<�B
^�B
��B
�B
�B
�:B
�tB
��B
�NBB~B"hBOBBv�B��B�xB�xB��B��B��B�-B�B�$B��B�B�OBŢB��B�*B�hB�B�"B��BhsBH�B.B&LB�B
��B
�B
�LB
�B
�B
tTB
RTB
)_B	�B	��B	��B	��B	��B	{�B	k�B	bB	`vB	XB	T,B	[�B	K�B	NpB	A�B	>�B	<B	C�B	@OB	C�B	9�B	8B	8�B	?�B	K�B	MjB	V�B	\�B	bNB	g�B	qvB	tTB	{B	��B	�~B	��B	��B	�4B	��B	r�B	Y�B	\)B	`BB	`B	d�B	i�B	p�B	m]B	iyB	jB	s�B	�uB	�xB	�1B	�B	�4B	��B	��B	��B
YB
�B
qB
IB
B
�B
!bB
"�B
 �B
	B
@B	��B	ޞB	бB	�#B	��B	�|B
  B	�]B	��B	ܒB	�B	�pB	��B
�B
$B
%�B
&�B
$tB
!�B
�B
�B
OB
�B
�B
MB
"B	�]B	�8B	�cB	�B	�B	ܒB	רB	ҽB	�XB	�B	�B	��B	ĜB	�jB	�XB	ʌB	ȴB	ǮB	ȀB	�KB	ɺB	ɆB	�)B	��B	��B	̘B	�0B	�dB	�vB	՛B	��B	�KB	��B	��B	�/B	��B	�sB	��B	�yB	��B	��B	��B	�+B	�fB	��B	�2B	��B	��B	��B	�rB	��B	�>B	��B	��B	��B	�B	�xB	�DB	��B	�xB	�B	��B	�>B	�>B	�fB	�B	�xB	�>B	�rB	�>B	�	B	��B	��B	��B	�2B	��B	��B	�B	�rB	�DB	��B	�B	�JB	��B	�B	�xB	��B	��B	�B	��B	�JB	��B	�B	�VB	��B
;B
{B
�B
�B
MB
�B
MB
YB
�B	��B	�>B	��B	�lB	��B	�rB	��B	�"B	��B	��B	��B	��B	�%B	��B	�oB	��B	��B	�B	��B	�B	�B	��B	�KB	��B	�B	�B	�B	�B	��B	�]B	�)B	�B	��B	�B	��B	��B	�8B	�+B	��B	�B	�B	�ZB	�fB	�	B	�ZB	��B	�8B	�B	��B	��B	��B	��B	�lB
B
SB
�B
�B
�B
�B
�B
�B
�B
YB
%B
B
B
�B
B
 iB
  B
�B
	B
	7B
	7B
	7B
	7B
�B
	B
�B
	�B
	�B
	lB
	lB

	B

=B

=B

=B

�B
B

�B

�B

�B

�B
B
xB
DB
B
DB
xB
B
B
�B
�B
B
DB
B

�B
xB
�B
�B
�B
JB
~B
�B
B
PB
B
�B
"B
�B
\B
.B
bB
hB
B
:B
�B
�B
B
B
oB
�B
uB
B
�B
�B
FB
B
MB
MB
B
�B
�B
B
$B
�B
SB
�B
�B
�B
$B
�B
�B
_B
�B
�B
_B
1B
1B
�B
�B
	B
�B
qB
B
CB
�B
xB
B
B
B
OB
OB
B
�B
OB
�B
�B
OB
�B
IB
xB
xB
�B
B
�B
�B
�B
�B
B
IB
OB
�B
~B
~B
�B
OB
OB
B
B
�B
B
IB
~B
�B
�B
VB
�B
�B
�B
�B
�B
�B
VB
 �B
 �B
 �B
 �B
 �B
 \B
!�B
!-B
!-B
 �B
 �B
!bB
!bB
"4B
!�B
#B
#�B
$B
$@B
$B
$�B
$�B
$�B
$tB
$tB
$@B
$tB
$�B
$tB
%FB
%�B
%�B
&LB
%zB
%FB
%zB
%zB
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
+�B
+6B
+B
+B
+�B
,�B
-B
-B
,�B
,�B
-CB
-�B
/OB
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/B
/OB
0�B
0�B
0�B
0�B
1[B
1�B
1�B
2-B
1�B
2�B
2aB
2aB
2aB
4B
3�B
4nB
4�B
4�B
5tB
5?B
5?B
5?B
5�B
6zB
6�B
6�B
6�B
7�B
8�B
8�B
9$B
9�B
9�B
:^B
:^B
:*B
:�B
:�B
:�B
;dB
<B
<B
<B
<jB
=B
=<B
=�B
=�B
>wB
?}B
@B
@�B
@�B
@�B
A B
@�B
@�B
A B
A B
A B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C-B
C�B
C�B
C�B
DgB
DgB
D3B
DgB
D�B
DgB
E9B
F?B
F�B
FtB
FB
F?B
F�B
F�B
F�B
GEB
GEB
HB
H�B
H�B
HKB
HKB
H�B
H�B
H�B
H�B
H�B
IRB
IB
H�B
I�B
K)B
J�B
J�B
K)B
K�B
K^B
L0B
K�B
K�B
LdB
LdB
L�B
LdB
MB
L�B
MB
L�B
L�B
L�B
M�B
M6B
MjB
M�B
M�B
N<B
M�B
N<B
NpB
OBB
PHB
PHB
PHB
QB
QNB
QB
Q�B
Q�B
QNB
Q�B
R B
R B
RTB
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T,B
S�B
T�B
U2B
UgB
U�B
UgB
VB
U�B
VB
U�B
U�B
VB
VmB
V�B
W�B
W�B
XB
XB
X�B
Y�B
ZQB
Y�B
Z�B
Z�B
Z�B
[�B
[�B
\]B
\]B
\�B
\�B
\�B
\�B
]�B
]�B
^B
^�B
^�B
_;B
_;B
_�B
_;B
_�B
_�B
`�B
`�B
aHB
bNB
b�B
b�B
b�B
cTB
c B
cTB
c B
cTB
c�B
c�B
c�B
c�B
dZB
d&B
c�B
d&B
d&B
d&B
dZB
d�B
e`B
e�B
e�B
e�B
e`B
e�B
e�B
e�B
f2B
gB
g8B
g�B
gmB
g�B
h>B
h
B
h>B
h>B
hsB
h�B
iyB
i�B
i�B
i�B
i�B
j�B
jB
jB
kB
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l"B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
l�B
m�B
m)B
m�B
ncB
n�B
o B
n�B
n�B
n�B
n�B
o�B
o�B
oiB
o�B
pB
pB
pB
poB
p;B
p�B
qB
p�B
qB
qvB
q�B
qvB
q�B
qvB
q�B
qvB
q�B
r|B
rGB
sB
sMB
r�B
sMB
r�B
sB
sMB
sB
s�B
s�B
t�B
t�B
tTB
t�B
tTB
t�B
tTB
tTB
t�B
t�B
uZB
u%B
u%B
uZB
u�B
u�B
u�B
v+B
v`B
xB
B
�B
	lB
�B

�B

rB
�B

�B
	�B

�B
B
	�B
DB

=B

�B
DB

�B
	lB
B
DB
B
�B

�B
�B
�B
PB
DB

�B

�B

=B
PB
xB
	�B
�B
fB
	7B
xB
DB
	�B
�B
�B
DB
JB
�B
~B
�B
 B
JB
�B
�B
xB
"B
(B
PB
�B
�B
�B
PB
�B
�B
�B
:B
VB
�B
(B
�B
.B
�B
�B
\B
�B
"G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              B
!B
B

�B
 B
CB
�B
�B
iB
#B
-B
�B
VB
�B
�B
�B
@B
B
<B
B
�B
wB
�B
�B
B
sB
�B
!PB
"�B
$B
%B
&�B
(]B
,�B
3fB
8hB
A�B
D�B
C�B
C�B
>�B
: B
9�B
;�B
,�B
2PB
)�B
%FB
(�B
(�B
(>B
'B
)�B
&�B
.8B
,}B
 �B
6�B
5�B
4�B
7�B
8}B
9hB
A*B
EOB
9XB
8�B
:�B
?�B
;�B
_rB
�-B
��B
�<B
��B
�B
��B
�B�BQB5qBe�B��B��B��B��B��B��B��B�B��B�eB�?B�OB�tBҋBƞB��B��B��B�#B�BzBQ�B2B2EB&�BDB
޵B
��B
��B
�B
�hB
rEB
K�B	�B	�7B	��B	��B	�B	��B	q�B	i�B	eyB	[�B	\�B	j�B	W�B	W�B	E&B	@lB	@�B	JoB	K�B	LB	>�B	;�B	=�B	F�B	O-B	Q}B	XMB	^YB	d�B	k�B	v(B	xoB	{B	�4B	�cB	��B	�MB	�OB	�B	|�B	_�B	]�B	byB	a�B	f�B	o�B	t^B	o�B	j{B	m�B	tbB	��B	��B	�MB	�B	��B	�zB	�XB	�*B
\B
<B
B
�B
:B
!�B
$.B
'�B
#nB
^B
	B	��B	�B	�cB	�XB	��B	ެB
B	�wB	�XB	�$B	ֲB	�B	��B
�B
$?B
&B
'xB
%�B
%�B
�B
 �B
 YB
�B
]B
$B
B
@B	��B	�VB	��B	�+B	��B	�8B	֓B	��B	�\B	�*B	�9B	ɡB	��B	��B	�vB	�&B	�*B	��B	�\B	�B	�B	�RB	̄B	�B	��B	̃B	��B	�^B	�HB	�GB	��B	��B	ފB	�\B	ݖB	��B	�hB	�"B	��B	�:B	��B	�
B	�PB	�/B	�B	��B	��B	�0B	�QB	�B	�B	�FB	��B	��B	��B	��B	�;B	��B	�CB	�	B	�hB	��B	�B	��B	�B	��B	��B	�^B	��B	��B
 xB	�6B	�>B	��B	�B	��B	��B	��B	��B	�pB	�KB	�JB	�?B	�B	��B	�\B	��B	��B	��B	�FB	��B	�2B	�3B	�)B
�B
wB
�B
	>B
B
 �B
B

uB

<B	��B	��B	��B	�oB	��B	��B	��B	��B
�B
 �B	��B	��B	��B	�FB	�B	�(B	�B	�B	�2B	�pB	�hB	�'B	�B	�7B	�MB	�mB	��B	�B	�^B	�!B	��B	�B	�.B	��B	�B	��B	��B	��B	�B	�B	��B	�B	��B	��B	��B	�$B	�fB	��B	��B
�B	��B	�>B	�lB
�B
�B
�B
�B
eB
	�B
�B
9B
pB
B
3B
�B
.B
sB
�B
 �B
 �B
�B
	�B
	�B
	zB
	�B
	�B
	B
	7B
	'B
	�B
	�B
	�B
	�B

�B

�B

�B
?B
�B
mB
B

�B
}B
B
�B
�B
^B
lB
�B
�B
�B
�B
$B
�B
B
PB
sB
B
JB
�B
|B
�B
B
�B
�B
�B
�B
B
�B
�B
B
�B
IB
�B
~B
�B
uB
B
'B
�B
�B
UB
�B
B
�B
�B
zB
B
+B
�B
�B
�B
EB
�B
>B
�B
ZB
"B
�B
�B
B
6B
�B
�B
WB
yB
�B
�B
WB
NB
KB
2B
"B
�B
B
�B
�B
B
�B
cB
yB
AB
UB
VB
)B
6B
�B
FB
 FB
�B
fB
�B
B
�B
PB
DB
�B
WB
1B
0B
�B
rB
B
�B
�B
�B
�B
5B
�B
uB
�B
dB
�B
B
�B
�B
nB
�B
0B
�B
 UB
�B
 B
 gB
 tB
!�B
 �B
!$B
 �B
 �B
!1B
"_B
!�B
!�B
!AB
![B
!�B
"	B
"�B
"�B
$B
$DB
$�B
$�B
$vB
%B
%.B
$�B
$�B
$�B
$�B
%B
$�B
%B
%�B
&]B
'B
'B
%�B
%�B
%�B
%�B
']B
)eB
)IB
(�B
(�B
)*B
)�B
*dB
+B
+DB
+�B
+�B
,	B
,�B
+sB
+cB
+�B
,�B
,�B
-ZB
-,B
-B
-&B
-�B
.�B
0(B
0.B
/�B
0B
/�B
/�B
/�B
0�B
0�B
/�B
0UB
1�B
1=B
0�B
1XB
1�B
1�B
2>B
2�B
2�B
3�B
2�B
36B
4;B
4�B
4	B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
6AB
6�B
72B
6�B
7eB
8yB
9!B
93B
9�B
:AB
:B
:�B
:�B
:�B
:�B
;.B
;xB
<LB
<�B
<�B
<�B
=ZB
=�B
>	B
>�B
>�B
?�B
@�B
@eB
@�B
AB
AB
A7B
AB
AB
A:B
AhB
A�B
AwB
B�B
CKB
CB
C4B
C%B
C�B
CxB
DB
C�B
DB
D�B
D�B
DaB
D�B
EB
E?B
F'B
F�B
F�B
F�B
FUB
F�B
GB
GUB
G�B
G�B
G�B
H�B
H�B
H�B
HsB
HB
H�B
H�B
IXB
IB
IB
I�B
IkB
IB
J�B
KmB
J�B
J�B
K�B
LB
K�B
LcB
L#B
L?B
L�B
L�B
L�B
L�B
MB
L�B
MB
L�B
L�B
M0B
NB
M]B
M�B
M�B
NTB
N�B
N/B
N�B
O�B
PtB
P�B
PB
P�B
Q}B
Q�B
QjB
Q�B
Q�B
Q�B
RrB
R>B
RTB
R�B
R�B
S8B
R�B
R�B
S�B
T@B
S�B
TB
T0B
TB
T7B
TvB
T�B
U�B
UsB
U�B
U�B
U�B
V9B
U�B
V-B
VB
V'B
V�B
W5B
WWB
XdB
X|B
XjB
X�B
Y�B
Z�B
Z�B
ZRB
[B
[B
[�B
\�B
\dB
\�B
\�B
\�B
\�B
]5B
]yB
]�B
]�B
^�B
_(B
^�B
_RB
_aB
_�B
_XB
`B
`:B
a)B
a\B
bB
b�B
c5B
c!B
cB
cyB
c9B
cnB
cfB
c~B
c�B
c�B
d%B
d�B
d�B
d>B
dB
dOB
dcB
dwB
d�B
eSB
e�B
e�B
e�B
e�B
e�B
e�B
fB
fB
f�B
g�B
g�B
g�B
g�B
hB
h�B
huB
h{B
h�B
iB
i�B
jB
i�B
i�B
j7B
j�B
j�B
j�B
kB
k�B
lCB
k�B
lB
k�B
k�B
l-B
l]B
l^B
l�B
l�B
l�B
l�B
l�B
mB
m.B
mMB
mAB
m�B
m�B
n{B
n�B
n�B
oB
n�B
n�B
o B
oIB
o�B
o�B
o�B
pCB
p�B
pEB
pJB
p�B
phB
p�B
qB
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rNB
r�B
r�B
spB
scB
r�B
sgB
s!B
sEB
s�B
s�B
s�B
t0B
uB
t�B
t�B
t�B
tlB
t�B
t�B
t�B
t�B
t�B
u�B
uPB
uRB
u�B
u�B
u�B
u�B
v_G�O�B
xB
B
�B
	lB
�B

�B

rB
�B

�B
	�B

�B
B
	�B
DB

=B

�B
DB

�B
	lB
B
DB
B
�B

�B
�B
�B
PB
DB

�B

�B

=B
PB
xB
	�B
�B
fB
	7B
xB
DB
	�B
�B
�B
DB
JB
�B
~B
�B
 B
JB
�B
�B
xB
"B
(B
PB
�B
�B
�B
PB
�B
�B
�B
:B
VB
�B
(B
�B
.B
�B
�B
\B
�B
"G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<BeB<#�
<]�m<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�]<�E=<I��<R5=<#�
<^��<���<#�
<#�
<#�
<w]�<B�
<q�<��<�f�<[�4<^3+<H��<#�
<#�
<]>u<E�<�`�<#�
<#�
<M�9<l��<��W<fU�<#�
<#�
<��s<���=b�=
q<)�<RI#<1�#<f��<�S<^Ge<#�
<#�
<#�
<#�
<#�
<{S<P�l<'9[<#�
<#�
<#�
<#�
<E�Z<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-)<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021061003245120210610032451IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021062004005420210620040054QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021062004005420210620040054QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364720220422133647IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064320220426160643IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064320220426160643IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064320220426160643IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                