CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92021-02-17T04:36:57Z creation; 2022-05-04T12:55:30Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         iPRIMARY | https://orcid.org/0000-0001-5113-1068 | Deborah West-Mack, Woods Hole Oceanographic Institution         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7d   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7t   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7x   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7|   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  84   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8d   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8h   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8l   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8p   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9    POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9$   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9,   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :,   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :0   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :4   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :8   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :<   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  a�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �,   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �,   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �,   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �,   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210217043657  20220504085530  1901663 US ARGO PROJECT                                                 BRECK OWENS, STEVE JAYNE, P.E. ROBBINS                          PRES            TEMP            PSAL               �A   AO  5137                            2C  D   S2A                             7179                            SBE602 V1.3                     854 @��!��G1   @�ʪ�� �C�٩T"@D���o�61   GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                           A   A   A   ?�\)?��H@=p�@�G�@�  @�  @�G�A   A\)A�RA?\)A_\)A�  A�  A�  A�Q�A�  AϮA�  A�B   B  B  B  B (�B((�B0(�B8(�B@(�BH  BO�
BX  B`  Bh  Bo�
Bw�
B�  B�  B�  B�{B�{B�{B�  B��B��B��B�  B�  B�{B�  B�  B�{B�{B�  B�  B�  B�  B��B�  B�{B�  B��B��B��B�  B�{B�{B�{C   C  C  C  C  C

=C  C  C  C  C  C  C  C��C��C��C   C"
=C$
=C&  C(  C*  C,  C.
=C0  C2  C4  C5��C8  C:
=C<  C>  C@
=CB  CC��CF  CH  CJ  CL
=CN  CO��CR  CT  CV  CX  CZ  C\  C^  C_��Cb  Cc��Cf  Ch
=Cj
=Cl
=Cn  Cp  Cr  Ct  Cv
=Cx  Cz  C{��C}��C��C�  C�C�C���C���C�  C�  C�  C�C�C���C�  C�  C�  C���C�  C�C�  C�  C�  C���C�  C�C�  C�  C�C�C�  C���C���C���C���C���C�  C�  C�C�
=C�C�C�C�C�  C���C���C���C���C���C���C���C�  C�C�  C�  C���C�  C�  C�  C�C�  C���C���C���C���C���C���C���C�  C�  C�  C���C�  C�C�  C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�C�C�  C�C�C�  C���C�  C���C���C���C���C���C�  C���C�  C�
=C�
=C�C�C�  C�  C�  C�  C�  C�C�  C���C���C�  C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C���D � D  D� D  D� D�D��D  D� D�qD}qD  D� D  D� D  D� D	�D	� D
  D
��D  D}qD�qD� D  D� D  D��D�D� D�qD� D  D}qD  D� D�qD� D�D}qD�qD}qD  D� D  D��D  D� D  D� D  D� D�qD� D  D}qD�D��DD�D  D}qD�qD }qD!  D!��D"  D"}qD#  D#}qD$  D$� D%  D%�D&�D&��D'�D'}qD(  D(��D)�D)� D)�qD*}qD*�qD+��D,  D,}qD,�qD-}qD.  D.��D/  D/� D0�D0��D1  D1� D2  D2}qD3  D3}qD4  D4� D4�qD5� D6�D6��D7  D7��D8  D8}qD9  D9��D:  D:� D;  D;}qD;�qD<� D=  D=��D>�D>� D>�qD?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD�qDE� DE�qDF}qDF�qDG� DH  DH� DI  DI��DJ�DJ� DK  DK� DK�qDL}qDM  DM��DN�DN� DO  DO� DP�DP��DP�qDQ}qDR  DR� DR�qDS}qDS�qDT}qDU  DU� DV�DV��DW  DW� DX�DX� DX�qDY� DZ  DZ}qD[  D[� D\  D\� D]�D]��D^  D^� D_  D_��D`�D`}qD`�qDa}qDb  Db}qDb�qDc��Dc�qDd}qDe  De� Df�Df� Dg  Dg��Dh�Dh� Di�Di��Dj�Dj��Dk  Dkz�Dk��Dl� Dm�Dm� Dm�qDn}qDo  Do��Dp  Dp}qDp�qDq}qDq�qDr� DsDs��Dt  Dt� Du  Du� Dv  Dv� Dw�Dw� Dx  Dx� Dy  Dy}qDz  Dz� Dz�qD{}qD|  D|��D}  D}� D~  D~� D  D}qD�  D�@ D�� D�� D�  D�>�D��HD�� D�  D�AHD�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D�� D�  D�>�D�� D�� D�  D�@ D��HD��HD�HD�AHD�� D���D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D��HD��HD�  D�AHD�~�D���D�  D�>�D�� D�� D���D�@ D�� D���D�  D�@ D�� D�� D�  D�AHD�� D���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�AHD�� D���D�  D�AHD�� D�� D�  D�>�D�~�D���D�  D�>�D�~�D�� D���D�>�D�� D���D�  D�@ D�~�D���D���D�>�D�~�D���D���D�@ D�~�D���D�  D�@ D��HD��HD���D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�� D�� D���D�>�D�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D��HD��D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D�� D�HD�AHD��HD��HD�  D�@ D�� D���D���D�@ D�� D�� D�  D�AHD��HD�� D�  D�AHD��HD�� D��qD�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D�~�D��qD�  D�AHD�~�D�� D�  D�@ D�� D���D�  D�@ D��HD�� D���D�>�D�~�D�� D��D�B�D�� D�� D��D�B�D�� D�� D�HD�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�~�D��qD���D�>�D�� D�� D�  D�AHD��HD��HD���D�>�D�� D��HD�  D�>�D�� D���D���D�@ D�� D���D���D�>�D�~�D�� D�HD�AHD��HD��HD�HD�@ D�� D��HD�  D�>�D�~�D�� D�  D�@ D�� D���D��qD�=qD�~�D¾�D�  D�>�D�~�Dþ�D�  D�@ DĀ Dľ�D���D�>�D�~�D�� D�HD�>�D�}qDƾ�D�  D�>�D�~�D��HD�  D�AHDȀ DȾ�D�  D�AHDɁHD�� D�HD�AHDʀ Dʾ�D�HD�AHDˁHD��HD�HD�AHD́HD�� D�  D�@ D�~�D�� D���D�>�D΀ D�� D��qD�=qD�~�DϾ�D�  D�@ DЁHD��HD�  D�>�Dр D��HD�HD�@ D�~�D�� D�  D�@ DӀ D�� D�  D�>�D�~�D�� D�  D�AHDՀ Dվ�D�  D�@ DցHD�D�HD�@ D׀ D׾�D�  D�>�D؀ D��HD�  D�@ Dـ D��HD�HD�@ Dڀ D��HD�  D�@ D�~�D۾�D�  D�AHD܁HD��HD�HD�AHD݀ D�� D�  D�>�Dހ D��HD�  D�@ D߀ D�� D�  D�AHD�� D�� D�  D�@ D�HD��HD�  D�@ D�HD�� D���D�>�D� D��HD�  D�@ D� D侸D���D�@ D� D徸D���D�>�D� D澸D���D�>�D� D��HD�  D�>�D� D��HD�HD�@ D� D�� D�  D�AHD�HD��HD�  D�@ D� D�� D�  D�@ D� D쾸D�  D�AHD� D�� D�  D�>�D�~�D�� D�HD�AHD�HD��HD�HD�AHD�� D��HD�HD�AHD� D�� D�  D�@ D� D�D���D�@ D�~�D�� D�  D�>�D� D�� D���D�>�D�~�D�� D���D�>�D�� D�� D�HD�@ D�� D��HD�  D�@ D��HD��HD�  D�@ D��HD��D�  D�@ D��HD��HD�fD�!H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�\)?��H@=p�@�G�@�  @�  @�G�A   A\)A�RA?\)A_\)A�  A�  A�  A�Q�A�  AϮA�  A�B   B  B  B  B (�B((�B0(�B8(�B@(�BH  BO�
BX  B`  Bh  Bo�
Bw�
B�  B�  B�  B�{B�{B�{B�  B��B��B��B�  B�  B�{B�  B�  B�{B�{B�  B�  B�  B�  B��B�  B�{B�  B��B��B��B�  B�{B�{B�{C   C  C  C  C  C

=C  C  C  C  C  C  C  C��C��C��C   C"
=C$
=C&  C(  C*  C,  C.
=C0  C2  C4  C5��C8  C:
=C<  C>  C@
=CB  CC��CF  CH  CJ  CL
=CN  CO��CR  CT  CV  CX  CZ  C\  C^  C_��Cb  Cc��Cf  Ch
=Cj
=Cl
=Cn  Cp  Cr  Ct  Cv
=Cx  Cz  C{��C}��C��C�  C�C�C���C���C�  C�  C�  C�C�C���C�  C�  C�  C���C�  C�C�  C�  C�  C���C�  C�C�  C�  C�C�C�  C���C���C���C���C���C�  C�  C�C�
=C�C�C�C�C�  C���C���C���C���C���C���C���C�  C�C�  C�  C���C�  C�  C�  C�C�  C���C���C���C���C���C���C���C�  C�  C�  C���C�  C�C�  C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�C�C�  C�C�C�  C���C�  C���C���C���C���C���C�  C���C�  C�
=C�
=C�C�C�  C�  C�  C�  C�  C�C�  C���C���C�  C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C���D � D  D� D  D� D�D��D  D� D�qD}qD  D� D  D� D  D� D	�D	� D
  D
��D  D}qD�qD� D  D� D  D��D�D� D�qD� D  D}qD  D� D�qD� D�D}qD�qD}qD  D� D  D��D  D� D  D� D  D� D�qD� D  D}qD�D��DD�D  D}qD�qD }qD!  D!��D"  D"}qD#  D#}qD$  D$� D%  D%�D&�D&��D'�D'}qD(  D(��D)�D)� D)�qD*}qD*�qD+��D,  D,}qD,�qD-}qD.  D.��D/  D/� D0�D0��D1  D1� D2  D2}qD3  D3}qD4  D4� D4�qD5� D6�D6��D7  D7��D8  D8}qD9  D9��D:  D:� D;  D;}qD;�qD<� D=  D=��D>�D>� D>�qD?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD�qDE� DE�qDF}qDF�qDG� DH  DH� DI  DI��DJ�DJ� DK  DK� DK�qDL}qDM  DM��DN�DN� DO  DO� DP�DP��DP�qDQ}qDR  DR� DR�qDS}qDS�qDT}qDU  DU� DV�DV��DW  DW� DX�DX� DX�qDY� DZ  DZ}qD[  D[� D\  D\� D]�D]��D^  D^� D_  D_��D`�D`}qD`�qDa}qDb  Db}qDb�qDc��Dc�qDd}qDe  De� Df�Df� Dg  Dg��Dh�Dh� Di�Di��Dj�Dj��Dk  Dkz�Dk��Dl� Dm�Dm� Dm�qDn}qDo  Do��Dp  Dp}qDp�qDq}qDq�qDr� DsDs��Dt  Dt� Du  Du� Dv  Dv� Dw�Dw� Dx  Dx� Dy  Dy}qDz  Dz� Dz�qD{}qD|  D|��D}  D}� D~  D~� D  D}qD�  D�@ D�� D�� D�  D�>�D��HD�� D�  D�AHD�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D�� D�  D�>�D�� D�� D�  D�@ D��HD��HD�HD�AHD�� D���D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D��HD��HD�  D�AHD�~�D���D�  D�>�D�� D�� D���D�@ D�� D���D�  D�@ D�� D�� D�  D�AHD�� D���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�AHD�� D���D�  D�AHD�� D�� D�  D�>�D�~�D���D�  D�>�D�~�D�� D���D�>�D�� D���D�  D�@ D�~�D���D���D�>�D�~�D���D���D�@ D�~�D���D�  D�@ D��HD��HD���D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�� D�� D���D�>�D�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D��HD��D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D�� D�HD�AHD��HD��HD�  D�@ D�� D���D���D�@ D�� D�� D�  D�AHD��HD�� D�  D�AHD��HD�� D��qD�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D�~�D��qD�  D�AHD�~�D�� D�  D�@ D�� D���D�  D�@ D��HD�� D���D�>�D�~�D�� D��D�B�D�� D�� D��D�B�D�� D�� D�HD�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�~�D��qD���D�>�D�� D�� D�  D�AHD��HD��HD���D�>�D�� D��HD�  D�>�D�� D���D���D�@ D�� D���D���D�>�D�~�D�� D�HD�AHD��HD��HD�HD�@ D�� D��HD�  D�>�D�~�D�� D�  D�@ D�� D���D��qD�=qD�~�D¾�D�  D�>�D�~�Dþ�D�  D�@ DĀ Dľ�D���D�>�D�~�D�� D�HD�>�D�}qDƾ�D�  D�>�D�~�D��HD�  D�AHDȀ DȾ�D�  D�AHDɁHD�� D�HD�AHDʀ Dʾ�D�HD�AHDˁHD��HD�HD�AHD́HD�� D�  D�@ D�~�D�� D���D�>�D΀ D�� D��qD�=qD�~�DϾ�D�  D�@ DЁHD��HD�  D�>�Dр D��HD�HD�@ D�~�D�� D�  D�@ DӀ D�� D�  D�>�D�~�D�� D�  D�AHDՀ Dվ�D�  D�@ DցHD�D�HD�@ D׀ D׾�D�  D�>�D؀ D��HD�  D�@ Dـ D��HD�HD�@ Dڀ D��HD�  D�@ D�~�D۾�D�  D�AHD܁HD��HD�HD�AHD݀ D�� D�  D�>�Dހ D��HD�  D�@ D߀ D�� D�  D�AHD�� D�� D�  D�@ D�HD��HD�  D�@ D�HD�� D���D�>�D� D��HD�  D�@ D� D侸D���D�@ D� D徸D���D�>�D� D澸D���D�>�D� D��HD�  D�>�D� D��HD�HD�@ D� D�� D�  D�AHD�HD��HD�  D�@ D� D�� D�  D�@ D� D쾸D�  D�AHD� D�� D�  D�>�D�~�D�� D�HD�AHD�HD��HD�HD�AHD�� D��HD�HD�AHD� D�� D�  D�@ D� D�D���D�@ D�~�D�� D�  D�>�D� D�� D���D�>�D�~�D�� D���D�>�D�� D�� D�HD�@ D�� D��HD�  D�@ D��HD��HD�  D�@ D��HD��D�  D�@ D��HD��HD�fD�!H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AhE�AhVAhVAhVAhQ�AhVAhM�AhQ�AhI�AhA�AhI�AhbNAhz�Ah�+Ah�uAh�uAh��Ah��Ah��Ah�Ah��Ah1AgS�AfĜAe�Aet�Ac��AcoAb�+Aa�-A`JA^�9A^(�A]��A]\)A]K�A]7LA]�A]
=A\�yA\�jA\��A\�DA\bNA\-A\1AZ��AZ��AZ�9AZ��AZ�AZz�AZr�AZffAZZAZQ�AZ{AY�AY�#AY��AY�^AY�AY�AY�AY�FAY�^AY�FAY�AY�AY��AY�-AY��AY�
AY��AY&�AX��AXVAW�-AWG�AV��AU��AUx�AUoAT��AT�uATbNAS�
AS�AR�/AR�ARZAR$�AQ��AQ��AQG�AP�9APbNAPz�AP^5AP��AP�9AQXAQx�AP  AN��AN^5ALr�AJv�AG��AF��AFv�AE�#AEhsAE&�AE�AE&�AD��ADI�AC�AC�;AC�FAC\)AC33ACoAB�yABv�ABbAA��AA�mAA�TAA�
AA�FA@9XA?�hA?33A>�A>bA=x�A=dZA<ZA;�A:�A9l�A9G�A9�A8z�A8  A7dZA6�`A6=qA5��A4��A4z�A4{A3��A3G�A2��A2 �A1��A1O�A1"�A1�A2$�A2n�A25?A1�mA1A1�A1G�A0��A0bA/�A/S�A.��A.�RA.A-XA,�DA+dZA*�yA*$�A)\)A)K�A(z�A'��A'��A'��A'��A'��A'ƨA'�A'��A'O�A&�DA%`BA$�A$�A"��A"1'A"  A!�wA!��A!p�A!/A!A �HA ��A bNA��A�A=qA7LA�9A?}A�9A�mA��A(�A��A��A�AK�A�yAffA �A�mA��A+AA�jA��Ar�AQ�AbA��A��AJA?}A1'A��A��A��AdZAXA7LAVA��A�`AȴAv�AZA-AA�FA\)AĜAr�AE�A  AƨA��A�7A"�A
�A	�A	hsAQ�A��An�AbA�A�AĜA��A�\Ar�AZA�A1A�FAC�A�A��A�A ��@�z�@�^5@���@�G�@� �@�ȴ@�O�@���@��@�b@�P@�+@���@���@�l�@���@�"�@��@�\)@ݲ-@ܣ�@�n�@���@�\)@�~�@�{@ղ-@ԋD@��@ӶF@�dZ@�C�@�o@ҟ�@щ7@�G�@�G�@�O�@�O�@�O�@�?}@��@ϝ�@Χ�@Ο�@Ώ\@�V@�=q@�$�@���@�O�@�/@�/@�&�@��@�V@��@�z�@�1@˶F@˅@�dZ@�l�@�\)@�
=@�n�@ɡ�@�Z@Ǯ@�|�@�"�@��@�5?@őh@��@�I�@�b@Ý�@���@�5?@���@��@�O�@�&�@��D@�+@�J@�x�@�Z@���@��@���@�9X@��F@���@�t�@�33@��@�=q@��@���@��P@�ȴ@���@��/@�I�@��@�l�@�=q@���@��h@��@�r�@�b@�dZ@�+@���@�J@�@��-@�V@���@��@�K�@��@�=q@���@�x�@��@��/@�z�@��@���@�C�@��+@�$�@��7@�hs@�?}@��@��@���@��@���@��u@�ƨ@�\)@�33@���@��!@�~�@�5?@�@���@��@�?}@�V@��/@�Ĝ@�r�@��F@�|�@�dZ@��@��@���@��+@�~�@�ff@�=q@�$�@�@���@�bN@��F@�\)@��@��H@���@���@���@�v�@�J@�hs@���@�"�@���@���@�O�@��F@�@�ff@�J@�@�x�@�x�@�7L@���@�I�@���@��;@���@�t�@�;d@���@��!@�~�@�-@�p�@��@��u@�r�@���@��#@�Ĝ@�|�@���@��@��/@���@���@�S�@��@���@�j@�Q�@�(�@��@� �@�(�@�(�@� �@�P@K�@
=@~��@~ȴ@}`B@|��@|�@|9X@{��@{t�@{t�@{dZ@y��@v��@vV@u�h@t1@s@qX@nff@n�+@n�@n�+@m�@pb@qX@rM�@r~�@r��@r�!@q7L@o\)@n�+@n$�@m��@l�/@l�/@m/@l9X@kS�@k"�@j�@j��@k@kC�@kS�@kS�@kS�@kS�@k��@k�
@lI�@l��@m`B@mp�@n{@n�+@n��@n�y@nȴ@nȴ@n�+@n$�@i�@g�@fE�@fV@e�@d9X@b��@a��@c��@c��@d1@d�D@d��@b��@b^5@a�#@`r�@_|�@_|�@^�y@\��@[�
@ZJ@Y�^@Y�#@Z�!@\�@]��@^��@a��@c��@ct�@c@b��@ax�@a&�@a7L@`��@`��@a��@a�7@ax�@a�@`Ĝ@`bN@` �@_�@_l�@_+@^�@^�+@^V@^E�@^E�@^$�@^{@]�T@]��@[��@[t�@[�@Z�\@Y�@Xr�@W��@Vȴ@V�@W;d@V�@U��@U��@U�-@U@U�-@U`B@Tj@T�@T�@TI�@T�@St�@St�@Tj@U`B@U��@U/@T��@S�
@S��@S33@R��@R��@R~�@R-@Q7L@Q�@P��@P�9@P��@P�u@Pr�@PbN@PQ�@PQ�@P1'@O�@OK�@O;d@O+@O
=@O
=@O
=@O
=@O
=@O
=@O�@O
=@O
=@O
=@O
=@O�@O
=@N��@N@M�T@M@M��@M?}@Lj@L9X@L�@K��@K�F@K�@KdZ@KC�@K"�@J��@JM�@JJ@I��@IX@IG�@I%@H��@H��@H�9@HA�@G;d@E��@E�h@E?}@E�@D�@D�@D��@D��@D�@D�@Ct�@B��@B�\@B^5@B-@B-@A��@A��@Ax�@AX@A&�@@�`@@��@@��@@��@@��@@�u@@�@@r�@@A�@@1'@@A�@@  @?��@?�P@?�P@?|�@?l�@?\)@?\)@?K�@?;d@?+@?+@?�@?
=@?
=@?
=@>�@>v�@>@=�@=@=`B@<��@<�@<�D@<I�@<(�@<1@;��@;S�@;"�@;@:��@:-@9�#@9��@9��@9��@9��@9�^@9�@8�`@8��@8��@8�@8�@8�@8bN@8Q�@8A�@81'@8b@7�@7�w@7�@7��@7�P@7|�@7|�@7|�@7\)@7
=@6v�@6$�@6@6{@6$�@65?@6{@5�@5��@5�-@5�-@5�-@5�-@5�-@5�-@5�-@5�h@5�@5�@5�@5�@5p�@5O�@5?}@5/@4��@4�j@4�@4��@4I�@4j@4Z@4�@3��@4�@49X@4�@41@3t�@3C�@3"�@2�@2��@2�!@2�!@2�!@2��@2��@2�!@2��@2��@2��@2��@2��@2�\@2�\@2�\@2�\@2n�@2-@1��@1��@1��@1��@1hs@1G�@17L@1�@0��@0�`@0Ĝ@0bN@0A�@0A�@01'@01'@0b@0b@0b@0b@0  @/�@/��@/�w@/�@/�@/��@/��@/��@/�P@/|�@/;d@/+@/�@/
=@.��@.��@.�y@.�@.�@.ȴ@.��@.�+@.�+@.v�@.v�@.v�@.v�@.ff@.V@.E�@.E�@.$�@.{@.@.@.@.@.@.@.@.@.@.@.@-�@-�T@-��@-@-@-@-�-@-�h@-p�@-`B@-O�@-?}@-�@-V@,�@,�@,�@,��@,�D@,��@,�D@,�D@,z�@,z�@,Z@,I�@,I�@,I�@,I�@,I�@,9X@,�@,1@+��@+�
@+ƨ@+ƨ@+ƨ@+�F@+�F@+�F@+�F@+�F@+ƨ@+ƨ@+�F@+��@+��@+�@+t�@+t�@+dZ@+33@*�!@*�\@*~�@*M�@*J@)��@)�@(��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AhE�AhVAhVAhVAhQ�AhVAhM�AhQ�AhI�AhA�AhI�AhbNAhz�Ah�+Ah�uAh�uAh��Ah��Ah��Ah�Ah��Ah1AgS�AfĜAe�Aet�Ac��AcoAb�+Aa�-A`JA^�9A^(�A]��A]\)A]K�A]7LA]�A]
=A\�yA\�jA\��A\�DA\bNA\-A\1AZ��AZ��AZ�9AZ��AZ�AZz�AZr�AZffAZZAZQ�AZ{AY�AY�#AY��AY�^AY�AY�AY�AY�FAY�^AY�FAY�AY�AY��AY�-AY��AY�
AY��AY&�AX��AXVAW�-AWG�AV��AU��AUx�AUoAT��AT�uATbNAS�
AS�AR�/AR�ARZAR$�AQ��AQ��AQG�AP�9APbNAPz�AP^5AP��AP�9AQXAQx�AP  AN��AN^5ALr�AJv�AG��AF��AFv�AE�#AEhsAE&�AE�AE&�AD��ADI�AC�AC�;AC�FAC\)AC33ACoAB�yABv�ABbAA��AA�mAA�TAA�
AA�FA@9XA?�hA?33A>�A>bA=x�A=dZA<ZA;�A:�A9l�A9G�A9�A8z�A8  A7dZA6�`A6=qA5��A4��A4z�A4{A3��A3G�A2��A2 �A1��A1O�A1"�A1�A2$�A2n�A25?A1�mA1A1�A1G�A0��A0bA/�A/S�A.��A.�RA.A-XA,�DA+dZA*�yA*$�A)\)A)K�A(z�A'��A'��A'��A'��A'��A'ƨA'�A'��A'O�A&�DA%`BA$�A$�A"��A"1'A"  A!�wA!��A!p�A!/A!A �HA ��A bNA��A�A=qA7LA�9A?}A�9A�mA��A(�A��A��A�AK�A�yAffA �A�mA��A+AA�jA��Ar�AQ�AbA��A��AJA?}A1'A��A��A��AdZAXA7LAVA��A�`AȴAv�AZA-AA�FA\)AĜAr�AE�A  AƨA��A�7A"�A
�A	�A	hsAQ�A��An�AbA�A�AĜA��A�\Ar�AZA�A1A�FAC�A�A��A�A ��@�z�@�^5@���@�G�@� �@�ȴ@�O�@���@��@�b@�P@�+@���@���@�l�@���@�"�@��@�\)@ݲ-@ܣ�@�n�@���@�\)@�~�@�{@ղ-@ԋD@��@ӶF@�dZ@�C�@�o@ҟ�@щ7@�G�@�G�@�O�@�O�@�O�@�?}@��@ϝ�@Χ�@Ο�@Ώ\@�V@�=q@�$�@���@�O�@�/@�/@�&�@��@�V@��@�z�@�1@˶F@˅@�dZ@�l�@�\)@�
=@�n�@ɡ�@�Z@Ǯ@�|�@�"�@��@�5?@őh@��@�I�@�b@Ý�@���@�5?@���@��@�O�@�&�@��D@�+@�J@�x�@�Z@���@��@���@�9X@��F@���@�t�@�33@��@�=q@��@���@��P@�ȴ@���@��/@�I�@��@�l�@�=q@���@��h@��@�r�@�b@�dZ@�+@���@�J@�@��-@�V@���@��@�K�@��@�=q@���@�x�@��@��/@�z�@��@���@�C�@��+@�$�@��7@�hs@�?}@��@��@���@��@���@��u@�ƨ@�\)@�33@���@��!@�~�@�5?@�@���@��@�?}@�V@��/@�Ĝ@�r�@��F@�|�@�dZ@��@��@���@��+@�~�@�ff@�=q@�$�@�@���@�bN@��F@�\)@��@��H@���@���@���@�v�@�J@�hs@���@�"�@���@���@�O�@��F@�@�ff@�J@�@�x�@�x�@�7L@���@�I�@���@��;@���@�t�@�;d@���@��!@�~�@�-@�p�@��@��u@�r�@���@��#@�Ĝ@�|�@���@��@��/@���@���@�S�@��@���@�j@�Q�@�(�@��@� �@�(�@�(�@� �@�P@K�@
=@~��@~ȴ@}`B@|��@|�@|9X@{��@{t�@{t�@{dZ@y��@v��@vV@u�h@t1@s@qX@nff@n�+@n�@n�+@m�@pb@qX@rM�@r~�@r��@r�!@q7L@o\)@n�+@n$�@m��@l�/@l�/@m/@l9X@kS�@k"�@j�@j��@k@kC�@kS�@kS�@kS�@kS�@k��@k�
@lI�@l��@m`B@mp�@n{@n�+@n��@n�y@nȴ@nȴ@n�+@n$�@i�@g�@fE�@fV@e�@d9X@b��@a��@c��@c��@d1@d�D@d��@b��@b^5@a�#@`r�@_|�@_|�@^�y@\��@[�
@ZJ@Y�^@Y�#@Z�!@\�@]��@^��@a��@c��@ct�@c@b��@ax�@a&�@a7L@`��@`��@a��@a�7@ax�@a�@`Ĝ@`bN@` �@_�@_l�@_+@^�@^�+@^V@^E�@^E�@^$�@^{@]�T@]��@[��@[t�@[�@Z�\@Y�@Xr�@W��@Vȴ@V�@W;d@V�@U��@U��@U�-@U@U�-@U`B@Tj@T�@T�@TI�@T�@St�@St�@Tj@U`B@U��@U/@T��@S�
@S��@S33@R��@R��@R~�@R-@Q7L@Q�@P��@P�9@P��@P�u@Pr�@PbN@PQ�@PQ�@P1'@O�@OK�@O;d@O+@O
=@O
=@O
=@O
=@O
=@O
=@O�@O
=@O
=@O
=@O
=@O�@O
=@N��@N@M�T@M@M��@M?}@Lj@L9X@L�@K��@K�F@K�@KdZ@KC�@K"�@J��@JM�@JJ@I��@IX@IG�@I%@H��@H��@H�9@HA�@G;d@E��@E�h@E?}@E�@D�@D�@D��@D��@D�@D�@Ct�@B��@B�\@B^5@B-@B-@A��@A��@Ax�@AX@A&�@@�`@@��@@��@@��@@��@@�u@@�@@r�@@A�@@1'@@A�@@  @?��@?�P@?�P@?|�@?l�@?\)@?\)@?K�@?;d@?+@?+@?�@?
=@?
=@?
=@>�@>v�@>@=�@=@=`B@<��@<�@<�D@<I�@<(�@<1@;��@;S�@;"�@;@:��@:-@9�#@9��@9��@9��@9��@9�^@9�@8�`@8��@8��@8�@8�@8�@8bN@8Q�@8A�@81'@8b@7�@7�w@7�@7��@7�P@7|�@7|�@7|�@7\)@7
=@6v�@6$�@6@6{@6$�@65?@6{@5�@5��@5�-@5�-@5�-@5�-@5�-@5�-@5�-@5�h@5�@5�@5�@5�@5p�@5O�@5?}@5/@4��@4�j@4�@4��@4I�@4j@4Z@4�@3��@4�@49X@4�@41@3t�@3C�@3"�@2�@2��@2�!@2�!@2�!@2��@2��@2�!@2��@2��@2��@2��@2��@2�\@2�\@2�\@2�\@2n�@2-@1��@1��@1��@1��@1hs@1G�@17L@1�@0��@0�`@0Ĝ@0bN@0A�@0A�@01'@01'@0b@0b@0b@0b@0  @/�@/��@/�w@/�@/�@/��@/��@/��@/�P@/|�@/;d@/+@/�@/
=@.��@.��@.�y@.�@.�@.ȴ@.��@.�+@.�+@.v�@.v�@.v�@.v�@.ff@.V@.E�@.E�@.$�@.{@.@.@.@.@.@.@.@.@.@.@.@-�@-�T@-��@-@-@-@-�-@-�h@-p�@-`B@-O�@-?}@-�@-V@,�@,�@,�@,��@,�D@,��@,�D@,�D@,z�@,z�@,Z@,I�@,I�@,I�@,I�@,I�@,9X@,�@,1@+��@+�
@+ƨ@+ƨ@+ƨ@+�F@+�F@+�F@+�F@+�F@+ƨ@+ƨ@+�F@+��@+��@+�@+t�@+t�@+dZ@+33@*�!@*�\@*~�@*M�@*J@)��@)�@(��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BoB	7B  B��B�B�B��BȴB�qB��B��B��B��B�{B�{B�{B�{B�{B�uB�uB�oB�oB�hB�bB�VB�PB�PB�PB�PB�PB�JB�JB�JB�JB�DB�7B�1B�1B�1B�+B�+B�+B�+B�1B�1B�1B�1B�1B�1B�7B�DB�JB�7B�B}�Bw�Bp�BjBbNBZBVBQ�BN�BM�BM�BJ�BH�BI�BK�BN�BO�BQ�BR�BS�B[#Be`Bl�Bx�B�oB��B�B�'B��B��B�{B|�BbNBK�BF�BD�BA�B?}B?}BC�BK�BM�BK�BL�BN�BN�BK�BJ�BI�BG�BI�BK�BK�BK�BJ�BI�BG�B;dB5?B1'B-B$�B�B�BPB  B��B�B�B�B�`B�BB�B��B��BǮB��B�jB�RB�3B�B�B��B��B��B��B�9B�RB�wB�wB�qB�jB�^B�FB�!B��B��B��B��B��B�bB�Bz�Bq�Br�Bn�BiyBgmBaHB\)BZBYBZBZB_;Be`BcTB_;BXBP�BL�BC�B7LB33B2-B1'B/B.B,B+B(�B'�B#�B�B�BJBB
��B
�B
�B
�NB
�
B
��B
��B
��B
��B
ȴB
ĜB
��B
�wB
�jB
�XB
�LB
�FB
�?B
�9B
�9B
�3B
�-B
�!B
�B
�B
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
��B
��B
��B
��B
�{B
�uB
�oB
�hB
�\B
�PB
�=B
�%B
�B
{�B
t�B
q�B
o�B
l�B
iyB
hsB
hsB
gmB
ffB
ffB
dZB
cTB
aHB
_;B
]/B
ZB
S�B
J�B
?}B
:^B
8RB
5?B
1'B
-B
'�B
�B
�B
�B
{B
hB
DB
  B	��B	�B	�TB	�5B	�
B	��B	��B	ƨB	ÖB	��B	�wB	�qB	�jB	�XB	�XB	�XB	�XB	�XB	�XB	�dB	�}B	B	B	ÖB	ÖB	B	B	��B	��B	��B	��B	��B	ÖB	ÖB	B	ÖB	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	ȴB	ȴB	ƨB	ƨB	ŢB	ŢB	ĜB	ĜB	ÖB	B	��B	��B	�}B	�wB	�qB	�jB	�dB	�^B	�XB	�RB	�LB	�FB	�9B	�'B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�9B	�FB	�XB	�dB	�^B	�jB	�wB	�}B	�qB	�dB	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�wB	�}B	��B	��B	��B	B	B	ÖB	ŢB	ŢB	ŢB	ŢB	ŢB	B	��B	��B	�}B	�wB	�qB	�dB	�dB	�wB	��B	B	ÖB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�#B	�)B	�BB	�HB	�HB	�HB	�HB	�NB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B
B
%B
	7B
\B
�B
�B
{B
uB
oB
uB
uB
uB
�B
�B
 �B
!�B
#�B
$�B
&�B
&�B
'�B
(�B
(�B
'�B
'�B
)�B
-B
/B
0!B
0!B
0!B
/B
/B
/B
0!B
.B
,B
+B
)�B
)�B
-B
/B
0!B
/B
0!B
0!B
0!B
1'B
1'B
0!B
2-B
33B
33B
33B
33B
5?B
9XB
<jB
@�B
A�B
B�B
D�B
E�B
F�B
G�B
G�B
G�B
H�B
L�B
M�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
T�B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
\)B
]/B
]/B
]/B
_;B
cTB
dZB
e`B
ffB
hsB
iyB
iyB
jB
k�B
m�B
n�B
o�B
p�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
v�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
z�B
{�B
}�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�1B
�1B
�1B
�1B
�7B
�7B
�=B
�DB
�DB
�DB
�JB
�hB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
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
��B
��B
��B
��B
��B
��B
�B
�B
�!B
�!B
�!B
�!B
�!B
�?B
�FB
�LB
�RB
�RB
�XB
�RB
�XB
�^B
�^B
�^B
�dB
�jB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�}B
�}B
��B
��B
B
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ȴB
ȴB
ȴB
ɺB
ɺB
ɺB
ɺB
��B
ɺB
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
�B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�#B
�)B
�)B
�)B
�)B
�)B
�/B
�5B
�BB
�HB
�HB
�TB
�ZB
�ZB
�`B
�fB
�fB
�mB
�yB
�B
�B
�B
�B
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
��B
��B
��B
��B
��B
��B
��B
��B  B  B  B  BBBBBBBBBBBB%B%B1B1B1B1B1B1B1B1B1B1B1B	7B	7B	7B
=B
=BDBDBDBJBJBJBJBPBPBPBPBPBPBVB\BbBbBhBhBhBhBoBoBoBuBuBuB{B�B�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BZB�B�B�B�B�B�B�B�BsBGB[B�B�B�B�B�B�B�B�B8B]B
�B>B�EB��B�DB�tB�1B��B��B�AB�	B�:B��B��B��B��B��B��B��B��B��B��B��B�B��B�xB��B��B�fB�gB�pB�oB�hB��B��B�lB�GB�uB�RB�.B�-B�B�$B�;B�DB�1B�;B�B��B�(B��B�nB�BRBy�Bq�Bl`BdnB["BW"BR�BO`BNhBOOBL�BITBJMBL�BOlBP�BR|BS�BU�B\Be$Bl�BxNB�B�B��B� B��B��B��B��Bj/BM�BG�BFMBB�B@<B?�BC�BLVBO�BL�BMBOKBO�BLFBK$BJ6BH�BJ�BLBL BK�BJ�BJHBK�B=KB6JB29B/TB&�BB�B�B�B��B�B�+B�;B��B� BیB��BРBɌB��B��B��B�*B�B�`B�eB��B�-B��B�VB��B�B�PB��B��B��B�B��B�"B��B��B��B��B�[B�|B~BsJBt�Bp�Bi�Bi�Bb�B]BZ8BY#BZ+BY�B^�Be�Bd�Ba|B[QBRUBO=BG�B8�B3�B2�B1�B/�B.�B,�B+dB)dB(�B%B�B�BHB�BB
�qB
��B
��B
�VB
ҏB
��B
�EB
�lB
��B
�B
�^B
�'B
�PB
��B
��B
�
B
��B
��B
��B
��B
�tB
��B
�uB
�pB
��B
��B
��B
�B
�xB
�B
�=B
�HB
�B
�B
�*B
��B
�B
�GB
�9B
��B
��B
�VB
��B
�B
�LB
�B
��B
��B
��B
��B
�_B
��B
�@B
�B
v]B
r�B
p�B
n�B
jB
h�B
h�B
g�B
f�B
gB
d�B
dNB
b�B
_�B
^RB
\)B
X�B
QqB
B�B
;]B
9(B
6�B
32B
/fB
+�B
"DB
/B
]B
*B
�B
�B
�B	�AB	�B	�B	�B	َB	өB	�B	�#B	��B	��B	�'B	�!B	�B	�B	��B	��B	��B	��B	�B	�B	��B	B	B	ÞB	ÜB	´B	��B	ûB	�B	��B	��B	��B	ÿB	��B	�B	�VB	��B	ưB	ƽB	ƿB	��B	��B	�iB	�qB	�DB	�B	��B	ɹB	��B	�9B	ǥB	��B	ǈB	ƨB	��B	�,B	�B	ÑB	B	��B	�yB	��B	�*B	��B	�@B	��B	��B	��B	��B	�DB	�]B	��B	�B	��B	�XB	�5B	�[B	�B	��B	��B	�B	�!B	�JB	��B	�=B	�^B	��B	��B	�B	�B	��B	��B	��B	�XB	�DB	�B	�MB	��B	�EB	��B	�B	��B	��B	�.B	��B	��B	�xB	��B	��B	��B	��B	�~B	�eB	�jB	�1B	�nB	��B	�hB	�`B	��B	�uB	��B	�B	�B	�B	��B	�B	��B	�B	�FB	�B	�zB	�B	�6B	�LB	�'B	�NB	��B	�B	�B	�BB	�0B	�+B	�B	�hB	��B	�>B	�B	�ZB	�,B	�"B	�XB	��B	�B	�,B	�B	�%B	��B	��B	��B	��B	�]B	�KB	�*B	�B	�	B	�:B	��B	��B	��B	�^B	��B	�FB	�bB	�BB	��B	��B	�\B	�IB	�KB	��B	�JB	��B	�oB	�hB	�B	�DB	�<B	�MB	�XB	�`B	�DB	��B	�B	��B	��B	�B	�B	��B	��B	�:B	�,B	�B	�B	�@B	��B	�B	��B	�4B	�B	��B	��B	��B	�fB	�eB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	��B	ŮB	��B	��B	��B	�&B	�1B	��B	�XB	��B	��B	�uB	�IB	��B	��B	�B	��B	�$B	��B	��B	� B	�B	�ZB	ѡB	�IB	�iB	ԘB	�B	��B	��B	��B	�NB	�RB	�dB	��B	�B	�@B	�NB	�PB	�LB	� B	�*B	�B	��B	�-B	�pB	�B	�=B	�MB	�B	��B	��B	�
B	�IB	��B	��B	�KB	�B	��B	��B	��B	�JB	�VB	��B	��B	��B	��B	�MB	�TB	�^B	��B	��B	��B	�JB	�5B	�B	�B	��B	��B	�,B	��B	��B
\B
 B
�B
�B
�B
�B
VB
�B
tB
�B
�B
�B
B
 �B
"!B
$B
%3B
'B
'B
(ZB
)/B
):B
(6B
("B
*B
-B
/7B
01B
0MB
0dB
0jB
/�B
/(B
0�B
/=B
,�B
+�B
*�B
)�B
,�B
/qB
0�B
/IB
0B
0B
07B
1nB
1�B
/�B
25B
3�B
3eB
3�B
3:B
4�B
8�B
< B
@�B
BB
C.B
D�B
E�B
GB
G�B
G�B
G�B
IzB
L�B
M�B
OB
O�B
O�B
P B
P�B
Q�B
Q�B
RB
R`B
UNB
WB
W B
X+B
XB
XB
XB
XB
XB
X
B
XB
XB
XB
XB
X
B
X(B
XtB
Y�B
\CB
]PB
]LB
]�B
_�B
cyB
dyB
e~B
f�B
h�B
i�B
i�B
j�B
k�B
m�B
n�B
o�B
qB
q�B
q�B
r�B
r�B
s�B
tB
t�B
u�B
wB
xB
w�B
x�B
x�B
y�B
y�B
y�B
{\B
|mB
~zB
�:B
�4B
�1B
�B
�;B
�WB
�DB
�:B
�HB
�XB
�^B
�4B
�7B
�5B
�FB
�EB
�JB
�dB
�QB
�AB
�B
��B
�pB
�rB
��B
��B
��B
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
��B
��B
�B
�
B
��B
� B
�?B
�#B
�B
�B
�(B
��B
�QB
�,B
�"B
�$B
�%B
�:B
��B
�_B
�WB
�tB
�kB
�VB
�YB
�rB
�gB
�nB
�qB
��B
��B
��B
�B
��B
��B
��B
�zB
�|B
��B
��B
��B
��B
��B
�qB
�sB
�uB
��B
��B
��B
«B
ÖB
×B
ØB
×B
×B
ÞB
ĶB
ŮB
ƧB
ƪB
ƫB
ƶB
��B
ȿB
��B
��B
��B
��B
��B
��B
ʫB
��B
��B
��B
̱B
̸B
��B
��B
�MB
�B
�B
�B
�"B
�B
�
B
�B
�B
�B
�B
� B
�B
�B
�B
�"B
�/B
�*B
�-B
�.B
�HB
�dB
�ZB
�aB
�\B
�WB
�oB
�pB
�fB
�wB
�zB
�uB
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
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
�B
�B
�B
�B
�B
�B
��B
�B
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
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B B B BBBBB$B*B"B$B)B7B.B?B_B3B>B>B$B@B3B?B3BNB>B3B	8B	8B	=B
HB
XBPBQBbBWBNBPB\BQBOBRBOBDBUBfBjBdB�BpBjBxB�B�B�B�B�B�B�BB�B�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#��<#�0<#�<#�{<#�&<#�*<#�<#�8<#�r<#�C<#�<#�&<#�*<#��<#�
<#�D<#�<#�<#�<<#��<%�<&�H<%��<'��<%�M<1 �<'d<%ȧ<(��<2��<-�<%�j<%�l<$?[<#��<#��<#�&<#��<#��<$/<#�&<#�4<#�a<$�<$�<)�<$'<#��<#�<#�<#؄<#ٛ<#�r<#�8<#��<$'<#��<#�<#؄<#�&<#ۮ<#�<#�<#��<#׎<#�X<#�$<#�
<#�X<#�o<#��<#�o<$$<$�-<$��<%K:<&1�<%
�<&�A<'T�<$�j<$��<$y�<$�<$�<%�l<'$�<$%<$�<$t <$�<$v�<$F<$�j<%��<$q@<#�<#�<$�<#��<%�@<#�</��<*>'<&�k<8H<<t�<N��<'|<$Ş<&\<%'<$F9<#�e<#�C<$f<&/<$�B<#��<#��<$��<$<<#��<$�<%(<$Ş<#�e<#��<#��<#��<$�<0x�<&��<$�	<$��<'�O<%�n<#��<*O�<-x�<)��<&|V<$�<$�<&y<%b�<&1�<%p<&n4<&J$<&��<%k�<$��<%&<$��<$�w<(�<%�d<$\"<#�W<'�<$t <$?[<$'<$f�<#�Q<#�M<$ح<&1�<&�<$��<$�B<$��<$o�<&�J<&�9<(%�<+�<%��<'uq<'x�<#�<'�T<%��<$��<#�D<#�{<#ף<$�<#�g<$�<%it<'�<+�<%rN<(j�<0�W<%G<$#(<$E<$�<$<<$<<$�<#�W<#��<$�3<$��<$��<0�I<*�<&�<0T�<&Z�<(�<-�]<%&<$(<$r�<$�<$0.<$�7<%�<$i&<$5w<$x+<%b<$�<$L<#��<$!><#��<$O�<%<(_�<(�<(M}<*��<$P�<$j|<#��<$ K<#ߜ<#��<$<<#ߜ<#�!<#�<$� <#��<$G<$	�<$x+<$��<&y<$��<$f<$\"<$.<#�N<#�Q<$��<%gB<'[)<&�<+�)</lQ<%�d<$��<%�<'a�<$f<#��<#�U<#�5<#�<$><<#�l<$�X<%,#<$G<$ح<'�<3k<C�O<+��<$�<$b�<&<'�<(�<.#�<(��<%�<$Z�<$4e<(�<1v�<5��<(��<0�t<(4<,�q<(�,<&/<, <(�<'�c<%2?<$5w<$5w<&e<$I�<$"2<$
�<#�<#�<$E<%�`<#��<#�<#�<#�<<#�&<#�8<#�<'�<%�J<#�*<#�8<#�g<#�+<#ޫ<$�<$Gd<#��<#�<<#�c<#ا<#��<#�<$:�<$=<$<#�<#�N<#�<#��<$�<$��<%0<&��<$�<#��<$F<$�<$��<$�	<$��<$�e<#��<$?[<%:{<$j|<$%<$�<#�<#�<$�w<'a�<&�<$�<&]p<(!�<%2?<%��<$�-<$U�<#�E<#�<#��<$#(<$�t<$�<&A�<&�}<%:<%��<%�V<$�t<#�(<$ʾ<&U"<$=<$<<$Gd<$�2<$*<$��<#�Q<$_�<$�Q<$�<#�8<$��<$Gd<$��<$�<$H�<$�Q<$8�<$Z<$$<#��<$(<$j|<$"2<$�<$�7<$/%<$�V<#��<#��<#�<#�<#��<#�C<#��<$/<$�-<$.<#�!<#�)<$/<#�<$�<$?[<#��<#�&<#��<#�5<#�<#ߜ<$'<$�7<#�<#��<$�<#�M<#�&<$ �<#��<#ߜ<#�M<#��<#�U<$0.<&�a<$�3<$ K<$ <#�N<#�N<#��<#��<#�<$4e<$��<$��<(��<&��<$ �<$�<(��<%�<$��<$'<$v<$p<#�$<$�<$v�<$
<$�<#�J<#�<#�5<#��<#��<$<<#�N<$�<$�`<$k<$\"<#�J<$U�<$��<&W�<&�<$x+<&�<&!�<&�%<#�D<$�<'J�<&?><$(<#��<#�4<#��<#�<#�<#�<<#�<$�<#��<#�e<#�<#�e<$�;<#�<#�<#�)<$
�<#��<#�{<#�8<$�<()+<$(<$3U<%s<$q@<%K:<'�:<#��<#�<#�<#��<%�<$�e<$A�<#��<#��<#ף<$��<%}�<$B�<#�<#��<$%<#�X<#�<$N�<$><<#�^<#ߜ<#��<#�&<#�J<#�<<#�&<#�<<#�<#�<#�<#�<$<<#��<#׺<#��<#�<#�<#�$<#��<#�0<#�<$p<+d<'�c<$Gd<#�{<#��<%B�<%6Z<$F9<%B�<#�o<#�"<#�N<#�<%@�<#��<$�<$ѩ<$MO<#׎<$	�<%��<$�<%it<#��<#؄<$.<$�<%*<$R'<'��<%��<#׺<#�m<#�m<$q@<#�<#�<#�!<#�l<$�Q<#�<#�c<#�<#�<#�<#��<#ޫ<#�Q<#��<#�&<#��<#ޫ<#�c<#�<#�o<#��<#��<#�<%,#<#��<#׎<$A�<$�`<$�<$1:<$.<#�<<#��<#�<$N�<#�<#�X<#�<<#؄<#�l<$:�<#�!<#�<<$�<#ޫ<$a<#�0<$F9<$I�<#�<$�<$<<$$<#�<#�<#�N<#�*<#ڑ<#�4<$N�<#�D<#ڑ<#��<#��<#�C<#�]<#�$<#׺<#�<#�*<$ <#�<#׺<#؄<#�D<#�&<#�&<#�0<#�&<#�&<#�&<#ף<#�<#�&<#�<#�&<#��<#��<$�<#�<#�]<#ٛ<#�<$0.<#�8<#��<#��<#�N<#�+<#ٛ<#�*<#�*<#�<#�N<#��<#�<#�<#�C<#��<#�<#��<#�*<#�)<$n�<$�w<#�<#�<#�]<#�8<#׎<#�<#�<#ڑ<$/<$�<$�<#�J<#�<#�8<#�I<#��<#�<#ܯ<#�]<#�+<#��<#��<#�<#�&<#�<#�c<#ף<#�$<#ۮ<#׎<#�<#�<#�5<#�<<#�<#׎<#�$<#׺<#�&<#�$<#؄<#ף<#�<#׺<#��<#�<#�&<#�^<#�"<#�(<#׺<#��<#�<#�<#�c<#�<#��<#��<#ٛ<#��<#��<#��<#��<#��<$�<#�e<#��<#�<#�<#�<#��<$�<#�*<#��<#��<#��<#�<#�<#�*<#׺<#��<#�$<#ڑ<#��<#�l<#ף<#�C<#�C<#׎<#�<#�<#��<#�M<$ <#�&<#ٛ<#�{<#�X<#�<<#��<#ٛ<#�o<#�o<#�
<#�<#�<#�<#�<#�<<#�<#�{<#�<#�<#�<#ף<#��<#�i<#��<#ܯ<#�<#�<#׺<#�<#؄<#�C<#��<#�D<#�o<#�c<#ٛ<#�$<$ <#�+<#�]<#�8<#�l<#�{<#�
<#�<#��<#�<#�X<#��<#�<#�
<#�<#�<#�{<#�<#�<#�<#��<#�<#ܯ<#��<#�<#׺<#ۮ<#�o<#�{<#ٛ<#�<#׺<#��<#�!<#��<#�<#ף<#�<#��<#�<#�<#�<#׎<#׺<#�D<#׎<#ף<#�<#׺<#�<#�
<#ף<#�<#ޫ<#��<#�{<#�<#׎<#�<#׺<#��<#�<#ף<#ٛ<#ٛ<#�<#�0<#�
<#�<#�<#�C<#�i<#�i<#�<#ٛ<#ף<#�X<#�<#�<#�
<#�<#�<#�<#�<#�<#�<#�
<#ף<#׎<#��<#�X<#�<#�<#׺<#��<#��<#��<#�i<#��<#��<#׺<#�<#�N<#�<#׎<#׎<#׎<#׺<#�<#ף<#�<#ٛ<#׎<#�<#�<#�<#�&<#�i<#�D<#�{<#׎<#��<#׎<#�<#�&<#�<#�<#�<#�<#�<#�{<#�<#��<#ף<#�<#��<#�<<#�<#��<#�<#��<#ٛ<#�$<#ܯ<#��<#�e<$�<#�<#�XPRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                              None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =1(+/-0.0001), vertically averaged dS =-0.005(+/-0.002),                                                                                                             SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; No significant drift detected in conductivity                                                                                                                                   202205040000002022050400000020220504000000  AO  ARGQQCPL                                                                    20210217043657  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210217043657  QCF$                G�O�G�O�G�O�0               WHOIARSQWHQCV0.5                                                                20211004000000  QC                  G�O�G�O�G�O�                WHOIARSQCTM V1.0                                                                20220503000000  IP                  G�O�G�O�G�O�                WHOIARCAOWC V2.0ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     20220504000000  IP                  G�O�G�O�G�O�                