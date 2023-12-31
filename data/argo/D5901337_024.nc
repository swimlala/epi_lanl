CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:30Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       j    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       r   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112731  20190522121836  1901_5055_024                   2C  D   APEX                            2140                            040306                          846 @�e ��1   @�e8�@,m�hr�!�cl�hr�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�33B�  B���B�  B�33B�33B�  B���B���B���B�  B�  B���C  C  C  C�fC
  C  C  C  C�C  C�fC  C�C  C�fC   C"�C$  C%�fC'�fC*  C,�C.  C/�fC1�fC3�fC5�fC8  C:�C<  C>  C@�CB  CD  CF�CH�CJ�CL  CM�fCP  CR  CT  CU�fCX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx�Cz�C|�C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C��C��C�  C��3C��3C�  C��C�  C��3C�  C��C��C�  C�  C��3C�  C��C�  C�  C�  C��3C��3C��3C�  C��C��C��C��C��C��C�  C��3C�  C�  C��C�  C��3C�  C��C��C��C��C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C��3C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3D   D � D  D�fD  D� D  Dy�D��Dy�D��Dy�D  D� D  D� D  Dy�D	  D	�fD
  D
y�D
��D� DfD� D  D� DfD�fD  D� D��D� D  Dy�D��Dy�D��D� D  D� D  D�fD  D� D  Dy�D  D� D��D� D  D� D��D� DfD� D��D� DfD� D  Dy�D   D � D!  D!�fD"fD"� D#  D#� D$fD$� D$��D%� D&fD&� D&��D'� D(  D(�fD)  D)y�D*  D*�fD+  D+y�D,  D,� D-fD-� D-��D.� D/fD/� D/��D0y�D1  D1y�D2  D2�fD3  D3� D3��D4y�D5  D5� D6  D6�fD7  D7� D8  D8y�D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=y�D>  D>� D?  D?y�D?��D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DGfDG� DH  DH� DH��DI� DJ  DJy�DK  DK�fDL  DLy�DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D\��D]y�D]��D^y�D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc�fDdfDd� De  Dey�De��Dfy�Df��Dg� Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  DwffDy��D�3D�VfD�� D��3D��fD�@ D�ffD��3D�fD�9�D�Y�DǼ�D��3D�  D�p D๚D��D�0 D�S3D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@fff@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6��B>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�  B�33B�ffB�33B�  B�33B�ffB�ffB�33B�  B�  B�  B�33B�33B�  C��C��C��C� C	��C��C��C��C�3C��C� C��C�3C��C� C��C!�3C#��C%� C'� C)��C+�3C-��C/� C1� C3� C5� C7��C9�3C;��C=��C?�3CA��CC��CE�3CG�3CI�3CK��CM� CO��CQ��CS��CU� CW��CY�3C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm�3Co��Cq��Cs��Cu��Cw�3Cy�3C{�3C}��C��C�ٚC���C���C���C���C���C���C���C���C���C�ٚC�ٚC���C�� C���C�ٚC�ٚC���C�� C�� C���C�ٚC���C�� C���C�ٚC�ٚC���C���C�� C���C�ٚC���C���C���C�� C�� C�� C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�� C���C���C�ٚC���C�� C���C�ٚC�ٚC�ٚC�ٚC���C�� C�� C�� C�� C���C���C���C���C���C���C���C�� C�� C�� C�� C���C�ٚC���C�� C�� C�� C���C���C���C���C���C�� C�� C�� C���C�ٚC���C���C�ٚC���C���C���C���C���C���C���C���C�ٚC���C���C���C�� C���C���C���C�ٚC���C���C���C���C�� C���C���C���C���C�� C�� C�� C���C���C���C���C���C�� C���D ffD �fDl�D�fDffD�fD` D� D` D� D` D�fDffD�fDffD�fD` D�fD	l�D	�fD
` D
� DffD��DffD�fDffD��Dl�D�fDffD� DffD�fD` D� D` D� DffD�fDffD�fDl�D�fDffD�fD` D�fDffD� DffD�fDffD� DffD��DffD� DffD��DffD�fD` D�fD ffD �fD!l�D!��D"ffD"�fD#ffD#��D$ffD$� D%ffD%��D&ffD&� D'ffD'�fD(l�D(�fD)` D)�fD*l�D*�fD+` D+�fD,ffD,��D-ffD-� D.ffD.��D/ffD/� D0` D0�fD1` D1�fD2l�D2�fD3ffD3� D4` D4�fD5ffD5�fD6l�D6�fD7ffD7�fD8` D8�fD9ffD9�fD:ffD:�fD;` D;�fD<ffD<�fD=` D=�fD>ffD>�fD?` D?� D@` D@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEl�DE�fDFffDF��DGffDG�fDHffDH� DIffDI�fDJ` DJ�fDKl�DK�fDL` DL�fDMl�DM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVl�DV�fDW` DW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\` D\� D]` D]� D^` D^�fD_ffD_�fD`` D`�fDaffDa�fDbffDb�fDcl�Dc��DdffDd�fDe` De� Df` Df� DgffDg�fDhffDh� DiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwL�Dys3D�fD�I�D�s3D��fD��D�33D�Y�D��fD���D�,�D�L�Dǰ D��fD�3D�c3D��D���D�#3D�FfD�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��;A���Aɝ�AɅA�M�A��Aȥ�A�z�A�p�A�dZA�bNA�^5A�\)A�ZA�ZA�S�A�O�A�M�A�G�A�E�A�E�A�C�A�?}A�?}A�;dA�7LA�+A��A��A�G�AƋDA�jA�C�A�-A�  AĲ-Aę�A�1A���A�-A�oA���A�bNA�A�A�;dA�A�|�A�t�A�|�A���A�bNA��mA��A���A�ZA���A�bA�+A���A�XA���A�JA��PA�ĜA���A�O�A�-A��TA�p�A��-A��mA�"�A�ƨA��^A��jA��A�;dA��TA�G�A�O�A�-A��wA���A�?}A���A���A�  A��A��Az�HAr��AmO�Ah��Ac��Aa"�A^�AZ�AR�APn�AOhsANĜAM��AL1AJ�AF�!AE�#AD�RAA�mA?"�A=��A<n�A:ȴA8��A7��A6E�A5K�A4 �A3x�A2��A0��A0-A/�A/�;A/��A/oA-�A,jA+�A+VA*r�A)?}A(��A(�\A$ȴA"�A!33A�AS�A��A��A!/A!��A!��A"JA"A�A!��A bNA��AA�FA\)AK�A��A~�AS�AC�At�AbNAp�A �A�+AdZA�Ap�A��A�A&�AQ�AJAdZAbA��AO�A�AƨAl�A;dA?}A
=A�/A�A"�A%A�A�-A��A�yAI�A{A��A��A�yA=qAJA��A
��A
VA
  A	;dAM�Ar�A��AĜA	;dA	G�AE�A �A/A%A�A�/A��Av�AjA(�A��At�A�A��Ar�Ar�AffA��A\)AA�`AjAVAbNAQ�A�AhsA ȴA �+A 5?@�^5@��9@��-@��-@�j@��@��@���@��D@�hs@�n�@�I�@�n�@�=q@�P@�w@�~�@�@�  @��@�ƨ@��@�7@�/@�I�@���@�j@�P@�I�@�z�@�"�@�ȴ@��#@���@�Z@�!@�@�Q�@߾w@�;d@�33@��@���@ݑh@ݙ�@��@�r�@�1'@�|�@ٺ^@�V@�1'@ו�@��@�t�@�;d@�V@���@� �@Ϯ@�dZ@Ώ\@��#@̬@�9X@�j@̋D@��@��@�M�@�@��T@�`B@�?}@�/@��@���@ȋD@�bN@�Z@��@�K�@�V@��@�@�`B@�7L@��@���@���@�x�@�hs@Ĵ9@��m@�C�@§�@�@�M�@��@��^@��@�9X@�ƨ@��@�ff@�=q@�E�@�=q@�$�@��@�?}@��@��`@���@�9X@���@��@��j@��
@�~�@��7@���@��7@�O�@��D@�A�@��@���@��@��@��-@�G�@�&�@��@�;d@��
@� �@��;@��@���@��7@���@�9X@��
@���@�$�@�E�@��h@���@��D@�j@�I�@� �@��m@�(�@�\)@�v�@��@�x�@�p�@�&�@��`@�Z@�I�@� �@���@��@��@�S�@�;d@��y@���@�{@�%@�Ĝ@��@�I�@��@�  @���@�S�@�o@��y@��@��y@��R@���@�M�@�@�@��@�j@��@��`@���@���@��@���@��@�E�@�@�@��h@�?}@��`@���@�I�@��w@���@�E�@�@�%@���@���@�z�@�A�@���@�|�@�+@�
=@��!@�n�@�M�@�=q@�=q@�-@��@���@�G�@��@��@��9@�I�@�1'@� �@��m@���@��P@�dZ@�33@��@���@��\@�ff@�M�@��@��-@���@��h@�7L@���@�I�@��@�  @��m@��w@��P@���@�S�@�33@�v�@��@��\@���@y%@qhs@g�w@`1'@V��@L�/@D�@<��@5@.v�@&�@!X@Z@bN@1@�;@	�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��;A���Aɝ�AɅA�M�A��Aȥ�A�z�A�p�A�dZA�bNA�^5A�\)A�ZA�ZA�S�A�O�A�M�A�G�A�E�A�E�A�C�A�?}A�?}A�;dA�7LA�+A��A��A�G�AƋDA�jA�C�A�-A�  AĲ-Aę�A�1A���A�-A�oA���A�bNA�A�A�;dA�A�|�A�t�A�|�A���A�bNA��mA��A���A�ZA���A�bA�+A���A�XA���A�JA��PA�ĜA���A�O�A�-A��TA�p�A��-A��mA�"�A�ƨA��^A��jA��A�;dA��TA�G�A�O�A�-A��wA���A�?}A���A���A�  A��A��Az�HAr��AmO�Ah��Ac��Aa"�A^�AZ�AR�APn�AOhsANĜAM��AL1AJ�AF�!AE�#AD�RAA�mA?"�A=��A<n�A:ȴA8��A7��A6E�A5K�A4 �A3x�A2��A0��A0-A/�A/�;A/��A/oA-�A,jA+�A+VA*r�A)?}A(��A(�\A$ȴA"�A!33A�AS�A��A��A!/A!��A!��A"JA"A�A!��A bNA��AA�FA\)AK�A��A~�AS�AC�At�AbNAp�A �A�+AdZA�Ap�A��A�A&�AQ�AJAdZAbA��AO�A�AƨAl�A;dA?}A
=A�/A�A"�A%A�A�-A��A�yAI�A{A��A��A�yA=qAJA��A
��A
VA
  A	;dAM�Ar�A��AĜA	;dA	G�AE�A �A/A%A�A�/A��Av�AjA(�A��At�A�A��Ar�Ar�AffA��A\)AA�`AjAVAbNAQ�A�AhsA ȴA �+A 5?@�^5@��9@��-@��-@�j@��@��@���@��D@�hs@�n�@�I�@�n�@�=q@�P@�w@�~�@�@�  @��@�ƨ@��@�7@�/@�I�@���@�j@�P@�I�@�z�@�"�@�ȴ@��#@���@�Z@�!@�@�Q�@߾w@�;d@�33@��@���@ݑh@ݙ�@��@�r�@�1'@�|�@ٺ^@�V@�1'@ו�@��@�t�@�;d@�V@���@� �@Ϯ@�dZ@Ώ\@��#@̬@�9X@�j@̋D@��@��@�M�@�@��T@�`B@�?}@�/@��@���@ȋD@�bN@�Z@��@�K�@�V@��@�@�`B@�7L@��@���@���@�x�@�hs@Ĵ9@��m@�C�@§�@�@�M�@��@��^@��@�9X@�ƨ@��@�ff@�=q@�E�@�=q@�$�@��@�?}@��@��`@���@�9X@���@��@��j@��
@�~�@��7@���@��7@�O�@��D@�A�@��@���@��@��@��-@�G�@�&�@��@�;d@��
@� �@��;@��@���@��7@���@�9X@��
@���@�$�@�E�@��h@���@��D@�j@�I�@� �@��m@�(�@�\)@�v�@��@�x�@�p�@�&�@��`@�Z@�I�@� �@���@��@��@�S�@�;d@��y@���@�{@�%@�Ĝ@��@�I�@��@�  @���@�S�@�o@��y@��@��y@��R@���@�M�@�@�@��@�j@��@��`@���@���@��@���@��@�E�@�@�@��h@�?}@��`@���@�I�@��w@���@�E�@�@�%@���@���@�z�@�A�@���@�|�@�+@�
=@��!@�n�@�M�@�=q@�=q@�-@��@���@�G�@��@��@��9@�I�@�1'@� �@��m@���@��P@�dZ@�33@��@���@��\@�ff@�M�@��@��-@���@��h@�7L@���@�I�@��@�  @��m@��w@��P@���@�S�@�33@�v�@��@��\@���@y%@qhs@g�w@`1'@V��@L�/@D�@<��@5@.v�@&�@!X@Z@bN@1@�;@	�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	gmB	ffB	gmB	gmB	ffB	gmB	gmB	hsB	hsB	hsB	gmB	gmB	gmB	gmB	gmB	hsB	hsB	gmB	gmB	gmB	gmB	hsB	hsB	hsB	hsB	iyB	jB	jB	q�B	�PB	�XB	��B	��B
B
�B
H�B
k�B
��B
�?B
�ZB
�B
�B
��B
��B1B2-BJ�B|�B�DB��B��B�1B��B=qBR�BO�BQ�BW
BXBXB_;BffBgmBgmBm�Bl�Bn�BjB`BBJ�B{B�yB�TB��B��B�7BcTB49BB
�#B
��B
jB
?}B
�B	�B	�B	ĜB	�B	��B	s�B	C�B	"�B	
=B��B�B�/B��B��B�jB�LB�9B�'B�B�B�'B�B��B��B��B��B��B�LB�/B�BB�HB�`B�B	VB	�B	!�B	"�B	#�B	%�B	'�B	2-B	9XB	C�B	M�B	N�B	K�B	@�B	>wB	?}B	�B	1B��B�B	JB	�B	9XB	M�B	T�B	XB	hsB	s�B	w�B	q�B	^5B	XB	[#B	^5B	gmB	u�B	q�B	w�B	y�B	~�B	|�B	x�B	t�B	o�B	p�B	s�B	x�B	� B	��B	��B	��B	��B	��B	�?B	�dB	�qB	�wB	�jB	�^B	�^B	�^B	��B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ȴB	ɺB	ɺB	ɺB	ɺB	ȴB	ǮB	ŢB	ÖB	��B	��B	�
B	�NB	�B	�mB	�B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�TB	�)B	�HB	�#B	�B	�B	�ZB	�mB	�fB	�TB	�NB	�ZB	�ZB	�TB	�BB	�BB	�BB	�)B	�
B	��B	�B	�#B	�#B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ƨB	��B	�dB	�LB	�FB	�9B	�3B	�B	�B	�B	�B	��B	��B	��B	�B	�?B	�FB	�9B	�FB	�LB	�RB	�XB	�^B	�dB	�^B	�dB	�jB	�qB	��B	��B	��B	B	��B	B	ÖB	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�5B	�NB	�TB	�HB	�/B	�#B	�5B	�mB	�mB	�`B	�mB	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B	��B	��B	��B	��B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
JB
JB
PB
\B
bB
bB
bB
�B
 �B
1'B
33B
=qB
A�B
F�B
L�B
R�B
XB
^5B
cTB
hsB
m�B
r�B
w�B
{�B
~�B
�B
�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	hsB	hsB	hsB	iyB	iyB	iyB	hsB	iyB	iyB	hsB	gmB	gmB	gmB	gmB	gmB	hsB	hsB	gmB	gmB	gmB	gmB	hsB	hsB	hsB	hsB	jB	k�B	m�B	v�B	�{B	��B	��B	��B
B
�B
J�B
o�B
��B
�3B
�fB
�B
��B  B
=B�BC�BaHB�\B�?BÖB��B�PBBL�BcTBcTB\)B^5B]/B`BBffBm�Br�Br�Bq�Bo�Bt�Br�Bo�BhsB&�B�B�B�TB�!B��B�B[#B+B	7B
�9B
�7B
]/B
6FB	��B	�NB	�
B	ĜB	�jB	��B	e`B	B�B	)�B	\B	  B	B��B��BĜB�wB�}B��B�qBÖB�^B�dB�qB�?B�'B�9B�LBĜB�sB�B�yB�B��B	�B	(�B	&�B	$�B	%�B	(�B	/B	;dB	C�B	I�B	R�B	T�B	S�B	F�B	G�B	W
B	.B	uB	B��B	PB	{B	2-B	J�B	S�B	XB	jB	z�B	�%B	�B	gmB	ZB	]/B	_;B	hsB	}�B	x�B	x�B	{�B	�1B	�%B	�B	� B	u�B	q�B	p�B	t�B	y�B	��B	��B	��B	��B	��B	�XB	�}B	ÖB	ÖB	�}B	�dB	�dB	�jB	��B	ĜB	��B	��B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	��B	��B	�B	�`B	�B	�B	�B	�sB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B	��B	��B	��B	��B	��B	��B
B
B
B
B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�)B	�yB	�BB	�B	�
B	�`B	�B	�B	�`B	�TB	�fB	�sB	�mB	�NB	�BB	�fB	�NB	�B	��B	�B	�#B	�#B	�B	�/B	�#B	�5B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	�XB	�FB	�9B	�3B	�B	�!B	�'B	�B	�B	��B	��B	�B	�RB	�FB	�LB	�LB	�LB	�^B	�^B	�dB	�dB	�dB	�dB	�qB	�qB	B	��B	ÖB	ÖB	��B	ÖB	ĜB	ĜB	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�NB	�TB	�fB	�/B	�/B	�5B	�sB	�yB	�`B	�mB	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
  B	��B	��B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
B
%B
+B
1B
+B
1B
1B
1B
1B
	7B
	7B
1B
	7B

=B
	7B
	7B

=B

=B
DB
JB
DB
DB
PB
PB
PB
bB
bB
bB
hB
�B
 �B
1'B
49B
=qB
A�B
F�B
L�B
R�B
XB
^5B
dZB
iyB
m�B
s�B
x�B
{�B
~�B
�B
�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<��
<�C�<�9X<�t�='�=�P<�`B<#�
<#�
<u<�o<���<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<u<�h<�C�<#�
<�o<�j<e`B<ě�=o=��=�w=8Q�<�<�<�`B<���<T��<D��<�C�<�1=\)=@�=+<��<��<���<�1=\)=49X<u<#�
<#�
<49X<u<u<�9X<#�
<e`B<��
<�t�<D��<D��<T��<T��<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�9X<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250152012011312501520120113125015  AO  ARGQ                                                                        20111205112731  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112731  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125015  IP                  G�O�G�O�G�O�                