CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:41Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143530  20190522121828  1728_5048_006                   2C  D   APEX                            2142                            040306                          846 @�9���1   @�9��?�@2š����c�x���1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dy` D�fD�I�D�vfD��fD�� D�P D�l�D��fD��3D�33D�y�D��fD��3D�<�D�l�D�ɚD���D��D�S3D�i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B�  B�  B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B�  B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM��CO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCb  Cc�fCe�fCg�fCi�fCk�fCm�fCo�fCr  Cs�fCv  Cw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�DyY�D�3D�FfD�s3D��3D���D�L�D�i�D��3D�� D�0 D�vfD��3D�� D�9�D�i�D��fD��D��D�P D�ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AՉ7A�|�A�v�A�p�A�n�A�n�A�n�A�n�A�l�A�l�A�^5A�G�A�&�A�  A��A�A�ĜAԝ�A�^5A�I�Aӡ�A�A�A�$�A�{A�x�A��A��A˴9A˶FA��Aɟ�AǕ�A�{A�A��A�`BA�1'A�  A��A���A��A���A�?}A�l�A�ȴA���A�O�A�$�A���A��A��mA��HA���A���A�ĜA��A���A���A�G�A�JA�XA�7LA�
=A���A�VA��A�t�A��7A�l�A��A��A��hA�XA��^A��A��A�x�A��PA�{A��A�33A��mA�+A�O�A��+A���A�l�A�VA��A���A���A�x�A~�A{C�Ay\)Aw�As�#Ar��Arr�Aq��Ap-Ajr�AghsAf9XAd��Ac��Ab��Aa`BA_�A]�;A\�A[�AZ�AY��AX�AW�wAVv�AT��AS�;AR��ARM�AQG�APAOp�AN��ANVAL�\AJ�jAIAG��AF�+AEp�ACS�A=A<Q�A9l�A8��A8E�A8JA7��A5��A3hsA2I�A1O�A0z�A.�RA,9XA*�RA)��A(n�A'�wA&��A%`BA$�/A$ZA#K�A!�A ZA��A��A(�AdZAr�A/A��A�A�
A\)AA�RAn�A��A��A�HAhsA�9A�uA�!A��A
�/A
�uA	33A�`A��A/A&�A�A%A�yA��A��A1'@�=q@�  @�-@�?}@���@���@�1@��@��^@�A�@��m@�@�@�j@�!@���@�I�@�l�@���@�n�@�-@�n�@��@��@�v�@�-@�@�D@��;@�X@�@�F@�ȴ@�ff@��@�7@�&�@�@� �@�C�@݉7@�1'@�\)@ڗ�@��@���@�  @�;d@�
=@֗�@��@�/@ԣ�@�Q�@�1@ӶF@�dZ@��y@ҏ\@�@��@ЋD@�1@�C�@�+@��@�dZ@�1'@Ь@ЋD@���@�V@�p�@ҏ\@�v�@�O�@���@���@�ff@��@���@��@˕�@��m@�1@˕�@�@Ȭ@�33@��T@���@�;d@���@��@�A�@�Z@�I�@�(�@� �@�b@���@���@��7@��9@�j@��@�K�@��^@�G�@��@�bN@�b@��w@�"�@��!@���@�@��-@���@�X@�%@��@�
=@�5?@�%@�j@�1'@�1@��
@��@�"�@��R@��\@�M�@��#@��7@�O�@�/@��@�%@���@�j@�j@�bN@�j@�Z@�A�@�1@�  @��m@�ƨ@�t�@�33@��@���@���@�n�@�-@��T@�@���@��@�hs@�G�@�/@��@��`@��9@���@�z�@�(�@�ƨ@�l�@���@�~�@�^5@�~�@�V@�M�@�5?@�$�@�@���@�p�@���@���@�A�@��m@�K�@�@���@���@�E�@���@�x�@�%@���@��j@���@��;@���@��@��H@��R@���@�n�@�$�@���@�p�@�V@���@��j@��9@��@���@��u@��@�I�@�ƨ@��P@�C�@��y@��+@��7@��`@���@��j@���@�j@�A�@� �@�  @��F@�+@��R@���@�V@��`@��9@��@�I�@�(�@��@��@��H@�5?@���@�x�@�G�@��@���@���@���@�z�@�A�@��@�1@�ƨ@���@�|�@�t�@�l�@�S�@��@��!@�ff@�$�@��T@���@��^@�`B@���@�r�@�9X@�1@��w@���@��@�l�@�S�@�"�@���@���@�v�@�V@�J@��@�@�O�@�&�@��@�Ĝ@���@��j@���@�I�@��@�C�@�33@��@���@���@{�@r�\@n�@f��@^{@SdZ@K�m@F�+@A��@:�@4��@-�@'��@!G�@9X@�w@(�@Q�@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AՉ7A�|�A�v�A�p�A�n�A�n�A�n�A�n�A�l�A�l�A�^5A�G�A�&�A�  A��A�A�ĜAԝ�A�^5A�I�Aӡ�A�A�A�$�A�{A�x�A��A��A˴9A˶FA��Aɟ�AǕ�A�{A�A��A�`BA�1'A�  A��A���A��A���A�?}A�l�A�ȴA���A�O�A�$�A���A��A��mA��HA���A���A�ĜA��A���A���A�G�A�JA�XA�7LA�
=A���A�VA��A�t�A��7A�l�A��A��A��hA�XA��^A��A��A�x�A��PA�{A��A�33A��mA�+A�O�A��+A���A�l�A�VA��A���A���A�x�A~�A{C�Ay\)Aw�As�#Ar��Arr�Aq��Ap-Ajr�AghsAf9XAd��Ac��Ab��Aa`BA_�A]�;A\�A[�AZ�AY��AX�AW�wAVv�AT��AS�;AR��ARM�AQG�APAOp�AN��ANVAL�\AJ�jAIAG��AF�+AEp�ACS�A=A<Q�A9l�A8��A8E�A8JA7��A5��A3hsA2I�A1O�A0z�A.�RA,9XA*�RA)��A(n�A'�wA&��A%`BA$�/A$ZA#K�A!�A ZA��A��A(�AdZAr�A/A��A�A�
A\)AA�RAn�A��A��A�HAhsA�9A�uA�!A��A
�/A
�uA	33A�`A��A/A&�A�A%A�yA��A��A1'@�=q@�  @�-@�?}@���@���@�1@��@��^@�A�@��m@�@�@�j@�!@���@�I�@�l�@���@�n�@�-@�n�@��@��@�v�@�-@�@�D@��;@�X@�@�F@�ȴ@�ff@��@�7@�&�@�@� �@�C�@݉7@�1'@�\)@ڗ�@��@���@�  @�;d@�
=@֗�@��@�/@ԣ�@�Q�@�1@ӶF@�dZ@��y@ҏ\@�@��@ЋD@�1@�C�@�+@��@�dZ@�1'@Ь@ЋD@���@�V@�p�@ҏ\@�v�@�O�@���@���@�ff@��@���@��@˕�@��m@�1@˕�@�@Ȭ@�33@��T@���@�;d@���@��@�A�@�Z@�I�@�(�@� �@�b@���@���@��7@��9@�j@��@�K�@��^@�G�@��@�bN@�b@��w@�"�@��!@���@�@��-@���@�X@�%@��@�
=@�5?@�%@�j@�1'@�1@��
@��@�"�@��R@��\@�M�@��#@��7@�O�@�/@��@�%@���@�j@�j@�bN@�j@�Z@�A�@�1@�  @��m@�ƨ@�t�@�33@��@���@���@�n�@�-@��T@�@���@��@�hs@�G�@�/@��@��`@��9@���@�z�@�(�@�ƨ@�l�@���@�~�@�^5@�~�@�V@�M�@�5?@�$�@�@���@�p�@���@���@�A�@��m@�K�@�@���@���@�E�@���@�x�@�%@���@��j@���@��;@���@��@��H@��R@���@�n�@�$�@���@�p�@�V@���@��j@��9@��@���@��u@��@�I�@�ƨ@��P@�C�@��y@��+@��7@��`@���@��j@���@�j@�A�@� �@�  @��F@�+@��R@���@�V@��`@��9@��@�I�@�(�@��@��@��H@�5?@���@�x�@�G�@��@���@���@���@�z�@�A�@��@�1@�ƨ@���@�|�@�t�@�l�@�S�@��@��!@�ff@�$�@��T@���@��^@�`B@���@�r�@�9X@�1@��w@���@��@�l�@�S�@�"�@���@���@�v�@�V@�J@��@�@�O�@�&�@��@�Ĝ@���@��j@���@�I�@��@�C�@�33@��@���@���@{�@r�\@n�@f��@^{@SdZ@K�m@F�+@A��@:�@4��@-�@'��@!G�@9X@�w@(�@Q�@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBH�BH�BI�BJ�BK�BK�BK�BL�BL�BK�BM�BQ�BYB`BBl�B|�B�%B�DB�DB�DB��B��B��B��B�-B�dBȴB�NB��BuB�BoB�B#�B�B�B�B1'B6FB;dBI�BP�B`BBk�Bq�Bu�Bw�B��B�B�B�!B�'B�-B�3B�3B�LB�}B�wB�-B�9B�B��B��B�LB�RB�+B\)BG�B0!B �B�B+B�B�;B�?B��B�1Bw�Bs�Be`B_;BW
B9XB$�B
�NB
��B
n�B
G�B
S�B
^5B
S�B
I�B
I�B
7LB
-B
�B
JB
B	��B	��B	�BB	�RB	��B	��B	�PB	�+B	�B	x�B	m�B	aHB	R�B	L�B	H�B	C�B	<jB	6FB	1'B	/B	)�B	%�B	&�B	 �B	�B	�B	 �B	�B	bB	�B��B	B	B�B�BB�NB��B�wB�}BB��B�}B�^B�RB�RB�LB�!B�B��B��B��B��B��B��B�B��B��B��B�uB��B��B�hB�PB�PB�\B�DB�+B�%B�B�=B�1B�+B�1B��B�=B�oB��B�1B�7B�=B�Bz�Bx�By�B}�B�B{�B{�B{�B{�Bz�Bz�By�Bs�Bt�Bn�BjBl�Bl�Bm�Bo�Bp�Br�Bt�Bt�Bt�Bs�Bv�Bx�B|�By�Bt�Bu�Bu�Bv�Bz�B� B�B�DB�PB�bB�uB��B��B��B�!B�'B�'B�-B�9B�?B�FB�FB�FB�qB��B��BBǮBŢBƨBƨBƨBǮBɺB��B��B��B��B��B��B��B��B��B�
B�B�;B�`B�fB�sB�B	  B	VB	bB	{B	�B	�B	)�B	2-B	8RB	;dB	<jB	9XB	9XB	;dB	8RB	@�B	B�B	C�B	H�B	J�B	E�B	G�B	I�B	K�B	K�B	L�B	C�B	D�B	G�B	M�B	P�B	P�B	P�B	P�B	W
B	XB	XB	ZB	ZB	YB	ZB	YB	ZB	ZB	ZB	YB	\)B	[#B	aHB	cTB	dZB	dZB	e`B	e`B	ffB	cTB	ffB	l�B	l�B	l�B	l�B	m�B	m�B	n�B	p�B	r�B	s�B	s�B	u�B	w�B	x�B	z�B	|�B	�B	�+B	�7B	�7B	�DB	�JB	�PB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�9B	�?B	�FB	�FB	�LB	�XB	�XB	�RB	�RB	�RB	�XB	�RB	�RB	�RB	�^B	�^B	�dB	�dB	�dB	�dB	�jB	�qB	�wB	�wB	�}B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�HB	�TB	�HB	�HB	�NB	�TB	�TB	�NB	�TB	�fB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
  B
  B	��B	��B
  B
  B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
{B
�B
 �B
$�B
)�B
2-B
;dB
D�B
F�B
I�B
P�B
VB
]/B
bNB
ffB
l�B
p�B
s�B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BH�BH�BI�BJ�BK�BK�BK�BL�BL�BK�BN�BR�BZB`BBl�B}�B�+B�JB�DB�PB��B��B��B��B�jB��BɺB�NB��B�B�B�B,B(�B�B�B �B1'B7LB>wBK�BT�BffBu�Bv�B� B}�B��B�B�B�!B�'B�-B�3B�9B�XBBB�FB�^B�!B��B��B�^B��B�PB`BBP�B7LB"�B�BoB�B�BÖB��B�PBz�Bv�BffB`BB^5BD�B0!B
�B
�XB
{�B
H�B
VB
bNB
YB
N�B
R�B
<jB
33B
'�B
VB
B	��B	��B	�B	��B	�B	��B	�bB	�=B	�1B	~�B	s�B	e`B	VB	O�B	M�B	G�B	?}B	;dB	7LB	2-B	.B	&�B	)�B	#�B	�B	�B	!�B	�B	�B	�B	B	B	+B��B�B�fB��B��B��BÖBÖBŢB��B�jB�jB�dB�LB�LB��B��B��B�B��B��B�B��B��B��B��B��B��B�uB�\B�\B�oB�\B�=B�1B�%B�DB�7B�1B�=B��B�\B��B��B�VB�\B�VB�B{�B}�B�B�B�B{�B{�B{�B|�B{�B~�B� B~�Bx�Bq�Bl�Bm�Bm�Bn�Bq�Br�Bt�Bu�Bu�Bv�Bw�By�By�B� B{�Bu�Bv�Bv�Bv�Bz�B�B�B�JB�VB�oB�{B��B��B��B�'B�-B�-B�3B�?B�FB�LB�LB�XB�}BBBÖBɺBǮBǮBǮBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�#B�HB�`B�fB�sB�B��B	VB	bB	{B	�B	�B	+B	49B	9XB	=qB	?}B	:^B	;dB	>wB	7LB	@�B	B�B	D�B	H�B	L�B	H�B	J�B	K�B	N�B	N�B	N�B	C�B	D�B	G�B	M�B	P�B	P�B	Q�B	P�B	YB	YB	YB	[#B	[#B	[#B	[#B	ZB	[#B	[#B	[#B	ZB	]/B	\)B	aHB	cTB	dZB	e`B	ffB	ffB	iyB	dZB	hsB	m�B	l�B	l�B	l�B	n�B	n�B	o�B	p�B	s�B	t�B	t�B	u�B	w�B	x�B	z�B	}�B	�B	�+B	�7B	�7B	�DB	�JB	�PB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�!B	�-B	�-B	�9B	�FB	�LB	�LB	�RB	�^B	�^B	�XB	�RB	�XB	�^B	�XB	�XB	�XB	�^B	�^B	�dB	�jB	�qB	�dB	�jB	�qB	�wB	�wB	��B	B	ÖB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�NB	�TB	�HB	�HB	�NB	�TB	�TB	�TB	�ZB	�mB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
  B
  B
B
B
  B
  B	��B
  B
  B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
{B
�B
 �B
$�B
)�B
2-B
;dB
D�B
F�B
J�B
P�B
VB
]/B
bNB
ffB
l�B
p�B
s�B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<��
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<49X<�o<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451542012011014515420120110145154  AO  ARGQ                                                                        20111130143530  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143530  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145154  IP                  G�O�G�O�G�O�                