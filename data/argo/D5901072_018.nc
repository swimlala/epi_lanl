CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:44Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143648  20190522121828  1728_5048_018                   2C  D   APEX                            2142                            040306                          846 @�W��Y�1   @�W�ww�@4y������cEO�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  @���A   A@  A^ffA~ffA�33A�33A�33A�  A�  A�  A�  B   B  BffB  B��B(  B0ffB8  B@  BH  BO��BW��B_��Bg��Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B���B���B���B���B���B�  B�33B�  B�  B�  B���B�  B�33B�33B�  B�  B�  B�33B�  B���B���B���C�fC  C�C  C
  C�C  C  C�C  C  C�C  C  C�fC�fC"  C$  C&  C(  C*  C+�fC-�fC0  C2  C4  C6  C7�fC:�C<�C>�C@�CB�CD�CF  CH  CJ  CL  CN�CP  CQ�fCT  CV�CX�CZ  C\  C]�fC_�fCb  Cd�Cf�Ch  Cj  Cl�Cn  Cp  Cr  Cs�fCv  Cx  Cy�fC{�fC}�fC�fC�  C��C��C�  C�  C��3C��3C�  C��C�  C�  C�  C��3C�  C��C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C��C��C�  C�  C�  C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C��C�  C��3C�  C��C�  C�  C�  C��D   D � D  Dy�D  D� D  Dy�D  D� DfD� D  D�fD  Dy�D  D� D	  D	� D	��D
� D  D�fD  Dy�D  D�fD  D� D  D�fDfD�fDfD�fD  D� D  D� D  D� D  D� D  D� DfD�fDfD� D��D� D  D�fD  Dy�D  D�fD  Dy�D  D� D  D� D   D � D!  D!�fD"  D"� D#fD#� D$  D$� D%  D%�fD&  D&y�D&��D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-fD-�fD.  D.y�D/  D/y�D0  D0�fD1  D1� D1��D2� D3fD3� D4  D4� D4��D5� D6fD6�fD7fD7�fD8fD8� D9  D9� D:  D:� D;fD;�fD<fD<�fD=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ�fDKfDK� DL  DL� DMfDM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Dc  Dc� DdfDd�fDefDe�fDf  Df� Dg  Dgy�Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv�fDw  Dy�fD�  D�@ D�y�D��3D��fD�9�D�� D��3D�fD�9�D�C3Dǰ D��3D���D�ffD��D���D�&fD�c3D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @,��@s33@���@�ffA��A<��A[33A{33A���A���A���A�ffA�ffA�ffA�ffA�ffB33B��B33B��B'33B/��B733B?33BG33BN��BV��B^��Bf��Bo33Bw33B33B���B���B�ffB�ffB���B���B���B���B���B���B�ffB�ffB�ffB�ffB�ffB���B���BǙ�B˙�Bϙ�B�ffBי�B���B���B㙚B癚B뙚B���B�B�ffB�ffB�ffC�3C��C�fC��C	��C�fC��C��C�fC��C��C�fC��C��C�3C�3C!��C#��C%��C'��C)��C+�3C-�3C/��C1��C3��C5��C7�3C9�fC;�fC=�fC?�fCA�fCC�fCE��CG��CI��CK��CM�fCO��CQ�3CS��CU�fCW�fCY��C[��C]�3C_�3Ca��Cc�fCe�fCg��Ci��Ck�fCm��Co��Cq��Cs�3Cu��Cw��Cy�3C{�3C}�3C�3C��fC��3C��3C��fC��fC�ٚC�ٚC��fC��3C��fC��fC��fC�ٚC��fC��3C��fC��fC�ٚC�ٚC��fC��3C��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC��fC��fC��fC��fC�ٚC��fC��fC�ٚC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC�ٚC��fC��fC�ٚC��3C��3C��fC��fC��fC�ٚC��fC��fC�ٚC�ٚC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��3C��fC�ٚC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��3C��fC�ٚC��fC��3C��3C��fC�ٚC��fC��3C��fC��fC��fC��3C��fD s3D �3Dl�D�3Ds3D�3Dl�D�3Ds3D��Ds3D�3Dy�D�3Dl�D�3Ds3D�3D	s3D	��D
s3D
�3Dy�D�3Dl�D�3Dy�D�3Ds3D�3Dy�D��Dy�D��Dy�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Dy�D��Ds3D��Ds3D�3Dy�D�3Dl�D�3Dy�D�3Dl�D�3Ds3D�3Ds3D�3D s3D �3D!y�D!�3D"s3D"��D#s3D#�3D$s3D$�3D%y�D%�3D&l�D&��D's3D'�3D(s3D(�3D)s3D)��D*s3D*�3D+s3D+�3D,s3D,��D-y�D-�3D.l�D.�3D/l�D/�3D0y�D0�3D1s3D1��D2s3D2��D3s3D3�3D4s3D4��D5s3D5��D6y�D6��D7y�D7��D8s3D8�3D9s3D9�3D:s3D:��D;y�D;��D<y�D<�3D=s3D=�3D>y�D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEl�DE�3DFs3DF�3DGs3DG��DHs3DH�3DIs3DI�3DJy�DJ��DKs3DK�3DLs3DL��DMs3DM�3DNy�DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT��DUs3DU�3DVy�DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZl�DZ�3D[s3D[�3D\s3D\�3D]l�D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbl�Db�3Dcs3Dc��Ddy�Dd��Dey�De�3Dfs3Df�3Dgl�Dg�3Dhs3Dh�3Diy�Di�3Djs3Dj�3Dks3Dk�3Dls3Dl��Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du��Dvy�Dv�3Dyy�D���D�9�D�s3D���D�� D�33D�y�D���D�  D�33D�<�Dǩ�D���D��fD�` D�fD��fD�  D�\�D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-A�/A�5?A�5?A�7LA�9XA�9XA�/A�33A�?}A�C�A�G�A�E�A�E�A�G�A�G�A�I�A�K�A�K�A�K�A�M�A�\)AAÏ\A�XAōPA�\)A�r�A�G�A�{A�1A���AŰ!Aş�A�ĜA��/AżjA�bA�JA���A�A�G�A��AhA�+A��7A��RA��TA���A�JA���A���A�A�G�A� �A�|�A�-A�\)A�$�A��mA��A�r�A��9A��A�v�A���A�K�A�bA��A�$�A��wA�bNA���A��hA�A���A���A��A���A���A��A�
=A��HA��9A��#A���A�t�A��jA�?}A��TA�K�A�&�A���A�1'A�bA�/A��wA�A�A���A�/A�K�A�ĜA��A���A��A�ZA��7A��AS�A{p�Ax�\Avv�At��Ar�\Ao�-Al�jAk?}Ai��Ah9XAf�!Ad��Ac��Ab�9AaVA^��AZ�AX�AW/AV{ATȴASS�AQ;dAM�AKl�AJ�`AJ��AJA�AIhsAG�AE�AC�mAC�PABE�AAO�A@�yA@jA?�^A>�`A>�A<�!A9�A7��A7�PA6�A4�A4I�A3��A2�A1�A/��A-�mA,bA*�A*  A(ĜA'��A'"�A&�A&M�A%hsA$I�A#/A!|�A �yA ��A ZA�wA�9A7LA+A1AXA=qA�jAz�A�!AAjA=qA�AA��A�AA��A��A~�A�^A
��A
$�A	�A�A�^A�A�A33AZAXA�A ��A �@�V@�A�@��`@���@�j@�b@�|�@��@�E�@��`@��@��@�A�@��y@�M�@��@�h@��@�Z@��
@��@�D@���@�1'@���@���@�E�@�`B@�Ĝ@�z�@�1@�\)@�
=@���@�ff@��@��`@�33@�-@��/@��@ٲ-@���@�Q�@�|�@�"�@�+@�K�@�;d@�ȴ@�33@�l�@�o@֏\@��@���@ՙ�@��@�I�@ӶF@���@�$�@�G�@�A�@��
@ϕ�@�K�@�o@���@�5?@�Ĝ@�bN@�z�@�ƨ@˝�@�1@�\)@��@�/@�j@�dZ@�M�@�J@őh@ă@�l�@�
=@§�@��@�G�@�A�@�"�@�{@�O�@��9@�z�@�  @�
=@�n�@���@�-@�|�@�(�@�I�@��D@�%@�`B@�X@���@�A�@�S�@��R@��+@�^5@�M�@�E�@�5?@�$�@���@��@�/@��/@��9@��D@��P@�$�@�ff@���@�~�@��@��7@�`B@��@���@�Ĝ@��@�Z@��@��@�"�@�
=@���@�5?@�x�@��`@�I�@��@�33@��H@���@�^5@�E�@��@��-@�&�@��/@��@��u@�bN@�  @�dZ@���@�-@���@��@�X@��@�r�@���@���@��+@�ff@�^5@�V@�-@��@���@�x�@�G�@�Ĝ@�j@�I�@� �@��m@��@��P@�C�@��@�ȴ@��\@�ff@�E�@��@�{@��T@��7@���@�Ĝ@�Ĝ@��@�Z@�b@��w@�K�@��@��+@�E�@��#@���@�X@��9@���@�o@��y@���@���@�V@��@�@�J@��@���@��#@��-@��h@�O�@��@���@�I�@��;@���@�dZ@�o@���@��+@�5?@��#@��^@���@�x�@�/@���@���@�bN@�1'@��@���@�
=@�n�@�E�@���@���@���@���@��7@�x�@�?}@���@��/@���@�r�@�Z@�Q�@�A�@�1'@� �@��@��@�"�@�$�@�V@�j@���@�\)@�@��@��H@��@���@�~�@���@�G�@�&�@��@�"�@�K�@{dZ@p��@f��@]p�@V�@N�@G\)@Bn�@:�@4��@.��@*n�@&ff@ Ĝ@`B@�@J@
�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�-A�/A�5?A�5?A�7LA�9XA�9XA�/A�33A�?}A�C�A�G�A�E�A�E�A�G�A�G�A�I�A�K�A�K�A�K�A�M�A�\)AAÏ\A�XAōPA�\)A�r�A�G�A�{A�1A���AŰ!Aş�A�ĜA��/AżjA�bA�JA���A�A�G�A��AhA�+A��7A��RA��TA���A�JA���A���A�A�G�A� �A�|�A�-A�\)A�$�A��mA��A�r�A��9A��A�v�A���A�K�A�bA��A�$�A��wA�bNA���A��hA�A���A���A��A���A���A��A�
=A��HA��9A��#A���A�t�A��jA�?}A��TA�K�A�&�A���A�1'A�bA�/A��wA�A�A���A�/A�K�A�ĜA��A���A��A�ZA��7A��AS�A{p�Ax�\Avv�At��Ar�\Ao�-Al�jAk?}Ai��Ah9XAf�!Ad��Ac��Ab�9AaVA^��AZ�AX�AW/AV{ATȴASS�AQ;dAM�AKl�AJ�`AJ��AJA�AIhsAG�AE�AC�mAC�PABE�AAO�A@�yA@jA?�^A>�`A>�A<�!A9�A7��A7�PA6�A4�A4I�A3��A2�A1�A/��A-�mA,bA*�A*  A(ĜA'��A'"�A&�A&M�A%hsA$I�A#/A!|�A �yA ��A ZA�wA�9A7LA+A1AXA=qA�jAz�A�!AAjA=qA�AA��A�AA��A��A~�A�^A
��A
$�A	�A�A�^A�A�A33AZAXA�A ��A �@�V@�A�@��`@���@�j@�b@�|�@��@�E�@��`@��@��@�A�@��y@�M�@��@�h@��@�Z@��
@��@�D@���@�1'@���@���@�E�@�`B@�Ĝ@�z�@�1@�\)@�
=@���@�ff@��@��`@�33@�-@��/@��@ٲ-@���@�Q�@�|�@�"�@�+@�K�@�;d@�ȴ@�33@�l�@�o@֏\@��@���@ՙ�@��@�I�@ӶF@���@�$�@�G�@�A�@��
@ϕ�@�K�@�o@���@�5?@�Ĝ@�bN@�z�@�ƨ@˝�@�1@�\)@��@�/@�j@�dZ@�M�@�J@őh@ă@�l�@�
=@§�@��@�G�@�A�@�"�@�{@�O�@��9@�z�@�  @�
=@�n�@���@�-@�|�@�(�@�I�@��D@�%@�`B@�X@���@�A�@�S�@��R@��+@�^5@�M�@�E�@�5?@�$�@���@��@�/@��/@��9@��D@��P@�$�@�ff@���@�~�@��@��7@�`B@��@���@�Ĝ@��@�Z@��@��@�"�@�
=@���@�5?@�x�@��`@�I�@��@�33@��H@���@�^5@�E�@��@��-@�&�@��/@��@��u@�bN@�  @�dZ@���@�-@���@��@�X@��@�r�@���@���@��+@�ff@�^5@�V@�-@��@���@�x�@�G�@�Ĝ@�j@�I�@� �@��m@��@��P@�C�@��@�ȴ@��\@�ff@�E�@��@�{@��T@��7@���@�Ĝ@�Ĝ@��@�Z@�b@��w@�K�@��@��+@�E�@��#@���@�X@��9@���@�o@��y@���@���@�V@��@�@�J@��@���@��#@��-@��h@�O�@��@���@�I�@��;@���@�dZ@�o@���@��+@�5?@��#@��^@���@�x�@�/@���@���@�bN@�1'@��@���@�
=@�n�@�E�@���@���@���@���@��7@�x�@�?}@���@��/@���@�r�@�Z@�Q�@�A�@�1'@� �@��@��@�"�@�$�@�V@�j@���@�\)@�@��@��H@��@���@�~�@���@�G�@�&�@��@�"�@�K�@{dZ@p��@f��@]p�@V�@N�@G\)@Bn�@:�@4��@.��@*n�@&ff@ Ĝ@`B@�@J@
�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
%B
t�B
ŢBA�B�FB��B�B�`B�mB�B�B�B\B)�B(�B{B��B��B��B�B�HB�B�#B��B%�BE�BN�Bk�Bu�Be`BL�BN�BO�BZBaHBs�B�B�B�B�B�\B��B��B��B��B��B�B�-B�9B�9B�-B�B��B�VB{�Bm�B_;BYBP�B=qB@�BN�BC�B;dB�BbB1BB��B�fB��B�dB��B�JBz�B`BBF�B,BbB
��B
�HB
��B
�}B
�B
�PB
iyB
D�B
 �B
JB	��B	�yB	�)B	��B	B	�qB	�FB	�B	��B	��B	��B	��B	��B	�hB	w�B	ffB	YB	P�B	I�B	B�B	49B	#�B	�B	�B	�B	�B	{B	uB		7B	B		7B	1B	1B	PB	VB		7B	B��B�B�TB�B�B��B��BȴBȴBŢB��B�^B�'B��B��B��B��B��B��B��B��B��B��B�uB�oB�\B�PB�DB�1B�B�B~�Bz�Bu�Bq�Bm�Bk�BhsBffBffBffBe`BdZBcTBbNBaHB^5B]/B\)B[#BYBVBS�BP�BM�BL�BI�BH�BF�B@�B>wB<jB;dB;dB7LB5?B6FB5?B5?B49B49B6FB<jBC�BH�BJ�BN�BO�BR�BW
B\)B`BBffBm�Bu�B~�B�%B�+B�7B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�^B�}BǮB��B��B�B�)B�ZB�sB�sB�yB�B��B��B��B��B��B	  B	B	B	B	%B	+B	1B	JB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	uB	{B	�B	�B	�B	�B	 �B	 �B	"�B	$�B	)�B	.B	33B	6FB	<jB	E�B	S�B	]/B	aHB	ffB	k�B	p�B	s�B	u�B	x�B	x�B	y�B	z�B	{�B	}�B	�B	�B	�B	�+B	�JB	�PB	�VB	�\B	�bB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�?B	�FB	�RB	�^B	�dB	�qB	�wB	�wB	�wB	�}B	��B	��B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�;B	�BB	�BB	�BB	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�`B	�TB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�;B	�5B	�;B	�yB
+B
VB
�B
�B
%�B
.B
7LB
>wB
C�B
K�B
S�B
VB
ZB
_;B
dZB
ffB
k�B
m�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
l�B
�^B;dB�FB��B�)B�fB�sB�B�B�B\B-B0!B�B��B��BB�B�`B�;B�TBBA�BYBVBt�B�=Bx�B_;BZBW
B`BBk�B~�B�%B�1B�1B�DB��B��B��B��B�B�!B�9B�LB�XB�^B�XB�XB�B��B�7By�BiyBdZBZB@�BE�B[#BP�BJ�B'�B�B\BVB1B��B�yB�
B�RB��B�\Bw�B\)BC�B%�B+B
��B
�BB
��B
ǮB
�!B
�=B
^5B
49B
�B
B	��B	�B	�HB	��B	ǮB	ÖB	�RB	�'B	�B	��B	�'B	�B	��B	�%B	r�B	bNB	[#B	XB	T�B	J�B	1'B	�B	�B	�B	�B	!�B	$�B	hB	1B	hB	VB	JB	oB	{B	bB	DB	JB	B�B�5B�;B�/B��B��B��B��B��BƨB�qB�!B�B��B��B��B��B��B��B��B��B��B��B�oB�hB�hB�hB�hB�\B�+B�B�B~�B|�Bx�Br�BjBhsBgmBgmBgmBiyBk�BffB`BBaHBbNBaHB`BB]/BYBW
BT�BS�BP�BO�BO�BK�BD�BC�BC�BE�BC�B>wB;dB7LB8RB7LB8RB<jBD�BH�BL�BN�BP�BQ�BT�BYB^5BaHBe`Bl�Bu�B�B�=B�7B�JB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�B�'B�FB�^B�}BǮB��B��B�B�)B�fB�B�sB�B�B��B��B��B��B	  B	B	B	B	%B	+B	+B	DB	JB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	�B	 �B	 �B	$�B	$�B	,B	1'B	33B	7LB	<jB	A�B	S�B	\)B	aHB	e`B	jB	q�B	u�B	w�B	{�B	z�B	y�B	{�B	{�B	}�B	�B	�B	�%B	�7B	�JB	�VB	�VB	�\B	�{B	��B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�!B	�9B	�3B	�?B	�FB	�FB	�FB	�XB	�^B	�dB	�qB	�wB	�wB	�}B	��B	B	B	ĜB	ÖB	ĜB	ĜB	ĜB	ƨB	ȴB	ǮB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�
B	�B	�B	�
B	�
B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�)B	�#B	�B	�#B	�#B	�#B	�)B	�/B	�BB	�BB	�HB	�HB	�HB	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�fB	�ZB	�HB	�BB	�BB	�BB	�BB	�HB	�NB	�;B	�5B	�;B	�yB
+B
VB
�B
�B
%�B
.B
7LB
>wB
D�B
K�B
S�B
W
B
ZB
`BB
dZB
ffB
k�B
m�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<���<���<#�
<#�
<��
<���<�C�<49X<#�
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
<D��<T��<D��<T��<D��<#�
<49X<#�
<#�
<#�
<D��<T��<u<#�
<#�
<#�
<#�
<�C�<���<�1<�/<�9X<��
<��
<�j<�1<�j<��
<�t�<�9X<�1<��
<���=C�=o<���<���<u<e`B<�o<��
<�t�<49X<#�
<D��<49X<49X<#�
<#�
<e`B<��
<�j<e`B<D��<#�
<#�
<e`B<�t�<�9X<T��<#�
<#�
<#�
<#�
<T��<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<�t�<D��<#�
<#�
<49X<#�
<#�
<#�
<49X<D��<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<T��<u<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451592012011014515920120110145159  AO  ARGQ                                                                        20111130143648  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143648  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145159  IP                  G�O�G�O�G�O�                