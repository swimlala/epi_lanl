CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:26Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               
A   AO  20111205112443  20190522121836  1901_5055_010                   2C  D   APEX                            2140                            040306                          846 @�BB�<��1   @�BDF(@.���l��cK��$�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bw��B�  B�  B�  B���B�  B�  B���B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�33B�33B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C�fC  C  C�fC  C �C"  C$  C&  C(  C*�C,�C.  C/�fC2  C4  C6  C8  C9�fC<  C>  C?�fCB  CD  CE�fCH  CJ  CL�CN  CP  CR  CT  CV�CX  CZ  C\  C^�C`�Cb�Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|�C~  C�fC�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C�  C�  C�  C��3C��3C�  C��C�  C�  C��3C��3C�  C��C��C��C��C�  C�  C�  C�  C��C�  C��3C�  C�  C��C�  C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C��C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3C�  C��C��C�  C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C��3D   D �fD  D� DfD� D  D�fD  D� D  D� D  Dy�D  D� D  D� D	fD	� D	��D
y�D
��D� DfD�fDfD� D  Dy�D  D�fDfD� D  D� D  Dy�D��D� DfD�fD  D� D  D�fD  Dy�D  D� D  D� D  D� D��D� D  Dy�D  D� D��D� D  D� D fD � D!  D!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(fD(� D(��D)y�D)��D*y�D+  D+� D,  D,� D,��D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5fD5� D6  D6� D6��D7� D8  D8�fD9  D9� D:  D:�fD;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDGfDG�fDH  DHy�DI  DI� DJ  DJ�fDK  DK� DL  DL�fDM  DM� DN  DN� DN��DO� DP  DPy�DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ�fD[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��Dey�Df  Df� Dg  Dg� Dg��Dh� Di  Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv�fDy�fD�fD�)�D��3D�� D��D�Y�D�s3D��3D�� D�)�D�\�D���D��3D�FfD�s3D���D��fD�  D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�33A�ffB��B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bn��Bv��B33B���B���B�ffB���B���B�ffB�ffB���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�B���B���B���Bߙ�B㙚B癚B뙚B���B���B���B���B���C��C�fC��C��C	��C��C��C��C��C��C�3C��C��C�3C��C�fC!��C#��C%��C'��C)�fC+�fC-��C/�3C1��C3��C5��C7��C9�3C;��C=��C?�3CA��CC��CE�3CG��CI��CK�fCM��CO��CQ��CS��CU�fCW��CY��C[��C]�fC_�fCa�fCc��Ce��Cg��Ci�3Ck��Cm��Co��Cq��Cs�3Cu��Cw��Cy��C{�fC}��C�3C��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC�ٚC��fC��3C��3C��fC��fC��fC�ٚC�ٚC��fC��3C��fC��fC�ٚC�ٚC��fC��3C��3C��3C��3C��fC��fC��fC��fC��3C��fC�ٚC��fC��fC��3C��fC�ٚC�ٚC��fC��fC��fC��3C��fC�ٚC��fC��fC��3C��3C��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC�ٚC��fC��3C��3C��fC��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��3C��3C��fC��fC�ٚC��fC��3C��fC��fC��fC��3C��fC��fC��fC�ٚC��fC��fC�ٚC��fC��fC��fC��3C��fC�ٚC��fD y�D �3Ds3D��Ds3D�3Dy�D�3Ds3D�3Ds3D�3Dl�D�3Ds3D�3Ds3D��D	s3D	��D
l�D
��Ds3D��Dy�D��Ds3D�3Dl�D�3Dy�D��Ds3D�3Ds3D�3Dl�D��Ds3D��Dy�D�3Ds3D�3Dy�D�3Dl�D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3Dl�D�3Ds3D��Ds3D�3Ds3D��D s3D �3D!s3D!��D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&y�D&�3D's3D'��D(s3D(��D)l�D)��D*l�D*�3D+s3D+�3D,s3D,��D-s3D-��D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2y�D2�3D3s3D3�3D4s3D4��D5s3D5�3D6s3D6��D7s3D7�3D8y�D8�3D9s3D9�3D:y�D:�3D;s3D;�3D<l�D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAy�DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFy�DF��DGy�DG�3DHl�DH�3DIs3DI�3DJy�DJ�3DKs3DK�3DLy�DL�3DMs3DM�3DNs3DN��DOs3DO�3DPl�DP�3DQs3DQ��DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY��DZy�DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_��D`l�D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd��Del�De�3Dfs3Df�3Dgs3Dg��Dhs3Dh�3Dis3Di��Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dol�Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3DvٚDyy�D�  D�#3D���D���D��3D�S3D�l�D���D��D�#3D�VfD��fD���D�@ D�l�D��3D�� D���D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��TA���A٬AٓuAمA�r�A�bNA�ZA�K�A�?}A�33A�(�A��A�VA�A��A��A���A�ȴA�ĜA���AؼjAؾwAؾwAؼjAظRAخAؑhA�VA�`BA�ĜA��yA�G�A�  A�A�C�A�
=A�dZA��`Aʝ�A�jAǴ9A�v�A�l�A�1'A���A�ĜA�p�A��A�ȴA���A�n�A��7A�1A�VA���A�{A��\A��!A�z�A���A��9A�$�A��!A���A�O�A���A�/A���A�VA�&�A�A��PA��RA�G�A��A�%A��A��
A��RA�7LA���A�  A�hsA��HA���A��DA���A��A���A���A�+A~�+A{�7Ayp�Aw��At�ApM�AmAh�\Ae�#A`�A[|�AV��AQ�AO?}AO+AMG�AK��AI?}AF{AB�+A>�A<�uA9��A7�#A6�\A5hsA4{A1�7A.��A-XA*ȴA(�A(bNA'�A&�9A$Q�A#/A!C�A|�A;dA��A�A�mA�A�A��A��A�jAƨA�`Ar�Ax�AhsAĜA;dA��AM�Al�Ax�A�A�9AG�A=qA��A|�A��A=qA  A�A
�RA	�-A	�A�DAƨAG�A|�Al�A��AffA�TA��A/A�HA�DA�mA`BA%A�A�A�#A��A+A r�A @��
@���A J@��;@��@�M�@�J@���@��/@��
@�o@���@���@��w@�S�@�|�@�+@�E�@�J@��^@�j@��H@�V@��#@�7L@�Ĝ@��`@�Ĝ@��@��m@�"�@�M�@��@���@�h@�9X@��@�1@�|�@��@��H@�
=@�h@�j@���@���@�=q@�@�I�@��
@�;d@�ff@��@��@��@��@��y@ް!@ޗ�@އ+@�=q@�5?@�M�@�ff@�33@ߍP@ޟ�@�/@�bN@���@��T@�  @�C�@�@��@���@�O�@�9X@�;d@�n�@�{@ѩ�@��@Гu@�j@Ϯ@�\)@�@��@���@���@��#@�V@�9X@��@�r�@� �@��@�J@�G�@ȋD@� �@�1@� �@��
@�K�@��H@��@�O�@ă@�33@��-@���@�z�@�z�@��;@��H@�=q@���@���@��h@��@�  @��
@�ƨ@�K�@�
=@��+@�J@���@�7L@�(�@��@���@�5?@��@���@�&�@��j@�Q�@�l�@��R@�E�@��@��@��@��@��@���@�"�@���@�v�@�@���@��@��D@��@���@�&�@��@�z�@��`@���@��u@�Z@� �@��m@��F@�|�@�;d@��@��R@��+@�{@�X@���@�(�@���@��@�$�@�`B@�Ĝ@�z�@�A�@���@�C�@��@�=q@�5?@�E�@�=q@���@�?}@�Ĝ@�bN@�(�@���@�ƨ@��@�+@��y@���@�V@�E�@�=q@�J@��T@�@�p�@�7L@�&�@��@��`@��9@�9X@�1@��;@��F@��@�o@��@��+@�E�@�=q@�$�@�{@�@�X@���@�Z@�1'@�A�@�z�@�1@��@�ƨ@�l�@�33@��H@���@��+@�~�@�=q@�{@���@��#@��7@��/@��@�Q�@�1'@��m@���@�\)@�"�@�@��@��\@�V@�E�@�=q@�{@��7@��`@���@�j@�A�@��@��
@�33@��y@��@��!@�V@��@��@��^@���@���@�`B@�&�@���@��9@�I�@�b@�  @��;@���@�"�@���@���@��\@�=q@��@��h@�p�@�O�@�7L@���@��u@�9X@��m@��w@�;d@��@�@��7@��@y&�@p�`@g
=@^��@V��@P �@K@C��@;S�@3dZ@,�@&�@!%@p�@�^@5?@"�@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��TA���A٬AٓuAمA�r�A�bNA�ZA�K�A�?}A�33A�(�A��A�VA�A��A��A���A�ȴA�ĜA���AؼjAؾwAؾwAؼjAظRAخAؑhA�VA�`BA�ĜA��yA�G�A�  A�A�C�A�
=A�dZA��`Aʝ�A�jAǴ9A�v�A�l�A�1'A���A�ĜA�p�A��A�ȴA���A�n�A��7A�1A�VA���A�{A��\A��!A�z�A���A��9A�$�A��!A���A�O�A���A�/A���A�VA�&�A�A��PA��RA�G�A��A�%A��A��
A��RA�7LA���A�  A�hsA��HA���A��DA���A��A���A���A�+A~�+A{�7Ayp�Aw��At�ApM�AmAh�\Ae�#A`�A[|�AV��AQ�AO?}AO+AMG�AK��AI?}AF{AB�+A>�A<�uA9��A7�#A6�\A5hsA4{A1�7A.��A-XA*ȴA(�A(bNA'�A&�9A$Q�A#/A!C�A|�A;dA��A�A�mA�A�A��A��A�jAƨA�`Ar�Ax�AhsAĜA;dA��AM�Al�Ax�A�A�9AG�A=qA��A|�A��A=qA  A�A
�RA	�-A	�A�DAƨAG�A|�Al�A��AffA�TA��A/A�HA�DA�mA`BA%A�A�A�#A��A+A r�A @��
@���A J@��;@��@�M�@�J@���@��/@��
@�o@���@���@��w@�S�@�|�@�+@�E�@�J@��^@�j@��H@�V@��#@�7L@�Ĝ@��`@�Ĝ@��@��m@�"�@�M�@��@���@�h@�9X@��@�1@�|�@��@��H@�
=@�h@�j@���@���@�=q@�@�I�@��
@�;d@�ff@��@��@��@��@��y@ް!@ޗ�@އ+@�=q@�5?@�M�@�ff@�33@ߍP@ޟ�@�/@�bN@���@��T@�  @�C�@�@��@���@�O�@�9X@�;d@�n�@�{@ѩ�@��@Гu@�j@Ϯ@�\)@�@��@���@���@��#@�V@�9X@��@�r�@� �@��@�J@�G�@ȋD@� �@�1@� �@��
@�K�@��H@��@�O�@ă@�33@��-@���@�z�@�z�@��;@��H@�=q@���@���@��h@��@�  @��
@�ƨ@�K�@�
=@��+@�J@���@�7L@�(�@��@���@�5?@��@���@�&�@��j@�Q�@�l�@��R@�E�@��@��@��@��@��@���@�"�@���@�v�@�@���@��@��D@��@���@�&�@��@�z�@��`@���@��u@�Z@� �@��m@��F@�|�@�;d@��@��R@��+@�{@�X@���@�(�@���@��@�$�@�`B@�Ĝ@�z�@�A�@���@�C�@��@�=q@�5?@�E�@�=q@���@�?}@�Ĝ@�bN@�(�@���@�ƨ@��@�+@��y@���@�V@�E�@�=q@�J@��T@�@�p�@�7L@�&�@��@��`@��9@�9X@�1@��;@��F@��@�o@��@��+@�E�@�=q@�$�@�{@�@�X@���@�Z@�1'@�A�@�z�@�1@��@�ƨ@�l�@�33@��H@���@��+@�~�@�=q@�{@���@��#@��7@��/@��@�Q�@�1'@��m@���@�\)@�"�@�@��@��\@�V@�E�@�=q@�{@��7@��`@���@�j@�A�@��@��
@�33@��y@��@��!@�V@��@��@��^@���@���@�`B@�&�@���@��9@�I�@�b@�  @��;@���@�"�@���@���@��\@�=q@��@��h@�p�@�O�@�7L@���@��u@�9X@��m@��w@�;d@��@�@��7@��@y&�@p�`@g
=@^��@V��@P �@K@C��@;S�@3dZ@,�@&�@!%@p�@�^@5?@"�@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
$�B
'�B
5?B
A�B
I�B
R�B
_;B
l�B
{�B
�B
�+B
�\B
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
ŢB
��B
�B
�fB
�ZB
�;B
�5B
�
B
��B
�B
�B
�#B
�
B
�B
�BS�B��B�/B�`B�B%BBB��B��BB+B%B��BB	7B�B�B �B(�B)�B'�B�BhBBB��B�?B��B|�BYB?}BG�BO�B6FBF�B!�B
�B
�dB
�VB
ZB
"�B
B	��B	�B	�-B	�B	��B	�uB	�B	t�B	gmB	XB	@�B	&�B	PB�B��BB�?B�LBÖB�dB�FB�!B��B��B�PB�PB�JB�=B�7B�DB�1B�+B�7B�PB�\B�{B��B��B��B�oB�hB�uB�oB�\B��B��B��B�\B�7B�B�B{�B}�B|�B{�B|�B�hB��B�B��B�#B�HB��B��B	B	  B	�B	�B	�B	 �B	#�B	(�B	0!B	0!B	-B	0!B	.B	-B	+B	5?B	A�B	G�B	J�B	N�B	N�B	N�B	R�B	W
B	VB	VB	XB	`BB	`BB	aHB	bNB	gmB	ffB	e`B	ffB	hsB	r�B	t�B	s�B	r�B	s�B	s�B	r�B	q�B	s�B	u�B	t�B	q�B	q�B	x�B	z�B	y�B	x�B	w�B	w�B	v�B	t�B	s�B	s�B	u�B	|�B	� B	�B	�B	�+B	�+B	�+B	�7B	�PB	�VB	�\B	�bB	�uB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�LB	�dB	�XB	�RB	�RB	�XB	�RB	�?B	�?B	�FB	�FB	�FB	�?B	�FB	�FB	�LB	�RB	�RB	�LB	�RB	�qB	�qB	�jB	�jB	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	ȴB	ǮB	ƨB	ƨB	ɺB	ȴB	ƨB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�
B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�
B	�B	�B	�#B	�/B	�;B	�`B	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
  B
  B
  B
  B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B

=B
DB
DB
JB
DB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
VB
\B
bB
hB
oB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
+B
33B
7LB
:^B
B�B
G�B
M�B
Q�B
XB
ZB
`BB
e`B
iyB
m�B
r�B
v�B
x�B
{�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
&�B
(�B
6FB
B�B
J�B
S�B
`BB
l�B
|�B
�B
�+B
�\B
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
�?B
��B
��B
�5B
�B
�B
�TB
�sB
�B
��B
�qB
ŢB
��B
��BBB_;B�XB�B�B��BoBVB	7BB��BJB�B�B  B1BJB�B!�B"�B1'B<jB8RB+B�BoB�B�fB��B�B��Bo�BM�B\)BhsBC�B_;BF�BB
�
B
�3B
�B
;dB
{B
 �B
  B	ƨB	�}B	�XB	��B	��B	�bB	�1B	s�B	`BB	B�B	6FB	oB��B�;BB�jB��B��BɺBȴB�qB�3B��B��B��B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�\B�B�B�B�B}�B�uB��B��B��B�BB�NB��B	  B	DB��B	�B	�B	�B	$�B	&�B	.B	7LB	7LB	0!B	8RB	33B	0!B	+B	7LB	E�B	L�B	N�B	Q�B	Q�B	Q�B	W
B	\)B	ZB	YB	\)B	cTB	bNB	dZB	gmB	l�B	iyB	ffB	gmB	gmB	t�B	w�B	u�B	t�B	u�B	w�B	v�B	t�B	v�B	z�B	z�B	r�B	q�B	z�B	}�B	{�B	{�B	|�B	|�B	x�B	v�B	u�B	t�B	u�B	}�B	�B	�B	�+B	�=B	�7B	�1B	�DB	�hB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�RB	�}B	�wB	�dB	�RB	��B	�wB	�?B	�XB	�LB	�FB	�XB	�XB	�^B	�XB	�XB	�RB	�^B	�XB	�XB	��B	�qB	��B	�jB	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	��B	��B	��B	��B	ɺB	ƨB	ǮB	ɺB	��B	ȴB	ǮB	ȴB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�)B	�)B	�/B	�5B	�`B	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
B
  B
B
B
B
  B
B
B
B
B
B
B
B
  B
  B
B
B
+B
%B
1B
	7B
	7B
	7B

=B

=B
	7B
DB
JB
JB
PB
JB
VB
JB
JB
PB
VB
VB
VB
VB
VB
\B
\B
bB
\B
\B
\B
bB
hB
hB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
,B
33B
7LB
:^B
B�B
H�B
N�B
Q�B
XB
ZB
`BB
e`B
iyB
m�B
r�B
v�B
x�B
{�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��
=t�<�C�<ě�=\)<��<�j<�t�<49X<�o<�t�<T��<T��<D��<#�
<#�
<#�
<#�
<49X<�o<�C�<49X<#�
<#�
<#�
<#�
<#�
<#�
<�t�<�o<e`B<D��<u<ě�<�1<�9X<�1<�h<�9X<e`B<��
<ě�<T��<ě�=t�<ě�<�/=t�=�w<ě�<�C�=�P=��<��
<�o<�9X<�o<�t�<�/=o<�/<��<�/=#�
=C�=+<�`B<T��<#�
<T��<u<�t�<ě�<ě�<���<u<���<49X<#�
<#�
<D��<���<�o<D��<u<#�
<#�
<#�
<#�
<u<#�
<e`B<T��<T��<#�
<#�
<#�
<#�
<#�
<#�
<T��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250102012011312501020120113125010  AO  ARGQ                                                                        20111205112443  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112443  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125010  IP                  G�O�G�O�G�O�                