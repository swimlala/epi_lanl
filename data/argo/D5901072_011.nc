CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:42Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143603  20190522121828  1728_5048_011                   2C  D   APEX                            2142                            040306                          846 @�FJj�`1   @�FJ����@3�j~��#�c�\(�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @&ff@�  @�  A   A   A@  A^ffA~ffA�33A�33A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�33B�  B�  B�  B���B�  B�33B�  B�  B���B�  B�  B���B���B���B�  C �C  C�fC  C  C
�C�C  C�fC  C  C�fC  C  C�C  C�fC"  C$  C&  C(  C*  C,�C.  C/�fC1�fC3�fC5�fC7�fC:  C<�C>  C@  CB�CD�CF  CH  CJ  CL  CM�fCP  CR�CT  CV  CX  CZ  C[�fC]�fC_�fCa�fCc�fCf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C��C��C�  C�  C��3C�  C��C��C�  C��C��C��C��C�  C��3C��3C�  C�  C�  C��C��C��C�  C��3C�  C��C�  C��3C��C��C��C��C��C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C��C�  C��3C��3C�  C�  C��C�  C��3C��3C��3C��3C�  C�  C�  C�  C��C��C�  C��3C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C��C��C��C��C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D ��Dy�D  D� D  D�fD  D� D  Dy�D  D� D  D�fDfD�fD	  D	y�D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D�fD  D� D��D� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D�fDfD� D  D� D  Dy�D��D �fD!fD!� D!��D"� D#  D#� D$fD$� D%  D%�fD&  D&� D'fD'�fD(fD(�fD)  D)� D*  D*� D*��D+� D,fD,� D,��D-� D.  D.y�D/  D/�fD0  D0y�D1  D1� D2fD2� D2��D3� D4  D4y�D5  D5y�D5��D6y�D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DKy�DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[�fD\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Dsy�Dt  Dt� Du  Du� Dv  Dv� DyffD�fD�I�D�l�D���D��3D�0 D�c3D���D��fD�` D�y�Dǣ3D��3D�3DچfD�3D��fD��D�S3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@s33@���@���A��A<��A[33A{33A���A���A�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B��B'33B/33B733B?33BG33BN��BW33B_33Bg33Bo33Bw33B33B�ffB���B���B���B���B���B���B���B���B���B���B���B���B�ffB���B���BÙ�BǙ�B˙�B�ffBә�B���Bۙ�Bߙ�B�ffB癚B뙚B�ffB�ffB�ffB���B���C��C�3C��C��C	�fC�fC��C�3C��C��C�3C��C��C�fC��C�3C!��C#��C%��C'��C)��C+�fC-��C/�3C1�3C3�3C5�3C7�3C9��C;�fC=��C?��CA�fCC�fCE��CG��CI��CK��CM�3CO��CQ�fCS��CU��CW��CY��C[�3C]�3C_�3Ca�3Cc�3Ce��Cg�fCi�fCk��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C�3C��fC��3C��3C��fC��fC�ٚC��fC��3C��3C��fC��3C��3C��3C��3C��fC�ٚC�ٚC��fC��fC��fC��3C��3C��3C��fC�ٚC��fC��3C��fC�ٚC��3C��3C��3C��3C��3C��fC��fC��fC��fC�ٚC��fC��3C��fC�ٚC��fC��3C��fC��fC��3C��fC�ٚC�ٚC��fC��fC��3C��fC�ٚC�ٚC�ٚC�ٚC��fC��fC��fC��fC��3C��3C��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��3C��fC�ٚC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��3C��3C��3C��3C��3C��3C��fC��fC�ٚC�ٚC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3D s3D ��Dl�D�3Ds3D�3Dy�D�3Ds3D�3Dl�D�3Ds3D�3Dy�D��Dy�D�3D	l�D	�3D
s3D
�3Dl�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3Ds3D�3Dy�D�3Ds3D��Ds3D�3Dl�D�3Ds3D�3Dy�D�3Ds3D�3Ds3D�3Ds3D�3Dy�D��Ds3D�3Ds3D�3Dl�D��D y�D ��D!s3D!��D"s3D"�3D#s3D#��D$s3D$�3D%y�D%�3D&s3D&��D'y�D'��D(y�D(�3D)s3D)�3D*s3D*��D+s3D+��D,s3D,��D-s3D-�3D.l�D.�3D/y�D/�3D0l�D0�3D1s3D1��D2s3D2��D3s3D3�3D4l�D4�3D5l�D5��D6l�D6�3D7s3D7�3D8s3D8��D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ��DKl�DK�3DLs3DL�3DMs3DM��DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWy�DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[y�D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgy�Dg��Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Dry�Dr�3Dsl�Ds�3Dts3Dt�3Dus3Du�3Dvs3DyY�D� D�C3D�ffD��fD���D�)�D�\�D��fD�� D�Y�D�s3Dǜ�D���D��Dڀ D��D�� D�fD�L�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aԕ�AԑhAԓuA�(�A�A�r�A�=qA�A��AҶFAҗ�A�ffA�K�A�(�A���A���AѲ-AѓuA�r�A�^5A�M�A�7LA��A�VA�1A�ĜAП�A�\)A��A�ƨAϲ-Aω7A�&�A��HA�"�A�-A̩�A�VA��TA�"�A�A�Aȉ7AƸRA���A��/A��AÙ�A�/A���A�VA���A��A�^5A�dZA���A�S�A��`A���A�ffA��mA���A��RA��+A�C�A��jA�K�A��A��;A��A�A�A��;A��A�bA�9XA�XA�ƨA���A���A��TA���A��`A�ffA��A�dZA�JA���A�{A�A�bA�n�A�t�A�S�A��A��-A�x�A�A�hsA�;dA��A�1'A�^5A�v�A�"�A�r�A~ZAst�Aq��Ao�TAo;dAnA�Aj��Afv�Ab�RA`�9A_��A^��A]\)AZ(�AVAT��AS+AQ?}AOVANJALQ�AH��AGK�AE�-AAƨA?;dA>�A;VA:  A8��A7x�A5�7A333A0��A.�A-VA+��A*  A)%A'��A&r�A%�A%+A$bNA#ƨA#/A"��A �A�A��A��A��AbNAAp�A�RAbAK�AȴAbNA�;A�PA"�A�!AVA�-AbAXAĜAG�A��A��A$�AXA	�TA�+A?}A��AjAA�AAĜA(�A�;A��A?}A ĜA I�@���@��y@�X@�1'@�l�@�C�@�
=@�~�@��h@� �@��@���@��@�;d@�ff@��@���@�E�@�x�@��@��@�u@�bN@�9X@��@��m@�ƨ@땁@�o@�J@���@�33@��@�G�@�z�@�\)@�M�@���@��@�/@���@�1'@�ƨ@�;d@���@�X@۶F@�@�J@��/@�j@�1@׶F@׍P@�dZ@�S�@�C�@�"�@�ȴ@���@�1'@ӕ�@�S�@�;d@�;d@�+@�"�@�o@��y@җ�@���@�`B@��@Ь@�Z@ϥ�@�v�@́@�&�@�%@�Ĝ@̃@�1'@ˍP@��@��y@�ff@��#@�G�@�j@�l�@���@�~�@��#@�Ĝ@�1@�+@�J@�x�@���@�1'@��
@��@���@��9@��@�33@�E�@���@��u@�(�@��@��H@��+@�{@��@��`@�r�@�9X@�b@��;@��@�33@��H@�^5@��@��#@���@��7@��7@�x�@�p�@�`B@��@���@��@�ƨ@���@��@�l�@�"�@��+@��@��-@���@���@��h@��@�x�@�`B@�O�@�G�@�7L@�&�@�&�@�&�@��@��@��@�V@��`@��@�A�@��m@�|�@�@��R@���@��!@��R@���@���@��\@��+@�v�@�^5@�M�@��@���@�hs@�%@��@�I�@��@�  @��@���@�|�@�o@��H@���@���@���@�n�@�^5@�M�@�$�@��@��@��@���@��h@�hs@�`B@�7L@�V@���@���@�z�@� �@��w@�dZ@�33@���@�{@��^@��h@��h@��7@�x�@�p�@�p�@�O�@��@�Z@��@�;d@��@��R@�M�@��@��^@���@��@�X@��@��j@�Z@�1'@�1'@�(�@�1@��m@���@��w@��F@���@�;d@�
=@���@�M�@���@�`B@�`B@�7L@�V@��`@�Ĝ@���@��u@�j@�1'@��
@���@�t�@�l�@�\)@�
=@��+@�@�hs@�&�@��@��@�V@��@�V@�%@��@���@��9@��D@�Z@�(�@���@��@�S�@��y@�~�@�^5@�M�@�5?@�{@��^@��7@��@�X@��`@���@�j@��P@�r�@�C�@z��@p  @i�@a�@^$�@Yx�@M`B@?�@6�@1�7@+��@#��@z�@�9@@�H@��@
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aԕ�AԑhAԓuA�(�A�A�r�A�=qA�A��AҶFAҗ�A�ffA�K�A�(�A���A���AѲ-AѓuA�r�A�^5A�M�A�7LA��A�VA�1A�ĜAП�A�\)A��A�ƨAϲ-Aω7A�&�A��HA�"�A�-A̩�A�VA��TA�"�A�A�Aȉ7AƸRA���A��/A��AÙ�A�/A���A�VA���A��A�^5A�dZA���A�S�A��`A���A�ffA��mA���A��RA��+A�C�A��jA�K�A��A��;A��A�A�A��;A��A�bA�9XA�XA�ƨA���A���A��TA���A��`A�ffA��A�dZA�JA���A�{A�A�bA�n�A�t�A�S�A��A��-A�x�A�A�hsA�;dA��A�1'A�^5A�v�A�"�A�r�A~ZAst�Aq��Ao�TAo;dAnA�Aj��Afv�Ab�RA`�9A_��A^��A]\)AZ(�AVAT��AS+AQ?}AOVANJALQ�AH��AGK�AE�-AAƨA?;dA>�A;VA:  A8��A7x�A5�7A333A0��A.�A-VA+��A*  A)%A'��A&r�A%�A%+A$bNA#ƨA#/A"��A �A�A��A��A��AbNAAp�A�RAbAK�AȴAbNA�;A�PA"�A�!AVA�-AbAXAĜAG�A��A��A$�AXA	�TA�+A?}A��AjAA�AAĜA(�A�;A��A?}A ĜA I�@���@��y@�X@�1'@�l�@�C�@�
=@�~�@��h@� �@��@���@��@�;d@�ff@��@���@�E�@�x�@��@��@�u@�bN@�9X@��@��m@�ƨ@땁@�o@�J@���@�33@��@�G�@�z�@�\)@�M�@���@��@�/@���@�1'@�ƨ@�;d@���@�X@۶F@�@�J@��/@�j@�1@׶F@׍P@�dZ@�S�@�C�@�"�@�ȴ@���@�1'@ӕ�@�S�@�;d@�;d@�+@�"�@�o@��y@җ�@���@�`B@��@Ь@�Z@ϥ�@�v�@́@�&�@�%@�Ĝ@̃@�1'@ˍP@��@��y@�ff@��#@�G�@�j@�l�@���@�~�@��#@�Ĝ@�1@�+@�J@�x�@���@�1'@��
@��@���@��9@��@�33@�E�@���@��u@�(�@��@��H@��+@�{@��@��`@�r�@�9X@�b@��;@��@�33@��H@�^5@��@��#@���@��7@��7@�x�@�p�@�`B@��@���@��@�ƨ@���@��@�l�@�"�@��+@��@��-@���@���@��h@��@�x�@�`B@�O�@�G�@�7L@�&�@�&�@�&�@��@��@��@�V@��`@��@�A�@��m@�|�@�@��R@���@��!@��R@���@���@��\@��+@�v�@�^5@�M�@��@���@�hs@�%@��@�I�@��@�  @��@���@�|�@�o@��H@���@���@���@�n�@�^5@�M�@�$�@��@��@��@���@��h@�hs@�`B@�7L@�V@���@���@�z�@� �@��w@�dZ@�33@���@�{@��^@��h@��h@��7@�x�@�p�@�p�@�O�@��@�Z@��@�;d@��@��R@�M�@��@��^@���@��@�X@��@��j@�Z@�1'@�1'@�(�@�1@��m@���@��w@��F@���@�;d@�
=@���@�M�@���@�`B@�`B@�7L@�V@��`@�Ĝ@���@��u@�j@�1'@��
@���@�t�@�l�@�\)@�
=@��+@�@�hs@�&�@��@��@�V@��@�V@�%@��@���@��9@��D@�Z@�(�@���@��@�S�@��y@�~�@�^5@�M�@�5?@�{@��^@��7@��@�X@��`@���@�j@��P@�r�@�C�@z��@p  @i�@a�@^$�@Yx�@M`B@?�@6�@1�7@+��@#��@z�@�9@@�H@��@
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBk�Bk�BjBjBl�Bl�BjBhsBe`BaHB^5BXBVBT�BR�BN�BL�BI�BD�BB�BA�B@�B<jB<jB>wBA�BC�BH�BO�BT�BaHBp�B~�B~�B�%B�hB��B��B��B��B�B�LB��B�#B�BB�fB�B�B%B%�B1'BA�BM�BT�BZB_;BffBw�B�B�=B��B��B��B��B�?B�dBǮB��B��B��B��B��B��B��B�B��B�DB�=B�{B�uB�JBl�BN�BB�B8RB.B�B
=B�B�#B�B��B�\B�1B� Bq�BG�BDB
�BB
�9B
{�B
[#B
7LB
B	��B	�hB	�%B	{�B	s�B	jB	_;B	A�B	,B	�B	{B	+B��B�fB�B�yB�B�sB�ZB�;B�#BɺB�qB�FB�9B�FB�FB�^B�LB�9B�'B�B��B��B��B��B��B��B��B�{B��B�{B�uB�{B�oB�hB�\B�hB�hB�\B�\B�bB�bB�bB�\B�\B�VB�\B�\B�VB�PB�JB�DB�7B�+B�B�B� B�B�1B�+B�1B�+B�%B�7B�7B�DB�DB�=B�=B�1B�7B�=B�DB�JB�JB�PB�VB�bB�hB�hB�hB�bB�\B�VB�PB�=B�1B�%B�%B�B�%B�%B�%B�1B�\B�bB�oB�uB�{B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�RB�dB��B��BÖBĜBŢBŢBŢBŢBŢBƨB��B��B�B�
B�B�
B�B�B�B�B�B�)B�;B�NB�TB�`B�yB�B��B��B��B��B��B��B	B	B	%B		7B	DB	VB	uB	�B	�B	�B	 �B	%�B	(�B	-B	2-B	49B	8RB	9XB	:^B	;dB	?}B	D�B	G�B	I�B	L�B	Q�B	R�B	T�B	VB	ZB	\)B	^5B	`BB	cTB	ffB	gmB	gmB	hsB	iyB	k�B	l�B	n�B	p�B	p�B	r�B	r�B	r�B	r�B	r�B	s�B	t�B	v�B	x�B	z�B	z�B	z�B	z�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�+B	�+B	�+B	�+B	�1B	�=B	�JB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�9B	�LB	�XB	�XB	�XB	�^B	�jB	�jB	�jB	�qB	�wB	�}B	��B	��B	B	B	B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�;B	�;B	�;B	�BB	�HB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B
%B
hB
�B
'�B
(�B
/B
1'B
5?B
?}B
XB
R�B
XB
\)B
cTB
k�B
n�B
q�B
t�B
z�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bk�Bk�Bm�Bm�Bn�Bn�Bl�BjBffBbNB`BBYBXBW
BS�BO�BM�BJ�BE�BC�BB�BA�B=qB=qB@�BC�BF�BK�BP�BVBcTBs�B�B�B�VB��B��B��B��B�'B�FBƨB�/B�ZB�sB�B��B%B"�B9XB:^BJ�BW
B\)B`BBffBp�B~�B�=B��B��B��B��B�B�jBÖB��B��B��B��B�B�B�#B��B�9B��B��B�oB��B��B��Bz�BVBG�B>wB7LB/B�BDB��B�XB��B�uB�\B�hB��Bv�B-B1B
�NB
��B
�B
e`B
<jB
B	��B	�oB	�B	�B	�B	}�B	YB	9XB	(�B	�B	�B	hB��B�TB��B��B��B�B�B�B�
B��B��BĜBBɺBÖB��B��B��B�}B�^B�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B��B��B��B�{B�uB�uB�oB�bB�bB�\B�VB�hB��B��B�hB�VB�JB�DB�VB�bB�hB�uB�oB�bB�VB�bB�oB�hB�hB�VB�VB�\B�\B�hB�oB�{B��B��B��B�uB�bB�bB�bB�VB�VB�PB�DB�%B�=B�=B�JB�VB�oB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�LB�XB�jB�}BBÖBĜBŢBƨBƨBƨBƨBǮB��B��B��B�
B�
B�B�
B�B�B�B�B�B�5B�HB�ZB�TB�sB�B�B��B��B��B��B��B	B	B	%B	%B	DB	VB	hB	�B	�B	�B	 �B	$�B	(�B	-B	-B	5?B	7LB	:^B	;dB	:^B	@�B	C�B	D�B	J�B	M�B	L�B	S�B	T�B	W
B	VB	\)B	^5B	`BB	bNB	e`B	gmB	hsB	gmB	hsB	k�B	m�B	n�B	o�B	p�B	p�B	r�B	r�B	r�B	r�B	s�B	u�B	v�B	v�B	y�B	{�B	z�B	{�B	|�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�+B	�+B	�1B	�+B	�=B	�DB	�JB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�3B	�3B	�3B	�?B	�RB	�XB	�XB	�^B	�dB	�jB	�jB	�qB	�wB	�wB	�}B	��B	B	ÖB	B	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�/B	�HB	�HB	�BB	�BB	�NB	�TB	�`B	�`B	�ZB	�ZB	�`B	�`B	�fB	�mB	�fB	�fB	�fB	�mB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
%B
hB
�B
'�B
(�B
/B
1'B
5?B
?}B
XB
R�B
XB
\)B
cTB
k�B
n�B
q�B
t�B
z�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<�t�<�`B<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<D��<49X<#�
<#�
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
<T��<49X<#�
<#�
<e`B<���<e`B<#�
<#�
<#�
<#�
<u<u<�j<�h<49X<#�
<#�
<#�
<�C�=t�=<j=+=�w=8Q�<�`B=�P=49X=aG�=e`B<D��<D��<#�
<e`B<���<�<�j<T��<#�
<#�
<u<ě�<ě�<49X<D��<u<u<49X<�o<�j<T��<�C�<���<�o<D��<�t�<#�
<#�
<T��<u<�t�<�t�<�o<49X<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<��
<�C�<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451562012011014515620120110145156  AO  ARGQ                                                                        20111130143603  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143603  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145156  IP                  G�O�G�O�G�O�                