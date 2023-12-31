CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:40Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143524  20190522121828  1728_5048_005                   2C  D   APEX                            2142                            040306                          846 @�7|V�1   @�7|�O��@2��;dZ�c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C9�fC;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D �fD!  D!� D"  D"� D#  D#�fD$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ�fDR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DyS3D���D�9�D���D��fD� D�I�D�l�D��fD��3D�#3D�s3Dǣ3D�  D�)�D�I�D��fD��fD��D�` D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s33@�ff@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB��B33B33B33B'33B/33B733B?33BG33BO��BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B�ffB���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C�fC��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/�3C1��C3��C5��C7��C9�3C;�3C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS�3CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs�3Cu�3Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC��fC��fC��fC�ٚC��fC��fC�ٚC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dy�D�3Ds3D�3Ds3D�3D y�D �3D!s3D!�3D"s3D"�3D#y�D#�3D$s3D$�3D%l�D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0y�D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4y�D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN��DOs3DO�3DPs3DP�3DQy�DQ�3DRs3DR�3DSl�DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3DyFfD��3D�33D��3D�� D�	�D�C3D�ffD�� D���D��D�l�Dǜ�D���D�#3D�C3D�� D�� D�3D�Y�D�Y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA���A��/A֣�A�z�A�ffA�Q�A��A��A�hsA�K�Aҙ�A�{A�S�A�\)A�ffA�K�A�bA�%A���A�5?A��HAāA��A���A�A��-A��wA�
=A��^A���A�O�A�XA��#A���A�I�A��hA��/A�9XA���A�/A���A��HA�?}A���A��A��A�-A�  A��HA�z�A�&�A��/A���A�Q�A�1A��jA�jA�$�A���A�l�A�7LA�(�A���A�A���A�1'A���A�  A�`BA���A���A���A��+A��7A�5?A�S�A�A�  A��A���A���A��A��wA���A�5?A�A��A�1'A�"�A��A�%A�G�A�oA��A�S�A��A�1A�$�A���A���A��A}�FAz�Au��ApAohsAoXAn�`AnbAmoAh�HAa��A^~�A[�PAZr�AY��AW�AW�AW7LAV��AT��AR�jAQ33APJAO\)AN��AN1AL�AF^5AB��A?ƨA=33A7��A5�A5K�A3p�A/�TA+�A*�A*�A*bNA)�A'ƨA&  A$ZA"$�A 5?A�#A�A�7AdZA33A�uA��A�AZA�AM�A��A�AVA�TAG�AȴA~�AS�A�A �Al�A��AQ�A|�A
��A	VAz�A~�A��Ar�A?}A��A�RA��A�A(�A��AA�+A��AC�A ff@�M�@�I�@�S�@�
=@�ȴ@��j@���@�dZ@�S�@�o@�5?@��`@��@�ƨ@�l�@��@���@�G�@��@�9@�w@���@�x�@�hs@�`B@���@�j@���@�o@�`B@�-@�P@�@ޏ\@���@ە�@�C�@��@��T@؛�@� �@�|�@Ձ@��@ԣ�@��@ӥ�@��y@�5?@�hs@�bN@ϕ�@�|�@�C�@�+@��y@Η�@�M�@�J@���@�X@̓u@�Q�@�A�@�1'@���@�|�@�33@�
=@�"�@�;d@�+@��@�hs@�O�@�/@���@ȼj@�z�@�ƨ@�;d@Ɨ�@�=q@�5?@���@�@�@Ɨ�@�$�@�X@�V@ă@�l�@°!@�=q@��@��T@�@���@��+@���@��@�  @��F@�K�@���@�hs@�I�@���@��
@��@�|�@�K�@�+@�
=@��y@���@�^5@�-@���@�p�@�G�@�%@��@���@���@��j@��9@���@��D@�  @��@���@�+@�;d@�C�@�\)@�|�@��P@�l�@�t�@���@�9X@���@��F@��@��@�9X@�1@��
@�|�@�K�@�@��H@��H@��-@�/@�9X@��F@��
@��
@�K�@�^5@��@�$�@�$�@��@�Z@��;@�t�@���@�?}@��@�j@��w@�|�@�
=@�^5@�{@���@���@��h@�?}@���@�Q�@��
@�33@��@��R@�^5@�J@���@�/@�%@���@��`@���@�j@���@���@�|�@�33@�ȴ@�ff@��@��7@�hs@�X@�?}@���@�  @�C�@��\@�-@�5?@�ff@�J@���@��h@��@���@�Ĝ@��9@�z�@�A�@��@�|�@�o@��@��!@�ff@�V@�$�@�@�x�@�O�@�/@���@���@��D@�z�@�r�@�Z@�1'@�1'@�1'@��@��;@�ƨ@��F@��@�ƨ@�1@�33@�ff@�5?@�=q@�M�@�E�@�=q@�$�@�{@�{@�@���@��@��@���@��-@�p�@�7L@�/@�/@�&�@��@��@���@��;@�  @��@��
@��R@�@��h@�p�@�?}@��`@��D@�A�@���@�C�@��@��!@�V@��@���@���@�X@�?}@�7L@�7L@��@�l�@|z�@sC�@i�#@a�@Y�@S33@LZ@D��@?\)@8�@0��@*�!@#��@�@"�@1'@��@bN@�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�9XA���A��/A֣�A�z�A�ffA�Q�A��A��A�hsA�K�Aҙ�A�{A�S�A�\)A�ffA�K�A�bA�%A���A�5?A��HAāA��A���A�A��-A��wA�
=A��^A���A�O�A�XA��#A���A�I�A��hA��/A�9XA���A�/A���A��HA�?}A���A��A��A�-A�  A��HA�z�A�&�A��/A���A�Q�A�1A��jA�jA�$�A���A�l�A�7LA�(�A���A�A���A�1'A���A�  A�`BA���A���A���A��+A��7A�5?A�S�A�A�  A��A���A���A��A��wA���A�5?A�A��A�1'A�"�A��A�%A�G�A�oA��A�S�A��A�1A�$�A���A���A��A}�FAz�Au��ApAohsAoXAn�`AnbAmoAh�HAa��A^~�A[�PAZr�AY��AW�AW�AW7LAV��AT��AR�jAQ33APJAO\)AN��AN1AL�AF^5AB��A?ƨA=33A7��A5�A5K�A3p�A/�TA+�A*�A*�A*bNA)�A'ƨA&  A$ZA"$�A 5?A�#A�A�7AdZA33A�uA��A�AZA�AM�A��A�AVA�TAG�AȴA~�AS�A�A �Al�A��AQ�A|�A
��A	VAz�A~�A��Ar�A?}A��A�RA��A�A(�A��AA�+A��AC�A ff@�M�@�I�@�S�@�
=@�ȴ@��j@���@�dZ@�S�@�o@�5?@��`@��@�ƨ@�l�@��@���@�G�@��@�9@�w@���@�x�@�hs@�`B@���@�j@���@�o@�`B@�-@�P@�@ޏ\@���@ە�@�C�@��@��T@؛�@� �@�|�@Ձ@��@ԣ�@��@ӥ�@��y@�5?@�hs@�bN@ϕ�@�|�@�C�@�+@��y@Η�@�M�@�J@���@�X@̓u@�Q�@�A�@�1'@���@�|�@�33@�
=@�"�@�;d@�+@��@�hs@�O�@�/@���@ȼj@�z�@�ƨ@�;d@Ɨ�@�=q@�5?@���@�@�@Ɨ�@�$�@�X@�V@ă@�l�@°!@�=q@��@��T@�@���@��+@���@��@�  @��F@�K�@���@�hs@�I�@���@��
@��@�|�@�K�@�+@�
=@��y@���@�^5@�-@���@�p�@�G�@�%@��@���@���@��j@��9@���@��D@�  @��@���@�+@�;d@�C�@�\)@�|�@��P@�l�@�t�@���@�9X@���@��F@��@��@�9X@�1@��
@�|�@�K�@�@��H@��H@��-@�/@�9X@��F@��
@��
@�K�@�^5@��@�$�@�$�@��@�Z@��;@�t�@���@�?}@��@�j@��w@�|�@�
=@�^5@�{@���@���@��h@�?}@���@�Q�@��
@�33@��@��R@�^5@�J@���@�/@�%@���@��`@���@�j@���@���@�|�@�33@�ȴ@�ff@��@��7@�hs@�X@�?}@���@�  @�C�@��\@�-@�5?@�ff@�J@���@��h@��@���@�Ĝ@��9@�z�@�A�@��@�|�@�o@��@��!@�ff@�V@�$�@�@�x�@�O�@�/@���@���@��D@�z�@�r�@�Z@�1'@�1'@�1'@��@��;@�ƨ@��F@��@�ƨ@�1@�33@�ff@�5?@�=q@�M�@�E�@�=q@�$�@�{@�{@�@���@��@��@���@��-@�p�@�7L@�/@�/@�&�@��@��@���@��;@�  @��@��
@��R@�@��h@�p�@�?}@��`@��D@�A�@���@�C�@��@��!@�V@��@���@���@�X@�?}@�7L@�7L@��@�l�@|z�@sC�@i�#@a�@Y�@S33@LZ@D��@?\)@8�@0��@*�!@#��@�@"�@1'@��@bN@�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B~�B� B~�B~�B~�B~�B}�B}�By�Bv�Bq�B�B�7B�B�B�B�JB��B�VB��B�B��B��BVB&�B.BN�B_;B_;Be`Bo�Bz�B�=B�PB�{B��B�RBÖBB��BŢB��B��B�#B�B��B��B�B��B��B��B��B��B��B�B�
B��B��B��B��B��B��B��B��BȴBŢB�B��B��B�dB�B��B��B�{B{�Bu�Bt�BiyBVBD�B)�B�B{BPB�5B��B�DBs�BK�B�B
�B
��B
�jB
�B
��B
��B
�oB
s�B
XB
D�B
9XB
oB	��B	��B	�FB	�9B	�3B	�-B	�'B	�3B	��B	t�B	_;B	L�B	D�B	A�B	8RB	6FB	5?B	2-B	'�B	�B	�B	hB	VB	DB	%B��B�B��BɺB�3B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�JB�=B�=B�7B�7B�%B�B�B�%B�B�B� B�B�B�B�%B�%B�B�B~�B}�Bz�Bz�B�B� B�B}�Bz�B{�B� B�PB�VB�JB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�9B�9B�-B�!B�B�!B�B�B�B�B�B�B�B�B�B�B�!B�-B�9B�9B�FB�?B�^B�dB�^B�qBBȴB��B��B�B�#B�/B�;B�BB�fB�`B�`B�`B�yB�B�B�yB�B��B��B��B��B��B��B��B	  B	B	B	1B		7B		7B	DB	JB	PB	bB	oB	�B	�B	�B	�B	�B	�B	�B	 �B	'�B	-B	,B	)�B	,B	1'B	<jB	@�B	B�B	E�B	K�B	M�B	M�B	L�B	N�B	M�B	K�B	K�B	K�B	M�B	M�B	K�B	L�B	M�B	K�B	L�B	N�B	N�B	O�B	N�B	O�B	P�B	Q�B	R�B	S�B	T�B	VB	W
B	W
B	XB	YB	[#B	\)B	\)B	^5B	^5B	_;B	_;B	`BB	`BB	aHB	bNB	e`B	gmB	jB	o�B	p�B	q�B	r�B	u�B	y�B	y�B	y�B	~�B	�+B	�1B	�+B	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�!B	�'B	�'B	�3B	�?B	�?B	�RB	�XB	�^B	�jB	�wB	��B	��B	ÖB	B	B	��B	��B	ÖB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
hB
�B
 �B
%�B
/B
5?B
:^B
D�B
G�B
L�B
Q�B
ZB
^5B
dZB
ffB
l�B
p�B
t�B
w�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B� B�B� B~�B~�B� B~�B� B|�B|�B{�B�B�PB�+B�B�B�VB��B��B��B�!BǮB��B{B'�B1'BQ�BdZBbNBhsBs�B|�B�=B�\B��B�B�}BƨBĜBĜBɺB��B�B�5B�B��B�B�
B�B�
B��B��B��B�B�B�B�
B��B��B��B��B��B��B��B��B��B�3B�B��B��B�'B��B��B��B� Bw�By�Bm�B\)BK�B-B�B�B�B�B�'B�oB�B]/B0!B
��B
�BB
B
�!B
��B
��B
��B
x�B
]/B
G�B
A�B
�B
B	�;B	�RB	�9B	�9B	�?B	�?B	��B	�dB	� B	iyB	P�B	H�B	G�B	:^B	7LB	7LB	9XB	/B	$�B	�B	uB	bB	PB	
=B	%B�B�B��B�}B�B�B��B��B�B��B��B��B��B�B��B��B��B��B�VB�PB�DB�DB�=B�JB�=B�1B�+B�DB�PB�7B�1B�+B�%B�%B�1B�1B�=B�7B�%B�B|�B|�B�B�B�B� Bz�B{�B�B�bB�bB�JB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�?B�FB�?B�-B�!B�'B�!B�'B�B�B�B�B�!B�B�B�B�'B�3B�?B�FB�^B�jB�}B�}B��B��BĜBɺB��B��B�B�)B�5B�NB�HB�mB�fB�fB�fB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	%B	1B		7B	
=B	JB	PB	PB	bB	oB	�B	�B	�B	�B	�B	 �B	�B	!�B	(�B	.B	-B	+B	,B	0!B	<jB	@�B	C�B	F�B	M�B	N�B	N�B	N�B	P�B	N�B	K�B	L�B	L�B	O�B	Q�B	M�B	N�B	N�B	L�B	M�B	O�B	P�B	Q�B	O�B	O�B	P�B	Q�B	R�B	S�B	T�B	VB	XB	XB	YB	ZB	\)B	\)B	]/B	^5B	^5B	_;B	_;B	`BB	`BB	aHB	cTB	gmB	gmB	jB	o�B	p�B	q�B	r�B	u�B	y�B	y�B	y�B	}�B	�1B	�7B	�1B	�=B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�-B	�9B	�FB	�FB	�RB	�XB	�^B	�qB	�}B	��B	B	ÖB	ÖB	ÖB	B	B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
hB
�B
 �B
&�B
/B
5?B
:^B
D�B
G�B
L�B
Q�B
ZB
_;B
dZB
gmB
m�B
p�B
t�B
w�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<��
<49X<#�
<e`B<�C�<�t�<D��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<T��<��
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
<D��<#�
<#�
<#�
<D��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451542012011014515420120110145154  AO  ARGQ                                                                        20111130143524  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143524  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145154  IP                  G�O�G�O�G�O�                