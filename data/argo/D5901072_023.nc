CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:45Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143720  20190522121828  1728_5048_023                   2C  D   APEX                            2142                            040306                          846 @�dX1   @�d�[�@5������c&5?|�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dy�3D��fD�)�D�ffD��fD��3D�L�D���D��fD���D��D��3Dǰ D��fD�I�D�ffD��D��3D�#3D�P 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s33@���@���A��A<��A\��A|��A�ffA���A�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?��BG33BN��BW33B_33Bg33Bo33Bw33B33B���B���B�ffB���B�ffB���B���B�ffB���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C�fC�fC��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ�fCS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$l�D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIy�DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dy�fD�� D�#3D�` D�� D���D�FfD��fD�� D��fD�fD���Dǩ�D�� D�C3D�` D�fD���D��D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�ZA��^A���A���A���A�ffA�$�A���A��!A�\)A��jA��A�bNA�K�A�5?A�"�A�
=A���A��TA��#A���A���A���A�ĜA��wA��FA��A���A���A���A��A�p�A�v�A��A�~�A�v�A�jA�\)A�K�A�9XA��A���A��;A��FA���A���A��hA�~�A�jA�Q�A�5?A��A��A�
=A���A��mA�ĜA���A���A��uA��RA�^5A�1A���A���A���A��A�1A�v�A�ȴA��yA�G�A�ĜA��A��A�?}A��A��A��DA�S�A�5?A��`A�VA�A���A��wA�VA��A�A�A�ȴA��TA��hA��A��A��PA��wA�/A��PA�M�A��A��A�7LA���A��`A��A���A{x�Ay%Au�At��As��Aqp�Ao�-Alr�Ak?}AiG�Ag�Ac��A_33A]l�AYƨAXz�AW`BAU�mAU/ATz�AS�AQXAPbAOhsAM��AL�\AK��AKXAJ��AIt�AH�uAGO�AFA�AEx�ADn�AC�FAC7LA@��A?C�A=`BA;K�A9;dA7dZA5
=A4�RA3A2jA0�A0��A0  A/�hA.�A.��A-�A-/A+��A*bNA)�
A)��A)oA(�DA'�TA'33A&�9A&5?A%��A$��A$A�A#��A#7LA!hsA v�A�mA�uA��A�A|�AE�Av�A33AVAt�A~�A�#A7LA"�A�/Ar�A�A7LAI�At�A;dA��A�!AbA�wAG�A
�jA	�A��A��An�AƨAG�A;dA1'AVA�A(�A��A��A@��@��!@�X@�
=@��@���@�j@�Q�@���@���@��9@��@�ff@���@��@�1@�R@�5?@���@�r�@�  @�F@�
=@���@�b@�+@�7@߶F@�-@ܴ9@���@ܼj@ܛ�@�j@��@ش9@ָR@�%@Ӯ@�x�@�@�C�@�p�@��@�bN@�bN@ǥ�@�-@Ĭ@�%@���@�(�@�J@ˮ@�A�@̬@��@�;d@���@�\)@�{@��h@���@Ý�@ǍP@�r�@�z�@�l�@�@�\)@ǍP@�dZ@�S�@�o@��@ƸR@�v�@�5?@���@�(�@Å@�@�V@�{@���@��h@��j@�1@��@��@��R@�^5@�v�@���@���@�^5@���@�p�@��@�9X@��
@��w@��@�+@���@�@��@���@�j@�(�@���@��P@�dZ@�o@�ȴ@�~�@�V@��@�@�O�@���@��@�  @���@�@�ȴ@���@��\@�n�@���@�X@�Ĝ@��D@�I�@���@���@��@�S�@�
=@��@���@���@�$�@�@��j@�9X@��
@�"�@��!@��`@�+@��+@�-@���@�G�@���@���@�b@��F@���@�9X@�A�@�A�@�A�@�I�@�I�@��;@��@��+@�n�@�v�@�~�@�v�@�E�@��^@�O�@�&�@��@��`@��9@��@�bN@�I�@�1@�dZ@��y@���@��R@���@��+@��T@�G�@�1@���@�=q@�^5@�V@�M�@�E�@��@�O�@�9X@�  @�1@�l�@�@���@�5?@���@�@�O�@��`@���@��@� �@��w@�C�@�@��R@�n�@�{@��#@��@�V@��@���@�Z@�  @��@�
=@��+@�-@��@���@���@���@���@��h@��@�?}@��@��@���@��@�Z@�9X@�  @��P@�33@���@���@�v�@�=q@��@�x�@�7L@��j@�r�@�I�@��m@���@�t�@�dZ@�C�@�@��H@���@�M�@���@��#@��-@���@�p�@��j@���@x�@n�y@f5?@^ȴ@V$�@P  @J�\@CdZ@<1@3�F@,�/@(�@#��@�P@��@�-@��@�T111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ZA��^A���A���A���A�ffA�$�A���A��!A�\)A��jA��A�bNA�K�A�5?A�"�A�
=A���A��TA��#A���A���A���A�ĜA��wA��FA��A���A���A���A��A�p�A�v�A��A�~�A�v�A�jA�\)A�K�A�9XA��A���A��;A��FA���A���A��hA�~�A�jA�Q�A�5?A��A��A�
=A���A��mA�ĜA���A���A��uA��RA�^5A�1A���A���A���A��A�1A�v�A�ȴA��yA�G�A�ĜA��A��A�?}A��A��A��DA�S�A�5?A��`A�VA�A���A��wA�VA��A�A�A�ȴA��TA��hA��A��A��PA��wA�/A��PA�M�A��A��A�7LA���A��`A��A���A{x�Ay%Au�At��As��Aqp�Ao�-Alr�Ak?}AiG�Ag�Ac��A_33A]l�AYƨAXz�AW`BAU�mAU/ATz�AS�AQXAPbAOhsAM��AL�\AK��AKXAJ��AIt�AH�uAGO�AFA�AEx�ADn�AC�FAC7LA@��A?C�A=`BA;K�A9;dA7dZA5
=A4�RA3A2jA0�A0��A0  A/�hA.�A.��A-�A-/A+��A*bNA)�
A)��A)oA(�DA'�TA'33A&�9A&5?A%��A$��A$A�A#��A#7LA!hsA v�A�mA�uA��A�A|�AE�Av�A33AVAt�A~�A�#A7LA"�A�/Ar�A�A7LAI�At�A;dA��A�!AbA�wAG�A
�jA	�A��A��An�AƨAG�A;dA1'AVA�A(�A��A��A@��@��!@�X@�
=@��@���@�j@�Q�@���@���@��9@��@�ff@���@��@�1@�R@�5?@���@�r�@�  @�F@�
=@���@�b@�+@�7@߶F@�-@ܴ9@���@ܼj@ܛ�@�j@��@ش9@ָR@�%@Ӯ@�x�@�@�C�@�p�@��@�bN@�bN@ǥ�@�-@Ĭ@�%@���@�(�@�J@ˮ@�A�@̬@��@�;d@���@�\)@�{@��h@���@Ý�@ǍP@�r�@�z�@�l�@�@�\)@ǍP@�dZ@�S�@�o@��@ƸR@�v�@�5?@���@�(�@Å@�@�V@�{@���@��h@��j@�1@��@��@��R@�^5@�v�@���@���@�^5@���@�p�@��@�9X@��
@��w@��@�+@���@�@��@���@�j@�(�@���@��P@�dZ@�o@�ȴ@�~�@�V@��@�@�O�@���@��@�  @���@�@�ȴ@���@��\@�n�@���@�X@�Ĝ@��D@�I�@���@���@��@�S�@�
=@��@���@���@�$�@�@��j@�9X@��
@�"�@��!@��`@�+@��+@�-@���@�G�@���@���@�b@��F@���@�9X@�A�@�A�@�A�@�I�@�I�@��;@��@��+@�n�@�v�@�~�@�v�@�E�@��^@�O�@�&�@��@��`@��9@��@�bN@�I�@�1@�dZ@��y@���@��R@���@��+@��T@�G�@�1@���@�=q@�^5@�V@�M�@�E�@��@�O�@�9X@�  @�1@�l�@�@���@�5?@���@�@�O�@��`@���@��@� �@��w@�C�@�@��R@�n�@�{@��#@��@�V@��@���@�Z@�  @��@�
=@��+@�-@��@���@���@���@���@��h@��@�?}@��@��@���@��@�Z@�9X@�  @��P@�33@���@���@�v�@�=q@��@�x�@�7L@��j@�r�@�I�@��m@���@�t�@�dZ@�C�@�@��H@���@�M�@���@��#@��-@���@�p�@��j@���@x�@n�y@f5?@^ȴ@V$�@P  @J�\@CdZ@<1@3�F@,�/@(�@#��@�P@��@�-@��@�T111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB_;Bk�B�JB�{B��B��B�?B�
B�B�B:^Bs�B�B�=B�bB��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�B�!B�!B�-B�'B�3B�?B�FB�FB�FB�LB�LB�RB�LB�RB�RB�XB�RB�XB�XB�XB�?B�XB�dB�^B�FB�3B�3B��B��B��B�JB�=B� B�B|�Bt�Bq�BdZB[#BR�BG�BD�B@�B'�BB�TB��B�B�bB~�Bt�Bo�B^5BM�B.B"�B�B1B
�B
�NB
�B
�wB
�B
��B
�uB
�{B
dZB
VB
1'B
#�B
bB	��B	��B	�B	�)B	�;B	B	�}B	��B	�B	cTB	S�B	=qB	49B	.B	%�B	�B	uB	DB��B��B�B�mB�BB�#B�B�B��BƨBÖB�qB�jB�LB�FB�9B�9B��B��B��B�oB��B�B��B�oB��B��B�{B��B��B��B��B��B��B��B��B�\B�VB�oB�\B�hB�bB�oB��B��B�uB�oB�bB�\B�PB�PB�DB�DB�DB�bB�B}�Bz�B{�Bv�Bv�Bs�Bs�Bv�Bw�Bw�Bu�Bu�B{�Bz�B|�B� B�B� B|�B�B�B�DB�\B�1B�B�B�B�B�B�B�B�+B�1B�1B��B�Bz�Bu�Bu�Bq�Bs�Bv�By�By�B|�B� B� B�B�B�B�=B�PB�7B�7B�7B�1B�1B�1B�Bx�Bu�Br�Bp�Bm�Bk�BiyBl�Bl�Bl�Bl�Bn�Bn�Bm�Bl�BgmB`BBXBO�BM�BS�BO�BYB[#B[#B\)BhsB�B�bB��B�3B�jB��BŢBĜB��BÖB�B�B�3BĜB�B��B	B	bB	�B	!�B	(�B	+B	2-B	6FB	7LB	8RB	9XB	9XB	:^B	:^B	:^B	>wB	B�B	E�B	J�B	L�B	K�B	K�B	N�B	P�B	Q�B	S�B	XB	ZB	\)B	^5B	bNB	gmB	k�B	l�B	m�B	m�B	n�B	p�B	r�B	w�B	y�B	z�B	}�B	~�B	�B	�B	�B	�%B	�7B	�DB	�JB	�VB	�VB	�hB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�'B	�'B	�-B	�3B	�3B	�dB	�qB	��B	B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	�B	��B	��B	�B	�#B	�BB	�HB	�NB	�NB	�TB	�TB	�ZB	�TB	�ZB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
B	��B
B
  B	��B
  B
  B	��B	��B	��B
B
B
B
B
B
%B
+B
%B
1B
PB
oB
�B
�B
&�B
.B
6FB
=qB
A�B
F�B
M�B
XB
ZB
`BB
e`B
jB
m�B
s�B
w�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B`BBm�B�JB�{B��B��B�FB�B�B�B=qBt�B�B�DB�hB��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�B�'B�'B�3B�-B�9B�FB�FB�FB�FB�LB�LB�XB�LB�RB�RB�XB�RB�^B�^B�jBÖB�qB�qB�jB�XB�RB�XB�FB��B��B�hB�bB�B�B�Bx�Bx�BjB`BBT�BH�BE�BE�B2-B	7B�yB�)B�?B��B�Bw�Bt�Be`BW
B33B$�B �BbB
�B
�yB
�NB
ÖB
�3B
��B
��B
��B
iyB
`BB
6FB
)�B
uB	��B
B	�B	�ZB	�NB	ǮB	ŢB	��B	�bB	iyB	_;B	A�B	8RB	33B	(�B	�B	�B	oB	B��B��B�B�TB�/B�)B�B��B��BǮB��B��B�^B�RB�jB�^B�B��B�B��B��B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�bB�{B�hB�uB�oB�{B��B��B��B�{B�oB�{B�bB�\B�\B�hB�bB��B�+B�B~�B~�By�By�Bu�Bu�Bw�Bx�By�Bw�Bx�B~�B}�B}�B�B�B�B}�B�B�+B�PB�oB�7B�B�+B�B�B�%B�1B�+B�1B�=B�=B��B�JB{�Bw�By�Bt�Bt�Bw�By�Bz�B�B�B�B�B�B�%B�JB�\B�=B�=B�DB�7B�7B�=B�DB}�Bw�Bu�Bt�Bp�Bn�BiyBl�Bm�Bm�Bo�Br�Br�Bp�Bo�Bk�Be`B^5BR�BL�BW
BO�B[#B^5B^5B[#Be`B�B�PB��B�-B�jB��BŢBŢB��B��B�B�B�'B�}B�B��B	B	hB	�B	!�B	(�B	+B	33B	6FB	8RB	9XB	:^B	;dB	;dB	;dB	<jB	?}B	C�B	F�B	K�B	N�B	L�B	M�B	O�B	P�B	Q�B	S�B	XB	ZB	]/B	_;B	cTB	iyB	l�B	m�B	m�B	n�B	o�B	q�B	t�B	x�B	z�B	z�B	~�B	� B	�B	�B	�B	�+B	�=B	�DB	�JB	�\B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�'B	�'B	�-B	�3B	�9B	�qB	�wB	��B	B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	�B	�
B	��B	�B	�)B	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�TB	�`B	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B	��B
B
  B
  B
B
  B	��B	��B
  B
B
B
B
B
B
%B
+B
%B
1B
PB
oB
�B
�B
&�B
.B
6FB
=qB
A�B
F�B
M�B
XB
ZB
`BB
e`B
jB
m�B
s�B
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
<e`B<#�
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
<D��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452002012011014520020120110145200  AO  ARGQ                                                                        20111130143720  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143720  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145200  IP                  G�O�G�O�G�O�                