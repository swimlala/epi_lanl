CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:03Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190603  20181005190603  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @������1   @���8��@1ܬ1&��c�I�^1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B��B��B��B ffB(  B/��B7��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(�C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCG�fCI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3D � D  D� D  D� D  D� D  D�fDfD� D  Dy�D  D� D  D�fD	  D	� D
  D
� D  D� D��D� D  D� D  D�fD  D� D  Dy�D  D�fD  Dy�D  D� D  D� D��D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  Dy�D  Dy�D  D� D   D y�D!  D!�fD"fD"y�D#  D#�fD$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,�fD-  D-y�D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7y�D7��D8y�D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=�fD>  D>�fD?  D?y�D@  D@� DA  DA� DB  DB� DB��DC� DD  DDy�DE  DE�fDF  DF� DG  DG� DH  DHy�DH��DI� DJfDJ� DK  DK� DL  DL� DL��DMy�DM��DN� DOfDO� DP  DP�fDQfDQ� DQ��DR� DS  DSy�DT  DT� DUfDU�fDVfDV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^fD^� D_  D_� D`  D`�fDa  Day�Db  Db� DcfDc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Ds��Dt� Du  Du� Dv  Dv� DwfDw�fDwٚDy��D�+�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@ə�A��A$��AD��Ad��A�ffA�33A�ffA�ffA�ffA�ffA�ffA�ffB33B��B��B��B!��B)33B0��B8��B@��BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�fgBę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CfgCL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&fgC(fgC*33C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CF33CH33CJ33CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�33C�33C�&fC�33C�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C��C�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC��C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�33C�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��D �D �3D3D�3D3D�3D3D�3D3D��D�D�3D3D��D3D�3D3D��D	3D	�3D
3D
�3D3D�3D�D�3D3D�3D3D��D3D�3D3D��D3D��D3D��D3D�3D3D�3D�D�3D3D�3D3D��D�D�3D3D�3D3D�3D3D�3D3D�3D3D��D3D��D3D�3D 3D ��D!3D!��D"�D"��D#3D#��D$3D$�3D%3D%�3D&�D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+��D,3D,��D-3D-��D.3D.�3D/3D/�3D03D0�3D1�D1�3D23D2�3D33D3�3D43D4�3D53D5��D63D6�3D73D7��D8�D8��D93D9�3D:3D:�3D;3D;�3D<3D<��D=3D=��D>3D>��D?3D?��D@3D@�3DA3DA�3DB3DB�3DC�DC�3DD3DD��DE3DE��DF3DF�3DG3DG�3DH3DH��DI�DI�3DJ�DJ�3DK3DK�3DL3DL�3DM�DM��DN�DN�3DO�DO�3DP3DP��DQ�DQ�3DR�DR�3DS3DS��DT3DT�3DU�DU��DV�DV�3DW3DW�3DX3DX��DY3DY�3DZ3DZ�3D[3D[�3D\�D\�3D]3D]�3D^�D^�3D_3D_�3D`3D`��Da3Da��Db3Db�3Dc�Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di��Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm��Dn3Dn�3Do3Do��Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds��Dt�Dt�3Du3Du�3Dv3Dv�3Dw�Dw��Dw��Dy�)D�5qD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA�9XA�9XA�9XA�=qA�A�A�C�A�E�A�E�A�G�A�I�A�I�A�I�A�K�A�M�A�O�A�O�A�M�A�K�A�K�A�K�A�K�A�M�A�O�A�O�A�O�A�O�A�O�A�M�A��A�  A̧�A˲-A���Aɥ�A�oAżjA�O�A�"�A���A���A�ƨA���A��;A�(�A� �A���A���A�Q�A��A���A� �A�1A�9XA���A�A���A��A��A���A�{A�1'A�A�A���A�JA�v�A���A�A�A���A��mA��A�t�A�(�A�ȴA��A���A��uA�33A�t�A���A���A��+A��jA��uA���A��A�{A�ffA�l�A���A�I�A��-A�^5A���A��HA�`BA��
A�ƨA�#A|�9Az��Ay�TAvjAqO�Aj�/Ad(�AY��AX�!AX�AU%AQ/ANJAL�+AKS�AJbNAI��AI
=AHr�AE�mAB(�A?
=A>  A<�jA;A7+A6E�A4��A3�A2�DA1O�A0E�A.~�A,��A*-A'A&�9A&bNA$M�A!�-A �DAXAdZA�
A�A�A��A�jA��AĜA�+A�yA�A�jAr�A9XA��A;dA��AffAffA�-AK�A�#A�jA$�A;dA��AhsA
��A
�A
ȴA
��A
�`A
��A	l�A�uA	�A
��A
�/A
�\A	��A	
=A	�A��AS�AVA��AG�A��A�A �H@�o@�x�@�G�@���@��9@��j@�  @���@�E�@��@���@�r�@���@�-@�9X@�@�\)@��@��@�  @�"�@���@�-@�-@��@��@��`@��@�(�@� �@蛦@��`@�@�M�@�o@�X@�ff@@�~�@�n�@�=q@�@�b@�@��@�~�@�x�@�@��H@���@�!@�~�@�V@�-@��@�-@��@��;@�S�@��@�v�@�h@���@�A�@�l�@���@��T@�%@�Ĝ@�bN@��@�|�@�M�@��@ٙ�@�hs@�?}@��@ؼj@�z�@�b@׍P@�"�@և+@�=q@պ^@�hs@��@��/@�Z@��m@ӕ�@�"�@�33@�l�@��
@�ƨ@Ӯ@�C�@�o@ҏ\@��@��@�9X@��@���@���@���@��#@���@���@˶F@�;d@�V@�J@��T@�hs@�?}@�%@�Ĝ@�Z@� �@� �@�  @�ƨ@ǅ@�S�@��@�-@���@��#@ř�@�X@���@Ĭ@�I�@�9X@�(�@�1@��@��;@���@Ý�@�\)@�C�@�"�@��H@\@�M�@��@���@�p�@�/@��`@�Z@��;@��@��@��@��@�hs@��/@�  @���@��+@��-@��/@��@���@��F@� �@� �@�1@�t�@���@�ff@�-@�J@���@��7@�x�@���@�j@�9X@��w@�K�@���@�^5@��@��@�X@�V@�V@��/@�r�@�(�@���@�S�@��^@��h@�X@�V@���@�9X@��F@�t�@��@���@��H@�E�@�%@�bN@��m@���@�dZ@�K�@�@���@��\@��@��@���@�hs@�/@��9@�j@�Q�@�I�@��;@��P@�l�@�;d@�"�@��@�ȴ@�v�@�E�@���@�7L@�V@�Ĝ@�j@�1@��P@�K�@�"�@��H@���@���@�~�@�n�@�^5@�@���@�p�@�7L@���@���@���@��`@��@�I�@��
@�|�@�dZ@�C�@��@���@�E�@�-@�{@�@���@�O�@���@�Ĝ@��@�j@��@���@�C�@�+@�o@�@��@��y@��\@�{@���@��@��T@���@��h@�O�@�?}@�/@�V@��/@�A�@�|�@�\)@�;d@�@�n�@���@��@w�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA�9XA�9XA�9XA�=qA�A�A�C�A�E�A�E�A�G�A�I�A�I�A�I�A�K�A�M�A�O�A�O�A�M�A�K�A�K�A�K�A�K�A�M�A�O�A�O�A�O�A�O�A�O�A�M�A��A�  A̧�A˲-A���Aɥ�A�oAżjA�O�A�"�A���A���A�ƨA���A��;A�(�A� �A���A���A�Q�A��A���A� �A�1A�9XA���A�A���A��A��A���A�{A�1'A�A�A���A�JA�v�A���A�A�A���A��mA��A�t�A�(�A�ȴA��A���A��uA�33A�t�A���A���A��+A��jA��uA���A��A�{A�ffA�l�A���A�I�A��-A�^5A���A��HA�`BA��
A�ƨA�#A|�9Az��Ay�TAvjAqO�Aj�/Ad(�AY��AX�!AX�AU%AQ/ANJAL�+AKS�AJbNAI��AI
=AHr�AE�mAB(�A?
=A>  A<�jA;A7+A6E�A4��A3�A2�DA1O�A0E�A.~�A,��A*-A'A&�9A&bNA$M�A!�-A �DAXAdZA�
A�A�A��A�jA��AĜA�+A�yA�A�jAr�A9XA��A;dA��AffAffA�-AK�A�#A�jA$�A;dA��AhsA
��A
�A
ȴA
��A
�`A
��A	l�A�uA	�A
��A
�/A
�\A	��A	
=A	�A��AS�AVA��AG�A��A�A �H@�o@�x�@�G�@���@��9@��j@�  @���@�E�@��@���@�r�@���@�-@�9X@�@�\)@��@��@�  @�"�@���@�-@�-@��@��@��`@��@�(�@� �@蛦@��`@�@�M�@�o@�X@�ff@@�~�@�n�@�=q@�@�b@�@��@�~�@�x�@�@��H@���@�!@�~�@�V@�-@��@�-@��@��;@�S�@��@�v�@�h@���@�A�@�l�@���@��T@�%@�Ĝ@�bN@��@�|�@�M�@��@ٙ�@�hs@�?}@��@ؼj@�z�@�b@׍P@�"�@և+@�=q@պ^@�hs@��@��/@�Z@��m@ӕ�@�"�@�33@�l�@��
@�ƨ@Ӯ@�C�@�o@ҏ\@��@��@�9X@��@���@���@���@��#@���@���@˶F@�;d@�V@�J@��T@�hs@�?}@�%@�Ĝ@�Z@� �@� �@�  @�ƨ@ǅ@�S�@��@�-@���@��#@ř�@�X@���@Ĭ@�I�@�9X@�(�@�1@��@��;@���@Ý�@�\)@�C�@�"�@��H@\@�M�@��@���@�p�@�/@��`@�Z@��;@��@��@��@��@�hs@��/@�  @���@��+@��-@��/@��@���@��F@� �@� �@�1@�t�@���@�ff@�-@�J@���@��7@�x�@���@�j@�9X@��w@�K�@���@�^5@��@��@�X@�V@�V@��/@�r�@�(�@���@�S�@��^@��h@�X@�V@���@�9X@��F@�t�@��@���@��H@�E�@�%@�bN@��m@���@�dZ@�K�@�@���@��\@��@��@���@�hs@�/@��9@�j@�Q�@�I�@��;@��P@�l�@�;d@�"�@��@�ȴ@�v�@�E�@���@�7L@�V@�Ĝ@�j@�1@��P@�K�@�"�@��H@���@���@�~�@�n�@�^5@�@���@�p�@�7L@���@���@���@��`@��@�I�@��
@�|�@�dZ@�C�@��@���@�E�@�-@�{@�@���@�O�@���@�Ĝ@��@�j@��@���@�C�@�+@�o@�@��@��y@��\@�{@���@��@��T@���@��h@�O�@�?}@�/@�V@��/@�A�@�|�@�\)@�;d@�@�n�@���@��@w�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BVBT�BT�BT�BZBm�B�%B��B��BŢB��B	$�B	0!B	G�B	N�B	N�B	N�B	T�B	aHB	�qB
6FB
��B
�B+BgmB�\B��B�!BBǮB��B�B�B�
B�
B�B�B�;B��BǮB��BĜB��B�B��BÖB�qB�XB��B�PB�Bt�BC�B�B
�B
��B
ɺB
�wB
��B
y�B
e`B
Q�B
1'B
�B
hB
PB
B	��B	�B	�B	�`B	�TB	�/B	��B	�^B	�B	��B	�oB	o�B	E�B	 �B��B��B��B�B��B	  B	DB	�B	�B	�B	�B	�B	�B	B�B�mB�5B��B�qB�dB�RB�FB�?B�?B�LB�qBB��B�jB�qB�qB�^B�3B�!B�B�B�-B�LB�LB�LB�FB�XB�wBɺB�sB	
=B	�B	�B	�B	!�B	 �B	!�B	)�B	5?B	.B	�B��B�B�B�sB�NB�NB�mB��B	B	%B	1B		7B	B��B	�B	5?B	9XB	7LB	2-B	0!B	1'B	1'B	)�B	!�B	�B	#�B	�B	DB	B��B�B�B�B��B��B��B	  B	B	B	B	  B	B	B	B	+B	+B	
=B	DB	VB	hB	{B	�B	�B	"�B	(�B	/B	2-B	7LB	<jB	C�B	G�B	P�B	VB	_;B	y�B	�%B	�=B	�JB	�\B	�hB	�hB	�VB	�PB	�PB	�PB	�PB	�bB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�FB	�FB	�RB	�wB	B	ǮB	ȴB	ȴB	ɺB	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�NB	�HB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�NB	�BB	�5B	�5B	�5B	�BB	�;B	�BB	�`B	�fB	�mB	�sB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
  B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B
DB
JB
VB
VB
\B
bB
bB
bB
bB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
{B
uB
hB
oB
oB
oB
�B
jB
�B
'222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BVBT�BT�BT�BZBm�B�%B��B��BŢB��B	$�B	0!B	G�B	N�B	N�B	N�B	T�B	aHB	�qB
6FB
��B
�B+BgmB�\B��B�!BBǮB��B�B�B�
B�
B�B�B�;B��BǮB��BĜB��B�B��BÖB�qB�XB��B�PB�Bt�BC�B�B
�B
��B
ɺB
�wB
��B
y�B
e`B
Q�B
1'B
�B
hB
PB
B	��B	�B	�B	�`B	�TB	�/B	��B	�^B	�B	��B	�oB	o�B	E�B	 �B��B��B��B�B��B	  B	DB	�B	�B	�B	�B	�B	�B	B�B�mB�5B��B�qB�dB�RB�FB�?B�?B�LB�qBB��B�jB�qB�qB�^B�3B�!B�B�B�-B�LB�LB�LB�FB�XB�wBɺB�sB	
=B	�B	�B	�B	!�B	 �B	!�B	)�B	5?B	.B	�B��B�B�B�sB�NB�NB�mB��B	B	%B	1B		7B	B��B	�B	5?B	9XB	7LB	2-B	0!B	1'B	1'B	)�B	!�B	�B	#�B	�B	DB	B��B�B�B�B��B��B��B	  B	B	B	B	  B	B	B	B	+B	+B	
=B	DB	VB	hB	{B	�B	�B	"�B	(�B	/B	2-B	7LB	<jB	C�B	G�B	P�B	VB	_;B	y�B	�%B	�=B	�JB	�\B	�hB	�hB	�VB	�PB	�PB	�PB	�PB	�bB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�FB	�FB	�RB	�wB	B	ǮB	ȴB	ȴB	ɺB	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�NB	�HB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�NB	�BB	�5B	�5B	�5B	�BB	�;B	�BB	�`B	�fB	�mB	�sB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
  B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B
DB
JB
VB
VB
\B
bB
bB
bB
bB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
{B
uB
hB
oB
oB
oB
�B
jB
�B
'222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190603                              AO  ARCAADJP                                                                    20181005190603    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190603  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190603  QCF$                G�O�G�O�G�O�8000            