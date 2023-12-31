CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:35Z creation      
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
resolution        =���   axis      Z        d  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ST   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  m8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  vx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190535  20181005190535  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�����1   @���>��&@0Ƨ-�c��;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  BpffBxffB�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  C   C�fC  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��3C��3C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� DfD� D  D� D  D� D  D�fDfD� D��Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D#��D$� D%  D%� D&  D&� D'fD'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D7��D8y�D9  D9�fD:fD:� D:��D;� D<  D<� D=  D=� D>  D>� D?fD?�fD@fD@� D@��DAy�DA��DB� DCfDC� DC��DD� DEfDE� DF  DF� DG  DG� DH  DH� DH��DU  DU� DU��DVy�DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[y�D[��D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc�fDd  Ddy�Dd��Dey�Df  Df�fDg  Dg� Dh  Dh� Di  Di�fDjfDj�fDk  Dk� Dl  Dly�Dl��Dm� DnfDn� Dn��Do� DpfDp� Dq  Dq� Dq��Dry�Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dws3Dyi�D�;�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @ə�A��A$��AD��Ad��A�ffA�ffA�ffA�33A�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33B`��Bi33Bq��By��B���B���B���B���B���B�fgB���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B�fgB虚B왚B�B���B���B���C L�C33CL�CL�CfgC
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*fgC,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChfgCjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC��C��C��C��C�&fC�&fC�&fC�33C�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC��C�&fC�&fC�&fC��C��C��C��C�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC��C�&fC�&fC��C��C�&fC�33C�&fC�&fC�&fC�&fD 3D �3D3D�3D3D�3D�D��D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D�D�3D3D�3D3D�3D3D��D�D�3D3D�3D3D�3D3D�3D�D�3D3D�3D3D�3D3D��D�D�3D�D��D3D��D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#��D$�D$�3D%3D%�3D&3D&�3D'�D'��D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.��D/3D/�3D0�D0�3D13D1�3D23D2�3D33D3�3D43D4��D53D5�3D63D6�3D73D7�3D8�D8��D93D9��D:�D:�3D;�D;�3D<3D<�3D=3D=�3D>3D>�3D?�D?��D@�D@�3DA�DA��DB�DB�3DC�DC�3DD�DD�3DE�DE�3DF3DF�3DG3DG�3DH3DH�3DI�DU3DU�3DV�DV��DW3DW�3DX3DX�3DY�DY�3DZ3DZ�3D[3D[��D\�D\��D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da��Db3Db�3Dc3Dc��Dd3Dd��De�De��Df3Df��Dg3Dg�3Dh3Dh�3Di3Di��Dj�Dj��Dk3Dk�3Dl3Dl��Dm�Dm�3Dn�Dn�3Do�Do�3Dp�Dp�3Dq3Dq�3Dr�Dr��Ds3Ds�3Dt3Dt��Du3Du�3Dv3Dv�3Dw3Dw�fDy|�D�Eq11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�%A�JA�{A��A��A�{A�oA�oA��A��A��A��A��A��A��A� �A�"�A�$�A�+A�-A�-A�/A�/A�1'A�33A�33A�33A�33A�5?A�5?A�5?A�7LA�5?A�5?A�7LA�7LA�9XA�=qA�?}A�A�A�K�A�ZA�jA�n�A҅Aқ�AҲ-A�;dA���A�/A�33A�z�AΉ7A���Aŕ�Að!A�A��7A���A�hsA��#A���A��HA�K�A�ȴA��+A�5?A�v�A���A���A�I�A�A�r�A��9A���A�A�C�A�|�A�-A��A��A��9A�VA�K�A�=qA�  A��A�p�A��+A��/A�33A���A���A��7A��DA��A�oA�ffA��/A���A�O�A�Q�A�`BA�Q�A�  A��^A�+A�ZA��A�Q�A��A�E�A|��A|�A|M�Az��Aw�mAv$�Ar��An��Aj��AjAg;dAeC�Aa��A]�#AZ��AU�AR�APQ�AK�AG��AD(�AA��A@bA>��A=�A=�A;��A9�A7|�A5C�A4M�A333A0ȴA/��A.�HA-��A,A�A+oA)�^A&��A$5?A!�A ��A�
A�#A1'AdZA�#A1'A�^A�!A�A�A�9A�A��AA�A`BAZA��Ax�A��A^5A��A\)A&�A�!A�A��A/A
�A	dZA	�AbNA|�A��AI�AƨA��AZA9XA9XA-A�AA�7AG�A
=AĜAjAVA��A �A I�A �@�G�@���@��@�/@��
@�9X@���@�Z@�(�@�bN@��@�@���@���@�@��@�`B@�j@��@�@�J@�/@�1'@�dZ@�C�@��H@�=q@ّh@��@��@�X@��/@ׅ@�dZ@�o@և+@թ�@�7L@�A�@���@���@�p�@�O�@���@ύP@�~�@��T@���@��@�bN@�ƨ@��y@�$�@�@�hs@�O�@��`@���@ȣ�@� �@�\)@��y@�n�@Ƈ+@�V@��@š�@�G�@��@���@ļj@�r�@�ƨ@�33@�o@�ȴ@�E�@��#@��@�  @��m@�;d@���@��^@��7@�O�@�V@��@���@�;d@�~�@�=q@�=q@��T@��@�ƨ@���@�l�@�+@��@��!@�ff@�{@���@��h@�G�@��j@�z�@�j@�(�@��m@��@�t�@�@�~�@�E�@�-@��@���@�`B@�?}@��@�I�@���@��H@��R@���@�V@�$�@�X@��j@�Z@��@�  @��P@�;d@���@���@�$�@���@��@�bN@�(�@�1@�ƨ@�t�@�K�@���@��!@�-@��#@��@�&�@�1'@��
@��P@�dZ@�K�@�"�@���@��\@��+@�{@���@�G�@�7L@�V@��`@��9@�1'@���@�|�@�S�@��@�=q@�J@��T@���@�X@��@��/@��j@�r�@��;@�;d@�;d@���@���@�$�@���@���@�`B@��@��D@�Z@��D@�I�@�1@�1@��m@�dZ@�;d@��R@�E�@�=q@�{@�@��T@��^@�O�@���@�z�@��
@���@���@���@��P@�|�@��@�ȴ@�v�@�-@�J@��@��@���@��@�`B@���@�`B@�/@��@���@��/@���@�A�@� �@�  @��m@��
@��w@���@�S�@���@�ȴ@���@�$�@��T@���@�`B@�7L@�&�@���@���@��u@�r�@�Q�@�9X@�1'@�1'@�1'@�(�@��m@���@yk�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�%A�JA�{A��A��A�{A�oA�oA��A��A��A��A��A��A��A� �A�"�A�$�A�+A�-A�-A�/A�/A�1'A�33A�33A�33A�33A�5?A�5?A�5?A�7LA�5?A�5?A�7LA�7LA�9XA�=qA�?}A�A�A�K�A�ZA�jA�n�A҅Aқ�AҲ-A�;dA���A�/A�33A�z�AΉ7A���Aŕ�Að!A�A��7A���A�hsA��#A���A��HA�K�A�ȴA��+A�5?A�v�A���A���A�I�A�A�r�A��9A���A�A�C�A�|�A�-A��A��A��9A�VA�K�A�=qA�  A��A�p�A��+A��/A�33A���A���A��7A��DA��A�oA�ffA��/A���A�O�A�Q�A�`BA�Q�A�  A��^A�+A�ZA��A�Q�A��A�E�A|��A|�A|M�Az��Aw�mAv$�Ar��An��Aj��AjAg;dAeC�Aa��A]�#AZ��AU�AR�APQ�AK�AG��AD(�AA��A@bA>��A=�A=�A;��A9�A7|�A5C�A4M�A333A0ȴA/��A.�HA-��A,A�A+oA)�^A&��A$5?A!�A ��A�
A�#A1'AdZA�#A1'A�^A�!A�A�A�9A�A��AA�A`BAZA��Ax�A��A^5A��A\)A&�A�!A�A��A/A
�A	dZA	�AbNA|�A��AI�AƨA��AZA9XA9XA-A�AA�7AG�A
=AĜAjAVA��A �A I�A �@�G�@���@��@�/@��
@�9X@���@�Z@�(�@�bN@��@�@���@���@�@��@�`B@�j@��@�@�J@�/@�1'@�dZ@�C�@��H@�=q@ّh@��@��@�X@��/@ׅ@�dZ@�o@և+@թ�@�7L@�A�@���@���@�p�@�O�@���@ύP@�~�@��T@���@��@�bN@�ƨ@��y@�$�@�@�hs@�O�@��`@���@ȣ�@� �@�\)@��y@�n�@Ƈ+@�V@��@š�@�G�@��@���@ļj@�r�@�ƨ@�33@�o@�ȴ@�E�@��#@��@�  @��m@�;d@���@��^@��7@�O�@�V@��@���@�;d@�~�@�=q@�=q@��T@��@�ƨ@���@�l�@�+@��@��!@�ff@�{@���@��h@�G�@��j@�z�@�j@�(�@��m@��@�t�@�@�~�@�E�@�-@��@���@�`B@�?}@��@�I�@���@��H@��R@���@�V@�$�@�X@��j@�Z@��@�  @��P@�;d@���@���@�$�@���@��@�bN@�(�@�1@�ƨ@�t�@�K�@���@��!@�-@��#@��@�&�@�1'@��
@��P@�dZ@�K�@�"�@���@��\@��+@�{@���@�G�@�7L@�V@��`@��9@�1'@���@�|�@�S�@��@�=q@�J@��T@���@�X@��@��/@��j@�r�@��;@�;d@�;d@���@���@�$�@���@���@�`B@��@��D@�Z@��D@�I�@�1@�1@��m@�dZ@�;d@��R@�E�@�=q@�{@�@��T@��^@�O�@���@�z�@��
@���@���@���@��P@�|�@��@�ȴ@�v�@�-@�J@��@��@���@��@�`B@���@�`B@�/@��@���@��/@���@�A�@� �@�  @��m@��
@��w@���@�S�@���@�ȴ@���@�$�@��T@���@�`B@�7L@�&�@���@���@��u@�r�@�Q�@�9X@�1'@�1'@�1'@�(�@��m@���@yk�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ƨB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ȴB
ǮB
ȴB
��B
��B
��B
�
B
�HB
�yB
�BB�BP�B�B� Bx�Bp�Br�Bm�Bp�BffBl�B[#B�LB�B�B��B�B�B�ZB�B1B�B7LBD�BZBe`BiyBl�BhsB\)BF�B=qB33B)�B�B��B�B�B�B�B�ZB��B�}B�B��B�JBz�BhsBVBD�B49B+B�B
��B
�;B
��B
�XB
�VB
r�B
`BB
A�B
�B
B	��B	�B	�5B	��B	�3B	�!B	�B	��B	�JB	z�B	aHB	L�B	<jB	5?B	%�B	�B	B�B�BƨB�FB�B��B�\B�1B�%B�B�B�B� B�B�B�DB�PB�\B�oB��B��B�B�'B�FB�^B�qBBÖBÖB�}BÖB�wB�FB�3B�B�B�!B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�9B�RB�dB�^B�LB�3B�FB��B��B��B��B�/B�/B�#B�#B�`B�mB�B��B�B��B��B��B��B	B��B��B	B	B��B�mB�B�B�B�B�NB�BB�TB�#B��B��B��B��B��B��B�B�B�#B�/B�)B�#B�BB�`B�fB�B��B��B��B	B	%B	1B	VB	�B	�B	�B	�B	�B	�B	 �B	'�B	,B	,B	,B	1'B	5?B	8RB	;dB	;dB	<jB	<jB	=qB	>wB	D�B	G�B	H�B	K�B	M�B	O�B	P�B	R�B	S�B	VB	VB	W
B	YB	ZB	ZB	]/B	bNB	ffB	gmB	hsB	gmB	hsB	gmB	gmB	gmB	gmB	hsB	jB	n�B	r�B	s�B	u�B	w�B	x�B	w�B	t�B	t�B	t�B	v�B	w�B	x�B	z�B	{�B	}�B	~�B	~�B	�B	�B	�%B	�+B	�1B	�=B	�DB	�PB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�?B	�?B	�?B	�LB	�RB	�RB	�XB	�XB	�dB	�dB	�dB	�qB	��B	B	ÖB	ƨB	ǮB	ȴB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�;B	�BB	�HAUG�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B

=B

=B
	7B
	7B
	7B
1B
1B
1B
+B
1B
DB
PB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
hB
bB
hB
hB
hB
hB
hB
hB
hB
oB
hB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
%�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222   B
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ƨB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ȴB
ǮB
ȴB
��B
��B
��B
�
B
�HB
�yB
�BB�BP�B�B� Bx�Bp�Br�Bm�Bp�BffBl�B[#B�LB�B�B��B�B�B�ZB�B1B�B7LBD�BZBe`BiyBl�BhsB\)BF�B=qB33B)�B�B��B�B�B�B�B�ZB��B�}B�B��B�JBz�BhsBVBD�B49B+B�B
��B
�;B
��B
�XB
�VB
r�B
`BB
A�B
�B
B	��B	�B	�5B	��B	�3B	�!B	�B	��B	�JB	z�B	aHB	L�B	<jB	5?B	%�B	�B	B�B�BƨB�FB�B��B�\B�1B�%B�B�B�B� B�B�B�DB�PB�\B�oB��B��B�B�'B�FB�^B�qBBÖBÖB�}BÖB�wB�FB�3B�B�B�!B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�9B�RB�dB�^B�LB�3B�FB��B��B��B��B�/B�/B�#B�#B�`B�mB�B��B�B��B��B��B��B	B��B��B	B	B��B�mB�B�B�B�B�NB�BB�TB�#B��B��B��B��B��B��B�B�B�#B�/B�)B�#B�BB�`B�fB�B��B��B��B	B	%B	1B	VB	�B	�B	�B	�B	�B	�B	 �B	'�B	,B	,B	,B	1'B	5?B	8RB	;dB	;dB	<jB	<jB	=qB	>wB	D�B	G�B	H�B	K�B	M�B	O�B	P�B	R�B	S�B	VB	VB	W
B	YB	ZB	ZB	]/B	bNB	ffB	gmB	hsB	gmB	hsB	gmB	gmB	gmB	gmB	hsB	jB	n�B	r�B	s�B	u�B	w�B	x�B	w�B	t�B	t�B	t�B	v�B	w�B	x�B	z�B	{�B	}�B	~�B	~�B	�B	�B	�%B	�+B	�1B	�=B	�DB	�PB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�?B	�?B	�?B	�LB	�RB	�RB	�XB	�XB	�dB	�dB	�dB	�qB	��B	B	ÖB	ƨB	ǮB	ȴB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�;B	�BB	�HAUG�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B

=B

=B
	7B
	7B
	7B
1B
1B
1B
+B
1B
DB
PB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
hB
bB
hB
hB
hB
hB
hB
hB
hB
oB
hB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
%�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190535                              AO  ARCAADJP                                                                    20181005190535    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190535  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190535  QCF$                G�O�G�O�G�O�8000            