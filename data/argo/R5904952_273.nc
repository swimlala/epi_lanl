CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:07Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190607  20181005190607  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @����8�01   @������@27�O�;d�c����F1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @y��@�  A   AffA>ffA`  A�  A�  A�  A�  A���A�  A�  A�  B   B  BffBffB   B(  B0  B8  B@  BH  BP  BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF�CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  Dy�D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  Dy�D  D� D  D� D  D� D  D� D��D� D  Dy�D��D� D  D�fD  Dy�D  D� D  D� D  D� DfD� D��D� D  D� D  D� D  D� D  Dy�D��D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*y�D*��D+� D,fD,�fD-  D-� D.  D.� D/  D/� D0  D0� D0��D1� D2fD2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8y�D8��D9� D:  D:y�D;  D;�fD<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DAy�DB  DB� DCfDC�fDDfDD�fDEfDE� DE��DFy�DF��DGy�DG��DH� DI  DI� DJfDJ� DJ��DKy�DK��DLy�DL��DMy�DM��DN� DO  DO�fDPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[�fD\fD\� D\��D]� D^  D^y�D_  D_�fD`  D`� Da  Da� Db  Dby�Dc  Dc�fDd  Dd� Dd��Dey�Df  Df� Dg  Dg� DhfDh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dw��Dy�D�X 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�fg@ə�A��A#33AC33Ad��A�ffA�ffA�ffA�ffA�33A�ffA�ffA�ffB33B	33B��B��B!33B)33B133B933BA33BI33BQ33BX��B`��Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B���B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CfgCfgCL�CfgCL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:fgC<L�C>L�C@L�CBL�CDL�CFfgCHfgCJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�Cb33CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�33C�33C�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC��C�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fD 3D �3D3D�3D3D�3D�D�3D3D��D3D�3D3D�3D3D�3D3D�3D	3D	��D
3D
�3D3D��D3D�3D3D�3D3D�3D3D�3D�D�3D3D��D�D�3D3D��D3D��D3D�3D3D�3D3D�3D�D�3D�D�3D3D�3D3D�3D3D�3D3D��D�D��D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$�D$�3D%3D%�3D&3D&�3D'3D'�3D(�D(�3D)3D)�3D*3D*��D+�D+�3D,�D,��D-3D-�3D.3D.�3D/3D/�3D03D0�3D1�D1�3D2�D2�3D33D3�3D43D4�3D5�D5�3D63D6�3D73D7�3D83D8��D9�D9�3D:3D:��D;3D;��D<3D<�3D=3D=�3D>�D>�3D?3D?�3D@3D@�3DA3DA��DB3DB�3DC�DC��DD�DD��DE�DE�3DF�DF��DG�DG��DH�DH�3DI3DI�3DJ�DJ�3DK�DK��DL�DL��DM�DM��DN�DN�3DO3DO��DP�DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV�DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[�D[��D\�D\�3D]�D]�3D^3D^��D_3D_��D`3D`�3Da3Da�3Db3Db��Dc3Dc��Dd3Dd�3De�De��Df3Df�3Dg3Dg�3Dh�Dh�3Di3Di��Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn��Do3Do�3Dp3Dp�3Dq3Dq�3Dr�Dr�3Ds3Ds�3Dt3Dt��Du3Du�3Dv3Dv�3Dw3Dw�3Dw� Dy�GD�a�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AȮAȶFAȸRAȸRA�A�A�ĜA�ĜA�ƨA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A�ȴAȬA�C�A��Aš�A�z�A�(�A�&�A���A��#A�ĜAēuA� �A�33A���A°!A¬A�z�A��A�$�A���A��7A��+A�"�A��A���A�jA�1'A���A�ĜA��+A�`BA�A��hA�A�A�9XA�7LA�7LA��mA�=qA�+A��#A��hA�5?A�A���A��A��
A���A�hsA�?}A��A���A�VA�n�A��TA�S�A�9XA��yA�|�A�Q�A�A�A��A���A���A��
A�  A�A�ZA�ĜA���A���A�p�A�;dA�ĜA�A�VA���A�?}A�ƨA�z�A��^A�&�A�K�A��+A���A��A���A%Ax�+AvA�Ao"�Al�/Aj�yAit�AhE�Ag"�Ae&�A`A�AZv�AW�AU��AS�AQO�AL�`AI�AG�PAF~�AEG�AD9XAD  AB�!A@ȴA>�+A="�A:�DA7dZA5`BA3�A2�`A0��A/��A-��A+��A+dZA+�A*-A)K�A'�A'%A#�A"�A"A�A!�7A r�A�-A"�AI�AC�AA�A�A�^A��AffA�A&�A�\AVA(�AK�A�!AA�A�mA"�A^5AbNA�A$�A�A33AA�AƨA�7AG�A
��A
r�A
�A	�A
=qA
$�A
�A
5?A
9XA	��A	7LA�!AI�A(�A  A;dA(�A �Ax�A5?A �yA �@���@�K�@�
=@���@�~�@�E�@��@�O�@��@�  @�b@�|�@�@���@�t�@�ȴ@�@��7@���@�@�Q�@�I�@��m@��@�V@�$�@���@�%@�j@���@�9@�(�@��@�
=@��/@�D@���@�X@�dZ@�S�@�ȴ@�R@�9@�+@��@�j@���@�hs@�x�@��@�r�@��
@�z�@�Ĝ@�A�@��
@�?}@�ff@�E�@�{@�`B@�Ĝ@�9X@�"�@ڧ�@�J@���@�hs@�O�@��@��
@�C�@�"�@ָR@�-@ՙ�@�7L@��/@ԋD@Ԭ@Ԭ@ԃ@ӝ�@�v�@�E�@�-@���@���@ѩ�@с@�?}@�V@��@мj@�I�@ϥ�@�33@�@θR@�=q@�@�hs@���@�(�@�+@ʏ\@�^5@�$�@ə�@�O�@�&�@��@���@ȓu@�(�@�b@��m@Ǖ�@�S�@��y@�ff@�{@�G�@�`B@�O�@�hs@őh@�O�@��@�%@��`@��;@Å@�dZ@�S�@��@�M�@��@�`B@�&�@�j@���@��\@�$�@��-@�hs@���@�bN@�Z@��@�+@��@�+@�"�@�
=@��y@�ȴ@���@�$�@���@���@�p�@�?}@���@��j@��u@�r�@�ƨ@�dZ@���@��@��7@���@��@� �@��
@���@��P@�
=@�ȴ@���@��@�@�`B@��@�bN@�ƨ@�E�@�@�/@��@���@��m@��F@�ƨ@���@�|�@��P@��P@��@�K�@�33@�+@�n�@���@��@�hs@��@�%@��@��@���@��j@���@���@�r�@�  @���@��;@���@��F@�o@��@���@�/@���@�r�@�A�@�1'@�(�@��@�  @���@��@��@��@���@��P@�o@���@�@���@���@�p�@���@��@�ƨ@��w@���@���@��@���@��@�  @�ƨ@�;d@�@��@���@��+@�~�@�V@�$�@�J@���@���@�%@���@���@��m@�ȴ@�=q@��^@�&�@�Ĝ@�bN@�9X@��@���@���@���@�n�@�-@��-@��/@�3�@�k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AȮAȶFAȸRAȸRA�A�A�ĜA�ĜA�ƨA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A�ȴAȬA�C�A��Aš�A�z�A�(�A�&�A���A��#A�ĜAēuA� �A�33A���A°!A¬A�z�A��A�$�A���A��7A��+A�"�A��A���A�jA�1'A���A�ĜA��+A�`BA�A��hA�A�A�9XA�7LA�7LA��mA�=qA�+A��#A��hA�5?A�A���A��A��
A���A�hsA�?}A��A���A�VA�n�A��TA�S�A�9XA��yA�|�A�Q�A�A�A��A���A���A��
A�  A�A�ZA�ĜA���A���A�p�A�;dA�ĜA�A�VA���A�?}A�ƨA�z�A��^A�&�A�K�A��+A���A��A���A%Ax�+AvA�Ao"�Al�/Aj�yAit�AhE�Ag"�Ae&�A`A�AZv�AW�AU��AS�AQO�AL�`AI�AG�PAF~�AEG�AD9XAD  AB�!A@ȴA>�+A="�A:�DA7dZA5`BA3�A2�`A0��A/��A-��A+��A+dZA+�A*-A)K�A'�A'%A#�A"�A"A�A!�7A r�A�-A"�AI�AC�AA�A�A�^A��AffA�A&�A�\AVA(�AK�A�!AA�A�mA"�A^5AbNA�A$�A�A33AA�AƨA�7AG�A
��A
r�A
�A	�A
=qA
$�A
�A
5?A
9XA	��A	7LA�!AI�A(�A  A;dA(�A �Ax�A5?A �yA �@���@�K�@�
=@���@�~�@�E�@��@�O�@��@�  @�b@�|�@�@���@�t�@�ȴ@�@��7@���@�@�Q�@�I�@��m@��@�V@�$�@���@�%@�j@���@�9@�(�@��@�
=@��/@�D@���@�X@�dZ@�S�@�ȴ@�R@�9@�+@��@�j@���@�hs@�x�@��@�r�@��
@�z�@�Ĝ@�A�@��
@�?}@�ff@�E�@�{@�`B@�Ĝ@�9X@�"�@ڧ�@�J@���@�hs@�O�@��@��
@�C�@�"�@ָR@�-@ՙ�@�7L@��/@ԋD@Ԭ@Ԭ@ԃ@ӝ�@�v�@�E�@�-@���@���@ѩ�@с@�?}@�V@��@мj@�I�@ϥ�@�33@�@θR@�=q@�@�hs@���@�(�@�+@ʏ\@�^5@�$�@ə�@�O�@�&�@��@���@ȓu@�(�@�b@��m@Ǖ�@�S�@��y@�ff@�{@�G�@�`B@�O�@�hs@őh@�O�@��@�%@��`@��;@Å@�dZ@�S�@��@�M�@��@�`B@�&�@�j@���@��\@�$�@��-@�hs@���@�bN@�Z@��@�+@��@�+@�"�@�
=@��y@�ȴ@���@�$�@���@���@�p�@�?}@���@��j@��u@�r�@�ƨ@�dZ@���@��@��7@���@��@� �@��
@���@��P@�
=@�ȴ@���@��@�@�`B@��@�bN@�ƨ@�E�@�@�/@��@���@��m@��F@�ƨ@���@�|�@��P@��P@��@�K�@�33@�+@�n�@���@��@�hs@��@�%@��@��@���@��j@���@���@�r�@�  @���@��;@���@��F@�o@��@���@�/@���@�r�@�A�@�1'@�(�@��@�  @���@��@��@��@���@��P@�o@���@�@���@���@�p�@���@��@�ƨ@��w@���@���@��@���@��@�  @�ƨ@�;d@�@��@���@��+@�~�@�V@�$�@�J@���@���@�%@���@���@��m@�ȴ@�=q@��^@�&�@�Ĝ@�bN@�9X@��@���@���@���@�n�@�-@��-@��/@�3�@�k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	49B	49B	49B	49B	49B	5?B	5?B	5?B	5?B	5?B	5?B	5?B	6FB	6FB	6FB	6FB	6FB	5?B	6FB	5?B	5?B	5?B	6FB	6FB	9XB	F�B
B
ÖB
�mB
�BB49B@�BI�BM�BM�BD�B7LB.B,B-B.B-B6FB7LB9XB;dBB�BM�BZBbNBiyBm�Bs�B{�B�JB��B�jB��B��B��B��B�BB�B8RBC�BL�BZBp�Bu�Bv�Bx�Bz�B{�B{�B}�B~�B�B�1B�DB�PB�7B�%B�B� Bz�Bk�BVBS�BT�BP�BE�B?}B7LB$�BoBB�B�5BȴB��B�BR�B�B
�B
��B
@�B	��B	�B	�dB	��B	�\B	t�B	R�B	B�B	,B	#�B	�B	�B	�B	hB		7B��B�B�HB�B�
B��BɺBǮBɺB��B��B��BȴBǮBǮBɺBɺB��B��BȴBǮBŢBŢBÖBÖBÖBÖBB��BĜBǮBĜBǮBǮBƨBŢB��B�;B�BB�HB�NB�NB�B�yB�BB�TB�5B�
B�B��B��B�B�)B�;B�B��B	B	B	+B	PB	VB	JB	DB	1B	1B		7B	VB	uB	�B	#�B	0!B	1'B	33B	6FB	:^B	;dB	9XB	;dB	=qB	<jB	>wB	?}B	7LB	.B	1'B	-B	(�B	&�B	%�B	&�B	'�B	)�B	)�B	)�B	)�B	)�B	-B	49B	7LB	=qB	<jB	8RB	5?B	49B	33B	49B	5?B	8RB	9XB	<jB	@�B	G�B	H�B	I�B	I�B	K�B	T�B	`BB	aHB	e`B	dZB	ZB	Q�B	P�B	M�B	J�B	L�B	Q�B	XB	[#B	iyB	z�B	y�B	z�B	~�B	�%B	�=B	�DB	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�!B	�!B	�dB	�}B	�}B	�wB	��B	��B	��B	��B	�}B	�}B	��B	��B	B	ÖB	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	��B	B	ÖB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�/B	�;B	�/B	�)B	�)B	�)B	�/B	�5B	�NB	�`B	�fB	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B

=B

=B

=B

=B

=B
DB

=B

=B
DB
DB
DB
DB

=B
	7B

=B
1B
+B
+B
+B
1B
1B
1B
+B
	7B
	7B
	7B
	7B
	7B
bB
B
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B	49B	49B	49B	49B	49B	5?B	5?B	5?B	5?B	5?B	5?B	5?B	6FB	6FB	6FB	6FB	6FB	5?B	6FB	5?B	5?B	5?B	6FB	6FB	9XB	F�B
B
ÖB
�mB
�BB49B@�BI�BM�BM�BD�B7LB.B,B-B.B-B6FB7LB9XB;dBB�BM�BZBbNBiyBm�Bs�B{�B�JB��B�jB��B��B��B��B�BB�B8RBC�BL�BZBp�Bu�Bv�Bx�Bz�B{�B{�B}�B~�B�B�1B�DB�PB�7B�%B�B� Bz�Bk�BVBS�BT�BP�BE�B?}B7LB$�BoBB�B�5BȴB��B�BR�B�B
�B
��B
@�B	��B	�B	�dB	��B	�\B	t�B	R�B	B�B	,B	#�B	�B	�B	�B	hB		7B��B�B�HB�B�
B��BɺBǮBɺB��B��B��BȴBǮBǮBɺBɺB��B��BȴBǮBŢBŢBÖBÖBÖBÖBB��BĜBǮBĜBǮBǮBƨBŢB��B�;B�BB�HB�NB�NB�B�yB�BB�TB�5B�
B�B��B��B�B�)B�;B�B��B	B	B	+B	PB	VB	JB	DB	1B	1B		7B	VB	uB	�B	#�B	0!B	1'B	33B	6FB	:^B	;dB	9XB	;dB	=qB	<jB	>wB	?}B	7LB	.B	1'B	-B	(�B	&�B	%�B	&�B	'�B	)�B	)�B	)�B	)�B	)�B	-B	49B	7LB	=qB	<jB	8RB	5?B	49B	33B	49B	5?B	8RB	9XB	<jB	@�B	G�B	H�B	I�B	I�B	K�B	T�B	`BB	aHB	e`B	dZB	ZB	Q�B	P�B	M�B	J�B	L�B	Q�B	XB	[#B	iyB	z�B	y�B	z�B	~�B	�%B	�=B	�DB	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�!B	�!B	�dB	�}B	�}B	�wB	��B	��B	��B	��B	�}B	�}B	��B	��B	B	ÖB	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	��B	B	ÖB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�/B	�;B	�/B	�)B	�)B	�)B	�/B	�5B	�NB	�`B	�fB	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B

=B

=B

=B

=B

=B
DB

=B

=B
DB
DB
DB
DB

=B
	7B

=B
1B
+B
+B
+B
1B
1B
1B
+B
	7B
	7B
	7B
	7B
	7B
bB
B
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190607                              AO  ARCAADJP                                                                    20181005190607    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190607  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190607  QCF$                G�O�G�O�G�O�8000            