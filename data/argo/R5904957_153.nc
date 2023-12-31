CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:33Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140833  20181024140833  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�������1   @���y\��@5e�S����c��1&�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BXffB_33Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B���C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D	��D
� D  D� D  D� D  D�fD  D� D  D� D  D� D  Dy�D  Dy�D��Dy�D  D� D  D�fD  D� D  D� D��D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1fD1�fD2  D2� D3  D3� D4  D4�fD5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DMy�DM��DN� DOfDO� DP  DP� DQ  DQ� DQ��DR� DS  DSy�DS��DTy�DT��DU� DV  DV� DW  DWy�DW��DXy�DX��DY� DZ  DZ� D[  D[� D[��D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� DlfDl�fDm  Dm� Dn  Dn� DofDo�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDtfDt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy��D�O\D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B9��BA��BI33BQ33BY��B`ffBh��Bq33By33B���B���B���B���B���B���B���B���B���B���B�fgB���B���B���B���B���B���Bę�B���B̙�BЙ�B�fgB�fgCL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,fgC.L�C0L�C2L�C4L�C6fgC8L�C:L�C<L�C>L�C@L�CBL�CD33CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\fgC^L�C`33CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC��C�&fC�&fC�33C�&fC��C�&fC�&fC�&fC��C�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�33C�33C�&fC�&fC��C�&fC�&fC�&fC��C�&fC�&fC�&fC��C�&fC�33C�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33D 3D �3D�D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D�D�3D	3D	�3D
�D
�3D3D�3D3D�3D3D��D3D�3D3D�3D3D�3D3D��D3D��D�D��D3D�3D3D��D3D�3D3D�3D�D�3D3D�3D3D�3D3D�3D3D��D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'�D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-��D.3D.�3D/3D/�3D03D0�3D1�D1��D23D2�3D33D3�3D43D4��D53D5�3D6�D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=��D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL��DM3DM��DN�DN�3DO�DO�3DP3DP�3DQ3DQ�3DR�DR�3DS3DS��DT�DT��DU�DU�3DV3DV�3DW3DW��DX�DX��DY�DY�3DZ3DZ�3D[3D[�3D\�D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De��Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di��Dj3Dj�3Dk3Dk�3Dl�Dl��Dm3Dm�3Dn3Dn�3Do�Do��Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds��Dt�Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3DxfDy��D�X�D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aԝ�Aԝ�Aԝ�Aԟ�Aԡ�Aԣ�Aԣ�Aԧ�Aԧ�Aԧ�Aԩ�Aԩ�Aԧ�Aԩ�AԬAԬAԩ�AԬAԮAԮAԮA԰!A԰!AԮAԬA԰!A�p�A�^5A�p�A�z�A��wA��A�v�A��A�JA���A�S�A�Q�A���A�-A��`A�9XA�9XA�7LA�O�A��`A�1A�S�A��uA���A���A�ƨA�1A��FA��A��FA��FA��A��A�bNA��A�ƨA��A��TA+A}�A}�hA}dZA|��Az�Ax$�Av��Ar��Ao�Am/Ak�
Ai��Af�HAf{AdM�Ab^5Aap�A_�A]33AW�FAT��AS7LAP��AM/AL�/ALjAK&�AJ$�AIx�AH��AGVAD�RACt�AB��AA�FA?K�A<-A;
=A:��A:M�A9��A9��A8�!A7|�A6~�A4�9A3G�A2�jA2ffA2�A1�^A0��A.�A,��A,v�A+"�A)��A(v�A'��A'
=A&~�A&=qA$�`A#XA"�9A!�A ��A n�A 1'A��A
=AJA+A�A�HA��A�AoA�A�A��A��Al�AO�A?}A33A�/A  A�DAĜA;dA��A��A=qA��A��AQ�A��A	�A��A7LA�RAbNA  A7LAn�A1'A�
A5?A��A;dA
=A �9A �@��@���@��9@�  @��\@��@�I�@�-@�%@��m@�R@��@�&�@�Ĝ@��@�F@�
=@��H@�R@�1@��@�r�@�1'@�b@���@���@�;d@�~�@�O�@�A�@�t�@�o@◍@���@�X@��/@��u@�1'@ߕ�@�v�@ݩ�@��@��m@���@�Q�@�9X@�9X@�(�@��m@�ƨ@ו�@�dZ@��@�A�@�C�@�ȴ@�^5@с@���@�J@�@ͺ^@ͺ^@ͺ^@Ͳ-@͑h@͉7@͉7@�hs@�&�@��@�A�@�S�@ʟ�@�M�@��@�7L@�j@Ǿw@�S�@��@�^5@��#@�`B@�I�@���@�S�@��@�V@�j@�1@�C�@��@�X@�(�@��@�l�@�@�J@�&�@�r�@�33@�=q@���@�G�@���@�Z@�1'@�I�@�bN@�Z@�bN@�I�@��@�9X@��@�A�@�;d@��!@�M�@��@�J@��#@�hs@���@�Q�@���@���@�-@���@�V@��@���@��@��@���@��P@���@�=q@���@�`B@���@��9@��@�1'@�ƨ@�dZ@���@��\@���@�p�@�X@�/@��@�V@�%@���@���@�Q�@�A�@�9X@�1'@�1@���@��@��;@�K�@�"�@���@���@�v�@�=q@�{@���@��-@���@�j@��m@���@��@�-@��@���@�hs@�O�@�&�@���@�bN@�9X@��
@���@���@��@��@�dZ@��@�ff@�J@��h@�G�@��@���@��@��/@��/@��@��D@�j@�Q�@�I�@�b@�  @��@��;@�ƨ@��w@���@���@��P@��P@�dZ@�"�@��@��@��@�o@�o@�
=@���@��H@���@���@�n�@�-@���@���@�@��@�/@��@��j@���@��u@�I�@�1'@� �@���@���@��F@���@��P@�\)@��@���@���@�n�@�J@���@��T@���@��-@��@�G�@�V@��`@��/@��9@��@�j@�Z@�A�@��;@�l�@�33@��y@���@�M�@�J@���@�`B@�%@��j@���@�A�@�b@���@�ƨ@���@��@��y@��+@�J@��T@� \@n��@b�'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aԝ�Aԝ�Aԝ�Aԟ�Aԡ�Aԣ�Aԣ�Aԧ�Aԧ�Aԧ�Aԩ�Aԩ�Aԧ�Aԩ�AԬAԬAԩ�AԬAԮAԮAԮA԰!A԰!AԮAԬA԰!A�p�A�^5A�p�A�z�A��wA��A�v�A��A�JA���A�S�A�Q�A���A�-A��`A�9XA�9XA�7LA�O�A��`A�1A�S�A��uA���A���A�ƨA�1A��FA��A��FA��FA��A��A�bNA��A�ƨA��A��TA+A}�A}�hA}dZA|��Az�Ax$�Av��Ar��Ao�Am/Ak�
Ai��Af�HAf{AdM�Ab^5Aap�A_�A]33AW�FAT��AS7LAP��AM/AL�/ALjAK&�AJ$�AIx�AH��AGVAD�RACt�AB��AA�FA?K�A<-A;
=A:��A:M�A9��A9��A8�!A7|�A6~�A4�9A3G�A2�jA2ffA2�A1�^A0��A.�A,��A,v�A+"�A)��A(v�A'��A'
=A&~�A&=qA$�`A#XA"�9A!�A ��A n�A 1'A��A
=AJA+A�A�HA��A�AoA�A�A��A��Al�AO�A?}A33A�/A  A�DAĜA;dA��A��A=qA��A��AQ�A��A	�A��A7LA�RAbNA  A7LAn�A1'A�
A5?A��A;dA
=A �9A �@��@���@��9@�  @��\@��@�I�@�-@�%@��m@�R@��@�&�@�Ĝ@��@�F@�
=@��H@�R@�1@��@�r�@�1'@�b@���@���@�;d@�~�@�O�@�A�@�t�@�o@◍@���@�X@��/@��u@�1'@ߕ�@�v�@ݩ�@��@��m@���@�Q�@�9X@�9X@�(�@��m@�ƨ@ו�@�dZ@��@�A�@�C�@�ȴ@�^5@с@���@�J@�@ͺ^@ͺ^@ͺ^@Ͳ-@͑h@͉7@͉7@�hs@�&�@��@�A�@�S�@ʟ�@�M�@��@�7L@�j@Ǿw@�S�@��@�^5@��#@�`B@�I�@���@�S�@��@�V@�j@�1@�C�@��@�X@�(�@��@�l�@�@�J@�&�@�r�@�33@�=q@���@�G�@���@�Z@�1'@�I�@�bN@�Z@�bN@�I�@��@�9X@��@�A�@�;d@��!@�M�@��@�J@��#@�hs@���@�Q�@���@���@�-@���@�V@��@���@��@��@���@��P@���@�=q@���@�`B@���@��9@��@�1'@�ƨ@�dZ@���@��\@���@�p�@�X@�/@��@�V@�%@���@���@�Q�@�A�@�9X@�1'@�1@���@��@��;@�K�@�"�@���@���@�v�@�=q@�{@���@��-@���@�j@��m@���@��@�-@��@���@�hs@�O�@�&�@���@�bN@�9X@��
@���@���@��@��@�dZ@��@�ff@�J@��h@�G�@��@���@��@��/@��/@��@��D@�j@�Q�@�I�@�b@�  @��@��;@�ƨ@��w@���@���@��P@��P@�dZ@�"�@��@��@��@�o@�o@�
=@���@��H@���@���@�n�@�-@���@���@�@��@�/@��@��j@���@��u@�I�@�1'@� �@���@���@��F@���@��P@�\)@��@���@���@�n�@�J@���@��T@���@��-@��@�G�@�V@��`@��/@��9@��@�j@�Z@�A�@��;@�l�@�33@��y@���@�M�@�J@���@�`B@�%@��j@���@�A�@�b@���@�ƨ@���@��@��y@��+@�J@��T@� \@n��@b�'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B%B �B8RB@�BC�BF�BH�BL�BL�BP�BW
BXBVBT�BR�BL�BE�B@�B6FB0!B,B'�B!�B�B�?���B
�oB
�1B
v�B
aHB
M�B
E�B
9XB
33B
1'B
0!B
&�B
�B
JB

=B
1B
B	��B	�sB	�5B	��B	�jB	�-B	��B	��B	�VB	�7B	� B	s�B	k�B	`BB	VB	>wB	49B	.B	"�B	�B	uB	bB	DB	%B	B��B��B�B�B�B�TB�B��B��B��B��BɺBȴBƨBÖB��B�wB�wB�qB�qB�jB�dB�XB�LB�?B�3B�'B�!B�!B�!B�B�B�B�B�B�B�B�B��B��B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�9B�?B�?B�?B�?B�?B�?B�?B�?B�jB�qB�qB�jB�wBĜBȴBɺBɺBɺBɺBɺBɺBɺBɺBɺB��B��B��B��B��B��B�B�B�B�/B�;B�HB�TB�`B�fB�B�B�B��B��B��B��B	B	B		7B	PB	\B	bB	oB	�B	�B	�B	!�B	&�B	(�B	+B	.B	1'B	49B	8RB	<jB	>wB	@�B	A�B	B�B	E�B	H�B	J�B	O�B	R�B	T�B	VB	W
B	YB	]/B	_;B	_;B	_;B	_;B	^5B	\)B	[#B	XB	XB	[#B	]/B	]/B	aHB	dZB	gmB	iyB	l�B	o�B	s�B	v�B	x�B	{�B	}�B	� B	�B	�JB	�VB	�VB	�bB	�oB	�oB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�9B	�?B	�RB	�^B	�^B	�dB	�jB	�wB	��B	B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�/B	�/B	�/B	�5B	�5B	�BB	�NB	�TB	�ZB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
\B
bB
4B
�B
,"1111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B%B �B8RB@�BC�BF�BH�BL�BL�BP�BW
BXBVBT�BR�BL�BE�B@�B6FB0!B,B'�B!�B�B�?���B
�oB
�1B
v�B
aHB
M�B
E�B
9XB
33B
1'B
0!B
&�B
�B
JB

=B
1B
B	��B	�sB	�5B	��B	�jB	�-B	��B	��B	�VB	�7B	� B	s�B	k�B	`BB	VB	>wB	49B	.B	"�B	�B	uB	bB	DB	%B	B��B��B�B�B�B�TB�B��B��B��B��BɺBȴBƨBÖB��B�wB�wB�qB�qB�jB�dB�XB�LB�?B�3B�'B�!B�!B�!B�B�B�B�B�B�B�B�B��B��B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�9B�?B�?B�?B�?B�?B�?B�?B�?B�jB�qB�qB�jB�wBĜBȴBɺBɺBɺBɺBɺBɺBɺBɺBɺB��B��B��B��B��B��B�B�B�B�/B�;B�HB�TB�`B�fB�B�B�B��B��B��B��B	B	B		7B	PB	\B	bB	oB	�B	�B	�B	!�B	&�B	(�B	+B	.B	1'B	49B	8RB	<jB	>wB	@�B	A�B	B�B	E�B	H�B	J�B	O�B	R�B	T�B	VB	W
B	YB	]/B	_;B	_;B	_;B	_;B	^5B	\)B	[#B	XB	XB	[#B	]/B	]/B	aHB	dZB	gmB	iyB	l�B	o�B	s�B	v�B	x�B	{�B	}�B	� B	�B	�JB	�VB	�VB	�bB	�oB	�oB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�9B	�?B	�RB	�^B	�^B	�dB	�jB	�wB	��B	B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�/B	�/B	�/B	�5B	�5B	�BB	�NB	�TB	�ZB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
\B
bB
4B
�B
,"1111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140833                              AO  ARCAADJP                                                                    20181024140833    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140833  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140833  QCF$                G�O�G�O�G�O�0               