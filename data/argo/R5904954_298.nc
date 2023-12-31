CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:57Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191757  20181005191757  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              *A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�������1   @���{Bq�@5=/��w�d���$�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     *A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCu�fCx  Cz�C|  C~  C��C�  C��3C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C��C��3C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C��3C��3C�  C��3C��3C�  C�  C��3C�  C��C�  C��3C�  C��C��3C��3D fD y�D ��D� D��D� DfDy�D��Dy�D��Dy�D  D� D  D� DfDy�D	  D	� D	��D
�fDfD� D  D�fD��Dy�D��D� DfD� D  D� DfDy�D  D� DfD�fD  D� D  D� D  D� D  D� D  D�fD��D� D  D� D  D� D  D� D��Dy�DfD� D  D� D   D �fD!  D!s3D"  D"� D#  D#� D$  D$y�D%fD%� D&  D&� D'fD'� D(fD(� D)  D)� D*  D*y�D+  D+� D+��D,�fD,��D-� D-��D.�fD.��D/� D0fD0y�D1  D1�fD2  D2� D2��D3y�D4  D4� D4��D5� D6  D6� D7  D7y�D7��D8�fD9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>fD>� D?fD?�fD@  D@� DA  DAy�DB  DB� DCfDC�fDDfDDy�DE  DE� DF  DF� DF��DG� DH  DHy�DIfDIy�DJ  DJ� DK  DKs3DK�3DL� DMfDM� DN  DN�fDO  DOy�DP  DP� DQ  DQ� DR  DR�fDR��DS� DS��DT�fDT��DU� DV  DV� DW  DWy�DX  DXy�DYfDY�fDZfDZ� D[  D[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�DbfDby�DcfDc� Dd  Dd�fDd��De� Df  Df� Df��Dgy�DhfDhy�DifDiy�Dj  Dj� DkfDk�fDl�Dl� DmfDm�fDnfDny�Do  Do�fDp  Dps3DqfDq� Dr  Dr� Ds  Dss3Ds��Dt� DufDu�fDvfDv�fDwfDw� Dw��Dys�D�?
D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CfgCL�CL�C L�C"L�C$L�C&L�C(fgC*L�C,L�C.L�C0L�C2L�C4L�C6L�C833C:L�C<L�C>L�C@L�CBfgCDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CX33CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�Ct33Cv33CxL�CzfgC|L�C~L�C�33C�&fC��C�&fC�&fC��C��C�&fC�&fC�33C�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�33C�33C�33C��C��C��C�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�33C�&fC��C�&fC�&fC�&fC�&fC�&fC��C��C�&fC�33C�&fC��C��C��C�&fC��C��C�&fC�&fC��C�&fC�33C�&fC��C�&fC�33C��C��D �D ��D�D�3D�D�3D�D��D�D��D�D��D3D�3D3D�3D�D��D	3D	�3D
�D
��D�D�3D3D��D�D��D�D�3D�D�3D3D�3D�D��D3D�3D�D��D3D�3D3D�3D3D�3D3D�3D3D��D�D�3D3D�3D3D�3D3D�3D�D��D�D�3D3D�3D 3D ��D!3D!�fD"3D"�3D#3D#�3D$3D$��D%�D%�3D&3D&�3D'�D'�3D(�D(�3D)3D)�3D*3D*��D+3D+�3D,�D,��D-�D-�3D.�D.��D/�D/�3D0�D0��D13D1��D23D2�3D3�D3��D43D4�3D5�D5�3D63D6�3D73D7��D8�D8��D93D9�3D:3D:�3D;3D;�3D<3D<��D=3D=�3D>�D>�3D?�D?��D@3D@�3DA3DA��DB3DB�3DC�DC��DD�DD��DE3DE�3DF3DF�3DG�DG�3DH3DH��DI�DI��DJ3DJ�3DK3DK�fDLfDL�3DM�DM�3DN3DN��DO3DO��DP3DP�3DQ3DQ�3DR3DR��DS�DS�3DT�DT��DU�DU�3DV3DV�3DW3DW��DX3DX��DY�DY��DZ�DZ�3D[3D[��D\�D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da��Db�Db��Dc�Dc�3Dd3Dd��De�De�3Df3Df�3Dg�Dg��Dh�Dh��Di�Di��Dj3Dj�3Dk�Dk��Dl  Dl�3Dm�Dm��Dn�Dn��Do3Do��Dp3Dp�fDq�Dq�3Dr3Dr�3Ds3Ds�fDt�Dt�3Du�Du��Dv�Dv��Dw�Dw�3Dw� Dy�
D�H�D�ˆ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�VA�VA�S�A�9XA�  A�O�A��+A�/A��`A��jA�p�A�"�A���A��FA�E�A�XA�S�A�33A���A��FA�1'A�1A�x�A��-A���A�`BA�/A���A���A���A��+A��A�p�A�XA�=qA�+A���A��;A�A��9A��A��7A�p�A�^5A�;dA�{A���A��/A�ĜA�G�A�(�A�&�A��TA���A�G�A��RA�;dA��A���A�n�A�bNA�dZA�O�A�1A���A�r�A�  A�bNA�XA�  A�|�A��A�ĜA��`A�p�A���A��A�+A��jA�O�A�`BA�hsA�
=A�XA��;A�dZA��!A�^5A���A�v�A�p�A�;dA�-A�~�A�%A���A���A��A�ƨA��uA�~�A�1'A���A�  A�z�A��A�&�A�1'A��A��A�1A�+A�+A�ƨA��RA�t�A�+A��TA�ȴA� �A�A�l�A+A|=qA{&�Az �Ay��Ax�`Aw�TAwt�Av�DAu�^At$�Ar�!Aq�Ao�-An��Am�mAl�Ak�
Akl�Ak\)Ai�PAg�hAg
=Af�RAfffAfJAc�Aa�A_|�A]�7A[��AZ$�AYVAV�AUhsAS�mARA�AQ�;AQ��APA�ALZAI�AG�AF�HAD�ACC�AB{AA
=A@(�A??}A<�A9�-A5\)A4bA2�`A0��A/G�A-�PA,Q�A+C�A)l�A'�FA%�A$Q�A#`BA"�9A"JA �A �+A 9XA��A`BA5?A�A�yA�A|�A��Av�AM�A5?AJAM�A��AO�A=qA�TA�A33AȴA�+A(�A�FA��A�hA
��A
ffA	��A�yAQ�A�A=qAx�AXA\)A�/A`BA`BA 5?@���@�t�@�33@���@�Q�@�V@�`B@��@��
@�1'@�@��`@�j@��
@�K�@��@@���@�O�@��@�I�@�@�{@�^@�l�@�V@��@��D@ߝ�@�S�@���@ۥ�@ؓu@�V@��@ӶF@���@��@��`@�I�@�5?@Ͳ-@�7L@���@�b@ʸR@�$�@���@ɑh@ɺ^@�X@�I�@�^5@�~�@�J@�?}@ă@Å@�dZ@�;d@§�@���@�9X@��h@�%@�\)@���@�(�@�S�@�+@��@��@�
=@���@���@�O�@�V@�A�@��@�=q@�p�@��@��9@�bN@�1'@�1@�  @�ƨ@�;d@�@��\@�5?@��@��-@�G�@��@��@��@�z�@��@��@��R@��\@�~�@�ff@�-@���@�&�@���@�(�@��m@���@�S�@�+@�"�@�o@�"�@��y@�$�@���@��`@���@�z�@�ƨ@�K�@���@�ȴ@���@��@�O�@��@�Ĝ@��D@���@��@��F@��P@�t�@�dZ@�\)@�"�@��H@�+@�dZ@�C�@�;d@���@�=q@���@���@�`B@�`B@��^@���@�p�@�G�@�V@��/@�Q�@��
@��P@�|�@��P@��w@���@�t�@��H@�~�@�ff@�V@���@�X@��j@��u@��u@�z�@�9X@�9X@�(�@� �@�1@��m@��w@��P@�S�@���@���@�^5@���@�7L@���@�Ĝ@��@�j@�Q�@�1@�1@��@�ƨ@�|�@�l�@�|�@�|�@�"�@���@�v�@�=q@�$�@�n�@���@�v�@���@���@�p�@�`B@�%@���@��@�bN@�1'@��m@��;@��
@��F@���@�1'@��@�K�@�o@�33@�\)@�33@�C�@��@��@��@�ff@���@���@��9@��u@�Z@��F@�\)@�K�@��@�
=@�o@��R@�ff@�^5@�V@�M�@�M�@�M�@�M�@�E�@�{@�b�@|V�@k�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�O�A�O�A�O�A�O�A�O�A�Q�A�VA�VA�S�A�9XA�  A�O�A��+A�/A��`A��jA�p�A�"�A���A��FA�E�A�XA�S�A�33A���A��FA�1'A�1A�x�A��-A���A�`BA�/A���A���A���A��+A��A�p�A�XA�=qA�+A���A��;A�A��9A��A��7A�p�A�^5A�;dA�{A���A��/A�ĜA�G�A�(�A�&�A��TA���A�G�A��RA�;dA��A���A�n�A�bNA�dZA�O�A�1A���A�r�A�  A�bNA�XA�  A�|�A��A�ĜA��`A�p�A���A��A�+A��jA�O�A�`BA�hsA�
=A�XA��;A�dZA��!A�^5A���A�v�A�p�A�;dA�-A�~�A�%A���A���A��A�ƨA��uA�~�A�1'A���A�  A�z�A��A�&�A�1'A��A��A�1A�+A�+A�ƨA��RA�t�A�+A��TA�ȴA� �A�A�l�A+A|=qA{&�Az �Ay��Ax�`Aw�TAwt�Av�DAu�^At$�Ar�!Aq�Ao�-An��Am�mAl�Ak�
Akl�Ak\)Ai�PAg�hAg
=Af�RAfffAfJAc�Aa�A_|�A]�7A[��AZ$�AYVAV�AUhsAS�mARA�AQ�;AQ��APA�ALZAI�AG�AF�HAD�ACC�AB{AA
=A@(�A??}A<�A9�-A5\)A4bA2�`A0��A/G�A-�PA,Q�A+C�A)l�A'�FA%�A$Q�A#`BA"�9A"JA �A �+A 9XA��A`BA5?A�A�yA�A|�A��Av�AM�A5?AJAM�A��AO�A=qA�TA�A33AȴA�+A(�A�FA��A�hA
��A
ffA	��A�yAQ�A�A=qAx�AXA\)A�/A`BA`BA 5?@���@�t�@�33@���@�Q�@�V@�`B@��@��
@�1'@�@��`@�j@��
@�K�@��@@���@�O�@��@�I�@�@�{@�^@�l�@�V@��@��D@ߝ�@�S�@���@ۥ�@ؓu@�V@��@ӶF@���@��@��`@�I�@�5?@Ͳ-@�7L@���@�b@ʸR@�$�@���@ɑh@ɺ^@�X@�I�@�^5@�~�@�J@�?}@ă@Å@�dZ@�;d@§�@���@�9X@��h@�%@�\)@���@�(�@�S�@�+@��@��@�
=@���@���@�O�@�V@�A�@��@�=q@�p�@��@��9@�bN@�1'@�1@�  @�ƨ@�;d@�@��\@�5?@��@��-@�G�@��@��@��@�z�@��@��@��R@��\@�~�@�ff@�-@���@�&�@���@�(�@��m@���@�S�@�+@�"�@�o@�"�@��y@�$�@���@��`@���@�z�@�ƨ@�K�@���@�ȴ@���@��@�O�@��@�Ĝ@��D@���@��@��F@��P@�t�@�dZ@�\)@�"�@��H@�+@�dZ@�C�@�;d@���@�=q@���@���@�`B@�`B@��^@���@�p�@�G�@�V@��/@�Q�@��
@��P@�|�@��P@��w@���@�t�@��H@�~�@�ff@�V@���@�X@��j@��u@��u@�z�@�9X@�9X@�(�@� �@�1@��m@��w@��P@�S�@���@���@�^5@���@�7L@���@�Ĝ@��@�j@�Q�@�1@�1@��@�ƨ@�|�@�l�@�|�@�|�@�"�@���@�v�@�=q@�$�@�n�@���@�v�@���@���@�p�@�`B@�%@���@��@�bN@�1'@��m@��;@��
@��F@���@�1'@��@�K�@�o@�33@�\)@�33@�C�@��@��@��@�ff@���@���@��9@��u@�Z@��F@�\)@�K�@��@�
=@�o@��R@�ff@�^5@�V@�M�@�M�@�M�@�M�@�E�@�{@�b�@|V�@k�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BJBJBDBJBJBPBVBVBVBVB�B%�B5?BG�B|�B�B�B�B�B}�Bw�B�B�1B�+B�By�Bs�Bw�B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�-B�-B�^B�wB�}BÖBǮB��B��B��B�B�B�/B�HB�NB�TB�`B�yB�B�;B��B�
B�/B�BJB�B�B��B��B�LB�3B��B��B��B�Bt�Bk�Bw�B}�BjBZBO�BL�BJ�B6FB	7B�B�B�sB�#BƨB�XB��B�PB�7B�Bz�Bv�BhsB`BBS�BN�BB�B5?B(�B�B{BB
�mB
�
B
��B
��B
ŢB
�9B
��B
�=B
x�B
}�B
x�B
v�B
q�B
k�B
gmB
`BB
YB
M�B
A�B
9XB
(�B
 �B
�B
uB
\B
\B
\B
B	�B	�B	�sB	�`B	�/B	�jB	�B	��B	�bB	�B	z�B	r�B	gmB	`BB	YB	P�B	O�B	N�B	B�B	+B	�B	VB	
=B	B��B��B�B�B�sB�;B��B�jB�FB�'B��B��B��B��B�VB�=B�B� B{�Bz�Bx�Bv�Bs�Br�Bp�Bn�BjBgmBffBcTBaHB^5B^5B^5B]/B\)BZBXBXBYBYBYBYBYBYBXBXBW
BVBVBYBXBW
BT�BQ�BP�BN�BN�BQ�BQ�BR�BP�BM�BK�BJ�BJ�BH�BF�BC�B@�B=qB:^B9XBM�BZBaHBe`BiyBl�Bo�Bp�Br�Bs�Bs�Br�Br�Bq�Bp�Bn�Bk�BhsBe`BdZBcTBdZBe`BiyBl�Bm�Bm�Bm�Bm�Bn�Bo�B{�B}�B}�B}�B|�B~�B�B�B�PB��B��B��B��B�B�!B�-B�?B�XB�XB�XB�dBBĜB��B��B��BǮB��B��B��B��B��B��B��B��B��B��B�B�B�NB�mB�sB�B�B�B��B	B	B	DB	JB	hB	{B	�B	�B	�B	�B	�B	�B	 �B	'�B	,B	0!B	1'B	1'B	1'B	1'B	1'B	33B	6FB	9XB	9XB	?}B	B�B	B�B	C�B	E�B	J�B	K�B	O�B	O�B	P�B	T�B	YB	ZB	ZB	\)B	^5B	_;B	aHB	cTB	e`B	gmB	k�B	q�B	q�B	r�B	s�B	u�B	u�B	v�B	w�B	x�B	�B	�+B	�7B	�=B	�=B	�JB	�VB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�LB	�RB	�XB	�^B	�^B	�jB	�wB	��B	��B	B	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�B	�#B	�/B	�/B	�/B	�)B	�)B	�/B	�;B	�;B	�BB	�;B	�;B	�BB	�HB	�HB	�HB	�TB	�`B	�yB	�sB	�`B	�fB	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
  B
B
,"222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BJBJBDBJBJBPBVBVBVBVB�B%�B5?BG�B|�B�B�B�B�B}�Bw�B�B�1B�+B�By�Bs�Bw�B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�-B�-B�^B�wB�}BÖBǮB��B��B��B�B�B�/B�HB�NB�TB�`B�yB�B�;B��B�
B�/B�BJB�B�B��B��B�LB�3B��B��B��B�Bt�Bk�Bw�B}�BjBZBO�BL�BJ�B6FB	7B�B�B�sB�#BƨB�XB��B�PB�7B�Bz�Bv�BhsB`BBS�BN�BB�B5?B(�B�B{BB
�mB
�
B
��B
��B
ŢB
�9B
��B
�=B
x�B
}�B
x�B
v�B
q�B
k�B
gmB
`BB
YB
M�B
A�B
9XB
(�B
 �B
�B
uB
\B
\B
\B
B	�B	�B	�sB	�`B	�/B	�jB	�B	��B	�bB	�B	z�B	r�B	gmB	`BB	YB	P�B	O�B	N�B	B�B	+B	�B	VB	
=B	B��B��B�B�B�sB�;B��B�jB�FB�'B��B��B��B��B�VB�=B�B� B{�Bz�Bx�Bv�Bs�Br�Bp�Bn�BjBgmBffBcTBaHB^5B^5B^5B]/B\)BZBXBXBYBYBYBYBYBYBXBXBW
BVBVBYBXBW
BT�BQ�BP�BN�BN�BQ�BQ�BR�BP�BM�BK�BJ�BJ�BH�BF�BC�B@�B=qB:^B9XBM�BZBaHBe`BiyBl�Bo�Bp�Br�Bs�Bs�Br�Br�Bq�Bp�Bn�Bk�BhsBe`BdZBcTBdZBe`BiyBl�Bm�Bm�Bm�Bm�Bn�Bo�B{�B}�B}�B}�B|�B~�B�B�B�PB��B��B��B��B�B�!B�-B�?B�XB�XB�XB�dBBĜB��B��B��BǮB��B��B��B��B��B��B��B��B��B��B�B�B�NB�mB�sB�B�B�B��B	B	B	DB	JB	hB	{B	�B	�B	�B	�B	�B	�B	 �B	'�B	,B	0!B	1'B	1'B	1'B	1'B	1'B	33B	6FB	9XB	9XB	?}B	B�B	B�B	C�B	E�B	J�B	K�B	O�B	O�B	P�B	T�B	YB	ZB	ZB	\)B	^5B	_;B	aHB	cTB	e`B	gmB	k�B	q�B	q�B	r�B	s�B	u�B	u�B	v�B	w�B	x�B	�B	�+B	�7B	�=B	�=B	�JB	�VB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�LB	�RB	�XB	�^B	�^B	�jB	�wB	��B	��B	B	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�B	�#B	�/B	�/B	�/B	�)B	�)B	�/B	�;B	�;B	�BB	�;B	�;B	�BB	�HB	�HB	�HB	�TB	�`B	�yB	�sB	�`B	�fB	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
  B
B
,"222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191757                              AO  ARCAADJP                                                                    20181005191757    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191757  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191757  QCF$                G�O�G�O�G�O�8000            