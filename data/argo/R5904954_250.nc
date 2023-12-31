CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:45Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191745  20181005191745  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��䍧R�1   @���""4v@53t�j~��d�z�G�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BW��B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C�C  C�C�C  C�fC   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Ck�fCm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C��C��C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��C��C�  C��fC�  C��C��C��C�  C�  C��3C�  C�  C�  C��C�  C��3C��3C��3C�  C��C��C�  C��3C��3C��3C��C��C�  C��C��C�  C�  C�  C�  C��3C��fC�  C��C�  C�  C��C��C��C�  C�  C��C��C��C��C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D�fDfD�fDfD�fD�D� D  D�fD	  D	y�D
  D
y�D
��D� D�D��D  D� DfD�fDfD�fDfD�fD�D� D  D� D  D� D  D� D  D� DfD��DfD� D  D� D  D� D  D�fD��D�fD��D�fD��D�fD��Dy�DfD� D   D �fD!  D!�fD"fD"y�D#  D#�fD$  D$�fD%fD%� D&  D&� D'fD'� D(  D(y�D(��D)�fD)��D*� D+  D+�fD+�3D,�fD-  D-�fD-��D.y�D/  D/�fD0  D0y�D0��D1� D1��D2� D3  D3� D4  D4� D5  D5� D6fD6� D6��D7� D8  D8� D9  D9� D:  D:�fD;  D;�fD<  D<� D=fD=� D>  D>y�D?  D?� D@  D@y�DAfDA� DB  DB� DB��DC� DC��DD� DE  DE� DF  DFy�DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DOfDO� DP  DP� DQ  DQ�fDR  DR�fDS  DS� DTfDT� DU  DU� DV  DV� DW  DW�fDX  DX�fDY  DYy�DZ  DZ� D[  D[� D[��D\� D\��D]� D^  D^� D^��D_� D`  D`� D`��Da� DbfDb� Dc  Dcs3Dc��Dd� De  De� De��Df� DgfDg�fDg��Dh� Di  Di� DjfDj� DkfDk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do�fDp  Dp� DqfDq� DrfDr� Ds  Dsy�DtfDt� Dt��Duy�DvfDv�fDv��Dwy�DwٚDy�fD�.�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @C�
@�Q�@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=B
=B
=B!
=B)
=B1
=B9
=BA
=BI
=BP��BX��Ba
=Bi
=Bqp�By
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��RB��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�C\)C\)CB�C\)C\)CB�C(�C B�C"B�C$(�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CX(�CZB�C\B�C^(�C`B�CbB�CdB�CfB�ChB�CjB�Cl(�Cn(�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�.C�!HC�!HC�!HC�.C�.C�.C�.C�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�.C�.C�!HC��C�!HC�.C�.C�.C�!HC�!HC�{C�!HC�!HC�!HC�.C�!HC�{C�{C�{C�!HC�.C�.C�!HC�{C�{C�{C�.C�.C�!HC�.C�.C�!HC�!HC�!HC�!HC�{C��C�!HC�.C�!HC�!HC�.C�.C�.C�!HC�!HC�.C�.C�.C�.C�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�.C�!HC�.C�!HC�{C�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�.C�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HD �D ��D�D��D�D��D�D�>D�D�
D
D�
D
D�
DqD��D�D�
D	�D	�>D
�D
�>D
>D��DqD�qD�D��D
D�
D
D�
D
D�
DqD��D�D��D�D��D�D��D�D��D
D�qD
D��D�D��D�D��D�D�
D
>D�
D
>D�
D
>D�
D
>D�>D
D��D �D �
D!�D!�
D"
D"�>D#�D#�
D$�D$�
D%
D%��D&�D&��D'
D'��D(�D(�>D)
>D)�
D*
>D*��D+�D+�
D,�D,�
D-�D-�
D.
>D.�>D/�D/�
D0�D0�>D1
>D1��D2
>D2��D3�D3��D4�D4��D5�D5��D6
D6��D7
>D7��D8�D8��D9�D9��D:�D:�
D;�D;�
D<�D<��D=
D=��D>�D>�>D?�D?��D@�D@�>DA
DA��DB�DB��DC
>DC��DD
>DD��DE�DE��DF�DF�>DG�DG��DH
DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN
>DN��DO
DO��DP�DP��DQ�DQ�
DR�DR�
DS�DS��DT
DT��DU�DU��DV�DV��DW�DW�
DX�DX�
DY�DY�>DZ�DZ��D[�D[��D\
>D\��D]
>D]��D^�D^��D_
>D_��D`�D`��Da
>Da��Db
Db��Dc�Dc��Dd
>Dd��De�De��Df
>Df��Dg
Dg�
Dh
>Dh��Di�Di��Dj
Dj��Dk
Dk��Dl�Dl�
Dm�Dm��Dn�Dn��Do�Do�
Dp�Dp��Dq
Dq��Dr
Dr��Ds�Ds�>Dt
Dt��Du
>Du�>Dv
Dv�
Dw
>Dw�>Dw�>Dy�
D�7
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��`A��A��A��A���A��A�ĜA���A��uA��PA��7A��+A��+A��+A��7A���A��;A�=qA�5?A�VA��HA��RA��A��!A��^A��RA�v�A�=qA�&�A���A�hsA�=qA�"�A��HA�dZA��A���A�A��A��\A�$�A�`BA�^5A�?}A�^5A�oA�A��A��/A���A�1A�ZA�A��A�p�A�A�l�A��/A�5?A�(�A���A���A��A��TA�~�A�A��RA�1A��A�hsA�VA��;A�33A�C�A�G�A��DA�S�A��!A�?}A��
A���A��A���A�S�A�A�A�5?A��`A�ƨA��hA�;dA�bA���A�K�A��RA�  A�$�A��+A���A�5?A��wA�VA���A�jA�
=A��/A�7LA�$�A��A�G�A�  A�9XA�A��PA�O�A��A~�9A}�A|�uAy�;Aw��Au��At��As?}ArI�AqVAoAn�Al�Ak�wAi�Af�Ae`BAcAa��A`��A_��A[�PAW��AS�
AQK�AP  AOt�AMC�AKdZAJ��AJ�yAI�AF��AE�
AC�-AB��AC33AB�yAAhsA>�uA<E�A:�DA9;dA7�A7�A7
=A6bNA5hsA3S�A2  A0�A/��A-`BA,A)�A)7LA)A( �A&�A%��A%/A$�A#��A#oA!C�A ~�A M�A�mA"�A�jAS�A��A��A��A  A��An�AAA�A7LA�
A�jAI�AA��A&�AQ�A�FAv�A�A
�A
��A
ĜAE�A��AĜA�Ap�A��A��A1Ap�A��A{A�PA �9@���@��+@��@�b@�S�@�dZ@�ff@��@�R@�@�1'@��@��@�(�@�ƨ@��@��y@�x�@��@�Q�@���@�P@�l�@�dZ@�\)@�K�@�ȴ@�@�l�@�S�@��@�@�I�@�`B@١�@�O�@���@�b@���@ёh@��H@��@ύP@�|�@�C�@�
=@�~�@���@��@���@�33@Ĵ9@�b@å�@���@�{@��h@�O�@�Q�@���@�hs@��@�hs@�Ĝ@�Q�@�I�@�  @�^5@��m@���@�?}@�@��9@��j@�{@��!@���@�X@�dZ@�n�@�O�@��@��@��F@��@�t�@�S�@�C�@�33@�o@�@�@���@���@��\@��@�\)@���@���@�1'@�C�@�
=@��@���@���@��+@��+@���@���@��;@��@�\)@�\)@�\)@�dZ@�|�@�dZ@���@���@���@��7@�hs@���@��
@��@��@�{@�X@��/@�b@�  @��
@���@�+@��+@�^5@��-@�`B@���@�z�@�1@��m@���@���@�n�@���@��@�`B@�X@���@�r�@�Q�@�A�@�1'@���@��
@���@�^5@��R@���@�V@�V@�M�@���@�&�@�/@�G�@�?}@�/@���@�9X@�|�@�@��@��!@���@�5?@��h@��@��u@�1'@�b@���@��
@�|�@��@�ȴ@�ff@�@��-@��7@��@�hs@�O�@��@�%@��@���@��j@��@��@�A�@��@��@��y@�ff@�J@���@���@��@�r�@�Q�@��
@��@�C�@�
=@���@��!@�=q@�{@���@��7@�G�@�7L@��@��@��@��D@�bN@�1'@�b@��@���@���@�ƨ@��@�|�@�+@��H@�ff@�V@�V@�M�@��@�@�p�@��/@���@��`@��j@� �@��
@��@��;@��
@�S�@�dZ@��@���@�M�@��@���@��h@�p�@�O�@�7L@���@�9X@� �@���@�ƨ@���@���@�dZ@�C�@�]d@sx111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A��A��A��A���A��A�ĜA���A��uA��PA��7A��+A��+A��+A��7A���A��;A�=qA�5?A�VA��HA��RA��A��!A��^A��RA�v�A�=qA�&�A���A�hsA�=qA�"�A��HA�dZA��A���A�A��A��\A�$�A�`BA�^5A�?}A�^5A�oA�A��A��/A���A�1A�ZA�A��A�p�A�A�l�A��/A�5?A�(�A���A���A��A��TA�~�A�A��RA�1A��A�hsA�VA��;A�33A�C�A�G�A��DA�S�A��!A�?}A��
A���A��A���A�S�A�A�A�5?A��`A�ƨA��hA�;dA�bA���A�K�A��RA�  A�$�A��+A���A�5?A��wA�VA���A�jA�
=A��/A�7LA�$�A��A�G�A�  A�9XA�A��PA�O�A��A~�9A}�A|�uAy�;Aw��Au��At��As?}ArI�AqVAoAn�Al�Ak�wAi�Af�Ae`BAcAa��A`��A_��A[�PAW��AS�
AQK�AP  AOt�AMC�AKdZAJ��AJ�yAI�AF��AE�
AC�-AB��AC33AB�yAAhsA>�uA<E�A:�DA9;dA7�A7�A7
=A6bNA5hsA3S�A2  A0�A/��A-`BA,A)�A)7LA)A( �A&�A%��A%/A$�A#��A#oA!C�A ~�A M�A�mA"�A�jAS�A��A��A��A  A��An�AAA�A7LA�
A�jAI�AA��A&�AQ�A�FAv�A�A
�A
��A
ĜAE�A��AĜA�Ap�A��A��A1Ap�A��A{A�PA �9@���@��+@��@�b@�S�@�dZ@�ff@��@�R@�@�1'@��@��@�(�@�ƨ@��@��y@�x�@��@�Q�@���@�P@�l�@�dZ@�\)@�K�@�ȴ@�@�l�@�S�@��@�@�I�@�`B@١�@�O�@���@�b@���@ёh@��H@��@ύP@�|�@�C�@�
=@�~�@���@��@���@�33@Ĵ9@�b@å�@���@�{@��h@�O�@�Q�@���@�hs@��@�hs@�Ĝ@�Q�@�I�@�  @�^5@��m@���@�?}@�@��9@��j@�{@��!@���@�X@�dZ@�n�@�O�@��@��@��F@��@�t�@�S�@�C�@�33@�o@�@�@���@���@��\@��@�\)@���@���@�1'@�C�@�
=@��@���@���@��+@��+@���@���@��;@��@�\)@�\)@�\)@�dZ@�|�@�dZ@���@���@���@��7@�hs@���@��
@��@��@�{@�X@��/@�b@�  @��
@���@�+@��+@�^5@��-@�`B@���@�z�@�1@��m@���@���@�n�@���@��@�`B@�X@���@�r�@�Q�@�A�@�1'@���@��
@���@�^5@��R@���@�V@�V@�M�@���@�&�@�/@�G�@�?}@�/@���@�9X@�|�@�@��@��!@���@�5?@��h@��@��u@�1'@�b@���@��
@�|�@��@�ȴ@�ff@�@��-@��7@��@�hs@�O�@��@�%@��@���@��j@��@��@�A�@��@��@��y@�ff@�J@���@���@��@�r�@�Q�@��
@��@�C�@�
=@���@��!@�=q@�{@���@��7@�G�@�7L@��@��@��@��D@�bN@�1'@�b@��@���@���@�ƨ@��@�|�@�+@��H@�ff@�V@�V@�M�@��@�@�p�@��/@���@��`@��j@� �@��
@��@��;@��
@�S�@�dZ@��@���@�M�@��@���@��h@�p�@�O�@�7L@���@�9X@� �@���@�ƨ@���@���@�dZ@�C�@�]d@sx111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bt�Bt�Bt�Bt�Bt�Bw�B�1B�uB��B��B��B��B��B��B��B�B�;BG�BiyB� B�=B�oB��B��B��B��B��B�'B�?BB��B�)B�;B�HB�TB�sB�yB�yB�B�yB�sB�NB�B��BƨBĜBÖBB��B�wB�LB�3B��B��B��B�hB�hB�{B��B��B��B�{By�BjBhsBR�Bk�Bx�BhsBaHB\)BYBP�B9XB0!B'�B$�B�B�B\B+B��B�B��B�-B��B��B��B��B�hB�JBz�Bp�BhsB[#BF�B,B%�B�BhB
��B
��B
��B
�B
�B
�wB
�B
��B
�\B
p�B
e`B
_;B
]/B
XB
P�B
I�B
B�B
6FB
#�B
�B
+B
  B	�B	�B	�NB	�B	��B	ÖB	�dB	�B	��B	�7B	{�B	r�B	p�B	iyB	C�B	'�B	PB��B��B�B�yB�B�B�sB�TB�B�B�B�)B�`B�B�HB��BƨBĜB��B��B�B�B�
B��B��BȴBĜB�}B�RB�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�VB�JB�7B�%B�B�B�B�B~�B{�By�Bz�Bz�B{�B{�By�Bw�By�Bw�Bx�By�Bx�Bu�Bl�B`BB^5BjBy�B~�B�B�B�B�B�B�B~�B|�Bz�Bp�Bm�Bq�Bw�Bw�Bx�Bz�B{�By�Bu�Bt�Bp�Bq�Bt�Bs�Bt�Bt�Bu�Bv�Bw�Bx�Bx�B~�B�B�B�B�B�1B�1B�7B�7B�DB�VB�VB�JB�=B�1B�%B�B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�XB�dB�^B�dB�jB�RB�3B�^B�wB�3B�3B�RBB��B��BɺBŢB��B�}B��BÖBÖBɺB��B��B��B��B��B��B��B�B�/B�NB�mB�B�B��B��B	DB	JB	PB	\B	bB	hB	oB	�B	�B	�B	�B	"�B	#�B	#�B	#�B	%�B	)�B	0!B	33B	6FB	7LB	7LB	8RB	<jB	@�B	B�B	B�B	E�B	H�B	M�B	P�B	R�B	R�B	S�B	VB	YB	\)B	^5B	`BB	aHB	bNB	cTB	e`B	hsB	iyB	k�B	l�B	l�B	m�B	p�B	r�B	r�B	s�B	t�B	u�B	v�B	v�B	w�B	�B	�B	�B	�B	�+B	�DB	�=B	�PB	�VB	�VB	�VB	�\B	�bB	�hB	�bB	�hB	�oB	�uB	�uB	�hB	�hB	�bB	�hB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�-B	�'B	�!B	�!B	�'B	�'B	�?B	�FB	�RB	�RB	�XB	�dB	�qB	�}B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�BB	�HB	�HB	�HB	�`B	�mB	�sB	�yB	�sB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
�B

222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  Bt�Bt�Bt�Bt�Bt�Bw�B�1B�uB��B��B��B��B��B��B��B�B�;BG�BiyB� B�=B�oB��B��B��B��B��B�'B�?BB��B�)B�;B�HB�TB�sB�yB�yB�B�yB�sB�NB�B��BƨBĜBÖBB��B�wB�LB�3B��B��B��B�hB�hB�{B��B��B��B�{By�BjBhsBR�Bk�Bx�BhsBaHB\)BYBP�B9XB0!B'�B$�B�B�B\B+B��B�B��B�-B��B��B��B��B�hB�JBz�Bp�BhsB[#BF�B,B%�B�BhB
��B
��B
��B
�B
�B
�wB
�B
��B
�\B
p�B
e`B
_;B
]/B
XB
P�B
I�B
B�B
6FB
#�B
�B
+B
  B	�B	�B	�NB	�B	��B	ÖB	�dB	�B	��B	�7B	{�B	r�B	p�B	iyB	C�B	'�B	PB��B��B�B�yB�B�B�sB�TB�B�B�B�)B�`B�B�HB��BƨBĜB��B��B�B�B�
B��B��BȴBĜB�}B�RB�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�VB�JB�7B�%B�B�B�B�B~�B{�By�Bz�Bz�B{�B{�By�Bw�By�Bw�Bx�By�Bx�Bu�Bl�B`BB^5BjBy�B~�B�B�B�B�B�B�B~�B|�Bz�Bp�Bm�Bq�Bw�Bw�Bx�Bz�B{�By�Bu�Bt�Bp�Bq�Bt�Bs�Bt�Bt�Bu�Bv�Bw�Bx�Bx�B~�B�B�B�B�B�1B�1B�7B�7B�DB�VB�VB�JB�=B�1B�%B�B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�XB�dB�^B�dB�jB�RB�3B�^B�wB�3B�3B�RBB��B��BɺBŢB��B�}B��BÖBÖBɺB��B��B��B��B��B��B��B�B�/B�NB�mB�B�B��B��B	DB	JB	PB	\B	bB	hB	oB	�B	�B	�B	�B	"�B	#�B	#�B	#�B	%�B	)�B	0!B	33B	6FB	7LB	7LB	8RB	<jB	@�B	B�B	B�B	E�B	H�B	M�B	P�B	R�B	R�B	S�B	VB	YB	\)B	^5B	`BB	aHB	bNB	cTB	e`B	hsB	iyB	k�B	l�B	l�B	m�B	p�B	r�B	r�B	s�B	t�B	u�B	v�B	v�B	w�B	�B	�B	�B	�B	�+B	�DB	�=B	�PB	�VB	�VB	�VB	�\B	�bB	�hB	�bB	�hB	�oB	�uB	�uB	�hB	�hB	�bB	�hB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�-B	�'B	�!B	�!B	�'B	�'B	�?B	�FB	�RB	�RB	�XB	�dB	�qB	�}B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�BB	�HB	�HB	�HB	�`B	�mB	�sB	�yB	�sB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
�B

222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191745                              AO  ARCAADJP                                                                    20181005191745    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191745  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191745  QCF$                G�O�G�O�G�O�8000            