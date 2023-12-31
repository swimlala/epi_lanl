CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:08Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190608  20181005190608  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @����^#'1   @���l�f@22���m�c��l�C�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B ffB  B  B  B   B(  B0  B8ffB@ffBHffBPffBX  B`  BhffBp  Bx  B��B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C  C  C�fC�fC  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C��3C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C�  D   D y�D ��Dy�D��Dy�D��D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D	��D
� DfD� D��Dy�D  D� DfD�fDfD� D  D� DfD� D  D� D  D�fD  D� D��Dy�D��Dy�D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� DfD�fDfD�fD fD � D!fD!� D!��D"y�D"��D#� D$  D$� D%  D%� D&  D&y�D'  D'� D(  D(y�D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D;��D<� D=fD=� D>fD>� D>��D?y�D?��D@y�D@��DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DIfDI� DJ  DJy�DK  DK�fDLfDL�fDM  DM� DN  DN�fDOfDO� DP  DP� DQ  DQy�DQ��DRy�DR��DSy�DS��DTy�DU  DUy�DU��DV� DW  DWy�DW��DXy�DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Do��Dp� DqfDq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du�fDv  Dvy�Dv��Dwy�Dw��Dy��D�FfD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��A��B�\B	(�B(�B(�B!(�B)(�B1(�B9�\BA�\BI�\BQ�\BY(�Ba(�Bi�\Bq(�By(�B�aHB�aHB��{B��{B��{B��{B��{B��{B�aHB��{B��{B��{B��{B�aHB��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=C0�CJ=CJ=CJ=C0�C0�CJ=CJ=CJ=CJ=C J=C"c�C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CF0�CHJ=CJJ=CLJ=CNJ=CPJ=CRc�CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�RC�RC�RC�%C�1�C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�1�C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�RC�%C�%C�%C�1�C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�RC�RC�RC�RC�%C�%C�RC�RC�RC�%C�%C�%C�%C�RC�%D �D �)D)D�)D)D�)D)D��D�D��D�D��D�D��D�D��D�D��D	�D	�)D
)D
��D�D��D)D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D�)D)D�)D)D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D")D"�)D#)D#��D$�D$��D%�D%��D&�D&�)D'�D'��D(�D(�)D)�D)��D*)D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4)D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<)D<��D=�D=��D>�D>��D?)D?�)D@)D@�)DA)DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH)DH��DI�DI��DJ�DJ�)DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ�)DR)DR�)DS)DS�)DT)DT�)DU�DU�)DV)DV��DW�DW�)DX)DX�)DY)DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg)Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp)Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv�)Dw)Dw�)Dw�\Dy�)D�O�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AȃAȇ+Aȇ+AȅAȇ+AȁAȉ7AȍPAȏ\AȑhAȏ\Aȏ\AȑhAȑhAȑhAȓuAȕ�AȓuAȕ�Aȗ�Aȗ�Aȗ�Aȗ�Aș�Aș�Aț�Aț�Aț�Aț�Aț�AȑhA�O�A�&�A�v�A�n�A��yAá�A�1'AÓuA�A�A�&�A��AA�z�A�%A���A���A��A�?}A��A�;dA��A���A�bA�ƨA��A�+A�ȴA�r�A�1A��A�
=A��wA��\A�bNA��A���A��TA�p�A�G�A�+A��yA��-A��PA��A���A�z�A�A�ZA�Q�A�1A��A�K�A��PA��#A��-A��jA��A���A�  A���A���A�oA�n�A���A��HA��A�-A�  A�bA�ffA�I�A��A�  A��9A���A��A�r�A}&�AzAp�yAi7LAg��Ae�;Ab�A\ȴATE�AQ�
AN�9AL  AH�jAFȴAD�AC33AA��A=��A;��A;?}A:��A9��A8M�A7?}A7
=A6v�A5�;A5��A5|�A5dZA4�jA2��A133A/�A.�RA-/A+"�A*�A)t�A(�HA(��A'��A&JA$��A!�
A �`A   A1A/A�Av�AA�A{A��A|�AhsAl�A�mAffA��AbNA$�A��A��A�+A��A|�AO�A�A�+A�AS�A+AVA�DA�+A=qA  A�;A�-A9XA�A�A33A�A�\AE�A�^Ax�A\)A/A
=A
=A
�9A
�DA
A	�hA	S�A	�TA
(�Av�A��A�!A�A�A�A��A�jA�A^5A|�A   @���@� �@�33@�/@�1@�@��7@��@�t�@��@���@�?}@���@��/@���@�z�@�K�@�\@�=q@�@�@�`B@�p�@�O�@� �@��@�=q@�E�@���@�X@���@�9X@�33@�$�@�hs@�j@�1'@�l�@�
=@�
=@��@�-@��@��@�A�@�33@�J@�O�@�I�@�(�@ۮ@�"�@���@�@�x�@ف@٩�@��T@��@�b@��@�;d@���@��y@�ȴ@ա�@�V@�Ĝ@Լj@ԛ�@ԋD@�1'@�dZ@�;d@���@ҸR@�ff@�V@�-@�{@��@�/@Ь@�z�@�Z@�9X@��m@ϕ�@�K�@�33@��@�
=@Ο�@��#@�p�@��@�Ĝ@�r�@�1'@�;d@��H@�M�@ə�@��@�(�@�t�@��@�=q@��T@ŉ7@�&�@ă@��
@Õ�@�o@§�@�~�@�ff@�V@�=q@�{@���@��h@�7L@��@��u@� �@��F@��@��@�ƨ@��;@���@�1@���@��w@�|�@�33@�ff@��#@�/@��@�S�@�5?@��^@��h@�`B@�V@�Ĝ@��@���@��@�v�@���@�p�@�7L@�V@��9@�1'@��F@���@�|�@���@�=q@��@��-@�p�@�X@�&�@��@�V@�V@�%@���@��j@�Q�@���@�dZ@�dZ@�C�@�+@�
=@���@��\@�E�@�J@��-@�p�@�`B@�7L@���@���@��u@�Z@�A�@�  @�ƨ@�+@��!@�~�@�M�@�{@���@��@��@��`@�r�@��@�S�@�
=@��H@��!@��\@�v�@�=q@�x�@��@��9@��@�1'@���@�t�@��H@��+@�5?@���@�&�@��@��@���@�o@�$�@��@�J@���@��7@��D@�l�@�C�@�C�@�;d@��@��H@���@�v�@�-@��T@��h@�O�@�%@��@���@���@���@���@��@��u@�I�@�1'@�  @�t�@�@��@��y@���@��+@�@�/@���@��@��@���@�r�@�  @���@��6@y�z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AȃAȇ+Aȇ+AȅAȇ+AȁAȉ7AȍPAȏ\AȑhAȏ\Aȏ\AȑhAȑhAȑhAȓuAȕ�AȓuAȕ�Aȗ�Aȗ�Aȗ�Aȗ�Aș�Aș�Aț�Aț�Aț�Aț�Aț�AȑhA�O�A�&�A�v�A�n�A��yAá�A�1'AÓuA�A�A�&�A��AA�z�A�%A���A���A��A�?}A��A�;dA��A���A�bA�ƨA��A�+A�ȴA�r�A�1A��A�
=A��wA��\A�bNA��A���A��TA�p�A�G�A�+A��yA��-A��PA��A���A�z�A�A�ZA�Q�A�1A��A�K�A��PA��#A��-A��jA��A���A�  A���A���A�oA�n�A���A��HA��A�-A�  A�bA�ffA�I�A��A�  A��9A���A��A�r�A}&�AzAp�yAi7LAg��Ae�;Ab�A\ȴATE�AQ�
AN�9AL  AH�jAFȴAD�AC33AA��A=��A;��A;?}A:��A9��A8M�A7?}A7
=A6v�A5�;A5��A5|�A5dZA4�jA2��A133A/�A.�RA-/A+"�A*�A)t�A(�HA(��A'��A&JA$��A!�
A �`A   A1A/A�Av�AA�A{A��A|�AhsAl�A�mAffA��AbNA$�A��A��A�+A��A|�AO�A�A�+A�AS�A+AVA�DA�+A=qA  A�;A�-A9XA�A�A33A�A�\AE�A�^Ax�A\)A/A
=A
=A
�9A
�DA
A	�hA	S�A	�TA
(�Av�A��A�!A�A�A�A��A�jA�A^5A|�A   @���@� �@�33@�/@�1@�@��7@��@�t�@��@���@�?}@���@��/@���@�z�@�K�@�\@�=q@�@�@�`B@�p�@�O�@� �@��@�=q@�E�@���@�X@���@�9X@�33@�$�@�hs@�j@�1'@�l�@�
=@�
=@��@�-@��@��@�A�@�33@�J@�O�@�I�@�(�@ۮ@�"�@���@�@�x�@ف@٩�@��T@��@�b@��@�;d@���@��y@�ȴ@ա�@�V@�Ĝ@Լj@ԛ�@ԋD@�1'@�dZ@�;d@���@ҸR@�ff@�V@�-@�{@��@�/@Ь@�z�@�Z@�9X@��m@ϕ�@�K�@�33@��@�
=@Ο�@��#@�p�@��@�Ĝ@�r�@�1'@�;d@��H@�M�@ə�@��@�(�@�t�@��@�=q@��T@ŉ7@�&�@ă@��
@Õ�@�o@§�@�~�@�ff@�V@�=q@�{@���@��h@�7L@��@��u@� �@��F@��@��@�ƨ@��;@���@�1@���@��w@�|�@�33@�ff@��#@�/@��@�S�@�5?@��^@��h@�`B@�V@�Ĝ@��@���@��@�v�@���@�p�@�7L@�V@��9@�1'@��F@���@�|�@���@�=q@��@��-@�p�@�X@�&�@��@�V@�V@�%@���@��j@�Q�@���@�dZ@�dZ@�C�@�+@�
=@���@��\@�E�@�J@��-@�p�@�`B@�7L@���@���@��u@�Z@�A�@�  @�ƨ@�+@��!@�~�@�M�@�{@���@��@��@��`@�r�@��@�S�@�
=@��H@��!@��\@�v�@�=q@�x�@��@��9@��@�1'@���@�t�@��H@��+@�5?@���@�&�@��@��@���@�o@�$�@��@�J@���@��7@��D@�l�@�C�@�C�@�;d@��@��H@���@�v�@�-@��T@��h@�O�@�%@��@���@���@���@���@��@��u@�I�@�1'@�  @�t�@�@��@��y@���@��+@�@�/@���@��@��@���@�r�@�  @���@��6@y�z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�/B�mB	$�B	�B
��B
�sB
��B
��BoB:^BhsB}�Bx�B�B�7B��B�B�3B�9B��B�{B�B�uB��B��B�wB��B�#B�BB�B+B9XBG�BP�BVB\)Bk�Br�B{�B}�B}�B~�B�B�1B�=B�PB�bB�hB�=B�By�BcTB[#BXBO�BP�BK�B;dB!�B\B��B�B�B�B�fB�B��B�XB��Bp�B_;BP�B:^B�B
�B
�9B
�=B
VB	ĜB	\)B	L�B	'�B	�B	uB	\B		7B��B�BB�B��BȴBǮBƨB��BɺBǮB��B��B��B��B��B�B�B�B�B�
B�
B�
B�
B�B�
B�B��B��B�B��B��B��BɺBǮBƨBŢBŢBĜB�wB�XB�wB�jB�XB�RB�}B��BȴBȴB��B�5B��B�NB��BŢBÖB��B��B��B��B�B�B��B�B�/B�;B�TB�fB�yB��B��B	B	B	uB	0!B	/B	-B	(�B	�B	�B	�B	�B	�B	�B	�B	#�B	-B	.B	/B	33B	5?B	8RB	E�B	J�B	D�B	9XB	6FB	33B	7LB	A�B	E�B	H�B	L�B	M�B	K�B	E�B	>wB	;dB	8RB	2-B	1'B	0!B	33B	5?B	D�B	H�B	J�B	J�B	J�B	P�B	]/B	`BB	cTB	dZB	e`B	gmB	jB	l�B	q�B	s�B	u�B	y�B	� B	�+B	�+B	�1B	�=B	�JB	�oB	��B	��B	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�?B	�^B	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	B	ÖB	ÖB	ŢB	ƨB	ǮB	ȴB	ǮB	ǮB	ƨB	ŢB	ŢB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	ɺB	ȴB	ɺB	ɺB	��B	ɺB	ȴB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�
B	�
B	�
B	�B	�B	�5B	�BB	�NB	�NB	�NB	�TB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
%B
%B
%B
%B
+B
	7B
	7B

=B
	7B
	7B
	7B
1B
1B
	7B
	7B
1B
1B
1B
+B
+B
+B
%B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
PB
\B
\B
VB
PB
PB
PB
PB
PB
JB
JB
PB
�B
�B
%z222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�/B�mB	$�B	�B
��B
�sB
��B
��BoB:^BhsB}�Bx�B�B�7B��B�B�3B�9B��B�{B�B�uB��B��B�wB��B�#B�BB�B+B9XBG�BP�BVB\)Bk�Br�B{�B}�B}�B~�B�B�1B�=B�PB�bB�hB�=B�By�BcTB[#BXBO�BP�BK�B;dB!�B\B��B�B�B�B�fB�B��B�XB��Bp�B_;BP�B:^B�B
�B
�9B
�=B
VB	ĜB	\)B	L�B	'�B	�B	uB	\B		7B��B�BB�B��BȴBǮBƨB��BɺBǮB��B��B��B��B��B�B�B�B�B�
B�
B�
B�
B�B�
B�B��B��B�B��B��B��BɺBǮBƨBŢBŢBĜB�wB�XB�wB�jB�XB�RB�}B��BȴBȴB��B�5B��B�NB��BŢBÖB��B��B��B��B�B�B��B�B�/B�;B�TB�fB�yB��B��B	B	B	uB	0!B	/B	-B	(�B	�B	�B	�B	�B	�B	�B	�B	#�B	-B	.B	/B	33B	5?B	8RB	E�B	J�B	D�B	9XB	6FB	33B	7LB	A�B	E�B	H�B	L�B	M�B	K�B	E�B	>wB	;dB	8RB	2-B	1'B	0!B	33B	5?B	D�B	H�B	J�B	J�B	J�B	P�B	]/B	`BB	cTB	dZB	e`B	gmB	jB	l�B	q�B	s�B	u�B	y�B	� B	�+B	�+B	�1B	�=B	�JB	�oB	��B	��B	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�?B	�^B	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	B	ÖB	ÖB	ŢB	ƨB	ǮB	ȴB	ǮB	ǮB	ƨB	ŢB	ŢB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	ɺB	ȴB	ɺB	ɺB	��B	ɺB	ȴB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�
B	�
B	�
B	�B	�B	�5B	�BB	�NB	�NB	�NB	�TB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
%B
%B
%B
%B
+B
	7B
	7B

=B
	7B
	7B
	7B
1B
1B
	7B
	7B
1B
1B
1B
+B
+B
+B
%B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
PB
\B
\B
VB
PB
PB
PB
PB
PB
JB
JB
PB
�B
�B
%z222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190608                              AO  ARCAADJP                                                                    20181005190608    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190608  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190608  QCF$                G�O�G�O�G�O�8000            