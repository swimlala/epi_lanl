CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:38Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140838  20181024140838  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @����S�1   @��嗴8�@5�+I��c���+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A^ffA�  A�  A�  A�33A�33A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,�C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cc�fCf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C��3D � D  Dy�D��Dy�D  D� D  D� D  D�fDfD� D  D�fD  D� D	  D	�fD
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� DfD�fDfD�fDfD� D  D� D  D� D  Dy�D  D� D  D�fDfD� D��Dy�D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&�fD'fD'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D-��D.� D.��D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DH��DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DY��DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dc��Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dty�Dt��Du� Dv  Dv� Dw  Dw�fDw�fDy��D�0RD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@�Q�A(�A$(�AD(�Ab�\A�{A�{A�{A�G�A�G�A�{A�{A�G�B
=B	
=B
=B
=B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bqp�Byp�B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B�Q�C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(\)C*B�C,\)C.B�C0B�C2B�C4B�C6B�C8(�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CP(�CRB�CTB�CVB�CXB�CZB�C\\)C^B�C`B�CbB�Cd(�CfB�ChB�Cj\)ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|\)C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�{C�!HC�!HC�.C�!HC�!HC�!HD 
=D ��D�D�>D
>D�>D�D��D�D��D�D�
D
D��D�D�
D�D��D	�D	�
D

D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�>D
>D��D
D�
D
D�
D
D��D�D��D�D��D�D�>D�D��D�D�
D
D��D
>D�>D �D ��D!�D!��D"�D"��D#�D#�>D$�D$��D%�D%��D&�D&�
D'
D'��D(
>D(��D)�D)��D*�D*��D+�D+��D,�D,��D-
D-��D.
>D.��D/
>D/��D0�D0�>D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=�
D>�D>��D?�D?��D@�D@�>DA�DA��DB�DB��DC�DC�>DD�DD��DE�DE��DF�DF��DG�DG��DH
DH��DI
>DI��DJ�DJ��DK�DK�
DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY
DY��DZ
>DZ��D[�D[��D\�D\��D]
D]��D^�D^��D_�D_��D`�D`��Da�Da��Db
>Db��Dc�Dc��Dd
>Dd�>De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�
Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt
>Dt�>Du
>Du��Dv�Dv��Dw�Dw�
Dw�
Dy��D�8�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA���A��A��#A��A��;A��;A��;A��HA��;A��;A��HA��HA��;A��TA��`A��mA��HA��#A�A�~�A�&�A��
A�ĜA�ĜA�ĜA�A�AмjAоwAд9AЋDA�^5A�%A��A���A�^5Aǣ�A�/A��A�XA�
=A��A���A�l�A���A�r�A�ĜA��A��HA�1'A�JA���A���A�&�A�r�A�C�A�t�A��A�7LA�^5A���A���A��-A�`BA��A���A��A��A�C�A���A�x�A��yA�XA�/A�{A��A���A�-A��A�JA�A��wA�dZA���A�n�A�%A�A�;dA���A�A��TA��A���A�7LA���A��#A��A��A�l�A�A�A�"�A��jA�oA�1'A�C�A��+A��;A���A�Q�A�$�A�VA���A���A���A��hA�v�A�VA�"�A�JAC�A{�-Ay\)Ax�RAsp�AnffAkS�Ah��Af�jAe�AdE�Ab��A`�yA^{A]S�A[�AY��AXz�AU�^AS��AQ��APffAP�AN�yANE�AMp�AKhsAIXAG�;AEƨAA`BA@~�A>�A=&�A;
=A9�FA8�A7O�A6(�A4�A41A29XA/t�A.bA-�PA-�A,9XA+K�A*=qA'XA&bA%�A%�mA%��A&JA&1A&  A%��A%7LA$��A$bNA"ZA"(�A!XA�
A/A��Az�A=qA��A�-A�-AG�A%A�jA�-A�uA�A��A5?A	��A�AffA$�A�TA�#A�^Ap�A��A��AjA�;A�A�9AJA�A��A�RAQ�A��A �/@���@��\@��+@�~�@�$�@�`B@�t�@��@�t�@��T@���@�@��#@�9X@�V@�9@�@�33@�$�@�r�@畁@�
=@��H@���@�!@�V@�Ĝ@��m@�C�@��@��H@��@�R@◍@�V@�&�@� �@�C�@��@��/@�Q�@ۥ�@�V@�dZ@�n�@�$�@��@��#@�&�@Ԭ@�Z@���@ҏ\@�J@Ѻ^@�X@�G�@��@�I�@��m@�K�@Ο�@�V@�G�@�I�@�(�@� �@�  @���@ˍP@�M�@�r�@�j@�z�@Ȭ@��@�l�@�ȴ@�M�@���@��
@��@¸R@�V@��@�@���@�`B@��/@�9X@��w@��F@��@���@��@���@�ff@�{@���@��T@���@�x�@�7L@���@�\)@�;d@�ȴ@�v�@��#@�X@��@���@��P@�@�-@�  @��h@��@��j@���@��@�I�@�1'@�  @��m@�l�@�33@�@��!@��\@�ff@�-@�@�?}@���@���@���@�"�@��y@��H@���@�E�@��T@���@�7L@��@��9@�Q�@��P@��H@�ff@�O�@��9@��u@�Q�@�
=@�M�@��@�p�@�V@��/@��9@��u@��@�1'@��@�b@��@��w@���@�t�@�l�@�C�@�"�@�@��H@��R@��+@�^5@�-@�@���@��@�G�@�V@��`@���@�bN@�1'@�1@��m@��@��y@��@���@�r�@�A�@�9X@�I�@�9X@�  @���@��w@��P@�;d@��H@�-@��#@��h@�X@���@��/@���@��j@��u@��@�C�@��H@��\@�{@��#@��^@���@��@�j@� �@��;@��;@��;@��@�C�@��R@�5?@��@�p�@��T@���@���@��@�V@�%@���@��@�bN@�Q�@� �@��
@���@���@�t�@�\)@�S�@�S�@�C�@�"�@��y@�=q@�@��-@���@�p�@�G�@�7L@�?}@�?}@�?}@�G�@�O�@�X@�`B@�`B@�hs@�O�@�?}@��/@w�@@gj�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ƨA���A��A��#A��A��;A��;A��;A��HA��;A��;A��HA��HA��;A��TA��`A��mA��HA��#A�A�~�A�&�A��
A�ĜA�ĜA�ĜA�A�AмjAоwAд9AЋDA�^5A�%A��A���A�^5Aǣ�A�/A��A�XA�
=A��A���A�l�A���A�r�A�ĜA��A��HA�1'A�JA���A���A�&�A�r�A�C�A�t�A��A�7LA�^5A���A���A��-A�`BA��A���A��A��A�C�A���A�x�A��yA�XA�/A�{A��A���A�-A��A�JA�A��wA�dZA���A�n�A�%A�A�;dA���A�A��TA��A���A�7LA���A��#A��A��A�l�A�A�A�"�A��jA�oA�1'A�C�A��+A��;A���A�Q�A�$�A�VA���A���A���A��hA�v�A�VA�"�A�JAC�A{�-Ay\)Ax�RAsp�AnffAkS�Ah��Af�jAe�AdE�Ab��A`�yA^{A]S�A[�AY��AXz�AU�^AS��AQ��APffAP�AN�yANE�AMp�AKhsAIXAG�;AEƨAA`BA@~�A>�A=&�A;
=A9�FA8�A7O�A6(�A4�A41A29XA/t�A.bA-�PA-�A,9XA+K�A*=qA'XA&bA%�A%�mA%��A&JA&1A&  A%��A%7LA$��A$bNA"ZA"(�A!XA�
A/A��Az�A=qA��A�-A�-AG�A%A�jA�-A�uA�A��A5?A	��A�AffA$�A�TA�#A�^Ap�A��A��AjA�;A�A�9AJA�A��A�RAQ�A��A �/@���@��\@��+@�~�@�$�@�`B@�t�@��@�t�@��T@���@�@��#@�9X@�V@�9@�@�33@�$�@�r�@畁@�
=@��H@���@�!@�V@�Ĝ@��m@�C�@��@��H@��@�R@◍@�V@�&�@� �@�C�@��@��/@�Q�@ۥ�@�V@�dZ@�n�@�$�@��@��#@�&�@Ԭ@�Z@���@ҏ\@�J@Ѻ^@�X@�G�@��@�I�@��m@�K�@Ο�@�V@�G�@�I�@�(�@� �@�  @���@ˍP@�M�@�r�@�j@�z�@Ȭ@��@�l�@�ȴ@�M�@���@��
@��@¸R@�V@��@�@���@�`B@��/@�9X@��w@��F@��@���@��@���@�ff@�{@���@��T@���@�x�@�7L@���@�\)@�;d@�ȴ@�v�@��#@�X@��@���@��P@�@�-@�  @��h@��@��j@���@��@�I�@�1'@�  @��m@�l�@�33@�@��!@��\@�ff@�-@�@�?}@���@���@���@�"�@��y@��H@���@�E�@��T@���@�7L@��@��9@�Q�@��P@��H@�ff@�O�@��9@��u@�Q�@�
=@�M�@��@�p�@�V@��/@��9@��u@��@�1'@��@�b@��@��w@���@�t�@�l�@�C�@�"�@�@��H@��R@��+@�^5@�-@�@���@��@�G�@�V@��`@���@�bN@�1'@�1@��m@��@��y@��@���@�r�@�A�@�9X@�I�@�9X@�  @���@��w@��P@�;d@��H@�-@��#@��h@�X@���@��/@���@��j@��u@��@�C�@��H@��\@�{@��#@��^@���@��@�j@� �@��;@��;@��;@��@�C�@��R@�5?@��@�p�@��T@���@���@��@�V@�%@���@��@�bN@�Q�@� �@��
@���@���@�t�@�\)@�S�@�S�@�C�@�"�@��y@�=q@�@��-@���@�p�@�G�@�7L@�?}@�?}@�?}@�G�@�O�@�X@�`B@�`B@�hs@�O�@�?}@��/@w�@@gj�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BL�BK�BK�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BM�BN�BT�B`BBn�B~�B�%B�1B�7B�=B�JB�oB��B��B��B��B�!B�HB
=B�B.B/B8RB@�BN�BXBXBYBYBcTBgmBk�Bq�Bt�B� B�+B�1B�=B�PB�VB��B��B��B��B��B��B��B��B��B��B�{B�\B�7B~�Bv�BjBS�B5?B$�BDB�B�HB��B��B�B��B�BO�B@�BE�BJ�BJ�B<jB+B�BPBB
��B
��B
��B
��B
�B
�B
�B
�B
�TB
ƨB
��B
|�B
jB
aHB
]/B
YB
W
B
W
B
W
B
T�B
T�B
VB
T�B
S�B
R�B
S�B
Q�B
@�B
-B
"�B	��B	�fB	��B	�dB	�9B	�3B	��B	��B	�JB	w�B	p�B	e`B	W
B	K�B	>wB	49B	)�B	#�B	 �B	�B	�B	\B	B�B�sB�;BɺBÖB�jB�FB�'B�'B�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�bB�VB�JB�B}�By�Bu�Bs�Br�Bo�Bq�Bv�Bz�B|�B|�B�B�B�%B�%B�+B�=B�=B�=B�1B�=B�PB�VB�VB�VB�VB�\B�oB�oB�oB�oB�hB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�'B�3B�^B�jB�qB�qB�qB�}B��B��B��BŢBŢBƨBƨBƨBƨBǮBǮBȴB��B��B��B��B��B��B��B��B��B��B�
B�#B�/B�;B�sB�B�B�B��B��B	B	B	B	1B		7B		7B	DB	VB	hB	uB	uB	uB	uB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	(�B	/B	1'B	5?B	8RB	:^B	=qB	C�B	E�B	H�B	S�B	`BB	cTB	dZB	e`B	e`B	ffB	ffB	gmB	hsB	k�B	l�B	m�B	n�B	o�B	o�B	p�B	q�B	s�B	u�B	x�B	{�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�1B	�7B	�=B	�7B	�1B	�1B	�1B	�1B	�=B	�JB	�PB	�VB	�VB	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�-B	�-B	�-B	�'B	�'B	�?B	�LB	�RB	�XB	�dB	�qB	�wB	B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�/B	�HB	�TB	�`B	�mB	�mB	�mB	�mB	�fB	�`B	�fB	�mB	�mB	�mB	�mB	�fB	�`B	�TB	�TB	�`B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
fB
	111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BL�BK�BK�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BM�BN�BT�B`BBn�B~�B�%B�1B�7B�=B�JB�oB��B��B��B��B�!B�HB
=B�B.B/B8RB@�BN�BXBXBYBYBcTBgmBk�Bq�Bt�B� B�+B�1B�=B�PB�VB��B��B��B��B��B��B��B��B��B��B�{B�\B�7B~�Bv�BjBS�B5?B$�BDB�B�HB��B��B�B��B�BO�B@�BE�BJ�BJ�B<jB+B�BPBB
��B
��B
��B
��B
�B
�B
�B
�B
�TB
ƨB
��B
|�B
jB
aHB
]/B
YB
W
B
W
B
W
B
T�B
T�B
VB
T�B
S�B
R�B
S�B
Q�B
@�B
-B
"�B	��B	�fB	��B	�dB	�9B	�3B	��B	��B	�JB	w�B	p�B	e`B	W
B	K�B	>wB	49B	)�B	#�B	 �B	�B	�B	\B	B�B�sB�;BɺBÖB�jB�FB�'B�'B�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�bB�VB�JB�B}�By�Bu�Bs�Br�Bo�Bq�Bv�Bz�B|�B|�B�B�B�%B�%B�+B�=B�=B�=B�1B�=B�PB�VB�VB�VB�VB�\B�oB�oB�oB�oB�hB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�'B�3B�^B�jB�qB�qB�qB�}B��B��B��BŢBŢBƨBƨBƨBƨBǮBǮBȴB��B��B��B��B��B��B��B��B��B��B�
B�#B�/B�;B�sB�B�B�B��B��B	B	B	B	1B		7B		7B	DB	VB	hB	uB	uB	uB	uB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	(�B	/B	1'B	5?B	8RB	:^B	=qB	C�B	E�B	H�B	S�B	`BB	cTB	dZB	e`B	e`B	ffB	ffB	gmB	hsB	k�B	l�B	m�B	n�B	o�B	o�B	p�B	q�B	s�B	u�B	x�B	{�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�1B	�7B	�=B	�7B	�1B	�1B	�1B	�1B	�=B	�JB	�PB	�VB	�VB	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�-B	�-B	�-B	�'B	�'B	�?B	�LB	�RB	�XB	�dB	�qB	�wB	B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�/B	�HB	�TB	�`B	�mB	�mB	�mB	�mB	�fB	�`B	�fB	�mB	�mB	�mB	�mB	�fB	�`B	�TB	�TB	�`B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
fB
	111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140838                              AO  ARCAADJP                                                                    20181024140838    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140838  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140838  QCF$                G�O�G�O�G�O�0               