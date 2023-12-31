CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:45Z AOML 3.0 creation; 2016-06-01T00:08:18Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230845  20160531170818  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               MA   AO  4055_7112_077                   2C  D   APEX                            5374                            041511                          846 @������1   @�� ���@:�|�hs�d$�/�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    MA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy��D��3D�\�D�� D�� D��D�S3D��3D���D��D�P D�y�D�� D���D�<�D�vfD��D�  D�@ D�vfD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
=Bq
=By
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D
>D��D�D��D�D��D�D��D�D��D 
D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�
Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt}qDy�>D���D�eD��RD��RD�%D�[�D���D��D�!�D�XRD���D��RD�D�ED�~�D��D�RD�HRD�~�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�5?A�33A�/A�"�A��A��TA��FA���A��-A�33A�-A�$�A��A�  A�t�A�A�ȴA�XA�E�A��;A�ȴA��A��A��#A��9A��\A��+A�-A��/A�XA�7LA���A�A�Q�A�r�A���A���A���A��\A�|�A�S�A��yA���A�1A�(�A�A���A���A�|�A�l�A�$�A��A�G�A�bA���A��jA�~�A�M�A�9XA�9XA�A�A��A�%A��A��-A�-A���A�;dA�
=A��uA�bA��A��A��A�Q�A��HA���A�O�A�+A��A�hsA�33A�+A��A��mA�+A���A���A�1'A�A}`BAz�Az�DAz1Axv�AvbNAt�!Ar�DAq�hAp�Ap9XAm�TAj��Ai�;Ai�Ag�TAf��Af�Ac�TAbI�Ab�Aa�;A`ZA^$�A\5?AY��AX��AX�AV��AUAUO�AT�AS��AQ�^AP�AP^5AO��AN�+AM+AJ�!AH��AHZAH  AG`BAF��AE�AD��AB��AB �AA��AA�A@1'A?�hA>�A>=qA=�-A=
=A<ZA;�^A:�A9�A9A8�/A8��A8jA8JA7��A7+A6�jA6bNA5�
A5A4M�A3\)A2��A2bNA29XA2Q�A2VA1�mA0�A/�mA/�A.�`A.�+A.9XA-�wA-&�A+�TA*r�A);dA(��A'S�A&1A%�7A#��A#S�A#&�A"��A"��A"r�A"n�A"M�A"5?A"(�A!�A!�-A!�hA!S�A!
=A ��A Q�A��AG�A��An�A��A��A-AhsA��A=qA^5AȴA?}AffAhsA�+AA�Ap�AXA��AQ�A��A�wA��AXA
A�A��AĜA�jA��AM�AƨA��AjA��At�A\)A��A�AhsA b@�J@���@�o@���@�$�@���@�l�@���@��@��@���@�V@��j@�1@�`B@�\@�A�@���@���@�^@�%@�D@�t�@���@ߕ�@޸R@��@��@܃@�ƨ@�+@��#@�  @׾w@�33@��T@�p�@�p�@�7L@�r�@���@�9X@���@˥�@���@ʰ!@�{@ɑh@�hs@�O�@��@�Ĝ@ȓu@�I�@��m@Ǯ@�dZ@�
=@ư!@ě�@�@�`B@�?}@�&�@�&�@�%@��@���@���@��9@��u@�bN@�A�@� �@���@��;@�@��D@��@��7@���@��@�n�@�=q@�&�@�1@��@��^@��@�(�@�ƨ@�dZ@��h@�z�@�dZ@��H@���@���@��R@�V@�J@���@��j@�bN@�(�@�b@���@���@�C�@���@�X@�1'@���@�33@��+@���@�hs@�/@��@��/@��D@� �@�  @��F@�
=@���@�$�@�J@�@��#@���@�/@��@��9@� �@��w@�33@��+@�=q@�%@��@��P@��@�@��y@���@��R@���@��+@�5?@���@��@�X@�&�@��j@���@��D@�j@�1'@��;@��P@�+@��H@�V@�5?@�J@���@��T@�@�O�@���@�j@��@��;@���@�ƨ@�ƨ@�ƨ@��F@���@�33@��+@��@��T@��7@�G�@��@�Ĝ@���@�z�@�1'@�  @��F@��@��@���@���@�v�@�V@�E�@�-@��@�@��@��#@��-@���@���@�7L@��j@�r�@�Q�@�Q�@�Q�@�A�@�  @��@K�@~��@~�+@~5?@}�@}��@}�-@}�h@}�h@}O�@}?}@}?}@}V@|�j@|9X@{��@{C�@{"�@{@z�H@z��@z-@y�@w��@w\)@w
=@v��@v5?@s�F@j�@cdZ@]O�@TI�@N��@H �@@  @:^5@2n�@+��@(r�@!G�@��@ȴ@��@~�@@��@�`@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�5?A�33A�/A�"�A��A��TA��FA���A��-A�33A�-A�$�A��A�  A�t�A�A�ȴA�XA�E�A��;A�ȴA��A��A��#A��9A��\A��+A�-A��/A�XA�7LA���A�A�Q�A�r�A���A���A���A��\A�|�A�S�A��yA���A�1A�(�A�A���A���A�|�A�l�A�$�A��A�G�A�bA���A��jA�~�A�M�A�9XA�9XA�A�A��A�%A��A��-A�-A���A�;dA�
=A��uA�bA��A��A��A�Q�A��HA���A�O�A�+A��A�hsA�33A�+A��A��mA�+A���A���A�1'A�A}`BAz�Az�DAz1Axv�AvbNAt�!Ar�DAq�hAp�Ap9XAm�TAj��Ai�;Ai�Ag�TAf��Af�Ac�TAbI�Ab�Aa�;A`ZA^$�A\5?AY��AX��AX�AV��AUAUO�AT�AS��AQ�^AP�AP^5AO��AN�+AM+AJ�!AH��AHZAH  AG`BAF��AE�AD��AB��AB �AA��AA�A@1'A?�hA>�A>=qA=�-A=
=A<ZA;�^A:�A9�A9A8�/A8��A8jA8JA7��A7+A6�jA6bNA5�
A5A4M�A3\)A2��A2bNA29XA2Q�A2VA1�mA0�A/�mA/�A.�`A.�+A.9XA-�wA-&�A+�TA*r�A);dA(��A'S�A&1A%�7A#��A#S�A#&�A"��A"��A"r�A"n�A"M�A"5?A"(�A!�A!�-A!�hA!S�A!
=A ��A Q�A��AG�A��An�A��A��A-AhsA��A=qA^5AȴA?}AffAhsA�+AA�Ap�AXA��AQ�A��A�wA��AXA
A�A��AĜA�jA��AM�AƨA��AjA��At�A\)A��A�AhsA b@�J@���@�o@���@�$�@���@�l�@���@��@��@���@�V@��j@�1@�`B@�\@�A�@���@���@�^@�%@�D@�t�@���@ߕ�@޸R@��@��@܃@�ƨ@�+@��#@�  @׾w@�33@��T@�p�@�p�@�7L@�r�@���@�9X@���@˥�@���@ʰ!@�{@ɑh@�hs@�O�@��@�Ĝ@ȓu@�I�@��m@Ǯ@�dZ@�
=@ư!@ě�@�@�`B@�?}@�&�@�&�@�%@��@���@���@��9@��u@�bN@�A�@� �@���@��;@�@��D@��@��7@���@��@�n�@�=q@�&�@�1@��@��^@��@�(�@�ƨ@�dZ@��h@�z�@�dZ@��H@���@���@��R@�V@�J@���@��j@�bN@�(�@�b@���@���@�C�@���@�X@�1'@���@�33@��+@���@�hs@�/@��@��/@��D@� �@�  @��F@�
=@���@�$�@�J@�@��#@���@�/@��@��9@� �@��w@�33@��+@�=q@�%@��@��P@��@�@��y@���@��R@���@��+@�5?@���@��@�X@�&�@��j@���@��D@�j@�1'@��;@��P@�+@��H@�V@�5?@�J@���@��T@�@�O�@���@�j@��@��;@���@�ƨ@�ƨ@�ƨ@��F@���@�33@��+@��@��T@��7@�G�@��@�Ĝ@���@�z�@�1'@�  @��F@��@��@���@���@�v�@�V@�E�@�-@��@�@��@��#@��-@���@���@�7L@��j@�r�@�Q�@�Q�@�Q�@�A�@�  @��@K�@~��@~�+@~5?@}�@}��@}�-@}�h@}�h@}O�@}?}@}?}@}V@|�j@|9X@{��@{C�@{"�@{@z�H@z��@z-@y�@w��@w\)@w
=@v��@v5?@s�F@j�@cdZ@]O�@TI�@N��@H �@@  @:^5@2n�@+��@(r�@!G�@��@ȴ@��@~�@@��@�`@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�)B�)B�#B�B�B�
B��B��B��BǮBǮBƨBƨBĜB�}B�^B�-B�B��B��B��B��B��B��B��B��B��B��By�Bn�B_;BR�BG�B9XB)�B �B�B�B�B�B{B\B
=BB��B��B��B��B�B�B�mB�B��B��BɺBƨB�}B�^B�XB�dB�qB�^B�^B�LB�B��B��B}�BE�B5?B��BB�B��B��B�JBr�B?}B	7B+B	7B
=B
=B+B
�B
��B
�B
_;B
W
B
B�B
2-B
#�B
 �B
�B
bB
B	�B	�`B	�/B	�B	��B	��B	�B	�B	��B	��B	��B	��B	�7B	�B	�B	�B	~�B	t�B	k�B	aHB	]/B	YB	Q�B	N�B	K�B	G�B	?}B	;dB	<jB	;dB	9XB	5?B	/B	#�B	�B	�B	{B	hB	JB	%B	B��B��B��B��B��B��B�B��B��B��B�B�B�B�B�mB�mB�`B�TB�HB�;B�)B�B�B��B��BɺBƨBĜBĜBƨB��B��B��BɺBŢBĜBB��B�}B�qB�dB�XB�9B�B��B��B��B��B�uB�{B��B��B�{B�uB�uB�uB�uB�oB�hB�bB�\B�PB�JB�DB�1B�%B�B�B}�Bx�Bt�Bq�Bo�Bm�BiyBcTB^5B[#BXBT�BR�BP�BL�BI�BG�BF�BF�BE�BD�BB�B?}B>wB>wB=qB=qB<jB:^B8RB7LB5?B5?B49B1'B-B)�B(�B'�B&�B&�B%�B$�B#�B%�B)�B+B-B/B1'B1'B0!B/B,B'�B#�B �B#�B"�B!�B�B!�B!�B!�B"�B#�B#�B"�B"�B$�B#�B#�B#�B$�B$�B$�B#�B"�B!�B"�B#�B%�B#�B#�B"�B#�B#�B#�B#�B#�B$�B$�B#�B"�B!�B!�B�B �B%�B&�B&�B&�B&�B&�B&�B)�B,B,B,B,B,B,B,B,B,B1'B49B8RB:^B;dB=qB<jB=qB?}BA�BC�BE�BF�BF�BF�BL�BP�BYBXBZB_;BdZBgmBhsBk�Bo�Bs�Bw�Bx�Bz�Bz�B|�B~�B�+B�VB�bB�oB��B��B��B��B��B��B��B��B��B��B�B�B�-B�-B�-B�3B�9B�LB�RB�XB�qB��BĜBɺB��B��B�#B�BB�ZB�ZB�`B�fB�fB�mB�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B	B	%B	+B	1B		7B	
=B	
=B	VB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	&�B	)�B	,B	/B	2-B	33B	6FB	7LB	8RB	;dB	<jB	?}B	@�B	D�B	G�B	H�B	J�B	K�B	L�B	L�B	M�B	N�B	N�B	O�B	Q�B	Q�B	Q�B	T�B	ZB	\)B	]/B	]/B	]/B	^5B	`BB	bNB	cTB	dZB	ffB	hsB	iyB	jB	jB	k�B	k�B	l�B	l�B	l�B	m�B	n�B	p�B	s�B	u�B	v�B	v�B	w�B	x�B	z�B	~�B	�B	�%B	�+B	�7B	�DB	��B	�FB	��B	�`B	��B
+B
oB
 �B
(�B
6FB
?}B
C�B
L�B
Q�B
[#B
^5B
aHB
hsB
k�B
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B��B��B��BʮBǠBǠBƗBƔBċB�oB�JB�B��B��B��B��B��B��B��B��B��B��B�pBy�Bn�B_(BR�BG�B9DB)�B �B�B�B�BwBeBHB
%B �B��B��B��B��B�B�B�VB��B��B��BɢBƔB�fB�JB�?B�LB�XB�GB�IB�5B�B��B�uB}�BE�B5)B��B�wB��B��B�vB�0Br�B?fB	 BB	"B
$B
$BB
��B
�iB
��B
_$B
V�B
B{B
2B
#�B
 �B
�B
QB
 �B	�B	�OB	�B	�B	��B	�wB	�B	��B	��B	��B	��B	��B	�&B	��B	��B	��B	~�B	t�B	kvB	a9B	]!B	Y
B	Q�B	N�B	K�B	G�B	?pB	;XB	<ZB	;UB	9LB	53B	/B	#�B	�B	|B	oB	ZB	?B	B	 B��B��B��B��B��B��B�B��B��B��B�B�B�B�yB�cB�aB�TB�IB�;B�1B�B�B��B��B��BɯBƝBēBēBƛBʸB˾B��BɯBŘBĒBB�wB�tB�fB�ZB�KB�1B�B��B��B��B��B�kB�qB�wB�xB�rB�lB�kB�kB�kB�hB�aB�[B�TB�GB�CB�=B�*B�B�B�B}�Bx�Bt�Bq�Bo�Bm�BipBcLB^-B[BXBT�BR�BP�BL�BI�BG�BF�BF�BE�BD�BB�B?wB>rB>qB=mB=lB<dB:XB8JB7FB5;B58B44B1#B-B)�B(�B'�B&�B&�B%�B$�B#�B%�B)�B*�B-B/B1!B1#B0B/B,B'�B#�B �B#�B"�B!�B�B!�B!�B!�B"�B#�B#�B"�B"�B$�B#�B#�B#�B$�B$�B$�B#�B"�B!�B"�B#�B%�B#�B#�B"�B#�B#�B#�B#�B#�B$�B$�B#�B"�B!�B!�B�B �B%�B&�B&�B&�B&�B&�B&�B)�B+�B,B+�B, B+�B,B,B+�B+�B1 B4/B8HB:UB;[B=gB<`B=hB?sBABC�BE�BF�BF�BF�BL�BP�BYBXBZB_-BdLBgaBhhBkzBo�Bs�Bw�Bx�Bz�Bz�B|�B~�B�B�GB�TB�bB�sB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�#B�+B�>B�AB�FB�aB�rBčBɨBʱB��B�B�0B�FB�FB�LB�SB�RB�[B�\B�eB�vB�B�B�B�B��B��B��B��B��B��B��B	 �B	B	B	B		&B	
(B	
'B	BB	aB	wB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	&�B	)�B	+�B	/B	2B	3B	6/B	75B	8<B	;NB	<VB	?iB	@mB	D�B	G�B	H�B	J�B	K�B	L�B	L�B	M�B	N�B	N�B	O�B	Q�B	Q�B	Q�B	T�B	ZB	\B	]B	]B	]B	^B	`-B	b7B	c;B	dCB	fLB	h[B	icB	jgB	jgB	kmB	koB	lsB	lsB	luB	m|B	n~B	p�B	s�B	u�B	v�B	v�B	w�B	x�B	z�B	~�B	� B	�B	�B	�B	�+B	�mB	�+B	νB	�EB	��B
B
OB
 �B
(�B
6*B
?]B
CyB
L�B
Q�B
[B
^B
a(B
hVB
kgB
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.26 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708182016053117081820160531170818  AO  ARCAADJP                                                                    20140721230845    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230845  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230845  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170818  IP                  G�O�G�O�G�O�                