CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:24Z AOML 3.0 creation; 2016-06-01T00:08:11Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230824  20160531170811  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               %A   AO  4055_7112_037                   2C  D   APEX                            5374                            041511                          846 @։�ޠ 1   @։�v���@:t�j~��c01&�x�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    %A   A   A   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D��D�,�D�i�D���D���D�@ D��3D���D�  D�@ D���D�� D��D�FfDړ3D��fD�3D�C3D�ffD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A(�A$(�AB�\Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�Cd\)CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3
D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI�>DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D�!�D�5D�q�D��D��D�HRD���D��D�RD�HRD���D��RD�D�N�Dڛ�D�޸D��D�K�D�n�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA���A���A���A���A���A��
A���A���A��
A���A�ƨA�ƨA�ȴA���A�A���A��jA��^A��9A��9A��FA��FA��RA��jA���A���A�A�A�A�A���A��jA��^A��^A��^A��RA��9A��9A��9A��9A��-A���A���A��A�K�A��A�$�A�v�A��/A��#A��A���A��7A���A���A��9A�^5A� �A��/A�"�A��A���A�~�A�|�A�A�A���A��A��7A�5?A��mA�ffA���A�A�oA�G�A�%A�
=A��/A���A��A�C�A��jA��A�ZA��
A��DA�7LA�~�A�G�A��wA�O�A��A���A�-A�JA���A��A���A��A�M�A��TA�E�A��/A��wA���A��\A�t�A�ĜA���A�7LA��A~�A}|�A|ZAx��Av�AsC�AnȴAj�Ahr�Ag
=Ad�Ab~�Aa+A`E�A^A]7LA\-AZ�`AYC�AW�AVbNAT�jAR��AQ��APQ�AO/AM"�AK�mAJ��AI
=AF�AE+ADVACdZAB�`ABI�AA33A?A?oA=��A=�A<  A:�DA8��A7�7A6�/A6�uA6~�A6I�A5�A4M�A3�A2�jA2  A1
=A0�+A/�#A/
=A.ZA-�;A,�A, �A+33A*=qA)�A(��A(9XA'"�A&9XA$�uA#�;A"��A"$�A!�A �A bNA�-A��A��A��A�#A�`A$�AO�A��A��A��A��A��A�
A�A�A^5A��A�AAC�A
z�A	��A	XA�yA�TAS�Az�A�TA�A�A�mA�/A��AJAO�A
=A V@��@�J@��7@���@��
@��+@���@�?}@��P@�ff@�p�@�+@�9@��@��`@�@��T@�  @��@�X@�(�@�@�%@�@�(�@�l�@�n�@���@۾w@�o@�E�@��T@ٲ-@�p�@ش9@��@�V@Դ9@� �@�
=@�{@�G�@�A�@�S�@�-@̛�@��m@���@�&�@ȋD@�  @�M�@�hs@���@�1'@��H@���@�ƨ@��@��@� �@�"�@�o@���@�J@��@�9X@�@�J@��j@��D@�  @�K�@�5?@���@�7L@�Q�@���@�|�@�@��+@��@�V@���@�Q�@��w@�
=@��\@�?}@��@��@�+@�E�@��^@���@��9@�r�@��@�
=@���@��#@�?}@���@��9@�z�@�A�@��m@�t�@�
=@�-@�?}@��@�;d@��@���@�-@��T@���@�V@�r�@�9X@�(�@�b@���@��P@�
=@�ff@�E�@��@�/@�j@�1'@���@��;@���@�;d@��R@�ff@��@�X@�/@���@��@���@�33@��y@���@�ff@�^5@�V@�@���@��@�hs@�?}@�V@���@�I�@��@��F@�;d@�S�@�
=@�v�@���@��^@�hs@�O�@�/@�%@���@�z�@�9X@�b@��
@���@���@���@�t�@�33@��@���@�V@�{@��@�{@��@�@��7@�7L@�%@���@��@��D@�j@�Q�@�1'@��@���@�dZ@�"�@�@�ȴ@���@��+@�v�@�5?@�@��@���@�@���@�p�@��@���@��/@���@��D@�r�@�Z@��@�@K�@~��@~��@~�+@~{@}/@|�j@|j@|1@{t�@{C�@{S�@{t�@{@y�#@y7L@y%@x��@xA�@xb@w�;@w�w@w�@w��@wl�@wK�@w;d@w
=@v�R@u�T@uO�@u�@t�/@tj@tI�@t9X@sƨ@sC�@r�H@r~�@n�+@f�@]��@YX@S@L��@F�R@?�P@:~�@4j@/\)@*�@&5?@!�^@V@�@I�@1'@9X@�`@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ƨA���A���A���A���A���A��
A���A���A��
A���A�ƨA�ƨA�ȴA���A�A���A��jA��^A��9A��9A��FA��FA��RA��jA���A���A�A�A�A�A���A��jA��^A��^A��^A��RA��9A��9A��9A��9A��-A���A���A��A�K�A��A�$�A�v�A��/A��#A��A���A��7A���A���A��9A�^5A� �A��/A�"�A��A���A�~�A�|�A�A�A���A��A��7A�5?A��mA�ffA���A�A�oA�G�A�%A�
=A��/A���A��A�C�A��jA��A�ZA��
A��DA�7LA�~�A�G�A��wA�O�A��A���A�-A�JA���A��A���A��A�M�A��TA�E�A��/A��wA���A��\A�t�A�ĜA���A�7LA��A~�A}|�A|ZAx��Av�AsC�AnȴAj�Ahr�Ag
=Ad�Ab~�Aa+A`E�A^A]7LA\-AZ�`AYC�AW�AVbNAT�jAR��AQ��APQ�AO/AM"�AK�mAJ��AI
=AF�AE+ADVACdZAB�`ABI�AA33A?A?oA=��A=�A<  A:�DA8��A7�7A6�/A6�uA6~�A6I�A5�A4M�A3�A2�jA2  A1
=A0�+A/�#A/
=A.ZA-�;A,�A, �A+33A*=qA)�A(��A(9XA'"�A&9XA$�uA#�;A"��A"$�A!�A �A bNA�-A��A��A��A�#A�`A$�AO�A��A��A��A��A��A�
A�A�A^5A��A�AAC�A
z�A	��A	XA�yA�TAS�Az�A�TA�A�A�mA�/A��AJAO�A
=A V@��@�J@��7@���@��
@��+@���@�?}@��P@�ff@�p�@�+@�9@��@��`@�@��T@�  @��@�X@�(�@�@�%@�@�(�@�l�@�n�@���@۾w@�o@�E�@��T@ٲ-@�p�@ش9@��@�V@Դ9@� �@�
=@�{@�G�@�A�@�S�@�-@̛�@��m@���@�&�@ȋD@�  @�M�@�hs@���@�1'@��H@���@�ƨ@��@��@� �@�"�@�o@���@�J@��@�9X@�@�J@��j@��D@�  @�K�@�5?@���@�7L@�Q�@���@�|�@�@��+@��@�V@���@�Q�@��w@�
=@��\@�?}@��@��@�+@�E�@��^@���@��9@�r�@��@�
=@���@��#@�?}@���@��9@�z�@�A�@��m@�t�@�
=@�-@�?}@��@�;d@��@���@�-@��T@���@�V@�r�@�9X@�(�@�b@���@��P@�
=@�ff@�E�@��@�/@�j@�1'@���@��;@���@�;d@��R@�ff@��@�X@�/@���@��@���@�33@��y@���@�ff@�^5@�V@�@���@��@�hs@�?}@�V@���@�I�@��@��F@�;d@�S�@�
=@�v�@���@��^@�hs@�O�@�/@�%@���@�z�@�9X@�b@��
@���@���@���@�t�@�33@��@���@�V@�{@��@�{@��@�@��7@�7L@�%@���@��@��D@�j@�Q�@�1'@��@���@�dZ@�"�@�@�ȴ@���@��+@�v�@�5?@�@��@���@�@���@�p�@��@���@��/@���@��D@�r�@�Z@��@�@K�@~��@~��@~�+@~{@}/@|�j@|j@|1@{t�@{C�@{S�@{t�@{@y�#@y7L@y%@x��@xA�@xb@w�;@w�w@w�@w��@wl�@wK�@w;d@w
=@v�R@u�T@uO�@u�@t�/@tj@tI�@t9X@sƨ@sC�@r�H@r~�@n�+@f�@]��@YX@S@L��@F�R@?�P@:~�@4j@/\)@*�@&5?@!�^@V@�@I�@1'@9X@�`@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BǮB��BɺBÖBƨBĜB�XB�-B��B�PBz�Bl�B=qBB�B�BPB��B��B�B�sB�)B��B��B��B�B|�Be`B8RB�B{BVBB��B�mB�/B��BɺB�?B��B��B�PB{�BdZBG�B49B33B)�B�B
��B
�mB
��B
ÖB
B
�'B
��B
��B
�B
u�B
XB
<jB
)�B
�B
�B
DB	�B	�)B	ĜB	��B	�hB	�B	x�B	k�B	`BB	YB	S�B	I�B	D�B	=qB	33B	(�B	%�B	 �B	�B	\B	JB	1B	B��B��B�B�yB�HB�#B�B��B��B��BȴBÖB��B�}B�jB�^B�LB�?B�9B�9B�9B�3B�3B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�=B�%B~�By�Bu�Br�Bo�Bm�Bl�BiyBffBdZBcTBaHB^5B]/B\)BYBQ�BK�BG�BB�B@�B?}B:^B;dB;dB8RB33B.B)�B'�B%�B#�B!�B �B�B�B�B�B�B�B�B�B�BuBoBhBhBbB\BVBVBPBJBDBDB	7B1B+B1B+B%BBBBBBB%BBBBB%B+B+B	7B	7B	7B1B+B	7B
=B
=BDBDBDBDBPBPBDBJBJBVBhBhBhB�B�B�B�B�B�B!�B"�B#�B'�B)�B)�B)�B+B-B.B1'B33B7LB6FB8RB:^B>wB?}B?}B?}B?}BA�BC�BF�BH�BH�BJ�BL�BO�BQ�BQ�BVBXBYB[#B]/B_;BaHBbNBbNBdZBiyBjBn�Br�Bs�Bu�Bv�Bw�By�B{�B}�B�B�+B�DB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�LB�jB�qB�}B��BBŢBȴB��B��B��B��B�B�B�#B�;B�NB�ZB�fB�fB�fB�sB�B�B�B�B�B��B��B��B��B��B��B��B	B	B	+B	
=B	DB	JB	PB	\B	oB	�B	�B	�B	�B	�B	 �B	"�B	$�B	'�B	)�B	+B	-B	.B	.B	0!B	2-B	49B	6FB	7LB	9XB	;dB	<jB	=qB	?}B	A�B	C�B	F�B	I�B	K�B	M�B	O�B	Q�B	R�B	R�B	VB	XB	ZB	ZB	ZB	]/B	_;B	cTB	dZB	dZB	gmB	hsB	iyB	iyB	k�B	m�B	o�B	q�B	u�B	x�B	z�B	{�B	|�B	~�B	�B	�B	�B	�B	�+B	�1B	�=B	�PB	�PB	�VB	�hB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�qB	�B	�B	��B
DB
�B
!�B
.B
6FB
<jB
D�B
J�B
O�B
VB
[#B
aHB
e`B
iyB
m�B
q�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǝB��BɪBÅBƖBčB�GB�B��B�>Bz�BlwB=]BB~B�B<B��B��B�B�]B�B��B�qB�|B�B|�BeGB89B�BdB=B�B��B�TB�B��BɣB�&B��B�wB�8B{�BdBBG�B4"B3B)�BnB
��B
�VB
ͻB
�~B
�wB
�B
��B
�B
�
B
u�B
W�B
<WB
)�B
�B
qB
3B	�B	�B	ČB	��B	�YB	��B	x�B	kwB	`5B	YB	S�B	I�B	D�B	=cB	3$B	(�B	%�B	 �B	�B	SB	>B	'B	B��B��B�B�oB�=B�B�B��B��B��BȫBÍB�B�tB�_B�UB�CB�6B�0B�0B�1B�+B�)B�#B�B�B�B��B��B��B��B��B��B��B��B��B��B�B�rB�gB�SB�5B�B~�By�Bu�Br�Bo�Bm�Bl�BirBf`BdRBcMBa@B^-B]'B\#BYBQ�BK�BG�BB�B@~B?yB:XB;]B;]B8MB3-B.B)�B'�B%�B#�B!�B �B�B�B�B�B{BtBnBhB}BqBhBHBGBBBVB6B3B/B+B=B"B	BB
BB
BB�B�B�B�B�B�BB�BB�B�BB%B	B	B	B	1BB$B	B
B
B>B=B?B>BJB.BBEBEB2BGBGBEBzB�B�B�B�B�B!�B"�B#�B'�B)�B)�B)�B*�B-B.
B1B3+B7BB6<B8JB:TB>nB?sB?qB?vB?sBA}BC�BF�BH�BH�BJ�BL�BO�BQ�BQ�BU�BXBY
B[B]#B_0Ba=BbBBbDBdMBinBjrBn�Br�Bs�Bu�Bv�Bw�By�B{�B}�B��B�B�5B�hB�sB�yB��B��B��B��B��B��B��B��B��B��B��B�B�B�"B�=B�[B�aB�lB�nBBŏBȣB˵B��B��B��B��B�B�B�(B�<B�GB�UB�UB�UB�`B�tB�B�B�B�B��B��B��B��B��B��B��B	 �B	
B	B	
+B	0B	6B	:B	GB	\B	mB	yB	�B	�B	�B	 �B	"�B	$�B	'�B	)�B	*�B	,�B	. B	-�B	0B	2B	4$B	61B	78B	9BB	;PB	<TB	=ZB	?hB	AsB	C�B	F�B	I�B	K�B	M�B	O�B	Q�B	R�B	R�B	U�B	W�B	ZB	ZB	ZB	]B	_%B	c=B	dDB	d@B	gUB	h\B	iaB	i`B	knB	mxB	o�B	q�B	u�B	x�B	z�B	{�B	|�B	~�B	��B	��B	��B	�B	�B	�B	�&B	�7B	�9B	�>B	�MB	�\B	�dB	�bB	�dB	�dB	�mB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�XB	��B	�B	��B
'B
rB
!�B
-�B
6(B
<NB
DB
J�B
O�B
U�B
[B
a)B
eCB
i[B
mrB
q�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.26 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708112016053117081120160531170811  AO  ARCAADJP                                                                    20140721230824    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230824  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230824  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170811  IP                  G�O�G�O�G�O�                