CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:52Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               1A   AO  20111130144008  20190522121829  1728_5048_049                   2C  D   APEX                            2142                            040306                          846 @Ԥ�p?�1   @Ԥ����@5��x����c�����1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  @���A   A@  A`  A~ffA�33A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DyffD��D�6fD�y�D��3D�	�D�6fD�|�D���D�	�D�,�D�vfDǣ3D��fD�,�D�\�D���D��fD�#3D�VfD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�ff@�33A33A;33A[33Ay��A���A���A���A�ffA͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB���BÙ�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C��C	�3C�3C�3C�3C�3C�3C�3C�3C��C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC��CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��DffD��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM�3DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�DyS3D� D�,�D�p D���D�  D�,�D�s3D�� D�  D�#3D�l�DǙ�D���D�#3D�S3D�� D���D��D�L�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�\)A�\)A�bNA�bNA�n�A�n�A�n�A�n�A�p�A�p�A�r�A�r�A�t�A�t�A�t�A�t�A�v�A�v�A�x�A�x�A�z�A�z�A�|�A�z�A�z�A�z�A�|�A�|�A�z�A�x�A�v�A�v�A�r�A�r�A�l�A�M�A���Aƴ9A�?}A�"�A�VA�1'A�O�A��A��jA��;A��\A���A� �A��RA�v�A�S�A��\A���A�ffA��A�`BA��-A�^5A��!A�t�A�XA��#A���A��+A�;dA�XA��A��A�$�A���A��A�C�A�ȴA�C�A��9A�XA��HA�9XA���A�G�A�dZA�bNA���A�Q�A�%A��A���A���A���A�G�A�?}A���A���A���A�/A���A��A���A�r�A��!A���A�ȴA��!A�l�A~�\A|M�AyVAtVAq��Aox�AnffAlȴAj�jAg��AfĜAfr�Adz�AcXAal�A_�-A^��A^��A^z�A\z�AZ�`AY"�AW�mAV��AU|�ASl�AQhsAO�AM
=AK�7AJbAH�`AH�\AG�^AF�/AE�mAD�yAB�AA�hA@r�A?��A>��A>�A=ƨA=�A;`BA9��A7ƨA4��A3�PA2�A0�!A/��A.M�A,�!A+x�A*~�A)7LA'dZA&  A%O�A$E�A$A$�/A$(�A#�A"��A!��A �A�A�AoA��A�9A�uA�AI�A`BA��A�
A�!A��AhsA��A�Az�A�^AA�mA��A-A��A�^A
5?A��A��A�/AI�A�A �A?}A�+A �A -@�-@��D@�7L@���@�G�@�ff@�?}@��;@�{@��@�9@���@���@�$�@���@�h@�z�@�!@�7@�7L@߶F@�-@�K�@ٺ^@ؼj@�1'@��@�b@�  @ם�@և+@Դ9@���@�C�@�@��@��@ӕ�@�1'@�r�@�b@�dZ@��H@ҏ\@��@�hs@�  @�33@·+@�`B@˅@���@ȼj@��;@��@�~�@�@ļj@�@��u@�ƨ@�+@�hs@�
=@�5?@��@��j@��F@��H@��@��@���@�o@�33@��@���@���@�b@���@�+@���@�$�@�j@��
@�9X@��@���@�G�@�E�@���@�^5@�ff@�v�@�n�@�O�@���@�O�@�bN@���@��@�(�@��m@�(�@��P@�V@�r�@�=q@��9@�V@��@�?}@��@�|�@��@��^@�5?@��@�G�@�;d@���@�S�@�S�@���@��#@�/@�j@�+@�O�@�`B@���@�-@�V@�@�Z@��H@��#@�@�x�@�G�@�p�@�/@�%@��`@��9@���@�V@��@�/@�%@�Ĝ@��u@�Z@� �@���@�t�@�+@��H@��\@�$�@���@��@�j@� �@��
@��@�+@��H@��R@���@���@��+@�^5@�{@��@�@��@��#@��-@�?}@�/@�/@�/@�V@���@���@���@�j@�1@�ƨ@���@��@�\)@�33@��@��\@�5?@���@�Ĝ@�(�@�9X@�9X@��@�  @��;@��P@�33@�M�@��7@�hs@��7@��@��7@��h@��h@�`B@���@�I�@���@�K�@�33@��!@�$�@���@���@��@��@��#@��7@�?}@�?}@�G�@�X@�`B@�X@�O�@�O�@�`B@�p�@�p�@�p�@�`B@�O�@��@��`@���@�Z@�Q�@�I�@�I�@�1@���@���@���@���@�ȴ@�n�@�E�@�5?@��@��^@�hs@�G�@�%@��@��@�j@�I�@�1'@�  @��;@��w@���@�l�@�;d@��H@���@��\@�~�@�ff@�$�@�@��#@��9@K�@rn�@j��@d(�@\��@W�;@Q�7@J�!@A&�@8��@2~�@.E�@*~�@'+@"�@V@�@�
@\)@
�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�XA�\)A�\)A�bNA�bNA�n�A�n�A�n�A�n�A�p�A�p�A�r�A�r�A�t�A�t�A�t�A�t�A�v�A�v�A�x�A�x�A�z�A�z�A�|�A�z�A�z�A�z�A�|�A�|�A�z�A�x�A�v�A�v�A�r�A�r�A�l�A�M�A���Aƴ9A�?}A�"�A�VA�1'A�O�A��A��jA��;A��\A���A� �A��RA�v�A�S�A��\A���A�ffA��A�`BA��-A�^5A��!A�t�A�XA��#A���A��+A�;dA�XA��A��A�$�A���A��A�C�A�ȴA�C�A��9A�XA��HA�9XA���A�G�A�dZA�bNA���A�Q�A�%A��A���A���A���A�G�A�?}A���A���A���A�/A���A��A���A�r�A��!A���A�ȴA��!A�l�A~�\A|M�AyVAtVAq��Aox�AnffAlȴAj�jAg��AfĜAfr�Adz�AcXAal�A_�-A^��A^��A^z�A\z�AZ�`AY"�AW�mAV��AU|�ASl�AQhsAO�AM
=AK�7AJbAH�`AH�\AG�^AF�/AE�mAD�yAB�AA�hA@r�A?��A>��A>�A=ƨA=�A;`BA9��A7ƨA4��A3�PA2�A0�!A/��A.M�A,�!A+x�A*~�A)7LA'dZA&  A%O�A$E�A$A$�/A$(�A#�A"��A!��A �A�A�AoA��A�9A�uA�AI�A`BA��A�
A�!A��AhsA��A�Az�A�^AA�mA��A-A��A�^A
5?A��A��A�/AI�A�A �A?}A�+A �A -@�-@��D@�7L@���@�G�@�ff@�?}@��;@�{@��@�9@���@���@�$�@���@�h@�z�@�!@�7@�7L@߶F@�-@�K�@ٺ^@ؼj@�1'@��@�b@�  @ם�@և+@Դ9@���@�C�@�@��@��@ӕ�@�1'@�r�@�b@�dZ@��H@ҏ\@��@�hs@�  @�33@·+@�`B@˅@���@ȼj@��;@��@�~�@�@ļj@�@��u@�ƨ@�+@�hs@�
=@�5?@��@��j@��F@��H@��@��@���@�o@�33@��@���@���@�b@���@�+@���@�$�@�j@��
@�9X@��@���@�G�@�E�@���@�^5@�ff@�v�@�n�@�O�@���@�O�@�bN@���@��@�(�@��m@�(�@��P@�V@�r�@�=q@��9@�V@��@�?}@��@�|�@��@��^@�5?@��@�G�@�;d@���@�S�@�S�@���@��#@�/@�j@�+@�O�@�`B@���@�-@�V@�@�Z@��H@��#@�@�x�@�G�@�p�@�/@�%@��`@��9@���@�V@��@�/@�%@�Ĝ@��u@�Z@� �@���@�t�@�+@��H@��\@�$�@���@��@�j@� �@��
@��@�+@��H@��R@���@���@��+@�^5@�{@��@�@��@��#@��-@�?}@�/@�/@�/@�V@���@���@���@�j@�1@�ƨ@���@��@�\)@�33@��@��\@�5?@���@�Ĝ@�(�@�9X@�9X@��@�  @��;@��P@�33@�M�@��7@�hs@��7@��@��7@��h@��h@�`B@���@�I�@���@�K�@�33@��!@�$�@���@���@��@��@��#@��7@�?}@�?}@�G�@�X@�`B@�X@�O�@�O�@�`B@�p�@�p�@�p�@�`B@�O�@��@��`@���@�Z@�Q�@�I�@�I�@�1@���@���@���@���@�ȴ@�n�@�E�@�5?@��@��^@�hs@�G�@�%@��@��@�j@�I�@�1'@�  @��;@��w@���@�l�@�;d@��H@���@��\@�~�@�ff@�$�@�@��#@��9@K�@rn�@j��@d(�@\��@W�;@Q�7@J�!@A&�@8��@2~�@.E�@*~�@'+@"�@V@�@�
@\)@
�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�PB�VB�VB�VB�PB�VB�VB�VB�VB�VB�\B�VB�VB�VB�VB�VB�VB�\B�\B�\B�\B�\B�\B�VB�\B�\B�\B�\B�\B�hB�{B��B��B��B��B�B��B�
B�HB�fB�)BÖB�^B�XB�LBĜB�XB�RB�}B�wB�dB�FB�XB�9B��B��B��B�B�bB�oB��B�+B}�BdZBZBZBXBN�BC�BE�B49B+B&�B�B�B�BbB	7BPB  B�B�;B��B��B��B��B��B��B�VB|�Bo�BG�B5?B�BuB
��B
ŢB
��B
��B
��B
�{B
�JB
{�B
VB
I�B
=qB
2-B
�B
1B	�B	�B	��B	�XB	�^B	�'B	�B	�B	��B	��B	�RB	�B	��B	��B	��B	��B	��B	��B	�DB	�+B	�B	t�B	gmB	YB	J�B	;dB	0!B	'�B	 �B	�B	�B	VB		7B	+B��B�B�mB�NB��B��B��BÖB�dB�3B��B��B��B�uB�DB�1B�{B�B�%B}�B~�B�PB�B�B�B�+B��B��B��B��B�{B�bB�JB�\B�VB�=B�7B�%B~�B�Bx�Bx�Bq�Bn�Bm�BiyBdZBbNB]/B]/B]/B^5B\)BZBZBVBQ�BO�BN�BM�BK�BO�BH�BI�BJ�BG�BE�BK�BF�BN�B;dB9XBVBA�B;dB7LB:^B7LB<jB9XB;dB9XB9XB8RBF�B5?B0!B49B2-B=qB7LB:^B8RB7LB7LB7LB7LB:^B?}B=qBA�BD�BI�BR�BZBdZBr�Bx�B{�B{�B{�B}�B~�B�B� B� B�B�B�B�B�B�+B�+B�+B�B�B{�Bz�B}�Bz�Bu�Bq�Bq�Bs�Bw�Bz�B� B�+B�PB�\B��B��B��B��B��B�VB��B��B��B��B��B��B��B�LB�'B�qBĜBŢBǮBȴB��B��BɺBÖB��B�wB��B��B��B�
B�/B�)B�
B��B��B�B�
B�B�)B�`B��B	JB	oB	VB	JB	�B	+B	+B	0!B	49B	:^B	9XB	9XB	33B	33B	49B	6FB	>wB	B�B	B�B	I�B	@�B	?}B	@�B	@�B	A�B	G�B	J�B	K�B	N�B	T�B	YB	^5B	aHB	gmB	hsB	hsB	hsB	iyB	jB	l�B	m�B	n�B	p�B	s�B	r�B	u�B	u�B	v�B	x�B	y�B	{�B	}�B	~�B	� B	� B	�B	�B	�B	�+B	�7B	�=B	�JB	�JB	�PB	�\B	�\B	�\B	�bB	�bB	�bB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�9B	�9B	�?B	�?B	�?B	�FB	�RB	�^B	�dB	�dB	�dB	�dB	�jB	�wB	�}B	�}B	�}B	��B	ÖB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
bB
�B
"�B
'�B
-B
33B
9XB
A�B
I�B
O�B
R�B
XB
[#B
aHB
dZB
iyB
n�B
s�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�PB�VB�VB�VB�PB�VB�VB�VB�VB�VB�\B�VB�VB�VB�VB�VB�VB�\B�\B�\B�\B�\B�\B�VB�\B�\B�\B�\B�\B�hB�{B��B��B��B��B�BB�B�TB�B�ZB�HBĜB��B�}BɺB�}B�jBŢB��B��B�}B��B�dB��B��B��B�+B�oB��B��B�PB�%BjB[#B\)B]/BS�BG�BJ�B7LB/B,B�B�B�BoBDBbBB��B�TB�5B�B��B��B��B��B�{B�B{�BM�B=qB$�B�BB
��B
�B
��B
��B
��B
�oB
�%B
`BB
J�B
A�B
6FB
%�B
hB	��B	�#B	��B	�jB	�}B	�RB	�'B	�B	�B	��B	�qB	�-B	��B	��B	��B	�!B	��B	��B	�\B	�DB	�B	z�B	m�B	_;B	Q�B	@�B	5?B	,B	!�B	�B	�B	hB	PB	PB��B��B�B�`B�B��B��BɺB��B�XB�'B��B��B��B�VB�DB��B�%B�1B�B�B�bB�+B�%B�B�B��B��B��B��B��B�uB�PB�bB�\B�DB�=B�DB�+B�%Bz�B{�Bu�Bp�Bo�Bo�BffBdZB`BB`BBaHBbNB^5B_;B^5B[#BVBS�BQ�BO�BO�BR�BK�BL�BO�BJ�BH�BM�BI�BQ�B=qB<jBXBC�B=qB9XB;dB8RB>wB:^B>wB<jB;dB;dBH�B6FB33B7LB7LB@�B9XB;dB8RB7LB7LB8RB9XB=qBA�B>wBB�BE�BI�BQ�BYBdZBs�By�B|�B|�B|�B~�B�B�B�B�B�%B�+B�%B�+B�+B�1B�7B�7B�7B�+B}�B|�B�B~�Bw�Bs�Br�Bu�Bw�B|�B� B�%B�PB�\B��B��B��B��B��B�VB��B��B��B��B��B��B�B�RB�!B�qBŢBŢBǮBȴB��B��B��BŢB��B�wB��B��B��B�B�;B�;B�#B��B��B�B�
B�
B�B�HB�B	JB	�B	oB		7B	�B	,B	+B	1'B	6FB	<jB	;dB	<jB	7LB	33B	33B	6FB	>wB	C�B	E�B	K�B	B�B	?}B	A�B	A�B	A�B	H�B	J�B	K�B	N�B	T�B	YB	^5B	aHB	gmB	iyB	iyB	iyB	jB	k�B	m�B	n�B	o�B	q�B	t�B	s�B	v�B	v�B	w�B	y�B	z�B	|�B	~�B	~�B	� B	� B	�B	�B	�B	�+B	�7B	�=B	�JB	�PB	�VB	�\B	�\B	�\B	�bB	�bB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�?B	�?B	�FB	�FB	�?B	�LB	�XB	�^B	�dB	�dB	�dB	�dB	�qB	�}B	�}B	�}B	�}B	��B	ÖB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�
B	�B	�B	�B	�#B	�B	�B	�B	�B	�)B	�5B	�5B	�BB	�HB	�HB	�NB	�NB	�TB	�`B	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
bB
�B
#�B
'�B
-B
33B
9XB
A�B
I�B
O�B
R�B
XB
[#B
aHB
dZB
jB
n�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�h<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452102012011014521020120110145210  AO  ARGQ                                                                        20111130144008  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144008  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145210  IP                  G�O�G�O�G�O�                