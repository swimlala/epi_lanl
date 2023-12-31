CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-09T19:17:35Z AOML 3.0 creation; 2016-08-07T21:17:34Z UW 3.1 conversion     
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
_FillValue                 �  At   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cl   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  px   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  zD   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150309191735  20160807141734  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               %A   AO  5285_8895_037                   2C  D   APEX                            6487                            072314                          846 @�@&Q���1   @�@&�?�@,�I�^5�c�I�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    %A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�33B���B�  B�  B���B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di��Djy�Dj��Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Dr��Ds� Dt  DtFfDyffD���D�0 D��3D���D�fD�S3D��3D�ɚD�  D�C3D�y�DǬ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@��
A	�A)�AI�Ai�A���A���A���A���A���A���A���A���Bz�B
z�Bz�Bz�B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bb�GBj�GBrz�Bzz�B�=qB�=qB�=qB�=qB�p�B�
>B�=qB�=qB�
>B�=qB�=qB�p�B�
>B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8�RC:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�B�C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj!HDj�HDk!HDk�HDl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq!HDq��Dr'�Dr��Ds!HDs��Dt'�DtnDy�D� �D�C�D��
D��qD�*=D�g
D��
D��qD��D�W
D��qD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aմ9Aմ9Aմ9AոRAպ^Aպ^AոRAոRAռjAռjAռjAվwAվwA���A�A�A�A�A�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ƨA�ȴAոRAԼjA���A�"�A�p�A�G�A��/A�"�AΧ�A��TA��A���A�VAơ�A��A�n�A�?}A�?}A��A�ĜA���A�E�A��A���A�~�A�/A�-A�~�A�A�JA��-A�E�A��A�?}A�?}A��+A�1A��A��\A���A�p�A���A�{A���A��A�z�A���A��A�dZA�`BA�33A��-A}ƨAv��Ar�uAk�Ag�AfȴAd�Ab�A`bNA]XA["�AW;dASXAO\)AIAE��ADbNAB�A?\)A=�A;+A9&�A7��A6z�A5�^A4�A41A3|�A2z�A1"�A/A-�
A,I�A+��A+l�A+;dA*��A*jA*�A)��A(�DA'�A'l�A'oA$z�A#&�A!��A JA��AhsAO�A
=A�^A?}A�HAAXA�AbNA`BA��A��A\)A�jA�;AoA{A�-A��A��A?}A�A��A��A1AO�A�A�!A��A33A+A�A�A
�`A
�uA
A�A	O�AVA�mA��A�A�TA\)A��A�AM�A7LA ��A n�@��;@�n�@�?}@��@��/@�r�@�+@�M�@���@���@��
@�V@��/@���@�/@�V@��u@�9X@���@�"�@��\@�$�@�@�hs@��@�
=@�@���@�/@�b@�\@��@��@��@�b@ꟾ@���@��@�Z@�l�@�;d@�~�@�p�@�@��@�@��@�V@�Ĝ@�I�@�ƨ@�P@�S�@��@�o@⟾@�@�@�x�@�V@���@��@�t�@��#@��/@�dZ@�;d@ى7@�`B@�X@�p�@ف@�&�@��@�j@�1@�  @��;@ָR@�{@�hs@��/@Ԭ@ԃ@�(�@�K�@��T@Ѻ^@�X@Ь@�I�@�A�@ϥ�@Χ�@��@�@͑h@�O�@���@�I�@ˍP@���@ʰ!@�n�@���@�`B@���@Ǿw@��@�ff@�@�&�@�I�@�ƨ@�l�@��@�V@��@��h@�7L@��@��;@�l�@�;d@�o@��@�V@�5?@�@��#@���@��@�Z@� �@�l�@��y@�v�@��@��@��@��j@�1'@��@�K�@���@��+@�5?@�{@��-@�X@��9@�I�@�1'@�b@��m@�dZ@���@��@��@��@���@���@�A�@��
@���@�t�@�33@�o@���@�v�@�E�@�@��h@�/@���@��D@�Z@��@���@�t�@�33@���@��!@�{@��#@�@���@�hs@�%@�Ĝ@�z�@�Z@�(�@��
@��
@�l�@��@��@��@��R@���@�^5@�$�@���@�G�@��j@�I�@��F@�;d@�o@���@�v�@�J@��-@�%@���@�bN@�1@�t�@�+@���@��R@�~�@�M�@�{@��T@���@�x�@�G�@���@�r�@��@��
@�\)@�o@���@��y@�J@��@�7L@��u@�A�@��w@�dZ@�S�@�33@�o@��H@�v�@�=q@��@��@���@��h@�G�@���@��@�b@��
@���@�K�@�"�@�ȴ@���@��+@�E�@�E�@�J@��#@��^@�hs@��@��9@�1'@��m@��m@��
@�ƨ@��F@���@�l�@�;d@�@���@���@�v�@�^5@�M�@�=q@�$�@�@��@���@��/@��@��u@�z�@�1'@��m@���@���@�|�@�dZ@�33@��y@��\@��\@�~�@�V@�{@�@���@���@~$�@t�j@hĜ@`Ĝ@V�+@O�;@I�7@@b@7�;@2J@-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   Aմ9Aմ9Aմ9AոRAպ^Aպ^AոRAոRAռjAռjAռjAվwAվwA���A�A�A�A�A�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ƨA�ȴAոRAԼjA���A�"�A�p�A�G�A��/A�"�AΧ�A��TA��A���A�VAơ�A��A�n�A�?}A�?}A��A�ĜA���A�E�A��A���A�~�A�/A�-A�~�A�A�JA��-A�E�A��A�?}A�?}A��+A�1A��A��\A���A�p�A���A�{A���A��A�z�A���A��A�dZA�`BA�33A��-A}ƨAv��Ar�uAk�Ag�AfȴAd�Ab�A`bNA]XA["�AW;dASXAO\)AIAE��ADbNAB�A?\)A=�A;+A9&�A7��A6z�A5�^A4�A41A3|�A2z�A1"�A/A-�
A,I�A+��A+l�A+;dA*��A*jA*�A)��A(�DA'�A'l�A'oA$z�A#&�A!��A JA��AhsAO�A
=A�^A?}A�HAAXA�AbNA`BA��A��A\)A�jA�;AoA{A�-A��A��A?}A�A��A��A1AO�A�A�!A��A33A+A�A�A
�`A
�uA
A�A	O�AVA�mA��A�A�TA\)A��A�AM�A7LA ��A n�@��;@�n�@�?}@��@��/@�r�@�+@�M�@���@���@��
@�V@��/@���@�/@�V@��u@�9X@���@�"�@��\@�$�@�@�hs@��@�
=@�@���@�/@�b@�\@��@��@��@�b@ꟾ@���@��@�Z@�l�@�;d@�~�@�p�@�@��@�@��@�V@�Ĝ@�I�@�ƨ@�P@�S�@��@�o@⟾@�@�@�x�@�V@���@��@�t�@��#@��/@�dZ@�;d@ى7@�`B@�X@�p�@ف@�&�@��@�j@�1@�  @��;@ָR@�{@�hs@��/@Ԭ@ԃ@�(�@�K�@��T@Ѻ^@�X@Ь@�I�@�A�@ϥ�@Χ�@��@�@͑h@�O�@���@�I�@ˍP@���@ʰ!@�n�@���@�`B@���@Ǿw@��@�ff@�@�&�@�I�@�ƨ@�l�@��@�V@��@��h@�7L@��@��;@�l�@�;d@�o@��@�V@�5?@�@��#@���@��@�Z@� �@�l�@��y@�v�@��@��@��@��j@�1'@��@�K�@���@��+@�5?@�{@��-@�X@��9@�I�@�1'@�b@��m@�dZ@���@��@��@��@���@���@�A�@��
@���@�t�@�33@�o@���@�v�@�E�@�@��h@�/@���@��D@�Z@��@���@�t�@�33@���@��!@�{@��#@�@���@�hs@�%@�Ĝ@�z�@�Z@�(�@��
@��
@�l�@��@��@��@��R@���@�^5@�$�@���@�G�@��j@�I�@��F@�;d@�o@���@�v�@�J@��-@�%@���@�bN@�1@�t�@�+@���@��R@�~�@�M�@�{@��T@���@�x�@�G�@���@�r�@��@��
@�\)@�o@���@��y@�J@��@�7L@��u@�A�@��w@�dZ@�S�@�33@�o@��H@�v�@�=q@��@��@���@��h@�G�@���@��@�b@��
@���@�K�@�"�@�ȴ@���@��+@�E�@�E�@�J@��#@��^@�hs@��@��9@�1'@��m@��m@��
@�ƨ@��F@���@�l�@�;d@�@���@���@�v�@�^5@�M�@�=q@�$�@�@��@���@��/@��@��u@�z�@�1'@��m@���@���@�|�@�dZ@�33@��y@��\@��\@�~�@�V@�{G�O�@���@���@~$�@t�j@hĜ@`Ĝ@V�+@O�;@I�7@@b@7�;@2J@-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB~�B~�B~�B� B� B� B� B� B� B� B� B� B~�B� B� B� B� B� B� B~�B� B� B� B�B�B�B�uB	�B	I�B	ZB	�B	�jB	�B	��B
'�B
R�B
�1B
�FB
�B
�BD�B��B  B�B>wBB�B0!B�B�HB�B��B�B��B�PB� Bv�Bm�BXB9XB�B
��B
�B
��B
��B
�LB
�uB
�+B
w�B
l�B
dZB
L�B
�B	��B	�B	�B	�HB	�#B	ŢB	��B	y�B	bNB	E�B	49B	-B	$�B	�B	VB	  B�B�NB��BB�FB�?B��B�mB�B�B��B��B	  B	B	%B	+B	
=B	
=B	DB	PB	VB	oB	uB	oB	oB	hB	hB	hB	hB	hB	hB	hB	bB	PB	DB		7B	PB	uB	oB	oB	oB	uB	�B	�B	�B	�B	�B	#�B	(�B	,B	.B	/B	6FB	6FB	<jB	?}B	B�B	C�B	E�B	M�B	L�B	J�B	M�B	K�B	L�B	VB	YB	[#B	\)B	]/B	_;B	`BB	`BB	_;B	`BB	_;B	aHB	^5B	^5B	]/B	[#B	\)B	YB	W
B	S�B	M�B	K�B	K�B	K�B	K�B	N�B	O�B	P�B	R�B	S�B	Q�B	VB	q�B	k�B	�B	��B	��B	��B	��B	�B	�3B	�LB	�RB	�dB	�jB	�}B	��B	B	B	ÖB	B	B	��B	�qB	�LB	�?B	�jB	ƨB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	�/B	�/B	�/B	�5B	�5B	�/B	�)B	�5B	�NB	�ZB	�`B	�ZB	�TB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�HB	�/B	�B	�)B	�)B	�)B	�;B	�BB	�ZB	�`B	�`B	�ZB	�TB	�TB	�NB	�BB	�;B	�;B	�HB	�NB	�ZB	�ZB	�ZB	�NB	�NB	�`B	�sB	�sB	�yB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
PB
PB
VB
VB
VB
VB
\B
\B
\B
bB
bB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
.B
(�B
/B
49B
?}B
E�B
J�B
P�B
S�B
XB
]/B
bNB
ffB
iy111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   B~�B~�B~�B�B�B�B�B�B�B�B�B�B~�B�B�B�B�B�B�B~�B�B�B�B��B��B��B�CB	aB	I�B	Y�B	��B	�FB	�B	��B
'�B
R�B
�B
�B
��B
�BDjB�XB��BRB>CBBYB/�B�hB�B�OB��B�uB��B�B�Bv�BmZBW�B9 B{B
��B
�FB
ѵB
ѵB
�B
�=B
��B
w�B
lUB
d#B
L�B
yB	��B	�RB	�GB	�B	��B	�lB	��B	y�B	bB	ErB	4B	,�B	$�B	iB	$B��B�B� BиB�_B�B�BдB�:B�]B�vB��B��B��B	�B	�B	�B	
B	
B	B	B	 B	7B	=B	6B	8B	0B	1B	1B	0B	/B	/B	0B	+B	B	B		 B	B	9B	5B	5B	5B	=B	kB	fB	lB	kB	~B	#�B	(�B	+�B	-�B	.�B	6
B	6
B	<,B	?AB	BQB	CXB	EeB	M�B	L�B	J�B	M�B	K�B	L�B	U�B	X�B	Z�B	[�B	\�B	^�B	`B	`B	^�B	`B	^�B	a
B	]�B	]�B	\�B	Z�B	[�B	X�B	V�B	S�B	M�B	K�B	K�B	K�B	K�B	N�B	O�B	P�B	R�B	S�B	Q�B	U�B	qgB	kEB	��B	�VB	�pB	��B	��B	��B	��B	�B	�B	� B	�$B	�8B	�EB	�LB	�JB	�PB	�LB	�MB	�?B	�*B	�	B	��B	�&B	�bB	�WB	�]B	�jB	�{B	ΕB	ΗB	ΕB	͍B	͎B	ԹB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�,B	�.B	�1B	�0B	�(B	�+B	�4B	�7B	�EB	�BB	�HB	�EB	�EB	�AB	�DB	�IB	�JB	�UB	�[B	�\B	�]B	�VB	�[B	�]B	�XB	�[B	�cB	�hB	�jB	�gB	�oB	�gB	�mB	�uB	�sB	�sB	�uB	�zB	�zB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B

�B

�B

�B
�B
B
B
B
B

B
B
B
B
B
B
B
B
B
B
%B
$B
#B
#B
+B
,B
+B
*B
*B
)B
+B
/B
5B
5B
8B
4B
7B
0B
6B
<B
6B
;B
?B
BB
IB
JB
HB
HB
IB
NB
RB
UB
TB
TB
\B
ZB
bB
^B
ZB
\B
^B
^B
iB
gB
rB
rB
rB
qB
qB
lB
mB
lB
mB
 xB
!�B
!}B
!~B
!~B
!~B
!B
!�B
!{B
!}B
!~B
!~B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�G�O�B
(�B
.�B
3�B
?/B
EUB
JuB
P�B
S�B
W�B
\�B
b B
fB
i,111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417342016080714173420160807141734  AO  ARCAADJP                                                                    20150309191735    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150309191735  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150309191735  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141734  IP                  G�O�G�O�G�O�                