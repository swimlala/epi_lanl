CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:15Z AOML 3.0 creation; 2016-08-07T21:17:34Z UW 3.1 conversion     
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
_FillValue                 �  Ap   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cd   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
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
resolution        :�o     �  pP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221315  20160807141734  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               "A   AO  5285_8895_034                   2C  D   APEX                            6487                            072314                          846 @�<9X 1   @�<9�ο�@,�n��O��c�(�\1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    "A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A���A�ffB��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�3Dy��D�  D�<�D�s3D��3D��D�P D��3D�� D� D�@ D�vfDǦf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��
@��
A	�A)�AI�Ai�A���A���A���A���A���A�A���A�B�B
{Bz�Bz�B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bbz�Bjz�Br�GBz�GB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz�RC|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�\)C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�\)C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt�Dy�{D��D�P�D��
D��
D�0�D�c�D��
D���D�#�D�S�D��=DǺ=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�33A�{A���A��yA��A��A���A���A���A���A���A���A���A�ȴA�ĜAԥ�A��Aщ7A�ĜA�dZA���Aϲ-A���Aω7AήA͓uA�A�E�A�ƨAʮAʣ�AʬAʴ9AʸRAʮAʡ�A�r�A�Q�AɸRAȍPA�G�AǁA��Aħ�A�jA�33A�/A�
=A���A��PA�|�A�+A���A�
=A�v�A�|�A�S�A��^A��;A��A�5?A��PA��A�ƨA�9XA�33A�1A�E�A��A�dZA��A�JA�ƨA���A�/A���A��jA��
A���A��A��
A�JA�dZA�~�A��A�/A��9A�hsAz��Av��As33Ah$�Aa"�AY33ATA�AQ�;AJ�AD��AB1'A@�/A@��A@r�A?ƨA?;dA=/A8�uA6ĜA4�A3�wA3VA2�yA2�RA2VA2JA2(�A2Q�A1�;A1oA.��A.�uA,{A'��A$�A"ZA!G�A M�A�hA�A�mA�HA�
A��Az�A�uA��A�uA�PA�
AZAƨA�`A�uAĜAS�AXA`BA\)A�A"�A��A\)A��A��AhsA\)A��A-A��Al�AȴA�^A7LAS�A;dAjA�A\)A��Ar�A/A��A��A�yA�TA��A�PA�PA"�A
�9A
VA
A�A
A	ƨA�HA^5A�A\)A�9AjA�A�PA;dA�HA+A��A�AQ�Ax�Az�AA�hA%A ��A =q@���@���@�Ĝ@�1'@���@�C�@�+@��@��\@�$�@��@���@��@�I�@�"�@�~�@���@�R@�7@�/@��D@��@��;@�|�@�v�@�=q@��T@���@� �@�;d@���@�~�@�G�@���@�I�@���@���@�S�@�~�@�5?@�-@�7@���@�z�@�+@���@��@߅@�ƨ@߾w@�@��@���@�r�@ۍP@��H@ڸR@ڗ�@�n�@��@ו�@�S�@��y@և+@���@�X@ԃ@� �@Ӯ@�K�@�ȴ@�ff@�5?@�@�X@�%@Гu@��;@ϕ�@�dZ@�33@���@�=q@͉7@���@�I�@�K�@��@�V@��@��#@ɲ-@�X@ȓu@�1@��m@ǝ�@�\)@��@�^5@���@ŉ7@�/@��@Ĭ@��@Ý�@�33@��y@\@�=q@��^@�%@���@���@��@�l�@��@�M�@��^@��@�hs@�/@�Ĝ@�1@�K�@���@�5?@�J@�@�G�@���@���@�j@�A�@��;@�S�@���@��+@��@��T@�x�@��/@�Z@�(�@��@��@�"�@�ȴ@���@���@�x�@�&�@��/@�Ĝ@��@�A�@��w@�t�@�ȴ@�V@�E�@�J@�p�@���@�Z@�  @��@�"�@�ȴ@�ff@�$�@��@�x�@��j@�j@�I�@���@�dZ@�\)@�C�@��@�@�ȴ@�=q@�@��h@���@��u@��D@�r�@�(�@���@�l�@�o@�
=@��y@��y@��H@��R@�=q@��T@�X@��/@���@���@���@�j@�A�@�(�@�  @�ƨ@�t�@�"�@���@�n�@�M�@�J@�@�%@�Ĝ@��u@��@�Z@� �@��@��F@�dZ@��@���@�-@���@�`B@�X@�X@�O�@�?}@���@��@�A�@���@���@��@���@�l�@��@�E�@���@���@���@�hs@�O�@���@��@�z�@�I�@��@��@�@���@�=q@��@�O�@��@��@���@�1'@��P@��@��\@�ff@�{@�X@���@���@�Z@�I�@�b@��
@��F@���@��P@�K�@��7@�dZ@�P@vv�@l�/@`��@W�;@Q%@G�;@?�;@:�@4��@-�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111A�33A�{A���A��yA��A��A���A���A���A���A���A���A���A�ȴA�ĜAԥ�A��Aщ7A�ĜA�dZA���Aϲ-A���Aω7AήA͓uA�A�E�A�ƨAʮAʣ�AʬAʴ9AʸRAʮAʡ�A�r�A�Q�AɸRAȍPA�G�AǁA��Aħ�A�jA�33A�/A�
=A���A��PA�|�A�+A���A�
=A�v�A�|�A�S�A��^A��;A��A�5?A��PA��A�ƨA�9XA�33A�1A�E�A��A�dZA��A�JA�ƨA���A�/A���A��jA��
A���A��A��
A�JA�dZA�~�A��A�/A��9A�hsAz��Av��As33Ah$�Aa"�AY33ATA�AQ�;AJ�AD��AB1'A@�/A@��A@r�A?ƨA?;dA=/A8�uA6ĜA4�A3�wA3VA2�yA2�RA2VA2JA2(�A2Q�A1�;A1oA.��A.�uA,{A'��A$�A"ZA!G�A M�A�hA�A�mA�HA�
A��Az�A�uA��A�uA�PA�
AZAƨA�`A�uAĜAS�AXA`BA\)A�A"�A��A\)A��A��AhsA\)A��A-A��Al�AȴA�^A7LAS�A;dAjA�A\)A��Ar�A/A��A��A�yA�TA��A�PA�PA"�A
�9A
VA
A�A
A	ƨA�HA^5A�A\)A�9AjA�A�PA;dA�HA+A��A�AQ�Ax�Az�AA�hA%A ��A =q@���@���@�Ĝ@�1'@���@�C�@�+@��@��\@�$�@��@���@��@�I�@�"�@�~�@���@�R@�7@�/@��D@��@��;@�|�@�v�@�=q@��T@���@� �@�;d@���@�~�@�G�@���@�I�@���@���@�S�@�~�@�5?@�-@�7@���@�z�@�+@���@��@߅@�ƨ@߾w@�@��@���@�r�@ۍP@��H@ڸR@ڗ�@�n�@��@ו�@�S�@��y@և+@���@�X@ԃ@� �@Ӯ@�K�@�ȴ@�ff@�5?@�@�X@�%@Гu@��;@ϕ�@�dZ@�33@���@�=q@͉7@���@�I�@�K�@��@�V@��@��#@ɲ-@�X@ȓu@�1@��m@ǝ�@�\)@��@�^5@���@ŉ7@�/@��@Ĭ@��@Ý�@�33@��y@\@�=q@��^@�%@���@���@��@�l�@��@�M�@��^@��@�hs@�/@�Ĝ@�1@�K�@���@�5?@�J@�@�G�@���@���@�j@�A�@��;@�S�@���@��+@��@��T@�x�@��/@�Z@�(�@��@��@�"�@�ȴ@���@���@�x�@�&�@��/@�Ĝ@��@�A�@��w@�t�@�ȴ@�V@�E�@�J@�p�@���@�Z@�  @��@�"�@�ȴ@�ff@�$�@��@�x�@��j@�j@�I�@���@�dZ@�\)@�C�@��@�@�ȴ@�=q@�@��h@���@��u@��D@�r�@�(�@���@�l�@�o@�
=@��y@��y@��H@��R@�=q@��T@�X@��/@���@���@���@�j@�A�@�(�@�  @�ƨ@�t�@�"�@���@�n�@�M�@�J@�@�%@�Ĝ@��u@��@�Z@� �@��@��F@�dZ@��@���@�-@���@�`B@�X@�X@�O�@�?}@���@��@�A�@���@���@��@���@�l�@��@�E�@���@���@���@�hs@�O�@���@��@�z�@�I�@��@��@�@���@�=q@��@�O�@��@��@���@�1'@��P@��@��\@�ff@�{@�X@���@���@�Z@�I�@�b@��
@��F@���@��PG�O�@��7@�dZ@�P@vv�@l�/@`��@W�;@Q%@G�;@?�;@:�@4��@-�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB	e`B	e`B	e`B	e`B	e`B	e`B	e`B	e`B	e`B	e`B	e`B	e`B	ffB	ffB	gmB	�3B
uB
L�B
gmB
s�B
}�B
�7B
��B
�?B
ǮB
�5B�B(�BB�BVBo�B�B��B��B�TB�`B�yB�B�B	7B!�B8RBN�BQ�BP�B[#BdZBs�B�%B�VB�bB�\B�JB�VB�B��B�B��B��B��B��B��B��B��B��B�uB�Bm�BffBS�B8RB1BȴB�!B|�Bn�BaHBYBZBT�BF�B&�B
��B
��B
�B
w�B
O�B
(�B	�B	��B	��B	ffB	7LB	oB	B��B��B�ZB�#B�B�#B�#B�B�B��B��B��B��B�B�5B�`B�B��B	bB	!�B	.B	W
B	�oB	��B	��B	��B	r�B	YB	I�B	D�B	A�B	=qB	<jB	E�B	e`B	VB	A�B	F�B	Q�B	[#B	\)B	_;B	n�B	}�B	� B	� B	�+B	��B	�}B	ƨB	�}B	��B	��B	��B
B
VB
VB
{B
�B
%�B
(�B
)�B
'�B
'�B
$�B
!�B
�B
 �B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
\B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
bB
\B
DB
	7B
1B
%B
B
B
	7B
\B
bB

=B
B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�NB	�HB	�TB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
  B
  B
  B
  B
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
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
	7B
1B
	7B
	7B

=B

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
JB
JB
PB
PB
PB
PB
VB
VB
VB
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
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
�B
 �B
!�B
!�B
!�B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
-B
1'B
8RB
?}B
D�B
L�B
Q�B
W
B
\)B
`BB
bNB
hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111B	e>B	e?B	e=B	e>B	e=B	e>B	eAB	e=B	e?B	e@B	e>B	e>B	fCB	fEB	gLB	�B
KB
L�B
g@B
s�B
}�B
�B
�nB
�B
ǁB
�BVB(�BB`BU�BolB��B�zBΧB�#B�0B�FB�XB�qB	B!�B8BN�BQ�BP�BZ�Bd%Bs�B��B�!B�1B�%B�B�B��B�pB��B��B��B��B��B��B��B��B��B�=B��BmXBf0BS�B8B�B�wB��B|�Bn]BaBX�BY�BT�BFlB&�B
��B
ːB
��B
w�B
O�B
(�B	�hB	ˏB	��B	f4B	7B	>B	�B��B��B�)B��B��B��B��B��B��BдB��BаBҽB��B� B�+B�OB��B	*B	!�B	-�B	V�B	�3B	��B	��B	�LB	ruB	X�B	I�B	DcB	AMB	=6B	<.B	EhB	e$B	U�B	ALB	FlB	Q�B	Z�B	[�B	^�B	nZB	}�B	�B	�B	��B	�AB	�<B	�fB	�:B	̊B	��B	��B
�B
B
B
7B
{B
%�B
(�B
)�B
'�B
'�B
$�B
!�B
\B
 �B
!�B
!�B
{B
yB
qB
hB
VB
bB
:B
B
B
9B
AB
[B
ZB
NB
JB
GB
BB
QB
<B
"B
B
B
 B
�B
�B
�B
�B
�B
�B
B
B
	�B
�B	��B	��B	��B	�}B	�kB	�^B	�MB	�GB	�LB	�?B	�LB	�\B	�dB	�jB	�jB	�qB	�rB	�tB	�}B	�wB	�uB	�pB	�lB	�dB	�WB	�SB	�TB	�RB	�IB	�JB	�KB	�LB	�IB	�FB	�:B	�9B	�2B	�3B	�;B	�4B	�3B	�HB	�KB	�MB	�VB	�XB	�XB	�VB	�JB	�?B	�1B	�B	�B	�B	�3B	�QB	�OB	�CB	�EB	�DB	�=B	�EB	�DB	�>B	�>B	�6B	�3B	�=B	�>B	�8B	�9B	�7B	�5B	�=B	�?B	�?B	�6B	�7B	�8B	�6B	�7B	�2B	�4B	�KB	�OB	�UB	�VB	�RB	�UB	�WB	�VB	�PB	�LB	�KB	�BB	�JB	�LB	�GB	�KB	�OB	�OB	�PB	�PB	�OB	�QB	�UB	�]B	�]B	�aB	�eB	�oB	�gB	�oB	�zB	�|B	�sB	�tB	��B	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
B
B
B

B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
$B
#B
#B
*B
+B
/B
0B
0B
0B
1B
7B
7B
5B
:B
=B
AB
HB
JB
JB
LB
HB
PB
HB
PB
RB
MB
TB
`B
`B
aB
fB
lB
lB
mB
lB
 |B
 {B
 {B
 {B
 |B
 xB
 }B
uB
 yB
!�B
!B
!B
 xB
!~B
"�B
"�B
"�B
#�B
#�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�G�O�B
'�B
,�B
0�B
8B
?0B
DOB
L�B
Q�B
V�B
[�B
_�B
bB
h$11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417342016080714173420160807141734  AO  ARCAADJP                                                                    20150226221315    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221315  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221315  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141734  IP                  G�O�G�O�G�O�                