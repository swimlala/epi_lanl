CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-28T10:17:57Z AOML 3.0 creation; 2016-08-07T21:17:44Z UW 3.1 conversion     
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
_FillValue                 �  A|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ct   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP_ADJUSTED            
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
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151228101757  20160807141744  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ]A   AO  5285_8895_093                   2C  D   APEX                            6487                            072314                          846 @׉��[�1   @׉�b���@08bM���dNn��O�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ]A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy� D� D�L�D�vfD���D���D�0 D�s3D�� D�3D�` D���Dǹ�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��
@��
A	�A)�AI�Ai�A���A���A���A���A���A���A���A���Bz�B
z�Bz�Bz�B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bb{Bjz�Brz�Bzz�B�=qB�p�B�p�B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�p�B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qBݣ�B�=qB�
>B�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C�RC��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD�RCF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�\)C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1�D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO.DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt��Dt�Dy��D�#�D�`�D��=D� �D��D�C�D��
D���D�
D�s�D��qD��qD��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�z�A�z�A�z�A�|�A�|�A�~�AсA�~�A�|�A�|�A�z�A�dZA�^5A�\)A�M�A�=qA�?}A�;dA�5?A�1'A�&�A��A�{A�VA�A�A�E�A�hsAя\A�~�A�v�A�/Aа!A�+A�K�A�ȴA�l�A���A���A˺^A�S�A�VAǋDA�S�A�M�A��A���A��yA��A��+A�oA���A���A�|�A���A��wA�+A��#A��A���A�A��A�I�A���A��
A��PA�Q�A��A�S�A��A�+A���A�x�A���A���A�ȴA�A�7LA�jA�/A���A�?}A� �A�VAxn�ApVAo�#Am��Ai�Ae��AaS�A^��A]`BAZ��AW�7AVffAT��AQx�AM�AK�AIx�AFĜAD1A@5?A<�A9��A6z�A4v�A3�7A2A�A01'A/&�A/�A0�\A1?}A.��A.n�A.�uA-�hA-x�A-dZA,�`A,9XA+��A*��A* �A(�A(�\A'�7A&�A&A%x�A%A$JA#�7A#S�A#VA"��A"(�A!��A!��A!`BA!\)A!�A �!A��A�AhsA�HAZA��AXA��A��AVA5?AJA�TA��A33A�+A�A�^AXA�A�yA�RA��Ar�AI�AJA�A��AbNA�-Al�A��A�AZA�^A�PAl�AC�A��A�\Ar�AVA �A��A�A7LA�A��AA�AA�AdZAXA��A-A�hA%A~�A�A\)AVA
�A
n�A	�mA	�;A	�
A	��A	"�A��AffA�A�A�
A�^A�AhsAVA��AE�A��AXA�HA=qA|�A&�A�A�#AXA �A jA @�\)@���@���@�V@��^@���@���@�33@�-@��h@�O�@�%@��D@�1'@��;@�o@�v�@�V@��@���@�/@�Ĝ@�(�@��;@��@�K�@�~�@�J@�hs@�@�1'@@��@�G�@�@�@�;d@�+@�"�@�o@�
=@��y@��y@�R@ꗍ@�v�@�E�@���@�h@�/@�\)@�V@��@�hs@�A�@��@�R@��T@��@�@�  @߾w@�l�@��H@�M�@�@�G�@�r�@�+@��@ٲ-@��@���@��`@ج@�z�@��@��
@�ƨ@׮@ם�@�|�@�ȴ@�n�@��@ՙ�@���@ԓu@���@�S�@ҸR@с@Л�@�bN@�9X@�1@�|�@�;d@�"�@��H@Ώ\@�M�@��#@�x�@�O�@�/@�Ĝ@˅@���@��@ɲ-@�?}@ȋD@��m@�\)@��y@�~�@�?}@�1@�@�x�@�Z@���@��@�O�@�V@���@�bN@��
@�"�@�
=@���@�$�@���@���@���@��@��`@���@��@�bN@�  @�S�@��\@�@���@�?}@��@��@���@��@�I�@�1@��
@��F@���@�"�@���@��R@���@�n�@�E�@��@��@���@�hs@�`B@��@�Ĝ@�C�@�v�@�?}@�bN@�b@�ƨ@���@�l�@�K�@�+@�@���@��\@��@�hs@�?}@��@��`@��9@�r�@�1'@��@��@�1@���@��F@��F@��@��@���@�ȴ@�ȴ@�ȴ@�=q@�hs@���@���@�Z@�  @�ƨ@���@�t�@�C�@�ȴ@�@�`B@�G�@�7L@�&�@�&�@��@�%@��9@��D@��@�z�@�j@�bN@�Q�@�9X@���@�S�@��@�ff@���@�O�@���@�Ĝ@��9@��@��u@�z�@�Z@�A�@� �@�b@�  @��@��
@��@��P@�|�@�K�@��T@��@��@���@xbN@u?}@k33@b�\@Z�H@Q�^@H�u@@bN@:�H@3o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A�z�A�z�A�z�A�z�A�|�A�|�A�~�AсA�~�A�|�A�|�A�z�A�dZA�^5A�\)A�M�A�=qA�?}A�;dA�5?A�1'A�&�A��A�{A�VA�A�A�E�A�hsAя\A�~�A�v�A�/Aа!A�+A�K�A�ȴA�l�A���A���A˺^A�S�A�VAǋDA�S�A�M�A��A���A��yA��A��+A�oA���A���A�|�A���A��wA�+A��#A��A���A�A��A�I�A���A��
A��PA�Q�A��A�S�A��A�+A���A�x�A���A���A�ȴA�A�7LA�jA�/A���A�?}A� �A�VAxn�ApVAo�#Am��Ai�Ae��AaS�A^��A]`BAZ��AW�7AVffAT��AQx�AM�AK�AIx�AFĜAD1A@5?A<�A9��A6z�A4v�A3�7A2A�A01'A/&�A/�A0�\A1?}A.��A.n�A.�uA-�hA-x�A-dZA,�`A,9XA+��A*��A* �A(�A(�\A'�7A&�A&A%x�A%A$JA#�7A#S�A#VA"��A"(�A!��A!��A!`BA!\)A!�A �!A��A�AhsA�HAZA��AXA��A��AVA5?AJA�TA��A33A�+A�A�^AXA�A�yA�RA��Ar�AI�AJA�A��AbNA�-Al�A��A�AZA�^A�PAl�AC�A��A�\Ar�AVA �A��A�A7LA�A��AA�AA�AdZAXA��A-A�hA%A~�A�A\)AVA
�A
n�A	�mA	�;A	�
A	��A	"�A��AffA�A�A�
A�^A�AhsAVA��AE�A��AXA�HA=qA|�A&�A�A�#AXA �A jA @�\)@���@���@�V@��^@���@���@�33@�-@��h@�O�@�%@��D@�1'@��;@�o@�v�@�V@��@���@�/@�Ĝ@�(�@��;@��@�K�@�~�@�J@�hs@�@�1'@@��@�G�@�@�@�;d@�+@�"�@�o@�
=@��y@��y@�R@ꗍ@�v�@�E�@���@�h@�/@�\)@�V@��@�hs@�A�@��@�R@��T@��@�@�  @߾w@�l�@��H@�M�@�@�G�@�r�@�+@��@ٲ-@��@���@��`@ج@�z�@��@��
@�ƨ@׮@ם�@�|�@�ȴ@�n�@��@ՙ�@���@ԓu@���@�S�@ҸR@с@Л�@�bN@�9X@�1@�|�@�;d@�"�@��H@Ώ\@�M�@��#@�x�@�O�@�/@�Ĝ@˅@���@��@ɲ-@�?}@ȋD@��m@�\)@��y@�~�@�?}@�1@�@�x�@�Z@���@��@�O�@�V@���@�bN@��
@�"�@�
=@���@�$�@���@���@���@��@��`@���@��@�bN@�  @�S�@��\@�@���@�?}@��@��@���@��@�I�@�1@��
@��F@���@�"�@���@��R@���@�n�@�E�@��@��@���@�hs@�`B@��@�Ĝ@�C�@�v�@�?}@�bN@�b@�ƨ@���@�l�@�K�@�+@�@���@��\@��@�hs@�?}@��@��`@��9@�r�@�1'@��@��@�1@���@��F@��F@��@��@���@�ȴ@�ȴ@�ȴ@�=q@�hs@���@���@�Z@�  @�ƨ@���@�t�@�C�@�ȴ@�@�`B@�G�@�7L@�&�@�&�@��@�%@��9@��D@��@�z�@�j@�bN@�Q�@�9X@���@�S�@��@�ff@���@�O�@���@�Ĝ@��9@��@��u@�z�@�Z@�A�@� �@�b@�  @��@��
@��@��P@�|�G�O�@��T@��@��@���@xbN@u?}@k33@b�\@Z�H@Q�^@H�u@@bN@:�H@3o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�RB	�RB	�RB	�RB	�RB	�RB	�RB	�RB	�RB	�RB	�RB	�RB	�RB	�LB	�LB	�LB	�RB	�RB	�RB	�RB	�RB	�XB	�^B	�^B	�dB	�}B	��B
�B
�)B,Be`Bp�B��B��B�B��B�;B�BB�B-BC�BA�B?}BP�Be`Bp�Bu�Bt�Bx�Bz�B� B�DB�PB�DB{�B�DB��B�bB�JB�DB�DB�7B�uB��B�+B_;B]/BXBF�B8RB'�B�B�BoBB�B�wB��Bn�BVB
�VB
Q�B
�B	��B	�B	p�B	gmB	O�B	0!B	�B��B��B�B�B�B�B�B�;B�B�B�5B�`B�B�B��B�B�B�B�B	DB	%B	+B	uB	2-B	O�B	k�B	}�B	�oB	��B	��B	�B	�RB	�wB	ǮB	��B	�B	�HB	�TB	�fB	�sB	�sB	�B	��B	��B
B
B
B
DB
{B
�B
�B
�B
 �B
#�B
$�B
-B
-B
.B
0!B
0!B
2-B
33B
6FB
7LB
8RB
7LB
8RB
9XB
9XB
9XB
9XB
8RB
8RB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
7LB
8RB
9XB
8RB
8RB
6FB
7LB
7LB
6FB
6FB
6FB
6FB
6FB
5?B
5?B
49B
49B
49B
49B
33B
2-B
1'B
0!B
/B
.B
-B
-B
,B
+B
+B
,B
+B
+B
)�B
(�B
'�B
'�B
'�B
'�B
&�B
&�B
%�B
%�B
$�B
#�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
uB
uB
uB
uB
uB
oB
oB
oB
hB
hB
bB
bB
bB
bB
\B
\B
VB
VB
PB
PB
JB
DB

=B
	7B
	7B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
+B
+B
+B
%B
B
B
B
B
B	��B	��B	��B	��B	��B
B
B
B
B
B
B
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
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
1B
+B
	7B
	7B
DB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
hB
oB
uB
uB
uB
{B
{B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
"�B
�B
#�B
'�B
+B
1'B
2-B
7LB
<jB
@�B
G�B
N�B
S�B
W
B
\)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B	�+B	�.B	�-B	�-B	�+B	�+B	�+B	�-B	�+B	�+B	�+B	�+B	�,B	�'B	�'B	�(B	�-B	�+B	�+B	�+B	�+B	�0B	�9B	�7B	�>B	�VB	ΰB
��B
��B+�Be0BptB�cB��B��B�NB�	B�YB�BgB,�BCcBAUB?LBP�Be+BpnBu�Bt�Bx�Bz�B�B�B�B�B{�B�B�hB�+B�B�B�B��B�@B�MB��B_B\�BW�BFoB8B'�B�BrB5B�B�gB�;B�qBn]BB
�B
Q�B
�B	��B	��B	pnB	g;B	O�B	/�B	OB��B��B�{B�hB�TB�aB�MB�
B��B��B�B�-B�PB�B��B�tB�bB�dB�nB	B	�B	�B	?B	1�B	O�B	kJB	}�B	�3B	�UB	��B	��B	�B	�9B	�sB	ѭB	��B	�B	�B	�&B	�2B	�5B	�hB	��B	��B
�B
�B
�B
 B
8B
WB
kB
tB
 �B
#�B
$�B
,�B
,�B
-�B
/�B
/�B
1�B
2�B
6 B
7B
8B
7	B
8B
9B
9B
9B
9B
8B
8B
7B
7B
7B
7B
7B
7	B
8B
7B
7B
7B
7B
8B
8B
8B
8B
8B
7B
8B
9B
8B
8B
6 B
7B
7B
6B
6B
6B
6 B
6 B
4�B
4�B
3�B
3�B
3�B
3�B
2�B
1�B
0�B
/�B
.�B
-�B
,�B
,�B
+�B
*�B
*�B
+�B
*�B
*�B
)�B
(�B
'�B
'�B
'�B
'�B
&�B
&�B
%�B
%�B
$�B
#�B
"�B
!�B
 B
xB
rB
mB
aB
]B
UB
MB
GB
HB
HB
CB
BB
CB
:B
4B
5B
4B
4B
/B
/B
/B
/B
0B
(B
*B
(B
!B
"B
B
B
B
B
B
B
B
B
B
	B
B

�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
B
B
B
B
B
B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
$B
&B
&B
B
#B
,B
*B
+B
1B
1B
1B
6B
7B
3B
6B
<B
EB
BB
BB
CB
BB
BB
BB
GB
JB
JB
JB
JB
IB
JB
KB
HB
OB
QB
SB
RB
]B
dB
`B
bB
cB
aB
aB
bB
bB
`B
aB
aB
_B
hB
hB
iB
gG�O�B
nB
#�B
'�B
*�B
0�B
1�B
7B
<B
@7B
GcB
N�B
S�B
V�B
[�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417442016080714174420160807141744  AO  ARCAADJP                                                                    20151228101757    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151228101757  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151228101757  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141744  IP                  G�O�G�O�G�O�                