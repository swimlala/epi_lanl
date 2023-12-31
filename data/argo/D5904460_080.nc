CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-21T02:15:37Z AOML 3.0 creation; 2016-08-07T21:17:41Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151021021537  20160807141742  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               PA   AO  5285_8895_080                   2C  D   APEX                            6487                            072314                          846 @�x{b���1   @�x{��g�@+Z��vȴ�c�V�u1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    PA   B   B   @�ff@�  A   A   A@  A`  A���A���A���A�33A�33A���A�  A�33B   B
ffB33B  B   B(  B0  B8  B@  BHffBP��BW33B_��Bg��Bp  Bx  B�  B�33B�  B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�fD�fD�P D�p D��fD�3D�I�D�s3D�� D�#3D�\�D�s3Dǳ3D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�=p@��
A	�A)�AI�Ai�A�A�A�A�(�A�(�A�A���A�(�Bz�B�GB�Bz�B"z�B*z�B2z�B:z�BBz�BJ�GBSG�BY�Bb{Bj{Brz�Bzz�B�=qB�p�B�=qB�p�B�
>B�
>B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�
>B�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�\)C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�B�C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D �D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"�D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN.DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt��Dt��Dy�D�*=D�c�D���D��=D�'
D�]qD��
D���D�7
D�p�D��
D��
D�S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�jA�`BA�Q�A�A�A�=qA�7LA�1'A�"�A��A���A�
=Aȴ9AōPA�+A�A�oA�XA�K�A���A�E�A��wA�E�A�S�A�`BA�$�A���A��7A�jA���A�ƨA���A�+A���A�+Az��Av��Aq�Am��Ak�PAi��Ag7LAe�Ad�AcƨA`�AY��AV��AS�TASp�ASx�AS�AR�DAQ�-AP1AO�7AO�AN~�AMx�AK�#AI
=AF{AD�uAB��A<�!A5�hA2��A2=qA2  A0  A)�^A&A�A$E�A ��AC�AVA�A�`A�-A��AE�A�A�DA�AȴA$�A�
A��A%A�7AAE�AjA�A/AjA�
A?}A�A�A�PA��A�
A�A
v�A
$�A	�TA	\)A�A��AVA�HA?}A�A��A	dZA	"�A&�A�jA	?}A	�;A	oA	|�A
1'A
1'A	��A	�hA	p�A	?}A	VA��A�7A��A��A�A�^A�A��A�7A\)A�A��A��A�AVA-AAAoA  A�A7LA �yA ��A =qA {@�|�@���@�V@�hs@�X@�?}@�z�@�ƨ@��H@�ȴ@�@��@���@���@��@�Q�@�  @��F@�t�@�C�@�"�@���@�^5@��#@�hs@�7L@�z�@�F@���@�n�@���@���@�dZ@�^5@�?}@��/@��@��m@�@��H@陚@�Q�@�+@�X@��@�Ĝ@�ƨ@�33@��@�n�@���@�1@�l�@�K�@ޟ�@���@��@݉7@ܼj@��@�dZ@��H@ڗ�@�^5@�^5@�V@�{@��#@٩�@�p�@��@�%@��/@�I�@�dZ@���@�v�@�@Ցh@��`@�I�@��@Ӿw@�|�@�C�@�@ҟ�@��#@�G�@мj@�1@Ͼw@���@�J@ͺ^@�x�@�/@���@�I�@ˮ@�l�@�"�@�ȴ@�$�@�`B@ȴ9@��@Ǖ�@�~�@ř�@��`@�bN@� �@��
@�l�@��y@�~�@�M�@�=q@�{@��@�@��7@��`@�Q�@��m@�dZ@�"�@���@���@���@��j@�bN@��
@�C�@���@�V@��@�X@���@��u@�(�@��F@�"�@��R@�^5@��@��-@�x�@�G�@�j@��@�;d@��@�^5@�$�@��@���@�%@�Z@�"�@��y@���@�{@�O�@��@���@��9@��D@�(�@���@��F@�o@��@���@���@��@��H@��\@�M�@��@��-@�?}@�O�@�V@���@���@�Z@�ƨ@�;d@�@���@�E�@��#@��^@��@�7L@���@��j@�r�@��@�ƨ@��w@���@�K�@��R@���@�^5@�M�@�5?@��^@��h@�p�@�X@���@���@��@��D@�1'@�ƨ@���@���@��P@��P@��@�|�@�C�@�ff@���@�p�@��j@�I�@�(�@�1@��
@�|�@�"�@���@��+@�5?@��@��#@���@�7L@��@�1'@��m@���@��@��y@��!@�v�@�5?@�@��7@�hs@�O�@�?}@���@���@�Q�@��w@�"�@��@��!@��\@�n�@�E�@��@��@�7L@�&�@��@��u@�I�@��@���@�l�@�;d@��y@��!@���@�ff@�{@���@���@��@�?}@��`@�Ĝ@���@�Z@��F@���@��@�;d@��H@�ȴ@���@�v�@�{@��^@��7@�/@��@�1'@��m@��@�K�@��@���@�n�@�=q@�{@���@�X@�%@��@��@�Q�@��@���@�t�@�C�@�
=@���@�~�@���@���@���@���@�V@�z�@
=@u�@nV@g�@_
=@T�/@N��@G;d@>{@6@0�@+�m@&@!�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A�t�A�jA�`BA�Q�A�A�A�=qA�7LA�1'A�"�A��A���A�
=Aȴ9AōPA�+A�A�oA�XA�K�A���A�E�A��wA�E�A�S�A�`BA�$�A���A��7A�jA���A�ƨA���A�+A���A�+Az��Av��Aq�Am��Ak�PAi��Ag7LAe�Ad�AcƨA`�AY��AV��AS�TASp�ASx�AS�AR�DAQ�-AP1AO�7AO�AN~�AMx�AK�#AI
=AF{AD�uAB��A<�!A5�hA2��A2=qA2  A0  A)�^A&A�A$E�A ��AC�AVA�A�`A�-A��AE�A�A�DA�AȴA$�A�
A��A%A�7AAE�AjA�A/AjA�
A?}A�A�A�PA��A�
A�A
v�A
$�A	�TA	\)A�A��AVA�HA?}A�A��A	dZA	"�A&�A�jA	?}A	�;A	oA	|�A
1'A
1'A	��A	�hA	p�A	?}A	VA��A�7A��A��A�A�^A�A��A�7A\)A�A��A��A�AVA-AAAoA  A�A7LA �yA ��A =qA {@�|�@���@�V@�hs@�X@�?}@�z�@�ƨ@��H@�ȴ@�@��@���@���@��@�Q�@�  @��F@�t�@�C�@�"�@���@�^5@��#@�hs@�7L@�z�@�F@���@�n�@���@���@�dZ@�^5@�?}@��/@��@��m@�@��H@陚@�Q�@�+@�X@��@�Ĝ@�ƨ@�33@��@�n�@���@�1@�l�@�K�@ޟ�@���@��@݉7@ܼj@��@�dZ@��H@ڗ�@�^5@�^5@�V@�{@��#@٩�@�p�@��@�%@��/@�I�@�dZ@���@�v�@�@Ցh@��`@�I�@��@Ӿw@�|�@�C�@�@ҟ�@��#@�G�@мj@�1@Ͼw@���@�J@ͺ^@�x�@�/@���@�I�@ˮ@�l�@�"�@�ȴ@�$�@�`B@ȴ9@��@Ǖ�@�~�@ř�@��`@�bN@� �@��
@�l�@��y@�~�@�M�@�=q@�{@��@�@��7@��`@�Q�@��m@�dZ@�"�@���@���@���@��j@�bN@��
@�C�@���@�V@��@�X@���@��u@�(�@��F@�"�@��R@�^5@��@��-@�x�@�G�@�j@��@�;d@��@�^5@�$�@��@���@�%@�Z@�"�@��y@���@�{@�O�@��@���@��9@��D@�(�@���@��F@�o@��@���@���@��@��H@��\@�M�@��@��-@�?}@�O�@�V@���@���@�Z@�ƨ@�;d@�@���@�E�@��#@��^@��@�7L@���@��j@�r�@��@�ƨ@��w@���@�K�@��R@���@�^5@�M�@�5?@��^@��h@�p�@�X@���@���@��@��D@�1'@�ƨ@���@���@��P@��P@��@�|�@�C�@�ff@���@�p�@��j@�I�@�(�@�1@��
@�|�@�"�@���@��+@�5?@��@��#@���@�7L@��@�1'@��m@���@��@��y@��!@�v�@�5?@�@��7@�hs@�O�@�?}@���@���@�Q�@��w@�"�@��@��!@��\@�n�@�E�@��@��@�7L@�&�@��@��u@�I�@��@���@�l�@�;d@��y@��!@���@�ff@�{@���@���@��@�?}@��`@�Ĝ@���@�Z@��F@���@��@�;d@��H@�ȴ@���@�v�@�{@��^@��7@�/@��@�1'@��m@��@�K�@��@���@�n�@�=q@�{@���@�X@�%@��@��@�Q�@��@���@�t�@�C�@�
=@���@�~�@���@���@���@���@�VG�O�@
=@u�@nV@g�@_
=@T�/@N��@G;d@>{@6@0�@+�m@&@!�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�1B�7B�7B�+B�B�%B�%B�+B�1BɺBE�B��B	�ZB
1'B
� B
��B
�VB
r�B
[#B
R�B
M�B
F�B
33B	��B	��B	B	�-B	�{B	�B	w�B	q�B	l�B	iyB	dZB	VB	H�B	G�B	A�B	;dB	5?B	.B	)�B	&�B	#�B	�B	oB	B	B	{B	 �B	%�B	$�B	)�B	M�B	cTB	ffB	gmB	iyB	n�B	q�B	k�B	^5B	T�B	J�B	(�B	hB	�B	-B	:^B	.B��B��BÖB�3B�B��B��B�9B��B	bB	&�B	.B	.B	#�B	#�B	$�B	'�B	&�B	#�B	'�B	-B	-B	%�B	%�B	%�B	"�B	 �B	 �B	#�B	&�B	'�B	&�B	'�B	/B	1'B	33B	2-B	/B	(�B	%�B	&�B	5?B	>wB	O�B	e`B	t�B	r�B	gmB	�B	��B	�3B	�B	�FB	�}B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ÖB	�}B	�qB	�wB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�ZB	�ZB	�`B	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
B
B
%B
+B
+B
1B
1B
1B
+B
%B
%B
+B
+B
+B
1B
1B
1B
	7B

=B

=B
JB
PB
VB
PB
PB
PB
PB
VB
\B
\B
\B
bB
hB
hB
hB
hB
oB
uB
uB
uB
uB
uB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
!�B
"�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
-B
-B
/B
/B
0!B
/B
/B
0!B
0!B
0!B
1'B
49B
49B
:^B
?}B
B�B
H�B
P�B
T�B
ZB
^5B
bNB
gmB
jB
n�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B�B�	B�B��B��B��B��B��B�BɋBEqB��B	�0B
0�B
�B
��B
�$B
rB
Z�B
R�B
M�B
FwB
3B	��B	ʔB	�_B	��B	�KB	��B	w�B	qyB	l[B	iIB	d,B	U�B	H�B	GB	AXB	;2B	5B	-�B	)�B	&�B	#�B	~B	<B	�B	 �B	HB	 �B	%�B	$�B	)�B	M�B	cB	f/B	g6B	iDB	naB	qqB	kMB	]�B	T�B	J�B	(�B	0B	\B	,�B	:%B	-�B��B��B�aB��B��B��B��B�BϨB	(B	&�B	-�B	-�B	#�B	#�B	$�B	'�B	&�B	#�B	'�B	,�B	,�B	%�B	%�B	%�B	"�B	 �B	 �B	#�B	&�B	'�B	&�B	'�B	.�B	0�B	2�B	1�B	.�B	(�B	%�B	&�B	5 B	>6B	O�B	eB	tB	roB	g,B	��B	�DB	��B	��B	�B	�8B	�RB	�ZB	�YB	�`B	�_B	�`B	�SB	�<B	�/B	�4B	�_B	�vB	�~B	�B	�|B	�~B	�~B	̈B	͏B	͏B	ϚB	РB	ѨB	ѧB	РB	ϙB	РB	ѥB	ѦB	РB	ѨB	ѧB	ТB	ϚB	ΒB	ΔB	ΔB	ΗB	̈B	͌B	ϙB	ҰB	ѧB	ѨB	ԺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	վB	տB	վB	ԹB	ԹB	ӴB	ҭB	ҬB	ԺB	վB	��B	��B	��B	��B	��B	��B	��B	��B	ռB	ԷB	ӳB	ӴB	ӱB	ӳB	սB	ջB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�,B	�0B	�1B	�5B	�6B	�4B	�EB	�[B	�[B	�\B	�\B	�ZB	�aB	�\B	�aB	�bB	�aB	�aB	�bB	�`B	�gB	�bB	�`B	�cB	�[B	�VB	�SB	�VB	�TB	�ZB	�^B	�aB	�`B	�dB	�oB	�sB	�tB	�rB	�zB	�yB	�zB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	�B	�B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
 �B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
#B
*B
+B
+B
+B
+B
!B
$B
"B
+B
(B
.B
:B
?B
AB
@B
BB
@B
IB
PB
NB
QB
^B
RB
SB
[B
[B
_B
`B
`B
`B
_B
hB
kB
sB
sB
 yB
 zB
 zB
 |B
 yB
 {B
 zB
 |B
!B
!~B
!}B
!~B
!}B
!~B
!B
"�B
"�B
!B
!�B
!|B
!~B
!~B
"�B
"�B
!B
"�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
.�B
.�B
/�B
.�B
.�B
/�B
/�B
/�B
0�G�O�B
3�B
:B
?0B
BBB
HgB
P�B
T�B
Y�B
]�B
b B
gB
j0B
nJB
sh11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417422016080714174220160807141742  AO  ARCAADJP                                                                    20151021021537    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151021021537  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151021021537  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141742  IP                  G�O�G�O�G�O�                