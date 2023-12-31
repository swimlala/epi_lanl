CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-08T19:17:22Z AOML 3.0 creation; 2016-08-07T21:17:39Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150808191722  20160807141739  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               BA   AO  5285_8895_066                   2C  D   APEX                            6487                            072314                          846 @�f"ܺ�1   @�f#@@+bM���c��1'1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    BA   B   B   @9��@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���A�33B  B  B  B   B(  B0  B8  B@ffBI��BO33BW��B`  Bh  Bp  Bx  B�  B�33B�33B���B�  B�  B�  B�  B�  B���B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�33Bԙ�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy�fD� D�@ D�� D���D�  D�I�D�y�D��3D��D�L�D�l�Dǰ D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @aG�@��
@�
=A	�A)�AI�Ai�A���A���A���A���A���A���A�A�B{B
z�Bz�Bz�B"z�B*z�B2z�B:z�BB�GBL{BQ�BZ{Bbz�Bjz�Brz�Bzz�B�=qB�p�B�p�B�
>B�=qB�=qB�=qB�=qB�=qB�
>B�=qB�p�B�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B��B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C�RC�RC��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�\)C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D!HD��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt�Dy�D�#�D�S�D���D��D�3�D�]qD��qD��
D�-qD�`�D���D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A㟾A�t�A��
A���A���A��A�9XA�v�A�x�A�5?Aؕ�A��#A�oAГuA�ĜA�(�A͗�A�$�Ả7A���A�"�Aɟ�AȋDA��AǁA��mAŲ-A���A�33AA�t�A�APA�VA���A���A��FA�S�A��A�?}A���A�+A�jA��HA��jA���A�z�A���A�(�A���A�ZA�A�-A��uA���A�(�A��-A��#A���A��^A�/A�(�A{l�An�HAghsAd�RAa�A\�/AW7LAR�\AOO�AK�FAHz�AF��AF�\AC�
A@=qA=��A<�uA;�A:Q�A8�+A6~�A3��A2�A1�A1C�A0ĜA/G�A.��A-�A-�A-|�A*ZA&��A&�DA'G�A'��A'�7A'?}A'�A%��A$9XA$$�A#�;A!S�AȴA7LA33A�`AS�AjA�;A��A`BA��A(�A�A(�A�A��A�-A$�Av�A-AK�AAv�A(�A�RA��A��A1'Al�A&�A��An�A5?A(�AJA��AhsA%AE�A��Al�A��A1'AAA�A�A�A��AI�A�wA�HA�RA�AVAVA�A7LAXA
��A	�#A	x�A	�A��AffA�hA7LA/A&�A�yA�!A9XA��A�RAffAbAAdZA�A�`AffA��A�PA �j@�\)@�ff@��T@�5?@��7@�&�@�z�@��;@�|�@�C�@�J@���@��@�A�@��m@���@�S�@��+@��T@��@�@�o@�ff@�-@�Z@��
@�
=@��@���@�7@웦@�F@�dZ@�"�@�$�@�?}@���@�D@�1'@�F@��H@��@�7L@�@�33@���@◍@��#@���@��@�  @���@�-@�7L@���@� �@ە�@��@�@ف@�/@��/@�j@ם�@�
=@�=q@�O�@ԛ�@�1'@ӥ�@�C�@҇+@���@�`B@�/@��@���@� �@�"�@��@́@�&�@�j@�b@�dZ@���@�-@�@��@ɉ7@���@ȴ9@ț�@ȃ@�I�@�ƨ@�dZ@��@��H@Ə\@�@�7L@���@�bN@���@�|�@�\)@�;d@���@�@�@�{@���@�p�@�/@��@��D@�A�@��w@�t�@�33@�
=@���@�E�@���@��T@�hs@�%@���@���@�1'@���@�"�@�ȴ@�E�@���@�x�@�hs@�X@�&�@��`@�Q�@�t�@��H@��!@�E�@��@��#@��-@�/@��D@��w@�dZ@�C�@�o@�ff@��#@��^@��@��@���@��D@�9X@�1@��F@�"�@�~�@�5?@��@���@�x�@�/@���@���@�Ĝ@��9@��@���@��@�r�@�A�@���@�"�@��y@��\@�$�@���@��@�G�@��@��/@��D@� �@��;@�ƨ@�33@���@��+@�=q@�@���@���@���@�hs@���@��u@�z�@�j@�(�@���@���@�-@��^@�/@��@���@���@�bN@�1'@�  @��;@��w@��P@�\)@�C�@�+@�
=@��H@��!@�v�@�{@��@���@�O�@�%@��9@�r�@�Q�@���@�S�@���@���@��+@�V@�$�@��T@���@�`B@��@��/@�Z@�A�@�1'@�1@���@���@�+@��y@�^5@���@��h@�`B@���@���@�bN@�b@���@��m@���@���@�|�@�o@��R@��\@�v�@�E�@�{@���@���@�p�@�V@�Ĝ@�j@�1@��F@��P@�$�@�E�@�9X@}��@q��@g��@\�D@S�m@J�@>v�@6ff@0bN@,I�@(1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A��A��A��A��A��A���A���A���A���A���A���A���A���A���A㟾A�t�A��
A���A���A��A�9XA�v�A�x�A�5?Aؕ�A��#A�oAГuA�ĜA�(�A͗�A�$�Ả7A���A�"�Aɟ�AȋDA��AǁA��mAŲ-A���A�33AA�t�A�APA�VA���A���A��FA�S�A��A�?}A���A�+A�jA��HA��jA���A�z�A���A�(�A���A�ZA�A�-A��uA���A�(�A��-A��#A���A��^A�/A�(�A{l�An�HAghsAd�RAa�A\�/AW7LAR�\AOO�AK�FAHz�AF��AF�\AC�
A@=qA=��A<�uA;�A:Q�A8�+A6~�A3��A2�A1�A1C�A0ĜA/G�A.��A-�A-�A-|�A*ZA&��A&�DA'G�A'��A'�7A'?}A'�A%��A$9XA$$�A#�;A!S�AȴA7LA33A�`AS�AjA�;A��A`BA��A(�A�A(�A�A��A�-A$�Av�A-AK�AAv�A(�A�RA��A��A1'Al�A&�A��An�A5?A(�AJA��AhsA%AE�A��Al�A��A1'AAA�A�A�A��AI�A�wA�HA�RA�AVAVA�A7LAXA
��A	�#A	x�A	�A��AffA�hA7LA/A&�A�yA�!A9XA��A�RAffAbAAdZA�A�`AffA��A�PA �j@�\)@�ff@��T@�5?@��7@�&�@�z�@��;@�|�@�C�@�J@���@��@�A�@��m@���@�S�@��+@��T@��@�@�o@�ff@�-@�Z@��
@�
=@��@���@�7@웦@�F@�dZ@�"�@�$�@�?}@���@�D@�1'@�F@��H@��@�7L@�@�33@���@◍@��#@���@��@�  @���@�-@�7L@���@� �@ە�@��@�@ف@�/@��/@�j@ם�@�
=@�=q@�O�@ԛ�@�1'@ӥ�@�C�@҇+@���@�`B@�/@��@���@� �@�"�@��@́@�&�@�j@�b@�dZ@���@�-@�@��@ɉ7@���@ȴ9@ț�@ȃ@�I�@�ƨ@�dZ@��@��H@Ə\@�@�7L@���@�bN@���@�|�@�\)@�;d@���@�@�@�{@���@�p�@�/@��@��D@�A�@��w@�t�@�33@�
=@���@�E�@���@��T@�hs@�%@���@���@�1'@���@�"�@�ȴ@�E�@���@�x�@�hs@�X@�&�@��`@�Q�@�t�@��H@��!@�E�@��@��#@��-@�/@��D@��w@�dZ@�C�@�o@�ff@��#@��^@��@��@���@��D@�9X@�1@��F@�"�@�~�@�5?@��@���@�x�@�/@���@���@�Ĝ@��9@��@���@��@�r�@�A�@���@�"�@��y@��\@�$�@���@��@�G�@��@��/@��D@� �@��;@�ƨ@�33@���@��+@�=q@�@���@���@���@�hs@���@��u@�z�@�j@�(�@���@���@�-@��^@�/@��@���@���@�bN@�1'@�  @��;@��w@��P@�\)@�C�@�+@�
=@��H@��!@�v�@�{@��@���@�O�@�%@��9@�r�@�Q�@���@�S�@���@���@��+@�V@�$�@��T@���@�`B@��@��/@�Z@�A�@�1'@�1@���@���@�+@��y@�^5@���@��h@�`B@���@���@�bN@�b@���@��m@���@���@�|�@�o@��R@��\@�v�@�E�@�{@���@���@�p�@�V@�Ĝ@�j@�1@��FG�O�@�$�@�E�@�9X@}��@q��@g��@\�D@S�m@J�@>v�@6ff@0bN@,I�@(1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�%B	�+B	�+B	�+B	�+B	�+B	�+B	�+B	�+B	�%B	�%B	�%B	�%B	�%B	|�B	l�B	dZB	p�B	�hB	��B	�
B	�mB	��B
{B
�B
bB
B	��B	��B
�B
/B
;dB
H�B
M�B
W
B
��B
�-B
�?B
�jB
��B
��B�-B  BB+BM�BP�BG�B�B	7B��B��B�^B��B��B��B��B��B�\B�B}�By�B[#B5?BVB
�HB
�}B
��B
�bB
�7B
�%B
w�B
o�B
x�B
YB
uB	�oB	>wB	�B	bB	$�B	�B	  B�B�HB��B��B��B��B��B�B�/B�NB�NB�B�B��B	bB	�B	+B	1'B	6FB	<jB	:^B	A�B	K�B	]/B	J�B	%�B	7LB	[#B	n�B	� B	�B	�B	}�B	}�B	}�B	�B	r�B	XB	O�B	W
B	]/B	gmB	�1B	�PB	�qB	��B	�dB	�LB	�-B	�B	�B	��B	�FB	��B	�B	�5B	�B	��B	ȴB	��B	�5B	�ZB	�/B	��B	��B	��B	��B	��B	��B	�
B	�5B	�/B	�/B	�)B	�B	�B	�/B	�/B	�)B	�/B	�/B	�/B	�/B	�/B	�;B	�fB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�sB	�mB	�mB	�fB	�sB	�sB	�mB	�fB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
	7B
	7B

=B

=B

=B
DB
DB

=B

=B
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
	7B
	7B

=B
DB
DB
DB
DB
PB
VB
\B
\B
bB
bB
bB
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
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
hB
hB
oB
oB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
%�B
+B
0!B
6FB
@�B
F�B
K�B
Q�B
VB
^5B
bNB
ffB
iyB
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B	�B	�	B	�B	�B	�B	�B	�	B	�	B	�B	�B	�B	�B	�B	�B	|�B	ljB	d;B	p�B	�EB	��B	��B	�IB	��B
SB
YB
9B
 �B	��B	��B
\B
.�B
;9B
H�B
M�B
V�B
�XB
��B
�B
�;B
��B
��B��B��B�B*�BM�BP�BGyBTB	B��BΦB�)B��B��B��B��B�sB�#B��B}�By�BZ�B5BB
�B
�FB
��B
�(B
�B
��B
w�B
ofB
x�B
X�B
AB	�:B	>GB	�B	/B	$�B	]B��B�mB�BѺB��BѺBϰBϯB��B��B�B�B�MB�B��B	)B	�B	*�B	0�B	6B	<1B	:%B	AQB	K�B	\�B	J�B	%�B	7B	Z�B	n[B	�B	��B	��B	}�B	}�B	}�B	��B	rrB	W�B	O�B	V�B	\�B	g1B	��B	�B	�2B	�BB	�$B	�B	��B	��B	��B	��B	�B	͑B	��B	��B	��B	ΗB	�rB	̌B	��B	�B	��B	ӶB	ΙB	͑B	̋B	˄B	͔B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	�GB	�;B	�ZB	�HB	�:B	�AB	�`B	�xB	��B	��B	��B	��B	��B	�wB	�eB	�bB	�_B	�dB	�eB	�qB	�lB	�`B	�MB	�BB	�AB	�FB	�SB	�eB	�hB	�jB	�_B	�NB	�FB	�/B	�B	�B	�#B	�HB	�LB	�NB	�KB	�9B	�@B	�EB	�FB	�@B	�AB	�9B	�:B	�?B	�>B	�DB	�9B	�9B	�>B	�@B	�8B	�-B	�.B	�'B	�$B	� B	�.B	�,B	�(B	�B	�5B	�@B	�2B	�9B	�@B	�FB	�?B	�6B	�?B	�?B	�>B	�MB	�@B	�KB	�PB	�PB	�?B	�EB	�EB	�JB	�FB	�KB	�JB	�KB	�QB	�WB	�XB	�[B	�[B	�\B	�[B	�bB	�cB	�cB	�aB	�dB	�bB	�cB	�\B	�ZB	�[B	�ZB	�[B	�\B	�VB	�OB	�RB	�QB	�IB	�KB	�JB	�IB	�IB	�IB	�IB	�JB	�PB	�LB	�VB	�VB	�XB	�UB	�WB	�ZB	�]B	�]B	�\B	�\B	�bB	�aB	�cB	�cB	�aB	�fB	�aB	�`B	�cB	�nB	�tB	�tB	�sB	�pB	�oB	�lB	�mB	�mB	�mB	�qB	�mB	�nB	�tB	�zB	�yB	�|B	�{B	��B	��B	��B	�{B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
	�B

�B

�B

�B

�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
#B
#B
#B
-B
,B
+B
.B
*B
+B
/B
8B
6B
7B
;B
6B
<B
=B
BB
AB
DB
IB
JB
GB
MB
HB
IB
OB
JB
OB
MB
VB
TB
TB
UB
[B
_B
`B
bB
`B
fB
iB
sB
rB
rB
qB
 yB
 yB
qB
sB
sB
tB
uB
rB
 yB
!~G�O�B
%�B
*�B
/�B
5�B
@9B
F]B
KzB
Q�B
U�B
]�B
bB
fB
i+B
mE11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417392016080714173920160807141739  AO  ARCAADJP                                                                    20150808191722    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150808191722  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150808191722  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141739  IP                  G�O�G�O�G�O�                