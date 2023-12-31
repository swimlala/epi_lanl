CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-08T09:16:55Z AOML 3.0 creation; 2016-08-07T21:17:38Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
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
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150708091655  20160807141738  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               <A   AO  5285_8895_060                   2C  D   APEX                            6487                            072314                          846 @�^F[��1   @�^F����@+���n��c�I�^51   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    <A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bw��B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dty�DyFfD�	�D�S3D��fD��fD��D�<�D�s3D��3D�3D�L�D�vfD��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
=@��
A	�A)�AI�Ai�A���A���A���A���A���A���A���A���Bz�B
�GBz�Bz�B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bb�GBjz�Brz�Bz{B�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�
>B�=qB�p�B�=qB�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8�RC:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�\)C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D .D ��D'�D��D.D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9.D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd�De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr!HDr��Ds'�Ds��Dt'�Dt�HDynD�qD�g
D��=D��=D�-qD�P�D��
D��
D�'
D�`�D��=D��
D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�Q�A�=qA�;dA�&�A��HAݑhA݉7A݇+A݃A݁A�z�A�x�A�t�A�n�A�ffA�bNA�ZA��yA�1'A�ffA�ZA�(�A�oA�  A��`Aُ\A��;A�v�A��A���Aҡ�A���A��Aϣ�A�^5A͝�A�?}A���A�1AȼjA�JA�$�A��A�{A�JA�A��jA�n�A�hsA���A��hA�Q�A��^A��yA���A�dZA��HA���A��hA���A�M�A��A�x�A�ZA�A�A�t�A�
=A�Q�A�A��7A��HA��jA�G�A�?}A�G�A�VA��+A��mA��9A�r�A�~�A�+A�ffA���A|�AxȴAq��Am�AdI�A\��AW�AS%ANQ�AJ�yAI|�AH�uAF�9AD�DAC"�AAƨAA7LA@�!A?`BA<=qA8�A6��A5K�A41'A2��A1oA0Q�A/��A-�-A-�
A-�A-?}A-��A.�A-ƨA-�PA-��A-C�A,=qA+�A+�7A+/A*�A+
=A*�9A)dZA(1A'��A'�^A'�7A'dZA&��A&��A&Q�A%�wA%&�A$�/A$r�A$ �A#��A#?}A"ȴA!��A!C�A �uA  �A�;A��A|�A"�A�+A�A��A��A&�AQ�A�A�mAA�^A�FA�7A��A��A�!A��AQ�AAx�AVA�A�uAr�AAl�A��An�A+A��A$�A��A�A��A^5A?}A��A��A�RA$�A��A�-A�hAO�A��A�!Ar�A5?A�#A?}A��AjA$�Ap�A
��A
��A
JA	��A	G�A	oA��A�jA5?A��A��A/A�/A~�AM�A1A��A�A��A�+AbNA��At�A&�A�/AjA{AAC�A ��A A�@�|�@��@�ȴ@��R@��@��7@��/@�1@�C�@��!@�M�@�/@���@�;d@��H@��+@�@��@�Ĝ@�@�  @�\)@���@�ff@��T@��@�9@�(�@�t�@�"�@��@�$�@��#@���@�7L@�Q�@�1@�dZ@��H@�n�@�{@�^@�V@�9X@�t�@�!@�$�@��@�p�@�9@�D@�b@��@�t�@��@���@�V@��@�7@�(�@�
=@��@ݑh@�hs@�7L@��/@���@���@�v�@ٲ-@؛�@�1@�ƨ@�33@�^5@�x�@���@���@�t�@��@���@�ȴ@җ�@�^5@��@ёh@���@�1@ϕ�@�"�@��@�5?@��T@͡�@�x�@�%@̛�@��@�S�@��@��@�5?@ə�@�X@��/@��@�o@Ƨ�@�ff@��@�p�@��@��/@���@ēu@��@�;d@�~�@�^5@��-@�hs@��@��9@�ƨ@���@�@�X@��@��D@��@��F@�+@���@�V@�J@��T@�X@�%@�1'@�dZ@���@�~�@�=q@�$�@��#@��h@�V@�z�@�ƨ@�dZ@��@���@�ff@�@���@���@�x�@�%@�1'@��@�o@��@���@���@���@���@�bN@�1@�ƨ@�t�@���@�n�@�M�@���@�?}@�%@��/@��9@�j@��;@�@���@�v�@�$�@���@��-@�`B@�%@��j@�Q�@� �@���@�\)@�+@�ff@��@��h@�hs@�G�@�/@��j@�bN@�I�@��m@�C�@�"�@��y@�v�@�=q@��@��h@�Ĝ@�  @���@��F@��P@�+@��@��H@��\@�V@�5?@��@���@�7L@���@�1'@�  @���@��;@��
@���@�\)@�
=@���@�n�@��@��^@�X@��@���@�I�@��
@�+@�ȴ@���@���@�ff@��@��@��\@�(�@y�#@r�@h�u@b�@ZJ@S�F@M?}@F��@?�@8bN@0�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�Q�A�=qA�;dA�&�A��HAݑhA݉7A݇+A݃A݁A�z�A�x�A�t�A�n�A�ffA�bNA�ZA��yA�1'A�ffA�ZA�(�A�oA�  A��`Aُ\A��;A�v�A��A���Aҡ�A���A��Aϣ�A�^5A͝�A�?}A���A�1AȼjA�JA�$�A��A�{A�JA�A��jA�n�A�hsA���A��hA�Q�A��^A��yA���A�dZA��HA���A��hA���A�M�A��A�x�A�ZA�A�A�t�A�
=A�Q�A�A��7A��HA��jA�G�A�?}A�G�A�VA��+A��mA��9A�r�A�~�A�+A�ffA���A|�AxȴAq��Am�AdI�A\��AW�AS%ANQ�AJ�yAI|�AH�uAF�9AD�DAC"�AAƨAA7LA@�!A?`BA<=qA8�A6��A5K�A41'A2��A1oA0Q�A/��A-�-A-�
A-�A-?}A-��A.�A-ƨA-�PA-��A-C�A,=qA+�A+�7A+/A*�A+
=A*�9A)dZA(1A'��A'�^A'�7A'dZA&��A&��A&Q�A%�wA%&�A$�/A$r�A$ �A#��A#?}A"ȴA!��A!C�A �uA  �A�;A��A|�A"�A�+A�A��A��A&�AQ�A�A�mAA�^A�FA�7A��A��A�!A��AQ�AAx�AVA�A�uAr�AAl�A��An�A+A��A$�A��A�A��A^5A?}A��A��A�RA$�A��A�-A�hAO�A��A�!Ar�A5?A�#A?}A��AjA$�Ap�A
��A
��A
JA	��A	G�A	oA��A�jA5?A��A��A/A�/A~�AM�A1A��A�A��A�+AbNA��At�A&�A�/AjA{AAC�A ��A A�@�|�@��@�ȴ@��R@��@��7@��/@�1@�C�@��!@�M�@�/@���@�;d@��H@��+@�@��@�Ĝ@�@�  @�\)@���@�ff@��T@��@�9@�(�@�t�@�"�@��@�$�@��#@���@�7L@�Q�@�1@�dZ@��H@�n�@�{@�^@�V@�9X@�t�@�!@�$�@��@�p�@�9@�D@�b@��@�t�@��@���@�V@��@�7@�(�@�
=@��@ݑh@�hs@�7L@��/@���@���@�v�@ٲ-@؛�@�1@�ƨ@�33@�^5@�x�@���@���@�t�@��@���@�ȴ@җ�@�^5@��@ёh@���@�1@ϕ�@�"�@��@�5?@��T@͡�@�x�@�%@̛�@��@�S�@��@��@�5?@ə�@�X@��/@��@�o@Ƨ�@�ff@��@�p�@��@��/@���@ēu@��@�;d@�~�@�^5@��-@�hs@��@��9@�ƨ@���@�@�X@��@��D@��@��F@�+@���@�V@�J@��T@�X@�%@�1'@�dZ@���@�~�@�=q@�$�@��#@��h@�V@�z�@�ƨ@�dZ@��@���@�ff@�@���@���@�x�@�%@�1'@��@�o@��@���@���@���@���@�bN@�1@�ƨ@�t�@���@�n�@�M�@���@�?}@�%@��/@��9@�j@��;@�@���@�v�@�$�@���@��-@�`B@�%@��j@�Q�@� �@���@�\)@�+@�ff@��@��h@�hs@�G�@�/@��j@�bN@�I�@��m@�C�@�"�@��y@�v�@�=q@��@��h@�Ĝ@�  @���@��F@��P@�+@��@��H@��\@�V@�5?@��@���@�7L@���@�1'@�  @���@��;@��
@���@�\)@�
=@���@�n�@��@��^@�X@��@���@�I�@��
@�+@�ȴ@���@���@�ffG�O�@��@��\@�(�@y�#@r�@h�u@b�@ZJ@S�F@M?}@F��@?�@8bN@0�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	H�B	P�B	P�B	T�B	^5B	k�B	k�B	k�B	l�B	l�B	m�B	m�B	n�B	n�B	n�B	o�B	p�B	{�B	�DB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�TB	�yB
�B
_;B
v�B
��B
�fBe`B��BPB�B[#B��B�!B�?BŢB�B��B  BVBhB{B��B�
B��BE�B�BǮB�qBǮB�B��B  BB��B��B�BB��B��BgmB�uB�9B��B�PBl�B$�B
��B
��B
}�B
S�B
5?B
!�B
B	��B	��B	{�B	\)B	)�B��B�HB��B��B��B��B��B��B��B��B�#B�)B�/B�5B�sB�B��B�B��B��B��B	PB	0!B	/B	;dB	O�B	N�B	cTB	� B	��B	��B	�qB	ɺB	�B	�ZB	�`B	�B	��B
B
B
PB
�B
"�B
#�B
$�B
%�B
'�B
+B
,B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
0!B
.B
.B
49B
5?B
5?B
7LB
9XB
9XB
:^B
;dB
<jB
;dB
8RB
9XB
9XB
9XB
9XB
9XB
8RB
9XB
<jB
=qB
@�B
A�B
A�B
B�B
C�B
D�B
D�B
C�B
C�B
B�B
A�B
@�B
?}B
?}B
?}B
>wB
=qB
>wB
=qB
:^B
8RB
6FB
:^B
:^B
;dB
;dB
;dB
:^B
8RB
8RB
8RB
8RB
8RB
8RB
7LB
6FB
5?B
49B
33B
2-B
1'B
0!B
0!B
0!B
/B
/B
/B
/B
.B
.B
-B
,B
,B
+B
)�B
(�B
'�B
'�B
&�B
%�B
$�B
#�B
"�B
!�B
 �B
!�B
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
�B
�B
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
oB
oB
oB
hB
hB
hB
hB
bB
bB
\B
\B
\B
VB
VB
VB
VB
VB
\B
\B
bB
\B
\B
VB
VB
VB
JB
DB

=B
1B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
B
B
%B
%B
B
B
B
B
B
B
B
%B
+B
+B
%B
B
B
B
%B
%B
+B
+B
+B
%B
%B
%B
+B
	7B
1B
1B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
\B
\B
\B
\B
\B
bB
bB
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
oB
oB
uB
oB
uB
uB
oB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
 �B
%�B
+B
2-B
6FB
:^B
B�B
G�B
L�B
O�B
T�B
VB
_;B
dZB
iy1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	H�B	P�B	P�B	T�B	^B	keB	kdB	kdB	ljB	lkB	moB	msB	nvB	nwB	nwB	oB	p�B	{�B	�#B	�cB	�qB	�eB	�]B	�\B	�eB	�kB	�bB	�hB	��B	��B	��B	̢B	�.B	�PB
rB
_B
v�B
�eB
�6Be-B��BB�BZ�B��B��B�B�oB�JB��B��B"B3BHB��B��B��BEjB�RB�xB�:B�wB��B��B��B�B��B��B�BʇB��Bg5B�<B��B��B�BlTB$�B
ϦB
��B
}�B
S�B
5B
!�B
�B	̓B	��B	{�B	[�B	)�B��B�BѼB͢BΧBΦB̜B̝BΧBеB��B��B��B�B�?B�B��B�B��B��B��B	B	/�B	.�B	;+B	O�B	N�B	cB	�B	�tB	��B	�1B	�xB	��B	�B	� B	�dB	��B
 �B
�B
B
]B
"�B
#�B
$�B
%�B
'�B
*�B
+�B
-�B
.�B
.�B
/�B
/�B
/�B
0�B
0�B
/�B
-�B
-�B
3�B
4�B
4�B
7B
9B
9B
:B
;"B
<%B
;!B
8B
9B
9B
9B
9B
9B
8B
9B
<%B
=+B
@=B
AFB
ACB
BKB
CRB
DVB
DYB
CRB
CRB
BMB
AFB
@?B
?8B
?8B
?:B
>2B
=.B
>1B
=.B
:B
8B
6B
:B
:B
; B
; B
;B
:B
8B
8B
8B
8B
8B
8B
7B
6B
4�B
3�B
2�B
1�B
0�B
/�B
/�B
/�B
.�B
.�B
.�B
.�B
-�B
-�B
,�B
+�B
+�B
*�B
)�B
(�B
'�B
'�B
&�B
%�B
$�B
#�B
"�B
!�B
 ~B
!�B
yB
qB
mB
gB
gB
qB
yB
pB
lB
fB
eB
_B
XB
VB
NB
GB
HB
GB
AB
@B
BB
AB
;B
DB
;B
3B
4B
5B
3B
0B
/B
/B
/B
'B
&B
'B
"B
 B
 B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B

�B
	�B
�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B

�B

�B

�B
�B
B
 B
�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
!B
%B
%B
$B
$B
#B
,B
$B
+B
)B
!B
*B
,B
,B
+B
.B
0B
-B
0B
/B
;B
=B
CB
JB
IB
KB
OB
\B
hB
jB
cB
aB
gB
sB
rB
rB
mB
nB
eB
eB
mB
tB
 zB
 yB
 |B
tB
lB
mB
 zB
 yB
 {G�O�B
%�B
*�B
1�B
5�B
:B
BDB
GaB
L�B
O�B
T�B
U�B
^�B
dB
i,1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417382016080714173820160807141738  AO  ARCAADJP                                                                    20150708091655    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150708091655  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150708091655  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141738  IP                  G�O�G�O�G�O�                