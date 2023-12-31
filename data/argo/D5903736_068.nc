CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:42Z AOML 3.0 creation; 2016-05-31T19:14:35Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230542  20160531121436  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               DA   AO  4051_7090_068                   2C  D   APEX                            5368                            041511                          846 @�ߨ�u�1   @�ߩ;�?�@4kI�^�ex���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    DA   B   B   @�ff@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D��D�6fD�y�D���D�fD�I�D�s3D��3D�	�D�,�D��fD��3D���D�<�Dڀ D�� D�fD�33D�|�D�i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@���A z�A z�AB{A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4RD4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�RDy��D��D�7\D�z�D���D�\D�J�D�t)D��)D�
�D�-�D��\D��)D���D�=�Dڀ�D���D�\D�4)D�}�D�j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AÉ7AËDAÏ\AÕ�AÑhAÍPAÓuA×�A×�A×�AÙ�AÛ�AÝ�AÝ�Aß�Aß�Aß�AÝ�AÝ�Aß�Aß�Aã�Aá�AÛ�AÙ�A×�AÙ�AÕ�A�z�A��A�O�A���A���A��A���A��A�  A��wA�t�A�Q�A��yA���A��7A��A�|�A�v�A�XA�K�A�{A�$�A�VA���A���A���A�r�A�  A��uA���A�oA�9XA�oA��A�A���A�C�A�bA���A��-A�9XA��A���A�Q�A�{A���A�z�A���A��+A�x�A�l�A�ZA�A�A�(�A��A���A��hA���A��uA���A�n�A�1A��A�r�A���A���A�ȴA��TA�K�A�~�A��!A��RA�&�A�E�A��\A���A�(�A��
A�VA� �A�5?A�A��A���A���A�7LA���A�ƨA�+A���A��#A�ĜA�/A�r�A��A�`BA}�Ay?}Au�Ar��Aq�^An��Ai��AioAh�Ah��AhE�AhAg�mAgAgx�Afz�AeC�AdffAb�/Ab(�AaƨA`��A_;dA]��AY��AV�+AV1AU�
AT�AS�;AR�\APM�AO%ANQ�AM�mAK�#AJ��AHQ�AF1'AE�AE33AC��ABffAA/A@E�A?G�A>(�A=�-A=�A=K�A=oA<��A<�uA<A�A<�A;�;A:��A9�A7��A61A3�
A3XA37LA3
=A2��A2v�A2=qA1��A1|�A17LA0��A0��A0M�A/��A,�jA+�
A*��A)hsA)O�A)/A(�/A(�\A(jA(A�A'33A&bA$��A#?}A!�A!G�A ��A A��A�A��A1'AA�;At�AI�A��A�A�^A�AffAO�AJA��A�A
�A	��A�jAĜA�A��A�uA��A�hA"�AI�AoA �A Q�@��m@��R@�hs@���@�r�@�b@��y@�V@��@�~�@���@��@�l�@�K�@�;d@��@�o@�
=@�^5@���@蛦@�dZ@��@���@�@��@� �@�P@��y@�7L@���@�9@�A�@��@۝�@�"�@ڸR@�^5@�7L@ج@�j@�1'@��@�  @��
@�-@��T@��@�v�@�M�@�E�@���@̬@�S�@ɑh@�dZ@��@�j@�1@þw@�=q@�Q�@��m@��@�@�%@�K�@��@��+@�5?@��#@�bN@�K�@��R@��@���@���@���@�r�@�j@�j@�bN@�Z@�9X@��
@��P@�|�@�t�@��!@��u@�Q�@�Z@�I�@� �@�b@�b@���@��;@��@�K�@�+@��y@���@��+@�^5@��T@���@�hs@��@��@��u@�1'@��m@���@��+@�V@��D@��@��/@���@�z�@�A�@� �@���@��F@��F@�|�@�l�@�\)@�S�@�K�@�C�@�"�@�ȴ@�^5@�O�@��@���@��D@�Q�@�1@��
@��P@�S�@���@�$�@�?}@���@���@��j@��9@��9@���@�bN@�b@�K�@�n�@�5?@��T@��h@�&�@���@�bN@� �@�b@���@��w@��F@��@���@�l�@�dZ@�S�@�K�@�+@��H@���@��\@��+@��+@��+@�$�@��T@���@��@�O�@�7L@��@���@��j@��u@�j@�Q�@� �@��m@���@�l�@�;d@�E�@��#@���@���@���@���@�x�@�?}@���@�A�@�b@���@�S�@�C�@�33@�@��@��H@���@��+@�~�@�5?@��7@�`B@�/@��@��@�bN@�(�@�  @��@��m@��;@��;@��;@�ƨ@��@��F@��F@���@�|�@�o@�ȴ@��+@�r�@��@���@y&�@m�h@f@]O�@UO�@O��@F��@?
=@:�H@5�-@.��@'
=@�@A�@n�@�@	��@E�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AÉ7AËDAÏ\AÕ�AÑhAÍPAÓuA×�A×�A×�AÙ�AÛ�AÝ�AÝ�Aß�Aß�Aß�AÝ�AÝ�Aß�Aß�Aã�Aá�AÛ�AÙ�A×�AÙ�AÕ�A�z�A��A�O�A���A���A��A���A��A�  A��wA�t�A�Q�A��yA���A��7A��A�|�A�v�A�XA�K�A�{A�$�A�VA���A���A���A�r�A�  A��uA���A�oA�9XA�oA��A�A���A�C�A�bA���A��-A�9XA��A���A�Q�A�{A���A�z�A���A��+A�x�A�l�A�ZA�A�A�(�A��A���A��hA���A��uA���A�n�A�1A��A�r�A���A���A�ȴA��TA�K�A�~�A��!A��RA�&�A�E�A��\A���A�(�A��
A�VA� �A�5?A�A��A���A���A�7LA���A�ƨA�+A���A��#A�ĜA�/A�r�A��A�`BA}�Ay?}Au�Ar��Aq�^An��Ai��AioAh�Ah��AhE�AhAg�mAgAgx�Afz�AeC�AdffAb�/Ab(�AaƨA`��A_;dA]��AY��AV�+AV1AU�
AT�AS�;AR�\APM�AO%ANQ�AM�mAK�#AJ��AHQ�AF1'AE�AE33AC��ABffAA/A@E�A?G�A>(�A=�-A=�A=K�A=oA<��A<�uA<A�A<�A;�;A:��A9�A7��A61A3�
A3XA37LA3
=A2��A2v�A2=qA1��A1|�A17LA0��A0��A0M�A/��A,�jA+�
A*��A)hsA)O�A)/A(�/A(�\A(jA(A�A'33A&bA$��A#?}A!�A!G�A ��A A��A�A��A1'AA�;At�AI�A��A�A�^A�AffAO�AJA��A�A
�A	��A�jAĜA�A��A�uA��A�hA"�AI�AoA �A Q�@��m@��R@�hs@���@�r�@�b@��y@�V@��@�~�@���@��@�l�@�K�@�;d@��@�o@�
=@�^5@���@蛦@�dZ@��@���@�@��@� �@�P@��y@�7L@���@�9@�A�@��@۝�@�"�@ڸR@�^5@�7L@ج@�j@�1'@��@�  @��
@�-@��T@��@�v�@�M�@�E�@���@̬@�S�@ɑh@�dZ@��@�j@�1@þw@�=q@�Q�@��m@��@�@�%@�K�@��@��+@�5?@��#@�bN@�K�@��R@��@���@���@���@�r�@�j@�j@�bN@�Z@�9X@��
@��P@�|�@�t�@��!@��u@�Q�@�Z@�I�@� �@�b@�b@���@��;@��@�K�@�+@��y@���@��+@�^5@��T@���@�hs@��@��@��u@�1'@��m@���@��+@�V@��D@��@��/@���@�z�@�A�@� �@���@��F@��F@�|�@�l�@�\)@�S�@�K�@�C�@�"�@�ȴ@�^5@�O�@��@���@��D@�Q�@�1@��
@��P@�S�@���@�$�@�?}@���@���@��j@��9@��9@���@�bN@�b@�K�@�n�@�5?@��T@��h@�&�@���@�bN@� �@�b@���@��w@��F@��@���@�l�@�dZ@�S�@�K�@�+@��H@���@��\@��+@��+@��+@�$�@��T@���@��@�O�@�7L@��@���@��j@��u@�j@�Q�@� �@��m@���@�l�@�;d@�E�@��#@���@���@���@���@�x�@�?}@���@�A�@�b@���@�S�@�C�@�33@�@��@��H@���@��+@�~�@�5?@��7@�`B@�/@��@��@�bN@�(�@�  @��@��m@��;@��;@��;@�ƨ@��@��F@��F@���@�|�@�o@�ȴG�O�@�r�@��@���@y&�@m�h@f@]O�@UO�@O��@F��@?
=@:�H@5�-@.��@'
=@�@A�@n�@�@	��@E�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�?B�?B�9B�9B�9B�?B�XBBǮB��B�NB�HB��B��B��B��B��B�oB�\B�bB�uB��B��B�{Bs�B[#B[#B\)B[#BXBVBR�BH�B=qB/B'�B&�B(�B'�B'�B%�B)�B33B2-B)�B%�B�B�BuBbBPB1B��B��B��B��B��B��B��B��B�B�B�`B�HB�
B��BȴBŢB�dB�3B��B�PB|�Bq�BcTBR�B33B%�BuB�fBĜB��B�oB�Bs�Be`B\)BL�B9XB%�B\B
�B
�B
��B
�RB
�B
��B
�B
l�B
ffB
ZB
A�B
,B
{B
B	��B	�mB	��B	ǮB	ƨB	ĜB	B	��B	��B	�}B	�jB	�FB	�!B	��B	��B	��B	��B	�uB	�7B	~�B	k�B	^5B	[#B	YB	Q�B	L�B	E�B	;dB	49B	2-B	.B	'�B	!�B	�B	{B	oB	\B	
=B	B	  B��B��B��B��B�B�B�B�B�B�B�B�B�sB�NB�/B�
B��B��B��B��B��B��B��B��BɺBȴBǮBŢBÖB�}B�XB�FB�9B�'B�'B�!B�B�B�B�B�B��B��B��B��B��B��B�{B�\B�7B{�Bt�Bs�Bu�B{�Bu�Bs�Bp�Bt�Bs�Bs�Bs�Bp�Bp�Bp�Bq�Bp�Bq�Bq�Bp�Bp�Bp�Bq�Bq�Bq�Br�Bv�Bv�Bv�Bv�Bv�Bu�Bw�By�By�Bz�B}�B~�B{�By�By�B|�B|�B|�B|�B|�B{�Bz�Bx�By�Bz�B|�B~�B� B�B�B�B�B�+B�7B�=B�JB�VB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�B�B�B�9B�9B�3B�RB�qB�wB�}BĜBǮB��B��B��B��B��B�B�)B�5B�BB�yB��B��B	  B	B	B	B	B	B	1B	DB	DB	DB	\B	�B	!�B	"�B	"�B	#�B	#�B	#�B	$�B	$�B	&�B	(�B	(�B	+B	+B	-B	-B	0!B	2-B	33B	5?B	6FB	8RB	:^B	;dB	<jB	?}B	=qB	?}B	F�B	H�B	J�B	J�B	K�B	L�B	N�B	P�B	T�B	^5B	_;B	_;B	`BB	`BB	aHB	bNB	ffB	jB	m�B	o�B	p�B	p�B	q�B	s�B	t�B	u�B	v�B	z�B	|�B	� B	�B	�B	�%B	�%B	�+B	�1B	�7B	�=B	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�9B	�?B	�FB	�RB	�RB	�XB	�XB	�dB	�jB	�qB	�qB	�wB	�}B	��B	B	ÖB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�HB	�NB	�NB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�yB	�yB	�B	�B	�B	�B	��B	��B
B
VB
�B
!�B
'�B
1'B
6FB
9XB
?}B
F�B
I�B
N�B
T�B
[#B
bNB
iyB
n�B
t�B
w�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�@B�>B�;B�>B�8B�@B�8B�AB�?B�AB�?B�AB�AB�?B�AB�AB�?B�?B�?B�?B�?B�?B�<B�BB�DB�?B�AB�@B�AB�\BBǶB�B�UB�MB��B��B��B��B��B�pB�]B�eB�vB��B��B�Bs�B[#B[(B\+B[ BXBVBR�BH�B=pB/B'�B&�B(�B'�B'�B%�B)�B33B2/B)�B%�B�B�BwB^BOB5B��B��B��B��B��B��B��B��B�B�B�[B�JB�
B��BȳBŞB�dB�/B��B�OB|�Bq�BcTBR�B30B%�BpB�cBĚB��B�kB�Bs�Be^B\'BL�B9WB%�B[B
�B
�B
��B
�RB
�B
��B
�B
l�B
fiB
ZB
A�B
,
B
}B
B	��B	�rB	��B	ǶB	ƭB	ģB	B	��B	��B	��B	�qB	�MB	�*B	�B	��B	��B	��B	�~B	�BB	B	k�B	^>B	[,B	Y"B	Q�B	L�B	E�B	;qB	4BB	28B	. B	'�B	!�B	�B	�B	|B	iB	
IB	'B	 B��B��B��B��B��B�B�B�B�B�B�B�B�B�^B�>B�B��B��B��B��B��B��B��B��B��B��BǽBųBçB��B�hB�XB�JB�5B�6B�/B�&B�$B�B�B�B��B��B��B��B��B��B��B�qB�JB{�Bt�Bs�Bu�B{�Bu�Bs�Bp�Bt�Bs�Bs�Bs�Bp�Bp�Bp�Bq�Bp�Bq�Bq�Bp�Bp�Bp�Bq�Bq�Bq�Br�Bv�Bv�Bv�Bv�Bv�Bu�Bw�By�By�Bz�B~BB{�By�By�B} B|�B} B|�B}B{�Bz�Bx�By�Bz�B}BB�B�B�%B�#B�#B�;B�GB�NB�YB�eB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�*B�0B�*B�B�)B�GB�HB�@B�`B�B��B��BĬBǻB��B��B�B�B�B�"B�5B�BB�PB�B��B��B	 B	B	B	B	B	+B	;B	MB	MB	MB	dB	�B	!�B	"�B	"�B	#�B	#�B	#�B	$�B	$�B	&�B	) B	(�B	+B	+B	-B	-B	0*B	26B	3;B	5HB	6OB	8ZB	:fB	;nB	<qB	?�B	={B	?�B	F�B	H�B	J�B	J�B	K�B	L�B	N�B	P�B	UB	^=B	_BB	_CB	`GB	`KB	aPB	bYB	flB	j�B	m�B	o�B	p�B	p�B	q�B	s�B	t�B	u�B	v�B	z�B	|�B	�B	�B	�"B	�*B	�*B	�/B	�9B	�9B	�DB	�bB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	�B	�B	�3B	�=B	�EB	�JB	�XB	�WB	�]B	�^B	�kB	�oB	�uB	�vB	�}B	��B	��B	B	ÚB	ɽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�#B	�&B	�&B	�+B	�2B	�2B	�2B	�9B	�NB	�OB	�PB	�cB	�cB	�iB	�pB	�rB	�uB	�yB	�vB	�yB	�yB	�{B	�~B	�{B	�|B	�B	�B	�B	�G�O�B	��B
B
WB
�B
!�B
'�B
1(B
6HB
9ZB
?~B
F�B
I�B
N�B
UB
[%B
bOB
iwB
n�B
t�B
w�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.03 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214362016053112143620160531121436  AO  ARCAADJP                                                                    20140721230542    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230542  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230542  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121436  IP                  G�O�G�O�G�O�                