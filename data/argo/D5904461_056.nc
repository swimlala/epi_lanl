CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-24T19:17:09Z AOML 3.0 creation; 2016-08-07T21:36:36Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150624191709  20160807143636  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               8A   AO  5286_8897_056                   2C  D   APEX                            6531                            072314                          846 @�Z�O���1   @�Z��s��@1ٙ�����cu/��w1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    8A   B   B   @@  @�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy� D�	�D�6fD���D���D�  D�@ D�� D���D�3D�VfD�vfD��fD� D�\�Dډ�D�� D��D�<�D�i�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @C�
@��@��A ��A ��A@��Ab�]A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB��B=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh��Bo�Bx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^(�C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D
=D�=D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN}qDO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D��D�8RD���D�޹D��D�A�D���D�ιD�D�XRD�xRD��RD��D�^�Dڋ�D���D��D�>�D�k�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AԴ9AԴ9AԶFAԴ9AԸRAԴ9AԶFAԶFAԺ^A�ĜA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���AԬAԡ�A�\)A��AӾwA�hsA���A��A�jA�~�A�bA��TA�?}AʅA���A��A�t�A���Aǡ�A��`A�  A��;A�Aŗ�A�z�A�n�A�^5A���A�&�A�1A�K�A�9XA���A��A���A��A�A�A���A��7A�S�A���A��9A�M�A��yA��+A�~�A��A�n�A�(�A�\)A��/A�C�A��RA��7A���A�(�A�{A�C�A��A� �A��^A��`A�C�A�t�A���A���A�l�A�7LA�bA��PA�"�A�dZA�I�A��A�{A��A�1'A�%A���A�Q�A��^A�=qA�5?A���A��A�VA���A�&�A�ƨA~1'Aw�FAp��Al1'Ah��Af1'Ac��A_?}A[�AXbNAV �ATffAS?}AQ��AP��ANȴAMl�AK33AIx�AH9XAFv�ADQ�AC�AB��AB��AB�\AB��AAdZA@�!A@I�A?�7A>~�A=�A:�jA5��A3��A1��A0n�A/�wA.�jA,bNA*z�A*ZA)��A'dZA(�DA'�FA%��A#dZA �\A1A�RA�/AoA��A�HA+A�A�A�/AXA=qA �A�FA��A��Al�A?}AA�DA �A�#A��AO�A�jAv�A$�A&�A	;dA��A�uAI�A%AA�FA`BA
=AG�A�;A{AM�A��A{A��AC�Az�A�TA&�A ff@�@���@��@�
=@��@���@�Ĝ@�Q�@��@�ȴ@���@�l�@�=q@�w@�R@���@�@�@�v�@��#@���@��@�@��@��#@��@��@��H@��
@��@旍@�Ĝ@�S�@�33@��#@�A�@��H@�-@݁@��`@�hs@޸R@�-@���@�o@�
=@ܬ@��
@�V@�5?@އ+@ޏ\@�V@އ+@�E�@ݲ-@�G�@��;@�@��/@��@���@ش9@���@��/@أ�@�b@׾w@��@�$�@�7L@Ӆ@ҧ�@�M�@҇+@҇+@��T@�x�@ёh@щ7@�G�@Гu@�S�@�E�@�5?@�-@Ͳ-@��T@�@��#@�7L@̓u@��@˾w@�dZ@�$�@���@�X@��@�Z@�(�@��@�b@Ǿw@���@Ɨ�@��@ź^@őh@�hs@�x�@�7L@�Ĝ@öF@�t�@�S�@�v�@�-@���@�p�@�&�@��/@�r�@�1'@���@�@�E�@���@��#@��7@��@���@�A�@��@���@�\)@�o@��@��@���@�J@�/@���@�Q�@�A�@��w@�dZ@�C�@�ȴ@�$�@���@��7@�V@��u@�9X@��D@�r�@�j@��u@� �@��;@��P@��@�^5@�@���@�/@��@���@��@�1'@�|�@�"�@��@��@���@�J@�7L@���@�b@��w@��@�\)@�
=@��@�~�@�5?@���@�G�@��j@���@���@�r�@�Q�@���@�ƨ@�ƨ@��P@�dZ@��H@���@�n�@�M�@�$�@���@�7L@��/@��D@�r�@�Q�@�(�@��
@��w@�dZ@�@��@���@���@�~�@�v�@�ff@�5?@��-@�G�@��9@�(�@��m@��P@�@��@��y@��@���@��\@�~�@�5?@�J@��@��#@���@�&�@��/@���@��j@�j@� �@�b@��
@��@���@�5?@���@��@��T@�hs@��/@�Ĝ@��u@��@�C�@���@��+@�^5@�-@���@��h@�x�@�p�@�`B@�?}@���@���@�A�@�  @�t�@��@���@���@�V@�\)@��u@�S�@}�@st�@j�!@c"�@W��@O��@E�@=O�@7|�@.V@'l�@#dZ@ 1'@��@�u@�
@�P@ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AԴ9AԴ9AԶFAԴ9AԸRAԴ9AԶFAԶFAԺ^A�ĜA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���AԬAԡ�A�\)A��AӾwA�hsA���A��A�jA�~�A�bA��TA�?}AʅA���A��A�t�A���Aǡ�A��`A�  A��;A�Aŗ�A�z�A�n�A�^5A���A�&�A�1A�K�A�9XA���A��A���A��A�A�A���A��7A�S�A���A��9A�M�A��yA��+A�~�A��A�n�A�(�A�\)A��/A�C�A��RA��7A���A�(�A�{A�C�A��A� �A��^A��`A�C�A�t�A���A���A�l�A�7LA�bA��PA�"�A�dZA�I�A��A�{A��A�1'A�%A���A�Q�A��^A�=qA�5?A���A��A�VA���A�&�A�ƨA~1'Aw�FAp��Al1'Ah��Af1'Ac��A_?}A[�AXbNAV �ATffAS?}AQ��AP��ANȴAMl�AK33AIx�AH9XAFv�ADQ�AC�AB��AB��AB�\AB��AAdZA@�!A@I�A?�7A>~�A=�A:�jA5��A3��A1��A0n�A/�wA.�jA,bNA*z�A*ZA)��A'dZA(�DA'�FA%��A#dZA �\A1A�RA�/AoA��A�HA+A�A�A�/AXA=qA �A�FA��A��Al�A?}AA�DA �A�#A��AO�A�jAv�A$�A&�A	;dA��A�uAI�A%AA�FA`BA
=AG�A�;A{AM�A��A{A��AC�Az�A�TA&�A ff@�@���@��@�
=@��@���@�Ĝ@�Q�@��@�ȴ@���@�l�@�=q@�w@�R@���@�@�@�v�@��#@���@��@�@��@��#@��@��@��H@��
@��@旍@�Ĝ@�S�@�33@��#@�A�@��H@�-@݁@��`@�hs@޸R@�-@���@�o@�
=@ܬ@��
@�V@�5?@އ+@ޏ\@�V@އ+@�E�@ݲ-@�G�@��;@�@��/@��@���@ش9@���@��/@أ�@�b@׾w@��@�$�@�7L@Ӆ@ҧ�@�M�@҇+@҇+@��T@�x�@ёh@щ7@�G�@Гu@�S�@�E�@�5?@�-@Ͳ-@��T@�@��#@�7L@̓u@��@˾w@�dZ@�$�@���@�X@��@�Z@�(�@��@�b@Ǿw@���@Ɨ�@��@ź^@őh@�hs@�x�@�7L@�Ĝ@öF@�t�@�S�@�v�@�-@���@�p�@�&�@��/@�r�@�1'@���@�@�E�@���@��#@��7@��@���@�A�@��@���@�\)@�o@��@��@���@�J@�/@���@�Q�@�A�@��w@�dZ@�C�@�ȴ@�$�@���@��7@�V@��u@�9X@��D@�r�@�j@��u@� �@��;@��P@��@�^5@�@���@�/@��@���@��@�1'@�|�@�"�@��@��@���@�J@�7L@���@�b@��w@��@�\)@�
=@��@�~�@�5?@���@�G�@��j@���@���@�r�@�Q�@���@�ƨ@�ƨ@��P@�dZ@��H@���@�n�@�M�@�$�@���@�7L@��/@��D@�r�@�Q�@�(�@��
@��w@�dZ@�@��@���@���@�~�@�v�@�ff@�5?@��-@�G�@��9@�(�@��m@��P@�@��@��y@��@���@��\@�~�@�5?@�J@��@��#@���@�&�@��/@���@��j@�j@� �@�b@��
@��@���@�5?@���@��@��T@�hs@��/@�Ĝ@��u@��@�C�@���@��+@�^5@�-@���@��h@�x�@�p�@�`B@�?}@���@���@�A�@�  @�t�@��@���@���G�O�@�\)@��u@�S�@}�@st�@j�!@c"�@W��@O��@E�@=O�@7|�@.V@'l�@#dZ@ 1'@��@�u@�
@�P@ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
PB
�B
,B
?}B
T�B
�B
��B
�B�BiyB�B��B�9B�)B�B	7B,BC�B[#B`BBdZBgmBhsBhsBjBo�Bp�Bt�B�bB��B�B�FB��B��B��BoBoBoBuB�B�B�B�B{BPB�B+B�qB��B�B�wB�dB�9B�dBVB8RBG�BT�BcTBG�B�;B��B+B%�B(�B�B+B�B��BŢB�!B�bBy�BL�BB�RBjB;dBDB
ƨB
�XB
�LB
��B
aHB
J�B
33B	��B	��B	�bB	[#B	:^B	"�B	PB��B�`B��B��BǮBĜBÖB��B��BÖBĜBƨBȴBȴB��B��B�B�;B�NB�ZB�B	B	JB	bB	�B	�B	�B	uB��B�sB�ZB�ZB�TB�NB�fB�B��B�B�ZB	  B��B��B�fB��BBB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�#B�#B�)B�)B�)B�5B�5B�;B�5B�)B�)B�BB�`B�`B�TB�TB�fB�mB�sB�yB�B	%B	uB	�B	�B	�B	!�B	�B	�B	�B	�B	bB	
=B	%B��B��B��B�B�B�B�B�B��B��B	  B��B��B��B	DB	JB	�B	�B	�B	�B	{B	�B	�B	�B	"�B	2-B	A�B	@�B	<jB	6FB	1'B	0!B	,B	$�B	 �B	�B	�B	!�B	0!B	@�B	?}B	<jB	2-B	8RB	D�B	K�B	[#B	hsB	p�B	s�B	s�B	w�B	y�B	{�B	|�B	}�B	z�B	{�B	�B	�DB	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�?B	�FB	�LB	�LB	�LB	�^B	�^B	�^B	�^B	�dB	�jB	�dB	�jB	�qB	��B	B	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�5B	�;B	�BB	�NB	�TB	�NB	�TB	�TB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�ZB	�ZB	�TB	�NB	�NB	�HB	�HB	�HB	�HB	�ZB	�ZB	�fB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
PB
�B
 �B
#�B
&�B
.B
7LB
=qB
D�B
K�B
N�B
VB
]/B
aHB
cTB
gmB
jB
m�B
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	��B	��B	� B	� B	� B	��B	� B	� B	��B	��B	��B	��B	� B	� B	��B	��B	��B	��B	��B	� B	��B	��B
B
B
TB
�B
,	B
?|B
T�B
�B
��B
�B�BiwB�B��B�5B�#B�B	1B, BC�B["B`<BdSBgjBhnBhpBj}Bo�Bp�Bt�B�]B��B�B�DB��B��B��BkBmBlBsB{B�B�B�BzBJB�B$B�oB��B�
B�sB�]B�6B�aBRB8PBG�BT�BcRBG�B�9B��B'B%�B(�B~B$B�B��BŜB�B�_By�BL�B �B�LBjzB;_B?B
ƨB
�UB
�IB
��B
aGB
J�B
36B	��B	��B	�gB	[+B	:hB	"�B	\B��B�kB�B��BǺBĩBäB��B��BäBĪBƵB��BȾB��B��B�$B�IB�\B�gB�B	$B	SB	jB	�B	�B	�B	B��B�~B�eB�dB�_B�ZB�sB�B��B�B�cB	 B�B��B�qB��BBB��B�B�B��B��B��B��B��B��B�B��B��B�B�"B�'B�,B�,B�5B�2B�2B�BB�BB�DB�@B�3B�3B�MB�iB�hB�^B�`B�pB�vB�|B�B�B	/B	~B	�B	�B	�B	!�B	�B	�B	�B	�B	iB	
DB	-B��B��B��B�B�B�B�B�B��B��B	 B��B��B��B	LB	QB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	23B	A�B	@�B	<oB	6KB	1-B	0)B	,B	$�B	 �B	�B	�B	!�B	0&B	@�B	?�B	<nB	24B	8XB	D�B	K�B	[&B	hzB	p�B	s�B	s�B	w�B	y�B	{�B	|�B	}�B	z�B	{�B	�B	�EB	�IB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�/B	�5B	�BB	�HB	�LB	�MB	�LB	�`B	�^B	�`B	�`B	�eB	�lB	�iB	�jB	�qB	��B	B	ţB	ƧB	ƨB	ǱB	ȶB	ɻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	�
B	�	B	�B	�B	�B	�B	�B	�B	�B	�
B	� B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�%B	�4B	�5B	�<B	�BB	�OB	�SB	�OB	�SB	�SB	�PB	�NB	�LB	�UB	�TB	�XB	�XB	�YB	�ZB	�[B	�]B	�^B	�^B	�YB	�XB	�TB	�MB	�NB	�HB	�IB	�GB	�HB	�ZB	�ZB	�eB	�qB	�sB	�tB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B	��B
B
G�O�B
B
NB
�B
 �B
#�B
&�B
.B
7LB
=oB
D�B
K�B
N�B
U�B
](B
aCB
cRB
gjB
j|B
m�B
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.06 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436362016080714363620160807143636  AO  ARCAADJP                                                                    20150624191709    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150624191709  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150624191709  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143636  IP                  G�O�G�O�G�O�                