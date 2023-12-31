CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-05T02:16:08Z AOML 3.0 creation; 2016-08-07T21:17:35Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150405021608  20160807141735  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               *A   AO  5285_8895_042                   2C  D   APEX                            6487                            072314                          846 @�F���
1   @�F����@.LI�^5?�c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    *A   B   B   @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bq��Bw��B�  B�  B�  B���B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�3D�S3D�� D�ٚD� D�<�D�y�D���D��D�FfD�i�DǬ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�fg@љ�A��A(��AH��Ah��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B
33B33B33B"33B*33B233B:33BB33BJ33BR33BZ33Bb33Bj33Bs��By��B��B��B��B��4B��B��gB��B��B��B��gB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C ��C��C��C��C�gC
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:�gC<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cfs3Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�FfC�FfC�FfC�FfC�FfC�FfC�9�C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfD #3D �3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D	#3D	�3D
#3D
�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D #3D �3D!#3D!�3D"#3D"�3D##3D#�3D$#3D$�3D%#3D%�3D&#3D&�3D'#3D'�3D(#3D(�3D)#3D)�3D*#3D*�3D+#3D+�3D,#3D,�3D-#3D-�3D.#3D.�3D/#3D/�3D0#3D0�3D1#3D1�3D2#3D2�3D3#3D3�3D4#3D4�3D5#3D5�3D6#3D6�3D7#3D7�3D8#3D8�3D9#3D9�3D:#3D:�3D;#3D;�3D<#3D<�3D=#3D=�3D>#3D>�3D?#3D?�3D@#3D@�3DA#3DA�3DB#3DB�3DC#3DC�3DD#3DD�3DE#3DE�3DF#3DF�3DG#3DG�3DH#3DH�3DI#3DI�3DJ#3DJ�3DK#3DK�3DL#3DL�3DM#3DM�3DN#3DN�3DO#3DO�3DP#3DP�3DQ#3DQ�3DR#3DR�3DS#3DS�3DT#3DT�3DU#3DU�3DV#3DV�3DW#3DW�3DX#3DX�3DY#3DY�3DZ#3DZ�3D[#3D[�3D\#3D\�3D]#3D]�3D^#3D^�3D_#3D_�3D`#3D`�3Da#3Da�3Db#3Db�3Dc#3Dc�3Dd#3Dd�3De#3De�3Df#3Df�3Dg#3Dg�3Dh#3Dh�3Di#3Di�3Dj#3Dj�3Dk#3Dk�3Dl#3Dl�3Dm#3Dm�3Dn#3Dn�3Do#3Do�3Dp#3Dp�3Dq#3Dq�3Dr#3Dr�3Ds#3Ds�3Dt#3Dt��Dy� D��D�d�D���D��4D�!�D�NgD��4D��gD�.gD�X D�{4DǾg111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӅAӅAӃAӅAӇ+AӅAӅAӇ+AӋDAӋDAӋDAӍPAӑhAӑhAӑhAӓuAӕ�Aӗ�Aә�Aӗ�Aӛ�Aӟ�Aӡ�Aӡ�Aӝ�Aӏ\AӃA�r�A��A�r�A�I�Aϕ�A��AρA�~�A�9XA�M�A�dZA�1A�~�A��A��;A�7LA���A���A�-A�dZA��A�;dA�1'A�I�A���A��A��uA�7LA�r�A���A��TA��A�C�A�O�A�/A��A���A�
=A�G�A�v�A��\A��A���A�G�A�oA��TA��A��-A�JA+Aw%Aq?}Ap��Ao�
Am7LAj�/Ah��Ah(�Ae�TA_?}A\~�AYt�AS��AOK�AL��AK
=AJ9XAF�ADAB�!AA�A@v�A?\)A=A8��A7��A6��A5�
A2�uA0ĜA/�PA.VA-�FA,�HA+t�A)��A'��A$�RA"�`A"z�A"A�A!�;A!7LA!&�A!|�A ��A��AM�A�wAx�A�9A(�A�7AA�wA�A�`AA�A�9AA�AȴA��A(�A�A$�Ap�A�A�AC�AZA�A+A
z�A	�;A �A33AĜAA��A?}A�jA�!A�+An�AbNA�-A/AVAx�A �uA �A (�A I�@��!@��#@�@��7@��@�1'@���@�  @�  @��@���@��@��@��P@���A z�AK�A��A��A�AdZA+@��w@��A   @�+@���@�7L@��@��@�J@��-@�@�&�@��@��@���@�J@�&�@�+@�I�@@�;d@�@�v�@�t�@@��@�bN@��@�1@�P@�\)@�C�@���@�ff@��T@���@�ƨ@�o@�@�C�@�!@��T@��@�1'@�  @�ƨ@���@�=q@ݺ^@�p�@�X@�hs@�G�@܃@��;@۾w@ۮ@�t�@�@���@�ȴ@ڏ\@�n�@�5?@���@ف@أ�@� �@�1@�  @׮@��@֧�@�{@�?}@�%@���@���@�Ĝ@��@���@җ�@�5?@��#@ѡ�@�O�@���@��@��@�r�@���@�l�@��@���@�G�@�/@�%@���@̼j@�1@˅@�+@��@ʸR@ʟ�@�n�@���@ɲ-@Ȭ@�A�@���@Ǖ�@�"�@Ƨ�@�n�@���@�hs@Ĵ9@�ƨ@��y@�=q@�J@�hs@�z�@��@�C�@��y@���@���@�^5@��h@���@�Q�@��@�t�@��\@�^5@�^5@�$�@�J@���@��h@�V@��D@��w@��!@�J@��-@�G�@���@���@��w@���@�o@��\@�n�@�{@���@�p�@�%@���@��j@��9@��u@�A�@���@��@�dZ@�33@��H@��R@�V@���@���@���@�hs@�/@���@��u@�1'@�b@��
@��w@��@�@�ȴ@��+@�$�@��#@���@��-@�p�@��@� �@��@���@���@�dZ@��@��+@��\@�~�@��@���@��7@�&�@���@�j@�A�@�1'@��
@�33@�ȴ@���@��\@�V@���@�@��7@�/@�&�@���@��`@���@�I�@�33@��@���@�ȴ@�~�@�=q@�@���@�p�@�hs@�/@���@��/@��@� �@��m@�ƨ@��F@���@�l�@�\)@��R@�M�@�@��^@���@���@��@�`B@�&�@���@��@�b@��F@��@�;d@��y@�ȴ@��!@���@�~�@�E�@�{@��^@�hs@�?}@��@��@��@���@��@��@�z�@�Q�@�b@�1@��@�dZ@���@���@�ff@�J@��-@�X@�?}@�V@���@�r�@�Z@��7@�=q@�z�@s"�@l��@fE�@_l�@W�;@N��@C"�@:=q@2��@-�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   AӅAӅAӃAӅAӇ+AӅAӅAӇ+AӋDAӋDAӋDAӍPAӑhAӑhAӑhAӓuAӕ�Aӗ�Aә�Aӗ�Aӛ�Aӟ�Aӡ�Aӡ�Aӝ�Aӏ\AӃA�r�A��A�r�A�I�Aϕ�A��AρA�~�A�9XA�M�A�dZA�1A�~�A��A��;A�7LA���A���A�-A�dZA��A�;dA�1'A�I�A���A��A��uA�7LA�r�A���A��TA��A�C�A�O�A�/A��A���A�
=A�G�A�v�A��\A��A���A�G�A�oA��TA��A��-A�JA+Aw%Aq?}Ap��Ao�
Am7LAj�/Ah��Ah(�Ae�TA_?}A\~�AYt�AS��AOK�AL��AK
=AJ9XAF�ADAB�!AA�A@v�A?\)A=A8��A7��A6��A5�
A2�uA0ĜA/�PA.VA-�FA,�HA+t�A)��A'��A$�RA"�`A"z�A"A�A!�;A!7LA!&�A!|�A ��A��AM�A�wAx�A�9A(�A�7AA�wA�A�`AA�A�9AA�AȴA��A(�A�A$�Ap�A�A�AC�AZA�A+A
z�A	�;A �A33AĜAA��A?}A�jA�!A�+An�AbNA�-A/AVAx�A �uA �A (�A I�@��!@��#@�@��7@��@�1'@���@�  @�  @��@���@��@��@��P@���A z�AK�A��A��A�AdZA+@��w@��A   @�+@���@�7L@��@��@�J@��-@�@�&�@��@��@���@�J@�&�@�+@�I�@@�;d@�@�v�@�t�@@��@�bN@��@�1@�P@�\)@�C�@���@�ff@��T@���@�ƨ@�o@�@�C�@�!@��T@��@�1'@�  @�ƨ@���@�=q@ݺ^@�p�@�X@�hs@�G�@܃@��;@۾w@ۮ@�t�@�@���@�ȴ@ڏ\@�n�@�5?@���@ف@أ�@� �@�1@�  @׮@��@֧�@�{@�?}@�%@���@���@�Ĝ@��@���@җ�@�5?@��#@ѡ�@�O�@���@��@��@�r�@���@�l�@��@���@�G�@�/@�%@���@̼j@�1@˅@�+@��@ʸR@ʟ�@�n�@���@ɲ-@Ȭ@�A�@���@Ǖ�@�"�@Ƨ�@�n�@���@�hs@Ĵ9@�ƨ@��y@�=q@�J@�hs@�z�@��@�C�@��y@���@���@�^5@��h@���@�Q�@��@�t�@��\@�^5@�^5@�$�@�J@���@��h@�V@��D@��w@��!@�J@��-@�G�@���@���@��w@���@�o@��\@�n�@�{@���@�p�@�%@���@��j@��9@��u@�A�@���@��@�dZ@�33@��H@��R@�V@���@���@���@�hs@�/@���@��u@�1'@�b@��
@��w@��@�@�ȴ@��+@�$�@��#@���@��-@�p�@��@� �@��@���@���@�dZ@��@��+@��\@�~�@��@���@��7@�&�@���@�j@�A�@�1'@��
@�33@�ȴ@���@��\@�V@���@�@��7@�/@�&�@���@��`@���@�I�@�33@��@���@�ȴ@�~�@�=q@�@���@�p�@�hs@�/@���@��/@��@� �@��m@�ƨ@��F@���@�l�@�\)@��R@�M�@�@��^@���@���@��@�`B@�&�@���@��@�b@��F@��@�;d@��y@�ȴ@��!@���@�~�@�E�@�{@��^@�hs@�?}@��@��@��@���@��@��@�z�@�Q�@�b@�1@��@�dZ@���@���@�ff@�J@��-@�X@�?}@�V@���@�r�G�O�@��7@�=q@�z�@s"�@l��@fE�@_l�@W�;@N��@C"�@:=q@2��@-�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB�7B�7B�7B�7B�7B�7B�7B�7B�7B�=B�7B�=B�DB�DB�=B�=B�DB�DB�PB�=B�JB�uB��B��B��B�-B�RB�}BȴB��B	+B	R�B	�/B
�\BBbB&�B>wB_;B� B�oB�bB�uBĜB�#B�BBŢB�?B�=BbNB>wB1'B!�B{BJBB
��B
�B
�B
��B
�^B
�PB
z�B
l�B
cTB
[#B
P�B
B�B
/B
�B
DB
B
1B	��B	�B	�ZB	�}B	��B	��B	��B	��B	�7B	{�B	r�B	_;B	D�B	0!B	�B��B��B��B�qB�5B�NB�NB�fB�B�B�B�B��B	  B	  B��B	%B	PB	VB	PB	oB	oB	VB	DB	B��B��B��B	  B	+B	B	JB	(�B	0!B	9XB	49B	1'B	1'B	0!B	7LB	9XB	:^B	7LB	@�B	=qB	8RB	-B	.B	.B	0!B	:^B	@�B	C�B	:^B	;dB	9XB	6FB	49B	5?B	;dB	9XB	6FB	1'B	1'B	2-B	-B	+B	)�B	)�B	)�B	-B	.B	/B	/B	.B	33B	7LB	8RB	6FB	49B	7LB	>wB	D�B	D�B	H�B	N�B	O�B	K�B	R�B	W
B	`BB	q�B	�7B	�=B	�1B	�%B	�PB	��B	ƨB	�)B	�yB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�fB	�B	�B	�HB	�B	�B	�B	�/B	�#B	�ZB	�TB	�;B	�)B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�
B	��B	�B	�B	�B	�
B	�B	�/B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�`B	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
%B
1B
1B
1B
1B
1B
1B
+B
+B
+B
1B
	7B

=B

=B

=B
	7B
	7B
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
DB
JB
JB
JB
JB
DB
DB
JB
JB
JB
JB
JB
PB
VB
PB
PB
PB
PB
VB
VB
\B
bB
oB
oB
oB
oB
oB
oB
oB
uB
{B
uB
uB
{B
�B
�B
�B
�B
{B
hB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
"�B
"�B
$�B
#�B
)�B
1'B
9XB
?}B
D�B
G�B
K�B
Q�B
XB
_;B
dZB
hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   B�B�B�
B�B�
B�B�
B�B�(B�B�%B�B�4B�4B�B�B�1B�B�"B�+B�B�aB��B��B��B�B�>B�iBȢB��B	B	R�B	�B
�7B�B4B&�B>JB_B�B�?B�4B�HB�mB��B��B�_B�sB�B�BbB>GB0�B!�BKBB�B
��B
�aB
��B
̝B
�.B
�B
z�B
l[B
c#B
Z�B
P�B
B`B
.�B
QB
B
�B
B	��B	��B	�*B	�KB	�~B	��B	��B	�QB	�	B	{�B	r�B	_B	DpB	/�B	B��B��B�aB�HB�B�&B�#B�:B�VB�SB�ZB�jB��B��B��B��B	�B	$B	&B	!B	?B	?B	%B	B	�B��B��B��B��B	�B	�B	B	(�B	/�B	9%B	4B	0�B	0�B	/�B	7B	9$B	:)B	7B	@OB	==B	8B	,�B	-�B	-�B	/�B	:*B	@NB	CaB	:*B	;0B	9$B	6B	4B	5B	;0B	9 B	6B	0�B	0�B	1�B	,�B	*�B	)�B	)�B	)�B	,�B	-�B	.�B	.�B	-�B	2�B	7B	8B	6B	4B	7B	>AB	DeB	DfB	H~B	N�B	O�B	K�B	R�B	V�B	`
B	qqB	��B	�B	��B	��B	�B	�gB	�lB	��B	�;B	�iB	�{B	�rB	�hB	�PB	�\B	�~B	�B	�xB	�aB	�NB	�OB	�SB	�fB	�|B	�wB	�B	�mB	�'B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	ЦB	�}B	ʅB	ШB	ҷB	ҵB	ѯB	ЫB	ϢB	ΜB	ϢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�	B	�B	�B	�"B	�6B	�6B	�5B	�6B	�5B	�7B	�7B	�:B	�?B	�GB	�EB	�OB	�NB	�FB	�GB	�GB	�EB	�NB	�SB	�WB	�ZB	�OB	�LB	�RB	�TB	�aB	�nB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�~B	�wB	�oB	�pB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
�B
�B
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
B
B

B
B
B
B
B
	B
	B
	B
	B

B
B
B
B
B
B
B
B
B
B
 B
-B
-B
+B
.B
-B
-B
-B
5B
8B
3B
6B
;B
>B
?B
AB
BB
:B
&B
2B
6B
8B
9B
@B
JB
KB
KB
FB
EB
FB
>B
AB
?B
@B
LB
KB
EB
HB
>B
KB
IB
JB
PB
RB
ZB
YB
XB
\B
\B
]B
]B
fB
fB
cB
jB
jB
kB
lB
eB
dB
oB
qB
pB
oB
pB
pB
{B
|B
|B
{B
|B
}B
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
!�B
!�B
"�B
"�G�O�B
#�B
)�B
0�B
9B
?;B
DXB
GlB
K�B
Q�B
W�B
^�B
dB
h0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.55 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417352016080714173520160807141735  AO  ARCAADJP                                                                    20150405021608    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150405021608  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150405021608  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141735  IP                  G�O�G�O�G�O�                