CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:50Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125950  20190405100757  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @����� 1   @��O�P@0Ƨ�da��l�D1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CC�fCF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D�	�D�<�D�l�D�ٚD��D�6fD�p D�� D���D�,�D�i�D��3D�3D�P D�p D��fD�3D�C3D� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@љ�A��A(��AH��Ah��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B
33B33B33B"33B*33B233B:33BB33BJ33BR33BZ33Bb33Bj33Br33Bz33B��B��B��B��B��B��B��B��B��B��B��B��B�� B�L�B��B�L�B��B��B��B��gB��B��B��B��B��B�L�B��B��B��gB��B��B��C ��C��C��C��C�gC
��C��C��C��C��C��C��C��C��C��C�gC ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<�gC>��C@��CB��CDs3CF��CH��CJs3CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`�gCb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�FfC�S3C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�S3C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfD #3D �3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D	#3D	�3D
#3D
�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D�D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D #3D �3D!#3D!�3D"#3D"�3D##3D#�3D$#3D$�3D%#3D%�3D&#3D&�3D'#3D'�3D(#3D(�3D)#3D)�3D*#3D*�3D+#3D+�3D,#3D,�3D-)�D-�3D.#3D.�3D/#3D/�3D0#3D0�3D1#3D1�3D2#3D2�3D3#3D3�3D4#3D4�3D5#3D5�3D6#3D6�3D7#3D7�3D8#3D8�3D9#3D9�3D:#3D:�3D;#3D;�3D<#3D<�3D=#3D=�3D>#3D>�3D?#3D?�3D@#3D@�3DA#3DA�3DB#3DB�3DC#3DC�3DD#3DD�3DE#3DE�3DF#3DF�3DG#3DG�3DH#3DH�3DI#3DI�3DJ#3DJ�3DK#3DK�3DL#3DL�3DM#3DM�3DN#3DN�3DO#3DO�3DP#3DP�3DQ#3DQ�3DR#3DR�3DS#3DS�3DT#3DT�3DU#3DU�3DV#3DV�3DW#3DW�3DX#3DX�3DY#3DY�3DZ#3DZ�3D[#3D[�3D\#3D\�3D]#3D]�3D^#3D^�3D_#3D_�3D`#3D`�3Da#3Da�3Db#3Db�3Dc#3Dc�3Dd#3Dd�3De#3De�3Df#3Df�3Dg#3Dg�3Dh#3Dh�3Di#3Di�3Dj)�Dj�3Dk#3Dk�3Dl#3Dl�3Dm#3Dm�3Dn#3Dn�3Do#3Do�3Dp#3Dp�3Dq#3Dq�3Dr#3Dr�3Ds#3Ds�3Dt#3Dt��Dy� D�4D�NgD�~gD��4D�+4D�H D���D��D�4D�>gD�{4D���D�$�D�a�Dځ�D�� D��D�T�D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�(�A�+A�+A�+A�(�A�(�A�+A�/A�1'A�1'A�33A�5?A�=qA�?}A�?}A�A�A�C�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�G�A�G�A�I�A�G�A�I�A�M�A�M�A�I�A�I�A�G�A�E�A�A�A�5?A��A�VA�1'A�JA�VA���A�"�A�O�A��RA��A��7A��A�  A���A�K�A���A�=qA��hA�S�A�$�A���A���A�E�A��RA��
A�\)A���A�/A��;A��+A���A�~�A��HA��A�%A��A���A�C�A�XA���A�JA���A��9A��A��#A�oA�+A~�DA}K�Az~�Aw�-Asx�AnĜAj�HAbĜA_G�A]dZAY��AUp�ASC�APĜAO+ANv�AM��AM+AJ�/AHM�AGC�AG%AF�\ACG�AA�hAAoA?��A;dZA9�A8JA6bNA4��A3ƨA3l�A2��A1�A//A.�+A-�mA-�FA-�PA-p�A-7LA+��A)S�A&��A$Q�A#�-A"jA!�;A!��A!�7A!��A!�A 5?A jA ZA��AdZAI�Ax�A(�A`BA��AA��A�TA�uA?}A��AQ�A��A7LA	��A�/A��AA7LAȴAoA&�A �A -@�dZ@���@���@�bN@�A�@�9X@��m@���@��-@�b@�M�@��/@�@��@�Q�@@���@�ȴ@�K�@��@��;@�?}@�@�+@�ƨ@�@웦@�1'@��@���@陚@�/@�9@�O�@畁@��H@�C�@���@�(�@��m@�-@�Z@�u@��@�%@���@�@�
=@��y@��@��@�K�@�%@�(�@�t�@�
=@��/@��@�7L@�Z@���@�O�@޸R@�Ĝ@��@�Z@߾w@�|�@�;d@���@ް!@�^5@�{@�/@��@ܣ�@� �@�dZ@���@�Ĝ@�1@׾w@׍P@�|�@�S�@�V@�O�@�r�@�  @��;@��;@Ӿw@�l�@ҸR@�J@�7L@�Q�@��y@Ώ\@θR@Χ�@�~�@�-@�@̴9@�bN@�I�@���@�ƨ@�l�@�^5@��T@�`B@���@Ȭ@�z�@� �@�1@��@ǥ�@�|�@�\)@���@��y@Ə\@��@�&�@�Z@���@�K�@��y@�-@��^@�7L@��@�V@��@��/@��j@��@��
@�;d@���@�~�@�n�@�=q@�@�`B@�V@��/@��u@�A�@��;@���@�"�@���@�=q@��T@��h@�?}@���@���@��j@���@�j@�A�@���@��@�~�@�J@��T@��7@���@�1'@��@�K�@�33@��R@�M�@��@���@�7L@���@�A�@��w@��@���@��@���@�E�@���@�&�@��`@�Ĝ@�Ĝ@�Q�@���@���@��@��@���@�=q@�@��T@��7@���@�  @���@��@�@��R@�=q@��^@�`B@�/@���@��/@���@��j@���@�1'@��F@���@��@�S�@��@��@��@��R@�v�@�V@�E�@�{@���@���@�/@�Ĝ@��u@�z�@�bN@���@�ƨ@�dZ@�"�@�
=@��y@���@���@�n�@�M�@��@��#@���@�O�@��9@�bN@�b@�ƨ@���@�|�@�\)@���@���@�v�@���@���@�G�@���@�I�@��@�b@�b@�b@� �@��@�1@��;@��w@��w@��w@��w@��w@��w@��F@��@�33@���@��@��y@��R@�~�@�hs@�Ĝ@��u@��u@��@�j@�j@�bN@�Q�@�A�@�1'@�1@�ƨ@�l�@�33@��@�~�@�M�@�-@���@��-@�`B@�/@�V@�{@��@���@���@x1'@lz�@d(�@\�j@S�m@L�j@Fff@@bN@7�@/;d@)�^@#C�@�@�`@�@�;@
M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�(�A�+A�+A�+A�(�A�(�A�+A�/A�1'A�1'A�33A�5?A�=qA�?}A�?}A�A�A�C�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�G�A�G�A�I�A�G�A�I�A�M�A�M�A�I�A�I�A�G�A�E�A�A�A�5?A��A�VA�1'A�JA�VA���A�"�A�O�A��RA��A��7A��A�  A���A�K�A���A�=qA��hA�S�A�$�A���A���A�E�A��RA��
A�\)A���A�/A��;A��+A���A�~�A��HA��A�%A��A���A�C�A�XA���A�JA���A��9A��A��#A�oA�+A~�DA}K�Az~�Aw�-Asx�AnĜAj�HAbĜA_G�A]dZAY��AUp�ASC�APĜAO+ANv�AM��AM+AJ�/AHM�AGC�AG%AF�\ACG�AA�hAAoA?��A;dZA9�A8JA6bNA4��A3ƨA3l�A2��A1�A//A.�+A-�mA-�FA-�PA-p�A-7LA+��A)S�A&��A$Q�A#�-A"jA!�;A!��A!�7A!��A!�A 5?A jA ZA��AdZAI�Ax�A(�A`BA��AA��A�TA�uA?}A��AQ�A��A7LA	��A�/A��AA7LAȴAoA&�A �A -@�dZ@���@���@�bN@�A�@�9X@��m@���@��-@�b@�M�@��/@�@��@�Q�@@���@�ȴ@�K�@��@��;@�?}@�@�+@�ƨ@�@웦@�1'@��@���@陚@�/@�9@�O�@畁@��H@�C�@���@�(�@��m@�-@�Z@�u@��@�%@���@�@�
=@��y@��@��@�K�@�%@�(�@�t�@�
=@��/@��@�7L@�Z@���@�O�@޸R@�Ĝ@��@�Z@߾w@�|�@�;d@���@ް!@�^5@�{@�/@��@ܣ�@� �@�dZ@���@�Ĝ@�1@׾w@׍P@�|�@�S�@�V@�O�@�r�@�  @��;@��;@Ӿw@�l�@ҸR@�J@�7L@�Q�@��y@Ώ\@θR@Χ�@�~�@�-@�@̴9@�bN@�I�@���@�ƨ@�l�@�^5@��T@�`B@���@Ȭ@�z�@� �@�1@��@ǥ�@�|�@�\)@���@��y@Ə\@��@�&�@�Z@���@�K�@��y@�-@��^@�7L@��@�V@��@��/@��j@��@��
@�;d@���@�~�@�n�@�=q@�@�`B@�V@��/@��u@�A�@��;@���@�"�@���@�=q@��T@��h@�?}@���@���@��j@���@�j@�A�@���@��@�~�@�J@��T@��7@���@�1'@��@�K�@�33@��R@�M�@��@���@�7L@���@�A�@��w@��@���@��@���@�E�@���@�&�@��`@�Ĝ@�Ĝ@�Q�@���@���@��@��@���@�=q@�@��T@��7@���@�  @���@��@�@��R@�=q@��^@�`B@�/@���@��/@���@��j@���@�1'@��F@���@��@�S�@��@��@��@��R@�v�@�V@�E�@�{@���@���@�/@�Ĝ@��u@�z�@�bN@���@�ƨ@�dZ@�"�@�
=@��y@���@���@�n�@�M�@��@��#@���@�O�@��9@�bN@�b@�ƨ@���@�|�@�\)@���@���@�v�@���@���@�G�@���@�I�@��@�b@�b@�b@� �@��@�1@��;@��w@��w@��w@��w@��w@��w@��F@��@�33@���@��@��y@��R@�~�@�hs@�Ĝ@��u@��u@��@�j@�j@�bN@�Q�@�A�@�1'@�1@�ƨ@�l�@�33@��@�~�@�M�@�-@���@��-@�`B@�/@�V@�{@��@���@���@x1'@lz�@d(�@\�j@S�m@L�j@Fff@@bN@7�@/;d@)�^@#C�@�@�`@�@�;@
M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	|�B	}�B	}�B	}�B	~�B	~�B	~�B	~�B	~�B	~�B	� B	� B	� B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�=B	�=B	�PB	��B	�!B	ŢB	��B	�HB	�HB	��B
PB
�B
I�B
\)B
��B
ÖB
��B
��B
�`B
��B
��BB+BVB�B�B/B>wB6FBT�B2-B
�B
�B
�B
_;B
0!B
 �B
uB
B	�B	�B	��B	ǮB	��B	�)B	�B	�/B	�5B	��B	��B	��B	�B	�B	�}B	��B	�1B	I�B	#�B	uB	PB	+B	  B��B��B��B��B��B��B��B��B��B�B��B��B��B�B�B�B��B	DB	oB	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	%�B	&�B	%�B	�B	oB	VB	PB	1B	DB	DB	hB	�B	'�B	/B	A�B	C�B	@�B	;dB	5?B	1'B	)�B	6FB	/B	$�B	{B	%B��B��B�B�mB�NB�5B��BÖBɺB�ZB�HB�#BȴB�}BBÖBɺB��B��B��B��B��B�B�NB�sB�B�B��B��B��B��B��B	B	hB	�B	$�B	-B	&�B	"�B	$�B	49B	;dB	=qB	@�B	B�B	A�B	A�B	C�B	D�B	L�B	J�B	H�B	T�B	bNB	iyB	iyB	gmB	dZB	iyB	o�B	r�B	t�B	z�B	z�B	}�B	~�B	�B	�DB	�B	m�B	k�B	n�B	� B	�B	�B	�1B	�\B	�{B	��B	�?B	�?B	�FB	�?B	�FB	�jB	�wB	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�
B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�BB	�HB	�HB	�HB	�TB	�TB	�TB	�TB	�fB	�fB	�`B	�`B	�TB	�NB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B
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
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
PB
JB
JB
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
VB
PB
PB
PB
PB
PB
PB
JB
PB
VB
VB
\B
\B
\B
\B
bB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
/B
5?B
8RB
>wB
A�B
H�B
K�B
R�B
VB
\)B
bNB
ffB
k�B
o�B
r�B
v�B
z�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	|�B	}�B	}�B	}�B	~�B	~�B	~�B	~�B	~�B	~�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�$B	��B	��B	�uB	��B	�B	�B	��B
!B
�B
I�B
[�B
��B
�gB
ΩB
ʔB
�0B
��B
��B�B�B$BQBuB.�B>CB6BT�B1�B
�bB
��B
��B
_	B
/�B
 �B
BB
 �B	�TB	��B	ʋB	�xB	ҺB	��B	��B	��B	��B	ѴB	ӾB	��B	�GB	��B	�EB	��B	��B	I�B	#�B	<B	B	�B��B��B��B��B��B��B��B��B��B��B�xB��B��B��B�sB�eB�eB��B	B	1B	AB	JB	OB	UB	NB	fB	!�B	#�B	$�B	%�B	&�B	%�B	yB	0B	B	B	�B	B	B	(B	uB	'�B	.�B	AIB	CUB	@DB	; B	5B	0�B	)�B	6B	.�B	$�B	7B	�B��B�yB�>B�)B�B��BӴB�VB�vB�B�B��B�qB�<B�KB�PB�wBΖBϜBѦBҮBӳB��B�B�0B�RB�rB��B��B��B��B��B	�B	#B	bB	$�B	,�B	&�B	"�B	$�B	3�B	; B	=+B	@>B	BHB	ADB	AAB	CPB	DTB	L�B	JzB	HsB	T�B	b	B	i3B	i4B	g'B	dB	i4B	oZB	rjB	txB	z�B	z�B	}�B	~�B	��B	��B	��B	mLB	k?B	nQB	�B	��B	��B	��B	�B	�7B	��B	��B	��B	� B	��B	�B	�&B	�1B	�RB	�]B	�hB	�|B	̈B	̈B	̉B	ˁB	�|B	�{B	�{B	�|B	̈B	͋B	ΔB	ѥB	ѧB	ӱB	ԷB	��B	��B	��B	��B	��B	��B	��B	��B	ӳB	ԹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	� B	�B	�B	�B	�B	�
B	�B	�B	�!B	� B	�(B	�%B	�,B	�*B	�2B	�8B	�HB	�VB	�UB	�PB	�KB	�KB	�LB	�CB	�DB	�JB	�KB	�[B	�dB	�jB	�rB	�pB	�oB	�pB	�nB	�tB	�uB	�vB	�sB	�uB	�uB	�vB	�oB	�pB	�gB	�jB	�uB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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

�B
B
B
B
B
B
B
B
B

B
B
B

B
	B
	B
B
B
B
B
B
B
B
B
B
B
B
B
	B
B
B
	B

B
B
	B
B
B
B
B
B
B
B
&B
,B
4B
4B
:B
8B
<B
9B
>B
>B
>B
>B
>B
>B
9B
?B
@B
?B
AB
7B
8B
3B
5B
3B
6B
9B
9B
=B
9B
:B
<B
8B
:B
9B
8B
>B
?B
>B
GB
HB
GB
GB
IB
JB
KG�O�B
^B
kB
$�B
.�B
4�B
8B
>/B
ACB
HmB
KB
R�B
U�B
[�B
bB
fB
k;B
oXB
rhB
v�B
z�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.55 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007572019040510075720190405100757  AO  ARCAADJP                                                                    20181121125950    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125950  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125950  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100757  IP                  G�O�G�O�G�O�                